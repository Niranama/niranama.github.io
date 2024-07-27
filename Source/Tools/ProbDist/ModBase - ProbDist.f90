
MODULE ModBase_ProbDist

!** PURPOSE OF THIS MODULE:
    ! contains procedures to compute densities, mass functions, distribution functions
    ! and their inverses, and reliability functions, for various continuous and
    ! discrete probability laws. It also offers a mechanism for collecting observational
    ! data and computing elementary statistics on it, and tools for performing and
    ! reporting different types of univariate goodness-of-fit tests.

    !** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_Memory_Handlers,    ONLY: MemAlloc, MemFree, MemResize
    USE ModBase_MyLib

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! procedures
    PUBLIC :: FreeFMassInfo
    PUBLIC :: fmass_PoissonTerm1,  fmass_PoissonTerm2,  fmass_CreatePoisson
    PUBLIC :: fmass_BinomialTerm1, fmass_BinomialTerm2, fmass_CreateBinomial
    PUBLIC :: fmass_BinomialTerm3, fmass_BinomialTerm4
    PUBLIC :: fmass_NegaBinTerm1,  fmass_NegaBinTerm2,  fmass_CreateNegaBin

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'ModBase_ProbDist'
    ! common number parameters
    tDouble,   PARAMETER    :: ZERO_DBL    = 0.0_kDouble
    tDouble,   PARAMETER    :: HALF_DBL    = 0.5_kDouble
    tDouble,   PARAMETER    :: ONE_DBL     = 1.0_kDouble
    tDouble,   PARAMETER    :: TWO_DBL     = 2.0_kDouble
    ! math parameters
    tDouble,   PARAMETER    :: DBL_MAX     = 1.7976931348623158D+308    ! max value
    tDouble,   PARAMETER    :: DBL_MIN     = 2.2250738585072014D-308    ! min positive value
    tDouble,   PARAMETER    :: DBL_EPSILON = EPSILON(ONE_DBL)           ! machine epsilon
    tInteger,  PARAMETER    :: DBL_MAX_10_EXP = 308                     ! max decimal exponent
    tInteger,  PARAMETER    :: DBL_MAX_EXP =  1024                      ! max binary exponent
    tInteger,  PARAMETER    :: DBL_MIN_EXP = -1021                      ! min binary exponent
    tInteger,  PARAMETER    :: FLT_RADIX   = 2                          ! exponent radix
    tDouble,   PARAMETER    :: num_Pi      = 3.14159265358979323846_kDouble
    tDouble,   PARAMETER    :: num_ebase   = 2.7182818284590452354_kDouble
    tDouble,   PARAMETER    :: num_Rac2    = 1.41421356237309504880_kDouble
    tDouble,   PARAMETER    :: num_1Rac2   = 0.70710678118654752440_kDouble
    tDouble,   PARAMETER    :: num_Ln2     = 0.69314718055994530941_kDouble
    tDouble,   PARAMETER    :: num_1Ln2    = 1.44269504088896340737_kDouble
    tDouble,   PARAMETER    :: num_MaxIntDouble = 9007199254740992.0_kDouble
    tInteger,  PARAMETER    :: num_MaxTwoExp    = 64
    tDouble,   PARAMETER    :: num_TwoExp(0:num_MaxTwoExp) = [      &
       1.0_kDouble, 2.0_kDouble, 4.0_kDouble, 8.0_kDouble, 1.6D+01, 3.2D+01, &
       6.4D+01, 1.28D+02, 2.56D+02, 5.12D+02, 1.024D+03, &
       2.048D+03, 4.096D+03, 8.192D+03, 1.6384D+04, 3.2768D+04, &
       6.5536D+04, 1.31072D+05, 2.62144D+05, 5.24288D+05, &
       1.048576D+06, 2.097152D+06, 4.194304D+06, 8.388608D+06, &
       1.6777216D+07, 3.3554432D+07, 6.7108864D+07, &
       1.34217728D+08, 2.68435456D+08, 5.36870912D+08, &
       1.073741824D+09, 2.147483648D+09, 4.294967296D+09, &
       8.589934592D+09, 1.7179869184D+10, 3.4359738368D+10, &
       6.8719476736D+10, 1.37438953472D+11, 2.74877906944D+11, &
       5.49755813888D+11, 1.099511627776D+12, 2.199023255552D+12, &
       4.398046511104D+12, 8.796093022208D+12, &
       1.7592186044416D+13, 3.5184372088832D+13, &
       7.0368744177664D+13, 1.40737488355328D+14, &
       2.81474976710656D+14, 5.62949953421312D+14, &
       1.125899906842624D+15, 2.251799813685248D+15, &
       4.503599627370496D+15, 9.007199254740992D+15, &
       1.8014398509481984D+16, 3.6028797018963968D+16, &
       7.2057594037927936D+16, 1.44115188075855872D+17, &
       2.88230376151711744D+17, 5.76460752303423488D+17, &
       1.152921504606846976D+18, 2.305843009213693952D+18, &
       4.611686018427387904D+18, 9.223372036854775808D+18, &
       1.8446744073709551616D+19]
    tInteger                :: JPow
    tDouble,   PARAMETER    :: num_TENNEGPOW(0:16) = [(10.0D0**JPow, JPow = 0, -16, -1)]
    ! parameters of fmass submodule
    tDouble,   PARAMETER    :: fmass_Epsilon          = 1.0D-16
    tDouble,   PARAMETER    :: fmass_MaxLambdaPoisson = 100000.0_kDouble
    tDouble,   PARAMETER    :: fmass_MaxnBinomial     = 100000.0_kDouble
    tDouble,   PARAMETER    :: fmass_MaxnNegaBin      = 100000.0_kDouble
    ! parameters of fdist submodule
    ! - x infinity for some distributions
    tDouble,   PARAMETER    :: fdist_XINF  = DBL_MAX
    tDouble,   PARAMETER    :: fdist_XBIG  = 100.0_kDouble
    tDouble,   PARAMETER    :: fdist_XBIGM = 1000.0
    ! - EpsArray[j]: Epsilon required for j decimal degits of precision
    tInteger                :: IPow
    tDouble,   PARAMETER    :: EpsArray(0:35) = [(0.5*10.0D0**IPow, IPow = 0, -35, -1)]

!** DERIVED TYPE DEFINITIONS
    ! --------------------------------------------------------------------------
    ! --- Type of structure used to store precomputed discrete distributions ---
    ! --------------------------------------------------------------------------
    ! For better precision in the tails, we keep the cumulative probabilities
    ! (F) in cdf[s] for s <= smed (i.e. cdf[s] is the sum off all the probabi-
    ! lities pdf[i] for i <= s),
    ! and the complementary cumulative probabilities (1 - F) in cdf[s] for
    ! s > smed (i.e. cdf[s] is the sum off all the probabilities pdf[i]
    ! for i >= s).
    ! --------------------------------------------------------------------------
    TYPE FMassInfo
        PRIVATE
        tDouble, ALLOCATABLE    :: CDF(:)       ! cumulative probabilities
        tDouble, ALLOCATABLE    :: PDF(:)       ! probability terms or mass distribution
        tDouble, ALLOCATABLE    :: ParamR(:)    ! real parameters of the distribution
        tLong,   ALLOCATABLE    :: ParamI(:)    ! integer parameters of the distribution
        tLong                   :: SMin         ! pdf(s) = 0 for s < smin
        tLong                   :: SMax         ! pdf(s) = 0 for s > smax
        tLong                   :: SMed         ! cdf(s) = F(s) for s <= smed, and
                                                ! cdf(s) = bar_F(s) for s > smed
    END TYPE FMassInfo

!** INTERFACE DEFINITIONS
    ABSTRACT INTERFACE
        FUNCTION wdist_CFUNC(Par, X) RESULT(ResVal)
            IMPORT
            tDouble, INTENT(IN)     :: Par(0:)
            tDouble, INTENT(IN)     :: X
            tDouble                 :: ResVal
        END FUNCTION
    END INTERFACE
    ! fmass procedures
    INTERFACE
        MODULE SUBROUTINE FreeFMassInfo(W)
!            IMPORT
            TYPE(FMassInfo), INTENT(INOUT)  :: W
        END SUBROUTINE
        MODULE FUNCTION fmass_PoissonTerm1(Lambda, S) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: Lambda
            tIndex,  INTENT(IN) :: S
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fmass_PoissonTerm2(W, S) RESULT(ResVal)
!            IMPORT
            TYPE(FMassInfo), INTENT(IN) :: W
            tIndex,          INTENT(IN) :: S
            tDouble                     :: ResVal
        END FUNCTION
        MODULE SUBROUTINE fmass_CreatePoisson(Lambda, W)
!            IMPORT
            tDouble,         INTENT(IN)     :: Lambda
            TYPE(FMassInfo), INTENT(OUT)    :: W
        END SUBROUTINE
        MODULE FUNCTION fmass_BinomialTerm1(N, PIn, QIn, SIn) RESULT(ResVal)
!            IMPORT
            tIndex,  INTENT(IN) :: N, SIn
            tDouble, INTENT(IN) :: PIn, QIn
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fmass_BinomialTerm2(W, S) RESULT(ResVal)
!            IMPORT
            TYPE(FMassInfo), INTENT(IN) :: W
            tIndex,          INTENT(IN) :: S
            tDouble                     :: ResVal
        END FUNCTION
        MODULE FUNCTION fmass_BinomialTerm3(N, PIn, SIn) RESULT(ResVal)
!            IMPORT
            tIndex,  INTENT(IN) :: N, SIn
            tDouble, INTENT(IN) :: PIn
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fmass_BinomialTerm4(N, P, P2, S) RESULT(ResVal)
!            IMPORT
            tIndex,  INTENT(IN) :: N, S
            tDouble, INTENT(IN) :: P, P2
            tDouble             :: ResVal
        END FUNCTION
        MODULE SUBROUTINE fmass_CreateBinomial(N, P, Q, W)
!            IMPORT
            tIndex,          INTENT(IN)     :: N
            tDouble,         INTENT(IN)     :: P, Q
            TYPE(FMassInfo), INTENT(OUT)    :: W
        END SUBROUTINE
        MODULE FUNCTION fmass_NegaBinTerm1(N, P, S) RESULT(ResVal)
!            IMPORT
            tIndex,  INTENT(IN) :: N, S
            tDouble, INTENT(IN) :: P
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fmass_NegaBinTerm2(W, S) RESULT(ResVal)
!            IMPORT
            TYPE(FMassInfo), INTENT(IN) :: W
            tIndex,          INTENT(IN) :: S
            tDouble                     :: ResVal
        END FUNCTION
        MODULE SUBROUTINE fmass_CreateNegaBin(N, P, W)
!            IMPORT
            tIndex,          INTENT(IN)     :: N
            tDouble,         INTENT(IN)     :: P
            TYPE(FMassInfo), INTENT(OUT)    :: W
        END SUBROUTINE
    END INTERFACE
    ! fdist procedures
    INTERFACE
        MODULE RECURSIVE FUNCTION fdist_belog(X) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE SUBROUTINE fdist_CalcB4(Alpha, PB, PLogB, PC, PLogC)
!            IMPORT
            tDouble,  INTENT(IN)    :: Alpha
            tDouble,  INTENT(INOUT) :: PB, PLogB, PC, PLogC
        END SUBROUTINE
        MODULE FUNCTION fdist_Unif(X) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_Expon(X) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_Weibull(Alpha, X) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: Alpha, X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_ExtremeValue(X) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_Logistic(X) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_Pareto(C, X) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: C, X
            tDouble             :: ResVal
        END FUNCTION
        MODULE RECURSIVE FUNCTION fdist_Normal1(X) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_Normal2(X) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_Normal3(X) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_Normal4(X) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_BiNormal1(X, Y, Rho, NDig) RESULT(ResVal)
!            IMPORT
            tDouble,  INTENT(IN)    :: X, Y, Rho
            tInteger, INTENT(IN)    :: NDig
            tDouble                 :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_BiNormal2(DH, DK, Rho) RESULT(ResVal)
!            IMPORT
            tDouble,  INTENT(IN)    :: DH, DK, Rho
            tDouble                 :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_LogNormal(Mu, Sigma, X) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: Mu, Sigma, X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_JohnsonSB(Alpha, Beta, A, B, X) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: Alpha, Beta, A, B, X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_JohnsonSU(Alpha, Beta, X) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: Alpha, Beta, X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_ChiSquare1(K, X) RESULT(ResVal)
!            IMPORT
            tIndex,  INTENT(IN) :: K
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_ChiSquare2(K, D, X) RESULT(ResVal)
!            IMPORT
            tIndex,   INTENT(IN)    :: K
            tInteger, INTENT(IN)    :: D
            tDouble,  INTENT(IN)    :: X
            tDouble                 :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_Student1(N, X) RESULT(ResVal)
!            IMPORT
            tIndex,  INTENT(IN) :: N
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_Student2(N, D, X) RESULT(ResVal)
!            IMPORT
            tIndex,   INTENT(IN)    :: N
            tInteger, INTENT(IN)    :: D
            tDouble,  INTENT(IN)    :: X
            tDouble                 :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_Gamma(Alpha, D, X) RESULT(ResVal)
!            IMPORT
            tDouble,  INTENT(IN)    :: Alpha, X
            tInteger, INTENT(IN)    :: D
            tDouble                 :: ResVal
        END FUNCTION
        MODULE RECURSIVE FUNCTION fdist_Beta(PIn, QIn, D, X) RESULT(ResVal)
!            IMPORT
            tDouble,  INTENT(IN)    :: PIn, QIn, X
            tInteger, INTENT(IN)    :: D
            tDouble                 :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_BetaSymmetric(P, X) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: P, X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_KSPlus(N, X) RESULT(ResVal)
!            IMPORT
            tIndex,  INTENT(IN) :: N
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_KS1(N, X) RESULT(ResVal)
!            IMPORT
            tIndex,  INTENT(IN) :: N
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_KS2(N0, X, NDig) RESULT(ResVal)
!            IMPORT
            tIndex,             INTENT(IN)  :: N0
            tDouble,            INTENT(IN)  :: X
            tInteger, OPTIONAL, INTENT(IN)  :: NDig
            tDouble                         :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_KSPlusJumpOne(N, A, X) RESULT(ResVal)
!            IMPORT
            tIndex,  INTENT(IN) :: N
            tDouble, INTENT(IN) :: A, X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_CramerMises(N, X) RESULT(ResVal)
!            IMPORT
            tIndex,  INTENT(IN) :: N
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_WatsonG(N, X) RESULT(ResVal)
!            IMPORT
            tIndex,  INTENT(IN) :: N
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_WatsonU(N, X) RESULT(ResVal)
!            IMPORT
            tIndex,  INTENT(IN) :: N
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_AndersonDarling(N, X) RESULT(ResVal)
!            IMPORT
            tIndex,  INTENT(IN) :: N
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_AndersonDarling2(N, X, IsExact) RESULT(ResVal)
!            IMPORT
            tIndex,             INTENT(IN)  :: N
            tDouble,            INTENT(IN)  :: X
            tLogical, OPTIONAL, INTENT(IN)  :: IsExact
            tDouble                         :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_Geometric(P, S) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: P
            tIndex,  INTENT(IN) :: S
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_Poisson1(Lambda, S) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: Lambda
            tIndex,  INTENT(IN) :: S
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_Poisson2(W, S) RESULT(ResVal)
!            IMPORT
            TYPE(FMassInfo), INTENT(IN) :: W
            tIndex,          INTENT(IN) :: S
            tDouble                     :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_Binomial1(N, PIn, SIn) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: PIn
            tIndex,  INTENT(IN) :: N, SIn
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_Binomial2(W, S) RESULT(ResVal)
!            IMPORT
            TYPE(FMassInfo), INTENT(IN) :: W
            tIndex,          INTENT(IN) :: S
            tDouble                     :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_NegaBin1(N, P, S) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: P
            tIndex,  INTENT(IN) :: N, S
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_NegaBin2(W, S) RESULT(ResVal)
!            IMPORT
            TYPE(FMassInfo), INTENT(IN) :: W
            tIndex,          INTENT(IN) :: S
            tDouble                     :: ResVal
        END FUNCTION
        MODULE FUNCTION fdist_Scan(N, D, M) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: D
            tIndex,  INTENT(IN) :: N, M
            tDouble             :: ResVal
        END FUNCTION
    END INTERFACE
    ! fbar procedures
    INTERFACE
        MODULE FUNCTION fbar_Unif(X) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fbar_Expon(X) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fbar_Weibull(Alpha, X) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: Alpha, X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fbar_Logistic(X) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fbar_Pareto(C, X) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: C, X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fbar_Normal1(X) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fbar_Normal2(X) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fbar_Normal3(X) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fbar_BiNormal1(X, Y, Rho, NDig) RESULT(ResVal)
!            IMPORT
            tDouble,  INTENT(IN)    :: X, Y, Rho
            tInteger, INTENT(IN)    :: NDig
            tDouble                 :: ResVal
        END FUNCTION
        MODULE FUNCTION fbar_BiNormal2(X, Y, Rho) RESULT(ResVal)
!            IMPORT
            tDouble,  INTENT(IN)    :: X, Y, Rho
            tDouble                 :: ResVal
        END FUNCTION
        MODULE FUNCTION fbar_LogNormal(Mu, Sigma, X) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: Mu, Sigma, X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fbar_JohnsonSB(Alpha, Beta, A, B, X) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: Alpha, Beta, A, B, X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fbar_JohnsonSU(Alpha, Beta, X) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: Alpha, Beta, X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fbar_ChiSquare1(K, X) RESULT(ResVal)
!            IMPORT
            tIndex,  INTENT(IN) :: K
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fbar_ChiSquare2(K, D, X) RESULT(ResVal)
!            IMPORT
            tIndex,   INTENT(IN)    :: K
            tInteger, INTENT(IN)    :: D
            tDouble,  INTENT(IN)    :: X
            tDouble                 :: ResVal
        END FUNCTION
        MODULE FUNCTION fbar_Gamma(Alpha, D, X) RESULT(ResVal)
!            IMPORT
            tDouble,  INTENT(IN)    :: Alpha, X
            tInteger, INTENT(IN)    :: D
            tDouble                 :: ResVal
        END FUNCTION
        MODULE FUNCTION fbar_KSPlus(N, X) RESULT(ResVal)
!            IMPORT
            tIndex,  INTENT(IN) :: N
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fbar_KS1(N, X) RESULT(ResVal)
!            IMPORT
            tIndex,  INTENT(IN) :: N
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fbar_CramerMises(N, X) RESULT(ResVal)
!            IMPORT
            tIndex,  INTENT(IN) :: N
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fbar_WatsonG(N, X) RESULT(ResVal)
!            IMPORT
            tIndex,  INTENT(IN) :: N
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fbar_WatsonU(N, X) RESULT(ResVal)
!            IMPORT
            tIndex,  INTENT(IN) :: N
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fbar_AndersonDarling(N, X) RESULT(ResVal)
!            IMPORT
            tIndex,  INTENT(IN) :: N
            tDouble, INTENT(IN) :: X
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fbar_Geometric(P, S) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: P
            tIndex,  INTENT(IN) :: S
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fbar_Poisson1(Lambda, S) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: Lambda
            tIndex,  INTENT(IN) :: S
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION fbar_Poisson2(W, S) RESULT(ResVal)
!            IMPORT
            TYPE(FMassInfo), INTENT(IN) :: W
            tIndex,          INTENT(IN) :: S
            tDouble                     :: ResVal
        END FUNCTION
        MODULE FUNCTION fbar_Binomial2(W, S) RESULT(ResVal)
!            IMPORT
            TYPE(FMassInfo), INTENT(IN) :: W
            tIndex,          INTENT(IN) :: S
            tDouble                     :: ResVal
        END FUNCTION
        MODULE FUNCTION fbar_NegaBin2(W, S) RESULT(ResVal)
!            IMPORT
            TYPE(FMassInfo), INTENT(IN) :: W
            tIndex,          INTENT(IN) :: S
            tDouble                     :: ResVal
        END FUNCTION
        MODULE FUNCTION fbar_Scan(N, D, M) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: D
            tIndex,  INTENT(IN) :: N, M
            tDouble             :: ResVal
        END FUNCTION
    END INTERFACE
    ! finv procedures
    INTERFACE
        MODULE FUNCTION finv_Expon(U) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: U
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION finv_Weibull(Alpha, U) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: Alpha, U
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION finv_ExtremeValue(U) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: U
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION finv_Logistic(U) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: U
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION finv_Pareto(C, U) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: C, U
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION finv_Normal1(U) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: U
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION finv_Normal2(U) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: U
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION finv_Normal3(U) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: U
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION finv_LogNormal(Mu, Sigma, U) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: Mu, Sigma, U
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION finv_JohnsonSB(Alpha, Beta, A, B, U) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: Alpha, Beta, A, B, U
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION finv_JohnsonSU(Alpha, Beta, U) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: Alpha, Beta, U
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION finv_ChiSquare1(K, U) RESULT(ResVal)
!            IMPORT
            tIndex,  INTENT(IN) :: K
            tDouble, INTENT(IN) :: U
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION finv_ChiSquare2(K, U) RESULT(ResVal)
!            IMPORT
            tIndex,   INTENT(IN)    :: K
            tDouble,  INTENT(IN)    :: U
            tDouble                 :: ResVal
        END FUNCTION
        MODULE FUNCTION finv_Student(N, U) RESULT(ResVal)
!            IMPORT
            tIndex,  INTENT(IN) :: N
            tDouble, INTENT(IN) :: U
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION finv_BetaSymmetric(P, U) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: P, U
            tDouble             :: ResVal
        END FUNCTION
        MODULE FUNCTION finv_Geometric(P, U) RESULT(ResVal)
!            IMPORT
            tDouble, INTENT(IN) :: P, U
            tLong               :: ResVal
        END FUNCTION
        MODULE FUNCTION finv_GenericD(W, U) RESULT(ResVal)
!            IMPORT
            TYPE(FMassInfo), TARGET, INTENT(IN) :: W
            tDouble,                 INTENT(IN) :: U
            tLong                               :: ResVal
        END FUNCTION
        MODULE FUNCTION finv_GenericC(F, Par, U, D) RESULT(ResVal)
!            IMPORT
            PROCEDURE(wdist_CFUNC)  :: F
            tDouble,  INTENT(IN)    :: Par(0:)
            tDouble,  INTENT(IN)    :: U
            tInteger, INTENT(IN)    :: D
            tLong                   :: ResVal
        END FUNCTION
    END INTERFACE

!** GENERIC DECLARATIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):


!******************************************************************************

END MODULE ModBase_ProbDist
