
SUBMODULE (ModBase_ProbDist) SubBase_FMass

!** PURPOSE OF THIS SUBMODULE:
    ! This submodule provides functions for computing the probability terms (or mass function)
    ! for some standard discrete distributions.
    !
    ! For certain distributions (e.g., the Poisson, binomial, and negative binomial), one can
    ! either recompute a probability term each time it is needed, or precompute tables that
    ! contain the probability terms and the distribution function, and then use these tables
    ! whenever a value is needed.  The latter trades memory for speed and is recommended
    ! especially if the distribution function has to be computed several times for the same
    ! parameter(s).  We describe how this works for the Poisson distribution.  Things work
    ! similarly for the other distributions.
    !
    ! To compute a single Poisson probability from scratch, simply use "fmass_PoissonTerm1".
    ! To precompute tables, one must first call "fmassCreatePoisson" with the desired parameter
    ! value 'Lambda' of the Poisson distribution.  This will precompute and store the
    ! non-negligible probability terms 'f(s)' (those that exceed 'fmass_Epsilon') in a table,
    ! and the cumulative distribution function
    !
    !           F(x) = SUM(f(s)) for s = 0 to x
    !
    ! for the corresponding values of 'x' in a second table.  In fact, that second table will
    ! contain F(x) when F(x) <= 1/2 and 1-F(x) when F(x) > 1/2.  These tables are kept in a
    ! structure of type 'FMassInfo' which can be deleted by calling "FreeFMassInfo".   Any value
    ! of the mass, distribution, complementary distribution, or inverse distribution function
    ! can be obtained from this structure by calling "fmass_Poisson2", "fdist_Poisson2",
    ! "fbar_Poisson2", or "finv_Poisson2", respectively.  As a rule of thumb, creating tables
    ! and using "fdist_Poisson2" is faster than just using "fdist_Poisson1" as soon as two or
    ! three calls are made to this function, unless 'Lambda' is large.  (If 'Lambda' is very
    ! large, i.e., exceeds 'fmass_MaxLambdaPoisson', the tables are not created because they
    ! would take too much space, and the functions with suffix_Poisson2 automatically call
    ! those with suffix_Poisson1 instead.)

!** USE STATEMENTS:
    USE ModBase_Error_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! name of submodule
    tCharStar, PARAMETER    :: SubModName = 'SubBase_FMass'
    ! When we precompute probability terms until terms are smaller than
    !   fmass_Epsilon, the last few terms will not be very precise. Instead we
    !   add terms as small as fmass_Epsilon * EPS_EXTRA to get a few correct digits
    !   at the tails of the precomputed distributions.
    tDouble,   PARAMETER    :: EPS_EXTRA = ONE_DBL / 100.0_kDouble
    ! To avoid overflow
    tDouble,   PARAMETER    :: MaxExp = (DBL_MAX_EXP - 1) * num_Ln2
    tDouble,   PARAMETER    :: MinExp = (DBL_MIN_EXP - 1) * num_Ln2

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS
    ! na

!** GENERIC DECLARATIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE SUBROUTINE FreeFMassInfo(W)

!** PURPOSE OF THIS SUBROUTINE:
    ! To free memory allocated by the structure 'FMassInfo'.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FMassInfo), INTENT(INOUT)  :: W

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MemFree(W%PDF)
    CALL MemFree(W%CDF)
    CALL MemFree(W%ParamR)
    CALL MemFree(W%ParamI)

    RETURN

END SUBROUTINE FreeFMassInfo

!******************************************************************************

MODULE FUNCTION fmass_PoissonTerm1(Lambda, S) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute and return the Poisson probability for the specified Lambda

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: Lambda
    tIndex,  INTENT(IN) :: S
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble, PARAMETER  :: Lambda_Limit = 20.0_kDouble

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble :: X, Y

! FLOW

    IF (S < 0_kLong) THEN
        ResVal = ZERO_DBL
        RETURN
    END IF

    X = S
    IF ((Lambda < Lambda_Limit).AND.(X < TWO_DBL * Lambda_Limit)) THEN
        ResVal = EXP(-Lambda) * (Lambda**X) / num2_Factorial(ToInteger(S))
    ELSE
        Y = X * LOG(Lambda) - num2_LnGamma(X + ONE_DBL) - Lambda
        ResVal = EXP(Y)
    END IF

    RETURN

END FUNCTION fmass_PoissonTerm1

!******************************************************************************

MODULE FUNCTION fmass_PoissonTerm2(W, S) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute and return the Poisson probability for the specified structure W

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FMassInfo), INTENT(IN) :: W
    tIndex,          INTENT(IN) :: S
    tDouble                     :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble :: Lambda

! FLOW

    IF (S < 0_kLong) THEN
        ResVal = ZERO_DBL
        RETURN
    END IF

    IF (.NOT.ALLOCATED(W%ParamR)) THEN
        CALL Handle_ErrLevel('fmass_PoissonTerm2', SubModName, ErrWarning, &
                          'FMassInfo has not yet been created.')
        RETURN
    END IF

    Lambda = W%ParamR(0)

    IF (.NOT.ALLOCATED(W%PDF)) THEN
        ResVal = fmass_PoissonTerm1(Lambda, S)
    ELSEIF ((S > W%SMax).OR.(S < W%SMin)) THEN
        ResVal = fmass_PoissonTerm1(Lambda, S)
    ELSE
        ResVal = W%PDF(S - W%SMin)
    END IF

    RETURN

END FUNCTION fmass_PoissonTerm2

!******************************************************************************

MODULE SUBROUTINE fmass_CreatePoisson(Lambda, W)

!** PURPOSE OF THIS SUBROUTINE:
    ! To create and return a structure that contains the mass and distribution
    ! functions for the Poisson distribution with the specified parameter Lambda,
    ! which are computed and stored in dynamic arrays inside that structure.
    ! Such a structure is needed for calling fmass_PoissonTerm2, fdist_Poisson2,
    ! fbar_Poisson2, or finv_Poisson2. It can be deleted by calling the procedure
    ! FreeFMassInfo.  Restriction: Lambda > 0.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble,         INTENT(IN)     :: Lambda
    TYPE(FMassInfo), INTENT(OUT)    :: W

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble, ALLOCATABLE    :: PDF(:)   ! Poisson probability terms
    tDouble, ALLOCATABLE    :: CDF(:)   ! Poisson cumulative probabilities
    tDouble                 :: Eps, Sum
    tIndex                  :: Mid
    tIndex                  :: NMax, I, IMin, IMax

! FLOW

    IF (Lambda < 0) THEN
        CALL Handle_ErrLevel('fmass_CreatePoisson', SubModName, ErrWarning, &
                          'Invalid Lambda: Lambda must be greater than or equal to zero.')
        RETURN
    END IF

    CALL MemAlloc(W%ParamR, 1_kIndex, StartID=0_kIndex)
    W%ParamR(0) = Lambda

    ! For Lambda > fmass_MaxLambdaPoisson, we do not use pre-computed arrays
    IF (Lambda > fmass_MaxLambdaPoisson) THEN
        RETURN
    END IF

    ! In theory, the Poisson distribution has an infinite range. But
    ! for i > NMax, probabilities should be extremely small.
    NMax = ToLong(Lambda + 16 * (2 + SQRT (Lambda)))
    CALL MemAlloc(PDF, NMax + 1_kIndex, StartID=0_kIndex)
    CALL MemAlloc(CDF, NMax + 1_kIndex, StartID=0_kIndex)

    Mid = ToLong(Lambda)
    Eps = EPS_EXTRA * fmass_Epsilon / fmass_PoissonTerm1 (Lambda, Mid)
    ! For large Lambda, fmass_PoissonTerm1 will lose a few digits of precision
    ! We shall normalize by explicitly summing all terms >= Eps
    PDF(Mid) = ONE_DBL
    Sum = PDF(Mid)

    ! Start from the maximum and compute terms > Eps on each side.
    I = Mid
    DO WHILE ((I > 0).AND.(PDF(I) > Eps))
        PDF(I - 1) = PDF(I) * I / Lambda
        I = I - 1
        Sum = Sum + PDF(I)
    END DO
    IMin = I
    W%SMin = IMin

    I = Mid
    DO WHILE (PDF(I) > Eps)
        PDF(I + 1) = PDF(I) * Lambda / (I + 1)
        I = I + 1
        Sum = Sum + PDF(I)
        IF (I >= NMax - 1) THEN
            NMax = NMax * 2
            CALL MemResize(PDF, NMax + 1_kIndex)
            CALL MemResize(CDF, NMax + 1_kIndex)
        END IF
    END DO
    IMax = I
    W%SMax = IMax

    ! Renormalize the sum of probabilities to 1
    DO I = IMin, IMax
        PDF(I) = PDF(I) /Sum
    END DO

    ! Compute the cumulative probabilities until CDF >= 0.5, and keep them in
    ! the lower part of array, i.e. CDF(s) contains all PDF(i) for i <= s
    CDF(IMin) = PDF(IMin)
    I = IMin
    DO WHILE ((I < IMax).AND.(CDF(I) < 0.5_kDouble))
        I = I + 1
        CDF(I) = PDF(I) + CDF(I - 1)
    END DO
    ! This is the boundary between F and 1 - F in the CDF
    W%SMed = I

    ! Compute the cumulative probabilities of the complementary distribution
    !   and keep them in the upper part of the array. i.e. CDF(s) contains all
    !   PDF(i) for i >= s
    CDF(IMax) = PDF(IMax)
    I = IMax - 1
    DO WHILE (I > W%SMed)
        CDF(I) = PDF(I) + CDF(I + 1)
        I = I - 1
    END DO

    ! Reset IMin because we lose too much precision for a few terms near
    !  IMin when we stop adding terms < Eps.
    I = IMin
    DO WHILE ((I < W%SMed).AND.(CDF(I) < fmass_Epsilon))
        I = I + 1
    END DO
    IMin = I
    W%SMin = IMin

    ! Same thing with IMax
    I = IMax
    DO WHILE ((I > W%SMed).AND.(CDF(I) < fmass_Epsilon))
        I = I - 1
    END DO
    IMax = I
    W%SMax = IMax

    CALL MemAlloc(W%PDF, IMax + 1_kIndex - IMin, StartID=0_kIndex)
    CALL MemAlloc(W%CDF, IMax + 1_kIndex - IMin, StartID=0_kIndex)
    DO I = IMin, IMax
        W%PDF(I - Imin) = PDF(I)
        W%CDF(I - Imin) = CDF(I)
    END DO
    CALL MemFree(PDF)
    CALL MemFree(CDF)

    RETURN

END SUBROUTINE fmass_CreatePoisson

!******************************************************************************

MODULE FUNCTION fmass_BinomialTerm1(N, PIn, QIn, SIn) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute and return the binomial term where P and Q are arbitrary real numbers.
    ! In the case where 0 <= P <= 1 and Q = 1-P, the returned value is a probability term
    ! for the binomial distribution.  Restriction:  0 <= S <= N.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,  INTENT(IN) :: N, SIn
    tDouble, INTENT(IN) :: PIn, QIn
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tIndex,   PARAMETER :: SLimit = 30      ! To avoid overflow

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: SignE
    tDouble     :: Res, P, Q
    tIndex      :: S

! FLOW

    ! initialize
    SignE = 1
    S = SIn
    P = PIn
    Q = QIn

    IF (N < 0) THEN
        CALL Handle_ErrLevel('fmass_BinomialTerm1', SubModName, ErrWarning, 'Invalid N: N < 0.')
        RETURN
    END IF
    IF (N == 0) THEN
        ResVal = ONE_DBL
        RETURN
    END IF
    IF ((S < 0).OR.(S > N)) THEN
        ResVal = ZERO_DBL
        RETURN
    END IF

    ! Combination(n, s) are symmetric between s and n-s
    IF (S > N / 2) THEN
        S = N - S
        Res = P
        P = Q
        Q = Res
    END IF

    IF (P < ZERO_DBL) THEN
        P = -P
        IF (IAND(S, 1_kIndex) /= 0_kIndex) SignE = SignE*(-1)     ! odd s
    END IF
    IF (Q < ZERO_DBL) THEN
        Q = -Q
        IF (IAND(N-S, 1_kIndex) /= 0_kIndex) SignE = SignE*(-1)   ! odd n - s
    END IF

    IF (N <= SLimit) THEN
        Res = (P**S) * num2_Combination(ToInteger(N), ToInteger(S)) * (Q**(N - S))
        ResVal = SignE * Res
    ELSE
        ! This could be calculated with more precision as there is some
        ! cancellation because of subtraction of the large LnFactorial: the
        ! last few digits can be lost. But we need the function lgammal in
        ! long double precision. Another possibility would be to use an
        ! asymptotic expansion for the binomial coefficient.
        Res = S*LOG(P) + (N - S)*LOG(Q) + num2_LnFactorial(ToInteger(N)) &
            - num2_LnFactorial(ToInteger(N - S)) - num2_LnFactorial(ToInteger(S))

        IF (Res > MaxExp) THEN
            CALL Handle_ErrLevel('fmass_BinomialTerm1', SubModName, ErrSevere, 'Term Overflow')
            RETURN
        END IF

        IF (Res < MinExp) THEN
            ResVal = ZERO_DBL
        ELSE
            ResVal = SignE * EXP(Res)
        END IF
    END IF

    RETURN

END FUNCTION fmass_BinomialTerm1

!******************************************************************************

MODULE FUNCTION fmass_BinomialTerm2(W, S) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute and return the binomial term for the specified structure W

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FMassInfo), INTENT(IN) :: W
    tIndex,          INTENT(IN) :: S
    tDouble                     :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: P, Q
    tIndex      :: N

! FLOW

    IF ((.NOT.ALLOCATED(W%ParamI)).OR.(.NOT.ALLOCATED(W%ParamR))) THEN
        CALL Handle_ErrLevel('fmass_BinomialTerm2', SubModName, ErrWarning, &
                          'FMassInfo has not yet been created.')
        RETURN
    END IF

    N = W%ParamI(0)
    IF (N == 0) THEN
        ResVal = ONE_DBL
        RETURN
    END IF
    IF ((S < 0).OR.(S > N)) THEN
        ResVal = ZERO_DBL
        RETURN
    END IF
    P = W%ParamR(0)
    IF (P == ZERO_DBL) THEN
        IF (S > 0) THEN
            ResVal = ZERO_DBL
            RETURN
        ELSE
            ResVal = ONE_DBL
            RETURN
        END IF
    END IF
    Q = W%ParamR(1)
    IF (Q == ZERO_DBL) THEN
        IF (S < N) THEN
            ResVal = ZERO_DBL
            RETURN
        ELSE
            ResVal = ONE_DBL
            RETURN
        END IF
    END IF
    IF (.NOT.ALLOCATED(W%PDF)) THEN
        ResVal = fmass_BinomialTerm1(N, P, Q, S)
    ELSEIF ((S > W%SMax).OR.(S < W%SMin)) THEN
        ResVal = fmass_BinomialTerm1(N, P, Q, S)
    ELSE
        ResVal = W%PDF(S - W%SMin)
    END IF

    RETURN

END FUNCTION fmass_BinomialTerm2

!******************************************************************************

MODULE FUNCTION fmass_BinomialTerm3(N, PIn, SIn) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute and return the binomial term where P is an arbitrary real number.
    ! In the case where 0 <= P <= 1, the returned value is a probability term
    ! for the binomial distribution.  Restriction:  0 <= S <= N.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,  INTENT(IN) :: N, SIn
    tDouble, INTENT(IN) :: PIn
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tIndex,   PARAMETER :: SLimit = 50      ! To avoid overflow

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: SignE
    tDouble     :: Res, P, Q, Temp
    tIndex      :: S

! FLOW

    ! initialize
    SignE = 1
    S = SIn
    P = PIn
    Q = ONE_DBL - P

    IF (N < 0) THEN
        CALL Handle_ErrLevel('fmass_BinomialTerm3', SubModName, ErrWarning, 'Invalid N: N < 0.')
        RETURN
    END IF
    IF (N == 0) THEN
        ResVal = ONE_DBL
        RETURN
    END IF
    IF ((S < 0).OR.(S > N)) THEN
        ResVal = ZERO_DBL
        RETURN
    END IF

    ! Combination(n, s) are symmetric between s and n-s
    IF (S > N / 2) THEN
        S = N - S
        Res = P
        P = Q
        Q = Res
    END IF

    IF (P < ZERO_DBL) THEN
        P = -P
        IF (IAND(S, 1_kIndex) /= 0_kIndex) SignE = SignE*(-1)     ! odd s
    END IF
    IF (Q < ZERO_DBL) THEN
        Q = -Q
        IF (IAND(N-S, 1_kIndex) /= 0_kIndex) SignE = SignE*(-1)   ! odd n - s
    END IF

    IF (N <= SLimit) THEN
        IF (P > 1.0D-1) THEN
            Res = (P**S) * num2_Combination(ToInteger(N), ToInteger(S)) * (Q**(N - S))
        ELSE
            Temp = (N - S) * num2_LOG1P(-P)
            Res = (P**S) * num2_Combination(ToInteger(N), ToInteger(S)) * EXP(Temp)
        END IF
        ResVal = SignE * Res
    ELSE
        ! This could be calculated with more precision as there is some
        ! cancellation because of subtraction of the large LnFactorial: the
        ! last few digits can be lost. But we need the function lgammal in
        ! long double precision. Another possibility would be to use an
        ! asymptotic expansion for the binomial coefficient.
        Res = S*LOG(P) + (N - S)*num2_LOG1P(-P) + num2_LnFactorial(ToInteger(N)) &
            - num2_LnFactorial(ToInteger(N - S)) - num2_LnFactorial(ToInteger(S))

        IF (Res > MaxExp) THEN
            CALL Handle_ErrLevel('fmass_BinomialTerm3', SubModName, ErrSevere, 'Term Overflow')
            RETURN
        END IF

        IF (Res < MinExp) THEN
            ResVal = ZERO_DBL
        ELSE
            ResVal = SignE * EXP(Res)
        END IF
    END IF

    RETURN

END FUNCTION fmass_BinomialTerm3

!******************************************************************************

MODULE FUNCTION fmass_BinomialTerm4(N, P, P2, S) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute and return the binomial term where P and P2 are real number in [0, 1].
    ! In the case where P2 = P, the returned value is a probability term for the
    ! binomial distribution.  If P2 is small, this function is more precise than
    ! 'fmass_BinomialTerm1'.  Restriction:  0 <= S <= N.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,  INTENT(IN) :: N, S
    tDouble, INTENT(IN) :: P, P2
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tIndex,   PARAMETER :: SLimit = 30      ! To avoid overflow

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: Res, Temp

! FLOW

    IF ((P >= ZERO_DBL).AND.(P <= ONE_DBL)) THEN
        CALL Handle_ErrLevel('fmass_BinomialTerm4', SubModName, ErrWarning, 'P Not In [0, 1].')
        RETURN
    END IF
    IF ((P2 >= ZERO_DBL).AND.(P2 <= ONE_DBL)) THEN
        CALL Handle_ErrLevel('fmass_BinomialTerm4', SubModName, ErrWarning, 'P2 Not In [0, 1].')
        RETURN
    END IF
    IF (N < 0) THEN
        CALL Handle_ErrLevel('fmass_BinomialTerm4', SubModName, ErrWarning, 'Invalid N: N < 0.')
        RETURN
    END IF
    IF (N == 0) THEN
        ResVal = ONE_DBL
        RETURN
    END IF
    IF ((S < 0).OR.(S > N)) THEN
        ResVal = ZERO_DBL
        RETURN
    END IF

    IF (N <= SLimit) THEN
        IF (P2 > 1.0D-1) THEN
            Res = (P**S) * num2_Combination(ToInteger(N), ToInteger(S)) * &
                  ((ONE_DBL-P2)**(N - S))
        ELSE
            Temp = (N - S) * num2_LOG1P(-P2)
            Res = (P**S) * num2_Combination(ToInteger(N), ToInteger(S)) * EXP(Temp)
        END IF
        ResVal = Res
    ELSE
        ! This could be calculated with more precision as there is some
        ! cancellation because of subtraction of the large LnFactorial: the
        ! last few digits can be lost. But we need the function lgammal in
        ! long double precision. Another possibility would be to use an
        ! asymptotic expansion for the binomial coefficient.
        Res = S*LOG(P) + (N - S)*num2_LOG1P(-P2) + num2_LnFactorial(ToInteger(N)) &
            - num2_LnFactorial(ToInteger(N - S)) - num2_LnFactorial(ToInteger(S))

        IF (Res > MaxExp) THEN
            CALL Handle_ErrLevel('fmass_BinomialTerm4', SubModName, ErrSevere, 'Term Overflow')
            RETURN
        END IF

        IF (Res < MinExp) THEN
            ResVal = ZERO_DBL
        ELSE
            ResVal = EXP(Res)
        END IF
    END IF

    RETURN

END FUNCTION fmass_BinomialTerm4

!******************************************************************************

MODULE SUBROUTINE fmass_CreateBinomial(N, P, Q, W)

!** PURPOSE OF THIS SUBROUTINE:
    ! To create and return a structure that contains binomial terms for 0 <= S <= N,
    ! and the corresponding cumulative function. If 0 <= P = 1-Q <= 1, these are
    ! the probabilities and the distribution function of a binomial random variable.
    ! The values are computed and stored in dynamic arrays. Such a structure is needed
    ! for calling fmass_BinomialTerm2, fdist_Binomial2, fbar_Binomial2, or finv_Binomial2.
    ! It can be deleted by calling FreeFMassInfo.  This function is more general than the
    ! binomial probability distribution as it computes the binomial terms when p + q /= 1,
    ! and even when p or q are negative. However in this case, the cumulative terms will
    ! be meaningless and only the mass terms fmass_BinomialTerm2 are computed.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,          INTENT(IN)     :: N
    tDouble,         INTENT(IN)     :: P, Q
    TYPE(FMassInfo), INTENT(OUT)    :: W

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble, PARAMETER  :: Eps = fmass_Epsilon * EPS_EXTRA

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble, ALLOCATABLE    :: PDF(:)   ! Binomial "probability" terms
    tDouble, ALLOCATABLE    :: CDF(:)   ! Binomial cumulative "probabilities"
    tDouble                 :: Z
    tIndex                  :: Mid
    tIndex                  :: I, IMin, IMax

! FLOW

    ! Compute all probability terms of the binomial distribution; start near
    ! the mean, and calculate probabilities on each side until they become
    ! smaller than epsilon, then stop there.
    ! However, this is more general than the binomial probability distribution
    ! as this will compute the binomial terms when p + q /= 1, and even
    ! when p or q are negative. However in this case, the cumulative terms
    ! are meaningless and are not computed.

    Z = ZERO_DBL

    IF (N <= 0) THEN
        CALL Handle_ErrLevel('fmass_CreateBinomial', SubModName, ErrWarning, &
                          'Invalid N: N must be greater than zero.')
        RETURN
    END IF

    CALL MemAlloc(W%ParamI, 1_kIndex, StartID=0_kIndex)
    CALL MemAlloc(W%ParamR, 2_kIndex, StartID=0_kIndex)
    W%ParamI(0) = N
    W%ParamR(0) = P
    W%ParamR(1) = Q

    ! For N > fmass_MaxnBinomial, we shall not use pre-computed arrays
    IF (N > fmass_MaxnBinomial) THEN
        RETURN
    END IF

    CALL MemAlloc(PDF, N + 1_kIndex, StartID=0_kIndex)
    CALL MemAlloc(CDF, N + 1_kIndex, StartID=0_kIndex)

    ! the maximum term in absolute value
    Mid = ToLong(((N + 1) * ABS(P) / (ABS(P) + ABS(Q))))
    IF (Mid > N) Mid = N
    PDF(Mid) = fmass_BinomialTerm1(N, P, Q, Mid)

    IF (ABS(P) > ZERO_DBL) THEN
        Z = Q / P
    ELSE
        Z = ZERO_DBL
        CALL Handle_ErrLevel('fmass_CreateBinomial', SubModName, ErrWarning, 'Q/P = Infinite')
    END IF

    I = Mid
    DO WHILE ((I > 0).AND.(ABS(PDF(I)) > Eps))
        PDF(I - 1) = PDF(I) * Z * I / (N - I + 1)
        I = I - 1
    END DO
    IMin = I

    IF (ABS(Q) > ZERO_DBL) THEN
        Z = P / Q
    ELSE
        Z = ZERO_DBL
        CALL Handle_ErrLevel('fmass_CreateBinomial', SubModName, ErrWarning, 'P/Q = Infinite')
    END IF

    I = Mid
    DO WHILE ((I < N).AND.(ABS(PDF(I)) > Eps))
        PDF(I + 1) = PDF(I) * Z * (N - I) / (I + 1)
        I = I + 1
    END DO
    IMax = I


    ! Here, we assume that we are dealing with a probability distribution.
    ! Compute the cumulative probabilities for F and keep them in the lower part of CDF.
    CDF(IMin) = PDF(IMin)
    I = IMin
    DO WHILE ((I < N).AND.(CDF(I) < 0.5_kDouble))
        I = I + 1
        CDF(I) = CDF(I - 1) + PDF(I)
    END DO

    ! This is the boundary between F (i <= smed) and 1 - F (i > smed) in the array CDF
    W%SMed = I

    ! Compute the cumulative probabilities of the complementary
    ! distribution and keep them in the upper part of the array
    CDF(IMax) = PDF(IMax)
    I = IMax - 1
    DO WHILE (I > W%SMed)
        CDF(I) = PDF(I) + CDF(I + 1)
        I = I - 1
    END DO

    ! Reset IMin because we lose too much precision for a few terms near
    !  IMin when we stop adding terms < Eps.
    I = IMin
    DO WHILE ((I < W%SMed).AND.(CDF(I) < fmass_Epsilon))
        I = I + 1
    END DO
    IMin = I
    W%SMin = IMin

    ! Same thing with IMax
    I = IMax
    DO WHILE ((I > W%SMed).AND.(CDF(I) < fmass_Epsilon))
        I = I - 1
    END DO
    IMax = I
    W%SMax = IMax

    CALL MemAlloc(W%PDF, IMax + 1_kIndex - IMin, StartID=0_kIndex)
    CALL MemAlloc(W%CDF, IMax + 1_kIndex - IMin, StartID=0_kIndex)
    DO I = IMin, IMax
        W%PDF(I - Imin) = PDF(I)
        W%CDF(I - Imin) = CDF(I)
    END DO
    CALL MemFree(PDF)
    CALL MemFree(CDF)

    RETURN

END SUBROUTINE fmass_CreateBinomial

!******************************************************************************

MODULE FUNCTION fmass_NegaBinTerm1(N, P, S) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute and return the negative binomial term which can be interpreted as
    ! the probability of having S failures before the Nth success in a sequence of
    ! independent Bernoulli trials with success probability P.  Restriction:  N > 0,
    ! 0 <= P <= 1, S >= 0.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,  INTENT(IN) :: N, S
    tDouble, INTENT(IN) :: P
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tIndex,   PARAMETER :: SLimit = 15      ! To avoid overflow

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: Res, Y

! FLOW

    IF ((P >= ZERO_DBL).AND.(P <= ONE_DBL)) THEN
        CALL Handle_ErrLevel('fmass_NegaBinTerm1', SubModName, ErrWarning, 'P Not In [0, 1].')
        RETURN
    END IF
    IF (N <= 0) THEN
        CALL Handle_ErrLevel('fmass_NegaBinTerm1', SubModName, ErrWarning, 'Invalid N: N <= 0.')
        RETURN
    END IF

    IF (S < 0) THEN
        ResVal = ZERO_DBL
        RETURN
    END IF
    IF (P >= ONE_DBL) THEN  ! In fact, p == 1
        IF (S == 0) THEN
            ResVal = ONE_DBL
        ELSE
            ResVal = ZERO_DBL
        END IF
        RETURN
    END IF
    IF (P <= 0) THEN            ! In fact, p == 0
        ResVal = ZERO_DBL
        RETURN
    END IF

    IF ((S <= SLimit).OR.(N <= SLimit)) THEN
        ResVal = (P**N) * num2_Combination(ToInteger(N + S - 1), ToInteger(S)) * &
                 ((ONE_DBL - P)**S)
    ELSE
        Y = S*num2_LOG1P(-P) + N*LOG(P) + num2_LnFactorial(ToInteger(N + S - 1)) &
          - num2_LnFactorial(ToInteger(N - 1)) - num2_LnFactorial(ToInteger(S))

        IF (Y >= MaxExp) THEN
            CALL Handle_ErrLevel('fmass_NegaBinTerm1', SubModName, ErrSevere, 'Term Overflow')
            RETURN
        END IF

        IF (Res < MinExp) THEN
            ResVal = ZERO_DBL
        ELSE
            ResVal = EXP(Y)
        END IF
    END IF

    RETURN

END FUNCTION fmass_NegaBinTerm1

!******************************************************************************

MODULE FUNCTION fmass_NegaBinTerm2(W, S) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute and return the negative binomial term for the specified structure W

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FMassInfo), INTENT(IN) :: W
    tIndex,          INTENT(IN) :: S
    tDouble                     :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: P
    tIndex      :: N

! FLOW

    IF (S < 0_kLong) THEN
        ResVal = ZERO_DBL
        RETURN
    END IF

    IF ((.NOT.ALLOCATED(W%ParamI)).OR.(.NOT.ALLOCATED(W%ParamR))) THEN
        CALL Handle_ErrLevel('fmass_NegaBinTerm2', SubModName, ErrWarning, &
                          'FMassInfo has not yet been created.')
        RETURN
    END IF

    N = W%ParamI(0)
    P = W%ParamR(0)

    IF (P == ZERO_DBL) THEN
        ResVal = ZERO_DBL
        RETURN
    END IF
    IF (P == ONE_DBL) THEN
        IF (S > 0) THEN
            ResVal = ZERO_DBL
        ELSE
            ResVal = ONE_DBL
        END IF
        RETURN
    END IF

    IF (.NOT.ALLOCATED(W%PDF)) THEN
        ResVal = fmass_NegaBinTerm1(N, P, S)
    ELSEIF ((S > W%SMax).OR.(S < W%SMin)) THEN
        ResVal = fmass_NegaBinTerm1(N, P, S)
    ELSE
        ResVal = W%PDF(S - W%SMin)
    END IF

    RETURN

END FUNCTION fmass_NegaBinTerm2

!******************************************************************************

MODULE SUBROUTINE fmass_CreateNegaBin(N, P, W)

!** PURPOSE OF THIS SUBROUTINE:
    ! To create and return a structure that contains the probability terms and
    ! the distribution functions for the negative binomial distribution with
    ! parameter N and P.  Such a structure is needed for calling fmass_NegaBinTerm2,
    ! fdist_NegaBin2, fbar_NegaBin2, or finv_NegaBin2.  It can be deleted by calling
    ! FreeFMassInfo.  Restriction:  N > 0 and 0 <= P <= 1.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,          INTENT(IN)     :: N
    tDouble,         INTENT(IN)     :: P
    TYPE(FMassInfo), INTENT(OUT)    :: W

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble, ALLOCATABLE    :: PDF(:)   ! Negative Binomial mass probabilities
    tDouble, ALLOCATABLE    :: CDF(:)   ! Negative Binomial cumulative "probabilities"
    tDouble                 :: Eps, Sum
    tIndex                  :: Mode, NMax
    tIndex                  :: I, IMin, IMax

! FLOW

    ! Compute all probability terms of the negative binomial distribution;
    ! start at the mode, and calculate probabilities on each side until they
    ! become smaller than epsilon. Set all others to 0.

    IF ((P >= ZERO_DBL).AND.(P <= ONE_DBL)) THEN
        CALL Handle_ErrLevel('fmass_CreateNegaBin', SubModName, ErrWarning, 'P Not In [0, 1].')
        RETURN
    END IF
    IF (N <= 0) THEN
        CALL Handle_ErrLevel('fmass_CreateNegaBin', SubModName, ErrWarning, &
                          'Invalid N: N must be greater than zero.')
        RETURN
    END IF

    CALL MemAlloc(W%ParamI, 1_kIndex, StartID=0_kIndex)
    CALL MemAlloc(W%ParamR, 1_kIndex, StartID=0_kIndex)
    W%ParamI(0) = N
    W%ParamR(0) = P

    ! Compute the mode (at the maximum term)
    Mode = ToLong(ONE_DBL + (N * (ONE_DBL - P) - ONE_DBL) / P)

    ! For mode > fmass_MaxnNegaBin, we shall not use pre-computed arrays.
    ! mode < 0 should be impossible, unless overflow of long occur, in
    ! which case mode will be = LONG_MIN.
    IF ((Mode < 0).OR.(Mode > fmass_MaxnNegaBin)) THEN
        RETURN
    END IF

    ! In theory, the negative binomial distribution has an infinite range.
    ! But for i > NMax, probabilities should be extremely small.
    ! NMax = Mean + 16 * Standard deviation.
    NMax = ToLong(N * (ONE_DBL - P) / P + 16 * SQRT (N * (ONE_DBL - P) / (P * P)))
    IF (NMax < 32) NMax = 32
    CALL MemAlloc(PDF, NMax + 1_kIndex, StartID=0_kIndex)
    CALL MemAlloc(CDF, NMax + 1_kIndex, StartID=0_kIndex)

    Eps = fmass_Epsilon * EPS_EXTRA / fmass_NegaBinTerm1(N, P, Mode)

    ! We shall normalize by explicitly summing all terms >= epsilon
    PDF(Mode) = ONE_DBL
    Sum       = ONE_DBL

    ! Start from the maximum and compute terms > epsilon on each side.
    I = Mode
    DO WHILE ((I > 0).AND.(PDF(I) > Eps))
        PDF(I - 1) = PDF(I) * I / ((ONE_DBL - P) * (N + I - 1))
        I = I - 1
        Sum = Sum + PDF(I)
    END DO
    IMin = I

    I = Mode
    DO WHILE (PDF(I) > Eps)
        PDF(I + 1) = PDF(I) * (ONE_DBL - P) * (N + I) / (I + 1)
        I = I + 1
        Sum = Sum + PDF(I)
        IF (I == NMax - 1) THEN
            NMax = NMax * 2
            CALL MemResize(PDF, NMax + 1_kIndex)
            CALL MemResize(CDF, NMax + 1_kIndex)
        END IF
    END DO
    IMax = I

    ! Renormalize the sum of probabilities to 1
    DO I = IMin, IMax
        PDF(I) = PDF(I) /Sum
    END DO

    ! Compute the cumulative probabilities for CDF and keep them in the
    ! the lower part of CDF
    CDF(IMin) = PDF(IMin)
    I = IMin
    DO WHILE ((I < IMax).AND.(CDF(I) < 0.5_kDouble))
        I = I + 1
        CDF(I) = PDF(I) + CDF(I - 1)
    END DO

    ! This is the boundary between F (i <= smed) and 1 - F (i > smed) in the array CDF
    W%SMed = I

    ! Compute the cumulative probabilities of the complementary distribution 1 - F
    ! and keep them in the upper part of the array
    CDF(IMax) = PDF(IMax)
    I = IMax - 1
    DO WHILE (I > W%SMed)
        CDF(I) = PDF(I) + CDF(I + 1)
        I = I - 1
    END DO

    ! Reset IMin because we lose too much precision for a few terms near
    !  IMin when we stop adding terms < Eps.
    I = IMin
    DO WHILE ((I < W%SMed).AND.(CDF(I) < fmass_Epsilon))
        I = I + 1
    END DO
    IMin = I
    W%SMin = IMin

    ! Same thing with IMax
    I = IMax
    DO WHILE ((I > W%SMed).AND.(CDF(I) < fmass_Epsilon))
        I = I - 1
    END DO
    IMax = I
    W%SMax = IMax

    CALL MemAlloc(W%PDF, IMax + 1_kIndex - IMin, StartID=0_kIndex)
    CALL MemAlloc(W%CDF, IMax + 1_kIndex - IMin, StartID=0_kIndex)
    DO I = IMin, IMax
        W%PDF(I - Imin) = PDF(I)
        W%CDF(I - Imin) = CDF(I)
    END DO
    CALL MemFree(PDF)
    CALL MemFree(CDF)

    RETURN

END SUBROUTINE fmass_CreateNegaBin

!******************************************************************************

END SUBMODULE SubBase_FMass
