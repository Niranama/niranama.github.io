
MODULE ModBase_MyLib

!** PURPOSE OF THIS MODULE:
    ! contains utility procedures described in [1].

!** REFERENCES:
    ! [1] P. L’Ecuyer and R. Simard.  MyLib-C, "A Small Library of Basic Utilities in ANSI C".

!** USE STATEMENTS:
    USE, INTRINSIC :: IEEE_ARITHMETIC
    USE ModBase_Common
    USE ModBase_Error_Handlers
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! procedures
    PUBLIC :: num2_Factorial
    PUBLIC :: num2_LnFactorial
    PUBLIC :: num2_LnGamma
    PUBLIC :: num2_EvalCheby
    PUBLIC :: num2_Combination
    PUBLIC :: num2_LOG1P
    PUBLIC :: num2_BesselK025

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'ModBase_MyLib'
    ! common number parameters
    tDouble,   PARAMETER    :: ZERO_DBL    = 0.0_kDouble
    tDouble,   PARAMETER    :: ONE_DBL     = 1.0_kDouble
    tDouble,   PARAMETER    :: TWO_DBL     = 2.0_kDouble
    ! parameters of num2 submodule
    tDouble,   PARAMETER    :: DBL_EPSILON = EPSILON(ONE_DBL)
    tDouble,   PARAMETER    :: EPSTOL      = 1.0D-15
    tInteger,  PARAMETER    :: MAXI        = 50

!** DERIVED TYPE DEFINITIONS
    ! na
    
!** INTERFACE DEFINITIONS
    INTERFACE
    END INTERFACE

!** GENERIC DECLARATIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na
    
    CONTAINS    

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

FUNCTION num2_Factorial(N) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the value of the factorial of N (N!)

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: N
    tDouble                 :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! The factorials n! from n = 0 to n = 170
    tDouble,   PARAMETER    :: Factorials(0:170) = [                                          &
        1.0_kDouble, 1.0_kDouble, 2.0_kDouble, 6.0_kDouble, 24.0_kDouble, 120.0_kDouble,      &
        720.0_kDouble, 5040.0_kDouble, 40320.0_kDouble, 362880.0_kDouble, 3628800.0_kDouble,  &
        39916800.0_kDouble, 479001600.0_kDouble, 6227020800.0_kDouble, 87178291200.0_kDouble, &
        1307674368000.0_kDouble, 20922789888000.0_kDouble, 355687428096000.0_kDouble,         &
        6402373705728000.0_kDouble, 1.21645100408832D+17, 2.43290200817664D+18,               &
        5.109094217170944D+19, 1.124000727777608D+21, 2.585201673888498D+22,                  &
        6.204484017332394D+23, 1.551121004333099D+25, 4.032914611266057D+26,                  &
        1.088886945041835D+28, 3.048883446117138D+29, 8.841761993739701D+30,                  &
        2.65252859812191D+32,  8.222838654177922D+33, 2.631308369336935D+35,                  &
        8.683317618811886D+36, 2.952327990396041D+38, 1.033314796638614D+40,                  &
        3.719933267899012D+41, 1.376375309122634D+43, 5.23022617466601D+44,                   &
        2.039788208119744D+46, 8.159152832478977D+47, 3.34525266131638D+49,                   &
        1.40500611775288D+51,  6.041526306337383D+52, 2.658271574788449D+54,                  &
        1.196222208654802D+56, 5.502622159812088D+57, 2.586232415111682D+59,                  &
        1.241391559253607D+61, 6.082818640342675D+62, 3.041409320171338D+64,                  &
        1.551118753287382D+66, 8.065817517094388D+67, 4.274883284060025D+69,                  &
        2.308436973392414D+71, 1.269640335365828D+73, 7.109985878048635D+74,                  &
        4.052691950487722D+76, 2.350561331282879D+78, 1.386831185456899D+80,                  &
        8.320987112741392D+81, 5.075802138772248D+83, 3.146997326038794D+85,                  &
        1.98260831540444D+87,  1.268869321858842D+89, 8.247650592082472D+90,                  &
        5.443449390774431D+92, 3.647111091818868D+94, 2.480035542436831D+96,                  &
        1.711224524281413D+98, 1.197857166996989D+100, 8.504785885678622D+101,                &
        6.123445837688608D+103, 4.470115461512683D+105, 3.307885441519386D+107,               &
        2.480914081139539D+109, 1.88549470166605D+111,  1.451830920282858D+113,               &
        1.132428117820629D+115, 8.946182130782973D+116, 7.156945704626378D+118,               &
        5.797126020747366D+120, 4.75364333701284D+122,  3.945523969720657D+124,               &
        3.314240134565352D+126, 2.817104114380549D+128, 2.422709538367272D+130,               &
        2.107757298379527D+132, 1.854826422573984D+134, 1.650795516090845D+136,               &
        1.485715964481761D+138, 1.352001527678402D+140, 1.24384140546413D+142,                &
        1.156772507081641D+144, 1.087366156656742D+146, 1.032997848823905D+148,               &
        9.916779348709491D+149, 9.619275968248206D+151, 9.426890448883242D+153,               &
        9.33262154439441D+155,  9.33262154439441D+157,  9.425947759838354D+159,               &
        9.614466715035121D+161, 9.902900716486175D+163, 1.029901674514562D+166,               &
        1.08139675824029D+168,  1.146280563734708D+170, 1.226520203196137D+172,               &
        1.324641819451828D+174, 1.443859583202493D+176, 1.588245541522742D+178,               &
        1.762952551090244D+180, 1.974506857221073D+182, 2.231192748659812D+184,               &
        2.543559733472186D+186, 2.925093693493014D+188, 3.393108684451897D+190,               &
        3.969937160808719D+192, 4.684525849754288D+194, 5.574585761207603D+196,               &
        6.689502913449124D+198, 8.09429852527344D+200,  9.875044200833598D+202,               &
        1.214630436702532D+205, 1.50614174151114D+207,  1.882677176888925D+209,               &
        2.372173242880046D+211, 3.012660018457658D+213, 3.856204823625803D+215,               &
        4.974504222477285D+217, 6.466855489220472D+219, 8.471580690878817D+221,               &
        1.118248651196004D+224, 1.487270706090685D+226, 1.992942746161518D+228,               &
        2.69047270731805D+230,  3.659042881952547D+232, 5.01288874827499D+234,                &
        6.917786472619486D+236, 9.615723196941086D+238, 1.346201247571752D+241,               &
        1.89814375907617D+243,  2.695364137888161D+245, 3.854370717180071D+247,               &
        5.550293832739301D+249, 8.047926057471987D+251, 1.17499720439091D+254,                &
        1.727245890454638D+256, 2.556323917872864D+258, 3.808922637630567D+260,               &
        5.71338395644585D+262,  8.627209774233235D+264, 1.311335885683452D+267,               &
        2.006343905095681D+269, 3.089769613847349D+271, 4.789142901463391D+273,               &
        7.471062926282891D+275, 1.172956879426414D+278, 1.853271869493734D+280,               &
        2.946702272495037D+282, 4.714723635992059D+284, 7.590705053947215D+286,               &
        1.229694218739449D+289, 2.004401576545302D+291, 3.287218585534294D+293,               &
        5.423910666131586D+295, 9.003691705778433D+297, 1.503616514864998D+300,               &
        2.526075744973197D+302, 4.269068009004703D+304, 7.257415615307994D+306]

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (N < 0) THEN
        CALL Handle_ErrLevel('num2_Factorial', ModName, ErrWarning, 'N < 0')
        ResVal = IEEE_VALUE(ZERO_DBL, IEEE_SIGNALING_NAN)
    ELSEIF (N > 170) THEN
        CALL Handle_ErrLevel('num2_Factorial', ModName, ErrWarning, 'N > 170')
        ResVal = IEEE_VALUE(ONE_DBL, IEEE_POSITIVE_INF)
    ELSE
        ResVal = Factorials(N)
    END IF

    RETURN

END FUNCTION num2_Factorial

!******************************************************************************

FUNCTION num2_LnFactorial(N) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the value of the natural logarithm of factorial of N, ln(N!)

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: N
    tDouble                 :: ResVal
    tInteger,  PARAMETER    :: MLIM    = 50

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! The natural logarithm of factorials n! from n = 0 to 50
    tDouble,   PARAMETER    :: LnFactorials(0:MLIM) = [ &
       0.0_kDouble, 0.0_kDouble, 0.6931471805599453_kDouble, 1.791759469228055_kDouble, &
       3.178053830347946_kDouble, 4.787491742782046_kDouble, 6.579251212010101_kDouble, &
       8.525161361065415_kDouble, 10.60460290274525_kDouble, 12.80182748008147_kDouble, &
       15.10441257307552_kDouble, 17.50230784587389_kDouble, 19.98721449566188_kDouble, &
       22.55216385312342_kDouble, 25.19122118273868_kDouble, 27.89927138384088_kDouble, &
       30.67186010608066_kDouble, 33.50507345013688_kDouble, 36.39544520803305_kDouble, &
       39.33988418719949_kDouble, 42.33561646075348_kDouble, 45.3801388984769_kDouble,  &
       48.47118135183522_kDouble, 51.60667556776437_kDouble, 54.7847293981123_kDouble,  &
       58.00360522298051_kDouble, 61.26170176100199_kDouble, 64.55753862700632_kDouble, &
       67.88974313718154_kDouble, 71.257038967168_kDouble,   74.65823634883016_kDouble, &
       78.09222355331529_kDouble, 81.55795945611503_kDouble, 85.05446701758153_kDouble, &
       88.58082754219767_kDouble, 92.13617560368708_kDouble, 95.7196945421432_kDouble,  &
       99.3306124547874_kDouble,  102.9681986145138_kDouble, 106.6317602606434_kDouble, &
       110.3206397147574_kDouble, 114.0342117814617_kDouble, 117.7718813997451_kDouble, &
       121.5330815154386_kDouble, 125.3172711493569_kDouble, 129.1239336391272_kDouble, &
       132.9525750356163_kDouble, 136.8027226373264_kDouble, 140.6739236482343_kDouble, &
       144.5657439463449_kDouble, 148.477766951773_kDouble]

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (N < 0) THEN
        CALL Handle_ErrLevel('num2_LnFactorial', ModName, ErrWarning, 'N < 0')
        ResVal = IEEE_VALUE(ZERO_DBL, IEEE_SIGNALING_NAN)
    ELSEIF (N <= MLIM) THEN
        ResVal = LnFactorials(N)
    ELSE
        BLOCK
            tDouble     :: X, Y, Z
            X = REAL(N+1, KIND=kDouble)
            Y = ONE_DBL / (X * X)
            Z = ((-(5.95238095238D-4 * Y) + 7.936500793651D-4) * Y - &
                 2.7777777777778D-3) * Y + 8.3333333333333D-2
            ResVal = ((X - 0.5_kDouble) * LOG(X) - X) + 9.1893853320467D-1 + Z / X
        END BLOCK
    END IF

    RETURN

END FUNCTION num2_LnFactorial

!******************************************************************************

FUNCTION num2_LnGamma(X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute the natural logarithm of the gamma function at X
    ! (using FORTRAN intrinsic function(s))

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
   
    IF (X <= ZERO_DBL) THEN
        CALL Handle_ErrLevel('num2_LnGamma', ModName, ErrWarning, 'Invalid X: Only accept X > 0')
        ResVal = HUGE(ONE_DBL)
    ELSE
        ResVal = LOG_GAMMA(X)
    END IF

    RETURN

END FUNCTION num2_LnGamma

!******************************************************************************

FUNCTION num2_LnGamma_MyLib(X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute the natural logarithm of the gamma function at X

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal
    
!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble,   PARAMETER    :: XLimBig = ONE_DBL / DBL_EPSILON
    tDouble,   PARAMETER    :: XLim1 = 18.0_kDouble
    ! Ln (sqrt (2 Pi))
    tDouble,   PARAMETER    :: DK2 = 0.91893853320467274178_kDouble
    tDouble,   PARAMETER    :: DK1 = 0.9574186990510627_kDouble
    ! Degree of Chebyshev polynomial
    tInteger,  PARAMETER    :: N = 15
    ! Chebyshev coefficients for lnGamma (X + 3), 0 <= X <= 1 In Yudell Luke:
    ! The special functions and their approximations, Vol. II, Academic Press,
    ! p. 301, 1969. There is an error in the additive constant in the formula:
    ! (Ln (2)). */
    tDouble,   PARAMETER    :: A(0:15) = [    &
        0.52854303698223459887_kDouble,  0.54987644612141411418_kDouble, &
        0.02073980061613665136_kDouble, -0.00056916770421543842_kDouble, &
        0.00002324587210400169_kDouble, -0.00000113060758570393_kDouble, &
        0.00000006065653098948_kDouble, -0.00000000346284357770_kDouble, &
        0.00000000020624998806_kDouble, -0.00000000001266351116_kDouble, &
        0.00000000000079531007_kDouble, -0.00000000000005082077_kDouble, &
        0.00000000000000329187_kDouble, -0.00000000000000021556_kDouble, &
        0.00000000000000001424_kDouble, -0.00000000000000000095_kDouble]

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: Y, Z
    tInteger    :: I, K

! FLOW
   
    Y = ZERO_DBL
    Z = ZERO_DBL

    IF (X <= ZERO_DBL) THEN
        CALL Handle_ErrLevel('num2_LnGamma', ModName, ErrWarning, 'Invalid X: Only accept X > 0')
        ResVal = HUGE(ONE_DBL)
        RETURN
    ELSEIF (X > XLim1) THEN
        IF (X > XLimBig) THEN
            Y = ZERO_DBL
        ELSE
            Y = ONE_DBL / (X * X)
        END IF
        Z = ((-(5.95238095238D-4 * Y) + 7.936500793651D-4) * Y - &
             2.7777777777778D-3) * Y + 8.3333333333333D-2
        ResVal = ((X - 0.5_kDouble) * LOG (X) - X) + DK2 + Z / X
        RETURN
    ELSEIF (X > 4.0_kDouble) THEN
        K = INT(X, KIND=kInteger)
        Z = X - K
        Y = ONE_DBL
        DO I = 3, K-1
            Y = Y*(Z+I)
        END DO
        Y = LOG(Y)
    ELSEIF (X < 3.0_kDouble) THEN
        K = INT(X, KIND=kInteger)
        Z = X - K
        Y = ONE_DBL
        I = 2
        DO WHILE (I >= K)
            Y = Y*(Z+I)
            I = I - 1
        END DO
        Y = -LOG(Y)
    ELSE    ! 3 <= X <= 4
        Z = X - 3.0_kDouble
        Y = ZERO_DBL
    END IF

    Z = num2_EvalCheby (A, N, TWO_DBL * Z - ONE_DBL)
    ResVal = Z + DK1 + Y

    RETURN

END FUNCTION num2_LnGamma_MyLib

!******************************************************************************

FUNCTION num2_EvalCheby(A, N, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To evaluate a series of Chebyshev polynomials Tj, at point X in the interval [-1, 1].

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble,  INTENT(IN)    :: A(0:)
    tInteger, INTENT(IN)    :: N
    tDouble,  INTENT(IN)    :: X
    tDouble                 :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: XX, B0, B1, B2
    tInteger    :: J

! FLOW

    IF (ABS(X) > ONE_DBL) THEN
        CALL Handle_ErrLevel('num2_EvalCheby', ModName, ErrWarning, \
                   'Chebychev polynomial evaluated at x outside [-1, 1]')
    END IF

    XX = TWO_DBL * X
    B0 = ZERO_DBL
    B1 = ZERO_DBL
    DO J = N, 0, -1
      B2 = B1
      B1 = B0
      B0 = (XX * B1 - B2) + A(J)
    END DO
    ResVal = (B0 - B2) / TWO_DBL

    RETURN

END FUNCTION num2_EvalCheby

!******************************************************************************

FUNCTION num2_Combination(N, SIn) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the value of the number of different combinations of S objects amongst N.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: N
    tInteger, INTENT(IN)    :: SIn
    tDouble                 :: ResVal
    
!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: NLIM = 100

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: I, Diff, S

! FLOW

    S = SIn
    IF ((S == 0).OR.(S == N)) THEN
        ResVal = ONE_DBL
        RETURN
    END IF
    IF (S < 0) THEN
        CALL Handle_ErrLevel('num2_Combination', ModName, ErrWarning, 'S < 0')
        ResVal = ZERO_DBL
        RETURN
    END IF
    IF (s > n) THEN
        CALL Handle_ErrLevel('num2_Combination', ModName, ErrWarning, 'S > N')
        ResVal = ZERO_DBL
        RETURN
    END IF
    IF (S > (N / 2)) S = N - S
    IF (N <= NLIM) THEN
        ResVal = ONE_DBL
        Diff = N - S
        DO I = 1, S
            ResVal = (ResVal*REAL(Diff + I, KIND=kDouble))/REAL(I, KIND=kDouble)
        END DO
    ELSE
        ResVal = (num2_LnFactorial(N) - num2_LnFactorial(S)) - num2_LnFactorial(N - S)
        ResVal = EXP(ResVal)
    END IF

    RETURN

END FUNCTION num2_Combination

!******************************************************************************

FUNCTION num2_LOG1P(X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return a value equivalent to log (1 + x), accurate also for small x.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
   
    IF (ABS(X) > 0.1_kDouble) THEN
        ResVal = LOG(ONE_DBL + X)
    ELSE
        BLOCK
            tDouble   :: Term, Sum
            tInteger  :: S
            Term = X
            Sum = X
            S = 2
            DO WHILE ((ABS(Term) > EPSTOL*ABS(Sum)).AND.(S < MAXI))
                Term = Term * (-X)
                Sum  = Sum + Term / S
                S = S + 1
            END DO
            ResVal = Sum
        END BLOCK
    END IF

    RETURN

END FUNCTION num2_LOG1P

!******************************************************************************

FUNCTION num2_BesselK025(X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the value of K1/4(x), where Kv is the modified
    ! Bessel’s function of the second kind.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: DEGREE = 6
    tDouble,  PARAMETER :: BParam(0:6) = [                  &
        75293843625.0_kDouble,    2891283595200.0_kDouble,  &
        18691126272000.0_kDouble, 36807140966400.0_kDouble, &
        27348959232000.0_kDouble, 7972533043200.0_kDouble,  &
        755914244096.0_kDouble]
    tDouble,  PARAMETER :: CParam(0:6) = [                  &
        32177591145.0_kDouble,    2099336339520.0_kDouble,  &
        16281990144000.0_kDouble, 34611957596160.0_kDouble, &
        26640289628160.0_kDouble, 7901666082816.0_kDouble,  &
        755914244096.0_kDouble]
    tDouble,  PARAMETER :: num_Pi   = 3.14159265358979323846_kDouble
    tDouble,  PARAMETER :: num_Rac2 = 1.41421356237309504880_kDouble
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: Rac, XX, Temp, C, B
    tInteger    :: J

! FLOW
   
    IF (X < 1.0D-300) THEN
        ResVal = HUGE(ONE_DBL)
        RETURN
    END IF
    
    !------------------------------------------------------------------
    ! x > 0.6 => approximation asymptotique rationnelle dans Luke: 
    ! Yudell L.Luke "Mathematical functions and their approximations", 
    ! Academic Press Inc. New York, 1975, p.371 
    !------------------------------------------------------------------
    IF (X >= 0.6) THEN
        B = BParam(DEGREE)
        C = CParam(DEGREE)
        DO J = DEGREE, 1, -1
            B = B * X + BParam(J - 1)
            C = C * X + CParam(J - 1)
        END DO
        ResVal = SQRT (num_Pi / (TWO_DBL * X)) * EXP (-X) * (C / B)
        RETURN
    END IF

    !------------------------------------------------------------------
    ! x < 0.6 => la serie de K_{1/4} = Pi/Sqrt(2) [I_{-1/4} - I_{1/4}] 
    !------------------------------------------------------------------
    XX = X * X
    Rac = (X / TWO_DBL)**0.25_kDouble
    ResVal = (((XX / 1386.0_kDouble + ONE_DBL / 42.0_kDouble) * XX + ONE_DBL / 3.0_kDouble) &
              * XX + ONE_DBL) / (1.225416702465177 * Rac)
    Temp = (((XX / 3510.0_kDouble + ONE_DBL / 90.0_kDouble) * XX + 0.2_kDouble) &
              * XX + ONE_DBL) * Rac / 0.906402477055477_kDouble
    ResVal = num_Pi * (ResVal - Temp) / num_Rac2

    RETURN

END FUNCTION num2_BesselK025

!******************************************************************************

END MODULE ModBase_MyLib
