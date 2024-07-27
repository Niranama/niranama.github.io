
SUBMODULE (ModBase_ProbDist) SubBase_FInv

!** PURPOSE OF THIS SUBMODULE:
    ! This submodule provides procedures to compute or approximate the inverse of certain
    ! distribution functions.  Each procedure computes FInv(u) = inf{x in REAL: F(x) >= u},
    ! where 0 <= u <= 1 and F is the distribution function of a specific type of random
    ! variable.  These procedures can be used, among other things, to generate the
    ! corresponding random variables by inversion, by passing a U(0, 1) random variate
    ! as the value of u.
    ! Several distributions are only implemented in standardized form here, i.e., with the
    ! location parameter set to 0 and the scale parameter set to 1.  To obtain the inverse
    ! for the distribution shifted by x0 and rescaled by a factor c, it suffices to multiply
    ! the returned value by c and add x0.

!** USE STATEMENTS:
    USE ModBase_Error_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! name of submodule
    tCharStar, PARAMETER    :: SubModName = 'SubBase_FInv1'
    tDouble,   PARAMETER    :: EPSTOL     = 1.0D-15         ! Tolerance

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

MODULE FUNCTION finv_Expon(U) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the inverse of the standard exponential distribution.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: U
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF ((U < ZERO_DBL).OR.(U > ONE_DBL)) THEN
        CALL Handle_ErrLevel('finv_Expon', SubModName, ErrWarning, 'U Not in [0, 1].')
        RETURN
    END IF
    
    IF (U >= ONE_DBL) THEN
        ResVal = fdist_XBIGM
        CALL Handle_ErrLevel('finv_Expon', SubModName, ErrWarning, 'U = 1.')
    ELSEIF (U <= ZERO_DBL) THEN
        ResVal = ZERO_DBL
    ELSE
        ResVal = -num2_LOG1P (-U)
    END IF

    RETURN

END FUNCTION finv_Expon

!******************************************************************************

MODULE FUNCTION finv_Weibull(Alpha, U) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the inverse of the standard Weibull distribution.
    ! Restriction:  Alpha > 0.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: Alpha, U
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: T

! FLOW

    IF (Alpha <= ZERO_DBL) THEN
        CALL Handle_ErrLevel('finv_Weibull', SubModName, ErrWarning, 'Alpha <= 0.')
        RETURN
    ELSEIF ((U < ZERO_DBL).OR.(U > ONE_DBL)) THEN
        CALL Handle_ErrLevel('finv_Weibull', SubModName, ErrWarning, 'U Not in [0, 1].')
        RETURN
    END IF
    
    IF (U <= ZERO_DBL) THEN
        ResVal = ZERO_DBL
    ELSEIF (U >= ONE_DBL) THEN
        ResVal = fdist_XINF
        CALL Handle_ErrLevel('finv_Weibull', SubModName, ErrWarning, 'U = 1.')
    ELSE
        T = -num2_LOG1P (-U)
        IF (LOG10 (T) >= Alpha * DBL_MAX_10_EXP) THEN
            ResVal = fdist_XINF
            CALL Handle_ErrLevel('finv_Weibull', SubModName, ErrWarning, 'U --> 1.')
        ELSE
            ResVal = T**(ONE_DBL / Alpha)
        END IF
    END IF

    RETURN

END FUNCTION finv_Weibull

!******************************************************************************

MODULE FUNCTION finv_ExtremeValue(U) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the inverse of the standard extreme value distribution.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: U
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF ((U < ZERO_DBL).OR.(U > ONE_DBL)) THEN
        CALL Handle_ErrLevel('finv_ExtremeValue', SubModName, ErrWarning, 'U Not in [0, 1].')
        RETURN
    END IF
    IF (U >= ONE_DBL) THEN
        ResVal = fdist_XBIG
        CALL Handle_ErrLevel('finv_ExtremeValue', SubModName, ErrWarning, 'U = 1.')
    ELSEIF (U <= ZERO_DBL) THEN
        ResVal = -fdist_XBIG
        CALL Handle_ErrLevel('finv_ExtremeValue', SubModName, ErrWarning, 'U = 0.')
    ELSE
        ResVal = -LOG (-LOG (U))
    END IF

    RETURN

END FUNCTION finv_ExtremeValue

!******************************************************************************

MODULE FUNCTION finv_Logistic(U) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the inverse of the standard logistic distribution.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: U
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF ((U < ZERO_DBL).OR.(U > ONE_DBL)) THEN
        CALL Handle_ErrLevel('finv_Logistic', SubModName, ErrWarning, 'U Not in [0, 1].')
        RETURN
    END IF
    IF (U >= ONE_DBL) THEN
        ResVal = fdist_XBIG
        CALL Handle_ErrLevel('finv_Logistic', SubModName, ErrWarning, 'U = 1.')
    ELSEIF (U <= ZERO_DBL) THEN
        ResVal = -fdist_XBIG
        CALL Handle_ErrLevel('finv_Logistic', SubModName, ErrWarning, 'U = 0.')
    ELSE
        ResVal = -LOG (U / (ONE_DBL - U))
    END IF

    RETURN

END FUNCTION finv_Logistic

!******************************************************************************

MODULE FUNCTION finv_Pareto(C, U) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To the inverse of the standard Pareto distribution.  Restriction:  C > 0.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: C, U
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: T

! FLOW

    IF (C <= ZERO_DBL) THEN
        CALL Handle_ErrLevel('finv_Pareto', SubModName, ErrWarning, 'C <= 0.')
        RETURN
    ELSEIF ((U < ZERO_DBL).OR.(U > ONE_DBL)) THEN
        CALL Handle_ErrLevel('finv_Pareto', SubModName, ErrWarning, 'U Not in [0, 1].')
        RETURN
    END IF

    IF (U <= ZERO_DBL) THEN
        ResVal = ONE_DBL
    ELSE
        T = -num2_LOG1P (-U)
        IF ((U >= ONE_DBL).OR.(T >= C * DBL_MAX_EXP)) THEN
            ResVal = fdist_XINF
            CALL Handle_ErrLevel('finv_Pareto', SubModName, ErrWarning, 'U = 1 or T >= C * DBL_MAX_EXP.')
        ELSE
            ResVal = (ONE_DBL / (1 - U))**(ONE_DBL / C)
        END IF
    END IF

    RETURN

END FUNCTION finv_Pareto

!******************************************************************************

MODULE FUNCTION finv_Normal1(U) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return an approximation of PhiInv(U), where Phi is the standard normal
    ! distribution function, with mean 0 and variance 1.  Uses rational Chebyshev
    ! approximations giving at least 15 decimal digits of precision over most of
    ! the range.  Far in the lower tail (u < 1.0e-122), the precision decreases
    ! slowly until for u < 1.0e-308, the function gives only 11 decimal digits
    ! of precision.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: U
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble, PARAMETER  :: InvNormal1P1(0:6) = [                    &
        0.160304955844066229311D2,      -0.90784959262960326650D2,  &
        0.18644914861620987391D3,       -0.16900142734642382420D3,  &
        0.6545466284794487048D2,        -0.864213011587247794D1,    &
        0.1760587821390590_kDouble]
    tDouble, PARAMETER  :: InvNormal1Q1(0:6) = [                    &
        0.147806470715138316110D2,      -0.91374167024260313396D2,  &
        0.21015790486205317714D3,       -0.22210254121855132366D3,  &
        0.10760453916055123830D3,       -0.206010730328265443D2,    &
        0.1D1]
    tDouble, PARAMETER  :: InvNormal1P2(0:7) = [                        &
        -0.152389263440726128D-1,       0.3444556924136125216_kDouble,  &
        -0.29344398672542478687D1,      0.11763505705217827302D2,       &
        -0.22655292823101104193D2,      0.19121334396580330163D2,       &
        -0.5478927619598318769D1,       0.237516689024448000_kDouble]
    tDouble, PARAMETER  :: InvNormal1Q2(0:7) = [                        &
        -0.108465169602059954D-1,       0.2610628885843078511_kDouble,  &
        -0.24068318104393757995D1,      0.10695129973387014469D2,       &
        -0.23716715521596581025D2,      0.24640158943917284883D2,       &
        -0.10014376349783070835D2,      0.1D1]
    tDouble, PARAMETER  :: InvNormal1P3(0:10) = [                       &
        0.56451977709864482298D-4,      0.53504147487893013765D-2,      &
        0.12969550099727352403_kDouble, 0.10426158549298266122D1,       &
        0.28302677901754489974D1,       0.26255672879448072726D1,       &
        0.20789742630174917228D1,       0.72718806231556811306_kDouble, &
        0.66816807711804989575D-1,     -0.17791004575111759979D-1,      &
        0.22419563223346345828D-2]
    tDouble, PARAMETER  :: InvNormal1Q3(0:10) = [                   &
        0.56451699862760651514D-4,      0.53505587067930653953D-2,  &
        0.12986615416911646934_kDouble, 0.10542932232626491195D1,   &
        0.30379331173522206237D1,       0.37631168536405028901D1,   &
        0.38782858277042011263D1,       0.20372431817412177929D1,   &
        0.1D1,                          0.0_kDouble,                &
        0.0_kDouble]

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: X, Z, V, Numerator, Denominator
    tInteger    :: I
    tLogical    :: Negative

! FLOW

    ! Returns the inverse of the cdf of the normal distribution.
    ! Rational approximations giving 16 decimals of precision.
    ! The authors also give an approximation with 23 decimals of
    ! precision.
    ! J.M. Blair, C.A. Edwards, J.H. Johnson, "Rational Chebyshev
    ! approximations for the Inverse of the Error Function", in
    ! Mathematics of Computation, Vol. 30, 136, pp 827, (1976)

    IF ((U < ZERO_DBL).OR.(U > ONE_DBL)) THEN
        CALL Handle_ErrLevel('finv_Normal1', SubModName, ErrWarning, 'U Not in [0, 1].')
        RETURN
    END IF
    
    IF (U <= ZERO_DBL) THEN
        ResVal = -fdist_XBIG
        CALL Handle_ErrLevel('finv_Normal1', SubModName, ErrWarning, 'U = 0.')
        RETURN
    END IF

    ! Transform U as argument of InvErf
    Z = U
    X = TWO_DBL * U - ONE_DBL
    IF (X >= ONE_DBL) THEN
        ResVal = fdist_XBIG
        CALL Handle_ErrLevel('finv_Normal1', SubModName, ErrWarning, 'U = 1.')
        RETURN
    END IF
    IF (X < ZERO_DBL) THEN
        X = -X
        Negative = TrueVal
    ELSE
        Negative = FalseVal
    END IF

    IF (X <= 0.75_kDouble) THEN
        V = X * X - 0.5625_kDouble
        Denominator = ZERO_DBL
        Numerator   = ZERO_DBL
        ! Evaluation of the 2 polynomials by Horner
        DO I = 6, 0, -1
            Numerator   = Numerator * V + InvNormal1P1(I)
            Denominator = Denominator * V + InvNormal1Q1(I)
        END DO
        Z = X * Numerator / Denominator

    ELSEIF (X <= 0.9375_kDouble) THEN
        V = X * X - 0.87890625_kDouble
        Denominator = ZERO_DBL
        Numerator   = ZERO_DBL
        DO I = 7, 0, -1
            Numerator   = Numerator * V + InvNormal1P2(I)
            Denominator = Denominator * V + InvNormal1Q2(I)
        END DO
        Z = X * Numerator / Denominator
    ELSE
        IF (Z > 1.0D-1) THEN
            V = ONE_DBL / SQRT (-LOG (ONE_DBL - X))
        ELSE
            V = ONE_DBL / SQRT (-LOG (TWO_DBL*Z))
        END IF
        Denominator = ZERO_DBL
        Numerator   = ZERO_DBL
        DO I = 10, 0, -1
            Numerator = Numerator * V + InvNormal1P3(I)
        END DO
        DO I = 8, 0, -1
            Denominator = Denominator * V + InvNormal1Q3(I)
        END DO
        Z = (ONE_DBL / V) * Numerator / Denominator
    END IF

    IF (Negative) THEN
        ResVal = -(Z * num_Rac2)
    ELSE
        ResVal = Z * num_Rac2
    END IF

    RETURN

END FUNCTION finv_Normal1

!******************************************************************************

MODULE FUNCTION finv_Normal2(U) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return an approximation of PhiInv(U), where Phi is the standard normal
    ! distribution function, with mean 0 and variance 1.  Uses Marsaglia’s method [1]
    ! with tables lookup.  The method works provided that the processor respects
    ! the IEEE-754 floating-point standard.  Returns 6 decimal digits of precision.
    ! This function is twice as fast as "finv_Normal1".

!** REFERENCE:
    ! [1] G. Marsaglia, A. Zaman, and J. C. W. Marsaglia. Rapid evaluation of the inverse
    !     normal distribution function. Statistics and Probability Letters, 19:259–266, 1994.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: U
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
        tDouble,  PARAMETER  :: A(0:1024) = [                                           &
         6.33795775455_kDouble,      6.33321372178_kDouble,      6.32860807635_kDouble, &
         6.32413288056_kDouble,      6.31978086301_kDouble,      6.31554534594_kDouble, &
         6.31142018224_kDouble,      6.30739970055_kDouble,      6.30347865735_kDouble, &
         6.29965219486_kDouble,      6.29591580408_kDouble,      6.29226529211_kDouble, &
         6.28869675323_kDouble,      6.28520654322_kDouble,      6.28179125646_kDouble, &
         6.27844770553_kDouble,      6.27517290294_kDouble,      6.27196404468_kDouble, &
          6.2688184955_kDouble,      6.26573377562_kDouble,      6.26270754867_kDouble, &
         6.25973761088_kDouble,      6.25682188116_kDouble,      6.25395839218_kDouble, &
         6.25114528219_kDouble,      6.24838078761_kDouble,       6.2456632362_kDouble, &
         6.24299104087_kDouble,        6.240362694_kDouble,      6.23777676216_kDouble, &
         6.23523188139_kDouble,      6.23272675268_kDouble,      6.23026013799_kDouble, &
         6.22543778081_kDouble,      6.22075598811_kDouble,          6.2162067_kDouble, &
         6.21178253304_kDouble,      6.20747670647_kDouble,      6.20328297819_kDouble, &
         6.19919558917_kDouble,      6.19520921468_kDouble,      6.19131892165_kDouble, &
         6.18752013112_kDouble,      6.18380858505_kDouble,        6.180180317_kDouble, &
         6.17663162601_kDouble,       6.1731590534_kDouble,      6.16975936206_kDouble, &
         6.16642951782_kDouble,      6.16316667288_kDouble,      6.15996815078_kDouble, &
          6.1568314329_kDouble,      6.15375414629_kDouble,      6.15073405259_kDouble, &
         6.14776903807_kDouble,      6.14485710448_kDouble,      6.14199636082_kDouble, &
         6.13918501574_kDouble,      6.13642137069_kDouble,      6.13370381356_kDouble, &
         6.13103081295_kDouble,      6.12840091282_kDouble,      6.12581272765_kDouble, &
         6.12326493791_kDouble,      6.12075628597_kDouble,      6.11585165173_kDouble, &
         6.11108986339_kDouble,      6.10646273321_kDouble,      6.10196276044_kDouble, &
         6.09758305644_kDouble,      6.09331727974_kDouble,      6.08915957943_kDouble, &
         6.08510454578_kDouble,      6.08114716689_kDouble,      6.07728279052_kDouble, &
         6.07350709041_kDouble,      6.06981603653_kDouble,      6.06620586849_kDouble, &
         6.06267307204_kDouble,      6.05921435799_kDouble,      6.05582664334_kDouble, &
         6.05250703436_kDouble,      6.04925281142_kDouble,      6.04606141524_kDouble, &
          6.0429304345_kDouble,      6.03985759465_kDouble,      6.03684074773_kDouble, &
         6.03387786311_kDouble,      6.03096701912_kDouble,      6.02810639533_kDouble, &
         6.02529426559_kDouble,       6.0225289916_kDouble,      6.01980901702_kDouble, &
         6.01713286212_kDouble,      6.01449911878_kDouble,      6.01190644597_kDouble, &
         6.00935356553_kDouble,      6.00436236049_kDouble,      5.99951639563_kDouble, &
         5.99480734912_kDouble,      5.99022759726_kDouble,      5.98577013833_kDouble, &
         5.98142852661_kDouble,       5.9771968149_kDouble,      5.97306950434_kDouble, &
         5.96904150032_kDouble,      5.96510807378_kDouble,      5.96126482692_kDouble, &
         5.95750766299_kDouble,      5.95383275929_kDouble,      5.95023654327_kDouble, &
         5.94671567111_kDouble,      5.94326700856_kDouble,      5.93988761376_kDouble, &
         5.93657472175_kDouble,       5.9333257306_kDouble,      5.93013818875_kDouble, &
         5.92700978364_kDouble,      5.92393833142_kDouble,      5.92092176747_kDouble, &
         5.91795813793_kDouble,      5.91504559188_kDouble,      5.91218237419_kDouble, &
         5.90936681905_kDouble,      5.90659734398_kDouble,      5.90387244436_kDouble, &
         5.90119068838_kDouble,      5.89855071241_kDouble,      5.89595121674_kDouble, &
         5.89086876372_kDouble,      5.88593406746_kDouble,      5.88113866541_kDouble, &
         5.87647480476_kDouble,      5.87193536504_kDouble,      5.86751379108_kDouble, &
         5.86320403456_kDouble,      5.85900050299_kDouble,      5.85489801495_kDouble, &
         5.85089176064_kDouble,      5.84697726716_kDouble,      5.84315036764_kDouble, &
         5.83940717394_kDouble,      5.83574405227_kDouble,      5.83215760142_kDouble, &
         5.82864463328_kDouble,      5.82520215541_kDouble,      5.82182735527_kDouble, &
         5.81851758606_kDouble,      5.81527035394_kDouble,      5.81208330646_kDouble, &
         5.80895422198_kDouble,      5.80588100021_kDouble,      5.80286165344_kDouble, &
         5.79989429868_kDouble,      5.79697715037_kDouble,      5.79410851379_kDouble, &
         5.79128677896_kDouble,      5.78851041509_kDouble,      5.78577796547_kDouble, &
         5.78308804272_kDouble,      5.78043932448_kDouble,       5.7752605134_kDouble, &
         5.77023210866_kDouble,      5.76534549972_kDouble,      5.76059279799_kDouble, &
         5.75596675813_kDouble,      5.75146070984_kDouble,      5.74706849845_kDouble, &
         5.74278443301_kDouble,      5.73860324073_kDouble,      5.73452002692_kDouble, &
         5.73053023962_kDouble,      5.72662963823_kDouble,      5.72281426575_kDouble, &
         5.71908042393_kDouble,      5.71542465116_kDouble,       5.7118437027_kDouble, &
         5.70833453283_kDouble,      5.70489427893_kDouble,      5.70152024702_kDouble, &
         5.69820989876_kDouble,      5.69496083963_kDouble,      5.69177080831_kDouble, &
         5.68863766688_kDouble,      5.68555939205_kDouble,      5.68253406704_kDouble, &
         5.67955987426_kDouble,      5.67663508853_kDouble,      5.67375807094_kDouble, &
         5.67092726314_kDouble,      5.66814118219_kDouble,       5.6653984157_kDouble, &
         5.66269761746_kDouble,      5.65741684752_kDouble,      5.65228927845_kDouble, &
         5.64730614385_kDouble,      5.64245941209_kDouble,       5.6377417063_kDouble, &
         5.63314623489_kDouble,      5.62866673113_kDouble,      5.62429740032_kDouble, &
          5.6200328734_kDouble,      5.61586816622_kDouble,      5.61179864349_kDouble, &
         5.60781998693_kDouble,      5.60392816693_kDouble,       5.6001194173_kDouble, &
          5.5963902128_kDouble,      5.59273724893_kDouble,      5.58915742386_kDouble, &
         5.58564782214_kDouble,      5.58220570003_kDouble,      5.57882847227_kDouble, &
         5.57551370004_kDouble,      5.57225908009_kDouble,      5.56906243489_kDouble, &
         5.56592170358_kDouble,      5.56283493377_kDouble,      5.55980027406_kDouble, &
         5.55681596716_kDouble,      5.55388034364_kDouble,      5.55099181609_kDouble, &
         5.54814887388_kDouble,      5.54535007824_kDouble,       5.5425940578_kDouble, &
         5.53720516928_kDouble,      5.53197243466_kDouble,      5.52688692307_kDouble, &
         5.52194045196_kDouble,       5.5171255056_kDouble,      5.51243516442_kDouble, &
         5.50786304341_kDouble,      5.50340323832_kDouble,      5.49905027849_kDouble, &
          5.4947990853_kDouble,      5.49064493552_kDouble,       5.4865834288_kDouble, &
         5.48261045887_kDouble,      5.47872218783_kDouble,       5.4749150232_kDouble, &
         5.47118559738_kDouble,      5.46753074927_kDouble,      5.46394750763_kDouble, &
         5.46043307616_kDouble,      5.45698482001_kDouble,      5.45360025356_kDouble, &
         5.45027702931_kDouble,      5.44701292782_kDouble,      5.44380584855_kDouble, &
         5.44065380149_kDouble,      5.43755489952_kDouble,      5.43450735142_kDouble, &
         5.43150945548_kDouble,      5.42855959358_kDouble,      5.42565622584_kDouble, &
         5.42279788561_kDouble,      5.41998317492_kDouble,      5.41447936842_kDouble, &
         5.40913484272_kDouble,      5.40394049304_kDouble,      5.39888797725_kDouble, &
         5.39396963282_kDouble,      5.38917840481_kDouble,      5.38450778312_kDouble, &
         5.37995174765_kDouble,      5.37550472027_kDouble,      5.37116152244_kDouble, &
         5.36691733786_kDouble,      5.36276767939_kDouble,      5.35870835965_kDouble, &
         5.35473546482_kDouble,       5.3508453313_kDouble,      5.34703452473_kDouble, &
         5.34329982125_kDouble,      5.33963819058_kDouble,       5.3360467808_kDouble, &
         5.33252290459_kDouble,      5.32906402674_kDouble,      5.32566775291_kDouble, &
         5.32233181932_kDouble,      5.31905408341_kDouble,      5.31583251534_kDouble, &
         5.31266519015_kDouble,      5.30955028069_kDouble,      5.30648605108_kDouble, &
         5.30347085068_kDouble,      5.30050310862_kDouble,      5.29758132874_kDouble, &
         5.29470408485_kDouble,      5.28907782499_kDouble,      5.28361416415_kDouble, &
         5.27830381332_kDouble,       5.2731382613_kDouble,         5.26810969_kDouble, &
         5.26321090103_kDouble,      5.25843525172_kDouble,       5.2537765992_kDouble, &
         5.24922925139_kDouble,      5.24478792384_kDouble,      5.24044770161_kDouble, &
         5.23620400551_kDouble,      5.23205256217_kDouble,      5.22798937729_kDouble, &
         5.22401071182_kDouble,       5.2201130606_kDouble,      5.21629313323_kDouble, &
         5.21254783681_kDouble,      5.20887426039_kDouble,        5.205269661_kDouble, &
         5.20173145085_kDouble,      5.19825718588_kDouble,      5.19484455528_kDouble, &
         5.19149137189_kDouble,       5.1881955636_kDouble,      5.18495516531_kDouble, &
         5.18176831175_kDouble,      5.17863323075_kDouble,      5.17554823716_kDouble, &
         5.17251172724_kDouble,      5.16952217346_kDouble,      5.16657811973_kDouble, &
         5.16082101937_kDouble,      5.15523004763_kDouble,      5.14979571985_kDouble, &
         5.14450934531_kDouble,      5.13936294072_kDouble,      5.13434915534_kDouble, &
         5.12946120568_kDouble,      5.12469281843_kDouble,       5.1200381805_kDouble, &
         5.11549189488_kDouble,      5.11104894181_kDouble,      5.10670464433_kDouble, &
         5.10245463765_kDouble,      5.09829484194_kDouble,      5.09422143797_kDouble, &
         5.09023084532_kDouble,      5.08631970285_kDouble,      5.08248485106_kDouble, &
          5.0787233163_kDouble,      5.07503229637_kDouble,       5.0714091476_kDouble, &
         5.06785137307_kDouble,      5.06435661191_kDouble,      5.06092262957_kDouble, &
         5.05754730896_kDouble,      5.05422864229_kDouble,      5.05096472372_kDouble, &
          5.0477537425_kDouble,      5.04459397676_kDouble,      5.04148378777_kDouble, &
         5.03842161464_kDouble,      5.03540596946_kDouble,      5.02950864968_kDouble, &
         5.02378122248_kDouble,      5.01821399503_kDouble,      5.01279808548_kDouble, &
         5.00752533468_kDouble,       5.0023882297_kDouble,      4.99737983708_kDouble, &
         4.99249374467_kDouble,      4.98772401045_kDouble,      4.98306511755_kDouble, &
         4.97851193459_kDouble,      4.97405968039_kDouble,      4.96970389282_kDouble, &
         4.96544040091_kDouble,      4.96126529999_kDouble,      4.95717492946_kDouble, &
         4.95316585275_kDouble,      4.94923483945_kDouble,      4.94537884901_kDouble, &
         4.94159501614_kDouble,      4.93788063759_kDouble,      4.93423316006_kDouble, &
         4.93065016935_kDouble,      4.92712938035_kDouble,      4.92366862801_kDouble, &
         4.92026585904_kDouble,      4.91691912431_kDouble,      4.91362657193_kDouble, &
         4.91038644087_kDouble,      4.90719705507_kDouble,      4.90405681805_kDouble, &
         4.90096420796_kDouble,      4.89491612703_kDouble,       4.8890419637_kDouble, &
         4.88333180298_kDouble,      4.87777655906_kDouble,      4.87236788511_kDouble, &
         4.86709809501_kDouble,      4.86196009527_kDouble,      4.85694732544_kDouble, &
         4.85205370587_kDouble,       4.8472735917_kDouble,      4.84260173228_kDouble, &
          4.8380332352_kDouble,       4.8335635343_kDouble,      4.82918836126_kDouble, &
         4.82490372013_kDouble,       4.8207058646_kDouble,      4.81659127758_kDouble, &
         4.81255665279_kDouble,      4.80859887824_kDouble,       4.8047150212_kDouble, &
         4.80090231471_kDouble,      4.79715814524_kDouble,      4.79348004153_kDouble, &
         4.78986566443_kDouble,      4.78631279763_kDouble,      4.78281933914_kDouble, &
         4.77938329358_kDouble,      4.77600276506_kDouble,      4.77267595063_kDouble, &
         4.76940113431_kDouble,      4.76617668159_kDouble,      4.76300103427_kDouble, &
         4.75679027709_kDouble,       4.7507577546_kDouble,       4.7448933141_kDouble, &
         4.73918765149_kDouble,      4.73363221901_kDouble,      4.72821914523_kDouble, &
         4.72294116527_kDouble,      4.71779155991_kDouble,      4.71276410212_kDouble, &
         4.70785300996_kDouble,      4.70305290507_kDouble,      4.69835877584_kDouble, &
          4.6937659447_kDouble,        4.689270039_kDouble,      4.68486696501_kDouble, &
         4.68055288459_kDouble,      4.67632419431_kDouble,      4.67217750662_kDouble, &
         4.66810963292_kDouble,       4.6641175682_kDouble,      4.66019847719_kDouble, &
         4.65634968177_kDouble,      4.65256864953_kDouble,      4.64885298336_kDouble, &
         4.64520041195_kDouble,      4.64160878112_kDouble,      4.63807604585_kDouble, &
         4.63460026304_kDouble,      4.63117958481_kDouble,      4.62781225236_kDouble, &
          4.6244965903_kDouble,       4.6212310015_kDouble,      4.61484401763_kDouble, &
         4.60863991397_kDouble,      4.60260828265_kDouble,      4.59673958525_kDouble, &
         4.59102505831_kDouble,      4.58545663137_kDouble,      4.58002685553_kDouble, &
         4.57472884104_kDouble,      4.56955620253_kDouble,      4.56450301082_kDouble, &
         4.55956375032_kDouble,      4.55473328135_kDouble,      4.55000680661_kDouble, &
         4.54537984133_kDouble,      4.54084818664_kDouble,       4.5364079057_kDouble, &
         4.53205530226_kDouble,      4.52778690138_kDouble,      4.52359943209_kDouble, &
         4.51948981163_kDouble,      4.51545513127_kDouble,      4.51149264343_kDouble, &
          4.5075997499_kDouble,      4.50377399122_kDouble,      4.50001303691_kDouble, &
         4.49631467662_kDouble,      4.49267681191_kDouble,       4.4890974489_kDouble, &
         4.48557469132_kDouble,      4.48210673427_kDouble,      4.47869185842_kDouble, &
         4.47532842465_kDouble,      4.46874969883_kDouble,      4.46235887005_kDouble, &
         4.45614525563_kDouble,      4.45009906476_kDouble,      4.44421130165_kDouble, &
         4.43847368137_kDouble,      4.43287855675_kDouble,      4.42741885424_kDouble, &
         4.42208801786_kDouble,      4.41687995966_kDouble,      4.41178901614_kDouble, &
          4.4068099095_kDouble,      4.40193771332_kDouble,      4.39716782193_kDouble, &
         4.39249592307_kDouble,       4.3879179734_kDouble,      4.38343017655_kDouble, &
         4.37902896328_kDouble,      4.37471097372_kDouble,      4.37047304122_kDouble, &
         4.36631217778_kDouble,       4.3622255608_kDouble,      4.35821052107_kDouble, &
         4.35426453179_kDouble,      4.35038519863_kDouble,      4.34657025053_kDouble, &
         4.34281753142_kDouble,      4.33912499252_kDouble,      4.33549068532_kDouble, &
         4.33191275513_kDouble,      4.32838943511_kDouble,      4.32491904083_kDouble, &
         4.31813067359_kDouble,      4.31153564248_kDouble,       4.3051229678_kDouble, &
         4.29888258592_kDouble,       4.2928052498_kDouble,      4.28688244266_kDouble, &
         4.28110630283_kDouble,      4.27546955801_kDouble,      4.26996546758_kDouble, &
         4.26458777186_kDouble,      4.25933064724_kDouble,      4.25418866648_kDouble, &
         4.24915676339_kDouble,       4.2442302014_kDouble,      4.23940454548_kDouble, &
           4.234675637_kDouble,      4.23003957111_kDouble,      4.22549267647_kDouble, &
         4.22103149691_kDouble,       4.2166527749_kDouble,      4.21235343653_kDouble, &
         4.20813057793_kDouble,      4.20398145293_kDouble,      4.19990346176_kDouble, &
         4.19589414084_kDouble,      4.19195115337_kDouble,      4.18807228072_kDouble, &
         4.18425541464_kDouble,      4.18049854994_kDouble,      4.17679977794_kDouble, &
         4.17315728028_kDouble,      4.16956932335_kDouble,      4.16255048996_kDouble, &
         4.15573091512_kDouble,      4.14909929681_kDouble,      4.14264527528_kDouble, &
         4.13635933075_kDouble,       4.1302326947_kDouble,      4.12425727258_kDouble, &
         4.11842557618_kDouble,      4.11273066434_kDouble,       4.1071660908_kDouble, &
         4.10172585798_kDouble,      4.09640437621_kDouble,       4.0911964274_kDouble, &
         4.08609713269_kDouble,      4.08110192358_kDouble,      4.07620651603_kDouble, &
         4.07140688728_kDouble,       4.0666992549_kDouble,      4.06208005798_kDouble, &
         4.05754594009_kDouble,      4.05309373387_kDouble,      4.04872044701_kDouble, &
         4.04442324957_kDouble,      4.04019946237_kDouble,      4.03604654644_kDouble, &
         4.03196209338_kDouble,      4.02794381649_kDouble,       4.0239895427_kDouble, &
         4.02009720515_kDouble,       4.0162648363_kDouble,      4.01249056171_kDouble, &
         4.00877259417_kDouble,      4.00149883606_kDouble,      3.99443081604_kDouble, &
         3.98755688078_kDouble,      3.98086634771_kDouble,      3.97434939969_kDouble, &
         3.96799699364_kDouble,      3.96180078097_kDouble,      3.95575303793_kDouble, &
         3.94984660457_kDouble,      3.94407483096_kDouble,      3.93843152965_kDouble, &
         3.93291093367_kDouble,      3.92750765903_kDouble,       3.9222166715_kDouble, &
         3.91703325678_kDouble,      3.91195299388_kDouble,      3.90697173117_kDouble, &
         3.90208556486_kDouble,      3.89729081955_kDouble,      3.89258403075_kDouble, &
         3.88796192889_kDouble,      3.88342142501_kDouble,      3.87895959754_kDouble, &
         3.87457368047_kDouble,      3.87026105237_kDouble,      3.86601922652_kDouble, &
         3.86184584173_kDouble,      3.85773865405_kDouble,      3.85369552908_kDouble, &
         3.84971443491_kDouble,      3.84579343567_kDouble,       3.8419306855_kDouble, &
         3.83437296653_kDouble,      3.82702811235_kDouble,      3.81988408487_kDouble, &
         3.81292984787_kDouble,      3.80615525838_kDouble,      3.79955097242_kDouble, &
         3.79310836293_kDouble,      3.78681944792_kDouble,       3.7806768275_kDouble, &
         3.77467362835_kDouble,      3.76880345477_kDouble,      3.76306034518_kDouble, &
         3.75743873361_kDouble,      3.75193341525_kDouble,      3.74653951576_kDouble, &
         3.74125246377_kDouble,      3.73606796613_kDouble,      3.73098198575_kDouble, &
         3.72599072151_kDouble,      3.72109059012_kDouble,      3.71627820978_kDouble, &
         3.71155038522_kDouble,      3.70690409424_kDouble,      3.70233647531_kDouble, &
         3.69784481637_kDouble,      3.69342654456_kDouble,      3.68907921677_kDouble, &
         3.68480051108_kDouble,      3.68058821879_kDouble,      3.67644023716_kDouble, &
         3.67235456274_kDouble,      3.66832928512_kDouble,      3.66045271041_kDouble, &
         3.65279688532_kDouble,      3.64534934809_kDouble,      3.63809867295_kDouble, &
         3.63103435798_kDouble,      3.62414672762_kDouble,      3.61742684789_kDouble, &
          3.6108664521_kDouble,      3.60445787576_kDouble,      3.59819399923_kDouble, &
           3.592068197_kDouble,      3.58607429286_kDouble,      3.58020651993_kDouble, &
         3.57445948514_kDouble,      3.56882813748_kDouble,       3.5633077395_kDouble, &
          3.5578938418_kDouble,      3.55258226001_kDouble,      3.54736905409_kDouble, &
         3.54225050954_kDouble,      3.53722312046_kDouble,      3.53228357414_kDouble, &
         3.52742873704_kDouble,      3.52265564207_kDouble,      3.51796147693_kDouble, &
         3.51334357346_kDouble,      3.50879939795_kDouble,      3.50432654219_kDouble, &
         3.49992271527_kDouble,      3.49558573607_kDouble,       3.4913135263_kDouble, &
         3.48710410411_kDouble,      3.47886614237_kDouble,      3.47085771144_kDouble, &
         3.46306588016_kDouble,      3.45547879117_kDouble,      3.44808554464_kDouble, &
         3.44087609744_kDouble,       3.4338411752_kDouble,      3.42697219544_kDouble, &
         3.42026120015_kDouble,      3.41370079626_kDouble,      3.40728410324_kDouble, &
         3.40100470653_kDouble,      3.39485661617_kDouble,      3.38883422999_kDouble, &
         3.38293230069_kDouble,      3.37714590628_kDouble,      3.37147042368_kDouble, &
         3.36590150482_kDouble,      3.36043505519_kDouble,      3.35506721435_kDouble, &
         3.34979433839_kDouble,      3.34461298392_kDouble,      3.33951989355_kDouble, &
         3.33451198267_kDouble,       3.3295863274_kDouble,      3.32474015352_kDouble, &
          3.3199708264_kDouble,      3.31527584174_kDouble,      3.31065281706_kDouble, &
          3.3060994839_kDouble,       3.3016136806_kDouble,      3.29719334569_kDouble, &
         3.28854129969_kDouble,      3.28012863599_kDouble,      3.27194189999_kDouble, &
         3.26396875282_kDouble,      3.25619785067_kDouble,      3.24861874006_kDouble, &
         3.24122176661_kDouble,      3.23399799517_kDouble,      3.22693913976_kDouble, &
         3.22003750184_kDouble,      3.21328591581_kDouble,      3.20667770063_kDouble, &
          3.2002066169_kDouble,      3.19386682855_kDouble,      3.18765286865_kDouble, &
         3.18155960874_kDouble,      3.17558223129_kDouble,      3.16971620498_kDouble, &
          3.1639572623_kDouble,      3.15830137938_kDouble,      3.15274475767_kDouble, &
         3.14728380734_kDouble,      3.14191513221_kDouble,      3.13663551594_kDouble, &
         3.13144190952_kDouble,      3.12633141979_kDouble,      3.12130129892_kDouble, &
         3.11634893481_kDouble,      3.11147184224_kDouble,      3.10666765471_kDouble, &
         3.10193411699_kDouble,       3.0972690782_kDouble,      3.08813637782_kDouble, &
         3.07925420286_kDouble,      3.07060851058_kDouble,       3.0621864209_kDouble, &
          3.0539760908_kDouble,      3.04596660532_kDouble,      3.03814788252_kDouble, &
         3.03051059033_kDouble,      3.02304607359_kDouble,      3.01574628977_kDouble, &
         3.00860375216_kDouble,      3.00161147949_kDouble,       2.9947629512_kDouble, &
         2.98805206756_kDouble,      2.98147311398_kDouble,      2.97502072908_kDouble, &
         2.96868987601_kDouble,      2.96247581659_kDouble,      2.95637408809_kDouble, &
         2.95038048206_kDouble,      2.94449102535_kDouble,      2.93870196271_kDouble, &
         2.93300974113_kDouble,      2.92741099539_kDouble,      2.92190253507_kDouble, &
          2.9164813325_kDouble,      2.91114451188_kDouble,      2.90588933915_kDouble, &
         2.90071321284_kDouble,      2.89561365554_kDouble,       2.8905883061_kDouble, &
         2.88563491243_kDouble,       2.8759354898_kDouble,      2.86649931119_kDouble, &
         2.85731166746_kDouble,      2.84835906508_kDouble,      2.83962909503_kDouble, &
         2.83111031887_kDouble,      2.82279216952_kDouble,       2.8146648644_kDouble, &
         2.80671932916_kDouble,      2.79894713055_kDouble,      2.79134041701_kDouble, &
         2.78389186607_kDouble,      2.77659463754_kDouble,      2.76944233182_kDouble, &
         2.76242895259_kDouble,      2.75554887345_kDouble,      2.74879680789_kDouble, &
         2.74216778225_kDouble,      2.73565711142_kDouble,      2.72926037674_kDouble, &
         2.72297340607_kDouble,      2.71679225565_kDouble,      2.71071319364_kDouble, &
         2.70473268509_kDouble,      2.69884737824_kDouble,        2.693054092_kDouble, &
         2.68734980447_kDouble,      2.68173164244_kDouble,       2.6761968717_kDouble, &
         2.67074288817_kDouble,      2.66536720972_kDouble,      2.66006746862_kDouble, &
         2.64968685818_kDouble,      2.63958415075_kDouble,       2.6297438751_kDouble, &
         2.62015183589_kDouble,      2.61079497623_kDouble,      2.60166125833_kDouble, &
         2.59273955939_kDouble,       2.5840195806_kDouble,       2.5754917671_kDouble, &
          2.5671472376_kDouble,      2.55897772201_kDouble,      2.55097550632_kDouble, &
         2.54313338341_kDouble,      2.53544460937_kDouble,      2.52790286427_kDouble, &
         2.52050221719_kDouble,      2.51323709464_kDouble,      2.50610225225_kDouble, &
         2.49909274914_kDouble,       2.4922039248_kDouble,       2.4854313781_kDouble, &
          2.4787709483_kDouble,      2.47221869765_kDouble,       2.4657708957_kDouble, &
         2.45942400483_kDouble,      2.45317466714_kDouble,      2.44701969236_kDouble, &
         2.44095604678_kDouble,      2.43498084314_kDouble,      2.42909133123_kDouble, &
         2.42328488933_kDouble,      2.41755901624_kDouble,      2.40633953091_kDouble, &
         2.39541501107_kDouble,      2.38476910497_kDouble,      2.37438680605_kDouble, &
         2.36425430827_kDouble,      2.35435888041_kDouble,      2.34468875653_kDouble, &
         2.33523304007_kDouble,       2.3259816196_kDouble,      2.31692509462_kDouble, &
         2.30805470992_kDouble,       2.2993622974_kDouble,      2.29084022432_kDouble, &
         2.28248134715_kDouble,      2.27427897031_kDouble,      2.26622680921_kDouble, &
         2.25831895701_kDouble,      2.25054985471_kDouble,      2.24291426415_kDouble, &
         2.23540724356_kDouble,      2.22802412548_kDouble,      2.22076049665_kDouble, &
          2.2136121797_kDouble,      2.20657521654_kDouble,      2.19964585312_kDouble, &
         2.19282052555_kDouble,      2.18609584736_kDouble,      2.17946859779_kDouble, &
          2.1729357111_kDouble,       2.1664942667_kDouble,      2.16014147999_kDouble, &
         2.15387469406_kDouble,      2.14158908915_kDouble,      2.12961846912_kDouble, &
         2.11794545368_kDouble,      2.10655408816_kDouble,      2.09542969053_kDouble, &
         2.08455871841_kDouble,      2.07392865314_kDouble,      2.06352789832_kDouble, &
         2.05334569055_kDouble,      2.04337202094_kDouble,      2.03359756554_kDouble, &
         2.02401362372_kDouble,      2.01461206336_kDouble,      2.00538527192_kDouble, &
         1.99632611278_kDouble,      1.98742788593_kDouble,      1.97868429286_kDouble, &
         1.97008940476_kDouble,       1.9616376339_kDouble,      1.95332370775_kDouble, &
         1.94514264545_kDouble,      1.93708973655_kDouble,      1.92916052156_kDouble, &
         1.92135077429_kDouble,      1.91365648573_kDouble,      1.90607384923_kDouble, &
         1.89859924703_kDouble,      1.89122923782_kDouble,      1.88396054536_kDouble, &
         1.87679004798_kDouble,      1.86971476892_kDouble,      1.86273186742_kDouble, &
         1.84903246517_kDouble,      1.83567153691_kDouble,      1.82263048664_kDouble, &
         1.80989223848_kDouble,      1.79744107395_kDouble,      1.78526249044_kDouble, &
         1.77334307781_kDouble,      1.76167041036_kDouble,      1.75023295184_kDouble, &
         1.73901997173_kDouble,      1.72802147122_kDouble,      1.71722811751_kDouble, &
         1.70663118536_kDouble,      1.69622250501_kDouble,      1.68599441547_kDouble, &
         1.67593972277_kDouble,      1.66605166239_kDouble,      1.65632386534_kDouble, &
         1.64675032767_kDouble,      1.63732538277_kDouble,      1.62804367632_kDouble, &
         1.61890014354_kDouble,      1.60988998842_kDouble,      1.60100866489_kDouble, &
         1.59225185952_kDouble,       1.5836154758_kDouble,      1.57509561963_kDouble, &
         1.56668858607_kDouble,      1.55839084718_kDouble,      1.55019904079_kDouble, &
         1.54210996014_kDouble,      1.53412054435_kDouble,      1.51842914115_kDouble, &
         1.50310294313_kDouble,      1.48812189602_kDouble,      1.47346757795_kDouble, &
         1.45912302502_kDouble,      1.44507257982_kDouble,       1.4313017591_kDouble, &
           1.417797138_kDouble,      1.40454624816_kDouble,        1.391537488_kDouble, &
         1.37876004322_kDouble,      1.36620381637_kDouble,      1.35385936408_kDouble, &
         1.34171784108_kDouble,      1.32977095018_kDouble,       1.3180108973_kDouble, &
         1.30643035113_kDouble,      1.29502240671_kDouble,      1.28378055261_kDouble, &
         1.27269864119_kDouble,      1.26177086164_kDouble,      1.25099171546_kDouble, &
         1.24035599423_kDouble,      1.22985875922_kDouble,      1.21949532285_kDouble, &
         1.20926123171_kDouble,      1.19915225099_kDouble,       1.1891643502_kDouble, &
         1.17929369001_kDouble,      1.16953661021_kDouble,      1.15988961853_kDouble, &
         1.15034938038_kDouble,      1.13157655839_kDouble,      1.11319427716_kDouble, &
         1.09518065276_kDouble,      1.07751556704_kDouble,      1.06018047944_kDouble, &
         1.04315826332_kDouble,      1.02643306314_kDouble,      1.00999016925_kDouble, &
        0.993815907861_kDouble,     0.977897543941_kDouble,     0.962223195295_kDouble, &
        0.946781756301_kDouble,     0.931562830007_kDouble,     0.916556667533_kDouble, &
         0.90175411383_kDouble,     0.887146559019_kDouble,     0.872725894627_kDouble, &
        0.858484474142_kDouble,     0.844415077375_kDouble,     0.830510878205_kDouble, &
        0.816765415315_kDouble,     0.803172565598_kDouble,     0.789726519943_kDouble, &
        0.776421761148_kDouble,     0.763253043733_kDouble,     0.750215375468_kDouble, &
        0.737304000439_kDouble,     0.724514383492_kDouble,     0.711842195939_kDouble, &
        0.699283302383_kDouble,     0.686833748575_kDouble,     0.674489750196_kDouble, &
        0.650104070648_kDouble,     0.626099012346_kDouble,     0.602449453164_kDouble, &
        0.579132162256_kDouble,     0.556125593619_kDouble,     0.533409706241_kDouble, &
        0.510965806738_kDouble,     0.488776411115_kDouble,     0.466825122853_kDouble, &
        0.445096524986_kDouble,     0.423576084201_kDouble,     0.402250065322_kDouble, &
        0.381105454764_kDouble,      0.36012989179_kDouble,     0.339311606539_kDouble, &
        0.318639363964_kDouble,      0.29810241293_kDouble,     0.277690439822_kDouble, &
        0.257393526101_kDouble,     0.237202109329_kDouble,      0.21710694721_kDouble, &
        0.197099084294_kDouble,     0.177169820992_kDouble,      0.15731068461_kDouble, &
        0.137513402144_kDouble,     0.117769874579_kDouble,    0.0980721524887_kDouble, &
       0.0784124127331_kDouble,    0.0587829360689_kDouble,    0.0391760855031_kDouble, &
       0.0195842852301_kDouble,                0.0_kDouble]
    tDouble,  PARAMETER  ::  B(0:1024) = [                                              &
         468043598.745_kDouble,      454185281.982_kDouble,      441133386.786_kDouble, &
         428819269.757_kDouble,      417181876.023_kDouble,      406166717.813_kDouble, &
         395725013.783_kDouble,      385812960.329_kDouble,      376391111.851_kDouble, &
         367423851.404_kDouble,      358878936.738_kDouble,      350727109.464_kDouble, &
         342941757.343_kDouble,      335498621.458_kDouble,       328375541.45_kDouble, &
         321552233.174_kDouble,      315010094.053_kDouble,      308732032.185_kDouble, &
         302702315.899_kDouble,      296906440.935_kDouble,      291331012.915_kDouble, &
         285963643.058_kDouble,      280792855.461_kDouble,      275808004.446_kDouble, &
         270999200.737_kDouble,      266357245.389_kDouble,      261873570.517_kDouble, &
         257540186.041_kDouble,      253349631.735_kDouble,      249294933.976_kDouble, &
         245369566.664_kDouble,      241567415.856_kDouble,      237882747.698_kDouble, &
         230844652.301_kDouble,      224215968.814_kDouble,      217961855.044_kDouble, &
         212051320.023_kDouble,      206456705.678_kDouble,      201153250.091_kDouble, &
         196118717.706_kDouble,      191333084.832_kDouble,      186778271.013_kDouble, &
         182437908.646_kDouble,      178297144.629_kDouble,      174342468.981_kDouble, &
          170561566.22_kDouble,      166943186.067_kDouble,      163477030.605_kDouble, &
         160153655.481_kDouble,      156964383.183_kDouble,      153901226.667_kDouble, &
         150956821.959_kDouble,      148124368.489_kDouble,      145397576.172_kDouble, &
         142770618.331_kDouble,      140238089.759_kDouble,      137794969.238_kDouble, &
         135436586.005_kDouble,      133158589.665_kDouble,      130956923.161_kDouble, &
         128827798.432_kDouble,      126767674.455_kDouble,      124773237.414_kDouble, &
         122841382.741_kDouble,      120969198.842_kDouble,      117393074.498_kDouble, &
         114024901.787_kDouble,      110846987.361_kDouble,      107843593.262_kDouble, &
         105000673.785_kDouble,       102305653.75_kDouble,      99747240.7669_kDouble, &
         97315265.5629_kDouble,      95000545.5991_kDouble,       92794768.101_kDouble, &
         90690389.3539_kDouble,      88680547.6828_kDouble,      86758987.9953_kDouble, &
         84919996.1313_kDouble,      83158341.5649_kDouble,      81469227.2427_kDouble, &
         79848245.5433_kDouble,      78291339.5027_kDouble,      76794768.5853_kDouble, &
         75355078.3902_kDouble,       73969073.775_kDouble,      72633794.9552_kDouble, &
         71346496.2016_kDouble,      70104626.8117_kDouble,      68905814.0771_kDouble, &
         67747848.0069_kDouble,      66628667.5985_kDouble,      65546348.4768_kDouble, &
         64499091.7445_kDouble,      63485213.9074_kDouble,      62503137.7568_kDouble, &
         61551384.1017_kDouble,      59733373.2481_kDouble,      58021039.3817_kDouble, &
         56405393.2293_kDouble,      54878438.8023_kDouble,      53433039.7483_kDouble, &
         52062806.7313_kDouble,      50762002.0764_kDouble,      49525458.6675_kDouble, &
         48348510.6706_kDouble,      47226934.1194_kDouble,      46156895.7613_kDouble, &
         45134908.8531_kDouble,      44157794.8309_kDouble,      43222649.9599_kDouble, &
         42326816.2279_kDouble,       41467855.862_kDouble,      40643528.9565_kDouble, &
         39851773.7738_kDouble,      39090689.3553_kDouble,      38358520.1316_kDouble, &
         37653642.2677_kDouble,      36974551.5205_kDouble,      36319852.4159_kDouble, &
          35688248.581_kDouble,      35078534.0907_kDouble,      34489585.7054_kDouble, &
         33920355.8956_kDouble,       33369866.562_kDouble,      32837203.3696_kDouble, &
         32321510.6295_kDouble,      31821986.6654_kDouble,      31337879.6134_kDouble, &
         30413135.3113_kDouble,      29542122.4706_kDouble,      28720271.6567_kDouble, &
         27943518.2501_kDouble,      27208234.5323_kDouble,      26511172.4564_kDouble, &
         25849415.1891_kDouble,      25220335.8956_kDouble,      24621562.5335_kDouble, &
         24050947.6575_kDouble,      23506542.4223_kDouble,      22986574.1168_kDouble, &
         22489426.6833_kDouble,      22013623.7674_kDouble,      21557813.9244_kDouble, &
         21120757.6673_kDouble,      20701316.0956_kDouble,      20298440.8828_kDouble, &
         19911165.4376_kDouble,      19538597.0812_kDouble,      19179910.1071_kDouble, &
         18834339.6081_kDouble,      18501175.9757_kDouble,      18179759.9851_kDouble, &
         17869478.3966_kDouble,      17569760.0097_kDouble,      17280072.1174_kDouble, &
         16999917.3132_kDouble,      16728830.6117_kDouble,      16466376.8456_kDouble, &
         16212148.3111_kDouble,      15965762.6321_kDouble,      15495105.5155_kDouble, &
         15051783.6023_kDouble,      14633472.8806_kDouble,      14238106.0377_kDouble, &
         13863837.9303_kDouble,      13509016.4872_kDouble,      13172158.0713_kDouble, &
         12851926.5229_kDouble,      12547115.2589_kDouble,      12256631.9188_kDouble, &
         11979485.1457_kDouble,      11714773.1625_kDouble,      11461673.8654_kDouble, &
         11219436.2047_kDouble,      10987372.6619_kDouble,       10764852.663_kDouble, &
         10551296.7957_kDouble,      10346171.7177_kDouble,      10148985.6613_kDouble, &
         9959284.45525_kDouble,      9776647.99501_kDouble,      9600687.10337_kDouble, &
         9431040.73252_kDouble,      9267373.46466_kDouble,      9109373.27455_kDouble, &
         8956749.52272_kDouble,      8809231.15191_kDouble,      8666565.06313_kDouble, &
         8528514.65085_kDouble,      8394858.47935_kDouble,      8265389.08465_kDouble, &
         8139911.88833_kDouble,      7900214.37602_kDouble,       7674431.7041_kDouble, &
            7461381.19_kDouble,       7260010.7574_kDouble,      7069381.37083_kDouble, &
         6888652.23304_kDouble,       6717068.2507_kDouble,      6553949.37303_kDouble, &
          6398681.4844_kDouble,      6250708.59292_kDouble,      6109526.10467_kDouble, &
         5974675.01146_kDouble,      5845736.85046_kDouble,      5722329.31864_kDouble, &
          5604102.4449_kDouble,      5490735.23866_kDouble,      5381932.74725_kDouble, &
           5277423.465_kDouble,      5176957.04587_kDouble,      5080302.27894_kDouble, &
         4987245.29223_kDouble,      4897587.95517_kDouble,      4811146.45477_kDouble, &
         4727750.02357_kDouble,      4647239.80112_kDouble,      4569467.81256_kDouble, &
         4494296.05084_kDouble,      4421595.65018_kDouble,      4351246.14058_kDouble, &
         4283134.77418_kDouble,      4217155.91547_kDouble,      4153210.48844_kDouble, &
         4031053.45636_kDouble,      3915984.28239_kDouble,       3807400.7263_kDouble, &
         3704767.03978_kDouble,      3607605.02544_kDouble,       3515486.5018_kDouble, &
         3428026.92286_kDouble,      3344879.95083_kDouble,      3265732.81994_kDouble, &
         3190302.35977_kDouble,      3118331.57125_kDouble,      3049586.66763_kDouble, &
         2983854.50841_kDouble,       2920940.3665_kDouble,      2860665.97936_kDouble, &
         2802867.84264_kDouble,      2747395.71194_kDouble,      2694111.28361_kDouble, &
         2642887.03006_kDouble,      2593605.16898_kDouble,      2546156.74866_kDouble, &
         2500440.83456_kDouble,      2456363.78429_kDouble,      2413838.59984_kDouble, &
         2372784.34781_kDouble,      2333125.63928_kDouble,      2294792.16242_kDouble, &
         2257718.26156_kDouble,      2221842.55759_kDouble,      2187107.60483_kDouble, &
         2153459.58052_kDouble,      2120848.00323_kDouble,      2058547.45994_kDouble, &
         1999859.79463_kDouble,      1944478.13147_kDouble,        1892129.468_kDouble, &
         1842570.12107_kDouble,      1795581.88886_kDouble,      1750968.80087_kDouble, &
         1708554.35336_kDouble,      1668179.14769_kDouble,      1629698.86458_kDouble, &
          1592982.5198_kDouble,      1557910.95684_kDouble,      1524375.53953_kDouble, &
         1492277.01457_kDouble,      1461524.51863_kDouble,      1432034.70898_kDouble, &
         1403731.00015_kDouble,      1376542.89185_kDouble,      1350405.37546_kDouble, &
         1325258.40889_kDouble,      1301046.45049_kDouble,      1277718.04459_kDouble, &
         1255225.45204_kDouble,      1233524.32016_kDouble,      1212573.38728_kDouble, &
         1192334.21771_kDouble,      1172770.96356_kDouble,      1153850.15023_kDouble, &
         1135540.48296_kDouble,      1117812.67195_kDouble,      1100639.27416_kDouble, &
         1083994.54978_kDouble,      1052195.90814_kDouble,      1022240.22693_kDouble, &
         993971.022577_kDouble,      967249.079973_kDouble,      941950.131258_kDouble, &
         917962.899577_kDouble,      895187.442482_kDouble,      873533.742731_kDouble, &
         852920.504418_kDouble,      833274.120331_kDouble,      814527.782755_kDouble, &
         796620.715006_kDouble,      779497.504963_kDouble,      763107.525152_kDouble, &
         747404.426526_kDouble,      732345.695244_kDouble,      717892.263476_kDouble, &
         704008.166703_kDouble,      690660.241153_kDouble,      677817.855988_kDouble, &
         665452.675682_kDouble,      653538.448664_kDouble,      642050.818932_kDouble, &
         630967.157747_kDouble,       620266.41297_kDouble,      609928.973904_kDouble, &
         599936.549831_kDouble,      590272.060629_kDouble,      580919.538101_kDouble, &
         571864.036815_kDouble,      563091.553384_kDouble,      554588.953291_kDouble, &
         538344.816665_kDouble,      523041.548206_kDouble,       508599.29108_kDouble, &
         494946.998456_kDouble,      482021.249542_kDouble,      469765.251724_kDouble, &
         458127.995525_kDouble,      447063.535752_kDouble,      436530.377359_kDouble, &
         426490.948641_kDouble,      416911.147609_kDouble,      407759.949924_kDouble, &
         399009.068882_kDouble,      390632.659531_kDouble,      382607.060391_kDouble, &
         374910.567309_kDouble,      367523.234875_kDouble,      360426.701557_kDouble, &
         353604.035312_kDouble,      347039.596927_kDouble,       340718.91876_kDouble, &
         334628.596889_kDouble,       328756.19497_kDouble,      323090.158348_kDouble, &
         317619.737168_kDouble,      312334.917401_kDouble,      307226.358847_kDouble, &
         302285.339317_kDouble,      297503.704266_kDouble,      292873.821293_kDouble, &
         288388.538937_kDouble,      284041.149331_kDouble,      275735.234396_kDouble, &
         267910.070584_kDouble,      260524.871062_kDouble,      253543.347354_kDouble, &
         246933.104947_kDouble,       240665.13389_kDouble,      234713.377391_kDouble, &
         229054.364815_kDouble,      223666.898142_kDouble,         218531.783_kDouble, &
         213631.597053_kDouble,      208950.489813_kDouble,      204474.009027_kDouble, &
         200188.949594_kDouble,      196083.221672_kDouble,      192145.735205_kDouble, &
         188366.298503_kDouble,      184735.528953_kDouble,      181244.774164_kDouble, &
          177886.04218_kDouble,      174651.939551_kDouble,      171535.616247_kDouble, &
         168530.716557_kDouble,      165631.335218_kDouble,      162831.978148_kDouble, &
         160127.527208_kDouble,      157513.208541_kDouble,      154984.564055_kDouble, &
         152537.425701_kDouble,      150167.892222_kDouble,      147872.308115_kDouble, &
         145647.244544_kDouble,      141395.994603_kDouble,      137390.624505_kDouble, &
         133610.283442_kDouble,      130036.419482_kDouble,      126652.470773_kDouble, &
         123443.605264_kDouble,      120396.500282_kDouble,      117499.155002_kDouble, &
         114740.730226_kDouble,       112111.41094_kDouble,      109602.287952_kDouble, &
         107205.255591_kDouble,      104912.922975_kDouble,      102718.536798_kDouble, &
         100615.913924_kDouble,      98599.3823607_kDouble,      96663.7294285_kDouble, &
          94804.156116_kDouble,      93016.2367777_kDouble,      91295.8834589_kDouble, &
         89639.3142378_kDouble,      88043.0250685_kDouble,      86503.7646782_kDouble, &
         85018.5121417_kDouble,      83584.4568033_kDouble,       82198.980265_kDouble, &
         80859.6401976_kDouble,      79564.1557618_kDouble,      78310.3944557_kDouble, &
         77096.3602287_kDouble,      75920.1827217_kDouble,      74780.1075103_kDouble, &
         72601.7735874_kDouble,      70549.3244291_kDouble,      68612.0902638_kDouble, &
         66780.5772822_kDouble,      65046.3097201_kDouble,      63401.6967477_kDouble, &
         61839.9197339_kDouble,      60354.8363331_kDouble,      58940.8985361_kDouble, &
         57593.0823673_kDouble,      56306.8273413_kDouble,      55077.9841309_kDouble, &
         53902.7691783_kDouble,      52777.7251933_kDouble,      51699.6866696_kDouble, &
         50665.7496879_kDouble,      49673.2453971_kDouble,       48719.716661_kDouble, &
         47802.8974376_kDouble,      46920.6945257_kDouble,      46071.1713659_kDouble, &
         45252.5336325_kDouble,      44463.1163889_kDouble,      43701.3726106_kDouble, &
         42965.8629118_kDouble,      42255.2463268_kDouble,      41568.2720255_kDouble, &
         40903.7718523_kDouble,      40260.6535941_kDouble,      39637.8948977_kDouble, &
         39034.5377622_kDouble,      38449.6835453_kDouble,      37332.1592893_kDouble, &
         36279.1577368_kDouble,      35285.2130631_kDouble,      34345.4616221_kDouble, &
         33455.5611057_kDouble,      32611.6223983_kDouble,      31810.1518596_kDouble, &
         31048.0022185_kDouble,      30322.3306142_kDouble,      29630.5625994_kDouble, &
         28970.3611385_kDouble,      28339.5998107_kDouble,      27736.3395662_kDouble, &
         27158.8084976_kDouble,      26605.3841795_kDouble,      26074.5782033_kDouble, &
         25565.0225957_kDouble,      25075.4578572_kDouble,      24604.7224005_kDouble, &
         24151.7432007_kDouble,      23715.5274976_kDouble,      23295.1554155_kDouble, &
         22889.7733832_kDouble,      22498.5882546_kDouble,      22120.8620455_kDouble, &
         21755.9072116_kDouble,       21403.082403_kDouble,      21061.7886423_kDouble, &
         20731.4658758_kDouble,      20411.5898558_kDouble,      20101.6693198_kDouble, &
         19801.2434311_kDouble,      19227.1706446_kDouble,      18686.2100916_kDouble, &
         18175.5584758_kDouble,      17692.7212135_kDouble,      17235.4710025_kDouble, &
         16801.8128973_kDouble,      16389.9547259_kDouble,      15998.2819199_kDouble, &
         15625.3360073_kDouble,      15269.7961595_kDouble,       14930.463299_kDouble, &
         14606.2463622_kDouble,      14296.1503821_kDouble,      13999.2661181_kDouble, &
         13714.7610004_kDouble,       13441.871201_kDouble,      13179.8946694_kDouble, &
          12928.184999_kDouble,      12686.1460114_kDouble,      12453.2269616_kDouble, &
         12228.9182827_kDouble,      12012.7478009_kDouble,      11804.2773608_kDouble, &
         11603.0998103_kDouble,      11408.8363005_kDouble,      11221.1338643_kDouble, &
         11039.6632386_kDouble,      10864.1169034_kDouble,      10694.2073132_kDouble, &
         10529.6652976_kDouble,      10370.2386141_kDouble,       10215.690635_kDouble, &
         9920.35530902_kDouble,      9642.03589933_kDouble,      9379.29279296_kDouble, &
         9130.84483975_kDouble,      8895.54809344_kDouble,       8672.3778898_kDouble, &
         8460.41366569_kDouble,      8258.82604172_kDouble,      8066.86578413_kDouble, &
         7883.85433402_kDouble,      7709.17565017_kDouble,      7542.26915762_kDouble, &
          7382.6236306_kDouble,      7229.77186868_kDouble,      7083.28604825_kDouble, &
          6942.7736517_kDouble,       6807.8738919_kDouble,      6678.25456313_kDouble, &
         6553.60926028_kDouble,      6433.65491683_kDouble,      6318.12961989_kDouble, &
         6206.79066653_kDouble,      6099.41283073_kDouble,      5995.78681498_kDouble, &
         5895.71786375_kDouble,      5799.02451956_kDouble,      5705.53750473_kDouble, &
         5615.09871424_kDouble,      5527.56030707_kDouble,      5442.78388486_kDouble, &
         5360.63974837_kDouble,      5281.00622313_kDouble,      5128.82081305_kDouble, &
         4985.39281878_kDouble,      4849.98187786_kDouble,      4721.92907957_kDouble, &
         4600.64604199_kDouble,      4485.60570297_kDouble,      4376.33451913_kDouble, &
         4272.40582769_kDouble,      4173.43417369_kDouble,      4079.07044241_kDouble, &
         3988.99766684_kDouble,      3902.92740311_kDouble,      3820.59658617_kDouble, &
         3741.76479301_kDouble,      3666.21185297_kDouble,      3593.73575486_kDouble, &
          3524.1508087_kDouble,      3457.28602662_kDouble,      3392.98369304_kDouble, &
         3331.09809869_kDouble,      3271.49441705_kDouble,      3214.04770487_kDouble, &
         3158.64201089_kDouble,      3105.16957952_kDouble,      3053.53013775_kDouble, &
         3003.63025539_kDouble,      2955.38276983_kDouble,      2908.70626808_kDouble, &
         2863.52461931_kDouble,      2819.76655239_kDouble,      2777.36527337_kDouble, &
         2736.25811858_kDouble,      2657.69431628_kDouble,       2583.6451563_kDouble, &
         2513.72928085_kDouble,      2447.60726343_kDouble,      2384.97598811_kDouble, &
         2325.56391052_kDouble,       2269.1270432_kDouble,      2215.44553905_kDouble, &
         2164.32077155_kDouble,      2115.57282931_kDouble,      2069.03835784_kDouble, &
         2024.56869368_kDouble,      1982.02824566_kDouble,      1941.29308582_kDouble, &
         1902.24971907_kDouble,      1864.79400552_kDouble,      1828.83021394_kDouble, &
         1794.27018797_kDouble,      1761.03260977_kDouble,      1729.04234809_kDouble, &
         1698.22987957_kDouble,      1668.53077394_kDouble,      1639.88523494_kDouble, &
         1612.23769017_kDouble,      1585.53642374_kDouble,      1559.73324667_kDouble, &
         1534.78320059_kDouble,      1510.64429085_kDouble,      1487.27724571_kDouble, &
         1464.64529866_kDouble,       1442.7139913_kDouble,      1421.45099465_kDouble, &
         1380.81030481_kDouble,      1342.50136932_kDouble,      1306.32740443_kDouble, &
          1272.1132493_kDouble,      1239.70246899_kDouble,      1208.95491167_kDouble, &
         1179.74463906_kDouble,       1151.9581651_kDouble,      1125.49295061_kDouble, &
         1100.25611146_kDouble,      1076.16330573_kDouble,      1053.13777154_kDouble, &
         1031.10949225_kDouble,      1010.01446977_kDouble,      989.794089986_kDouble, &
         970.394566925_kDouble,      951.766454485_kDouble,      933.864216342_kDouble, &
         916.645846088_kDouble,      900.072530873_kDouble,      884.108352851_kDouble, &
         868.720023553_kDouble,      853.876647032_kDouble,      839.549508182_kDouble, &
         825.711883191_kDouble,       812.33886945_kDouble,      799.407232626_kDouble, &
         786.895268923_kDouble,      774.782680783_kDouble,      763.050464528_kDouble, &
         751.680808624_kDouble,      740.657001406_kDouble,      719.585090388_kDouble, &
         699.720033366_kDouble,      680.960086838_kDouble,      663.214678935_kDouble, &
         646.402913465_kDouble,      630.452308433_kDouble,      615.297727189_kDouble, &
         600.880468722_kDouble,      587.147490089_kDouble,      574.050739088_kDouble, &
         561.546579359_kDouble,       549.59529327_kDouble,      538.160650604_kDouble, &
         527.209533055_kDouble,      516.711606313_kDouble,      506.639032813_kDouble, &
          496.96621939_kDouble,      487.669594983_kDouble,      478.727414292_kDouble, &
         470.119583902_kDouble,      461.827507957_kDouble,      453.833950828_kDouble, &
         446.122914658_kDouble,      438.679529921_kDouble,      431.489957406_kDouble, &
         424.541300267_kDouble,      417.821524944_kDouble,      411.319389924_kDouble, &
         405.024381463_kDouble,      398.926655468_kDouble,       393.01698488_kDouble, &
         387.286711944_kDouble,      376.332318284_kDouble,      366.004045462_kDouble, &
         356.249171211_kDouble,      347.020757519_kDouble,      338.276876563_kDouble, &
         329.979957895_kDouble,      322.096235284_kDouble,       314.59527586_kDouble, &
         307.449577629_kDouble,      300.634224017_kDouble,      294.126586238_kDouble, &
         287.906065913_kDouble,       281.95387174_kDouble,      276.252825046_kDouble, &
         270.787189968_kDouble,      265.542524683_kDouble,       260.50555071_kDouble, &
         255.664037759_kDouble,      251.006702025_kDouble,      246.523116109_kDouble, &
         242.203629065_kDouble,      238.039295241_kDouble,      234.021810832_kDouble, &
         230.143457166_kDouble,      226.397049911_kDouble,      222.775893491_kDouble, &
         219.273740104_kDouble,      215.884752796_kDouble,       212.60347214_kDouble, &
         209.424786113_kDouble,      206.343902815_kDouble,      203.356325724_kDouble, &
         197.644448118_kDouble,       192.25828345_kDouble,       187.17044241_kDouble, &
         182.356537811_kDouble,      177.794783117_kDouble,      173.465653828_kDouble, &
         169.351600521_kDouble,      165.436804568_kDouble,      161.706969302_kDouble, &
         158.149140755_kDouble,      154.751553204_kDouble,      151.503495587_kDouble, &
         148.395195583_kDouble,      145.417718676_kDouble,      142.562879997_kDouble, &
         139.823167083_kDouble,      137.191672013_kDouble,      134.662031621_kDouble, &
         132.228374673_kDouble,      129.885275086_kDouble,      127.627710402_kDouble, &
         125.451024827_kDouble,      123.350896269_kDouble,      121.323306878_kDouble, &
         119.364516656_kDouble,      117.471039772_kDouble,      115.639623265_kDouble, &
         113.867227861_kDouble,      112.151010651_kDouble,      110.488309437_kDouble, &
         108.876628554_kDouble,      107.313626007_kDouble,      104.324987222_kDouble, &
         101.506311917_kDouble,      98.8433301246_kDouble,      96.3233343285_kDouble, &
         93.9349706793_kDouble,      91.6680628859_kDouble,      89.5134629529_kDouble, &
         87.4629241036_kDouble,      85.5089921298_kDouble,      83.6449121192_kDouble, &
         81.8645480786_kDouble,      80.1623134152_kDouble,      78.5331106002_kDouble, &
         76.9722786296_kDouble,      75.4755471284_kDouble,      74.0389961376_kDouble, &
         72.6590207791_kDouble,      71.3323001202_kDouble,      70.0557696658_kDouble, &
         68.8265969947_kDouble,      67.6421601273_kDouble,      66.5000282736_kDouble, &
         65.3979446612_kDouble,      64.3338111858_kDouble,      63.3056746605_kDouble, &
         62.3117144747_kDouble,      61.3502314957_kDouble,      60.4196380687_kDouble, &
         59.5184489908_kDouble,      58.6452733506_kDouble,      57.7988071362_kDouble, &
         56.9778265301_kDouble,      55.4077918373_kDouble,      53.9267643608_kDouble, &
         52.5272846644_kDouble,      51.2027090519_kDouble,      49.9471006653_kDouble, &
         48.7551376125_kDouble,       47.622035089_kDouble,      46.5434790675_kDouble, &
         45.5155695954_kDouble,      44.5347721114_kDouble,      43.5978754878_kDouble, &
         42.7019557373_kDouble,      41.8443445089_kDouble,      41.0226016526_kDouble, &
         40.2344912502_kDouble,      39.4779606118_kDouble,       38.751121818_kDouble, &
         38.0522354538_kDouble,      37.3796962368_kDouble,      36.7320202863_kDouble, &
         36.1078338186_kDouble,      35.5058630854_kDouble,      34.9249253981_kDouble, &
         34.3639211041_kDouble,      33.8218263982_kDouble,      33.2976868701_kDouble, &
         32.7906117014_kDouble,      32.2997684362_kDouble,      31.8243782605_kDouble, &
         31.3637117346_kDouble,       30.917084926_kDouble,      30.4838559023_kDouble, &
         29.6552146411_kDouble,      28.8733782988_kDouble,       28.134432563_kDouble, &
         27.4348905633_kDouble,      26.7716358691_kDouble,      26.1418743927_kDouble, &
         25.5430936131_kDouble,      24.9730278502_kDouble,      24.4296285677_kDouble, &
         23.9110388709_kDouble,      23.4155715259_kDouble,      22.9416899422_kDouble, &
         22.4879916639_kDouble,      22.0531939906_kDouble,      21.6361214127_kDouble, &
         21.2356946011_kDouble,      20.8509207292_kDouble,      20.4808849434_kDouble, &
         20.1247428261_kDouble,      19.7817137183_kDouble,      19.4510747894_kDouble, &
          19.132155759_kDouble,       18.824334187_kDouble,      18.5270312637_kDouble, &
         18.2397080369_kDouble,      17.9618620255_kDouble,      17.6930241733_kDouble, &
         17.4327561035_kDouble,      17.1806476409_kDouble,      16.9363145704_kDouble, &
         16.6993966065_kDouble,      16.4695555516_kDouble,      16.0298519219_kDouble, &
         15.6148798725_kDouble,      15.2225761964_kDouble,      14.8511026109_kDouble, &
         14.4988157995_kDouble,      14.1642421305_kDouble,      13.8460562183_kDouble, &
         13.5430626642_kDouble,      13.2541804371_kDouble,        12.97842946_kDouble, &
         12.7149190454_kDouble,      12.4628378888_kDouble,      12.2214453808_kDouble, &
         11.9900640388_kDouble,      11.7680728923_kDouble,      11.5549016866_kDouble, &
         11.3500257858_kDouble,      11.1529616813_kDouble,      10.9632630215_kDouble, &
         10.7805170944_kDouble,      10.6043417028_kDouble,      10.4343823832_kDouble, &
         10.2703099236_kDouble,      10.1118181445_kDouble,      9.95862191002_kDouble, &
         9.81045534239_kDouble,       9.6670702159_kDouble,      9.52823450908_kDouble, &
          9.3937310977_kDouble,      9.26335657253_kDouble,      9.13692016828_kDouble, &
         9.01424279167_kDouble,      8.77950188724_kDouble,       8.5579029016_kDouble, &
         8.34835265336_kDouble,      8.14987690272_kDouble,      7.96160453251_kDouble, &
         7.78275419548_kDouble,      7.61262298955_kDouble,      7.45057681007_kDouble, &
         7.29604209573_kDouble,      7.14849873828_kDouble,      7.00747396878_kDouble, &
         6.87253706654_kDouble,      6.74329476426_kDouble,       6.6193872446_kDouble, &
         6.50048464099_kDouble,      6.38628396998_kDouble,      6.27650643422_kDouble, &
         6.17089504471_kDouble,      6.06921251895_kDouble,      5.97123941835_kDouble, &
         5.87677249363_kDouble,      5.78562321153_kDouble,      5.69761644002_kDouble, &
         5.61258927241_kDouble,      5.53038997362_kDouble,      5.45087703381_kDouble, &
         5.37391831699_kDouble,      5.29939029358_kDouble,      5.22717734731_kDouble, &
         5.15717114836_kDouble,      5.08927008528_kDouble,      5.02337874941_kDouble, &
         4.89727186893_kDouble,      4.77819450111_kDouble,      4.66556389779_kDouble, &
          4.5588605707_kDouble,      4.45761989234_kDouble,      4.36142500459_kDouble, &
         4.26990080294_kDouble,      4.18270881023_kDouble,      4.09954278998_kDouble, &
         4.02012497734_kDouble,       3.9442028284_kDouble,      3.87154620636_kDouble, &
          3.8019449374_kDouble,      3.73520668068_kDouble,      3.67115506623_kDouble, &
         3.60962806223_kDouble,       3.5504765392_kDouble,      3.49356300388_kDouble, &
         3.43876047989_kDouble,      3.38595151559_kDouble,      3.33502730255_kDouble, &
         3.28588689046_kDouble,       3.2384364864_kDouble,      3.19258882806_kDouble, &
         3.14826262191_kDouble,      3.10538203853_kDouble,      3.06387625858_kDouble, &
          3.0236790634_kDouble,      2.98472846526_kDouble,       2.9469663728_kDouble, &
         2.91033828793_kDouble,       2.8747930306_kDouble,       2.8067613898_kDouble, &
         2.74251940505_kDouble,      2.68175424311_kDouble,      2.62418694186_kDouble, &
         2.56956792209_kDouble,      2.51767319702_kDouble,      2.46830115574_kDouble, &
         2.42126982168_kDouble,      2.37641450585_kDouble,         2.33358579_kDouble, &
         2.29264778675_kDouble,       2.2534766331_kDouble,      2.21595918157_kDouble, &
         2.17999185932_kDouble,      2.14547967046_kDouble,      2.11233532111_kDouble, &
         2.08047844974_kDouble,       2.0498349484_kDouble,      2.02033636247_kDouble, &
         1.99191935846_kDouble,       1.9645252511_kDouble,      1.93809958206_kDouble, &
         1.91259174386_kDouble,       1.8879546434_kDouble,      1.86414440028_kDouble, &
         1.84112007579_kDouble,      1.81884342903_kDouble,      1.79727869694_kDouble, &
         1.77639239563_kDouble,      1.75615314058_kDouble,      1.73653148369_kDouble, &
         1.71749976531_kDouble,      1.68110365251_kDouble,      1.64677447223_kDouble, &
         1.61434304574_kDouble,      1.58365847132_kDouble,      1.55458570812_kDouble, &
          1.5270035349_kDouble,      1.50080281743_kDouble,      1.47588503118_kDouble, &
         1.45216099646_kDouble,      1.42954979098_kDouble,      1.40797781157_kDouble, &
         1.38737796141_kDouble,      1.36768894386_kDouble,      1.34885464657_kDouble, &
         1.33082360294_kDouble,      1.31354851955_kDouble,       1.2969858605_kDouble, &
         1.28109548066_kDouble,       1.2658403013_kDouble,      1.25118602242_kDouble, &
         1.23710086713_kDouble,      1.22355535382_kDouble,      1.21052209274_kDouble, &
         1.19797560406_kDouble,      1.18589215458_kDouble,      1.17424961117_kDouble, &
         1.16302730872_kDouble,      1.15220593111_kDouble,      1.14176740369_kDouble, &
          1.1316947959_kDouble,      1.12197223308_kDouble,      1.11258481634_kDouble, &
         1.09476027388_kDouble,      1.07811888208_kDouble,      1.06256992542_kDouble, &
         1.04803270312_kDouble,      1.03443522136_kDouble,      1.02171308882_kDouble, &
         1.00980857969_kDouble,     0.998669835485_kDouble,     0.988250182301_kDouble, &
        0.978507544678_kDouble,      0.96940394074_kDouble,     0.960905045915_kDouble, &
        0.952979814867_kDouble,     0.945600153008_kDouble,     0.938740630449_kDouble, &
        0.932378232401_kDouble,     0.926492141047_kDouble,     0.921063544663_kDouble, &
        0.916075470469_kDouble,     0.911512638216_kDouble,     0.907361331987_kDouble, &
        0.903609288065_kDouble,     0.900245597058_kDouble,     0.897260618748_kDouble, &
        0.894645908353_kDouble,     0.892394153106_kDouble,     0.890499118227_kDouble, &
        0.888955601518_kDouble,     0.887759395947_kDouble,     0.886907259694_kDouble, &
        0.886396893265_kDouble,                0.0_kDouble]

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: V, X
    tInteger    :: N
    tLogical    :: Negative
    tInteger    :: UI, WI
    tSingle     :: UF, WF
    EQUIVALENCE (UI, UF)
    EQUIVALENCE (WI, WF)

! FLOW

    IF ((U < ZERO_DBL).OR.(U > ONE_DBL)) THEN
        CALL Handle_ErrLevel('finv_Normal2', SubModName, ErrWarning, 'U Not in [0, 1].')
        RETURN
    END IF

    IF (X >= ONE_DBL) THEN
        ResVal = fdist_XBIG
        CALL Handle_ErrLevel('finv_Normal2', SubModName, ErrWarning, 'U = 1.')
        RETURN
    ELSEIF (U <= ZERO_DBL) THEN
        ResVal = -fdist_XBIG
        CALL Handle_ErrLevel('finv_Normal2', SubModName, ErrWarning, 'U = 0.')
        RETURN
    END IF

    IF (U < HALF_DBL) THEN
        Negative = TrueVal
        UF = TWO_DBL*U
    ELSE
        Negative = FalseVal
        UF = TWO_DBL*(ONE_DBL - U)
    END IF
    WI = IAND(UI, 2147221504)
    N = SHIFTA(WI, 18) - 3040
    !   util_Assert (N <= 1024, "N >= 1024")
    IF (N < 0) THEN
        IF (Negative) THEN
            ResVal = -fdist_XBIG
            CALL Handle_ErrLevel('finv_Normal2', SubModName, ErrWarning, 'U --> 0.')
        ELSE
            ResVal = fdist_XBIG
            CALL Handle_ErrLevel('finv_Normal2', SubModName, ErrWarning, 'U --> 1.')
        END IF
        RETURN
    END IF
    V = (UF - WF) * B(N)
    X = A(N) - V * (1.414214_kDouble - V * (A(N) - 0.4714045_kDouble * (ONE_DBL + &
               TWO_DBL * A(N) * A(N)) * V))
    IF (Negative) THEN
        ResVal = -X
    ELSE
        ResVal = X
    END IF

    RETURN

END FUNCTION finv_Normal2

!******************************************************************************

MODULE FUNCTION finv_Normal3(U) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return an approximation of PhiInv(U), where Phi is the standard normal
    ! distribution function, with mean 0 and variance 1.  Uses a rational
    ! approximation giving at least 7 decimal digits of precision when
    ! 1.0e-20 < U < 1.0-1.0e-20.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: U
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble, PARAMETER  :: PLim = 1.0D-18
    tDouble, PARAMETER  :: P0 = -0.322232431088_kDouble
    tDouble, PARAMETER  :: P1 = -1.0_kDouble
    tDouble, PARAMETER  :: P2 = -0.342242088547_kDouble
    tDouble, PARAMETER  :: P3 = -0.0204231210245_kDouble
    tDouble, PARAMETER  :: P4 = -0.453642210148D-4
    tDouble, PARAMETER  :: Q0 = 0.099348462606_kDouble
    tDouble, PARAMETER  :: Q1 = 0.588581570495_kDouble
    tDouble, PARAMETER  :: Q2 = 0.531103462366_kDouble
    tDouble, PARAMETER  :: Q3 = 0.10353775285_kDouble
    tDouble, PARAMETER  :: Q4 = 0.38560700634D-2

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: P, VTemp, Y

! FLOW

    ! Returns Inverse of the cdf of the normal distribution
    ! As in P.Bratley, B.L.Fox, and L.E.Schrage. A Guide to Simulation
    ! Springer-Verlag, New York, second edition, 1987.
    ! in Figure L27
 
    P = U
    IF (P > HALF_DBL) P = ONE_DBL - P

    IF (P >= PLim) THEN
        Y = SQRT (-LOG (P * P))
        VTemp = Y + ((((Y * P4 + P3) * Y + P2) * Y + P1) * Y + P0) / &
                ((((Y * Q4 + Q3) * Y + Q2) * Y + Q1) * Y + Q0)
        IF (U < HALF_DBL) VTemp = -VTemp
        ResVal = VTemp
    ELSE
        VTemp = 8.0_kDouble
        IF (U < ZERO_DBL) THEN
            CALL Handle_ErrLevel('finv_Normal3', SubModName, ErrWarning, 'U < 0.')
            RETURN
        END IF
        IF (U < HALF_DBL) VTemp = -VTemp
        ResVal = VTemp
    END IF
    
    RETURN

END FUNCTION finv_Normal3

!******************************************************************************

MODULE FUNCTION finv_LogNormal(Mu, Sigma, U) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the inverse of the lognormal distribution.  Restriction: sigma > 0.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble,  INTENT(IN)    :: Mu, Sigma, U
    tDouble                 :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: T, V

! FLOW

    IF (Sigma <= ZERO_DBL) THEN
        CALL Handle_ErrLevel('finv_LogNormal', SubModName, ErrWarning, 'Sigma <= 0.')
        RETURN
    ELSEIF ((U < ZERO_DBL).OR.(U > ONE_DBL)) THEN
        CALL Handle_ErrLevel('finv_LogNormal', SubModName, ErrWarning, 'U Not in [0, 1].')
        RETURN
    END IF
    
    IF (DBL_EPSILON >= ONE_DBL - U) THEN
        ResVal = fdist_XINF
        CALL Handle_ErrLevel('finv_LogNormal', SubModName, ErrWarning, 'U --> 1.')
    ELSEIF (U <= ZERO_DBL) THEN
        ResVal = ZERO_DBL
    ELSE
        T = finv_Normal1 (U)
        V = Mu + Sigma * T
        IF ((T >= fdist_XBIGM).OR.(V >= DBL_MAX_EXP * num_Ln2)) THEN
            ResVal = fdist_XINF
            CALL Handle_ErrLevel('finv_LogNormal', SubModName, ErrWarning, 'U --> 1.')
        ELSEIF ((T <= -fdist_XBIGM).OR.(V <= -DBL_MAX_EXP * num_Ln2)) THEN
            ResVal = ZERO_DBL
            CALL Handle_ErrLevel('finv_LogNormal', SubModName, ErrWarning, 'U --> 0.')
        ELSE
            ResVal = EXP (Mu + Sigma * T)
        END IF
    END IF

    RETURN
    
END FUNCTION finv_LogNormal

!******************************************************************************

MODULE FUNCTION finv_JohnsonSB(Alpha, Beta, A, B, U) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the inverse of the Johnson JSB distribution.  Restriction: beta > 0,
    ! a < b.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble,  INTENT(IN)    :: Alpha, Beta, A, B, U
    tDouble                 :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: T, V

! FLOW

    IF (Beta <= ZERO_DBL) THEN
        CALL Handle_ErrLevel('finv_JohnsonSB', SubModName, ErrWarning, 'Beta <= 0.')
        RETURN
    ELSEIF (B <= A) THEN
        CALL Handle_ErrLevel('finv_JohnsonSB', SubModName, ErrWarning, 'B <= A.')
        RETURN
    ELSEIF ((U < ZERO_DBL).OR.(U > ONE_DBL)) THEN
        CALL Handle_ErrLevel('finv_JohnsonSB', SubModName, ErrWarning, 'U Not in [0, 1].')
        RETURN
    END IF

    IF (U >= ONE_DBL) THEN
        ! U == 1, in fact
        ResVal = B
    ELSEIF (U <= ZERO_DBL)  THEN
        ! U == 0, in fact
        ResVal = A
    ELSE

        T = finv_Normal1 (U)
        V = (T - Alpha) / Beta
        IF ((T >= fdist_XBIGM).OR.(V >= DBL_MAX_EXP * num_Ln2)) THEN
            ResVal = B
            CALL Handle_ErrLevel('finv_JohnsonSB', SubModName, ErrWarning, 'U --> 1.')
        ELSEIF ((T <= -fdist_XBIGM).OR.(V <= -DBL_MAX_EXP * num_Ln2)) THEN
            ResVal = A
            CALL Handle_ErrLevel('finv_JohnsonSB', SubModName, ErrWarning, 'U --> 0.')
        ELSE
            V = EXP (V)
            ResVal = (A + B * V) / (1 + V)
        END IF
    END IF

    RETURN
    
END FUNCTION finv_JohnsonSB

!******************************************************************************

MODULE FUNCTION finv_JohnsonSU(Alpha, Beta, U) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the inverse of the Johnson JSU distribution.  Restriction: beta > 0.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble,  INTENT(IN)    :: Alpha, Beta, U
    tDouble                 :: ResVal
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: T, V

! FLOW

    IF (Beta <= ZERO_DBL) THEN
        CALL Handle_ErrLevel('finv_JohnsonSU', SubModName, ErrWarning, 'Beta <= 0')
        RETURN
    ELSEIF ((U < ZERO_DBL).OR.(U > ONE_DBL)) THEN
        CALL Handle_ErrLevel('finv_JohnsonSU', SubModName, ErrWarning, 'U Not in [0, 1].')
        RETURN
    END IF
    
    IF (U >= ONE_DBL) THEN
        ResVal = fdist_XINF
        CALL Handle_ErrLevel('finv_JohnsonSU', SubModName, ErrWarning, 'U = 1.')
    ELSEIF (U <= ZERO_DBL) THEN
        ResVal = -fdist_XINF
        CALL Handle_ErrLevel('finv_JohnsonSU', SubModName, ErrWarning, 'U = 0.')
    ELSE
        T = finv_Normal1 (U)
        IF (T >= fdist_XBIGM) THEN
            ResVal = fdist_XINF
            CALL Handle_ErrLevel('finv_JohnsonSU', SubModName, ErrWarning, 'U --> 1.')
        ELSEIF (T <= -fdist_XBIGM) THEN
            ResVal = -fdist_XINF
            CALL Handle_ErrLevel('finv_JohnsonSU', SubModName, ErrWarning, 'U --> 0.')
        ELSE
            V = (T - Alpha) / Beta
            IF (V >= num_Ln2 * DBL_MAX_EXP) THEN
                ResVal = fdist_XINF
                CALL Handle_ErrLevel('finv_JohnsonSU', SubModName, ErrWarning, 'V > Ln 2 * DBL_MAX_EXP.')
            ELSEIF (V <= num_Ln2 * DBL_MIN_EXP) THEN
                ResVal = -fdist_XINF
                CALL Handle_ErrLevel('finv_JohnsonSU', SubModName, ErrWarning, 'V < Ln 2 * DBL_MIN_EXP.')
            ELSE
                ResVal = SINH (V)
            END IF
        END IF
    END IF

    RETURN
    
END FUNCTION finv_JohnsonSU

!******************************************************************************

MODULE FUNCTION finv_ChiSquare1(K, U) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return an approximation of the inverse of the chi-square distribution
    ! function with k degrees of freedom.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,  INTENT(IN) :: K
    tDouble, INTENT(IN) :: U
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble, PARAMETER  :: SQP5 = 0.70710678118654752440_kDouble
    tDouble, PARAMETER  :: Dwarf = 0.1D-15
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: Z, Arg, ZSQ, CH, SQDF

! FLOW

    ! Returns an approximation of the inverse of Chi square cdf
    ! with k degrees of freedom.
    ! As in Figure L.24 of P.Bratley, B.L.Fox, and L.E.Schrage.
    !     A Guide to Simulation Springer-Verlag,
    !       New York, second edition, 1987.

    IF ((U < ZERO_DBL).OR.(U > ONE_DBL)) THEN
        CALL Handle_ErrLevel('finv_ChiSquare1', SubModName, ErrWarning, 'U Not in [0, 1].')
        RETURN
    END IF
    
    IF (U <= ZERO_DBL) THEN
        ResVal = ZERO_DBL
    ELSEIF (U >= ONE_DBL) THEN
        ResVal = K * fdist_XBIG
        CALL Handle_ErrLevel('finv_ChiSquare1', SubModName, ErrWarning, 'U = 1.')
    ELSEIF (K == 1) THEN
        Z = finv_Normal1 ((ONE_DBL + U) / TWO_DBL)
        ResVal = (Z * Z)
    ELSE
        IF (K == 2) THEN
            Arg = ONE_DBL - U
            IF (Arg < Dwarf) Arg = Dwarf
            ResVal = (-LOG (Arg) * TWO_DBL)
        ELSE
            IF (U > Dwarf) THEN
                Z = finv_Normal1 (U)
                SQDF = SQRT (REAL(K, KIND=kDouble))
                ZSQ = Z * Z
                CH = -(((3753.0_kDouble * ZSQ + 4353.0_kDouble) * ZSQ - 289517.0_kDouble) &
                       * ZSQ - 289717.0_kDouble) * Z * SQP5 / 9185400.0_kDouble
                CH = CH / SQDF + (((12.0_kDouble * ZSQ - 243.0_kDouble) * ZSQ - 923.0_kDouble) &
                       * ZSQ + 1472.0_kDouble) / 25515.0_kDouble
                CH = CH / SQDF + ((9.0_kDouble * ZSQ + 256.0_kDouble) * ZSQ - 433.0_kDouble) &
                       * Z * SQP5 / 4860.0_kDouble
                CH = CH / SQDF - ((6.0_kDouble * ZSQ + 14.0_kDouble) * ZSQ - 32.0_kDouble) &
                        / 405.0_kDouble
                CH = CH / SQDF + (ZSQ - 7.0_kDouble) * Z * SQP5 / 9.0_kDouble
                CH = CH / SQDF + TWO_DBL * (ZSQ - ONE_DBL) / 3.0_kDouble
                CH = CH / SQDF + Z / SQP5
                ResVal = (K * (CH / SQDF + ONE_DBL))
            ELSE
                ResVal = ZERO_DBL
            END IF
        END IF
    END IF

    RETURN
    
END FUNCTION finv_ChiSquare1

!******************************************************************************

MODULE FUNCTION finv_ChiSquare2(K, U) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return an approximation of the inverse of the chi-square distribution
    ! function with k degrees of freedom.  This function is up to 20 times slower
    ! than "finv_ChiSquare1".

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,   INTENT(IN)    :: K
    tDouble,  INTENT(IN)    :: U
    tDouble                 :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble, PARAMETER  :: E = 0.5D-5       ! Precision of this approximation
    tDouble, PARAMETER  :: AA = 0.6931471805_kDouble

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: A, XX, X, C, G, CH, Q, P1, P2, T, B, S1, S2, S3, S4, S5, S6

! FLOW

    ! Returns an approximation of the inverse of Chi square cdf
    ! with k degrees of freedom.
    ! As in Figure L.23 of P.Bratley, B.L.Fox, and L.E.Schrage.
    !    A Guide to Simulation Springer-Verlag,
    !    New York, second edition, 1987.

    IF ((U < ZERO_DBL).OR.(U > ONE_DBL)) THEN
        CALL Handle_ErrLevel('finv_ChiSquare2', SubModName, ErrWarning, 'U Not in [0, 1].')
        RETURN
    END IF
    
    IF ((K < 0).OR.(U <= 0.000002_kDouble)) THEN
        ResVal = ZERO_DBL
    ELSEIF (U >= ONE_DBL) THEN
        ResVal = K * fdist_XBIG
        CALL Handle_ErrLevel('finv_ChiSquare2', SubModName, ErrWarning, 'U = 1.')
    ELSEIF (U >= 0.999998_kDouble) THEN
        ResVal = (K + 4.0_kDouble * SQRT (TWO_DBL * K))
    ELSE
        G = num2_LnGamma (K / TWO_DBL)
        XX = HALF_DBL * K
        C = XX - ONE_DBL
        IF (K >= -1.24_kDouble * LOG (U)) THEN
            IF (K <= 0.32_kDouble) THEN
                CH = 0.4_kDouble
                A = num2_LOG1P (-U)
                Q = CH
                P1 = ONE_DBL + CH * (4.67_kDouble + CH)
                P2 = CH * (6.73_kDouble + CH * (6.66_kDouble + CH))
                T = -HALF_DBL + (4.67_kDouble + TWO_DBL * CH) / P1 - &
                    (6.73_kDouble + CH * (13.32_kDouble + 3.0_kDouble * CH)) / P2
                CH = CH - (ONE_DBL - EXP (A + G + HALF_DBL * CH + C * AA) * P2 / P1) / T
                DO WHILE (ABS (Q / CH - ONE_DBL) - 0.01_kDouble > ZERO_DBL)
                    Q = CH
                    P1 = ONE_DBL + CH * (4.67_kDouble + CH)
                    P2 = CH * (6.73_kDouble + CH * (6.66_kDouble + CH))
                    T = -HALF_DBL + (4.67_kDouble + TWO_DBL * CH) / P1 - &
                        (6.73_kDouble + CH * (13.32_kDouble + 3.0_kDouble * CH)) / P2
                    CH = CH - (ONE_DBL - EXP (A + G + HALF_DBL * CH + C * AA) * P2 / P1) / T
                END DO
            ELSE
                X = finv_Normal1 (U)
                P1 = 0.222222_kDouble / K
                CH = K * ((X * SQRT (P1) + ONE_DBL - P1)**3)
                IF (CH > 2.2_kDouble * K + 6.0_kDouble) THEN
                    CH = -TWO_DBL * (num2_LOG1P (-U) - C * LOG (HALF_DBL * CH) + G)
                END IF
            END IF
        ELSE
            CH = ((U * XX * EXP (G + XX * AA))**(ONE_DBL / XX))
            IF (CH - E < ZERO_DBL) THEN
                ResVal = CH
                RETURN
            END IF
        END IF

        Q = CH
        P1 = HALF_DBL * CH
        P2 = U - fdist_Gamma (XX, 5, P1)
        IF (fdist_Gamma (XX, 5, P1) == -ONE_DBL) THEN
            ResVal = -ONE_DBL
            RETURN
        END IF

        T = P2 * EXP (XX * AA + G + P1 - C * LOG (CH))
        B = T / CH
        A = HALF_DBL * T - B * C
        S1 = (210.0_kDouble + A * (140.0_kDouble + A * (105.0_kDouble &
                            + A * (84.0_kDouble  + A * (70.0_kDouble  &
                            + 60.0_kDouble * A))))) / 420.0_kDouble
        S2 = (420.0_kDouble + A * (735.0_kDouble + A * (966.0_kDouble &
                            + A * (1141.0_kDouble + 1278.0_kDouble * A)))) / 2520.0_kDouble
        S3 = (210.0_kDouble + A * (462.0_kDouble + A * (707.0_kDouble &
                            + 932.0_kDouble * A))) / 2520.0_kDouble
        S4 = (252.0_kDouble + A * (672.0_kDouble + 1182.0_kDouble * A) &
                            + C * (294.0_kDouble + A * (889.0_kDouble  &
                            + 1740.0_kDouble * A))) / 5040.0_kDouble
        S5 = (84.0_kDouble + 264.0_kDouble * A + C * (175.0_kDouble &
                           + 606.0_kDouble * A)) / 2520.0_kDouble
        S6 = (120.0_kDouble + C * (346.0_kDouble + 127.0_kDouble * C)) / 5040.0_kDouble
        CH = CH + T * (ONE_DBL + HALF_DBL * T * S1 - B * C * (S1 - B * (S2 - B * &
                      (S3 - B * (S4 - B * (S5 - B * S6))))))

        DO WHILE (ABS (Q / CH - ONE_DBL) > E)
            Q = CH
            P1 = HALF_DBL * CH
            P2 = U - fdist_Gamma (XX, 5, P1)
            IF (fdist_Gamma (XX, 5, P1) == -ONE_DBL) THEN
                ResVal = -ONE_DBL
                RETURN
            END IF

            T = P2 * EXP (XX * AA + G + P1 - C * LOG (CH))
            B = T / CH
            A = HALF_DBL * T - B * C
            S1 = (210.0_kDouble + A * (140.0_kDouble + A * (105.0_kDouble &
                                + A * (84.0_kDouble  + A * (70.0_kDouble  &
                                + 60.0_kDouble * A))))) / 420.0_kDouble
            S2 = (420.0_kDouble + A * (735.0_kDouble + A * (966.0_kDouble &
                                + A * (1141.0_kDouble + 1278.0_kDouble * A)))) / 2520.0_kDouble
            S3 = (210.0_kDouble + A * (462.0_kDouble + A * (707.0_kDouble &
                                + 932.0_kDouble * A))) / 2520.0_kDouble
            S4 = (252.0_kDouble + A * (672.0_kDouble + 1182.0_kDouble * A) &
                                + C * (294.0_kDouble + A * (889.0_kDouble  &
                                + 1740.0_kDouble * A))) / 5040.0_kDouble
            S5 = (84.0_kDouble + 264.0_kDouble * A + C * (175.0_kDouble &
                               + 606.0_kDouble * A)) / 2520.0_kDouble
            S6 = (120.0_kDouble + C * (346.0_kDouble + 127.0_kDouble * C)) / 5040.0_kDouble
            CH = CH + T * (ONE_DBL + HALF_DBL * T * S1 - B * C * (S1 - B * (S2 - B * &
                          (S3 - B * (S4 - B * (S5 - B * S6))))))
        END DO

        ResVal = CH
    END IF

    RETURN
    
END FUNCTION finv_ChiSquare2

!******************************************************************************

MODULE FUNCTION finv_Student(N, U) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return an approximation of the inverse of the Student-t distribution
    ! function with n degrees of freedom.  Uses an approximation giving at least
    ! 5 decimal digits of precision when n >= 8 or n <= 2, and 3 decimal digits
    ! of precision when 3 <= n <= 7.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,  INTENT(IN) :: N
    tDouble, INTENT(IN) :: U
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble,  PARAMETER :: PiOvr2 = 1.5707963268_kDouble
    tDouble,  PARAMETER :: Dwarf  = 1.0D-307
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: Arg, T, VTemp, UX2, P2Tail, X, C, Y2, Y
    tDouble     :: W(1:4)

! FLOW
    
    ! Returns an approximation of the inverse of a the Student Distribution
    !     with k degrees of freedom.
    ! As in Figure L.28 of P.Bratley, B.L.Fox, and L.E.Schrage.
    !       A Guide to Simulation Springer-Verlag,
    !       New York, second edition, 1987.

    IF (N <= 0) THEN
        CALL Handle_ErrLevel('finv_Student', SubModName, ErrWarning, 'N <= 0')
        RETURN
    ELSEIF ((U < ZERO_DBL).OR.(U > ONE_DBL)) THEN
        CALL Handle_ErrLevel('finv_Student', SubModName, ErrWarning, 'U Not in [0, 1].')
        RETURN
    END IF
    
    W = ZERO_DBL
    
    IF (N == 1) THEN
        ! For 1 degree of freedom we use a special method of Standard Cauchy Inversion
        Arg = num_Pi * (ONE_DBL - U)
        IF (SIN (Arg) > Dwarf) THEN
            ResVal = (COS (Arg) / SIN (Arg))
        ELSE
            ResVal = (COS (Arg) / Dwarf)
        END IF
    ELSEIF (N == 2) THEN
        ! There is also a special method for 2 degrees of freedom
        IF (U <= HALF_DBL) THEN
            T = TWO_DBL * U
        ELSE
            T = TWO_DBL * (ONE_DBL - U)
        END IF
        IF (T <= Dwarf) T = Dwarf
        VTemp = SQRT ((TWO_DBL / (T * (TWO_DBL - T))) - TWO_DBL)
        IF (U <= HALF_DBL) THEN
            ResVal = (-VTemp)
        ELSE
            ResVal = (VTemp)
        END IF
    ELSEIF ((U <= Dwarf).OR.((ONE_DBL - U) <= Dwarf)) THEN
        T = 10.0D30
        IF (U < HALF_DBL) THEN
            ResVal = (-T)
        ELSE
            ResVal = (T)
        END IF
    ELSE

        UX2 = U * TWO_DBL

        IF (UX2 < TWO_DBL - UX2) THEN
            P2Tail = UX2
        ELSE
            P2Tail = TWO_DBL - UX2
        END IF

        W(2) = ONE_DBL / (N - HALF_DBL)
        W(1) = 48.0_kDouble / (W(2) * W(2))
        W(3) = ((20700.0_kDouble * W(2) / W(1) - 98.0_kDouble) * W(2) - 16.0_kDouble) &
             * W(2) + 96.36_kDouble
        W(4) = ((94.5_kDouble / (W(1) + W(3)) - 3.0_kDouble) / W(1) + ONE_DBL) &
             * SQRT (W(2) * PiOvr2) * N

        X = W(4) * P2Tail
        C = W(3)
        Y = X**(TWO_DBL / N)

        IF (Y <= (0.05_kDouble + W(2))) THEN
            Y2 = (N + 6.0_kDouble) / (N + Y) - 0.089_kDouble * W(4) - 0.822_kDouble
            Y2 = Y2 * (N + TWO_DBL) * 3.0_kDouble
            Y2 = (ONE_DBL / Y2 + HALF_DBL / (N + 4.0_kDouble)) * Y - ONE_DBL
            Y2 = Y2 * (N + ONE_DBL) / (N + TWO_DBL)
            Y = Y2 + ONE_DBL / Y
        ELSE
            X = finv_Normal1 (P2Tail * HALF_DBL)
            Y = X * X
            IF (N < 5) C = C + 0.3_kDouble * (N - 4.5_kDouble) * (X + 0.6_kDouble)
            C = (((0.05_kDouble * W(4) * X - 5.0_kDouble) * X - 7.0_kDouble) * X - TWO_DBL) &
              * X + W(1) + C
            Y = (((((0.4_kDouble * Y + 6.3_kDouble) * Y + 36.0_kDouble) * Y + 94.5_kDouble) &
              / C - Y - 3.0_kDouble) / W(1) + ONE_DBL) * X
            Y = W(2) * Y * Y
            IF (Y <= 0.002_kDouble) THEN
                Y = HALF_DBL * Y * Y + Y
            ELSE
                Y = EXP (Y) - ONE_DBL
            END IF
        END IF

        T = SQRT (N * Y)
        IF (U < HALF_DBL) THEN
            ResVal = (-T)
        ELSE
            ResVal = (T)
        END IF
    END IF
        
    RETURN
    
END FUNCTION finv_Student

!******************************************************************************

MODULE FUNCTION finv_BetaSymmetric(P, U) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return a special approximation of the inverse of the symmetrical beta
    ! distribution function with shape parameter p = q.  Uses four different
    ! hypergeometric series (for the four cases x close to 0 and p <= 1, x close to 0
    ! and p > 1, x close to 1/2 and p <= 1, and x close to 1/2 and p > 1) to compute
    ! the distribution u =F(x), which are then solved by Newton’s method for the
    ! solution of equations.  For p > 100000, uses a normal approximation.
    ! Restrictions: p > 0 and u in [0, 1].

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble,  INTENT(IN)    :: P, U
    tDouble                 :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble,  PARAMETER :: EPSSINGLE = 1.0D-5                       ! Limited Tolerance
    tDouble,  PARAMETER :: EPSILON   = 1.0D-15                      ! Tolerance double precision
    tDouble,  PARAMETER :: EPSBETA   = 1.0D-10                      ! < 0.75 sqrt(DBL_EPSILON)
    tInteger, PARAMETER :: MAXI      = 11                           ! Max number of Newton's iterations */
    tInteger, PARAMETER :: MAXIB     = 50                           ! Max number of bisection iterations */
    tInteger, PARAMETER :: MAXJ      = 2000                         ! Max number of terms in series */
    tDouble,  PARAMETER :: LOG2      = 0.6931471805599453_kDouble   ! Ln(2) */
    tDouble,  PARAMETER :: LOG4      = 1.3862943611198906_kDouble   ! Ln(4) */
    tDouble,  PARAMETER :: PI_2      = 1.5707963267948966_kDouble   ! Pi / 2 */
    tDouble,  PARAMETER :: SQPI_2    = 0.88622692545275801_kDouble  ! Sqrt(Pi) / 2 */
    tDouble,  PARAMETER :: ALIM1     = 100000.0_kDouble             ! Limiting alpha for normal approx. */

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: IsUpper          ! True if u > 0.5
    tDouble     :: Alpha
    tDouble     :: Temp, X, Y0, U2
    tDouble     :: B                ! Beta(alpha, alpha)
    tDouble     :: C                ! 4^(alpha-1) * Beta(alpha, alpha)
    tDouble     :: LogB             ! Ln(Beta(alpha, alpha))
    tDouble     :: LogC             ! Ln(4^(alpha-1)*Beta(alpha, alpha))
    tDouble     :: LogBUA           ! Ln(alpha * u * Beta(alpha, alpha))
    tDouble     :: LogBVA           ! Ln(4^(alpha-1)*Beta(alpha,alpha)*(0.5 - u)

! FLOW

    ! Compute the inverse of the symmetrical beta distribution.
    ! Returns a negative value on error, otherwise returns x in [0, 1].

    Alpha = P
    
    IF (Alpha <= ZERO_DBL) THEN
        CALL Handle_ErrLevel('finv_BetaSymmetric', SubModName, ErrWarning, 'P <= 0.')
        RETURN
    ELSEIF ((U < ZERO_DBL).OR.(U > ONE_DBL)) THEN
        CALL Handle_ErrLevel('finv_BetaSymmetric', SubModName, ErrWarning, 'U Not in [0, 1].')
        RETURN
    END IF
    
    IF (U == ZERO_DBL) THEN
        ResVal = ZERO_DBL
    ELSEIF (U == ONE_DBL) THEN
        ResVal = ONE_DBL
    ELSEIF (U == HALF_DBL) THEN
        ResVal = HALF_DBL
    ELSEIF (Alpha == ONE_DBL) THEN
        ! alpha = 1 is the uniform law
        ResVal = U
    ELSEIF (Alpha == HALF_DBL) THEN
        ! alpha = 1/2 is the arcsin law!
        Temp = SIN (U * PI_2)
        ResVal = Temp * Temp
    ELSEIF (Alpha > ALIM1) THEN
        ResVal = PeizerInverse (Alpha, U)
    ELSE
        IF (U > HALF_DBL) THEN
            U2 = ONE_DBL - U
            IsUpper = TrueVal
        ELSE
            U2 = U
            IsUpper = FalseVal
        END IF

        CALL fdist_CalcB4 (Alpha, B, LogB, C, LogC)

        IF (Alpha <= ONE_DBL) THEN
            ! First term of integrated series around 1/2!
            Y0 = C * (HALF_DBL - U2)
            IF (Y0 > 0.25_kDouble) THEN
                X = Inverse1 (Alpha, B * U2)
            ELSE
                X = Inverse2 (Alpha, Y0)
            END IF
        ELSE
            IF (U2 < ONE_DBL / (2.5_kDouble + 2.25_kDouble*SQRT(Alpha))) THEN
                LogBUA = LogB + LOG (U2 * Alpha)
                X = Inverse3 (Alpha, LogBUA)
            ELSE
                LogBVA = LogC - LOG2 + num2_log1p (-TWO_DBL*U2)
                X = Inverse4 (Alpha, LogBVA)
            END IF
        END IF

        IF (IsUpper) THEN
            ResVal = ONE_DBL - X - DBL_EPSILON
        ELSE
            ResVal = X
        END IF
    END IF

    RETURN
    
    CONTAINS

    FUNCTION Inverse1(Alpha, BU) RESULT(ResVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! This method is used for alpha < 1 and x close to 0.
    
        IMPLICIT NONE
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble,  INTENT(IN)    :: Alpha        ! Shape parameter
        tDouble,  INTENT(IN)    :: BU           ! u * Beta(alpha, alpha)
        tDouble                 :: ResVal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: Iter, J
        tDouble     :: Sum, Term, Poc, X, XNew

    ! FLOW

        ! First term of series
        X = (BU * Alpha)**(ONE_DBL / Alpha) 

        ! If T1/T0 is very small, neglect all terms of series except T0
        Term = Alpha * (ONE_DBL - Alpha) * X / (ONE_DBL + Alpha)
        IF (Term < EPSILON) THEN
            ResVal = X
            RETURN
        END IF

        X = BU * Alpha / (ONE_DBL + Term)
        ! Starting point of Newton's iterates
        XNew = X**(ONE_DBL / Alpha)
        Iter = 0
        DO
            Iter = Iter + 1
            X = XNew

            ! Compute the series for F(X)
            Poc = ONE_DBL
            Sum = ONE_DBL / Alpha
            J = 1
            DO
                Poc  = Poc * (X * (J - Alpha) / J)
                Term = Poc / (J + Alpha)
                Sum  = Sum + Term
                J = J + 1
                IF ((Term <= Sum * EPSILON).OR.(J >= MAXJ)) EXIT
            END DO
            Sum = Sum * (X**Alpha)

            ! Newton's method
            Term = (Sum - BU) * (X * (ONE_DBL - X))**(ONE_DBL - Alpha)
            XNew = X - Term

            IF ((ABS (Term) <= EPSILON).OR.(Iter > MAXI)) EXIT
        END DO

        ResVal = XNew

        RETURN

    END FUNCTION Inverse1

    !**************************************************************************

    FUNCTION Inverse2(Alpha, W) RESULT(ResVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! This method is used for alpha < 1 and x close to 1/2.
    
        IMPLICIT NONE
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble,  INTENT(IN)    :: Alpha    ! Shape parameter
        tDouble,  INTENT(IN)    :: W        ! (0.5 - u)B/pow(4, 1 - alpha)
        tDouble                 :: ResVal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: Iter, J
        tDouble     :: Sum, Term, Poc, Z, Y, YNew

    ! FLOW

        Term = (ONE_DBL - Alpha) * W * W * 4.0_kDouble / 3.0_kDouble
            
        ! If T1/T0 is very small, neglect all terms of series except T0
        IF (Term < EPSILON) THEN
            ResVal = HALF_DBL - W
            RETURN
        END IF

        ! Starting point of Newton's iterates
        YNew = W / (1 + Term)
        Iter = 0
        DO
            Iter = Iter + 1
            Y = YNew
            Z = 4.0_kDouble * Y * Y

            ! Compute the series for G(Y)
            Sum = ONE_DBL
            POC = ONE_DBL
            J = 1
            DO
                POC = POC * (Z * (J - Alpha) / J)
                Term = POC / (2 * J + 1)
                Sum = Sum + Term
                J = J + 1
                IF ((Term <= Sum * EPSILON).OR.(J >= MAXJ)) EXIT
            END DO
            Sum = Sum * Y

            ! Newton's method
            Term = (Sum - W) * ((ONE_DBL - Z)**(ONE_DBL - Alpha))
            YNew = Y - Term

            IF ((ABS (Term) <= EPSILON).OR.(Iter > MAXI)) EXIT
        END DO

        ResVal = HALF_DBL - YNew

        RETURN

    END FUNCTION Inverse2

    !**************************************************************************

    FUNCTION Inverse3(Alpha, LogBUA) RESULT(ResVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! This method is used for alpha > 1 and x close to 0.
    
        IMPLICIT NONE
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble,  INTENT(IN)    :: Alpha    ! Shape parameter
        tDouble,  INTENT(IN)    :: LogBUA   ! Ln(alpha * u * Beta(alpha, alpha))
        tDouble                 :: ResVal
        
    !** SUBROUTINE PARAMETER DECLARATIONS:
        ! For alpha <= 100000 and u < 1.0/(2.5 + 2.25*sqrt(alpha)), X0 is always
        ! to the right of the solution, so Newton is certain to converge.
        tDouble, PARAMETER  :: X0 = 0.497_kDouble

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: Iter, J
        tDouble     :: Sum, Term, Z, X, W, XNew, Eps

    ! FLOW

        Eps = EPSSINGLE
        
        ! Compute starting point of Newton's iterates
        W = LogBUA / Alpha
        X = EXP (W)
        Term = (num2_LOG1P(-X) + LogBUA) / Alpha
        Z = EXP (Term)
        IF (Z >= 0.25_kDouble) THEN
            XNew = X0
        ELSEIF (Z > 1.0D-6) THEN
            XNew = (ONE_DBL - SQRT(ONE_DBL - 4.0_kDouble*Z)) / TWO_DBL
        ELSE
            XNew = Z
        END IF

        Iter = 0
        DO
            Iter = Iter + 1
            IF (XNew >= HALF_DBL) XNew = X0
            X = XNew

            W = LogBUA - (Alpha - ONE_DBL) * LOG (X * (ONE_DBL - X))
            IF (ABS (W) >= DBL_MAX_EXP * LOG2) THEN
                XNew = X0
                CYCLE
            END IF
            W = EXP (W)
            Z = -X / (ONE_DBL - X)

            ! Compute the series for F(X)
            Term = ONE_DBL
            Sum = ONE_DBL
            J = 1
            DO
                Term = Term * (Z * (J - Alpha) / (J + Alpha))
                Sum = Sum + Term
                J = J + 1
                IF ((ABS (Term/Sum) <= Eps).OR.(J >= MAXJ)) EXIT
            END DO
            Sum = Sum * X

            ! Newton's method
            Term = (Sum - W) / Alpha
            XNew = X - Term
            IF (ABS(Term) < 32.0_kDouble*EPSSINGLE) Eps = EPSILON

            IF (.NOT.((ABS (XNew - X) > EPSILON).AND.(ABS (XNew - X) > Sum * EPSILON).AND. &
                (Iter <= MAXI))) EXIT
        END DO

        ! If Newton has not converged with enough precision, call bisection
        ! method. It is very slow, but will be called very rarely.
        IF ((Iter >= MAXI).AND.(ABS (XNew - X) > 10.0_kDouble * EPSILON)) THEN
            ResVal = Bisection (Alpha, LogBUA, ZERO_DBL, HALF_DBL)
        ELSE
            ResVal = XNew
        END IF

        RETURN

    END FUNCTION Inverse3

    !**************************************************************************

    FUNCTION Inverse4(Alpha, LogBVA) RESULT(ResVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! This method is used for alpha > 1 and x close to 1/2.
    
        IMPLICIT NONE
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble,  INTENT(IN)    :: Alpha        ! Shape parameter
        tDouble,  INTENT(IN)    :: LogBVA       ! Ln(B) + Ln(1/2 - u) + (alpha - 1)*Ln(4)
        tDouble                 :: ResVal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: Iter, J
        tDouble     :: Sum, Term, Y, YNew, Z, Eps

    ! FLOW

        Eps = EPSSINGLE

        ! Starting point of Newton's iterates
        YNew = EXP (LogBVA)

        Iter = 0
        DO
            Iter = Iter + 1
            Y = YNew

            ! Compute the series for G(Y)
            Z = 4.0_kDouble * Y * Y
            Sum = ONE_DBL
            Term = ONE_DBL 
            J = 1
            DO
                Term = Term * (Z * (J + Alpha - HALF_DBL) / (HALF_DBL + J))
                Sum = Sum + Term
                J = J + 1
                IF ((Term <= Sum * Eps).OR.(J >= MAXJ)) EXIT
            END DO
            Sum = Sum * (Y * (ONE_DBL - Z))

            ! Newton's method
            Term = Sum - EXP (LogBVA - (Alpha - ONE_DBL) * num2_LOG1P (-Z))
            YNew = Y - Term
            IF (ABS(Term) < 32.0_kDouble*EPSSINGLE) Eps = EPSILON

            IF (.NOT.((ABS (YNew - Y) > EPSILON).AND.(ABS (YNew - Y) > Sum*EPSILON) &
                                                .AND.(Iter <= MAXI))) EXIT
        END DO

        ResVal = HALF_DBL - YNew

        RETURN

    END FUNCTION Inverse4

    !**************************************************************************

    FUNCTION PeizerInverse(Alpha, U) RESULT(ResVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute Inverse of the normal approximation of Peizer and Pratt.

        IMPLICIT NONE
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble,  INTENT(IN)    :: Alpha, U
        tDouble                 :: ResVal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tDouble     :: T1, T3, XPrev, C2, Z, X, Y
        tInteger    :: Iter

    ! FLOW
        
        C2 = Alpha - ONE_DBL / 3.0_kDouble + 0.025_kDouble / Alpha
        Z = finv_Normal1 (U)
        X = HALF_DBL
        Y = ONE_DBL - X
        Iter = 0

        DO
            Iter = Iter + 1
            T1 = (TWO_DBL * Alpha - 5.0_kDouble / 6.0_kDouble) * X * Y
            T3 = ONE_DBL - Y * fdist_belog (TWO_DBL * X) - X * fdist_belog (TWO_DBL * Y)
            XPrev = X
            X = HALF_DBL + HALF_DBL * Z * SQRT(T1 / T3) / C2
            Y = ONE_DBL - X
            IF ((Iter > MAXI).OR.(ABS (X - XPrev) <= EPSBETA)) EXIT
        END DO

        ResVal = X

        RETURN

    END FUNCTION PeizerInverse

    !**************************************************************************

    FUNCTION Bisection(Alpha, LogBUA, AIn, BIn) RESULT(ResVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! The Bisection method to find a solution x.  This method is used
        ! for alpha > 1 and u very close to 0. It will rarely be called.
    
        IMPLICIT NONE
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble,  INTENT(IN)    :: Alpha    ! Shape parameter
        tDouble,  INTENT(IN)    :: LogBUA   ! Ln(alpha * u * Beta(alpha, alpha))
        tDouble,  INTENT(IN)    :: AIn      ! x is presumed in [a, b]
        tDouble,  INTENT(IN)    :: BIn      ! 
        tDouble                 :: ResVal
        
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: Iter, J
        tDouble     :: Z, Sum, Term, X, XPrev, A, B

    ! FLOW
        
        A = AIn
        B = BIn

        IF ((A >= HALF_DBL).OR.(A > B)) THEN
            A = ZERO_DBL
            B = HALF_DBL
        END IF

        X = HALF_DBL * (A + B)
        Iter = 0
        DO
            Iter = Iter + 1
            Z = -X / (ONE_DBL - X)

            ! Compute the series for F(X)
            Term = ONE_DBL
            SUM = ONE_DBL
            J = 1
            DO
                Term = Term * (Z * (J - Alpha) / (J + Alpha))
                SUM  = SUM + Term
                J = J + 1
                IF ((ABS (Term/SUM) <= EPSILON).OR.(J >= MAXJ)) EXIT
            END DO
            SUM = SUM * X

            ! Bisection method
            Z = LogBUA - (Alpha - ONE_DBL) * LOG (X * (ONE_DBL - X))
            IF (Z > LOG(SUM)) THEN
                A = X
            ELSE
                B = X
            END IF
            XPrev = X
            X = HALF_DBL * (A + B)

            IF ((ABS(XPrev - X) <= EPSILON).OR.(Iter >= MAXIB)) EXIT
        END DO

        ResVal = X

        RETURN

    END FUNCTION Bisection

    !**************************************************************************

END FUNCTION finv_BetaSymmetric

!******************************************************************************
MODULE FUNCTION finv_Geometric(P, U) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the inverse of the geometric distribution.  Restriction: 0 <= p <= 1.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: P, U
    tLong               :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: INT_MAX = 2147483647_kLong   ! maximum (signed) int value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble, SAVE   :: POld = -ONE_DBL
    tDouble, SAVE   :: V    = ONE_DBL

! FLOW

    IF ((P < ZERO_DBL).OR.(P > ONE_DBL)) THEN
        CALL Handle_ErrLevel('finv_Geometric', SubModName, ErrWarning, 'P Not in [0, 1].')
        RETURN
    ELSEIF ((U < ZERO_DBL).OR.(U > ONE_DBL)) THEN
        CALL Handle_ErrLevel('finv_Geometric', SubModName, ErrWarning, 'U Not in [0, 1].')
        RETURN
    END IF

    IF (P >= ONE_DBL) THEN
        ResVal = 0_kLong
    ELSEIF (U <= ZERO_DBL) THEN
        ResVal = 0_kLong
    ELSEIF ((U >= ONE_DBL).OR.(P <= ZERO_DBL)) THEN
        ResVal = INT_MAX
    ELSE
        IF (POld /= P) THEN
            POld = P
            V = num2_LOG1P (-P)
        END IF
        ResVal = ToLong(num2_LOG1P (-U) / V)
    END IF

    RETURN
    
END FUNCTION finv_Geometric

!******************************************************************************
MODULE FUNCTION finv_GenericD(W, U) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! Uses binary search to find the inverse of a generic discrete distribution function,
    ! evaluated at u, and whose values have been precomputed and are kept inside the
    ! structure W.  The routine returns an approximation of FInv(u).
    ! Restriction: 0 <= u <= 1.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FMassInfo), TARGET, INTENT(IN) :: W
    tDouble,                 INTENT(IN) :: U
    tLong                               :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong               :: I, J, K
    tDouble             :: U2
    tDouble, POINTER    :: F(:)

! FLOW

    IF (.NOT.ALLOCATED(W%CDF)) THEN
        CALL Handle_ErrLevel('finv_GenericD', SubModName, ErrWarning, &
                          'FMassInfo has not yet been created.')
        RETURN
    ELSEIF ((U < ZERO_DBL).OR.(U > ONE_DBL)) THEN
        CALL Handle_ErrLevel('finv_GenericD', SubModName, ErrWarning, 'U Not in [0, 1].')
        RETURN
    END IF

    F => W%CDF

    ! Remember: the upper part of cdf contains the complementary distribu-
    ! tion for W%SMed < s <= W%SMax, and the lower part of cdf the
    ! distribution for W%SMin <= s <= W%SMed

    IF (U <= F(W%SMed - W%SMin)) THEN
        ! In the lower part of cfd
        IF (U <= F(0)) THEN
            ResVal = W%SMin
            RETURN
        END IF
        I = 0
        J = W%SMed - W%SMin
        DO WHILE (I < J)
            K = (I + J) / 2
            IF (U > F(K)) THEN
                I = K + 1
            ELSE
                J = K
            END IF
        END DO

    ELSE
        ! In the upper part of cdf
        U2 = 1 - U
        IF (U2 < W%CDF(W%SMax - W%SMin)) THEN
            ResVal = W%SMax
            RETURN
        END IF

        I = W%SMed - W%SMin + 1
        J = W%SMax - W%SMin
        DO WHILE (I < J)
            K = (I + J) / 2
            IF (U2 < F(K)) THEN
                I = K + 1
            ELSE
                J = K
            END IF
        END DO
    END IF

    ResVal = I + W%SMin

    NULLIFY(F)
    
    RETURN
    
END FUNCTION finv_GenericD

!******************************************************************************
    MODULE FUNCTION finv_GenericC(F, Par, U, D) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! Uses binary search to find the inverse of a generic continuous distribution function F,
    ! evaluated at u.  The parameters of F (if any) are passed in the array par.  The returned
    ! value has d decimal digits of precision.
    ! Restriction: 0 <= u <= 1 and d > 0.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    PROCEDURE(wdist_CFUNC)  :: F
    tDouble,  INTENT(IN)    :: Par(0:)
    tDouble,  INTENT(IN)    :: U
    tInteger, INTENT(IN)    :: D
    tLong                   :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: MaxIter = 100    ! Maximum number of iterations
    tInteger, PARAMETER :: DBL_DIG = 15     ! maximum number of decimal digits of precision

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: FInit
    tLong       :: Iter
    tDouble     :: Eps, X, Y, Yb, Ya, Xb, Xa

! FLOW
    
    IF ((U < ZERO_DBL).OR.(U > ONE_DBL)) THEN
        CALL Handle_ErrLevel('finv_GenericC', SubModName, ErrWarning, 'U Not in [0, 1].')
        RETURN
    ELSEIF ((D <= 0).OR.(D > DBL_DIG)) THEN
        CALL Handle_ErrLevel('finv_GenericC', SubModName, ErrWarning, 'D Not in [1, 15].')
        RETURN
    END IF

    Eps = EpsArray(D)   ! Absolute precision
    Y = ONE_DBL
    
    IF (U <= ZERO_DBL) THEN
        X = -DBL_MAX
        ResVal = X
    ELSEIF (U >= ONE_DBL) THEN
        X = DBL_MAX
        ResVal = X
    ELSE
        Xb = 8.0_kDouble        ! tentative upper limit
        Xa = -8.0_kDouble       ! tentative lower limit
        Yb = F (Par, Xb)
        Ya = F (Par, Xa)

        IF (Yb < Ya) THEN
            CALL Handle_ErrLevel('finv_GenericC', SubModName, ErrWarning, 'F is decreasing.')
        END IF

        ! make sure that unknown X is inside [Xa, Xb]
        DO WHILE (Yb < U)
            Xa = Xb
            Ya = Yb
            Xb = Xb * TWO_DBL
            Yb = F (Par, Xb)
        END DO
        DO WHILE (Ya > U)
            Xb = Xa
            Yb = Ya
            Xa = Xa * TWO_DBL
            Ya = F (Par, Xa)
        END DO

        Yb = Yb - U
        Ya = Ya - U

        FInit = FalseVal
        Iter = 0
        DO WHILE (.NOT.FInit)
            X = (Xa + Xb) / TWO_DBL
            Y = F (Par, X) - U
            IF ((ABS (Y) <= Eps).OR.(ABS ((Xb - Xa)/(X + DBL_EPSILON)) <= Eps)) THEN
                FInit = TrueVal
            ELSEIF (Y * Ya < ZERO_DBL) THEN
                Xb = X
            ELSE
                Xa = X
            END IF
            Iter = Iter + 1
            IF (Iter > MaxIter) THEN
                CALL Handle_ErrLevel('finv_GenericC', SubModName, ErrWarning, &
                           'Search does not seem to converge.')
                FInit = TrueVal
            END IF
        END DO
        ResVal = X
    END IF

    RETURN
    
END FUNCTION finv_GenericC

!******************************************************************************

END SUBMODULE SubBase_FInv
