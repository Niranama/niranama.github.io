
SUBMODULE (ModTest_Diehard) SubTest_OverlapSum

!** PURPOSE OF THIS MODULE:
    ! contains routines to perform "Overlapping Sums" test.

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tCharLen(80), PARAMETER :: TestDescription(11) = [                              &
        "     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", &
        "     ::             The  OVERLAPPING SUMS test                        ::", &
        "     :: Integers are floated to get a sequence U(1),U(2),... of uni-  ::", &
        "     :: form [0,1) variables.  Then overlapping sums,                 ::", &
        "     ::   S(1)=U(1)+...+U(100), S2=U(2)+...+U(101),... are formed.    ::", &
        "     :: The S's are virtually normal with a certain covariance mat-   ::", &
        "     :: rix.  A linear transformation of the S's converts them to a   ::", &
        "     :: sequence of independent standard normals, which are converted ::", &
        "     :: to uniform variables for a KSTEST. The  p-values from ten     ::", &
        "     :: KSTESTs are given still another KSTEST.                       ::", &
        "     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::"]
    tSingle,      PARAMETER :: F(0:100) =                                       &
      [ 0.0,    0.0017, 0.0132, 0.0270, 0.0406, 0.0538, 0.0665, 0.0787,         &
        0.0905, 0.1020, 0.1133, 0.1242, 0.1349, 0.1454, 0.1557, 0.1659, 0.1760, &
        0.1859, 0.1957, 0.2054, 0.2150, 0.2246, 0.2341, 0.2436, 0.2530, 0.2623, &
        0.2716, 0.2809, 0.2902, 0.2995, 0.3087, 0.3180, 0.3273, 0.3366, 0.3459, &
        0.3552, 0.3645, 0.3739, 0.3833, 0.3928, 0.4023, 0.4118, 0.4213, 0.4309, &
        0.4406, 0.4504, 0.4602, 0.4701, 0.4800, 0.4900, 0.5000, 0.5100, 0.5199, &
        0.5299, 0.5397, 0.5495, 0.5593, 0.5690, 0.5787, 0.5882, 0.5978, 0.6073, &
        0.6167, 0.6260, 0.6354, 0.6447, 0.6540, 0.6632, 0.6724, 0.6817, 0.6910, &
        0.7003, 0.7096, 0.7189, 0.7282, 0.7375, 0.7468, 0.7562, 0.7657, 0.7752, &
        0.7848, 0.7944, 0.8041, 0.8140, 0.8239, 0.8340, 0.8442, 0.8545, 0.8650, &
        0.8757, 0.8867, 0.8980, 0.9095, 0.9214, 0.9337, 0.9464, 0.9595, 0.9731, &
        0.9868, 0.9983, 1.0 ]

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

MODULE SUBROUTINE FT13_OverlapSum_FromFile(InpFileName, OutUnit, InpRec)

    ! To perform the "Overlapping Sums" test based on the specified input file

    ! arguments
    tCharLen(25),   INTENT(IN)      :: InpFileName
    tInteger,       INTENT(IN)      :: OutUnit
    CLASS(I32Data), INTENT(INOUT)   :: InpRec
    
    ! local variables
    tSingle         :: X(100), Y(100), T(199), U(100), W(10)
    tInteger        :: I, IJ, IK, J, M
    tSingle         :: A, B, H, P, PK, QQ, S

    ! execution

    ! write test description
    WRITE (*, 5000)       TestDescription(1:11)
    WRITE (OutUnit, 5000) TestDescription(1:11)

    M = 100
    InpRec%RecNum = 1
    
    DO  IK = 1, 10
        DO  IJ = 1, 100
            S = 0.0
            DO  I = 1, 199
                T(I) = InpRec%GetSample() * 0.806549E-9
                IF (I <= M) S = S + T(I)
            END DO
            Y(1) = S
            DO  J = 2, M
                Y(J) = Y(J-1) - T(J-1) + T(M+J-1)
            END DO
            !***now y(j)=z(j)+...+z(99+j)
            !*** They are virtually normal, mean 0, variance 100, but correlated.
            !**** Now a matrix transformation of the y's: x=yM, will make the
            !*** x's independent normal.
            !***The y's covariance matrix T is Toeplitz with diagonals 100,99,...2,1
            !***A Cholesky factorization of T: V'V=T provides M=V^(-1).  The
            !***covariance of x=yM is M'TM=I.
            !*** The columns of M have at most 3 non-zero elements.
            X(1) = Y(1) / SQRT(M+0.0)
            X(2) = -(M-1.0) * Y(1) / SQRT(M*(M+M-1.0)) + Y(2) * SQRT(M/(M+M-1.0))
            QQ = X(1) ** 2 + X(2) ** 2
            DO  I = 3, M
                A = M + M + 2 - I
                B = 4 * M + 2 - I - I
                X(I) = Y(1)/SQRT(A*B) - SQRT((A-1.0)/(B+2.0))*Y(I-1) + SQRT(A/B)*Y(I)
                QQ = QQ + X(I) ** 2
            END DO
            !****now the x's are a bunch of iid rnors with mean 0, variance 1.
            !***Does sum(x(i)^2) behave as ChiSquare with m deg. freedom?
            !****now convert  x(1),...,x(m) to uni's.
            DO  I = 1, M
                P = Phi(X(I))
                H = 100.0 * P
                J = H
                X(I) = F(J) + (H-J) * (F(J+1)-F(J))
            END DO
            !***test to see if the transformed x's are uniforms.
            CALL KS_Test(X, M, P)
            U(IJ) = P
        END DO
        !***Now do a KSTEST on the 100 p's from the tests for normality.
        CALL KS_Test(U, 100, PK)
        !***And a KSTEST on the 100 p's from the ChiSquare tests.
        !       call KS_Test(uu,100,pq)
        W(IK) = PK
        !       uq(ik)=pq
        WRITE (*,5200)        IK, PK
        WRITE (OutUnit, 5200) IK, PK
    END DO
    WRITE (OutUnit, 5100) InpFileName
    WRITE (*,5100)        InpFileName
    CALL KS_Test(W, 10, P)
    WRITE (OutUnit, 5300) P
    WRITE (*,5300)        P
    WRITE (*,5400)
    WRITE (OutUnit, 5400)

    RETURN

5000 FORMAT (A78)
5100 FORMAT ('   Results of the OSUM test for ', A15)
5200 FORMAT (T16, ' Test no. ',I2,'      p-value ', 2F8.6)
5300 FORMAT (T8, ' KSTEST on the above 10 p-values: ', 2F8.6)
5400 FORMAT (/ '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/ )

END SUBROUTINE FT13_OverlapSum_FromFile

!******************************************************************************

MODULE SUBROUTINE FT13_OverlapSum_FromRNG(OutUnit, RNG)

    ! To perform the "Overlapping Sums" test based on the specified generator

    ! arguments
    tInteger,       INTENT(IN)      :: OutUnit
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG
    
    ! local variables
    tCharAlloc      :: RngName
    tSingle         :: X(100), Y(100), T(199), U(100), W(10)
    tInteger        :: I, IJ, IK, J, M
    tSingle         :: A, B, H, P, PK, QQ, S

    ! execution

    RngName = RNG%GetName()

    ! write test description
    WRITE (*, 5000)       TestDescription(1:11)
    WRITE (OutUnit, 5000) TestDescription(1:11)

    M = 100
    CALL RNG%ReInit()
    
    DO  IK = 1, 10
        DO  IJ = 1, 100
            S = 0.0
            DO  I = 1, 199
                T(I) = GetSample(RNG) * 0.806549E-9
                IF (I <= M) S = S + T(I)
            END DO
            Y(1) = S
            DO  J = 2, M
                Y(J) = Y(J-1) - T(J-1) + T(M+J-1)
            END DO
            !***now y(j)=z(j)+...+z(99+j)
            !*** They are virtually normal, mean 0, variance 100, but correlated.
            !**** Now a matrix transformation of the y's: x=yM, will make the
            !*** x's independent normal.
            !***The y's covariance matrix T is Toeplitz with diagonals 100,99,...2,1
            !***A Cholesky factorization of T: V'V=T provides M=V^(-1).  The
            !***covariance of x=yM is M'TM=I.
            !*** The columns of M have at most 3 non-zero elements.
            X(1) = Y(1) / SQRT(M+0.0)
            X(2) = -(M-1.0) * Y(1) / SQRT(M*(M+M-1.0)) + Y(2) * SQRT(M/(M+M-1.0))
            QQ = X(1) ** 2 + X(2) ** 2
            DO  I = 3, M
                A = M + M + 2 - I
                B = 4 * M + 2 - I - I
                X(I) = Y(1)/SQRT(A*B) - SQRT((A-1.0)/(B+2.0))*Y(I-1) + SQRT(A/B)*Y(I)
                QQ = QQ + X(I) ** 2
            END DO
            !****now the x's are a bunch of iid rnors with mean 0, variance 1.
            !***Does sum(x(i)^2) behave as ChiSquare with m deg. freedom?
            !****now convert  x(1),...,x(m) to uni's.
            DO  I = 1, M
                P = Phi(X(I))
                H = 100.0 * P
                J = H
                X(I) = F(J) + (H-J) * (F(J+1)-F(J))
            END DO
            !***test to see if the transformed x's are uniforms.
            CALL KS_Test(X, M, P)
            U(IJ) = P
        END DO
        !***Now do a KSTEST on the 100 p's from the tests for normality.
        CALL KS_Test(U, 100, PK)
        !***And a KSTEST on the 100 p's from the ChiSquare tests.
        !       call KS_Test(uu,100,pq)
        W(IK) = PK
        !       uq(ik)=pq
        WRITE (*,5200)        IK, PK
        WRITE (OutUnit, 5200) IK, PK
    END DO
    WRITE (OutUnit, 5100) RngName
    WRITE (*,5100)        RngName
    CALL KS_Test(W, 10, P)
    WRITE (OutUnit, 5300) P
    WRITE (*,5300)        P
    WRITE (*,5400)
    WRITE (OutUnit, 5400)

    RETURN

5000 FORMAT (A78)
5100 FORMAT ('   Results of the OSUM test for ', A15)
5200 FORMAT (T16, ' Test no. ',I2,'      p-value ', 2F8.6)
5300 FORMAT (T8, ' KSTEST on the above 10 p-values: ', 2F8.6)
5400 FORMAT (/ '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/ )
     
END SUBROUTINE FT13_OverlapSum_FromRNG

!******************************************************************************

END SUBMODULE SubTest_OverlapSum
