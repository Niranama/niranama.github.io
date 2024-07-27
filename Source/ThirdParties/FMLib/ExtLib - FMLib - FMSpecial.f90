

!  These FM routines perform gamma and related functions.

!  FMBERNOULLI(N,MA)    MA = B(N)  Nth Bernoulli number

!  FMBETA(MA,MB,MC)     MC = Beta(MA,MB)

!  FMCOMB(MA,MB,MC)     MC = Combination MA choose MB  (Binomial coeff.)

!  FMEULER(MA)          MA = Euler's constant ( 0.5772156649... )     < old name: FMEULR >

!  FMFACT(MA,MB)        MB = MA Factorial  (Gamma(MA+1))

!  FMGAM(MA,MB)         MB = Gamma(MA)

!  FMIBTA(MX,MA,MB,MC)  MC = Incomplete Beta(MX,MA,MB)

!  FMIGM1(MA,MB,MC)     MC = Incomplete Gamma(MA,MB).  Lower case Gamma(a,x)

!  FMIGM2(MA,MB,MC)     MC = Incomplete Gamma(MA,MB).  Upper case Gamma(a,x)

!  FMLNGM(MA,MB)        MB = Ln(Gamma(MA))

!  FMPGAM(N,MA,MB)      MB = Polygamma(N,MA)  (Nth derivative of Psi)

!  FMPOCH(MA,N,MB)      MB = MA*(MA+1)*(MA+2)*...*(MA+N-1)  (Pochhammer)

!  FMPSI(MA,MB)         MB = Psi(MA)      (Derivative of Ln(Gamma(MA))

!  For each of these routines there is also a version available for which the argument list is the
!  same but all FM numbers are in packed format.  The packed versions have the same names except
!  'FM' is replaced by 'FP' at the start of each name.

! --------------------------------------------------------------------------------------------------

      SUBROUTINE FMARG2(KROUTN,NARGS,MA,MB,KRESLT)

!  Check the input arguments to a routine for special cases.

!  KROUTN - Name of the subroutine that was called
!  NARGS  - The number of input arguments (1 or 2)
!  MA     - First input argument
!  MB     - Second input argument (if NARGS is 2)
!  KRESLT - Result code returned to the calling routine.

      USE ModLib_FMVALS
      IMPLICIT NONE

      CHARACTER(9) :: KROUTN
      TYPE(MULTI) :: MA,MB
      INTEGER :: NARGS,KRESLT

      INTEGER :: NCATMA,NCATMB

      INTEGER, PARAMETER ::                                                     &
               KFACT(15) = (/ 12,12, 0,12, 0, 0, 8, 8, 8, 0, 0, 8, 0, 4, 4 /),  &
               KGAM(15)  = (/ 12,12, 0,12, 0, 0, 3,12, 4, 0, 0, 8, 0, 4, 4 /),  &
               KLNGM(15) = (/ 12,12, 0,12,12,12,12,12,12, 0, 0,11, 0, 0, 4 /),  &
               KPSI(15)  = (/ 12,12, 0,12, 0, 0, 4,12, 3, 0, 0, 0, 0, 0,12 /)
      INTENT (IN) :: MA,MB

      CALL FMARGS(KROUTN,NARGS,MA,MB,KRESLT)
      IF (KFLAG /= 0) RETURN

!             Check for special cases.

      CALL FMCAT(MA,NCATMA)
      NCATMB = 0
      IF (NARGS == 2) CALL FMCAT(MB,NCATMB)

      IF (KROUTN == 'FMFACT') THEN
          KRESLT = KFACT(NCATMA)
          GO TO 110
      ENDIF

      IF (KROUTN == 'FMGAM') THEN
          KRESLT = KGAM(NCATMA)
          GO TO 110
      ENDIF

      IF (KROUTN == 'FMLNGM') THEN
          KRESLT = KLNGM(NCATMA)
          GO TO 110
      ENDIF

      IF (KROUTN == 'FMPSI') THEN
          KRESLT = KPSI(NCATMA)
          GO TO 110
      ENDIF

      KRESLT = 0
      RETURN

  110 IF (KRESLT == 12) THEN
          KFLAG = -4
          CALL FMWRN2
      ENDIF
      IF (KRESLT == 3 .OR. KRESLT == 4) THEN
          IF (NCATMA == 1 .OR. NCATMA == 7 .OR. NCATMA == 9 .OR. NCATMA == 15 .OR.  &
              NCATMB == 1 .OR. NCATMB == 7 .OR. NCATMB == 9 .OR. NCATMB == 15) THEN
              KFLAG = -5
          ELSE
              KFLAG = -5
              CALL FMWRN2
          ENDIF
      ENDIF
      IF (KRESLT == 5 .OR. KRESLT == 6) THEN
          IF (NCATMA == 1 .OR. NCATMA == 7 .OR. NCATMA == 9 .OR. NCATMA == 15 .OR.  &
              NCATMB == 1 .OR. NCATMB == 7 .OR. NCATMB == 9 .OR. NCATMB == 15) THEN
              KFLAG = -6
          ELSE
              KFLAG = -6
              CALL FMWRN2
          ENDIF
      ENDIF
      RETURN
      END SUBROUTINE FMARG2

      SUBROUTINE FMBERNOULLI(N,MA)

!  MA = B(N)  where B(N) is the Nth Bernoulli number.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: N
      TYPE(MULTI) :: MA

      DOUBLE PRECISION :: B
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,K,KL,KOVUN,KR_RETRY,NDSAVE
      INTENT (IN) :: N
      INTENT (INOUT) :: MA
      TYPE(MULTI) :: MXY(4)

      IF (.NOT. ALLOCATED(MA%MP)) THEN
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MA%MP) < NDIG+2) THEN
          DEALLOCATE(MA%MP)
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NCALL = NCALL + 1
      NAMEST(NCALL) = 'BERNOULLI'
      IF (NTRACE /= 0) THEN
          CALL FMNTRI(2,N,1)
      ENDIF
      IF (MBLOGS /= MBASE) CALL FMCONS
      NDSAVE = NDIG
      IF (NCALL == 1) THEN
          K = INT(5.0/ALOGMT + 2.0 + (REAL(NDIG)*ALOGMT)**0.35/ALOGMT)
          NDIG = MAX(NDIG+K,2)
      ENDIF
      KOVUN = 0
      MXSAVE = MXEXP
      MXEXP = MXEXP2
      KR_RETRY = 0
      IF ((MOD(N,2) == 1 .AND. N > 2) .OR. N < 0) THEN
          CALL FMI2M(0,MXY(4))
          GO TO 120
      ENDIF

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF

!             For functions that sum series using Bernoulli numbers, N will normally be NUMBRN+2
!             here, or possibly 28 when NUMBRN is zero.  Check to see if this N is much larger than
!             NUMBRN and can be computed directly from the zeta(N) series without computing
!             and saving the intermediate Bernoulli numbers, otherwise call FMBERN.

      B = NDIG*LOG(DBLE(MBASE))/6.91
      IF (N > NUMBRN+100 .AND. N > B) THEN
          CALL FMI2M(1,MXY(1))
          DO J = 2, 10000
             CALL FMI2M(J,MXY(2))
             CALL FMIPWR(MXY(2),-N,MXY(3))
             CALL FMCSADD_R1(MXY(1),MXY(3))
             IF (KFLAG == 1) EXIT
          ENDDO
          CALL FMI2M(N,MXY(3))
          CALL FMFACT(MXY(3),MXY(2))
          CALL FMMPY_R1(MXY(1),MXY(2))
          CALL FMMPYI_R1(MXY(1),2*(-1)**(N/2+1))
          CALL FMPI(MXY(2))
          CALL FMMPYI_R1(MXY(2),2)
          CALL FMIPWR(MXY(2),N,MXY(3))
          CALL FMDIV(MXY(1),MXY(3),MXY(4))
      ELSE
          CALL FMI2M(1,MXY(1))
          CALL FMBERN(N,MXY(1),MXY(4))
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(4)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF

  120 CALL FMEXT2(MXY(4),MA,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMBERNOULLI

      SUBROUTINE FMBERN(N,MA,MB)

!  MB = MA*B(N)      B(N) is the Nth Bernoulli number.  (Internal routine used by special functions)

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: N
      TYPE(MULTI) :: MA,MB

!             MBERN is the array used to save Bernoulli numbers so they do not have to be
!                   re-computed on subsequent calls.

!             Only the even-numbered Bernoulli numbers are stored.
!             B(2N) starts in MBERN(N) for 2N >= 28.
!             The first few numbers have small numerators and denominators, and they are done using
!             FMMPYI and FMDIVI, and are not stored in MBERN.

      DOUBLE PRECISION :: U,UJ,X,B
      REAL (KIND(1.0D0)) :: MNEXP,MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: INTNDG,J,J2,K,KL,KOVUN,KR_RETRY,KRESLT,L,LARGE,LARGED,N2,NBOT,  &
                 NDIV,NDP,NDSAV1,NDSAV2,NDSAVE,NEXTD,NEXTN,NMPY,NSTART,NTD,NTN,NTOP,NX
      INTENT (IN) :: N,MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(5)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (NTRACE /= 0) THEN
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'FMBERN'
          CALL FMNTRI(2,N,1)
          NCALL = NCALL - 1
      ENDIF
      IF (MBLOGS /= MBASE) CALL FMCONS
      IF (ABS(MA%MP(2)) > MEXPAB) THEN
          CALL FMENT2('FMBERN   ',MA,MA,1,0,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
          IF (KRESLT /= 0) THEN
              RETURN
          ENDIF
      ELSE
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'FMBERN'
          IF (NTRACE /= 0) CALL FMNTR(2,MA,MA,1,0)
          KOVUN = 0
          IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN) KOVUN = 1
          NDSAVE = NDIG
          IF (NCALL == 1) THEN
              K = INT(5.0/ALOGMT + 2.0 + (REAL(NDIG)*ALOGMT)**0.35/ALOGMT)
              NDIG = MAX(NDIG+K,2)
          ENDIF
          MXSAVE = MXEXP
          MXEXP = MXEXP2
      ENDIF

      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL FMEQU(MA,MXY(5),NDSAVE,NDIG)

!             Check for special cases.

      IF (N >= 2 .AND. N <= 26) THEN
          CALL FMBER2(N,MXY(5),MXY(4))
          GO TO 120
      ELSE IF (N == 0) THEN
          CALL FMEQ(MXY(5),MXY(4))
          GO TO 120
      ELSE IF (N == 1) THEN
          CALL FMDIVI(MXY(5),-2,MXY(4))
          GO TO 120
      ELSE IF (MA%MP(3) == 0) THEN
          CALL FMI2M(0,MXY(4))
          GO TO 120
      ENDIF

      IF (MOD(N,2) == 1 .OR. N < 0) THEN
          CALL FMI2M(0,MXY(4))
          GO TO 120
      ELSE IF (N/2 > LMBERN) THEN
          KFLAG = -11
          CALL FMWRN2
          WRITE (KW,*) ' '
          WRITE (KW,*) ' Too many Bernoulli numbers were needed in FMBERN.'
          WRITE (KW,*) ' B(',N,') was requested, and the current maximum is B(',LMBERN*2,').'
          WRITE (KW,*) ' '
          MXEXP = MXSAVE
          NDIG = NDSAVE
          CALL FMST2M('UNKNOWN',MB)
          IF (NTRACE /= 0) CALL FMNTR(1,MB,MB,1,1)
          NCALL = NCALL - 1
          RETURN
      ENDIF

!             See if B(N) has already been computed with sufficient precision.

      N2 = N/2
      IF (MBASE == MBSBRN) THEN
          IF (N <= NUMBRN) THEN
              IF (ALLOCATED(MBERN(N2)%MP)) THEN
                  IF (SIZE(MBERN(N2)%MP) >= NDIG+2) THEN
                      IF (NDBERN(N2) >= NDIG) THEN
                          CALL FMMPY(MBERN(N2),MXY(5),MXY(4))
                          GO TO 120
                      ENDIF
                  ENDIF
              ENDIF
          ENDIF
      ELSE
          DO J = 28, NUMBRN, 2
             NDBERN(J/2) = 0
          ENDDO
          NUMBRN = 0
      ENDIF

!             Compute more Bernoulli numbers.

      X = 1.0D0
      B = DBLE(MBASE)
      NDP = 0
      DO J = 1, 80
         X = X/B
         IF ((1.0D0+X) <= 1.0D0) THEN
             NDP = J-1
             IF (NDIG <= NDP) X = 4.0D0*DPPI*DPPI
             EXIT
         ENDIF
      ENDDO
      INTNDG = INT(ALOGMX/ALOGMB + 1.0)
      NX = INT(DBLE(NDIG)*DLOGMB/DLOGTW + 2.0D0)
      NSTART = 28
      IF (MBSBRN == MBASE .AND. NUMBRN >= 28) THEN
          NSTART = NUMBRN + 2
          DO J = 28, NUMBRN, 2
             IF (SIZE(MBERN(J/2)%MP) < NDIG+3 .OR. NDBERN(J/2) < NDIG) THEN
                 NSTART = J
                 EXIT
             ENDIF
          ENDDO
      ENDIF

      DO J = NSTART, N, 2

!             Check to see if J is large enough so that the formula
!             B(J) = -B(J-2)*(J-1)*J/(2*pi)**2 can be used.

         IF (J >= NX .AND. NDIG <= NDP .AND. J > 28) THEN
             J2 = J/2
             MNEXP = MBERN(J2-1)%MP(2)
             MBERN(J2-1)%MP(2) = 0
             CALL FMM2DP(MBERN(J2-1),U)
             MBERN(J2-1)%MP(2) = MNEXP
             UJ = J
             U = -U*(UJ*UJ-UJ)/X
             NUMBRN = J
             MBSBRN = MBASE
             CALL FMDPM(U,MBERN(J2))
             MBERN(J2)%MP(2) = MBERN(J2)%MP(2) + MNEXP
             NDBERN(J2) = NDIG
             CYCLE
         ENDIF

         IF (J >= NX .AND. J > 28) THEN
             LARGE = INT(INTMAX/J)
             J2 = J/2
             NUMBRN = J
             MBSBRN = MBASE
             CALL FMPI(MXY(2))
             CALL FMSQR_R1(MXY(2))
             IF (MOD(J,4) == 0 .OR. MOD(J,4) == 1) THEN
                 IF (J < LARGE) THEN
                     L = -(J*J-J)/4
                     CALL FMCSMPYI(MBERN(J2-1),L,MXY(3))
                 ELSE
                     CALL FMCSMPYI(MBERN(J2-1),-J,MXY(3))
                     CALL FMCSMPYI_R1(MXY(3),J-1)
                     CALL FMCSDIVI_R1(MXY(3),4)
                 ENDIF
             ELSE
                 IF (J < LARGE) THEN
                     L = -(J*J-J)
                     CALL FMCSMPYI(MBERN(J2-1),L,MXY(3))
                     CALL FMCSDIVI_R1(MXY(3),4)
                 ELSE
                     CALL FMCSMPYI(MBERN(J2-1),-J,MXY(3))
                     CALL FMCSMPYI_R1(MXY(3),J-1)
                     CALL FMCSDIVI_R1(MXY(3),4)
                 ENDIF
             ENDIF
             CALL FMCSDIV(MXY(3),MXY(2),MBERN(J2))
             NDBERN(J2) = NDIG
             CYCLE
         ENDIF

!             Use the recurrence involving a sum of binomial coefficients times previous B's.

         NTOP = J + 3
         NBOT = J - 6
         LARGE = INT(INTMAX/NTOP)
         LARGED = MIN(LARGE,INT(MXBASE))
         CALL FMCMBI(NTOP,NBOT,MXY(2))
         IF (NBOT <= 26) THEN
             CALL FMBER2(NBOT,MXY(2),MXY(3))
         ELSE
             CALL FMMPY(MBERN(NBOT/2),MXY(2),MXY(3))
         ENDIF
         NDSAV1 = NDIG
         DO NBOT = J-12, 0, -6
            NTN = NBOT + 6
            NTD = NTOP - NBOT - 5
            NEXTN = NTN
            NEXTD = NTD
            IF (NBOT >= 6) THEN
                NDSAV2 = NDIG
                DO K = 1, 5
                   NEXTN = NEXTN - 1
                   NEXTD = NEXTD + 1
                   NMPY = NTN*NEXTN
                   NDIV = NTD*NEXTD
                   IF (NMPY <= LARGE .AND. NDIV <= LARGED) THEN
                       NTN = NMPY
                       NTD = NDIV
                   ELSE
                       CALL FMGCDI(NMPY,NDIV)
                       IF (NMPY <= LARGE .AND. NDIV <= LARGED) THEN
                           NTN = NMPY
                           NTD = NDIV
                       ELSE
                           NDIG = MAX(NGRD22,MIN(NDSAV2,INT(MXY(2)%MP(2))+INTNDG))
                           CALL FMCSMPYI_R1(MXY(2),NTN)
                           CALL FMCSDIVI_R1(MXY(2),NTD)
                           NTN = NEXTN
                           NTD = NEXTD
                       ENDIF
                   ENDIF
                ENDDO
                NDIG = MAX(NGRD22,MIN(NDSAV2,INT(MXY(2)%MP(2))+INTNDG))
                CALL FMCSMPYI_R1(MXY(2),NTN)
                CALL FMCSDIVI_R1(MXY(2),NTD)
                NDIG = NDSAV2
            ELSE
                CALL FMCMBI(NTOP,NBOT,MXY(2))
            ENDIF

!             Now MXY(2) is the combination NTOP choose NBOT.

            IF (NBOT <= 26) THEN
                CALL FMBER2(NBOT,MXY(2),MXY(4))
            ELSE
                CALL FMMPY(MBERN(NBOT/2),MXY(2),MXY(4))
            ENDIF
            NDIG = NDSAV1
            CALL FMCSADD_R1(MXY(3),MXY(4))
            NDIG = MAX(NGRD22,NDSAV1-INT(MXY(3)%MP(2)-MXY(4)%MP(2)))
         ENDDO

         NDIG = NDSAV1
         IF (MOD(J,6) == 4) THEN
             CALL FMI2M(NTOP,MXY(1))
             CALL FMCSDIVI(MXY(1),-6,MXY(4))
             CALL FMSUB_R2(MXY(4),MXY(3))
         ELSE
             CALL FMI2M(NTOP,MXY(1))
             CALL FMCSDIVI(MXY(1),3,MXY(4))
             CALL FMSUB_R2(MXY(4),MXY(3))
         ENDIF

         J2 = J/2
         NUMBRN = J
         MBSBRN = MBASE

         CALL FMCSMPYI_R1(MXY(3),6)
         NTN = NTOP*(NTOP-1)
         LARGE = INT(INTMAX/NTOP)
         IF (NTN > MXBASE .OR. NTOP > LARGE) THEN
             CALL FMCSDIVI_R1(MXY(3),NTOP)
             NTN = NTOP - 1
             CALL FMCSDIVI_R1(MXY(3),NTN)
             NTN = NTOP - 2
             CALL FMCSDIVI(MXY(3),NTN,MBERN(J2))
         ELSE IF (NTN > MXBASE/(NTOP-2) .OR. NTN > LARGE) THEN
             CALL FMCSDIVI_R1(MXY(3),NTN)
             NTN = NTOP - 2
             CALL FMCSDIVI(MXY(3),NTN,MBERN(J2))
         ELSE
             NTN = NTN*(NTOP-2)
             CALL FMCSDIVI(MXY(3),NTN,MBERN(J2))
         ENDIF
         NDBERN(J2) = NDIG
      ENDDO

      CALL FMMPY(MBERN(N2),MXY(5),MXY(4))

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  120 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(4)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF

      CALL FMEXT2(MXY(4),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMBERN

      SUBROUTINE FMBER2(N,MA,MB)

!  Internal routine for small Bernoulli numbers.

!  MB = MA*B(N) for N an even integer between 2 and 26.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: N
      TYPE(MULTI) :: MA,MB
      INTEGER :: N2
      INTEGER :: NBTOP(13) = (/  &
              1,   1,  1,   1,  5, -691, 7, -3617, 43867, -174611, 854513, -236364091, 8553103 /)
      INTEGER :: NBBOT(13) = (/  &
              6, -30, 42, -30, 66, 2730, 6,   510,   798,     330,    138,       2730,       6 /)
      INTENT (IN) :: N,MA
      INTENT (INOUT) :: MB

      IF (N <= 0) THEN
          CALL FMEQ(MA,MB)
          RETURN
      ELSE IF (N == 1) THEN
          CALL FMDIVI(MA,-2,MB)
          RETURN
      ELSE IF (MOD(N,2) == 1) THEN
          CALL FMI2M(0,MB)
          RETURN
      ENDIF

      N2 = N/2

      IF (N <= 26) THEN
          IF (NBTOP(N2) == 1) THEN
              CALL FMDIVI(MA,NBBOT(N2),MB)
          ELSE
              CALL FMMPYI(MA,NBTOP(N2),MB)
              CALL FMDIVI_R1(MB,NBBOT(N2))
          ENDIF
      ENDIF
      RETURN
      END SUBROUTINE FMBER2

      SUBROUTINE FMBETA(MA,MB,MC)

!  MC = beta(MA,MB).  beta(MA,MB) = gamma(MA) * gamma(MB) / gamma(MA+MB)

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC
      REAL (KIND(1.0D0)) :: MXSAVE,MZERO
      DOUBLE PRECISION :: ERR
      INTEGER :: IEXTRA,J,JR,K,K10,K11,KB,KC,KFLKB,KFLNKB,KL,KOVUN,KR_RETRY,KRESLT,  &
                 KWRNSV,N,NB,NBOT,NDSAVE,NK,NKB
      LOGICAL, EXTERNAL :: FMCOMP
      REAL :: X
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      TYPE(MULTI) :: MXY(22)

      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < NDIG+2) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IEXTRA = 0

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1 .AND. (MA%MP(2) < -NDIG .OR. MB%MP(2) < -NDIG) .AND.  &
          MA%MP(2) > (-MXEXP) .AND. MB%MP(2) > (-MXEXP)              .AND.  &
          MA%MP(2) /= MUNKNO .AND. MB%MP(2) /= MUNKNO) THEN
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          NDSAVE = NDIG
          NDIG = 2*NDIG + NGRD52
          IF (MA%MP(2) <= MB%MP(2)) THEN
              CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
              CALL FMEQU(MB,MXY(2),NDSAVE,NDIG)
          ELSE
              CALL FMEQU(MA,MXY(2),NDSAVE,NDIG)
              CALL FMEQU(MB,MXY(1),NDSAVE,NDIG)
          ENDIF
          CALL FMABS(MXY(1),MXY(3))
          CALL FMABS(MXY(2),MXY(4))
          CALL FMADD(MXY(3),MXY(4),MXY(7))
          CALL FMSQR(MXY(7),MXY(3))
          IF (MXY(3)%MP(2) <= MXY(7)%MP(2) - NDSAVE) THEN
              CALL FMI2M(1,MXY(3))
              CALL FMDIV(MXY(3),MXY(1),MXY(4))
              CALL FMDIV(MXY(3),MXY(2),MXY(6))
              IF (MXY(4)%MP(2) < MEXPOV) THEN
                  CALL FMADD(MXY(1),MXY(2),MXY(7))
                  CALL FMPI(MXY(8))
                  CALL FMSQR_R1(MXY(8))
                  CALL FMMPY(MXY(7),MXY(8),MXY(9))
                  CALL FMDIVI_R1(MXY(9),6)
                  CALL FMSUB(MXY(6),MXY(9),MXY(10))
                  CALL FMADD_R2(MXY(4),MXY(10))
                  CALL FMEQU(MXY(10),MC,NDIG,NDSAVE)
                  KFLAG = 0
                  NTRACE = J
                  KWARN = K
                  NDIG = NDSAVE
                  IF (MC%MP(2) < (-MXEXP) .OR. MC%MP(2) > MXEXP+1) GO TO 110
                  IF (NTRACE /= 0) THEN
                      NCALL = NCALL + 1
                      NAMEST(NCALL) = 'FMBETA'
                      CALL FMNTR(2,MA,MB,2,1)
                      NCALL = NCALL - 1
                  ENDIF
                  IF (NTRACE /= 0) THEN
                      NCALL = NCALL + 1
                      NAMEST(NCALL) = 'FMBETA'
                      CALL FMNTR(1,MC,MC,1,1)
                      NCALL = NCALL - 1
                  ENDIF
                  RETURN
              ENDIF
          ELSE
              CALL FMI2M(1,MXY(3))
              CALL FMSUB(MXY(2),MXY(3),MXY(4))
              IF (MXY(4)%MP(3) == 0) THEN
                  NDIG = NDSAVE
                  CALL FMDIV(MXY(3),MXY(1),MC)
                  KFLAG = 0
                  NTRACE = J
                  KWARN = K
                  IF (MC%MP(2) < (-MXEXP) .OR. MC%MP(2) > MXEXP+1) GO TO 110
                  IF (NTRACE /= 0) THEN
                      NCALL = NCALL + 1
                      NAMEST(NCALL) = 'FMBETA'
                      CALL FMNTR(2,MA,MB,2,1)
                      NCALL = NCALL - 1
                  ENDIF
                  IF (NTRACE /= 0) THEN
                      NCALL = NCALL + 1
                      NAMEST(NCALL) = 'FMBETA'
                      CALL FMNTR(1,MC,MC,1,1)
                      NCALL = NCALL - 1
                  ENDIF
                  RETURN
              ELSE
                  CALL FMDIV(MXY(3),MXY(1),MXY(4))
                  CALL FMEULR(MXY(5))
                  CALL FMPSI(MXY(2),MXY(6))
                  CALL FMADD(MXY(5),MXY(6),MXY(7))
                  CALL FMSUB(MXY(4),MXY(7),MXY(8))
                  CALL FMEQU(MXY(8),MC,NDIG,NDSAVE)
                  KFLAG = 0
                  NTRACE = J
                  KWARN = K
                  NDIG = NDSAVE
                  IF (MC%MP(2) < (-MXEXP) .OR. MC%MP(2) > MXEXP+1) GO TO 110
                  IF (NTRACE /= 0) THEN
                      NCALL = NCALL + 1
                      NAMEST(NCALL) = 'FMBETA'
                      CALL FMNTR(2,MA,MB,2,1)
                      NCALL = NCALL - 1
                  ENDIF
                  IF (NTRACE /= 0) THEN
                      NCALL = NCALL + 1
                      NAMEST(NCALL) = 'FMBETA'
                      CALL FMNTR(1,MC,MC,1,1)
                      NCALL = NCALL - 1
                  ENDIF
                  RETURN
              ENDIF
          ENDIF
          KFLAG = 0
          NTRACE = J
          KWARN = K
          NDIG = NDSAVE
      ENDIF

  110 CALL FMENT2('FMBETA   ',MA,MB,2,1,MC,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF
      KR_RETRY = 0

  120 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
      CALL FMEQU(MB,MXY(2),NDSAVE,NDIG)
      CALL FMEQ(MXY(1),MXY(21))

      JR = KROUND
      KROUND = 1
      CALL FMADD(MXY(1),MXY(2),MXY(17))
      KROUND = JR
      IF (MXY(1)%MP(3) == 0 .OR. MXY(2)%MP(3) == 0) THEN
          CALL FMST2M('UNKNOWN',MXY(22))
          KFLAG = -4
          GO TO 140
      ENDIF

!             See if any of the terms are negative integers.

      CALL FMINT(MXY(1),MXY(8))
      IF (MXY(1)%MP(1) < 0) THEN
          IF (FMCOMP(MXY(1),'==',MXY(8))) THEN
              CALL FMST2M('UNKNOWN',MXY(22))
              KFLAG = -4
              GO TO 140
          ENDIF
      ENDIF
      CALL FMINT(MXY(2),MXY(9))
      IF (MXY(2)%MP(1) < 0) THEN
          IF (FMCOMP(MXY(2),'==',MXY(9))) THEN
              CALL FMST2M('UNKNOWN',MXY(22))
              KFLAG = -4
              GO TO 140
          ENDIF
      ENDIF
      IF (FMCOMP(MXY(17),'==',MXY(1))) THEN
          IF (MXY(2)%MP(2) > MEXPAB) THEN
              CALL FMABS(MXY(2),MXY(13))
              CALL FMDPM(DLOGMB,MXY(6))
              CALL FMMPY_R2(MXY(6),MXY(13))
              J = (MXY(1)%MP(2)+1)
              CALL FMMPYI_R1(MXY(13),J)
          ELSE
              CALL FMABS(MXY(2),MXY(13))
          ENDIF
          CALL FMI2M(1,MXY(6))
          CALL FMULP(MXY(6),MXY(7))
          IF (FMCOMP(MXY(13),'<=',MXY(7))) THEN
              CALL FMGAM(MXY(2),MXY(22))
              GO TO 140
          ENDIF
      ENDIF
      IF (FMCOMP(MXY(17),'==',MXY(2))) THEN
          IF (MXY(1)%MP(2) > MEXPAB) THEN
              CALL FMABS(MXY(1),MXY(13))
              CALL FMDPM(DLOGMB,MXY(6))
              CALL FMMPY_R2(MXY(6),MXY(13))
              J = (MXY(2)%MP(2)+1)
              CALL FMMPYI_R1(MXY(13),J)
          ELSE
              CALL FMABS(MXY(1),MXY(13))
          ENDIF
          CALL FMI2M(1,MXY(6))
          CALL FMULP(MXY(6),MXY(7))
          IF (FMCOMP(MXY(13),'<=',MXY(7))) THEN
              CALL FMGAM(MXY(1),MXY(22))
              GO TO 140
          ENDIF
      ENDIF
      IF (MXY(1)%MP(2) == MEXPOV) THEN
          IF (MXY(1)%MP(1)*MXY(1)%MP(3) > 0 .AND. MXY(2)%MP(1) > 0 .AND.  &
              MXY(2)%MP(2) >= 1) THEN
              CALL FMST2M('UNDERFLOW',MXY(22))
              KFLAG = -6
              GO TO 140
          ENDIF
      ENDIF
      IF (MXY(2)%MP(2) == MEXPOV) THEN
          IF (MXY(2)%MP(1)*MXY(2)%MP(3) > 0 .AND. MXY(1)%MP(1) > 0 .AND.  &
              MXY(1)%MP(2) >= 1) THEN
              CALL FMST2M('UNDERFLOW',MXY(22))
              KFLAG = -6
              GO TO 140
          ENDIF
      ENDIF
      IF (MXY(17)%MP(3) == 0) THEN
          CALL FMI2M(0,MXY(22))
          GO TO 130
      ELSE IF (MXY(17)%MP(1) < 0) THEN
          CALL FMSUB(MXY(1),MXY(8),MXY(6))
          CALL FMSUB(MXY(2),MXY(9),MXY(13))
          CALL FMADD_R2(MXY(6),MXY(13))
          CALL FMINT(MXY(13),MXY(14))
          IF (FMCOMP(MXY(13),'==',MXY(14))) THEN
              CALL FMI2M(0,MXY(22))
              GO TO 130
          ENDIF
      ENDIF

!             See if any of the terms are small integers.

      KWRNSV = KWARN
      KWARN = 0
      CALL FMM2I(MXY(1),N)
      KFLKB = KFLAG
      CALL FMM2I(MXY(2),K)
      KFLNKB = KFLAG
      CALL FMM2I(MXY(17),NK)
      KWARN = KWRNSV
      NB = N + K - 2
      KB = N - 1
      NKB = K - 1

      IF (KFLKB == 0 .AND. KFLNKB == 0) THEN
          IF (MIN(KB,NKB) <= 200) THEN
              CALL FMCMBI(NB,KB,MXY(22))
              CALL FMI2M(N+K-1,MXY(8))
              CALL FMMPY_R1(MXY(22),MXY(8))
              CALL FMI2M(1,MXY(6))
              CALL FMDIV_R2(MXY(6),MXY(22))
              GO TO 130
          ENDIF
      ENDIF
      NBOT = 0
      IF (KFLKB == 0 .AND. N <= 200) THEN
          CALL FMEQ(MXY(2),MXY(20))
          CALL FMPOCH(MXY(20),N,MXY(5))
          CALL FMFCTI(KB,MXY(11))
          CALL FMDIV(MXY(11),MXY(5),MXY(21))
          IF (ABS(MXY(21)%MP(2)) < MXSAVE) THEN
              CALL FMEQ(MXY(21),MXY(22))
              GO TO 140
          ENDIF
          NBOT = 1
      ELSE IF (KFLNKB == 0 .AND. K <= 200) THEN
          CALL FMEQ(MXY(1),MXY(20))
          CALL FMPOCH(MXY(20),K,MXY(5))
          CALL FMFCTI(NKB,MXY(11))
          CALL FMDIV(MXY(11),MXY(5),MXY(21))
          IF (ABS(MXY(21)%MP(2)) < MXSAVE) THEN
              CALL FMEQ(MXY(21),MXY(22))
              GO TO 140
          ENDIF
          NBOT = 1
      ENDIF
      IF (NBOT == 1) THEN
          CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
          CALL FMEQU(MB,MXY(2),NDSAVE,NDIG)
          CALL FMEQ(MXY(1),MXY(21))
          CALL FMADD(MXY(1),MXY(2),MXY(17))
      ENDIF

!             General case.  Use FMGAM, unless one of the numbers is too big.  If so, use FMLNGM.

      X = ALOGMB*REAL(MXEXP)
      CALL FMSP2M(X/LOG(X),MXY(7))
      CALL FMABS(MXY(17),MXY(18))
      CALL FMABS(MXY(1),MXY(19))
      CALL FMABS(MXY(2),MXY(3))
      IF (FMCOMP(MXY(18),'>=',MXY(7)) .OR. FMCOMP(MXY(19),'>=',MXY(7)) .OR.  &
          FMCOMP(MXY(3),'>=',MXY(7))) THEN

!             See if one argument is not very large and the other is
!             much larger.  For many of these cases, Stirling's formula
!             can be used to simplify Beta and avoid cancellation.

          IF (MXY(1)%MP(2) > MXY(2)%MP(2)) THEN
              CALL FMEQ(MXY(1),MXY(10))
              CALL FMEQ(MXY(2),MXY(11))
          ELSE
              CALL FMEQ(MXY(2),MXY(10))
              CALL FMEQ(MXY(1),MXY(11))
          ENDIF
          IF (MXY(10)%MP(2) > NDIG .AND.  &
              MXY(10)%MP(2) >= MXY(11)%MP(2)+NDIG) THEN
              IF (MXY(11)%MP(1) < 0) THEN
                  IF (MXY(11)%MP(2) > NDIG) THEN
                      KFLAG = -9
                      CALL FMWRN2
                      NDIG = NDIG - IEXTRA
                      CALL FMST2M('UNKNOWN',MXY(22))
                      GO TO 140
                  ELSE
                      CALL FMI2M(2,MXY(18))
                      CALL FMEQ(MXY(11),MXY(19))
                      MXY(19)%MP(1) = -MXY(19)%MP(1)
                      CALL FMINT(MXY(19),MXY(6))
                      CALL FMMOD(MXY(6),MXY(18),MXY(12))
                      IF (MXY(12)%MP(3) == 0) THEN
                          CALL FMADD(MXY(10),MXY(11),MXY(16))
                          CALL FMLN(MXY(16),MXY(6))
                          CALL FMMPY(MXY(11),MXY(6),MXY(16))
                          CALL FMI2M(1,MXY(6))
                          CALL FMADD(MXY(11),MXY(6),MXY(17))
                          CALL FMEQ(MXY(11),MXY(20))
                          CALL FMLNGM(MXY(17),MXY(4))
                          CALL FMSUB(MXY(4),MXY(16),MXY(6))
                          CALL FMEXP(MXY(6),MXY(13))
                          CALL FMDIV_R1(MXY(13),MXY(20))
                          CALL FMEQ(MXY(13),MXY(22))
                          GO TO 130
                      ENDIF
                  ENDIF
              ENDIF
              CALL FMADD(MXY(10),MXY(11),MXY(16))
              CALL FMLN(MXY(16),MXY(6))
              CALL FMMPY(MXY(11),MXY(6),MXY(16))
              CALL FMEQ(MXY(11),MXY(20))
              CALL FMLNGM(MXY(20),MXY(17))
              CALL FMSUB(MXY(17),MXY(16),MXY(6))
              CALL FMEXP(MXY(6),MXY(13))
              CALL FMEQ(MXY(13),MXY(22))
              GO TO 130
          ENDIF

!             See if both arguments are large.  For many of these cases, Stirling's formula can be
!             used to detect cases where the result will underflow.

          CALL FMDPM(1.0D7,MXY(6))
          IF (FMCOMP(MXY(1),'>',MXY(6)) .AND. FMCOMP(MXY(2),'>',MXY(6))) THEN
              CALL FMADD(MXY(1),MXY(2),MXY(6))
              CALL FMLN(MXY(6),MXY(15))
              CALL FMMPY_R2(MXY(6),MXY(15))
              IF (MXY(15)%MP(2) /= MUNKNO .AND. MXY(15)%MP(3) /= 0)  &
                  MXY(15)%MP(1) = -MXY(15)%MP(1)
              CALL FMLN(MXY(1),MXY(6))
              CALL FMMPY_R2(MXY(1),MXY(6))
              CALL FMADD_R1(MXY(15),MXY(6))
              CALL FMLN(MXY(2),MXY(6))
              CALL FMMPY_R2(MXY(2),MXY(6))
              CALL FMADD_R1(MXY(15),MXY(6))
              CALL FMEXP(MXY(15),MXY(16))
              IF (MXY(16)%MP(2) == MEXPUN) THEN
                  CALL FMEQ(MXY(16),MXY(22))
                  GO TO 140
              ENDIF
          ENDIF

!             Compute IEXTRA, the number of extra digits required to compensate for
!             cancellation error.

          MZERO = 0
          IEXTRA = INT(MAX(MXY(17)%MP(2),MXY(1)%MP(2),MXY(2)%MP(2),MZERO))
          IF (IEXTRA > 0) THEN
              CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
              CALL FMEQU_R1(MXY(2),NDIG,NDIG+IEXTRA)
          ENDIF
          NDIG = NDIG + IEXTRA
          CALL FMADD(MXY(1),MXY(2),MXY(17))
          CALL FMI2M(1,MXY(10))
          CALL FMI2M(2,MXY(11))
          CALL FMEQ(MXY(17),MXY(22))
          K10 = 0
          K11 = 0
          KC = 0
          IF (MXY(1)%MP(1) < 0) THEN
              CALL FMINT(MXY(1),MXY(12))
              CALL FMMOD(MXY(12),MXY(11),MXY(13))
              IF (MXY(13)%MP(3) == 0) THEN
                  K10 = 1
                  CALL FMADD_R1(MXY(1),MXY(10))
              ENDIF
          ENDIF
          IF (MXY(2)%MP(1) < 0) THEN
              CALL FMINT(MXY(2),MXY(12))
              CALL FMMOD(MXY(12),MXY(11),MXY(13))
              IF (MXY(13)%MP(3) == 0) THEN
                  K11 = 1
                  CALL FMADD_R1(MXY(2),MXY(10))
              ENDIF
          ENDIF
          IF (MXY(22)%MP(1) < 0) THEN
              CALL FMINT(MXY(22),MXY(12))
              CALL FMMOD(MXY(12),MXY(11),MXY(13))
              IF (MXY(13)%MP(3) == 0) THEN
                  KC = 1
                  CALL FMADD_R1(MXY(22),MXY(10))
              ENDIF
          ENDIF
          CALL FMLNGM(MXY(1),MXY(17))
          CALL FMLNGM(MXY(2),MXY(20))
          CALL FMADD_R1(MXY(17),MXY(20))
          CALL FMLNGM(MXY(22),MXY(20))
          CALL FMSUB(MXY(17),MXY(20),MXY(6))
          CALL FMEXP(MXY(6),MXY(17))
          IF (K10 == 1 .OR. K11 == 1 .OR. KC == 1) THEN
              CALL FMI2M(1,MXY(10))
              IF (K10 == 1) THEN
                  CALL FMSUB_R1(MXY(1),MXY(10))
                  CALL FMDIV_R1(MXY(17),MXY(1))
              ENDIF
              IF (K11 == 1) THEN
                  CALL FMSUB_R1(MXY(2),MXY(10))
                  CALL FMDIV_R1(MXY(17),MXY(2))
              ENDIF
              IF (KC == 1) THEN
                  CALL FMSUB_R1(MXY(22),MXY(10))
                  CALL FMMPY_R1(MXY(17),MXY(22))
              ENDIF
          ENDIF
          CALL FMEQ(MXY(17),MXY(22))
      ELSE
          CALL FMGAM(MXY(1),MXY(20))
          CALL FMGAM(MXY(2),MXY(1))
          CALL FMGAM(MXY(17),MXY(22))
          CALL FMMPY(MXY(20),MXY(1),MXY(8))
          CALL FMDIV_R2(MXY(8),MXY(22))
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  130 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(22)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 120
      ENDIF

  140 CALL FMEXT2(MXY(22),MC,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMBETA

      SUBROUTINE FMCMBI(N,K,MA)

!  Internal routine for computing binomial coefficients for integers.

!  MA = N choose K.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: N,K
      TYPE(MULTI) :: MA

      INTEGER :: INTNDG,J,KSTART,KT,L,LARGE,LARGED,NDIV,NDSAVE,NEXTD,NEXTN,NMPY,NTD,NTN
      INTENT (IN) :: N,K
      INTENT (INOUT) :: MA

      IF (MBLOGS /= MBASE) CALL FMCONS
      L = MIN(K,N-K)
      IF (L <= 0) THEN
          CALL FMI2M(1,MA)
          RETURN
      ENDIF
      IF (L <= 1) THEN
          CALL FMI2M(N,MA)
          RETURN
      ENDIF

!             Find the largest value for N choose J using integers.

      NTN = N
      NTD = 1
      LARGE = INT(INTMAX/N)
      DO J = 2, L
         IF (NTN <= LARGE) THEN
             NTN = (NTN*((N+1)-J))/J
         ELSE
             CALL FMI2M(NTN,MA)
             NTN = (N+1) - J
             NTD = J
             GO TO 110
         ENDIF
      ENDDO

  110 IF (NTD == 1) THEN
          CALL FMI2M(NTN,MA)
          RETURN
      ENDIF

      INTNDG = INT(ALOGMX/ALOGMB + 1.0)
      NEXTN = NTN
      NEXTD = NTD
      KSTART = NTD + 1
      NDSAVE = NDIG

!             Compute the rest of N choose K.

      LARGED = MIN(LARGE,INT(MXBASE))
      DO KT = KSTART, L
         NEXTN = NEXTN - 1
         NEXTD = NEXTD + 1
         IF (NTN >= LARGE .OR. NTD >= LARGED) THEN
             NDIG = MAX(2,MIN(NDSAVE,INT(MA%MP(2))+INTNDG))
             CALL FMCSMPYI_R1(MA,NTN)
             CALL FMCSDIVI_R1(MA,NTD)
             NTN = NEXTN
             NTD = NEXTD
             CYCLE
         ENDIF
         NMPY = NTN*NEXTN
         NDIV = NTD*NEXTD
         IF (NMPY <= LARGE .AND. NDIV <= LARGED) THEN
             NTN = NMPY
             NTD = NDIV
         ELSE
             CALL FMGCDI(NMPY,NDIV)
             IF (NMPY <= LARGE .AND. NDIV <= LARGED) THEN
                 NTN = NMPY
                 NTD = NDIV
             ELSE
                 NDIG = MAX(2,MIN(NDSAVE,INT(MA%MP(2))+INTNDG))
                 CALL FMCSMPYI_R1(MA,NTN)
                 CALL FMCSDIVI_R1(MA,NTD)
                 NTN = NEXTN
                 NTD = NEXTD
             ENDIF
         ENDIF
      ENDDO
      NDIG = MAX(2,MIN(NDSAVE,INT(MA%MP(2))+INTNDG))
      CALL FMGCDI(NTN,NTD)
      CALL FMCSMPYI_R1(MA,NTN)
      CALL FMCSDIVI_R1(MA,NTD)
      NDIG = NDSAVE

      RETURN
      END SUBROUTINE FMCMBI

      SUBROUTINE FMCOMB(MA,MB,MC)

!  MC = MA choose MB.  (Binomial coefficient -- uses gamma for non-integers)

!  MC = (MA)! / ( (MB)! * (MA-MB)! )

!  This routine extends this definition to allow negative integer values for MA and/or MB.
!  The standard recurrence comb(n,k) = comb(n-1,k-1) + comb(n-1,k) can be used to define
!  comb(n,k) for all integers n,k starting from the initializing definitions
!  comb(0,k) = 0, for all k except 0,
!  comb(n,0) = 1, for all n.
!  Ex:  n = 1, k = 1 => comb(1,1) = comb(0,0)   +  comb(0,1)
!                    => comb(1,1) =         1   +          0,  so  comb(1,1)   =  1
!       n = 0, k = 0 => comb(0,0) = comb(-1,-1) + comb(-1,0)
!                    =>         1 = comb(-1,-1) +          1,  so  comb(-1,-1) =  0
!       n = 0, k = 1 => comb(0,1) = comb(-1,0)  + comb(-1,1)
!                    =>         0 =          1  + comb(-1,1),  so  comb(-1,1)  = -1

!  This definition agrees with the standard factorial definition when n and k are non-negative
!  integers.  The factorial definition is undefined when n or k is a negative integer, since
!  x! has singularities at negative integers.

!  For negative n or k, this extended definition simplifies to:
!  if k < 0,             comb(n,k) = 0
!  if k >= 0 and n < 0,  comb(n,k) = (-1)**k * comb(-n+k-1,k)

!  These extended definitions are useful in some combinatorial applications.
!  Reference:  Concrete Mathematics - Graham, Knuth, Patashnik, Addison-Wesley, 1989.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC
      REAL (KIND(1.0D0)) :: MXSAVE,MZERO
      DOUBLE PRECISION :: ERR
      INTEGER :: IEXTRA,J,JR,K,K09,K10,K11,KBOT,KC,KFLGK,KFLGNK,KL,KOVUN,KR_RETRY,  &
                 KRESLT,KSIGN,KWRNSV,LARGE,N,NBOT,NDSAVE,NK
      LOGICAL, EXTERNAL :: FMCOMP
      LOGICAL :: LC1,LC2,LC3
      REAL :: X
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      TYPE(MULTI) :: MXY(17)

      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < NDIG+2) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      K = 0
      IF (MA%MP(1) < 0) THEN
          K10 = NTRACE
          NTRACE = 0
          K11 = KWARN
          KWARN = 0
          CALL FMM2I(MA,J)
          IF (KFLAG == 0) K = 1
          NTRACE = K10
          KWARN = K11
      ENDIF

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1 .AND. MB%MP(2) < -NDIG .AND. K == 0) THEN
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMCOMB'
              CALL FMNTR(2,MA,MB,2,1)
              NCALL = NCALL - 1
          ENDIF
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          IF (MA%MP(2) == MEXPUN) THEN
              CALL FMTINY(MXY(1))
              MXY(1)%MP(1) = MA%MP(1)
          ELSE
              CALL FMEQ(MA,MXY(1))
          ENDIF
          IF (MB%MP(2) == MEXPUN) THEN
              CALL FMTINY(MXY(2))
              MXY(2)%MP(1) = MB%MP(1)
          ELSE
              CALL FMEQ(MB,MXY(2))
          ENDIF
          CALL FMSUB(MXY(1),MXY(2),MXY(3))
          IF (MXY(3)%MP(3) /= 0) THEN
              CALL FMPI(MXY(4))
              CALL FMSQR(MXY(4),MXY(5))
              CALL FMDIVI(MXY(5),6,MXY(4))
              CALL FMI2M(1,MXY(6))
              CALL FMPGAM(2,MXY(6),MXY(5))
              CALL FMMPY_R1(MXY(5),MXY(1))
              CALL FMDIVI_R1(MXY(5),2)
              CALL FMADD_R1(MXY(4),MXY(5))
              CALL FMMPY_R1(MXY(3),MXY(4))
              CALL FMMPY_R1(MXY(3),MXY(2))
              CALL FMADD_R2(MXY(6),MXY(3))
          ELSE
              CALL FMI2M(1,MXY(3))
          ENDIF
          KFLAG = 0
          NTRACE = J
          KWARN = K
          CALL FMEQ(MXY(3),MC)
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMCOMB'
              CALL FMNTR(1,MC,MC,1,1)
              NCALL = NCALL - 1
          ENDIF
          RETURN
      ENDIF

      CALL FMENT2('FMCOMB   ',MA,MB,2,1,MC,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF
      KSIGN = 1
      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
      CALL FMEQU(MB,MXY(2),NDSAVE,NDIG)

      CALL FMSUB(MXY(1),MXY(2),MXY(15))
      IF (MXY(2)%MP(3) == 0) THEN
          CALL FMI2M(1,MXY(17))
          GO TO 120
      ENDIF
      CALL FMI2M(1,MXY(3))
      CALL FMSUB(MXY(2),MXY(3),MXY(4))
      IF (MXY(4)%MP(3) == 0) THEN
          CALL FMEQ(MXY(1),MXY(17))
          GO TO 120
      ENDIF

!             See if any of the terms are negative integers.

      CALL FMI2M(1,MXY(11))
      K10 = 0
      IF (MXY(1)%MP(1) < 0) THEN
          CALL FMINT(MXY(1),MXY(7))
          IF (FMCOMP(MXY(1),'==',MXY(7))) K10 = -1
          IF (MA%MP(2) == MEXPOV)  K10 = -1
      ENDIF
      K11 = 0
      IF (MXY(2)%MP(1) < 0) THEN
          CALL FMINT(MXY(2),MXY(8))
          IF (FMCOMP(MXY(2),'==',MXY(8))) K11 = -1
          IF (MB%MP(2) == MEXPOV)  K11 = -1
      ENDIF
      K09 = 0
      IF (FMCOMP(MXY(1),'<',MXY(2))) THEN
          CALL FMMOD(MXY(1),MXY(11),MXY(9))
          CALL FMMOD(MXY(2),MXY(11),MXY(10))
          CALL FMSUB_R2(MXY(9),MXY(10))
          CALL FMINT(MXY(10),MXY(9))
          IF (FMCOMP(MXY(10),'==',MXY(9))) K09 = -1
      ENDIF

      CALL FMI2M(2,MXY(10))

      IF (K11 == -1) THEN
          CALL FMI2M(0,MXY(17))
          GO TO 120
      ELSE IF (MXY(15)%MP(3) == 0) THEN
          CALL FMI2M(1,MXY(17))
          GO TO 120
      ELSE IF (K09 == -1 .AND. K10 == 0) THEN
          CALL FMI2M(0,MXY(17))
          GO TO 120
      ELSE IF (K10 == -1 .AND. K09 == 0) THEN
          CALL FMST2M('UNKNOWN',MXY(17))
          KFLAG = -4
          GO TO 130
      ELSE IF (K10 == -1 .AND. K09 == -1) THEN
          JR = KROUND
          KROUND = 1
          CALL FMMOD(MXY(2),MXY(10),MXY(12))
          IF (MXY(12)%MP(3) /= 0) KSIGN = -1
          CALL FMSUB(MXY(2),MXY(1),MXY(12))
          CALL FMSUB(MXY(12),MXY(11),MXY(1))
          CALL FMSUB(MXY(1),MXY(2),MXY(15))
          KROUND = JR
      ENDIF

!             Check for an obviously overflowed result.

      IF (MXY(1)%MP(2) == MEXPOV) THEN
          IF (MXY(1)%MP(1)*MXY(1)%MP(3) > 0 .AND. MXY(2)%MP(1) > 0 .AND.  &
              MXY(2)%MP(2) >= 1 .AND. MXY(2)%MP(2) < MEXPOV) THEN
              CALL FMST2M('OVERFLOW',MXY(17))
              KFLAG = -5
              GO TO 130
          ENDIF
      ENDIF
      IF (MXY(1)%MP(2) >= 10000) THEN
          CALL FMI2M(1,MXY(5))
          IF (FMCOMP(MXY(2),'>',MXY(5)) .AND. FMCOMP(MXY(2),'<',MXY(1))) THEN
              CALL FMSUB(MXY(1),MXY(2),MXY(5))
              CALL FMMIN(MXY(2),MXY(5),MXY(13))
              CALL FMSUB(MXY(1),MXY(13),MXY(5))
              CALL FMADDI(MXY(5),1)
              CALL FMDIV(MXY(5),MXY(13),MXY(12))
              CALL FMLN(MXY(12),MXY(5))
              CALL FMADDI(MXY(5),1)
              CALL FMMPY(MXY(13),MXY(5),MXY(12))
              CALL FMDPM(6.283185D0,MXY(3))
              CALL FMMPY(MXY(3),MXY(13),MXY(5))
              CALL FMLN(MXY(5),MXY(3))
              CALL FMDIVI(MXY(3),2,MXY(5))
              CALL FMSUB_R1(MXY(12),MXY(5))
              CALL FMEXP(MXY(12),MXY(4))
              IF (MXY(4)%MP(2) == MEXPOV) THEN
                  CALL FMST2M('OVERFLOW',MXY(17))
                  KFLAG = -5
                  GO TO 130
              ENDIF
          ENDIF
      ENDIF

!             See if any of the terms are small integers.

      KWRNSV = KWARN
      KWARN = 0
      CALL FMM2I(MXY(1),N)
      CALL FMM2I(MXY(2),K)
      KFLGK = KFLAG
      CALL FMM2I(MXY(15),NK)
      KFLGNK = KFLAG
      KWARN = KWRNSV

      CALL FMI2M(1,MXY(5))
      JR = KROUND
      KROUND = 1
      CALL FMADD(MXY(1),MXY(5),MXY(3))
      CALL FMSUB_R1(MXY(3),MXY(5))
      KROUND = JR
      IF (KFLGK == 0 .AND. MXY(3)%MP(3) == 0) THEN
          CALL FMI2M(2,MXY(17))
          CALL FMMOD(MXY(2),MXY(17),MXY(5))
          CALL FMEQ(MXY(5),MXY(17))
          IF (MXY(17)%MP(3) == 0) THEN
              CALL FMDIV(MXY(1),MXY(2),MXY(17))
              IF (MXY(17)%MP(2) /= MUNKNO .AND. MXY(17)%MP(3) /= 0)  &
                  MXY(17)%MP(1) = -MXY(17)%MP(1)
          ELSE
              CALL FMDIV(MXY(1),MXY(2),MXY(17))
          ENDIF
          GO TO 120
      ENDIF
      IF (KFLGK == 0 .AND. KFLGNK == 0 .AND. N /= 0) THEN
          IF (MIN(K,NK) <= 200) THEN
              CALL FMCMBI(N,K,MXY(17))
              GO TO 120
          ENDIF
      ENDIF
      NBOT = 0
      IF (KFLGK == 0 .AND. K <= 200) NBOT = K
      IF (KFLGNK == 0 .AND. NK <= 200) NBOT = NK
      IF (NBOT > 0) THEN
          LARGE = INT(MXBASE/NBOT)
          KBOT = 1
          CALL FMEQ(MXY(1),MXY(7))
          CALL FMEQ(MXY(1),MXY(8))
          CALL FMI2M(-1,MXY(9))
          DO J = 2, NBOT
             CALL FMADD_R1(MXY(7),MXY(9))
             CALL FMMPY_R2(MXY(7),MXY(8))
             KBOT = KBOT*J
             IF (KBOT >= LARGE) THEN
                 CALL FMDIVI_R1(MXY(8),KBOT)
                 KBOT = 1
             ENDIF
          ENDDO
          CALL FMDIVI(MXY(8),KBOT,MXY(17))
          GO TO 120
      ENDIF

!             General case.  Use FMFACT, unless one of the numbers is too big.  If so, use FMLNGM.

      X = ALOGMB*REAL(MXEXP)
      CALL FMSP2M(X/LOG(X),MXY(6))
      CALL FMABS(MXY(15),MXY(5))
      LC1 = FMCOMP(MXY(5),'>=',MXY(6))
      CALL FMABS(MXY(1),MXY(5))
      LC2 = FMCOMP(MXY(5),'>=',MXY(6))
      CALL FMABS(MXY(2),MXY(5))
      LC3 = FMCOMP(MXY(5),'>=',MXY(6))
      IF (LC1 .OR. LC2 .OR. LC3) THEN

!             See if the second argument is not very large and the first is much larger.
!             For many of these cases, Stirling's formula can be used to simplify Comb
!             and avoid cancellation.

          IF (MXY(1)%MP(2) > MXY(2)%MP(2) .AND. MXY(1)%MP(1) > 0 .AND.  &
              MXY(2)%MP(1) > 0) THEN
              CALL FMEQ(MXY(1),MXY(9))
              CALL FMEQ(MXY(2),MXY(10))
          ELSE
              CALL FMI2M(1,MXY(9))
              CALL FMI2M(1,MXY(10))
          ENDIF
          IF (MXY(9)%MP(2) > NDIG .AND.  &
              MXY(9)%MP(2) >= MXY(10)%MP(2)+NDIG) THEN
              CALL FMI2M(1,MXY(5))
              CALL FMADD(MXY(10),MXY(5),MXY(16))
              CALL FMLN(MXY(9),MXY(5))
              CALL FMADDI(MXY(5),-1)
              CALL FMMPY(MXY(10),MXY(5),MXY(14))
              CALL FMADD_R2(MXY(10),MXY(14))
              CALL FMLNGM(MXY(16),MXY(15))
              CALL FMSUB(MXY(14),MXY(15),MXY(5))
              CALL FMEXP(MXY(5),MXY(12))
              CALL FMEQ(MXY(12),MXY(17))
              GO TO 120
          ENDIF

!             Compute IEXTRA, the number of extra digits required to compensate for
!             cancellation error.

          MZERO = 0
          IEXTRA = INT(MAX(MXY(15)%MP(2),MXY(1)%MP(2),MXY(2)%MP(2),MZERO))
          IF (NDIG+IEXTRA > 3*10**5) THEN
              KFLAG = -4
              CALL FMST2M('UNKNOWN',MXY(17))
              GO TO 130
          ENDIF
          IF (IEXTRA > 0) THEN
              CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
              CALL FMEQU_R1(MXY(2),NDIG,NDIG+IEXTRA)
          ENDIF
          NDIG = NDIG + IEXTRA
          IF (K10 == -1 .AND. K09 == -1) THEN
              JR = KROUND
              KROUND = 1
              CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
              CALL FMM2I(MXY(1),N)
              CALL FMEQU(MB,MXY(2),NDSAVE,NDIG)
              CALL FMSUB(MXY(2),MXY(1),MXY(12))
              CALL FMI2M(1,MXY(11))
              CALL FMSUB(MXY(12),MXY(11),MXY(1))
              KROUND = JR
          ENDIF

          CALL FMSUB(MXY(1),MXY(2),MXY(15))
          CALL FMI2M(1,MXY(9))
          CALL FMI2M(2,MXY(10))
          CALL FMADD(MXY(15),MXY(9),MXY(17))
          CALL FMADD_R1(MXY(1),MXY(9))
          CALL FMADD_R1(MXY(2),MXY(9))
          K10 = 0
          K11 = 0
          KC = 0
          IF (MXY(1)%MP(1) < 0) THEN
              CALL FMINT(MXY(1),MXY(11))
              CALL FMMOD(MXY(11),MXY(10),MXY(12))
              IF (MXY(12)%MP(3) == 0) THEN
                  K10 = 1
                  CALL FMADD_R1(MXY(1),MXY(9))
              ENDIF
          ENDIF
          IF (MXY(2)%MP(1) < 0) THEN
              CALL FMINT(MXY(2),MXY(11))
              CALL FMMOD(MXY(11),MXY(10),MXY(12))
              IF (MXY(12)%MP(3) == 0) THEN
                  K11 = 1
                  CALL FMADD_R1(MXY(2),MXY(9))
              ENDIF
          ENDIF
          IF (MXY(17)%MP(1) < 0) THEN
              CALL FMINT(MXY(17),MXY(11))
              CALL FMMOD(MXY(11),MXY(10),MXY(12))
              IF (MXY(12)%MP(3) == 0) THEN
                  KC = 1
                  CALL FMADD_R1(MXY(17),MXY(9))
              ENDIF
          ENDIF
          CALL FMLNGM(MXY(1),MXY(15))
          CALL FMLNGM(MXY(2),MXY(16))
          CALL FMSUB_R1(MXY(15),MXY(16))
          CALL FMLNGM(MXY(17),MXY(16))
          CALL FMSUB_R1(MXY(15),MXY(16))
          CALL FMEXP(MXY(15),MXY(4))
          IF (K10 == 1 .OR. K11 == 1 .OR. KC == 1) THEN
              CALL FMI2M(1,MXY(9))
              IF (K10 == 1) THEN
                  CALL FMSUB_R1(MXY(1),MXY(9))
                  CALL FMDIV_R1(MXY(4),MXY(1))
              ENDIF
              IF (K11 == 1) THEN
                  CALL FMSUB_R1(MXY(2),MXY(9))
                  CALL FMMPY_R1(MXY(4),MXY(2))
              ENDIF
              IF (KC == 1) THEN
                  CALL FMSUB_R1(MXY(17),MXY(9))
                  CALL FMMPY_R1(MXY(4),MXY(17))
              ENDIF
          ENDIF
          CALL FMEQ(MXY(4),MXY(17))
      ELSE
          CALL FMFACT(MXY(1),MXY(16))
          CALL FMFACT(MXY(2),MXY(3))
          CALL FMFACT(MXY(15),MXY(17))
          CALL FMMPY(MXY(17),MXY(3),MXY(7))
          CALL FMDIV(MXY(16),MXY(7),MXY(17))
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  120 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(17)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF

  130 MXY(17)%MP(1) = KSIGN*MXY(17)%MP(1)
      CALL FMEXT2(MXY(17),MC,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMCOMB

      FUNCTION FMDPLG(A)

!  Internal routine for computing an approximation to Log(Gamma(A)) using Stirling's formula.

      USE ModLib_FMVALS
      IMPLICIT NONE

      DOUBLE PRECISION :: FMDPLG,A
      INTENT (IN) :: A

      IF (MBLOGS /= MBASE) CALL FMCONS
      IF (A > 0.0D0) THEN
          FMDPLG = -A + (A-0.5D0)*LOG(A) + DLOGTP/2.0D0
      ELSE IF (A < 0.0D0) THEN
          IF (ABS(A) < 1.0D+8) THEN
              FMDPLG = -(A-1.0D0) - (0.5D0-A)*LOG(1.0D0-A) - DLOGTP/2.0D0 -  &
                       LOG(ABS(SIN(DPPI*A))+1.0D-10) + DLOGPI
          ELSE
              FMDPLG = -(A-1.0D0) - (0.5D0-A)*LOG(1.0D0-A) - DLOGTP/2.0D0 + DLOGPI
          ENDIF
      ELSE

!             A = 0 is really an approximation for some value in [-1,1].

          FMDPLG = 0.0D0
      ENDIF
      RETURN
      END FUNCTION FMDPLG

      SUBROUTINE FMENT2(NROUTN,MA,MB,NARGS,KNAM,MC,KRESLT,NDSAVE,MXSAVE,KOVUN)

!  Do the argument checking and increasing of precision, overflow threshold, etc., upon entry
!  to an FM routine.

!  NROUTN - routine name of calling routine
!  MA     - first input argument
!  MB     - second input argument (optional)
!  NARGS  - number of input arguments
!  KNAM   - positive if the routine name is to be printed.
!  MC     - result argument
!  KRESLT - returned nonzero if the input arguments give the result immediately
!           (e.g., MA*0 or OVERFLOW*MB)
!  NDSAVE - saves the value of NDIG after NDIG is increased
!  MXSAVE - saves the value of MXEXP
!  KOVUN  - returned nonzero if an input argument is (+ or -) overflow or underflow.

      USE ModLib_FMVALS
      IMPLICIT NONE

      CHARACTER(9) :: NROUTN
      TYPE(MULTI) :: MA,MB,MC
      REAL (KIND(1.0D0)) :: MXSAVE
      INTEGER :: KNAM,NARGS,KRESLT,NDSAVE,KOVUN
      INTEGER :: K
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC

      IF (MBLOGS /= MBASE) CALL FMCONS
      NCALL = NCALL + 1
      NAMEST(NCALL) = NROUTN
      IF (NTRACE /= 0) CALL FMNTR(2,MA,MB,NARGS,KNAM)
      CALL FMARG2(NROUTN,NARGS,MA,MB,KRESLT)

      KOVUN = 0
      IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN) KOVUN = 1
      IF (NARGS == 2) THEN
          IF (MB%MP(2) == MEXPOV .OR. MB%MP(2) == MEXPUN) KOVUN = 1
      ENDIF

!             Increase the working precision.

      NDSAVE = NDIG
      MXSAVE = MXEXP
      IF (NCALL >= 1 .OR. RAISE_NDIG > 0) THEN
          K = INT(NGRD52 + 1 + 0.002*NDIG + (REAL(NDIG)*ALOGMT)**0.35/ALOGMT)
          NDIG = MAX(NDIG+K,2)
      ENDIF

      IF (KRESLT /= 0) THEN
          IF (KRESLT == 9 .OR. KRESLT == 10 .OR. KRESLT >= 13) THEN
              IF (KRAD == 1) THEN
                  CALL FMPI(MC)
              ELSE
                  CALL FMI2M(180,MC)
              ENDIF
              IF (KRESLT <= 10) CALL FMDIVI_R1(MC,2)
              IF (KRESLT >= 14) CALL FMDIVI_R1(MC,4)
              IF (KRESLT == 9 .OR. KRESLT == 14) MC%MP(1) = -1
              CALL FMEQU_R1(MC,NDIG,NDSAVE)
              NDIG = NDSAVE
              IF (NTRACE /= 0) CALL FMNTR(1,MC,MC,1,1)
              NCALL = NCALL - 1
              RETURN
          ENDIF

          NDIG = NDSAVE
          CALL FMRSLT(MA,MB,MC,KRESLT)
          IF (NTRACE /= 0 .AND. NROUTN /= 'FMIBTA') THEN
              CALL FMNTR(1,MC,MC,1,1)
          ENDIF
          NCALL = NCALL - 1
          RETURN
      ENDIF

!             Extend the overflow/underflow threshold.

      MXEXP = MXEXP2
      RETURN
      END SUBROUTINE FMENT2

      SUBROUTINE FMEULR(MA)

!  MA = Euler's constant ( 0.5772156649... )

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      CHARACTER(2315) :: STRING
      INTEGER :: K,NDMB,NDSAVE,NDSV
      TYPE(MULTI) :: MXY(2)
      INTENT (INOUT) :: MA

      IF (.NOT. ALLOCATED(MA%MP)) THEN
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MA%MP) < NDIG+2) THEN
          DEALLOCATE(MA%MP)
          ALLOCATE(MA%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      KFLAG = 0
      NCALL = NCALL + 1
      NAMEST(NCALL) = 'FMEULR'
      IF (ABS(NTRACE) >= 2 .AND. NCALL <= LVLTRC) THEN
          WRITE (KW,"(' Input to FMEULR')")
      ENDIF

!             Increase the working precision.

      NDSAVE = NDIG
      IF (NCALL == 1) THEN
          K = INT(5.0/ALOGMT + 2.0 + (REAL(NDIG)*ALOGMT)**0.35/ALOGMT)
          NDIG = MAX(NDIG+K,2)
      ENDIF

!             Check to see if Euler's constant has previously been saved in base MBASE with
!             sufficient precision.

      IF (MBSEUL == MBASE .AND. NDGEUL >= NDIG) THEN
          CALL FMEQU(M_EULER,MA,NDGEUL,NDSAVE)
      ELSE

!             Euler's constant is slower to compute than the other saved constants, so more digits
!             are stored in STRING for quick conversion.

          NDMB = INT(2300.0*2.302585/ALOGMB)
          IF (NDMB >= NDIG) THEN
              NDSV = NDIG
              NDIG = NDMB
              STRING = '0.57721566490153286060651209008240243104215933593992359880576723488486'//  &
              '7726777664670936947063291746749514631447249807082480960504014486542836224173997'//  &
              '6449235362535003337429373377376739427925952582470949160087352039481656708532331'//  &
              '5177661152862119950150798479374508570574002992135478614669402960432542151905877'//  &
              '5535267331399254012967420513754139549111685102807984234877587205038431093997361'//  &
              '3725530608893312676001724795378367592713515772261027349291394079843010341777177'//  &
              '8088154957066107501016191663340152278935867965497252036212879226555953669628176'//  &
              '3887927268013243101047650596370394739495763890657296792960100901512519595092224'//  &
              '3501409349871228247949747195646976318506676129063811051824197444867836380861749'//  &
              '4551698927923018773910729457815543160050021828440960537724342032854783670151773'//  &
              '9439870030237033951832869000155819398804270741154222781971652301107356583396734'//  &
              '8717650491941812300040654693142999297779569303100503086303418569803231083691640'//  &
              '0258929708909854868257773642882539549258736295961332985747393023734388470703702'//  &
              '8441292016641785024873337908056275499843459076164316710314671072237002181074504'//  &
              '4418664759134803669025532458625442225345181387912434573501361297782278288148945'//  &
              '9098638460062931694718871495875254923664935204732436410972682761608775950880951'//  &
              '2620840454447799229915724829251625127842765965708321461029821461795195795909592'//  &
              '2704208989627971255363217948873764210660607065982561990102880756125199137511678'//  &
              '2176436190570584407835735015800560774579342131449885007864151716151945657061704'//  &
              '3245075008168705230789093704614306684817916496842549150496724312183783875356489'//  &
              '4950868454102340601622508515583867234944187880440940770106883795111307872023426'//  &
              '3952269209716088569083825113787128368204911789259447848619911852939102930990592'//  &
              '5526691727446892044386971114717457157457320393520912231608508682755889010945168'//  &
              '1181016874975470969366671210206304827165895049327314860874940207006742590918248'//  &
              '7596213738423114426531350292303175172257221628324883811245895743862398703757662'//  &
              '8551303314392999540185313414158621278864807611003015211965780068117773763501681'//  &
              '8389733896639868957932991456388644310370608078174489957958324579418962026049841'//  &
              '0439225078604603625277260229196829958609883390137871714226917883819529844560791'//  &
              '6051972797360475910251099577913351579177225150254929324632502874767794842158405'//  &
              '07599290401855764599018627262'
              CALL FMST2M(STRING,M_EULER)
              MBSEUL = MBASE
              NDGEUL = NDIG
              IF (ABS(M_EULER%MP(2)) > 10) NDGEUL = 0
              CALL FMEQU(M_EULER,MA,NDIG,NDSAVE)
              NDIG = NDSV
          ELSE
              NDSV = NDIG
              NDIG = NDIG + 2
              CALL FMEULR_B(MXY(1))
              CALL FMEQ(MXY(1),M_EULER)
              MBSEUL = MBASE
              NDGEUL = NDIG
              IF (ABS(M_EULER%MP(2)) > 10) NDGEUL = 0
              CALL FMEQU(M_EULER,MA,NDIG,NDSAVE)
              NDIG = NDSV
          ENDIF
      ENDIF

      NDIG = NDSAVE
      IF (NTRACE /= 0) CALL FMNTR(1,MA,MA,1,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMEULR

      SUBROUTINE FMEULR_B(MB)

!  MB = Euler's constant = .577215...

!  Binary splitting version.
!  Called by FMEULR for large NDIG.  Faster and saves memory by not using Bernoulli numbers.

!  Sum f(x) = 1 + x/(1!)^2 + x^2/(2!)^2 + ...
!  and g(x) = H(1)*x/(1!)^2 + H(2)*x^2/(2!)^2 + ...
!  where H(n) = 1 + 1/2 + ... + 1/n.

!  As x --> infinity, g(x)/f(x) - Ln(x)/2 --> Euler's constant.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MB
      REAL (KIND(1.0D0)) :: MXSAVE
      INTEGER :: K,KT,N,NDSAVE,LEVEL_OF_RECURSION
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(7),X

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (MBLOGS /= MBASE) CALL FMCONS
      NCALL = NCALL + 1
      NDSAVE = NDIG
      IF (NCALL == 1) THEN
          K = MAX(NGRD52-1,2)
          NDIG = MAX(NDIG+K,2)
      ENDIF
      MXSAVE = MXEXP
      MXEXP = MXEXP2

!             Determine X, variable in the series.

      N = NDIG*DLOGMB/4
      DO
         K = N
         DO
            KT = K/2
            IF (2*KT == K) THEN
                K = KT
            ELSE
                EXIT
            ENDIF
         ENDDO
         DO
            KT = K/3
            IF (3*KT == K) THEN
                K = KT
            ELSE
                EXIT
            ENDIF
         ENDDO
         DO
            KT = K/5
            IF (5*KT == K) THEN
                K = KT
            ELSE
                EXIT
            ENDIF
         ENDDO
         DO
            KT = K/7
            IF (7*KT == K) THEN
                K = KT
            ELSE
                EXIT
            ENDIF
         ENDDO
         IF (K == 1) EXIT
         N = N + 1
      ENDDO

      CALL IMI2M(N,MXY(1))
      CALL IMSQR(MXY(1),X)

!             Determine K, the number of terms to sum in the series.

      K = 3.5912D0*N
      LEVEL_OF_RECURSION = 0
      CALL FMEULR_PTDCV(0,K,MXY(1),MXY(2),MXY(3),MXY(4),MXY(5),X,LEVEL_OF_RECURSION)
      IF (MXY(3)%MP(2) >= NDIG) THEN
          CALL FMEQ(MXY(3),MXY(7))
      ELSE
          CALL IMI2FM(MXY(3),MXY(7))
      ENDIF
      CALL FMSQR(MXY(7),MXY(4))
      IF (MXY(2)%MP(2) >= NDIG) THEN
          CALL FMEQ(MXY(2),MXY(1))
      ELSE
          CALL IMI2FM(MXY(2),MXY(1))
      ENDIF
      CALL FMDIV(MXY(1),MXY(4),MXY(6))
      CALL FMADDI(MXY(6),1)

      CALL FMMPY(MXY(7),MXY(4),MXY(1))
      IF (MXY(5)%MP(2) >= NDIG) THEN
          CALL FMEQ(MXY(5),MXY(4))
      ELSE
          CALL IMI2FM(MXY(5),MXY(4))
      ENDIF
      CALL FMDIV(MXY(4),MXY(1),MXY(2))

      CALL FMDIV(MXY(2),MXY(6),MXY(5))
      CALL FMLNI(N,MXY(1))
      CALL FMSUB(MXY(5),MXY(1),M_EULER)

!             Round the result and return.

      CALL FMEXIT(M_EULER,MB,NDSAVE,MXSAVE,0)
      RETURN
      END SUBROUTINE FMEULR_B

      RECURSIVE SUBROUTINE FMEULR_PTDCV(A,B,MP,MT,MD,MC,MV,MX,LEVEL_OF_RECURSION)

!  This routine does the binary splitting for computing Euler's constant.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MP,MT,MD,MC,MV,MX
      INTEGER :: A,B
      INTENT (IN) :: A,B
      INTENT (INOUT) :: MP,MT,MD,MC,MV,MX
      TYPE(MULTI) :: MXY(10)
      INTEGER :: J,JP,JD,M,RESULT_SIZE,LEVEL_OF_RECURSION
      REAL (KIND(0.0D0)) :: DA,DB

      DA = A
      DB = B
      RESULT_SIZE = ( (DB - DA + 1)*MX%MP(2) ) + 5
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      RESULT_SIZE = MIN(NDIG+10,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MP%MP)) THEN
          ALLOCATE(MP%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MP%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MP%MP)
          ALLOCATE(MP%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      JP = RESULT_SIZE
      RESULT_SIZE = ( (DB+1.5D0)*LOG(DB+2) - DB + 1/(12*(DB+2)) -  &
                    ( (DA+0.5D0)*LOG(DA+1) - DA + 1/(12*(DA+1)) ) - 1 ) / DLOGMB + 10
      RESULT_SIZE = MIN(NDIG+10,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MD%MP)) THEN
          ALLOCATE(MD%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MD%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MD%MP)
          ALLOCATE(MD%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      JD = RESULT_SIZE
      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      CALL IMI2M(A,MXY(1))
      IF (MXY(1)%MP(2) > MX%MP(2)) THEN
          RESULT_SIZE = MX%MP(2) + 4*JD
      ELSE
          RESULT_SIZE = JP + 2*JD
      ENDIF
      RESULT_SIZE = MIN(NDIG+10,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MT%MP)) THEN
          ALLOCATE(MT%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MT%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MT%MP)
          ALLOCATE(MT%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      RESULT_SIZE = RESULT_SIZE + JD
      RESULT_SIZE = MIN(NDIG+10,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MV%MP)) THEN
          ALLOCATE(MV%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MV%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MV%MP)
          ALLOCATE(MV%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      LEVEL_OF_RECURSION = LEVEL_OF_RECURSION + 1

      IF (B-A < 12) THEN
          IF (.NOT. ALLOCATED(MXY(1)%MP)) THEN
              ALLOCATE(MXY(1)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MXY(1)%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MXY(1)%MP)
              ALLOCATE(MXY(1)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MXY(2)%MP)) THEN
              ALLOCATE(MXY(2)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MXY(2)%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MXY(2)%MP)
              ALLOCATE(MXY(2)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MXY(3)%MP)) THEN
              ALLOCATE(MXY(3)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MXY(3)%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MXY(3)%MP)
              ALLOCATE(MXY(3)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MXY(4)%MP)) THEN
              ALLOCATE(MXY(4)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MXY(4)%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MXY(4)%MP)
              ALLOCATE(MXY(4)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          IF (.NOT. ALLOCATED(MXY(5)%MP)) THEN
              ALLOCATE(MXY(5)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MXY(5)%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MXY(5)%MP)
              ALLOCATE(MXY(5)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF

          CALL IMI2M(B-A+1,MXY(2))
          CALL IMPWR(MX,MXY(2),MP)

          CALL IMI2M(1,MD)
          DO J = A, B-1, 2
             CALL IMMPYI(MD,J+1,MXY(1))
             CALL IMMPYI(MXY(1),J+2,MD)
          ENDDO
          IF (MOD(B-A,2) == 0) THEN
              CALL IMMPYI(MD,B+1,MXY(1))
              CALL IMEQ(MXY(1),MD)
          ENDIF

          CALL IMI2M(0,MC)
          DO J = A, B
             CALL IMDIVI(MD,J+1,MXY(2))
             CALL IMADD(MC,MXY(2),MXY(3))
             CALL IMEQ(MXY(3),MC)
          ENDDO

!             MT is the T sum
!             MXY(1) is the next term
!             MV is the V sum
!             MXY(3) is the harmonic sum in the V terms

          CALL IMI2M(0,MT)
          CALL IMI2M(0,MV)
          CALL IMSQR(MD,MXY(1))
          CALL IMI2M(0,MXY(3))
          DO J = A, B
             CALL IMDIVI(MXY(1),J+1,MXY(2))
             CALL IMDIVI(MXY(2),J+1,MXY(1))
             CALL IMMPY(MXY(1),MX,MXY(2))
             CALL IMEQ(MXY(2),MXY(1))
             CALL IMADD(MT,MXY(1),MXY(2))
             CALL IMEQ(MXY(2),MT)

             CALL IMDIVI(MD,J+1,MXY(4))
             CALL IMADD(MXY(3),MXY(4),MXY(5))
             CALL IMEQ(MXY(5),MXY(3))
             CALL IMMPY(MXY(1),MXY(3),MXY(5))
             CALL IMADD(MV,MXY(5),MXY(2))
             CALL IMEQ(MXY(2),MV)
          ENDDO
          GO TO 110
      ENDIF

      M = A/2 + B/2 + MOD(A,2)*MOD(B,2)
      CALL FMEULR_PTDCV(A,M-1,MXY(1),MXY(2),MXY(3),MXY(4),MXY(5),MX,LEVEL_OF_RECURSION)
      CALL FMEULR_PTDCV(M,B,MXY(6),MXY(7),MXY(8),MXY(9),MXY(10),MX,LEVEL_OF_RECURSION)
      CALL IM_OR_FM_MPY(MXY(1),MXY(7),MT)
      CALL IM_OR_FM_SQR(MXY(8),MP)
      CALL IM_OR_FM_MPY(MXY(4),MXY(8),MD)

!             MP and MC are not needed in FMEULR_B, so their calculations can be skipped at the
!             top level of the recursion.

      IF (LEVEL_OF_RECURSION > 1) THEN
          CALL IM_OR_FM_MPY(MXY(9),MXY(3),MXY(4))
          CALL IM_OR_FM_ADD(MD,MXY(4),MC)
      ELSE
          CALL IMI2M(0,MC)
      ENDIF

      CALL IM_OR_FM_MPY(MD,MT,MXY(4))
      CALL IM_OR_FM_MPY(MP,MXY(8),MV)
      CALL IM_OR_FM_MPY(MV,MXY(5),MXY(9))
      CALL IM_OR_FM_ADD(MXY(9),MXY(4),MV)
      CALL IM_OR_FM_MPY(MXY(3),MXY(1),MXY(4))
      CALL IM_OR_FM_MPY(MXY(4),MXY(10),MXY(9))
      CALL IM_OR_FM_ADD(MV,MXY(9),MXY(4))
      CALL IM_OR_FM_EQ(MXY(4),MV)

      CALL IM_OR_FM_MPY(MXY(3),MXY(8),MD)

      CALL IM_OR_FM_MPY(MP,MXY(2),MXY(3))
      CALL IM_OR_FM_ADD(MXY(3),MT,MXY(8))
      CALL IM_OR_FM_EQ(MXY(8),MT)

      IF (LEVEL_OF_RECURSION > 1) THEN
          CALL IM_OR_FM_MPY(MXY(1),MXY(6),MP)
      ELSE
          CALL IMI2M(0,MP)
      ENDIF

  110 LEVEL_OF_RECURSION = LEVEL_OF_RECURSION - 1
      RETURN
      END SUBROUTINE FMEULR_PTDCV

      SUBROUTINE FMEXT2(MT,MC,NDSAVE,MXSAVE,KOVUN)

!  Upon exit from an FM routine, the result MT (having precision NDIG) is rounded and returned
!  in MC (having precision NDSAVE).  The values of NDIG, MXEXP, and KACCSW are restored.
!  KOVUN is nonzero if one of the routine's input arguments was overflow or underflow.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MT,MC
      REAL (KIND(1.0D0)) :: MXSAVE
      INTEGER :: NDSAVE,KOVUN

      INTEGER :: KFSAVE,KWRNSV
      INTENT (IN) :: MT
      INTENT (INOUT) :: MC,NDSAVE,MXSAVE,KOVUN
      KWRNSV = KWARN
      KWARN = 0
      MXEXP = MXSAVE
      KFSAVE = KFLAG
      CALL FMEQU(MT,MC,NDIG,NDSAVE)
      IF (KFLAG /= -5 .AND. KFLAG /= -6) KFLAG = KFSAVE
      NDIG = NDSAVE
      KWARN = KWRNSV
      IF (KFLAG == 1) KFLAG = 0
      IF ((MC%MP(2) == MUNKNO .AND. KFLAG /= -9)     &
         .OR. (MC%MP(2) == MEXPUN .AND. KOVUN == 0)  &
         .OR. (MC%MP(2) == MEXPOV .AND. KOVUN == 0)) CALL FMWRN2
      IF (NTRACE /= 0) CALL FMNTR(1,MC,MC,1,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE FMEXT2

      SUBROUTINE FMFACT(MA,MB)

!  MB = MA!  ( = GAMMA(MA+1))

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,K,KL,KOVUN,KR_RETRY,KRESLT,NDSAVE
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(3)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1 .AND. MA%MP(2) < -NDIG) THEN
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMFACT'
              CALL FMNTR(2,MA,MA,1,1)
              NCALL = NCALL - 1
          ENDIF
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          CALL FMEULR(MXY(1))
          CALL FMMPY(MXY(1),MA,MXY(3))
          CALL FMI2M(1,MXY(1))
          CALL FMSUB(MXY(1),MXY(3),MB)
          KFLAG = 0
          NTRACE = J
          KWARN = K
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMFACT'
              CALL FMNTR(1,MB,MB,1,1)
              NCALL = NCALL - 1
          ENDIF
          RETURN
      ENDIF

      CALL FMENT2('FMFACT   ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF
      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
      CALL FMADDI(MXY(1),1)
      CALL FMGAM(MXY(1),MXY(2))

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL FMEXT2(MXY(2),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMFACT

      SUBROUTINE FMFCTI(NUM,MA)

!  MA = NUM factorial, where NUM is an integer.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: NUM
      TYPE(MULTI) :: MA

      INTEGER :: J,JK,K,LARGE
      INTENT (IN) :: NUM
      INTENT (INOUT) :: MA

      CALL FMI2M(1,MA)
      IF (NUM <= 1) THEN
          RETURN
      ENDIF
      J = NUM
      K = 1
      LARGE = INT(INTMAX/J)
      DO JK = 2, J
         K = K*JK
         IF (K > LARGE) THEN
             CALL FMCSMPYI_R1(MA,K)
             K = 1
         ENDIF
      ENDDO
      IF (K > 1) CALL FMMPYI_R1(MA,K)
      RETURN
      END SUBROUTINE FMFCTI

      SUBROUTINE FMGAM(MA,MB)

!  MB = GAMMA(MA)

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR,X,Z
      INTEGER :: IEXTRA,INTA,J,J2,K,K0,K1,K2,KDIFF,KFL,KL,KOVUN,KC_RETRY,KR_RETRY,KRESLT,  &
                 KRFLCT,KRSAVE,KSIGN,KWRNSV,KWSAVE,LARGE,LSHIFT,NDSAV1,NDSAV2,NDSAVE,NMETHD,NTERM
      LOGICAL, EXTERNAL :: FMCOMP
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI), SAVE :: MXY(17),MJSUMS(LJSUMS),C(0:196)
      INTEGER, SAVE ::  NDIG_C = 0
      REAL (KIND(1.0D0)), SAVE :: MBASE_C = 0

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1 .AND. MA%MP(2) < -NDIG) THEN
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMGAM'
              CALL FMNTR(2,MA,MA,1,1)
              NCALL = NCALL - 1
          ENDIF
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          CALL FMI2M(1,MXY(1))
          CALL FMDIV(MXY(1),MA,MXY(2))
          CALL FMEULR(MXY(1))
          CALL FMSUB(MXY(2),MXY(1),MB)
          IF (KFLAG > 0) KFLAG = 0
          NTRACE = J
          KWARN = K
          IF (KFLAG == -5 .AND. MA%MP(2) > MEXPUN) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMGAM'
              CALL FMWRN2
              NCALL = NCALL - 1
          ENDIF
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMGAM'
              CALL FMNTR(1,MB,MB,1,1)
              NCALL = NCALL - 1
          ENDIF
          RETURN
      ENDIF

      CALL FMENT2('FMGAM    ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF
      KR_RETRY = 0
      KC_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)

!             See if there is a small integer separating this argument from the last one.

      IF (MBASE == MBSGAM .AND. NDIG <= NDGGAM) THEN
          IF (MA%MP(1) == -1) THEN
              CALL FMINT(MXY(1),MXY(10))
              IF (FMCOMP(MXY(1),'==',MXY(10))) THEN
                  CALL FMST2M('UNKNOWN',MXY(11))
                  KFLAG = -4
                  GO TO 200
              ENDIF
          ENDIF
          CALL FMSUB(MXY(1),M_GAMMA_MA,MXY(7))
          IF (MXY(7)%MP(3) == 0) THEN
              CALL FMEQ(M_GAMMA_MB,MXY(11))
              GO TO 200
          ENDIF
          KWRNSV = KWARN
          KWARN = 0
          CALL FMM2I(MXY(7),KDIFF)
          KWARN = KWRNSV
          IF (KFLAG == 0 .AND. ABS(KDIFF) <= 50) THEN
              IF (KDIFF > 0) THEN
                  CALL FMEQ(M_GAMMA_MA,MXY(10))
              ELSE
                  CALL FMEQ(MXY(1),MXY(10))
              ENDIF
              CALL FMEQ(MXY(10),MXY(9))
              DO J = 1, ABS(KDIFF)-1
                 CALL FMI2M(1,MXY(5))
                 CALL FMADD_R1(MXY(10),MXY(5))
                 CALL FMMPY_R1(MXY(9),MXY(10))
              ENDDO
              IF (KDIFF > 0) THEN
                  CALL FMMPY(M_GAMMA_MB,MXY(9),MXY(11))
              ELSE
                  CALL FMDIV(M_GAMMA_MB,MXY(9),MXY(11))
              ENDIF
              GO TO 190
          ENDIF
      ENDIF
      CALL FMEQ(MXY(1),MXY(15))

!             Near zero Gamma(x) is about 1/x.

      IF (MXY(15)%MP(2) < (-NDIG-3)) THEN
          CALL FMI2M(1,MXY(5))
          CALL FMDIV(MXY(5),MXY(15),MXY(11))
          GO TO 190
      ENDIF

!             Check for special cases.

      KRFLCT = 0
      CALL FMDPM(DBLE(-0.5),MXY(7))
      IF (FMCOMP(MXY(15),'<=',MXY(7))) THEN
          KRFLCT = 1
          KFL = 0
          IF (MXY(1)%MP(2) <= NDSAVE) THEN
              CALL FMINT(MXY(15),MXY(10))
              IF (FMCOMP(MXY(15),'==',MXY(10))) KFL = -4
          ELSE
              KFL = -4
          ENDIF
          IF (KFL /= 0) THEN
              CALL FMST2M('UNKNOWN',MXY(11))
              KFLAG = -4
              GO TO 200
          ELSE
              CALL FMI2M(1,MXY(5))
              CALL FMSUB_R2(MXY(5),MXY(15))
          ENDIF
      ENDIF

!             Determine which method to use.

!             NMETHD = 1 means use the polynomial approximation,
!                    = 2 means use the asymptotic series.

      NMETHD = 1
      CALL FMNINT(MXY(15),MXY(2))
      CALL FMSUB(MXY(15),MXY(2),MXY(6))
      CALL FMM2DP(MXY(6),Z)
      Z = MAX(ABS(Z),1.0D-50)
      IF (KFLAG /= 0 .OR. ABS(Z) >= 1) THEN
          NMETHD = 2
      ELSE
          IF (190*LOG(Z) - 77*DLOGTN >= -(NDIG+1)*DLOGMB .OR. -190*DLOGTN >= -NDIG*DLOGMB) THEN
              NMETHD = 2
          ENDIF
      ENDIF
      CALL FMM2DP(MXY(15),X)
      IF (KFLAG /= 0) THEN
          NMETHD = 2
      ELSE IF (NMETHD == 1) THEN
          IF (X > 145 - (8+NDIG*DLOGMB/(25*DLOGTN))*LOG(Z) .OR. X > 250) NMETHD = 2
      ENDIF
      IF (NMETHD == 2) GO TO 170

!             Method 1.  Use the polynomial c(0) + c(1)*(x-3) + ... + c(196)*(x-3)**196

      IF (MA%MP(1) > 0 .AND. NDSAVE+NGRD52 < NDIG .AND. KR_RETRY == 0 .AND. KC_RETRY == 0)  &
          NDIG = NDSAVE + NGRD52

!             Since precision may have changed, check again to see if the saved value from a
!             previous call can be used as a shortcut.

      IF (MBASE == MBSGAM .AND. NDIG <= NDGGAM) THEN
          IF (MA%MP(1) == -1) THEN
              CALL FMINT(MXY(1),MXY(10))
              IF (FMCOMP(MXY(1),'==',MXY(10))) THEN
                  CALL FMST2M('UNKNOWN',MXY(11))
                  KFLAG = -4
                  GO TO 200
              ENDIF
          ENDIF
          CALL FMSUB(MXY(1),M_GAMMA_MA,MXY(7))
          IF (MXY(7)%MP(3) == 0) THEN
              CALL FMEQ(M_GAMMA_MB,MXY(11))
              GO TO 200
          ENDIF
          KWRNSV = KWARN
          KWARN = 0
          CALL FMM2I(MXY(7),KDIFF)
          KWARN = KWRNSV
          IF (KFLAG == 0 .AND. ABS(KDIFF) <= 50) THEN
              IF (KDIFF > 0) THEN
                  CALL FMEQ(M_GAMMA_MA,MXY(10))
              ELSE
                  CALL FMEQ(MXY(1),MXY(10))
              ENDIF
              CALL FMEQ(MXY(10),MXY(9))
              DO J = 1, ABS(KDIFF)-1
                 CALL FMI2M(1,MXY(5))
                 CALL FMADD_R1(MXY(10),MXY(5))
                 CALL FMMPY_R1(MXY(9),MXY(10))
              ENDDO
              IF (KDIFF > 0) THEN
                  CALL FMMPY(M_GAMMA_MB,MXY(9),MXY(11))
              ELSE
                  CALL FMDIV(M_GAMMA_MB,MXY(9),MXY(11))
              ENDIF
              GO TO 190
          ENDIF
      ENDIF
      CALL FMM2I(MXY(2),LSHIFT)
      LSHIFT = LSHIFT - 3
      IF (NDIG_C < NDIG .OR. MBASE_C /= MBASE) THEN
          CALL FMGAM_C(NDIG_C,MBASE_C,C)
      ENDIF
      J2 = (0.38*LOG(Z) + 4.1)*(NDIG*DLOGMB/(84*DLOGTN))
      J2 = MAX(2,MIN(J2+MOD(J2,2),LJSUMS))
      NDSAV1 = NDIG
      DO J = 1, 2
         CALL FMEQ(C(J-1),MJSUMS(J))
      ENDDO
      CALL FMSQR(MXY(6),MXY(5))
      CALL FMEQ(MXY(5),MXY(17))
      NTERM = 1
  120 IF (NTERM > 1) THEN
          K = NDIG
          NDIG = NDSAV1
          CALL FMCSMPY_R1(MXY(17),MXY(5))
          NDIG = K
      ENDIF
      DO J = 1, 2
         NTERM = NTERM + 1
         CALL FMEQ(MXY(17),MXY(4))
         CALL FMCSMPY_R1(MXY(4),C(NTERM))
         NDIG = NDSAV1
         CALL FMCSADDNN_R1(MJSUMS(J),MXY(4))
         IF (KFLAG /= 0 .OR. NTERM == 13) GO TO 130
         NDIG = NDSAV1 - INT(MJSUMS(J)%MP(2)-MXY(4)%MP(2))
         NDIG = MIN(NDSAV1,NDIG)
         IF (NDIG < NGRD22) NDIG = NGRD22
      ENDDO
      GO TO 120
  130 KFL = KFLAG
      KFLAG = 0
      CALL FMCSNSUMS(2,MJSUMS)
      NDIG = NDSAV1
      CALL FMEQ(MJSUMS(2),MXY(16))
      CALL FMEQ(MXY(6),MXY(3))
      CALL FMCSMPY_R1(MXY(16),MXY(3))
      CALL FMCSADD_R1(MXY(16),MJSUMS(1))
      IF (KFL == 0) THEN
          CALL FMCSMPY_R1(MXY(17),MXY(5))
      ELSE
          CALL FMI2M(0,MXY(8))
          GO TO 160
      ENDIF
      CALL FMI2M(1,MXY(4))
      DO J = 14, J2+13
         CALL FMADD(C(J),MXY(4),MJSUMS(J-13))
      ENDDO
      CALL FMIPWR(MXY(6),J2,MXY(5))
      CALL FMEQ(MXY(5),MXY(7))
      NTERM = J2 + 13
  140 IF (NTERM > J2+13) CALL FMCSMPY_R1(MXY(7),MXY(5))
      DO J = 1, J2
         NTERM = NTERM + 1
         CALL FMEQ(MXY(7),MXY(4))
         CALL FMCSMPY_R1(MXY(4),C(NTERM))
         NDIG = NDSAV1
         CALL FMCSADDNN_R1(MJSUMS(J),MXY(4))
         IF (KFLAG /= 0 .OR. NTERM == 196) GO TO 150
         NDIG = NDSAV1 - INT(MJSUMS(J)%MP(2)-MXY(4)%MP(2))
         NDIG = MIN(NDSAV1,NDIG)
         IF (NDIG < NGRD22) NDIG = NGRD22
      ENDDO
      GO TO 140
  150 KFLAG = 0
      CALL FMCSNSUMS(J2,MJSUMS)
      NDIG = NDSAV1
      IF (NTERM == 196) THEN
          GO TO 170
      ENDIF
      CALL FMI2M(1,MXY(4))
      CALL FMSUB(MJSUMS(J2),MXY(4),MXY(8))
      CALL FMEQ(MXY(6),MXY(3))
      MXY(3)%MP(1) = -MXY(3)%MP(1)
      DO J = 2, J2
         CALL FMCSMPY_R1(MXY(8),MXY(3))
         CALL FMSUB_R1(MJSUMS(J2-J+1),MXY(4))
         CALL FMCSADD_R1(MXY(8),MJSUMS(J2-J+1))
      ENDDO

!             Recover from using a shifted argument.

  160 CALL FMCSMPY_R1(MXY(8),MXY(17))
      CALL FMADD(MXY(8),MXY(16),MXY(10))
      IF (LSHIFT < 0) THEN
          CALL FMEQ(MXY(15),MXY(3))
          CALL FMEQ(MXY(15),MXY(14))
          DO J = 2, -LSHIFT
             CALL FMADDI(MXY(14),1)
             CALL FMCSMPY_R1(MXY(3),MXY(14))
          ENDDO
          CALL FMDIV_R1(MXY(10),MXY(3))
      ENDIF
      IF (LSHIFT > 0) THEN
          IF (MOD(LSHIFT,4) == 0) THEN
              CALL FMI2M(1,MXY(3))
              CALL FMEQ(MXY(6),MXY(14))
              CALL FMADDI(MXY(14),3)
          ELSE
              CALL FMEQ(MXY(6),MXY(3))
              CALL FMADDI(MXY(3),3)
              CALL FMEQ(MXY(3),MXY(14))
              DO J = 1, MOD(LSHIFT,4)-1
                 CALL FMADDI(MXY(14),1)
                 CALL FMCSMPY_R1(MXY(3),MXY(14))
              ENDDO
              CALL FMADDI(MXY(14),1)
          ENDIF
          LSHIFT = LSHIFT - MOD(LSHIFT,4)

!             The product Z*(Z+1)*...*(Z+LSHIFT-1) is computed four terms at a time to reduce
!             the number of FMMPY calls.

!             MXY(14) is Z
!             MXY(6) is Z**2
!             MXY(7) is Z**3
!             MXY(8) is (Z+K)*...*(Z+K+3)
!             MXY(11) is the current product

          CALL FMI2M(1,MXY(11))
          IF (LSHIFT > 0) THEN
              CALL FMSQR(MXY(14),MXY(6))
              CALL FMMPY(MXY(14),MXY(6),MXY(7))
              CALL FMSQR(MXY(6),MXY(8))
              CALL FMCSMPYI(MXY(7),6,MXY(12))
              CALL FMCSADD_R1(MXY(8),MXY(12))
              CALL FMCSMPYI(MXY(6),11,MXY(12))
              CALL FMCSADD_R1(MXY(8),MXY(12))
              CALL FMCSMPYI(MXY(14),6,MXY(12))
              CALL FMCSADD_R1(MXY(8),MXY(12))
              CALL FMEQ(MXY(8),MXY(11))
              CALL FMCSMPYI_R1(MXY(7),16)
              DO K = 0, LSHIFT-8, 4
                 CALL FMCSADD_R1(MXY(8),MXY(7))
                 K2 = 24*(2*K + 7)
                 CALL FMCSMPYI(MXY(6),K2,MXY(12))
                 CALL FMCSADD_R1(MXY(8),MXY(12))
                 IF (K <= SQRT(REAL(INTMAX)/49.0)) THEN
                     K1 = 8*(6*K*K + 42*K + 79)
                     CALL FMCSMPYI(MXY(14),K1,MXY(12))
                     CALL FMCSADD_R1(MXY(8),MXY(12))
                 ELSE
                     K1 = 48*K
                     CALL FMCSMPYI(MXY(14),K1,MXY(12))
                     CALL FMCSMPYI_R1(MXY(12),K)
                     CALL FMCSADD_R1(MXY(8),MXY(12))
                     K1 = 336*K + 632
                     CALL FMCSMPYI(MXY(14),K1,MXY(12))
                     CALL FMCSADD_R1(MXY(8),MXY(12))
                 ENDIF
                 IF (K <= (REAL(INTMAX)/17.0)**0.3333) THEN
                     K0 = 8*(2*K + 7)*(K*K + 7*K + 15)
                     CALL FMADDI(MXY(8),K0)
                 ELSE IF (K <= SQRT(REAL(INTMAX)*0.9)) THEN
                     K0 = 8*(2*K + 7)
                     CALL FMI2M(K0,MXY(12))
                     K0 = K*K + 7*K + 15
                     CALL FMCSMPYI_R1(MXY(12),K0)
                     CALL FMCSADD_R1(MXY(8),MXY(12))
                 ELSE
                     K0 = 8*(2*K + 7)
                     CALL FMI2M(K0,MXY(12))
                     CALL FMCSMPYI(MXY(12),K,MXY(9))
                     CALL FMCSMPYI_R1(MXY(9),K)
                     CALL FMCSADD_R1(MXY(8),MXY(9))
                     K0 = 7*K + 15
                     CALL FMCSMPYI_R1(MXY(12),K0)
                     CALL FMCSADD_R1(MXY(8),MXY(12))
                 ENDIF
                 CALL FMCSMPY_R1(MXY(11),MXY(8))
              ENDDO
          ENDIF
          CALL FMCSMPY_R1(MXY(11),MXY(3))
          CALL FMCSMPY_R1(MXY(10),MXY(11))
      ENDIF
      CALL FMEQ(MXY(10),MXY(11))
      GO TO 180

!             Method 2.  Use the Ln(Gamma(z)) asymptotic series, then use exp.
!                        To speed the asymptotic series calculation,
!                        increase the argument by LSHIFT.

  170 KWSAVE = KWARN
      KWARN = 0
      CALL FMM2I(MXY(15),INTA)
      KWARN = KWSAVE

      IF (KFLAG == -4) THEN
          LSHIFT = 0
      ELSE
          LSHIFT = INT(MAX(0.0,REAL(NDIG)*ALOGMB/4.46-REAL(INTA)))
      ENDIF
      IF (LSHIFT > 0) LSHIFT = 4*(LSHIFT/4 + 1)
      IF (KFLAG == 0) THEN
          IF (INTA <= MAX(200,INT(DLOGMB*NDIG))) THEN
              IF (INTA <= 2) THEN
                  CALL FMI2M(1,MXY(11))
                  GO TO 180
              ENDIF
              INTA = INTA - 1
              CALL FMFCTI(INTA,MXY(11))
              GO TO 180
          ENDIF
      ENDIF

      IF (LSHIFT /= 0) THEN
          CALL FMI2M(LSHIFT,MXY(5))
          CALL FMADD(MXY(15),MXY(5),MXY(14))
      ELSE
          CALL FMEQ(MXY(15),MXY(14))
      ENDIF

!             Get Gamma for the shifted argument.

!             Compute IEXTRA, the number of extra digits required to compensate for cancellation
!             error when the argument is large.

      IEXTRA = MIN(MAX(INT(MXY(14)%MP(2))-1,0),INT(1.0+ALOGMX/ALOGMB))
      IF (IEXTRA > 0) THEN
          CALL FMEQU_R1(MXY(14),NDIG,NDIG+IEXTRA)
          CALL FMEQU_R1(MXY(15),NDIG,NDIG+IEXTRA)
      ENDIF
      NDSAV2 = NDIG
      NDIG = NDIG + IEXTRA
      CALL FMLNGM(MXY(14),MXY(4))
      CALL FMEXP(MXY(4),MXY(11))

      NDIG = NDSAV2

!             Reverse the shifting.
!             The product MA*(MA+1)*...*(MA+LSHIFT-1) is computed four terms at a time to reduce
!             the number of FMMPY calls.

!             MXY(6)  is Z
!             MXY(7)  is Z**2
!             MXY(8)  is Z**3
!             MXY(9)  is (Z+K)*...*(Z+K+3)
!             MXY(12) is the current product

      CALL FMEQ(MXY(15),MXY(6))
      IF (LSHIFT > 0) THEN
          CALL FMSQR(MXY(6),MXY(7))
          CALL FMMPY(MXY(6),MXY(7),MXY(8))
          CALL FMSQR(MXY(7),MXY(9))
          CALL FMCSMPYI(MXY(8),6,MXY(13))
          CALL FMCSADD_R1(MXY(9),MXY(13))
          CALL FMCSMPYI(MXY(7),11,MXY(13))
          CALL FMCSADD_R1(MXY(9),MXY(13))
          CALL FMCSMPYI(MXY(6),6,MXY(13))
          CALL FMCSADD_R1(MXY(9),MXY(13))
          CALL FMEQ(MXY(9),MXY(12))
          CALL FMCSMPYI_R1(MXY(8),16)
          LARGE = INTMAX
          DO K = 0, LSHIFT-8, 4
             CALL FMCSADD_R1(MXY(9),MXY(8))
             K2 = 24*(2*K + 7)
             CALL FMCSMPYI(MXY(7),K2,MXY(13))
             CALL FMCSADD_R1(MXY(9),MXY(13))
             IF (K <= SQRT(REAL(LARGE)/49.0)) THEN
                 K1 = 8*(6*K*K + 42*K + 79)
                 CALL FMCSMPYI(MXY(6),K1,MXY(13))
                 CALL FMCSADD_R1(MXY(9),MXY(13))
             ELSE
                 K1 = 48*K
                 CALL FMCSMPYI(MXY(6),K1,MXY(13))
                 CALL FMCSMPYI_R1(MXY(13),K)
                 CALL FMCSADD_R1(MXY(9),MXY(13))
                 K1 = 336*K + 632
                 CALL FMCSMPYI(MXY(6),K1,MXY(13))
                 CALL FMCSADD_R1(MXY(9),MXY(13))
             ENDIF
             IF (K <= (REAL(LARGE)/17.0)**0.3333) THEN
                 K0 = 8*(2*K + 7)*(K*K + 7*K + 15)
                 CALL FMADDI(MXY(9),K0)
             ELSE IF (K <= SQRT(REAL(LARGE)*0.9)) THEN
                 K0 = 8*(2*K + 7)
                 CALL FMI2M(K0,MXY(13))
                 K0 = K*K + 7*K + 15
                 CALL FMCSMPYI_R1(MXY(13),K0)
                 CALL FMCSADD_R1(MXY(9),MXY(13))
             ELSE
                 K0 = 8*(2*K + 7)
                 CALL FMI2M(K0,MXY(13))
                 CALL FMCSMPYI(MXY(13),K,MXY(10))
                 CALL FMCSMPYI_R1(MXY(10),K)
                 CALL FMCSADD_R1(MXY(9),MXY(10))
                 K0 = 7*K + 15
                 CALL FMCSMPYI_R1(MXY(13),K0)
                 CALL FMCSADD_R1(MXY(9),MXY(13))
             ENDIF
             CALL FMCSMPY_R1(MXY(12),MXY(9))
          ENDDO
          CALL FMDIV_R1(MXY(11),MXY(12))
      ENDIF

!             Use the reflection formula if MA was less than -1/2.

  180 IF (KRFLCT == 1) THEN

!             Reduce the argument before multiplying by Pi.

          CALL FMNINT(MXY(15),MXY(7))
          CALL FMDIVI(MXY(7),2,MXY(8))
          CALL FMINT(MXY(8),MXY(2))
          CALL FMMPYI(MXY(2),2,MXY(9))
          KSIGN = -1
          IF (FMCOMP(MXY(7),'==',MXY(9))) KSIGN = 1
          CALL FMSUB(MXY(15),MXY(7),MXY(10))
          CALL FMPI(MXY(12))
          CALL FMMPY_R1(MXY(12),MXY(10))
          KRSAVE = KRAD
          KRAD = 1
          CALL FMSIN(MXY(12),MXY(3))
          MXY(3)%MP(1) = KSIGN*MXY(3)%MP(1)
          KRAD = KRSAVE
          CALL FMDIV_R2(MPISAV,MXY(3))
          CALL FMDIV_R2(MXY(3),MXY(11))
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  190 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(11)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF

  200 CALL FMEQ(MXY(1),M_GAMMA_MA)
      CALL FMEQ(MXY(11),M_GAMMA_MB)
      NDGGAM = NDIG
      IF (ABS(M_GAMMA_MB%MP(2)) > MEXPOV) NDGGAM = 0
      MBSGAM = MBASE

      CALL FMEXT2(MXY(11),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMGAM

      SUBROUTINE FMGAM_C(NDIG_C,MBASE_C,C)

!  Initialize the constants used in the gamma polynomial.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: C(0:196)
      INTEGER :: NDIG_C
      REAL (KIND(1.0D0)) :: MBASE_C
      INTENT (INOUT) :: C,NDIG_C,MBASE_C
      INTEGER :: NDSAVE
      CHARACTER(220) :: ST

      NDSAVE = NDIG
      NDIG = MAX(NDIG,NINT(210*DLOGTN/DLOGMB))
      NDIG_C = NDIG
      MBASE_C = MBASE

      ST = " 2 "
      CALL FMST2M(ST,C(0))
      ST = " 1.845568670196934278786975819835195137915681328120152802388465530" //  &
           "230264546444670658126105873416506500970737105500385835038078991971" //  &
           "02691432755165200471015292749299933251412532452465211441480948350581017M+0"
      CALL FMST2M(ST,C(1))
      ST = " 1.246464995951346528971255032754062122751889033636657384469089862" //  &
           "717670083765963530684050979376234434063149542816318341831199882570" //  &
           "54136293454241409125655601075903604372266188019198826399323980756659507M+0"
      CALL FMST2M(ST,C(2))
      ST = " 5.749941689206122275465545297069514629181683280995911506411578080" //  &
           "726118046263102563376406423582057300595042023225863076035376079997" //  &
           "58104823944616583929664137138189407083120305067949277678404599957423943M-1"
      CALL FMST2M(ST,C(3))
      ST = " 2.300749407541140630184757374755110638744965329574536384291172327" //  &
           "224547634301735916779398033223621567106468104559113876289562540922" //  &
           "49810183476230067472728208959273220898874770326648720581128183324434328M-1"
      CALL FMST2M(ST,C(4))
      ST = " 7.371504661602386878317788712652808511825657580343462427360683175" //  &
           "509260544574956944742301697314778437297368730369274995615677873789" //  &
           "35133238331475104267889872993214666032986278826546933416543800481312385M-2"
      CALL FMST2M(ST,C(5))
      ST = " 2.204110936751696733001055930504381988327566876073187686154801967" //  &
           "909973103233273981903764688650115297934327575508639951789708251337" //  &
           "92707722587521058280471694968009955629145006098566559417433720825728363M-2"
      CALL FMST2M(ST,C(6))
      ST = " 5.448754075820309416743506865467445124448139461938118242185774040" //  &
           "091229588005782816224467061542163141883870132680748064163312525326" //  &
           "79285947455982778971828128624591203392889667559929450792010659863151692M-3"
      CALL FMST2M(ST,C(7))
      ST = " 1.355220860239435200782800511692912879531376156061699075016521316" //  &
           "639789508602632933261809843993626303860552687494426922068347662436" //  &
           "50638664308894605216771132545723510121680479395222846647397509645416658M-3"
      CALL FMST2M(ST,C(8))
      ST = " 2.647856630454963762916703027588487719988377646494592862588577877" //  &
           "670108768701128952589855350528375734341831836105261848089596212014" //  &
           "43438318203511988335596380750860411655605437077878364306986421443576571M-4"
      CALL FMST2M(ST,C(9))
      ST = " 6.120306281920072864297933044336079606323020239995311203374612431" //  &
           "474530317806041317622369821667524651305326663533305730887143946711" //  &
           "61266460849960962516510977842732486514289292246744768624817147886489117M-5"
      CALL FMST2M(ST,C(10))
      ST = " 8.505579174881354789674853004022777303159200477826996950880457101" //  &
           "421412876328884015915508261943090083057713903043228932335601231203" //  &
           "40141960454601937991265277526358454689641320642200080030286310049533045M-6"
      CALL FMST2M(ST,C(11))
      ST = " 2.406177240131441866815352548925218907359016756341071832415175589" //  &
           "258269044824185640836078834625474374503849871861065571818953770220" //  &
           "34975008487473056363745663434694457043463856223590513289898353487696130M-6"
      CALL FMST2M(ST,C(12))
      ST = " 8.802390990648096801589149015845004893557805933708362167755975543" //  &
           "372057695537675243732864757390172399681575843089705450975708832670" //  &
           "47894590480007482518734894597525237926945509694423313414680575733951262M-8"
      CALL FMST2M(ST,C(13))
      ST = " 1.142227645342158377586670448383448546854299529797736913759691985" //  &
           "910949533336448816683159056251333466684021985223318722135340801690" //  &
           "72260625981817135095639775689249807942339836246106905787385604364337194M-7"
      CALL FMST2M(ST,C(14))
      ST = " 1.631475210082743727954198968671352602693131133480080900245408554" //  &
           "939316524124591839852475746114674778530360919625180046232777005122" //  &
           "27236317509067632839828070252526276470500723929148927321567689434907911M-8"
      CALL FMST2M(ST,C(15))
      ST = " 8.623497389978272698928826543954910754554854080065660353539135917" //  &
           "773916063595516849273098040739690585246037837539507713246122281847" //  &
           "07020525306666205624027189054003604432165771697650991757066138256773314M-9"
      CALL FMST2M(ST,C(16))
      ST = " 2.441104025235454263800352688428668916552720940475516379601031040" //  &
           "328703631852876301331001731922670655825329213688582617123340479731" //  &
           "30453030855966898642614830460583649086634599347191276139711017402687474M-9"
      CALL FMST2M(ST,C(17))
      ST = " 8.729150638722326041474546798911555513733749137711634358293958504" //  &
           "106518519494766446793260191698472463238634461285021617971659072089" //  &
           "64830740638437222612735276515039966197638188663669442608898762584643632M-10"
      CALL FMST2M(ST,C(18))
      ST = " 2.839029513083401421929170732563229229668325235436226039051054572" //  &
           "783508115759922623716045941336385750870556656615699867502848974601" //  &
           "25690803078266597331301210600902409196080711025610915592408863593987027M-10"
      CALL FMST2M(ST,C(19))
      ST = " 9.560889804988413908509285509380646041154360005479482459192247765" //  &
           "357528992664976535701311631674646910607500850899901331610584447013" //  &
           "61092693917685042900085585432034263673380064316273851782544357969469641M-11"
      CALL FMST2M(ST,C(20))
      ST = " 3.178270013361840262252491679259206811383776452439447460847595023" //  &
           "655033077497304599287176316699672432816255052883697868521388307529" //  &
           "40383229860610241927494715179896090660025570060233690995092455619052433M-11"
      CALL FMST2M(ST,C(21))
      ST = " 1.061093469576602661180070676287816314794459534888083832039943125" //  &
           "106258601567850805437194313637331057164170163808855202657895983163" //  &
           "75070581362231098256864140947088822349901059528755152892715695385840670M-11"
      CALL FMST2M(ST,C(22))
      ST = " 3.536842812699221803341361602631772393030775267007960695600138342" //  &
           "475056171314799106864974683735705965078815702431194850519988946394" //  &
           "60297349449303074734807619318567432754104422976962193319806157373676528M-12"
      CALL FMST2M(ST,C(23))
      ST = " 1.179381890210746859019626609111101179189547278121204798102640669" //  &
           "089589106613555171765287658695791241618592970217136500883583515937" //  &
           "07042245469783966737397377617384891192343427489490590732643362286614565M-12"
      CALL FMST2M(ST,C(24))
      ST = " 3.931867733137363527554885326768660997026944151573526847589071835" //  &
           "861161866217034417877097706264724092166846479646830599648529767479" //  &
           "29972500613317852095515147938660038231478063008302625870299351511379064M-13"
      CALL FMST2M(ST,C(25))
      ST = " 1.310821437225892696503587059884518604419875759028225403332569486" //  &
           "263124550715228960875605764084965585015810053174223154386683364225" //  &
           "82940623880095674287091265178854323811489670635974124477257044106228515M-13"
      CALL FMST2M(ST,C(26))
      ST = " 4.369852680143739417301538884456930730266716017178643408797678607" //  &
           "489727662002478005547544394166113056281405054193512564204527806783" //  &
           "21820023428536694798337606178682340961609542697763176968052664696930530M-14"
      CALL FMST2M(ST,C(27))
      ST = " 1.456734329592486133181460315029414930377211960023817769883504280" //  &
           "093804008390039009055883271224157190963826905955811848380812603165" //  &
           "54224971663883071707665069028985176275185941888330865659033519170133935M-14"
      CALL FMST2M(ST,C(28))
      ST = " 4.856068671580721652825423134564758955030518962120078212851284455" //  &
           "507593020938761405142314134920533228978526051606006790133466381223" //  &
           "50359680218686853748359459683818517509731959413893343189086282950465935M-15"
      CALL FMST2M(ST,C(29))
      ST = " 1.618761872087034547065539210485881034248882944054015543045073210" //  &
           "910330056730122088065602760823181836215863575050044591541254549198" //  &
           "06533155170588574561959173365139660326081560892373137180542173284519690M-15"
      CALL FMST2M(ST,C(30))
      ST = " 5.396053371990928374785154960328771472570021055817837913960904096" //  &
           "760099091618418168792262897777467993922856366402610309885017994638" //  &
           "31375328961628239846054792847689945091796341112338488175505578851091459M-16"
      CALL FMST2M(ST,C(31))
      ST = " 1.798729611660976774380774106560359446517486065245326232319884896" //  &
           "239912275072122853296640979664466879259429595297450987702461004648" //  &
           "40931772056892963070221773391675849856440134420289464625930506084236358M-16"
      CALL FMST2M(ST,C(32))
      ST = " 5.995878246226820269265104853882744953833262677515218945976170565" //  &
           "702779924974464149394857574835844004490020924890860726739848899923" //  &
           "95696324825550829660096649087698612645940055809410964879423696294173532M-17"
      CALL FMST2M(ST,C(33))
      ST = " 1.998654305589399275726998250553254783542933078465976133420153128" //  &
           "272381058668053909214099708471764295907486937756314951544992358421" //  &
           "97144228984621236873733301083447645823872653994272286143235273563570481M-17"
      CALL FMST2M(ST,C(34))
      ST = " 6.662251581403731529306004514600953655139824062623862163440755266" //  &
           "354957947460012639499946309112697689542491881916960750384664086547" //  &
           "74220845245988438400136805239697524619278380861197973816032911820925462M-18"
      CALL FMST2M(ST,C(35))
      ST = " 2.220768169107932862809420803989156159852745026278540490315171027" //  &
           "166630658162840832652660890951304947572979341837638582334903909155" //  &
           "05010378303851983844455677013599258308919578609796169265316664802443791M-18"
      CALL FMST2M(ST,C(36))
      ST = " 7.402604670809900203376581979566914810234491834807160670117173102" //  &
           "679048535794208132168246922357691220447769517665621319685807754668" //  &
           "30524972355162004213858351086934604282899918908012797069231868984875389M-19"
      CALL FMST2M(ST,C(37))
      ST = " 2.467545917514843068413870814987535001534095121397550756283431485" //  &
           "174239672848806618511211094654861620856356668212851618482113031080" //  &
           "96983868763519240779629920703298714890476973363239861006308885857104223M-19"
      CALL FMST2M(ST,C(38))
      ST = " 8.225180627404782368968305939492307217250324537166525666738082095" //  &
           "443986514199471391717760463558674271532612180804614900051667267307" //  &
           "84828422378491452316833093910246472565776447483034439499635086721544230M-20"
      CALL FMST2M(ST,C(39))
      ST = " 2.741733768240659509936720350920901622511204615005846102750077012" //  &
           "225034115334522335690591847723821637675971676287391586929140867246" //  &
           "24157389063680220659693708857804287064240054553450007399411745938008177M-20"
      CALL FMST2M(ST,C(40))
      ST = " 9.139129792265891288417212538920834449088699985871081943229349237" //  &
           "308947859965368326191615768810794304428951588196306949098368574539" //  &
           "28605395142908365050404570472066333401820331325326810443520131888072775M-21"
      CALL FMST2M(ST,C(41))
      ST = " 3.046380905361179933871534873717109530976853470108494529882869084" //  &
           "727927972747326006151030730201866380503024676995328258901563345953" //  &
           "00658838459344187262962809267579300899208726174226364547735769503160384M-21"
      CALL FMST2M(ST,C(42))
      ST = " 1.015461378786517931007333337553073166997854684093926238094172044" //  &
           "744795968432075139196598233114739312125317801492097820991352317417" //  &
           "40293799704807080092075189476011412233026066523677916838565466920346563M-21"
      CALL FMST2M(ST,C(43))
      ST = " 3.384873955149684581869145002322761445395722444488361773402583112" //  &
           "448446196579967067830143563552847474861011075678427137967175986491" //  &
           "77423399240122512958345857834877457342567505386357823149491980041361130M-22"
      CALL FMST2M(ST,C(44))
      ST = " 1.128291991521080392925404045314099582618612165311869719114516335" //  &
           "417518456246100356538652790526704797786336190873078001030469915965" //  &
           "30342451983442856226503526150387619863083398681667798679726159209459487M-22"
      CALL FMST2M(ST,C(45))
      ST = " 3.760974987926624413050052192380026663425879102031305346359345746" //  &
           "935876097802220422384767553022229983969369775506060549782771493660" //  &
           "85309029600033693383948806798756523782518697759939484580588143464857350M-23"
      CALL FMST2M(ST,C(46))
      ST = " 1.253658750025309164633924716897191398251340412647146221607418983" //  &
           "652266605895765756004913716691084072256991264216324248980009909612" //  &
           "59662905496332238803040488263666172235045176892125202448596644986008747M-23"
      CALL FMST2M(ST,C(47))
      ST = " 4.178863551880140415652989937671280452129584936360929800867252116" //  &
           "863675482287565904618697224054245640513684845309376666513701521263" //  &
           "52903495786573790786121590010548700491620543046740323819627331118835276M-24"
      CALL FMST2M(ST,C(48))
      ST = " 1.392954780243262418680115701340497736540387201421965383888625288" //  &
           "310091918535511115914306440504769869531128337443764472227581178596" //  &
           "20874940378776837220110757980964940848165877287239082243508787832464445M-24"
      CALL FMST2M(ST,C(49))
      ST = " 4.643183258187456757363871480237105948494753871341586050997508099" //  &
           "595411224954239240596072142323602079719155358372134330760070497787" //  &
           "55364393549841452404607067507295993200758608506748675165475527877530949M-25"
      CALL FMST2M(ST,C(50))
      ST = " 1.547727917073673030893407669122051522765823276651858723363536117" //  &
           "860559780947473082931957648716608923599799859242924550204192502327" //  &
           "93202961853805884072569165830891801693280119768515402102389811485890830M-25"
      CALL FMST2M(ST,C(51))
      ST = " 5.159093467774295934623929324290510691555682927619718795937537456" //  &
           "443748751641411020603883876314313578961537671360400477911224749990" //  &
           "32619951820624633170971754679797702479266215176809962392844181877752600M-26"
      CALL FMST2M(ST,C(52))
      ST = " 1.719697925307095214819334128534788375205527844306001827485335608" //  &
           "178891188953930792336558066117385344867670787386310361133421997787" //  &
           "32437687430376200145015883834569265381524778650808598371769510961689717M-26"
      CALL FMST2M(ST,C(53))
      ST = " 5.732326674479775700977445632081415478242873164984623088760026987" //  &
           "046479843038489986034336241981077004936330751439598114357862316445" //  &
           "25391747885525449466566054526270451667824579066863518701758860381778789M-27"
      CALL FMST2M(ST,C(54))
      ST = " 1.910775622357349859006177503807224410484768271435458299879734511" //  &
           "091872750666800933837592866045341090057920381872911981841539613531" //  &
           "34532853835424993139130432850991078908443110651776717566117546886621263M-27"
      CALL FMST2M(ST,C(55))
      ST = " 6.369252235018181185695546747418083648737457861666758649658733495" //  &
           "450326624296352111648047112820334535082139732219092514871542197623" //  &
           "45438407830542732130710826023492126216948058010058111121438710893185370M-28"
      CALL FMST2M(ST,C(56))
      ST = " 2.123084118462838161068146373210838937742442106504888126619598244" //  &
           "689122838503449807948835352511168142442994048637204680573237012416" //  &
           "88214862798756633312023510560919642366853351380446601871574640377801289M-28"
      CALL FMST2M(ST,C(57))
      ST = " 7.076947161851452988857602069175395219260001742350326617810798286" //  &
           "412997417428572002897367283988898197215995346896337658412178803828" //  &
           "71600585988840954970771671278144596708302210216220397647874970105717455M-29"
      CALL FMST2M(ST,C(58))
      ST = " 2.358982412360992050118851554506979955647201613693684896233810502" //  &
           "944787306402820374939041591698662853950957584483745174219063623695" //  &
           "13294756585697364356697316138631583855315160276428811116906407698998717M-29"
      CALL FMST2M(ST,C(59))
      ST = " 7.863274770562928683386208182169555716408523732927181605570254642" //  &
           "783489336698077228912261535122643231786047779260925663715692218511" //  &
           "54079525591282153925777222923833127312465613000068227777044732926174616M-30"
      CALL FMST2M(ST,C(60))
      ST = " 2.621091605860885533233898952848549221472883408639509048156174492" //  &
           "952501040386862212314276518084902725209461671925415533723193076219" //  &
           "02166363594957080995260573241730794841247260537467180156250342238881167M-30"
      CALL FMST2M(ST,C(61))
      ST = " 8.736972058719399393773664808053499235464951884789194640368079086" //  &
           "177143026321168823182202095050518931856284116458482040667912530023" //  &
           "17830489701338487555343938051214348611787932621065770035562250631323146M-31"
      CALL FMST2M(ST,C(62))
      ST = " 2.912324029368913239221905377728027928932487827862157086587586846" //  &
           "896450700145411015082377114761533704468584083695734375828642409882" //  &
           "69259984928264053284030731675607849783920249775114691953904644907709015M-31"
      CALL FMST2M(ST,C(63))
      ST = " 9.707746789052497475085146052433896715360382198339051019251763880" //  &
           "518185755627159066562273677244070805548234512751403592056913207327" //  &
           "80251017233714875898746055682870640485635412611964103637495935078818638M-32"
      CALL FMST2M(ST,C(64))
      ST = " 3.235915602473196442669228672981134089711213167383910530833953829" //  &
           "792990006951794162312454635926395818450571401725370707981435503583" //  &
           "55570191731695332982121199537121246583519326445577596987891273510182193M-32"
      CALL FMST2M(ST,C(65))
      ST = " 1.078638535688323258277639294637321095007154872462571028231301143" //  &
           "605467142930516231430752807470673434147256477737840344704980947455" //  &
           "43053554636974298772295627289032328328373036848556849010802235448385016M-32"
      CALL FMST2M(ST,C(66))
      ST = " 3.595461789454222217017444496718864771407199081151484546256978274" //  &
           "929484367469843471052909175769814275173782972424785067903628062172" //  &
           "38267851839020384161912309600523898207350166920811200389438694198073310M-33"
      CALL FMST2M(ST,C(67))
      ST = " 1.198487264108026960553291549779110538131003576080453431007006512" //  &
           "268160233237697351800460921121832519391521815357041008416084888598" //  &
           "27557131190172270346227678454195010189227547853345612460399635783630149M-33"
      CALL FMST2M(ST,C(68))
      ST = " 3.994957549418305520762016700744032876853769637744488924267188269" //  &
           "584393732051483805590060972426573672962844815489806656051668047426" //  &
           "97928538442568628891602616126915832496578665989847126282893366534237083M-34"
      CALL FMST2M(ST,C(69))
      ST = " 1.331652517070655772993246798258154050119486912201311455268437349" //  &
           "114193156345900532664095994600506577877633489606677937802093759650" //  &
           "12137846480275713137733722530126859812615084188357652189387132980702776M-34"
      CALL FMST2M(ST,C(70))
      ST = " 4.438841725063570781178568100283946176488573680079007649477905193" //  &
           "017992386971455066988895758316824569677887493778455225021106826855" //  &
           "29026392300941094435783945574507271437954562521405284456074260144096753M-35"
      CALL FMST2M(ST,C(71))
      ST = " 1.479613908728203152730390143133201780942639054825565044507758917" //  &
           "773304661939287594029105088656760879117520103897129013490206639754" //  &
           "87435121659508261905208347102886094942106298340234319457765604513963509M-35"
      CALL FMST2M(ST,C(72))
      ST = " 4.932046363361542755686335602696750869830288252199443432734105813" //  &
           "943462260083819436478927601708900720110450661448493818380083105762" //  &
           "71590161306544076696091310423964644508740590548145866774529617253109923M-36"
      CALL FMST2M(ST,C(73))
      ST = " 1.644015454687397316689944677831911620211552013511192085616959005" //  &
           "082583370344704494312506871603969621047795163740944612049682148927" //  &
           "59356353337859121253234322350982876222750860528515262326476034089950266M-36"
      CALL FMST2M(ST,C(74))
      ST = " 5.480051516208532057249213551633236335403175178397767120722791995" //  &
           "833335758082301610732726590856354303605009314671070073367443285322" //  &
           "81719944336949977298072286244404059176341969451639495219169668432874625M-37"
      CALL FMST2M(ST,C(75))
      ST = " 1.826683838882145937413050728363254416465068672788666223790911345" //  &
           "427048802205722820790024964162626522627539834294555447603439041292" //  &
           "92403842411688509922659248102884925797066250995812515878053578599537543M-37"
      CALL FMST2M(ST,C(76))
      ST = " 6.088946129972074589720378401074071029218658010685074635511831120" //  &
           "277695386768911269891962771874827650899040106965903278757448364187" //  &
           "28574155937467016044284632565747254397900833533383192879263530571086410M-38"
      CALL FMST2M(ST,C(77))
      ST = " 2.029648710081921896663063938682967174463636477103609117669608797" //  &
           "346033378612490707724833983253281429017993058131396854118481222339" //  &
           "85407951679907254491401107628467015394676060959591476686232563916557226M-38"
      CALL FMST2M(ST,C(78))
      ST = " 6.765495700501148906774995358078228324235325066490434291070320211" //  &
           "666634035845840680799492809322166392640915607858746928019847633868" //  &
           "45020162815858209793574963810268541266706689851018528823213713218014553M-39"
      CALL FMST2M(ST,C(79))
      ST = " 2.255165233557401948601014902186205602789864371948507888958512686" //  &
           "894666882870433310683786297682141353940837232625056953901094633832" //  &
           "03813175465801305121046951268786662654220745685289766957573976902789674M-39"
      CALL FMST2M(ST,C(80))
      ST = " 7.517217445333887278263065941349084170211801907036126548415826613" //  &
           "097540864542220787119574822998915671722740530800746773131556370537" //  &
           "17036257265431709600760536998990725283074006064618438905604324155029841M-40"
      CALL FMST2M(ST,C(81))
      ST = " 2.505739148480265955233204413682186719497131266833487675446538783" //  &
           "092174430438507742800231748635072748340810415087125614579378635688" //  &
           "66151322710496155055815000392267489014427962480151154107924769960320348M-40"
      CALL FMST2M(ST,C(82))
      ST = " 8.352463828356645340468994201986157494921881256993571664141509541" //  &
           "823273109890101870519752598083111861872055684078254288142631302953" //  &
           "90656071678927672118181416754149675034388261080192075026592829943869784M-41"
      CALL FMST2M(ST,C(83))
      ST = " 2.784154609474488152611480955615773344997254762720123569187499710" //  &
           "482501105031142742130099147104245620254144813103304543108707461283" //  &
           "03676294384838274788281739537719423506089439601147338491518140111085245M-41"
      CALL FMST2M(ST,C(84))
      ST = " 9.280515364970643106573953106487348079696240473524263398418948085" //  &
           "686805363427549650077259362814392417398055197835215069289928148824" //  &
           "67963286545183505632254520821084189068903494611293308100453524544737026M-42"
      CALL FMST2M(ST,C(85))
      ST = " 3.093505121670801685004800389514423527223386419080428824653989649" //  &
           "679279518505124420083928960371367692487927529341787666280479065858" //  &
           "20227384416309509890526585569312748427209499872674012860539332555258032M-42"
      CALL FMST2M(ST,C(86))
      ST = " 1.031168373893747390707549843146989045155823157470502623225595250" //  &
           "851710797449759760498072278428597174778192322827019231254514348869" //  &
           "35739852926303601231003771300391351671865972802045601948838060198753437M-42"
      CALL FMST2M(ST,C(87))
      ST = " 3.437227912987858374961865176796344063877564237772445606496240106" //  &
           "757864483254589585823971686699471956766584959383102055399682270269" //  &
           "52352941784639649980429822410674941181991395320188803002740041936488314M-43"
      CALL FMST2M(ST,C(88))
      ST = " 1.145742637664794559805828100464320725242403637362145522388352767" //  &
           "861006812839084798058391247493282342027006725688139044431839514660" //  &
           "53654774876669055000807030176806220727453288403581728457403167939166997M-43"
      CALL FMST2M(ST,C(89))
      ST = " 3.819142125554752953067839505315181594779066795846739238810201522" //  &
           "091877503536260790984284322275300395166507566249167177334314175598" //  &
           "37162971702086753249324866744493897786886912065021929714836592305111972M-44"
      CALL FMST2M(ST,C(90))
      ST = " 1.273047375186277089451795607705259637601635385366665197585993775" //  &
           "074526908019448436164455682411887757929212685231623835262681489558" //  &
           "82648858340071059461127000212205125717165045854603000054982553620290401M-44"
      CALL FMST2M(ST,C(91))
      ST = " 4.243491250624322227579766750508513228093573956231840709615359110" //  &
           "716873987770463557146491514771994072759888669351546219633295358033" //  &
           "66517307409098637643711145428866491214789294814344532842968257523643745M-45"
      CALL FMST2M(ST,C(92))
      ST = " 1.414497083542290391545199323374869916206725399320438141384531327" //  &
           "265477522477777915006882588147304129058542612492747596977199928651" //  &
           "94869243918440516701383178149493577193540062534758351817970713054178365M-45"
      CALL FMST2M(ST,C(93))
      ST = " 4.714990278476425427697520544930204334035129865733446210245204030" //  &
           "355616997118279522525088487975375771703855109208249987771017533193" //  &
           "90291285366875213381956263086437252260808891626942562877561594888642475M-46"
      CALL FMST2M(ST,C(94))
      ST = " 1.571663426159339506535953571864003053494761363991342538284158655" //  &
           "584260596318169435197761794741189000363244392470381778816827538385" //  &
           "70503152182049565811831241089687440405169499752285888183719228270040702M-46"
      CALL FMST2M(ST,C(95))
      ST = " 5.238878087199125931711927428699616143954533668740342266002176483" //  &
           "066317524800888207882487253378527961702366607212427983576029574311" //  &
           "70477008468882187587684440329802260450306316947736342927462019072062451M-47"
      CALL FMST2M(ST,C(96))
      ST = " 1.746292695733373871385356099575152433557917368382321044143123707" //  &
           "125733276511427813036854586467480879063125488695590156424518309858" //  &
           "40776387319594992052236593986791986838450118314507991811649011797951517M-47"
      CALL FMST2M(ST,C(97))
      ST = " 5.820975652445409306654690542379619928753899950100022072575451976" //  &
           "896049502425251330032699117677706811131644722750272961660598812323" //  &
           "95528274546713027672214860795936676307396171912021238503162387066702843M-48"
      CALL FMST2M(ST,C(98))
      ST = " 1.940325217482010536060783297163692278507114464120206717431891734" //  &
           "053010284461713330551549047577932453595163579118201359494986630173" //  &
           "01494093463680448771677875123107012368194613913742739952428603002168229M-48"
      CALL FMST2M(ST,C(99))
      ST = " 6.467750724940553704809014908981646231351194700919134365131054499" //  &
           "044206347270309742459318050095528539735477919872551695967650699099" //  &
           "88953248180134764653841795790617888566272316181621003159336089850848991M-49"
      CALL FMST2M(ST,C(100))
      ST = " 2.155916908313647547754610174771515398662821458991609891428955252" //  &
           "290583442620154212776250181824196267358169201447214242363354724316" //  &
           "89903280067672089810407626181445703141886320049309134936640041251647199M-49"
      CALL FMST2M(ST,C(101))
      ST = " 7.186389694379149274561055379686410434878982979762824712204962808" //  &
           "871607736477144813893452054031932010405337926726732941950536667006" //  &
           "84810252591610140816882396575440916998193275137670082479286587744869582M-50"
      CALL FMST2M(ST,C(102))
      ST = " 2.395463231459797453698442182708267050312141998051092331530640409" //  &
           "893924848764443035384500574394756654315653015364123642221973250880" //  &
           "68362683781580014370472600811020274312157465907532281112816349722673774M-50"
      CALL FMST2M(ST,C(103))
      ST = " 7.984877438199527417773369963127799969498648440970510669498583482" //  &
           "161029591317677710672043612633451504552983943030656888570069226104" //  &
           "04295259910858734307563648182821156659161484763549481645932818220413101M-51"
      CALL FMST2M(ST,C(104))
      ST = " 2.661625812733226448952431335647978161413961687446878335164533806" //  &
           "702910875419295287094135193138742564146281522194125689011703047524" //  &
           "45739306220536816835347815852152246454806822705745730784267021864361647M-51"
      CALL FMST2M(ST,C(105))
      ST = " 8.872086042444214770744709174167341653906439941556420653788154173" //  &
           "929048445735834186760727780453829909511846845819831435241835442504" //  &
           "04079891107632829549289090071219101574622546793854950277712602194795534M-52"
      CALL FMST2M(ST,C(106))
      ST = " 2.957362014148103242140721175656428119533141966179427551155676500" //  &
           "671725653671481845278656623962048055150306031384279834386038577295" //  &
           "72584556615286334177021422182622284915641930082182710316087748100105028M-52"
      CALL FMST2M(ST,C(107))
      ST = " 9.857873380493756603533616420387300817360969314828251071875082336" //  &
           "698703611411040709238371719721287012785465953801003782742219530827" //  &
           "73983924014324680294547858829752678323087789357138505841177563631130388M-53"
      CALL FMST2M(ST,C(108))
      ST = " 3.285957793497938650277342040351753052374376933619786220663421040" //  &
           "427587332321480401454840426545683394351958951963755749546928108005" //  &
           "75979757809361423163013264428850345811205867029087095522459004260861910M-53"
      CALL FMST2M(ST,C(109))
      ST = " 1.095319264499317829033981510140817754508285369349556399833223737" //  &
           "174998151312514038433336840462205568158214000003108865975414472963" //  &
           "70581756862795260586373374661842883378795511151785344346238350698780645M-53"
      CALL FMST2M(ST,C(110))
      ST = " 3.651064214997738460800440485464116453512465873766107387248343452" //  &
           "458997203837952291715916094766655886246438013372552836368973829605" //  &
           "29202550932685485563387565304843167421189066345923958359568125707813957M-54"
      CALL FMST2M(ST,C(111))
      ST = " 1.217021404999249244605272366723881223583838603878543320302210671" //  &
           "409056933719216026722268770996716099812270381542447122348883231949" //  &
           "38922656964910194439295935941196896773701049861546343314133034224282491M-54"
      CALL FMST2M(ST,C(112))
      ST = " 4.056738016664171876197055085310199605151026698635243534713457892" //  &
           "107296169062875134691895441571499550130093746425411512549327123228" //  &
           "16601740468340561446321921103466463550189353682758076018331072415996381M-55"
      CALL FMST2M(ST,C(113))
      ST = " 1.352246005554725890610555164289246652995273648088741088360045973" //  &
           "623667364361191345555088170702142598112873445326035312084757222791" //  &
           "25547996491724650392650447881135472685838861374568021340343847835638534M-55"
      CALL FMST2M(ST,C(114))
      ST = " 4.507486685182424465064025894185168278371602793396197455600512532" //  &
           "406474418197117862785333707359281266142995363043310398030388775224" //  &
           "96855355439438225180328883970301863313369556592379689501165566142115306M-56"
      CALL FMST2M(ST,C(115))
      ST = " 1.502495561727476029111885802751588446409759610042467877987373043" //  &
           "860038180719787874668067312469292183193912200682893897594757683194" //  &
           "08099696091579813882633694420046951596146901116935629924246701724845131M-56"
      CALL FMST2M(ST,C(116))
      ST = " 5.008318539091589782265978940332517362043223441916201948536518072" //  &
           "099322344676502315123489826715051040283829780783100995910094849108" //  &
           "65034584595413084839701258466960248343166395950551930374997082566708225M-57"
      CALL FMST2M(ST,C(117))
      ST = " 1.669439513030530682061916213454823087218137831151025299167070973" //  &
           "190879095582514009353872307284741332656640434042133116166845579709" //  &
           "81635243531150493888288323800171093455101171326873303863260272601517849M-57"
      CALL FMST2M(ST,C(118))
      ST = " 5.564798376768437493472862129317060203208340292866327711857634512" //  &
           "795204118217070748492626708664751935590615442683377362519336018325" //  &
           "51004856969881087903485123401331518636537459253893877449123506707146950M-58"
      CALL FMST2M(ST,C(119))
      ST = " 1.854932792256146302807572731110803878429090610101994543365419672" //  &
           "049548214027388780306433203424820289079873088082231836000918687537" //  &
           "23334792833523598579662177790912527794946886117189075782702240392408839M-58"
      CALL FMST2M(ST,C(120))
      ST = " 6.183109307520488855150122490825215119994140805333496249840425315" //  &
           "260483680241891966697616887289397465885493861812930613441993149861" //  &
           "75026053269580573584851572426023443652902431378207237270671672948970109M-59"
      CALL FMST2M(ST,C(121))
      ST = " 2.061036435840163246497927510477654121008516108356441555748893883" //  &
           "538337771585715140176109550136417103188492842915425790968199189673" //  &
           "75115850238127872748852300922000306375635048830126739836684145179941549M-59"
      CALL FMST2M(ST,C(122))
      ST = " 6.870121452800544891946141735609200171965629448722202434217816987" //  &
           "853447483793178217562366316730782300693739885096060635165076652920" //  &
           "41995525029854871068500757566629399110685057219153231404821304733713204M-60"
      CALL FMST2M(ST,C(123))
      ST = " 2.290040484266848481553643087076101079355964961713023345779978400" //  &
           "982834140969803837340319397848019038722638870138573135003750122280" //  &
           "67644649360313184772818630784452829131845685063424590204739986167925734M-60"
      CALL FMST2M(ST,C(124))
      ST = " 7.633468280889495399107799895007148312706155591452982323077149986" //  &
           "120899748236386385448066771935353332443050965065257767414246512604" //  &
           "54742904592796348751455213798689879834161009662040701046444537526427115M-61"
      CALL FMST2M(ST,C(125))
      ST = " 2.544489426963165248184847366204930714902412817306190051775615959" //  &
           "530879632335665207219382767168166468926593335092194936311475724489" //  &
           "22045567698482778031098111145226005087891270132458202092192296860039337M-61"
      CALL FMST2M(ST,C(126))
      ST = " 8.481631423210551115155109723717829106916950881296846054083649985" //  &
           "810525218550324029659230629073310007635840875342630862789117074541" //  &
           "87098166442436318923705470772023710069850200723928832617470068484538900M-62"
      CALL FMST2M(ST,C(127))
      ST = " 2.827210474403517110353107867003629422731212331386162488716730521" //  &
           "351064005716979204840102164742676308078783782599324060431157817109" //  &
           "88654841674710921627884749621343607380343694451939622450448527669612216M-62"
      CALL FMST2M(ST,C(128))
      ST = " 9.424034914678390547763871121100990122397792341363117728757038903" //  &
           "649538172737671552516290289880795766718250080584367536271759196670" //  &
           "39697325553891033518536553021634662617083010606852575129875069462163167M-63"
      CALL FMST2M(ST,C(129))
      ST = " 3.141344971559463560901334931474821601568811602982335715041502627" //  &
           "827982932089655486622557870156127718095126494732372850918038888332" //  &
           "54551941638266722522763646352278545149136303285285604558173356388928708M-63"
      CALL FMST2M(ST,C(130))
      ST = " 1.047114990519821198212122783269017133872736396216455065194396848" //  &
           "772182771964575553018906528347835085989108829213513953424609300128" //  &
           "95630377523418163375718344777771305545237149991145713842104433495405185M-63"
      CALL FMST2M(ST,C(131))
      ST = " 3.490383301732737355486270459507822699258345311341941846849921659" //  &
           "545613492122376436148560471436321288125576975707576514288727189135" //  &
           "81234008256049155059395856211182401981576994675285833396791180771117772M-64"
      CALL FMST2M(ST,C(132))
      ST = " 1.163461100577579125523555448655563780187894154820747936001916075" //  &
           "833373823024344903789897831688942109838607391549196509166544386710" //  &
           "32984677138183863587254008956883033409075083480154979676313298835611005M-64"
      CALL FMST2M(ST,C(133))
      ST = " 3.878203668591930435982181400901299436435180157598905860060992780" //  &
           "769432703831655807641542938250104063615345695580486254706194715454" //  &
           "60229972174586144050891402920273633768999530012198276610097006412695734M-65"
      CALL FMST2M(ST,C(134))
      ST = " 1.292734556197310149719976276646194118747432707279683488755745696" //  &
           "740947679118453560585318506784845508484302703353713015155212188239" //  &
           "59563349122999271475430680808380714359508027924891221587469189007133570M-65"
      CALL FMST2M(ST,C(135))
      ST = " 4.309115187324367176714710446351861339597497796718143278961320946" //  &
           "586948534200778409616190905114541820432168004724255101140047339531" //  &
           "16503452628730864113604659140394417734969121530536316322243286765720986M-66"
      CALL FMST2M(ST,C(136))
      ST = " 1.436371729108122394983600863166786192397968628737741530628906208" //  &
           "660940944816769480739740878584831113565249313940195309419096432913" //  &
           "23986224865537803560999682866769231274805486620569029108114354610082602M-66"
      CALL FMST2M(ST,C(137))
      ST = " 4.787905763693741323475412996513093551712741258381159384549759206" //  &
           "662442822072738588515832300420257228803664662013090680472314997210" //  &
           "47994366822587964409268875557318665191628748298284520227769123490709406M-67"
      CALL FMST2M(ST,C(138))
      ST = " 1.595968587897913776207656861993669785407285064500745185434113800" //  &
           "096356494376241989760638754749237977344465547010132327612679509965" //  &
           "90606634208693254568607053260913032981301281185089491854456988622988789M-67"
      CALL FMST2M(ST,C(139))
      ST = " 5.319895292993045924981820864535497749048234307774210374132739226" //  &
           "662962493896584326632876715466366610480847242863534749368020824442" //  &
           "26347859777033625778816141835475590087036758263889428530371159240382397M-68"
      CALL FMST2M(ST,C(140))
      ST = " 1.773298430997681976066348119317279371541102493785396123709304532" //  &
           "963732653591086234204258910992398206481527117154837605280677866587" //  &
           "40860519847234648346851518177302296315561478928449266143246491562286927M-68"
      CALL FMST2M(ST,C(141))
      ST = " 5.910994769992273256235513308905467236327084289024213039675731610" //  &
           "148156011515848201203908937219678972731231236325011698208574736079" //  &
           "80168427550976937837979750480068076198135077050989661245596743172379630M-69"
      CALL FMST2M(ST,C(142))
      ST = " 1.970331589997424419415425997445581585340102744812802313473388703" //  &
           "562069927312660229009236127416616182518232875620838559077842053926" //  &
           "92349876121987505300275410560304423025355686582773348179088042892097570M-69"
      CALL FMST2M(ST,C(143))
      ST = " 6.567771966658081399727057227641186570727529727910776691977632634" //  &
           "165946988657121076389626270986170591723497353782252176685973776995" //  &
           "43528417602815951738160201869055681397572348387528732209187423794662534M-70"
      CALL FMST2M(ST,C(144))
      ST = " 2.189257322219360466994595051007995333389857163416210506204122933" //  &
           "434092909606199446262463730440462471061414357649669288454519822486" //  &
           "33439286916366526139243135196086518512441365472582736479048872262858017M-70"
      CALL FMST2M(ST,C(145))
      ST = " 7.297524407397868224362590109254773419614298510807208602130225559" //  &
           "169843334057978765051658253298688634379979983044623872624689424224" //  &
           "07554019829693239747911909633693703476580188084738005493123701521791467M-71"
      CALL FMST2M(ST,C(146))
      ST = " 2.432508135799289408382681708766459710814072648334087437525650247" //  &
           "402627470576079904894860765869748078205431190272078201162332612662" //  &
           "86925426506431278564366927546261623009686964819372619868333948357411039M-71"
      CALL FMST2M(ST,C(147))
      ST = " 8.108360452664298028596818159143547381983866077866697620113475392" //  &
           "867557609420149388391146208299955439299097511073088600020008797364" //  &
           "90254145831112916894519121436284980569670233198361465597386807462396245M-72"
      CALL FMST2M(ST,C(148))
      ST = " 2.702786817554766009695908457319192274936196517820205990720071929" //  &
           "267436760629307786446087225623272749165348119673554381961913455458" //  &
           "93050660537495047726291186100258874974179781198259808664194749915523789M-72"
      CALL FMST2M(ST,C(149))
      ST = " 9.009289391849220032728785940466253732893526544523495429162278552" //  &
           "296342689979948102270331957628702643264702399700740206586719143369" //  &
           "63516748801336419497668271497140970017083857566651892279908542196784275M-73"
      CALL FMST2M(ST,C(150))
      ST = " 3.003096463949740011011890954041623476083928522268342083771940551" //  &
           "403977301983996687445005688089790997335068650775774411500709972696" //  &
           "00052352997217396436659500075907900727290635046242137258265027272046707M-73"
      CALL FMST2M(ST,C(151))
      ST = " 1.001032154649913337029531966564025740882006821416233309378396854" //  &
           "352648141475996724306705629001209846327383244434702908491488661840" //  &
           "33666122248311247777472186264526745924646761167750923503037999124457410M-73"
      CALL FMST2M(ST,C(152))
      ST = " 3.336773848833044456828335032653876240316804704924144044073849579" //  &
           "816206165795104200289660358785996996484528456071385377435139123676" //  &
           "57709192610536558746890780425020107260403503414920896791666418548557679M-74"
      CALL FMST2M(ST,C(153))
      ST = " 1.112257949611014818958755021576306436637772541032306466006509854" //  &
           "188967690019629658157071337514432146127855476046945605695950179554" //  &
           "39845790791214616926248699635001115509938807913024195016995950944619712M-74"
      CALL FMST2M(ST,C(154))
      ST = " 3.707526498703382729922171432032250515805881149465794264372984724" //  &
           "327809012896984209000978195800679443631566152731613311969108735093" //  &
           "51590966384556456803036744807880814153226574822433894031208374029742738M-75"
      CALL FMST2M(ST,C(155))
      ST = " 1.235842166234460909984137514518387752942293928074591712157524109" //  &
           "560139067764692539980008571135341128797657051727809999365157975182" //  &
           "53322235173852278845187716078871110242776570216352174337398795667971328M-75"
      CALL FMST2M(ST,C(156))
      ST = " 4.119473887448203032778434343895875695597840528814474282367848794" //  &
           "205301550322451475974554397356862932040320235498247930630480575568" //  &
           "25417807718570483485133432555365627049395094885528093686245256557591509M-76"
      CALL FMST2M(ST,C(157))
      ST = " 1.373157962482734344263376419251248172548827589625457941472696797" //  &
           "100175825647692982915548765225643706779206935633946071637767635709" //  &
           "79966315426397983742300900492572704510866186342432593363321634147388215M-76"
      CALL FMST2M(ST,C(158))
      ST = " 4.577193208275781160777550837017921925055088338901698850930906043" //  &
           "311063967012540679380363564085256672110282867832397021602920753059" //  &
           "62941763379439217899247719980351463557310935320189195654346581389521961M-77"
      CALL FMST2M(ST,C(159))
      ST = " 1.525731069425260386985167951519330997746387629867941705993359606" //  &
           "803096607303855736985249240116888915751939824222470295586537784557" //  &
           "14766559903817261522752203057544899378743721133360484530771023423930290M-77"
      CALL FMST2M(ST,C(160))
      ST = " 5.085770231417534312974277748264028715371006669759625749888114997" //  &
           "040535088461914628203462029057668451395058320957895155954018999649" //  &
           "89570133824275977321769674422087255596053232511973102592783007464375573M-78"
      CALL FMST2M(ST,C(161))
      ST = " 1.695256743805844769769991874189352969259021005362418248962236686" //  &
           "450929128768148436336404059930819742510685161671532907373324896396" //  &
           "92385277655969091090205296324356998243606127274626181023449273081779115M-78"
      CALL FMST2M(ST,C(162))
      ST = " 5.650855812686156039218806037909235903455451213739689260418037318" //  &
           "509446943950605499341032642835238412850215403194710979806557582704" //  &
           "37936259002958165848823746298586989930806700558037154118861273544045145M-79"
      CALL FMST2M(ST,C(163))
      ST = " 1.883618604228718704933778437940690307805004962163899221957470918" //  &
           "058716333776074607305943978262640017375578856797709581746515819407" //  &
           "51489901662136678406269201616881521607807260952614167843862068838710312M-79"
      CALL FMST2M(ST,C(164))
      ST = " 6.278728680762256512126149402150794782943415735579294630724544224" //  &
           "964157228232060226941740181573870879258048900467741995915140892478" //  &
           "63661908097641164941373210234799596031030310767570648902522277881849187M-80"
      CALL FMST2M(ST,C(165))
      ST = " 2.092909560254085021914720610883566852288010979452630217312140514" //  &
           "778472364540695790415885690351086108014265966713921440025212436361" //  &
           "73967300532411889825601604598920039776377478055710366101143130338433473M-80"
      CALL FMST2M(ST,C(166))
      ST = " 6.976365200849595928554228570881565721048784920515468405614250170" //  &
           "741926248557408642176573843779290903009186026063040672689273898432" //  &
           "88910357288258162475303198325632084369407383550262099668489007926100917M-81"
      CALL FMST2M(ST,C(167))
      ST = " 2.325455066949873863603395561978748601800147911008192886172463050" //  &
           "451820515051068562317761231512963759254765453729061173550997868120" //  &
           "58407734788242875774377719715615329774495325545245938734484625573096582M-81"
      CALL FMST2M(ST,C(168))
      ST = " 7.751516889786273230324814807015203014115787760363240263773567207" //  &
           "238770582250611249333986919911192763902738500220512285746471179825" //  &
           "77571260191751902463570497659486034004412403189688932637270843323495731M-82"
      CALL FMST2M(ST,C(169))
      ST = " 2.583838963261950823749231801853943339353886424780054671299212593" //  &
           "953839989881747235093144189509781483529556454394968872792726683435" //  &
           "27643326289916001403186650841236534106807785205861848342075189337865239M-82"
      CALL FMST2M(ST,C(170))
      ST = " 8.612796544966339593458567326158368856673430376795891557224772538" //  &
           "947415233055213978085860502193079097945197318656791149694366866539" //  &
           "72822366330690625287806683992931214418731468102411025539925415006202370M-83"
      CALL FMST2M(ST,C(171))
      ST = " 2.870932181657563853681094248589222833815589612305003013771994066" //  &
           "486301003023979828481138638939770854259457931497871219438623202895" //  &
           "38689833732465547124246759187251713896087958971527124804970116015889039M-83"
      CALL FMST2M(ST,C(172))
      ST = " 9.569773927459502329186355848231519845153615256953762486987693008" //  &
           "642824433756506369875737104132534466116626389178752624525042798289" //  &
           "22548975065811830797487032916317956125950813783158098289413462007841293M-84"
      CALL FMST2M(ST,C(173))
      ST = " 3.189924642457193947274111017893685291988923107347172733780095937" //  &
           "504990242848262256269173114579259710506482337540368636019319969386" //  &
           "73520058153132922629586433368902183759949488890960340616457392616954301M-84"
      CALL FMST2M(ST,C(174))
      ST = " 1.063308229833233763649615961280021009800959973857039083389096247" //  &
           "411276485934365597139214502542489173998160641619626621325276246417" //  &
           "76894303109902050056828926820785431328629167285741168999780547981801651M-84"
      CALL FMST2M(ST,C(175))
      ST = " 3.544360766480896268661403387806726005830929777366570657261489401" //  &
           "694611013460386506117284436161560434270675994478125024682804092939" //  &
           "15998067887747220478181658639560020083789407982402045792182967207779297M-85"
      CALL FMST2M(ST,C(176))
      ST = " 1.181453391991692193233769222790948883554160416544364056162345177" //  &
           "930450135664579747942304147194452606107088227217835015752643760372" //  &
           "89824167494419232372153209806697818554092135462778008255059604732701717M-85"
      CALL FMST2M(ST,C(177))
      ST = " 3.938177969065104259816236800771282813142970611653130614923372425" //  &
           "876301727778466158006947186295720851119739977650492841350794668106" //  &
           "75909743699184418774259697017658821999190863053142757985704314111950238M-86"
      CALL FMST2M(ST,C(178))
      ST = " 1.312728231460229835781690690024789271337659581458734016769897717" //  &
           "615695172767934655904718896955205839028238358310418293184633392500" //  &
           "09974095126152086147719595157884930629859403536081952476482731834669587M-86"
      CALL FMST2M(ST,C(179))
      ST = " 4.375760815315077535413569523720083421030822320092520690986988142" //  &
           "097203566380515277459454385250926657264251938784637571214707790842" //  &
           "13901922704849922902680056772549256776092849835805341467509317300857668M-87"
      CALL FMST2M(ST,C(180))
      ST = " 1.458563929048378339389303099779179984031579246114615330084586060" //  &
           "391340363385450968349489278357541331271188532814617890065655968100" //  &
           "90864106047591784785100253214527821779035249121314752150297703091924694M-87"
      CALL FMST2M(ST,C(181))
      ST = " 4.861879359454319859107717556333622023158902013437521581162884873" //  &
           "340193641755594234893503470462192100416392001882798233766279202968" //  &
           "02742623676171486751287082335949083937364296608060437072511301740262916M-88"
      CALL FMST2M(ST,C(182))
      ST = " 1.620837575639336717669552632235434719742418568812043976527454822" //  &
           "311557369332525067245294696277568831932145434818066929071312277435" //  &
           "59667900060244610446776086220578843202917562200852939911588169882033048M-88"
      CALL FMST2M(ST,C(183))
      ST = " 5.402795219201229254205787739591614371703058387299744106863813045" //  &
           "890074410469634798352875615158044666258805327295738061277040804255" //  &
           "11706167845408641690753715592108911157218063444936673090628364776228851M-89"
      CALL FMST2M(ST,C(184))
      ST = " 1.799216964519093606731199173063133848252752532638951929530332271" //  &
           "088757960440794118609774620691669941349561692799601479867289303361" //  &
           "36573900179559749556590933182210908857440047681409893684369308365396546M-89"
      CALL FMST2M(ST,C(185))
      ST = " 5.997366309057088767075246255186377444897385269608659113392135251" //  &
           "083240010154867782825603589060665479283491811344357943450419502328" //  &
           "30863890415054273770878371228558497389363862954168787497855394907612488M-90"
      CALL FMST2M(ST,C(186))
      ST = " 2.011301294926252140726377746074781953178862150579839442308024059" //  &
           "805516340858995612053131399074329904610352755793706433609739823274" //  &
           "35441280105568641478715458116693621474886783585502490686457813740208553M-90"
      CALL FMST2M(ST,C(187))
      ST = " 6.704482593621562564692904617177008442804824808114929721560806403" //  &
           "083889880113942537426720271890350736903006981375786722754398310176" //  &
           "64290862799510364108316770714557534855989672010291914054754244834573505M-91"
      CALL FMST2M(ST,C(188))
      ST = " 2.160351395289544486253218378105888708749936652071087348664876866" //  &
           "590595135247617954742090212769559780976713437175453455128216319294" //  &
           "79621451615761249300873617293850399062283747501629953564198534458556254M-91"
      CALL FMST2M(ST,C(189))
      ST = " 7.200419569439204992397564583120569704507791779761471734959614293" //  &
           "903195879211244064072895412909288124860635443865351473928153163314" //  &
           "35648140579845782685750371868061482151886800120453245481648754938151403M-92"
      CALL FMST2M(ST,C(190))
      ST = " 2.784319712626698759566042489347515628094423349698152057970857358" //  &
           "347754564870738442303737360847085557292665631767832280403826508366" //  &
           "53961878379952572306276562137966777737866334027041960083257419418474932M-92"
      CALL FMST2M(ST,C(191))
      ST = " 9.284263699802481030477799658347338855061343963325012856142524485" //  &
           "337551481107985173202144915554476793675287039218856327451603285101" //  &
           "60910890143062738353240079101453208140103839949856135593728282192403791M-93"
      CALL FMST2M(ST,C(192))
      ST = " 1.469128321729119845643778628274835930841237239875100463308434786" //  &
           "723561392709397386626554407894406027910890887423959020221165249040" //  &
           "77624688503604251276023392875008399104638564047518405899300367036809361M-93"
      CALL FMST2M(ST,C(193))
      ST = " 4.886379978539319342120516219783307378314461447995883185804991015" //  &
           "296960664895043875586362313203658735183472813064062209400386162722" //  &
           "42806131645480493919198786449897808824436924850601891974937230254873784M-94"
      CALL FMST2M(ST,C(194))
      ST = " 7.046670647920504479063472745872909513874517692559573509267252909" //  &
           "755965774748698930668458487567120021155413053722800379339662049820" //  &
           "26470884186471084008635684699341681154401901074420825646205892400124466M-94"
      CALL FMST2M(ST,C(195))
      ST = " 2.351541158501990523752731183260855698650611680331234753194545827" //  &
           "615066582884384059556932538832948082760844393963381962615300495238" //  &
           "72892129343291577201989578726783775602985638203462774865396580295272103M-94"
      CALL FMST2M(ST,C(196))

      NDIG = NDSAVE

      END SUBROUTINE FMGAM_C

      SUBROUTINE FMCANCEL(MA,MB,MC,NC)
      USE ModLib_FMVALS
      IMPLICIT NONE

!  Return NC as the number of bits of precision lost to cancellation after
!  MC = MA + MB or MC = MA - MB.

      TYPE(MULTI) :: MA,MB,MC
      INTEGER :: NC
      INTENT(IN) :: MA,MB,MC
      INTENT(INOUT) :: NC
      TYPE(MULTI) :: MXY(5)

      IF (MA%MP(3) == 0 .OR. MA%MP(3) == 0) THEN
          NC = 0
          RETURN
      ENDIF
      IF (MC%MP(3) == 0) THEN
          NC = NINT(NDIG*ALOGM2)
          RETURN
      ENDIF
      IF (ABS(MA%MP(2)) >= MEXPOV .OR. ABS(MB%MP(2)) >= MEXPOV .OR.  &
          ABS(MC%MP(2)) >= MEXPOV) THEN
          NC = 0
          RETURN
      ENDIF

      CALL FMABS(MA,MXY(1))
      CALL FMABS(MB,MXY(2))
      CALL FMABS(MC,MXY(3))
      CALL FMMAX(MXY(1),MXY(2),MXY(4))
      CALL FMDIV(MXY(4),MXY(3),MXY(5))
      NC = ( MXY(5)%MP(2)*LOG(MBASE) +  &
           LOG(MXY(5)%MP(3)/MBASE + MXY(5)%MP(4)/MBASE**2) ) / 0.69314718056D0

      END SUBROUTINE FMCANCEL

      SUBROUTINE FMIBTA(MX,MA,MB,MC)

!  MC = Incomplete Beta(MX,MA,MB)

!  Integral from 0 to MX of  t**(MA-1) * (1-t)**(MB-1)  dt.

!  0 <= MX <= 1,    0 < MA,    0 <= MB.

!  Some comments below refer to this function and its arguments as B(x,a,b).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MX,MA,MB,MC
      REAL (KIND(1.0D0)) :: MLA,MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: IEXTRA,J,K,KASHIFT,KBIGAB,KBSHIFT,KICK,KL,KOVUN,KR_RETRY,KRESLT,KRS,  &
                 K_RETURN_CODE,N_ACC,NCSAVE,NDGOAL,NDOLD,NDS,NDSAVE,NGOAL,NMETHD,      &
                 NTERMS,NUMTRY,NWDS1
      LOGICAL, EXTERNAL :: FMCOMP

      INTENT(IN) :: MX,MA,MB
      INTENT(INOUT) :: MC
      TYPE(MULTI) :: MXY(40),MRETRY

      N_ACC = NINT(NDIG*ALOGM2)
      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < NDIG+2) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      K_RETURN_CODE = 0

      CALL FMIBTA3(MX,MA,MB,MC,MXY,K_RETURN_CODE)

      IF (K_RETURN_CODE == 1) THEN
          RETURN
      ENDIF

      NCSAVE = NCALL
      CALL FMENT2('FMIBTA   ',MX,MA,2,1,MC,KRESLT,NDSAVE,MXSAVE,KOVUN)
      NCALL = NCSAVE + 1
      KRS = KRESLT
      IF (MB%MP(2) == MEXPOV .OR. MB%MP(2) == MEXPUN) KOVUN = 1
      IF (ABS(NTRACE) >= 2 .AND. NCALL <= LVLTRC) THEN
          NDS = NDIG
          NDIG = NDSAVE
          IF (NTRACE < 0) THEN
              CALL FMNTRJ(MB,NDIG)
          ELSE
              CALL FMPRNT(MB)
          ENDIF
          NDIG = NDS
      ENDIF
      KRESLT = KRS
      IF (MB%MP(2) == MUNKNO .OR. MX%MP(1) < 0 .OR. MA%MP(1) < 0 .OR.  &
          MB%MP(1) < 0) THEN
          KRESLT = 12
          KFLAG = -4
      ENDIF
      IF (KRESLT /= 0) THEN
          NDIG = NDSAVE
          CALL FMRSLT(MA,MB,MC,KRESLT)
          IF (KFLAG == -4) THEN
              IF (MX%MP(2) /= MUNKNO .AND. MA%MP(2) /= MUNKNO .AND.  &
                  MB%MP(2) /= MUNKNO) THEN
                  CALL FMWRN2
              ENDIF
          ENDIF
          IF (NTRACE /= 0) CALL FMNTR(1,MC,MC,1,1)
          MXEXP = MXSAVE
          NCALL = NCALL - 1
          RETURN
      ENDIF

      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL FMEQU(MX,MXY(1),NDSAVE,NDIG)
      CALL FMEQU(MA,MXY(2),NDSAVE,NDIG)
      CALL FMEQU(MB,MXY(3),NDSAVE,NDIG)

!             Handle cases where at least one of X, A, B is underflow or overflow.
!             Increasing any underflowed values to 1/HUGE makes the calculations more stable.
!             If A is underflow and the final result is overflow, it is safe to return overflow.
!             If X is underflow and the final result is underflow, it is safe to return underflow.
!             If B is underflow, it is replaced by zero.
!             Similarly, decreasing any overflowed A or B values to HUGE and then getting a final
!             result of underflow means it is safe to return underflow.
!             Any cases where the inequalities conflict, such as A = underflow, B = overflow,
!             will return unknown.

      KBIGAB = 0
      IF (MA%MP(2) == MEXPOV) THEN
          CALL FMBIG(MXY(2))
          MXY(2)%MP(2) = MXSAVE + 1
          KBIGAB = -1
      ENDIF
      IF (MB%MP(2) == MEXPOV) THEN
          CALL FMBIG(MXY(3))
          MXY(3)%MP(2) = MXSAVE + 1
          KBIGAB = -1
      ENDIF
      IF (MX%MP(2) == MEXPUN) THEN
          CALL FMBIG(MXY(1))
          MXY(1)%MP(2) = MXSAVE + 1
          CALL FMI2M(1,MXY(12))
          CALL FMDIV_R2(MXY(12),MXY(1))
          KBIGAB = -1
      ENDIF
      IF (MA%MP(2) == MEXPUN) THEN
          CALL FMBIG(MXY(2))
          MXY(2)%MP(2) = MXSAVE + 1
          CALL FMI2M(1,MXY(12))
          CALL FMDIV_R2(MXY(12),MXY(2))
          IF (KBIGAB < 0) THEN
              KBIGAB = -9
              CALL FMI2M(0,MXY(21))
              GO TO 150
          ELSE
              KBIGAB = 1
          ENDIF
      ENDIF
      IF (MB%MP(2) == MEXPUN) THEN
          CALL FMI2M(1,MXY(12))
          IF (FMCOMP(MXY(1),'/=',MXY(12))) THEN
              CALL FMI2M(0,MXY(3))
          ENDIF
      ENDIF
      NUMTRY = 0
      NDGOAL = 0
      NWDS1 = 0
      KASHIFT = 0
      KBSHIFT = 0

  120 KICK = 0

!             Check for special cases.

      N_ACC = NINT(NDIG*ALOGM2)
      CALL FMIBTA2(K_RETURN_CODE,MXY,MXSAVE,NTERMS,NUMTRY,NMETHD)
      IF (K_RETURN_CODE == 1) GO TO 130
      IF (K_RETURN_CODE == 2) GO TO 150

!             Determine which method to use.

!             NMETHD = 1 means use the convergent series for B(x,a,b),
!                    = 2 means use continued fraction expansion 1 for B(x,a,b),
!                    = 3 means use the convergent series for B(1-x,b,a).
!                    = 4 means use continued fraction expansion 1 for B(1-x,b,a).
!                    = 5 means use continued fraction expansion 2 for B(x,a,b).
!                    = 6 means use continued fraction expansion 2 for B(1-x,b,a).

      CALL FMSQR(MXY(2),MXY(12))
      CALL FMDPM(DBLE(0.00173),MXY(7))
      CALL FMMPY(MXY(7),MXY(12),MXY(6))
      CALL FMSQR(MXY(3),MXY(12))
      CALL FMDPM(DBLE(0.01253),MXY(7))
      CALL FMMPY(MXY(7),MXY(12),MXY(5))
      CALL FMADD_R1(MXY(6),MXY(5))
      CALL FMDPM(DBLE(0.21583),MXY(7))
      CALL FMMPY(MXY(7),MXY(2),MXY(5))
      CALL FMADD_R1(MXY(6),MXY(5))
      CALL FMDPM(DBLE(0.03891),MXY(7))
      CALL FMMPY(MXY(7),MXY(3),MXY(5))
      CALL FMADD_R1(MXY(6),MXY(5))
      CALL FMDPM(DBLE(9.14350),MXY(5))
      CALL FMADD_R1(MXY(6),MXY(5))

      CALL FMDPM(DBLE(0.11709),MXY(7))
      CALL FMMPY(MXY(7),MXY(2),MXY(5))
      CALL FMDPM(DBLE(0.62633),MXY(7))
      CALL FMMPY(MXY(7),MXY(3),MXY(4))
      CALL FMADD_R1(MXY(5),MXY(4))
      CALL FMADDI(MXY(5),1)

      CALL FMDIV(MXY(5),MXY(6),MXY(33))

      CALL FMDPM(DBLE(0.29217),MXY(7))
      CALL FMMPY(MXY(7),MXY(2),MXY(6))
      CALL FMDPM(DBLE(2.09304),MXY(7))
      CALL FMMPY(MXY(7),MXY(3),MXY(5))
      CALL FMADD_R1(MXY(6),MXY(5))
      CALL FMDPM(DBLE(1.53724),MXY(5))
      CALL FMADD_R1(MXY(6),MXY(5))

      CALL FMDPM(DBLE(0.29217),MXY(7))
      CALL FMMPY(MXY(7),MXY(2),MXY(5))
      CALL FMDPM(DBLE(2.09304),MXY(7))
      CALL FMMPY(MXY(7),MXY(3),MXY(4))
      CALL FMADD_R1(MXY(5),MXY(4))
      CALL FMADDI(MXY(5),1)

      CALL FMDIV(MXY(5),MXY(6),MXY(34))

      CALL FMSQR(MXY(2),MXY(12))
      CALL FMDPM(DBLE(0.04038),MXY(7))
      CALL FMMPY(MXY(7),MXY(12),MXY(6))
      CALL FMSQR(MXY(3),MXY(12))
      CALL FMDPM(DBLE(0.05754),MXY(7))
      CALL FMMPY(MXY(7),MXY(12),MXY(5))
      CALL FMADD_R1(MXY(6),MXY(5))
      CALL FMDPM(DBLE(0.02670),MXY(7))
      CALL FMMPY(MXY(7),MXY(2),MXY(5))
      CALL FMADD_R1(MXY(6),MXY(5))
      CALL FMDPM(DBLE(0.56206),MXY(7))
      CALL FMMPY(MXY(7),MXY(3),MXY(5))
      CALL FMADD_R1(MXY(6),MXY(5))
      CALL FMDPM(DBLE(0.13746),MXY(5))
      CALL FMADD_R1(MXY(6),MXY(5))

      CALL FMDPM(DBLE(0.87312),MXY(7))
      CALL FMMPY(MXY(7),MXY(2),MXY(5))
      CALL FMDPM(DBLE(0.20334),MXY(7))
      CALL FMMPY(MXY(7),MXY(3),MXY(4))
      CALL FMADD_R1(MXY(5),MXY(4))
      CALL FMADDI(MXY(5),1)

      CALL FMDIV(MXY(5),MXY(6),MXY(35))

      CALL FMDPM(DBLE(0.64584),MXY(7))
      CALL FMMPY(MXY(7),MXY(2),MXY(6))
      CALL FMDPM(DBLE(0.64584),MXY(7))
      CALL FMMPY(MXY(7),MXY(3),MXY(5))
      CALL FMADD_R1(MXY(6),MXY(5))
      CALL FMDPM(DBLE(6.31958),MXY(5))
      CALL FMADD_R1(MXY(6),MXY(5))

      CALL FMDPM(DBLE(0.64584),MXY(7))
      CALL FMMPY(MXY(7),MXY(2),MXY(5))
      CALL FMADDI(MXY(5),1)

      CALL FMDIV(MXY(5),MXY(6),MXY(36))

      CALL FMSQR(MXY(2),MXY(12))
      CALL FMDPM(DBLE(0.11637),MXY(7))
      CALL FMMPY(MXY(7),MXY(12),MXY(6))
      CALL FMSQR(MXY(3),MXY(12))
      CALL FMDPM(DBLE(0.10718),MXY(7))
      CALL FMMPY(MXY(7),MXY(12),MXY(5))
      CALL FMADD_R1(MXY(6),MXY(5))
      CALL FMDPM(DBLE(0.92626),MXY(7))
      CALL FMMPY(MXY(7),MXY(2),MXY(5))
      CALL FMADD_R1(MXY(6),MXY(5))
      CALL FMDPM(DBLE(0.05518),MXY(7))
      CALL FMMPY(MXY(7),MXY(3),MXY(5))
      CALL FMADD_R1(MXY(6),MXY(5))
      CALL FMDPM(DBLE(0.28962),MXY(5))
      CALL FMADD_R1(MXY(6),MXY(5))

      CALL FMDPM(DBLE(0.99773),MXY(7))
      CALL FMMPY(MXY(7),MXY(2),MXY(5))
      CALL FMDPM(DBLE(0.56855),MXY(7))
      CALL FMMPY(MXY(7),MXY(3),MXY(4))
      CALL FMADD_R1(MXY(5),MXY(4))
      CALL FMADDI(MXY(5),1)

      CALL FMDIV(MXY(5),MXY(6),MXY(37))
      IF (FMCOMP(MXY(1),'<=',MXY(33))) THEN
          NMETHD = 1
      ELSE IF (FMCOMP(MXY(1),'>=',MXY(34))) THEN
          NMETHD = 3
      ELSE IF (FMCOMP(MXY(1),'<',MXY(37))) THEN
          IF (FMCOMP(MXY(1),'<',MXY(35))) THEN
              NMETHD = 2
          ELSE
              NMETHD = 4
          ENDIF
      ELSE
          IF (FMCOMP(MXY(1),'<',MXY(36))) THEN
              NMETHD = 5
          ELSE
              NMETHD = 6
          ENDIF
      ENDIF
      IF (MXY(3)%MP(2) <= 0 .AND. MXY(2)%MP(2)+NDIG < 0) THEN
          NMETHD = 1
      ENDIF

      IF (NMETHD == 1) CALL FMIBTA_M1(MXY,N_ACC)
      IF (NMETHD == 2) CALL FMIBTA_M2(MXY,N_ACC)
      IF (NMETHD == 5) CALL FMIBTA_M5(MXY,MX,NMETHD,NDSAVE,N_ACC)

!             Method 3, 4, or 6.  B(X,A,B) = B(A,B) - B(1-X,B,A).

      IF (NMETHD == 3 .OR. NMETHD == 4 .OR. NMETHD == 6) THEN
          CALL FMI2M(1,MXY(12))
          CALL FMSUB_R2(MXY(12),MXY(1))
          DO J = 1, NDIG+2
             MLA = MXY(2)%MP(J)
             MXY(2)%MP(J) = MXY(3)%MP(J)
             MXY(3)%MP(J) = MLA
          ENDDO
          IF (NMETHD == 3) THEN
              CALL FMIBTA_M1(MXY,N_ACC)
          ELSE IF (NMETHD == 4) THEN
              CALL FMIBTA_M2(MXY,N_ACC)
          ELSE
              CALL FMIBTA_M5(MXY,MX,NMETHD,NDSAVE,N_ACC)
          ENDIF

          K = NWDS1
          CALL FMEQ(MXY(21),MXY(30))
          CALL FMBETA(MXY(2),MXY(3),MXY(32))
          N_ACC = NINT(NDIG*ALOGM2)
          IF (MXY(32)%MP(1) > 0) THEN
              CALL FMSUB(MXY(32),MXY(39),MXY(37))
              CALL FMMPYI(MXY(38),-1,MXY(39))
              CALL FMEQ(MXY(37),MXY(38))
          ELSE
              CALL FMSUB(MXY(32),MXY(38),MXY(37))
              CALL FMMPYI(MXY(39),-1,MXY(38))
              CALL FMEQ(MXY(37),MXY(39))
          ENDIF
          CALL FMADD(MXY(38),MXY(39),MXY(21))
          NWDS1 = INT(MAX(MXY(32)%MP(2),MXY(30)%MP(2)))
          CALL FMCANCEL(MXY(38),MXY(39),MXY(21),J)
          N_ACC = N_ACC - J
          NWDS1 = MAX(0,NWDS1-INT(MXY(21)%MP(2)))
          IF (K /= NWDS1 .AND. NUMTRY >= 1) THEN
              IF (KASHIFT == 0 .AND. KBSHIFT == 0) N_ACC = -1
          ENDIF
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  130 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(21)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF

!             Check for too much cancellation.

      K = KFLAG
      IF (KICK < 0) N_ACC = KICK

!             Reverse the translation if KASHIFT is positive.
!             This is used when a is small and a retry was required because of cancellation.

      IF (KASHIFT > 0 .AND. N_ACC > 0) THEN
          CALL FMEQU(MX,MXY(22),NDSAVE,NDIG)
          CALL FMEQU(MA,MXY(23),NDSAVE,NDIG)
          CALL FMEQU(MB,MXY(24),NDSAVE,NDIG)
          IF (KBSHIFT > 0) CALL FMADDI(MXY(24),KBSHIFT)
          CALL FMI2M(1,MXY(19))
          CALL FMADD(MXY(23),MXY(24),MXY(16))
          CALL FMI2M(1,MXY(12))
          CALL FMADD(MXY(23),MXY(12),MXY(7))
          CALL FMDIV(MXY(16),MXY(7),MXY(20))
          CALL FMI2M(1,MXY(12))
          CALL FMSUB(MXY(12),MXY(22),MXY(17))
          CALL FMEQ(MXY(22),MXY(18))
          CALL FMMPY(MXY(20),MXY(22),MXY(12))
          CALL FMADD_R1(MXY(19),MXY(12))
          CALL FMEQ(MXY(16),MXY(14))
          CALL FMEQ(MXY(23),MXY(15))
          CALL FMADDI(MXY(15),1)
          DO J = 2, KASHIFT-1
             CALL FMADDI(MXY(14),1)
             CALL FMADDI(MXY(15),1)
             CALL FMMPY_R1(MXY(20),MXY(14))
             CALL FMDIV_R1(MXY(20),MXY(15))
             CALL FMMPY_R1(MXY(18),MXY(22))
             CALL FMMPY(MXY(20),MXY(18),MXY(13))
             CALL FMADD_R1(MXY(19),MXY(13))
          ENDDO
          IF (MXY(22)%MP(2)*(-10) >= NDIG) THEN
              CALL FMEQ(MXY(22),MXY(15))
              CALL FMEQ(MXY(22),MXY(17))
              DO K = 2, NTERMS
                 CALL FMMPY_R1(MXY(15),MXY(22))
                 CALL FMDIVI(MXY(15),K,MXY(12))
                 CALL FMADD_R1(MXY(17),MXY(12))
                 IF (KFLAG /= 0) EXIT
              ENDDO
              CALL FMMPY(MXY(17),MXY(24),MXY(12))
              IF (MXY(12)%MP(2) /= MUNKNO .AND. MXY(12)%MP(3) /= 0)  &
                  MXY(12)%MP(1) = -MXY(12)%MP(1)
              CALL FMEXP(MXY(12),MXY(18))
              CALL FMEQ(MXY(19),MXY(15))
              CALL FMPWR(MXY(22),MXY(23),MXY(12))
              CALL FMMPY(MXY(19),MXY(12),MXY(7))
              CALL FMMPY(MXY(7),MXY(18),MXY(12))
              CALL FMDIV(MXY(12),MXY(23),MXY(19))
              IF (MXY(19)%MP(2) == MUNKNO) THEN
                  CALL FMLN(MXY(22),MXY(12))
                  CALL FMMPY(MXY(23),MXY(12),MXY(19))
                  CALL FMLN(MXY(15),MXY(12))
                  CALL FMADD_R2(MXY(12),MXY(19))
                  CALL FMMPY(MXY(17),MXY(24),MXY(12))
                  CALL FMSUB_R1(MXY(19),MXY(12))
                  CALL FMLN(MXY(23),MXY(12))
                  CALL FMSUB_R2(MXY(19),MXY(12))
                  CALL FMEXP(MXY(12),MXY(19))
              ENDIF
          ELSE
              CALL FMPWR(MXY(22),MXY(23),MXY(12))
              CALL FMMPY_R1(MXY(19),MXY(12))
              CALL FMPWR(MXY(17),MXY(24),MXY(12))
              CALL FMMPY_R1(MXY(19),MXY(12))
              CALL FMDIV_R1(MXY(19),MXY(23))
          ENDIF
          CALL FMMPY(MXY(21),MXY(20),MXY(12))
          CALL FMI2M(KASHIFT-1,MXY(7))
          CALL FMADD_R2(MXY(16),MXY(7))
          CALL FMMPY_R1(MXY(12),MXY(7))
          CALL FMDIV(MXY(12),MXY(23),MXY(20))
          CALL FMADD(MXY(20),MXY(19),MXY(21))
          CALL FMCANCEL(MXY(20),MXY(19),MXY(21),J)
          N_ACC = N_ACC - J
      ENDIF

!             Reverse the translation if KBSHIFT is positive.
!             This is used when x is close to 1, b is small, and a retry was required because
!             of cancellation.

      IF (KBSHIFT > 0 .AND. N_ACC > 0) THEN
          CALL FMEQU(MX,MXY(22),NDSAVE,NDIG)
          CALL FMEQU(MA,MXY(23),NDSAVE,NDIG)
          CALL FMEQU(MB,MXY(24),NDSAVE,NDIG)
          CALL FMI2M(1,MXY(19))
          CALL FMI2M(1,MXY(12))
          CALL FMADD(MXY(24),MXY(12),MXY(7))
          CALL FMADD(MXY(23),MXY(24),MXY(12))
          CALL FMDIV(MXY(12),MXY(7),MXY(20))
          CALL FMADD(MXY(23),MXY(24),MXY(16))
          CALL FMI2M(1,MXY(12))
          CALL FMSUB(MXY(12),MXY(22),MXY(17))
          CALL FMEQ(MXY(17),MXY(18))
          CALL FMMPY(MXY(20),MXY(18),MXY(12))
          CALL FMADD_R1(MXY(19),MXY(12))
          CALL FMEQ(MXY(16),MXY(14))
          CALL FMEQ(MXY(24),MXY(15))
          CALL FMADDI(MXY(15),1)
          DO J = 2, KBSHIFT-1
             CALL FMADDI(MXY(14),1)
             CALL FMADDI(MXY(15),1)
             CALL FMMPY_R1(MXY(20),MXY(14))
             CALL FMDIV_R1(MXY(20),MXY(15))
             CALL FMMPY_R1(MXY(18),MXY(17))
             CALL FMMPY(MXY(20),MXY(18),MXY(13))
             CALL FMADD_R1(MXY(19),MXY(13))
          ENDDO
          IF (MXY(22)%MP(2)*(-10) >= NDIG) THEN
              CALL FMEQ(MXY(22),MXY(15))
              CALL FMEQ(MXY(22),MXY(17))
              DO K = 2, NTERMS
                 CALL FMMPY_R1(MXY(15),MXY(22))
                 CALL FMDIVI(MXY(15),K,MXY(12))
                 CALL FMADD_R1(MXY(17),MXY(12))
                 IF (KFLAG /= 0) EXIT
              ENDDO
              CALL FMMPY(MXY(17),MXY(24),MXY(12))
              IF (MXY(12)%MP(2) /= MUNKNO .AND. MXY(12)%MP(3) /= 0)  &
                  MXY(12)%MP(1) = -MXY(12)%MP(1)
              CALL FMEXP(MXY(12),MXY(17))
              CALL FMPWR(MXY(22),MXY(23),MXY(12))
              CALL FMMPY(MXY(19),MXY(12),MXY(7))
              CALL FMMPY(MXY(7),MXY(17),MXY(12))
              CALL FMDIV(MXY(12),MXY(24),MXY(19))
          ELSE
              CALL FMPWR(MXY(22),MXY(23),MXY(12))
              CALL FMMPY_R1(MXY(19),MXY(12))
              CALL FMPWR(MXY(17),MXY(24),MXY(12))
              CALL FMMPY_R1(MXY(19),MXY(12))
              CALL FMDIV_R1(MXY(19),MXY(24))
          ENDIF
          CALL FMMPY(MXY(21),MXY(20),MXY(12))
          CALL FMI2M(KBSHIFT-1,MXY(7))
          CALL FMADD_R2(MXY(16),MXY(7))
          CALL FMMPY_R1(MXY(12),MXY(7))
          CALL FMDIV(MXY(12),MXY(24),MXY(20))
          CALL FMSUB(MXY(20),MXY(19),MXY(21))
          CALL FMCANCEL(MXY(20),MXY(19),MXY(21),J)
          N_ACC = N_ACC - J
      ENDIF
      IF (NCALL >= 1) THEN
          NGOAL = 1.06*(INT(DBLE(NDSAVE)*ALOGM2) + 29)
      ELSE
          NGOAL = INT(-MXEXP2)
      ENDIF
      NDGOAL = INT(DBLE(NGOAL)/ALOGM2 + 1.0)
      IF (N_ACC <= NGOAL) THEN
          IF (NUMTRY > 0) THEN
              IF (MXY(21)%MP(3) == 0 .OR. K < 0) GO TO 140
              DO J = 1, NDGOAL+1
                 IF (MRETRY%MP(J+1) /= MXY(21)%MP(J+1)) GO TO 140
              ENDDO
              CALL FMI2M(1,MXY(15))
              N_ACC = NINT(NDIG*ALOGM2)
              GO TO 150
          ENDIF
  140     IEXTRA = INT(DBLE(NGOAL-N_ACC)/ALOGM2 + 23.03/ALOGMB) + 1
          NDOLD = NDIG
          NDIG = NDIG + IEXTRA
          IF (N_ACC < 0) NDIG = NDOLD + 10*2**NUMTRY
          IF (ABS(MX%MP(2)) >= MEXPOV .OR. ABS(MA%MP(2)) >= MEXPOV .OR.  &
              ABS(MB%MP(2)) >= MEXPOV) THEN
              CALL FMST2M('UNKNOWN',MXY(21))
              KFLAG = -4
              GO TO 150
          ENDIF
          CALL FMEQU_R1(MXY(1),NDSAVE,NDIG)
          CALL FMEQU_R1(MXY(2),NDSAVE,NDIG)
          CALL FMEQU_R1(MXY(3),NDSAVE,NDIG)
          IF (NMETHD == 3 .OR. NMETHD == 4 .OR. NMETHD == 6) THEN
              CALL FMEQU(MX,MXY(1),NDSAVE,NDIG)
              DO J = 1, NDIG+2
                 MLA = MXY(2)%MP(J)
                 MXY(2)%MP(J) = MXY(3)%MP(J)
                 MXY(3)%MP(J) = MLA
              ENDDO
          ENDIF

          IF (KASHIFT > 0) THEN
              CALL FMEQU(MA,MXY(2),NDSAVE,NDIG)
              IF (KASHIFT <= 2000) THEN
                  KASHIFT = 9*KASHIFT
              ELSE
                  KASHIFT = NDIG
              ENDIF
              CALL FMADDI(MXY(2),KASHIFT)
          ENDIF
          IF (KBSHIFT > 0) THEN
              CALL FMEQU(MB,MXY(3),NDSAVE,NDIG)
              IF (KBSHIFT <= 2000) THEN
                  KBSHIFT = 9*KBSHIFT
              ELSE
                  KBSHIFT = NDIG
              ENDIF
              CALL FMADDI(MXY(3),KBSHIFT)
          ENDIF

!             Check to see if a retry is about to be done for small a and large b.
!             If so, raise a by 2*NDIG to reduce the potential cancellation error.

          CALL FMI2M(200,MXY(12))
          IF (NUMTRY == 0 .AND.                  &
              FMCOMP(MXY(2),'<=',MXY(12)) .AND.  &
              FMCOMP(MXY(3),'>=',MXY(2))) THEN
              KASHIFT = 2*NDIG
              CALL FMADDI(MXY(2),2*NDIG)
          ENDIF

!             Check to see if a retry is about to be done for a > 100 and b < 2.
!             If so, raise b by 2*NDIG to reduce the potential cancellation error.

          CALL FMI2M(100,MXY(12))
          CALL FMI2M(2,MXY(7))
          IF (NUMTRY == 0 .AND.                  &
              FMCOMP(MXY(2),'>=',MXY(12)) .AND.  &
              FMCOMP(MXY(3),'<=',MXY(7))) THEN
              KBSHIFT = 2*NDIG
              CALL FMADDI(MXY(3),2*NDIG)
          ENDIF

          CALL FMI2M(40*NUMTRY,MXY(12))
          CALL FMI2M(100,MXY(7))
          IF (NUMTRY > 0 .AND. KASHIFT == 0 .AND. &
              FMCOMP(MXY(2),'<=',MXY(12)) .AND.   &
              FMCOMP(MXY(3),'>=',MXY(7))) THEN
              KASHIFT = 2*NDIG
              CALL FMADDI(MXY(2),2*NDIG)
          ENDIF

          CALL FMI2M(40*NUMTRY,MXY(12))
          CALL FMI2M(100,MXY(7))
          IF (NUMTRY > 0 .AND. KBSHIFT == 0 .AND. &
              FMCOMP(MXY(2),'>=',MXY(12)) .AND.   &
              FMCOMP(MXY(3),'<=',MXY(7))) THEN
              KBSHIFT = 2*NDIG
              CALL FMADDI(MXY(3),2*NDIG)
          ENDIF

          NUMTRY = NUMTRY + 1
          CALL FMEQU(MXY(21),MRETRY,NDOLD,NDIG)
          IF (KASHIFT == 2*NDIG .OR. KBSHIFT == 2*NDIG) THEN
              NDIG = MAX(NDIG,NDOLD+2)
          ENDIF
          GO TO 120
      ENDIF

  150 MXEXP = MXSAVE
      CALL FMEQU(MXY(21),MXY(20),NDIG,NDSAVE)
      IF (KBIGAB /= 0) THEN
          IF ((MXY(20)%MP(2) >= -MXSAVE  .AND. KBIGAB == -1) .OR.  &
              (MXY(20)%MP(2) <= MXSAVE+1 .AND. KBIGAB ==  1) .OR. (KBIGAB == -9)) THEN
              CALL FMST2M('UNKNOWN',MXY(21))
              KFLAG = -4
          ENDIF
      ENDIF
      CALL FMEXT2(MXY(21),MC,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMIBTA

      SUBROUTINE FMIBTA_M1(MXY,N_ACC)
      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MXY(40)
      INTEGER :: N_ACC
      INTENT(INOUT) :: MXY,N_ACC

      INTEGER :: INT_TEMP1,INT_TEMP2,INT_TEMP3,INT_TEMP4,J,JCHECK,JEXTRA,K,KRSAVE,NDIG2,  &
                 NDSAV1,NTERMS

!             Method 1.  Use the Pochhammer(1-B,N)*X**N/((A+N)*N!) series.

!             MXY(21) is the sum.
!             MXY(17) is the current term.
!             MXY(18) is J-B.
!             MXY(19) is 1.
!             MXY(20) is A+J.

      N_ACC = NINT(NDIG*ALOGM2)
      NTERMS = INT(INTMAX/10)
      JEXTRA = INT(0.06*NDIG)
      IF (NDIG+JEXTRA > NDIG) THEN
          CALL FMEQU_R1(MXY(1),NDIG,NDIG+JEXTRA)
          CALL FMEQU_R1(MXY(2),NDIG,NDIG+JEXTRA)
          CALL FMEQU_R1(MXY(3),NDIG,NDIG+JEXTRA)
      ENDIF
      NDIG = NDIG + JEXTRA
      CALL FMI2M(1,MXY(17))
      CALL FMDIV(MXY(17),MXY(2),MXY(38))
      CALL FMI2M(0,MXY(39))
      CALL FMEQ(MXY(3),MXY(18))
      IF (MXY(18)%MP(2) /= MUNKNO .AND. MXY(18)%MP(3) /= 0)  &
          MXY(18)%MP(1) = -MXY(18)%MP(1)
      CALL FMEQ(MXY(2),MXY(20))
      CALL FMI2M(1,MXY(19))
      CALL FMI2M(0,MXY(16))
      CALL FMI2M(0,MXY(22))
      JCHECK = 5
      NDSAV1 = NDIG

!             Method 1 summation loop.

      KRSAVE = KROUND
      KROUND = 1
      DO J = 1, NTERMS
         NDIG = NDSAV1
         CALL FMCSADD_R1(MXY(18),MXY(19))
         INT_TEMP3 = NDSAV1 - INT(MXY(38)%MP(2) - MXY(17)%MP(2))
         INT_TEMP4 = NDSAV1 - INT(MXY(39)%MP(2) - MXY(17)%MP(2))
         INT_TEMP1 = MAX(INT_TEMP3,INT_TEMP4)
         INT_TEMP2 = MAX(NGRD22,INT_TEMP1)
         NDIG2 = MIN(NDSAV1,INT_TEMP2)
         NDIG = NDIG2
         CALL FMCSMPY_R1(MXY(17),MXY(18))
         CALL FMCSMPY_R1(MXY(17),MXY(1))
         IF (J > 1) CALL FMDIVI_R1(MXY(17),J)
         NDIG = NDSAV1
         CALL FMCSADD_R1(MXY(20),MXY(19))
         NDIG = NDIG2
         CALL FMCSDIV(MXY(17),MXY(20),MXY(16))

         NDIG = NDSAV1
         IF (INT(MXY(16)%MP(1)) < 0) THEN
             CALL FMADD_R2(MXY(16),MXY(39))
         ELSE
             CALL FMADD_R2(MXY(16),MXY(38))
         ENDIF

         IF (KFLAG < 0) EXIT
         IF (MOD(J,JCHECK) == 0) THEN
             CALL FMADD(MXY(38),MXY(39),MXY(16))
             DO K = NDIG+1, 1, -1
                IF (MXY(16)%MP(K+1) /= MXY(22)%MP(K+1)) THEN
                    CALL FMEQ(MXY(16),MXY(22))
!                    CYCLE SUM_M1
                    GO TO 110
                ENDIF
             ENDDO
             EXIT
         ENDIF
  110    K = -31
      ENDDO
      KROUND = KRSAVE

      CALL FMPWR(MXY(1),MXY(2),MXY(12))
      CALL FMADD(MXY(38),MXY(39),MXY(7))
      CALL FMCANCEL(MXY(38),MXY(39),MXY(7),J)
      N_ACC = N_ACC - J
      CALL FMMPY(MXY(7),MXY(12),MXY(21))
      CALL FMMPY_R1(MXY(38),MXY(12))
      CALL FMMPY_R1(MXY(39),MXY(12))

      END SUBROUTINE FMIBTA_M1


      SUBROUTINE FMIBTA_M2(MXY,N_ACC)
      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MXY(40)
      INTEGER :: N_ACC
      INTENT(INOUT) :: MXY,N_ACC

      INTEGER :: INT_TEMP1,INT_TEMP2,J,JEXTRA,K,KFLAG1,KRSAVE,NDSAV1,NTERMS

!             Method 2.  Continued fraction expansion for B(x,a,b).

!             MXY(22) is the current approximation.
!             MXY(21) is the term in the sum, S(k).
!             MXY(19), MXY(20) are the latest denominators, Q(k-1) and Q(k).

      N_ACC = NINT(NDIG*ALOGM2)
      NTERMS = INT(INTMAX/10)
      JEXTRA = INT(MAX(1.0,5.76/ALOGMB + 1.0)) + NGRD52 + INT(0.152*NDIG)
      IF (NDIG+JEXTRA > NDIG) THEN
          CALL FMEQU_R1(MXY(1),NDIG,NDIG+JEXTRA)
          CALL FMEQU_R1(MXY(2),NDIG,NDIG+JEXTRA)
          CALL FMEQU_R1(MXY(3),NDIG,NDIG+JEXTRA)
      ENDIF
      NDIG = NDIG + JEXTRA
      NDSAV1 = NDIG
      CALL FMI2M(1,MXY(19))
      CALL FMI2M(1,MXY(20))
      CALL FMI2M(1,MXY(21))
      CALL FMEQ(MXY(21),MXY(38))
      CALL FMI2M(0,MXY(39))
      CALL FMEQ(MXY(2),MXY(24))
      CALL FMADD(MXY(2),MXY(3),MXY(25))
      CALL FMEQ(MXY(2),MXY(26))
      CALL FMI2M(1,MXY(12))
      CALL FMADD(MXY(2),MXY(12),MXY(27))
      CALL FMEQ(MXY(3),MXY(28))
      CALL FMSQR(MXY(2),MXY(29))
      CALL FMADD(MXY(29),MXY(26),MXY(30))
      CALL FMSUB(MXY(29),MXY(26),MXY(31))
      CALL FMMPY(MXY(24),MXY(25),MXY(12))
      CALL FMMPY(MXY(12),MXY(1),MXY(7))
      CALL FMDIV(MXY(7),MXY(30),MXY(33))
      IF (MXY(33)%MP(2) /= MUNKNO .AND. MXY(33)%MP(3) /= 0)  &
          MXY(33)%MP(1) = -MXY(33)%MP(1)
      CALL FMMPY(MXY(33),MXY(19),MXY(18))
      CALL FMADD(MXY(20),MXY(18),MXY(17))
      CALL FMMPY(MXY(18),MXY(21),MXY(12))
      CALL FMDIV(MXY(12),MXY(17),MXY(21))
      IF (MXY(21)%MP(2) /= MUNKNO .AND. MXY(21)%MP(3) /= 0)  &
          MXY(21)%MP(1) = -MXY(21)%MP(1)
      IF (MXY(21)%MP(1) > 0) THEN
          CALL FMADD(MXY(38),MXY(21),MXY(19))
          CALL FMEQ(MXY(19),MXY(38))
      ELSE
          CALL FMADD(MXY(39),MXY(21),MXY(19))
          CALL FMEQ(MXY(19),MXY(39))
      ENDIF
      CALL FMEQ(MXY(20),MXY(19))
      CALL FMEQ(MXY(17),MXY(20))

!             Method 2 continued fraction loop.

      KRSAVE = KROUND
      KROUND = 1
      DO J = 1, NTERMS
         CALL FMADDI(MXY(24),1)
         CALL FMADDI(MXY(25),1)
         CALL FMADDI(MXY(26),2)
         CALL FMADDI(MXY(27),2)
         CALL FMADDI(MXY(28),-1)
         CALL FMMPYI(MXY(26),4,MXY(15))
         CALL FMADDI(MXY(15),-4)
         CALL FMCSADD_R1(MXY(29),MXY(15))
         CALL FMADD(MXY(29),MXY(26),MXY(30))
         CALL FMSUB(MXY(29),MXY(26),MXY(31))

         CALL FMEQ(MXY(28),MXY(15))
         CALL FMCSMPY_R1(MXY(15),MXY(1))
         CALL FMMPYI_R1(MXY(15),J)
         CALL FMCSDIV(MXY(15),MXY(31),MXY(33))

         CALL FMEQ(MXY(33),MXY(18))
         CALL FMCSMPY_R1(MXY(18),MXY(19))
         CALL FMADD(MXY(20),MXY(18),MXY(17))
         CALL FMEQ(MXY(18),MXY(15))
         CALL FMCSMPY_R1(MXY(15),MXY(21))
         CALL FMCSDIV(MXY(15),MXY(17),MXY(21))
         IF (MXY(21)%MP(2) /= MUNKNO .AND. MXY(21)%MP(3) /= 0)  &
             MXY(21)%MP(1) = -MXY(21)%MP(1)

         NDIG = NDSAV1
         IF (MXY(21)%MP(1) > 0) THEN
             CALL FMADD(MXY(38),MXY(21),MXY(19))
             INT_TEMP1 = KFLAG
             CALL FMEQ(MXY(19),MXY(38))
         ELSE
             CALL FMADD(MXY(39),MXY(21),MXY(19))
             INT_TEMP1 = KFLAG
             CALL FMEQ(MXY(19),MXY(39))
         ENDIF
         KFLAG1 = INT_TEMP1

         CALL FMEQ(MXY(20),MXY(19))
         CALL FMEQ(MXY(17),MXY(20))
         CALL FMADD(MXY(38),MXY(39),MXY(15))
         INT_TEMP1 = NDSAV1 - INT(MXY(15)%MP(2) - MXY(21)%MP(2))
         INT_TEMP2 = MAX(NGRD22,INT_TEMP1)
         NDIG = MIN(NDSAV1,INT_TEMP2)
         CALL FMEQ(MXY(24),MXY(15))
         CALL FMCSMPY_R1(MXY(15),MXY(25))
         CALL FMCSMPY_R1(MXY(15),MXY(1))
         CALL FMCSDIV(MXY(15),MXY(30),MXY(33))
         IF (MXY(33)%MP(2) /= MUNKNO .AND. MXY(33)%MP(3) /= 0)  &
             MXY(33)%MP(1) = -MXY(33)%MP(1)

         CALL FMEQ(MXY(33),MXY(18))
         CALL FMCSMPY_R1(MXY(18),MXY(19))
         CALL FMADD(MXY(20),MXY(18),MXY(17))
         CALL FMEQ(MXY(18),MXY(15))
         CALL FMCSMPY_R1(MXY(15),MXY(21))
         CALL FMCSDIV(MXY(15),MXY(17),MXY(21))
         IF (MXY(21)%MP(2) /= MUNKNO .AND. MXY(21)%MP(3) /= 0)  &
             MXY(21)%MP(1) = -MXY(21)%MP(1)

         NDIG = NDSAV1
         IF (MXY(21)%MP(1) > 0) THEN
             CALL FMADD(MXY(38),MXY(21),MXY(19))
             INT_TEMP1 = KFLAG
             CALL FMEQ(MXY(19),MXY(38))
         ELSE
             CALL FMADD(MXY(39),MXY(21),MXY(19))
             INT_TEMP1 = KFLAG
             CALL FMEQ(MXY(19),MXY(39))
         ENDIF
         KFLAG = INT_TEMP1

!             Check for convergence.

         IF (KFLAG1 == 1 .AND. KFLAG == 1) THEN
             EXIT
         ENDIF
         CALL FMEQ(MXY(20),MXY(19))
         CALL FMEQ(MXY(17),MXY(20))
         CALL FMADD(MXY(38),MXY(39),MXY(15))
         INT_TEMP1 = NDSAV1 - INT(MXY(15)%MP(2) - MXY(21)%MP(2))
         INT_TEMP2 = MAX(NGRD22,INT_TEMP1)
         NDIG = MIN(NDSAV1,INT_TEMP2)
      ENDDO
      CALL FMADD(MXY(38),MXY(39),MXY(15))
      CALL FMCANCEL(MXY(38),MXY(39),MXY(15),K)
      N_ACC = N_ACC - K
      CALL FMEQ(MXY(15),MXY(22))
      KROUND = KRSAVE

!             Multiply the sums by x^a * (1-x)^b / a

      CALL FMEQ(MXY(38),MXY(40))
      CALL FMIBTA_M2B(MXY)
      CALL FMEQ(MXY(40),MXY(38))
      CALL FMEQ(MXY(39),MXY(40))
      CALL FMIBTA_M2B(MXY)
      CALL FMEQ(MXY(40),MXY(39))
      CALL FMEQ(MXY(22),MXY(40))
      CALL FMIBTA_M2B(MXY)
      CALL FMEQ(MXY(40),MXY(21))

      END SUBROUTINE FMIBTA_M2

      SUBROUTINE FMIBTA_M2B(MXY)
      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MXY(40)
      INTENT(INOUT) :: MXY
      INTEGER :: K,NTERMS

      NTERMS = INT(INTMAX/10)

      CALL FMLN(MXY(1),MXY(19))
      CALL FMMPY_R1(MXY(19),MXY(2))
      IF (MXY(1)%MP(2)*(-10) >= NDIG) THEN
          CALL FMEQ(MXY(1),MXY(15))
          CALL FMEQ(MXY(1),MXY(20))
          DO K = 2, NTERMS
             CALL FMMPY_R1(MXY(15),MXY(1))
             CALL FMDIVI(MXY(15),K,MXY(12))
             CALL FMADD_R1(MXY(20),MXY(12))
             IF (KFLAG /= 0) EXIT
          ENDDO
          CALL FMMPY_R1(MXY(20),MXY(3))
          IF (MXY(20)%MP(2) /= MUNKNO .AND. MXY(20)%MP(3) /= 0)  &
              MXY(20)%MP(1) = -MXY(20)%MP(1)
      ELSE
          CALL FMI2M(1,MXY(12))
          CALL FMSUB_R1(MXY(12),MXY(1))
          CALL FMLN(MXY(12),MXY(20))
          CALL FMMPY_R1(MXY(20),MXY(3))
      ENDIF
      CALL FMADD(MXY(19),MXY(20),MXY(12))
      CALL FMEXP(MXY(12),MXY(21))
      CALL FMMPY_R1(MXY(40),MXY(21))
      IF (MXY(21)%MP(2) == MUNKNO) THEN
          IF (MXY(40)%MP(1)*MXY(40)%MP(3) > 0) THEN
              CALL FMLN(MXY(40),MXY(12))
              CALL FMADD(MXY(12),MXY(19),MXY(7))
              CALL FMADD(MXY(7),MXY(20),MXY(12))
              CALL FMEXP(MXY(12),MXY(21))
          ELSE
              CALL FMEQ(MXY(40),MXY(13))
              IF (MXY(13)%MP(2) /= MUNKNO .AND. MXY(13)%MP(3) /= 0)  &
                  MXY(13)%MP(1) = -MXY(13)%MP(1)
              CALL FMLN(MXY(13),MXY(12))
              CALL FMADD(MXY(12),MXY(19),MXY(7))
              CALL FMADD(MXY(7),MXY(20),MXY(12))
              CALL FMEXP(MXY(12),MXY(21))
              IF (MXY(21)%MP(2) /= MUNKNO .AND. MXY(21)%MP(3) /= 0)  &
                  MXY(21)%MP(1) = -MXY(21)%MP(1)
          ENDIF
      ENDIF
      IF (ABS(MXY(40)%MP(2)) < MEXPOV) CALL FMDIV_R1(MXY(40),MXY(2))

      END SUBROUTINE FMIBTA_M2B

      SUBROUTINE FMIBTA_M5(MXY,MX,NMETHD,NDSAVE,N_ACC)
      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MXY(40), MX
      INTEGER :: NMETHD, NDSAVE, N_ACC

      INTENT(IN) :: MX, NMETHD, NDSAVE
      INTENT(INOUT) :: MXY

      INTEGER :: INT_TEMP1,INT_TEMP2,J,JEXTRA,K,KRSAVE,NDSAV1,NTERMS

!             Method 5.  Continued fraction expansion 2 for B(x,a,b).

!             MXY(22) is the current sum.
!             MXY(21) is the term in the sum, S(k).
!             MXY(19), MXY(20) are the latest denominators, Q(k-1) and Q(k).

      N_ACC = NINT(NDIG*ALOGM2)
      NTERMS = INT(INTMAX/10)
      JEXTRA = INT(MAX(1.0,5.76/ALOGMB + 1.0)) + INT(0.07*NDIG)
      IF (NDIG+JEXTRA > NDIG) THEN
          CALL FMEQU_R1(MXY(1),NDIG,NDIG+JEXTRA)
          CALL FMEQU_R1(MXY(2),NDIG,NDIG+JEXTRA)
          CALL FMEQU_R1(MXY(3),NDIG,NDIG+JEXTRA)
      ENDIF
      NDIG = NDIG + JEXTRA
      NDSAV1 = NDIG
      CALL FMSQR(MXY(1),MXY(23))
      CALL FMI2M(1,MXY(12))
      CALL FMSUB(MXY(2),MXY(12),MXY(24))
      CALL FMADD(MXY(2),MXY(3),MXY(25))
      CALL FMADDI(MXY(25),-1)
      CALL FMEQ(MXY(24),MXY(26))
      CALL FMI2M(1,MXY(12))
      CALL FMADD(MXY(2),MXY(12),MXY(27))
      CALL FMEQ(MXY(3),MXY(28))
      CALL FMI2M(1,MXY(7))
      CALL FMSUB(MXY(2),MXY(7),MXY(12))
      CALL FMSQR(MXY(12),MXY(29))
      CALL FMI2M(2,MXY(12))
      CALL FMSUB(MXY(12),MXY(1),MXY(30))
      IF (NMETHD == 6) THEN
          CALL FMEQU(MX,MXY(19),NDSAVE,NDIG)
          CALL FMMPY(MXY(2),MXY(19),MXY(7))
          CALL FMMPY(MXY(3),MXY(1),MXY(12))
          CALL FMSUB(MXY(7),MXY(12),MXY(31))
          CALL FMADDI(MXY(31),1)
      ELSE
          CALL FMADD(MXY(2),MXY(3),MXY(7))
          CALL FMMPY(MXY(7),MXY(1),MXY(12))
          CALL FMSUB(MXY(2),MXY(12),MXY(31))
          CALL FMADDI(MXY(31),1)
      ENDIF
      CALL FMEQ(MXY(2),MXY(35))
      CALL FMI2M(2,MXY(7))
      CALL FMSUB(MXY(2),MXY(7),MXY(12))
      CALL FMMPYI(MXY(12),4,MXY(36))

      CALL FMI2M(1,MXY(33))
      CALL FMMPY(MXY(35),MXY(31),MXY(12))
      CALL FMDIV(MXY(12),MXY(27),MXY(34))

      CALL FMI2M(1,MXY(19))
      CALL FMEQ(MXY(34),MXY(20))
      CALL FMDIV(MXY(33),MXY(34),MXY(21))
      IF (MXY(21)%MP(1) > 0) THEN
          CALL FMEQ(MXY(21),MXY(38))
          CALL FMI2M(0,MXY(39))
      ELSE
          CALL FMEQ(MXY(21),MXY(39))
          CALL FMI2M(0,MXY(38))
      ENDIF

!             Method 5 continued fraction loop.

      KRSAVE = KROUND
      KROUND = 1
      DO J = 1, NTERMS
         CALL FMADDI(MXY(24),1)
         CALL FMADDI(MXY(25),1)
         CALL FMADDI(MXY(26),2)
         CALL FMADDI(MXY(27),2)
         CALL FMADDI(MXY(28),-1)
         CALL FMADDI(MXY(36),8)
         CALL FMCSADD_R1(MXY(29),MXY(36))
         CALL FMCSADD_R1(MXY(31),MXY(30))
         CALL FMADDI(MXY(35),1)

         CALL FMEQ(MXY(24),MXY(15))
         CALL FMCSMPY_R1(MXY(15),MXY(25))
         CALL FMCSMPYI_R1(MXY(15),J)
         CALL FMCSMPY_R1(MXY(15),MXY(28))
         CALL FMCSMPY_R1(MXY(15),MXY(23))
         CALL FMCSDIV(MXY(15),MXY(29),MXY(33))

         CALL FMEQ(MXY(35),MXY(15))
         CALL FMCSMPY_R1(MXY(15),MXY(31))
         CALL FMCSDIV(MXY(15),MXY(27),MXY(34))
         CALL FMEQ(MXY(28),MXY(15))
         CALL FMCSMPY_R1(MXY(15),MXY(1))
         CALL FMCSMPYI_R1(MXY(15),J)
         CALL FMDIV_R1(MXY(15),MXY(26))
         CALL FMCSADD_R1(MXY(34),MXY(15))
         CALL FMADDI(MXY(34),J)

         CALL FMEQ(MXY(33),MXY(18))
         CALL FMCSMPY_R1(MXY(18),MXY(19))
         CALL FMEQ(MXY(34),MXY(15))
         CALL FMCSMPY_R1(MXY(15),MXY(20))
         CALL FMADD(MXY(15),MXY(18),MXY(17))
         CALL FMEQ(MXY(18),MXY(15))
         CALL FMCSMPY_R1(MXY(15),MXY(21))
         CALL FMCSDIV(MXY(15),MXY(17),MXY(21))
         IF (MXY(21)%MP(2) /= MUNKNO .AND. MXY(21)%MP(3) /= 0)  &
             MXY(21)%MP(1) = -MXY(21)%MP(1)

         NDIG = NDSAV1
         IF (MXY(21)%MP(1) > 0) THEN
             CALL FMADD(MXY(38),MXY(21),MXY(19))
             INT_TEMP1 = KFLAG
             CALL FMEQ(MXY(19),MXY(38))
         ELSE
             CALL FMADD(MXY(39),MXY(21),MXY(19))
             INT_TEMP1 = KFLAG
             CALL FMEQ(MXY(19),MXY(39))
         ENDIF
         KFLAG = INT_TEMP1

!             Check for convergence.

         IF (KFLAG == 1 .AND. J > 4) THEN
             EXIT
         ENDIF

         CALL FMEQ(MXY(20),MXY(19))
         CALL FMEQ(MXY(17),MXY(20))
         CALL FMADD(MXY(38),MXY(39),MXY(15))
         INT_TEMP1 = NDSAV1 - INT(MXY(15)%MP(2) - MXY(21)%MP(2))
         INT_TEMP2 = MAX(NGRD22,INT_TEMP1)
         NDIG = MIN(NDSAV1,INT_TEMP2)
      ENDDO
      CALL FMADD(MXY(38),MXY(39),MXY(15))
      CALL FMCANCEL(MXY(38),MXY(39),MXY(15),K)
      N_ACC = N_ACC - K
      CALL FMEQ(MXY(15),MXY(22))

      KROUND = KRSAVE
      NDIG = NDSAV1

!             Multiply the sums by x^a * (1-x)^b

      CALL FMEQ(MXY(38),MXY(40))
      CALL FMIBTA_M5B(MX,MXY,NDSAVE,NMETHD)
      CALL FMEQ(MXY(40),MXY(38))
      CALL FMEQ(MXY(39),MXY(40))
      CALL FMIBTA_M5B(MX,MXY,NDSAVE,NMETHD)
      CALL FMEQ(MXY(40),MXY(39))
      CALL FMEQ(MXY(22),MXY(40))
      CALL FMIBTA_M5B(MX,MXY,NDSAVE,NMETHD)
      CALL FMEQ(MXY(40),MXY(21))

      END SUBROUTINE FMIBTA_M5

      SUBROUTINE FMIBTA_M5B(MX,MXY,NDSAVE,NMETHD)
      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MX,MXY(40)
      INTEGER :: NDSAVE,NMETHD
      INTENT(IN) :: MX,NDSAVE,NMETHD
      INTENT(INOUT) :: MXY
      INTEGER :: K,NTERMS
      LOGICAL, EXTERNAL :: FMCOMP

      NTERMS = INT(INTMAX/10)
      CALL FMI2M(1,MXY(12))
      IF (FMCOMP(MXY(1),'==',MXY(12)) .AND.  &
          NMETHD == 6) THEN
          CALL FMEQU(MX,MXY(19),NDSAVE,NDIG)
          CALL FMMPY_R1(MXY(19),MXY(2))
          IF (MXY(19)%MP(2) /= MUNKNO .AND. MXY(19)%MP(3) /= 0)  &
              MXY(19)%MP(1) = -MXY(19)%MP(1)
      ELSE IF (MX%MP(2) <= -1 .AND. NMETHD == 6) THEN
          CALL FMEQU(MX,MXY(19),NDSAVE,NDIG)
          CALL FMEQ(MXY(19),MXY(15))
          CALL FMEQ(MXY(19),MXY(20))
          DO K = 2, NTERMS
             CALL FMMPY_R1(MXY(15),MXY(19))
             CALL FMDIVI(MXY(15),K,MXY(12))
             CALL FMADD_R1(MXY(20),MXY(12))
             IF (KFLAG /= 0) EXIT
          ENDDO
          CALL FMMPY(MXY(20),MXY(2),MXY(19))
          IF (MXY(19)%MP(2) /= MUNKNO .AND. MXY(19)%MP(3) /= 0)  &
              MXY(19)%MP(1) = -MXY(19)%MP(1)
      ELSE
          CALL FMLN(MXY(1),MXY(19))
          CALL FMMPY_R1(MXY(19),MXY(2))
      ENDIF
      IF (NMETHD == 6) THEN
          CALL FMEQU(MX,MXY(20),NDSAVE,NDIG)
          CALL FMLN(MXY(20),MXY(10))
          CALL FMMPY(MXY(10),MXY(3),MXY(20))
      ELSE IF (MXY(1)%MP(2) <= -1) THEN
          CALL FMEQ(MXY(1),MXY(15))
          CALL FMEQ(MXY(1),MXY(20))
          DO K = 2, NTERMS
             CALL FMMPY_R1(MXY(15),MXY(1))
             CALL FMDIVI(MXY(15),K,MXY(12))
             CALL FMADD_R1(MXY(20),MXY(12))
             IF (KFLAG /= 0) EXIT
          ENDDO
          CALL FMMPY_R1(MXY(20),MXY(3))
          IF (MXY(20)%MP(2) /= MUNKNO .AND. MXY(20)%MP(3) /= 0)  &
              MXY(20)%MP(1) = -MXY(20)%MP(1)
      ELSE
          CALL FMI2M(1,MXY(12))
          CALL FMSUB_R1(MXY(12),MXY(1))
          CALL FMLN(MXY(12),MXY(20))
          CALL FMMPY_R1(MXY(20),MXY(3))
      ENDIF
      CALL FMADD(MXY(19),MXY(20),MXY(12))
      CALL FMEXP(MXY(12),MXY(21))
      CALL FMMPY_R2(MXY(40),MXY(21))
      IF (MXY(21)%MP(2) == MUNKNO) THEN
          IF (MXY(40)%MP(1)*MXY(40)%MP(3) > 0) THEN
              CALL FMLN(MXY(40),MXY(12))
              CALL FMADD(MXY(12),MXY(19),MXY(7))
              CALL FMADD(MXY(7),MXY(20),MXY(12))
              CALL FMEXP(MXY(12),MXY(21))
          ELSE
              CALL FMEQ(MXY(40),MXY(13))
              IF (MXY(13)%MP(2) /= MUNKNO .AND. MXY(13)%MP(3) /= 0)  &
                  MXY(13)%MP(1) = -MXY(13)%MP(1)
              CALL FMLN(MXY(13),MXY(12))
              CALL FMADD(MXY(12),MXY(19),MXY(7))
              CALL FMADD(MXY(7),MXY(20),MXY(12))
              CALL FMEXP(MXY(12),MXY(21))
              IF (MXY(21)%MP(2) /= MUNKNO .AND. MXY(21)%MP(3) /= 0)  &
                  MXY(21)%MP(1) = -MXY(21)%MP(1)
          ENDIF
      ENDIF
      CALL FMEQ(MXY(21),MXY(40))

      END SUBROUTINE FMIBTA_M5B

      SUBROUTINE FMIBTA2(K_RETURN_CODE,MXY,MXSAVE,NTERMS,NUMTRY,NMETHD)

!  Check for various special cases in Incomplete Beta.

      USE ModLib_FMVALS
      IMPLICIT NONE

      REAL (KIND(1.0D0)) :: MXSAVE
      INTEGER :: IEXTRA,J,J4,JR,JSWITCH,K,KRSAVE,K_RETURN_CODE,N,NDSAV1,NMETHD,NTERMS,NUMTRY,NUP
      INTEGER, PARAMETER :: KPRIME(8) = (/ 2, 3, 5, 7, 11, 13, 17, 19 /)
      LOGICAL, EXTERNAL :: FMCOMP
      TYPE(MULTI) :: MXY(37),MJSUMS(LJSUMS)
      INTENT (INOUT) :: K_RETURN_CODE,MXY,MXSAVE,NTERMS,NUMTRY,NMETHD

      K_RETURN_CODE = 0
      CALL FMI2M(0,MXY(32))
      NDSAV1 = NDIG

!             If B is small, use more guard digits.

      CALL FMDPM(1.0D-10,MXY(12))
      IF (FMCOMP(MXY(3),'<=',MXY(12))) THEN
          IEXTRA = NGRD52
          CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
          CALL FMEQU_R1(MXY(2),NDIG,NDIG+IEXTRA)
          CALL FMEQU_R1(MXY(3),NDIG,NDIG+IEXTRA)
          NDIG = NDIG + IEXTRA
      ENDIF

      NTERMS = INT(INTMAX/10)
      NMETHD = 0

!             Check for special cases.

      IF (MXY(1)%MP(3) == 0) THEN
          CALL FMI2M(0,MXY(21))
          K_RETURN_CODE = 1
          RETURN
      ENDIF
      CALL FMI2M(1,MXY(28))
      IF (FMCOMP(MXY(28),'==',MXY(1))) THEN
          IEXTRA = NGRD52
          CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
          CALL FMEQU_R1(MXY(2),NDIG,NDIG+IEXTRA)
          CALL FMEQU_R1(MXY(3),NDIG,NDIG+IEXTRA)
          NDIG = NDIG + IEXTRA
          CALL FMBETA(MXY(2),MXY(3),MXY(31))
          CALL FMEQ(MXY(31),MXY(21))
          K_RETURN_CODE = 1
          RETURN
      ELSE IF (MXY(1)%MP(1) < 0 .OR. FMCOMP(MXY(1),'>',MXY(28))) THEN
          CALL FMST2M('UNKNOWN',MXY(21))
          KFLAG = -4
          K_RETURN_CODE = 2
          RETURN
      ENDIF
      IF (MXY(2)%MP(1) < 0 .OR. MXY(2)%MP(3) == 0) THEN
          CALL FMST2M('UNKNOWN',MXY(21))
          KFLAG = -4
          K_RETURN_CODE = 2
          RETURN
      ENDIF
      IF (MXY(3)%MP(1) < 0) THEN
          CALL FMST2M('UNKNOWN',MXY(21))
          KFLAG = -4
          K_RETURN_CODE = 2
          RETURN
      ENDIF
      JR = NUMTRY
      IF (MXY(2)%MP(2) < (-NDIG) .AND. MXY(3)%MP(2) < (-NDIG)) THEN
          CALL FMSUB(MXY(28),MXY(1),MXY(12))
          CALL FMLN(MXY(12),MXY(21))
          CALL FMDIV(MXY(28),MXY(2),MXY(12))
          CALL FMSUB(MXY(12),MXY(21),MXY(13))
          CALL FMPWR(MXY(1),MXY(2),MXY(12))
          CALL FMMPY(MXY(13),MXY(12),MXY(21))
          K_RETURN_CODE = 1
          RETURN
      ENDIF
      CALL FMI2M(1,MXY(12))
      CALL FMSUB(MXY(12),MXY(3),MXY(7))
      CALL FMMPY(MXY(1),MXY(7),MXY(12))
      JR = KROUND
      KROUND = 1
      CALL FMADD(MXY(12),MXY(28),MXY(7))
      KROUND = JR
      IF (FMCOMP(MXY(7),'==',MXY(28))) THEN
          CALL FMLN(MXY(1),MXY(12))
          CALL FMMPY(MXY(2),MXY(12),MXY(21))
          CALL FMLN(MXY(2),MXY(12))
          CALL FMSUB_R2(MXY(21),MXY(12))
          CALL FMEXP(MXY(12),MXY(21))
          K_RETURN_CODE = 2
          RETURN
      ENDIF

!             When A or B is large, check for an underflowed result.

      CALL FMDPM(1.0D+7,MXY(12))
      IF (FMCOMP(MXY(2),'>',MXY(12)) .OR. FMCOMP(MXY(3),'>',MXY(12))) THEN

!             If B is much larger than A, approximate BETA(A,B) and use that as an upper bound.

          IF (MXY(3)%MP(2) >= MXY(2)%MP(2)+NDIG) THEN
              CALL FMADD(MXY(3),MXY(2),MXY(12))
              CALL FMLN(MXY(12),MXY(23))
              CALL FMMPY_R2(MXY(2),MXY(23))
              CALL FMEQ(MXY(2),MXY(27))
              CALL FMLNGM(MXY(27),MXY(24))
              CALL FMSUB(MXY(24),MXY(23),MXY(12))
              CALL FMEXP(MXY(12),MXY(21))
              IF (MXY(21)%MP(2) <= -MXSAVE-1) THEN
                  K_RETURN_CODE = 2
                  RETURN
              ENDIF
          ENDIF

!             If A > 2 > B, use the bound
!                C = min( X , (A-2)/(A+B-2) )
!                BETA(X,A,B) < (A-1)*X/B * C**(A-2) * (1-C)**B
!
!             An alternate bound is also tried:
!                C = min( X , (A-1)/(A+B-2) )
!                BETA(X,A,B) < C**A * (1-C)**(1-B)

          CALL FMI2M(2,MXY(12))
          IF (FMCOMP(MXY(2),'>',MXY(12)) .AND. FMCOMP(MXY(3),'<',MXY(12))) THEN
              CALL FMI2M(2,MXY(6))
              CALL FMSUB(MXY(2),MXY(6),MXY(12))
              CALL FMADD(MXY(2),MXY(3),MXY(7))
              CALL FMSUB_R1(MXY(7),MXY(6))
              CALL FMDIV_R1(MXY(12),MXY(7))
              CALL FMMIN(MXY(1),MXY(12),MXY(23))
              CALL FMI2M(1,MXY(12))
              CALL FMSUB_R2(MXY(2),MXY(12))
              CALL FMLN(MXY(12),MXY(27))
              CALL FMLN(MXY(1),MXY(12))
              CALL FMADD_R1(MXY(27),MXY(12))
              CALL FMLN(MXY(3),MXY(12))
              CALL FMSUB_R1(MXY(27),MXY(12))
              CALL FMI2M(2,MXY(7))
              CALL FMSUB(MXY(2),MXY(7),MXY(21))
              CALL FMLN(MXY(23),MXY(12))
              CALL FMMPY_R2(MXY(21),MXY(12))
              CALL FMADD_R1(MXY(27),MXY(12))
              CALL FMI2M(1,MXY(7))
              CALL FMSUB(MXY(7),MXY(23),MXY(12))
              CALL FMLN(MXY(12),MXY(21))
              CALL FMMPY(MXY(3),MXY(21),MXY(12))
              CALL FMADD_R1(MXY(27),MXY(12))
              CALL FMEXP(MXY(27),MXY(21))
              IF (MXY(21)%MP(2) <= -MXSAVE-1) THEN
                  K_RETURN_CODE = 2
                  RETURN
              ENDIF
              CALL FMI2M(1,MXY(7))
              CALL FMSUB(MXY(2),MXY(7),MXY(12))
              CALL FMADD(MXY(2),MXY(3),MXY(7))
              CALL FMI2M(2,MXY(6))
              CALL FMSUB_R1(MXY(7),MXY(6))
              CALL FMDIV_R1(MXY(12),MXY(7))
              CALL FMMIN(MXY(1),MXY(12),MXY(23))
              CALL FMI2M(1,MXY(7))
              CALL FMSUB(MXY(7),MXY(23),MXY(12))
              CALL FMLN(MXY(12),MXY(27))
              CALL FMSUB(MXY(3),MXY(7),MXY(6))
              CALL FMMPY_R2(MXY(6),MXY(27))
              CALL FMLN(MXY(1),MXY(12))
              CALL FMMPY_R2(MXY(2),MXY(12))
              CALL FMADD_R2(MXY(12),MXY(27))
              CALL FMEXP(MXY(27),MXY(21))
              IF (MXY(21)%MP(2) <= -MXSAVE-1) THEN
                  K_RETURN_CODE = 2
                  RETURN
              ENDIF
          ENDIF

!             If A > 2 and B > 2, use the bound
!                C = min( X , (A-1)/(A+B-2) )
!                BETA(X,A,B) < X * C**(A-1) * (1-C)**(B-1)

          CALL FMI2M(2,MXY(12))
          IF (FMCOMP(MXY(2),'>',MXY(12)) .AND. FMCOMP(MXY(3),'>',MXY(12))) THEN
              CALL FMI2M(1,MXY(7))
              CALL FMSUB(MXY(2),MXY(7),MXY(12))
              CALL FMADD(MXY(2),MXY(3),MXY(7))
              CALL FMI2M(2,MXY(6))
              CALL FMSUB_R1(MXY(7),MXY(6))
              CALL FMDIV_R1(MXY(12),MXY(7))
              CALL FMMIN(MXY(1),MXY(12),MXY(23))
              CALL FMI2M(1,MXY(7))
              CALL FMSUB(MXY(7),MXY(23),MXY(12))
              CALL FMLN(MXY(12),MXY(27))
              CALL FMSUB(MXY(3),MXY(7),MXY(6))
              CALL FMMPY_R2(MXY(6),MXY(27))
              CALL FMLN(MXY(23),MXY(12))
              CALL FMSUB(MXY(2),MXY(7),MXY(6))
              CALL FMMPY_R2(MXY(6),MXY(12))
              CALL FMADD_R2(MXY(12),MXY(27))
              CALL FMLN(MXY(1),MXY(12))
              CALL FMADD_R2(MXY(12),MXY(27))
              CALL FMEXP(MXY(27),MXY(21))
              IF (MXY(21)%MP(2) <= -MXSAVE-1) THEN
                  K_RETURN_CODE = 2
                  RETURN
              ENDIF
          ENDIF
      ENDIF

!             Check for cases where X is large enough so that at this precision, B(X,A,B) = B(A,B).
!             These are often unstable, so it is better to use Beta.

      CALL FMI2M(1,MXY(12))
      CALL FMI2M(2,MXY(6))
      CALL FMADD(MXY(2),MXY(3),MXY(7))
      IF (FMCOMP(MXY(2),'>',MXY(12)) .AND. FMCOMP(MXY(7),'>',MXY(6))) THEN
              CALL FMI2M(1,MXY(7))
              CALL FMSUB(MXY(2),MXY(7),MXY(12))
              CALL FMADD(MXY(2),MXY(3),MXY(7))
              CALL FMI2M(2,MXY(6))
              CALL FMSUB_R1(MXY(7),MXY(6))
              CALL FMDIV(MXY(12),MXY(7),MXY(31))
          CALL FMI2M(1,MXY(12))
          CALL FMADD(MXY(2),MXY(3),MXY(7))
          CALL FMADDI(MXY(7),-3)
          IF (FMCOMP(MXY(31),'<',MXY(12)) .AND. FMCOMP(MXY(1),'>',MXY(31)) .AND.  &
              MXY(7)%MP(3) /= 0) THEN
              CALL FMI2M(1,MXY(7))
              CALL FMSUB(MXY(2),MXY(7),MXY(6))
              CALL FMSUB(MXY(3),MXY(7),MXY(12))
              CALL FMMPY_R2(MXY(6),MXY(12))
              CALL FMADD(MXY(2),MXY(3),MXY(6))
              CALL FMI2M(3,MXY(7))
              CALL FMSUB_R2(MXY(6),MXY(7))
              CALL FMDIV(MXY(12),MXY(7),MXY(30))
              IF (MXY(30)%MP(1) >= 0) THEN
                  CALL FMI2M(1,MXY(7))
                  CALL FMSUB_R2(MXY(2),MXY(7))
                  CALL FMSQRT(MXY(30),MXY(12))
                  CALL FMADD(MXY(7),MXY(12),MXY(30))
                  CALL FMADD(MXY(2),MXY(3),MXY(7))
                  CALL FMI2M(2,MXY(6))
                  CALL FMSUB_R1(MXY(7),MXY(6))
                  CALL FMDIV_R1(MXY(30),MXY(7))
              ELSE
                  CALL FMDPM(DBLE(1.1),MXY(30))
              ENDIF
              CALL FMI2M(1,MXY(12))
              IF (FMCOMP(MXY(30),'>',MXY(31)) .AND. FMCOMP(MXY(30),'<',MXY(12)) .AND.  &
                  FMCOMP(MXY(1),'>=',MXY(30))) THEN

!                 Approximate B(A,B).

                  JR = KROUND
                  KROUND = 1
                  CALL FMADD(MXY(2),MXY(3),MXY(12))
                  KROUND = JR
                  IF (FMCOMP(MXY(12),'==',MXY(2))) THEN
                      CALL FMLN(MXY(3),MXY(12))
                      CALL FMDPM(0.5D0,MXY(7))
                      CALL FMSUB_R2(MXY(3),MXY(7))
                      CALL FMMPY(MXY(7),MXY(12),MXY(29))
                      CALL FMSUB_R1(MXY(29),MXY(3))
                      CALL FMDPM(DLOGTP/2.0D0,MXY(12))
                      CALL FMSUB_R1(MXY(29),MXY(12))
                      CALL FMLN(MXY(2),MXY(12))
                      CALL FMMPY_R2(MXY(3),MXY(12))
                      CALL FMSUB_R1(MXY(29),MXY(12))
                  ELSE IF (FMCOMP(MXY(12),'==',MXY(3))) THEN
                      CALL FMLN(MXY(2),MXY(12))
                      CALL FMDP2M(0.5D0,MXY(7))
                      CALL FMSUB_R2(MXY(2),MXY(7))
                      CALL FMMPY(MXY(7),MXY(12),MXY(29))
                      CALL FMSUB_R1(MXY(29),MXY(2))
                      CALL FMDPM(DLOGTP/2.0D0,MXY(12))
                      CALL FMSUB_R1(MXY(29),MXY(12))
                      CALL FMLN(MXY(3),MXY(12))
                      CALL FMMPY_R2(MXY(2),MXY(12))
                      CALL FMSUB_R1(MXY(29),MXY(12))
                  ELSE
                      CALL FMLN(MXY(2),MXY(12))
                      CALL FMDP2M(0.5D0,MXY(7))
                      CALL FMSUB_R2(MXY(2),MXY(7))
                      CALL FMMPY(MXY(7),MXY(12),MXY(29))
                      CALL FMLN(MXY(3),MXY(12))
                      CALL FMDP2M(0.5D0,MXY(7))
                      CALL FMSUB_R2(MXY(3),MXY(7))
                      CALL FMMPY_R2(MXY(7),MXY(12))
                      CALL FMADD_R1(MXY(29),MXY(12))
                      CALL FMADD(MXY(2),MXY(3),MXY(12))
                      CALL FMLN(MXY(12),MXY(7))
                      CALL FMDP2M(0.5D0,MXY(6))
                      CALL FMSUB_R2(MXY(12),MXY(6))
                      CALL FMMPY(MXY(6),MXY(7),MXY(12))
                      CALL FMSUB_R1(MXY(29),MXY(12))
                      CALL FMDPM(DLOGTP/2.0D0,MXY(12))
                      CALL FMSUB_R1(MXY(29),MXY(12))
                  ENDIF
                  CALL FMEXP(MXY(29),MXY(9))
                  CALL FMEQ(MXY(9),MXY(29))

!                 Bound the area from X to 1.

                  CALL FMI2M(1,MXY(12))
                  JR = KROUND
                  KROUND = 1
                  CALL FMSUB(MXY(12),MXY(1),MXY(7))
                  KROUND = JR
                  IF (FMCOMP(MXY(7),'==',MXY(12))) THEN
                      CALL FMLN(MXY(1),MXY(12))
                      CALL FMI2M(1,MXY(6))
                      CALL FMSUB(MXY(2),MXY(6),MXY(7))
                      CALL FMMPY(MXY(7),MXY(12),MXY(28))
                      CALL FMSUB(MXY(3),MXY(6),MXY(7))
                      CALL FMMPY(MXY(1),MXY(7),MXY(12))
                      CALL FMSUB_R1(MXY(28),MXY(12))
                      CALL FMSUB(MXY(6),MXY(1),MXY(12))
                      CALL FMDIVI_R1(MXY(12),2)
                      CALL FMLN(MXY(12),MXY(13))
                      CALL FMSUB_R1(MXY(28),MXY(13))
                  ELSE
                      CALL FMLN(MXY(1),MXY(12))
                      CALL FMI2M(1,MXY(6))
                      CALL FMSUB(MXY(2),MXY(6),MXY(7))
                      CALL FMMPY(MXY(7),MXY(12),MXY(28))
                      CALL FMSUB(MXY(3),MXY(6),MXY(7))
                      CALL FMSUB(MXY(6),MXY(1),MXY(12))
                      CALL FMLN(MXY(12),MXY(13))
                      CALL FMMPY_R2(MXY(7),MXY(13))
                      CALL FMADD_R1(MXY(28),MXY(13))
                      CALL FMDIVI_R1(MXY(12),2)
                      CALL FMLN(MXY(12),MXY(13))
                      CALL FMADD_R1(MXY(28),MXY(13))
                  ENDIF
                  CALL FMEXP(MXY(28),MXY(9))
                  CALL FMEQ(MXY(9),MXY(28))
                  JR = KROUND
                  KROUND = 1
                  CALL FMSUB(MXY(29),MXY(28),MXY(12))
                  KROUND = JR
                  IF (FMCOMP(MXY(12),'==',MXY(29))) THEN
                      CALL FMEQ(MXY(28),MXY(33))
                      CALL FMBETA(MXY(2),MXY(3),MXY(31))
                      JR = KROUND
                      KROUND = 1
                      CALL FMSUB(MXY(31),MXY(33),MXY(12))
                      KROUND = JR
                      IF (FMCOMP(MXY(12),'==',MXY(31))) THEN
                          CALL FMEQ(MXY(31),MXY(21))
                          K_RETURN_CODE = 1
                          RETURN
                      ENDIF
                  ENDIF
              ENDIF
          ENDIF
      ELSE IF (MXY(2)%MP(2) < 1 .AND. FMCOMP(MXY(3),'>',MXY(12))) THEN

!                 Approximate B(A,B).

          JR = KROUND
          KROUND = 1
          CALL FMADD(MXY(2),MXY(3),MXY(12))
          KROUND = JR
          IF (FMCOMP(MXY(12),'==',MXY(2))) THEN
              CALL FMLN(MXY(3),MXY(12))
              CALL FMDP2M(0.5D0,MXY(7))
              CALL FMSUB_R2(MXY(3),MXY(7))
              CALL FMMPY(MXY(7),MXY(12),MXY(29))
              CALL FMSUB_R1(MXY(29),MXY(3))
              CALL FMDPM(DLOGTP/2.0D0,MXY(12))
              CALL FMSUB_R1(MXY(29),MXY(12))
              CALL FMLN(MXY(2),MXY(12))
              CALL FMMPY_R2(MXY(3),MXY(12))
              CALL FMSUB_R1(MXY(29),MXY(12))
          ELSE IF (FMCOMP(MXY(12),'==',MXY(3))) THEN
              CALL FMLN(MXY(2),MXY(12))
              CALL FMDP2M(0.5D0,MXY(7))
              CALL FMSUB_R2(MXY(2),MXY(7))
              CALL FMMPY(MXY(7),MXY(12),MXY(29))
              CALL FMSUB_R1(MXY(29),MXY(2))
              CALL FMDPM(DLOGTP/2.0D0,MXY(12))
              CALL FMSUB_R1(MXY(29),MXY(12))
              CALL FMLN(MXY(3),MXY(12))
              CALL FMMPY_R2(MXY(2),MXY(12))
              CALL FMSUB_R1(MXY(29),MXY(12))
          ELSE
              CALL FMLN(MXY(2),MXY(12))
              CALL FMDP2M(0.5D0,MXY(7))
              CALL FMSUB_R2(MXY(2),MXY(7))
              CALL FMMPY(MXY(7),MXY(12),MXY(29))
              CALL FMLN(MXY(3),MXY(12))
              CALL FMDP2M(0.5D0,MXY(7))
              CALL FMSUB_R2(MXY(3),MXY(7))
              CALL FMMPY_R2(MXY(7),MXY(12))
              CALL FMADD_R1(MXY(29),MXY(12))
              CALL FMADD(MXY(2),MXY(3),MXY(12))
              CALL FMLN(MXY(12),MXY(7))
              CALL FMDP2M(0.5D0,MXY(6))
              CALL FMSUB_R2(MXY(12),MXY(6))
              CALL FMMPY(MXY(6),MXY(7),MXY(12))
              CALL FMSUB_R1(MXY(29),MXY(12))
              CALL FMDPM(DLOGTP/2.0D0,MXY(12))
              CALL FMSUB_R1(MXY(29),MXY(12))
          ENDIF
          CALL FMEXP(MXY(29),MXY(9))
          CALL FMEQ(MXY(9),MXY(29))

!                 Bound the area from X to 1.

          CALL FMI2M(1,MXY(12))
          JR = KROUND
          KROUND = 1
          CALL FMSUB(MXY(12),MXY(1),MXY(7))
          KROUND = JR
          IF (FMCOMP(MXY(7),'==',MXY(12))) THEN
              CALL FMLN(MXY(1),MXY(12))
              CALL FMI2M(1,MXY(6))
              CALL FMSUB(MXY(2),MXY(6),MXY(7))
              CALL FMMPY(MXY(7),MXY(12),MXY(28))
              CALL FMSUB(MXY(3),MXY(6),MXY(7))
              CALL FMMPY(MXY(1),MXY(7),MXY(12))
              CALL FMSUB_R1(MXY(28),MXY(12))
              CALL FMSUB(MXY(6),MXY(1),MXY(12))
              CALL FMDIVI_R1(MXY(12),2)
              CALL FMLN(MXY(12),MXY(13))
              CALL FMSUB_R1(MXY(28),MXY(13))
              CALL FMEXP(MXY(28),MXY(9))
              CALL FMEQ(MXY(9),MXY(28))
          ELSE
              CALL FMLN(MXY(1),MXY(12))
              CALL FMI2M(1,MXY(6))
              CALL FMSUB(MXY(2),MXY(6),MXY(7))
              CALL FMMPY(MXY(7),MXY(12),MXY(28))
              CALL FMSUB(MXY(3),MXY(6),MXY(7))
              CALL FMSUB(MXY(6),MXY(1),MXY(12))
              CALL FMLN(MXY(12),MXY(13))
              CALL FMMPY_R2(MXY(7),MXY(13))
              CALL FMADD_R1(MXY(28),MXY(13))
              CALL FMDIVI_R1(MXY(12),2)
              CALL FMLN(MXY(12),MXY(13))
              CALL FMADD_R1(MXY(28),MXY(13))
              CALL FMEXP(MXY(28),MXY(9))
              CALL FMEQ(MXY(9),MXY(28))
          ENDIF
          JR = KROUND
          KROUND = 1
          CALL FMSUB(MXY(29),MXY(28),MXY(12))
          KROUND = JR
          IF (FMCOMP(MXY(12),'==',MXY(29))) THEN
              CALL FMBETA(MXY(2),MXY(3),MXY(31))
              CALL FMEQ(MXY(31),MXY(21))
              K_RETURN_CODE = 1
              RETURN
          ENDIF
      ENDIF

!             If B is small enough, use one of two series or an asymptotic series,
!             depending on the size of X and A.

      CALL FMI2M(1,MXY(6))
      JR = KROUND
      KROUND = 1
      CALL FMADD(MXY(6),MXY(3),MXY(7))
      CALL FMADD(MXY(2),MXY(3),MXY(12))
      KROUND = JR
      IF ((FMCOMP(MXY(7),'==',MXY(6)) .AND. FMCOMP(MXY(12),'==',MXY(2))) ) THEN
          CALL FMDP2M(0.5D0,MXY(12))
          IF (FMCOMP(MXY(1),'<=',MXY(12))) THEN
              CALL FMI2M(0,MXY(22))
              CALL FMEQ(MXY(1),MXY(23))
              CALL FMI2M(1,MXY(7))
              CALL FMADD(MXY(2),MXY(7),MXY(12))
              CALL FMDIV(MXY(23),MXY(12),MXY(24))
              CALL FMEQ(MXY(2),MXY(14))
              CALL FMADDI(MXY(14),1)
              NDSAV1 = NDIG
              DO J = 2, NTERMS
                 CALL FMADD_R1(MXY(22),MXY(24))
                 IF (KFLAG /= 0 .AND. J >= 3) EXIT
                 NDIG = MIN(NDSAV1,MAX(NGRD22,NDSAV1-INT(MXY(22)%MP(2)-  &
                                                         MXY(24)%MP(2))+1))
                 CALL FMMPY_R1(MXY(23),MXY(1))
                 CALL FMADDI(MXY(14),1)
                 CALL FMDIV(MXY(23),MXY(14),MXY(24))
                 NDIG = NDSAV1
              ENDDO
              CALL FMPWR(MXY(1),MXY(2),MXY(12))
              CALL FMI2M(1,MXY(6))
              CALL FMDIV(MXY(6),MXY(2),MXY(7))
              CALL FMADD(MXY(7),MXY(22),MXY(6))
              CALL FMMPY(MXY(12),MXY(6),MXY(22))
              CALL FMEQ(MXY(22),MXY(21))
              K_RETURN_CODE = 1
              RETURN
          ENDIF
          CALL FMDP2M(0.5D0,MXY(12))
          CALL FMI2M(20,MXY(7))
          IF ((FMCOMP(MXY(1),'>',MXY(12)) .AND. FMCOMP(MXY(2),'<',MXY(7)))) THEN
              CALL FMI2M(0,MXY(22))
              CALL FMI2M(1,MXY(12))
              CALL FMSUB(MXY(12),MXY(1),MXY(25))
              CALL FMI2M(1,MXY(7))
              CALL FMADD(MXY(3),MXY(7),MXY(12))
              CALL FMPWR(MXY(25),MXY(12),MXY(23))
              CALL FMI2M(1,MXY(12))
              CALL FMSUB(MXY(12),MXY(2),MXY(7))
              CALL FMMPY_R2(MXY(7),MXY(23))

              CALL FMEQ(MXY(23),MXY(24))
              NDSAV1 = NDIG
              DO J = 2, NTERMS
                 CALL FMADD_R1(MXY(22),MXY(24))
                 IF (KFLAG /= 0 .AND. J >= 3) EXIT
                 NDIG = MIN(NDSAV1,MAX(NGRD22,NDSAV1-INT(MXY(22)%MP(2)-  &
                                                         MXY(24)%MP(2))+1))
                 CALL FMI2M(J,MXY(7))
                 CALL FMSUB(MXY(7),MXY(2),MXY(12))
                 CALL FMMPY(MXY(23),MXY(12),MXY(7))
                 CALL FMMPY(MXY(7),MXY(25),MXY(12))
                 CALL FMDIVI(MXY(12),J,MXY(23))
                 CALL FMDIVI(MXY(23),J,MXY(24))
                 NDIG = NDSAV1
              ENDDO
              CALL FMLN(MXY(25),MXY(12))
              CALL FMI2M(1,MXY(7))
              CALL FMDIV(MXY(7),MXY(2),MXY(6))
              CALL FMSUB(MXY(6),MXY(12),MXY(7))
              CALL FMSUB(MXY(7),MXY(22),MXY(23))
              CALL FMEULR(MXY(24))
              CALL FMI2M(1,MXY(12))
              CALL FMADD(MXY(2),MXY(12),MXY(25))
              CALL FMPSI(MXY(25),MXY(11))
              CALL FMEQ(MXY(11),MXY(25))
              CALL FMSUB(MXY(23),MXY(24),MXY(12))
              CALL FMSUB(MXY(12),MXY(25),MXY(21))
              K_RETURN_CODE = 1
              RETURN
          ENDIF

          CALL FMDP2M(0.5D0,MXY(12))
          CALL FMI2M(20,MXY(7))
          IF ((FMCOMP(MXY(1),'>',MXY(12)) .AND. FMCOMP(MXY(2),'>=',MXY(7)))) THEN
              CALL FMSP2M(0.7*REAL(NDIG)*ALOGMT,MXY(28))
              IF (FMCOMP(MXY(2),'>=',MXY(28))) THEN
                  NUP = 0
                  CALL FMEQ(MXY(2),MXY(32))
                  CALL FMI2M(0,MXY(33))
              ELSE
                  CALL FMSUB(MXY(28),MXY(2),MXY(12))
                  CALL FMADDI(MXY(12),1)
                  CALL FMM2I(MXY(12),NUP)
                  CALL FMI2M(NUP,MXY(12))
                  CALL FMADD(MXY(2),MXY(12),MXY(32))
                  CALL FMI2M(1,MXY(33))
                  CALL FMEQ(MXY(2),MXY(23))
                  NDSAV1 = NDIG
                  DO J = 1, NUP-1
                     CALL FMMPY_R1(MXY(23),MXY(1))
                     CALL FMI2M(J,MXY(12))
                     CALL FMADD(MXY(2),MXY(12),MXY(7))
                     CALL FMDIV(MXY(23),MXY(7),MXY(24))
                     NDIG = NDSAV1
                     CALL FMADD_R1(MXY(33),MXY(24))
                     NDIG = MIN(NDSAV1,  &
                            MAX(NGRD22,NDSAV1-INT(MXY(33)%MP(2)-MXY(24)%MP(2))+1))
                  ENDDO
                  NDIG = NDSAV1
                  CALL FMPWR(MXY(1),MXY(2),MXY(12))
                  CALL FMMPY(MXY(33),MXY(12),MXY(13))
                  CALL FMI2M(1,MXY(7))
                  CALL FMSUB(MXY(7),MXY(1),MXY(12))
                  CALL FMPWR(MXY(12),MXY(3),MXY(33))
                  CALL FMMPY_R2(MXY(13),MXY(33))
                  CALL FMDIV_R1(MXY(33),MXY(2))
              ENDIF

              CALL FMI2M(1,MXY(7))
              CALL FMDIVI(MXY(7),2,MXY(12))
              CALL FMSUB(MXY(32),MXY(12),MXY(29))
              CALL FMLN(MXY(1),MXY(12))
              CALL FMMPY(MXY(29),MXY(12),MXY(30))
              IF (MXY(30)%MP(2) /= MUNKNO .AND. MXY(30)%MP(3) /= 0)  &
                  MXY(30)%MP(1) = -MXY(30)%MP(1)
              CALL FMIGM2(MXY(3),MXY(30),MXY(31))
              CALL FMPWR(MXY(30),MXY(3),MXY(12))
              CALL FMEQ(MXY(30),MXY(13))
              IF (MXY(13)%MP(2) /= MUNKNO .AND. MXY(13)%MP(3) /= 0)  &
                  MXY(13)%MP(1) = -MXY(13)%MP(1)
              CALL FMEXP(MXY(13),MXY(7))
              CALL FMMPY(MXY(7),MXY(12),MXY(13))
              CALL FMDIV_R1(MXY(31),MXY(13))
              CALL FMEQ(MXY(31),MXY(22))
              CALL FMSQR(MXY(29),MXY(12))
              CALL FMMPYI(MXY(12),4,MXY(23))
              CALL FMI2M(1,MXY(24))
              CALL FMI2M(1,MXY(25))
              CALL FMI2M(1,MXY(26))
              CALL FMLN(MXY(1),MXY(12))
              CALL FMDIVI(MXY(12),2,MXY(7))
              CALL FMSQR(MXY(7),MXY(28))
              NDSAV1 = NDIG
              J4 = 0
              KRSAVE = KROUND
              KROUND = 1
              DO J = 1, NTERMS
                 JSWITCH = MAX(2,INT(NDIG*DLOGMB/(2.0D0*LOG(23.0)) + 2))
                 IF (J < JSWITCH) THEN
                     J4 = 0
                     CALL FMMPYI_R1(MXY(25),4)
                     CALL FMMPYI(MXY(26),2*J-1,MXY(12))
                     CALL FMMPYI(MXY(12),2*J,MXY(26))
                     CALL FMI2M(2,MXY(7))
                     CALL FMSUB(MXY(7),MXY(25),MXY(12))
                     CALL FMDIV(MXY(12),MXY(26),MXY(27))
                     CALL FMBERN(2*J,MXY(27),MXY(8))
                     CALL FMEQ(MXY(8),MXY(27))
                 ELSE
                     IF (J4 == 0) THEN
                         J4 = 1
                         N = 2*J
                         DO K = 1, 8
                            CALL FMI2M(KPRIME(K),MJSUMS(K))
                            CALL FMIPWR(MJSUMS(K),N,MXY(12))
                            CALL FMEQ(MXY(12),MJSUMS(K))
                         ENDDO
                     ELSE
                         DO K = 1, 8
                            CALL FMMPYI_R1(MJSUMS(K),KPRIME(K)**2)
                         ENDDO
                     ENDIF
                     CALL FMPI(MXY(18))
                     CALL FMI2M(1,MXY(14))
                     CALL FMI2M(1,MXY(15))
                     DO K = 1, 8
                        CALL FMEQ(MJSUMS(K),MXY(17))
                        CALL FMI2M(KPRIME(K)**2-1,MXY(12))
                        JR = KROUND
                        KROUND = 1
                        CALL FMSUB(MXY(17),MXY(14),MXY(7))
                        CALL FMDIV_R2(MXY(12),MXY(7))
                        CALL FMSUB(MXY(14),MXY(7),MXY(16))
                        KROUND = JR
                        CALL FMI2M(1,MXY(12))
                        IF (FMCOMP(MXY(16),'==',MXY(12))) EXIT
                        CALL FMMPY_R1(MXY(15),MXY(16))
                     ENDDO
                     CALL FMEQ(MJSUMS(1),MXY(17))
                     CALL FMI2M(-1,MXY(7))
                     CALL FMSQR(MXY(18),MXY(13))
                     CALL FMDIV(MXY(7),MXY(13),MXY(12))
                     CALL FMI2M(2,MXY(7))
                     CALL FMSUB(MXY(7),MXY(17),MXY(6))
                     CALL FMI2M(8,MXY(7))
                     CALL FMSUB_R1(MXY(7),MXY(17))
                     CALL FMDIV(MXY(6),MXY(7),MXY(13))
                     CALL FMMPY(MXY(12),MXY(13),MXY(7))
                     CALL FMMPY(MXY(7),MXY(15),MXY(16))
                     CALL FMMPY_R2(MXY(16),MXY(27))
                 ENDIF
                 CALL FMI2M(2*J-2,MXY(7))
                 CALL FMADD(MXY(3),MXY(7),MXY(12))
                 CALL FMMPY(MXY(12),MXY(31),MXY(7))
                 CALL FMMPYI(MXY(7),2*J-1,MXY(31))
                 CALL FMI2M(2*J-1,MXY(7))
                 CALL FMADD(MXY(30),MXY(7),MXY(12))
                 CALL FMMPY(MXY(24),MXY(12),MXY(7))
                 CALL FMADD_R1(MXY(31),MXY(7))
                 CALL FMDIV_R1(MXY(31),MXY(23))
                 CALL FMMPY_R1(MXY(24),MXY(28))
                 CALL FMMPY(MXY(27),MXY(31),MXY(19))
                 NDIG = NDSAV1
                 CALL FMADD_R1(MXY(22),MXY(19))
                 IF (KFLAG /= 0 .AND. J >= 3) EXIT
                 NDIG = MIN(NDSAV1,MAX(NGRD22,NDSAV1-INT(MXY(22)%MP(2)-  &
                                                         MXY(19)%MP(2))+1))
              ENDDO
              KROUND = KRSAVE
              NDIG = NDSAV1
              CALL FMPWR(MXY(1),MXY(29),MXY(12))
              CALL FMLN(MXY(1),MXY(13))
              IF (MXY(13)%MP(2) /= MUNKNO .AND. MXY(13)%MP(3) /= 0)  &
                  MXY(13)%MP(1) = -MXY(13)%MP(1)
              CALL FMPWR(MXY(13),MXY(3),MXY(21))
              CALL FMMPY(MXY(22),MXY(12),MXY(7))
              CALL FMMPY_R2(MXY(7),MXY(21))
              CALL FMADD_R2(MXY(33),MXY(21))
              K_RETURN_CODE = 1
              RETURN
          ENDIF
      ENDIF

!             If A or B is large in magnitude, use more guard digits.

      IEXTRA = MIN(MAX(INT(MXY(2)%MP(2)),INT(MXY(3)%MP(2)),0) ,  &
                   INT(1.0+ALOGMX/ALOGMB))
      IF (IEXTRA > 0) THEN
          CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
          CALL FMEQU_R1(MXY(2),NDIG,NDIG+IEXTRA)
          CALL FMEQU_R1(MXY(3),NDIG,NDIG+IEXTRA)
      ENDIF
      NDIG = NDIG + IEXTRA
      RETURN
      END SUBROUTINE FMIBTA2

      SUBROUTINE FMIBTA3(MX,MA,MB,MC,MXY,K_RETURN_CODE)

!  Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MX,MA,MB,MC
      INTEGER :: J,J1,J2,K,K1,K2,K_RETURN_CODE,KRSAVE,NDS,NDSAVE
      INTENT (IN) :: MX,MA,MB
      INTENT (INOUT) :: MC
      TYPE(MULTI) :: MXY(37)

      J = NTRACE
      NTRACE = 0
      K = KWARN
      KWARN = 0
      NDSAVE = NDIG
      KRSAVE = KROUND
      KROUND = 1
      CALL FMI2M(1,MXY(1))
      CALL FMSUB(MXY(1),MX,MXY(2))
      IF (MX%MP(1) < 0 .OR. MXY(2)%MP(1) < 0 .OR. MA%MP(1) < 0 .OR.  &
          MB%MP(1) < 0) GO TO 110
      IF (KRSAVE /= 1 .AND. (MXY(2)%MP(3) == 0 .OR. MX%MP(2) < -NDIG .OR.  &
                             MA%MP(2) < -NDIG .OR. MB%MP(2) < -NDIG)) THEN
          IF (MXY(2)%MP(3) == 0) THEN
              KROUND = KRSAVE
              CALL FMBETA(MA,MB,MC)
              K_RETURN_CODE = 1
          ELSE IF (MX%MP(3) == 0) THEN
              KROUND = KRSAVE
              CALL FMI2M(0,MC)
              K_RETURN_CODE = 1
          ELSE
              NDIG = NDIG + NGRD52
              CALL FMEQU(MX,MXY(1),NDSAVE,NDIG)
              CALL FMEQU(MA,MXY(2),NDSAVE,NDIG)
              CALL FMEQU(MB,MXY(3),NDSAVE,NDIG)
              CALL FMI2M(1,MXY(4))
              CALL FMSUB(MXY(4),MXY(3),MXY(5))
              CALL FMMPY(MXY(1),MXY(5),MXY(6))
              CALL FMADD(MXY(2),MXY(4),MXY(7))
              CALL FMDIV(MXY(6),MXY(7),MXY(8))
              CALL FMDIV(MXY(4),MXY(2),MXY(9))
              IF (MXY(9)%MP(2) - MXY(8)%MP(2) > NDIG) THEN
                  CALL FMPWR(MXY(1),MXY(2),MXY(10))
                  IF (MXY(10)%MP(2) > MEXPUN .AND. MXY(10)%MP(2) < MEXPOV) THEN
                      CALL FMDIV(MXY(10),MXY(2),MXY(11))
                      CALL FMMPY(MXY(10),MXY(8),MXY(12))
                      IF (MA%MP(2) < -NDSAVE) THEN
                          CALL FMLN(MXY(1),MXY(13))
                          CALL FMADD_R1(MXY(12),MXY(13))
                          CALL FMEQU(MXY(9),MXY(5),NDIG,NDSAVE)
                          CALL FMEQU(MXY(13),MXY(6),NDIG,NDSAVE)
                          NDIG = NDSAVE
                          KROUND = KRSAVE
                          CALL FMADD(MXY(5),MXY(6),MC)
                          K_RETURN_CODE = 1
                      ELSE IF (MXY(11)%MP(2) < MEXPOV) THEN
                          CALL FMEQU(MXY(11),MXY(5),NDIG,NDSAVE)
                          CALL FMEQU(MXY(5),MXY(13),NDSAVE,NDIG)
                          CALL FMSUB(MXY(11),MXY(13),MXY(8))
                          IF (MXY(8)%MP(3) == 0) THEN
                              CALL FMEQU(MXY(12),MXY(6),NDIG,NDSAVE)
                              NDIG = NDSAVE
                              KROUND = KRSAVE
                              CALL FMADD(MXY(5),MXY(6),MC)
                              IF (MC%MP(2) == MUNKNO) THEN
                                  J1 = MXY(5)%MP(2)
                                  K1 = MXY(6)%MP(2)
                                  J2 = J1
                                  IF (J1 /= MEXPUN .AND. J1 < 0) J2 = J1 + 2*NDIG
                                  IF (J1 /= MEXPOV .AND. J1 > 0) J2 = J1 - 2*NDIG
                                  K2 = K1
                                  IF (K1 /= MEXPUN .AND. K1 < 0) K2 = K1 + 2*NDIG
                                  IF (K1 /= MEXPOV .AND. K1 > 0) K2 = K1 - 2*NDIG
                                  MXY(5)%MP(2) = J2
                                  MXY(6)%MP(2) = K2
                                  CALL FMADD(MXY(5),MXY(6),MC)
                                  K2 = MC%MP(2) - (J2 - J1) - (K2 - K1)
                                  IF (MC%MP(2) /= MUNKNO) MC%MP(2) = K2
                              ENDIF
                          ELSE
                              KROUND = KRSAVE
                              CALL FMEQU(MXY(11),MC,NDIG,NDSAVE)
                          ENDIF
                          K_RETURN_CODE = 1
                      ENDIF
                  ENDIF
              ENDIF
              IF (K_RETURN_CODE /= 1) THEN
                  CALL FMI2M(1,MXY(4))
                  CALL FMSUB(MXY(4),MXY(1),MXY(5))
                  CALL FMPWR(MXY(5),MXY(3),MXY(6))
                  CALL FMDIV(MXY(4),MXY(3),MXY(7))
                  CALL FMSUB(MXY(4),MXY(2),MXY(8))
                  CALL FMMPY(MXY(1),MXY(8),MXY(9))
                  CALL FMADD(MXY(3),MXY(4),MXY(10))
                  CALL FMDIV(MXY(9),MXY(10),MXY(11))
                  IF (MXY(6)%MP(2) <= MEXPUN) THEN
                      NDIG = NDSAVE
                      KROUND = KRSAVE
                      CALL FMBETA(MA,MB,MC)
                      K_RETURN_CODE = 1
                  ELSE
                      CALL FMADD(MXY(7),MXY(11),MXY(12))
                      CALL FMMPY(MXY(6),MXY(12),MXY(8))
                      NDIG = NDSAVE
                      KROUND = KRSAVE
                      CALL FMBETA(MA,MB,MC)
                      IF (MC%MP(2) - MXY(8)%MP(2) > NDIG) K_RETURN_CODE = 1
                  ENDIF
              ENDIF
          ENDIF
      ENDIF

  110 KFLAG = 0
      NTRACE = J
      KWARN = K
      NDIG = NDSAVE
      KROUND = KRSAVE
      IF (K_RETURN_CODE == 1) THEN
          IF (MC%MP(2) == MUNKNO .AND. MX%MP(2) /= MUNKNO .AND.  &
              MA%MP(2) /= MUNKNO .AND. MB%MP(2) /= MUNKNO) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMIBTA'
              KFLAG = -4
              CALL FMWRN2
              NCALL = NCALL - 1
          ELSE IF (ABS(MC%MP(2)) == MEXPOV .AND. ABS(MX%MP(2)) < MEXPOV .AND.  &
                   ABS(MA%MP(2))  < MEXPOV .AND. ABS(MB%MP(2)) < MEXPOV) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMIBTA'
              IF (MC%MP(2) == MEXPOV) KFLAG = -5
              IF (MC%MP(2) == MEXPUN) KFLAG = -6
              CALL FMWRN2
              NCALL = NCALL - 1
          ENDIF
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMIBTA'
              CALL FMNTR(2,MX,MA,2,1)
              IF (ABS(NTRACE) >= 2 .AND. NCALL <= LVLTRC) THEN
                  NDS = NDIG
                  NDIG = NDSAVE
                  IF (NTRACE < 0) THEN
                      CALL FMNTRJ(MB,NDIG)
                  ELSE
                      CALL FMPRNT(MB)
                  ENDIF
                  NDIG = NDS
              ENDIF
              CALL FMNTR(1,MC,MC,1,1)
              NCALL = NCALL - 1
          ENDIF
      ENDIF
      RETURN
      END SUBROUTINE FMIBTA3

      SUBROUTINE FMIGM1(MA,MB,MC)

!  MC = Incomplete Gamma(MA,MB)

!  Integral from 0 to MB of e**(-t) * t**(MA-1)  dt.

!  This is (lower case) gamma(a,x).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC
      DOUBLE PRECISION :: X,A,B,ERR,SMALL,BIG,TOL,T1,BIGJ,YT
      REAL (KIND(1.0D0)) :: MAXE,MODA2,MXSAVE
      INTEGER :: IEXTRA,INTA,INTG,J,JEXTRA,JR,JTERMS,K,KFLAG1,KFLAGA,KFLAGI,KFLAGX,KFLGOK,      &
                 KL,KOVUN,KR_RETRY,KRESLT,K_RETURN_CODE,KRSAVE,KWRNSV,KXNEG,LESS,NDGOAL,NDIG2,  &
                 NDOLD,NDSAV1,NDSAV2,NDSAVE,NGOAL,NMETHD,NMNNDG,NMXDIF,NT,NTERMS,NUMTRY,N_ACC
      LOGICAL, EXTERNAL :: FMCOMP
      DOUBLE PRECISION, EXTERNAL :: FMDPLG
      REAL :: C,C1,C2,D,T,TLNB,Y
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      TYPE(MULTI) :: MXY(19),MRETRY
      INTEGER :: INT_TEMP1

      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < NDIG+2) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      N_ACC = NINT(NDIG*ALOGM2)
      K_RETURN_CODE = 0
      IF (MB%MP(3) == 0) GO TO 110

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1 .AND. (MA%MP(2) < -NDIG .OR. MB%MP(2) < -NDIG)) THEN
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          NDSAVE = NDIG
          KRSAVE = KROUND
          KROUND = 1
          NDIG = NDIG + NGRD52
          N_ACC = NINT(NDIG*ALOGM2)
          CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
          CALL FMEQU(MB,MXY(2),NDSAVE,NDIG)
          CALL FMI2M(1,MXY(3))
          CALL FMADD(MXY(1),MXY(3),MXY(4))
          IF (MXY(1)%MP(3) /= 0 .AND. MXY(4)%MP(3) /= 0) THEN
              CALL FMDIV(MXY(2),MXY(4),MXY(6))
              CALL FMMPY(MXY(6),MXY(1),MXY(5))
              IF (MXY(3)%MP(2) - MXY(5)%MP(2) > NDIG) THEN
                  CALL FMPWR(MXY(2),MXY(1),MXY(6))
                  CALL FMDIV(MXY(6),MXY(1),MXY(7))
                  CALL FMEQU(MXY(7),MXY(6),NDIG,NDSAVE)
                  CALL FMEQU(MXY(6),MXY(8),NDSAVE,NDIG)
                  CALL FMSUB(MXY(7),MXY(8),MXY(6))
                  IF (MXY(6)%MP(3) == 0) THEN
                      CALL FMMPY(MXY(5),MXY(7),MXY(8))
                      CALL FMEQU(MXY(7),MXY(9),NDIG,NDSAVE)
                      CALL FMEQU(MXY(8),MXY(6),NDIG,NDSAVE)
                      NDIG = NDSAVE
                      KROUND = KRSAVE
                      CALL FMSUB(MXY(9),MXY(6),MC)
                      IF (MC%MP(2) == MUNKNO) THEN
                          CALL FMI2M(2,MXY(9))
                          CALL FMMOD(MA,MXY(9),MXY(8))
                          IF (MB%MP(1) < 0 .AND. MXY(8)%MP(3) /= 0) THEN
                              IF (KRSAVE ==  2) KROUND = -1
                              IF (KRSAVE == -1) KROUND =  2
                          ENDIF
                          CALL FMI2M(1,MXY(8))
                          CALL FMSUB(MXY(8),MXY(5),MXY(6))
                          CALL FMMPY(MXY(7),MXY(6),MC)
                      ENDIF
                  ELSE
                      CALL FMSUB(MXY(3),MXY(5),MXY(6))
                      CALL FMMPY(MXY(6),MXY(7),MXY(8))
                      KROUND = KRSAVE
                      CALL FMEQU(MXY(8),MC,NDIG,NDSAVE)
                  ENDIF
                  K_RETURN_CODE = 1
              ENDIF
              IF (K_RETURN_CODE /= 1 .AND. MA%MP(2) < -NDIG) THEN
                  CALL FMDIV(MXY(3),MXY(1),MXY(4))
                  CALL FMEQU(MXY(4),MXY(6),NDIG,NDSAVE)
                  CALL FMEQU(MXY(6),MXY(5),NDSAVE,NDIG)
                  CALL FMSUB(MXY(4),MXY(5),MXY(6))
                  IF (MXY(6)%MP(3) == 0) THEN
                      CALL FMI2M(0,MXY(5))
                      CALL FMSUB(MXY(5),MXY(2),MXY(6))
                      CALL FMEI(MXY(6),MXY(7))
                      CALL FMLN(MXY(2),MXY(8))
                      CALL FMSUB(MXY(7),MXY(8),MXY(9))
                      CALL FMEULR(MXY(10))
                      CALL FMSUB(MXY(9),MXY(10),MXY(11))
                      CALL FMPWR(MXY(2),MXY(1),MXY(5))
                      CALL FMMPY(MXY(5),MXY(11),MXY(12))
                      CALL FMADD(MXY(8),MXY(12),MXY(13))
                      CALL FMEQU(MXY(4),MXY(6),NDIG,NDSAVE)
                      CALL FMEQU(MXY(13),MXY(7),NDIG,NDSAVE)
                      NDIG = NDSAVE
                      KROUND = KRSAVE
                      CALL FMADD(MXY(6),MXY(7),MC)
                      K_RETURN_CODE = 1
                  ENDIF
              ENDIF
          ENDIF
          KFLAG = 0
          NTRACE = J
          KWARN = K
          NDIG = NDSAVE
          KROUND = KRSAVE
          IF (K_RETURN_CODE == 1) THEN
              IF (NTRACE /= 0) THEN
                  NCALL = NCALL + 1
                  NAMEST(NCALL) = 'FMIGM1'
                  CALL FMNTR(2,MA,MB,2,1)
                  NCALL = NCALL - 1
              ENDIF
              IF (MC%MP(2) == MUNKNO .AND. MA%MP(2) /= MUNKNO .AND.  &
                  MB%MP(2) /= MUNKNO) THEN
                  NCALL = NCALL + 1
                  NAMEST(NCALL) = 'FMIGM1'
                  KFLAG = -4
                  CALL FMWRN2
                  NCALL = NCALL - 1
              ELSE IF (ABS(MC%MP(2)) == MEXPOV .AND. ABS(MA%MP(2)) < MEXPOV .AND.  &
                       ABS(MB%MP(2))  < MEXPOV) THEN
                  NCALL = NCALL + 1
                  NAMEST(NCALL) = 'FMIGM1'
                  IF (MC%MP(2) == MEXPOV) KFLAG = -5
                  IF (MC%MP(2) == MEXPUN) KFLAG = -6
                  CALL FMWRN2
                  NCALL = NCALL - 1
              ENDIF
              IF (NTRACE /= 0) THEN
                  NCALL = NCALL + 1
                  NAMEST(NCALL) = 'FMIGM1'
                  CALL FMNTR(1,MC,MC,1,1)
                  NCALL = NCALL - 1
              ENDIF
          ENDIF
      ENDIF
      IF (K_RETURN_CODE == 1) RETURN

  110 CALL FMENT2('FMIGM1   ',MA,MB,2,1,MC,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF

      KR_RETRY = 0

  120 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      N_ACC = NINT(NDIG*ALOGM2)
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
      CALL FMEQU(MB,MXY(2),NDSAVE,NDIG)
      NUMTRY = 0

  130 NTERMS = INT(INTMAX/10)

!             Check for special cases.

!             See if A is small enough so that the result is X**A/A.

      CALL FMI2M(1,MXY(5))
      JR = KROUND
      KROUND = 1
      CALL FMADD(MXY(1),MXY(5),MXY(4))
      KROUND = JR
      IF (FMCOMP(MXY(4),'==',MXY(5))) THEN
          CALL FMPWR(MXY(2),MXY(1),MXY(5))
          CALL FMDIV(MXY(5),MXY(1),MXY(13))
          IF (MXY(13)%MP(2) /= MUNKNO) GO TO 200
      ENDIF

!             Check to see if X is large enough so that the result is Gamma(A).

      CALL FMI2M(1,MXY(5))
      CALL FMDIV(MXY(1),MXY(2),MXY(4))
      MXY(4)%MP(1) = 1
      CALL FMDPM(DBLE(0.001),MXY(3))
      IF (FMCOMP(MXY(2),'>',MXY(5)) .AND. FMCOMP(MXY(4),'<=',MXY(3))) THEN
          CALL FMI2M(1,MXY(4))
          CALL FMSUB(MXY(1),MXY(4),MXY(5))
          CALL FMLN(MXY(2),MXY(6))
          CALL FMMPY(MXY(5),MXY(6),MXY(4))
          CALL FMSUB(MXY(4),MXY(2),MXY(6))
          CALL FMEXP(MXY(6),MXY(15))
          IF (MXY(15)%MP(2) /= MUNKNO) THEN
              CALL FMGAM(MXY(1),MXY(14))
              CALL FMSUB_R1(MXY(14),MXY(15))
              IF (MXY(14)%MP(2) > MXY(15)%MP(2)+NDIG .AND.  &
                  MXY(14)%MP(2) /= MUNKNO) THEN
                  CALL FMEQ(MXY(14),MXY(13))
                  GO TO 200
              ENDIF
          ENDIF
      ENDIF

!             A,X are double precision approximations to the two arguments to this function.
!             INTA = A if A is a small integer.  It is used to limit the number of terms used in
!                    the asymptotic series and in the continued fraction expansion.

      INTA = NTERMS
      KWRNSV = KWARN
      KWARN = 0
      CALL FMM2I(MXY(1),INTG)
      KFLAGI = KFLAG
      IF (KFLAG == 0) INTA = INTG
      CALL FMM2DP(MXY(1),A)
      KFLAGA = KFLAG
      IF (KFLAG /= 0 .AND. MXY(1)%MP(2) < 0) THEN
          A = 1.0D0/DPMAX
          IF (MXY(1)%MP(1) < 0) A = -A
          KFLAGA = 0
      ENDIF
      CALL FMM2DP(MXY(2),X)
      KFLAGX = KFLAG
      IF (KFLAG /= 0 .AND. MXY(2)%MP(2) < 0) THEN
          X = 1.0D0/DPMAX
          IF (MXY(2)%MP(1) < 0) X = -X
          KFLAGX = 0
      ENDIF
      KWARN = KWRNSV

!             If A or X is large in magnitude, use more guard digits.

      J = MXY(1)%MP(2)
      IF (MXY(1)%MP(3) >= SQRT(DBLE(MBASE))) J = J + 1
      K = MXY(2)%MP(2)
      IF (MXY(2)%MP(3) >= SQRT(DBLE(MBASE))) K = K + 1
      IEXTRA = MIN(MAX(J,K,0) , INT(1.0+ALOGMX/ALOGMB))
      IF (IEXTRA > 0) THEN
          CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
          CALL FMEQU_R1(MXY(2),NDIG,NDIG+IEXTRA)
      ENDIF
      NDOLD = NDIG
      NDIG = NDIG + IEXTRA
      N_ACC = NINT(NDIG*ALOGM2)

!             KXNEG = 1 if X is negative and A is a positive integer.

      KXNEG = 0

!             MODA2 = MOD(A,2) when KXNEG is 1.

      MODA2 = 0

      IF (MXY(1)%MP(2) == MEXPOV .OR. MXY(2)%MP(2) == MEXPOV) THEN
          IF (MXY(1)%MP(2) == MEXPOV .AND. MXY(1)%MP(1) > 0) THEN
              IF (MXY(2)%MP(3) == 0) THEN
                  CALL FMI2M(0,MXY(13))
                  GO TO 180
              ENDIF
              IF (MXY(2)%MP(2) == MEXPOV .AND. MXY(2)%MP(1) > 0) THEN
                  CALL FMST2M('OVERFLOW',MXY(13))
                  KFLAG = -5
                  GO TO 180
              ELSE IF (MXY(2)%MP(1) > 0) THEN
                  CALL FMI2M(1,MXY(13))
                  IF (FMCOMP(MXY(2),'<=',MXY(13))) THEN
                      CALL FMST2M('UNDERFLOW',MXY(13))
                      KFLAG = -6
                      GO TO 180
                  ELSE
                      CALL FMST2M('OVERFLOW',MXY(13))
                      KFLAG = -5
                      GO TO 180
                  ENDIF
              ENDIF
          ENDIF
          IF (MXY(2)%MP(2) == MEXPOV .AND. MXY(2)%MP(1) > 0) THEN
              CALL FMGAM(MXY(1),MXY(15))
              CALL FMEQ(MXY(15),MXY(13))
              GO TO 180
          ENDIF
          IF (MXY(2)%MP(2) == MEXPOV .AND. MXY(2)%MP(1) < 0 .AND.  &
              MXY(1)%MP(1) > 0.AND. MXY(1)%MP(3) > 0) THEN
              IF (MXY(1)%MP(2) /= MEXPOV) THEN
                  CALL FMINT(MXY(1),MXY(12))
                  IF (FMCOMP(MXY(1),'==',MXY(12))) THEN
                      CALL FMI2M(2,MXY(9))
                      CALL FMMOD(MXY(12),MXY(9),MXY(5))
                      CALL FMEQ(MXY(5),MXY(9))
                      IF (MXY(9)%MP(3) /= 0) THEN
                          CALL FMST2M('-OVERFLOW',MXY(13))
                          KFLAG = -5
                          GO TO 180
                      ELSE
                          CALL FMST2M('OVERFLOW',MXY(13))
                          KFLAG = -5
                          GO TO 180
                      ENDIF
                  ENDIF
              ENDIF
          ENDIF
          CALL FMST2M('UNKNOWN',MXY(13))
          KFLAG = -4
          GO TO 200
      ENDIF

      IF (MXY(1)%MP(2) == MEXPUN .OR. MXY(2)%MP(2) == MEXPUN) THEN
          CALL FMABS(MXY(1),MXY(4))
          CALL FMI2M(1,MXY(5))
          IF (FMCOMP(MXY(4),'<',MXY(5)) .AND. MXY(2)%MP(2) == MEXPUN) THEN
              CALL FMST2M('UNKNOWN',MXY(13))
              KFLAG = -4
              GO TO 200
          ENDIF
          CALL FMABS(MXY(1),MXY(4))
          CALL FMI2M(1,MXY(5))
          IF (FMCOMP(MXY(4),'>=',MXY(5)) .AND. MXY(2)%MP(2) == MEXPUN .AND.  &
              MXY(1)%MP(1) > 0 .AND. MXY(2)%MP(1) > 0) THEN
              CALL FMST2M('UNDERFLOW',MXY(13))
              KFLAG = -6
              GO TO 200
          ENDIF
      ENDIF

      IF (MXY(1)%MP(1) < 0 .OR. MXY(1)%MP(3) == 0) THEN
          CALL FMINT(MXY(1),MXY(12))
          IF (FMCOMP(MXY(1),'==',MXY(12))) THEN
              CALL FMST2M('UNKNOWN',MXY(13))
              KFLAG = -4
              GO TO 200
          ENDIF
      ENDIF
      IF (MXY(2)%MP(3) == 0) THEN
          IF (MXY(1)%MP(1) <= 0) THEN
              CALL FMST2M('UNKNOWN',MXY(13))
              KFLAG = -4
              GO TO 200
          ELSE
              CALL FMI2M(0,MXY(13))
              GO TO 200
          ENDIF
      ENDIF
      IF (MXY(2)%MP(1) < 0) THEN
          CALL FMINT(MXY(1),MXY(12))
          IF (FMCOMP(MXY(1),'==',MXY(12))) THEN
              KXNEG = 1
              CALL FMI2M(2,MXY(9))
              CALL FMMOD(MXY(12),MXY(9),MXY(5))
              CALL FMEQ(MXY(5),MXY(9))
              IF (MXY(9)%MP(3) /= 0) MODA2 = 1
          ELSE
              CALL FMST2M('UNKNOWN',MXY(13))
              KFLAG = -4
              GO TO 200
          ENDIF
      ENDIF
      CALL FMMAX(MXY(1),MXY(2),MXY(5))
      CALL FMMIN(MXY(1),MXY(2),MXY(6))
      CALL FMDPM(1.0D6,MXY(3))
      CALL FMDPM(1.0D2,MXY(4))
      IF (FMCOMP(MXY(5),'>=',MXY(3)) .AND. FMCOMP(MXY(6),'>=',MXY(4))) THEN
          CALL FMI2M(1,MXY(5))
          CALL FMSUB(MXY(1),MXY(5),MXY(7))
          CALL FMMIN(MXY(7),MXY(2),MXY(8))
          CALL FMADDI(MXY(8),-1)
          CALL FMLN(MXY(8),MXY(5))
          CALL FMMPY(MXY(7),MXY(5),MXY(4))
          CALL FMSUB(MXY(4),MXY(8),MXY(5))
          CALL FMEXP(MXY(5),MXY(10))
          IF ((MXY(10)%MP(2) == MEXPOV .AND. MXY(10)%MP(1) > 0) .OR.  &
              MXY(10)%MP(2) > MXSAVE+1) THEN
              CALL FMST2M('OVERFLOW',MXY(13))
              KFLAG = -5
              GO TO 180
          ENDIF
      ENDIF
      CALL FMI2M(1,MXY(5))
      IF (FMCOMP(MXY(1),'==',MXY(5)) .AND. MXY(2)%MP(2) >= 0) THEN
          IF (ABS(MXY(2)%MP(2)) < MEXPOV) THEN
              CALL FMEQ(MXY(2),MXY(6))
              IF (MXY(6)%MP(2) /= MUNKNO .AND. MXY(6)%MP(3) /= 0)  &
                  MXY(6)%MP(1) = -MXY(6)%MP(1)
              CALL FMEXP(MXY(6),MXY(7))
              CALL FMSUB(MXY(5),MXY(7),MXY(13))
              IF (MXY(13)%MP(2) /= MUNKNO) GO TO 200
          ENDIF
      ENDIF

!             Determine which method to use.

!             NMETHD = 1 means use the convergent series,
!                    = 2 means use the asymptotic series,
!                    = 3 means use the continued fraction expansion.

      CALL FMI2M(-10000,MXY(8))
      CALL FMI2M(10000,MXY(9))
      CALL FMABS(MXY(1),MXY(11))
      CALL FMABS(MXY(2),MXY(12))
      CALL FMSUB(MXY(12),MXY(11),MXY(10))

!             Check whether the smallest term in the asymptotic series is small enough to give
!             the required accuracy.

      IF (KFLAGA /= 0 .OR. KFLAGX /= 0) THEN
          CALL FMDIV(MXY(12),MXY(11),MXY(10))
          CALL FMI2M(1,MXY(8))
          CALL FMSUB(MXY(8),MXY(10),MXY(9))
          IF (MXY(9)%MP(1) < 0) THEN
              NMETHD = 2
          ELSE
              NMETHD = 1
          ENDIF
      ELSE
          T1 = FMDPLG(A)
          SMALL = T1 - FMDPLG(-ABS(X)) - (A+ABS(X))*LOG(ABS(X))
          TOL = -DBLE(NDIG+2)*DLOGMB - 12.0D0
          B = 1.0D0
          IF (A > ABS(X)) B = A - ABS(X)
          BIG = T1 - FMDPLG(A-B) - B*LOG(ABS(X))

          IF (FMCOMP(MXY(10),'<=',MXY(8))) THEN
              NMETHD = 1
          ELSE IF (FMCOMP(MXY(10),'>=',MXY(9)) .AND. MXY(1)%MP(1) > 0  &
                   .AND. MXY(2)%MP(1) > 0) THEN
              NMETHD = 3
              IF (SMALL < TOL+BIG) NMETHD = 2
          ELSE IF (FMCOMP(MXY(10),'>=',MXY(9))) THEN
              NMETHD = 3
          ELSE IF (MXY(1)%MP(1) > 0 .AND. MXY(2)%MP(1) > 0) THEN
              CALL FMDP2M(SQRT(DPMAX),MXY(8))
              IF (FMCOMP(MXY(2),'>=',MXY(8))) THEN
                  KFLAG = -5
                  CALL FMST2M('OVERFLOW',MXY(13))
                  GO TO 180
              ENDIF

              C2 = REAL(DBLE(NDSAVE)*DLOGMB)
              C1 = REAL(DBLE(C2)/10.0D0 + A + 10.0D0)
              C2 = REAL(MAX( 10.0D0 , DBLE(C2)/6.0D0 , A - 3.5D0*A/(SQRT(A)+1.0D0)))
              IF (X < C1) THEN
                  NMETHD = 1
              ELSE
                  NMETHD = 3
              ENDIF
              IF (X > C2) THEN
                  NMETHD = 3
                  IF (SMALL < TOL+BIG) NMETHD = 2
              ENDIF
          ELSE IF (MXY(1)%MP(1) < 0 .AND. MXY(2)%MP(1) > 0) THEN
              TLNB = REAL(NDIG)*ALOGMB
              C = 0.75/TLNB**0.35
              D = 0.80*TLNB**0.70
              IF (KFLAGA == 0 .AND. KFLAGX == 0) THEN
                  T = REAL(-A) - D/C
                  Y = D + C*T/2.0 + (C/2.0)*SQRT(T**2 + T + (2.0/C)**2)
                  IF (X > Y) THEN
                      NMETHD = 3
                  ELSE
                      NMETHD = 1
                  ENDIF
              ELSE
                  CALL FMDPM(DBLE(C),MXY(5))
                  CALL FMMPY(MXY(5),MXY(1),MXY(8))
                  MXY(8)%MP(1) = 1
                  IF (FMCOMP(MXY(2),'>',MXY(8))) THEN
                      NMETHD = 3
                  ELSE
                      NMETHD = 1
                  ENDIF
              ENDIF
          ELSE IF (MXY(1)%MP(1) > 0 .AND. MXY(2)%MP(1) < 0) THEN
              CALL FMDPM(DBLE(-0.8),MXY(5))
              CALL FMMPY(MXY(5),MXY(1),MXY(8))
              IF (FMCOMP(MXY(8),'<',MXY(2))) THEN
                  NMETHD = 1
              ELSE
                  NMETHD = 3
              ENDIF
          ENDIF
      ENDIF

      IF (NMETHD == 2) GO TO 150
      IF (NMETHD == 3) GO TO 170

!             Method 1.  Use the X**N/Pochhammer(A+1,N) series.

!             MXY(13) = MXY(18) + MXY(19) is the current sum.
!             MXY(9) is the current term.
!             MXY(8) is (A+N)/X.
!             MXY(14) is 1/X

!             Raise the precision if A is negative and near an integer, to compensate
!             for cancellation when (A+N)/X is near zero.

      IF (MXY(1)%MP(1) < 0) THEN
          CALL FMNINT(MXY(1),MXY(18))
          CALL FMSUB(MXY(1),MXY(18),MXY(14))
          IEXTRA = MAX(-INT(MXY(14)%MP(2)),0)
          IF (IEXTRA > 0) THEN
              CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
              CALL FMEQU_R1(MXY(2),NDIG,NDIG+IEXTRA)
          ENDIF
          NDOLD = NDIG
          NDIG = NDIG + IEXTRA
          N_ACC = NINT(NDIG*ALOGM2)
      ENDIF

      JEXTRA = 0

  140 CALL FMI2M(1,MXY(18))
      CALL FMI2M(0,MXY(19))
      CALL FMI2M(1,MXY(7))
      CALL FMADD(MXY(1),MXY(7),MXY(8))
      CALL FMDIV(MXY(2),MXY(8),MXY(9))
      CALL FMDIV_R1(MXY(8),MXY(2))
      CALL FMDIV(MXY(7),MXY(2),MXY(14))
      NDSAV1 = NDIG
      MAXE = 1

!             If A is negative and ABS(A) > ABS(X), the terms in the series first decrease,
!             then increase, then decrease.  Try to predict the number of extra digits required
!             to keep the precision from prematurely becoming too small.

      KFLGOK = 1
      IF (MXY(1)%MP(1) < 0) THEN
          IF (KFLAGA == 0) THEN
              IF (ABS(A) > 1.0D3) THEN
                  NMETHD = 3
                  GO TO 170
              ENDIF
          ELSE
              NMETHD = 3
              GO TO 170
          ENDIF
          KFLGOK = 0
          CALL FMABS(MXY(1),MXY(3))
          CALL FMABS(MXY(2),MXY(4))
          IF (FMCOMP(MXY(3),'>',MXY(4))) THEN
              IF (JEXTRA == 0) THEN
                  IF (KFLAGA == 0 .AND. KFLAGX == 0) THEN
                      T1 = FMDPLG(A+AINT(-ABS(X)-A)) - FMDPLG(A+1.0D0+AINT(ABS(X)-A))
                      T1 = (T1 + 2.0D0*ABS(X)*LOG(ABS(X)+1.0D-10))/DLOGMB
                      T1 = MAX(0.0D0,T1+1.0D0)
                      JEXTRA = T1
                  ENDIF
              ENDIF

!             If A is negative and ABS(A) is much bigger than ABS(X), the later increase in
!             the size of the terms can be ignored.

              IF (KFLAGA == 0 .AND. KFLAGX == 0) THEN
                  T1 = (AINT(X-A)*LOG(ABS(X)+1.0D-10) + FMDPLG(A+1.0D0)  &
                       - FMDPLG(A+1.0D0+AINT(X-A))) / DLOGMB
                  IF (T1 < -DBLE(NDIG)) KFLGOK = 1
              ELSE
                  KFLGOK = 1
              ENDIF
          ENDIF
      ENDIF

      NMNNDG = NDSAV1
      NMXDIF = 0

!             Method 1 summation loop.

      N_ACC = NINT(NDIG*ALOGM2)
      DO J = 1, NTERMS
         NDIG = NDSAV1
         MAXE = MAX(MAXE,MXY(9)%MP(2))
         IF (MXY(9)%MP(1) > 0) THEN
             CALL FMADD(MXY(18),MXY(9),MXY(17))
             INT_TEMP1 = KFLAG
             CALL FMEQ(MXY(17),MXY(18))
         ELSE
             CALL FMADD(MXY(19),MXY(9),MXY(17))
             INT_TEMP1 = KFLAG
             CALL FMEQ(MXY(17),MXY(19))
         ENDIF
         CALL FMADD(MXY(18),MXY(19),MXY(13))
         KFLAG = INT_TEMP1
         IF (KFLAG /= 0 .AND. (J > 2 .OR. ABS(MXY(18)%MP(2)) > MXEXP .OR.  &
             ABS(MXY(19)%MP(2)) > MXEXP)) THEN
             IF (KFLGOK == 0 .AND. KFLAGA == 0 .AND. KFLAGX == 0) THEN
                 IF (DBLE(J) > X-A) EXIT
             ELSE
                 EXIT
             ENDIF
         ENDIF

         CALL FMCSADD_R1(MXY(8),MXY(14))

         NDIG2 = MAX(NGRD22,NDSAV1-INT(MXY(13)%MP(2)-MXY(9)%MP(2)))
         NDIG = MIN(NDSAV1,NDIG2+JEXTRA)
         NMNNDG = MIN(NMNNDG,NDIG)
         NMXDIF = MAX(NMXDIF,NDIG-NMNNDG)
         CALL FMCSDIV(MXY(9),MXY(8),MXY(5))
         CALL FMEQ(MXY(5),MXY(9))
      ENDDO
      CALL FMADD(MXY(18),MXY(19),MXY(17))
      CALL FMCANCEL(MXY(18),MXY(19),MXY(17),K)
      N_ACC = N_ACC - K
      CALL FMEQ(MXY(17),MXY(13))

      NDIG = NDSAV1
      IF (NMXDIF > JEXTRA+1) THEN
          JEXTRA = NMXDIF
          GO TO 140
      ENDIF

      CALL FMABS(MXY(2),MXY(5))
      CALL FMLN(MXY(5),MXY(6))
      CALL FMMPY(MXY(1),MXY(6),MXY(4))
      CALL FMSUB(MXY(4),MXY(2),MXY(14))
      CALL FMEXP(MXY(14),MXY(15))
      IF (MXY(15)%MP(2) == MUNKNO) THEN
          CALL FMPWR(MXY(5),MXY(1),MXY(4))
          CALL FMEXP(MXY(2),MXY(6))
          CALL FMDIV(MXY(4),MXY(6),MXY(15))
      ENDIF
      CALL FMDIV(MXY(13),MXY(1),MXY(12))
      CALL FMMPY(MXY(15),MXY(12),MXY(11))
      IF (MXY(11)%MP(2) == MUNKNO) THEN
          CALL FMLN(MXY(13),MXY(5))
          CALL FMLN(MXY(1),MXY(6))
          CALL FMADD(MXY(14),MXY(5),MXY(4))
          CALL FMSUB(MXY(4),MXY(6),MXY(14))
          CALL FMEXP(MXY(14),MXY(13))
      ELSE
          CALL FMEQ(MXY(11),MXY(13))
      ENDIF
      IF (KXNEG == 1 .AND. MODA2 == 1 .AND. MXY(13)%MP(2) /= MUNKNO  &
          .AND. MXY(13)%MP(3) /= 0) THEN
          MXY(13)%MP(1) = -MXY(13)%MP(1)
      ENDIF

      GO TO 180

!             Method 2.  Use the Pochhammer(A-N,N)/X**N series.

!             MXY(13) = MXY(18) + MXY(19) is the current sum.
!             MXY(9) is the current term.
!             MXY(8) is (A-N)/X.
!             MXY(14) is -1/X

!             Raise the precision if A is positive and near an integer, to compensate for
!             cancellation when (A-N)/X is near zero.

  150 IF (MXY(1)%MP(1) > 0) THEN
          CALL FMNINT(MXY(1),MXY(13))
          CALL FMSUB(MXY(1),MXY(13),MXY(14))
          IEXTRA = MAX(-INT(MXY(14)%MP(2)),0)
          IF (IEXTRA > 0) THEN
              CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
              CALL FMEQU_R1(MXY(2),NDIG,NDIG+IEXTRA)
          ENDIF
          NDOLD = NDIG
          NDIG = NDIG + IEXTRA
      ENDIF
      N_ACC = NINT(NDIG*ALOGM2)

      CALL FMGAM(MXY(1),MXY(15))
      IF (KFLAGA == 0 .AND. KFLAGX == 0) THEN
          NT = INT(((A-1)*LOG(ABS(X)+1.0D-10) - X)/DLOGMB)
          LESS = MAX(0,INT(MXY(15)%MP(2)) - NT - 1)
          IF (LESS > NDIG .AND. ABS(A) < ABS(X)) THEN
              CALL FMEQ(MXY(15),MXY(13))
              GO TO 180
          ENDIF
      ENDIF
      IF (KFLAG /= 0) THEN
          CALL FMEQ(MXY(15),MXY(13))
          GO TO 180
      ENDIF
      IF (KXNEG == 0) THEN
          CALL FMLN(MXY(2),MXY(14))
          CALL FMMPY(MXY(1),MXY(14),MXY(5))
          CALL FMSUB(MXY(5),MXY(2),MXY(13))
          CALL FMSUB_R2(MXY(13),MXY(14))
          CALL FMEXP(MXY(14),MXY(9))
      ELSE
          CALL FMI2M(1,MXY(5))
          CALL FMSUB(MXY(1),MXY(5),MXY(13))
          CALL FMPWR(MXY(2),MXY(13),MXY(14))
          CALL FMEXP(MXY(2),MXY(12))
          CALL FMDIV(MXY(14),MXY(12),MXY(9))
      ENDIF

!             Here MXY(9) is X**(A-1)/EXP(X).

      IF (MXY(9)%MP(2) /= MUNKNO .AND. MXY(9)%MP(3) /= 0)  &
          MXY(9)%MP(1) = -MXY(9)%MP(1)
      CALL FMEQ(MXY(15),MXY(13))
      IF (MXY(15)%MP(1) > 0) THEN
          CALL FMEQ(MXY(15),MXY(18))
          CALL FMI2M(0,MXY(19))
      ELSE
          CALL FMEQ(MXY(15),MXY(19))
          CALL FMI2M(0,MXY(18))
      ENDIF
      CALL FMDIV(MXY(1),MXY(2),MXY(8))
      CALL FMI2M(1,MXY(5))
      CALL FMDIV(MXY(5),MXY(2),MXY(14))
      IF (MXY(14)%MP(2) /= MUNKNO .AND. MXY(14)%MP(3) /= 0)  &
          MXY(14)%MP(1) = -MXY(14)%MP(1)
      NDSAV1 = NDIG

!             Disable NDIG reduction until the terms in the sum begin to decrease in size.

      BIGJ = 0
      IF (KFLAGA == 0 .AND. KFLAGX == 0) BIGJ = ABS(A) - ABS(X)
      JTERMS = NTERMS
      IF (KFLAGI == 0 .AND. INTA > 0) THEN
          JTERMS = INTA
      ELSE IF (KFLAGA == 0 .AND. KFLAGX == 0) THEN
          T1 = A + ABS(X)
          IF (T1 > 0 .AND. T1 < DBLE(NTERMS)) JTERMS = INT(T1) + 2
      ENDIF

!             Method 2 summation loop.

      N_ACC = NINT(NDIG*ALOGM2)
      DO J = 1, JTERMS
         NDIG = NDSAV1
         CALL FMCSADD_R1(MXY(13),MXY(9))
         IF (MXY(9)%MP(1) > 0) THEN
             CALL FMADD_R2(MXY(9),MXY(18))
         ELSE
             CALL FMADD_R2(MXY(9),MXY(19))
         ENDIF
         K = MAX(MXY(18)%MP(2),MXY(19)%MP(2)) - NDIG
         IF (MXY(9)%MP(2) < K) KFLAG = 1
         IF (KFLAG /= 0 .AND. J > 1) GO TO 160
         CALL FMCSADD_R1(MXY(8),MXY(14))
         IF (REAL(J) >= BIGJ) THEN
             CALL FMADD(MXY(18),MXY(19),MXY(13))
             NDIG2 = MAX(NGRD22,NDSAV1-INT(MXY(13)%MP(2)-MXY(9)%MP(2)))
             NDIG = MIN(NDSAV1,NDIG2)
         ENDIF
         CALL FMCSMPY_R1(MXY(9),MXY(8))
      ENDDO

  160 NDIG = NDSAV1
      CALL FMADD(MXY(18),MXY(19),MXY(13))
      CALL FMCANCEL(MXY(18),MXY(19),MXY(13),K)
      N_ACC = N_ACC - K
      GO TO 180

!             Method 3.  Use the continued fraction expansion.

!             MXY(14) = MXY(18) + MXY(19) is the current approximation.
!             MXY(13) is the term in the sum, S(k).
!             MXY(11), MXY(12) are the latest denominators, Q(k-1) and Q(k).

  170 CALL FMGAM(MXY(1),MXY(15))
      N_ACC = NINT(NDIG*ALOGM2)
      NDSAV1 = NDIG
      IF (KFLAGA == 0 .AND. KFLAGX == 0) THEN
          YT = MIN(DBLE(HUGE(1))/11,AINT(((A-1)*LOG(ABS(X)+1.0D-10) - X)/DLOGMB))
          NT = HUGE(1)/10
          NT = MIN(DBLE(NT),YT)
          LESS = MAX(0,INT(MXY(15)%MP(2)) - NT - 1)
          IF (LESS > NDIG) THEN
              CALL FMEQ(MXY(15),MXY(13))
              GO TO 180
          ENDIF
          NDIG = MIN(NDSAV1,MAX(NGRD22,NDIG-LESS))
      ENDIF
      JEXTRA = INT(MAX(1.0,5.76/ALOGMB + 1.0))
      IF (NDIG+JEXTRA > NDSAV1) THEN
          CALL FMEQU_R1(MXY(1),NDSAV1,NDSAV1+JEXTRA)
          CALL FMEQU_R1(MXY(2),NDSAV1,NDSAV1+JEXTRA)
      ENDIF
      NDIG = NDIG + JEXTRA
      CALL FMEQU(MXY(1),MXY(8),NDSAV1,NDIG)
      IF (MXY(8)%MP(2) /= MUNKNO .AND. MXY(8)%MP(3) /= 0)  &
          MXY(8)%MP(1) = -MXY(8)%MP(1)
      CALL FMI2M(1,MXY(11))
      CALL FMEQU(MXY(2),MXY(12),NDSAV1,NDIG)
      CALL FMI2M(1,MXY(5))
      CALL FMDIV(MXY(5),MXY(12),MXY(13))
      IF (MXY(13)%MP(1) > 0) THEN
          CALL FMEQ(MXY(13),MXY(18))
          CALL FMI2M(0,MXY(19))
      ELSE
          CALL FMEQ(MXY(13),MXY(19))
          CALL FMI2M(0,MXY(18))
      ENDIF
      N_ACC = NINT(NDIG*ALOGM2)

!             Method 3 continued fraction loop.

      NDSAV2 = NDIG
      N_ACC = NINT(NDIG*ALOGM2)
      DO J = 1, MIN(NTERMS,INTA-1)
         CALL FMADDI(MXY(8),1)
         CALL FMEQ(MXY(8),MXY(10))
         CALL FMCSMPY_R1(MXY(10),MXY(11))
         CALL FMADD(MXY(12),MXY(10),MXY(9))
         CALL FMEQ(MXY(10),MXY(6))
         CALL FMCSMPY_R1(MXY(6),MXY(13))
         CALL FMCSDIV(MXY(6),MXY(9),MXY(13))
         IF (MXY(13)%MP(2) /= MUNKNO .AND. MXY(13)%MP(3) /= 0)  &
             MXY(13)%MP(1) = -MXY(13)%MP(1)
         NDIG = NDSAV2
         IF (MXY(13)%MP(1) > 0) THEN
             CALL FMADD_R1(MXY(18),MXY(13))
         ELSE
             CALL FMADD_R1(MXY(19),MXY(13))
         ENDIF
         KFLAG1 = KFLAG
         CALL FMADD(MXY(18),MXY(19),MXY(14))
         CALL FMEQ(MXY(12),MXY(11))
         CALL FMEQ(MXY(9),MXY(12))
         NDIG = MIN(NDSAV2,MAX(NGRD22,NDSAV2-INT(MXY(14)%MP(2)-MXY(13)%MP(2))))
         CALL FMCSMPYI(MXY(11),J,MXY(10))
         CALL FMEQ(MXY(2),MXY(6))
         CALL FMCSMPY_R1(MXY(6),MXY(12))
         CALL FMADD(MXY(6),MXY(10),MXY(9))
         CALL FMEQ(MXY(10),MXY(6))
         CALL FMCSMPY_R1(MXY(6),MXY(13))
         CALL FMCSDIV(MXY(6),MXY(9),MXY(13))
         IF (MXY(13)%MP(2) /= MUNKNO .AND. MXY(13)%MP(3) /= 0)  &
             MXY(13)%MP(1) = -MXY(13)%MP(1)
         NDIG = NDSAV2
         IF (MXY(13)%MP(1) > 0) THEN
             CALL FMADD_R1(MXY(18),MXY(13))
         ELSE
             CALL FMADD_R1(MXY(19),MXY(13))
         ENDIF

!             Check for convergence.

         IF (KFLAG1 == 1 .AND. KFLAG == 1 .AND. J > 2) THEN
             EXIT
         ENDIF
         CALL FMADD(MXY(18),MXY(19),MXY(14))
         CALL FMEQ(MXY(12),MXY(11))
         CALL FMEQ(MXY(9),MXY(12))
         NDIG = MIN(NDSAV2,MAX(NGRD22,NDSAV2-INT(MXY(14)%MP(2)-MXY(13)%MP(2))))
      ENDDO

      NDIG = NDSAV2
      CALL FMEQU_R1(MXY(14),NDIG,NDSAV1)
      CALL FMEQU_R1(MXY(18),NDIG,NDSAV1)
      CALL FMEQU_R1(MXY(19),NDIG,NDSAV1)
      NDIG = NDSAV1
      IF (MXY(2)%MP(1) > 0) THEN
          CALL FMLN(MXY(2),MXY(5))
          CALL FMMPY(MXY(1),MXY(5),MXY(4))
          CALL FMSUB(MXY(4),MXY(2),MXY(5))
          CALL FMEXP(MXY(5),MXY(12))
      ELSE IF (KFLAGI == 0) THEN
          CALL FMEXP(MXY(2),MXY(13))
          CALL FMIPWR(MXY(2),INTA,MXY(5))
          CALL FMDIV(MXY(5),MXY(13),MXY(12))
      ELSE
          CALL FMABS(MXY(2),MXY(5))
          CALL FMLN(MXY(5),MXY(6))
          CALL FMMPY(MXY(1),MXY(6),MXY(4))
          CALL FMSUB(MXY(4),MXY(2),MXY(5))
          CALL FMEXP(MXY(5),MXY(12))
          IF (MODA2 == 1 .AND. MXY(12)%MP(2) /= MUNKNO .AND.  &
              MXY(12)%MP(3) /= 0) MXY(12)%MP(1) = -MXY(12)%MP(1)
      ENDIF

      IF (MXY(12)%MP(2) /= MEXPOV) THEN
          CALL FMMPY_R2(MXY(12),MXY(18))
          CALL FMMPY_R2(MXY(12),MXY(19))
      ELSE IF (MXY(12)%MP(2)+MXY(14)%MP(2) >= MXEXP2/2) THEN
          CALL FMEQ(MXY(12),MXY(13))
          IF (MXY(14)%MP(1) < 0 .AND. MXY(13)%MP(2) /= MUNKNO .AND.  &
              MXY(13)%MP(3) /= 0) MXY(13)%MP(1) = -MXY(13)%MP(1)
      ELSE
          CALL FMMPY_R2(MXY(12),MXY(18))
          CALL FMMPY_R2(MXY(12),MXY(19))
      ENDIF
      N_ACC = NINT(NDIG*ALOGM2)
      IF (MXY(15)%MP(1) > 0) THEN
          CALL FMSUB(MXY(15),MXY(19),MXY(17))
          CALL FMMPYI(MXY(18),-1,MXY(19))
          CALL FMEQ(MXY(17),MXY(18))
      ELSE
          CALL FMSUB(MXY(15),MXY(18),MXY(17))
          CALL FMMPYI(MXY(19),-1,MXY(18))
          CALL FMEQ(MXY(17),MXY(19))
      ENDIF
      CALL FMADD(MXY(18),MXY(19),MXY(13))
      CALL FMCANCEL(MXY(18),MXY(19),MXY(13),K)
      N_ACC = N_ACC - K

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  180 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(13)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 120
      ENDIF

!             Check for too much cancellation.

      IF (NCALL >= 1) THEN
          NGOAL = INT(REAL(NDSAVE)*ALOGM2) + 17
      ELSE
          NGOAL = INT(-MXEXP2)
      ENDIF
      IF (N_ACC <= NGOAL) THEN
          IF (NUMTRY > 0) THEN
              NDGOAL = INT(REAL(NGOAL)/ALOGM2 + 1.0)
              DO J = 1, NDGOAL+1
                 IF (MRETRY%MP(J+1) /= MXY(13)%MP(J+1)) GO TO 190
              ENDDO
              GO TO 200
          ENDIF
  190     IEXTRA = INT(REAL(NGOAL-N_ACC)/ALOGM2 + 23.03/ALOGMB) + 1
          NDOLD = NDIG
          NDIG = NDIG + IEXTRA
          CALL FMEQU_R1(MXY(1),NDSAVE,NDIG)
          CALL FMEQU_R1(MXY(2),NDSAVE,NDIG)
          NUMTRY = NUMTRY + 1
          CALL FMEQU(MXY(13),MRETRY,NDOLD,NDIG)
          GO TO 130
      ENDIF

  200 CALL FMEXT2(MXY(13),MC,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMIGM1

      SUBROUTINE FMIGM2(MA,MB,MC)

!  MC = Incomplete Gamma(MA,MB)

!  Integral from MB to infinity of e**(-t) * t**(MA-1)  dt.

!  This is (upper case) Gamma(a,x).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC
      DOUBLE PRECISION :: X,A,B,ERR,SMALL,BIG,TOL,T1,T2,BIGJ,C1,C2
      REAL (KIND(1.0D0)) :: MAS,MAXM09,MBS,MODA2,MXSAVE
      INTEGER :: IEXTRA,INTA,INTG,J,JEXTRA,JR,JTERMS,K,KABIGR,KFLAG1,KFLAGA,KFLAGI,KFLAGX,         &
                 KFLGOK,KL,KMETH4,KOVUN,KR_RETRY,KRESLT,KRSAVE,KWRNSV,KXNEG,N,NDGOAL,NDIG2,NDOLD,  &
                 NDSAV1,NDSAV2,NDSAVE,NGOAL,NMETHD,NMNNDG,NTERMS,NUMTRY,N_ACC
      LOGICAL, EXTERNAL :: FMCOMP
      DOUBLE PRECISION, EXTERNAL :: FMDPLG
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      TYPE(MULTI) :: MXY(22),MRETRY

      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < NDIG+2) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      N_ACC = NINT(NDIG*ALOGM2)

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1 .AND. MB%MP(1) > 0 .AND. MA%MP(3) /= 0 .AND.   &
          MB%MP(2) < -NDIG - NGRD52) THEN
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          NDSAVE = NDIG
          KRSAVE = KROUND
          KROUND = 1
          NDIG = NDIG + NGRD52
          N_ACC = NINT(NDIG*ALOGM2)
          CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
          CALL FMEQU(MB,MXY(2),NDSAVE,NDIG)
          CALL FMGAM(MXY(1),MXY(3))
          IF (MXY(3)%MP(2) /= MUNKNO) THEN
              CALL FMIGM1(MXY(1),MXY(2),MXY(4))
              KROUND = KRSAVE
              CALL FMSUB(MXY(3),MXY(4),MXY(5))
              IF (MXY(5)%MP(3) == 0 .OR.  &
                  MXY(5)%MP(2) < MXY(3)%MP(2)-NGRD52) THEN
                  KFLAG = 0
                  NTRACE = J
                  KWARN = K
                  NDIG = NDSAVE
                  GO TO 110
              ENDIF
          ELSE
              CALL FMM2I(MXY(1),KL)
              CALL FMIPWR(MXY(2),KL,MXY(4))
              IF (MXY(4)%MP(2) /= MEXPOV) THEN
                  CALL FMDIVI_R1(MXY(4),-KL)
              ENDIF
              CALL FMTINY(MXY(5))
              KROUND = KRSAVE
              CALL FMSUB_R2(MXY(4),MXY(5))
          ENDIF
          KFLAG = 0
          NTRACE = J
          KWARN = K
          CALL FMEQU(MXY(5),MC,NDIG,NDSAVE)
          NDIG = NDSAVE
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMIGM2'
              CALL FMNTR(2,MA,MB,2,1)
              NCALL = NCALL - 1
          ENDIF
          IF (MC%MP(2) == MUNKNO .AND. MA%MP(2) /= MUNKNO .AND.  &
              MB%MP(2) /= MUNKNO) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMIGM2'
              KFLAG = -4
              CALL FMWRN2
              NCALL = NCALL - 1
          ELSE IF (ABS(MC%MP(2)) == MEXPOV .AND. ABS(MA%MP(2)) < MEXPOV .AND.  &
                   ABS(MB%MP(2))  < MEXPOV) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMIGM2'
              IF (MC%MP(2) == MEXPOV) KFLAG = -5
              IF (MC%MP(2) == MEXPUN) KFLAG = -6
              CALL FMWRN2
              NCALL = NCALL - 1
          ENDIF
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'FMIGM2'
              CALL FMNTR(1,MC,MC,1,1)
              NCALL = NCALL - 1
          ENDIF
          RETURN
      ENDIF

  110 CALL FMENT2('FMIGM2   ',MA,MB,2,1,MC,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF

      MAS = MA%MP(1)
      MBS = MB%MP(1)
      KR_RETRY = 0

  120 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
          N_ACC = NINT(NDIG*ALOGM2)
      ENDIF
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
      CALL FMEQU(MB,MXY(2),NDSAVE,NDIG)
      KMETH4 = 0
      NUMTRY = 0

  130 NTERMS = INT(INTMAX/10)

!             A,X are double precision approximations to the two arguments to this function.
!             INTA = A if A is a small integer.  It is used to limit the number of terms used
!                    in the asymptotic series and in the continued fraction expansion.

      INTA = NTERMS
      KWRNSV = KWARN
      KWARN = 0
      CALL FMM2I(MXY(1),INTG)
      KFLAGI = KFLAG
      IF (KFLAG == 0) INTA = INTG
      CALL FMM2DP(MXY(1),A)
      KFLAGA = KFLAG
      IF (KFLAG /= 0 .AND. MXY(1)%MP(2) < 0) THEN
          A = 1.0D0/DPMAX
          IF (MXY(1)%MP(1) < 0) A = -A
          KFLAGA = 0
      ENDIF
      CALL FMM2DP(MXY(2),X)
      KFLAGX = KFLAG
      IF (KFLAG /= 0 .AND. MXY(2)%MP(2) < 0) THEN
          X = 1.0D0/DPMAX
          IF (MXY(2)%MP(1) < 0) X = -X
          KFLAGX = 0
      ENDIF
      KWARN = KWRNSV

!             If A or X is large in magnitude use more guard digits.

      IEXTRA = MIN(MAX(INT(MXY(1)%MP(2)),INT(MXY(2)%MP(2)),0) ,  &
                   INT(1.0+ALOGMX/ALOGMB))
      IF (IEXTRA > 0) THEN
          CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
          CALL FMEQU_R1(MXY(2),NDIG,NDIG+IEXTRA)
      ENDIF
      NDIG = NDIG + IEXTRA
      N_ACC = NINT(NDIG*ALOGM2)

!             KXNEG = 1 if X is negative and A is a positive integer.

      KXNEG = 0

!             MODA2 = MOD(A,2) when KXNEG is 1.

      MODA2 = 0

!             Check for special cases.

      IF (MXY(1)%MP(2) == MEXPOV .OR. MXY(2)%MP(2) == MEXPOV) THEN
          IF (MXY(1)%MP(2) == MEXPOV .AND. MXY(1)%MP(3) /= 0 .AND.  &
              MXY(1)%MP(1) > 0) THEN
              IF (MXY(2)%MP(2) /= MEXPOV) THEN
                  CALL FMST2M('OVERFLOW',MXY(16))
                  KFLAG = -5
                  GO TO 180
              ENDIF
          ENDIF
          IF (MXY(2)%MP(2) == MEXPOV .AND. MXY(2)%MP(3) /= 0 .AND.  &
              MXY(2)%MP(1) > 0) THEN
              CALL FMBIG(MXY(17))
              MXY(17)%MP(2) = MXSAVE + 1
              CALL FMLN(MXY(17),MXY(7))
              CALL FMDIV(MXY(17),MXY(7),MXY(18))
              IF (FMCOMP(MXY(1),'<=',MXY(18))) THEN
                  CALL FMST2M('UNDERFLOW',MXY(16))
                  KFLAG = -6
                  GO TO 180
              ELSE
                  CALL FMST2M('UNKNOWN',MXY(16))
                  KFLAG = -4
                  GO TO 200
              ENDIF
          ENDIF
          IF (MXY(2)%MP(2) == MEXPOV .AND. MXY(2)%MP(1) < 0 .AND.  &
              MXY(1)%MP(1) > 0 .AND. MXY(1)%MP(3) /= 0) THEN
              IF (MXY(1)%MP(2) /= MEXPOV) THEN
                  CALL FMINT(MXY(1),MXY(15))
                  IF (FMCOMP(MXY(1),'==',MXY(15))) THEN
                      CALL FMI2M(2,MXY(12))
                      CALL FMMOD(MXY(15),MXY(12),MXY(7))
                      CALL FMEQ(MXY(7),MXY(12))
                      IF (MXY(12)%MP(3) /= 0) THEN
                          CALL FMST2M('OVERFLOW',MXY(16))
                          KFLAG = -5
                          GO TO 180
                      ELSE
                          CALL FMST2M('-OVERFLOW',MXY(16))
                          KFLAG = -5
                          GO TO 180
                      ENDIF
                  ENDIF
              ENDIF
          ENDIF
          IF (MXY(1)%MP(2) == MEXPOV .AND. MXY(1)%MP(1) < 0 .AND.  &
              MXY(1)%MP(3) /= 0) THEN
              IF (MXY(2)%MP(2) /= MEXPOV .AND. MXY(2)%MP(1) > 0 .AND.  &
                  MXY(2)%MP(3) /= 0) THEN
                  CALL FMI2M(1,MXY(7))
                  IF (FMCOMP(MXY(2),'<',MXY(7))) THEN
                      CALL FMST2M('OVERFLOW',MXY(16))
                      KFLAG = -5
                      GO TO 180
                  ELSE
                      CALL FMST2M('UNDERFLOW',MXY(16))
                      KFLAG = -6
                      GO TO 180
                  ENDIF
              ENDIF
          ENDIF
          CALL FMST2M('UNKNOWN',MXY(16))
          KFLAG = -4
          GO TO 200
      ENDIF

      IF (MXY(1)%MP(2) == MEXPUN .OR. MXY(2)%MP(2) == MEXPUN) THEN
          IF (MXY(1)%MP(2) == MEXPUN .AND. MXY(2)%MP(2) == MEXPUN) THEN
              CALL FMST2M('UNKNOWN',MXY(16))
              KFLAG = -4
              GO TO 200
          ENDIF
          IF (MXY(2)%MP(2) == MEXPUN .AND. MXY(2)%MP(1) > 0 .AND.  &
              MXY(2)%MP(3) /= 0                                  ) THEN
              IF (MXY(1)%MP(2) >= 1 .AND. MXY(1)%MP(1) > 0) THEN
                  CALL FMGAM(MXY(1),MXY(20))
                  CALL FMEQ(MXY(20),MXY(16))
                  GO TO 180
              ELSE IF (MXY(1)%MP(2) >= 1 .AND. MXY(1)%MP(1) < 0) THEN
                  CALL FMST2M('OVERFLOW',MXY(16))
                  KFLAG = -5
                  GO TO 200
              ELSE
                  CALL FMST2M('UNKNOWN',MXY(16))
                  KFLAG = -4
                  GO TO 200
              ENDIF
          ENDIF
      ENDIF

      IF (MXY(2)%MP(3) == 0) THEN
          IF (MXY(1)%MP(1) < 0 .OR. MXY(1)%MP(3) == 0) THEN
              CALL FMST2M('UNKNOWN',MXY(16))
              KFLAG = -4
              GO TO 200
          ELSE
              CALL FMGAM(MXY(1),MXY(20))
              CALL FMEQ(MXY(20),MXY(16))
              GO TO 180
          ENDIF
      ENDIF
      IF (MXY(2)%MP(1) < 0) THEN
          CALL FMINT(MXY(1),MXY(15))
          IF (FMCOMP(MXY(1),'==',MXY(15)) .AND. MXY(1)%MP(1)*MXY(1)%MP(3) > 0) THEN
              KXNEG = 1
              CALL FMI2M(2,MXY(12))
              CALL FMMOD(MXY(15),MXY(12),MXY(7))
              CALL FMEQ(MXY(7),MXY(12))
              IF (MXY(12)%MP(3) /= 0) MODA2 = 1
          ELSE
              CALL FMST2M('UNKNOWN',MXY(16))
              KFLAG = -4
              GO TO 200
          ENDIF
      ENDIF
      IF (MXY(2)%MP(2) == MEXPUN) THEN
          IF (MXY(1)%MP(1) < 0 .AND. MXY(1)%MP(2) >= 1) THEN
              CALL FMST2M('OVERFLOW',MXY(16))
              KFLAG = -5
          ELSE
              CALL FMGAM(MXY(1),MXY(20))
              CALL FMEQ(MXY(20),MXY(16))
          ENDIF
          GO TO 180
      ENDIF
      IF (MXY(1)%MP(2) == MEXPUN) THEN
          CALL FMI2M(0,MXY(1))
          MAS = 1
      ENDIF
      CALL FMMAX(MXY(1),MXY(2),MXY(7))
      CALL FMMIN(MXY(1),MXY(2),MXY(8))
      CALL FMDPM(1.0D6,MXY(3))
      CALL FMDPM(1.0D2,MXY(4))
      IF (FMCOMP(MXY(7),'>=',MXY(3)) .AND. FMCOMP(MXY(8),'>=',MXY(4))) THEN
          CALL FMI2M(1,MXY(7))
          CALL FMSUB(MXY(1),MXY(7),MXY(9))
          CALL FMMAX(MXY(9),MXY(2),MXY(11))
          CALL FMADDI(MXY(11),1)
          CALL FMLN(MXY(11),MXY(7))
          CALL FMMPY(MXY(9),MXY(7),MXY(4))
          CALL FMSUB(MXY(4),MXY(11),MXY(7))
          CALL FMEXP(MXY(7),MXY(13))
          IF ((MXY(13)%MP(2) == MEXPOV .AND. MXY(13)%MP(1) > 0 .AND.  &
              MXY(13)%MP(3) /= 0) .OR. MXY(13)%MP(2) > MXSAVE+1) THEN
              CALL FMST2M('OVERFLOW',MXY(16))
              KFLAG = -5
              GO TO 180
          ENDIF
      ENDIF

!             If A is 1 the result is exp(-X).

      CALL FMI2M(1,MXY(7))
      IF (FMCOMP(MXY(1),'==',MXY(7))) THEN
          IF (ABS(MXY(2)%MP(2)) < MEXPOV) THEN
              CALL FMEQ(MXY(2),MXY(8))
              IF (MXY(8)%MP(2) /= MUNKNO .AND. MXY(8)%MP(3) /= 0)  &
                  MXY(8)%MP(1) = -MXY(8)%MP(1)
              CALL FMEXP(MXY(8),MXY(16))
              IF (MXY(16)%MP(2) /= MUNKNO) GO TO 200
          ENDIF
      ENDIF

!             If A is negative and X is small, the result is -X**A / A.

      IF (MXY(1)%MP(1) == -1 .AND. MXY(2)%MP(2) < -NDIG) THEN
          CALL FMPWR(MXY(2),MXY(1),MXY(8))
          CALL FMDIV(MXY(8),MXY(1),MXY(16))
          IF (MXY(16)%MP(2) == MUNKNO) THEN
              CALL FMLN(MXY(2),MXY(8))
              CALL FMMPY(MXY(8),MXY(1),MXY(9))
              CALL FMABS(MXY(1),MXY(10))
              CALL FMLN(MXY(10),MXY(11))
              CALL FMSUB(MXY(9),MXY(11),MXY(10))
              CALL FMEXP(MXY(10),MXY(16))
              IF (MXY(16)%MP(2) /= MUNKNO .AND. MXY(16)%MP(3) /= 0)  &
                  MXY(16)%MP(1) = -MXY(16)%MP(1)
          ENDIF
          IF (MXY(16)%MP(2) /= MUNKNO .AND. MXY(16)%MP(3) /= 0)  &
              MXY(16)%MP(1) = -MXY(16)%MP(1)
          IF (MXY(16)%MP(2) /= MUNKNO) GO TO 200
      ENDIF
      IF (MXY(1)%MP(1) == -1 .AND. MXY(2)%MP(1) == 1 .AND.  &
          MXY(1)%MP(2) > NDIG .AND.                         &
          MXY(1)%MP(2) - MXY(2)%MP(2) > NDIG) THEN
          CALL FMI2M(1,MXY(3))
          CALL FMSUB(MXY(3),MXY(1),MXY(4))
          CALL FMADD(MXY(4),MXY(2),MXY(6))
          CALL FMDIV(MXY(3),MXY(6),MXY(5))
          CALL FMSQR(MXY(1),MXY(4))
          CALL FMMPYI(MXY(2),2,MXY(6))
          CALL FMADDI(MXY(6),3)
          CALL FMMPY(MXY(1),MXY(6),MXY(7))
          CALL FMSUB(MXY(4),MXY(7),MXY(8))
          CALL FMDIV(MXY(5),MXY(8),MXY(9))
          CALL FMSUB(MXY(1),MXY(3),MXY(10))
          CALL FMMPY(MXY(10),MXY(9),MXY(11))
          IF (MXY(11)%MP(2) /= MUNKNO) THEN
              CALL FMADD(MXY(5),MXY(11),MXY(12))
          ELSE
              CALL FMEQ(MXY(5),MXY(12))
          ENDIF
          CALL FMPWR(MXY(2),MXY(1),MXY(13))
          IF (MXY(1)%MP(2) == MEXPOV .OR. MXY(1)%MP(2) == MEXPUN) THEN
              CALL FMEQ(MXY(13),MXY(16))
          ELSE
              CALL FMEXP(MXY(2),MXY(14))
              CALL FMMPY(MXY(12),MXY(13),MXY(15))
              CALL FMDIV(MXY(15),MXY(14),MXY(16))
          ENDIF
          IF (MXY(16)%MP(2) /= MUNKNO) GO TO 200
      ENDIF

!             Determine which method to use.

!             NMETHD = 1 means use the convergent series,
!                    = 2 means use the asymptotic series,
!                    = 3 means use the continued fraction expansion,
!                    = 4 means use an O(A**2) formula.

      CALL FMI2M(-10000,MXY(11))
      CALL FMI2M(10000,MXY(12))
      CALL FMABS(MXY(1),MXY(14))
      CALL FMABS(MXY(2),MXY(15))
      CALL FMSUB(MXY(15),MXY(14),MXY(13))
      KABIGR = 1
      IF (MXY(13)%MP(3) >= 0 .AND. MXY(13)%MP(1) > 0) KABIGR = 0

!             Check whether the smallest term in the asymptotic series is small enough to give
!             the required accuracy.

      IF (KFLAGA /= 0 .OR. KFLAGX /= 0) THEN
          CALL FMDIV(MXY(14),MXY(15),MXY(10))
          CALL FMI2M(1,MXY(8))
          CALL FMSUB(MXY(8),MXY(10),MXY(9))
          IF (MXY(9)%MP(1) < 0) THEN
              NMETHD = 1
          ELSE
              NMETHD = 2
          ENDIF
      ELSE
          T1 = FMDPLG(A)
          SMALL = T1 - FMDPLG(-ABS(X)) - (A+ABS(X))*LOG(ABS(X))
          TOL = -DBLE(NDIG+2)*DLOGMB - 12.0D0
          B = 1.0D0
          IF (A > ABS(X)) B = A - ABS(X)
          BIG = T1 - FMDPLG(A-B) - B*LOG(ABS(X))

          NMETHD = 0
          IF (FMCOMP(MXY(13),'<=',MXY(11))) THEN
              IF (MXY(1)%MP(1) > 0 .AND. MXY(1)%MP(3) /= 0) THEN
                  NMETHD = 1
              ELSE
                  NMETHD = 3
              ENDIF
          ELSE IF (FMCOMP(MXY(13),'>=',MXY(12)) .AND. MXY(1)%MP(1) > 0 .AND.  &
                   MXY(1)%MP(3) > 0 .AND. MXY(2)%MP(1) > 0 .AND.              &
                   MXY(2)%MP(3) > 0) THEN
              NMETHD = 3
              IF (SMALL < TOL+BIG) NMETHD = 2
          ELSE IF (FMCOMP(MXY(13),'>=',MXY(12))) THEN
              NMETHD = 3
          ELSE IF (MXY(1)%MP(1) > 0 .AND. MXY(2)%MP(1) > 0 .AND.  &
                   MXY(2)%MP(3) > 0) THEN
              CALL FMDP2M(SQRT(DPMAX),MXY(11))
              IF (FMCOMP(MXY(2),'>=',MXY(11))) THEN
                  KFLAG = -5
                  CALL FMST2M('OVERFLOW',MXY(16))
                  GO TO 180
              ENDIF

              IF (MXY(1)%MP(1) > 0 .AND. MXY(1)%MP(3) /= 0) THEN
                  C2 = DBLE(NDSAVE)*DLOGMB/6.0D0
                  C1 = MAX( 10.0D0 , C2 , A )
                  C2 = MAX( 10.0D0 , C2 , A - 6.5D0*A/(SQRT(A)+1.0D0) )
              ELSE
                  C1 = MAX( 15.0D0 , DBLE(NDSAVE)*DLOGMB/5.0D0 )
                  C2 = C1
              ENDIF
              IF (X < MIN(C1,C2)) THEN
                  IF (-2*MXY(1)%MP(2) > NDIG .OR. MXY(1)%MP(3) == 0) THEN
                      NMETHD = 4
                  ELSE
                      NMETHD = 1
                  ENDIF
              ELSE IF (X > C2) THEN
                  IF (SMALL < TOL+BIG) NMETHD = 2
              ENDIF
              IF (NMETHD == 0 .AND. X > C1) NMETHD = 3
              IF (NMETHD == 0) NMETHD = 1
          ELSE IF (MXY(1)%MP(1) < 0 .AND. MXY(2)%MP(1) > 0 .AND.  &
              MXY(2)%MP(3) > 0) THEN
              CALL FMDP2M(SQRT(DPMAX),MXY(11))
              IF (FMCOMP(MXY(2),'>=',MXY(11))) THEN
                  KFLAG = -6
                  CALL FMST2M('UNDERFLOW',MXY(16))
                  GO TO 180
              ENDIF

              C1 = MAX( 10.0D0 , DBLE(NDSAVE)*DLOGMB/7.0D0 )
              C2 = -2.0D0*A
              IF (X < C1) THEN
                  IF (-2*MXY(1)%MP(2) > NDIG) THEN
                      NMETHD = 4
                  ELSE
                      NMETHD = 1
                  ENDIF
              ELSE IF (X > C2) THEN
                  T1 = FMDPLG(A)
                  SMALL = T1 - FMDPLG(-ABS(X)) - (A+ABS(X))*LOG(ABS(X))
                  TOL = -DBLE(NDIG+2)*DLOGMB - 12.0D0
                  B = 1.0D0
                  IF (A > ABS(X)) B = A - ABS(X)
                  BIG = T1 - FMDPLG(A-B) - B*LOG(ABS(X))
                  IF (SMALL < TOL+BIG) NMETHD = 2
              ENDIF
              IF (NMETHD == 0 .AND. X > C1) NMETHD = 3
              IF (NMETHD == 0) NMETHD = 1
          ELSE IF (MXY(1)%MP(1) > 0 .AND. MXY(1)%MP(3) > 0 .AND.  &
              MXY(2)%MP(1) < 0) THEN
              CALL FMEQ(MXY(2),MXY(11))
              IF (MXY(11)%MP(2) /= MUNKNO .AND. MXY(11)%MP(3) /= 0)  &
                  MXY(11)%MP(1) = -MXY(11)%MP(1)
              CALL FMMPYI(MXY(1),2,MXY(12))
              IF (FMCOMP(MXY(11),'<',MXY(1))) THEN
                  NMETHD = 1
              ELSE IF (FMCOMP(MXY(11),'<',MXY(12))) THEN
                  NMETHD = 3
              ELSE
                  NMETHD = 3
                  IF (SMALL < TOL+BIG) NMETHD = 2
              ENDIF
          ENDIF
      ENDIF

      IF (NMETHD == 2) GO TO 140
      IF (NMETHD == 3) GO TO 160
      IF (NMETHD == 4) GO TO 170

!             Method 1.  Use the X**N/Pochhammer(A+1,N) series.

!             MXY(16) = MXY(21) + MXY(22) is the current sum.
!             MXY(12) is the current term.
!             MXY(11) is (A+N)/X.
!             MXY(19) is 1/X

!             Raise the precision if A is negative and near an integer, to compensate for
!             cancellation when (A+N)/X is near zero.
!             Raise the precision if A is positive and near zero, since there will be cancellation
!             in subtracting the sum from Gamma(A).
!             If A is a negative integer use method 3 or 4.

      IEXTRA = 0
      IF (MXY(1)%MP(1) < 0) THEN
          IF (KFLAGA == 0) THEN
              IF (ABS(A) > 1.0D3) THEN
                  NMETHD = 3
                  GO TO 160
              ENDIF
          ELSE
              NMETHD = 3
              GO TO 160
          ENDIF
          CALL FMNINT(MXY(1),MXY(16))
          IF (FMCOMP(MXY(16),'==',MXY(1))) THEN
              IF (KFLAGI == 0) THEN
                  IF (KFLAGX /= 0) THEN
                      GO TO 160
                  ELSE
                      IF (ABS(X) <= 20.0D0) THEN
                          C1 = 0.7D0*(DBLE(NDSAVE)*DLOGMB*(20.0D0-X))**0.75D0
                          IF (ABS(A) > C1) THEN
                              GO TO 160
                          ELSE
                              GO TO 170
                          ENDIF
                      ELSE
                          GO TO 160
                      ENDIF
                  ENDIF
              ELSE
                  GO TO 160
              ENDIF
          ENDIF
          CALL FMSUB(MXY(1),MXY(16),MXY(19))
          IEXTRA = MAX(-2*INT(MXY(19)%MP(2)),-INT(MXY(1)%MP(2))+1,0)
      ELSE
          IEXTRA = MAX(-INT(MXY(1)%MP(2))+1,0)
      ENDIF

!             Raise the precision further as X increases in magnitude.

      IF (KFLAGX == 0 .AND. KFLAGA == 0) THEN
          T1 = (0.92D0 + (X-A) + (A-0.5D0)*LOG(ABS(A)+1.0D-10) -  &
               (A-1.0D0)*LOG(ABS(X)+1.0D-10))/DLOGMB
          IF (T1 > 0 .AND. ABS(X) > 1.0D0) THEN
              IF (A < 0.0D0 .OR. X >= A) THEN
                  IEXTRA = IEXTRA + MAX(0,INT(T1)+1)
              ENDIF
          ENDIF
      ENDIF

      IF (IEXTRA > 0) THEN
          CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
          CALL FMEQU_R1(MXY(2),NDIG,NDIG+IEXTRA)
      ENDIF
      NDIG = NDIG + IEXTRA
      N_ACC = NINT(NDIG*ALOGM2)

      JEXTRA = 0

      IF (KABIGR == 1) THEN
          CALL FMGAM(MXY(1),MXY(20))
          IF (KFLAG /= 0) THEN
              CALL FMEQ(MXY(20),MXY(16))
              GO TO 180
          ENDIF
          CALL FMEQ(MXY(20),MXY(16))
          IF (MXY(20)%MP(1) > 0) THEN
              CALL FMEQ(MXY(20),MXY(21))
              CALL FMI2M(0,MXY(22))
          ELSE
              CALL FMEQ(MXY(20),MXY(22))
              CALL FMI2M(0,MXY(21))
          ENDIF
      ELSE
          CALL FMI2M(0,MXY(16))
          CALL FMI2M(0,MXY(21))
          CALL FMI2M(0,MXY(22))
      ENDIF

      MAXM09 = MXY(16)%MP(2)

      CALL FMABS(MXY(2),MXY(19))
      CALL FMLN(MXY(19),MXY(6))
      CALL FMMPY_R2(MXY(1),MXY(6))
      CALL FMSUB_R1(MXY(6),MXY(2))
      CALL FMEXP(MXY(6),MXY(20))
      CALL FMDIV(MXY(20),MXY(1),MXY(12))
      IF (MXY(12)%MP(2) == MUNKNO) THEN
          CALL FMLN(MXY(1),MXY(15))
          CALL FMSUB_R1(MXY(6),MXY(15))
          CALL FMEXP(MXY(6),MXY(12))
      ENDIF
      IF (KXNEG == 1 .AND. MODA2 == 1 .AND. MXY(12)%MP(2) /= MUNKNO  &
          .AND. MXY(12)%MP(3) /= 0) THEN
          MXY(12)%MP(1) = -MXY(12)%MP(1)
      ENDIF

      IF (MXY(12)%MP(2) /= MUNKNO .AND. MXY(12)%MP(3) /= 0) THEN
          MXY(12)%MP(1) = -MXY(12)%MP(1)
      ENDIF
      CALL FMADD_R1(MXY(16),MXY(12))
      IF (MXY(12)%MP(1) > 0) THEN
          CALL FMADD_R1(MXY(21),MXY(12))
      ELSE
          CALL FMADD_R1(MXY(22),MXY(12))
      ENDIF
      MAXM09 = MAX(MAXM09,MXY(16)%MP(2))

      CALL FMI2M(1,MXY(9))
      CALL FMADD(MXY(1),MXY(9),MXY(11))
      CALL FMDIV_R1(MXY(12),MXY(11))
      CALL FMMPY_R1(MXY(12),MXY(2))
      CALL FMDIV_R1(MXY(11),MXY(2))
      CALL FMDIV(MXY(9),MXY(2),MXY(19))
      NDSAV1 = NDIG

!             If A is negative and ABS(A) > ABS(X), the terms in the series first decrease,
!             then increase, then decrease. Try to predict the number of extra digits required to
!             keep the precision from prematurely becoming too small.

      KFLGOK = 1
      IF (MXY(1)%MP(1) < 0) THEN
          KFLGOK = 0
          MXY(1)%MP(1) = 1
          MXY(2)%MP(1) = 1
          IF (FMCOMP(MXY(1),'>',MXY(2))) THEN
              IF (JEXTRA == 0) THEN
                  IF (KFLAGA == 0 .AND. KFLAGX == 0) THEN
                      T1 = FMDPLG(A+AINT(-ABS(X)-A)) - FMDPLG(A+1.0D0+AINT(ABS(X)-A))
                      T1 = (T1 + 2.0D0*ABS(X)*LOG(ABS(X)+1.0D-10))/DLOGMB
                      T1 = MAX(0.0D0,T1+1.0D0)
                      JEXTRA = T1
                  ENDIF
              ENDIF

!             If A is negative and ABS(A) is much bigger than ABS(X), then the later increase
!             in the size of the terms can be ignored.

              IF (KFLAGA == 0 .AND. KFLAGX == 0) THEN
                  T1 = (AINT(X-A)*LOG(ABS(X)+1.0D-10) + FMDPLG(A+1.0D0)  &
                       - FMDPLG(A+1.0D0+AINT(X-A))) / DLOGMB
                  IF (T1 < -DBLE(NDIG)) KFLGOK = 1
              ELSE
                  KFLGOK = 1
              ENDIF
          ENDIF
          MXY(1)%MP(1) = MAS
          MXY(2)%MP(1) = MBS
      ENDIF

      NMNNDG = NDSAV1

!             Method 1 summation loop.

      DO J = 1, NTERMS
         NDIG = NDSAV1
         IF (MXY(12)%MP(1) > 0) THEN
             CALL FMADD_R1(MXY(21),MXY(12))
         ELSE
             CALL FMADD_R1(MXY(22),MXY(12))
         ENDIF
         CALL FMCSADD_R1(MXY(16),MXY(12))
         MAXM09 = MAX(MAXM09,MXY(16)%MP(2))
         IF (KFLAG /= 0) THEN
             IF (KFLGOK == 0 .AND. KFLAGA == 0 .AND. KFLAGX == 0) THEN
                 IF (DBLE(J) > X-A) EXIT
             ELSE
                 EXIT
             ENDIF
         ENDIF

         CALL FMCSADD_R1(MXY(11),MXY(19))

         NDIG2 = MAX(NGRD22,NDSAV1-INT(MXY(16)%MP(2)-MXY(12)%MP(2)))
         NDIG = MIN(NDSAV1,NDIG2+JEXTRA)
         NMNNDG = MIN(NMNNDG,NDIG)
         CALL FMCSDIV(MXY(12),MXY(11),MXY(5))
         CALL FMEQ(MXY(5),MXY(12))
      ENDDO

      NDIG = NDSAV1
      IF (KABIGR == 0) THEN
          CALL FMEQ(MXY(16),MXY(19))
          CALL FMGAM(MXY(1),MXY(20))
          IF (KFLAG /= 0) THEN
              CALL FMEQ(MXY(20),MXY(16))
              GO TO 180
          ENDIF
          CALL FMADD(MXY(20),MXY(19),MXY(16))
          IF (MXY(9)%MP(1) > 0) THEN
              CALL FMADD_R1(MXY(21),MXY(20))
          ELSE
              CALL FMADD_R1(MXY(22),MXY(20))
          ENDIF

      ENDIF

!             If too much cancellation occurred, raise the precision and do the calculation again.

      CALL FMADD(MXY(21),MXY(22),MXY(16))
      CALL FMCANCEL(MXY(21),MXY(22),MXY(16),K)
      N_ACC = N_ACC - K

      GO TO 180

!             Method 2.  Use the Pochhammer(A-N,N)/X**N series.

!             MXY(16) is the current sum.
!             MXY(12) is the current term.
!             MXY(11) is (A-N)/X.
!             MXY(19) is -1/X

  140 CALL FMABS(MXY(2),MXY(19))
      CALL FMLN(MXY(19),MXY(6))
      CALL FMMPY(MXY(1),MXY(6),MXY(16))
      CALL FMSUB_R2(MXY(16),MXY(6))
      CALL FMSUB_R1(MXY(6),MXY(2))
      CALL FMEXP(MXY(6),MXY(12))
      IF (KXNEG == 1 .AND. MODA2 == 0 .AND. MXY(12)%MP(2) /= MUNKNO .AND.  &
          MXY(12)%MP(3) /= 0) MXY(12)%MP(1) = -MXY(12)%MP(1)
      IF (ABS(MXY(12)%MP(2)) >= MXEXP2) THEN
          CALL FMEQ(MXY(12),MXY(16))
          GO TO 180
      ENDIF

!             Here MXY(12) is X**(A-1)/EXP(X).

      CALL FMI2M(0,MXY(16))
      CALL FMI2M(0,MXY(21))
      CALL FMI2M(0,MXY(22))
      CALL FMEQ(MXY(1),MXY(11))
      CALL FMDIV_R1(MXY(11),MXY(2))
      CALL FMI2M(1,MXY(9))
      CALL FMDIV(MXY(9),MXY(2),MXY(19))
      IF (MXY(19)%MP(2) /= MUNKNO .AND. MXY(19)%MP(3) /= 0)  &
          MXY(19)%MP(1) = -MXY(19)%MP(1)
      NDSAV1 = NDIG

!             Disable NDIG reduction until the terms in the sum begin to decrease in size.

      BIGJ = 0
      IF (KFLAGA == 0 .AND. KFLAGX == 0) BIGJ = ABS(A) - ABS(X)
      JTERMS = NTERMS
      IF (KFLAGI == 0 .AND. INTA > 0) THEN
          JTERMS = INTA
      ELSE IF (KFLAGX == 0) THEN
          IF (KFLAGA == 0) THEN
              T1 = A + ABS(X)
              IF (T1 > 0 .AND. T1 < DBLE(NTERMS)) JTERMS = INT(T1) + 2
          ELSE IF (MXY(1)%MP(2) < 0) THEN
              T1 = ABS(X)
              IF (T1 > 0 .AND. T1 < DBLE(NTERMS)) JTERMS = INT(T1) + 2
          ENDIF
      ENDIF

!             Method 2 summation loop.

      DO J = 1, JTERMS
         NDIG = NDSAV1
         IF (MXY(12)%MP(1) > 0) THEN
             CALL FMADD_R1(MXY(21),MXY(12))
         ELSE
             CALL FMADD_R1(MXY(22),MXY(12))
         ENDIF
         CALL FMCSADD_R1(MXY(16),MXY(12))
         IF (KFLAG /= 0 .AND. J > 1) GO TO 150
         CALL FMCSADD_R1(MXY(11),MXY(19))
         IF (REAL(J) >= BIGJ) THEN
             NDIG2 = MAX(NGRD22,NDSAV1-INT(MXY(16)%MP(2)-MXY(12)%MP(2)))
             NDIG = MIN(NDSAV1,NDIG2)
         ENDIF
         CALL FMCSMPY_R1(MXY(12),MXY(11))
      ENDDO

  150 NDIG = NDSAV1
      CALL FMADD(MXY(21),MXY(22),MXY(16))
      CALL FMCANCEL(MXY(21),MXY(22),MXY(16),K)
      N_ACC = N_ACC - K
      GO TO 180

!             Method 3.  Use the continued fraction expansion.

!             MXY(19) = MXY(21) + MXY(22) is the current approximation.
!             MXY(16) is the term in the sum, S(k).
!             MXY(14), MXY(15) are the latest denominators, Q(k-1) and Q(k).

!             Raise the precision so that convergence of the continued fraction expansion
!             is easier to detect.

  160 JEXTRA = INT(MAX(1.0,5.76/ALOGMB + 1.0))

!             Raise the precision further for small X if A is positive.

      IF (KFLAGX == 0 .AND. KFLAGA == 0) THEN
          T1 = (0.92D0 + (ABS(X)-A) + (A-0.5D0)*LOG(ABS(A)+1.0D-10) -  &
               (A-1.0D0)*LOG(ABS(X)+1.0D-10))/DLOGMB
          IF (T1 > 0.0D0 .AND. A > 0.0D0) THEN
              IF (ABS(X) < A) THEN
                  JEXTRA = JEXTRA + MAX(0,INT(1.5D0*T1)+1)
              ENDIF
          ENDIF
      ENDIF
      NDSAV1 = NDIG
      IF (NDIG+JEXTRA > NDSAV1) THEN
          CALL FMEQU_R1(MXY(1),NDSAV1,NDSAV1+JEXTRA)
          CALL FMEQU_R1(MXY(2),NDSAV1,NDSAV1+JEXTRA)
      ENDIF
      NDIG = NDIG + JEXTRA
      N_ACC = NINT(NDIG*ALOGM2)
      CALL FMEQU(MXY(1),MXY(11),NDSAV1,NDIG)
      IF (MXY(11)%MP(2) /= MUNKNO .AND. MXY(11)%MP(3) /= 0)  &
          MXY(11)%MP(1) = -MXY(11)%MP(1)
      CALL FMI2M(1,MXY(14))
      CALL FMEQU(MXY(2),MXY(15),NDSAV1,NDIG)
      CALL FMI2M(1,MXY(7))
      CALL FMDIV(MXY(7),MXY(15),MXY(16))
      CALL FMEQ(MXY(16),MXY(19))
      IF (MXY(19)%MP(1) > 0) THEN
          CALL FMEQ(MXY(19),MXY(21))
          CALL FMI2M(0,MXY(22))
      ELSE
          CALL FMEQ(MXY(19),MXY(22))
          CALL FMI2M(0,MXY(21))
      ENDIF

      JTERMS = NTERMS
      IF (INTA > 0) JTERMS = INTA - 1

!             Method 3 continued fraction loop.

      NDSAV2 = NDIG
      DO J = 1, JTERMS
         CALL FMADDI(MXY(11),1)
         CALL FMEQ(MXY(11),MXY(13))
         CALL FMCSMPY_R1(MXY(13),MXY(14))
         CALL FMADD(MXY(15),MXY(13),MXY(12))
         CALL FMEQ(MXY(13),MXY(8))
         CALL FMCSMPY_R1(MXY(8),MXY(16))
         CALL FMCSDIV(MXY(8),MXY(12),MXY(16))
         IF (MXY(16)%MP(2) /= MUNKNO .AND. MXY(16)%MP(3) /= 0)  &
             MXY(16)%MP(1) = -MXY(16)%MP(1)
         NDIG = NDSAV2
         IF (MXY(16)%MP(1) > 0) THEN
             CALL FMADD_R1(MXY(21),MXY(16))
         ELSE
             CALL FMADD_R1(MXY(22),MXY(16))
         ENDIF
         CALL FMCSADD_R1(MXY(19),MXY(16))
         KFLAG1 = KFLAG
         CALL FMEQ(MXY(15),MXY(14))
         CALL FMEQ(MXY(12),MXY(15))
         NDIG = MIN(NDSAV2,MAX(NGRD22,NDSAV2-INT(MXY(19)%MP(2)-MXY(16)%MP(2))))
         CALL FMCSMPYI(MXY(14),J,MXY(13))
         CALL FMEQ(MXY(2),MXY(8))
         CALL FMCSMPY_R1(MXY(8),MXY(15))
         CALL FMADD(MXY(8),MXY(13),MXY(12))
         CALL FMEQ(MXY(13),MXY(8))
         CALL FMCSMPY_R1(MXY(8),MXY(16))
         CALL FMCSDIV(MXY(8),MXY(12),MXY(16))
         IF (MXY(16)%MP(2) /= MUNKNO .AND. MXY(16)%MP(3) /= 0)  &
             MXY(16)%MP(1) = -MXY(16)%MP(1)
         NDIG = NDSAV2
         IF (MXY(16)%MP(1) > 0) THEN
             CALL FMADD_R1(MXY(21),MXY(16))
         ELSE
             CALL FMADD_R1(MXY(22),MXY(16))
         ENDIF
         CALL FMCSADD_R1(MXY(19),MXY(16))

!             Check for convergence.

         IF (KFLAG1 == 1 .AND. KFLAG == 1) THEN
             EXIT
         ENDIF
         CALL FMEQ(MXY(15),MXY(14))
         CALL FMEQ(MXY(12),MXY(15))
         NDIG = MIN(NDSAV2,MAX(NGRD22,NDSAV2-INT(MXY(19)%MP(2)-MXY(16)%MP(2))))
      ENDDO

      NDIG = NDSAV2
      CALL FMADD(MXY(21),MXY(22),MXY(19))
      CALL FMCANCEL(MXY(21),MXY(22),MXY(19),K)
      N_ACC = N_ACC - K
      CALL FMEQU_R1(MXY(19),NDIG,NDSAV1)
      NDIG = NDSAV1
      CALL FMABS(MXY(2),MXY(15))
      CALL FMLN(MXY(15),MXY(6))
      CALL FMMPY_R2(MXY(1),MXY(6))
      CALL FMSUB_R1(MXY(6),MXY(2))
      CALL FMEXP(MXY(6),MXY(5))
      IF (KXNEG == 1 .AND. MODA2 == 1 .AND. MXY(5)%MP(2) /= MUNKNO .AND.  &
          MXY(5)%MP(3) /= 0) MXY(5)%MP(1) = -MXY(5)%MP(1)
      IF (ABS(MXY(5)%MP(2)) >= MXEXP2) THEN
          CALL FMEQ(MXY(5),MXY(16))
          IF (MXY(19)%MP(1) < 0 .AND. MXY(16)%MP(2) /= MUNKNO .AND.  &
              MXY(16)%MP(3) /= 0) MXY(16)%MP(1) = -MXY(16)%MP(1)
          GO TO 180
      ENDIF

      CALL FMMPY(MXY(5),MXY(19),MXY(16))
      GO TO 180

!             Method 4.  Use the O(A**2) formula when A is small.

!             MXY(16) is the current term.
!             MXY(19) is the current sum.

!             Raise the precision if X is larger than A in magnitude.
!             The terms initially increase in size, and the final sum is small.

  170 IEXTRA = 0

!             If A is a negative integer, replace it by zero and later use a recurrence to recover
!             the original function value.

      IF (KFLAGI == 0 .AND. INTA < 0) THEN
          CALL FMI2M(0,MXY(1))
          A = 0.0D0
          KMETH4 = 1
      ENDIF

      IF (KFLAGX == 0) THEN
          IF (KFLAGA == 0) THEN
              T1 = ABS(X) - ABS(A)
          ELSE
              T1 = ABS(X)
          ENDIF
          IF (T1 > 0) THEN
              T2 = (T1 + LOG(T1))/DLOGMB
              IF (T2 > DBLE(MXEXP2/10)) T2 = DBLE(MXEXP2/10)
              IEXTRA = INT(MAX(0.0D0,T2))
          ENDIF
          T1 = ABS(X)+1.0D-10
          T2 = (T1 - 0.5D0*LOG(6.2831853D0*T1))/DLOGMB
          IF (T2 > DBLE(MXEXP2/10)) T2 = DBLE(MXEXP2/10)
          IEXTRA = IEXTRA + INT(MAX(0.0D0,T2))
      ENDIF

      IF (IEXTRA > 0) THEN
          CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
          CALL FMEQU_R1(MXY(2),NDIG,NDIG+IEXTRA)
      ENDIF
      NDIG = NDIG + IEXTRA
      N_ACC = NINT(NDIG*ALOGM2)

      CALL FMEULR(MXY(19))
      CALL FMEQ(MXY(19),MXY(20))
      MXY(19)%MP(1) = -1
      CALL FMABS(MXY(2),MXY(16))
      CALL FMLN(MXY(16),MXY(15))
      CALL FMSUB_R1(MXY(19),MXY(15))
      IF (MXY(1)%MP(3) /= 0 .AND. MXY(1)%MP(2) >= -NDIG-1) THEN
          CALL FMSQR(MXY(15),MXY(7))
          CALL FMMPY(MXY(7),MXY(1),MXY(4))
          CALL FMDIVI(MXY(4),2,MXY(16))
          CALL FMSUB_R1(MXY(19),MXY(16))

          CALL FMSQR(MXY(20),MXY(14))
          CALL FMPI(MXY(13))
          CALL FMSQR(MXY(13),MXY(7))
          CALL FMDIVI(MXY(7),6,MXY(13))
          CALL FMADD(MXY(13),MXY(14),MXY(7))
          CALL FMMPY(MXY(7),MXY(1),MXY(4))
          CALL FMDIVI(MXY(4),2,MXY(14))
          CALL FMADD_R1(MXY(19),MXY(14))
      ENDIF

      NDSAV1 = NDIG
      CALL FMI2M(1,MXY(14))
      JR = KROUND
      KROUND = 1
      CALL FMADD(MXY(1),MXY(14),MXY(13))
      KROUND = JR
      IF (FMCOMP(MXY(14),'==',MXY(13))) THEN
          CALL FMI2M(-1,MXY(16))
          DO J = 1, NTERMS
             NDIG2 = MAX(NGRD22,NDSAV1-INT(MXY(19)%MP(2)-MXY(16)%MP(2)))
             NDIG = MIN(NDSAV1,NDIG2)
             CALL FMMPY_R1(MXY(16),MXY(2))
             IF (MXY(16)%MP(2) /= MUNKNO .AND. MXY(16)%MP(3) /= 0)  &
                 MXY(16)%MP(1) = -MXY(16)%MP(1)
             CALL FMDIVI_R1(MXY(16),J)
             CALL FMDIVI(MXY(16),J,MXY(15))
             NDIG = NDSAV1
             CALL FMADD_R1(MXY(19),MXY(15))
             IF (KFLAG /= 0) EXIT
          ENDDO
      ELSE
          CALL FMPWR(MXY(2),MXY(1),MXY(16))
          IF (MXY(16)%MP(2) /= MUNKNO .AND. MXY(16)%MP(3) /= 0)  &
              MXY(16)%MP(1) = -MXY(16)%MP(1)
          CALL FMEQ(MXY(1),MXY(20))
          DO J = 1, NTERMS
             NDIG2 = MAX(NGRD22,NDSAV1-INT(MXY(19)%MP(2)-MXY(16)%MP(2)))
             NDIG = MIN(NDSAV1,NDIG2)
             CALL FMMPY_R1(MXY(16),MXY(2))
             IF (MXY(16)%MP(2) /= MUNKNO .AND. MXY(16)%MP(3) /= 0)  &
                 MXY(16)%MP(1) = -MXY(16)%MP(1)
             CALL FMDIVI_R1(MXY(16),J)
             NDIG = NDSAV1
             CALL FMADD_R1(MXY(20),MXY(14))
             NDIG = MIN(NDSAV1,NDIG2)
             CALL FMDIV(MXY(16),MXY(20),MXY(15))
             NDIG = NDSAV1
             CALL FMADD_R1(MXY(19),MXY(15))
             IF (KFLAG /= 0) EXIT
          ENDDO
      ENDIF
      CALL FMEQ(MXY(19),MXY(16))

!             Use the recurrence relation if A was a negative integer.

      IF (KFLAGI == 0 .AND. INTA < 0) THEN
          N = -INTA
          CALL FMI2M(1,MXY(19))
          CALL FMDIV_R1(MXY(19),MXY(2))
          CALL FMEQ(MXY(19),MXY(15))
          CALL FMEQ(MXY(19),MXY(14))
          DO J = 1, N-1
             CALL FMMPYI_R1(MXY(15),J)
             CALL FMMPY_R1(MXY(15),MXY(14))
             IF (MXY(15)%MP(2) /= MUNKNO .AND. MXY(15)%MP(3) /= 0)  &
                 MXY(15)%MP(1) = -MXY(15)%MP(1)
             CALL FMADD_R1(MXY(19),MXY(15))
          ENDDO
          CALL FMEXP(MXY(2),MXY(14))
          CALL FMDIV_R1(MXY(19),MXY(14))
          CALL FMSUB_R1(MXY(16),MXY(19))
          CALL FMFCTI(N,MXY(14))
          CALL FMDIV_R1(MXY(16),MXY(14))
          IF (MOD(N,2) == 1 .AND. MXY(16)%MP(2) /= MUNKNO .AND.  &
              MXY(16)%MP(3) /= 0) MXY(16)%MP(1) = -MXY(16)%MP(1)
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  180 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(16)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 120
      ENDIF

!             Check for too much cancellation.

      IF (NCALL >= 1 .AND. ABS(MXY(16)%MP(2)) <= MXEXP) THEN
          NGOAL = INT(REAL(NDSAVE)*ALOGM2) + 17
      ELSE
          NGOAL = INT(-MXEXP2)
      ENDIF
      IF (N_ACC <= NGOAL) THEN
          IF (NUMTRY > 0) THEN
              NDGOAL = INT(REAL(NGOAL)/ALOGM2 + 1.0)
              DO J = 1, NDGOAL+1
                 IF (MRETRY%MP(J+1) /= MXY(16)%MP(J+1)) GO TO 190
              ENDDO
              GO TO 200
          ENDIF
  190     IEXTRA = INT(REAL(NGOAL-N_ACC)/ALOGM2 + 23.03/ALOGMB) + 1
          NDOLD = NDIG
          NDIG = NDIG + IEXTRA
          N_ACC = NINT(NDIG*ALOGM2)
          CALL FMEQU_R1(MXY(1),NDSAVE,NDIG)
          IF (KMETH4 == 1) THEN
              CALL FMI2M(INTA,MXY(1))
          ENDIF
          CALL FMEQU_R1(MXY(2),NDSAVE,NDIG)
          NUMTRY = NUMTRY + 1
          CALL FMEQU(MXY(16),MRETRY,NDOLD,NDIG)
          GO TO 130
      ENDIF

  200 CALL FMEXT2(MXY(16),MC,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMIGM2

      SUBROUTINE FMLNGM(MA,MB)

!  MB = LN(GAMMA(MA))

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MAS,MXSAVE
      DOUBLE PRECISION :: ERR,X,Z
      INTEGER :: IEXTRA,INTA,J,J2,K,K0,K1,K2,KFL,KL,KOVUN,KC_RETRY,KR_RETRY,KRESLT,KRSAVE,     &
                 KSIGN,KWRNSV,LSHIFT,NDENOM,NDGOAL,NDIG2,NDMB,NDOLD,NDSAV1,NDSAVE,NDSV,NGOAL,  &
                 NMETHD,NMXDIF,NTERM,NUMTRY,N_ACC
      LOGICAL, EXTERNAL :: FMCOMP
      CHARACTER(155) :: STRING
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI), SAVE :: MXY(15),MJSUMS(LJSUMS),MRETRY,C(0:196)
      INTEGER, SAVE :: NDIG_C = 0
      REAL (KIND(1.0D0)), SAVE :: MBASE_C = 0

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL FMENT2('FMLNGM   ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF
      MAS = MA%MP(1)
      KR_RETRY = 0
      KC_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      N_ACC = NINT(NDIG*ALOGM2)
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
      CALL FMEQ(MXY(1),MXY(13))
      NUMTRY = 0

!             Near zero Gamma(x) is about 1/x.

  120 IF (MXY(13)%MP(2) < (-NDIG-3)) THEN
          CALL FMLN(MXY(13),MXY(10))
          IF (MXY(10)%MP(2) /= MUNKNO .AND. MXY(10)%MP(3) /= 0)  &
              MXY(10)%MP(1) = -MXY(10)%MP(1)
          GO TO 190
      ENDIF

!             Check for special cases.

      IF (MAS < 0) THEN
          KFL = 0
          IF (MXY(1)%MP(2) <= NDSAVE) THEN
              CALL FMINT(MXY(13),MXY(9))
              IF (FMCOMP(MXY(13),'==',MXY(9))) KFL = -4
              CALL FMI2M(2,MXY(10))
              MXY(9)%MP(1) = 1
              CALL FMMOD(MXY(9),MXY(10),MXY(5))
              CALL FMEQ(MXY(5),MXY(10))
              IF (MXY(10)%MP(3) == 0) KFL = -4
          ELSE
              KFL = -4
          ENDIF
          IF (KFL /= 0) THEN
              CALL FMST2M('UNKNOWN',MXY(10))
              KFLAG = -4
              GO TO 210
          ELSE
              CALL FMI2M(1,MXY(5))
              CALL FMSUB_R2(MXY(5),MXY(13))
          ENDIF
      ENDIF

!             Determine which method to use.

!             NMETHD = 1 means use the polynomial approximation,
!                    = 2 means use the asymptotic series.

      NMETHD = 1
      CALL FMNINT(MXY(13),MXY(2))
      CALL FMSUB(MXY(13),MXY(2),MXY(6))
      CALL FMM2DP(MXY(6),Z)
      Z = MAX(ABS(Z),1.0D-50)
      IF (KFLAG /= 0 .OR. ABS(Z) >= 1) THEN
          NMETHD = 2
      ELSE
          IF (190*LOG(Z) - 90*DLOGTN >= -NDIG*DLOGMB .OR. -190*DLOGTN >= -NDIG*DLOGMB) THEN
              NMETHD = 2
          ENDIF
      ENDIF
      CALL FMM2DP(MXY(13),X)
      IF (KFLAG /= 0) THEN
          NMETHD = 2
      ELSE IF (NMETHD == 1) THEN
          IF (X > 35 - 8*LOG(Z) + NDIG*DLOGMB/(2.5*DLOGTN) .OR. X > 250) NMETHD = 2
      ENDIF
      IF (NMETHD == 2) GO TO 150

!             Method 1.  Use the polynomial c(0) + c(1)*(x-3) + ... + c(196)*(x-3)**196

      IF (MA%MP(1) > 0 .AND. NDSAVE+NGRD52 < NDIG .AND. KR_RETRY == 0 .AND. KC_RETRY == 0)  &
          NDIG = NDSAVE + NGRD52
      N_ACC = NINT(NDIG*ALOGM2)
      CALL FMM2I(MXY(2),LSHIFT)
      LSHIFT = LSHIFT - 3
      IF (NDIG_C < NDIG .OR. MBASE_C /= MBASE) THEN
          CALL FMLNGM_C(NDIG_C,MBASE_C,C)
      ENDIF
      J2 = 0.42*LOG(Z) + 7.9
      J2 = MAX(2,MIN(J2+MOD(J2,2),LJSUMS))
      NDSAV1 = NDIG
      DO J = 1, J2
         CALL FMEQ(C(J+1),MJSUMS(J))
      ENDDO
      CALL FMIPWR(MXY(6),J2,MXY(5))
      CALL FMEQ(MXY(5),MXY(7))
      NTERM = J2 + 1
  130 IF (NTERM > J2+1) CALL FMCSMPY_R1(MXY(7),MXY(5))
      DO J = 1, J2
         NTERM = NTERM + 1
         CALL FMEQ(MXY(7),MXY(4))
         CALL FMCSMPY_R1(MXY(4),C(NTERM))
         NDIG = NDSAV1
         CALL FMCSADDNN_R1(MJSUMS(J),MXY(4))
         IF (KFLAG /= 0 .OR. NTERM == 196) GO TO 140
         NDIG = NDSAV1 - INT(MJSUMS(J)%MP(2)-MXY(4)%MP(2))
         NDIG = MIN(NDSAV1,NDIG)
         IF (NDIG < NGRD22) NDIG = NGRD22
      ENDDO
      GO TO 130
  140 KFLAG = 0
      CALL FMCSNSUMS(J2,MJSUMS)
      NDIG = NDSAV1
      IF (NTERM == 196) THEN
          GO TO 150
      ENDIF
      CALL FMEQ(MJSUMS(J2),MXY(8))
      CALL FMEQ(MXY(6),MXY(3))
      MXY(3)%MP(1) = -MXY(3)%MP(1)
      DO J = 2, J2
         CALL FMCSMPY_R1(MXY(8),MXY(3))
         CALL FMCSADD_R1(MXY(8),MJSUMS(J2-J+1))
      ENDDO
      CALL FMSQR(MXY(6),MXY(3))
      CALL FMCSMPY_R1(MXY(8),MXY(3))
      CALL FMADD(MXY(8),C(0),MXY(10))
      CALL FMMPY(MXY(6),C(1),MXY(3))
      CALL FMADD_R1(MXY(10),MXY(3))

!             Recover from using a shifted argument.

      IF (LSHIFT < 0) THEN
          CALL FMEQ(MXY(13),MXY(3))
          CALL FMEQ(MXY(13),MXY(14))
          DO J = 2, -LSHIFT
             CALL FMADDI(MXY(14),1)
             CALL FMMPY_R1(MXY(3),MXY(14))
          ENDDO
          CALL FMLN(MXY(3),MXY(4))
          CALL FMSUB(MXY(10),MXY(4),MXY(3))
          CALL FMCANCEL(MXY(10),MXY(4),MXY(3),K)
          N_ACC = N_ACC - K
          CALL FMEQ(MXY(3),MXY(10))
      ENDIF
      IF (LSHIFT > 0) THEN
          IF (MOD(LSHIFT,4) == 0) THEN
              CALL FMI2M(1,MXY(3))
              CALL FMEQ(MXY(6),MXY(14))
              CALL FMADDI(MXY(14),3)
          ELSE
              CALL FMEQ(MXY(6),MXY(3))
              CALL FMADDI(MXY(3),3)
              CALL FMEQ(MXY(3),MXY(14))
              DO J = 1, MOD(LSHIFT,4)-1
                 CALL FMADDI(MXY(14),1)
                 CALL FMMPY_R1(MXY(3),MXY(14))
              ENDDO
              CALL FMADDI(MXY(14),1)
          ENDIF
          LSHIFT = LSHIFT - MOD(LSHIFT,4)

!             The product Z*(Z+1)*...*(Z+LSHIFT-1) is computed four terms at a time to reduce
!             the number of FMMPY calls.

!             MXY(14) is Z
!             MXY(6) is Z**2
!             MXY(7) is Z**3
!             MXY(8) is (Z+K)*...*(Z+K+3)
!             MXY(11) is the current product

          CALL FMI2M(1,MXY(11))
          IF (LSHIFT > 0) THEN
              CALL FMSQR(MXY(14),MXY(6))
              CALL FMMPY(MXY(14),MXY(6),MXY(7))
              CALL FMSQR(MXY(6),MXY(8))
              CALL FMCSMPYI(MXY(7),6,MXY(12))
              CALL FMCSADD_R1(MXY(8),MXY(12))
              CALL FMCSMPYI(MXY(6),11,MXY(12))
              CALL FMCSADD_R1(MXY(8),MXY(12))
              CALL FMCSMPYI(MXY(14),6,MXY(12))
              CALL FMCSADD_R1(MXY(8),MXY(12))
              CALL FMEQ(MXY(8),MXY(11))
              CALL FMCSMPYI_R1(MXY(7),16)
              DO K = 0, LSHIFT-8, 4
                 CALL FMCSADD_R1(MXY(8),MXY(7))
                 K2 = 24*(2*K + 7)
                 CALL FMCSMPYI(MXY(6),K2,MXY(12))
                 CALL FMCSADD_R1(MXY(8),MXY(12))
                 IF (K <= SQRT(REAL(INTMAX)/49.0)) THEN
                     K1 = 8*(6*K*K + 42*K + 79)
                     CALL FMCSMPYI(MXY(14),K1,MXY(12))
                     CALL FMCSADD_R1(MXY(8),MXY(12))
                 ELSE
                     K1 = 48*K
                     CALL FMCSMPYI(MXY(14),K1,MXY(12))
                     CALL FMCSMPYI_R1(MXY(12),K)
                     CALL FMCSADD_R1(MXY(8),MXY(12))
                     K1 = 336*K + 632
                     CALL FMCSMPYI(MXY(14),K1,MXY(12))
                     CALL FMCSADD_R1(MXY(8),MXY(12))
                 ENDIF
                 IF (K <= (REAL(INTMAX)/17.0)**0.3333) THEN
                     K0 = 8*(2*K + 7)*(K*K + 7*K + 15)
                     CALL FMADDI(MXY(8),K0)
                 ELSE IF (K <= SQRT(REAL(INTMAX)*0.9)) THEN
                     K0 = 8*(2*K + 7)
                     CALL FMI2M(K0,MXY(12))
                     K0 = K*K + 7*K + 15
                     CALL FMCSMPYI_R1(MXY(12),K0)
                     CALL FMCSADD_R1(MXY(8),MXY(12))
                 ELSE
                     K0 = 8*(2*K + 7)
                     CALL FMI2M(K0,MXY(12))
                     CALL FMCSMPYI(MXY(12),K,MXY(9))
                     CALL FMCSMPYI_R1(MXY(9),K)
                     CALL FMCSADD_R1(MXY(8),MXY(9))
                     K0 = 7*K + 15
                     CALL FMCSMPYI_R1(MXY(12),K0)
                     CALL FMCSADD_R1(MXY(8),MXY(12))
                 ENDIF
                 CALL FMCSMPY_R1(MXY(11),MXY(8))
              ENDDO
          ENDIF
          CALL FMMPY_R1(MXY(11),MXY(3))
          CALL FMLN(MXY(11),MXY(4))
          CALL FMADD(MXY(10),MXY(4),MXY(3))
          CALL FMCANCEL(MXY(10),MXY(4),MXY(3),K)
          N_ACC = N_ACC - K
          CALL FMEQ(MXY(3),MXY(10))
      ENDIF
      GO TO 180

!             Method 2.  Use the B(2n)/(2n*(2n-1)*X**(2n-1) asymptotic series.
!                        To speed the asymptotic series calculation,
!                        increase the argument by LSHIFT.

  150 IEXTRA = 0
      N_ACC = NINT(NDIG*ALOGM2)
      KWRNSV = KWARN
      KWARN = 0
      CALL FMM2I(MXY(13),INTA)
      KWARN = KWRNSV

      IF (KFLAG == -4) THEN
          LSHIFT = 0
      ELSE
          LSHIFT = INT(MAX(0.0,REAL(NDIG)*ALOGMB/4.46-REAL(INTA)))
      ENDIF
      IF (LSHIFT > 0) LSHIFT = 4*(LSHIFT/4 + 1)
      IF (KFLAG == 0) THEN
          IF (LSHIFT > 0 .OR. INTA <= 10) THEN
              IF (INTA <= 2) THEN
                  CALL FMI2M(0,MXY(10))
                  GO TO 190
              ENDIF
              INTA = INTA - 1
              CALL FMFCTI(INTA,MXY(13))
              CALL FMLN(MXY(13),MXY(10))
              GO TO 190
          ENDIF
      ENDIF

      IF (LSHIFT /= 0) THEN
          CALL FMI2M(LSHIFT,MXY(5))
          CALL FMADD(MXY(13),MXY(5),MXY(12))
      ELSE
          CALL FMEQ(MXY(13),MXY(12))
      ENDIF

!             Sum the asymptotic series.

!       MXY(13) is Z
!       MXY(12) is Z + LSHIFT
!       MXY(9)  is X**J2 = (1/(Z+LSHIFT)**2)**J2
!       MXY(10) is the current power of X
!       MXY(11) is the current term in the sum
!       MJSUMS  is the partial sum

      J2 = INT(0.3*ALOGMB + 0.2*SQRT(REAL(NDIG)))
      J2 = MAX(1,MIN(LJSUMS,J2))
      NDSAV1 = NDIG
      CALL FMI2M(1,MXY(10))
      J = -2*J2
      CALL FMIPWR(MXY(12),J,MXY(9))
      IF (ABS(MXY(9)%MP(2)) >= MEXPAB) THEN
          J2 = 1
          CALL FMIPWR(MXY(12),-2,MXY(9))
      ENDIF
      DO J = 1, J2
         NTERM = 2*J
         CALL FMBERN(NTERM,MXY(10),MXY(11))
         IF (KFLAG == -11) THEN
             CALL FMST2M('UNKNOWN',MXY(10))
             KFLAG = -4
             GO TO 210
         ENDIF
         NDENOM = NTERM*(NTERM-1)
         CALL FMCSDIVI(MXY(11),NDENOM,MJSUMS(J))
      ENDDO

      NDIG2 = NDIG
  160 CALL FMCSMPY_R1(MXY(10),MXY(9))

      NMXDIF = MIN(NDSAV1,NGRD22)
      DO J = 1, J2
         NTERM = NTERM + 2
         CALL FMBERN(NTERM,MXY(10),MXY(11))
         IF (KFLAG == -11) THEN
             CALL FMST2M('UNKNOWN',MXY(10))
             KFLAG = -4
             GO TO 210
         ENDIF
         NDENOM = NTERM*(NTERM-1)
         IF (NTERM <= MXBASE/(NTERM-1)) THEN
             CALL FMCSDIVI_R1(MXY(11),NDENOM)
         ELSE
             CALL FMCSDIVI_R1(MXY(11),NTERM)
             NDENOM = NTERM - 1
             CALL FMCSDIVI_R1(MXY(11),NDENOM)
         ENDIF
         NDIG = NDSAV1
         CALL FMCSADD_R1(MJSUMS(J),MXY(11))
         NMXDIF = MAX(NMXDIF,NDSAV1-INT(MJSUMS(J)%MP(2)-MXY(11)%MP(2)))
         NDIG = NDIG2
         IF (KFLAG /= 0) GO TO 170
      ENDDO
      NDIG2 = NMXDIF
      NDIG = NDIG2
      GO TO 160

!             Put the J2 concurrent sums back together.

  170 NDIG = NDSAV1
      IF (J2 > 1) THEN
          CALL FMSQR(MXY(12),MXY(11))
          CALL FMI2M(1,MXY(5))
          CALL FMDIV_R2(MXY(5),MXY(11))
          CALL FMEQ(MJSUMS(J2),MXY(9))
          DO J = J2-1, 1, -1
             CALL FMCSMPY_R1(MXY(9),MXY(11))
             CALL FMCSADD_R1(MXY(9),MJSUMS(J))
          ENDDO
          CALL FMEQ(MXY(9),MJSUMS(1))
      ENDIF

!             Add the log terms to the asymptotic series.

!       MXY(10) is the current sum as the log terms are added
!       MXY(11) is now LN(Z+LSHIFT)

      CALL FMDIV(MJSUMS(1),MXY(12),MXY(10))
      CALL FMLN(MXY(12),MXY(11))
      IF (MBASE /= MBS2PI .OR. NDIG > NDG2PI) THEN
          NDMB = INT(150.0*2.302585/ALOGMB)
          IF (NDMB >= NDIG) THEN
              NDSV = NDIG
              NDIG = NDMB
              STRING = '1.837877066409345483560659472811235279722794'//  &
              '94727556682563430308096553139185452079538948659727190'//  &
              '8395244011293249268674892733725763681587144311751830445'
              CALL FMST2M(STRING,M_LN_2PI)
              MBS2PI = MBASE
              NDG2PI = NDIG
              IF (ABS(M_LN_2PI%MP(2)) > 10) NDG2PI = 0
              NDIG = NDSV
          ELSE
              NDSV = NDIG
              NDIG = NDIG + 2
              CALL FMPI(MXY(9))
              CALL FMMPYI(MXY(9),2,MXY(5))
              CALL FMLN(MXY(5),M_LN_2PI)
              MBS2PI = MBASE
              NDG2PI = NDIG
              IF (ABS(M_LN_2PI%MP(2)) > 10) NDG2PI = 0
              NDIG = NDSV
          ENDIF
      ENDIF
      CALL FMSUB(M_LN_2PI,MXY(11),MXY(5))
      CALL FMCANCEL(M_LN_2PI,MXY(11),MXY(5),K)
      N_ACC = N_ACC - K
      CALL FMCSDIVI(MXY(5),2,MXY(9))
      CALL FMADD(MXY(10),MXY(9),MXY(3))
      CALL FMCANCEL(MXY(10),MXY(9),MXY(3),K)
      N_ACC = N_ACC - K
      CALL FMEQ(MXY(3),MXY(10))
      CALL FMSUB(MXY(10),MXY(12),MXY(3))
      CALL FMCANCEL(MXY(10),MXY(12),MXY(3),K)
      N_ACC = N_ACC - K
      CALL FMEQ(MXY(3),MXY(10))
      CALL FMMPY(MXY(11),MXY(12),MXY(9))
      CALL FMADD(MXY(10),MXY(9),MXY(3))
      CALL FMCANCEL(MXY(10),MXY(9),MXY(3),K)
      N_ACC = N_ACC - K
      CALL FMEQ(MXY(3),MXY(10))

!             Now the log of gamma of the shifted argument has been computed.  Reverse the shifting.
!             The product MA*(MA+1)*...*(MA+LSHIFT-1) is computed four terms at a time to reduce
!             the number of FMMPY calls.

!             MXY(13) is Z
!             MXY(6) is Z**2
!             MXY(7) is Z**3
!             MXY(8) is (Z+K)*...*(Z+K+3)
!             MXY(11) is the current product

      IF (LSHIFT > 0) THEN
          CALL FMSQR(MXY(13),MXY(6))
          CALL FMMPY(MXY(13),MXY(6),MXY(7))
          CALL FMSQR(MXY(6),MXY(8))
          CALL FMCSMPYI(MXY(7),6,MXY(12))
          CALL FMCSADD_R1(MXY(8),MXY(12))
          CALL FMCSMPYI(MXY(6),11,MXY(12))
          CALL FMCSADD_R1(MXY(8),MXY(12))
          CALL FMCSMPYI(MXY(13),6,MXY(12))
          CALL FMCSADD_R1(MXY(8),MXY(12))
          CALL FMEQ(MXY(8),MXY(11))
          CALL FMCSMPYI_R1(MXY(7),16)
          DO K = 0, LSHIFT-8, 4
             CALL FMCSADD_R1(MXY(8),MXY(7))
             K2 = 24*(2*K + 7)
             CALL FMCSMPYI(MXY(6),K2,MXY(12))
             CALL FMCSADD_R1(MXY(8),MXY(12))
             IF (K <= SQRT(REAL(INTMAX)/49.0)) THEN
                 K1 = 8*(6*K*K + 42*K + 79)
                 CALL FMCSMPYI(MXY(13),K1,MXY(12))
                 CALL FMCSADD_R1(MXY(8),MXY(12))
             ELSE
                 K1 = 48*K
                 CALL FMCSMPYI(MXY(13),K1,MXY(12))
                 CALL FMCSMPYI_R1(MXY(12),K)
                 CALL FMCSADD_R1(MXY(8),MXY(12))
                 K1 = 336*K + 632
                 CALL FMCSMPYI(MXY(13),K1,MXY(12))
                 CALL FMCSADD_R1(MXY(8),MXY(12))
             ENDIF
             IF (K <= (REAL(INTMAX)/17.0)**0.3333) THEN
                 K0 = 8*(2*K + 7)*(K*K + 7*K + 15)
                 CALL FMADDI(MXY(8),K0)
             ELSE IF (K <= SQRT(REAL(INTMAX)*0.9)) THEN
                 K0 = 8*(2*K + 7)
                 CALL FMI2M(K0,MXY(12))
                 K0 = K*K + 7*K + 15
                 CALL FMCSMPYI_R1(MXY(12),K0)
                 CALL FMCSADD_R1(MXY(8),MXY(12))
             ELSE
                 K0 = 8*(2*K + 7)
                 CALL FMI2M(K0,MXY(12))
                 CALL FMCSMPYI(MXY(12),K,MXY(9))
                 CALL FMCSMPYI_R1(MXY(9),K)
                 CALL FMCSADD_R1(MXY(8),MXY(9))
                 K0 = 7*K + 15
                 CALL FMCSMPYI_R1(MXY(12),K0)
                 CALL FMCSADD_R1(MXY(8),MXY(12))
             ENDIF
             CALL FMCSMPY_R1(MXY(11),MXY(8))
          ENDDO
          CALL FMLN(MXY(11),MXY(4))
          CALL FMSUB(MXY(10),MXY(4),MXY(3))
          CALL FMCANCEL(MXY(10),MXY(4),MXY(3),K)
          N_ACC = N_ACC - K
          CALL FMEQ(MXY(3),MXY(10))
      ENDIF

!             Use the reflection formula if MA was negative.

  180 IF (MAS < 0) THEN

!             Reduce the argument before multiplying by Pi.

          CALL FMNINT(MXY(13),MXY(6))
          CALL FMDIVI(MXY(6),2,MXY(7))
          CALL FMINT(MXY(7),MXY(2))
          CALL FMMPYI(MXY(2),2,MXY(8))
          KSIGN = -1
          IF (FMCOMP(MXY(6),'==',MXY(8))) KSIGN = 1
          CALL FMSUB(MXY(13),MXY(6),MXY(9))
          CALL FMCANCEL(MXY(13),MXY(6),MXY(9),K)
          N_ACC = N_ACC - K
          CALL FMPI(MXY(11))
          CALL FMMPY_R1(MXY(11),MXY(9))
          KRSAVE = KRAD
          KRAD = 1
          CALL FMSIN(MXY(11),MXY(3))
          MXY(3)%MP(1) = KSIGN*MXY(3)%MP(1)
          KRAD = KRSAVE
          CALL FMDIV_R2(MPISAV,MXY(3))
          CALL FMLN(MXY(3),MXY(4))
          CALL FMSUB(MXY(4),MXY(10),MXY(3))
          CALL FMCANCEL(MXY(4),MXY(10),MXY(3),K)
          N_ACC = N_ACC - K
          CALL FMEQ(MXY(3),MXY(10))
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  190 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(10)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF

!             Check for too much cancellation.

      IF (NCALL >= 1) THEN
          NGOAL = INT(REAL(NDSAVE)*ALOGM2) + 17
      ELSE
          NGOAL = INT(-MXEXP2)
      ENDIF
      IF (N_ACC <= NGOAL) THEN
          IF (NUMTRY > 0) THEN
              NDGOAL = INT(REAL(NGOAL)/ALOGM2 + 1.0)
              DO J = 1, NDGOAL+1
                 IF (MRETRY%MP(J+1) /= MXY(10)%MP(J+1)) GO TO 200
              ENDDO
              GO TO 210
          ENDIF
  200     IEXTRA = INT(REAL(NGOAL-N_ACC)/ALOGM2 + 23.03/ALOGMB) + 1
          NDOLD = NDIG
          NDIG = NDIG + IEXTRA
          CALL FMEQU_R1(MXY(1),NDSAVE,NDIG)
          CALL FMEQ(MXY(1),MXY(13))
          KC_RETRY = 1
          NUMTRY = NUMTRY + 1
          CALL FMEQU(MXY(10),MRETRY,NDOLD,NDIG)
          GO TO 120
      ENDIF

  210 CALL FMEXT2(MXY(10),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMLNGM

      SUBROUTINE FMLNGM_C(NDIG_C,MBASE_C,C)

!  Initialize the constants used in the log gamma polynomial.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: C(0:196)
      INTEGER :: NDIG_C
      REAL (KIND(1.0D0)) :: MBASE_C
      INTENT (INOUT) :: C,NDIG_C,MBASE_C
      INTEGER :: NDSAVE
      CHARACTER(220) :: ST

      NDSAVE = NDIG
      NDIG = MAX(NDIG,NINT(210*DLOGTN/DLOGMB))
      NDIG_C = NDIG
      MBASE_C = MBASE

      ST = " 6.931471805599453094172321214581765680755001343602552541206800094" //  &
           "933936219696947156058633269964186875420014810205706857336855202357" //  &
           "58130557032670751635075961930727570828371435190307038623891673471123350M-1"
      CALL FMST2M(ST,C(0))
      ST = " 9.227843350984671393934879099175975689578406640600764011942327651" //  &
           "151322732223353290630529367082532504853685527501929175190394959855" //  &
           "13457163775826002355076463746499666257062662262326057207404741752905084M-1"
      CALL FMST2M(ST,C(1))
      ST = " 1.974670334241132182362075833230125946094749506033992188677791146" //  &
           "850037352016004369168144503098793526520021594811685953398143623435" //  &
           "02503893967551473165433138415866654683881302547625503436070027398405779M-1"
      CALL FMST2M(ST,C(2))
      ST = " 2.568563438653142846657938717048333025499543078016629393075718511" //  &
           "394606859543769672881862453644508604873305259842023980616399866622" //  &
           "44277379254656124026333871513139276497866889063971918407474980813205213M-2"
      CALL FMST2M(ST,C(3))
      ST = " 4.955808427784547879000924135291975693687737979681726920744053861" //  &
           "030154046742211639227408985424979308247703477010685603646017893642" //  &
           "51133732050087867905480177194587022770925733154718370654381840105887655M-3"
      CALL FMST2M(ST,C(4))
      ST = " 1.135551028673985266273097291406833611416183900382562394838535580" //  &
           "760717957256296912008621311426667275924068293311321808560192355831" //  &
           "19416836702214436017528973257267436070719672792473025777796267055355048M-3"
      CALL FMST2M(ST,C(5))
      ST = " 2.863436640748566190863216318200879836362483388089269737347773340" //  &
           "553638169929829804623296564225284217131985270424264778996969080555" //  &
           "16975632152723012109455292088401608926031282713256898710394755277209657M-4"
      CALL FMST2M(ST,C(6))
      ST = " 7.668248313183240568536426425667994283765150931981520246901951022" //  &
           "878261676510504944242413055018913842306480735586784104096820283433" //  &
           "36343308283287191406517315813519407491295119077092395710810995273468308M-5"
      CALL FMST2M(ST,C(7))
      ST = " 2.138827474304242233565481358155815737009883123125254113877533157" //  &
           "286907184360179941090379654649639056545605878533625329428744350917" //  &
           "29963707838227249505773740942777859121824166285189382035452217278554155M-5"
      CALL FMST2M(ST,C(8))
      ST = " 6.140869564690490872529914712451165067316821654306283177401767753" //  &
           "894821139822105599229903018842906378733862871895584602819330080270" //  &
           "01700768138875158345454772076408505044108823934202891440461609291305270M-6"
      CALL FMST2M(ST,C(9))
      ST = " 1.801262781808533714595890031901700601953156447751725778899463629" //  &
           "146515191295439704196861038565275400689206320530767736809020353629" //  &
           "38073190695949842873953621603334722352596732052178932328832066543779584M-6"
      CALL FMST2M(ST,C(10))
      ST = " 5.370321926785962456620478609033153278577962007833744674032818230" //  &
           "963452927104275083493657782390759550975407401180548454361763677022" //  &
           "25719502127813332819843513403345897099400330508911246024488125358988974M-7"
      CALL FMST2M(ST,C(11))
      ST = " 1.621606923373582198331706449725800346740381669503777534126777710" //  &
           "016401617427540869007126583916228785817454388595224584374431558565" //  &
           "62424003947302186656188945000933064454593249101343833268069567881424866M-7"
      CALL FMST2M(ST,C(12))
      ST = " 4.946423680685744244896356595351648270045350075731820789781699205" //  &
           "590998801613945328568871293350300180372347783056466467421940336962" //  &
           "37993051766799459115354359781619794227922798691721786471256812355243228M-8"
      CALL FMST2M(ST,C(13))
      ST = " 1.521277205034494703893608109526767726401208246782019680016091878" //  &
           "150165553006261855592657131910806289218694348060771816544039319519" //  &
           "70268151503584997891542218258262605500864512242956007183617857332741602M-8"
      CALL FMST2M(ST,C(14))
      ST = " 4.710545468032903448567376337505841863247123878500437995526222151" //  &
           "143756151971538155623134503232957797896283285550201985050591879362" //  &
           "14529603180002288823222154049517099193894892610640999441314930078235979M-9"
      CALL FMST2M(ST,C(15))
      ST = " 1.466896634491983285717977295126452336811904470697069075224299192" //  &
           "939247071986642850302895153896641951172329934777380990036068940704" //  &
           "92049327640255496401589822485890032872392277699930611719523121420745147M-9"
      CALL FMST2M(ST,C(16))
      ST = " 4.590062735154278588407978252478287205347213340644341084689962143" //  &
           "111930858436518669786421571299766271527706531603537599776303515851" //  &
           "07346286911988982005532291493494947163957826550954358278107733999478261M-10"
      CALL FMST2M(ST,C(17))
      ST = " 1.442221874911031367580345522072474831788307406190652461104238910" //  &
           "857027803132223315927216471971838450273509423200923289440756059767" //  &
           "68857423403274844425475990232160708382227977333807182882605816718263351M-10"
      CALL FMST2M(ST,C(18))
      ST = " 4.547809165468029773462079480806624058657045422294384456641178682" //  &
           "659691101645601195358296330035349908208769392426188702234370516180" //  &
           "56888377164494448967006811680025457321926050295945478985302205704218490M-11"
      CALL FMST2M(ST,C(19))
      ST = " 1.438587332730565760193417246729718970937052978750282449255687568" //  &
           "655719501289180489881937394774257940434077254947095313349728878573" //  &
           "86843008158160916187514601728862636178280076324418505631445853717950171M-11"
      CALL FMST2M(ST,C(20))
      ST = " 4.563265937212529367600208240284116509275473219057499398885563766" //  &
           "090388563517935169737964577745895815190180995118452252225828163120" //  &
           "99722426850021624306978846433211406278468453662456948824141382999031282M-12"
      CALL FMST2M(ST,C(21))
      ST = " 1.451073916840454711281216045224320082808166284993163778021566315" //  &
           "255587037332552553079272066121649499425851990060321384124955433677" //  &
           "67207993278060823840271886615031403999360286605743411932260491743002791M-12"
      CALL FMST2M(ST,C(22))
      ST = " 4.624528056444812082051687967994458694688921677653576433580975534" //  &
           "968450053554049283580808625060631085218986361103614972923204710045" //  &
           "87054803107018718005909662543849301887854984204280816281128477629296891M-13"
      CALL FMST2M(ST,C(23))
      ST = " 1.476781612022755183419973250511459966182209498276769571800718539" //  &
           "731445227020086180343751505787486916705816774093130509272233480605" //  &
           "44047487934404934553435264532664986314948239961478420907068538648968405M-13"
      CALL FMST2M(ST,C(24))
      ST = " 4.724507827870744254820277464047378923678173249594725356053537843" //  &
           "069856328276686251584801735153843799291487544822727471278651677292" //  &
           "03640042606725210618105758740751314683086483423378587429797730710810703M-14"
      CALL FMST2M(ST,C(25))
      ST = " 1.513978913019171763487041148572556877568791963643984495346043449" //  &
           "018701962532639151560210080671482947559816365848586253646530778070" //  &
           "18537123717716793256274213468254921216965019092087180696648092689644960M-14"
      CALL FMST2M(ST,C(26))
      ST = " 4.858996725976554852006318671090915519699354289999749614013042503" //  &
           "935984106218549522233834213516072290014623125419558504921174284920" //  &
           "78107160688407534237846157628451620609106027079137343790816294845243811M-15"
      CALL FMST2M(ST,C(27))
      ST = " 1.561654519392582828714942943686889037831993268563190525220343034" //  &
           "296584463347814354645233174014087011036651133377861482059644157593" //  &
           "18350440790655464091846868252936233878726656395056209922850191735361446M-15"
      CALL FMST2M(ST,C(28))
      ST = " 5.025614514474191003429454120021263907058345057163756484675204032" //  &
           "160626854269473972760278335161149373855516115547324556053250680761" //  &
           "35960353957624688528998376864715123324194768194244608331227956016708835M-16"
      CALL FMST2M(ST,C(29))
      ST = " 1.619268063222415588245007073271189318378938728336204453480653557" //  &
           "646832121167874868009309695437374462682430335955717169955384406157" //  &
           "35461655626485084754177259606520323581270018242949002547200222070367875M-16"
      CALL FMST2M(ST,C(30))
      ST = " 5.223211739191891688145554861633126404312733249590231268939276354" //  &
           "483256718224458681957003067572062210319658187678206505904762786890" //  &
           "39036962584739426283467796928036615339321696596679043636072535930177735M-17"
      CALL FMST2M(ST,C(31))
      ST = " 1.686605565375918423742481404695259319639157222117507057462225591" //  &
           "850533083720983505743619854637504171953994487105802289063236646168" //  &
           "31565771482245546699030298377683203899275714959867483319947682463166471M-17"
      CALL FMST2M(ST,C(32))
      ST = " 5.451517284342611284349867604595522641598896765349022422637246200" //  &
           "744471171937423465939660935482207447703449379192989322923063888266" //  &
           "31229372543901089934035916918836653287495719335136501890625959613216773M-18"
      CALL FMST2M(ST,C(33))
      ST = " 1.763692929460761010585031266299209488371094586913873180122443276" //  &
           "358212007822366334544290589814884405499150265809186554929224793099" //  &
           "67159580614375385516830687382686358909934655641565142758100304193688125M-18"
      CALL FMST2M(ST,C(34))
      ST = " 5.710924940930289429222525846887709139266580644631501378754425400" //  &
           "402344096734033498368417722870024289649356569956325790061028789490" //  &
           "90201209337135311180824855625236387835485476090642686407138148182122938M-19"
      CALL FMST2M(ST,C(35))
      ST = " 1.850743092341469168679255116939946915034455019205428394047707184" //  &
           "730616709166372869327273711967997594444383081358747754694208065117" //  &
           "59243078812287887751986055564202797220696526499221359781019466183552083M-19"
      CALL FMST2M(ST,C(36))
      ST = " 6.002362311111352855438751511526624123122881372177829574698482082" //  &
           "268472189497794035844163044963208202280717860883655584808569201908" //  &
           "84840540664364095292197489013673097320200190188150543497148096517991518M-20"
      CALL FMST2M(ST,C(37))
      ST = " 1.948123521680466000278613877664857969433940710389346300112405920" //  &
           "570813255255307791273062459813736293577657819726099571791897412896" //  &
           "04499052083706223596278073126974722712855926508631000226750654451336437M-20"
      CALL FMST2M(ST,C(38))
      ST = " 6.327210499275762922454617564886905374837540270134647521961777337" //  &
           "241208973850468966202699863294902233722101674486074157046009900783" //  &
           "94943168708628546063295850691161365924829349560770067069849204243333875M-21"
      CALL FMST2M(ST,C(39))
      ST = " 2.056336517258456819404872718846500024771970712636992752800634217" //  &
           "390177588265180513218328462256818578504975537617618012477658735253" //  &
           "91357901005319898451317799466679533736509835947206121997945730419581125M-21"
      CALL FMST2M(ST,C(40))
      ST = " 6.687256410663997968707038300952324368643567824691228269512829884" //  &
           "600241200120276396209386234811912763595929600746527477568692380093" //  &
           "65846941054535500737409129245379236794232295378392955072876104303576437M-22"
      CALL FMST2M(ST,C(41))
      ST = " 2.176007902867600569564968838424519571170445852508011400529276121" //  &
           "985778848606677081067574991228552820893084306450020295730062213612" //  &
           "47319852047491615671942139635029927212587004293525094112235574131058548M-22"
      CALL FMST2M(ST,C(42))
      ST = " 7.084666872547745583697885218008349498259999765144828626416539742" //  &
           "112817790036483006355449086393842354631309401157315763399808881211" //  &
           "63034428362049804231045013084677403365104232875460124915055150115352532M-23"
      CALL FMST2M(ST,C(43))
      ST = " 2.307881457325186109702251558024026338622462538196464637417250145" //  &
           "271014677258724350821337105472668901649544195158107332539530013289" //  &
           "58690754615380156771062458199324618311119940721724411263802909784747175M-23"
      CALL FMST2M(ST,C(44))
      ST = " 7.521978024882350363251147127637517006885196169534728483482110729" //  &
           "767330760893859561440994301232670866451144204592673539313561020703" //  &
           "16229680624073031879503528876975581601231458676853213367966509358616500M-24"
      CALL FMST2M(ST,C(45))
      ST = " 2.452817457661126560656022558232578383346781683732614004543641047" //  &
           "950608697083659342320485224577786541964544409466877827518882696166" //  &
           "77785583812635059733861275531662340097681814492066607106278651398864552M-24"
      CALL FMST2M(ST,C(46))
      ST = " 8.002095926418321660462882983562136934827167736984869734245484961" //  &
           "990362339894468819446499270621544724743989273983343985659243797090" //  &
           "02617723679997529884675116233201816776147675793642923747702696525736417M-25"
      CALL FMST2M(ST,C(47))
      ST = " 2.611794321654760040868482915030344896471378127607902926579083608" //  &
           "600355795254414772701179436107007717915331779124504018381582369712" //  &
           "35303338971473297210813756554582658474866145972030534849911654352767572M-25"
      CALL FMST2M(ST,C(48))
      ST = " 8.528305842439874174397891979909201174136636691345390271985961340" //  &
           "951635472717713994160270983358301811012414732109357429143927205677" //  &
           "24451493207732247380061110874133984404716199859823728402868897884262789M-26"
      CALL FMST2M(ST,C(49))
      ST = " 2.785912715941407608332964765121774292928933289953979801676819715" //  &
           "042211815722358756877838540023159292634878096952517082899902453482" //  &
           "75453420374439979087786662261117071847545271472089893478013180932399900M-26"
      CALL FMST2M(ST,C(50))
      ST = " 9.104288632359302486087921738156084451459177031330022615977426375" //  &
           "389281113948847333526780396722062239770555955153203912544147152689" //  &
           "10635505098052873178484650793531962248685195239569728214014565219560910M-27"
      CALL FMST2M(ST,C(51))
      ST = " 2.976401736830878380813487190839719864305643062243346972314080928" //  &
           "571659774700309771395429511816849731798335554291299072632159561785" //  &
           "13853498257646786277128193242849488156599291079190249617494763928122532M-27"
      CALL FMST2M(ST,C(52))
      ST = " 9.734143269753073619444573395909866046087281775327043563820191216" //  &
           "592074666762884461637185638436344417629043795119832288445119716594" //  &
           "54681757554722607199255640890946515559342596226071714786376672991314824M-28"
      CALL FMST2M(ST,C(53))
      ST = " 3.184626928899218981583942072541398262319779552845939844429551688" //  &
           "572650417481499150961066174605543822869303332888433674715027707880" //  &
           "04490731942311456494319892555319423931924782646792891555889559629511546M-28"
      CALL FMST2M(ST,C(54))
      ST = " 1.042241493677369606594579977699800680670958579629702706075164525" //  &
           "911994152904008645633144958538688068689955477642378460321912420061" //  &
           "44713093711611925694102923080704525452321573189336778639403115171327402M-28"
      CALL FMST2M(ST,C(55))
      ST = " 3.412100013470891170294585716908258368140834550044525411190398041" //  &
           "104754921079862028936399162559763428726672165337980689582820113040" //  &
           "13416198464826440959995204574451765227502181269455928284526174312187664M-29"
      CALL FMST2M(ST,C(56))
      ST = " 1.117412841751556843302074248615568735696721287113186281328504084" //  &
           "312562222725968138902580374246217663276066641488490501477225640253" //  &
           "42579986440180416098324496187285052304475776686406646945052809161071102M-29"
      CALL FMST2M(ST,C(57))
      ST = " 3.660490274490043678026226644620183224238038689661082986523077162" //  &
           "981414308819425679643969633533180026092880760915904811622562886923" //  &
           "84487396264603185012812742600008480127678888705848700357490206046685979M-30"
      CALL FMST2M(ST,C(58))
      ST = " 1.199482671814381161791341850793296252666044625258418739613110049" //  &
           "104551626086380287997089537812232630867438710978470875637869385419" //  &
           "01607003246251775392139826953129400440619709892748008039595593739769704M-30"
      CALL FMST2M(ST,C(59))
      ST = " 3.931637604707211006390296608348190956772148951812063256727344243" //  &
           "900822542438555764328768554173091304365555553142603137727525766441" //  &
           "84525659723982712681986023184076666876842449810082867584381453291908627M-31"
      CALL FMST2M(ST,C(60))
      ST = " 1.289061499462497744696303533633690434193534414239284689686070597" //  &
           "676814880055275326028949838642762828605994974529472129232550363697" //  &
           "60950396038294052993668381573708577221742561680239596349647149571718672M-31"
      CALL FMST2M(ST,C(61))
      ST = " 4.227567257903962351790508597715893632841769835136110706800940837" //  &
           "829652461572432184043919015812076564236545988931174492991859433101" //  &
           "25778000859246543191517786955778130308647637177926547762610318839215217M-32"
      CALL FMST2M(ST,C(62))
      ST = " 1.386820999018780266062944049316369528834057757206382128765313047" //  &
           "270244006158501294298057326510552601141951742736581362353221200763" //  &
           "47128739854666116108186922912060771673278920840786086509429347218298547M-32"
      CALL FMST2M(ST,C(63))
      ST = " 4.550506387724437506082055927268146785006102873281969302732936243" //  &
           "298895572505202160358438794407807710437237187496655791039130216893" //  &
           "87511924875421792179416180376854086009363053011270507639350841775560466M-33"
      CALL FMST2M(ST,C(64))
      ST = " 1.493499528613739965372638972965391238482547633295265869054828367" //  &
           "829416466398480374395870941759362578697731625235707888992530344564" //  &
           "64118616870137110287959590352137938213456367349222761768780656871967062M-33"
      CALL FMST2M(ST,C(65))
      ST = " 4.902902483647573629190444872668433339026722693539103782664050983" //  &
           "225694025564768841074155141457059295428474046880275907189034299237" //  &
           "32944103681204447082846734232378539796234753927604216121433267289535418M-34"
      CALL FMST2M(ST,C(66))
      ST = " 1.609908275928169570082165607481456370083831087969406562557256167" //  &
           "364188362846846549047875066952892162692919458759129310778349489953" //  &
           "22770448873819282967830858947780630728913545194644474976588962028982890M-34"
      CALL FMST2M(ST,C(67))
      ST = " 5.287443841783967253336743861164853484418810262590989794938834387" //  &
           "073299404536655202675616502143848847208164288382903044431016858516" //  &
           "20009538336266282148044005430788026679223320414557498337146361309107021M-35"
      CALL FMST2M(ST,C(68))
      ST = " 1.736938072243109805185530725881959995611305822994512775248040487" //  &
           "238177289693647144646295113799732566680032754548503116064306224752" //  &
           "78198414114124592660116717229603109916607439068859873993478598764324167M-35"
      CALL FMST2M(ST,C(69))
      ST = " 5.707082233953717693925573957068774870416069447922801957662691273" //  &
           "270987710445603289113054181853887859043710418144298938936434365836" //  &
           "22910905014974542359781276508954019188261527426571788239315967868621528M-36"
      CALL FMST2M(ST,C(70))
      ST = " 1.875566930504197126372674019737550486762741857855257836323427280" //  &
           "225964790637523226088657691476666774290875901414134481518247331060" //  &
           "71906874659900660807946217987043810912477339050139457949133330400430348M-36"
      CALL FMST2M(ST,C(71))
      ST = " 6.165057963933168318023358517808802281756068067015463002715232153" //  &
           "371564234012532194529099415076087246371078651694171463516031915534" //  &
           "01898954460353148685542639895937235904606585101085654511370630295486436M-37"
      CALL FMST2M(ST,C(72))
      ST = " 2.026868371192165401197185010283739193718905421948964225522412850" //  &
           "262737204848695188406921800082238582127540011428039129914658458542" //  &
           "37634642867581786423872725394317163245280102057930986208119247853710652M-37"
      CALL FMST2M(ST,C(73))
      ST = " 6.664927525630725280315186707639828112376671188813408419458083523" //  &
           "173661970136566062651079390703015968881216092673504456192083877599" //  &
           "15804352132821230683566017780795660498858012885579266098347848714173428M-38"
      CALL FMST2M(ST,C(74))
      ST = " 2.192020608118261066539037933528189571558724587838583580744609163" //  &
           "952034074131032109073883254528947795371825396404707207594960566130" //  &
           "08388120561413633077336889145268058079508281341166298919160705693687869M-38"
      CALL FMST2M(ST,C(75))
      ST = " 7.210594104883918757684569353059178987815466143353338512729476387" //  &
           "590176795023385801954403484227849398454243048187188857331730164109" //  &
           "61130821394037069074892323683471141755158288684895958165035966426280590M-39"
      CALL FMST2M(ST,C(76))
      ST = " 2.372316675010334396327093341566291045560598544381746314200798914" //  &
           "361335388290174563770200693760939384221618459565684611293948349762" //  &
           "99760715272696457406804603245507893941691344795060823654575302742922341M-39"
      CALL FMST2M(ST,C(77))
      ST = " 7.806341195078978788896796569353844545546683670636287097360101000" //  &
           "674819867286255396945996712341079701193140494784764939652507657527" //  &
           "10776509577749635871445825212032538401033636476215858432876024174293608M-40"
      CALL FMST2M(ST,C(78))
      ST = " 2.569175583075068496597802624641121843008905917144485747977702000" //  &
           "084716234785945953181138133105853830584529894491272387561559175918" //  &
           "06056635101515773378678277304459406269213364274389438074596955963853941M-40"
      CALL FMST2M(ST,C(79))
      ST = " 8.456869627337005555483543945000411129981141780087868584229158532" //  &
           "975156474362982393176600086105992624536940685475408780825634971862" //  &
           "82928746682884903136048829786952212289839035702247341347744500864695940M-41"
      CALL FMST2M(ST,C(80))
      ST = " 2.784154609752488682347830682317399839437808496760216533359957844" //  &
           "108419936184123983123382869988781032193042150722634481595940148079" //  &
           "47471887024692990616525022991390619756225607915282611367773077871045722M-41"
      CALL FMST2M(ST,C(81))
      ST = " 9.167338349011185351110270732877594395344097568799016206592150045" //  &
           "211148681681966966935859814202597754161577071431235188923674247559" //  &
           "93097788462831296346214350780525347998671878506396254329112437194116937M-42"
      CALL FMST2M(ST,C(82))
      ST = " 3.018962829751912207860422524685002365325324396585948666388970238" //  &
           "468900826307200635678324473325455973126327818463443937367696783407" //  &
           "31216822788529908321030119923263022821575928202721712476375757572174960M-42"
      CALL FMST2M(ST,C(83))
      ST = " 9.943409320108569385064969631771335447816687861933253786118347289" //  &
           "942417796521205611575152291429262471820315825809582098019324351274" //  &
           "19736007449168383766428225696036997597572676990996005739875078089934258M-43"
      CALL FMST2M(ST,C(84))
      ST = " 3.275476011303678103893551709440913333007415558028940493123054436" //  &
           "102576111138063293549771997379461070822607505834700351586923416924" //  &
           "36130515829060972565436000080016114747837549174742969323583514548979337M-43"
      CALL FMST2M(ST,C(85))
      ST = " 1.079129693640085894851434181915227380977317782277815512357487683" //  &
           "235378082315776477576402633427103515916665364561427248912637998286" //  &
           "84497336603622824659763899982809691142662878118295489894583048485526949M-43"
      CALL FMST2M(ST,C(86))
      ST = " 3.555753013510719060083393999120543994047683529242126292544972222" //  &
           "356576357296720319402739291176562890757829588273748247638519856640" //  &
           "67480610895714757344602846046498898788815371622608783151322531494411842M-44"
      CALL FMST2M(ST,C(87))
      ST = " 1.171782243084804960250703836375832017652851729860152530408648875" //  &
           "579920584019031061094655011368498851035103058003948560022564877979" //  &
           "17046691205570131651535433024120306105982182247803947139577240760760240M-44"
      CALL FMST2M(ST,C(88))
      ST = " 3.862053834876472821052586382551496075098455346933359196501245526" //  &
           "180729685642432233621061970152222441386142907366726076208958805328" //  &
           "70627198276478969445218445704378235779473329084661077747371015095013204M-45"
      CALL FMST2M(ST,C(89))
      ST = " 1.273047375197605743053349834215496117644063043473174035166207181" //  &
           "768692059655110422250456339545311971995535730478050915338536261677" //  &
           "65444071293612365602178273688293636174364964871538328370023554265881805M-45"
      CALL FMST2M(ST,C(90))
      ST = " 4.196859478667449972989243439715623246016537566511261080114559563" //  &
           "758135987775813449384022624930316510736358259866183402630340459291" //  &
           "72668526906989173105067164347823743598675043393233726870416684054894389M-46"
      CALL FMST2M(ST,C(91))
      ST = " 1.383747146950471434825293091024083280747013661478053604841911598" //  &
           "022310276318757265593640560735257184999994184693906229160627176389" //  &
           "15615636692838842344470182112269725385554047172321124664394879685510285M-46"
      CALL FMST2M(ST,C(92))
      ST = " 4.562893817897541724781950430442496378260377512288497109803666040" //  &
           "621854629198431249084998659248405128970881921614143709856471314566" //  &
           "71187340235712918767373708561614114398918238461996856137728111624669887M-47"
      CALL FMST2M(ST,C(93))
      ST = " 1.504784131433391793257248428485220036318516611757628814861273953" //  &
           "241786001999746507693533747202375916340889614020197821617964401281" //  &
           "09223232430673952549932990159444752780504141754928914375319317506618075M-47"
      CALL FMST2M(ST,C(94))
      ST = " 4.963147661567547539987958807734897162902530398116312148944960055" //  &
           "796268377992054321453624007551095855206124470724586679336263613115" //  &
           "89091300344790164050318056261092913116282166570730942288235067198317928M-48"
      CALL FMST2M(ST,C(95))
      ST = " 1.637149402252630927456890226306016168030553715912510338166421018" //  &
           "082250171874970269061229738963393420354696075864382273796383465054" //  &
           "54500820509537840793087307985392209620656037379007014938893681782187286M-48"
      CALL FMST2M(ST,C(96))
      ST = " 5.400905244543393186874620589431418037639831941879628434368248998" //  &
           "722906347036053715166862260092049668825631280975790017953688626636" //  &
           "15617148734065300820447033765217546328421226646834925099741473350545005M-49"
      CALL FMST2M(ST,C(97))
      ST = " 1.781931322178944118647377674747852837095476406297808108639777839" //  &
           "710695191748931947375083891115191186633327477558489708276330078077" //  &
           "84355945700557484952518727190790584478520941307748997078811897348262566M-49"
      CALL FMST2M(ST,C(98))
      ST = " 5.879773386313522948360905422632431077145168395771420120384935880" //  &
           "807587039058575601777312600437920559716301465154041152414856698220" //  &
           "03215044402130085493835602372831521053966369492752843371900098859590805M-50"
      CALL FMST2M(ST,C(99))
      ST = " 1.940325217483255139116418908036918257576300407306275798031334364" //  &
           "409600080862034828483316076995017274377332356098893076821893973234" //  &
           "40382503373462356947628021148992374363794778068565614630007284818985556M-50"
      CALL FMST2M(ST,C(100))
      ST = " 6.403713589053133873710962618711106281414140108122214547412024301" //  &
           "692917083153216025874647060197659674432573844849077628829246217574" //  &
           "60249742706649007645817489620506941120553080709097100788465375868770515M-51"
      CALL FMST2M(ST,C(101))
      ST = " 2.113644027759240612416047225977851974737748960538792675174058961" //  &
           "161021976645079782965719680309463063367873086114357869089879642560" //  &
           "91741774488093918815994311848793042306225840389086585033999559719325383M-51"
      CALL FMST2M(ST,C(102))
      ST = " 6.977077373185528123141201643198765846940778946562652437177053180" //  &
           "068202610894634761485494475059094517798144344611412525007119040744" //  &
           "47619138364742657951321893292966244857904586187576743960389378098756527M-52"
      CALL FMST2M(ST,C(103))
      ST = " 2.303330030250272718045226178470266630636882414669936525013604526" //  &
           "663877295122333165527547705932002051525319167095677157791507288736" //  &
           "64026571366295683502084852088545059435402597033343652161501470364354290M-52"
      CALL FMST2M(ST,C(104))
      ST = " 7.604645179238802714709378571216485567328230281028063584140295724" //  &
           "376585957946481904384646230934323963536666254241381678030728396763" //  &
           "58178737195689600607109967321748886837389095954530241223910865091320330M-53"
      CALL FMST2M(ST,C(105))
      ST = " 2.510967747861821044453112929896090858886255004999542279673792434" //  &
           "537208309423086735805390141224743694821441092754412288490320556507" //  &
           "90129078122051056660926795028903236517945321893911606739679056323908271M-53"
      CALL FMST2M(ST,C(106))
      ST = " 8.291669198546705061835867356103780154660596199979498029660459478" //  &
           "188005146704679300246430992623307755626947749069085299081605791921" //  &
           "74019998848391467377542111031322996337770209654839668772234104877607677M-54"
      CALL FMST2M(ST,C(107))
      ST = " 2.738298161248419586570035179837181952253572874107137240705415238" //  &
           "006265247194645939919863261646299211192029639655806078478918931977" //  &
           "00593190887265237361465699889511989159926295238433577049420127008639310M-54"
      CALL FMST2M(ST,C(108))
      ST = " 9.043920532563515029285236381202044755606494352793597664587001938" //  &
           "974036674170391738386361814188362066318933595609565234700634164118" //  &
           "70487847122956051574723330382189769818956900012640799709823774379723508M-55"
      CALL FMST2M(ST,C(109))
      ST = " 2.987234357725506677158331533521097034607039220763832901730464971" //  &
           "787758443274701147417241273125203557371693807601776736455273977366" //  &
           "05803089099448475489215616434095538698506543339535511526713482057801648M-55"
      CALL FMST2M(ST,C(110))
      ST = " 9.867741121615743267416100354228158234050959334204884456346592478" //  &
           "319239204688512319808683885900238794688432245788378036339502910311" //  &
           "95256267366461853050971201508370739481479021507935296576188223198846781M-56"
      CALL FMST2M(ST,C(111))
      ST = " 3.259878763390904147253084369202388245824453621447959285540458582" //  &
           "736012876275248713545534462435398177897630905297446122213135317168" //  &
           "82679622090783961983706156224847005117892642811444666243882001638133683M-56"
      CALL FMST2M(ST,C(112))
      ST = " 1.077010092919706009412412261729007614456466446787757717767031010" //  &
           "139026034411384632610387807051427675075586800080476142293720773922" //  &
           "20272670887152593395631417854201652294534598158204732976231392133784649M-56"
      CALL FMST2M(ST,C(113))
      ST = " 3.558542119880893194099946299932858465278352616190909402621150713" //  &
           "569585013772896155460681917876143100438779403865861290242704606012" //  &
           "88545096496889803741553260243082158570103416935736970008787448980973697M-57"
      CALL FMST2M(ST,C(114))
      ST = " 1.175866091786728245112848768043708292858352425382756439383320272" //  &
           "578862130652141603853077265569964741389836105657992359851950676535" //  &
           "04338748169984545024264075261487170842373741152762109550508117751868574M-57"
      CALL FMST2M(ST,C(115))
      ST = " 3.885764383777977106238860501037171757872128590510931610888692650" //  &
           "910285334868201439319749230933970529786352570062128138073985716014" //  &
           "24775116761936632328170508559558303761107738144547614997375397249406443M-58"
      CALL FMST2M(ST,C(116))
      ST = " 1.284184240792720746713802455067655929838349505783919451560002320" //  &
           "126603715783906155689196669126320213369826150934215436793861556561" //  &
           "85972584789613262065820990433273679374866590546774541475968702317884481M-58"
      CALL FMST2M(ST,C(117))
      ST = " 4.244337744992888045444183568112374868793650501308024498979691533" //  &
           "890762987246535790550234061933625766554201413681894303054318079987" //  &
           "15016824304537959514257416420938167100044999559848904364273216745412168M-59"
      CALL FMST2M(ST,C(118))
      ST = " 1.402890347084483369665088764818551961487912000082962187928218261" //  &
           "785089551297393342668713450909249794752403435631477836303274921114" //  &
           "55362046946784217832779503423966063510800253866244156993968505509787734M-59"
      CALL FMST2M(ST,C(119))
      ST = " 4.637331980640374010893092224461382977846892054658359797965131441" //  &
           "456865261185257586124099243621668370219467668437596174204742705530" //  &
           "83668639386769597559944266590300361763651777592096905886759195912398458M-60"
      CALL FMST2M(ST,C(120))
      ST = " 1.533002307649710027030813938667476629444482989777771349009392774" //  &
           "898001472950267391452028201567335345539750749325735733749863033002" //  &
           "69548753895579308292780126786018722213349444264135999363029954583214006M-60"
      CALL FMST2M(ST,C(121))
      ST = " 5.068122383213521254015903956436646089688648450514341540538884175" //  &
           "869469050644753957457320055403310802554634158264858859825705672789" //  &
           "91662731199394270951038339532656379827609414997054764442302975300731162M-61"
      CALL FMST2M(ST,C(122))
      ST = " 1.675639378731841475760363897846833007051598415558153712467179977" //  &
           "432925958506220080221518370999946501032024092613108461163906099341" //  &
           "82913060234132996850622927932164528031237880466865308048851412708423659M-61"
      CALL FMST2M(ST,C(123))
      ST = " 5.540420526452055898116485439538895779663375849440317942175183295" //  &
           "232404269513749797742436944684450241469393462030971608777003451754" //  &
           "24373370189591105970695273793446260615786377827169998532420885870021362M-62"
      CALL FMST2M(ST,C(124))
      ST = " 1.832032387413479669586574511502151686557921294324566012744100766" //  &
           "650543443902590363016386993218294974037346220641170740186656299364" //  &
           "35042400328594920807120027126914313294894904628053322733053994091135028M-62"
      CALL FMST2M(ST,C(125))
      ST = " 6.058308159436109652922013909249826504043342021872382519752917607" //  &
           "688622870375413804432273155114051562335231336139713200850863559312" //  &
           "92844948695425942146384859065918295663347978412467638683301971137894309M-63"
      CALL FMST2M(ST,C(126))
      ST = " 2.003534981860760581951442341610686092565547269730489837317963845" //  &
           "620455952950683642101005961045434369116586359058385158147557980542" //  &
           "11638367930496346396938524050401454422736023568038546321742832881166705M-63"
      CALL FMST2M(ST,C(127))
      ST = " 6.626274549383244408116266205294449505997555094384610044245319246" //  &
           "030550314415978028317676220529142615194623162680155029534077714720" //  &
           "63797428182476797631017411639144206566456794143210250637764789933866701M-64"
      CALL FMST2M(ST,C(128))
      ST = " 2.191636026669393443536074125395262735461777636080096892966692835" //  &
           "413006758477349117295402033106637857465621733449271837481434101507" //  &
           "59193133722553066298412025354918228064474978225340425161474540247412694M-64"
      CALL FMST2M(ST,C(129))
      ST = " 7.249257626675685867142261929211249822372411007081385395432760399" //  &
           "456066369908496028419563900512609131853389393280859633882497500591" //  &
           "28369747912197909028518904445517467819162688494876358555633787825066496M-65"
      CALL FMST2M(ST,C(130))
      ST = " 2.397973260732415138001223113122005679063725787264243397335901198" //  &
           "698321759116583811702053382300628630291425464911847398771216167493" //  &
           "18664813035306053986440746004247939745287869514463237143921611339050020M-65"
      CALL FMST2M(ST,C(131))
      ST = " 7.932689322119858073349921363235515368474949336836316506068072766" //  &
           "546624999168511473276482943408448126386413211956579156398290157039" //  &
           "88529011287347733351088257288781160869062308105566110665892110886142277M-66"
      CALL FMST2M(ST,C(132))
      ST = " 2.624348347167471822677772594888450230485642320545658876475286310" //  &
           "055771115343647866370773378641890749683248791810064619124781313040" //  &
           "39655556535083837498441324798836325222767712349195699260834872702802847M-66"
      CALL FMST2M(ST,C(133))
      ST = " 8.682545526698351997704083743124967023750577000605836108664866495" //  &
           "160128302689923574758622135053769717872441667362682928425542104432" //  &
           "66558278030150559101531475636719213636150787481043024038745922498650758M-67"
      CALL FMST2M(ST,C(134))
      ST = " 2.872743458216244845484563580154599626121124250656149184804291244" //  &
           "427536302457588668570923419780410649470498203175250876036620935134" //  &
           "64093415505201626499567870169234200516491544807537627932184985980888123M-67"
      CALL FMST2M(ST,C(135))
      ST = " 9.505401148509633647613758343551012792444980481501235466486933914" //  &
           "265907192633387738944843898328544931131135589195243791819300008378" //  &
           "30106163188879275241944204168059306729141838253133216063222884137562944M-68"
      CALL FMST2M(ST,C(136))
      ST = " 3.145339552791508936206896052221534466211147212254477273131941831" //  &
           "512277630670890754387204205624927221141107680833883854535942453092" //  &
           "60656013674364311521617359444957869733091866905428746679446531471773156M-68"
      CALL FMST2M(ST,C(137))
      ST = " 1.040849079063856819895496485119313778206922325391046796592400903" //  &
           "438704272609812766874547219070728506140021601572906667448239089200" //  &
           "17410157182972609752570220147431398012450147044399178635448118006623721M-68"
      CALL FMST2M(ST,C(138))
      ST = " 3.444536520642979399033002670688986667656937918697573314979061458" //  &
           "919964194677753822933252127653285396401597050249715424969440622451" //  &
           "73896561346967424455299327210221686160915541794905027119226479284135693M-69"
      CALL FMST2M(ST,C(139))
      ST = " 1.139977562784224133216265743521281955028156700765266508712800967" //  &
           "848831883748374530246175200161064830753252846881090666971108606926" //  &
           "84608508079113604819022125345471848395222113457719457394701080327610407M-69"
      CALL FMST2M(ST,C(140))
      ST = " 3.772975385101451028879155185726947819831206176127486435982734889" //  &
           "252802708561571080135558366656253315506022065599884895931599609988" //  &
           "98731864477490133149597217959601291802546927566215903357984266276909864M-70"
      CALL FMST2M(ST,C(141))
      ST = " 1.248801711970198579225912046586232268562301776030667048611621632" //  &
           "465628194502888533349817467748088996697006070450714611279370044918" //  &
           "58919189486279287758063253685579639080251285807174486835285307209688606M-70"
      CALL FMST2M(ST,C(142))
      ST = " 4.133562776218372917707434109108607724393370061767622907143777544" //  &
           "878888798728408345872714164317266154909605719593487267350335578853" //  &
           "93934162223974482538311480054078977999291506457599828857537063130739588M-71"
      CALL FMST2M(ST,C(143))
      ST = " 1.368285826387100294053441225148146980381219160778152569563356532" //  &
           "725964967014895839699806485363534911338607141236453063094587484467" //  &
           "66893839721308893253877636532577439328086482952402541207429714733339152M-71"
      CALL FMST2M(ST,C(144))
      ST = " 4.529497908040056144676469409188309794321421988959637669575342522" //  &
           "116514782664298860106021153887341542873949848678983271520138033675" //  &
           "79495262187662834453100157981375055331539396277574179995789688030466410M-72"
      CALL FMST2M(ST,C(145))
      ST = " 1.499491316588603061306884181776653655545748070661401693040695069" //  &
           "053696590521220167504120163356893714569898088385033607285458815124" //  &
           "35872442007725124320485376254435248066997500475451183595538404386947583M-72"
      CALL FMST2M(ST,C(146))
      ST = " 4.964302317957733490235530469745361589786652164074076392491530822" //  &
           "224428024784809035798407669056637309772933378074926314581475254129" //  &
           "86616192319821312175154821971972127073013580628971532023157018109185362M-73"
      CALL FMST2M(ST,C(147))
      ST = " 1.643586578242763114833507846160154608374622266203265530676336751" //  &
           "694818277497961440503117616606491252858317008970780154993421364483" //  &
           "78099010504715300393033107665724003113140293042459780949957368683890959M-73"
      CALL FMST2M(ST,C(148))
      ST = " 5.441852652794830894311470706477538221849565053218214215349180811" //  &
           "064294636030553844028448630827867613349267037250477900533924480506" //  &
           "82460851565237813605665759673888889710736177209599734575799006709861746M-74"
      CALL FMST2M(ST,C(149))
      ST = " 1.801857878369844007118484883880761798414766692597217574900046197" //  &
           "360262152730609587228705384643316107409543482294232641569660175011" //  &
           "79519154781224338457932814908298988696375455365226741142441036489254992M-74"
      CALL FMST2M(ST,C(150))
      ST = " 5.966416815794185453763830530254737531064047428340471319398688069" //  &
           "007552470288020366752010161084016011730287854271568912859552337203" //  &
           "41453128747443716412531087761908717466819377449540627696187031359721010M-75"
      CALL FMST2M(ST,C(151))
      ST = " 1.975721357861671060279948825620105307975444162740595055137362881" //  &
           "848246417197234999961571849669754470375412457219647332906833168922" //  &
           "68071772240997929407621098946132582262214118221958928604276333611006417M-75"
      CALL FMST2M(ST,C(152))
      ST = " 6.542693821241263641717538449532346497940622472878710087928587547" //  &
           "135890790293782933631752384033436259896798958027902059478261401316" //  &
           "51898760968041471004067759299223993845873964885693255185054469449588983M-76"
      CALL FMST2M(ST,C(153))
      ST = " 2.166736265476002894293516585134968365492470343594290484160425853" //  &
           "634757103189631345447367869704134537187600769397997799583921963715" //  &
           "66991272921221434244459511112119776440312273606272563588060032932425347M-76"
      CALL FMST2M(ST,C(154))
      ST = " 7.175857739425902058446366213267163717870002184056285139492810907" //  &
           "165151388326241994224874557956711286862577806517065215553936524682" //  &
           "13250466673505382651861797375923534026910443196181011966541606084919661M-77"
      CALL FMST2M(ST,C(155))
      ST = " 2.376619550450886365485623751880291284047099214872959375092451724" //  &
           "342950586001550942436415678973426556922327249108654045209317268184" //  &
           "82082947451080863768578126711240615665895962605984771627264126442044223M-77"
      CALL FMST2M(ST,C(156))
      ST = " 7.871606154359623630068573317612764902762392638995019862408631291" //  &
           "008061939353640535233024537980362873819209185759931590349366135182" //  &
           "14586054502659862300174333186280592070419277216786077915707312111770211M-78"
      CALL FMST2M(ST,C(157))
      ST = " 2.607261954081141160150797999367222244498208263011867403220673096" //  &
           "800582313576057737826578818707489920864730296871775420774049041364" //  &
           "79431809641123667403151516016216413258555378265536018764789936965838617M-78"
      CALL FMST2M(ST,C(158))
      ST = " 8.636213600520341807753750476732474442820999819309686673192125811" //  &
           "444182648267187160168499080611902341484327275294303024770270222135" //  &
           "59223769662625003083305590998863860730206646251762971580585644914132026M-79"
      CALL FMST2M(ST,C(159))
      ST = " 2.860745755172363223898311967180521130986380848162555027528393662" //  &
           "915983444061966784564310775020004680613450195420353857249494047775" //  &
           "73818249551580545509645629320363387364925214204842915131007525800466880M-79"
      CALL FMST2M(ST,C(160))
      ST = " 9.476590493324598214557576012349336522703693594124571054279111616" //  &
           "798531592192686914431167230715058815630334239433475110837262090138" //  &
           "33301645943249512127076707890530532222551337127599066189782985037412169M-80"
      CALL FMST2M(ST,C(161))
      ST = " 3.139364340381194056746408425980351704553046571903970795447364975" //  &
           "882887158738580119004810652625014397096678884159435971472598704137" //  &
           "54339572525201214956186891250608510839418632104354221549889413573604846M-80"
      CALL FMST2M(ST,C(162))
      ST = " 1.040034812150826026710373679719002963399053176200107466780745572" //  &
           "000065388516270021753222010998681619656807884101412895225381983273" //  &
           "03585371769586543699202508103518904235084186177627200599704020839402387M-80"
      CALL FMST2M(ST,C(163))
      ST = " 3.445643788223265129236923220146442325334098948153988879546081638" //  &
           "282932221486522368247949883425191269877715114051526159278376683546" //  &
           "54478379087683377717562788666572331232941361970188301510421846831598762M-81"
      CALL FMST2M(ST,C(164))
      ST = " 1.141587032865869378324299022323731794401778416670178670987733185" //  &
           "814446261010130113286467916958273916598065282809528144498877625408" //  &
           "51786835211126241486118129987351739916755135907809166050772678111097018M-81"
      CALL FMST2M(ST,C(165))
      ST = " 3.782366675158000172438186031560392817938018879105893232376963665" //  &
           "758045582935962507746785156805849115039876109316259116490068195664" //  &
           "22154600940352929680886350387924367311407243398433694317491879496500728M-82"
      CALL FMST2M(ST,C(166))
      ST = " 1.253239257637571101011205444147249909041208363526150999161557930" //  &
           "530122175707904339132697555874221097743565894565280785124100424739" //  &
           "12341727673603475533755872933253879415498534305672767854397632835354647M-82"
      CALL FMST2M(ST,C(167))
      ST = " 4.152598333838791095457780574441131208990623618377645706663873970" //  &
           "409219842573498216786365580094065012089293058629821760308697172924" //  &
           "25649947447826939431820718042482922400940273337175959594512550863844028M-83"
      CALL FMST2M(ST,C(168))
      ST = " 1.376008915348001441142775089390756952291415569331728264389528531" //  &
           "840532583703944779638084800244642630871018954424019239838171107774" //  &
           "08455142709222529998017403722473530832343485779953472597270935996990435M-83"
      CALL FMST2M(ST,C(169))
      ST = " 4.559715817525503407894512559569944355041250549999373143331298588" //  &
           "042902011136449984273691723063087171534265607563490346238292992622" //  &
           "43694334064073133108518335548127416155718965768703096927698154664008186M-84"
      CALL FMST2M(ST,C(170))
      ST = " 1.511016937693380717359102098553590521576253151812935620881255183" //  &
           "676747573506694189735416802045982192198119977018955499638830448943" //  &
           "99139906962195905603908709462882471176378436053656496936740374119626233M-84"
      CALL FMST2M(ST,C(171))
      ST = " 5.007439851661721308524939431791288925473304269198627995505717966" //  &
           "042698143618956580402359781596868665105789229557574207071195578504" //  &
           "34784490998580851675508563655209200033656690762499193037679456058834741M-85"
      CALL FMST2M(ST,C(172))
      ST = " 1.659498369200591197022274845367449670587666822869779371596678693" //  &
           "434849663818573237359515507605355977016921178742109368988856546946" //  &
           "53987167820516459592809407463255771494757163532540963217681912265486931M-85"
      CALL FMST2M(ST,C(173))
      ST = " 5.499870074123138278587253453863123198782240874965235399880295435" //  &
           "189868323747896824633224719144787009990778913499316775325760318709" //  &
           "84151200565534853443611640092085568829894647596561585415506088061475496M-86"
      CALL FMST2M(ST,C(174))
      ST = " 1.822814104787822173171970582684126136729373783986451680232463776" //  &
           "995284747235440436579147791215434063408671873139502336867256882493" //  &
           "52907684446136684618467464533340860151713404827732293178363163584088691M-86"
      CALL FMST2M(ST,C(175))
      ST = " 6.041524022149206216514749478207951898414628220569359023936273098" //  &
           "431129999705544414314317322005670901754042242107672777454004410860" //  &
           "23728922114946882252911249911639377024536952401619252235639914064675297M-87"
      CALL FMST2M(ST,C(176))
      ST = " 2.002463416328745336974658582823885810920380274053175588795927666" //  &
           "714050828711674000372182654975669270167646317724425684411171485274" //  &
           "63888736139146918349514677140009027878433312540016907441088200168772901M-87"
      CALL FMST2M(ST,C(177))
      ST = " 6.637378732657623720229439419358377518409139927005258254140285180" //  &
           "914849963099373381335080302807182958739736822642496046941955995156" //  &
           "47580543357281795983541588632113533651700800850371216214989020246965499M-88"
      CALL FMST2M(ST,C(178))
      ST = " 2.200102767530555026604991024957956952823900377482112800591761008" //  &
           "660084843014248387788035100004987819764909462237889078992186315330" //  &
           "60158332375064476625052936559137839280476740402400480416136592265795082M-88"
      CALL FMST2M(ST,C(179))
      ST = " 7.292933318740382159132169272746181235841016457994964554263454208" //  &
           "492201073732473833848574569619679758849986320998185690723690770932" //  &
           "41462402016226262443880472321890006685113024701301315267987027615012503M-89"
      CALL FMST2M(ST,C(180))
      ST = " 2.417513092202831923304643749980362419472891672259150932920867637" //  &
           "832384236931175709234025174145241123797309720555077484497334156484" //  &
           "70273284353674851435852469743857826682908838509841004308849506350039697M-89"
      CALL FMST2M(ST,C(181))
      ST = " 8.014099523920820532691698949493969623669374514095447937521752186" //  &
           "098803953643697356090844942590659454999844318173870304557001940555" //  &
           "42525475893078866822127874440270390910047929036227162217460866884004221M-90"
      CALL FMST2M(ST,C(182))
      ST = " 2.657079598189421611225399332494614836172370837056707080573411081" //  &
           "077314111961101307612637519570469581636094934124837596398237792257" //  &
           "42836805118176886050043438632186029854385799944806006391590874149011746M-90"
      CALL FMST2M(ST,C(183))
      ST = " 8.808801832338891822547417575762619398673718065912507825752674890" //  &
           "111044965171749095170501464417801064468367983054179217820785485005" //  &
           "97684256880162841894825031174637280632623961080123218247631394854404406M-91"
      CALL FMST2M(ST,C(184))
      ST = " 2.917871827472767744801985504187470385461224411949197505438209784" //  &
           "552995184888997609387528889784137922680585612807183342956402094608" //  &
           "01943529407670923901992925226137852941513101357366936300944346484030974M-91"
      CALL FMST2M(ST,C(185))
      ST = " 9.673909686763801191886681572020182885252222531903039060187477306" //  &
           "277095817639315200476096440352681128855627885542339760940019503157" //  &
           "11902076792371625818162067652299279665268178569377108196296372219380510M-92"
      CALL FMST2M(ST,C(186))
      ST = " 3.225317339965144219466139388048879231367171359221844461940917969" //  &
           "250112356759739827102732375554724422636709224874307206769709880215" //  &
           "16808317218174915989091357841379986431733603301918969106178493014736385M-92"
      CALL FMST2M(ST,C(187))
      ST = " 1.069410576590312480755235192735211157086107353088423400669923656" //  &
           "677428694478915259443669100953558846603833879903904822845298497420" //  &
           "71143211982703738257538091316334035362562254796294606412951506359364815M-92"
      CALL FMST2M(ST,C(188))
      ST = " 3.436230247075590625300285568770398628374034305782922085529568066" //  &
           "268230300872273810320168642643487560176713818087616304872506761508" //  &
           "46310739710431810178913824922315129263580429076241251322833952169352928M-93"
      CALL FMST2M(ST,C(189))
      ST = " 1.139260014627810596016761865641404521146852141822567620126418127" //  &
           "549772266075482248466335323616377804969776249070578213315603095439" //  &
           "66803478295001515092190429139446518480363880483757347561526976211621457M-93"
      CALL FMST2M(ST,C(190))
      ST = " 4.343070458433874603042514728830057473477940614398096372223763568" //  &
           "372476710887610850250214061613248238834003723272762897566055896217" //  &
           "49542103193171278645483518744915143059042740324768324837960269146747307M-94"
      CALL FMST2M(ST,C(191))
      ST = " 1.440667367566540553672654836910082201055609375400592746447756244" //  &
           "176284526124155943246633922603816785133029633668454281491778597874" //  &
           "45028943290797079216498815025085467176654584077190605132981537168513742M-94"
      CALL FMST2M(ST,C(192))
      ST = " 2.384816222561370113514027430584139466337689187141928475880205055" //  &
           "969980605519512189876139389796445441195296187421190296481218115986" //  &
           "36517542721807591974162349259651427967273755554095270040863620533051177M-95"
      CALL FMST2M(ST,C(193))
      ST = " 7.891080859500628750618564157769100520532680589132484946442038829" //  &
           "299825933371426348457257350822254140519284945880389565858120333678" //  &
           "80845449586455112014663973071181240064800376338635797974159751681049875M-96"
      CALL FMST2M(ST,C(194))
      ST = " 1.059066626703749406081450817511793688125712964427249229411570236" //  &
           "550112600348902027013047427990927965524515088187243012802502472093" //  &
           "22261048153901594967680556527547070843534915035469463356163636215773539M-95"
      CALL FMST2M(ST,C(195))
      ST = " 3.516498583838005375775193339944623281315914658931041130875008620" //  &
           "636983702886095342086986357214180450129972642316547945003279723791" //  &
           "81126461652035134771983947459420613651499692168433361308886876030693800M-96"
      CALL FMST2M(ST,C(196))

      NDIG = NDSAVE

      END SUBROUTINE FMLNGM_C

      SUBROUTINE FMPGAM(N,MA,MB)

!  MB = POLYGAMMA(N,MA)      (Nth Derivative of PSI)

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: N
      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: IEXTRA,INTA,J,J2,JN,JNC,JSTART,K,KFL,KL,KOVUN,KR_RETRY,KRESLT,KRFLCT,  &
                 KRSAVE,KWRNSV,K_RETURN_CODE,LSHIFT,N1,NBOT,NC,NDGOAL,NDIG2,NDOLD,      &
                 NDSAV1,NDSAVE,NDSV2,NGOAL,NMXDIF,NTERM,NTOP,NUMTRY,N_ACC

!             Set the coefficients used in computing various derivatives of COT(Pi*X)
!             for the reflection formula.

      INTEGER :: KGCD(14) =  &
              (/ 1, 2, 2, 8, 8, 16, 16, 128, 128, 256, 256, 1024, 1024, 2048 /)
      INTEGER :: KCOEFF(56) = (/                                                     &
                    1,          1,              3, 1,     3, 2,                      &
                    15, 15, 2,                  45, 60, 17,                          &
                    315, 525, 231, 17,          315, 630, 378, 62,                   &
                    2835, 6615, 5040, 1320, 62,                                      &
                    14175, 37800, 34965, 12720, 1382,                                &
                    155925, 467775, 509355, 238425, 42306, 1382,                     &
                    467775, 1559250, 1954260, 1121670, 280731, 21844,                &
                    6081075, 22297275, 31621590, 21531510, 7012005,  907725, 21844,  &
                    42567525, 170270100, 269594325, 212612400, 85630545, 15839460, 929569 /)
      LOGICAL, EXTERNAL :: FMCOMP
      INTENT (IN) :: N,MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(13),MRETRY,MJSUMS(LJSUMS)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      N_ACC = NINT(NDIG*ALOGM2)
      K_RETURN_CODE = 0

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1 .AND. N >= 0) THEN
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          KRSAVE = KROUND
          KROUND = 1
          NDSAVE = NDIG
          NDIG = NDIG + NGRD52
          CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
          CALL FMI2M(0,MXY(2))
          CALL FMSUB(MXY(2),MXY(1),MXY(3))
          CALL FMIPWR(MXY(3),-N-1,MXY(4))
          IF (N >= 2) THEN
              CALL FMI2M(N,MXY(6))
              CALL FMFACT(MXY(6),MXY(7))
              CALL FMMPY_R1(MXY(4),MXY(7))
          ENDIF
          IF (N == 0) THEN
              CALL FMEULR(MXY(2))
              CALL FMMPYI(MXY(2),-1,MXY(5))
          ELSE IF (MOD(N,2) == 1) THEN
              CALL FMI2M(1,MXY(2))
              CALL FMBERN(N+1,MXY(2),MXY(3))
              CALL FMABS(MXY(3),MXY(6))
              CALL FMPI(MXY(7))
              CALL FMIPWR(MXY(7),N+1,MXY(8))
              CALL FMMPY(MXY(6),MXY(8),MXY(7))
              CALL FMI2M(2,MXY(2))
              CALL FMIPWR(MXY(2),N-1,MXY(6))
              CALL FMMPY(MXY(6),MXY(7),MXY(8))
              CALL FMDIVI(MXY(8),(N+1)/2,MXY(5))
          ELSE
              CALL FMI2M(1,MXY(2))
              CALL FMBERN(N,MXY(2),MXY(3))
              CALL FMABS(MXY(3),MXY(6))
              CALL FMPI(MXY(7))
              CALL FMIPWR(MXY(7),N,MXY(8))
              CALL FMMPY(MXY(6),MXY(8),MXY(7))
              CALL FMI2M(2,MXY(2))
              CALL FMIPWR(MXY(2),N-2,MXY(6))
              CALL FMMPY(MXY(6),MXY(7),MXY(8))
              CALL FMDIVI(MXY(8),N/2,MXY(10))
              CALL FMI2M(1,MXY(2))
              CALL FMBERN(N+2,MXY(2),MXY(3))
              CALL FMABS(MXY(3),MXY(6))
              CALL FMPI(MXY(7))
              CALL FMIPWR(MXY(7),N+2,MXY(8))
              CALL FMMPY(MXY(6),MXY(8),MXY(7))
              CALL FMI2M(2,MXY(2))
              CALL FMIPWR(MXY(2),N,MXY(6))
              CALL FMMPY(MXY(6),MXY(7),MXY(8))
              CALL FMDIVI(MXY(8),(N+2)/2,MXY(11))
              CALL FMMPY(MXY(10),MXY(11),MXY(9))
              CALL FMSQRT(MXY(9),MXY(10))
              CALL FMI2M(0,MXY(8))
              CALL FMSUB(MXY(8),MXY(10),MXY(5))
          ENDIF
          IF (MXY(4)%MP(2) - MXY(5)%MP(2) > NDIG+1 .AND.  &
              MXY(4)%MP(2) < MEXPOV) THEN
              CALL FMEQU(MXY(4),MXY(6),NDIG,NDSAVE)
              CALL FMEQU(MXY(6),MXY(8),NDSAVE,NDIG)
              CALL FMSUB(MXY(4),MXY(8),MXY(6))
              IF (MXY(6)%MP(3) == 0) THEN
                  CALL FMEQU(MXY(4),MXY(6),NDIG,NDSAVE)
                  CALL FMEQU(MXY(5),MXY(7),NDIG,NDSAVE)
                  NDIG = NDSAVE
                  KROUND = KRSAVE
                  CALL FMADD(MXY(6),MXY(7),MB)
              ELSE
                  KROUND = KRSAVE
                  CALL FMEQU(MXY(4),MB,NDIG,NDSAVE)
              ENDIF
              K_RETURN_CODE = 1
          ENDIF
          KFLAG = 0
          NTRACE = J
          KWARN = K
          NDIG = NDSAVE
          KROUND = KRSAVE
          IF (K_RETURN_CODE == 1) THEN
              IF (NTRACE /= 0) THEN
                  NCALL = NCALL + 1
                  NAMEST(NCALL) = 'FMPGAM'
                  CALL FMNTRI(2,N,1)
                  CALL FMNTR(2,MA,MA,1,0)
                  CALL FMNTR(1,MB,MB,1,1)
                  NCALL = NCALL - 1
              ENDIF
          ENDIF
      ENDIF
      IF (K_RETURN_CODE == 1) RETURN

      IF (NTRACE /= 0) THEN
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'FMPGAM'
          CALL FMNTRI(2,N,1)
          NCALL = NCALL - 1
      ENDIF
      CALL FMENT2('FMPGAM   ',MA,MA,1,0,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF
      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      N_ACC = NINT(NDIG*ALOGM2)
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
      CALL FMEQ(MXY(1),MXY(13))
      NUMTRY = 0

  120 IF (N == 0) THEN
          CALL FMPSI(MXY(1),MXY(10))
          GO TO 160
      ENDIF
      IF (N < 0 .OR. MA%MP(3) == 0) THEN
          CALL FMST2M('UNKNOWN',MXY(10))
          KFLAG = -4
          GO TO 180
      ENDIF

!             Near zero PGAM(x) is about n!/(-x)**(n+1).

      IF (MXY(13)%MP(2) < (-NDIG-1)) THEN
          CALL FMFCTI(N,MXY(12))
          IF (MXY(13)%MP(2) /= MUNKNO .AND. MXY(13)%MP(3) /= 0)  &
              MXY(13)%MP(1) = -MXY(13)%MP(1)
          CALL FMIPWR(MXY(13),N+1,MXY(11))
          CALL FMDIV(MXY(12),MXY(11),MXY(10))
          GO TO 160
      ENDIF

!             Check for special cases.

      KRFLCT = 0
      CALL FMDP2M(-0.5D0,MXY(5))
      IF (FMCOMP(MXY(13),'<=',MXY(5))) THEN
          KRFLCT = 1
          KFL = 0
          IF (MA%MP(2) <= NDSAVE) THEN
              CALL FMINT(MXY(13),MXY(9))
              IF (FMCOMP(MXY(13),'==',MXY(9))) KFL = -4
          ELSE
              KFL = -4
          ENDIF
          IF (KFL /= 0) THEN
              CALL FMST2M('UNKNOWN',MXY(10))
              KFLAG = -4
              GO TO 180
          ELSE
              CALL FMI2M(1,MXY(4))
              CALL FMSUB_R2(MXY(4),MXY(13))
          ENDIF
      ENDIF
      IF (MA%MP(2) > NDIG+3) THEN
          CALL FMIPWR(MXY(13),-N,MXY(10))
          IF (MXY(10)%MP(2) /= MEXPUN) THEN
              CALL FMFCTI(N-1,MXY(9))
              CALL FMMPY_R1(MXY(10),MXY(9))
          ENDIF
          IF (MOD(N-1,2) == 1 .AND. MXY(10)%MP(2) /= MUNKNO .AND.  &
              MXY(10)%MP(3) /= 0) MXY(10)%MP(1) = -MXY(10)%MP(1)
          IF (KROUND /= 1) THEN
              IF (MXY(10)%MP(2) /= MEXPUN) THEN
                  CALL FMMPYI(MXY(10),N,MXY(4))
                  CALL FMDIVI_R1(MXY(4),2)
                  CALL FMDIV_R1(MXY(4),MXY(13))
                  CALL FMADD_R1(MXY(10),MXY(4))
              ENDIF
          ENDIF
          GO TO 160
      ENDIF

!             To speed the asymptotic series calculation, increase the argument by LSHIFT.

      IEXTRA = 0
      KWRNSV = KWARN
      KWARN = 0
      CALL FMM2I(MXY(13),INTA)
      KWARN = KWRNSV

      IF (KFLAG == -4) THEN
          LSHIFT = 0
      ELSE
          LSHIFT = INT(MAX(0.0,REAL(NDIG)*ALOGMB/4.46-REAL(INTA)))
          LSHIFT = LSHIFT + (7*N)/20
      ENDIF
      IF (LSHIFT > 0) LSHIFT = 4*(LSHIFT/4 + 1)

      IF (LSHIFT /= 0) THEN
          CALL FMI2M(LSHIFT,MXY(4))
          CALL FMADD(MXY(13),MXY(4),MXY(12))
      ELSE
          CALL FMEQ(MXY(13),MXY(12))
      ENDIF

!             Sum the asymptotic series.

      J2 = INT(0.3*ALOGMB + 0.2*SQRT(REAL(NDIG)))
      J2 = MAX(1,MIN(LJSUMS,J2))

!             MXY(13) is Z
!             MXY(12) is Z + LSHIFT
!             MXY(9) is X**J2 = (1/(Z+LSHIFT)**2)**J2
!             MXY(10) is the current power of X times the quotient of factorials in each term
!             MXY(11) is the current term in the sum
!             MXY(8) is (N+1)!
!             MJSUMS holds the partial sums

      NDSAV1 = NDIG
      CALL FMFCTI(N+1,MXY(8))
      CALL FMDIVI(MXY(8),2,MXY(10))
      J = -2*J2
      CALL FMIPWR(MXY(12),J,MXY(9))
      IF (ABS(MXY(9)%MP(2)) >= MEXPAB) THEN
          J2 = 1
          CALL FMIPWR(MXY(12),-2,MXY(9))
      ENDIF
      DO J = 1, J2
         NTERM = 2*J
         CALL FMBERN(NTERM,MXY(10),MJSUMS(J))
         IF (KFLAG == -11) THEN
             CALL FMST2M('UNKNOWN',MXY(10))
             KFLAG = -4
             GO TO 180
         ENDIF
         NTOP = (N+NTERM)*(N+NTERM+1)
         CALL FMCSMPYI_R1(MXY(10),NTOP)
         NBOT = (NTERM+1)*(NTERM+2)
         CALL FMCSDIVI_R1(MXY(10),NBOT)
      ENDDO

      NDIG2 = NDIG
  130 CALL FMCSMPY_R1(MXY(10),MXY(9))
      NMXDIF = MIN(NDSAV1,NGRD22)
      DO J = 1, J2
         NTERM = NTERM + 2
         CALL FMBERN(NTERM,MXY(10),MXY(11))
         IF (KFLAG == -11) THEN
             CALL FMST2M('UNKNOWN',MXY(10))
             KFLAG = -4
             GO TO 180
         ENDIF
         NDIG = NDSAV1
         CALL FMCSADD_R1(MJSUMS(J),MXY(11))
         IF (KFLAG /= 0) THEN
             GO TO 140
         ELSE
             NMXDIF = MAX(NMXDIF,NDSAV1-INT(MJSUMS(J)%MP(2)-MXY(11)%MP(2)))
             NDIG = NDIG2
             IF (N+NTERM > INTMAX/(N+NTERM+1) .OR. N+NTERM > MXBASE/(N+NTERM+1)) THEN
                 CALL FMCSMPYI_R1(MXY(10),N+NTERM)
                 CALL FMCSMPYI_R1(MXY(10),N+NTERM+1)
                 CALL FMCSDIVI_R1(MXY(10),NTERM+1)
                 CALL FMCSDIVI_R1(MXY(10),NTERM+2)
             ELSE
                 NTOP = (N+NTERM)*(N+NTERM+1)
                 CALL FMCSMPYI_R1(MXY(10),NTOP)
                 NBOT = (NTERM+1)*(NTERM+2)
                 CALL FMCSDIVI_R1(MXY(10),NBOT)
             ENDIF
         ENDIF
      ENDDO
      NDIG2 = NMXDIF
      NDIG = NDIG2
      GO TO 130

!             Put the J2 concurrent sums back together.

  140 NDIG = NDSAV1
      IF (J2 > 1) THEN
          CALL FMI2M(1,MXY(9))
          CALL FMSQR(MXY(12),MXY(11))
          CALL FMDIV_R2(MXY(9),MXY(11))
          CALL FMEQ(MJSUMS(J2),MXY(9))
          DO J = J2-1, 1, -1
             CALL FMMPY_R1(MXY(9),MXY(11))
             CALL FMADD_R1(MXY(9),MJSUMS(J))
          ENDDO
          CALL FMEQ(MXY(9),MJSUMS(1))
      ENDIF
      CALL FMIPWR(MXY(12),N+2,MXY(6))
      CALL FMDIV_R1(MJSUMS(1),MXY(6))

!             Add the initial terms to the asymptotic series.

      CALL FMDIVI(MXY(8),N+1,MXY(9))
      CALL FMDIVI(MXY(9),N,MXY(8))
      CALL FMMPYI(MXY(12),2,MXY(7))
      CALL FMI2M(N,MXY(10))
      CALL FMADD(MXY(7),MXY(10),MXY(3))
      CALL FMCANCEL(MXY(7),MXY(10),MXY(3),K)
      N_ACC = N_ACC - K
      CALL FMEQ(MXY(3),MXY(7))
      CALL FMMPY_R1(MXY(7),MXY(8))
      CALL FMMPYI_R1(MXY(6),2)
      CALL FMDIV_R1(MXY(6),MXY(12))
      CALL FMDIV(MXY(7),MXY(6),MXY(10))
      CALL FMADD(MJSUMS(1),MXY(10),MXY(3))
      CALL FMCANCEL(MJSUMS(1),MXY(10),MXY(3),K)
      N_ACC = N_ACC - K
      CALL FMEQ(MXY(3),MXY(10))
      IF (MOD(N-1,2) == 1 .AND. MXY(10)%MP(2) /= MUNKNO .AND.  &
          MXY(10)%MP(3) /= 0) MXY(10)%MP(1) = -MXY(10)%MP(1)

!             Now PGAM of the shifted argument has been computed.  Reverse the shifting.
!             The sum 1/(MA)**(N+1) + ... + 1/(MA+LSHIFT-1)**(N+1) is computed.

!             MXY(13) is Z
!             MXY(9) is N!
!             MXY(10) is the sum of the asymptotic series
!             MXY(11) is the sum 1/(MA)**(N+1) + ... + 1/(MA+LSHIFT-1)**(N+1)

      IF (LSHIFT > 0) THEN
          CALL FMI2M(1,MXY(6))
          CALL FMEQ(MXY(13),MXY(7))
          N1 = -(N + 1)
          CALL FMIPWR(MXY(7),N1,MXY(11))
          DO K = 1, LSHIFT-1
             CALL FMADD_R1(MXY(7),MXY(6))
             CALL FMIPWR(MXY(7),N1,MXY(12))
             CALL FMADD_R1(MXY(11),MXY(12))
          ENDDO
          CALL FMMPY_R2(MXY(9),MXY(11))
          IF (MOD(N+1,2) == 1 .AND. MXY(11)%MP(2) /= MUNKNO .AND.  &
              MXY(11)%MP(3) /= 0) MXY(11)%MP(1) = -MXY(11)%MP(1)
          CALL FMADD(MXY(10),MXY(11),MXY(3))
          CALL FMCANCEL(MXY(10),MXY(11),MXY(3),K)
          N_ACC = N_ACC - K
          CALL FMEQ(MXY(3),MXY(10))
      ENDIF

!             Use the reflection formula if MA was less than -1/2.

      IF (KRFLCT == 1) THEN

!             MXY(11) is COT(Pi*Z)
!             MXY(9) is MXY(11)**2

!             Reduce the argument before multiplying by Pi.

          CALL FMMPYI(MXY(13),2,MXY(5))
          CALL FMINT(MXY(5),MXY(9))
          IF (FMCOMP(MXY(5),'==',MXY(9))) THEN
              CALL FMI2M(0,MXY(11))
              CALL FMEQ(MXY(11),MXY(9))
              CALL FMI2M(1,MXY(5))
          ELSE
              CALL FMNINT(MXY(13),MXY(5))
              CALL FMSUB(MXY(13),MXY(5),MXY(9))
              NDSV2 = NDIG
  150         CALL FMPI(MXY(11))
              CALL FMMPY_R1(MXY(11),MXY(9))
              KRSAVE = KRAD
              KRAD = 1
              CALL FMTAN(MXY(11),MXY(2))
              CALL FMEQ(MXY(2),MXY(11))
              KRAD = KRSAVE
              IF ((MXY(11)%MP(2) < 0 .OR. MXY(11)%MP(2) > 1) .AND.  &
                  NDSV2 == NDIG) THEN
                  IEXTRA = INT(MAX(-MXY(11)%MP(2),MXY(11)%MP(2)))
                  IF (IEXTRA > 0) THEN
                      CALL FMEQU_R1(MXY(9),NDIG,NDIG+IEXTRA)
                  ENDIF
                  NDIG = NDIG + IEXTRA
                  GO TO 150
              ENDIF

              NDIG = NDSV2
              CALL FMI2M(1,MXY(5))
              CALL FMDIV_R2(MXY(5),MXY(11))
              CALL FMSQR(MXY(11),MXY(9))
          ENDIF
          NC = (N+1)/2

!             For N up to 14, use the stored coefficients to compute the Nth derivative
!             of Cot(Pi*Z).  For larger N, the coefficients are generated from a recurrence
!             relation and stored as FM numbers.

          IF (N <= 14) THEN
              JSTART = (N*N + 4 - MOD(N,2))/4
              IF (N <= 2) THEN
                  CALL FMI2M(1,MXY(6))
              ELSE
                  CALL FMMPYI(MXY(9),KCOEFF(JSTART),MXY(6))
              ENDIF
              DO J = 2, NC
                 CALL FMI2M(KCOEFF(JSTART+J-1),MXY(7))
                 CALL FMADD_R1(MXY(6),MXY(7))
                 IF (J < NC) CALL FMMPY_R1(MXY(6),MXY(9))
              ENDDO
              IF (MOD(N,2) == 0) CALL FMMPY_R1(MXY(6),MXY(11))
              IF (N > 1) CALL FMMPYI_R1(MXY(6),KGCD(N))
          ELSE
              IF (NC > LJSUMS) THEN
                  KFLAG = -12
                  CALL FMWRN2
                  WRITE (KW,                                                  &
                         "(' For PGAM(',I5,',*) with NDIG =',I5,',',I7,"  //  &
                         "' words are needed'/' in array MJSUMS.',"       //  &
                         "'  The current dimension of MJSUMS IS',I7/)"        &
                        ) N,NDIG,NC*(NDIG+3),LJSUMS
                  MXEXP = MXSAVE
                  NDIG = NDSAVE
                  CALL FMST2M('UNKNOWN',MB)
                  IF (NTRACE /= 0) CALL FMNTR(1,MB,MB,1,1)
                  NCALL = NCALL - 1
                  RETURN
              ENDIF

              DO J = 1, 7
                 CALL FMI2M(KCOEFF(J+49),MJSUMS(J))
                 CALL FMMPYI_R1(MJSUMS(J),KGCD(14))
              ENDDO
              DO JN = 15, N
                 JNC = (JN+1)/2
                 DO K = JNC, 2, -1
                    IF (K == JNC .AND. MOD(JN,2) == 1) THEN
                        CALL FMEQ(MJSUMS(K-1),MJSUMS(K))
                    ELSE
                        CALL FMADD_R2(MJSUMS(K-1),MJSUMS(K))
                        CALL FMMPYI_R1(MJSUMS(K),JN-2*(K-1))
                    ENDIF
                 ENDDO
                 CALL FMMPYI_R1(MJSUMS(1),JN)
              ENDDO

!             MJSUMS now has the coefficients needed for the polynomial in Cot**2 that defines
!             the Nth derivative of Cot.

              CALL FMEQ(MJSUMS(1),MXY(6))
              DO J = 2, NC
                 CALL FMMPY_R1(MXY(6),MXY(9))
                 CALL FMADD_R1(MXY(6),MJSUMS(J))
              ENDDO
              IF (MOD(N,2) == 0) CALL FMMPY_R1(MXY(6),MXY(11))
          ENDIF

!             To complete the calculation of the Nth derivative of Cot, multiply the polynomial
!             in Cot**2 by Csc**2.

          CALL FMADD(MXY(9),MXY(5),MXY(7))
          CALL FMMPY_R1(MXY(6),MXY(7))

          CALL FMPI(MXY(7))
          CALL FMIPWR(MPISAV,N+1,MXY(7))
          CALL FMMPY_R1(MXY(6),MXY(7))
          IF (MOD(N,2) == 1 .AND. MXY(10)%MP(2) /= MUNKNO .AND.  &
              MXY(10)%MP(3) /= 0) MXY(10)%MP(1) = -MXY(10)%MP(1)
          CALL FMADD(MXY(10),MXY(6),MXY(3))
          CALL FMCANCEL(MXY(10),MXY(6),MXY(3),K)
          N_ACC = N_ACC - K
          CALL FMEQ(MXY(3),MXY(10))
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  160 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(10)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF

!             Check for too much cancellation.

      IF (NCALL >= 1) THEN
          NGOAL = INT(REAL(NDSAVE)*ALOGM2) + 17
      ELSE
          NGOAL = INT(-MXEXP2)
      ENDIF
      IF (N_ACC <= NGOAL) THEN
          IF (NUMTRY > 0) THEN
              NDGOAL = INT(REAL(NGOAL)/ALOGM2 + 1.0)
              DO J = 1, NDGOAL+1
                 IF (MRETRY%MP(J+1) /= MXY(10)%MP(J+1)) GO TO 170
              ENDDO
              GO TO 180
          ENDIF
  170     IEXTRA = INT(REAL(NGOAL-N_ACC)/ALOGM2 + 23.03/ALOGMB) + 1
          NDOLD = NDIG
          NDIG = NDIG + IEXTRA
          CALL FMEQU_R1(MXY(1),NDSAVE,NDIG)
          CALL FMEQ(MXY(1),MXY(13))
          NUMTRY = NUMTRY + 1
          CALL FMEQU(MXY(10),MRETRY,NDOLD,NDIG)
          GO TO 120
      ENDIF

  180 CALL FMEXT2(MXY(10),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMPGAM

      SUBROUTINE FMPOCH(MA,N,MB)

!  MB = MA*(MA+1)*(MA+2)*...*(MA+N-1)       (Pochhammer's symbol)

!  MB = Gamma(MA+N)/Gamma(MA)

!  For negative N, Pochhammer(MA,N) = 1/Pochhammer(MA+N,-N).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTEGER :: N
      REAL (KIND(1.0D0)) :: MA2,MAS,MBSIGN,MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: IEXTRA,J,JR,K,K0,K1,K2,KL,KLAST,KM08,KMB,KOVUN,KR_RETRY,KRESLT,  &
                 KRSAVE,K_RETURN_CODE,LT,NDSAVE,NT
      LOGICAL, EXTERNAL :: FMCOMP
      REAL :: T
      INTENT (IN) :: N,MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(15)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      K_RETURN_CODE = 0

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1 .AND. MA%MP(2) < -NDIG .AND. N /= 0 .AND. N /= 1) THEN
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          KRSAVE = KROUND
          KROUND = 1
          NDSAVE = NDIG
          NDIG = NDIG + NGRD52
          CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
          NT = ABS(N) - 1
          IF (N < 0) NT = NT + 1
          CALL FMI2M(NT,MXY(2))
          CALL FMFACT(MXY(2),MXY(3))
          CALL FMMPY(MXY(1),MXY(3),MXY(4))
          IF (NT <= 20) THEN
              CALL FMI2M(1,MXY(2))
              CALL FMI2M(1,MXY(5))
              DO K1 = 2, NT
                 CALL FMDIVI(MXY(2),K1,MXY(6))
                 CALL FMADD_R1(MXY(5),MXY(6))
              ENDDO
          ELSE
              CALL FMI2M(NT,MXY(7))
              CALL FMLN(MXY(7),MXY(5))
              CALL FMEULR(MXY(6))
              CALL FMADD_R1(MXY(5),MXY(6))
              CALL FMI2M(1,MXY(2))
              CALL FMDIV(MXY(2),MXY(7),MXY(8))
              CALL FMDIVI(MXY(8),2,MXY(6))
              CALL FMADD_R1(MXY(5),MXY(6))
              CALL FMSQR(MXY(8),MXY(9))
              CALL FMDIVI(MXY(9),12,MXY(6))
              CALL FMSUB_R1(MXY(5),MXY(6))
              CALL FMSQR(MXY(9),MXY(8))
              CALL FMDIVI(MXY(8),120,MXY(6))
              CALL FMADD_R1(MXY(5),MXY(6))
          ENDIF
          IF (N > 0) THEN
              CALL FMEQ(MXY(4),MXY(6))
              CALL FMMPY(MXY(4),MXY(5),MXY(8))
              CALL FMMPY(MXY(1),MXY(8),MXY(7))
          ELSE
              IF (MOD(NT,2) == 0) THEN
                  CALL FMI2M(1,MXY(2))
              ELSE
                  CALL FMI2M(-1,MXY(2))
              ENDIF
              CALL FMDIV(MXY(2),MXY(3),MXY(6))
              CALL FMMPY(MXY(6),MXY(5),MXY(8))
              CALL FMMPY(MXY(1),MXY(8),MXY(7))
          ENDIF
          IF (MXY(6)%MP(2) - MXY(7)%MP(2) > NDIG+1 .AND.  &
              MXY(6)%MP(2) < MEXPOV) THEN
              CALL FMEQU(MXY(6),MXY(10),NDIG,NDSAVE)
              CALL FMEQU(MXY(6),MXY(9),NDIG,NDSAVE)
              CALL FMEQU(MXY(9),MXY(8),NDSAVE,NDIG)
              CALL FMSUB(MXY(6),MXY(8),MXY(9))
              IF (MXY(9)%MP(3) == 0) THEN
                  CALL FMEQU(MXY(6),MXY(8),NDIG,NDSAVE)
                  CALL FMEQU(MXY(7),MXY(9),NDIG,NDSAVE)
                  NDIG = NDSAVE
                  KROUND = KRSAVE
                  CALL FMADD(MXY(8),MXY(9),MB)
                  IF (MB%MP(2) >= MEXPOV) THEN
                      IF (MXY(8)%MP(1) > 0) THEN
                          IF (MXY(9)%MP(1) < 0 .AND. (KROUND == -1 .OR. KROUND == 0)) THEN
                              CALL FMEQ(MXY(8),MXY(1))
                              MXY(1)%MP(2) = 0
                              CALL FMULP(MXY(1),MXY(2))
                              CALL FMSUB(MXY(1),MXY(2),MXY(3))
                              MXY(3)%MP(2) = MXY(8)%MP(2) + MXY(3)%MP(2)
                              CALL FMEQ(MXY(3),MB)
                          ELSE IF (MXY(9)%MP(1) > 0 .AND. KROUND == 2) THEN
                              CALL FMEQ(MXY(8),MXY(1))
                              MXY(1)%MP(2) = 0
                              CALL FMULP(MXY(1),MXY(2))
                              CALL FMADD(MXY(1),MXY(2),MXY(3))
                              MXY(3)%MP(2) = MXY(8)%MP(2) + MXY(3)%MP(2)
                              CALL FMEQ(MXY(3),MB)
                          ELSE
                              KROUND = KRSAVE
                              CALL FMEQ(MXY(10),MB)
                          ENDIF
                      ELSE
                          IF (MXY(9)%MP(1) < 0 .AND. KROUND == -1) THEN
                              CALL FMEQ(MXY(8),MXY(1))
                              MXY(1)%MP(2) = 0
                              CALL FMULP(MXY(1),MXY(2))
                              CALL FMADD(MXY(1),MXY(2),MXY(3))
                              MXY(3)%MP(2) = MXY(8)%MP(2) + MXY(3)%MP(2)
                              CALL FMEQ(MXY(3),MB)
                          ELSE IF (MXY(9)%MP(1) > 0 .AND. (KROUND == 2 .OR. KROUND == 0)) THEN
                              CALL FMEQ(MXY(8),MXY(1))
                              MXY(1)%MP(2) = 0
                              CALL FMULP(MXY(1),MXY(2))
                              CALL FMSUB(MXY(1),MXY(2),MXY(3))
                              MXY(3)%MP(2) = MXY(8)%MP(2) + MXY(3)%MP(2)
                              CALL FMEQ(MXY(3),MB)
                          ELSE
                              KROUND = KRSAVE
                              CALL FMEQ(MXY(10),MB)
                          ENDIF
                      ENDIF
                  ENDIF
              ELSE
                  KROUND = KRSAVE
                  CALL FMEQU(MXY(6),MB,NDIG,NDSAVE)
              ENDIF
              K_RETURN_CODE = 1
          ENDIF
          KFLAG = 0
          NTRACE = J
          KWARN = K
          NDIG = NDSAVE
          KROUND = KRSAVE
          IF (K_RETURN_CODE == 1) THEN
              IF (NTRACE /= 0) THEN
                  NCALL = NCALL + 1
                  NAMEST(NCALL) = 'FMPOCH'
                  CALL FMNTR(2,MA,MA,1,1)
                  CALL FMNTRI(2,N,0)
                  NCALL = NCALL - 1
              ENDIF
              IF (MB%MP(2) == MUNKNO .AND. MA%MP(2) /= MUNKNO) THEN
                  NCALL = NCALL + 1
                  NAMEST(NCALL) = 'FMPOCH'
                  KFLAG = -4
                  CALL FMWRN2
                  NCALL = NCALL - 1
              ELSE IF (ABS(MB%MP(2)) == MEXPOV .AND. ABS(MA%MP(2)) < MEXPOV) THEN
                  NCALL = NCALL + 1
                  NAMEST(NCALL) = 'FMPOCH'
                  IF (MB%MP(2) == MEXPOV) KFLAG = -5
                  IF (MB%MP(2) == MEXPUN) KFLAG = -6
                  CALL FMWRN2
                  NCALL = NCALL - 1
              ENDIF
              IF (NTRACE /= 0) THEN
                  NCALL = NCALL + 1
                  NAMEST(NCALL) = 'FMPOCH'
                  CALL FMNTR(1,MB,MB,1,1)
                  NCALL = NCALL - 1
              ENDIF
          ENDIF
      ENDIF
      IF (K_RETURN_CODE == 1) RETURN

      CALL FMENT2('FMPOCH   ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      CALL FMNTRI(2,N,0)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF

      MA2 = MA%MP(3)
      MAS = MA%MP(1)
      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      NT = N
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
      CALL FMEQ(MXY(1),MXY(13))

!             Check for special cases.

      IEXTRA = 0
      IF (N == 0) THEN
          CALL FMI2M(1,MXY(11))
          GO TO 140
      ENDIF
      IF (NT < 0) THEN
          CALL FMADDI(MXY(1),NT)
          CALL FMEQ(MXY(1),MXY(13))
          NT = -NT
          MA2 = MXY(1)%MP(3)
          MAS = MXY(1)%MP(1)
      ENDIF
      IF (MA2 == 0) THEN
          IF (NT > 0) THEN
              CALL FMI2M(0,MXY(11))
              GO TO 130
          ELSE
              CALL FMST2M('UNKNOWN',MXY(11))
              KFLAG = -4
              GO TO 140
          ENDIF
      ENDIF
      IF (NT == 0) THEN
          CALL FMI2M(1,MXY(11))
          GO TO 130
      ELSE IF (NT == 1) THEN
          CALL FMEQU(MXY(1),MXY(11),NDSAVE,NDIG)
          GO TO 130
      ENDIF
      CALL FMI2M(1,MXY(4))
      JR = KROUND
      KROUND = 1
      CALL FMADD(MXY(1),MXY(4),MXY(5))
      KROUND = JR
      IF (MXY(1)%MP(2) == MEXPOV) THEN
          CALL FMST2M('OVERFLOW',MXY(11))
          IF (MAS < 0) MXY(11)%MP(1) = (-1)**NT
          GO TO 130
      ELSE IF (MXY(1)%MP(2) == MEXPUN) THEN
          IF (NT == 2) THEN
              CALL FMST2M('UNDERFLOW',MXY(11))
              IF (MAS < 0) MXY(11)%MP(1) = -1
          ELSE
              CALL FMST2M('UNKNOWN',MXY(11))
              KFLAG = -4
          ENDIF
          GO TO 140
      ELSE IF (FMCOMP(MXY(5),'==',MXY(4))) THEN
          T = NDIG
          J = INT(15.21*SQRT(T)*ALOGMT + 42.87*SQRT(T) + 30.0)
          IF (NT <= J) THEN
              K1 = NT - 1
              CALL FMFCTI(K1,MXY(11))
              CALL FMMPY_R2(MXY(1),MXY(11))
              GO TO 130
          ENDIF
      ENDIF

!             Look for cases where overflow is easy to detect.

      CALL FMI2M(NT,MXY(9))
      CALL FMABS(MXY(13),MXY(7))
      IF (MXY(13)%MP(2) > 0 .AND. FMCOMP(MXY(9),'<',MXY(7))) THEN
          CALL FMADD(MXY(13),MXY(9),MXY(8))
          MXY(8)%MP(1) = 1
          CALL FMMIN(MXY(7),MXY(8),MXY(10))
          IF (INT(MXY(10)%MP(2))-1 > INTMAX/NT) THEN
              CALL FMST2M('OVERFLOW',MXY(11))
              IF (MXY(13)%MP(1) > 0) THEN
                  MXY(11)%MP(1) = 1
              ELSE
                  MXY(11)%MP(1) = (-1)**MOD(NT,2)
              ENDIF
              KFLAG = -5
              GO TO 130
          ENDIF
      ENDIF

!             For large values of MA, the result is MA**NT.

      LT = NDIG + 3 + INT(2.0D0*LOG(DBLE(NT))/DLOGMB)
      IF (MXY(1)%MP(2) > LT) THEN
          CALL FMIPWR(MXY(13),NT,MXY(11))
          IF (KROUND /= 1 .AND. NT > 1) THEN
              IF (ABS(MXY(11)%MP(2)) /= MEXPOV) THEN
                  CALL FMDIV(MXY(11),MXY(13),MXY(8))
                  IF (MOD(NT,2) == 0) THEN
                      CALL FMMPYI_R1(MXY(8),NT-1)
                      CALL FMMPYI_R1(MXY(8),NT/2)
                  ELSE
                      CALL FMMPYI_R1(MXY(8),(NT-1)/2)
                      CALL FMMPYI_R1(MXY(8),NT)
                  ENDIF
                  CALL FMADD_R1(MXY(11),MXY(8))
              ENDIF
          ENDIF
          GO TO 130
      ENDIF

      MBSIGN = 1
      IF (MAS < 0) THEN
          CALL FMINT(MXY(13),MXY(8))
          CALL FMI2M(NT,MXY(9))
          JR = KROUND
          KROUND = 1
          CALL FMADD(MXY(13),MXY(9),MXY(10))
          KROUND = JR
          IF (FMCOMP(MXY(13),'==',MXY(8))) THEN

!                  If MA is a negative integer and MA+NT is positive, then the result is zero.

              IF (MXY(10)%MP(1)*MXY(10)%MP(3) > 0) THEN
                  CALL FMI2M(0,MXY(11))
                  GO TO 130
              ENDIF
          ENDIF

!                  If MA is negative and MA+NT-1 is negative, then use the reflection formula
!                  Pochhammer(MA,NT) = (-1)**NT*Pochhammer(-MA-(NT-1),NT).

          CALL FMI2M(1,MXY(11))
          IF (FMCOMP(MXY(10),'<',MXY(11))) THEN

!                 Extra guard digits may be required to insure the reflection formula is accurate.

              IEXTRA = MAX(INT(MXY(13)%MP(2)),IEXTRA)
              IF (IEXTRA > 0) THEN
                  CALL FMEQU_R1(MXY(13),NDIG,NDIG+IEXTRA)
              ENDIF
              NDIG = NDIG + IEXTRA
              CALL FMI2M(NT-1,MXY(11))
              IF (MXY(13)%MP(2) /= MUNKNO .AND. MXY(13)%MP(3) /= 0)  &
                  MXY(13)%MP(1) = -MXY(13)%MP(1)
              CALL FMSUB_R1(MXY(13),MXY(11))
              IF (MOD(NT,2) == 1) MBSIGN = -1
          ENDIF
      ENDIF

!             If NT is large enough, it is faster to use two calls to FMLNGM.
!             The formula below gives a rough approximation of where to change methods.

      T = NDIG
      J = INT(15.21*SQRT(T)*ALOGMT + 42.87*SQRT(T) + 25.03)
      IF (NT > J) THEN
          CALL FMI2M(NT,MXY(4))
          CALL FMADD(MXY(13),MXY(4),MXY(14))

!             Compute IEXTRA, the number of extra digits required to compensate for
!             cancellation error.

          IF (MAX(MXY(13)%MP(2),MXY(14)%MP(2)) > IEXTRA) THEN
              IEXTRA = INT(MAX(MXY(13)%MP(2),MXY(14)%MP(2)))
          ENDIF
          IF (IEXTRA > 0) THEN
              CALL FMEQU_R1(MXY(13),NDIG,NDIG+IEXTRA)
          ENDIF
          NDIG = NDIG + IEXTRA

          CALL FMI2M(-1,MXY(15))
          IF (IEXTRA > 0) THEN
              CALL FMI2M(NT,MXY(4))
              CALL FMADD(MXY(13),MXY(4),MXY(14))
          ENDIF
          CALL FMI2M(2,MXY(9))
          KMB = 0
          IF (MXY(13)%MP(1) < 0) THEN
              CALL FMMOD(MXY(13),MXY(9),MXY(8))
              IF (FMCOMP(MXY(8),'>',MXY(15))) KMB = 1
          ENDIF
          KM08 = 0
          IF (MXY(14)%MP(1) < 0) THEN
              CALL FMMOD(MXY(14),MXY(9),MXY(8))
              IF (FMCOMP(MXY(8),'>',MXY(15))) KM08 = 1
          ENDIF
          CALL FMI2M(1,MXY(15))
          IF (MXY(13)%MP(1) < 0 .AND. KMB == 1) THEN
              CALL FMEQ(MXY(13),MXY(15))
              CALL FMI2M(1,MXY(4))
              CALL FMADD(MXY(13),MXY(4),MXY(2))
              CALL FMLNGM(MXY(2),MXY(13))
          ELSE
              CALL FMLNGM(MXY(13),MXY(3))
              CALL FMEQ(MXY(3),MXY(13))
          ENDIF
          IF (MXY(14)%MP(1) < 0 .AND. KM08 == 1) THEN
              CALL FMI2M(-1,MXY(7))
              CALL FMADD_R1(MXY(14),MXY(7))
              CALL FMMPY(MXY(15),MXY(14),MXY(2))
              CALL FMLNGM(MXY(2),MXY(14))
          ELSE
              CALL FMLNGM(MXY(14),MXY(3))
              CALL FMEQ(MXY(3),MXY(14))
          ENDIF

          CALL FMSUB(MXY(14),MXY(13),MXY(11))
          CALL FMEXP(MXY(11),MXY(2))
          CALL FMMPY(MXY(2),MXY(15),MXY(11))
          GO TO 120
      ENDIF

!             Compute the product Z*(Z+1)*...*(Z+NT-1) four terms at a time to reduce the number
!             of FMMPY calls.

!             MXY(13) is Z
!             MXY(6) is Z**2
!             MXY(7) is Z**3
!             MXY(8) is (Z+K)*...*(Z+K+3)
!             MXY(11) is the current product

!             If MXY(13) is negative and MXY(13)+NT is positive, extra digits are required when
!             MXY(13) is close to an integer.

      IF (MXY(13)%MP(1) < 0) THEN
          CALL FMI2M(NT,MXY(8))
          CALL FMADD(MXY(13),MXY(8),MXY(9))
          IF (MXY(9)%MP(1)*MXY(9)%MP(3) > 0) THEN
              CALL FMNINT(MXY(13),MXY(10))
              IF (MXY(10)%MP(3) /= 0) THEN
                  CALL FMSUB(MXY(13),MXY(10),MXY(9))
                  IEXTRA = MAX(IEXTRA,NDIG-NDSAVE)
                  IF (MAX(MXY(13)%MP(2),MXY(9)%MP(2)) > IEXTRA) THEN
                      IEXTRA = INT(MAX(MXY(13)%MP(2),MXY(9)%MP(2)))
                  ENDIF
                  IF (IEXTRA > 0) THEN
                      CALL FMEQU_R1(MXY(13),NDIG,NDIG+IEXTRA)
                  ENDIF
                  NDIG = NDIG + IEXTRA
              ENDIF
          ENDIF
      ENDIF

      CALL FMI2M(1,MXY(11))
      IF (NT >= 4) THEN
          CALL FMSQR(MXY(13),MXY(6))
          CALL FMMPY(MXY(13),MXY(6),MXY(7))
          CALL FMSQR(MXY(6),MXY(8))
          CALL FMCSMPYI(MXY(7),6,MXY(12))
          CALL FMCSADD_R1(MXY(8),MXY(12))
          CALL FMCSMPYI(MXY(6),11,MXY(12))
          CALL FMCSADD_R1(MXY(8),MXY(12))
          CALL FMCSMPYI(MXY(13),6,MXY(12))
          CALL FMCSADD_R1(MXY(8),MXY(12))
          CALL FMEQ(MXY(8),MXY(11))
          CALL FMCSMPYI_R1(MXY(7),16)
          DO K = 0, NT-8, 4
             CALL FMCSADD_R1(MXY(8),MXY(7))
             K2 = 24*(2*K + 7)
             CALL FMCSMPYI(MXY(6),K2,MXY(12))
             CALL FMCSADD_R1(MXY(8),MXY(12))
             IF (K <= SQRT(REAL(INTMAX)/49.0)) THEN
                 K1 = 8*(6*K*K + 42*K + 79)
                 CALL FMCSMPYI(MXY(13),K1,MXY(12))
                 CALL FMCSADD_R1(MXY(8),MXY(12))
             ELSE
                 K1 = 48*K
                 CALL FMCSMPYI(MXY(13),K1,MXY(12))
                 CALL FMCSMPYI_R1(MXY(12),K)
                 CALL FMCSADD_R1(MXY(8),MXY(12))
                 K1 = 336*K + 632
                 CALL FMCSMPYI(MXY(13),K1,MXY(12))
                 CALL FMCSADD_R1(MXY(8),MXY(12))
             ENDIF
             IF (K <= (REAL(INTMAX)/17.0)**0.3333) THEN
                 K0 = 8*(2*K + 7)*(K*K + 7*K + 15)
                 CALL FMADDI(MXY(8),K0)
             ELSE IF (K <= SQRT(REAL(INTMAX)*0.9)) THEN
                 K0 = 8*(2*K + 7)
                 CALL FMI2M(K0,MXY(12))
                 K0 = K*K + 7*K + 15
                 CALL FMCSMPYI_R1(MXY(12),K0)
                 CALL FMCSADD_R1(MXY(8),MXY(12))
             ELSE
                 K0 = 8*(2*K + 7)
                 CALL FMI2M(K0,MXY(12))
                 CALL FMCSMPYI(MXY(12),K,MXY(9))
                 CALL FMCSMPYI_R1(MXY(9),K)
                 CALL FMCSADD_R1(MXY(8),MXY(9))
                 K0 = 7*K + 15
                 CALL FMCSMPYI_R1(MXY(12),K0)
                 CALL FMCSADD_R1(MXY(8),MXY(12))
             ENDIF
             CALL FMCSMPY_R1(MXY(11),MXY(8))
          ENDDO
      ENDIF

      KLAST = (NT/4)*4
      DO J = KLAST, NT-1
         CALL FMI2M(J,MXY(9))
         CALL FMCSADD_R1(MXY(9),MXY(13))
         CALL FMCSMPY_R1(MXY(11),MXY(9))
      ENDDO

!             If the reflection formula was used, multiply by (-1)**NT.

  120 MXY(11)%MP(1) = MBSIGN*MXY(11)%MP(1)

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  130 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(11)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF

  140 IF (N < 0) THEN
          CALL FMI2M(1,MXY(6))
          CALL FMDIV_R2(MXY(6),MXY(11))
      ENDIF
      CALL FMEXT2(MXY(11),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMPOCH

      SUBROUTINE FMPSI(MA,MB)

!  MB = PSI(MA)      (Derivative of Ln(Gamma(MA))

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR,X,Z
      INTEGER :: IEXTRA,INTA,J,J2,K,K0,K0B,K1,K1B,K2,KFL,KL,KOVUN,KC_RETRY,KR_RETRY,KRESLT,  &
                 KRFLCT,KRSAVE,KWRNSV,K_RETURN_CODE,LSHIFT,NDENOM,NDGOAL,NDIG2,NDOLD,        &
                 NDSAV1,NDSAVE,NGOAL,NMETHD,NMXDIF,NTERM,NUMTRY,N_ACC
      LOGICAL, EXTERNAL :: FMCOMP
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI), SAVE :: MXY(13),MRETRY,MJSUMS(LJSUMS),C(0:196)
      INTEGER, SAVE :: NDIG_C = 0
      REAL (KIND(1.0D0)), SAVE :: MBASE_C = 0

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
!$print*,' psi enter.  ndig = ', ndig
      N_ACC = NINT(NDIG*ALOGM2)
      K_RETURN_CODE = 0

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1 .AND. MA%MP(2) < -NDIG) THEN
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          KRSAVE = KROUND
          KROUND = 1
          NDSAVE = NDIG
          NDIG = NDIG + NGRD52
          N_ACC = NINT(NDIG*ALOGM2)
          CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
          CALL FMI2M(0,MXY(2))
          CALL FMSUB(MXY(2),MXY(1),MXY(3))
          CALL FMI2M(1,MXY(2))
          CALL FMDIV(MXY(2),MXY(3),MXY(4))
          CALL FMEULR(MXY(6))
          CALL FMMPYI(MXY(6),-1,MXY(5))
          IF (MXY(4)%MP(2) - MXY(5)%MP(2) > NDIG+1 .AND.  &
              MXY(4)%MP(2) < MEXPOV) THEN
              CALL FMEQU(MXY(4),MXY(6),NDIG,NDSAVE)
              CALL FMEQU(MXY(6),MXY(8),NDSAVE,NDIG)
              CALL FMSUB(MXY(4),MXY(8),MXY(6))
              IF (MXY(6)%MP(3) == 0) THEN
                  CALL FMEQU(MXY(4),MXY(6),NDIG,NDSAVE)
                  CALL FMEQU(MXY(5),MXY(7),NDIG,NDSAVE)
                  NDIG = NDSAVE
                  KROUND = KRSAVE
                  CALL FMADD(MXY(6),MXY(7),MB)
              ELSE
                  KROUND = KRSAVE
                  CALL FMEQU(MXY(4),MB,NDIG,NDSAVE)
              ENDIF
              K_RETURN_CODE = 1
          ENDIF
          KFLAG = 0
          NTRACE = J
          KWARN = K
          NDIG = NDSAVE
          KROUND = KRSAVE
          IF (K_RETURN_CODE == 1) THEN
              IF (NTRACE /= 0) THEN
                  NCALL = NCALL + 1
                  NAMEST(NCALL) = 'FMPSI'
                  CALL FMNTR(2,MA,MA,1,1)
                  CALL FMNTR(1,MB,MB,1,1)
                  NCALL = NCALL - 1
              ENDIF
          ENDIF
      ENDIF
      IF (K_RETURN_CODE == 1) RETURN

      CALL FMENT2('FMPSI    ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
!$print*,' psi after fment2.  ndig = ', ndig, '   ndsave = ', ndsave
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF
      KR_RETRY = 0
      KC_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      N_ACC = NINT(NDIG*ALOGM2)
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
      CALL FMEQ(MXY(1),MXY(11))
      NUMTRY = 0

!             Near zero Psi(x) is about -1/x.

  120 IF (MXY(11)%MP(2) < (-NDIG-1)) THEN
          CALL FMI2M(-1,MXY(3))
          CALL FMDIV(MXY(3),MXY(11),MXY(8))
          GO TO 190
      ENDIF

!             Check for special cases.

      KRFLCT = 0
      CALL FMDPM(DBLE(-0.5),MXY(4))
      IF (FMCOMP(MXY(11),'<=',MXY(4))) THEN
          KRFLCT = 1
          KFL = 0
          IF (MA%MP(2) <= NDSAVE) THEN
              CALL FMINT(MXY(11),MXY(7))
              IF (FMCOMP(MXY(11),'==',MXY(7))) KFL = -4
          ELSE
              KFL = -4
          ENDIF
          IF (KFL /= 0) THEN
              CALL FMST2M('UNKNOWN',MXY(8))
              KFLAG = -4
              GO TO 210
          ELSE
              CALL FMI2M(1,MXY(3))
              CALL FMSUB_R2(MXY(3),MXY(11))
          ENDIF
      ENDIF

!             Determine which method to use.

!             NMETHD = 1 means use the polynomial approximation,
!                    = 2 means use the asymptotic series.

      NMETHD = 1
      CALL FMNINT(MXY(11),MXY(2))
      CALL FMSUB(MXY(11),MXY(2),MXY(6))
      CALL FMM2DP(MXY(6),Z)
      Z = MAX(ABS(Z),1.0D-50)
      IF (KFLAG /= 0 .OR. ABS(Z) >= 1) THEN
          NMETHD = 2
      ELSE
          IF (190*LOG(Z) - 90*DLOGTN >= -NDIG*DLOGMB .OR. -190*DLOGTN >= -NDIG*DLOGMB) THEN
              NMETHD = 2
          ENDIF
      ENDIF
      CALL FMM2DP(MXY(11),X)
      IF (KFLAG /= 0) THEN
          NMETHD = 2
      ELSE IF (NMETHD == 1) THEN
          IF (X > 35 - 8*LOG(Z) + NDIG*DLOGMB/(2.5*DLOGTN) .OR. X > 250) NMETHD = 2
      ENDIF
      IF (NMETHD == 2) GO TO 150

!             Method 1.  Use the polynomial c(0) + c(1)*(x-3) + ... + c(196)*(x-3)**196

      IF (MA%MP(1) > 0 .AND. NDSAVE+NGRD52 < NDIG .AND. KR_RETRY == 0 .AND. KC_RETRY == 0)  &
          NDIG = NDSAVE + NGRD52
      N_ACC = NINT(NDIG*ALOGM2)
!$print*,' psi top of method 1.  ndig = ', ndig, '   n_acc = ', n_acc
      CALL FMM2I(MXY(2),LSHIFT)
      LSHIFT = LSHIFT - 3
      IF (NDIG_C < NDIG .OR. MBASE_C /= MBASE) THEN
          CALL FMPSI_C(NDIG_C,MBASE_C,C)
      ENDIF
      J2 = 0.42*LOG(Z) + 7.9
      J2 = MAX(2,MIN(J2+MOD(J2,2),LJSUMS))
      NDSAV1 = NDIG
      DO J = 1, J2
         CALL FMEQ(C(J),MJSUMS(J))
      ENDDO
      CALL FMIPWR(MXY(6),J2,MXY(5))
      CALL FMEQ(MXY(5),MXY(7))
      NTERM = J2
  130 IF (NTERM > J2) CALL FMCSMPY_R1(MXY(7),MXY(5))
      DO J = 1, J2
         NTERM = NTERM + 1
         CALL FMEQ(MXY(7),MXY(4))
         CALL FMCSMPY_R1(MXY(4),C(NTERM))
         NDIG = NDSAV1
         CALL FMCSADDNN_R1(MJSUMS(J),MXY(4))
         IF (KFLAG /= 0 .OR. NTERM == 196) GO TO 140
         NDIG = NDSAV1 - INT(MJSUMS(J)%MP(2)-MXY(4)%MP(2))
         NDIG = MIN(NDSAV1,NDIG)
         IF (NDIG < NGRD22) NDIG = NGRD22
      ENDDO
      GO TO 130
  140 KFLAG = 0
      CALL FMCSNSUMS(J2,MJSUMS)
      NDIG = NDSAV1
      IF (NTERM == 196) THEN
          GO TO 150
      ENDIF
      CALL FMEQ(MJSUMS(J2),MXY(8))
      CALL FMEQ(MXY(6),MXY(3))
      MXY(3)%MP(1) = -MXY(3)%MP(1)
      DO J = 2, J2
         CALL FMCSMPY_R1(MXY(8),MXY(3))
         CALL FMCSADD_R1(MXY(8),MJSUMS(J2-J+1))
      ENDDO
      CALL FMCSMPY_R1(MXY(8),MXY(6))
      CALL FMADD_R1(MXY(8),C(0))

!             Now Psi of the shifted argument has been computed.  Reverse the shifting.
!             The sum 1/(MA-LSHIFT) + ... + 1/(MA-1) is computed.

!             MXY(11) is Z
!             MXY(4) is Z**2
!             MXY(5) is 16*Z**3
!             MXY(6) is the current four-term numerator
!             MXY(7) is the current four-term denominator
!             MXY(9) is the current sum

!$print*,'  lshift = ', lshift
      IF (LSHIFT < 0) THEN
          CALL FMI2M(-1,MXY(4))
          CALL FMEQ(MXY(11),MXY(5))
          IF (MXY(8)%MP(1) > 0) THEN
              CALL FMEQ(MXY(8),MXY(12))
              CALL FMI2M(0,MXY(13))
          ELSE
              CALL FMEQ(MXY(8),MXY(13))
              CALL FMI2M(0,MXY(12))
          ENDIF
          DO J = 1, -LSHIFT
             CALL FMCSDIV(MXY(4),MXY(5),MXY(7))
             IF (MXY(7)%MP(1) > 0) THEN
                 CALL FMADD_R1(MXY(12),MXY(7))
             ELSE
                 CALL FMADD_R1(MXY(13),MXY(7))
             ENDIF

             CALL FMCSADD_R1(MXY(8),MXY(7))
             CALL FMSUB_R1(MXY(5),MXY(4))
          ENDDO
          CALL FMADD(MXY(12),MXY(13),MXY(8))
!$print*,' lshift < 0 add in method 1.  input: MXY(12),MXY(13) = '
!$call fmprint(mxy(12))
!$call fmprint(mxy(13))
          CALL FMCANCEL(MXY(12),MXY(13),MXY(8),K)
          N_ACC = N_ACC - K
!$print*,' lshift < 0 add in method 1.  n_acc = ', n_acc, ' result: MXY(8) = '
!$call fmprint(mxy(8))
      ENDIF
      IF (LSHIFT > 0) THEN
          CALL FMADDI(MXY(11),-LSHIFT)
          IF (MOD(LSHIFT,4) /= 0) THEN
              CALL FMI2M(1,MXY(4))
              CALL FMEQ(MXY(11),MXY(5))
              DO J = 1, MOD(LSHIFT,4)
                 CALL FMCSDIV(MXY(4),MXY(5),MXY(7))
                 CALL FMCSADD_R1(MXY(8),MXY(7))
                 CALL FMCSADD_R1(MXY(5),MXY(4))
              ENDDO
              CALL FMADDI(MXY(11),MOD(LSHIFT,4))
          ENDIF
          LSHIFT = LSHIFT - MOD(LSHIFT,4)
      ENDIF
      IF (LSHIFT > 0) THEN
          CALL FMSQR(MXY(11),MXY(4))
          CALL FMMPY(MXY(11),MXY(4),MXY(5))
          CALL FMSQR(MXY(4),MXY(6))
          CALL FMCSMPYI(MXY(5),6,MXY(10))
          CALL FMCSADD_R1(MXY(6),MXY(10))
          CALL FMCSMPYI(MXY(4),11,MXY(10))
          CALL FMCSADD_R1(MXY(6),MXY(10))
          CALL FMCSMPYI(MXY(11),6,MXY(10))
          CALL FMADD(MXY(6),MXY(10),MXY(7))
          CALL FMCSMPYI(MXY(5),4,MXY(6))
          CALL FMCSMPYI(MXY(4),18,MXY(10))
          CALL FMCSADD_R1(MXY(6),MXY(10))
          CALL FMCSMPYI(MXY(11),22,MXY(10))
          CALL FMCSADD_R1(MXY(6),MXY(10))
          CALL FMI2M(6,MXY(10))
          CALL FMCSADD_R1(MXY(6),MXY(10))
          CALL FMCSDIV(MXY(6),MXY(7),MXY(9))
          CALL FMCSMPYI_R1(MXY(5),16)
          DO K = 4, LSHIFT-4, 4
             CALL FMCSADD_R1(MXY(7),MXY(5))

             CALL FMCSMPYI(MXY(4),48,MXY(10))
             CALL FMCSADD_R1(MXY(6),MXY(10))

             K2 = 8*(6*K - 3)
             CALL FMCSMPYI(MXY(4),K2,MXY(10))
             CALL FMCSADD_R1(MXY(7),MXY(10))

             K1 = 16*(6*K - 3)
             CALL FMCSMPYI(MXY(11),K1,MXY(10))
             CALL FMCSADD_R1(MXY(6),MXY(10))

             IF (K <= SQRT(REAL(INTMAX)/49.0)) THEN
                 K1 = 8*(6*K*K - 6*K + 7)
                 CALL FMCSMPYI(MXY(11),K1,MXY(10))
                 CALL FMCSADD_R1(MXY(7),MXY(10))

                 CALL FMI2M(K1,MXY(10))
                 CALL FMCSADD_R1(MXY(6),MXY(10))
             ELSE
                 K1 = 48*K
                 CALL FMCSMPYI(MXY(11),K1,MXY(10))
                 CALL FMCSMPYI_R1(MXY(10),K)
                 CALL FMCSADD_R1(MXY(7),MXY(10))
                 K1B = 8*(-6*K + 7)
                 CALL FMCSMPYI(MXY(11),K1B,MXY(10))
                 CALL FMCSADD_R1(MXY(7),MXY(10))

                 CALL FMI2M(K1,MXY(10))
                 CALL FMCSMPYI_R1(MXY(10),K)
                 CALL FMCSADD_R1(MXY(6),MXY(10))
                 CALL FMI2M(K1B,MXY(10))
                 CALL FMCSADD_R1(MXY(6),MXY(10))
             ENDIF
             IF (K <= (REAL(INTMAX)/17.0)**0.3333) THEN
                 K0 = 8*(2*K - 1)*(K*K - K + 3)
                 CALL FMI2M(K0,MXY(10))
                 CALL FMCSADD_R1(MXY(7),MXY(10))
             ELSE IF (K <= SQRT(REAL(INTMAX)*0.9)) THEN
                 K0 = 8*(2*K - 1)
                 CALL FMI2M(K0,MXY(10))
                 K0B = K*K - K + 3
                 CALL FMCSMPYI_R1(MXY(10),K0B)
                 CALL FMCSADD_R1(MXY(7),MXY(10))
             ELSE
                 K0 = 8*(2*K - 1)
                 CALL FMI2M(K0,MXY(10))
                 CALL FMCSMPYI_R1(MXY(10),K)
                 CALL FMCSMPYI_R1(MXY(10),K)
                 CALL FMCSADD_R1(MXY(7),MXY(10))
                 K0B = -K + 3
                 CALL FMI2M(K0,MXY(10))
                 CALL FMCSMPYI_R1(MXY(10),K0B)
                 CALL FMCSADD_R1(MXY(7),MXY(10))
             ENDIF
             CALL FMCSDIV(MXY(6),MXY(7),MXY(10))
             CALL FMCSADD_R1(MXY(9),MXY(10))
          ENDDO
          CALL FMADD(MXY(8),MXY(9),MXY(3))
!$print*,' final add in method 1.  input: MXY(8),MXY(9) = '
!$call fmprint(mxy(8))
!$call fmprint(mxy(9))
          CALL FMCANCEL(MXY(8),MXY(9),MXY(3),K)
          N_ACC = N_ACC - K
!$print*,' final add in method 1.  n_acc = ', n_acc, ' result: MXY(8) = '
!$call fmprint(mxy(3))
          CALL FMEQ(MXY(3),MXY(8))
      ENDIF
      GO TO 180

!             Method 2.  Use the B(2n)/(2n*X**(2n) asymptotic series.
!                        To speed the asymptotic series calculation,
!                        increase the argument by LSHIFT.

  150 IEXTRA = 0
      N_ACC = NINT(NDIG*ALOGM2)
      KWRNSV = KWARN
      KWARN = 0
      CALL FMM2I(MXY(11),INTA)
      KWARN = KWRNSV

      IF (KFLAG == -4) THEN
          LSHIFT = 0
      ELSE
          LSHIFT = INT(MAX(0.0,REAL(NDIG)*ALOGMB/4.46-REAL(INTA)))
      ENDIF
      IF (LSHIFT > 0) LSHIFT = 4*(LSHIFT/4 + 1)

      IF (LSHIFT /= 0) THEN
          CALL FMI2M(LSHIFT,MXY(3))
          CALL FMADD(MXY(11),MXY(3),MXY(10))
      ELSE
          CALL FMEQ(MXY(11),MXY(10))
      ENDIF

!             Sum the asymptotic series.

      J2 = INT(0.3*ALOGMB + 0.2*SQRT(REAL(NDIG)))
      J2 = MAX(1,MIN(LJSUMS,J2))

!             MXY(11) is Z
!             MXY(10) is Z + LSHIFT
!             MXY(7) is X**J2 = (1/(Z+LSHIFT)**2)**J2
!             MXY(8) is the current power of X
!             MXY(9) is the current term in the sum
!             MJSUMS is the partial sum

      NDSAV1 = NDIG
      CALL FMI2M(1,MXY(8))
      J = -2*J2
      CALL FMIPWR(MXY(10),J,MXY(7))
      IF (ABS(MXY(7)%MP(2)) >= MEXPAB) THEN
          J2 = 1
          CALL FMIPWR(MXY(10),-2,MXY(7))
      ENDIF
      DO J = 1, J2
         NTERM = 2*J
         CALL FMBERN(NTERM,MXY(8),MXY(9))
         IF (KFLAG == -11) THEN
             CALL FMST2M('UNKNOWN',MXY(8))
             KFLAG = -4
             GO TO 210
         ENDIF
         NDENOM = NTERM
         CALL FMDIVI(MXY(9),NDENOM,MJSUMS(J))
      ENDDO

      NDIG2 = NDIG
  160 CALL FMMPY_R1(MXY(8),MXY(7))
      NMXDIF = MIN(NDSAV1,NGRD22)
      DO J = 1, J2
         NTERM = NTERM + 2
         CALL FMBERN(NTERM,MXY(8),MXY(9))
         IF (KFLAG == -11) THEN
             CALL FMST2M('UNKNOWN',MXY(8))
             KFLAG = -4
             GO TO 210
         ENDIF
         NDENOM = NTERM
         CALL FMDIVI_R1(MXY(9),NDENOM)
         NDIG = NDSAV1
         CALL FMADD_R1(MJSUMS(J),MXY(9))
         NMXDIF = MAX(NMXDIF,NDSAV1-INT(MJSUMS(J)%MP(2)-MXY(9)%MP(2)))
         NDIG = NDIG2
         IF (KFLAG /= 0) GO TO 170
      ENDDO
      NDIG2 = NMXDIF
      NDIG = NDIG2
      GO TO 160

!             Put the J2 concurrent sums back together.

  170 NDIG = NDSAV1
      CALL FMI2M(1,MXY(7))
      CALL FMSQR(MXY(10),MXY(9))
      CALL FMDIV_R2(MXY(7),MXY(9))
      IF (J2 > 1) THEN
          CALL FMEQ(MJSUMS(J2),MXY(7))
          DO J = J2-1, 1, -1
             CALL FMMPY_R1(MXY(7),MXY(9))
             CALL FMADD_R1(MXY(7),MJSUMS(J))
          ENDDO
          CALL FMEQ(MXY(7),MJSUMS(1))
      ENDIF

!             Add the log term to the asymptotic series.

!             MXY(8) is the current sum as the log terms are added
!             MXY(9) is now LN(Z+LSHIFT)

      CALL FMMPY(MJSUMS(1),MXY(9),MXY(8))
      CALL FMLN(MXY(10),MXY(9))
      CALL FMI2M(1,MXY(4))
      CALL FMDIV(MXY(4),MXY(10),MXY(5))
      CALL FMDIVI_R1(MXY(5),2)
      CALL FMSUB(MXY(9),MXY(5),MXY(3))
      CALL FMCANCEL(MXY(9),MXY(5),MXY(3),K)
      N_ACC = N_ACC - K
      CALL FMEQ(MXY(3),MXY(5))
      CALL FMSUB(MXY(5),MXY(8),MXY(3))
      CALL FMCANCEL(MXY(5),MXY(8),MXY(3),K)
      N_ACC = N_ACC - K
      CALL FMEQ(MXY(3),MXY(8))

!             Now Psi of the shifted argument has been computed.  Reverse the shifting.
!             The sum 1/(MA) + ... + 1/(MA+LSHIFT-1) is computed.

!             MXY(11) is Z
!             MXY(4) is Z**2
!             MXY(5) is 16*Z**3
!             MXY(6) is the current four-term numerator
!             MXY(7) is the current four-term denominator
!             MXY(9) is the current sum

      IF (LSHIFT > 0) THEN
          CALL FMSQR(MXY(11),MXY(4))
          CALL FMMPY(MXY(11),MXY(4),MXY(5))
          CALL FMSQR(MXY(4),MXY(6))
          CALL FMMPYI(MXY(5),6,MXY(10))
          CALL FMADD_R1(MXY(6),MXY(10))
          CALL FMMPYI(MXY(4),11,MXY(10))
          CALL FMADD_R1(MXY(6),MXY(10))
          CALL FMMPYI(MXY(11),6,MXY(10))
          CALL FMADD(MXY(6),MXY(10),MXY(7))
          CALL FMMPYI(MXY(5),4,MXY(6))
          CALL FMMPYI(MXY(4),18,MXY(10))
          CALL FMADD_R1(MXY(6),MXY(10))
          CALL FMMPYI(MXY(11),22,MXY(10))
          CALL FMADD_R1(MXY(6),MXY(10))
          CALL FMI2M(6,MXY(10))
          CALL FMADD_R1(MXY(6),MXY(10))
          CALL FMDIV(MXY(6),MXY(7),MXY(9))
          CALL FMMPYI_R1(MXY(5),16)
          DO K = 4, LSHIFT-4, 4
             CALL FMADD_R1(MXY(7),MXY(5))

             CALL FMMPYI(MXY(4),48,MXY(10))
             CALL FMADD_R1(MXY(6),MXY(10))

             K2 = 8*(6*K - 3)
             CALL FMMPYI(MXY(4),K2,MXY(10))
             CALL FMADD_R1(MXY(7),MXY(10))

             K1 = 16*(6*K - 3)
             CALL FMMPYI(MXY(11),K1,MXY(10))
             CALL FMADD_R1(MXY(6),MXY(10))

             IF (K <= SQRT(REAL(INTMAX)/49.0)) THEN
                 K1 = 8*(6*K*K - 6*K + 7)
                 CALL FMMPYI(MXY(11),K1,MXY(10))
                 CALL FMADD_R1(MXY(7),MXY(10))

                 CALL FMI2M(K1,MXY(10))
                 CALL FMADD_R1(MXY(6),MXY(10))
             ELSE
                 K1 = 48*K
                 CALL FMMPYI(MXY(11),K1,MXY(10))
                 CALL FMMPYI_R1(MXY(10),K)
                 CALL FMADD_R1(MXY(7),MXY(10))
                 K1B = 8*(-6*K + 7)
                 CALL FMMPYI(MXY(11),K1B,MXY(10))
                 CALL FMADD_R1(MXY(7),MXY(10))

                 CALL FMI2M(K1,MXY(10))
                 CALL FMMPYI_R1(MXY(10),K)
                 CALL FMADD_R1(MXY(6),MXY(10))
                 CALL FMI2M(K1B,MXY(10))
                 CALL FMADD_R1(MXY(6),MXY(10))
             ENDIF
             IF (K <= (REAL(INTMAX)/17.0)**0.3333) THEN
                 K0 = 8*(2*K - 1)*(K*K - K + 3)
                 CALL FMI2M(K0,MXY(10))
                 CALL FMADD_R1(MXY(7),MXY(10))
             ELSE IF (K <= SQRT(REAL(INTMAX)*0.9)) THEN
                 K0 = 8*(2*K - 1)
                 CALL FMI2M(K0,MXY(10))
                 K0B = K*K - K + 3
                 CALL FMMPYI_R1(MXY(10),K0B)
                 CALL FMADD_R1(MXY(7),MXY(10))
             ELSE
                 K0 = 8*(2*K - 1)
                 CALL FMI2M(K0,MXY(10))
                 CALL FMMPYI_R1(MXY(10),K)
                 CALL FMMPYI_R1(MXY(10),K)
                 CALL FMADD_R1(MXY(7),MXY(10))
                 K0B = -K + 3
                 CALL FMI2M(K0,MXY(10))
                 CALL FMMPYI_R1(MXY(10),K0B)
                 CALL FMADD_R1(MXY(7),MXY(10))
             ENDIF
             CALL FMDIV(MXY(6),MXY(7),MXY(10))
             CALL FMADD_R1(MXY(9),MXY(10))
          ENDDO
          CALL FMSUB(MXY(8),MXY(9),MXY(3))
          CALL FMCANCEL(MXY(8),MXY(9),MXY(3),K)
          N_ACC = N_ACC - K
          CALL FMEQ(MXY(3),MXY(8))
      ENDIF

!             Use the reflection formula if MA was less than -1/2.

  180 IF (KRFLCT == 1) THEN

!             Reduce the argument before multiplying by Pi.

          CALL FMNINT(MXY(11),MXY(4))
          CALL FMSUB(MXY(11),MXY(4),MXY(7))
          CALL FMCANCEL(MXY(11),MXY(4),MXY(7),K)
          N_ACC = N_ACC - K
          CALL FMPI(MXY(9))
          CALL FMMPY_R1(MXY(9),MXY(7))
          KRSAVE = KRAD
          KRAD = 1
          CALL FMTAN(MXY(9),MXY(2))
          KRAD = KRSAVE
          CALL FMDIV_R2(MPISAV,MXY(2))
          CALL FMADD(MXY(8),MXY(2),MXY(3))
          CALL FMCANCEL(MXY(8),MXY(2),MXY(3),K)
          N_ACC = N_ACC - K
          CALL FMEQ(MXY(3),MXY(8))
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  190 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(8)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF

!             Check for too much cancellation.

!$print*, ' Check for too much cancellation.  ndsave = ', ndsave, '   ndig = ', ndig
      IF (NCALL >= 1) THEN
          NGOAL = INT(REAL(NDSAVE)*ALOGM2) + 17
      ELSE
          NGOAL = INT(-MXEXP2)
      ENDIF
!$print*, '   ncall = ', ncall, '   ngoal = ', ngoal
      IF (N_ACC <= NGOAL) THEN
          IF (NUMTRY > 0) THEN
              NDGOAL = INT(REAL(NGOAL)/ALOGM2 + 1.0)
              DO J = 1, NDGOAL+1
                 IF (MRETRY%MP(J+1) /= MXY(8)%MP(J+1)) GO TO 200
              ENDDO
              GO TO 210
          ENDIF
  200     IEXTRA = INT(REAL(NGOAL-N_ACC)/ALOGM2 + 23.03/ALOGMB) + 1
          NDOLD = NDIG
          NDIG = NDIG + IEXTRA
          N_ACC = NINT(NDIG*ALOGM2)
          CALL FMEQU_R1(MXY(1),NDSAVE,NDIG)
          CALL FMEQ(MXY(1),MXY(11))
          KC_RETRY = 1
          NUMTRY = NUMTRY + 1
          CALL FMEQU(MXY(8),MRETRY,NDOLD,NDIG)
          GO TO 120
      ENDIF

  210 CALL FMEXT2(MXY(8),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMPSI

      SUBROUTINE FMPSI_C(NDIG_C,MBASE_C,C)

!  Initialize the constants used in the psi polynomial.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: C(0:196)
      INTEGER :: NDIG_C
      REAL (KIND(1.0D0)) :: MBASE_C
      INTENT (INOUT) :: C,NDIG_C,MBASE_C
      INTEGER :: NDSAVE
      CHARACTER(220) :: ST

      NDSAVE = NDIG
      NDIG = MAX(NDIG,NINT(210*DLOGTN/DLOGMB))
      NDIG_C = NDIG
      MBASE_C = MBASE

      ST = " 9.227843350984671393934879099175975689578406640600764011942327651" //  &
           "151322732223353290630529367082532504853685527501929175190394959855" //  &
           "13457163775826002355076463746499666257062662262326057207404741752905084M-1"
      CALL FMST2M(ST,C(0))
      ST = " 3.949340668482264364724151666460251892189499012067984377355582293" //  &
           "700074704032008738336289006197587053040043189623371906796287246870" //  &
           "05007787935102946330866276831733309367762605095251006872140054796811559M-1"
      CALL FMST2M(ST,C(1))
      ST = " 7.705690315959428539973816151144999076498629234049888179227155534" //  &
           "183820578631309018645587360933525814619915779526071941849199599867" //  &
           "32832137763968372079001614539417829493600667191915755222424942439615639M-2"
      CALL FMST2M(ST,C(2))
      ST = " 1.982323371113819151600369654116790277475095191872690768297621544" //  &
           "412061618696884655690963594169991723299081390804274241458407157457" //  &
           "00453492820035147162192070877834809108370293261887348261752736042355062M-2"
      CALL FMST2M(ST,C(3))
      ST = " 5.677755143369926331365486457034168057080919501912811974192677903" //  &
           "803589786281484560043106557133336379620341466556609042800961779155" //  &
           "97084183511072180087644866286337180353598363962365128888981335276775240M-3"
      CALL FMST2M(ST,C(4))
      ST = " 1.718061984449139714517929790920527901817490032853561842408664004" //  &
           "332182901957897882773977938535170530279191162254558867398181448333" //  &
           "10185379291633807265673175253040965355618769627954139226236853166325794M-3"
      CALL FMST2M(ST,C(5))
      ST = " 5.367773819228268397975498497967595998635605652387064172831365716" //  &
           "014783173557353460969689138513239689614536514910748872867774198403" //  &
           "35440315798301033984562121069463585243906583353964676997567696691427804M-4"
      CALL FMST2M(ST,C(6))
      ST = " 1.711061979443393786852385086524652589607906498500203291102026525" //  &
           "829525747488143952872303723719711245236484702826900263542995480733" //  &
           "83970966270581799604618992754222287297459333028151505628361773822843426M-4"
      CALL FMST2M(ST,C(7))
      ST = " 5.526782608221441785276923241206048560585139488875654859661590978" //  &
           "505339025839895039306912716958615740860476584706026142537397072243" //  &
           "01530691324987642510909294868767654539697941540782602296415448362507489M-5"
      CALL FMST2M(ST,C(8))
      ST = " 1.801262781808533714595890031901700601953156447751725778899463629" //  &
           "146515191295439704196861038565275400689206320530767736809020353629" //  &
           "38073190695949842873953621603334722352596732052178932328832066541508011M-5"
      CALL FMST2M(ST,C(9))
      ST = " 5.907354119464558702282526469936468606435758208617119141436100054" //  &
           "059798219814702591843023560629835506072948141298603299797940044724" //  &
           "48291452340594666101827864743680486809340363559802370626936931930763706M-6"
      CALL FMST2M(ST,C(10))
      ST = " 1.945928308048298637998047739670960416088458003404533040952133252" //  &
           "019681940913049042808551900699474542980945266314269501249317870278" //  &
           "74908804736762623987426734001119677345511898921612599921683514419564006M-6"
      CALL FMST2M(ST,C(11))
      ST = " 6.430350784891467518365263573957142751058955098451367026716208967" //  &
           "268298442098128927139532681355390234484052117973406407648522438051" //  &
           "09390967296839296849960667716105732496299638299238322412706670233440209M-7"
      CALL FMST2M(ST,C(12))
      ST = " 2.129788087048292585451051353337474816961691545494827552022528629" //  &
           "410231774208766597829719984675128804906172087285080543161655047327" //  &
           "58375412105018997048159105561567647701210317140138410056728113033026445M-7"
      CALL FMST2M(ST,C(13))
      ST = " 7.065818202049355172851064506258762794870685817750656993289333226" //  &
           "715634227957307233434701754849436696844424928325302977575887819043" //  &
           "21794404770003433234833231074275648790842338915961498517998739808022623M-8"
      CALL FMST2M(ST,C(14))
      ST = " 2.347034615187173257148763672202323738899047153115310520358878708" //  &
           "702795315178628560484632246234627121875727895643809584057710305127" //  &
           "87278924224408794242543715977424052595827644319888981305672643671262471M-8"
      CALL FMST2M(ST,C(15))
      ST = " 7.803106649762273600293563029213088249090262679095379843972935643" //  &
           "290282459342081738636916671209602661597101103726013919619715976946" //  &
           "82488687750381269409404895538941410178728305136626722016628210197601304M-9"
      CALL FMST2M(ST,C(16))
      ST = " 2.595999374839856461644621939730454697218953331143174429987630039" //  &
           "542650045638001968668989649549309210492316961761661920993360907581" //  &
           "83943362125894719965856782417889275088010359200837997812612518790010306M-9"
      CALL FMST2M(ST,C(17))
      ST = " 8.640837414389256569577951013532585711448386302359330467618239497" //  &
           "053413093126642271180763027067164825596661845609758534245303980743" //  &
           "08087916612539453037312942192048368911659495539677311896754022319241514M-10"
      CALL FMST2M(ST,C(18))
      ST = " 2.877174665461131520386834493459437941874105957500564898511375137" //  &
           "311439002578360979763874789548515880868154509894190626699457757147" //  &
           "73686016316321832375029203457725272356560152718132192583462548282779007M-10"
      CALL FMST2M(ST,C(19))
      ST = " 9.582858468146311671960437304596644669478493760020748737659683908" //  &
           "789815983387663856449725613266381211899380089748749729674239142554" //  &
           "09417096385045411044655577509743953184783848043803078810843691683066120M-11"
      CALL FMST2M(ST,C(20))
      ST = " 3.192362617049000364818675299493504182177965826984960311647445893" //  &
           "562291482131615616774398545467628898736874378132707045074901954090" //  &
           "87857585211733812448598150553069088798592369166325857786128311238584878M-11"
      CALL FMST2M(ST,C(21))
      ST = " 1.063641452982306778871888232638725499778451985860322579723624373" //  &
           "042743512317431335223585983763945149600366863053831443772337083310" //  &
           "55022604714614305141359222385085339434173650339778299984225419876751487M-11"
      CALL FMST2M(ST,C(22))
      ST = " 3.544275868854612440207935801227503918837302795864246972321724495" //  &
           "355468544848206832825003613889968600093960257823513222253360353453" //  &
           "05713971042571842928244634878395967156692305652444148244943360077022711M-12"
      CALL FMST2M(ST,C(23))
      ST = " 1.181126956967686063705069366011844730919543312398681339013384460" //  &
           "767464082069171562896200433788460949822871886205681867819662919323" //  &
           "00910010651681302654526439685187828766124647111195799560742489176171727M-12"
      CALL FMST2M(ST,C(24))
      ST = " 3.936345173849846585066306986288647881678859105474359687899712967" //  &
           "448625102584861794056546209745855663655522551206324259480980022982" //  &
           "48196521666063662466312955017462793017857218058858813422030279362129550M-13"
      CALL FMST2M(ST,C(25))
      ST = " 1.311929116013669810041706041194547190318825658299932395783521476" //  &
           "062715708679008371003135237649339518303948243863280796328717056928" //  &
           "61088933385870034244218462559681704119429612455202721574336572434693909M-13"
      CALL FMST2M(ST,C(26))
      ST = " 4.372632654299231920401840242323289305929581151976933470616960496" //  &
           "030436497373880193006652887239443630902623173458012149767003641260" //  &
           "91381234213835299457171231108226264358946266150962591099913744278927353M-14"
      CALL FMST2M(ST,C(27))
      ST = " 1.457428209197515390994541694806166533046920066577489380555809169" //  &
           "326581787738147452100480717196733318418099673508724121255442697420" //  &
           "79428502647711159673409529291257489352915130912350484504348941167745402M-14"
      CALL FMST2M(ST,C(28))
      ST = " 4.857804189667246764735021219813567955136816185008613360441960672" //  &
           "940496363503624604027929086312123388047291007867151509866153218472" //  &
           "06384966879455254262531778810269062220190874499923455125800419069661498M-15"
      CALL FMST2M(ST,C(29))
      ST = " 1.619195639149486423325122007106269185336947307372971693371175669" //  &
           "889809582649582191406670950947339285199094038180244016830476463936" //  &
           "02101458401269222147875016156102854033684337727151883392617646517180429M-15"
      CALL FMST2M(ST,C(30))
      ST = " 5.397137809202938955975940495024829822845303110776022583879121893" //  &
           "921705867907147218379583534840013350252782358738567325002357267738" //  &
           "61010468743185749436896970436317859962951219444890985968097249049622718M-16"
      CALL FMST2M(ST,C(31))
      ST = " 1.799000703833061723835456309516522471727635932565177399470291246" //  &
           "245675486739349743760088108709128457742138295133686476564611083127" //  &
           "88305692939487359678233270751804270008374154159339121093086571227836484M-16"
      CALL FMST2M(ST,C(32))
      ST = " 5.996555960166587435989106305417312260461721595507168812416307139" //  &
           "617920826596045537450588005370606978697110903751234286759364296538" //  &
           "88342574088876310757201262943137995532500249048583561327234609512080698M-17"
      CALL FMST2M(ST,C(33))
      ST = " 1.998823729325601300227884046410698198743303225621025482564048890" //  &
           "140820433856911724428946203004508501377274799484714026521360076321" //  &
           "81570423267997358911300788282146663959893146975667195154844645417775406M-17"
      CALL FMST2M(ST,C(34))
      ST = " 6.662675132429289007245318420983808894124038069139542218571745865" //  &
           "030220152998942329578185363084791339999779092891491916899149034423" //  &
           "33275083724236395937279776479045434110852072326963373822046800189057081M-18"
      CALL FMST2M(ST,C(35))
      ST = " 2.220874055111200556512338059264850925555466107705796942638438370" //  &
           "439334710114183793262340326636387034843865608526952566379170604706" //  &
           "27391000045814717730873567070447343919893162122347939701787863570280570M-18"
      CALL FMST2M(ST,C(36))
      ST = " 7.402869382385770801058732735126460283848974699479515940427142498" //  &
           "169090369970169606837637347292197915595099714959178372809210169004" //  &
           "97096397918083614643468550094209927318490673314894967308013075647465808M-19"
      CALL FMST2M(ST,C(37))
      ST = " 2.467612094717547539757300850305893096186640705352512533565093161" //  &
           "524071499801682896819052946685011871151619653049568921247943861305" //  &
           "74027835796362386640519365699573759086773066188473623555158872217826510M-19"
      CALL FMST2M(ST,C(38))
      ST = " 8.225346069033827277619490875386000099087882850547971011202536869" //  &
           "560710353060722052873313849027274314019902150470472049910634941015" //  &
           "65431604021316042692200940962539817779136949003756418974351775701389418M-20"
      CALL FMST2M(ST,C(39))
      ST = " 2.741775128372239167169885703390452991143862808123403590500260252" //  &
           "686098892049313322445848356272884233074331136306076265803163875838" //  &
           "39997245835097889674208425702496742768010046284312175596063967715214956M-20"
      CALL FMST2M(ST,C(40))
      ST = " 9.139233192043922392172869121382982198915872580533647882222959712" //  &
           "340271164148043740483814963159921847750954087090085242066261297172" //  &
           "38743378565324605175470969019495974103495135080039518080872470260574797M-21"
      CALL FMST2M(ST,C(41))
      ST = " 3.046406755195530600990090643743590284251799899012276309359112089" //  &
           "108511649715687692732843107149352212491463042497645778261917818921" //  &
           "00104801732363345270093806101054860184893098558607015592900408049840396M-21"
      CALL FMST2M(ST,C(42))
      ST = " 1.015467841223081888268990685530571588993883516806444440463590063" //  &
           "919246457993838714361388326407974316725799445869567226317393205847" //  &
           "41823934922206879509013719077372316295552261963694116207153809783055715M-21"
      CALL FMST2M(ST,C(43))
      ST = " 3.384890111197057663463016207436882653098338276290627817566949828" //  &
           "395298842402236802648447435554701889903014892066703092691102459316" //  &
           "42305364341869125221913232186873905776329877020426296097438946487577807M-22"
      CALL FMST2M(ST,C(44))
      ST = " 1.128296030524118217901770376786986056339519574517002442090074882" //  &
           "057280000658483297467423203305781809303690428354763800658686040236" //  &
           "71779144955091790824204211870513439500061673502868286865350206893118470M-22"
      CALL FMST2M(ST,C(45))
      ST = " 3.760985085416611180417555002274204359368768836382888775095377932" //  &
           "135470299750400345139854657192126020629674958772171673259844584632" //  &
           "29740983608860531344547744626628209787912508838546108351215194899892193M-23"
      CALL FMST2M(ST,C(46))
      ST = " 1.253661274394284819616871799214565550306261501251793404757960132" //  &
           "128170781722119090896566129331363704599359253979761928823159537461" //  &
           "94504254386899126662346756986738717779547048333427719023221981595254954M-23"
      CALL FMST2M(ST,C(47))
      ST = " 4.178869862795538345454967070155508575326951978759241233273121057" //  &
           "066301381631679857138532781845567887396083218733585140280524330791" //  &
           "93670559242093402244963076978237398106292624308475677695772071716993274M-24"
      CALL FMST2M(ST,C(48))
      ST = " 1.392956357970703804166482382560887146464466644976989900838409857" //  &
           "521105907861179378438919270011579646317439048476258541449951226731" //  &
           "38419505723140506483598868257584439349831369024815848605796104120125198M-24"
      CALL FMST2M(ST,C(49))
      ST = " 4.643187202503244267904840086459603070244180285978311534148487451" //  &
           "448533368113912140098658002328251742282983537128133995397515041612" //  &
           "48378614922938242264999090255128209599611777303756393485557336865892384M-25"
      CALL FMST2M(ST,C(50))
      ST = " 1.547728903152056758023013339236654329438934392366540425603322082" //  &
           "857263082844161081125623346144761860535134488231475517768722978006" //  &
           "97598742236912925627747252856046998990338903727397536889459369962969529M-25"
      CALL FMST2M(ST,C(51))
      ST = " 5.159095932969129018305623899832229004426259340923333088824701344" //  &
           "793799573384328764667708388371262541343393211413511112875917018796" //  &
           "26630534722311094084888639746029822030436534231881230203368649013498048M-26"
      CALL FMST2M(ST,C(52))
      ST = " 1.719698541605578250055328719172355061652680958536807515991957911" //  &
           "829231225440009541518975734286993664349423799759754184346111779848" //  &
           "98079517451175822251033508127845088987176652373067048423597073312187144M-26"
      CALL FMST2M(ST,C(53))
      ST = " 5.732328215225532836270189877348903743690272187963364883413404892" //  &
           "515967840972047550982297271962784377794755127033081531768642894199" //  &
           "64735397060888178165089361563869938931990240827823701959473704067979558M-27"
      CALL FMST2M(ST,C(54))
      ST = " 1.910776007543699055364968001468624686158867348024934230266622903" //  &
           "018662755804722736204383531033467520086936412589269186167968820789" //  &
           "31707316415310370070101583553225900248595006769483528789991489268005919M-27"
      CALL FMST2M(ST,C(55))
      ST = " 6.369253197983874006821823217108741793471311336545161803572473280" //  &
           "581604669538018391744708133203440680673579856484395859330652484509" //  &
           "72888929728309163536568371996260645727875086529584444740409818707168597M-28"
      CALL FMST2M(ST,C(56))
      ST = " 2.123084359204225333255211453879706270058062440003428132183384754" //  &
           "529220299115266894193502387449244415133870841331224790006731846829" //  &
           "83950869831354206398680371015186352923965072680137000319344572605758909M-28"
      CALL FMST2M(ST,C(57))
      ST = " 7.076947763704848854568916919680447890729663289024670563717349289" //  &
           "716854593909643699182828273092172522117888394772977756940206908939" //  &
           "88161833503128621124125052157189818939182888192518392960394844544910580M-29"
      CALL FMST2M(ST,C(58))
      ST = " 2.358982562824326603834177965008914574063289371087237954036406546" //  &
           "340493525463133458597261132503854782619333331885562197138535071408" //  &
           "23201579847913351266307691059142085404786612749993269600147335936631424M-29"
      CALL FMST2M(ST,C(59))
      ST = " 7.863275146721236242647451555165511648580559926859636607085030645" //  &
           "828570768337179488776594015720853254496569344629950772241890794413" //  &
           "09501208585846001333592788550065884722420498762308117260230188148248777M-30"
      CALL FMST2M(ST,C(60))
      ST = " 2.621091699900456658110115330583854052361897297784388638216583319" //  &
           "454384526174907954107229789803487469826658513137203063896172131706" //  &
           "68596675776534544455338620395186907938046711829117294010990223395191200M-30"
      CALL FMST2M(ST,C(61))
      ST = " 8.736972293818315676196547510693128031654563870400207411221472197" //  &
           "802537238798558154077761157016481387194295979174198832797107057716" //  &
           "55476119097180397397991205334340641881095558538017176052985551441032265M-31"
      CALL FMST2M(ST,C(62))
      ST = " 2.912324088143640003892515793451613942403905838900460353749079195" //  &
           "711293166403329382629400828420996934679831800044190489170302330372" //  &
           "89702647828398182289986576193569329546458981411599932131292250327217139M-31"
      CALL FMST2M(ST,C(63))
      ST = " 9.707746935989309774922153324275043050136559616419228148856384390" //  &
           "891207031590122433573161121435856761535255587985039419083521928281" //  &
           "47684192432450134453307134064460958883080711047615861323976659104916668M-32"
      CALL FMST2M(ST,C(64))
      ST = " 3.235915639207398595265693615961166003757636977735808496558273648" //  &
           "928958056872747435108942393361659134982792854945130370217146958406" //  &
           "96083858500218202729498941860260710013473689957089195606216847161948489M-32"
      CALL FMST2M(ST,C(65))
      ST = " 1.078638544871873611955050957012575767956166828939502396913361632" //  &
           "134006203107387187862076294858437749004255229312668679921800233056" //  &
           "74417612290238598612397811829199658493331617719065112441605814593748378M-32"
      CALL FMST2M(ST,C(66))
      ST = " 3.595461812413097732268985825592100369404790978561873060558407383" //  &
           "209843595084925537819419221457817216101556873881028197503043710989" //  &
           "71993446117511506289219252721788791857419650690058381970466123873747686M-33"
      CALL FMST2M(ST,C(67))
      ST = " 1.198487269847745765578016200858552396971801017866213814921147936" //  &
           "194342329888616529805943628521815471009477410250705796740507122991" //  &
           "05724055220198767971701073860808057115160815984757015916251499607980512M-33"
      CALL FMST2M(ST,C(68))
      ST = " 3.994957563767602385747901769948142409291248613545961370363883891" //  &
           "289691397311922302379137927297721501329041681797620724073370767562" //  &
           "18203408450216245071443700831249773241142322486002435752961943222542254M-34"
      CALL FMST2M(ST,C(69))
      ST = " 1.331652520657979959724598554013660845601546719077233063789633368" //  &
           "960435001352641490522946960948433409671303505299895763210435682642" //  &
           "64618257123231948431647534610234863412519330901304158646595801812800221M-34"
      CALL FMST2M(ST,C(70))
      ST = " 4.438841734031881188976818132822337642864369008251133361954967150" //  &
           "427526248489023180060951578854782817826656197035293616050385245228" //  &
           "69777224342488088789975020608283234140930896389710286843599614310829011M-35"
      CALL FMST2M(ST,C(71))
      ST = " 1.479613910970280742873945057507129611414800958022743884631361380" //  &
           "691798159539547487537052914060034185767422767130178282321602757860" //  &
           "83348663879834983529553026971940768027904120497673742153199914849116728M-35"
      CALL FMST2M(ST,C(72))
      ST = " 4.932046368966736707433238163653472803158736679721922230398981807" //  &
           "148509857901058886361798749120231700519606437174141329665127637580" //  &
           "60684029229789780624847253236488984529122857484143590545454988280087620M-36"
      CALL FMST2M(ST,C(73))
      ST = " 1.644015456088695799904278450146142178669043440878937685558456872" //  &
           "964025555598274081805412440896705440428517289000442501284221387694" //  &
           "55648732942491166386921283069386388704660866255758554699458240090291278M-36"
      CALL FMST2M(ST,C(74))
      ST = " 5.480051519711778255840272708324976030739754268948537269674402054" //  &
           "568534364217773209485346648013194520472068667682265478950114643548" //  &
           "57817502762576875030228023959898997369951420244275748984693326649040707M-37"
      CALL FMST2M(ST,C(75))
      ST = " 1.826683839757957485171861873006044105081660879173944661934615164" //  &
           "058228248983434414103054534197242809510738787922754612090096010008" //  &
           "32972234391112427466530639806131521397347115826207673334357276487387067M-37"
      CALL FMST2M(ST,C(76))
      ST = " 6.088946132161603455339501324095998745526413263096303935940878780" //  &
           "526359496483279209617877435619263014735196336319313697924877016533" //  &
           "09653632325766553783756318556864993407257715531353455924785891667301524M-38"
      CALL FMST2M(ST,C(77))
      ST = " 2.029648710629304112312264073466486255977035674544143740902384580" //  &
           "066925825480897303013099124850654602496048781418796500497911435814" //  &
           "55895496319542786203501573229289360635504754435873888697991558050538067M-38"
      CALL FMST2M(ST,C(78))
      ST = " 6.765495701869604444386835156000328903984913424070294867383326826" //  &
           "380125179490385914541280070377395224806865489400873522529357739540" //  &
           "13593097642976584317974244356761507356447028338622758884083690956549688M-39"
      CALL FMST2M(ST,C(79))
      ST = " 2.255165233899515832701742852677093869944624882375775392021565853" //  &
           "727820148309140426329940190201858812882180125645736863330093959501" //  &
           "39799185623421035440349003272811535983000708934970150324591131369976675M-39"
      CALL FMST2M(ST,C(80))
      ST = " 7.517217446189171987910422000959627404182160006415193289405563037" //  &
           "073141918979212912887404738051712797448922513382013488736054976966" //  &
           "44850928549751713769786767895881754130344984097285305963557272547145616M-40"
      CALL FMST2M(ST,C(81))
      ST = " 2.505739148694087132524150695488551963220019249166337393102845297" //  &
           "929187685834976527612995960496060104814687111155509389611886823768" //  &
           "45665657833105762416203034605047428562380070279581109059970215759343049M-40"
      CALL FMST2M(ST,C(82))
      ST = " 8.352463828891198283454574490687921776166017804023933180339411723" //  &
           "551630949077812713723188475201183251308083861018090692334062700224" //  &
           "19315941542359676555596601655478811403243506810721773017251413934374514M-41"
      CALL FMST2M(ST,C(83))
      ST = " 2.784154609608126388309518953024776333056303224324599419154596270" //  &
           "687189694467353799519873714017939494084666475975688987152456659642" //  &
           "26654762776582515405508388836892873507912509482458807279902034848895589M-41"
      CALL FMST2M(ST,C(84))
      ST = " 9.280515365304738695722333964470955476404932927589213406274394075" //  &
           "824251507915677707145886824360137294712999378365268640074244947377" //  &
           "75283581062718625871709431187118953937401911092491908328625126927814924M-42"
      CALL FMST2M(ST,C(85))
      ST = " 3.093505121754325582272552779234873274821484670440649874514125833" //  &
           "450221430848146677414227286193944601969620041844899866725992046638" //  &
           "96302520509550456127533362858925937984066799183063861137995551939093475M-42"
      CALL FMST2M(ST,C(86))
      ST = " 1.031168373914628365020619376010732175534509522276934226759611010" //  &
           "510330113936747333958108874337789248731979567783939919999577683507" //  &
           "22523295728071715108784714082583918137083993400414421602035476389181042M-42"
      CALL FMST2M(ST,C(87))
      ST = " 3.437227913040060810736801880470831506837625258770689684886108518" //  &
           "300849420221764767894609466818173933155866849989575822424072255004" //  &
           "78234006087579117395941989824600255765731792341982892120664183263431722M-43"
      CALL FMST2M(ST,C(88))
      ST = " 1.145742637677845168748014850793946505879656739125856631649586463" //  &
           "591822853689599347930344326959430427527959142809100423533805439264" //  &
           "43279388738637495289806414123857142554621616733197424850302473869617722M-43"
      CALL FMST2M(ST,C(89))
      ST = " 3.819142125587379475420211530141217153875049185525247582904249203" //  &
           "019903748875977266231256469090038860233530356197288111009590135316" //  &
           "90898695851949084954771545562975566659699180249151895098720201541444897M-44"
      CALL FMST2M(ST,C(90))
      ST = " 1.273047375194433720039269643742156618287252568559809316454558670" //  &
           "180525454213261685018950248687574295353166212229651718103175620816" //  &
           "43760967542155803486826726482394407727128624904626495857158860946505910M-44"
      CALL FMST2M(ST,C(91))
      ST = " 4.243491250644713804047213900311521631782151086428302312117409417" //  &
           "778324805156532130172471108085960753421387148016192082165090341252" //  &
           "19545466280371097678329835612276755167203138210639186157532227100590980M-45"
      CALL FMST2M(ST,C(92))
      ST = " 1.414497083547388285661813522776106834139405615052171085969597516" //  &
           "047278841879024411976249475067654110853871648213868458414159189231" //  &
           "79891226179605118296023118379963942164595272691469246552143553753880550M-45"
      CALL FMST2M(ST,C(93))
      ST = " 4.714990278489170162988560867348152304757403878210496541497712053" //  &
           "006454958803147191193828617164924983154190125072717651655296406044" //  &
           "51236582999806300249260089437863523751237893569424876357019027689437850M-46"
      CALL FMST2M(ST,C(94))
      ST = " 1.571663426162525690358614617253775521309331567276009924639764177" //  &
           "358960165102899050236124157794756906336803102013665429067943394214" //  &
           "57838273206825762014705848617755003448485196508751247061517100802618351M-46"
      CALL FMST2M(ST,C(95))
      ST = " 5.238878087207091391268381971748475496510636983623239581337201528" //  &
           "761219196441713174722639342282806574120757748535810991181068549199" //  &
           "89053413431795493494028007267927630087829683052755847961739807896841957M-47"
      CALL FMST2M(ST,C(96))
      ST = " 1.746292695735365236274430121252895780353566878171851946466982282" //  &
           "916481274302755866597786442055146506112819565744487880483609437125" //  &
           "77321479855945945213560009077900564250744371125376843314616795699839063M-47"
      CALL FMST2M(ST,C(97))
      ST = " 5.820975652450387718877296368406106766373716711813705919181086521" //  &
           "999505975590854145900097731105648953941956782748776332821329494966" //  &
           "87768367928768066507060035439222042970126325626238817880646490785855621M-48"
      CALL FMST2M(ST,C(98))
      ST = " 1.940325217483255139116418908036918257576300407306275798031334364" //  &
           "409601786683742364221787811915022144162851994955921858529462196970" //  &
           "10653973717672741870614055497821682510355774493997650251816154517127732M-48"
      CALL FMST2M(ST,C(99))
      ST = " 6.467750724943665212448072244898217344228281509203436692886144544" //  &
           "710488376123702904475209151820097662127258578415899891069730495416" //  &
           "37336243969682934496722083054141458419645859035208215900305459742433866M-49"
      CALL FMST2M(ST,C(100))
      ST = " 2.155916908314425424664368170497409014232503939749568528677540140" //  &
           "384039734094541956602791846039169318650021459160866243512075243331" //  &
           "45679505658773347404938241005432513933656138666470399349344701169458177M-49"
      CALL FMST2M(ST,C(101))
      ST = " 7.186389694381093966835437692494728822349002314959532010292364775" //  &
           "394946247059389433028322727013522877670270796020110428790305813054" //  &
           "79391776953852548570365845653848831536967043669276608555999675341559488M-50"
      CALL FMST2M(ST,C(102))
      ST = " 2.395463231460283626767035225609077295862357711256733986014148707" //  &
           "753272199463531514035780844426290282772850231675309468999618270293" //  &
           "06226615257093166993337223904134396989045750924820301622864539591983840M-50"
      CALL FMST2M(ST,C(103))
      ST = " 7.984877438200742850444847499777309845694641795079466763347310518" //  &
           "973477267679513036692556925499430697000376845097566527468301532263" //  &
           "68593074139587048157403440914703927108798266764367635240248126118737063M-51"
      CALL FMST2M(ST,C(104))
      ST = " 2.661625812733530307120299705689856310419430305299514816454219978" //  &
           "167720121577781636596013737099879504260242494410428404547299997096" //  &
           "29214426445076866714408263418452627026007712187686306994919339537176315M-51"
      CALL FMST2M(ST,C(105))
      ST = " 8.872086042444974416164378071031044765486837933978062891736690757" //  &
           "063302976670153482438120949026393088923322321063932750682909484640" //  &
           "80118939886335367290880449750827102760828907286267653373299382121005954M-52"
      CALL FMST2M(ST,C(106))
      ST = " 2.957362014148293153495637994224156508433858704035708219961848704" //  &
           "752814743630032704058576844386754167614614033997051904620347311738" //  &
           "61904345412997662020042929619746892567448362596680832152684925750243880M-52"
      CALL FMST2M(ST,C(107))
      ST = " 9.857873380494231381920907655510228783611078844545021454399920772" //  &
           "141062619227406263790226964711069068688447840761328895136653289698" //  &
           "66981214088475384265075552305006338119605469313298818722398857563732976M-53"
      CALL FMST2M(ST,C(108))
      ST = " 3.285957793498057344874164686873206738067743142840216191903487618" //  &
           "019941680979531742712197349882728018267551869210987929616034749064" //  &
           "30303538626114994902987549690732156800575304242342439460223044767290112M-53"
      CALL FMST2M(ST,C(109))
      ST = " 1.095319264499347502683187139319325563979656486096742174653628128" //  &
           "976581637422575945930267442158932925061363256302618217044235776727" //  &
           "78289678865564842523041070219617158649346807209669750276034200426122137M-53"
      CALL FMST2M(ST,C(110))
      ST = " 3.651064214997812644923454493506674835323388056021714399807493703" //  &
           "605967302048397979154313095594794645711111183057404193592791956678" //  &
           "66172552061879738476243285533776439739119193738969145723129786576511450M-54"
      CALL FMST2M(ST,C(111))
      ST = " 1.217021404999267790636025855753778604335807084870166221152973014" //  &
           "506916159759972779875308117306931009803008009678064932776645093318" //  &
           "00842098229392080507684746488956996056988713932200497206909259105378943M-54"
      CALL FMST2M(ST,C(112))
      ST = " 4.056738016664218241273938781923458650417321982457636718798922936" //  &
           "477302304740685369168107498238001936713766023175775737524892409160" //  &
           "01634272341340971077361157145530586562647498591786411480985240150120553M-55"
      CALL FMST2M(ST,C(113))
      ST = " 1.352246005554737481879776083250264536787105289190169898749791140" //  &
           "348233188200740056915744202769004512689087720776827335229272164151" //  &
           "84364840446652924162659286494044457495071328336364564380197269062778195M-55"
      CALL FMST2M(ST,C(114))
      ST = " 4.507486685182453443237078181203119239131669164992680684219261837" //  &
           "277644454396847551795676165306867658683702735066801164086167252711" //  &
           "19484010084871947031012001315393818015815520382515662702573278448350109M-56"
      CALL FMST2M(ST,C(115))
      ST = " 1.502495561727483273655148872429157437910868921767186291387141573" //  &
           "512573119402964065902228084442655668385002115354783777594857713998" //  &
           "29523050153288241379342972449409357035473017504499197092941874010814147M-56"
      CALL FMST2M(ST,C(116))
      ST = " 5.008318539091607893624136610372602345176507591543467689236137943" //  &
           "884673550591046086592830872513439140608629637038740043915787148541" //  &
           "52394870950686380101306680838055095132070888200259040353691065606620624M-57"
      CALL FMST2M(ST,C(117))
      ST = " 1.669439513030535209901455630134076834170615280098683744729402670" //  &
           "296396657337808597037921736436660468249832659227172779367140247404" //  &
           "03362990043020714217884143429674645399729964793508983794394882884002423M-57"
      CALL FMST2M(ST,C(118))
      ST = " 5.564798376768448813071710669353659573416270465590122350543315121" //  &
           "413388656111319238743752535035180158976403027283093627581775200347" //  &
           "08950549480561185545001882864566882001602568174009919346046996759045750M-58"
      CALL FMST2M(ST,C(119))
      ST = " 1.854932792256149132707284865787646721627824417634136205769228907" //  &
           "351320889807301200265876310886504825399338639660085162561571671143" //  &
           "80178781198866361165024767169800779385647991333075996890986127709064413M-58"
      CALL FMST2M(ST,C(120))
      ST = " 6.183109307520495929899402826852708229420151109621107470264900121" //  &
           "134345986789710812316854495785567736467232785746686941727193331941" //  &
           "56103871513252933011548432169303614875570158457053586066135942237566488M-59"
      CALL FMST2M(ST,C(121))
      ST = " 2.061036435840165015185247594351604598673466050924812223481238383" //  &
           "744650549307488207514436927186243454249400309582767262472952477865" //  &
           "39488089477469737100729325446476505038057243548633416390135776998909246M-59"
      CALL FMST2M(ST,C(122))
      ST = " 6.870121452800549313664441945028230766782586053733765495536930184" //  &
           "148735432170307184364193773249356218482192867580453706078982702322" //  &
           "36906147589341063176294087284637487815185909131173012735635871205970777M-60"
      CALL FMST2M(ST,C(123))
      ST = " 2.290040484266849586983218139377689608197401631939278155837315152" //  &
           "749176998379162942186288612171254733497127165504629660957484257215" //  &
           "03960145633908464981886082313132803826782445898913850664341670496286810M-60"
      CALL FMST2M(ST,C(124))
      ST = " 7.633468280889498162681737525654781395094610920374961079941892134" //  &
           "693686042107521045090976432509818240749828811152884417434183792443" //  &
           "40208045334338980097933050903816256868615301460865703415771912193724986M-61"
      CALL FMST2M(ST,C(125))
      ST = " 2.544489426963165939078331773845571337558244149440634270398446661" //  &
           "730902552806699411443989280898922405978534154707887233634737588764" //  &
           "69277444899419995451128654151512085551424867809084295696469106990521519M-61"
      CALL FMST2M(ST,C(126))
      ST = " 8.481631423210552842388820742776895367676872160158770358942482422" //  &
           "735183525513672190915958631633045631581296672220414159119266677730" //  &
           "00882790484855601476924199497856972658319507003728226425960873810966916M-62"
      CALL FMST2M(ST,C(127))
      ST = " 2.827210474403517542161535621759888928745745898925998679999176972" //  &
           "500449767039313885942472306882332999282623012200216004408023722942" //  &
           "35548228163002484117039136830425719490113419540684925428659006215127579M-62"
      CALL FMST2M(ST,C(128))
      ST = " 9.424034914678391627284940507974624769084040519392748443016620129" //  &
           "151493273032159506798645125157903918497199981418255987213825116544" //  &
           "91313520782402912277192097971388451766192404480480266469096595803740649M-63"
      CALL FMST2M(ST,C(129))
      ST = " 3.141344971559463830781602278189827439570491126580879213267566098" //  &
           "634582653573961371388430476987677106802274861280003439184958337497" //  &
           "55871137194361985101053142410164165629409400589300651854575488722127625M-63"
      CALL FMST2M(ST,C(130))
      ST = " 1.047114990519821265682189619947088028639202210810159318824710297" //  &
           "418144878455534152843738752328520535580617712220747998447348456148" //  &
           "87128390653434870273058245640426383654361423379776610643883264096972007M-63"
      CALL FMST2M(ST,C(131))
      ST = " 3.490383301732737524161437551201638806706639111674055435356822668" //  &
           "242340068122359401431212212101510842260288841134358619348306937471" //  &
           "32292385947826289933592114188205257470612591292887863954967567373067619M-64"
      CALL FMST2M(ST,C(132))
      ST = " 1.163461100577579167692347221578745581156399690416630219259420060" //  &
           "140471624651128200456995286998175955667309375958539555927592191244" //  &
           "69108542176232753118949875989033621399852815112398485508138602381100101M-64"
      CALL FMST2M(ST,C(133))
      ST = " 3.878203668591930541404160833208709487069318555519787588536748926" //  &
           "079489227092349654317217820179356059640911818309293644987827038914" //  &
           "23113705652681321350172045452704473329699236304435049788227394174622197M-65"
      CALL FMST2M(ST,C(134))
      ST = " 1.292734556197310176075471134722937741048543949863716607618949311" //  &
           "134596575499614268058353886586544618677755951138400973297684683800" //  &
           "49929217081801053935564115665217627139106390261578300958135297518693689M-65"
      CALL FMST2M(ST,C(135))
      ST = " 4.309115187324367242603447591543502614635432074915146455974961100" //  &
           "783991800140039817841446475586229840527592677387439828807409996406" //  &
           "58572000140795436674747033557343891137764503605700790270107002153194098M-66"
      CALL FMST2M(ST,C(136))
      ST = " 1.436371729108122411455785149464652955014483913555925454567566343" //  &
           "033751539926464502402671954114290440329353433043868042553216356430" //  &
           "95760927792248401840769385171665086466412488627981015154342470344616262M-66"
      CALL FMST2M(ST,C(137))
      ST = " 4.787905763693741364655873712257673345968093702500400380413759009" //  &
           "343709671855218190105116076110599847270179980048633459127967075496" //  &
           "95329570740860986232312515609865359747150038120760918167660196395395859M-67"
      CALL FMST2M(ST,C(138))
      ST = " 1.595968587897913786502772040929797311513936155534630802777161042" //  &
           "323085710283013809416229252137018873920387828320283701537288724338" //  &
           "18222527779868222288064331588809401461453186408435455724607829956650895M-67"
      CALL FMST2M(ST,C(139))
      ST = " 5.319895292993045950719608811875781719400488218015940339624963833" //  &
           "904468790238424394665705538673163852924018058746539220799914498871" //  &
           "87123883979205073632821149986880853121193839902134978166092188353175974M-68"
      CALL FMST2M(ST,C(140))
      ST = " 1.773298430997681982500795106152343395146291244985375992301024831" //  &
           "172880492620523208419431056495256021417076918908947764372774801634" //  &
           "66308162036559401365955897830840212263006984532789801526556111330540563M-68"
      CALL FMST2M(ST,C(141))
      ST = " 5.910994769992273272321630775993113357374306776150164446878995429" //  &
           "199988365546454090281139320572057370171863201292260358152263114701" //  &
           "84849003352679924628129743645300899848128310062948455843231411729916686M-69"
      CALL FMST2M(ST,C(142))
      ST = " 1.970331589997424423436955364217490328008758498727308342596972914" //  &
           "838317948717210312982993577885955575234727205227508863543462414701" //  &
           "19790327280248261234206607030131137515730415891713737174983639027604997M-69"
      CALL FMST2M(ST,C(143))
      ST = " 6.567771966658081409780880644570952852212869394142774498275969597" //  &
           "051352302027804591436366292098249004955749719212407233759626410804" //  &
           "23094265521905707737671802871297906382006218125926665760693315871567840M-70"
      CALL FMST2M(ST,C(144))
      ST = " 2.189257322219360469508050905240435788723932139126639198455650265" //  &
           "181880562634665346793855787464756222043343074562742957750457331886" //  &
           "51136835170527293161085727720893207671856322049649329515124067695351675M-70"
      CALL FMST2M(ST,C(145))
      ST = " 7.297524407397868230646229744835872327874966073160276767231213689" //  &
           "623218792935837978230418401268856718201925396539510384196175753170" //  &
           "66652198429614667985969715027449338257278758408128943199367755324652975M-71"
      CALL FMST2M(ST,C(146))
      ST = " 2.432508135799289409953591617661733991864335564333111377209692880" //  &
           "074976374750213242253051652321561734041852268969020358465621089424" //  &
           "14424996317706067303976880419998421002358404179645130678096779653358570M-71"
      CALL FMST2M(ST,C(147))
      ST = " 8.108360452664298032524092931381732192579715420011362584102530122" //  &
           "399082594247638810857284958385615023239610775424662458807879174869" //  &
           "99208578767507795046313489342326234225642585624250987626962498888032182M-72"
      CALL FMST2M(ST,C(148))
      ST = " 2.702786817554766010677727150378738299179197264006681328494022283" //  &
           "461713062938474709283827967021738008108993688482511856757752986924" //  &
           "83022205593730217582984347916302439090400201412445704126518051302925734M-72"
      CALL FMST2M(ST,C(149))
      ST = " 9.009289391849220035183332673115118436689105231658609100131576105" //  &
           "746817524522178460300592879017536531814751063263253522537492214691" //  &
           "66287676119612980647194230638456363682773535596502263016913065096674627M-73"
      CALL FMST2M(ST,C(150))
      ST = " 3.003096463949740011625527637203839580670438558448697439316922582" //  &
           "080500699241877972753395936644410394325148621157014967621235051277" //  &
           "19702357955843408402299203602763881244696185804277033716869763248333675M-73"
      CALL FMST2M(ST,C(151))
      ST = " 1.001032154649913337182941137354579752756157403350587184082572655" //  &
           "668720365134577993101924383011283318540511319882434333492124758136" //  &
           "56328900549930706700593452010716708224185836746712866383541238461850333M-73"
      CALL FMST2M(ST,C(152))
      ST = " 3.336773848833044457211857959630261241457227305512566083673355396" //  &
           "481818433800591390918626541367515865084768432579659244905807486980" //  &
           "29134378942434319177868848768173954755492210535184756504211022367766391M-74"
      CALL FMST2M(ST,C(153))
      ST = " 1.112257949611014819054635753320402681213887420341206221531310860" //  &
           "985682956261502881389306545840511198169923612506852778221149565252" //  &
           "81459361368566900183720441504615495296290954141555036222396831117077406M-74"
      CALL FMST2M(ST,C(154))
      ST = " 3.707526498703382730161873261392491115828186807291897198915909604" //  &
           "155674067990161834342683376529820781124868349247913942628339699553" //  &
           "19187594923162356418905498458595704597809148916916571510786341062302288M-75"
      CALL FMST2M(ST,C(155))
      ST = " 1.235842166234460910044062971858447900664274034201950858925878124" //  &
           "083887957837752863211714821751135044697824600808010987957735653910" //  &
           "85021760966460551881259692413828184054079514253554146831394157759200228M-75"
      CALL FMST2M(ST,C(156))
      ST = " 4.119473887448203032928247987246026060335598144698999438082486484" //  &
           "981872504026407021712061503540785866057593612250257886109051123261" //  &
           "20553540800492774307915913243657294990555992068366814198248544293239073M-76"
      CALL FMST2M(ST,C(157))
      ST = " 1.373157962482734344300829830088785762819828470154677406792486349" //  &
           "582215434835624433211455183472833063222300779138846503724376654756" //  &
           "97153990917671843512951431419776274311910081550489091472125204982219742M-76"
      CALL FMST2M(ST,C(158))
      ST = " 4.577193208275781160871184364111765898905714315410964770181334551" //  &
           "559734462338946414187167832153901075953527960902149729732952392999" //  &
           "05739092117020357910064590291184764747354548289282230511803212990314666M-77"
      CALL FMST2M(ST,C(159))
      ST = " 1.525731069425260387008576333292791990843668717239246139931723054" //  &
           "113730949750810518714216058816633073359740920341684018573316882922" //  &
           "19161413294992398162552504179370639600545735768135221710078494678533369M-77"
      CALL FMST2M(ST,C(160))
      ST = " 5.085770231417534313032798702697681197383439273915544588375043156" //  &
           "982436257501835267954582765438953697752001176322824202363456833480" //  &
           "84822237757173265524313083452491401792161623828870993995239601500752613M-78"
      CALL FMST2M(ST,C(161))
      ST = " 1.695256743805844769784622112797766089615978935755501503220932068" //  &
           "343174291091693715417344263720144349029281513168498097220402832110" //  &
           "79996411923403065677514822313169241774858920814773828257766665033772313M-78"
      CALL FMST2M(ST,C(162))
      ST = " 5.650855812686156039255381634430268704055968949073218709347085199" //  &
           "742155287063313738806948673339105867918184157411397848315417982770" //  &
           "28216209989585045629974649193921980908524940232212434747464651403766044M-79"
      CALL FMST2M(ST,C(163))
      ST = " 1.883618604228718704942922337070948507896675501998486050379053659" //  &
           "203222058229334282486849245788565645787611354772468558241052640628" //  &
           "68090996198582244338892337657054683701568414292331480633193743238899042M-79"
      CALL FMST2M(ST,C(164))
      ST = " 6.278728680762256512149009149976440283047018296660977205232840187" //  &
           "711671745561355189005624289121525977497317264018845068610009630166" //  &
           "24422122072055952477161863795374233113339410369040977100353306447007727M-80"
      CALL FMST2M(ST,C(165))
      ST = " 2.092909560254085021920435547839978227290505174219696260063426880" //  &
           "879036670946017746113568504819405540190923727599189799888162522342" //  &
           "82688033004648088111864002916433340227047169032122354503266913856888244M-80"
      CALL FMST2M(ST,C(166))
      ST = " 6.976365200849595928568515913272594158672772019779337814697530205" //  &
           "514328112647957246760099455857571687826289491085647676946101437355" //  &
           "54833971648944261391739603826369822509790583940615701556419202194043338M-81"
      CALL FMST2M(ST,C(167))
      ST = " 2.325455066949873863606967397576505711197188840184692731505571050" //  &
           "057010126988203224760379663164013876837099897428308506101979830146" //  &
           "10509395537389044680077986544130564059305007513097257902320076380128891M-81"
      CALL FMST2M(ST,C(168))
      ST = " 7.751516889786273230333744396009595784689618339833872888076148749" //  &
           "510954270591897268986918218161521302808459344904049745958883339834" //  &
           "97636368620176777364813719377132199681517454923842508307735464876551893M-82"
      CALL FMST2M(ST,C(169))
      ST = " 2.583838963261950823751464199102541531987080961128181249653133059" //  &
           "486343391931814427787289188223115323267471180053604447425299309852" //  &
           "43333277896763688823725567334545875021894956285785557169534759498102028M-82"
      CALL FMST2M(ST,C(170))
      ST = " 8.612796544966339593464148319279864385495846603731409332903857040" //  &
           "341111578596179250787433586544432993469429659882790724588244062567" //  &
           "78136790746398664694553035857800597304056454220060017665421505719967290M-83"
      CALL FMST2M(ST,C(171))
      ST = " 2.870932181657563853682489496869596716118149568767000014172109841" //  &
           "753698478327036128411797178375480340579847035333592372289787795793" //  &
           "96627619330877369349750053557082939116418335842754914658257204585102926M-83"
      CALL FMST2M(ST,C(172))
      ST = " 9.569773927459502329189843968932453842109493094807395751883972275" //  &
           "108106634425357758674379648287072224301747951696615265196165041805" //  &
           "20236585969398304163224702023392754793840706634155432621406698219669058M-84"
      CALL FMST2M(ST,C(173))
      ST = " 3.189924642457193947274983048068918789864588583187967339730200869" //  &
           "383977309483707594218810831491599791827472770895338811393310795354" //  &
           "69234531929746996008196354596059905584934184315535092321474774208305020M-84"
      CALL FMST2M(ST,C(174))
      ST = " 1.063308229833233763649833968823830359312501010885534593366422641" //  &
           "631138341936187529001129656673501820789940104427992740873815715203" //  &
           "53412492554104141309331928436306992144198744553482555692829414468432600M-84"
      CALL FMST2M(ST,C(175))
      ST = " 3.544360766480896268661948406666249396818945618937640720661704673" //  &
           "296436347072454208561096669377429691244542539862734276732737789906" //  &
           "87985432036327462359694991859663225608649510569830536167392892074714882M-85"
      CALL FMST2M(ST,C(176))
      ST = " 1.181453391991692193233905477505817491959791629667967979777058345" //  &
           "391387128260137859898575835696679473316746285686162580343604354886" //  &
           "33746615556680423641254628884245686047585862162458511355057706178924765M-85"
      CALL FMST2M(ST,C(177))
      ST = " 3.938177969065104259816577437558454136986820863927184608342465808" //  &
           "644130213208017150749164656763736226904608349061342579349394401954" //  &
           "11955443745482049001748543874113465084785050139876732361231723529022225M-86"
      CALL FMST2M(ST,C(178))
      ST = " 1.312728231460229835781775849221721497298632770454454059027673781" //  &
           "147657857867325372034109920647863595009065024284231880168892910826" //  &
           "53488283009582034319652319525792167583043745345137953029043134270740314M-86"
      CALL FMST2M(ST,C(179))
      ST = " 4.375760815315077535413782421712416021566276398233684094737691912" //  &
           "768825492780444215747826165057411744207345952312538488710400858828" //  &
           "27700668026171627589196059317745967100234000846254430975531508363872519M-87"
      CALL FMST2M(ST,C(180))
      ST = " 1.458563929048378339389356324275832391447007080182288931684002700" //  &
           "009472184445323295339567304943041273340490012950372985830518037286" //  &
           "27460201464773612755677066535469683435847032008950889605333804095236722M-87"
      CALL FMST2M(ST,C(181))
      ST = " 4.861879359454319859107850617575234255777767433683221708284430699" //  &
           "990829575672698923326870227967687183467108336363217071248382357963" //  &
           "22818905179326628057052156616846776700350617514896748728636343647107755M-88"
      CALL FMST2M(ST,C(182))
      ST = " 1.620837575639336717669585897558965566795189581102158562505695709" //  &
           "761058058214152430539033470517727442776271455644632399079159816470" //  &
           "13032790990474394168725899077889253682309738675257461400762111892656053M-88"
      CALL FMST2M(ST,C(183))
      ST = " 5.402795219201229254205870902900594939962067229843983888917879564" //  &
           "569447106085277432976440651841249190437245953800537313805443864980" //  &
           "06342541344494885255739000367067646594242251182208803771724906930826406M-89"
      CALL FMST2M(ST,C(184))
      ST = " 1.799216964519093606731219963783752545188788013842673930468641591" //  &
           "448479933065901726577672674840349510506058583228709079379586399536" //  &
           "92651555915483777543359909186871398885320260440340033124792023837781809M-89"
      CALL FMST2M(ST,C(185))
      ST = " 5.997366309057088767075298231986828202847652039333576421385057338" //  &
           "145629178278871106675461140725723229061908016307033578803459780175" //  &
           "07666172639242691690123978056031089958911595034842389306632163036435150M-90"
      CALL FMST2M(ST,C(186))
      ST = " 2.011301294926252140726390741032210138299321852132295904710257720" //  &
           "158209117243470457856408196331997445864262615242552875300814542975" //  &
           "47170830674630176097026011113551746496002275896336822879606882224230602M-90"
      CALL FMST2M(ST,C(187))
      ST = " 6.704482593621562564692937104577317810644610039525202376129260569" //  &
           "861749723553513086341477081381278126242511169725032335903896422492" //  &
           "18854955408659222455853625926956312559220163444056344847372949727294762M-91"
      CALL FMST2M(ST,C(188))
      ST = " 2.160351395289544486253226495324951606774126604957407637555483550" //  &
           "521132050784441566021695109519541771023447038718264653048827134039" //  &
           "73414393141529555898480576201941789130545638907969682039980597723815399M-91"
      CALL FMST2M(ST,C(189))
      ST = " 7.200419569439204992397584876133276247439864059745857658513848169" //  &
           "634972230829808411159889094321665351721254003922021586700615812024" //  &
           "99355813646045295572813794504886619128172862586448322114482098387570581M-92"
      CALL FMST2M(ST,C(190))
      ST = " 2.784319712626698759566047586489485590648301900234226820496273235" //  &
           "253278946132569789086775657480520424132403062835830911025372468898" //  &
           "94806632232823225075878972837000029411691959653897178979682691313860829M-92"
      CALL FMST2M(ST,C(191))
      ST = " 9.284263699802481030477812401350944543556932743559221880685236144" //  &
           "608807942910647790463633201299242608070391986248492383364489375260" //  &
           "66506636257311455988821088223165685964257289358894735278070005979762986M-93"
      CALL FMST2M(ST,C(192))
      ST = " 1.469128321729119845643781712942080844517377977136622326900618953" //  &
           "248797751416855621723808444613828590416570423767280724666117572301" //  &
           "34285990191248448522554726873193884488157002183510418398417800853692970M-93"
      CALL FMST2M(ST,C(193))
      ST = " 4.886379978539319342120523930953291961277105452075509621364036950" //  &
           "981474984583217822110836621562995692449070713522036826314376565947" //  &
           "22694606242271109407828035720163781683675686187162678102051578537829111M-94"
      CALL FMST2M(ST,C(194))
      ST = " 7.046670647920504479063475010556879295393094797174920722994400551" //  &
           "715505897095883223398696557122758497726817737906295249088240133320" //  &
           "85226292447629611992116676332110323776745430326383013890000067486498923M-94"
      CALL FMST2M(ST,C(195))
      ST = " 2.351541158501990523752731749555092275030088173405523455954546630" //  &
           "113446321330723773662111826297238234224538212221014494477821368974" //  &
           "29678996883123994564079639022862654561283884722866834645277830050207084M-94"
      CALL FMST2M(ST,C(196))

      NDIG = NDSAVE

      END SUBROUTINE FMPSI_C

      SUBROUTINE FMWRN2

!  Called by one of the FM routines to print a warning message if any error condition arises
!  in that routine.

      USE ModLib_FMVALS
      IMPLICIT NONE

      CHARACTER(9) :: NAME

      INTEGER :: NCS

      IF (KFLAG >= 0 .OR. NCALL /= 1 .OR. KWARN <= 0) RETURN
      NCS = NCALL
      NAME = NAMEST(NCALL)
      WRITE (KW,                                   &
             "(/' Error of type KFLAG =',I3,"  //  &
             "' in FM package in routine ',A/)"    &
            ) KFLAG,TRIM(NAME)

  110 NCALL = NCALL - 1
      IF (NCALL > 0) THEN
          NAME = NAMEST(NCALL)
          WRITE (KW,"( ' called from ',A)") TRIM(NAME)
          GO TO 110
      ENDIF

      IF (KFLAG == -1) THEN
          WRITE (KW,"(' NDIG must be at least 2'/)")
      ELSE IF (KFLAG == -2) THEN
          WRITE (KW,"(' MBASE must be between 2 and',I10/)") INT(MXBASE)
      ELSE IF (KFLAG == -3) THEN
          WRITE (KW,                                                     &
                 "(' An input argument is not a valid FM number.',"  //  &
                 "'  Its exponent is out of range.'/)"                   &
                )
          WRITE (KW,"(' UNKNOWN has been returned.'/)")
      ELSE IF (KFLAG == -4 .OR. KFLAG == -7) THEN
          WRITE (KW,"(' Invalid input argument for this routine.'/)")
          WRITE (KW,"(' UNKNOWN has been returned.'/)")
      ELSE IF (KFLAG == -5) THEN
          WRITE (KW,"(' The result has overflowed.'/)")
      ELSE IF (KFLAG == -6) THEN
          WRITE (KW,"(' The result has underflowed.'/)")
      ELSE IF (KFLAG == -8) THEN
          WRITE (KW,                                                          &
                 "(' The result array is not big enough to hold the',"    //  &
                 "' output character string'/' in the current format.'/"  //  &
                 "' The result ''***...***'' has been returned.'/)"           &
                )
      ELSE IF (KFLAG == -9) THEN
          WRITE (KW,                                                &
                 "(' Precision could not be raised enough to'"  //  &
                 ",' provide all requested guard digits.'/)"        &
                )
          WRITE (KW,                                        &
                 "(I23,' digits were requested (NDIG).'/)"  &
                ) NDIG
          WRITE (KW,"(' UNKNOWN has been returned.'/)")
      ELSE IF (KFLAG == -10) THEN
          IF (NAMEST(NCS) == 'FMM2SP') THEN
              WRITE (KW,                                                     &
                     "(' An FM number was too small in magnitude to ',"  //  &
                     "'convert to single precision.'/)"                      &
                    )
          ELSE
              WRITE (KW,                                                     &
                     "(' An FM number was too small in magnitude to ',"  //  &
                     "'convert to double precision.'/)"                      &
                    )
          ENDIF
          WRITE (KW,"(' Zero has been returned.'/)")
      ELSE IF (KFLAG == -11) THEN
          WRITE (KW,"(' Array MBERN is not large enough.')")
      ELSE IF (KFLAG == -12) THEN
          WRITE (KW,"(' Array MJSUMS is not large enough.')")
      ENDIF

      NCALL = NCS
      IF (KWARN >= 2) THEN
          STOP
      ENDIF
      RETURN
      END SUBROUTINE FMWRN2

!  These are the longer and more readable routine names, equivalent to the older names.

      SUBROUTINE FMATAN2(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      CALL FMATN2(MA,MB,MC)
      RETURN
      END SUBROUTINE FMATAN2

      SUBROUTINE FMCOSH_SINH(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      CALL FMCHSH(MA,MB,MC)
      RETURN
      END SUBROUTINE FMCOSH_SINH

      FUNCTION FMCOMPARE(MA,LREL,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      LOGICAL :: FMCOMPARE
      LOGICAL, EXTERNAL :: FMCOMP
      CHARACTER(*) :: LREL
      TYPE(MULTI) :: MA,MB
      FMCOMPARE = FMCOMP(MA,LREL,MB)
      RETURN
      END FUNCTION FMCOMPARE

      SUBROUTINE FMCOS_SIN(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      CALL FMCSSN(MA,MB,MC)
      RETURN
      END SUBROUTINE FMCOS_SIN

      SUBROUTINE FMEULER(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      CALL FMEULR(MA)
      RETURN
      END SUBROUTINE FMEULER

      SUBROUTINE FMFPRINT(FORM,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      CHARACTER(*) :: FORM
      TYPE(MULTI) :: MA
      CALL FMFPRT(FORM,MA)
      RETURN
      END SUBROUTINE FMFPRINT

      SUBROUTINE FMIPOWER(MA,IVAL,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTEGER :: IVAL
      CALL FMIPWR(MA,IVAL,MB)
      RETURN
      END SUBROUTINE FMIPOWER

      SUBROUTINE FMLOG10(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      CALL FMLG10(MA,MB)
      RETURN
      END SUBROUTINE FMLOG10

      SUBROUTINE FMPRINT(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      CALL FMPRNT(MA)
      RETURN
      END SUBROUTINE FMPRINT

      SUBROUTINE FMPOWER(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      CALL FMPWR(MA,MB,MC)
      RETURN
      END SUBROUTINE FMPOWER

      SUBROUTINE FMRATIONAL_POWER(MA,IVAL,JVAL,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTEGER :: IVAL,JVAL
      CALL FMRPWR(MA,IVAL,JVAL,MB)
      RETURN
      END SUBROUTINE FMRATIONAL_POWER

      SUBROUTINE FMWRITE(KWRITE,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER :: KWRITE
      TYPE(MULTI) :: MA
      CALL FMWRIT(KWRITE,MA)
      RETURN
      END SUBROUTINE FMWRITE

      SUBROUTINE FPATAN2(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      CALL FPATN2(MA,MB,MC)
      RETURN
      END SUBROUTINE FPATAN2

      SUBROUTINE FPCOSH_SINH(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      CALL FPCHSH(MA,MB,MC)
      RETURN
      END SUBROUTINE FPCOSH_SINH

      FUNCTION FPCOMPARE(MA,LREL,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      LOGICAL :: FPCOMPARE
      LOGICAL, EXTERNAL :: FPCOMP
      CHARACTER(*) :: LREL
      TYPE(MULTI) :: MA,MB
      FPCOMPARE = FPCOMP(MA,LREL,MB)
      RETURN
      END FUNCTION FPCOMPARE

      SUBROUTINE FPCOS_SIN(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      CALL FPCSSN(MA,MB,MC)
      RETURN
      END SUBROUTINE FPCOS_SIN

      SUBROUTINE FPEULER(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      CALL FPEULR(MA)
      RETURN
      END SUBROUTINE FPEULER

      SUBROUTINE FPFPRINT(FORM,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      CHARACTER(*) :: FORM
      TYPE(MULTI) :: MA
      CALL FPFPRT(FORM,MA)
      RETURN
      END SUBROUTINE FPFPRINT

      SUBROUTINE FPIPOWER(MA,IVAL,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTEGER :: IVAL
      CALL FPIPWR(MA,IVAL,MB)
      RETURN
      END SUBROUTINE FPIPOWER

      SUBROUTINE FPLOG10(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      CALL FPLG10(MA,MB)
      RETURN
      END SUBROUTINE FPLOG10

      SUBROUTINE FPPRINT(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      CALL FPPRNT(MA)
      RETURN
      END SUBROUTINE FPPRINT

      SUBROUTINE FPPOWER(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      CALL FPPWR(MA,MB,MC)
      RETURN
      END SUBROUTINE FPPOWER

      SUBROUTINE FPRATIONAL_POWER(MA,IVAL,JVAL,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTEGER :: IVAL,JVAL
      CALL FPRPWR(MA,IVAL,JVAL,MB)
      RETURN
      END SUBROUTINE FPRATIONAL_POWER

      SUBROUTINE FPWRITE(KWRITE,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER :: KWRITE
      TYPE(MULTI) :: MA
      CALL FPWRIT(KWRITE,MA)
      RETURN
      END SUBROUTINE FPWRITE

!             Packed versions of routines for special functions.

      SUBROUTINE FPBERNOULLI(INTVAL,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      INTEGER :: INTVAL
      INTENT (IN) :: INTVAL
      INTENT (INOUT) :: MA
      CALL FMBERNOULLI(INTVAL,MPA)
      CALL FMPACK(MPA,MA)
      RETURN
      END SUBROUTINE FPBERNOULLI

      SUBROUTINE FPBERN(INTVAL,MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTEGER :: INTVAL
      INTENT (IN) :: MA,INTVAL
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMBERN(INTVAL,MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPBERN

      SUBROUTINE FPBETA(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FMUNPK(MA,MPA)
      CALL FMUNPK(MB,MPB)
      CALL FMBETA(MPA,MPB,MPC)
      CALL FMPACK(MPC,MC)
      RETURN
      END SUBROUTINE FPBETA

      SUBROUTINE FPCMBI(N,K,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      INTEGER :: K,N
      INTENT (IN) :: N,K
      INTENT (INOUT) :: MA
      CALL FMCMBI(N,K,MPA)
      CALL FMPACK(MPA,MA)
      RETURN
      END SUBROUTINE FPCMBI

      SUBROUTINE FPCOMB(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FMUNPK(MA,MPA)
      CALL FMUNPK(MB,MPB)
      CALL FMCOMB(MPA,MPB,MPC)
      CALL FMPACK(MPC,MC)
      RETURN
      END SUBROUTINE FPCOMB

      SUBROUTINE FPEULR(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      INTENT (INOUT) :: MA
      CALL FMEULR(MPA)
      CALL FMPACK(MPA,MA)
      RETURN
      END SUBROUTINE FPEULR

      SUBROUTINE FPFACT(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMFACT(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPFACT

      SUBROUTINE FPGAM(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMGAM(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPGAM

      SUBROUTINE FPIBTA(MA,MB,MC,MD)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC,MD
      INTENT (IN) :: MA,MB,MC
      INTENT (INOUT) :: MD
      CALL FMUNPK(MA,MPA)
      CALL FMUNPK(MB,MPB)
      CALL FMUNPK(MC,MPC)
      CALL FMIBTA(MPA,MPB,MPC,MPD)
      CALL FMPACK(MPD,MD)
      RETURN
      END SUBROUTINE FPIBTA

      SUBROUTINE FPIGM1(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FMUNPK(MA,MPA)
      CALL FMUNPK(MB,MPB)
      CALL FMIGM1(MPA,MPB,MPC)
      CALL FMPACK(MPC,MC)
      RETURN
      END SUBROUTINE FPIGM1

      SUBROUTINE FPIGM2(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FMUNPK(MA,MPA)
      CALL FMUNPK(MB,MPB)
      CALL FMIGM2(MPA,MPB,MPC)
      CALL FMPACK(MPC,MC)
      RETURN
      END SUBROUTINE FPIGM2

      SUBROUTINE FPLNGM(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMLNGM(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPLNGM

      SUBROUTINE FPPGAM(N,MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTEGER :: N
      INTENT (IN) :: MA,N
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMPGAM(N,MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPPGAM

      SUBROUTINE FPPOCH(MA,N,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTEGER :: N
      INTENT (IN) :: MA,N
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMPOCH(MPA,N,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPPOCH

      SUBROUTINE FPPSI(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMPSI(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPPSI


!     FM exponential integral and related functions

!  Here is a list of the routines that are designed to be called by the user:


!  FMBESJ(N,MA,MB)      MB = J(N,MA)   Bessel function of the first kind.

!  FMBESY(N,MA,MB)      MB = Y(N,MA)   Bessel function of the second kind.

!  FMC(MA,MB)           MB = C(MA)     Fresnel Cosine Integral

!  FMCHI(MA,MB)         MB = Chi(MA)   Hyperbolic Cosine Integral

!  FMCI(MA,MB)          MB = Ci(MA)    Cosine Integral

!  FMEI(MA,MB)          MB = Ei(MA)    Exponential Integral

!  FMEN(N,MA,MB)        MB = E(N,MA)   Exponential Integral E_n

!  FMERF(MA,MB)         MB = Erf(MA)   Error function

!  FMERFC(MA,MB)        MB = Erfc(MA)  Complimentary Error function

!  FMERFCS(MA,MB)       MB = Erfc_Scaled(MA)  Scaled Complimentary Error function.

!  FMLERC(MA,MB)        MB = Ln(Erfc(MA))  Log Erfc

!  FMLI(MA,MB)          MB = Li(MA)    Logarithmic Integral

!  FMS(MA,MB)           MB = S(MA)     Fresnel Sine Integral

!  FMSHI(MA,MB)         MB = Shi(MA)   Hyperbolic Sine Integral

!  FMSI(MA,MB)          MB = Si(MA)    Sine Integral

!  For each of these routines there is also a version available for which the argument list is the
!  same but all FM numbers are in packed format.  The packed versions have the same names except
!  'FM' is replaced by 'FP' at the start of each name.

! --------------------------------------------------------------------------------------------------

      SUBROUTINE FMBESJ(N,MA,MB)

!  MB = Bessel J(N,MA)

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: N
      TYPE(MULTI) :: MA,MB
      DOUBLE PRECISION :: CBIG,CKLOG,CRHS,DBIG,DKLOG,DRHS,ERR,X,XLOG
      DOUBLE PRECISION, EXTERNAL :: FMNTERMS,FMDPLG
      REAL (KIND(1.0D0)) :: MXSAVE
      INTEGER :: IEXTRA,J,J2,K,KD,KL,KOVUN,KR_RETRY,KRESLT,KRSAVE,K_RETURN_CODE,LARGE,  &
                 NDGOAL,NDIG2,NDOLD,NDSAV1,NDSAVE,NGOAL,NMETHD,NTERM,NTERMS,NUMTRY,N_ACC
      INTENT (IN) :: N,MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(17),MRETRY,MJSUMS(LJSUMS)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      K_RETURN_CODE = 0

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1 .AND. N >= 0 .AND. MA%MP(2) < -NDIG) THEN
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          KRSAVE = KROUND
          KROUND = 1
          NDSAVE = NDIG
          NDIG = NDIG + NGRD52
          N_ACC = NINT(NDIG*ALOGM2)
          CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
          CALL FMDIVI(MXY(1),2,MXY(3))
          CALL FMIPWR(MXY(3),N,MXY(2))
          CALL FMI2M(N,MXY(4))
          CALL FMFACT(MXY(4),MXY(5))
          CALL FMDIV(MXY(2),MXY(5),MXY(4))
          CALL FMMPY(MXY(4),MXY(3),MXY(5))
          CALL FMMPY(MXY(5),MXY(3),MXY(6))
          CALL FMDIVI(MXY(6),N+1,MXY(5))
          IF (MXY(4)%MP(2) - MXY(5)%MP(2) > NDIG+1 .AND.  &
              MXY(4)%MP(2) < MEXPOV) THEN
              CALL FMEQU(MXY(4),MXY(10),NDIG,NDSAVE)
              CALL FMEQU(MXY(4),MXY(6),NDIG,NDSAVE)
              CALL FMEQU(MXY(6),MXY(8),NDSAVE,NDIG)
              CALL FMSUB(MXY(4),MXY(8),MXY(6))
              IF (MXY(6)%MP(3) == 0) THEN
                  CALL FMEQU(MXY(4),MXY(6),NDIG,NDSAVE)
                  CALL FMEQU(MXY(5),MXY(7),NDIG,NDSAVE)
                  NDIG = NDSAVE
                  KROUND = KRSAVE
                  CALL FMSUB(MXY(6),MXY(7),MB)
                  IF (MB%MP(2) >= MEXPOV) THEN
                      IF (MXY(6)%MP(1) > 0) THEN
                          IF (MXY(7)%MP(1) > 0 .AND. (KROUND == -1 .OR. KROUND == 0)) THEN
                              CALL FMEQ(MXY(6),MXY(1))
                              MXY(1)%MP(2) = 0
                              CALL FMULP(MXY(1),MXY(2))
                              CALL FMSUB(MXY(1),MXY(2),MXY(3))
                              MXY(3)%MP(2) = MXY(6)%MP(2) + MXY(3)%MP(2)
                              CALL FMEQ(MXY(3),MB)
                          ELSE IF (MXY(7)%MP(1) < 0 .AND. KROUND == 2) THEN
                              CALL FMEQ(MXY(6),MXY(1))
                              MXY(1)%MP(2) = 0
                              CALL FMULP(MXY(1),MXY(2))
                              CALL FMADD(MXY(1),MXY(2),MXY(3))
                              MXY(3)%MP(2) = MXY(6)%MP(2) + MXY(3)%MP(2)
                              CALL FMEQ(MXY(3),MB)
                          ELSE
                              KROUND = KRSAVE
                              CALL FMEQ(MXY(10),MB)
                          ENDIF
                      ELSE
                          IF (MXY(7)%MP(1) > 0 .AND. KROUND == -1) THEN
                              CALL FMEQ(MXY(6),MXY(1))
                              MXY(1)%MP(2) = 0
                              CALL FMULP(MXY(1),MXY(2))
                              CALL FMADD(MXY(1),MXY(2),MXY(3))
                              MXY(3)%MP(2) = MXY(6)%MP(2) + MXY(3)%MP(2)
                              CALL FMEQ(MXY(3),MB)
                          ELSE IF (MXY(7)%MP(1) < 0 .AND. (KROUND == 2 .OR. KROUND == 0)) THEN
                              CALL FMEQ(MXY(6),MXY(1))
                              MXY(1)%MP(2) = 0
                              CALL FMULP(MXY(1),MXY(2))
                              CALL FMSUB(MXY(1),MXY(2),MXY(3))
                              MXY(3)%MP(2) = MXY(6)%MP(2) + MXY(3)%MP(2)
                              CALL FMEQ(MXY(3),MB)
                          ELSE
                              KROUND = KRSAVE
                              CALL FMEQ(MXY(10),MB)
                          ENDIF
                      ENDIF
                  ENDIF
              ELSE
                  KROUND = KRSAVE
                  CALL FMEQU(MXY(4),MB,NDIG,NDSAVE)
              ENDIF
              K_RETURN_CODE = 1
          ENDIF
          KFLAG = 0
          NTRACE = J
          KWARN = K
          NDIG = NDSAVE
          KROUND = KRSAVE
          IF (K_RETURN_CODE == 1) THEN
              IF (NTRACE /= 0) THEN
                  NCALL = NCALL + 1
                  NAMEST(NCALL) = 'FMBESJ'
                  CALL FMNTRI(2,N,1)
                  CALL FMNTR(2,MA,MA,1,0)
                  CALL FMNTR(1,MB,MB,1,1)
                  NCALL = NCALL - 1
              ENDIF
          ENDIF
      ENDIF
      IF (K_RETURN_CODE == 1) RETURN

      IF (NTRACE /= 0) THEN
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'FMBESJ'
          CALL FMNTRI(2,N,1)
          NCALL = NCALL - 1
      ENDIF
      CALL FMENT2('FMBESJ   ',MA,MA,1,0,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF
      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      N_ACC = NINT(NDIG*ALOGM2)
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
      MXY(1)%MP(1) = 1
      NUMTRY = 0

!             Check for special cases.

  120 N_ACC = NINT(NDIG*ALOGM2)
      IF (N < 0) THEN
          CALL FMST2M('UNKNOWN',MXY(12))
          KFLAG = -4
          GO TO 210
      ENDIF
      IF (MA%MP(3) == 0) THEN
          IF (N == 0) THEN
              CALL FMI2M(1,MXY(12))
          ELSE
              CALL FMI2M(0,MXY(12))
          ENDIF
          GO TO 210
      ENDIF
      IF (2*MXY(1)%MP(2) < -NDIG-1) THEN
          CALL FMDIVI(MXY(1),2,MXY(12))
          CALL FMIPWR(MXY(12),N,MXY(15))
          CALL FMFCTI(N,MXY(14))
          CALL FMDIV(MXY(15),MXY(14),MXY(12))
          GO TO 190
      ENDIF

!             Determine which method to use.

!             NMETHD = 1 means use the convergent series
!                    = 2 means use the asymptotic series

      NMETHD = 1
      CALL FMABS(MXY(1),MXY(15))
      CKLOG = MXY(15)%MP(2)
      MXY(15)%MP(2) = 0
      CALL FMM2DP(MXY(15),XLOG)
      XLOG = LOG(XLOG) + CKLOG*DLOGMB

!             c(k) is the absolute value of the kth term of the convergent series.
!             The number of terms needed at this precision is roughly the smallest k for which
!             |c(k)/c(0)| < MBASE**(-NDIG) * 10**(-20).
!             Check Log(|c(k)|) < Log(|c(0)|) - NDIG*Log(MBASE) - 20*Log(10)

      CRHS = N*(XLOG-DLOGTW) - FMDPLG(DBLE(N+1)) - NDIG*DLOGMB - 20*DLOGTN

!             d(k) is the absolute value of the kth term of the asymptotic series.
!             The number of terms needed at this precision is roughly the smallest k for which
!             |d(k)/d(0)| < MBASE**(-NDIG) * 10**(-20).
!             Check Log(|d(k)|) < Log(|d(0)|) - NDIG*Log(MBASE) - 20*Log(10)

      DRHS = N
      DRHS = MIN(0.0D0,LOG(ABS(4*DRHS*DRHS-1))-XLOG-3*DLOGTW) - NDIG*DLOGMB - 20*DLOGTN - 50

      CBIG = N*(XLOG-DLOGTW) - FMDPLG(DBLE(N+1))
      DBIG = 1
      K = 1
      DO J = 0, 50
         K = 2*K
         CKLOG = (2*K+N)*XLOG - (2*K+N)*DLOGTW - FMDPLG(DBLE(K+1)) - FMDPLG(DBLE(N+K+1))
         CBIG = MAX(CBIG,CKLOG)
         IF (CKLOG < CRHS) THEN
             NMETHD = 1
             CKLOG = MIN(0.0D0,N*XLOG - N*DLOGTW - FMDPLG(DBLE(N+1)))
             IEXTRA = ((CBIG-CKLOG)/DLOGMB + 3)*1.2 - 14/ALOGMT
             NDIG = NDIG+MAX(0,IEXTRA)
             N_ACC = NINT(NDIG*ALOGM2)
             CALL FMEQU_R1(MXY(1),NDSAVE,NDIG)
             EXIT
         ENDIF
         KD = K/2
         IF (N-2*KD < 0) THEN
             DKLOG = FMDPLG(DBLE(2*KD+N+0.5)) + FMDPLG(DBLE(2*KD-N+0.5)) -  &
                     2*KD*DLOGTW - 2*KD*XLOG - FMDPLG(DBLE(2*KD+1))
         ELSE
             DKLOG = FMDPLG(DBLE(2*KD+N+0.5)) - FMDPLG(DBLE(N-2*KD+0.5)) -  &
                     2*KD*DLOGTW - 2*KD*XLOG - FMDPLG(DBLE(2*KD+1))
         ENDIF
         DBIG = MAX(DBIG,DKLOG)
         IF (DKLOG < DRHS) THEN
             NMETHD = 2
             IEXTRA = (DBIG/DLOGMB + 3)*1.2 - 14/ALOGMT
             NDIG = NDIG+MAX(0,IEXTRA)
             N_ACC = NINT(NDIG*ALOGM2)
             CALL FMEQU_R1(MXY(1),NDSAVE,NDIG)
             EXIT
         ENDIF
      ENDDO
      IF (KR_RETRY <= 0 .AND. NCALL <= 1 .AND. N <= 100) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG-2)
      ENDIF
      N_ACC = NINT(NDIG*ALOGM2)
      IF (NMETHD == 2) GO TO 160

!             Method 1.  Sum the convergent series.
!                        J(n,x) = Sum( (-1)^k (x/2)^(2k+n) / ( k! (n+k)! )

  130 CALL FMM2DP(MXY(1),X)
      X = ABS(X)
      IF (KFLAG == 0 .AND. X <= SQRT(HUGE(X))) THEN
          J2 = INT(0.63*SQRT(FMNTERMS(X*X/4,1,1,N,1)) - 1)
      ELSE IF (MXY(1)%MP(2) < 0) THEN
          J2 = 2
      ELSE IF (MXY(1)%MP(2) > 0) THEN
          GO TO 160
      ENDIF
      J2 = MAX(2,MIN(J2+MOD(J2,2),LJSUMS))

!             MXY(1) is X
!             MXY(14) is X**2/4
!             MXY(15) is MXY(14)**J2
!             MXY(13) is the current term in the sum
!             MJSUMS holds the partial sums

      NDSAV1 = NDIG
      CALL FMSQR(MXY(1),MXY(14))
      CALL FMDIVI_R1(MXY(14),4)
      CALL FMIPWR(MXY(14),J2,MXY(15))
      CALL FMI2M(1,MXY(10))
      CALL FMFCTI(N,MXY(11))
      CALL FMDIV(MXY(10),MXY(11),MXY(13))
      DO J = 1, J2
         NTERM = J
         CALL FMEQ(MXY(13),MJSUMS(J))
         IF (J > 1) CALL FMCSDIVI_R1(MXY(13),J)
         CALL FMCSDIVI_R1(MXY(13),N+J)
      ENDDO

      NTERM = J2
      NDIG2 = NDIG
  140 CALL FMCSMPY_R1(MXY(13),MXY(15))
      NDIG = NDIG2
      DO J = 1, J2
         NDIG = NDSAV1
         CALL FMCSADDNN_R1(MJSUMS(J),MXY(13))
         IF (KFLAG /= 0) THEN
             GO TO 150
         ELSE
             NDIG = MAX(NGRD22,NDSAV1-INT(MJSUMS(J)%MP(2)-MXY(13)%MP(2)))
             NDIG = MIN(NDSAV1,NDIG)
             NTERM = NTERM + 1
             CALL FMCSDIVI_R1(MXY(13),NTERM)
             CALL FMCSDIVI_R1(MXY(13),N+NTERM)
         ENDIF
      ENDDO
      GO TO 140

!             Put the J2 concurrent sums back together.

  150 NDIG = NDSAV1
      CALL FMCSNSUMS(J2,MJSUMS)
      CALL FMEQ(MJSUMS(1),MXY(11))
      MXY(14)%MP(1) = -1
      IF (MXY(11)%MP(1) > 0) THEN
          CALL FMEQ(MXY(11),MXY(16))
          CALL FMI2M(0,MXY(17))
      ELSE
          CALL FMEQ(MXY(11),MXY(17))
          CALL FMI2M(0,MXY(16))
      ENDIF
      CALL FMEQ(MXY(14),MXY(12))
      DO J = 1, J2-1
         CALL FMMPY(MXY(12),MJSUMS(J+1),MXY(3))
         IF (MXY(3)%MP(1) > 0) THEN
             CALL FMADD_R1(MXY(16),MXY(3))
         ELSE
             CALL FMADD_R1(MXY(17),MXY(3))
         ENDIF
         CALL FMMPY_R1(MXY(12),MXY(14))
      ENDDO
      CALL FMADD(MXY(16),MXY(17),MJSUMS(1))
      CALL FMCANCEL(MXY(16),MXY(17),MJSUMS(1),K)
      N_ACC = N_ACC - K
      CALL FMCSDIVI(MXY(1),2,MXY(11))
      CALL FMIPWR(MXY(11),N,MXY(12))
      CALL FMMPY_R1(MXY(12),MJSUMS(1))
      GO TO 190

!             Method 2.  Sum the asymptotic series.
!                        J(n,x) = Sqrt(2/(Pi*x)) * (Cos(c)*Sum(a(k)) - Sin(c)*Sum(b(k)))
!                        c = x - n*Pi/2 - Pi/4
!                        a(k) = (-1)^k * Gamma(2k+n+0.5) / ( (2x)^(2k) * (2k)! * Gamma(-2k+n+0.5) )
!                        b(k) = (-1)^k * Gamma(2k+n+1.5) /
!                               ( (2x)^(2k+1) * (2k+1)! * Gamma(-2k+n-0.5) )

!             MXY(1) is x
!             MXY(15) is the current term for the series
!             MXY(14) is 1/x^2
!             MXY(13) is MXY(14)^J2
!             MXY(12) is the sum of the a(k) series
!             MXY(11) is the sum of the b(k) series

  160 NDSAV1 = NDIG
      CALL FMM2DP(MXY(1),X)
      X = ABS(X)
      IF (KFLAG == 0 .AND. X <= SQRT(HUGE(X))) THEN
          J2 = NINT(0.41*SQRT(FMNTERMS(2*X,2,1,N,0)) + 0.02)
      ELSE IF (MXY(1)%MP(2) > 0) THEN
          J2 = 1
      ELSE IF (MXY(1)%MP(2) < 0) THEN
          GO TO 130
      ENDIF
      J2 = MAX(1,MIN(LJSUMS,J2))

!             Sum the A(k) series.

      CALL FMI2M(1,MXY(15))
      CALL FMSQR(MXY(1),MXY(14))
      CALL FMDIV_R2(MXY(15),MXY(14))
      CALL FMIPWR(MXY(14),J2,MXY(13))
      LARGE = SQRT(MXBASE+1.0D-3)
      DO J = 1, J2
         NTERM = J - 1
         CALL FMEQ(MXY(15),MJSUMS(J))
         IF (3+4*NTERM+2*N > LARGE) THEN
             CALL FMCSMPYI_R1(MXY(15),1+4*NTERM-2*N)
             CALL FMCSMPYI_R1(MXY(15),3+4*NTERM-2*N)
             CALL FMCSMPYI_R1(MXY(15),1+4*NTERM+2*N)
             CALL FMCSMPYI_R1(MXY(15),3+4*NTERM+2*N)
             CALL FMCSDIVI_R1(MXY(15),-128)
             CALL FMCSDIVI_R1(MXY(15),1+NTERM)
             CALL FMCSDIVI_R1(MXY(15),1+2*NTERM)
         ELSE
             CALL FMCSMPYI_R1(MXY(15),(1+4*NTERM-2*N)*(3+4*NTERM-2*N))
             CALL FMCSMPYI_R1(MXY(15),(1+4*NTERM+2*N)*(3+4*NTERM+2*N))
             CALL FMCSDIVI_R1(MXY(15),-128)
             CALL FMCSDIVI_R1(MXY(15),(1+NTERM)*(1+2*NTERM))
         ENDIF
      ENDDO
      NTERMS = INT(INTMAX/10)

      DO K = 1, NTERMS
         CALL FMCSMPY_R1(MXY(15),MXY(13))
         DO J = 1, J2
            NDIG = NDSAV1
            CALL FMADD_R1(MJSUMS(J),MXY(15))
            IF (KFLAG /= 0) GO TO 170
            NDIG = MAX(NGRD22,NDSAV1-INT(MJSUMS(J)%MP(2)-MXY(15)%MP(2)))
            NDIG = MIN(NDSAV1,NDIG)
            NTERM = NTERM + 1
            IF (3+4*NTERM+2*N > LARGE) THEN
                CALL FMCSMPYI_R1(MXY(15),1+4*NTERM-2*N)
                CALL FMCSMPYI_R1(MXY(15),3+4*NTERM-2*N)
                CALL FMCSMPYI_R1(MXY(15),1+4*NTERM+2*N)
                CALL FMCSMPYI_R1(MXY(15),3+4*NTERM+2*N)
                CALL FMCSDIVI_R1(MXY(15),-128)
                CALL FMCSDIVI_R1(MXY(15),1+NTERM)
                CALL FMCSDIVI_R1(MXY(15),1+2*NTERM)
            ELSE
                CALL FMCSMPYI_R1(MXY(15),(1+4*NTERM-2*N)*(3+4*NTERM-2*N))
                CALL FMCSMPYI_R1(MXY(15),(1+4*NTERM+2*N)*(3+4*NTERM+2*N))
                CALL FMCSDIVI_R1(MXY(15),-128)
                CALL FMCSDIVI_R1(MXY(15),(1+NTERM)*(1+2*NTERM))
            ENDIF
         ENDDO
      ENDDO

!             Put the J2 concurrent sums back together.

  170 NDIG = NDSAV1
      IF (J2 > 1) THEN
          CALL FMEQ(MJSUMS(J2),MXY(11))
          DO J = J2-1, 1, -1
             CALL FMCSMPY_R1(MXY(11),MXY(14))
             CALL FMADD_R1(MXY(11),MJSUMS(J))
          ENDDO
          CALL FMEQ(MXY(11),MXY(12))
      ELSE
          CALL FMEQ(MJSUMS(1),MXY(12))
      ENDIF

!             Sum the B(k) series.

      NDIG = NDSAV1
      IF (N < LARGE) THEN
          CALL FMI2M(4*N*N-1,MXY(15))
      ELSE
          CALL FMI2M(N,MXY(15))
          CALL FMSQR_R1(MXY(15))
          CALL FMMPYI_R1(MXY(15),4)
          CALL FMADDI(MXY(15),-1)
      ENDIF
      CALL FMCSDIVI_R1(MXY(15),8)
      CALL FMDIV_R1(MXY(15),MXY(1))
      DO J = 1, J2
         NTERM = J - 1
         CALL FMEQ(MXY(15),MJSUMS(J))
         IF (3+4*NTERM+2*N > LARGE) THEN
             CALL FMCSMPYI_R1(MXY(15),3+4*NTERM-2*N)
             CALL FMCSMPYI_R1(MXY(15),5+4*NTERM-2*N)
             CALL FMCSMPYI_R1(MXY(15),3+4*NTERM+2*N)
             CALL FMCSMPYI_R1(MXY(15),5+4*NTERM+2*N)
             CALL FMCSDIVI_R1(MXY(15),-128)
             CALL FMCSDIVI_R1(MXY(15),1+NTERM)
             CALL FMCSDIVI_R1(MXY(15),3+2*NTERM)
         ELSE
             CALL FMCSMPYI_R1(MXY(15),(3+4*NTERM-2*N)*(5+4*NTERM-2*N))
             CALL FMCSMPYI_R1(MXY(15),(3+4*NTERM+2*N)*(5+4*NTERM+2*N))
             CALL FMCSDIVI_R1(MXY(15),-128)
             CALL FMCSDIVI_R1(MXY(15),(1+NTERM)*(3+2*NTERM))
         ENDIF
      ENDDO
      NTERMS = INT(INTMAX/10)

      DO K = 1, NTERMS
         CALL FMCSMPY_R1(MXY(15),MXY(13))
         DO J = 1, J2
            NDIG = NDSAV1
            CALL FMADD_R1(MJSUMS(J),MXY(15))
            IF (KFLAG /= 0) GO TO 180
            NDIG = MAX(NGRD22,NDSAV1-INT(MJSUMS(J)%MP(2)-MXY(15)%MP(2)))
            NDIG = MIN(NDSAV1,NDIG)
            NTERM = NTERM + 1
            IF (3+4*NTERM+2*N > LARGE) THEN
                CALL FMCSMPYI_R1(MXY(15),3+4*NTERM-2*N)
                CALL FMCSMPYI_R1(MXY(15),5+4*NTERM-2*N)
                CALL FMCSMPYI_R1(MXY(15),3+4*NTERM+2*N)
                CALL FMCSMPYI_R1(MXY(15),5+4*NTERM+2*N)
                CALL FMCSDIVI_R1(MXY(15),-128)
                CALL FMCSDIVI_R1(MXY(15),1+NTERM)
                CALL FMCSDIVI_R1(MXY(15),3+2*NTERM)
            ELSE
                CALL FMCSMPYI_R1(MXY(15),(3+4*NTERM-2*N)*(5+4*NTERM-2*N))
                CALL FMCSMPYI_R1(MXY(15),(3+4*NTERM+2*N)*(5+4*NTERM+2*N))
                CALL FMCSDIVI_R1(MXY(15),-128)
                CALL FMCSDIVI_R1(MXY(15),(1+NTERM)*(3+2*NTERM))
            ENDIF
         ENDDO
      ENDDO

!             Put the J2 concurrent sums back together.

  180 NDIG = NDSAV1
      IF (J2 > 1) THEN
          CALL FMEQ(MJSUMS(J2),MXY(11))
          DO J = J2-1, 1, -1
             CALL FMCSMPY_R1(MXY(11),MXY(14))
             CALL FMADD_R1(MXY(11),MJSUMS(J))
          ENDDO
      ELSE
          CALL FMEQ(MJSUMS(1),MXY(11))
      ENDIF

!             To minimize cancellation error for very large x, with c = x - n*Pi/2 - Pi/4,
!             then we have
!             cos(c) = (k1*sin(x) + k2*cos(x)) / sqrt(2)
!             sin(c) = (k2*sin(x) - k1*cos(x)) / sqrt(2),  where
!             k1 = cos(n*Pi/2) + sin(n*Pi/2)
!             k2 = cos(n*Pi/2) - sin(n*Pi/2)
!             This is equivalent to
!             Mod( n, 4 ) =   0   1   2   3
!                      k1 =   1   1  -1  -1
!                      k2 =   1  -1  -1   1

      KRSAVE = KRAD
      KRAD = 1
      CALL FMCSSN(MXY(1),MXY(7),MXY(8))
      KRAD = KRSAVE
      K = MOD(N,4)
      IF (K == 0) THEN
          CALL FMADD(MXY(8),MXY(7),MXY(9))
          CALL FMSUB(MXY(8),MXY(7),MXY(10))
      ELSE IF (K == 1) THEN
          CALL FMSUB(MXY(8),MXY(7),MXY(9))
          CALL FMADD(MXY(8),MXY(7),MXY(10))
          CALL FMMPYI_R1(MXY(10),-1)
      ELSE IF (K == 2) THEN
          CALL FMADD(MXY(8),MXY(7),MXY(9))
          CALL FMMPYI_R1(MXY(9),-1)
          CALL FMSUB(MXY(7),MXY(8),MXY(10))
      ELSE
          CALL FMSUB(MXY(7),MXY(8),MXY(9))
          CALL FMADD(MXY(8),MXY(7),MXY(10))
      ENDIF
      CALL FMI2M(2,MXY(5))
      CALL FMSQRT(MXY(5),MXY(6))
      CALL FMDIV(MXY(9),MXY(6),MXY(7))
      CALL FMDIV(MXY(10),MXY(6),MXY(8))

      CALL FMMPY_R2(MXY(7),MXY(12))
      CALL FMMPY_R2(MXY(8),MXY(11))
      CALL FMSUB(MXY(12),MXY(11),MXY(9))
      CALL FMPI(MXY(10))
      CALL FMMPY(MXY(10),MXY(1),MXY(8))
      CALL FMI2M(2,MXY(7))
      CALL FMDIV(MXY(7),MXY(8),MXY(6))
      CALL FMSQRT(MXY(6),MXY(7))
      CALL FMMPY(MXY(7),MXY(9),MXY(12))

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  190 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(12)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF

!             Check for too much cancellation.

      IF (NCALL >= 1) THEN
          NGOAL = INT(REAL(NDSAVE)*ALOGM2) + 17
      ELSE
          NGOAL = INT(-MXEXP2)
      ENDIF
      IF (N_ACC <= NGOAL) THEN
          IF (NUMTRY > 0) THEN
              NDGOAL = INT(REAL(NGOAL)/ALOGM2 + 1.0)
              DO J = 1, NDGOAL+1
                 IF (MRETRY%MP(J+1) /= MXY(12)%MP(J+1)) GO TO 200
              ENDDO
              GO TO 210
          ENDIF
  200     IEXTRA = INT(REAL(NGOAL-N_ACC)/ALOGM2 + 23.03/ALOGMB) + 1
          IF (N >= 1000) IEXTRA = MAX(NDIG/2,IEXTRA)
          NDOLD = NDIG
          NDIG = MAX(NDIG+IEXTRA,NDIG+NGRD52+2)
          CALL FMEQU_R1(MXY(1),NDSAVE,NDIG)
          CALL FMEQ(MXY(1),MXY(15))
          NUMTRY = NUMTRY + 1
          CALL FMEQU(MXY(12),MRETRY,NDOLD,NDIG)
          GO TO 120
      ENDIF

  210 IF (MA%MP(1) == -1) THEN
          IF (MOD(N,2) == 1 .AND. MXY(12)%MP(2) /= MUNKNO .AND.  &
              MXY(12)%MP(3) /= 0) THEN
              MXY(12)%MP(1) = -MXY(12)%MP(1)
          ENDIF
      ENDIF
      CALL FMEXT2(MXY(12),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMBESJ

      SUBROUTINE FMBESJ2(N1,N2,X,ARRAY)

!  ARRAY = (/  J(n1,x) , ..., J(n2,x)  /)

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: J,K,N,N1,N2,NDSAVE
      TYPE(MULTI) :: ARRAY(ABS(N2-N1)+1),X
      INTENT (IN) :: N1,N2,X
      INTENT (INOUT) :: ARRAY
      TYPE(MULTI) :: MXY(4)

      N = ABS(N2-N1) + 1
      DO J = 1, N
         IF (.NOT. ALLOCATED(ARRAY(J)%MP)) THEN
             ALLOCATE(ARRAY(J)%MP(NDIG+2),STAT=K_STAT)
             IF (K_STAT /= 0) CALL FMDEFINE_ERROR
         ELSE IF (SIZE(ARRAY(J)%MP) < NDIG+2) THEN
             DEALLOCATE(ARRAY(J)%MP)
             ALLOCATE(ARRAY(J)%MP(NDIG+2),STAT=K_STAT)
             IF (K_STAT /= 0) CALL FMDEFINE_ERROR
         ENDIF
      ENDDO
      NDSAVE = NDIG

      IF (N1 < 0 .OR. N2 < 0) THEN
          DO J = 1, SIZE(ARRAY)
             CALL FMST2M('UNKNOWN',ARRAY(J))
          ENDDO
          RETURN
      ENDIF

!             The last two entries in the array are done with calls to FMBESJ.
!             The rest use this recurrence:

!             J(k-1,x) = 2*k*J(k,x) / x  -  J(k+1,x)

      NDIG = NDIG + NGRD52
      CALL FMEQU(X,MXY(1),NDSAVE,NDIG)
      K = MAX(N1,N2)
      CALL FMBESJ(K,MXY(1),MXY(4))
      CALL FMEQU(MXY(4),ARRAY(N),NDIG,NDSAVE)
      IF (N <= 1) GO TO 110
      CALL FMBESJ(K-1,MXY(1),MXY(3))
      CALL FMEQU(MXY(3),ARRAY(N-1),NDIG,NDSAVE)
      IF (N <= 2) GO TO 110

      DO J = K-2, MIN(N1,N2), -1
         CALL FMMPYI(MXY(3),2*(J+1),MXY(2))
         CALL FMDIV_R1(MXY(2),MXY(1))
         CALL FMSUB_R1(MXY(2),MXY(4))
         CALL FMEQU(MXY(2),ARRAY(J-MIN(N1,N2)+1),NDIG,NDSAVE)
         IF (J == MIN(N1,N2)) EXIT
         CALL FMEQ(MXY(3),MXY(4))
         CALL FMEQ(MXY(2),MXY(3))
      ENDDO

!             Reverse the list if N2 < N1.

      IF (N2 < N1) THEN
          NDIG = NDSAVE
          DO J = 1, N/2
             CALL FMEQ(ARRAY(J),MXY(4))
             CALL FMEQ(ARRAY(N+1-J),ARRAY(J))
             CALL FMEQ(MXY(4),ARRAY(N+1-J))
          ENDDO
      ENDIF

  110 NDIG = NDSAVE
      RETURN
      END SUBROUTINE FMBESJ2

      SUBROUTINE FMBESY(N,MA,MB)

!  MB = Bessel Y(N,MA)

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: N
      TYPE(MULTI) :: MA,MB
      DOUBLE PRECISION :: CBIG,CKLOG,CRHS,DBIG,DKLOG,DRHS,ERR,X,XLOG
      DOUBLE PRECISION, EXTERNAL :: FMNTERMS,FMDPLG
      REAL (KIND(1.0D0)) :: MXSAVE
      INTEGER :: IEXTRA,J,J2,K,KD,KL,KOVUN,KR_RETRY,KRESLT,KRSAVE,LARGE,NDGOAL,  &
                 NDOLD,NDSAV1,NDSAVE,NGOAL,NMETHD,NTERM,NTERMS,NUMTRY,N_ACC
      INTENT (IN) :: N,MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(17),MRETRY,MJSUMS(LJSUMS)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (NTRACE /= 0) THEN
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'FMBESY'
          CALL FMNTRI(2,N,1)
          NCALL = NCALL - 1
      ENDIF
      CALL FMENT2('FMBESY   ',MA,MA,1,0,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF
      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      N_ACC = NINT(NDIG*ALOGM2)
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
      MXY(1)%MP(1) = 1
      NUMTRY = 0

!             Check for special cases.

  120 IF (N < 0 .OR. MA%MP(3) == 0 .OR. MA%MP(1) == -1) THEN
          CALL FMST2M('UNKNOWN',MXY(9))
          KFLAG = -4
          GO TO 220
      ENDIF
      IF (N < 2 .AND. MA%MP(2) == MEXPUN) THEN
          CALL FMST2M('UNKNOWN',MXY(9))
          KFLAG = -4
          GO TO 220
      ENDIF

!             Determine which method to use.

!             NMETHD = 1 means use the convergent series
!                    = 2 means use the asymptotic series

      NMETHD = 1
      CALL FMABS(MXY(1),MXY(12))
      CKLOG = MXY(12)%MP(2)
      MXY(12)%MP(2) = 0
      CALL FMM2DP(MXY(12),XLOG)
      XLOG = LOG(XLOG) + CKLOG*DLOGMB

!             c(k) is the absolute value of the kth term of the convergent series.  The psi terms
!                  grow logarithmically, so they are ignored for this estimate.
!             The number of terms needed at this precision is roughly the smallest k for which
!             |c(k)/c(0)| < MBASE**(-NDIG) * 10**(-20).
!             Check Log(|c(k)|) < Log(|c(0)|) - NDIG*Log(MBASE) - 20*Log(10)

      CRHS = N*(XLOG-DLOGTW) - FMDPLG(DBLE(N+1)) - NDIG*DLOGMB - 20*DLOGTN

!             d(k) is the absolute value of the kth term of the asymptotic series.
!             The number of terms needed at this precision is roughly the smallest k for which
!             |d(k)/d(0)| < MBASE**(-NDIG) * 10**(-20).
!             Check Log(|d(k)|) < Log(|d(0)|) - NDIG*Log(MBASE) - 20*Log(10)

      DRHS = N
      DRHS = MIN(0.0D0,LOG(ABS(4*DRHS*DRHS-1))-XLOG-3*DLOGTW) - NDIG*DLOGMB - 20*DLOGTN - 50

      CBIG = N*(XLOG-DLOGTW) - FMDPLG(DBLE(N+1))
      DBIG = 1
      K = 1
      DO J = 0, 50
         K = 2*K
         CKLOG = (2*K+N)*XLOG - (2*K+N)*DLOGTW - FMDPLG(DBLE(K+1)) - FMDPLG(DBLE(N+K+1))
         CBIG = MAX(CBIG,CKLOG)
         IF (CKLOG < CRHS) THEN
             NMETHD = 1
             CKLOG = MIN(0.0D0,N*XLOG - N*DLOGTW - FMDPLG(DBLE(N+1)))
             IEXTRA = ((CBIG-CKLOG)/DLOGMB + 3)*1.2 - 14/ALOGMT
             NDIG = NDIG+MAX(0,IEXTRA)
             N_ACC = NINT(NDIG*ALOGM2)
             CALL FMEQU_R1(MXY(1),NDSAVE,NDIG)
             EXIT
         ENDIF
         KD = K
         IF (N-2*KD < 0) THEN
             DKLOG = FMDPLG(DBLE(2*KD+N+0.5)) + FMDPLG(DBLE(2*KD-N+0.5)) -  &
                     2*KD*DLOGTW - 2*KD*XLOG - FMDPLG(DBLE(2*KD+1))
         ELSE
             DKLOG = FMDPLG(DBLE(2*KD+N+0.5)) - FMDPLG(DBLE(N-2*KD+0.5)) -  &
                     2*KD*DLOGTW - 2*KD*XLOG - FMDPLG(DBLE(2*KD+1))
         ENDIF
         DBIG = MAX(DBIG,DKLOG)
         IF (DKLOG < DRHS) THEN
             NMETHD = 2
             IEXTRA = (DBIG/DLOGMB + 3)*1.2 - 14/ALOGMT
             NDIG = NDIG+MAX(0,IEXTRA)
             N_ACC = NINT(NDIG*ALOGM2)
             CALL FMEQU_R1(MXY(1),NDSAVE,NDIG)
             EXIT
         ENDIF
      ENDDO
      IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG-2)
      ENDIF
      N_ACC = NINT(NDIG*ALOGM2)
      IF (NMETHD == 2) GO TO 170

!             Method 1.  Sum the convergent series.
!                        Y(n,x) = (-1/pi)*
!                        Sum( (-1)^k (psi(k+1) + psi(n+k+1)) (x/2)^(2k+n) / ( k! (n+k)! )
!                        - (1/pi)*Sum( (n-k-1)! (x/2)^(2k-n) / k! ) + (2/pi)*ln(x/2)*J(n,x)

!                        The first sum above (involving psi) runs from k=0 to infinity, but the
!                        second sum is finite, for k=0 to k=n-1.

  130 CALL FMM2DP(MXY(1),X)
      N_ACC = NINT(NDIG*ALOGM2)

!             Sum( (-1)^k (psi(k+1) + psi(n+k+1)) (x/2)^(2k+n) / ( k! (n+k)! )

!             MXY(1) is X
!             MXY(11) is -X**2/4
!             MXY(13) is psi(k+1)
!             MXY(14) is psi(n+k+1)
!             MXY(10) is the current value of (-1)^k (x/2)^(2k+n) / ( k! (n+k)! )
!             MXY(12) holds the partial sum

      NDSAV1 = NDIG
      CALL FMEULR(MXY(13))
      MXY(13)%MP(1) = -1
      IF (N < 5000) THEN
          CALL FMEQ(MXY(13),MXY(14))
          IF (N > 0) THEN
              CALL FMI2M(1,MXY(7))
              DO J = 1, N
                 CALL FMCSDIVI(MXY(7),J,MXY(8))
                 CALL FMADD_R1(MXY(14),MXY(8))
              ENDDO
          ENDIF
      ELSE
          CALL FMI2M(N+1,MXY(12))
          CALL FMPSI(MXY(12),MXY(14))
      ENDIF
      CALL FMCSDIVI(MXY(1),2,MXY(8))
      CALL FMIPWR(MXY(8),N,MXY(9))
      CALL FMSQR(MXY(1),MXY(11))
      CALL FMCSDIVI_R1(MXY(11),4)
      MXY(11)%MP(1) = -1
      CALL FMI2M(1,MXY(7))
      CALL FMFCTI(N,MXY(8))
      CALL FMADD(MXY(13),MXY(14),MXY(6))
      CALL FMDIV(MXY(6),MXY(8),MXY(2))
      CALL FMMPY(MXY(2),MXY(9),MXY(3))
      CALL FMEQ(MXY(3),MXY(12))
      IF (MXY(12)%MP(1) > 0) THEN
          CALL FMEQ(MXY(12),MXY(16))
          CALL FMI2M(0,MXY(17))
      ELSE
          CALL FMEQ(MXY(12),MXY(17))
          CALL FMI2M(0,MXY(16))
      ENDIF
      IF (MXY(3)%MP(2) <= MEXPUN) THEN
          CALL FMEQ(MXY(3),MXY(9))
          IF (MXY(9)%MP(2) /= MUNKNO) MXY(9)%MP(1) = -MXY(9)%MP(1)
          IF (MXY(9)%MP(1) > 0) THEN
              CALL FMEQ(MXY(9),MXY(16))
              CALL FMI2M(0,MXY(17))
          ELSE
              CALL FMADD_R1(MXY(9),MXY(17))
              CALL FMI2M(0,MXY(16))
          ENDIF
          GO TO 140
      ENDIF
      IF (MXY(1)%MP(2) < -NDIG) THEN
          CALL FMPI(MXY(6))
          MXY(6)%MP(1) = -1
          CALL FMDIV(MXY(3),MXY(6),MXY(9))
          IF (MXY(9)%MP(1) > 0) THEN
              CALL FMEQ(MXY(9),MXY(16))
              CALL FMI2M(0,MXY(17))
          ELSE
              CALL FMADD_R1(MXY(9),MXY(17))
              CALL FMI2M(0,MXY(16))
          ENDIF
          GO TO 140
      ENDIF
      CALL FMDIV(MXY(9),MXY(8),MXY(10))
      CALL FMI2M(1,MXY(4))

      DO K = 1, 10**8
         CALL FMMPY_R1(MXY(10),MXY(11))
         CALL FMDIVI_R1(MXY(10),K)
         CALL FMDIVI_R1(MXY(10),N+K)
         CALL FMDIVI(MXY(4),K,MXY(3))
         CALL FMADD_R1(MXY(13),MXY(3))
         CALL FMDIVI(MXY(4),N+K,MXY(3))
         CALL FMADD_R1(MXY(14),MXY(3))
         CALL FMADD(MXY(13),MXY(14),MXY(3))
         CALL FMMPY_R1(MXY(3),MXY(10))
         IF (MXY(3)%MP(1) > 0) THEN
             CALL FMADD_R1(MXY(16),MXY(3))
         ELSE
             CALL FMADD_R1(MXY(17),MXY(3))
         ENDIF
         CALL FMADD_R1(MXY(12),MXY(3))
         IF (KFLAG /= 0) EXIT
      ENDDO
      CALL FMPI(MXY(6))
      MXY(6)%MP(1) = -1
      CALL FMDIV_R1(MXY(12),MXY(6))
      CALL FMDIV_R1(MXY(16),MXY(6))
      CALL FMDIV_R1(MXY(17),MXY(6))

!             Sum( (n-k-1)! (x/2)^(2k-n) / k! )

  140 IF (N > 0) THEN
          MXY(11)%MP(1) = 1
          CALL FMCSDIVI(MXY(1),2,MXY(6))
          CALL FMIPWR(MXY(6),-N,MXY(7))
          CALL FMFCTI(N-1,MXY(6))
          CALL FMCSMPY_R1(MXY(7),MXY(6))
          CALL FMEQ(MXY(7),MXY(8))
          IF (MXY(8)%MP(2) == MEXPOV) THEN
              MXY(8)%MP(1) = -1
              GO TO 150
          ENDIF
          DO J = 1, N-1
             CALL FMCSMPY_R1(MXY(7),MXY(11))
             CALL FMCSDIVI_R1(MXY(7),N-J)
             CALL FMCSDIVI_R1(MXY(7),J)
             CALL FMADD_R1(MXY(8),MXY(7))
          ENDDO
          CALL FMPI(MXY(6))
          MXY(6)%MP(1) = -1
          CALL FMDIV_R1(MXY(8),MXY(6))

  150     IF (MXY(8)%MP(1) > 0) THEN
              CALL FMADD_R1(MXY(16),MXY(8))
          ELSE
              CALL FMADD_R1(MXY(17),MXY(8))
          ENDIF
      ENDIF

!             Add the J(n,x) term.

      CALL FMBESJ(N,MXY(1),MXY(15))
      IF (MXY(15)%MP(2) == MEXPUN) THEN
          MXY(15)%MP(1) = -1
          GO TO 160
      ENDIF
      CALL FMCSDIVI(MXY(1),2,MXY(6))
      CALL FMLN(MXY(6),MXY(7))
      CALL FMMPY(MXY(15),MXY(7),MXY(8))
      CALL FMPI(MXY(6))
      CALL FMDIV_R1(MXY(8),MXY(6))
      CALL FMCSMPYI(MXY(8),2,MXY(15))

  160 IF (MXY(15)%MP(1) > 0) THEN
          CALL FMADD_R1(MXY(16),MXY(15))
      ELSE
          CALL FMADD_R1(MXY(17),MXY(15))
      ENDIF
      CALL FMADD(MXY(16),MXY(17),MXY(9))
      CALL FMCANCEL(MXY(16),MXY(17),MXY(9),K)
      N_ACC = N_ACC - K
      GO TO 200

!             Method 2.  Sum the asymptotic series.
!                        Y(n,x) = Sqrt(2/(Pi*x))*(Sin(c)*Sum(a(k)) + Cos(c)*Sum(b(k)))
!                        c = x - n*Pi/2 - Pi/4
!                        a(k) = (-1)^k * Gamma(2k+n+0.5) / ( (2x)^(2k) * (2k)! * Gamma(-2k+n+0.5) )
!                        b(k) = (-1)^k * Gamma(2k+n+1.5) /
!                               ( (2x)^(2k+1) * (2k+1)! * Gamma(-2k+n-0.5) )

!             MXY(1) is x
!             MXY(12) is the current term for the series
!             MXY(11) is 1/x^2
!             MXY(10) is MXY(11)^J2
!             MXY(9) is the sum of the a(k) series
!             MXY(8) is the sum of the b(k) series

  170 NDSAV1 = NDIG
      CALL FMM2DP(MXY(1),X)
      X = ABS(X)
      IF (KFLAG == 0 .AND. X <= SQRT(HUGE(X))) THEN
          J2 = NINT(0.41*SQRT(FMNTERMS(2*X,2,1,N,0)) + 0.02)
      ELSE IF (MXY(1)%MP(2) > 0) THEN
          J2 = 1
      ELSE IF (MXY(1)%MP(2) < 0) THEN
          GO TO 130
      ENDIF
      J2 = MAX(1,MIN(LJSUMS,J2))

!             Sum the A(k) series.

      CALL FMI2M(1,MXY(12))
      CALL FMSQR(MXY(1),MXY(11))
      CALL FMDIV_R2(MXY(12),MXY(11))
      CALL FMIPWR(MXY(11),J2,MXY(10))
      LARGE = SQRT(MXBASE+1.0D-3)
      DO J = 1, J2
         NTERM = J - 1
         CALL FMEQ(MXY(12),MJSUMS(J))
         IF (3+4*NTERM+2*N > LARGE) THEN
             CALL FMCSMPYI_R1(MXY(12),1+4*NTERM-2*N)
             CALL FMCSMPYI_R1(MXY(12),3+4*NTERM-2*N)
             CALL FMCSMPYI_R1(MXY(12),1+4*NTERM+2*N)
             CALL FMCSMPYI_R1(MXY(12),3+4*NTERM+2*N)
             CALL FMCSDIVI_R1(MXY(12),-128)
             CALL FMCSDIVI_R1(MXY(12),1+NTERM)
             CALL FMCSDIVI_R1(MXY(12),1+2*NTERM)
         ELSE
             CALL FMCSMPYI_R1(MXY(12),(1+4*NTERM-2*N)*(3+4*NTERM-2*N))
             CALL FMCSMPYI_R1(MXY(12),(1+4*NTERM+2*N)*(3+4*NTERM+2*N))
             CALL FMCSDIVI_R1(MXY(12),-128)
             CALL FMCSDIVI_R1(MXY(12),(1+NTERM)*(1+2*NTERM))
         ENDIF
      ENDDO
      NTERMS = INT(INTMAX/10)

      DO K = 1, NTERMS
         CALL FMCSMPY_R1(MXY(12),MXY(10))
         DO J = 1, J2
            NDIG = NDSAV1
            CALL FMADD_R1(MJSUMS(J),MXY(12))
            IF (KFLAG /= 0) GO TO 180
            NDIG = MAX(NGRD22,NDSAV1-INT(MJSUMS(J)%MP(2)-MXY(12)%MP(2)))
            NDIG = MIN(NDSAV1,NDIG)
            NTERM = NTERM + 1
            IF (3+4*NTERM+2*N > LARGE) THEN
                CALL FMCSMPYI_R1(MXY(12),1+4*NTERM-2*N)
                CALL FMCSMPYI_R1(MXY(12),3+4*NTERM-2*N)
                CALL FMCSMPYI_R1(MXY(12),1+4*NTERM+2*N)
                CALL FMCSMPYI_R1(MXY(12),3+4*NTERM+2*N)
                CALL FMCSDIVI_R1(MXY(12),-128)
                CALL FMCSDIVI_R1(MXY(12),1+NTERM)
                CALL FMCSDIVI_R1(MXY(12),1+2*NTERM)
            ELSE
                CALL FMCSMPYI_R1(MXY(12),(1+4*NTERM-2*N)*(3+4*NTERM-2*N))
                CALL FMCSMPYI_R1(MXY(12),(1+4*NTERM+2*N)*(3+4*NTERM+2*N))
                CALL FMCSDIVI_R1(MXY(12),-128)
                CALL FMCSDIVI_R1(MXY(12),(1+NTERM)*(1+2*NTERM))
            ENDIF
         ENDDO
      ENDDO

!             Put the J2 concurrent sums back together.

  180 NDIG = NDSAV1
      IF (J2 > 1) THEN
          CALL FMEQ(MJSUMS(J2),MXY(8))
          DO J = J2-1, 1, -1
             CALL FMCSMPY_R1(MXY(8),MXY(11))
             CALL FMADD_R1(MXY(8),MJSUMS(J))
          ENDDO
          CALL FMEQ(MXY(8),MXY(9))
      ELSE
          CALL FMEQ(MJSUMS(1),MXY(9))
      ENDIF

!             Sum the B(k) series.

      NDIG = NDSAV1
      IF (N < LARGE) THEN
          CALL FMI2M(4*N*N-1,MXY(12))
      ELSE
          CALL FMI2M(N,MXY(12))
          CALL FMSQR_R1(MXY(12))
          CALL FMCSMPYI_R1(MXY(12),4)
          CALL FMADDI(MXY(12),-1)
      ENDIF
      CALL FMCSDIVI_R1(MXY(12),8)
      CALL FMDIV_R1(MXY(12),MXY(1))
      DO J = 1, J2
         NTERM = J - 1
         CALL FMEQ(MXY(12),MJSUMS(J))
         IF (3+4*NTERM+2*N > LARGE) THEN
             CALL FMCSMPYI_R1(MXY(12),3+4*NTERM-2*N)
             CALL FMCSMPYI_R1(MXY(12),5+4*NTERM-2*N)
             CALL FMCSMPYI_R1(MXY(12),3+4*NTERM+2*N)
             CALL FMCSMPYI_R1(MXY(12),5+4*NTERM+2*N)
             CALL FMCSDIVI_R1(MXY(12),-128)
             CALL FMCSDIVI_R1(MXY(12),1+NTERM)
             CALL FMCSDIVI_R1(MXY(12),3+2*NTERM)
         ELSE
             CALL FMCSMPYI_R1(MXY(12),(3+4*NTERM-2*N)*(5+4*NTERM-2*N))
             CALL FMCSMPYI_R1(MXY(12),(3+4*NTERM+2*N)*(5+4*NTERM+2*N))
             CALL FMCSDIVI_R1(MXY(12),-128)
             CALL FMCSDIVI_R1(MXY(12),(1+NTERM)*(3+2*NTERM))
         ENDIF
      ENDDO
      NTERMS = INT(INTMAX/10)

      DO K = 1, NTERMS
         CALL FMCSMPY_R1(MXY(12),MXY(10))
         DO J = 1, J2
            NDIG = NDSAV1
            CALL FMADD_R1(MJSUMS(J),MXY(12))
            IF (KFLAG /= 0) GO TO 190
            NDIG = MAX(NGRD22,NDSAV1-INT(MJSUMS(J)%MP(2)-MXY(12)%MP(2)))
            NDIG = MIN(NDSAV1,NDIG)
            NTERM = NTERM + 1
            IF (3+4*NTERM+2*N > LARGE) THEN
                CALL FMCSMPYI_R1(MXY(12),3+4*NTERM-2*N)
                CALL FMCSMPYI_R1(MXY(12),5+4*NTERM-2*N)
                CALL FMCSMPYI_R1(MXY(12),3+4*NTERM+2*N)
                CALL FMCSMPYI_R1(MXY(12),5+4*NTERM+2*N)
                CALL FMCSDIVI_R1(MXY(12),-128)
                CALL FMCSDIVI_R1(MXY(12),1+NTERM)
                CALL FMCSDIVI_R1(MXY(12),3+2*NTERM)
            ELSE
                CALL FMCSMPYI_R1(MXY(12),(3+4*NTERM-2*N)*(5+4*NTERM-2*N))
                CALL FMCSMPYI_R1(MXY(12),(3+4*NTERM+2*N)*(5+4*NTERM+2*N))
                CALL FMCSDIVI_R1(MXY(12),-128)
                CALL FMCSDIVI_R1(MXY(12),(1+NTERM)*(3+2*NTERM))
            ENDIF
         ENDDO
      ENDDO

!             Put the J2 concurrent sums back together.

  190 NDIG = NDSAV1
      IF (J2 > 1) THEN
          CALL FMEQ(MJSUMS(J2),MXY(8))
          DO J = J2-1, 1, -1
             CALL FMCSMPY_R1(MXY(8),MXY(11))
             CALL FMADD_R1(MXY(8),MJSUMS(J))
          ENDDO
      ELSE
          CALL FMEQ(MJSUMS(1),MXY(8))
      ENDIF

!             To minimize cancellation error for very large x, with c = x - n*Pi/2 - Pi/4,
!             then we have
!             cos(c) = (k1*sin(x) + k2*cos(x)) / sqrt(2)
!             sin(c) = (k2*sin(x) - k1*cos(x)) / sqrt(2),  where
!             k1 = cos(n*Pi/2) + sin(n*Pi/2)
!             k2 = cos(n*Pi/2) - sin(n*Pi/2)
!             This is equivalent to
!             Mod( n, 4 ) =   0   1   2   3
!                      k1 =   1   1  -1  -1
!                      k2 =   1  -1  -1   1

      KRSAVE = KRAD
      KRAD = 1
      CALL FMCSSN(MXY(1),MXY(4),MXY(5))
      KRAD = KRSAVE
      K = MOD(N,4)
      IF (K == 0) THEN
          CALL FMADD(MXY(5),MXY(4),MXY(6))
          CALL FMSUB(MXY(5),MXY(4),MXY(7))
      ELSE IF (K == 1) THEN
          CALL FMSUB(MXY(5),MXY(4),MXY(6))
          CALL FMADD(MXY(5),MXY(4),MXY(7))
          CALL FMMPYI_R1(MXY(7),-1)
      ELSE IF (K == 2) THEN
          CALL FMADD(MXY(5),MXY(4),MXY(6))
          CALL FMMPYI_R1(MXY(6),-1)
          CALL FMSUB(MXY(4),MXY(5),MXY(7))
      ELSE
          CALL FMSUB(MXY(4),MXY(5),MXY(6))
          CALL FMADD(MXY(5),MXY(4),MXY(7))
      ENDIF
      CALL FMI2M(2,MXY(2))
      CALL FMSQRT(MXY(2),MXY(3))
      CALL FMDIV(MXY(6),MXY(3),MXY(4))
      CALL FMDIV(MXY(7),MXY(3),MXY(5))

      CALL FMMPY_R2(MXY(5),MXY(9))
      CALL FMMPY_R2(MXY(4),MXY(8))
      CALL FMADD(MXY(9),MXY(8),MXY(6))
      CALL FMPI(MXY(7))
      CALL FMMPY(MXY(7),MXY(1),MXY(5))
      CALL FMI2M(2,MXY(4))
      CALL FMDIV(MXY(4),MXY(5),MXY(3))
      CALL FMSQRT(MXY(3),MXY(4))
      CALL FMMPY(MXY(4),MXY(6),MXY(9))

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  200 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(9)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF

!             Check for too much cancellation.

      IF (NCALL >= 1) THEN
          NGOAL = INT(REAL(NDSAVE)*ALOGM2) + 17
      ELSE
          NGOAL = INT(-MXEXP2)
      ENDIF
      IF (N_ACC <= NGOAL) THEN
          IF (NUMTRY > 0) THEN
              NDGOAL = INT(REAL(NGOAL)/ALOGM2 + 1.0)
              IF (MRETRY%MP(3) == 0) GO TO 210
              DO J = 1, NDGOAL+1
                 IF (MRETRY%MP(J+1) /= MXY(9)%MP(J+1)) GO TO 210
              ENDDO
              GO TO 220
          ENDIF
  210     IEXTRA = INT(REAL(NGOAL-N_ACC)/ALOGM2 + 23.03/ALOGMB) + 1
          NDOLD = NDIG
          NDIG = MAX(NDIG+IEXTRA,NDIG+NGRD52+2)
          N_ACC = NINT(NDIG*ALOGM2)
          CALL FMEQU_R1(MXY(1),NDSAVE,NDIG)
          CALL FMEQ(MXY(1),MXY(12))
          NUMTRY = NUMTRY + 1
          CALL FMEQU(MXY(9),MRETRY,NDOLD,NDIG)
          GO TO 120
      ENDIF

  220 IF (MA%MP(1) == -1) THEN
          IF (MOD(N,2) == 1 .AND. MXY(9)%MP(2) /= MUNKNO .AND.  &
              MXY(9)%MP(3) /= 0) THEN
              MXY(9)%MP(1) = -MXY(9)%MP(1)
          ENDIF
      ENDIF
      CALL FMEXT2(MXY(9),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMBESY

      SUBROUTINE FMBESY2(N1,N2,X,ARRAY)

!  ARRAY = (/  Y(n1,x) , ..., Y(n2,x)  /)


      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: J,K,N,N1,N2,NDSAVE
      TYPE(MULTI) :: ARRAY(ABS(N2-N1)+1),X
      INTENT (IN) :: N1,N2,X
      INTENT (INOUT) :: ARRAY
      TYPE(MULTI) :: MXY(4)

      N = ABS(N2-N1) + 1
      DO J = 1, N
         IF (.NOT. ALLOCATED(ARRAY(J)%MP)) THEN
             ALLOCATE(ARRAY(J)%MP(NDIG+2),STAT=K_STAT)
             IF (K_STAT /= 0) CALL FMDEFINE_ERROR
         ELSE IF (SIZE(ARRAY(J)%MP) < NDIG+2) THEN
             DEALLOCATE(ARRAY(J)%MP)
             ALLOCATE(ARRAY(J)%MP(NDIG+2),STAT=K_STAT)
             IF (K_STAT /= 0) CALL FMDEFINE_ERROR
         ENDIF
      ENDDO
      NDSAVE = NDIG

      IF (N1 < 0 .OR. N2 < 0) THEN
          DO J = 1, SIZE(ARRAY)
             CALL FMST2M('UNKNOWN',ARRAY(J))
          ENDDO
          RETURN
      ENDIF

!             The first two entries in the array are done with calls to FMBESY.
!             The rest use this recurrence:

!             Y(k+1,x) = 2*k*Y(k,x) / x  -  Y(k-1,x)

      NDIG = NDIG + NGRD52
      CALL FMEQU(X,MXY(1),NDSAVE,NDIG)
      K = MIN(N1,N2)
      CALL FMBESY(K,MXY(1),MXY(4))
      CALL FMEQU(MXY(4),ARRAY(1),NDIG,NDSAVE)
      IF (N <= 1) GO TO 110
      CALL FMBESY(K+1,MXY(1),MXY(3))
      CALL FMEQU(MXY(3),ARRAY(2),NDIG,NDSAVE)
      IF (N <= 2) GO TO 110

      DO J = K+2, MAX(N1,N2)
         CALL FMMPYI(MXY(3),2*(J-1),MXY(2))
         CALL FMDIV_R1(MXY(2),MXY(1))
         CALL FMSUB_R1(MXY(2),MXY(4))
         CALL FMEQU(MXY(2),ARRAY(J-K+1),NDIG,NDSAVE)
         IF (J == MIN(N1,N2)) EXIT
         CALL FMEQ(MXY(3),MXY(4))
         CALL FMEQ(MXY(2),MXY(3))
      ENDDO

!             Reverse the list if N2 < N1.

      IF (N2 < N1) THEN
          NDIG = NDSAVE
          DO J = 1, N/2
             CALL FMEQ(ARRAY(J),MXY(4))
             CALL FMEQ(ARRAY(N+1-J),ARRAY(J))
             CALL FMEQ(MXY(4),ARRAY(N+1-J))
          ENDDO
      ENDIF

  110 NDIG = NDSAVE
      RETURN
      END SUBROUTINE FMBESY2

      SUBROUTINE FMC(MA,MB)

!  MB = C(MA)    Fresnel Cosine Integral.

!  Integral from 0 to MA of Cos(pi*t**2/2) dt.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      DOUBLE PRECISION :: ERR,X,Y
      DOUBLE PRECISION, EXTERNAL :: FMNTERMS
      REAL (KIND(1.0D0)) :: MXSAVE
      INTEGER :: IEXTRA,J,J2,K,KFLAGX,KL,KOVUN,KR_RETRY,KRESLT,KRSAVE,K_RETURN_CODE,LARGE,  &
                 NBOT,NDGOAL,NDOLD,NDSAV1,NDSAVE,NGOAL,NMETHD,NTERM,NUMTRY,N_ACC
      LOGICAL, EXTERNAL :: FMCOMP
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(16),MRETRY,MJSUMS(LJSUMS)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      N_ACC = NINT(NDIG*ALOGM2)
      K_RETURN_CODE = 0
      K = 0
      NCALL = NCALL + 1

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1) THEN
          J = NTRACE
          NTRACE = 0
          KL = KWARN
          KWARN = 0
          CALL FMDP2M(1.0D-10,MXY(1))
          CALL FMULP(MXY(1),MXY(2))
          CALL FMSQRT(MXY(2),MXY(3))
          CALL FMSQRT(MXY(3),MXY(2))
          CALL FMABS(MA,MXY(3))
          CALL FMSUB(MXY(3),MXY(2),MXY(4))
          IF (MXY(4)%MP(1) < 0) K = 1
          NTRACE = J
          KWARN = KL
      ENDIF
      IF (KROUND /= 1 .AND. K == 1) THEN
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          KRSAVE = KROUND
          KROUND = 1
          NDSAVE = NDIG
          NDIG = NDIG + NGRD52
          N_ACC = NINT(NDIG*ALOGM2)
          CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
          CALL FMIPWR(MXY(1),5,MXY(2))
          CALL FMPI(MXY(3))
          CALL FMSQR(MXY(3),MXY(4))
          CALL FMDIVI(MXY(4),40,MXY(3))
          CALL FMMPY(MXY(2),MXY(3),MXY(5))
          CALL FMEQ(MXY(1),MXY(4))
          IF (MXY(4)%MP(2) - MXY(5)%MP(2) > NDIG .AND.  &
              MXY(4)%MP(2) > MEXPUN) THEN
              CALL FMEQU(MXY(4),MXY(10),NDIG,NDSAVE)
              CALL FMEQU(MXY(4),MXY(6),NDIG,NDSAVE)
              CALL FMEQU(MXY(6),MXY(8),NDSAVE,NDIG)
              CALL FMSUB(MXY(4),MXY(8),MXY(6))
              IF (MXY(6)%MP(3) == 0) THEN
                  CALL FMEQU(MXY(4),MXY(6),NDIG,NDSAVE)
                  CALL FMEQU(MXY(5),MXY(7),NDIG,NDSAVE)
                  NDIG = NDSAVE
                  KROUND = KRSAVE
                  CALL FMSUB(MXY(6),MXY(7),MB)
                  IF (MB%MP(2) >= MEXPOV) THEN
                      IF (MXY(6)%MP(1) > 0) THEN
                          IF (MXY(7)%MP(1) > 0 .AND. (KROUND == -1 .OR. KROUND == 0)) THEN
                              CALL FMEQ(MXY(6),MXY(1))
                              MXY(1)%MP(2) = 0
                              CALL FMULP(MXY(1),MXY(2))
                              CALL FMSUB(MXY(1),MXY(2),MXY(3))
                              MXY(3)%MP(2) = MXY(6)%MP(2) + MXY(3)%MP(2)
                              CALL FMEQ(MXY(3),MB)
                          ELSE IF (MXY(7)%MP(1) < 0 .AND. KROUND == 2) THEN
                              CALL FMEQ(MXY(6),MXY(1))
                              MXY(1)%MP(2) = 0
                              CALL FMULP(MXY(1),MXY(2))
                              CALL FMADD(MXY(1),MXY(2),MXY(3))
                              MXY(3)%MP(2) = MXY(6)%MP(2) + MXY(3)%MP(2)
                              CALL FMEQ(MXY(3),MB)
                          ELSE
                              KROUND = KRSAVE
                              CALL FMEQ(MXY(10),MB)
                          ENDIF
                      ELSE
                          IF (MXY(7)%MP(1) > 0 .AND. KROUND == -1) THEN
                              CALL FMEQ(MXY(6),MXY(1))
                              MXY(1)%MP(2) = 0
                              CALL FMULP(MXY(1),MXY(2))
                              CALL FMADD(MXY(1),MXY(2),MXY(3))
                              MXY(3)%MP(2) = MXY(6)%MP(2) + MXY(3)%MP(2)
                              CALL FMEQ(MXY(3),MB)
                          ELSE IF (MXY(7)%MP(1) < 0 .AND. (KROUND == 2 .OR. KROUND == 0)) THEN
                              CALL FMEQ(MXY(6),MXY(1))
                              MXY(1)%MP(2) = 0
                              CALL FMULP(MXY(1),MXY(2))
                              CALL FMSUB(MXY(1),MXY(2),MXY(3))
                              MXY(3)%MP(2) = MXY(6)%MP(2) + MXY(3)%MP(2)
                              CALL FMEQ(MXY(3),MB)
                          ELSE
                              KROUND = KRSAVE
                              CALL FMEQ(MXY(10),MB)
                          ENDIF
                      ENDIF
                  ENDIF
              ELSE
                  KROUND = KRSAVE
                  CALL FMEQU(MXY(4),MB,NDIG,NDSAVE)
              ENDIF
              K_RETURN_CODE = 1
          ENDIF
          KFLAG = 0
          NTRACE = J
          KWARN = K
          NDIG = NDSAVE
          KROUND = KRSAVE
          IF (K_RETURN_CODE == 1) THEN
              IF (NTRACE /= 0) THEN
                  NAMEST(NCALL) = 'FMC'
                  CALL FMNTR(2,MA,MA,1,1)
              ENDIF
              IF (MB%MP(2) == MUNKNO .AND. MA%MP(2) /= MUNKNO) THEN
                  NAMEST(NCALL) = 'FMC'
                  KFLAG = -4
                  CALL FMWRN2
              ELSE IF (ABS(MB%MP(2)) == MEXPOV .AND. ABS(MA%MP(2)) < MEXPOV) THEN
                  NAMEST(NCALL) = 'FMC'
                  IF (MB%MP(2) == MEXPOV) KFLAG = -5
                  IF (MB%MP(2) == MEXPUN) KFLAG = -6
                  CALL FMWRN2
              ENDIF
              IF (NTRACE /= 0) THEN
                  NAMEST(NCALL) = 'FMC'
                  CALL FMNTR(1,MB,MB,1,1)
              ENDIF
          ENDIF
      ENDIF
      NCALL = NCALL - 1
      IF (K_RETURN_CODE == 1) RETURN

      CALL FMENT2('FMC      ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF

      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      N_ACC = NINT(NDIG*ALOGM2)
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
      MXY(1)%MP(1) = 1
      NUMTRY = 0

!             Check for special cases.

      IF (MXY(1)%MP(3) == 0) THEN
          CALL FMEQ(MXY(1),MXY(10))
          GO TO 180
      ENDIF
      IF (4*MXY(1)%MP(2) < -NDIG) THEN
          CALL FMEQ(MXY(1),MXY(10))
          GO TO 180
      ENDIF
      CALL FMINT(MXY(1),MXY(8))
      IF (3*MXY(1)%MP(2) > NDIG .AND. FMCOMP(MXY(1),'==',MXY(8)) .AND.  &
          MXY(1)%MP(2) < MEXPOV) THEN
          MXY(1)%MP(1) = MA%MP(1)
          IF (MOD(INT(MBASE),2) == 0 .AND. MXY(1)%MP(2) > NDSAVE) THEN
              K = 0
          ELSE
              CALL FMI2M(2,MXY(8))
              CALL FMMOD(MXY(1),MXY(8),MXY(7))
              CALL FMM2I(MXY(7),K)
          ENDIF
          CALL FMI2M(1,MXY(8))
          IF (MXY(1)%MP(1) < 0) THEN
              CALL FMDIVI(MXY(8),-2,MXY(10))
          ELSE
              CALL FMDIVI(MXY(8),2,MXY(10))
          ENDIF
          IF (K == 0) THEN
              CALL FMIPWR(MXY(1),3,MXY(7))
              CALL FMPI(MXY(6))
              CALL FMSQR_R1(MXY(6))
              CALL FMMPY_R1(MXY(6),MXY(7))
              CALL FMDIV_R2(MXY(8),MXY(6))
              CALL FMSUB_R1(MXY(10),MXY(6))
          ELSE
              CALL FMPI(MXY(6))
              CALL FMMPY_R1(MXY(6),MXY(1))
              CALL FMDIV_R2(MXY(8),MXY(6))
              CALL FMADD_R1(MXY(10),MXY(6))
          ENDIF
          IF (MA%MP(1) < 0) MXY(10)%MP(1) = -MXY(10)%MP(1)
          GO TO 180
      ENDIF
      IF (MXY(1)%MP(2) > NDIG) THEN
          CALL FMI2M(1,MXY(8))
          IF (MXY(1)%MP(1) < 0) THEN
              CALL FMDIVI(MXY(8),-2,MXY(10))
          ELSE
              CALL FMDIVI(MXY(8),2,MXY(10))
          ENDIF
          GO TO 180
      ENDIF

!             X is a double precision approximation to the input argument to this function.

  120 CALL FMM2DP(MXY(1),X)
      KFLAGX = KFLAG
      IF (KFLAG /= 0 .AND. MXY(1)%MP(2) < 0) THEN
          X = 1.0D0/DPMAX
          IF (MXY(1)%MP(1) < 0) X = -X
          KFLAGX = 0
      ENDIF

!             If MA is large in magnitude, use more guard digits.

      IEXTRA = MIN(MAX(INT(MXY(1)%MP(2)),0) , INT(2.0+ALOGMX/ALOGMB))
      IF (IEXTRA > 0) THEN
          CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
      ENDIF
      NDIG = NDIG + IEXTRA
      N_ACC = NINT(NDIG*ALOGM2)

!             Determine which method to use.

!             NMETHD = 1 means use the convergent series,
!                    = 2 means use the asymptotic series.

      IF (KFLAGX == 0) THEN
          IF (ABS(X) < SQRT(HUGE(X)/(4*LOG(HUGE(X))))) THEN
              Y = (DPPI*X*X-1)/2
              Y = (2*Y+1.5)*LOG(2*Y+2) - (2*Y+1) - Y*DLOGTW - (Y+0.5)*LOG(Y+1) + Y - Y*LOG(DPPI*X*X)
              IF (Y <= -(NDIG+1)*DLOGMB) THEN
                  NMETHD = 2
              ELSE
                  NMETHD = 1
              ENDIF
          ELSE
              NMETHD = 2
          ENDIF
      ELSE
          IF (MXY(1)%MP(2) <= 0) THEN
              NMETHD = 1
          ELSE
              NMETHD = 2
          ENDIF
      ENDIF

      IF (NMETHD == 2) GO TO 150

!             Method 1.  Use x times the series (-1)**n*(pi*x*x/2)**(2*n)/((4*n+1)*(2*n)!).

      IEXTRA = 0
      IF (KFLAGX == 0) THEN
          IEXTRA = MAX(0.0D0,(0.096*X*X + 0.033*ABS(X) - 0.5)*LOG(1.0E7)/ALOGMB)*1.02
          IF (IEXTRA > 0) THEN
              CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
          ENDIF
      ENDIF
      NDIG = NDIG + IEXTRA
      N_ACC = NINT(NDIG*ALOGM2)
      IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG-1)
      ENDIF

      IF (KFLAGX == 0) THEN
          J2 = INT(0.68*SQRT(FMNTERMS(3.14159D0*X*X/2,2,0,0,1)) - 1.6)
          J2 = MAX(2,MIN(J2+MOD(J2,2),LJSUMS))
      ELSE
          J2 = 2
      ENDIF
      NDSAV1 = NDIG

!             Split into J2 concurrent sums.

      CALL FMSQR(MXY(1),MXY(4))
      CALL FMCSDIVI(MXY(4),2,MXY(9))
      CALL FMPI(MXY(3))
      CALL FMCSMPY_R1(MXY(9),MXY(3))
      CALL FMI2M(1,MXY(6))
      CALL FMI2M(1,MJSUMS(1))
      NTERM = 0
      DO J = 2, J2
         NTERM = NTERM + 2
         NBOT = NTERM*(NTERM-1)
         LARGE = INT(INTMAX/NTERM)
         IF (NTERM > LARGE .OR. NBOT > MXBASE) THEN
             IF (NTERM > 2) CALL FMCSDIVI_R1(MXY(6),NTERM-1)
             CALL FMCSDIVI_R1(MXY(6),NTERM)
         ELSE
             CALL FMCSDIVI_R1(MXY(6),NBOT)
         ENDIF
         CALL FMCSDIVI(MXY(6),2*NTERM+1,MJSUMS(J))
      ENDDO
      IF (MXY(1)%MP(2) < -NDIG) GO TO 140
      CALL FMIPWR(MXY(9),2*J2,MXY(7))

  130 CALL FMCSMPY_R1(MXY(6),MXY(7))
      DO J = 1, J2
         NTERM = NTERM + 2
         LARGE = INT(INTMAX/NTERM)
         IF (NTERM > LARGE .OR. NTERM > MXBASE/(NTERM-1)) THEN
             CALL FMCSDIVI_R1(MXY(6),NTERM-1)
             CALL FMCSDIVI_R1(MXY(6),NTERM)
         ELSE
             NBOT = NTERM*(NTERM-1)
             CALL FMCSDIVI_R1(MXY(6),NBOT)
         ENDIF
         CALL FMCSDIVI(MXY(6),2*NTERM+1,MXY(2))
         NDIG = NDSAV1
         CALL FMCSADDNN_R1(MJSUMS(J),MXY(2))
         IF (KFLAG /= 0) GO TO 140
         NDIG = NDSAV1 - INT(MJSUMS(J)%MP(2)-MXY(6)%MP(2))
         NDIG = MIN(NDSAV1,NDIG)
         IF (NDIG < NGRD22) NDIG = NGRD22
      ENDDO
      GO TO 130

!             Put the J2 separate sums back together.

  140 KFLAG = 0
      CALL FMCSNSUMS(J2,MJSUMS)
      CALL FMSQR(MXY(9),MXY(8))
      MXY(8)%MP(1) = -1
      IF (MJSUMS(1)%MP(1) > 0) THEN
          CALL FMEQ(MJSUMS(1),MXY(15))
          CALL FMI2M(0,MXY(16))
      ELSE
          CALL FMEQ(MJSUMS(1),MXY(16))
          CALL FMI2M(0,MXY(15))
      ENDIF
      CALL FMEQ(MXY(8),MXY(12))
      DO J = 1, J2-1
         CALL FMMPY(MXY(12),MJSUMS(J+1),MXY(3))
         IF (MXY(3)%MP(1) > 0) THEN
             CALL FMADD_R1(MXY(15),MXY(3))
         ELSE
             CALL FMADD_R1(MXY(16),MXY(3))
         ENDIF
         CALL FMMPY_R1(MXY(12),MXY(8))
      ENDDO
      CALL FMADD(MXY(15),MXY(16),MXY(10))
      CALL FMCANCEL(MXY(15),MXY(16),MXY(10),K)
      N_ACC = N_ACC - K
      CALL FMMPY_R1(MXY(10),MXY(1))

      GO TO 160

!             Method 2.  Use the two N!/X**N asymptotic series for f(x) and g(x).
!                        Then C(x) = 0.5 + f(x)*sin(pi*x*x/2) - g(x)*cos(pi*x*x/2).

  150 IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG-1)
      ENDIF
      N_ACC = NINT(NDIG*ALOGM2)
      CALL FMFGFI(MXY(1),MXY(11),MXY(12))
      CALL FMSQR(MXY(1),MXY(4))
      KRSAVE = KRAD
      KRAD = 0
      CALL FMMPYI(MXY(4),90,MXY(7))
      CALL FMCSSN(MXY(7),MXY(13),MXY(14))
      KRAD = KRSAVE
      CALL FMMPY(MXY(11),MXY(14),MXY(10))
      CALL FMMPY(MXY(12),MXY(13),MXY(7))
      CALL FMI2M(1,MXY(4))
      CALL FMDIVI(MXY(4),2,MXY(5))
      CALL FMADD(MXY(5),MXY(10),MXY(6))
      CALL FMSUB(MXY(6),MXY(7),MXY(10))

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  160 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(10)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF

!             Check for too much cancellation.

      IF (NCALL >= 1) THEN
          NGOAL = INT(REAL(NDSAVE)*ALOGM2) + 17
      ELSE
          NGOAL = INT(-MXEXP2)
      ENDIF
      IF (N_ACC <= NGOAL) THEN
          IF (NUMTRY > 0) THEN
              NDGOAL = INT(REAL(NGOAL)/ALOGM2 + 1.0)
              DO J = 1, NDGOAL+1
                 IF (MRETRY%MP(J+1) /= MXY(10)%MP(J+1)) GO TO 170
              ENDDO
              GO TO 180
          ENDIF
  170     IEXTRA = INT(REAL(NGOAL-N_ACC)/ALOGM2 + 23.03/ALOGMB) + 1
          NDOLD = NDIG
          NDIG = MAX(NDIG+IEXTRA,NDIG+NGRD52+2)
          CALL FMEQU_R1(MXY(1),NDSAVE,NDIG)
          NUMTRY = NUMTRY + 1
          CALL FMEQU(MXY(10),MRETRY,NDOLD,NDIG)
          GO TO 120
      ENDIF

  180 IF (MA%MP(1) == -1) THEN
          IF (MXY(10)%MP(3) /= 0 .AND. MXY(10)%MP(2) /= MUNKNO)  &
              MXY(10)%MP(1) =-1
      ENDIF
      CALL FMEXT2(MXY(10),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMC

      SUBROUTINE FMCHI(MA,MB)

!  MB = Hyperbolic Cosine Integral(MA)

!  EulerGamma + Ln(MA) + Integral from 0 to MA of ( Cosh(t) - 1 ) / t  dt.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      DOUBLE PRECISION :: X,Y
      DOUBLE PRECISION, EXTERNAL :: FMNTERMS
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: IEXTRA,J,J2,K,KFLAGX,KL,KOVUN,KR_RETRY,KRESLT,LARGE,NBOT,NDGOAL,  &
                 NDOLD,NDSAV1,NDSAVE,NGOAL,NMETHD,NTERM,NUMTRY,N_ACC

      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(11),MRETRY,MJSUMS(LJSUMS)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL FMENT2('FMCHI    ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF

      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      N_ACC = NINT(NDIG*ALOGM2)
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
      NUMTRY = 0

!             X is a double precision approximation to the input argument to this function.

  120 CALL FMM2DP(MXY(1),X)
      KFLAGX = KFLAG
      IF (KFLAG /= 0 .AND. MXY(1)%MP(2) < 0) THEN
          X = 1.0D0/DPMAX
          IF (MXY(1)%MP(1) < 0) X = -X
          KFLAGX = 0
      ENDIF

!             If MA is large in magnitude, use more guard digits.

      IEXTRA = MIN(MAX(INT(MXY(1)%MP(2)),0) ,INT(2.0+ALOGMX/ALOGMB))
      IF (IEXTRA > 0) THEN
          CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
      ENDIF
      NDIG = NDIG + IEXTRA
      N_ACC = NINT(NDIG*ALOGM2)

!             Check for special cases.

      IF (MXY(1)%MP(1) == -1 .OR. MXY(1)%MP(2) == MEXPUN .OR.  &
          MXY(1)%MP(3) ==0) THEN
          CALL FMST2M('UNKNOWN',MXY(7))
          KFLAG = -4
          GO TO 180
      ENDIF
      IF (MXY(1)%MP(2) == MEXPOV) THEN
          CALL FMST2M('OVERFLOW',MXY(7))
          KFLAG = -5
          GO TO 180
      ENDIF

!             Determine which method to use.

!             NMETHD = 1 means use the convergent series,
!                    = 2 means use the asymptotic series.

      NMETHD = 1
      IF (KFLAGX /= 0) THEN
          IF (MXY(1)%MP(2) <= 0) THEN
              NMETHD = 1
          ELSE
              NMETHD = 2
          ENDIF
      ELSE
          Y = (NDIG+5)*DLOGMB
          IF (ABS(X) > Y+(DLOGTP+LOG(Y))/2.0D0) NMETHD = 2
      ENDIF

      IF (NMETHD == 2) GO TO 150

!             Method 1.  Use the gamma + ln(X) + X**(2*N)/((2*N)*(2*N)!) series.

      IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG-1)
      ENDIF
      N_ACC = NINT(NDIG*ALOGM2)
      IF (KFLAGX == 0) THEN
          J2 = INT(0.62*SQRT(FMNTERMS(X,2,0,0,1)) - 1.3)
          J2 = MAX(1,MIN(LJSUMS,J2))
      ELSE
          J2 = 2
      ENDIF
      NDSAV1 = NDIG

!             Split into J2 concurrent sums.

      CALL FMSQR(MXY(1),MXY(2))
      CALL FMCSDIVI(MXY(2),2,MXY(6))
      CALL FMCSDIVI(MXY(6),2,MJSUMS(1))
      NTERM = 2
      DO J = 2, J2
         NTERM = NTERM + 1
         CALL FMCSDIVI_R1(MXY(6),NTERM)
         NTERM = NTERM + 1
         NBOT = NTERM
         CALL FMCSDIVI_R1(MXY(6),NBOT)
         CALL FMCSDIVI(MXY(6),NTERM,MJSUMS(J))
      ENDDO
      IF (MXY(1)%MP(2) < -NDIG) GO TO 140
      CALL FMIPWR(MXY(1),2*J2,MXY(4))

  130 CALL FMCSMPY_R1(MXY(6),MXY(4))
      DO J = 1, J2
         NTERM = NTERM + 2
         LARGE = INT(INTMAX/NTERM)
         IF (NTERM > LARGE .OR. NTERM > MXBASE/(NTERM-1)) THEN
             CALL FMCSDIVI_R1(MXY(6),NTERM)
             NBOT = (NTERM - 1)
             CALL FMCSDIVI_R1(MXY(6),NBOT)
         ELSE
             NBOT = NTERM*(NTERM-1)
             CALL FMCSDIVI_R1(MXY(6),NBOT)
         ENDIF
         CALL FMCSDIVI(MXY(6),NTERM,MXY(3))
         NDIG = NDSAV1
         CALL FMCSADDNN_R1(MJSUMS(J),MXY(3))
         IF (KFLAG /= 0) GO TO 140
         NDIG = NDSAV1 - INT(MJSUMS(J)%MP(2)-MXY(3)%MP(2))
         NDIG = MIN(NDSAV1,NDIG)
         IF (NDIG < NGRD22) NDIG = NGRD22
      ENDDO
      GO TO 130

!             Put the J2 separate sums back together.

  140 KFLAG = 0
      CALL FMCSNSUMS(J2,MJSUMS)
      CALL FMEQ(MJSUMS(J2),MXY(7))
      IF (J2 >= 2) THEN
          CALL FMSQR(MXY(1),MXY(5))
          DO J = 2, J2
             CALL FMCSMPY_R1(MXY(7),MXY(5))
             CALL FMADD_R1(MXY(7),MJSUMS(J2-J+1))
          ENDDO
      ENDIF
      CALL FMEULR(MXY(9))
      CALL FMADD(MXY(7),MXY(9),MXY(3))
      CALL FMLN(MXY(1),MXY(2))
      CALL FMADD(MXY(3),MXY(2),MXY(7))
      CALL FMCANCEL(MXY(3),MXY(2),MXY(7),K)
      N_ACC = N_ACC - K

      GO TO 160

!             Method 2.  Use the two N!/X**N asymptotic series for f(x) and g(x).
!                        Then CHI(x) = f(x)*sinh(x) + g(x)*cosh(x).

  150 IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG-2)
      ENDIF
      N_ACC = NINT(NDIG*ALOGM2)
      CALL FMFHGH(MXY(1),MXY(8),MXY(9))
      CALL FMCHSH(MXY(1),MXY(10),MXY(11))
      IF (MXY(10)%MP(2) < MEXPOV) THEN
          CALL FMMPY(MXY(8),MXY(11),MXY(7))
          CALL FMMPY(MXY(9),MXY(10),MXY(4))
          CALL FMADD_R1(MXY(7),MXY(4))
      ELSE
          CALL FMADD(MXY(8),MXY(9),MXY(4))
          CALL FMDIVI_R1(MXY(4),2)
          CALL FMLN(MXY(4),MXY(7))
          CALL FMADD(MXY(1),MXY(7),MXY(4))
          CALL FMEXP(MXY(4),MXY(7))
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  160 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(7)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF

!             Check for too much cancellation.

      IF (NCALL >= 1) THEN
          NGOAL = INT(REAL(NDSAVE)*ALOGM2) + 17
      ELSE
          NGOAL = INT(-MXEXP2)
      ENDIF
      IF (N_ACC <= NGOAL) THEN
          IF (NUMTRY > 0) THEN
              NDGOAL = INT(REAL(NGOAL)/ALOGM2 + 1.0)
              DO J = 1, NDGOAL+1
                 IF (MRETRY%MP(J+1) /= MXY(7)%MP(J+1)) GO TO 170
              ENDDO
              GO TO 180
          ENDIF
  170     IEXTRA = INT(REAL(NGOAL-N_ACC)/ALOGM2 + 23.03/ALOGMB) + 1
          NDOLD = NDIG
          NDIG = MAX(NDIG+IEXTRA,NDIG+NGRD52+2)
          N_ACC = NINT(NDIG*ALOGM2)
          CALL FMEQU_R1(MXY(1),NDSAVE,NDIG)
          NUMTRY = NUMTRY + 1
          CALL FMEQU(MXY(7),MRETRY,NDOLD,NDIG)
          GO TO 120
      ENDIF

  180 CALL FMEXT2(MXY(7),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMCHI

      SUBROUTINE FMCI(MA,MB)

!  MB = Cosine Integral(MA)

!  Integral from MA to Infinity of -Cos(t) / t  dt.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      DOUBLE PRECISION :: ERR,X,Y
      DOUBLE PRECISION, EXTERNAL :: FMNTERMS
      REAL (KIND(1.0D0)) :: MXSAVE
      INTEGER :: IEXTRA,J,J2,K,KFLAGX,KL,KOVUN,KR_RETRY,KRESLT,KRSAVE,LARGE,NBOT,  &
                 NDGOAL,NDOLD,NDSAV1,NDSAVE,NGOAL,NMETHD,NTERM,NUMTRY,N_ACC

      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(13),MRETRY,MJSUMS(LJSUMS)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL FMENT2('FMCI     ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF

      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      N_ACC = NINT(NDIG*ALOGM2)
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
      NUMTRY = 0

!             X is a double precision approximation to the input argument to this function.

  120 CALL FMM2DP(MXY(1),X)
      KFLAGX = KFLAG
      IF (KFLAG /= 0 .AND. MXY(1)%MP(2) < 0) THEN
          X = 1.0D0/DPMAX
          IF (MXY(1)%MP(1) < 0) X = -X
          KFLAGX = 0
      ENDIF

!             If MA is large in magnitude, use more guard digits.

      IEXTRA = MIN(MAX(INT(MXY(1)%MP(2)),0) ,INT(2.0+ALOGMX/ALOGMB))
      IF (IEXTRA > 0) THEN
          CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
      ENDIF
      NDIG = NDIG + IEXTRA
      N_ACC = NINT(NDIG*ALOGM2)

!             Check for special cases.

      IF (MXY(1)%MP(1) == -1 .OR. MXY(1)%MP(3) == 0) THEN
          CALL FMST2M('UNKNOWN',MXY(7))
          KFLAG = -4
          GO TO 180
      ENDIF
      IF (MXY(1)%MP(2) == MEXPOV .OR. MXY(1)%MP(2) == MEXPUN) THEN
          CALL FMST2M('UNKNOWN',MXY(7))
          KFLAG = -4
          GO TO 180
      ENDIF

!             Determine which method to use.

!             NMETHD = 1 means use the convergent series,
!                    = 2 means use the asymptotic series.

      NMETHD = 1
      IF (KFLAGX /= 0) THEN
          IF (MXY(1)%MP(2) <= 0) THEN
              NMETHD = 1
          ELSE
              NMETHD = 2
          ENDIF
      ELSE
          Y = (NDIG+5)*DLOGMB
          IF (ABS(X) > Y+(DLOGTP+LOG(Y))/2.0D0) NMETHD = 2
      ENDIF

      IF (NMETHD == 2) GO TO 150

!             Method 1.  Use the  gamma + ln(X) + (-1)**N*X**(2*N)/((2*N)*(2*N)!) series.

      IEXTRA = 0
      IF (KFLAGX == 0) THEN
          Y = NINT(ABS(X)/2)
          Y = 2*Y*LOG(ABS(X)+1.0E-9) - LOG(2*Y+1.0E-9) - (2*Y+0.5)*LOG(2*Y+1) + 2*Y
          Y = 1.03*Y/DLOGMB
          IEXTRA = MAX(0,INT(Y+1))
      ENDIF
      IF (IEXTRA > 0) THEN
          CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
      ENDIF
      IF (ABS(X) < 5 .AND. KR_RETRY <= 0) THEN
          NDIG = NDIG - 2
          N_ACC = NINT(NDIG*ALOGM2)
          IEXTRA = 0
      ENDIF
      NDIG = NDIG + IEXTRA
      N_ACC = NINT(NDIG*ALOGM2)

      IF (KFLAGX == 0) THEN
          J2 = INT(0.64*SQRT(FMNTERMS(X,2,0,0,1)) - 1.4)
          J2 = MAX(2,MIN(J2+MOD(J2,2),LJSUMS))
      ELSE
          J2 = 2
      ENDIF
      NDSAV1 = NDIG
      CALL FMI2M(0,MXY(12))
      CALL FMI2M(0,MXY(13))

!             Split into J2 concurrent sums.

      CALL FMSQR(MXY(1),MXY(2))
      CALL FMCSDIVI(MXY(2),2,MXY(6))
      CALL FMCSDIVI(MXY(6),2,MJSUMS(1))
      NTERM = 2
      DO J = 2, J2
         NTERM = NTERM + 2
         LARGE = INT(INTMAX/NTERM)
         NBOT = NTERM*(NTERM-1)
         IF (NTERM > LARGE .OR. NBOT > MXBASE) THEN
             CALL FMCSDIVI_R1(MXY(6),NTERM-1)
             CALL FMCSDIVI_R1(MXY(6),NTERM)
         ELSE
             CALL FMCSDIVI_R1(MXY(6),NBOT)
         ENDIF
         CALL FMCSDIVI(MXY(6),NTERM,MJSUMS(J))
      ENDDO
      IF (MXY(1)%MP(2) < -NDIG) GO TO 140
      CALL FMIPWR(MXY(1),2*J2,MXY(4))

  130 CALL FMCSMPY_R1(MXY(6),MXY(4))
      DO J = 1, J2
         NTERM = NTERM + 2
         LARGE = INT(INTMAX/NTERM)
         IF (NTERM > LARGE .OR. NTERM > MXBASE/(NTERM-1)) THEN
             CALL FMCSDIVI_R1(MXY(6),NTERM-1)
             CALL FMCSDIVI_R1(MXY(6),NTERM)
         ELSE
             NBOT = NTERM*(NTERM-1)
             CALL FMCSDIVI_R1(MXY(6),NBOT)
         ENDIF
         CALL FMCSDIVI(MXY(6),NTERM,MXY(3))
         NDIG = NDSAV1
         CALL FMCSADDNN_R1(MJSUMS(J),MXY(3))
         IF (KFLAG /= 0) GO TO 140
         NDIG = NDSAV1 - INT(MJSUMS(J)%MP(2)-MXY(3)%MP(2))
         NDIG = MIN(NDSAV1,NDIG)
         IF (NDIG < NGRD22) NDIG = NGRD22
      ENDDO
      GO TO 130

!             Put the J2 separate sums back together.

  140 KFLAG = 0
      CALL FMCSNSUMS(J2,MJSUMS)
      CALL FMSQR(MXY(1),MXY(5))
      MXY(5)%MP(1) = -1
      IF (MJSUMS(1)%MP(1) > 0) THEN
          CALL FMEQ(MJSUMS(1),MXY(12))
          CALL FMI2M(0,MXY(13))
      ELSE
          CALL FMEQ(MJSUMS(1),MXY(13))
          CALL FMI2M(0,MXY(12))
      ENDIF
      CALL FMEQ(MXY(5),MXY(7))
      DO J = 1, J2-1
         CALL FMMPY(MXY(7),MJSUMS(J+1),MXY(3))
         IF (MXY(3)%MP(1) > 0) THEN
             CALL FMADD_R1(MXY(12),MXY(3))
         ELSE
             CALL FMADD_R1(MXY(13),MXY(3))
         ENDIF
         CALL FMMPY_R1(MXY(7),MXY(5))
      ENDDO
      CALL FMMPYI(MXY(12),-1,MXY(3))
      CALL FMMPYI(MXY(13),-1,MXY(12))
      CALL FMEQ(MXY(3),MXY(13))
      CALL FMEULR(MXY(9))
      CALL FMADD_R1(MXY(12),MXY(9))
      CALL FMLN(MXY(1),MXY(2))
      IF (MXY(2)%MP(1) > 0) THEN
          CALL FMADD_R1(MXY(12),MXY(2))
      ELSE
          CALL FMADD_R1(MXY(13),MXY(2))
      ENDIF
      CALL FMADD(MXY(12),MXY(13),MXY(7))
      CALL FMCANCEL(MXY(12),MXY(13),MXY(7),K)
      N_ACC = N_ACC - K

      GO TO 160

!             Method 2.  Use the two N!/X**N asymptotic series for f(x) and g(x).
!                        Then CI(x) = f(x)*sin(x) - g(x)*cos(x).

  150 CALL FMFXGX(MXY(1),MXY(8),MXY(9))
      KRSAVE = KRAD
      KRAD = 1
      CALL FMCSSN(MXY(1),MXY(10),MXY(11))
      KRAD = KRSAVE
      CALL FMMPY(MXY(8),MXY(11),MXY(7))
      CALL FMMPY(MXY(9),MXY(10),MXY(4))
      CALL FMSUB_R1(MXY(7),MXY(4))

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  160 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(7)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF

!             Check for too much cancellation.

      IF (NCALL >= 1) THEN
          NGOAL = INT(REAL(NDSAVE)*ALOGM2) + 17
      ELSE
          NGOAL = INT(-MXEXP2)
      ENDIF
      IF (N_ACC <= NGOAL) THEN
          IF (NUMTRY > 0) THEN
              NDGOAL = INT(REAL(NGOAL)/ALOGM2 + 1.0)
              DO J = 1, NDGOAL+1
                 IF (MRETRY%MP(J+1) /= MXY(7)%MP(J+1)) GO TO 170
              ENDDO
              GO TO 180
          ENDIF
  170     IEXTRA = INT(REAL(NGOAL-N_ACC)/ALOGM2 + 23.03/ALOGMB) + 1
          NDOLD = NDIG
          NDIG = MAX(NDIG+IEXTRA,NDIG+NGRD52+2)
          N_ACC = NINT(NDIG*ALOGM2)
          CALL FMEQU_R1(MXY(1),NDSAVE,NDIG)
          NUMTRY = NUMTRY + 1
          CALL FMEQU(MXY(7),MRETRY,NDOLD,NDIG)
          GO TO 120
      ENDIF

  180 CALL FMEXT2(MXY(7),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMCI

      SUBROUTINE FMEI(MA,MB)

!  MB = Exponential Integral(MA)

!  Integral from -Infinity to MA of e**t / t  dt.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      DOUBLE PRECISION :: ERR,X,Y
      DOUBLE PRECISION, EXTERNAL :: FMNTERMS
      REAL (KIND(1.0D0)) :: MXSAVE
      INTEGER :: IEXTRA,J,J2,JEXTRA,JTERMS,K,KFLAG1,KFLAGX,KL,KOVUN,KR_RETRY,KRESLT,  &
                 NDGOAL,NDOLD,NDSAV1,NDSAV2,NDSAVE,NGOAL,NMETHD,NTERM,NTERMS,NUMTRY,N_ACC
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(17),MRETRY,MJSUMS(LJSUMS)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL FMENT2('FMEI     ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF

      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      N_ACC = NINT(NDIG*ALOGM2)
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
      NUMTRY = 0

  120 NTERMS = INT(INTMAX/10)

!             X is a double precision approximation to the input argument to this function.

      CALL FMM2DP(MXY(1),X)
      KFLAGX = KFLAG
      IF (KFLAG /= 0 .AND. MXY(1)%MP(2) < 0) THEN
          X = 1.0D0/DPMAX
          IF (MXY(1)%MP(1) < 0) X = -X
          KFLAGX = 0
      ENDIF

!             If MA is large in magnitude, use more guard digits.

      IEXTRA = MIN(MAX(INT(MXY(1)%MP(2)),0),INT(2.0+ALOGMX/ALOGMB))
      IF (IEXTRA > 0) THEN
          CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
      ENDIF
      NDIG = NDIG + IEXTRA
      N_ACC = NINT(NDIG*ALOGM2)

!             Check for special cases.

      IF (MXY(1)%MP(3) == 0 .OR. MXY(1)%MP(2) == MEXPUN) THEN
          CALL FMST2M('UNKNOWN',MXY(10))
          KFLAG = -4
          GO TO 200
      ELSE IF (MXY(1)%MP(2) == MEXPOV) THEN
          CALL FMI2M(0,MXY(10))
          IF (MXY(1)%MP(1) > 0) THEN
              CALL FMST2M('OVERFLOW',MXY(10))
              KFLAG = -5
          ELSE
              CALL FMST2M('-UNDERFLOW',MXY(10))
              KFLAG = -6
          ENDIF
          GO TO 200
      ENDIF

!             Determine which method to use.

!             NMETHD = 1 means use the convergent series,
!                    = 2 means use the asymptotic series,
!                    = 3 means use the continued fraction expansion.

      NMETHD = 1
      IF (KFLAGX /= 0) THEN
          IF (MXY(1)%MP(2) <= 0) THEN
              NMETHD = 1
          ELSE
              NMETHD = 2
          ENDIF
      ELSE
          Y = (NDIG+3)*DLOGMB
          IF (ABS(X) > Y+(DLOGTP+LOG(Y))/2.0D0) NMETHD = 2
          IF (X < 0.0D0 .AND. NMETHD == 1) THEN
              IF (X <= -14.3D0-0.275D0*Y) NMETHD = 3
          ENDIF
      ENDIF

      IF (NMETHD == 2) GO TO 150
      IF (NMETHD == 3) GO TO 170

!             Method 1.  Use the X**N/(N*N!) series.

      IF (KFLAGX == 0) THEN
          IF (X < 0) THEN
              IEXTRA = INT(2.0D0*ABS(X)/DLOGMB)
          ELSE IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
              NDIG = NDIG - IEXTRA - 1
              IEXTRA = 0
          ELSE IF (KR_RETRY <= 0 .AND. NCALL > 1) THEN
              NDIG = NDIG - IEXTRA
              IEXTRA = 0
          ENDIF
      ENDIF
      IF (IEXTRA > 0) THEN
          CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
      ENDIF
      NDIG = NDIG + IEXTRA
      N_ACC = NINT(NDIG*ALOGM2)

      IF (KFLAGX == 0) THEN
          J2 = INT(0.63*SQRT(FMNTERMS(ABS(X),1,0,0,1)) - 1.4)
          J2 = MAX(2,MIN(J2+MOD(J2,2),LJSUMS))
      ELSE
          J2 = 2
      ENDIF
      NDSAV1 = NDIG

!             Split into J2 concurrent sums.

      CALL FMI2M(1,MXY(6))
      CALL FMEQ(MXY(6),MJSUMS(1))
      NTERM = 1
      DO J = 2, J2
         NTERM = NTERM + 1
         CALL FMCSDIVI_R1(MXY(6),NTERM)
         CALL FMCSDIVI(MXY(6),NTERM,MJSUMS(J))
      ENDDO
      IF (MXY(1)%MP(2) < -NDIG) GO TO 140
      CALL FMIPWR(MXY(1),J2,MXY(5))

  130 CALL FMCSMPY_R1(MXY(6),MXY(5))
      DO J = 1, J2
         NTERM = NTERM + 1
         CALL FMCSDIVI_R1(MXY(6),NTERM)
         CALL FMCSDIVI(MXY(6),NTERM,MXY(4))
         NDIG = NDSAV1
         CALL FMCSADDNN_R1(MJSUMS(J),MXY(4))
         IF (KFLAG /= 0) GO TO 140
         NDIG = NDSAV1 - INT(MJSUMS(J)%MP(2)-MXY(4)%MP(2))
         NDIG = MIN(NDSAV1,NDIG)
         IF (NDIG < NGRD22) NDIG = NGRD22
      ENDDO
      GO TO 130

!             Put the J2 separate sums back together.

  140 KFLAG = 0
      CALL FMCSNSUMS(J2,MJSUMS)
      CALL FMI2M(0,MXY(16))
      CALL FMI2M(0,MXY(17))
      CALL FMEQ(MXY(1),MXY(12))
      DO J = 1, J2
         CALL FMMPY(MXY(12),MJSUMS(J),MXY(3))
         IF (MXY(3)%MP(1) > 0) THEN
             CALL FMADD_R1(MXY(16),MXY(3))
         ELSE
             CALL FMADD_R1(MXY(17),MXY(3))
         ENDIF
         CALL FMMPY_R1(MXY(12),MXY(1))
      ENDDO
      CALL FMEULR(MXY(3))
      CALL FMADD_R1(MXY(16),MXY(3))
      CALL FMABS(MXY(1),MXY(2))
      CALL FMLN(MXY(2),MXY(11))
      IF (MXY(11)%MP(1) > 0) THEN
          CALL FMADD_R1(MXY(16),MXY(11))
      ELSE
          CALL FMADD_R1(MXY(17),MXY(11))
      ENDIF
      CALL FMADD(MXY(16),MXY(17),MXY(10))
      CALL FMCANCEL(MXY(16),MXY(17),MJSUMS(1),K)
      N_ACC = N_ACC - K

      GO TO 180

!             Method 2.  Use the N!/X**N series.

  150 IF (KFLAGX == 0) THEN
          J2 = INT(0.38*SQRT(FMNTERMS(ABS(X),1,0,0,0)) + 0.6)
          J2 = MAX(2,MIN(J2+MOD(J2,2),LJSUMS))
      ELSE
          J2 = 2
      ENDIF
      IF (KFLAGX == 0) THEN
          IF (X > 0 .AND. KR_RETRY <= 0 .AND. NCALL <= 1) THEN
              NDIG = MAX(NDSAVE+NGRD52,NDIG-2)
          ENDIF
      ENDIF
      N_ACC = NINT(NDIG*ALOGM2)
      NDSAV1 = NDIG

!             Split into J2 concurrent sums.

      CALL FMI2M(1,MXY(6))
      CALL FMDIV(MXY(6),MXY(1),MXY(15))
      CALL FMEQ(MXY(6),MJSUMS(1))
      NTERM = 1
      DO J = 2, J2
         IF (NTERM >= 2) CALL FMCSMPYI_R1(MXY(6),NTERM)
         CALL FMEQ(MXY(6),MJSUMS(J))
         NTERM = NTERM + 1
      ENDDO
      IF (MXY(15)%MP(2)+MXY(6)%MP(2) < -NDIG-3) GO TO 160
      CALL FMIPWR(MXY(15),J2,MXY(5))

      DO JTERMS = 1, NTERMS
         CALL FMCSMPY_R1(MXY(6),MXY(5))
         DO J = 1, J2
            CALL FMCSMPYI_R1(MXY(6),NTERM)
            NDIG = NDSAV1
            CALL FMCSADDNN_R1(MJSUMS(J),MXY(6))
            IF (KFLAG /= 0) GO TO 160
            NDIG = NDSAV1 - INT(MJSUMS(J)%MP(2)-MXY(6)%MP(2))
            NDIG = MIN(NDSAV1,NDIG)
            IF (NDIG < NGRD22) NDIG = NGRD22
            NTERM = NTERM + 1
         ENDDO
      ENDDO

!             Put the J2 separate sums back together.

  160 KFLAG = 0
      CALL FMCSNSUMS(J2,MJSUMS)
      CALL FMEQ(MJSUMS(J2),MXY(5))
      DO J = 2, J2
         CALL FMCSMPY_R1(MXY(5),MXY(15))
         CALL FMADD_R1(MXY(5),MJSUMS(J2-J+1))
      ENDDO
      CALL FMMPY(MXY(15),MXY(5),MXY(13))
      CALL FMEXP(MXY(1),MXY(11))
      IF (MXY(11)%MP(2) == MEXPOV .AND. MXY(1)%MP(1) > 0) THEN
          CALL FMEQ(MXY(11),MXY(10))
          GO TO 180
      ELSE
          CALL FMMPY(MXY(11),MXY(13),MXY(10))
      ENDIF

      NDIG = NDSAV1
      GO TO 180

!             Method 3.  Use the continued fraction expansion.

!             MXY(14) is the current approximation.
!             MXY(10) is the term in the sum, S(k).
!             MXY(8), MXY(9) are the latest denominators, Q(k-1) and Q(k).

  170 NDSAV1 = NDIG
      JEXTRA = INT(MAX(1.0,5.76/ALOGMB + 1.0))
      IF (NDIG+JEXTRA > NDSAV1) THEN
          CALL FMEQU_R1(MXY(1),NDSAV1,NDSAV1+JEXTRA)
      ENDIF
      NDIG = NDIG + JEXTRA
      N_ACC = NINT(NDIG*ALOGM2)
      CALL FMEQ(MXY(1),MXY(13))
      IF (MXY(13)%MP(2) /= MUNKNO .AND. MXY(13)%MP(3) /= 0)  &
          MXY(13)%MP(1) = -MXY(13)%MP(1)
      CALL FMI2M(1,MXY(8))
      CALL FMEQU(MXY(13),MXY(9),NDSAV1,NDIG)
      CALL FMI2M(1,MXY(2))
      CALL FMDIV(MXY(2),MXY(13),MXY(10))
      CALL FMEQ(MXY(10),MXY(14))

!             Method 3 continued fraction loop.

      NDSAV2 = NDIG
      DO J = 1, NTERMS
         CALL FMCSMPYI(MXY(8),J,MXY(6))
         CALL FMEQ(MXY(9),MXY(7))
         CALL FMCSADD_R1(MXY(7),MXY(6))
         CALL FMCSMPY_R1(MXY(10),MXY(6))
         CALL FMCSDIV(MXY(10),MXY(7),MXY(11))
         CALL FMEQ(MXY(11),MXY(10))
         IF (MXY(10)%MP(2) /= MUNKNO .AND. MXY(10)%MP(3) /= 0)  &
             MXY(10)%MP(1) = -MXY(10)%MP(1)
         NDIG = NDSAV2
         CALL FMCSADD_R1(MXY(14),MXY(10))
         KFLAG1 = KFLAG
         CALL FMEQ(MXY(9),MXY(8))
         CALL FMEQ(MXY(7),MXY(9))
         NDIG = MIN(NDSAV2,MAX(NGRD22,NDSAV2-INT(MXY(14)%MP(2)-MXY(10)%MP(2))))
         CALL FMCSMPYI(MXY(8),J,MXY(6))
         CALL FMEQ(MXY(9),MXY(3))
         CALL FMCSMPY_R1(MXY(3),MXY(13))
         CALL FMEQ(MXY(3),MXY(7))
         CALL FMCSADD_R1(MXY(7),MXY(6))
         CALL FMCSMPY_R1(MXY(10),MXY(6))
         CALL FMCSDIV(MXY(10),MXY(7),MXY(11))
         CALL FMEQ(MXY(11),MXY(10))
         IF (MXY(10)%MP(2) /= MUNKNO .AND. MXY(10)%MP(3) /= 0)  &
             MXY(10)%MP(1) = -MXY(10)%MP(1)
         NDIG = NDSAV2
         CALL FMCSADD_R1(MXY(14),MXY(10))

!             Check for convergence.

         IF (KFLAG1 == 1 .AND. KFLAG == 1) THEN
             EXIT
         ENDIF
         CALL FMEQ(MXY(9),MXY(8))
         CALL FMEQ(MXY(7),MXY(9))
         NDIG = MIN(NDSAV2,MAX(NGRD22,NDSAV2-INT(MXY(14)%MP(2)-MXY(10)%MP(2))))
      ENDDO

      CALL FMEQU_R1(MXY(14),NDIG,NDSAV1)
      NDIG = NDSAV1
      CALL FMEXP(MXY(1),MXY(15))
      CALL FMMPY(MXY(14),MXY(15),MXY(10))
      IF (MXY(10)%MP(2) /= MUNKNO .AND. MXY(10)%MP(3) /= 0)  &
          MXY(10)%MP(1) = -MXY(10)%MP(1)

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  180 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(10)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF

!             Check for too much cancellation.

      IF (NCALL >= 1) THEN
          NGOAL = INT(REAL(NDSAVE)*ALOGM2) + 17
      ELSE
          NGOAL = INT(-MXEXP2)
      ENDIF
      IF (N_ACC <= NGOAL) THEN
          IF (NUMTRY > 0) THEN
              NDGOAL = INT(REAL(NGOAL)/ALOGM2 + 1.0)
              DO J = 1, NDGOAL+1
                 IF (MRETRY%MP(J+1) /= MXY(10)%MP(J+1)) GO TO 190
              ENDDO
              GO TO 200
          ENDIF
  190     IEXTRA = INT(REAL(NGOAL-N_ACC)/ALOGM2 + 23.03/ALOGMB) + 1
          NDOLD = NDIG
          NDIG = MAX(NDIG+IEXTRA,NDIG+NGRD52+2)
          N_ACC = NINT(NDIG*ALOGM2)
          CALL FMEQU_R1(MXY(1),NDSAVE,NDIG)
          NUMTRY = NUMTRY + 1
          CALL FMEQU(MXY(10),MRETRY,NDOLD,NDIG)
          GO TO 120
      ENDIF

  200 CALL FMEXT2(MXY(10),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMEI

      SUBROUTINE FMEN(IVAL,MA,MB)

!  MB = Exponential Integral(IVAL,MA)

!  Integral from 1 to Infinity of e**(-MA*t) / t**IVAL  dt.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTEGER :: IVAL,N
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR,X
      DOUBLE PRECISION, EXTERNAL :: FMNTERMS
      INTEGER :: IEXTRA,J,J2,JS,JTERM,K,KFLAGX,KL,KOVUN,KR_RETRY,KRESLT,KRSAVE,  &
                 K_RETURN_CODE,NDSAVE,NDSAV1,NDSAV2,NGOAL,NMETHD,NTERM,NTERMS,N_ACC
      TYPE(MULTI), SAVE :: M_EULER_HARMONIC
      INTEGER, SAVE :: IVAL_E_H = 0, NDIG_E_H = 0
      REAL (KIND(1.0D0)), SAVE :: MBS_E_H = 0
      LOGICAL, EXTERNAL :: FMCOMP
      INTENT (IN) :: IVAL,MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(14),MJSUMS(LJSUMS)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IEXTRA = 0
      K_RETURN_CODE = 0
      K = 0
      N = IVAL

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1) THEN
          IF (N > 1 .AND. MA%MP(2) < -NDIG) K = 1
          IF (N <= 0) THEN
              J = NTRACE
              NTRACE = 0
              KL = KWARN
              KWARN = 0
              KRSAVE = KROUND
              KROUND = 1
              NDSAVE = NDIG
              NDIG = NDIG + NGRD52
              CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
              CALL FMI2M(-N,MXY(2))
              CALL FMFACT(MXY(2),MXY(3))
              CALL FMIPWR(MXY(1),-N+1,MXY(5))
              CALL FMDIV(MXY(3),MXY(5),MXY(4))
              CALL FMI2M(-1,MXY(6))
              CALL FMDIVI(MXY(6),-N+1,MXY(5))
              IF (MXY(4)%MP(2) - MXY(5)%MP(2) > NDIG) K = 1
              NTRACE = J
              KWARN = KL
              NDIG = NDSAVE
              KROUND = KRSAVE
          ENDIF
          IF (MA%MP(1) < 0 .AND. N > 0) K = 0
      ENDIF
      IF (KROUND /= 1 .AND. K == 1) THEN
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          KRSAVE = KROUND
          KROUND = 1
          NDSAVE = NDIG
          NDIG = NDIG + NGRD52
          CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
          IF (MA%MP(2) == MEXPUN) CALL FMTINY(MXY(1))
          IF (N == 2) THEN
              CALL FMI2M(1,MXY(4))
              CALL FMLN(MXY(1),MXY(2))
              CALL FMEULR(MXY(3))
              CALL FMADD(MXY(2),MXY(3),MXY(5))
              CALL FMMPY(MXY(5),MXY(1),MXY(6))
              CALL FMSUB(MXY(6),MXY(1),MXY(5))
          ELSE IF (N >= 3) THEN
              CALL FMI2M(1,MXY(2))
              CALL FMDIVI(MXY(2),N-1,MXY(4))
              CALL FMDIVI(MXY(1),-(N-2),MXY(5))
          ENDIF
          IF (MXY(4)%MP(2) - MXY(5)%MP(2) > NDIG+1 .AND.  &
              MXY(4)%MP(2) < MUNKNO) THEN
              CALL FMEQU(MXY(4),MXY(10),NDIG,NDSAVE)
              CALL FMEQU(MXY(4),MXY(6),NDIG,NDSAVE)
              CALL FMEQU(MXY(6),MXY(8),NDSAVE,NDIG)
              CALL FMSUB(MXY(4),MXY(8),MXY(6))
              IF (MXY(6)%MP(3) == 0) THEN
                  CALL FMEQU(MXY(4),MXY(6),NDIG,NDSAVE)
                  CALL FMEQU(MXY(5),MXY(7),NDIG,NDSAVE)
                  NDIG = NDSAVE
                  KROUND = KRSAVE
                  CALL FMADD(MXY(6),MXY(7),MB)
                  IF (MB%MP(2) >= MEXPOV) THEN
                      IF (MXY(6)%MP(1) > 0) THEN
                          IF (MXY(7)%MP(1) < 0 .AND. (KROUND == -1 .OR. KROUND == 0)) THEN
                              CALL FMEQ(MXY(6),MXY(1))
                              MXY(1)%MP(2) = 0
                              CALL FMULP(MXY(1),MXY(2))
                              CALL FMSUB(MXY(1),MXY(2),MXY(3))
                              MXY(3)%MP(2) = MXY(6)%MP(2) + MXY(3)%MP(2)
                              CALL FMEQ(MXY(3),MB)
                          ELSE IF (MXY(7)%MP(1) > 0 .AND. KROUND == 2) THEN
                              CALL FMEQ(MXY(6),MXY(1))
                              MXY(1)%MP(2) = 0
                              CALL FMULP(MXY(1),MXY(2))
                              CALL FMADD(MXY(1),MXY(2),MXY(3))
                              MXY(3)%MP(2) = MXY(6)%MP(2) + MXY(3)%MP(2)
                              CALL FMEQ(MXY(3),MB)
                          ELSE
                              KROUND = KRSAVE
                              CALL FMEQ(MXY(10),MB)
                          ENDIF
                      ELSE
                          IF (MXY(7)%MP(1) < 0 .AND. KROUND == -1) THEN
                              CALL FMEQ(MXY(6),MXY(1))
                              MXY(1)%MP(2) = 0
                              CALL FMULP(MXY(1),MXY(2))
                              CALL FMADD(MXY(1),MXY(2),MXY(3))
                              MXY(3)%MP(2) = MXY(6)%MP(2) + MXY(3)%MP(2)
                              CALL FMEQ(MXY(3),MB)
                          ELSE IF (MXY(7)%MP(1) > 0 .AND. (KROUND == 2 .OR. KROUND == 0)) THEN
                              CALL FMEQ(MXY(6),MXY(1))
                              MXY(1)%MP(2) = 0
                              CALL FMULP(MXY(1),MXY(2))
                              CALL FMSUB(MXY(1),MXY(2),MXY(3))
                              MXY(3)%MP(2) = MXY(6)%MP(2) + MXY(3)%MP(2)
                              CALL FMEQ(MXY(3),MB)
                          ELSE
                              KROUND = KRSAVE
                              CALL FMEQ(MXY(10),MB)
                          ENDIF
                      ENDIF
                  ENDIF
              ELSE
                  KROUND = KRSAVE
                  CALL FMEQU(MXY(4),MB,NDIG,NDSAVE)
              ENDIF
              K_RETURN_CODE = 1
          ENDIF
          KFLAG = 0
          NTRACE = J
          KWARN = K
          NDIG = NDSAVE
          KROUND = KRSAVE
          IF (K_RETURN_CODE == 1) THEN
              IF (NTRACE /= 0) THEN
                  NCALL = NCALL + 1
                  NAMEST(NCALL) = 'FMEN'
                  CALL FMNTRI(2,N,1)
                  CALL FMNTR(2,MA,MA,1,0)
                  NCALL = NCALL - 1
              ENDIF
              IF (MB%MP(2) == MUNKNO .AND. MA%MP(2) /= MUNKNO) THEN
                  NCALL = NCALL + 1
                  NAMEST(NCALL) = 'FMEN'
                  KFLAG = -4
                  CALL FMWRN2
                  NCALL = NCALL - 1
              ELSE IF (ABS(MB%MP(2)) == MEXPOV .AND. ABS(MA%MP(2)) < MEXPOV) THEN
                  NCALL = NCALL + 1
                  NAMEST(NCALL) = 'FMEN'
                  IF (MB%MP(2) == MEXPOV) KFLAG = -5
                  IF (MB%MP(2) == MEXPUN) KFLAG = -6
                  CALL FMWRN2
                  NCALL = NCALL - 1
              ENDIF
              IF (NTRACE /= 0) THEN
                  NCALL = NCALL + 1
                  NAMEST(NCALL) = 'FMEN'
                  CALL FMNTR(1,MB,MB,1,1)
                  NCALL = NCALL - 1
              ENDIF
          ENDIF
      ENDIF
      IF (K_RETURN_CODE == 1) RETURN

      IF (NTRACE /= 0) THEN
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'FMEN'
          CALL FMNTRI(2,IVAL,1)
          NCALL = NCALL - 1
      ENDIF
      CALL FMENT2('FMEN     ',MA,MA,1,0,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF

      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      N_ACC = NINT(NDIG*ALOGM2)
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)

!             Special cases.

      IF (IVAL > 0 .AND. (MXY(1)%MP(1) < 0 .OR. MXY(1)%MP(3) == 0)) THEN
          IF (IVAL > 1 .AND. MXY(1)%MP(3) == 0) THEN
              CALL FMI2M(1,MXY(2))
              CALL FMDIVI(MXY(2),IVAL-1,MXY(12))
          ELSE
              CALL FMST2M('UNKNOWN',MXY(12))
              KFLAG = -4
          ENDIF
          GO TO 180
      ENDIF
      IF (IVAL <= 0 .AND. MXY(1)%MP(3) == 0) THEN
          CALL FMST2M('UNKNOWN',MXY(12))
          KFLAG = -4
          GO TO 180
      ENDIF
      IF (IVAL == 0) THEN
          CALL FMI2M(-1000,MXY(3))
          IF (FMCOMP(MXY(1),'>=',MXY(3))) THEN
              CALL FMEQ(MXY(1),MXY(3))
              IF (MXY(3)%MP(2) /= MUNKNO .AND. MXY(3)%MP(3) /= 0)  &
                  MXY(3)%MP(1) = -MXY(3)%MP(1)
              CALL FMEXP(MXY(3),MXY(4))
              CALL FMDIV(MXY(4),MXY(1),MXY(12))
          ELSE
              CALL FMEQ(MXY(1),MXY(3))
              IF (MXY(3)%MP(2) /= MUNKNO .AND. MXY(3)%MP(3) /= 0)  &
                  MXY(3)%MP(1) = -MXY(3)%MP(1)
              CALL FMLN(MXY(3),MXY(4))
              CALL FMSUB(MXY(3),MXY(4),MXY(5))
              CALL FMEXP(MXY(5),MXY(12))
              IF (MXY(12)%MP(2) /= MUNKNO .AND. MXY(12)%MP(3) /= 0)  &
                  MXY(12)%MP(1) = -MXY(12)%MP(1)
          ENDIF
          GO TO 180
      ELSE IF (IVAL == 1 .AND. MXY(1)%MP(2) < -NDIG) THEN
          CALL FMEULR(MXY(11))
          CALL FMLN(MXY(1),MXY(10))
          CALL FMADD(MXY(10),MXY(11),MXY(4))
          CALL FMSUB(MXY(1),MXY(4),MXY(12))
          GO TO 180
      ELSE IF (IVAL == 2 .AND. (MXY(1)%MP(2) < -NDIG .OR. MXY(1)%MP(3) == 0)) THEN
          IF (MXY(1)%MP(2) < -NDIG*2 .OR. MXY(1)%MP(3) == 0) THEN
              CALL FMI2M(1,MXY(12))
          ELSE
              CALL FMEULR(MXY(11))
              CALL FMLN(MXY(1),MXY(10))
              CALL FMADD(MXY(10),MXY(11),MXY(4))
              CALL FMI2M(1,MXY(3))
              CALL FMSUB(MXY(4),MXY(3),MXY(2))
              CALL FMMPY(MXY(2),MXY(1),MXY(5))
              CALL FMADD(MXY(3),MXY(5),MXY(12))
          ENDIF
          GO TO 180
      ELSE IF (IVAL > 2 .AND. (MXY(1)%MP(2) < -NDIG .OR. MXY(1)%MP(3) == 0)) THEN
          CALL FMI2M(1,MXY(3))
          CALL FMDIVI(MXY(3),IVAL-1,MXY(4))
          CALL FMDIVI(MXY(1),IVAL-2,MXY(5))
          CALL FMSUB(MXY(4),MXY(5),MXY(12))
          GO TO 180
      ENDIF

!             Determine which method to use.

!             NMETHD = 1 use the convergent series,
!                    = 2 use the continued fraction expansion,
!                    = 3 for small positive IVAL use a recurrence involving ExponentialEi,
!                    = 4 for small negative IVAL use a recurrence involving exp,
!                    = 5 use incomplete gamma.

      CALL FMM2DP(MXY(1),X)
      KFLAGX = KFLAG
      IF (KFLAG /= 0 .AND. MXY(1)%MP(2) < 0) THEN
          X = 1.0D0/DPMAX
          IF (MXY(1)%MP(1) < 0) X = -X
          KFLAGX = 0
      ENDIF
      IF (KFLAG /= 0 .AND. MXY(1)%MP(2) > 0) THEN
          X = DPMAX
          IF (MXY(1)%MP(1) < 0) X = -X
          KFLAGX = 0
      ENDIF
      NMETHD = 5
      K = 0
      IF (N == 1 .AND. ABS(X) >= 1) K = 10
      IF (IVAL > 0) THEN
          IF (ABS(X) < (NDIG*ALOGMB)/(5.5 + 0.00095*IVAL) + K +                                  &
              (NDIG*ALOGMB)**2 * 1.0E-4 * (4.22 + LOG(REAL(IVAL)))/(24 + LOG(REAL(IVAL))) .AND.  &
              ABS(MXY(1)%MP(2)) < MEXPOV .AND. MXY(1)%MP(3) /= 0) THEN
              NMETHD = 1
          ELSE IF (IVAL < MAX(2,INT((NDIG+3)*ALOGMB/32)) .AND.  &
                   ABS(X) > (NDIG+5)*ALOGMB + LOG(6.2832*(NDIG+5)*ALOGMB)/2) THEN
              NMETHD = 3
          ELSE
              NMETHD = 2
          ENDIF
      ELSE IF (IVAL < 0 .AND. IVAL >= -10 .AND. ABS(X) > 1 .AND.  &
               ABS(MXY(1)%MP(2)) < MEXPOV .AND. MXY(1)%MP(3) /= 0) THEN
          NMETHD = 2
      ELSE IF (IVAL < 0 .AND. IVAL >= -10 .AND. ABS(X) <= 1 .AND.  &
               ABS(MXY(1)%MP(2)) < MEXPOV .AND. MXY(1)%MP(3) /= 0) THEN
          NMETHD = 4
      ELSE IF (IVAL < 0 .AND. IVAL > -500 .AND.  &
               ABS(MXY(1)%MP(2)) < MEXPOV .AND. MXY(1)%MP(3) /= 0) THEN
          NMETHD = 4
      ENDIF
      IF (N == 1 .AND. K == 10 .AND. NMETHD == 1) NMETHD = 3
      IF (NMETHD == 2) GO TO 140
      IF (NMETHD == 3) GO TO 150
      IF (NMETHD == 4) GO TO 160
      IF (NMETHD == 5) GO TO 170

!             Method 1.  Use the (-X)**N/((N-IVAL+1)*N!) series.

      IF (KFLAGX == 0) THEN
          IF (X > 0) THEN
              IEXTRA = INT(2.0D0*ABS(X)/DLOGMB)
          ELSE IF (KR_RETRY <= 0) THEN
              NDIG = NDIG - IEXTRA - 1
              IEXTRA = 0
          ENDIF
      ENDIF
      IF (IEXTRA > 0) THEN
          CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
      ENDIF
      NDIG = NDIG + IEXTRA
      N_ACC = NINT(NDIG*ALOGM2)

      IF (KFLAGX == 0) THEN
          J2 = INT(0.68*SQRT(FMNTERMS(ABS(X),1,0,0,1)) + 0.7)
          J2 = MAX(1,MIN(LJSUMS,J2))
      ELSE
          J2 = 1
      ENDIF
      NDSAV1 = NDIG

!             Split into J2 concurrent sums.

      CALL FMI2M(1,MXY(6))
      JTERM = 0
      NTERM = IVAL - 1
      DO J = 1, J2
         IF (J > 2) CALL FMCSDIVI_R1(MXY(6),JTERM)
         IF (NTERM /= 0) THEN
             CALL FMCSDIVI(MXY(6),NTERM,MJSUMS(J))
         ELSE
             IF (IVAL == IVAL_E_H .AND. NDIG <= NDIG_E_H .AND. MBS_E_H == MBASE) THEN
                 CALL FMEQ(M_EULER_HARMONIC,MXY(2))
                 CALL FMLN(MXY(1),MXY(3))
                 CALL FMSUB_R1(MXY(2),MXY(3))
                 CALL FMMPY(MXY(6),MXY(2),MJSUMS(J))
             ELSE
                 CALL FMEULER(MXY(2))
                 MXY(2)%MP(1) = -MXY(2)%MP(1)
                 CALL FMI2M(1,MXY(3))
                 DO K = 1, IVAL-1
                    CALL FMCSDIVI(MXY(3),K,MXY(4))
                    CALL FMADD_R1(MXY(2),MXY(4))
                 ENDDO
                 IVAL_E_H = IVAL
                 NDIG_E_H = NDIG
                 MBS_E_H = MBASE
                 CALL FMEQ(MXY(2),M_EULER_HARMONIC)
                 CALL FMLN(MXY(1),MXY(3))
                 CALL FMSUB_R1(MXY(2),MXY(3))
                 CALL FMMPY(MXY(6),MXY(2),MJSUMS(J))
             ENDIF
         ENDIF
         JTERM = JTERM + 1
         NTERM = NTERM - 1
      ENDDO
      IF (MXY(1)%MP(2) < -NDIG) GO TO 130
      CALL FMEQ(MXY(1),MXY(2))
      IF (MXY(2)%MP(2) /= MUNKNO .AND. MXY(2)%MP(3) /= 0)  &
          MXY(2)%MP(1) = -MXY(2)%MP(1)
      CALL FMIPWR(MXY(2),J2,MXY(5))

  120 CALL FMCSMPY_R1(MXY(6),MXY(5))
      DO J = 1, J2
         CALL FMCSDIVI_R1(MXY(6),JTERM)
         IF (NTERM /= 0) THEN
             CALL FMCSDIVI(MXY(6),NTERM,MXY(4))
         ELSE
             IF (IVAL == IVAL_E_H .AND. NDIG <= NDIG_E_H .AND. MBS_E_H == MBASE) THEN
                 CALL FMEQ(M_EULER_HARMONIC,MXY(7))
                 CALL FMLN(MXY(1),MXY(3))
                 CALL FMSUB_R1(MXY(7),MXY(3))
                 CALL FMMPY(MXY(6),MXY(7),MXY(4))
             ELSE
                 CALL FMEULER(MXY(7))
                 MXY(7)%MP(1) = -MXY(7)%MP(1)
                 CALL FMI2M(1,MXY(3))
                 DO K = 1, IVAL-1
                    CALL FMCSDIVI(MXY(3),K,MXY(4))
                    CALL FMADD_R1(MXY(7),MXY(4))
                 ENDDO
                 IVAL_E_H = IVAL
                 NDIG_E_H = NDIG
                 MBS_E_H = MBASE
                 CALL FMEQ(MXY(7),M_EULER_HARMONIC)
                 CALL FMLN(MXY(1),MXY(3))
                 CALL FMSUB_R1(MXY(7),MXY(3))
                 CALL FMMPY(MXY(6),MXY(7),MXY(4))
             ENDIF
         ENDIF
         NDIG = NDSAV1
         CALL FMADD_R1(MJSUMS(J),MXY(4))
         IF (KFLAG /= 0) GO TO 130
         NDIG = NDSAV1 - INT(MJSUMS(J)%MP(2)-MXY(4)%MP(2))
         NDIG = MIN(NDSAV1,NDIG)
         IF (NDIG < NGRD22) NDIG = NGRD22
         JTERM = JTERM + 1
         NTERM = NTERM - 1
      ENDDO
      GO TO 120

!             Put the J2 separate sums back together.

  130 KFLAG = 0
      CALL FMEQ(MJSUMS(J2),MXY(5))
      DO J = 2, J2
         CALL FMCSMPY_R1(MXY(5),MXY(2))
         CALL FMADD_R1(MXY(5),MJSUMS(J2-J+1))
      ENDDO
      CALL FMEQ(MXY(5),MXY(12))
      GO TO 180

!             Method 2.  use the continued fraction expansion.

!             MXY(14) is the current approximation.
!             MXY(10) is the term in the sum, S(k).
!             MXY(8), MXY(9) are the latest denominators, Q(k-1) and Q(k).

  140 IF (KFLAGX == 0) THEN
          IF (X > 0 .AND. IVAL > 0 .AND. KR_RETRY <= 0 .AND. NCALL <= 1) THEN
              NDIG = MAX(NDSAVE+NGRD52,NDIG-1)
          ENDIF
      ENDIF
      N_ACC = NINT(NDIG*ALOGM2)
      CALL FMI2M(-IVAL,MXY(2))
      CALL FMSUB(MXY(2),MXY(1),MXY(3))
      CALL FMNINT(MXY(3),MXY(2))
      CALL FMSUB(MXY(3),MXY(2),MXY(4))
      IF (MXY(4)%MP(3) == 0) THEN
          IF (IVAL <= 0) THEN
              IF (ABS(IVAL) <= 100) THEN
                  GO TO 160
              ELSE
                  GO TO 170
              ENDIF
          ENDIF
      ENDIF
      IEXTRA = 0
      IF (MXY(2)%MP(1) >= 0) THEN
          IF (MXY(4)%MP(2) < 0) IEXTRA = -MXY(4)%MP(2)
      ENDIF
      IF (IVAL < 0) THEN
          IF (ABS(X) > 1/SQRT(DPMAX) .AND. ABS(X) < SQRT(DPMAX)) THEN
              J = (LOG(6.2831853D0) + LOG(DBLE(-IVAL)) - IVAL*(LOG(DBLE(-IVAL))-1) -  &
                  (1-IVAL)*LOG(ABS(X))) / DLOGMB + NGRD21
          ELSE
              J = (LOG(6.2831853D0) + LOG(DBLE(-IVAL)) - IVAL*(LOG(DBLE(-IVAL))-1)) / DLOGMB  &
                  - DBLE(1-IVAL)*(MXY(1)%MP(2)-1)
          ENDIF
          IEXTRA = MAX(IEXTRA,J)
      ENDIF
      IF (IEXTRA > 0) THEN
          CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
      ENDIF
      NDIG = NDIG + IEXTRA
      N_ACC = NINT(NDIG*ALOGM2)
      NDSAV1 = NDIG
      CALL FMI2M(0,MXY(8))
      CALL FMI2M(1,MXY(9))
      CALL FMI2M(0,MXY(10))
      CALL FMI2M(0,MXY(14))
      NTERMS = INT(INTMAX/10)

!             Continued fraction loop.

      NDSAV2 = NDIG
      DO J = 1, NTERMS
         IF (MOD(J,2) == 1) THEN
             CALL FMEQ(MXY(1),MXY(11))
             CALL FMCSMPY_R1(MXY(11),MXY(9))
             CALL FMCSMPYI(MXY(8),(J-1)/2,MXY(12))
         ELSE
             CALL FMEQ(MXY(9),MXY(11))
             CALL FMCSMPYI(MXY(8),IVAL+(J-2)/2,MXY(12))
         ENDIF
         IF (J == 1) THEN
             CALL FMEQ(MXY(11),MXY(7))
             CALL FMEXP(MXY(1),MXY(5))
             IF (MXY(5)%MP(2) >= MEXPOV) GO TO 170
             CALL FMMPY(MXY(1),MXY(5),MXY(6))
             CALL FMI2M(1,MXY(2))
             CALL FMDIV(MXY(2),MXY(6),MXY(10))
         ELSE
             CALL FMADD(MXY(11),MXY(12),MXY(7))
             CALL FMCSMPY_R1(MXY(10),MXY(12))
             CALL FMCSDIV(MXY(10),MXY(7),MXY(3))
             CALL FMEQ(MXY(3),MXY(10))
             IF (MXY(10)%MP(2) /= MUNKNO .AND. MXY(10)%MP(3) /= 0)  &
                 MXY(10)%MP(1) = -MXY(10)%MP(1)
         ENDIF
         CALL FMEQ(MXY(9),MXY(8))
         CALL FMEQ(MXY(7),MXY(9))
         NDIG = NDSAV2
         CALL FMCSADD_R1(MXY(14),MXY(10))

!             Check for convergence.

         IF (KFLAG == 1 .AND. J > 1) THEN
             EXIT
         ENDIF
         NDIG = MIN(NDSAV2,MAX(NGRD22,NDSAV2-INT(MXY(14)%MP(2)-MXY(10)%MP(2))))
      ENDDO

      CALL FMEQ(MXY(14),MXY(12))
      NDIG = NDSAV1
      IF (MXY(12)%MP(2) == MUNKNO) GO TO 170
      GO TO 180

!             Method 3.  For small positive IVAL use a recurrence involving ExponentialEi.

  150 NGOAL = INT(REAL(NDSAVE)*ALOGM2) + 27
      IF (MXY(1)%MP(1) < 0) THEN
          CALL FMST2M('UNKNOWN',MXY(12))
          KFLAG = -4
      ELSE IF (IVAL == 1) THEN
          CALL FMEQ(MXY(1),MXY(3))
          IF (MXY(3)%MP(2) /= MUNKNO .AND. MXY(3)%MP(3) /= 0)  &
              MXY(3)%MP(1) = -MXY(3)%MP(1)
          CALL FMEI(MXY(3),MXY(12))
          IF (MXY((12))%MP(2) /= MUNKNO .AND. MXY((12))%MP(3) /= 0)  &
              MXY((12))%MP(1) = -MXY((12))%MP(1)
      ELSE
          IEXTRA = 0
          IF (KFLAGX == 0) THEN
              IEXTRA = INT(IVAL/2.25D0*(LOG(ABS(X))-1.1D0*(LOG(DBLE(IVAL))-1.4D0))*DLOGTN/DLOGMB)
              IEXTRA = MAX(0,IEXTRA)
          ENDIF
          IF (IEXTRA > 0) THEN
              CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
          ENDIF
          NDIG = NDIG + IEXTRA
          N_ACC = NINT(NDIG*ALOGM2)
          CALL FMEQ(MXY(1),MXY(3))
          IF (MXY(3)%MP(2) /= MUNKNO .AND. MXY(3)%MP(3) /= 0)  &
              MXY(3)%MP(1) = -MXY(3)%MP(1)
          CALL FMEI(MXY(3),MXY(4))
          CALL FMIPWR(MXY(3),IVAL-1,MXY(6))
          CALL FMMPY_R1(MXY(4),MXY(6))
          IF (MXY((4))%MP(2) /= MUNKNO .AND. MXY((4))%MP(3) /= 0)  &
              MXY((4))%MP(1) = -MXY((4))%MP(1)
          IF (MXY(4)%MP(1) > 0) THEN
              CALL FMEQ(MXY(4),MXY(13))
              CALL FMI2M(0,MXY(14))
          ELSE
              CALL FMEQ(MXY(4),MXY(14))
              CALL FMI2M(0,MXY(13))
          ENDIF
          IF (MXY(4)%MP(2) == MEXPUN) THEN
              CALL FMEQ(MXY(4),MXY(12))
          ELSE
              CALL FMEXP(MXY(3),MXY(5))
              CALL FMMPY_R1(MXY(6),MXY(5))
              DO J = IVAL-2, 0, -1
                 CALL FMDIV_R1(MXY(6),MXY(3))
                 IF (MXY(6)%MP(1) > 0) THEN
                     CALL FMADD_R1(MXY(13),MXY(6))
                 ELSE
                     CALL FMADD_R1(MXY(14),MXY(6))
                 ENDIF
                 CALL FMMPYI_R1(MXY(6),IVAL-1-J)
              ENDDO
              CALL FMADD(MXY(13),MXY(14),MXY(12))
              CALL FMCANCEL(MXY(13),MXY(14),MXY(12),K)
              N_ACC = N_ACC - K
              CALL FMFCTI(IVAL-1,MXY(7))
              CALL FMDIV_R1(MXY(12),MXY(7))
          ENDIF
          IF (N_ACC <= NGOAL) THEN
              NDIG = 2*NDIG
              N_ACC = NINT(NDIG*ALOGM2)
              GO TO 110
          ENDIF
      ENDIF
      IF (MXY(12)%MP(2) == MUNKNO) GO TO 170
      GO TO 180

!             Method 4.  For small negative IVAL use a recurrence involving exp.

  160 CALL FMEQ(MXY(1),MXY(3))
      IF (MXY(3)%MP(2) /= MUNKNO .AND. MXY(3)%MP(3) /= 0)  &
          MXY(3)%MP(1) = -MXY(3)%MP(1)
      CALL FMI2M(1,MXY(4))
      CALL FMI2M(1,MXY(5))
      DO J = 1, -IVAL
         CALL FMMPY_R1(MXY(5),MXY(1))
         IF (J > 1) CALL FMDIVI_R1(MXY(5),J)
         CALL FMADD_R1(MXY(4),MXY(5))
      ENDDO
      CALL FMMPY_R1(MXY(5),MXY(1))
      CALL FMDIV(MXY(4),MXY(5),MXY(6))
      CALL FMABS(MXY(6),MXY(7))
      CALL FMLN(MXY(7),MXY(8))
      CALL FMSUB(MXY(8),MXY(1),MXY(5))
      CALL FMEXP(MXY(5),MXY(12))
      IF (MXY(12)%MP(2) /= MUNKNO .AND. MXY(6)%MP(1) < 0)  &
          MXY(12)%MP(1) = -MXY(12)%MP(1)
      IF (MXY(12)%MP(2) == MUNKNO) GO TO 170
      GO TO 180

!             Method 5.  use incomplete gamma.

  170 IF (MXY(1)%MP(1) < 0) THEN
          IF (IVAL > 0) THEN
              CALL FMST2M('UNKNOWN',MXY(12))
              KFLAG = -4
          ELSE
              CALL FMI2M(-1,MXY(12))
              IF (IVAL == -1 .AND. FMCOMP(MXY(1),'==',MXY(12))) THEN
                  CALL FMI2M(0,MXY(12))
              ELSE
                  N = 1 - IVAL
                  CALL FMI2M(N,MXY(12))
                  RAISE_NDIG = 1
                  CALL FMIGM2(MXY(12),MXY(1),MXY(13))
                  RAISE_NDIG = 0
                  IF (MXY(13)%MP(2) == MEXPOV) THEN
                      N = IVAL
                      CALL FMI2M(1,MXY(7))
                      CALL FMI2M(1,MXY(8))
                      CALL FMDIV(MXY(8),MXY(1),MXY(9))
                      IF (MXY(9)%MP(2) /= MUNKNO .AND. MXY(9)%MP(3) /= 0)  &
                          MXY(9)%MP(1) = -MXY(9)%MP(1)
                      JS = 0
                      DO J = 0, 10000
                         CALL FMMPYI_R1(MXY(7),N+J)
                         CALL FMMPY_R1(MXY(7),MXY(9))
                         CALL FMADD_R1(MXY(8),MXY(7))
                         IF (KFLAG == 1) THEN
                             JS = 1
                             EXIT
                         ENDIF
                      ENDDO
                      IF (JS == 1) THEN
                          CALL FMLN(MXY(8),MXY(9))
                          CALL FMSUB(MXY(9),MXY(1),MXY(7))
                          CALL FMI2M(0,MXY(6))
                          CALL FMSUB(MXY(6),MXY(1),MXY(2))
                          CALL FMLN(MXY(2),MXY(3))
                          CALL FMSUB(MXY(7),MXY(3),MXY(2))
                          CALL FMEXP(MXY(2),MXY(3))
                          CALL FMSUB(MXY(6),MXY(3),MXY(12))
                      ELSE
                          CALL FMST2M('UNKNOWN',MXY(12))
                          KFLAG = -4
                      ENDIF
                  ELSE
                      CALL FMEQ(MXY(13),MXY(12))
                      N = IVAL - 1
                      CALL FMIPWR(MXY(1),N,MXY(3))
                      CALL FMMPY_R2(MXY(3),MXY(12))
                  ENDIF
              ENDIF
          ENDIF
      ELSE
          N = 1 - IVAL
          CALL FMI2M(N,MXY(12))
          RAISE_NDIG = 1
          CALL FMIGM2(MXY(12),MXY(1),MXY(13))
          RAISE_NDIG = 0
          IF (MXY(13)%MP(2) == MEXPUN .AND. IVAL > 0) THEN
              CALL FMEQ(MXY(1),MXY(4))
              IF (MXY(4)%MP(2) /= MUNKNO .AND. MXY(4)%MP(3) /= 0)  &
                  MXY(4)%MP(1) = -MXY(4)%MP(1)
              CALL FMEXP(MXY(4),MXY(5))
              CALL FMDIV(MXY(5),MXY(1),MXY(6))
              IF (MXY(6)%MP(2) == MEXPUN) THEN
                  CALL FMST2M('UNDERFLOW',MXY(12))
                  KFLAG = -6
              ELSE
                  CALL FMI2M(1,MXY(7))
                  CALL FMI2M(1,MXY(8))
                  CALL FMDIV(MXY(8),MXY(1),MXY(9))
                  IF (MXY(9)%MP(2) /= MUNKNO .AND. MXY(9)%MP(3) /= 0)  &
                      MXY(9)%MP(1) = -MXY(9)%MP(1)
                  JS = 0
                  DO J = 0, 10000
                     CALL FMMPYI_R1(MXY(7),N+J)
                     CALL FMMPY_R1(MXY(7),MXY(9))
                     CALL FMADD_R1(MXY(8),MXY(7))
                     IF (KFLAG == 1) THEN
                         JS = 1
                         EXIT
                     ENDIF
                  ENDDO
                  IF (JS == 1) THEN
                      CALL FMMPY(MXY(6),MXY(8),MXY(12))
                  ELSE
                      CALL FMST2M('UNKNOWN',MXY(12))
                      KFLAG = -4
                  ENDIF
              ENDIF
          ELSE
              CALL FMEQ(MXY(13),MXY(12))
              N = IVAL - 1
              CALL FMIPWR(MXY(1),N,MXY(3))
              CALL FMMPY_R2(MXY(3),MXY(12))
          ENDIF
      ENDIF

  180 NAMEST(NCALL) = 'FMEN'

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(12)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL FMEXT2(MXY(12),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMEN

      SUBROUTINE FMERF(MA,MB)

!  MB = Erf(MA)    Error function.

!  2/Sqrt(pi) * Integral from 0 to MA of e**(-t**2) dt.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: C1,C2,ERR,X
      DOUBLE PRECISION, EXTERNAL :: FMNTERMS
      INTEGER :: IEXTRA,J,J2,KFLAGX,KL,KOVUN,KR_RETRY,KRESLT,KRSAVE,NDSAVE,NDSAV1,NDSAV2,  &
                 NMETHD,NTERM,NTERMS
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(13),MJSUMS(LJSUMS)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL FMENT2('FMERF    ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF

      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)

!             Check for special cases.

      IF (MXY(1)%MP(3) == 0) THEN
          CALL FMEQ(MXY(1),MXY(3))
          GO TO 180
      ENDIF
      IF (MXY(1)%MP(2) == MEXPUN) THEN
          CALL FMST2M('UNKNOWN',MXY(3))
          KFLAG = -4
          GO TO 180
      ENDIF

!             X is a double precision approximation to the input argument to this function.

      MXY(1)%MP(1) = 1
      CALL FMM2DP(MXY(1),X)
      KFLAGX = KFLAG
      IF (KFLAG /= 0 .AND. MXY(1)%MP(2) < 0) THEN
          X = 1.0D0/DPMAX**0.33D0
          IF (MXY(1)%MP(1) < 0) X = -X
          KFLAGX = 0
      ENDIF
      IF (KFLAG /= 0 .AND. MXY(1)%MP(2) > 0) THEN
          X = DPMAX**0.33D0
          IF (MXY(1)%MP(1) < 0) X = -X
          KFLAGX = 0
      ENDIF

!             Determine which method to use.

!             NMETHD = 1 use the convergent series,
!                    = 2 use a 2nd convergent series -- slower but no cancellation,
!                    = 3 use a continued fraction expansion.

      C1 = SQRT(DLOGMB*(NDIG+21))/5
      C2 = (DLOGMB*(NDIG+6))**0.6D0/4.1D0
      IF (MXY(1)%MP(2) < 1) THEN
          NMETHD = 1
      ELSE IF (ABS(X) <= C1) THEN
          NMETHD = 1
      ELSE IF (ABS(X) <= C2) THEN
          NMETHD = 2
      ELSE
          NMETHD = 3
      ENDIF
      IF (NMETHD /= 1) GO TO 140

!             Method 1.  Use the (-1)**n x**(2n+1) / n! / (2n+1) series.

!             If MA is large in magnitude, use more guard digits.

      IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG-1)
      ENDIF
      IF (KFLAGX == 0) THEN
          IF (ABS(X) > 2 .AND. ABS(X) < SQRT(HUGE(X))) THEN
              IEXTRA = MAX(INT((X*X-1.83-LOG(2*X*X+1))/ALOGMB+1),0)
              IF (IEXTRA > 0) THEN
                  CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
              ENDIF
              NDIG = NDIG + IEXTRA
          ENDIF
      ENDIF
      IF (KFLAGX == 0) THEN
          J2 = INT(0.66*SQRT(FMNTERMS(X**2,1,0,0,1)) - 0.8)
          J2 = MAX(2,MIN(J2+MOD(J2,2),LJSUMS))
      ELSE
          J2 = 2
      ENDIF
      NDSAV1 = NDIG

!             Split into J2 concurrent sums.

      NTERM = 1
      CALL FMI2M(1,MXY(6))
      CALL FMI2M(1,MJSUMS(1))
      DO J = 2, J2
         IF (NTERM > 1) CALL FMCSDIVI_R1(MXY(6),NTERM)
         CALL FMCSDIVI(MXY(6),2*NTERM+1,MJSUMS(J))
         NTERM = NTERM + 1
      ENDDO
      IF (MXY(1)%MP(2) < -NDIG) GO TO 130
      CALL FMIPWR(MXY(1),2*J2,MXY(5))

  120 CALL FMCSMPY_R1(MXY(6),MXY(5))
      DO J = 1, J2
         CALL FMCSDIVI_R1(MXY(6),NTERM)
         CALL FMCSDIVI(MXY(6),2*NTERM+1,MXY(4))
         NDIG = NDSAV1
         CALL FMCSADDNN_R1(MJSUMS(J),MXY(4))
         IF (KFLAG /= 0) GO TO 130
         NDIG = NDSAV1 - INT(MJSUMS(J)%MP(2)-MXY(4)%MP(2))
         NDIG = MIN(NDSAV1,NDIG)
         IF (NDIG < NGRD22) NDIG = NGRD22
         NTERM = NTERM + 1
      ENDDO
      GO TO 120

!             Put the J2 separate sums back together.

  130 KFLAG = 0
      CALL FMCSNSUMS(J2,MJSUMS)
      CALL FMSQR(MXY(1),MXY(4))
      IF (MXY(4)%MP(2) /= MUNKNO .AND. MXY(4)%MP(3) /= 0)  &
          MXY(4)%MP(1) = -MXY(4)%MP(1)
      CALL FMEQ(MJSUMS(J2),MXY(5))
      DO J = 2, J2
         CALL FMCSMPY_R1(MXY(5),MXY(4))
         CALL FMADD_R1(MXY(5),MJSUMS(J2-J+1))
      ENDDO
      CALL FMCSMPY_R1(MXY(5),MXY(1))

      CALL FMPI(MXY(4))
      CALL FMSQRT(MXY(4),MXY(3))
      CALL FMCSMPYI(MXY(5),2,MXY(4))
      CALL FMDIV_R2(MXY(4),MXY(3))
      GO TO 180

  140 IF (NMETHD /= 2) GO TO 170

!             Method 2.  Use the x**(2n+1) * 2**(n+1) / (1*3*5*...*(2n+1)) series.

      IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG-1)
      ENDIF
      MXY(1)%MP(1) = 1
      IF (KFLAGX == 0) THEN
          J2 = INT(0.51*SQRT(FMNTERMS(2*X,2,-1,0,1)))
      ELSE
          J2 = 1
      ENDIF
      J2 = MAX(1,MIN(LJSUMS,J2))
      NDSAV1 = NDIG

!             Split into J2 concurrent sums.

      NTERM = 1
      CALL FMCSMPYI(MXY(1),2,MXY(6))
      CALL FMEQ(MXY(6),MJSUMS(1))
      DO J = 2, J2
         CALL FMCSMPYI_R1(MXY(6),2)
         CALL FMCSDIVI_R1(MXY(6),2*NTERM+1)
         CALL FMEQ(MXY(6),MJSUMS(J))
         NTERM = NTERM + 1
      ENDDO
      IF (MXY(1)%MP(2) < -NDIG) GO TO 160
      CALL FMIPWR(MXY(1),2*J2,MXY(5))

  150 CALL FMCSMPY_R1(MXY(6),MXY(5))
      DO J = 1, J2
         CALL FMCSMPYI_R1(MXY(6),2)
         CALL FMCSDIVI_R1(MXY(6),2*NTERM+1)
         NDIG = NDSAV1
         CALL FMCSADDNN_R1(MJSUMS(J),MXY(6))
         IF (KFLAG /= 0) GO TO 160
         NDIG = NDSAV1 - INT(MJSUMS(J)%MP(2)-MXY(6)%MP(2))
         NDIG = MIN(NDSAV1,NDIG)
         IF (NDIG < NGRD22) NDIG = NGRD22
         NTERM = NTERM + 1
      ENDDO
      GO TO 150

!             Put the J2 separate sums back together.

  160 KFLAG = 0
      CALL FMCSNSUMS(J2,MJSUMS)
      CALL FMSQR(MXY(1),MXY(6))
      CALL FMEQ(MJSUMS(J2),MXY(5))
      DO J = 2, J2
         CALL FMCSMPY_R1(MXY(5),MXY(6))
         CALL FMADD_R1(MXY(5),MJSUMS(J2-J+1))
      ENDDO

      CALL FMPI(MXY(4))
      CALL FMSQRT(MXY(4),MXY(3))
      CALL FMDIV_R2(MXY(5),MXY(3))
      CALL FMEXP(MXY(6),MXY(4))
      CALL FMDIV_R1(MXY(3),MXY(4))

      GO TO 180

!             Method 3.  Use the continued fraction expansion.

!             MXY(2) is abs(x).
!             MXY(3) is abs(2x).
!             MXY(4) is -x*x.
!             MXY(13) is the current approximation.
!             MXY(10) is the term in the sum, S(k).
!             MXY(8), MXY(9) are the latest denominators, Q(k-1) and Q(k).

  170 IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG-1)
      ENDIF
      NDSAV1 = NDIG
      CALL FMABS(MXY(1),MXY(2))
      CALL FMMPYI(MXY(2),2,MXY(3))
      CALL FMSQR(MXY(2),MXY(4))
      IF (MXY(4)%MP(2) /= MUNKNO .AND. MXY(4)%MP(3) /= 0)  &
          MXY(4)%MP(1) = -MXY(4)%MP(1)
      CALL FMI2M(0,MXY(8))
      CALL FMI2M(1,MXY(9))
      CALL FMI2M(1,MXY(10))
      CALL FMI2M(1,MXY(13))
      NTERMS = INT(INTMAX/10)

!             Continued fraction loop.

      NDSAV2 = NDIG
      DO J = 1, NTERMS
         IF (MOD(J,2) == 1) THEN
             CALL FMEQ(MXY(2),MXY(11))
             CALL FMCSMPY_R1(MXY(11),MXY(9))
         ELSE
             CALL FMEQ(MXY(3),MXY(11))
             CALL FMCSMPY_R1(MXY(11),MXY(9))
         ENDIF
         IF (J == 1) THEN
             CALL FMEQ(MXY(11),MXY(7))
             CALL FMEXP(MXY(4),MXY(5))
             CALL FMPI(MXY(12))
             CALL FMSQRT(MXY(12),MXY(6))
             CALL FMDIV(MXY(5),MXY(6),MXY(10))
             CALL FMDIV_R1(MXY(10),MXY(2))
             IF (MXY(10)%MP(2) /= MUNKNO .AND. MXY(10)%MP(3) /= 0)  &
                 MXY(10)%MP(1) = -MXY(10)%MP(1)
         ELSE
             CALL FMCSMPYI(MXY(8),J-1,MXY(12))
             CALL FMADD(MXY(11),MXY(12),MXY(7))
             CALL FMCSMPY_R1(MXY(10),MXY(12))
             CALL FMEQ(MXY(10),MXY(5))
             CALL FMCSDIV(MXY(5),MXY(7),MXY(10))
             IF (MXY(10)%MP(2) /= MUNKNO .AND. MXY(10)%MP(3) /= 0)  &
                 MXY(10)%MP(1) = -MXY(10)%MP(1)
         ENDIF
         CALL FMEQ(MXY(9),MXY(8))
         CALL FMEQ(MXY(7),MXY(9))
         NDIG = NDSAV2
         CALL FMCSADD_R1(MXY(13),MXY(10))

!             Check for convergence.

         IF (KFLAG == 1) THEN
             EXIT
         ENDIF
         NDIG = MIN(NDSAV2,MAX(NGRD22,NDSAV2-INT(MXY(13)%MP(2)-MXY(10)%MP(2))))
      ENDDO

      CALL FMEQ(MXY(13),MXY(3))
      NDIG = NDSAV1
  180 IF (KROUND /= 1) THEN
          IF (MXY(3)%MP(2) == 1 .AND. MA%MP(1) > 0 .AND.  &
              (KROUND == -1 .OR. KROUND == 0)) THEN
              CALL FMTINY(MXY(1))
              CALL FMSUB_R1(MXY(3),MXY(1))
          ENDIF
          IF (MXY(3)%MP(2) == 1 .AND. MA%MP(1) < 0 .AND.  &
              (KROUND == 2 .OR. KROUND == 0)) THEN
              KRSAVE = KROUND
              KROUND = 0
              CALL FMTINY(MXY(1))
              CALL FMSUB_R1(MXY(3),MXY(1))
              KROUND = KRSAVE
          ENDIF
          IF (KFLAG == 1) KFLAG = 0
      ENDIF

      IF (MA%MP(1) < 0) THEN
          IF (MXY(3)%MP(2) /= MUNKNO .AND. MXY(3)%MP(3) /= 0)  &
              MXY(3)%MP(1) = -MXY(3)%MP(1)
      ENDIF
      NAMEST(NCALL) = 'FMERF'

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(3)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL FMEXT2(MXY(3),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMERF

      SUBROUTINE FMERFC(MA,MB)

!  MB = ERFC(MA)    Complimentary Error function.

!  2/Sqrt(pi) * Integral from MA to infinity of e**(-t**2) dt.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: C1,ERR,X
      INTEGER :: IEXTRA,J,K,KL,KOVUN,KR_RETRY,KRESLT,KRSAVE,K_RETURN_CODE,  &
                 NDSAVE,NDSAV1,NDSAV2,NMETHD,NTERMS
      LOGICAL, EXTERNAL :: FMCOMP
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(13)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      K_RETURN_CODE = 0

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1 .AND. MA%MP(2) < -NDIG) THEN
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          KRSAVE = KROUND
          KROUND = 1
          NDSAVE = NDIG
          NDIG = NDIG + NGRD52
          CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
          CALL FMI2M(1,MXY(4))
          IF (MA%MP(2) == MEXPUN) THEN
              CALL FMI2M(0,MXY(2))
              CALL FMSUB(MXY(2),MXY(1),MXY(5))
          ELSE
              CALL FMPI(MXY(2))
              CALL FMSQRT(MXY(2),MXY(3))
              CALL FMDIV(MXY(4),MXY(3),MXY(5))
              CALL FMMPYI(MXY(5),-2,MXY(6))
              CALL FMMPY(MXY(1),MXY(6),MXY(5))
          ENDIF
          IF (MXY(4)%MP(2) - MXY(5)%MP(2) > NDIG) THEN
              CALL FMEQU(MXY(4),MXY(6),NDIG,NDSAVE)
              CALL FMEQU(MXY(5),MXY(7),NDIG,NDSAVE)
              NDIG = NDSAVE
              KROUND = KRSAVE
              CALL FMADD(MXY(6),MXY(7),MB)
              K_RETURN_CODE = 1
          ENDIF
          KFLAG = 0
          NTRACE = J
          KWARN = K
          NDIG = NDSAVE
          KROUND = KRSAVE
          IF (K_RETURN_CODE == 1) THEN
              IF (NTRACE /= 0) THEN
                  NCALL = NCALL + 1
                  NAMEST(NCALL) = 'FMERFC'
                  CALL FMNTR(2,MA,MA,1,1)
                  CALL FMNTR(1,MB,MB,1,1)
                  NCALL = NCALL - 1
              ENDIF
          ENDIF
      ENDIF
      IF (K_RETURN_CODE == 1) RETURN

      CALL FMENT2('FMERFC   ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF

      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL FMI2M(1,MXY(3))
      CALL FMDIVI_R1(MXY(3),2)
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)

!             Check for special cases.

      IF (MXY(1)%MP(3) == 0 .OR. MXY(1)%MP(2) == MEXPUN) THEN
          CALL FMI2M(1,MXY(3))
          GO TO 130
      ENDIF

!             X is a double precision approximation to the input argument to this function.

      CALL FMM2DP(MXY(1),X)
      IF (KFLAG /= 0 .AND. MXY(1)%MP(2) < 0) THEN
          X = 1.0D0/DPMAX
          IF (MXY(1)%MP(1) < 0) X = -X
      ENDIF
      IF (KFLAG /= 0 .AND. MXY(1)%MP(2) > 0) THEN
          X = DPMAX
          IF (MXY(1)%MP(1) < 0) X = -X
      ENDIF

!             Determine which method to use.

!             NMETHD = 1 means use 1 - erf(x),
!                    = 2 means use the continued fraction expansion.

      C1 = (DLOGMB*(NDIG+6))**0.7D0/9
      IF (MXY(1)%MP(2) < 1) THEN
          NMETHD = 1
      ELSE IF (X <= C1) THEN
          NMETHD = 1
      ELSE
          NMETHD = 2
      ENDIF
      IF (NMETHD /= 1) GO TO 120

!             Method 1.  Use ERF.

      IF (KR_RETRY <= 0 .AND. NCALL <= 1 .AND. NDIG < 100) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG-1)
      ENDIF
      IF (X < 2) THEN
          CALL FMERF(MXY(1),MXY(4))
          CALL FMI2M(1,MXY(2))
          CALL FMSUB(MXY(2),MXY(4),MXY(3))
          GO TO 130
      ENDIF

!             Raise precision to compensate for cancellation if x is large.

      IEXTRA = MAX(INT((X*X+LOG(2*X))/ALOGMB+1),0)
      IF (IEXTRA > 0) THEN
          CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
      ENDIF
      NDIG = NDIG + IEXTRA
      RAISE_NDIG = 1
      CALL FMERF(MXY(1),MXY(4))
      RAISE_NDIG = 0
      CALL FMI2M(1,MXY(2))
      CALL FMSUB(MXY(2),MXY(4),MXY(3))
      GO TO 130

  120 IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG-1)
      ENDIF
      NDSAV1 = NDIG

!             Method 2.  Use the continued fraction expansion.

!             MXY(2) is abs(x).
!             MXY(3) is abs(2x).
!             MXY(4) is -x*x.
!             MXY(13) is the current approximation.
!             MXY(10) is the term in the sum, S(k).
!             MXY(8), MXY(9) are the latest denominators, Q(k-1) and Q(k).

      CALL FMABS(MXY(1),MXY(2))
      CALL FMMPYI(MXY(2),2,MXY(3))
      CALL FMSQR(MXY(2),MXY(4))
      IF (MXY(4)%MP(2) /= MUNKNO .AND. MXY(4)%MP(3) /= 0)  &
          MXY(4)%MP(1) = -MXY(4)%MP(1)
      CALL FMI2M(0,MXY(8))
      CALL FMI2M(1,MXY(9))
      CALL FMI2M(1,MXY(10))
      CALL FMI2M(0,MXY(13))
      NTERMS = INT(INTMAX/10)

!             Continued fraction loop.

      NDSAV2 = NDIG
      DO J = 1, NTERMS
         IF (MOD(J,2) == 1) THEN
             CALL FMEQ(MXY(2),MXY(11))
             CALL FMCSMPY_R1(MXY(11),MXY(9))
         ELSE
             CALL FMEQ(MXY(3),MXY(11))
             CALL FMCSMPY_R1(MXY(11),MXY(9))
         ENDIF
         IF (J == 1) THEN
             CALL FMEQ(MXY(11),MXY(7))
             CALL FMEXP(MXY(4),MXY(5))
             CALL FMPI(MXY(12))
             CALL FMSQRT(MXY(12),MXY(6))
             CALL FMDIV(MXY(5),MXY(6),MXY(10))
             CALL FMDIV_R1(MXY(10),MXY(2))
             IF (MXY(10)%MP(2) == MEXPUN) THEN
                 CALL FMEQ(MXY(10),MXY(3))
                 GO TO 130
             ENDIF
         ELSE
             CALL FMCSMPYI(MXY(8),J-1,MXY(12))
             CALL FMADD(MXY(11),MXY(12),MXY(7))
             CALL FMCSMPY_R1(MXY(10),MXY(12))
             CALL FMEQ(MXY(10),MXY(5))
             CALL FMCSDIV(MXY(5),MXY(7),MXY(10))
             IF (MXY(10)%MP(2) /= MUNKNO .AND. MXY(10)%MP(3) /= 0)  &
                 MXY(10)%MP(1) = -MXY(10)%MP(1)
         ENDIF
         CALL FMEQ(MXY(9),MXY(8))
         CALL FMEQ(MXY(7),MXY(9))
         NDIG = NDSAV2
         CALL FMCSADD_R1(MXY(13),MXY(10))

!             Check for convergence.

         IF (J > 1 .AND. KFLAG == 1) THEN
             EXIT
         ENDIF
         NDIG = MIN(NDSAV2,MAX(NGRD22,NDSAV2-INT(MXY(13)%MP(2)-MXY(10)%MP(2))))
      ENDDO

      CALL FMEQ(MXY(13),MXY(3))
      NDIG = NDSAV1

  130 NAMEST(NCALL) = 'FMERFC'

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(3)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (KROUND == -1 .OR. KROUND == 0) THEN
          CALL FMI2M(2,MXY(1))
          IF (FMCOMP(MXY(3),'==',MXY(1))) THEN
              CALL FMDP2M(1.9999D0,MXY(2))
              CALL FMULP(MXY(2),MXY(4))
              CALL FMSUB(MXY(1),MXY(4),MXY(3))
          ENDIF
      ENDIF
      CALL FMEXT2(MXY(3),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMERFC

      SUBROUTINE FMERFCS(MA,MB)

!  MB = ERFC_SCALED(MA)    Scaled Complimentary Error function.

!     = exp(x^2) * erfc(x)

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: C1,ERR,X
      INTEGER :: IEXTRA,J,K,KL,KOVUN,KR_RETRY,KRESLT,KRSAVE,K_RETURN_CODE,  &
                 NDSAVE,NDSAV1,NDSAV2,NMETHD,NTERMS
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(13)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      K_RETURN_CODE = 0

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1 .AND. MA%MP(2) < -NDIG) THEN
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          KRSAVE = KROUND
          KROUND = 1
          NDSAVE = NDIG
          NDIG = NDIG + NGRD52
          CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
          CALL FMI2M(1,MXY(4))
          IF (MA%MP(2) == MEXPUN) THEN
              CALL FMI2M(0,MXY(2))
              CALL FMSUB(MXY(2),MXY(1),MXY(5))
          ELSE
              CALL FMPI(MXY(2))
              CALL FMSQRT(MXY(2),MXY(3))
              CALL FMDIV(MXY(4),MXY(3),MXY(5))
              CALL FMMPYI(MXY(5),-2,MXY(6))
              CALL FMMPY(MXY(1),MXY(6),MXY(5))
          ENDIF
          IF (MXY(4)%MP(2) - MXY(5)%MP(2) > NDIG) THEN
              CALL FMEQU(MXY(4),MXY(6),NDIG,NDSAVE)
              CALL FMEQU(MXY(5),MXY(7),NDIG,NDSAVE)
              NDIG = NDSAVE
              KROUND = KRSAVE
              CALL FMADD(MXY(6),MXY(7),MB)
              K_RETURN_CODE = 1
          ENDIF
          KFLAG = 0
          NTRACE = J
          KWARN = K
          NDIG = NDSAVE
          KROUND = KRSAVE
          IF (K_RETURN_CODE == 1) THEN
              IF (NTRACE /= 0) THEN
                  NCALL = NCALL + 1
                  NAMEST(NCALL) = 'FMERFCS'
                  CALL FMNTR(2,MA,MA,1,1)
                  CALL FMNTR(1,MB,MB,1,1)
                  NCALL = NCALL - 1
              ENDIF
          ENDIF
      ENDIF
      IF (K_RETURN_CODE == 1) RETURN

      CALL FMENT2('FMERFCS  ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF

      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL FMI2M(1,MXY(3))
      CALL FMDIVI_R1(MXY(3),2)
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)

!             Check for special cases.

      IF (MXY(1)%MP(3) == 0 .OR. MXY(1)%MP(2) == MEXPUN) THEN
          CALL FMI2M(1,MXY(3))
          GO TO 130
      ENDIF

!             X is a double precision approximation to the input argument to this function.

      CALL FMM2DP(MXY(1),X)
      IF (KFLAG /= 0 .AND. MXY(1)%MP(2) < 0) THEN
          X = 1.0D0/DPMAX
          IF (MXY(1)%MP(1) < 0) X = -X
      ENDIF
      IF (KFLAG /= 0 .AND. MXY(1)%MP(2) > 0) THEN
          X = DPMAX
          IF (MXY(1)%MP(1) < 0) X = -X
      ENDIF

!             Determine which method to use.

!             NMETHD = 1 means use exp(x^2) * (1 - erf(x)),
!                    = 2 means use the continued fraction expansion.

      C1 = (DLOGMB*(NDIG+6))**0.7D0/9
      IF (MXY(1)%MP(2) < 1) THEN
          NMETHD = 1
      ELSE IF (X <= C1) THEN
          NMETHD = 1
      ELSE
          NMETHD = 2
      ENDIF
      IF (NMETHD /= 1) GO TO 120

!             Method 1.  Use ERF.

      IF (KR_RETRY <= 0 .AND. NCALL <= 1 .AND. NDIG < 100) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG-1)
      ENDIF
      IF (X < 2) THEN
          RAISE_NDIG = 1
          CALL FMERF(MXY(1),MXY(4))
          RAISE_NDIG = 0
          CALL FMI2M(1,MXY(2))
          CALL FMSUB(MXY(2),MXY(4),MXY(3))
          CALL FMSQR(MXY(1),MXY(2))
          CALL FMEXP(MXY(2),MXY(4))
          CALL FMMPY_R1(MXY(3),MXY(4))
          GO TO 130
      ENDIF

!             Raise precision to compensate for cancellation if x is large.

      IEXTRA = MAX(INT((X*X+LOG(2*X))/ALOGMB+1),0)
      IF (IEXTRA > 0) THEN
          CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
      ENDIF
      NDIG = NDIG + IEXTRA
      RAISE_NDIG = 1
      CALL FMERF(MXY(1),MXY(4))
      RAISE_NDIG = 0
      CALL FMI2M(1,MXY(2))
      CALL FMSUB(MXY(2),MXY(4),MXY(3))
      CALL FMSQR(MXY(1),MXY(2))
      CALL FMEXP(MXY(2),MXY(4))
      CALL FMMPY_R1(MXY(3),MXY(4))
      GO TO 130

  120 IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG-1)
      ENDIF
      NDSAV1 = NDIG

!             Method 2.  Use the continued fraction expansion.

!             MXY(2) is abs(x).
!             MXY(3) is abs(2x).
!             MXY(13) is the current approximation.
!             MXY(10) is the term in the sum, S(k).
!             MXY(8), MXY(9) are the latest denominators, Q(k-1) and Q(k).

      CALL FMABS(MXY(1),MXY(2))
      CALL FMMPYI(MXY(2),2,MXY(3))
      CALL FMI2M(0,MXY(8))
      CALL FMI2M(1,MXY(9))
      CALL FMI2M(1,MXY(10))
      CALL FMI2M(0,MXY(13))
      NTERMS = INT(INTMAX/10)

!             Continued fraction loop.

      NDSAV2 = NDIG
      DO J = 1, NTERMS
         IF (MOD(J,2) == 1) THEN
             CALL FMEQ(MXY(2),MXY(11))
             CALL FMCSMPY_R1(MXY(11),MXY(9))
         ELSE
             CALL FMEQ(MXY(3),MXY(11))
             CALL FMCSMPY_R1(MXY(11),MXY(9))
         ENDIF
         IF (J == 1) THEN
             CALL FMEQ(MXY(11),MXY(7))
             CALL FMI2M(1,MXY(5))
             CALL FMPI(MXY(12))
             CALL FMSQRT(MXY(12),MXY(6))
             CALL FMDIV(MXY(5),MXY(6),MXY(10))
             CALL FMDIV_R1(MXY(10),MXY(2))
             IF (MXY(10)%MP(2) == MEXPUN) THEN
                 CALL FMEQ(MXY(10),MXY(3))
                 GO TO 130
             ENDIF
         ELSE
             CALL FMCSMPYI(MXY(8),J-1,MXY(12))
             CALL FMADD(MXY(11),MXY(12),MXY(7))
             CALL FMCSMPY_R1(MXY(10),MXY(12))
             CALL FMEQ(MXY(10),MXY(5))
             CALL FMCSDIV(MXY(5),MXY(7),MXY(10))
             IF (MXY(10)%MP(2) /= MUNKNO .AND. MXY(10)%MP(3) /= 0)  &
                 MXY(10)%MP(1) = -MXY(10)%MP(1)
         ENDIF
         CALL FMEQ(MXY(9),MXY(8))
         CALL FMEQ(MXY(7),MXY(9))
         NDIG = NDSAV2
         CALL FMCSADD_R1(MXY(13),MXY(10))

!             Check for convergence.

         IF (J > 1 .AND. KFLAG == 1) THEN
             EXIT
         ENDIF
         NDIG = MIN(NDSAV2,MAX(NGRD22,NDSAV2-INT(MXY(13)%MP(2)-MXY(10)%MP(2))))
      ENDDO

      CALL FMEQ(MXY(13),MXY(3))
      NDIG = NDSAV1

  130 NAMEST(NCALL) = 'FMERFCS'

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(3)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF

      CALL FMEXT2(MXY(3),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMERFCS

      SUBROUTINE FMFGFI(MA,MB,MC)

!  Internal routine for the two auxiliary asymptotic series used in the sine and cosine
!  Fresnel integral functions.

!  For x = MA the two values returned are MB and MC, where

!  MB = f(x) = (1 - 1*3/(pi*x*x)**2 + 1*3*5*7/(pi*x*x)**4 - ...) / (pi*x)
!  MC = g(x) = (1 - 1*3*5/(pi*x*x)**2 + 1*3*5*7*9/(pi*x*x)**4 - ...) / (pi**2*x**3)

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC
      INTEGER :: J,J2,JTERMS,NBOT,NDSAV1,NTERM,NTERMS
      DOUBLE PRECISION :: X
      DOUBLE PRECISION, EXTERNAL :: FMNTERMS
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB,MC
      TYPE(MULTI) :: MXY(6),MJSUMS(LJSUMS)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < NDIG+2) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NTERMS = INT(INTMAX/10)
      CALL FMM2DP(MA,X)
      IF (KFLAG == 0) THEN
          J2 = INT(0.35*SQRT(FMNTERMS(3.14159D0*X*X/2,2,0,0,0)) - 0.2)
          J2 = MAX(2,MIN(J2+MOD(J2,2),LJSUMS/2))
      ELSE
          J2 = 2
      ENDIF
      NDSAV1 = NDIG

!             Split into J2 concurrent sums for f(x) and another J2 concurrent sums for g(x).
!             Because the two sums use the same powers of x, the calculations for f and g
!             are interleaved.

      CALL FMI2M(1,MXY(4))
      CALL FMDIV(MXY(4),MA,MXY(2))
      CALL FMSQR(MXY(2),MXY(6))
      CALL FMPI(MXY(1))
      CALL FMDIV(MXY(6),MXY(1),MXY(2))
      CALL FMSQR(MXY(2),MXY(6))
      NTERM = 0
      DO J = 1, J2
         NBOT = 2*NTERM - 1
         IF (NTERM >= 2) CALL FMCSMPYI_R1(MXY(4),NBOT)
         CALL FMEQ(MXY(4),MJSUMS(J))
         NTERM = NTERM + 1

         NBOT = 2*NTERM - 1
         IF (NTERM >= 2) CALL FMCSMPYI_R1(MXY(4),NBOT)
         CALL FMEQ(MXY(4),MJSUMS(J2+J))
         NTERM = NTERM + 1
      ENDDO
      IF (MXY(6)%MP(2)+MXY(4)%MP(2) < -NDIG-3) GO TO 110
      CALL FMIPWR(MXY(6),J2,MXY(3))

      DO JTERMS = 1, NTERMS
         CALL FMCSMPY_R1(MXY(4),MXY(3))
         DO J = 1, J2
            NBOT = 2*NTERM - 1
            CALL FMCSMPYI_R1(MXY(4),NBOT)
            NDIG = NDSAV1
            CALL FMCSADDNN_R1(MJSUMS(J),MXY(4))
            IF (KFLAG /= 0) GO TO 110
            NDIG = NDSAV1 - INT(MJSUMS(J)%MP(2)-MXY(4)%MP(2))
            NDIG = MIN(NDSAV1,NDIG)
            IF (NDIG < NGRD22) NDIG = NGRD22
            NTERM = NTERM + 1

            NBOT = 2*NTERM - 1
            CALL FMCSMPYI_R1(MXY(4),NBOT)
            NDIG = NDSAV1
            CALL FMCSADDNN_R1(MJSUMS(J2+J),MXY(4))
            IF (KFLAG /= 0) GO TO 110
            NDIG = NDSAV1 - INT(MJSUMS(J2+J)%MP(2)-MXY(4)%MP(2))
            NDIG = MIN(NDSAV1,NDIG)
            IF (NDIG < NGRD22) NDIG = NGRD22
            NTERM = NTERM + 1
         ENDDO
      ENDDO

!             Put the J2 separate sums back together.

  110 KFLAG = 0
      CALL FMCSNSUMS(2*J2,MJSUMS)
      CALL FMEQ(MJSUMS(J2),MXY(3))
      CALL FMEQ(MJSUMS(J2+J2),MXY(5))
      IF( MXY(6)%MP(2) /= MUNKNO .AND. MXY(6)%MP(3) /= 0) MXY(6)%MP(1) = -1
      DO J = 2, J2
         CALL FMCSMPY_R1(MXY(3),MXY(6))
         CALL FMADD_R1(MXY(3),MJSUMS(J2-J+1))

         CALL FMCSMPY_R1(MXY(5),MXY(6))
         CALL FMADD_R1(MXY(5),MJSUMS(J2+J2-J+1))
      ENDDO
      CALL FMMPY(MA,MXY(1),MXY(2))
      CALL FMDIV(MXY(3),MXY(2),MB)

      CALL FMSQR(MXY(2),MXY(3))
      CALL FMMPY(MXY(3),MA,MXY(2))
      CALL FMDIV(MXY(5),MXY(2),MC)

      NDIG = NDSAV1
      RETURN
      END SUBROUTINE FMFGFI

      SUBROUTINE FMFHGH(MA,MB,MC)

!  Internal routine for the two auxiliary asymptotic series used in the hyperbolic sine and
!  cosine integral functions.

!  For x = MA the two values returned are MB and MC, where

!  MB = f(x) = (1 + 2!/x**2 + 4!/x**4 + ...) / x
!  MC = g(x) = (1 + 3!/x**2 + 5!/x**4 + ...) / x**2

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC
      INTEGER :: J,J2,JTERMS,NBOT,NDSAV1,NTERM,NTERMS
      DOUBLE PRECISION :: X
      DOUBLE PRECISION, EXTERNAL :: FMNTERMS
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB,MC
      TYPE(MULTI) :: MXY(5),MJSUMS(LJSUMS)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < NDIG+2) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NTERMS = INT(INTMAX/10)
      CALL FMM2DP(MA,X)
      IF (KFLAG == 0) THEN
          J2 = INT(0.30*SQRT(FMNTERMS(X,2,0,0,0)) + 0.8)
          J2 = MAX(1,MIN(LJSUMS/2,J2))
      ELSE
          J2 = 1
      ENDIF
      NDSAV1 = NDIG

!             Split into J2 concurrent sums for f(x) and another J2 concurrent sums for g(x).
!             Because the two sums use the same powers of x, the calculations for f and g
!             are interleaved.

      CALL FMI2M(1,MXY(3))
      CALL FMDIV(MXY(3),MA,MXY(1))
      CALL FMSQR(MXY(1),MXY(5))
      NTERM = 0
      DO J = 1, J2
         NBOT = NTERM
         IF (NTERM >= 2) CALL FMCSMPYI_R1(MXY(3),NBOT)
         CALL FMEQ(MXY(3),MJSUMS(J))
         NTERM = NTERM + 1

         IF (NTERM >= 2) CALL FMCSMPYI_R1(MXY(3),NTERM)
         CALL FMEQ(MXY(3),MJSUMS(J2+J))
         NTERM = NTERM + 1
      ENDDO
      IF (MXY(5)%MP(2)+MXY(3)%MP(2) < -NDIG-3) GO TO 110
      CALL FMIPWR(MXY(5),J2,MXY(2))

      DO JTERMS = 1, NTERMS
         CALL FMCSMPY_R1(MXY(3),MXY(2))
         DO J = 1, J2
            NBOT = NTERM
            CALL FMCSMPYI_R1(MXY(3),NBOT)
            NDIG = NDSAV1
            CALL FMCSADDNN_R1(MJSUMS(J),MXY(3))
            IF (KFLAG /= 0) GO TO 110
            NDIG = NDSAV1 - INT(MJSUMS(J)%MP(2)-MXY(3)%MP(2))
            NDIG = MIN(NDSAV1,NDIG)
            IF (NDIG < NGRD22) NDIG = NGRD22
            NTERM = NTERM + 1

            CALL FMCSMPYI_R1(MXY(3),NTERM)
            NDIG = NDSAV1
            CALL FMCSADDNN_R1(MJSUMS(J2+J),MXY(3))
            IF (KFLAG /= 0) GO TO 110
            NDIG = NDSAV1 - INT(MJSUMS(J2+J)%MP(2)-MXY(3)%MP(2))
            NDIG = MIN(NDSAV1,NDIG)
            IF (NDIG < NGRD22) NDIG = NGRD22
            NTERM = NTERM + 1
         ENDDO
      ENDDO

!             Put the J2 separate sums back together.

  110 KFLAG = 0
      CALL FMCSNSUMS(2*J2,MJSUMS)
      CALL FMEQ(MJSUMS(J2),MXY(2))
      CALL FMEQ(MJSUMS(2*J2),MXY(4))
      DO J = 2, J2
         CALL FMCSMPY_R1(MXY(2),MXY(5))
         CALL FMADD_R1(MXY(2),MJSUMS(J2-J+1))

         CALL FMCSMPY_R1(MXY(4),MXY(5))
         CALL FMADD_R1(MXY(4),MJSUMS(J2+J2-J+1))
      ENDDO
      CALL FMDIV(MXY(2),MA,MB)
      CALL FMMPY(MXY(4),MXY(5),MC)

      NDIG = NDSAV1
      RETURN
      END SUBROUTINE FMFHGH

      SUBROUTINE FMFXGX(MA,MB,MC)

!  Internal routine for the two auxiliary asymptotic series used in the sine and
!  cosine integral functions.

!  For x = MA the two values returned are MB and MC, where

!  MB = f(x) = (1 - 2!/x**2 + 4!/x**4 - ...) / x
!  MC = g(x) = (1 - 3!/x**2 + 5!/x**4 - ...) / x**2

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC
      INTEGER :: J,J2,JTERMS,NDSAV1,NTERM,NTERMS
      DOUBLE PRECISION :: X
      DOUBLE PRECISION, EXTERNAL :: FMNTERMS
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB,MC
      TYPE(MULTI) :: MXY(5),MJSUMS(LJSUMS)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < NDIG+2) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NTERMS = INT(INTMAX/10)
      CALL FMM2DP(MA,X)
      IF (KFLAG == 0) THEN
          J2 = INT(0.38*SQRT(FMNTERMS(X,2,0,0,0)) - 0.2)
          J2 = MAX(2,MIN(J2+MOD(J2,2),LJSUMS/2))
      ELSE
          J2 = 2
      ENDIF
      NDSAV1 = NDIG

!             Split into J2 concurrent sums for f(x) and another J2 concurrent sums for g(x).
!             Because the two sums use the same powers of x, the calculations for f and g
!             are interleaved.

      CALL FMI2M(1,MXY(3))
      CALL FMDIV(MXY(3),MA,MXY(1))
      CALL FMSQR(MXY(1),MXY(5))
      NTERM = 0
      DO J = 1, J2
         IF (NTERM >= 2) CALL FMCSMPYI_R1(MXY(3),NTERM)
         CALL FMEQ(MXY(3),MJSUMS(J))
         NTERM = NTERM + 1

         IF (NTERM >= 2) CALL FMCSMPYI_R1(MXY(3),NTERM)
         CALL FMEQ(MXY(3),MJSUMS(J2+J))
         NTERM = NTERM + 1
      ENDDO
      IF (MXY(5)%MP(2)+MXY(3)%MP(2) < -NDIG-3) GO TO 110
      CALL FMIPWR(MXY(5),J2,MXY(2))

      DO JTERMS = 1, NTERMS
         CALL FMCSMPY_R1(MXY(3),MXY(2))
         DO J = 1, J2
            CALL FMCSMPYI_R1(MXY(3),NTERM)
            NDIG = NDSAV1
            CALL FMCSADDNN_R1(MJSUMS(J),MXY(3))
            IF (KFLAG /= 0) GO TO 110
            NDIG = NDSAV1 - INT(MJSUMS(J)%MP(2)-MXY(3)%MP(2))
            NDIG = MIN(NDSAV1,NDIG)
            IF (NDIG < NGRD22) NDIG = NGRD22
            NTERM = NTERM + 1

            CALL FMCSMPYI_R1(MXY(3),NTERM)
            NDIG = NDSAV1
            CALL FMCSADDNN_R1(MJSUMS(J2+J),MXY(3))
            IF (KFLAG /= 0) GO TO 110
            NDIG = NDSAV1 - INT(MJSUMS(J2+J)%MP(2)-MXY(3)%MP(2))
            NDIG = MIN(NDSAV1,NDIG)
            IF (NDIG < NGRD22) NDIG = NGRD22
            NTERM = NTERM + 1
         ENDDO
      ENDDO

!             Put the J2 separate sums back together.

  110 KFLAG = 0
      CALL FMCSNSUMS(2*J2,MJSUMS)
      CALL FMEQ(MJSUMS(J2),MXY(2))
      CALL FMEQ(MJSUMS(J2+J2),MXY(4))
      IF (MXY(5)%MP(2) /= MUNKNO .AND. MXY(5)%MP(3) /= 0)  &
          MXY(5)%MP(1) = -MXY(5)%MP(1)
      DO J = 2, J2
         CALL FMCSMPY_R1(MXY(2),MXY(5))
         CALL FMADD_R1(MXY(2),MJSUMS(J2-J+1))

         CALL FMCSMPY_R1(MXY(4),MXY(5))
         CALL FMADD_R1(MXY(4),MJSUMS(J2+J2-J+1))
      ENDDO
      CALL FMDIV(MXY(2),MA,MB)
      MXY(5)%MP(1) = 1
      CALL FMMPY(MXY(4),MXY(5),MC)

      NDIG = NDSAV1
      RETURN
      END SUBROUTINE FMFXGX

      SUBROUTINE FMLERC(MA,MB)

!  MB = Ln(erfc(MA))

!  MA must be positive, and is assumed to be large enough that erfc(MA) might underflow.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,JEXTRA,KL,KOVUN,KR_RETRY,KRESLT,NDSAV1,NDSAV2,NDSAVE,NMETHD,NTERMS
      LOGICAL, EXTERNAL :: FMCOMP
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(16)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL FMENT2('FMLERC   ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF

      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)

      NTERMS = INT(INTMAX/10)

!             Check for special cases.

      IF (MXY(1)%MP(3) == 0) THEN
          CALL FMI2M(0,MXY(11))
          GO TO 140
      ENDIF
      IF (MXY(1)%MP(2) == MEXPUN) THEN
          KFLAG = -4
          CALL FMST2M('UNKNOWN',MXY(11))
          GO TO 140
      ENDIF
      IF (MXY(1)%MP(2) == MEXPOV .AND. MXY(1)%MP(1) > 0) THEN
              CALL FMST2M('-OVERFLOW',MXY(11))
              KFLAG = -5
          GO TO 140
      ENDIF

!             Close to zero use the series for Ln(1-erf(x)).

      IF (LOG(MXY(1)%MP(3)+1.0D0)+(MXY(1)%MP(2)-1)*DLOGMB < -10.0D0*DLOGTN) THEN
          CALL FMERF(MXY(1),MXY(10))
          CALL FMEQ(MXY(10),MXY(11))
          CALL FMEQ(MXY(10),MXY(6))
          IF (MXY(11)%MP(2) /= MUNKNO .AND. MXY(11)%MP(3) /= 0)  &
              MXY(11)%MP(1) = -MXY(11)%MP(1)
          DO J = 2, NTERMS
             CALL FMMPY_R1(MXY(6),MXY(10))
             CALL FMDIVI(MXY(6),J,MXY(3))
             CALL FMSUB_R1(MXY(11),MXY(3))
             IF (KFLAG == 1) EXIT
          ENDDO
          GO TO 140
      ENDIF

!             Determine which method to use.

!             NMETHD = 1 means use FMERFC,
!                    = 2 means use the asymptotic value,
!                    = 3 means use the continued fraction expansion.

      NMETHD = 1
      CALL FMI2M(1000,MXY(3))
      IF (MXY(1)%MP(2) >= NDIG) THEN
          NMETHD = 2
      ELSE IF(FMCOMP(MXY(1),'>',MXY(3))) THEN
          NMETHD = 3
      ENDIF

      IF (NMETHD == 2) GO TO 120
      IF (NMETHD == 3) GO TO 130

!             Method 1.  Use FMERFC.

      IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG-1)
      ENDIF
      CALL FMERFC(MXY(1),MXY(15))
      CALL FMLN(MXY(15),MXY(11))

      NAMEST(NCALL) = 'FMLERC'

      GO TO 140

!             Method 2.  Use the asymptotic value.

  120 IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG-1)
      ENDIF
      IF (MXY(1)%MP(1) == 1) THEN
          CALL FMPI(MXY(16))
          CALL FMSQRT(MXY(16),MXY(3))
          CALL FMMPY_R1(MXY(3),MXY(1))
          CALL FMI2M(1,MXY(2))
          CALL FMDIV(MXY(2),MXY(3),MXY(11))
          CALL FMLN(MXY(11),MXY(4))
          CALL FMSQR(MXY(1),MXY(5))
          CALL FMSUB(MXY(4),MXY(5),MXY(11))
      ELSE
          CALL FMI2M(2,MXY(3))
          CALL FMLN(MXY(3),MXY(11))
      ENDIF

      GO TO 140

!             Method 3.  Use the continued fraction expansion.

!             MXY(14) is the current approximation.
!             MXY(11) is the term in the sum, S(k).
!             MXY(9), MXY(10) are the latest denominators, Q(k-1) and Q(k).

  130 IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG-1)
      ENDIF
      NDSAV1 = NDIG
      JEXTRA = INT(MAX(1.0,5.76/ALOGMB + 1.0))
      IF (NDIG+JEXTRA > NDSAV1) THEN
          CALL FMEQU_R1(MXY(1),NDSAV1,NDSAV1+JEXTRA)
      ENDIF
      NDIG = NDIG + JEXTRA
      CALL FMI2M(1,MXY(9))
      CALL FMEQ(MXY(1),MXY(10))
      CALL FMI2M(1,MXY(11))
      CALL FMI2M(1,MXY(14))

!             Method 3 continued fraction loop.

      NDSAV2 = NDIG
      DO J = 1, NTERMS
         IF (MOD(J,2) == 0) THEN
             CALL FMCSMPYI(MXY(9),J/2,MXY(7))
             CALL FMEQ(MXY(1),MXY(3))
             CALL FMCSMPY_R1(MXY(3),MXY(10))
             CALL FMADD(MXY(3),MXY(7),MXY(8))
         ELSE
             CALL FMCSMPYI(MXY(9),J,MXY(3))
             CALL FMCSDIVI(MXY(3),2,MXY(7))
             CALL FMEQ(MXY(1),MXY(3))
             CALL FMCSMPY_R1(MXY(3),MXY(10))
             CALL FMADD(MXY(3),MXY(7),MXY(8))
         ENDIF
         CALL FMEQ(MXY(7),MXY(3))
         CALL FMCSMPY_R1(MXY(3),MXY(11))
         CALL FMDIV(MXY(3),MXY(8),MXY(11))
         IF (MXY(11)%MP(2) /= MUNKNO .AND. MXY(11)%MP(3) /= 0)  &
             MXY(11)%MP(1) = -MXY(11)%MP(1)
         NDIG = NDSAV2
         CALL FMCSADD_R1(MXY(14),MXY(11))

!             Check for convergence.

         IF (KFLAG == 1) THEN
             EXIT
         ENDIF
         CALL FMEQ(MXY(10),MXY(9))
         CALL FMEQ(MXY(8),MXY(10))
         NDIG = MIN(NDSAV2,MAX(NGRD22,NDSAV2-INT(MXY(14)%MP(2)-MXY(11)%MP(2))))
      ENDDO

      CALL FMEQU_R1(MXY(14),NDIG,NDSAV1)
      NDIG = NDSAV1
      CALL FMPI(MXY(16))
      CALL FMSQRT(MXY(16),MXY(3))
      CALL FMDIV(MXY(14),MXY(3),MXY(11))
      CALL FMDIV(MXY(11),MXY(1),MXY(12))
      CALL FMLN(MXY(12),MXY(13))
      CALL FMSQR(MXY(1),MXY(12))
      IF (MXY(12)%MP(2) < MEXPOV) THEN
          CALL FMSUB(MXY(13),MXY(12),MXY(11))
      ELSE
          CALL FMEQ(MXY(12),MXY(11))
          IF (MXY(11)%MP(2) /= MUNKNO .AND. MXY(11)%MP(3) /= 0)  &
              MXY(11)%MP(1) = -MXY(11)%MP(1)
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  140 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(11)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL FMEXT2(MXY(11),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMLERC

      SUBROUTINE FMLI(MA,MB)

!  MB = Logarithmic Integral(MA)

!  Integral from 0 to MA of 1 / Ln(t)  dt.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR,X
      INTEGER :: J,KL,KOVUN,KR_RETRY,KRESLT,NDSAVE
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(3)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL FMENT2('FMLI     ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF

      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG-1)
      ENDIF
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)

      IF (MXY(1)%MP(3) == 0) THEN
          CALL FMI2M(0,MXY(2))
      ELSE IF (MXY(1)%MP(2) == MEXPUN .AND. MXY(1)%MP(1) == 1) THEN
          CALL FMST2M('-UNDERFLOW',MXY(2))
          KFLAG = -6
      ELSE
          CALL FMLN(MXY(1),MXY(3))
          CALL FMEI(MXY(3),MXY(2))
      ENDIF

      NAMEST(NCALL) = 'FMLI'

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF

!             Check for too much cancellation.

      KL = KFLAG
      CALL FMM2DP(MXY(1),X)
      IF (KFLAG == 0 .AND. KL == 0) THEN
          IF (X > 1.44 .AND. X < 1.46) THEN
              IF (MXY(2)%MP(2) < 0) THEN
                  IF (KR_RETRY <= 1 .AND. NDIG < 2*NDSAVE+10) THEN
                      KR_RETRY = 2
                      GO TO 110
                  ENDIF
              ENDIF
          ENDIF
      ENDIF
      KFLAG = KL
      CALL FMEXT2(MXY(2),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMLI

      SUBROUTINE FMS(MA,MB)

!  MB = S(MA)    Fresnel Sine Integral.

!  Integral from 0 to MA of Sin(pi*t**2/2) dt.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      DOUBLE PRECISION :: ERR,X,Y
      DOUBLE PRECISION, EXTERNAL :: FMNTERMS
      REAL (KIND(1.0D0)) :: MXSAVE
      INTEGER :: IEXTRA,J,J2,JR,K,KFLAGX,KL,KOVUN,KR_RETRY,KRESLT,KRSAVE,LARGE,NBOT,  &
                 NDGOAL,NDOLD,NDSAV1,NDSAVE,NGOAL,NMETHD,NTERM,NUMTRY,N_ACC

      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(16),MRETRY,MJSUMS(LJSUMS)
      LOGICAL, EXTERNAL :: FMCOMP

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL FMENT2('FMS      ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF

      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      N_ACC = NINT(NDIG*ALOGM2)
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
      MXY(1)%MP(1) = 1
      NUMTRY = 0

!             Check for special cases.

      IF (MXY(1)%MP(3) == 0) THEN
          CALL FMEQ(MXY(1),MXY(11))
          GO TO 180
      ENDIF
      IF (4*MXY(1)%MP(2) < -NDIG) THEN
          CALL FMSQR(MXY(1),MXY(11))
          CALL FMMPY_R2(MXY(1),MXY(11))
          CALL FMPI(MXY(3))
          CALL FMDIVI(MXY(3),6,MXY(4))
          CALL FMMPY_R1(MXY(11),MXY(4))
          GO TO 160
      ENDIF
      CALL FMINT(MXY(1),MXY(8))
      IF (MXY(1)%MP(2) > NDIG .AND. FMCOMP(MXY(1),'==',MXY(8)) .AND.  &
          MXY(1)%MP(2) < MEXPOV) THEN
          CALL FMI2M(1,MXY(8))
          IF (MXY(1)%MP(1) < 0) THEN
              CALL FMDIVI(MXY(8),-2,MXY(11))
          ELSE
              CALL FMDIVI(MXY(8),2,MXY(11))
          ENDIF
          CALL FMPI(MXY(6))
          CALL FMMPY_R1(MXY(6),MXY(1))
          CALL FMDIV_R2(MXY(8),MXY(6))
          JR = KROUND
          IF (KROUND == -1 .AND. MA%MP(1) < 0) KROUND =  2
          IF (KROUND ==  2 .AND. MA%MP(1) < 0) KROUND = -1
          CALL FMSUB_R1(MXY(11),MXY(6))
          KROUND = JR
          GO TO 180
      ENDIF
      IF (MXY(1)%MP(2) > NDIG) THEN
          CALL FMI2M(1,MXY(8))
          IF (MXY(1)%MP(1) < 0) THEN
              CALL FMDIVI(MXY(8),-2,MXY(11))
          ELSE
              CALL FMDIVI(MXY(8),2,MXY(11))
          ENDIF
          GO TO 160
      ENDIF

!             X is a double precision approximation to the input argument to this function.

  120 CALL FMM2DP(MXY(1),X)
      KFLAGX = KFLAG
      IF (KFLAG /= 0 .AND. MXY(1)%MP(2) < 0) THEN
          X = 1.0D0/DPMAX
          IF (MXY(1)%MP(1) < 0) X = -X
          KFLAGX = 0
      ENDIF

!             If MA is large in magnitude, use more guard digits.

      IEXTRA = MIN(MAX(INT(MXY(1)%MP(2)),0) ,INT(2.0+ALOGMX/ALOGMB))
      IF (IEXTRA > 0) THEN
          CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
      ENDIF
      NDIG = NDIG + IEXTRA
      N_ACC = NINT(NDIG*ALOGM2)

!             Determine which method to use.

!             NMETHD = 1 means use the convergent series,
!                    = 2 means use the asymptotic series.

      IF (KFLAGX == 0) THEN
          IF (ABS(X) < SQRT(HUGE(X)/(4*LOG(HUGE(X))))) THEN
              Y = (DPPI*X*X-1)/2
              Y = (2*Y+1.5)*LOG(2*Y+2) - (2*Y+1) - Y*DLOGTW - (Y+0.5)*LOG(Y+1) + Y - Y*LOG(DPPI*X*X)
              IF (Y <= -(NDIG+1)*DLOGMB) THEN
                  NMETHD = 2
              ELSE
                  NMETHD = 1
              ENDIF
          ELSE
              NMETHD = 2
          ENDIF
      ELSE
          IF (MXY(1)%MP(2) <= 0) THEN
              NMETHD = 1
          ELSE
              NMETHD = 2
          ENDIF
      ENDIF

      IF (NMETHD == 2) GO TO 150

!             Method 1.  Use x times the series (-1)**n*(pi*x*x/2)**(2*n+1)/((4*n+3)*(2*n+1)!).

!             MXY(6) is the current term
!             MXY(9) is pi*x*x/2

      IEXTRA = 0
      IF (KFLAGX == 0) THEN
          IEXTRA = MAX(0.0D0,(0.096*X*X + 0.033*ABS(X) - 0.5)*LOG(1.0E7)/ALOGMB)*1.02
          IF (IEXTRA > 0) THEN
              CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
          ENDIF
      ENDIF
      NDIG = NDIG + IEXTRA
      IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG-1)
      ENDIF
      N_ACC = NINT(NDIG*ALOGM2)

      IF (KFLAGX == 0) THEN
          J2 = INT(0.68*SQRT(FMNTERMS(3.14159D0*X*X/2,2,0,0,1)) - 1.6)
          J2 = MAX(2,MIN(J2+MOD(J2,2),LJSUMS))
      ELSE
          J2 = 2
      ENDIF
      NDSAV1 = NDIG

!             Split into J2 concurrent sums.

      CALL FMSQR(MXY(1),MXY(4))
      CALL FMCSDIVI(MXY(4),2,MXY(9))
      CALL FMPI(MXY(3))
      CALL FMCSMPY_R1(MXY(9),MXY(3))
      CALL FMI2M(1,MXY(6))
      CALL FMCSDIVI(MXY(6),3,MJSUMS(1))
      NTERM = 0
      DO J = 2, J2
         NTERM = NTERM + 2
         NBOT = NTERM*(NTERM+1)
         LARGE = INT(INTMAX/NTERM)
         IF (NTERM > LARGE .OR. NBOT > MXBASE) THEN
             CALL FMCSDIVI_R1(MXY(6),NTERM)
             CALL FMCSDIVI_R1(MXY(6),NTERM+1)
         ELSE
             CALL FMCSDIVI_R1(MXY(6),NBOT)
         ENDIF
         CALL FMCSDIVI(MXY(6),2*NTERM+3,MJSUMS(J))
      ENDDO
      IF (MXY(1)%MP(2) < -NDIG) GO TO 140
      CALL FMIPWR(MXY(9),2*J2,MXY(7))

  130 CALL FMCSMPY_R1(MXY(6),MXY(7))
      DO J = 1, J2
         NTERM = NTERM + 2
         LARGE = INT(INTMAX/NTERM)
         IF (NTERM > LARGE .OR. NTERM > MXBASE/(NTERM-1)) THEN
             CALL FMCSDIVI_R1(MXY(6),NTERM)
             CALL FMCSDIVI_R1(MXY(6),NTERM+1)
         ELSE
             NBOT = NTERM*(NTERM+1)
             CALL FMCSDIVI_R1(MXY(6),NBOT)
         ENDIF
         CALL FMCSDIVI(MXY(6),2*NTERM+3,MXY(2))
         NDIG = NDSAV1
         CALL FMCSADDNN_R1(MJSUMS(J),MXY(2))
         IF (KFLAG /= 0) GO TO 140
         NDIG = NDSAV1 - INT(MJSUMS(J)%MP(2)-MXY(6)%MP(2))
         NDIG = MIN(NDSAV1,NDIG)
         IF (NDIG < NGRD22) NDIG = NGRD22
      ENDDO
      GO TO 130

!             Put the J2 separate sums back together.

  140 KFLAG = 0
      CALL FMCSNSUMS(J2,MJSUMS)
      CALL FMSQR(MXY(9),MXY(8))
      MXY(8)%MP(1) = -1
      IF (MJSUMS(1)%MP(1) > 0) THEN
          CALL FMEQ(MJSUMS(1),MXY(15))
          CALL FMI2M(0,MXY(16))
      ELSE
          CALL FMEQ(MJSUMS(1),MXY(16))
          CALL FMI2M(0,MXY(15))
      ENDIF
      CALL FMEQ(MXY(8),MXY(12))
      DO J = 1, J2-1
         CALL FMMPY(MXY(12),MJSUMS(J+1),MXY(3))
         IF (MXY(3)%MP(1) > 0) THEN
             CALL FMADD_R1(MXY(15),MXY(3))
         ELSE
             CALL FMADD_R1(MXY(16),MXY(3))
         ENDIF
         CALL FMMPY_R1(MXY(12),MXY(8))
      ENDDO
      CALL FMADD(MXY(15),MXY(16),MXY(11))
      CALL FMCANCEL(MXY(15),MXY(16),MXY(11),K)
      N_ACC = N_ACC - K
      CALL FMCSMPY_R1(MXY(11),MXY(1))
      CALL FMCSMPY_R1(MXY(11),MXY(9))

      GO TO 160

!             Method 2.  Use the two N!/X**N asymptotic series for f(x) and g(x).
!                        Then S(x) = 0.5 - f(x)*cos(pi*x*x/2) - g(x)*sin(pi*x*x/2).

  150 IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG-1)
      ENDIF
      N_ACC = NINT(NDIG*ALOGM2)
      CALL FMFGFI(MXY(1),MXY(12),MXY(13))
      CALL FMSQR(MXY(1),MXY(4))
      KRSAVE = KRAD
      KRAD = 0
      CALL FMMPYI(MXY(4),90,MXY(7))
      CALL FMCSSN(MXY(7),MXY(14),MXY(10))
      KRAD = KRSAVE
      CALL FMMPY(MXY(12),MXY(14),MXY(11))
      CALL FMMPY(MXY(13),MXY(10),MXY(7))
      CALL FMI2M(1,MXY(4))
      CALL FMDIVI(MXY(4),2,MXY(5))
      CALL FMSUB(MXY(5),MXY(11),MXY(6))
      CALL FMSUB(MXY(6),MXY(7),MXY(11))

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  160 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(11)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF

!             Check for too much cancellation.

      IF (NCALL >= 1) THEN
          NGOAL = INT(REAL(NDSAVE)*ALOGM2) + 17
      ELSE
          NGOAL = INT(-MXEXP2)
      ENDIF
      IF (N_ACC <= NGOAL) THEN
          IF (NUMTRY > 0) THEN
              NDGOAL = INT(REAL(NGOAL)/ALOGM2 + 1.0)
              DO J = 1, NDGOAL+1
                 IF (MRETRY%MP(J+1) /= MXY(11)%MP(J+1)) GO TO 170
              ENDDO
              GO TO 180
          ENDIF
  170     IEXTRA = INT(REAL(NGOAL-N_ACC)/ALOGM2 + 23.03/ALOGMB) + 1
          NDOLD = NDIG
          NDIG = MAX(NDIG+IEXTRA,NDIG+NGRD52+2)
          N_ACC = NINT(NDIG*ALOGM2)
          CALL FMEQU_R1(MXY(1),NDSAVE,NDIG)
          NUMTRY = NUMTRY + 1
          CALL FMEQU(MXY(11),MRETRY,NDOLD,NDIG)
          GO TO 120
      ENDIF

  180 IF (MA%MP(1) == -1) THEN
          IF(MXY(11)%MP(3) /= 0 .AND. MXY(11)%MP(2) /= MUNKNO)  &
             MXY(11)%MP(1) =-1
      ENDIF
      CALL FMEXT2(MXY(11),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMS

      SUBROUTINE FMSHI(MA,MB)

!  MB = Hyperbolic Sine Integral(MA)

!  Integral from 0 to MA of sinh(t) / t  dt.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      DOUBLE PRECISION :: ERR,X,Y
      DOUBLE PRECISION, EXTERNAL :: FMNTERMS
      REAL (KIND(1.0D0)) :: MXSAVE
      INTEGER :: IEXTRA,J,J2,K,KFLAGX,KL,KOVUN,KR_RETRY,KRESLT,KRSAVE,K_RETURN_CODE,LARGE,  &
                 NBOT,NDSAV1,NDSAVE,NMETHD,NTERM
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(12),MJSUMS(LJSUMS)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      K_RETURN_CODE = 0
      K = 0
      NCALL = NCALL + 1

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1) THEN
          J = NTRACE
          NTRACE = 0
          KL = KWARN
          KWARN = 0
          CALL FMSQR(MA,MXY(1))
          CALL FMDIVI(MXY(1),18,MXY(2))
          IF (MXY(2)%MP(2) < -NDIG) K = 1
          NTRACE = J
          KWARN = KL
      ENDIF
      IF (KROUND /= 1 .AND. K == 1) THEN
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          KRSAVE = KROUND
          KROUND = 1
          NDSAVE = NDIG
          NDIG = NDIG + NGRD52
          CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
          CALL FMSQR(MXY(1),MXY(2))
          CALL FMMPY(MXY(1),MXY(2),MXY(3))
          CALL FMDIVI(MXY(3),18,MXY(5))
          CALL FMEQ(MXY(1),MXY(4))
          IF (MXY(4)%MP(2) - MXY(5)%MP(2) > NDIG .AND.  &
              MXY(4)%MP(2) > MEXPUN) THEN
              CALL FMEQU(MXY(4),MXY(10),NDIG,NDSAVE)
              CALL FMEQU(MXY(4),MXY(6),NDIG,NDSAVE)
              CALL FMEQU(MXY(6),MXY(8),NDSAVE,NDIG)
              CALL FMSUB(MXY(4),MXY(8),MXY(6))
              IF (MXY(6)%MP(3) == 0) THEN
                  CALL FMEQU(MXY(4),MXY(6),NDIG,NDSAVE)
                  CALL FMEQU(MXY(5),MXY(7),NDIG,NDSAVE)
                  NDIG = NDSAVE
                  KROUND = KRSAVE
                  CALL FMADD(MXY(6),MXY(7),MXY(9))
                  IF (MXY(9)%MP(2) >= MEXPOV) THEN
                      IF (MXY(6)%MP(1) > 0) THEN
                          IF (MXY(7)%MP(1) < 0 .AND. (KROUND == -1 .OR. KROUND == 0)) THEN
                              CALL FMEQ(MXY(6),MXY(1))
                              MXY(1)%MP(2) = 0
                              CALL FMULP(MXY(1),MXY(2))
                              CALL FMSUB(MXY(1),MXY(2),MXY(3))
                              MXY(3)%MP(2) = MXY(6)%MP(2) + MXY(3)%MP(2)
                              CALL FMEQ(MXY(3),MXY(9))
                          ELSE IF (MXY(7)%MP(1) > 0 .AND. KROUND == 2) THEN
                              CALL FMEQ(MXY(6),MXY(1))
                              MXY(1)%MP(2) = 0
                              CALL FMULP(MXY(1),MXY(2))
                              CALL FMADD(MXY(1),MXY(2),MXY(3))
                              MXY(3)%MP(2) = MXY(6)%MP(2) + MXY(3)%MP(2)
                              CALL FMEQ(MXY(3),MXY(9))
                          ELSE
                              KROUND = KRSAVE
                              CALL FMEQ(MXY(10),MXY(9))
                          ENDIF
                      ELSE
                          IF (MXY(7)%MP(1) < 0 .AND. KROUND == -1) THEN
                              CALL FMEQ(MXY(6),MXY(1))
                              MXY(1)%MP(2) = 0
                              CALL FMULP(MXY(1),MXY(2))
                              CALL FMADD(MXY(1),MXY(2),MXY(3))
                              MXY(3)%MP(2) = MXY(6)%MP(2) + MXY(3)%MP(2)
                              CALL FMEQ(MXY(3),MXY(9))
                          ELSE IF (MXY(7)%MP(1) > 0 .AND. (KROUND == 2 .OR. KROUND == 0)) THEN
                              CALL FMEQ(MXY(6),MXY(1))
                              MXY(1)%MP(2) = 0
                              CALL FMULP(MXY(1),MXY(2))
                              CALL FMSUB(MXY(1),MXY(2),MXY(3))
                              MXY(3)%MP(2) = MXY(6)%MP(2) + MXY(3)%MP(2)
                              CALL FMEQ(MXY(3),MXY(9))
                          ELSE
                              KROUND = KRSAVE
                              CALL FMEQ(MXY(10),MXY(9))
                          ENDIF
                      ENDIF
                  ENDIF
                  CALL FMEQU(MXY(9),MB,NDIG,NDSAVE)
              ELSE
                  KROUND = KRSAVE
                  CALL FMEQU(MXY(4),MB,NDIG,NDSAVE)
              ENDIF
              K_RETURN_CODE = 1
          ELSE IF (MXY(4)%MP(2) == MEXPUN) THEN
              IF (KRSAVE == 2 .AND. MA%MP(1) == 1) THEN
                  CALL FMTINY(MXY(9))
                  CALL FMEQU(MXY(9),MB,NDIG,NDSAVE)
                  K_RETURN_CODE = 1
              ENDIF
              IF (KRSAVE == -1 .AND. MA%MP(1) == -1) THEN
                  CALL FMTINY(MXY(9))
                  MXY(9)%MP(1) = -1
                  CALL FMEQU(MXY(9),MB,NDIG,NDSAVE)
                  K_RETURN_CODE = 1
              ENDIF
          ENDIF
          KFLAG = 0
          NTRACE = J
          KWARN = K
          NDIG = NDSAVE
          KROUND = KRSAVE
          IF (K_RETURN_CODE == 1) THEN
              IF (NTRACE /= 0) THEN
                  NAMEST(NCALL) = 'FMSHI'
                  CALL FMNTR(2,MA,MA,1,1)
                  CALL FMNTR(1,MB,MB,1,1)
              ENDIF
          ENDIF
      ENDIF
      NCALL = NCALL - 1
      IF (K_RETURN_CODE == 1) RETURN

      CALL FMENT2('FMSHI    ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF

      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
      MXY(1)%MP(1) = 1

!             X is a double precision approximation to the input argument to this function.

      CALL FMM2DP(MXY(1),X)
      KFLAGX = KFLAG
      IF (KFLAG /= 0 .AND. MXY(1)%MP(2) < 0) THEN
          X = 1.0D0/DPMAX
          IF (MXY(1)%MP(1) < 0) X = -X
          KFLAGX = 0
      ENDIF

!             Check for special cases.

      IF (MXY(1)%MP(3) == 0) THEN
          CALL FMI2M(0,MXY(8))
          GO TO 160
      ELSE IF (MXY(1)%MP(2) == MEXPUN) THEN
          CALL FMEQ(MXY(1),MXY(8))
          GO TO 160
      ELSE IF (MXY(1)%MP(2) == MEXPOV) THEN
          CALL FMST2M('OVERFLOW',MXY(8))
          KFLAG = -5
          GO TO 160
      ENDIF

!             If MA is large in magnitude, use more guard digits.

      IEXTRA = MIN(MAX(INT(MXY(1)%MP(2)),0) ,INT(2.0+ALOGMX/ALOGMB))
      IF (IEXTRA > 0) THEN
          CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
      ENDIF
      NDIG = NDIG + IEXTRA

!             Determine which method to use.

!             NMETHD = 1 means use the convergent series,
!                    = 2 means use the asymptotic series.

      NMETHD = 1
      IF (KFLAGX /= 0) THEN
          IF (MXY(1)%MP(2) <= 0) THEN
              NMETHD = 1
          ELSE
              NMETHD = 2
          ENDIF
      ELSE
          Y = (NDIG+5)*DLOGMB
          IF (ABS(X) > Y+(DLOGTP+LOG(Y))/2.0D0) NMETHD = 2
      ENDIF

      IF (NMETHD == 2) GO TO 140

!             Method 1.  Use the X**(2*N+1)/((2*N+1)*(2*N+1)!) series.

      IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG-1)
      ENDIF
      IF (KFLAGX == 0) THEN
          J2 = INT(0.62*SQRT(FMNTERMS(X,2,0,0,1)) - 1.3)
          J2 = MAX(1,MIN(LJSUMS,J2))
      ELSE
          J2 = 2
      ENDIF
      NDSAV1 = NDIG

!             Split into J2 concurrent sums.

      CALL FMEQ(MXY(1),MXY(7))
      CALL FMEQ(MXY(1),MJSUMS(1))
      NTERM = 1
      DO J = 2, J2
         NTERM = NTERM + 2
         NBOT = NTERM*(NTERM-1)
         LARGE = INT(INTMAX/NTERM)
         IF (NTERM > LARGE .OR. NBOT > MXBASE) THEN
             CALL FMCSDIVI_R1(MXY(7),NTERM)
             CALL FMCSDIVI_R1(MXY(7),NTERM-1)
         ELSE
             CALL FMCSDIVI_R1(MXY(7),NBOT)
         ENDIF
         CALL FMCSDIVI(MXY(7),NTERM,MJSUMS(J))
      ENDDO
      IF (MXY(1)%MP(2) < -NDIG) GO TO 130
      CALL FMIPWR(MXY(1),2*J2,MXY(5))

  120 CALL FMCSMPY_R1(MXY(7),MXY(5))
      DO J = 1, J2
         NTERM = NTERM + 2
         LARGE = INT(INTMAX/NTERM)
         IF (NTERM > LARGE .OR. NTERM > MXBASE/(NTERM-1)) THEN
             CALL FMCSDIVI_R1(MXY(7),NTERM)
             CALL FMCSDIVI_R1(MXY(7),NTERM-1)
         ELSE
             NBOT = NTERM*(NTERM-1)
             CALL FMCSDIVI_R1(MXY(7),NBOT)
         ENDIF
         CALL FMCSDIVI(MXY(7),NTERM,MXY(4))
         NDIG = NDSAV1
         CALL FMCSADDNN_R1(MJSUMS(J),MXY(4))
         IF (KFLAG /= 0) GO TO 130
         NDIG = NDSAV1 - INT(MJSUMS(J)%MP(2)-MXY(4)%MP(2))
         NDIG = MIN(NDSAV1,NDIG)
         IF (NDIG < NGRD22) NDIG = NGRD22
      ENDDO
      GO TO 120

!             Put the J2 separate sums back together.

  130 KFLAG = 0
      CALL FMCSNSUMS(J2,MJSUMS)
      CALL FMEQ(MJSUMS(J2),MXY(8))
      IF (J2 >= 2) THEN
          CALL FMSQR(MXY(1),MXY(6))
          DO J = 2, J2
             CALL FMCSMPY_R1(MXY(8),MXY(6))
             CALL FMADD_R1(MXY(8),MJSUMS(J2-J+1))
          ENDDO
      ENDIF

      GO TO 150

!             Method 2.  Use the two N!/X**N asymptotic series for f(x) and g(x).
!                        Then Shi(x) = f(x)*cosh(x) + g(x)*sinh(x).

  140 IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG-2)
      ENDIF
      CALL FMFHGH(MXY(1),MXY(9),MXY(10))
      CALL FMCHSH(MXY(1),MXY(11),MXY(12))
      IF (MXY(11)%MP(2) < MEXPOV) THEN
          CALL FMMPY(MXY(9),MXY(11),MXY(2))
          CALL FMMPY(MXY(10),MXY(12),MXY(3))
          CALL FMADD(MXY(2),MXY(3),MXY(8))
      ELSE
          CALL FMADD(MXY(9),MXY(10),MXY(5))
          CALL FMDIVI_R1(MXY(5),2)
          CALL FMLN(MXY(5),MXY(8))
          CALL FMADD(MXY(1),MXY(8),MXY(5))
          CALL FMEXP(MXY(5),MXY(8))
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  150 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(8)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF

  160 IF (MA%MP(1) < 0 .AND. MXY(8)%MP(2) /= MUNKNO .AND.  &
          MXY(8)%MP(3) /= 0) THEN
          MXY(8)%MP(1) = -MXY(8)%MP(1)
      ENDIF
      CALL FMEXT2(MXY(8),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMSHI

      SUBROUTINE FMSI(MA,MB)

!  MB = Sine Integral(MA)

!  Integral from 0 to MA of sin(t) / t  dt.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      DOUBLE PRECISION :: ERR,X,Y
      DOUBLE PRECISION, EXTERNAL :: FMNTERMS
      REAL (KIND(1.0D0)) :: MXSAVE
      INTEGER :: IEXTRA,J,J2,K,KFLAGX,KL,KOVUN,KR_RETRY,KRESLT,KRSAVE,K_RETURN_CODE,LARGE,  &
                 NBOT,NDGOAL,NDOLD,NDSAV1,NDSAVE,NGOAL,NMETHD,NTERM,NUMTRY,N_ACC
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(13),MRETRY,MJSUMS(LJSUMS)

      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      K_RETURN_CODE = 0
      K = 0
      NCALL = NCALL + 1

!             Rounding for special cases in non-standard rounding modes (KROUND = -1, 0, or 2).

      IF (KROUND /= 1) THEN
          J = NTRACE
          NTRACE = 0
          KL = KWARN
          KWARN = 0
          CALL FMSQR(MA,MXY(1))
          CALL FMDIVI(MXY(1),18,MXY(2))
          IF (MXY(2)%MP(2) < -NDIG) K = 1
          NTRACE = J
          KWARN = KL
      ENDIF
      IF (KROUND /= 1 .AND. K == 1) THEN
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          KRSAVE = KROUND
          KROUND = 1
          NDSAVE = NDIG
          NDIG = NDIG + NGRD52
          N_ACC = NINT(NDIG*ALOGM2)
          CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
          CALL FMSQR(MXY(1),MXY(2))
          CALL FMMPY(MXY(1),MXY(2),MXY(3))
          CALL FMDIVI(MXY(3),-18,MXY(5))
          CALL FMEQ(MXY(1),MXY(4))
          IF (MXY(4)%MP(2) - MXY(5)%MP(2) > NDIG .AND.  &
              MXY(4)%MP(2) > MEXPUN) THEN
              CALL FMEQU(MXY(4),MXY(10),NDIG,NDSAVE)
              CALL FMEQU(MXY(4),MXY(6),NDIG,NDSAVE)
              CALL FMEQU(MXY(6),MXY(8),NDSAVE,NDIG)
              CALL FMSUB(MXY(4),MXY(8),MXY(6))
              IF (MXY(6)%MP(3) == 0) THEN
                  CALL FMEQU(MXY(4),MXY(6),NDIG,NDSAVE)
                  CALL FMEQU(MXY(5),MXY(7),NDIG,NDSAVE)
                  NDIG = NDSAVE
                  KROUND = KRSAVE
                  CALL FMADD(MXY(6),MXY(7),MB)
                  IF (MB%MP(2) >= MEXPOV) THEN
                      IF (MXY(6)%MP(1) > 0) THEN
                          IF (MXY(7)%MP(1) < 0 .AND. (KROUND == -1 .OR. KROUND == 0)) THEN
                              CALL FMEQ(MXY(6),MXY(1))
                              MXY(1)%MP(2) = 0
                              CALL FMULP(MXY(1),MXY(2))
                              CALL FMSUB(MXY(1),MXY(2),MXY(3))
                              MXY(3)%MP(2) = MXY(6)%MP(2) + MXY(3)%MP(2)
                              CALL FMEQ(MXY(3),MB)
                          ELSE IF (MXY(7)%MP(1) > 0 .AND. KROUND == 2) THEN
                              CALL FMEQ(MXY(6),MXY(1))
                              MXY(1)%MP(2) = 0
                              CALL FMULP(MXY(1),MXY(2))
                              CALL FMADD(MXY(1),MXY(2),MXY(3))
                              MXY(3)%MP(2) = MXY(6)%MP(2) + MXY(3)%MP(2)
                              CALL FMEQ(MXY(3),MB)
                          ELSE
                              KROUND = KRSAVE
                              CALL FMEQ(MXY(10),MB)
                          ENDIF
                      ELSE
                          IF (MXY(7)%MP(1) < 0 .AND. KROUND == -1) THEN
                              CALL FMEQ(MXY(6),MXY(1))
                              MXY(1)%MP(2) = 0
                              CALL FMULP(MXY(1),MXY(2))
                              CALL FMADD(MXY(1),MXY(2),MXY(3))
                              MXY(3)%MP(2) = MXY(6)%MP(2) + MXY(3)%MP(2)
                              CALL FMEQ(MXY(3),MB)
                          ELSE IF (MXY(7)%MP(1) > 0 .AND. (KROUND == 2 .OR. KROUND == 0)) THEN
                              CALL FMEQ(MXY(6),MXY(1))
                              MXY(1)%MP(2) = 0
                              CALL FMULP(MXY(1),MXY(2))
                              CALL FMSUB(MXY(1),MXY(2),MXY(3))
                              MXY(3)%MP(2) = MXY(6)%MP(2) + MXY(3)%MP(2)
                              CALL FMEQ(MXY(3),MB)
                          ELSE
                              KROUND = KRSAVE
                              CALL FMEQ(MXY(10),MB)
                          ENDIF
                      ENDIF
                  ENDIF
              ELSE
                  KROUND = KRSAVE
                  CALL FMEQU(MXY(4),MB,NDIG,NDSAVE)
              ENDIF
              K_RETURN_CODE = 1
          ENDIF
          KFLAG = 0
          NTRACE = J
          KWARN = K
          NDIG = NDSAVE
          KROUND = KRSAVE
          IF (K_RETURN_CODE == 1) THEN
              IF (NTRACE /= 0) THEN
                  NAMEST(NCALL) = 'FMSI'
                  CALL FMNTR(2,MA,MA,1,1)
              ENDIF
              IF (MB%MP(2) == MUNKNO .AND. MA%MP(2) /= MUNKNO) THEN
                  NAMEST(NCALL) = 'FMSI'
                  KFLAG = -4
                  CALL FMWRN2
              ELSE IF (ABS(MB%MP(2)) == MEXPOV .AND. ABS(MA%MP(2)) < MEXPOV) THEN
                  NAMEST(NCALL) = 'FMSI'
                  IF (MB%MP(2) == MEXPOV) KFLAG = -5
                  IF (MB%MP(2) == MEXPUN) KFLAG = -6
                  CALL FMWRN2
              ENDIF
              IF (NTRACE /= 0) THEN
                  NAMEST(NCALL) = 'FMSI'
                  CALL FMNTR(1,MB,MB,1,1)
              ENDIF
          ENDIF
      ENDIF
      NCALL = NCALL - 1
      IF (K_RETURN_CODE == 1) RETURN

      CALL FMENT2('FMSI     ',MA,MA,1,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF

      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      N_ACC = NINT(NDIG*ALOGM2)
      CALL FMEQU(MA,MXY(1),NDSAVE,NDIG)
      MXY(1)%MP(1) = 1
      NUMTRY = 0

!             X is a double precision approximation to the input argument to this function.

  120 CALL FMM2DP(MXY(1),X)
      KFLAGX = KFLAG
      IF (KFLAG /= 0 .AND. MXY(1)%MP(2) < 0) THEN
          X = 1.0D0/DPMAX
          IF (MXY(1)%MP(1) < 0) X = -X
          KFLAGX = 0
      ENDIF

!             Check for special cases.

      IF (MXY(1)%MP(3) == 0) THEN
          CALL FMI2M(0,MXY(8))
          GO TO 180
      ELSE IF (MXY(1)%MP(2) == MEXPUN) THEN
          CALL FMEQ(MXY(1),MXY(8))
          GO TO 180
      ELSE IF (MXY(1)%MP(2) > NDIG) THEN
          CALL FMPI(MXY(8))
          CALL FMDIVI_R1(MXY(8),2)
          GO TO 160
      ENDIF

!             If MA is large in magnitude, use more guard digits.

      IEXTRA = MIN(MAX(INT(MXY(1)%MP(2)),0) ,INT(2.0+ALOGMX/ALOGMB))
      IF (IEXTRA > 0) THEN
          CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
      ENDIF
      NDIG = NDIG + IEXTRA
      N_ACC = NINT(NDIG*ALOGM2)

!             Determine which method to use.

!             NMETHD = 1 means use the convergent series,
!                    = 2 means use the asymptotic series.

      NMETHD = 1
      IF (KFLAGX /= 0) THEN
          IF (MXY(1)%MP(2) <= 0) THEN
              NMETHD = 1
          ELSE
              NMETHD = 2
          ENDIF
      ELSE
          Y = (NDIG+5)*DLOGMB
          IF (ABS(X) > Y+(DLOGTP+LOG(Y))/2.0D0) NMETHD = 2
      ENDIF

      IF (NMETHD == 2) GO TO 150

!             Method 1.  Use the (-1)**N*X**(2*N+1)/((2*N+1)*(2*N+1)!) series.

      IEXTRA = 0
      IF (KFLAGX == 0) THEN
          Y = NINT(ABS(X)/2)
          Y = 2*Y*LOG(ABS(X)+1.0E-9) - LOG(2*Y+1.0E-9) - (2*Y+0.5)*LOG(2*Y+1) + 2*Y
          Y = 1.03*Y/DLOGMB - 7/ALOGMT
          IEXTRA = MAX(0,INT(Y+1))
          IF (ABS(X) < 5 .AND. KR_RETRY <= 0) THEN
              NDIG = NDIG - 2
              IEXTRA = 0
          ENDIF
      ENDIF
      IF (IEXTRA > 0) THEN
          CALL FMEQU_R1(MXY(1),NDIG,NDIG+IEXTRA)
      ENDIF
      NDIG = NDIG + IEXTRA
      N_ACC = NINT(NDIG*ALOGM2)

      IF (KFLAGX == 0) THEN
          J2 = INT(0.64*SQRT(FMNTERMS(X,2,0,0,1)) - 1.4)
          J2 = MAX(2,MIN(J2+MOD(J2,2),LJSUMS))
      ELSE
          J2 = 2
      ENDIF
      NDSAV1 = NDIG

!             Split into J2 concurrent sums.

      CALL FMEQ(MXY(1),MXY(7))
      CALL FMEQ(MXY(1),MJSUMS(1))
      NTERM = 1
      DO J = 2, J2
         NTERM = NTERM + 2
         LARGE = INT(INTMAX/NTERM)
         NBOT = NTERM*(NTERM-1)
         IF (NTERM > LARGE .OR. NBOT > MXBASE) THEN
             CALL FMCSDIVI_R1(MXY(7),NTERM)
             NBOT = NTERM - 1
             CALL FMCSDIVI_R1(MXY(7),NBOT)
         ELSE
             CALL FMCSDIVI_R1(MXY(7),NBOT)
         ENDIF
         CALL FMCSDIVI(MXY(7),NTERM,MJSUMS(J))
      ENDDO
      IF (MXY(1)%MP(2) < -NDIG) GO TO 140
      CALL FMIPWR(MXY(1),2*J2,MXY(5))

  130 CALL FMCSMPY_R1(MXY(7),MXY(5))
      DO J = 1, J2
         NTERM = NTERM + 2
         LARGE = INT(INTMAX/NTERM)
         IF (NTERM > LARGE .OR. NTERM > MXBASE/(NTERM-1)) THEN
             CALL FMCSDIVI_R1(MXY(7),NTERM)
             NBOT = NTERM - 1
             CALL FMCSDIVI_R1(MXY(7),NBOT)
         ELSE
             NBOT = NTERM*(NTERM-1)
             CALL FMCSDIVI_R1(MXY(7),NBOT)
         ENDIF
         CALL FMCSDIVI(MXY(7),NTERM,MXY(4))
         NDIG = NDSAV1
         CALL FMCSADDNN_R1(MJSUMS(J),MXY(4))
         IF (KFLAG /= 0) GO TO 140
         NDIG = NDSAV1 - INT(MJSUMS(J)%MP(2)-MXY(4)%MP(2))
         NDIG = MIN(NDSAV1,NDIG)
         IF (NDIG < NGRD22) NDIG = NGRD22
      ENDDO
      GO TO 130

!             Put the J2 separate sums back together.

  140 KFLAG = 0
      CALL FMCSNSUMS(J2,MJSUMS)
      CALL FMSQR(MXY(1),MXY(5))
      MXY(5)%MP(1) = -1
      CALL FMSQR(MXY(1),MXY(5))
      MXY(5)%MP(1) = -1
      IF (MJSUMS(1)%MP(1) > 0) THEN
          CALL FMEQ(MJSUMS(1),MXY(12))
          CALL FMI2M(0,MXY(13))
      ELSE
          CALL FMEQ(MJSUMS(1),MXY(13))
          CALL FMI2M(0,MXY(12))
      ENDIF
      CALL FMEQ(MXY(5),MXY(7))
      DO J = 1, J2-1
         CALL FMMPY(MXY(7),MJSUMS(J+1),MXY(3))
         IF (MXY(3)%MP(1) > 0) THEN
             CALL FMADD_R1(MXY(12),MXY(3))
         ELSE
             CALL FMADD_R1(MXY(13),MXY(3))
         ENDIF
         CALL FMMPY_R1(MXY(7),MXY(5))
      ENDDO
      CALL FMADD(MXY(12),MXY(13),MXY(8))
      CALL FMCANCEL(MXY(12),MXY(13),MXY(8),K)
      N_ACC = N_ACC - K

      GO TO 160

!             Method 2.  Use the two N!/X**N asymptotic series for f(x) and g(x).
!                        Then Si(x) = pi/2 - f(x)*cos(x) - g(x)*sin(x).

  150 CALL FMFXGX(MXY(1),MXY(10),MXY(11))
      N_ACC = NINT(NDIG*ALOGM2)
      KRSAVE = KRAD
      KRAD = 1
      CALL FMCSSN(MXY(1),MXY(12),MXY(13))
      KRAD = KRSAVE
      CALL FMPI(MXY(9))
      IF (MXY(12)%MP(2) == MUNKNO .OR. MXY(13)%MP(2) == MUNKNO) THEN
          CALL FMMPYI(MXY(9),2,MXY(8))
          CALL FMDIV(MXY(1),MXY(8),MXY(3))
          CALL FMINT(MXY(3),MXY(2))
          CALL FMMPY(MXY(2),MXY(8),MXY(3))
          CALL FMSUB(MXY(1),MXY(3),MXY(8))
          KRSAVE = KRAD
          KRAD = 1
          CALL FMCSSN(MXY(8),MXY(12),MXY(13))
          KRAD = KRSAVE
      ENDIF
      CALL FMDIVI(MXY(9),2,MXY(2))
      CALL FMMPY(MXY(10),MXY(12),MXY(3))
      CALL FMSUB_R1(MXY(2),MXY(3))
      CALL FMMPY(MXY(11),MXY(13),MXY(3))
      CALL FMSUB(MXY(2),MXY(3),MXY(8))

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  160 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(8)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF

!             Check for too much cancellation.

      IF (NCALL >= 1) THEN
          NGOAL = INT(REAL(NDSAVE)*ALOGM2) + 17
      ELSE
          NGOAL = INT(-MXEXP2)
      ENDIF
      IF (N_ACC <= NGOAL) THEN
          IF (NUMTRY > 0) THEN
              NDGOAL = INT(REAL(NGOAL)/ALOGM2 + 1.0)
              DO J = 1, NDGOAL+1
                 IF (MRETRY%MP(J+1) /= MXY(8)%MP(J+1)) GO TO 170
              ENDDO
              GO TO 180
          ENDIF
  170     IEXTRA = INT(REAL(NGOAL-N_ACC)/ALOGM2 + 23.03/ALOGMB) + 1
          NDOLD = NDIG
          NDIG = MAX(NDIG+IEXTRA,NDIG+NGRD52+2)
          N_ACC = NINT(NDIG*ALOGM2)
          CALL FMEQU_R1(MXY(1),NDSAVE,NDIG)
          NUMTRY = NUMTRY + 1
          CALL FMEQU(MXY(8),MRETRY,NDOLD,NDIG)
          GO TO 120
      ENDIF

  180 IF (MA%MP(1) < 0 .AND. MXY(8)%MP(2) /= MUNKNO .AND.  &
          MXY(8)%MP(3) /= 0) THEN
          MXY(8)%MP(1) = -MXY(8)%MP(1)
      ENDIF
      CALL FMEXT2(MXY(8),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE FMSI


!  Here are the routines which work with packed FM numbers.
!  All names are the same as unpacked versions with 'FM' replaced by 'FP'.

!  This packed format is not available when using the FM, IM, or ZM derived types.

      SUBROUTINE FPBESJ(IVAL,MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTEGER :: IVAL
      INTENT (IN) :: MA,IVAL
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMBESJ(IVAL,MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPBESJ

      SUBROUTINE FPBESY(IVAL,MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTEGER :: IVAL
      INTENT (IN) :: MA,IVAL
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMBESY(IVAL,MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPBESY

      SUBROUTINE FPC(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMC(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPC

      SUBROUTINE FPCHI(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMCHI(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPCHI

      SUBROUTINE FPCI(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMCI(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPCI

      SUBROUTINE FPEI(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMEI(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPEI

      SUBROUTINE FPEN(IVAL,MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTEGER :: IVAL
      INTENT (IN) :: MA,IVAL
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMEN(IVAL,MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPEN

      SUBROUTINE FPERF(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMERF(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPERF

      SUBROUTINE FPERFC(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMERFC(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPERFC

      SUBROUTINE FPERFCS(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMERFCS(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPERFCS

      SUBROUTINE FPLERC(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMLERC(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPLERC

      SUBROUTINE FPLI(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMLI(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPLI

      SUBROUTINE FPS(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMS(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPS

      SUBROUTINE FPSHI(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMSHI(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPSHI

      SUBROUTINE FPSI(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL FMSI(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE FPSI

