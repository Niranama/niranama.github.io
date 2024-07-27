
!  The ZM routines perform complex multiple-precision arithmetic.

      SUBROUTINE ZMSET(NPREC)

!  Set precision to at least NPREC significant digits for using ZM arithmetic.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: NPREC
      INTENT (IN) :: NPREC

!             Set JFORMZ to ' 1.23 + 4.56 i ' format.

      JFORMZ = 1

!             Set JPRNTZ to print real and imaginary parts on one line whenever possible.

      JPRNTZ = 1

!             Use FMSET to initialize the other variables.

      CALL FMSET(NPREC)

      RETURN
      END SUBROUTINE ZMSET

      SUBROUTINE ZMABS(MA,MBFM)

!  MBFM = ABS(MA)

!  Complex absolute value.  The result is a real FM number.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MBFM
      REAL (KIND(1.0D0)) :: MXEXP1,MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,KL,KOVUN,KR_RETRY,KRESLT,NDSAVE,NTRSAV
      INTENT (IN) :: MA
      INTENT (INOUT) :: MBFM
      TYPE(MULTI) :: MXY(3),MZ01(2),MZ02(2)

      IF (.NOT. ALLOCATED(MBFM%MP)) THEN
          ALLOCATE(MBFM%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MBFM%MP) < NDIG+2) THEN
          DEALLOCATE(MBFM%MP)
          ALLOCATE(MBFM%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NTRSAV = NTRACE
      IF (NTRACE /= 0) THEN
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'ZMABS'
          CALL ZMNTR(2,MA,MA,1)
          NCALL = NCALL - 1
      ENDIF
      NTRACE = 0
      CALL ZMENTR('ZMABS    ',MA,MA,1,MZ01,KRESLT,NDSAVE,MXSAVE,KOVUN)
      NTRACE = NTRSAV
      IF (KRESLT /= 0) THEN
          CALL FMEQ(MZ01(1),MBFM)
          NCALL = NCALL + 1
          IF (NTRACE /= 0) CALL FMNTR(1,MBFM,MBFM,1,1)
          NCALL = NCALL - 1
          RETURN
      ENDIF
      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL ZMEQU(MA,MZ02,NDSAVE,NDIG)

!             Check for special cases.

      MXEXP1 = INT(MXEXP2/2.01D0)
      IF (MA(1)%MP(3) == 0) THEN
          CALL FMABS(MZ02(2),MXY(3))
          GO TO 120
      ELSE IF (MA(2)%MP(3) == 0) THEN
          CALL FMABS(MZ02(1),MXY(3))
          GO TO 120
      ELSE IF (MA(1)%MP(2) == MEXPOV .OR. MA(2)%MP(2) == MEXPOV) THEN
          CALL FMI2M(1,MXY(3))
          MXY(3)%MP(2) = MAX(MZ02(1)%MP(2),MZ02(2)%MP(2))
          GO TO 120
      ELSE IF (MA(1)%MP(2) == MEXPUN) THEN
          IF (MA(2)%MP(2) > -MXEXP1+NDIG+1) THEN
              CALL FMABS(MZ02(2),MXY(3))
          ELSE
              CALL FMST2M('UNKNOWN',MXY(3))
              KFLAG = -4
          ENDIF
          GO TO 120
      ELSE IF (MA(2)%MP(2) == MEXPUN) THEN
          IF (MA(1)%MP(2) > -MXEXP1+NDIG+1) THEN
              CALL FMABS(MZ02(1),MXY(3))
          ELSE
              CALL FMST2M('UNKNOWN',MXY(3))
              KFLAG = -4
          ENDIF
          GO TO 120
      ELSE IF (MA(1)%MP(2) /= MUNKNO .AND. MA(2)%MP(2) /= MUNKNO) THEN
          IF (MA(1)%MP(2) > MA(2)%MP(2)+NDIG+1) THEN
              CALL FMABS(MZ02(1),MXY(3))
              GO TO 120
          ELSE IF (MA(2)%MP(2) > MA(1)%MP(2)+NDIG+1) THEN
              CALL FMABS(MZ02(2),MXY(3))
              GO TO 120
          ENDIF
      ENDIF

      CALL FMSQR(MZ02(1),MXY(1))
      CALL FMSQR(MZ02(2),MXY(2))
      CALL FMADD(MXY(1),MXY(2),MXY(3))
      CALL FMSQRT_R1(MXY(3))

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  120 IF (NCALL >= 1) THEN
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
      CALL ZMEXI2(MXY(3),MBFM,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE ZMABS

      SUBROUTINE ZMACOS(MA,MB)

!  MB = ACOS(MA).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,KL,KOVUN,KR_RETRY,KRESLT,KRSAVE,NDSAVE
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(4),MZ01(2),MZ02(2),MZ03(2),MZ04(2),MZ05(2)

      IF (.NOT. ALLOCATED(MB(1)%MP)) THEN
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(1)%MP)
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MB(2)%MP)) THEN
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(2)%MP)
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL ZMENTR('ZMACOS   ',MA,MA,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF
      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL ZMEQU(MA,MZ04,NDSAVE,NDIG)

!             Check for special cases.

      IF (MA(1)%MP(3) == 0 .AND. MA(2)%MP(3) == 0) THEN
          CALL FMPI(MZ01(1))
          CALL FMDIVI_R1(MZ01(1),2)
          CALL FMI2M(0,MZ01(2))
          GO TO 120
      ELSE IF (MA(2)%MP(3) == 0) THEN
          KRSAVE = KRAD
          KRAD = 1
          CALL FMACOS(MZ04(1),MZ01(1))
          KRAD = KRSAVE
          IF (KFLAG == 0) THEN
              CALL FMI2M(0,MZ01(2))
              GO TO 120
          ENDIF
      ENDIF
      IF ((MA(1)%MP(3) == 0 .OR. MA(1)%MP(2)*2 <= -NDIG) .AND.  &
          (MA(2)%MP(3) == 0 .OR. MA(2)%MP(2)*2 <= -NDIG)) THEN
          CALL FMPI(MZ02(1))
          CALL FMDIVI_R1(MZ02(1),2)
          CALL FMI2M(0,MZ02(2))
          CALL ZMSUB(MZ02,MZ04,MZ01)
          GO TO 120
      ENDIF

      CALL ZMI2M(0,MZ01)
      CALL ZMI2M(1,MZ03)
      CALL ZMSUB(MZ03,MZ04,MZ02)
      CALL ZMADD(MZ03,MZ04,MZ05)
      CALL ZMMPY(MZ02,MZ05,MZ03)
      CALL ZMSQRT(MZ03,MZ02)
      DO J = 1, NDIG+2
         MZ03(1)%MP(J) = MZ02(2)%MP(J)
         MZ03(2)%MP(J) = MZ02(1)%MP(J)
      ENDDO
      IF (MZ03(1)%MP(2) /= MUNKNO .AND. MZ03(1)%MP(3) /= 0)  &
          MZ03(1)%MP(1) = -MZ03(1)%MP(1)

      IF ((MA(1)%MP(3) /= 0 .AND. MZ03(1)%MP(2) == MA(1)%MP(2) .AND.  &
          MZ03(1)%MP(1)*MZ03(1)%MP(3) == MA(1)%MP(1)*MA(1)%MP(3))     &
          .OR. (MA(2)%MP(3) /= 0 .AND. MZ03(2)%MP(2) == MA(2)%MP(2)   &
          .AND. MZ03(2)%MP(1)*MZ03(2)%MP(3) ==                        &
          MA(2)%MP(1)*MA(2)%MP(3)) ) THEN
          CALL ZMADD(MZ04,MZ03,MZ05)
          CALL FMSQR(MZ05(1),MXY(2))
          CALL FMSQR(MZ05(2),MXY(3))
          CALL FMADD(MXY(2),MXY(3),MXY(4))
          CALL FMI2M(1,MXY(1))
          CALL FMSUB_R2(MXY(4),MXY(1))
          IF (MXY(1)%MP(2) < 0) THEN
              NDIG = NDIG - INT(MXY(1)%MP(2))
              CALL ZMEQU_R1(MZ04,NDSAVE,NDIG)
              CALL ZMI2M(0,MZ01)
              CALL ZMI2M(1,MZ03)
              CALL ZMSUB(MZ03,MZ04,MZ02)
              CALL ZMADD(MZ03,MZ04,MZ05)
              CALL ZMMPY(MZ02,MZ05,MZ03)
              CALL ZMSQRT(MZ03,MZ02)
              DO J = 1, NDIG+2
                 MZ03(1)%MP(J) = MZ02(2)%MP(J)
                 MZ03(2)%MP(J) = MZ02(1)%MP(J)
              ENDDO
              IF (MZ03(1)%MP(2) /= MUNKNO .AND. MZ03(1)%MP(3) /= 0)  &
                  MZ03(1)%MP(1) = -MZ03(1)%MP(1)
              CALL ZMADD(MZ04,MZ03,MZ05)
          ENDIF

          CALL ZMLN(MZ05,MZ03)
          DO J = 1, NDIG+2
             MZ01(1)%MP(J) = MZ03(2)%MP(J)
             MZ01(2)%MP(J) = MZ03(1)%MP(J)
          ENDDO
          IF (MZ01(2)%MP(2) /= MUNKNO .AND. MZ01(2)%MP(3) /= 0)  &
              MZ01(2)%MP(1) = -MZ01(2)%MP(1)
      ELSE
          CALL ZMSUB(MZ04,MZ03,MZ05)
          CALL FMSQR(MZ05(1),MXY(2))
          CALL FMSQR(MZ05(2),MXY(3))
          CALL FMADD(MXY(2),MXY(3),MXY(4))
          CALL FMI2M(1,MXY(1))
          CALL FMSUB_R2(MXY(4),MXY(1))
          IF (MXY(1)%MP(2) < 0) THEN
              NDIG = NDIG - INT(MXY(1)%MP(2))
              CALL ZMEQU_R1(MZ04,NDSAVE,NDIG)
              CALL ZMI2M(0,MZ01)
              CALL ZMI2M(1,MZ03)
              CALL ZMSUB(MZ03,MZ04,MZ02)
              CALL ZMADD(MZ03,MZ04,MZ05)
              CALL ZMMPY(MZ02,MZ05,MZ03)
              CALL ZMSQRT(MZ03,MZ02)
              DO J = 1, NDIG+2
                 MZ03(1)%MP(J) = MZ02(2)%MP(J)
                 MZ03(2)%MP(J) = MZ02(1)%MP(J)
              ENDDO
              IF (MZ03(1)%MP(2) /= MUNKNO .AND. MZ03(1)%MP(3) /= 0)  &
                  MZ03(1)%MP(1) = -MZ03(1)%MP(1)
              CALL ZMSUB(MZ04,MZ03,MZ05)
          ENDIF

          CALL ZMLN(MZ05,MZ03)
          DO J = 1, NDIG+2
             MZ01(1)%MP(J) = MZ03(2)%MP(J)
             MZ01(2)%MP(J) = MZ03(1)%MP(J)
          ENDDO
          IF (MZ01(1)%MP(2) /= MUNKNO .AND. MZ01(1)%MP(3) /= 0)  &
              MZ01(1)%MP(1) = -MZ01(1)%MP(1)
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  120 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(1)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL ZMEXIT(MZ01,MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE ZMACOS

      SUBROUTINE ZMACOSH(MA,MB)

!  MB = ACOSH(MA).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,KL,KOVUN,KR_RETRY,KRESLT,NDSAVE
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MZ01(2),MZ02(2),MZ03(2),MZ04(2),MZ05(2)

      IF (.NOT. ALLOCATED(MB(1)%MP)) THEN
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(1)%MP)
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MB(2)%MP)) THEN
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(2)%MP)
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL ZMENTR('ZMACOSH  ',MA,MA,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF
      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL ZMEQU(MA,MZ04,NDSAVE,NDIG)

      IF ((MA(1)%MP(3) == 0 .OR. MA(1)%MP(2)*4 <= -NDIG) .AND.  &
          (MA(2)%MP(3) == 0 .OR. MA(2)%MP(2)*4 <= -NDIG)) THEN
          CALL FMPI(MZ02(1))
          CALL FMDIVI_R1(MZ02(1),2)
          CALL FMI2M(0,MZ02(2))
          CALL ZMSUB(MZ02,MZ04,MZ03)
          IF (MZ04(2)%MP(2) /= MEXPUN) THEN
              CALL ZMIPWR(MZ04,3,MZ02)
              CALL ZMDIVI(MZ02,6,MZ01)
              CALL ZMSUB(MZ03,MZ01,MZ02)
              CALL FMEQ(MZ02(1),MZ01(2))
              CALL FMMPYI(MZ02(2),-1,MZ01(1))
          ELSE
              CALL FMIPWR(MZ04(1),3,MZ02(1))
              CALL FMDIVI(MZ02(1),6,MZ01(1))
              CALL FMSUB(MZ03(1),MZ01(1),MZ01(2))
              CALL FMMPYI(MZ03(2),-1,MZ01(1))
          ENDIF
          IF (MZ04(2)%MP(1) < 0) THEN
              MZ01(2)%MP(1) = -1
              IF (MZ01(1)%MP(2) /= MUNKNO .AND. MZ01(1)%MP(3) /= 0)  &
                  MZ01(1)%MP(1) = -MZ01(1)%MP(1)
          ENDIF
          GO TO 120
      ENDIF

      CALL ZMI2M(1,MZ03)
      CALL ZMADD(MZ04,MZ03,MZ05)
      CALL ZMSQRT(MZ05,MZ02)
      CALL ZMSUB(MZ04,MZ03,MZ05)
      CALL ZMSQRT(MZ05,MZ01)
      CALL ZMMPY(MZ01,MZ02,MZ05)
      CALL ZMADD(MZ04,MZ05,MZ02)
      CALL ZMLN(MZ02,MZ01)

  120 IF (MA(2)%MP(3) == 0 .AND. MA(1)%MP(2) <= 0) THEN
          CALL FMI2M(0,MZ01(1))
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(1)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL ZMEXIT(MZ01,MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE ZMACOSH

      SUBROUTINE ZMADD(MA,MB,MC)

!  MC = MA + MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2),MC(2)
      INTEGER :: KF1,KOVUN,KRESLT,KWRNSV,NDSAVE,NTRSAV
      REAL (KIND(1.0D0)) :: MAR,MAI,MBR,MBI,MXSAVE
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC

      IF (.NOT. ALLOCATED(MC(1)%MP)) THEN
          ALLOCATE(MC(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MC(1)%MP)
          ALLOCATE(MC(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MC(2)%MP)) THEN
          ALLOCATE(MC(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MC(2)%MP)
          ALLOCATE(MC(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (ABS(MA(1)%MP(2)) > MEXPAB .OR. ABS(MA(2)%MP(2)) > MEXPAB .OR.  &
          ABS(MB(1)%MP(2)) > MEXPAB .OR. ABS(MB(2)%MP(2)) > MEXPAB .OR.  &
          KDEBUG >= 1) THEN
          CALL ZMENTR('ZMADD    ',MA,MB,2,MC,KRESLT,NDSAVE,MXSAVE,KOVUN)
          IF (KRESLT /= 0) THEN
              RETURN
          ENDIF
          NDIG = NDSAVE
          MXEXP = MXSAVE
          NTRSAV = NTRACE
          NTRACE = 0
      ELSE
          NCALL = NCALL + 1
          NTRSAV = NTRACE
          IF (NTRACE /= 0) THEN
              NAMEST(NCALL) = 'ZMADD'
              CALL ZMNTR(2,MA,MB,2)
              NTRACE = 0
          ENDIF
          KOVUN = 0
      ENDIF

!             Force FMADD to use more guard digits for user calls.

      NCALL = NCALL - 1

      KWRNSV = KWARN
      KWARN = 0
      MAR = MA(1)%MP(2)
      IF (MA(1)%MP(3) == 0) MAR = MEXPUN - 1
      MAI = MA(2)%MP(2)
      IF (MA(2)%MP(3) == 0) MAI = MEXPUN - 1
      MBR = MB(1)%MP(2)
      IF (MB(1)%MP(3) == 0) MBR = MEXPUN - 1
      MBI = MB(2)%MP(2)
      IF (MB(2)%MP(3) == 0) MBI = MEXPUN - 1

      CALL FMADD(MA(1),MB(1),MC(1))
      KF1 = KFLAG
      CALL FMADD(MA(2),MB(2),MC(2))

      NCALL = NCALL + 1
      IF (NTRSAV /= 0) THEN
          NTRACE = NTRSAV
          NAMEST(NCALL) = 'ZMADD'
      ENDIF
      KWARN = KWRNSV
      IF (KFLAG == 1) KFLAG = KF1
      IF (KFLAG == 1) THEN
          KFLAG = 0
          IF (MAR <= MBR .AND. MAI <= MBI) KFLAG = 1
          IF (MAR >= MBR .AND. MAI >= MBI) KFLAG = 1
      ENDIF

      IF (MC(1)%MP(2) == MUNKNO .OR. MC(2)%MP(2) == MUNKNO) THEN
          KFLAG = -4
      ELSE IF (MC(1)%MP(2) == MEXPOV .OR. MC(2)%MP(2) == MEXPOV) THEN
          KFLAG = -5
      ELSE IF (MC(1)%MP(2) == MEXPUN .OR. MC(2)%MP(2) == MEXPUN) THEN
          KFLAG = -6
      ENDIF
      IF ((MC(1)%MP(2) == MUNKNO) .OR. (MC(2)%MP(2) == MUNKNO)  &
         .OR. (MC(1)%MP(2) == MEXPUN .AND. KOVUN == 0)          &
         .OR. (MC(2)%MP(2) == MEXPUN .AND. KOVUN == 0)          &
         .OR. (MC(1)%MP(2) == MEXPOV .AND. KOVUN == 0)          &
         .OR. (MC(2)%MP(2) == MEXPOV .AND. KOVUN == 0)) THEN
          NAMEST(NCALL) = 'ZMADD'
          CALL ZMWARN
      ENDIF
      IF (NTRACE /= 0) THEN
          CALL ZMNTR(1,MC,MC,1)
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE ZMADD

      SUBROUTINE ZMADD_R1(MA,MB)

!  MA = MA + MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (INOUT) :: MA
      INTENT (IN) :: MB

      TYPE(MULTI) :: MXY(2)


      CALL ZMADD(MA,MB,MXY)
      CALL ZMEQ(MXY,MA)


      END SUBROUTINE ZMADD_R1

      SUBROUTINE ZMADD_R2(MA,MB)

!  MB = MA + MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB

      TYPE(MULTI) :: MXY(2)


      CALL ZMADD(MA,MB,MXY)
      CALL ZMEQ(MXY,MB)


      END SUBROUTINE ZMADD_R2

      SUBROUTINE ZMADDI(MA,INTEG)

!  MA = MA + INTEG        Increment by one-word (real) integer.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2)
      INTEGER :: INTEG

      INTEGER :: KOVUN,KRESLT,KWRNSV,NDSAVE,NTRSAV
      REAL (KIND(1.0D0)) :: MXSAVE
      INTENT (INOUT) :: MA
      INTENT (IN) :: INTEG
      TYPE(MULTI) :: MZ01(2)


      IF (ABS(MA(1)%MP(2)) > MEXPAB .OR. ABS(MA(2)%MP(2)) > MEXPAB .OR.  &
          KDEBUG >= 1) THEN
          NTRSAV = NTRACE
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'ZMADDI'
              CALL ZMNTR(2,MA,MA,1)
              CALL FMNTRI(2,INTEG,0)
              NCALL = NCALL - 1
          ENDIF
          NTRACE = 0
          CALL ZMENTR('ZMADDI   ',MA,MA,1,MZ01,KRESLT,NDSAVE,MXSAVE,KOVUN)
          NTRACE = NTRSAV
          IF (KRESLT /= 0) THEN
              CALL FMEQ(MZ01(1),MA(1))
              NCALL = NCALL + 1
              IF (NTRACE /= 0) CALL ZMNTR(1,MA,MA,1)
              NCALL = NCALL - 1
              RETURN
          ENDIF
          NDIG = NDSAVE
          MXEXP = MXSAVE
          NTRSAV = NTRACE
      ELSE
          NCALL = NCALL + 1
          IF (NTRACE /= 0) THEN
              NAMEST(NCALL) = 'ZMADDI'
              CALL ZMNTR(2,MA,MA,1)
              CALL FMNTRI(2,INTEG,0)
          ENDIF
          KOVUN = 0
      ENDIF

!             Force FMADDI to use more guard digits for user calls.

      NCALL = NCALL - 1
      NTRSAV = NTRACE
      NTRACE = 0
      KWRNSV = KWARN
      KWARN = 0

      CALL FMADDI(MA(1),INTEG)

      NTRACE = NTRSAV
      KWARN = KWRNSV
      NCALL = NCALL + 1
      IF (NTRACE /= 0) NAMEST(NCALL) = 'ZMADDI'
      IF (MA(1)%MP(2) == MUNKNO .OR. MA(2)%MP(2) == MUNKNO) THEN
          KFLAG = -4
      ELSE IF (MA(1)%MP(2) == MEXPOV .OR. MA(2)%MP(2) == MEXPOV) THEN
          KFLAG = -5
      ELSE IF (MA(1)%MP(2) == MEXPUN .OR. MA(2)%MP(2) == MEXPUN) THEN
          KFLAG = -6
      ENDIF
      IF ((MA(1)%MP(2) == MUNKNO) .OR. (MA(2)%MP(2) == MUNKNO)  &
         .OR. (MA(1)%MP(2) == MEXPUN .AND. KOVUN == 0)          &
         .OR. (MA(2)%MP(2) == MEXPUN .AND. KOVUN == 0)          &
         .OR. (MA(1)%MP(2) == MEXPOV .AND. KOVUN == 0)          &
         .OR. (MA(2)%MP(2) == MEXPOV .AND. KOVUN == 0)) THEN
          NAMEST(NCALL) = 'ZMADDI'
          CALL ZMWARN
      ENDIF
      IF (NTRACE /= 0) CALL ZMNTR(1,MA,MA,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE ZMADDI

      SUBROUTINE ZMARG(MA,MBFM)

!  MBFM = ARG(MA)

!  Complex argument.  The result is a real FM number.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MBFM
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,KL,KOVUN,KR_RETRY,KRESLT,NDSAVE,NTRSAV
      INTENT (IN) :: MA
      INTENT (INOUT) :: MBFM
      TYPE(MULTI) :: MZ01(2),MZ02(2)

      IF (.NOT. ALLOCATED(MBFM%MP)) THEN
          ALLOCATE(MBFM%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MBFM%MP) < NDIG+2) THEN
          DEALLOCATE(MBFM%MP)
          ALLOCATE(MBFM%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NTRSAV = NTRACE
      IF (NTRACE /= 0) THEN
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'ZMARG'
          CALL ZMNTR(2,MA,MA,1)
          NCALL = NCALL - 1
      ENDIF
      NTRACE = 0
      CALL ZMENTR('ZMARG    ',MA,MA,1,MZ01,KRESLT,NDSAVE,MXSAVE,KOVUN)
      NTRACE = NTRSAV
      IF (KRESLT /= 0) THEN
          CALL FMEQ(MZ01(1),MBFM)
          NCALL = NCALL + 1
          IF (NTRACE /= 0) CALL FMNTR(1,MBFM,MBFM,1,1)
          NCALL = NCALL - 1
          RETURN
      ENDIF
      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL ZMEQU(MA,MZ02,NDSAVE,NDIG)

      CALL FMATN2(MZ02(2),MZ02(1),MZ01(1))

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(1)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL ZMEXI2(MZ01(1),MBFM,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE ZMARG

      SUBROUTINE ZMASIN(MA,MB)

!  MB = ASIN(MA).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,KL,KOVUN,KR_RETRY,KRESLT,KRSAVE,NDSAVE
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(4),MZ01(2),MZ02(2),MZ03(2),MZ04(2),MZ05(2)

      IF (.NOT. ALLOCATED(MB(1)%MP)) THEN
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(1)%MP)
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MB(2)%MP)) THEN
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(2)%MP)
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL ZMENTR('ZMASIN   ',MA,MA,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF
      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL ZMEQU(MA,MZ04,NDSAVE,NDIG)

!             Check for special cases.

      IF (MA(1)%MP(3) == 0 .AND. MA(2)%MP(3) == 0) THEN
          CALL ZMI2M(0,MZ01)
          GO TO 120
      ELSE IF ((MA(1)%MP(3) == 0 .OR. MA(1)%MP(2)*2 <= -NDIG) .AND.  &
               (MA(2)%MP(3) == 0 .OR. MA(2)%MP(2)*2 <= -NDIG)) THEN
          CALL ZMEQ(MZ04,MZ01)
          GO TO 120
      ELSE IF (MA(2)%MP(3) == 0) THEN
          KRSAVE = KRAD
          KRAD = 1
          CALL FMASIN(MZ04(1),MZ01(1))
          KRAD = KRSAVE
          IF (KFLAG == 0) THEN
              CALL FMI2M(0,MZ01(2))
              GO TO 120
          ENDIF
      ENDIF

      CALL ZMI2M(0,MZ01)
      CALL ZMI2M(1,MZ03)
      CALL ZMSUB(MZ03,MZ04,MZ02)
      CALL ZMADD(MZ03,MZ04,MZ05)
      CALL ZMMPY(MZ02,MZ05,MZ03)
      CALL ZMSQRT(MZ03,MZ02)
      DO J = 1, NDIG+2
         MZ03(1)%MP(J) = MZ04(2)%MP(J)
         MZ03(2)%MP(J) = MZ04(1)%MP(J)
      ENDDO
      IF (MZ03(1)%MP(2) /= MUNKNO .AND. MZ03(1)%MP(3) /= 0)  &
          MZ03(1)%MP(1) = -MZ03(1)%MP(1)

      IF ((MZ02(1)%MP(3) /= 0 .AND. MZ03(1)%MP(2) == MZ02(1)%MP(2)  &
          .AND. MZ03(1)%MP(1)*MZ03(1)%MP(3) ==                      &
                MZ02(1)%MP(1)*MZ02(1)%MP(3)) .OR.                   &
          (MZ02(2)%MP(3) /= 0 .AND. MZ03(2)%MP(2) == MZ02(2)%MP(2)  &
          .AND.  MZ03(2)%MP(1)*MZ03(2)%MP(3) ==                     &
                 MZ02(2)%MP(1)*MZ02(2)%MP(3)) ) THEN
          CALL ZMADD(MZ02,MZ03,MZ05)
          CALL FMSQR(MZ05(1),MXY(2))
          CALL FMSQR(MZ05(2),MXY(3))
          CALL FMADD(MXY(2),MXY(3),MXY(4))
          CALL FMI2M(1,MXY(1))
          CALL FMSUB_R2(MXY(4),MXY(1))
          IF (MXY(1)%MP(2) < 0) THEN
              NDIG = NDIG - INT(MXY(1)%MP(2))
              CALL ZMEQU_R1(MZ04,NDSAVE,NDIG)
              CALL ZMI2M(0,MZ01)
              CALL ZMI2M(1,MZ03)
              CALL ZMSUB(MZ03,MZ04,MZ02)
              CALL ZMADD(MZ03,MZ04,MZ05)
              CALL ZMMPY(MZ02,MZ05,MZ03)
              CALL ZMSQRT(MZ03,MZ02)
              DO J = 1, NDIG+2
                 MZ03(1)%MP(J) = MZ04(2)%MP(J)
                 MZ03(2)%MP(J) = MZ04(1)%MP(J)
              ENDDO
              IF (MZ03(1)%MP(2) /= MUNKNO .AND. MZ03(1)%MP(3) /= 0)  &
                  MZ03(1)%MP(1) = -MZ03(1)%MP(1)
              CALL ZMADD(MZ02,MZ03,MZ05)
          ENDIF

          CALL ZMLN(MZ05,MZ03)
          DO J = 1, NDIG+2
             MZ01(1)%MP(J) = MZ03(2)%MP(J)
             MZ01(2)%MP(J) = MZ03(1)%MP(J)
          ENDDO
          IF (MZ01(2)%MP(2) /= MUNKNO .AND. MZ01(2)%MP(3) /= 0)  &
              MZ01(2)%MP(1) = -MZ01(2)%MP(1)
      ELSE
          CALL ZMSUB(MZ02,MZ03,MZ05)
          CALL FMSQR(MZ05(1),MXY(2))
          CALL FMSQR(MZ05(2),MXY(3))
          CALL FMADD(MXY(2),MXY(3),MXY(4))
          CALL FMI2M(1,MXY(1))
          CALL FMSUB_R2(MXY(4),MXY(1))
          IF (MXY(1)%MP(2) < 0) THEN
              NDIG = NDIG - INT(MXY(1)%MP(2))
              CALL ZMEQU_R1(MZ04,NDSAVE,NDIG)
              CALL ZMI2M(0,MZ01)
              CALL ZMI2M(1,MZ03)
              CALL ZMSUB(MZ03,MZ04,MZ02)
              CALL ZMADD(MZ03,MZ04,MZ05)
              CALL ZMMPY(MZ02,MZ05,MZ03)
              CALL ZMSQRT(MZ03,MZ02)
              DO J = 1, NDIG+2
                 MZ03(1)%MP(J) = MZ04(2)%MP(J)
                 MZ03(2)%MP(J) = MZ04(1)%MP(J)
              ENDDO
              IF (MZ03(1)%MP(2) /= MUNKNO .AND. MZ03(1)%MP(3) /= 0)  &
                  MZ03(1)%MP(1) = -MZ03(1)%MP(1)
              CALL ZMSUB(MZ02,MZ03,MZ05)
          ENDIF
          CALL ZMLN(MZ05,MZ03)
          DO J = 1, NDIG+2
             MZ01(1)%MP(J) = MZ03(2)%MP(J)
             MZ01(2)%MP(J) = MZ03(1)%MP(J)
          ENDDO
          IF (MZ01(1)%MP(2) /= MUNKNO .AND. MZ01(1)%MP(3) /= 0)  &
              MZ01(1)%MP(1) = -MZ01(1)%MP(1)
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  120 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(1)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL ZMEXIT(MZ01,MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE ZMASIN

      SUBROUTINE ZMASINH(MA,MB)

!  MB = ASINH(MA).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,KL,KOVUN,KR_RETRY,KRESLT,NDSAVE
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MZ01(2),MZ02(2),MZ03(2),MZ04(2),MZ05(2)

      IF (.NOT. ALLOCATED(MB(1)%MP)) THEN
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(1)%MP)
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MB(2)%MP)) THEN
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(2)%MP)
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL ZMENTR('ZMASINH  ',MA,MA,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF
      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL ZMEQU(MA,MZ04,NDSAVE,NDIG)

!             Check for special cases.

      IF (MA(1)%MP(3) == 0 .AND. MA(2)%MP(3) == 0) THEN
          CALL ZMI2M(0,MZ01)
          GO TO 120
      ELSE IF ((MA(1)%MP(3) == 0 .OR. MA(1)%MP(2)*2 <= -NDIG) .AND.  &
               (MA(2)%MP(3) == 0 .OR. MA(2)%MP(2)*2 <= -NDIG)) THEN
          IF (KROUND == 1) THEN
              CALL ZMEQ(MZ04,MZ01)
          ELSE
              CALL ZMIPWR(MZ04,3,MZ03)
              CALL ZMDIVI(MZ03,6,MZ02)
              CALL ZMSUB(MZ04,MZ02,MZ01)
              KR_RETRY = 2
          ENDIF
          GO TO 120
      ELSE IF (MA(2)%MP(3) == 0) THEN
          CALL FMASINH(MZ04(1),MZ01(1))
          IF (KFLAG == 0) THEN
              CALL FMI2M(0,MZ01(2))
              GO TO 120
          ENDIF
      ENDIF

      CALL ZMI2M(1,MZ03)
      CALL ZMSQR(MZ04,MZ02)
      CALL ZMADD(MZ03,MZ02,MZ05)
      CALL ZMSQRT(MZ05,MZ02)

      IF (MZ04(1)%MP(1) > 0) THEN
          CALL ZMADD(MZ02,MZ04,MZ05)
          CALL ZMLN(MZ05,MZ01)
      ELSE
          CALL ZMSUB(MZ02,MZ04,MZ05)
          CALL ZMLN(MZ05,MZ01)
          IF (MZ01(1)%MP(2) /= MUNKNO .AND. MZ01(1)%MP(3) /= 0)  &
              MZ01(1)%MP(1) = -MZ01(1)%MP(1)
          IF (MZ01(2)%MP(2) /= MUNKNO .AND. MZ01(2)%MP(3) /= 0)  &
              MZ01(2)%MP(1) = -MZ01(2)%MP(1)
      ENDIF

  120 IF (MA(1)%MP(3) == 0 .AND. MA(2)%MP(2) <= 0) THEN
          CALL FMI2M(0,MZ01(1))
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(1)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL ZMEXIT(MZ01,MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE ZMASINH

      SUBROUTINE ZMATAN(MA,MB)

!  MB = ATAN(MA).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,JTERM,KL,KOVUN,KR_RETRY,KRESLT,KRSAVE,NDSAVE
      LOGICAL, EXTERNAL :: FMCOMP
      REAL :: X
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(5),MZ01(2),MZ02(2),MZ03(2),MZ04(2),MZ05(2)

      IF (.NOT. ALLOCATED(MB(1)%MP)) THEN
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(1)%MP)
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MB(2)%MP)) THEN
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(2)%MP)
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL ZMENTR('ZMATAN   ',MA,MA,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF
      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL ZMEQU(MA,MZ04,NDSAVE,NDIG)

!             Check for special cases.

      IF (MA(1)%MP(3) == 0 .AND. MA(2)%MP(3) == 0) THEN
          CALL ZMI2M(0,MZ03)
          GO TO 130
      ELSE IF ((MA(1)%MP(3) == 0 .OR. MA(1)%MP(2)*2 <= -NDIG) .AND.  &
               (MA(2)%MP(3) == 0 .OR. MA(2)%MP(2)*2 <= -NDIG)) THEN
          CALL ZMEQ(MZ04,MZ03)
          GO TO 130
      ELSE IF (MA(2)%MP(3) == 0) THEN
          KRSAVE = KRAD
          KRAD = 1
          CALL FMATAN(MZ04(1),MZ03(1))
          KRAD = KRSAVE
          IF (KFLAG == 0) THEN
              CALL FMI2M(0,MZ03(2))
              GO TO 130
          ENDIF
      ENDIF

      X = 1.0E+5
      CALL FMDPM(DBLE(X),MXY(1))
      CALL FMABS(MZ04(1),MXY(2))
      CALL FMABS(MZ04(2),MXY(3))
      CALL FMADD_R2(MXY(2),MXY(3))

      IF (FMCOMP(MXY(3),'>=',MXY(1))) THEN
          CALL ZMI2M(0,MZ03)
          CALL FMPI(MZ03(1))
          CALL FMDIVI_R1(MZ03(1),2)
          IF (MA(1)%MP(1) < 0 .AND. MZ03(1)%MP(2) /= MUNKNO .AND.  &
              MZ03(1)%MP(3) /= 0) MZ03(1)%MP(1) = -MZ03(1)%MP(1)
          CALL ZMI2M(1,MZ01)
          CALL ZMDIV(MZ01,MZ04,MZ05)
          CALL ZMEQ(MZ05,MZ02)
          CALL ZMSUB(MZ03,MZ05,MZ01)
          CALL ZMEQ(MZ01,MZ03)
          IF (MA(1)%MP(2) > NDIG .OR. MA(2)%MP(2) > NDIG) GO TO 130
          CALL ZMSQR(MZ05,MZ01)
          JTERM = 1
  120     CALL ZMMPY(MZ02,MZ01,MZ05)
          CALL ZMEQ(MZ05,MZ02)
          JTERM = JTERM + 2
          CALL FMEQ(MZ02(1),MXY(4))
          CALL FMEQ(MZ02(2),MXY(5))
          CALL ZMDIVI(MZ02,JTERM,MZ05)
          CALL ZMEQ(MZ05,MZ02)
          CALL ZMADD(MZ03,MZ02,MZ05)
          CALL ZMEQ(MZ05,MZ03)
          IF (KFLAG /= 0) GO TO 130
          CALL FMEQ(MXY(4),MZ02(1))
          CALL FMEQ(MXY(5),MZ02(2))
          CALL ZMMPY(MZ02,MZ01,MZ05)
          CALL ZMEQ(MZ05,MZ02)
          JTERM = JTERM + 2
          CALL FMEQ(MZ02(1),MXY(4))
          CALL FMEQ(MZ02(2),MXY(5))
          CALL ZMDIVI(MZ02,JTERM,MZ05)
          CALL ZMEQ(MZ05,MZ02)
          CALL ZMSUB(MZ03,MZ02,MZ05)
          CALL ZMEQ(MZ05,MZ03)
          IF (KFLAG /= 0) GO TO 130
          CALL FMEQ(MXY(4),MZ02(1))
          CALL FMEQ(MXY(5),MZ02(2))
          GO TO 120
      ELSE
          CALL ZM2I2M(0,1,MZ01)
          CALL ZMSUB(MZ01,MZ04,MZ03)
          CALL ZMADD(MZ01,MZ04,MZ05)
          CALL ZMDIV(MZ05,MZ03,MZ02)
          CALL FMSQR(MZ02(1),MXY(3))
          CALL FMSQR(MZ02(2),MXY(4))
          CALL FMADD(MXY(3),MXY(4),MXY(5))
          CALL FMI2M(1,MXY(2))
          CALL FMSUB_R2(MXY(5),MXY(2))
          IF (MXY(2)%MP(2) < 0) THEN
              NDIG = NDIG - INT(MXY(2)%MP(2))
              CALL ZMEQU_R1(MZ04,NDSAVE,NDIG)
              CALL ZM2I2M(0,1,MZ01)
              CALL ZMSUB(MZ01,MZ04,MZ03)
              CALL ZMADD(MZ01,MZ04,MZ05)
              CALL ZMDIV(MZ05,MZ03,MZ02)
          ENDIF
          CALL ZMLN(MZ02,MZ05)
          CALL ZMDIVI(MZ05,2,MZ02)
          DO J = 1, NDIG+2
             MZ03(1)%MP(J) = MZ02(2)%MP(J)
             MZ03(2)%MP(J) = MZ02(1)%MP(J)
          ENDDO
          IF (MZ03(1)%MP(2) /= MUNKNO .AND. MZ03(1)%MP(3) /= 0)  &
              MZ03(1)%MP(1) = -MZ03(1)%MP(1)
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  130 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ03(1)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ03(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL ZMEXIT(MZ03,MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE ZMATAN

      SUBROUTINE ZMATANH(MA,MB)

!  MB = ATANH(MA).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,JTERM,KL,KOVUN,KR_RETRY,KRESLT,NDSAVE
      LOGICAL, EXTERNAL :: FMCOMP
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(5),MZ01(2),MZ02(2),MZ03(2),MZ04(2),MZ05(2),MZ06(2)

      IF (.NOT. ALLOCATED(MB(1)%MP)) THEN
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(1)%MP)
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MB(2)%MP)) THEN
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(2)%MP)
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL ZMENTR('ZMATANH  ',MA,MA,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF
      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL ZMEQU(MA,MZ04,NDSAVE,NDIG)

!             Check for special cases.

      IF (MA(1)%MP(3) == 0 .AND. MA(2)%MP(3) == 0) THEN
          CALL ZMI2M(0,MZ03)
          GO TO 130
      ELSE IF ((MA(1)%MP(3) == 0 .OR. MA(1)%MP(2)*2 <= -NDIG) .AND.  &
               (MA(2)%MP(3) == 0 .OR. MA(2)%MP(2)*2 <= -NDIG)) THEN
          CALL ZMEQ(MZ04,MZ03)
          GO TO 130
      ELSE IF (MA(2)%MP(3) == 0 .AND. MA(2)%MP(2) <= 0) THEN
          CALL FMATANH(MZ04(1),MZ03(1))
          IF (KFLAG == 0) THEN
              CALL FMI2M(0,MZ03(2))
              GO TO 130
          ENDIF
      ENDIF

      CALL FMDPM(1.0D+5,MXY(1))
      CALL FMABS(MZ04(1),MXY(2))
      CALL FMABS(MZ04(2),MXY(3))
      CALL FMADD_R2(MXY(2),MXY(3))

      IF (FMCOMP(MXY(3),'>=',MXY(1))) THEN
          CALL ZMI2M(0,MZ03)
          CALL FMPI(MZ03(2))
          IF (MA(2)%MP(1) > 0) THEN
              CALL FMDIVI_R1(MZ03(2),2)
          ELSE
              CALL FMDIVI_R1(MZ03(2),-2)
          ENDIF
          IF (MA(1)%MP(1) < 0 .AND. MZ03(1)%MP(2) /= MUNKNO .AND.  &
              MZ03(1)%MP(3) /= 0) MZ03(1)%MP(1) = -MZ03(1)%MP(1)
          CALL ZMI2M(1,MZ01)
          CALL ZMDIV(MZ01,MZ04,MZ05)
          CALL ZMEQ(MZ05,MZ02)
          CALL ZMADD(MZ03,MZ05,MZ01)
          CALL ZMEQ(MZ01,MZ03)
          IF (MA(1)%MP(2) > NDIG .OR. MA(2)%MP(2) > NDIG) GO TO 130
          CALL ZMSQR(MZ05,MZ01)
          JTERM = 1
  120     CALL ZMMPY(MZ02,MZ01,MZ05)
          CALL ZMEQ(MZ05,MZ02)
          JTERM = JTERM + 2
          CALL ZMDIVI(MZ02,JTERM,MZ06)
          CALL ZMADD(MZ03,MZ06,MZ05)
          CALL ZMEQ(MZ05,MZ03)
          IF (KFLAG /= 0) GO TO 130
          GO TO 120
      ELSE
          CALL ZMI2M(0,MZ03)
          CALL ZMI2M(1,MZ01)
          CALL ZMSUB(MZ01,MZ04,MZ02)
          CALL ZMADD(MZ01,MZ04,MZ05)
          CALL ZMDIV(MZ05,MZ02,MZ03)
          CALL FMSQR(MZ03(1),MXY(3))
          CALL FMSQR(MZ03(2),MXY(4))
          CALL FMADD(MXY(3),MXY(4),MXY(5))
          CALL FMI2M(1,MXY(2))
          CALL FMSUB_R2(MXY(5),MXY(2))
          IF (MXY(2)%MP(2) < 0) THEN
              NDIG = NDIG - INT(MXY(2)%MP(2))
              CALL ZMEQU_R1(MZ04,NDSAVE,NDIG)
              CALL ZMI2M(1,MZ01)
              CALL ZMSUB(MZ01,MZ04,MZ02)
              CALL ZMADD(MZ01,MZ04,MZ05)
              CALL ZMDIV(MZ05,MZ02,MZ03)
          ENDIF
          CALL ZMLN(MZ03,MZ05)
          CALL ZMDIVI(MZ05,2,MZ03)
      ENDIF

  130 IF (MA(2)%MP(3) == 0 .AND. MA(1)%MP(2) >= 1) THEN
          MZ03(2)%MP(1) = -MA(1)%MP(1)
      ENDIF
      IF (MA(1)%MP(3) == 0) THEN
          CALL FMI2M(0,MZ03(1))
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ03(1)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ03(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL ZMEXIT(MZ03,MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE ZMATANH

      SUBROUTINE ZMCHSH(MA,MB,MC)

!  MB = COSH(MA),    MC = SINH(MA).

!  If both the hyperbolic sine and cosine are needed, this routine is faster than calling both
!  ZMCOSH and ZMSINH.

!  MB and MC must be distinct arrays.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2),MC(2)
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,KL,KOVUN,KR_RETRY,KRESLT,KRSAVE,NCSAVE,NDSAVE
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB,MC
      TYPE(MULTI) :: MZ01(2),MZ02(2),MZ03(2),MZ04(2),MZ05(2)

      IF (.NOT. ALLOCATED(MB(1)%MP)) THEN
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(1)%MP)
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MB(2)%MP)) THEN
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(2)%MP)
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MC(1)%MP)) THEN
          ALLOCATE(MC(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MC(1)%MP)
          ALLOCATE(MC(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MC(2)%MP)) THEN
          ALLOCATE(MC(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MC(2)%MP)
          ALLOCATE(MC(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NCSAVE = NCALL
      CALL ZMENTR('ZMCHSH   ',MA,MA,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      NCALL = NCSAVE + 1
      IF (KRESLT /= 0) THEN
          CALL ZMEQ(MB,MC)
          IF (NTRACE /= 0) THEN
              CALL ZMNTR(1,MB,MB,1)
              IF (ABS(NTRACE) >= 1 .AND. NCALL <= LVLTRC) THEN
                  IF (NTRACE < 0) THEN
                      CALL ZMNTRJ(MC,NDIG)
                  ELSE
                      CALL ZMPRNT(MC)
                  ENDIF
              ENDIF
          ENDIF
          NCALL = NCALL - 1
          RETURN
      ENDIF
      KR_RETRY = 0
      KRSAVE = KRAD
      KRAD = 1

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL ZMEQU(MA,MZ04,NDSAVE,NDIG)

!             Check for special cases.

      IF (MA(1)%MP(3) == 0 .AND. MA(2)%MP(3) == 0) THEN
          CALL ZMI2M(1,MZ01)
          CALL ZMI2M(0,MZ05)
          GO TO 120
      ELSE IF (MA(2)%MP(3) == 0) THEN
          CALL FMCHSH(MZ04(1),MZ01(1),MZ05(1))
          CALL FMI2M(0,MZ01(2))
          CALL FMI2M(0,MZ05(2))
          GO TO 120
      ELSE IF (MA(1)%MP(3) == 0) THEN
          CALL FMCSSN(MZ04(2),MZ01(1),MZ05(2))
          CALL FMI2M(0,MZ01(2))
          CALL FMI2M(0,MZ05(1))
          GO TO 120
      ENDIF

!             Find SINH(REAL(MA)) and COSH(REAL(MA)).

      CALL FMCHSH(MZ04(1),MZ02(1),MZ02(2))

!             Find SIN(IMAG(MA)) and COS(IMAG(MA)).

      CALL FMCSSN(MZ04(2),MZ03(1),MZ03(2))

!             COSH(MA) =  COSH(REAL(MA))*COS(IMAG(MA)) + SINH(REAL(MA))*SIN(IMAG(MA)) i

      CALL FMMPY(MZ02(1),MZ03(1),MZ01(1))
      CALL FMMPY(MZ02(2),MZ03(2),MZ01(2))

!             SINH(MA) =  SINH(REAL(MA))*COS(IMAG(MA)) + COSH(REAL(MA))*SIN(IMAG(MA)) i

      CALL FMMPY(MZ02(2),MZ03(1),MZ05(1))
      CALL FMMPY(MZ02(1),MZ03(2),MZ05(2))

      IF (MZ05(1)%MP(2) == MUNKNO .OR. MZ05(2)%MP(2) == MUNKNO) THEN
          CALL ZMCOSH(MZ04,MZ01)
          CALL ZMSINH(MZ04,MZ05)
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  120 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(1)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ05(1)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ05(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL ZMEQU(MZ05,MC,NDIG,NDSAVE)
      CALL ZMEXIT(MZ01,MB,NDSAVE,MXSAVE,KOVUN)
      IF (NTRACE /= 0) THEN
          IF (ABS(NTRACE) >= 1 .AND. NCALL+1 <= LVLTRC) THEN
              IF (NTRACE < 0) THEN
                  CALL ZMNTRJ(MC,NDIG)
              ELSE
                  CALL ZMPRNT(MC)
              ENDIF
          ENDIF
      ENDIF
      KRAD = KRSAVE
      RETURN
      END SUBROUTINE ZMCHSH

      SUBROUTINE ZMCMPX(MAFM,MBFM,MC)

!  MC = COMPLEX( MAFM , MBFM )

!  MAFM and MBFM are real FM numbers, MC is a complex ZM number.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MAFM,MBFM,MC(2)
      INTENT (IN) :: MAFM,MBFM
      INTENT (INOUT) :: MC

      IF (.NOT. ALLOCATED(MC(1)%MP)) THEN
          ALLOCATE(MC(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MC(1)%MP)
          ALLOCATE(MC(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MC(2)%MP)) THEN
          ALLOCATE(MC(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MC(2)%MP)
          ALLOCATE(MC(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      KFLAG = 0
      NCALL = NCALL + 1
      NAMEST(NCALL) = 'ZMCMPX'
      IF (NTRACE /= 0) CALL FMNTR(2,MAFM,MBFM,2,1)

      CALL FMEQ(MAFM,MC(1))
      CALL FMEQ(MBFM,MC(2))

      IF (NTRACE /= 0) CALL ZMNTR(1,MC,MC,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE ZMCMPX

      SUBROUTINE ZMCONJ(MA,MB)

!  MB = CONJG(MA)

!  Complex conjugate.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB

      KFLAG = 0
      NCALL = NCALL + 1
      NAMEST(NCALL) = 'ZMCONJ'
      IF (NTRACE /= 0) CALL ZMNTR(2,MA,MA,1)

      CALL FMEQ(MA(1),MB(1))
      CALL FMEQ(MA(2),MB(2))
      IF (MB(2)%MP(2) /= MUNKNO .AND. MB(2)%MP(3) /= 0)  &
          MB(2)%MP(1) = -MB(2)%MP(1)

      IF (NTRACE /= 0) CALL ZMNTR(1,MB,MB,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE ZMCONJ

      SUBROUTINE ZMCOS(MA,MB)

!  MB = COS(MA).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,KL,KOVUN,KR_RETRY,KRESLT,KRSAVE,NDSAVE
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(5),MZ01(2),MZ02(2)

      IF (.NOT. ALLOCATED(MB(1)%MP)) THEN
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(1)%MP)
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MB(2)%MP)) THEN
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(2)%MP)
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL ZMENTR('ZMCOS    ',MA,MA,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF
      KR_RETRY = 0
      KRSAVE = KRAD
      KRAD = 1

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL ZMEQU(MA,MZ02,NDSAVE,NDIG)

!             Check for special cases.

      IF (MA(1)%MP(3) == 0 .AND. MA(2)%MP(3) == 0) THEN
          CALL ZMI2M(1,MZ01)
          GO TO 120
      ELSE IF (MA(2)%MP(3) == 0) THEN
          CALL FMCOS(MZ02(1),MZ01(1))
          CALL FMI2M(0,MZ01(2))
          GO TO 120
      ELSE IF (MA(1)%MP(3) == 0) THEN
          CALL FMCOSH(MZ02(2),MZ01(1))
          CALL FMI2M(0,MZ01(2))
          GO TO 120
      ENDIF

!             Find COS(REAL(MA)) and SIN(REAL(MA)).

      CALL FMCSSN(MZ02(1),MZ01(1),MZ01(2))

!             Find COSH(IMAG(MA)) and SINH(IMAG(MA)).

      CALL FMCHSH(MZ02(2),MXY(1),MXY(2))

!             COS(MA) =  COS(REAL(MA))*COSH(IMAG(MA)) - SIN(REAL(MA))*SINH(IMAG(MA)) i

      IF (MXY(1)%MP(2) == MEXPOV) THEN
          CALL FMABS(MZ01(1),MXY(1))
          CALL FMDIVI_R1(MXY(1),2)
          CALL FMLN(MXY(1),MXY(2))
          CALL FMABS(MZ02(2),MXY(3))
          CALL FMADD(MXY(2),MXY(3),MXY(5))
          CALL FMEXP(MXY(5),MXY(4))
          IF (MZ01(1)%MP(1) < 0) CALL FMMPYI_R1(MXY(4),-1)

          CALL FMABS(MZ01(2),MXY(1))
          CALL FMDIVI_R1(MXY(1),2)
          CALL FMLN(MXY(1),MXY(2))
          CALL FMADD(MXY(2),MXY(3),MXY(1))
          CALL FMEXP(MXY(1),MXY(5))
          IF (MZ02(2)%MP(1) > 0) CALL FMMPYI_R1(MXY(5),-1)
          IF (MZ01(2)%MP(1) < 0) CALL FMMPYI_R1(MXY(5),-1)

          CALL FMEQ(MXY(4),MZ01(1))
          CALL FMEQ(MXY(5),MZ01(2))
      ELSE
          CALL FMMPY_R1(MZ01(1),MXY(1))
          IF (MXY(2)%MP(2) /= MUNKNO .AND. MXY(2)%MP(3) /= 0)  &
              MXY(2)%MP(1) = -MXY(2)%MP(1)
          CALL FMMPY_R1(MZ01(2),MXY(2))
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  120 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(1)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL ZMEXIT(MZ01,MB,NDSAVE,MXSAVE,KOVUN)
      KRAD = KRSAVE
      RETURN
      END SUBROUTINE ZMCOS

      SUBROUTINE ZMCOSH(MA,MB)

!  MB = COSH(MA).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,KL,KOVUN,KR_RETRY,KRESLT,KRSAVE,NDSAVE
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(5),MZ01(2),MZ02(2)

      IF (.NOT. ALLOCATED(MB(1)%MP)) THEN
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(1)%MP)
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MB(2)%MP)) THEN
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(2)%MP)
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL ZMENTR('ZMCOSH   ',MA,MA,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF
      KR_RETRY = 0
      KRSAVE = KRAD
      KRAD = 1

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL ZMEQU(MA,MZ02,NDSAVE,NDIG)

!             Check for special cases.

      IF (MA(1)%MP(3) == 0 .AND. MA(2)%MP(3) == 0) THEN
          CALL ZMI2M(1,MZ01)
          GO TO 120
      ELSE IF (MA(1)%MP(3) == 0) THEN
          CALL FMCOS(MZ02(2),MZ01(1))
          CALL FMI2M(0,MZ01(2))
          GO TO 120
      ELSE IF (MA(2)%MP(3) == 0) THEN
          CALL FMCOSH(MZ02(1),MZ01(1))
          CALL FMI2M(0,MZ01(2))
          GO TO 120
      ENDIF

!             Find COS(IMAG(MA)) and SIN(IMAG(MA)).

      CALL FMCSSN(MZ02(2),MZ01(1),MZ01(2))

!             Find COSH(REAL(MA)) and SINH(REAL(MA)).

      CALL FMCHSH(MZ02(1),MXY(1),MXY(2))

!             COSH(MA) =  COSH(REAL(MA))*COS(IMAG(MA)) + SINH(REAL(MA))*SIN(IMAG(MA)) i

      IF (MXY(1)%MP(2) == MEXPOV) THEN
          CALL FMABS(MZ01(1),MXY(1))
          CALL FMDIVI_R1(MXY(1),2)
          CALL FMLN(MXY(1),MXY(2))
          CALL FMABS(MZ02(1),MXY(3))
          CALL FMADD(MXY(2),MXY(3),MXY(5))
          CALL FMEXP(MXY(5),MXY(4))
          IF (MZ01(1)%MP(1) < 0) CALL FMMPYI_R1(MXY(4),-1)

          CALL FMABS(MZ01(2),MXY(1))
          CALL FMDIVI_R1(MXY(1),2)
          CALL FMLN(MXY(1),MXY(2))
          CALL FMADD(MXY(2),MXY(3),MXY(1))
          CALL FMEXP(MXY(1),MXY(5))
          IF (MZ02(1)%MP(1) < 0) CALL FMMPYI_R1(MXY(5),-1)
          IF (MZ01(2)%MP(1) < 0) CALL FMMPYI_R1(MXY(5),-1)

          CALL FMEQ(MXY(4),MZ01(1))
          CALL FMEQ(MXY(5),MZ01(2))
      ELSE
          CALL FMMPY_R1(MZ01(1),MXY(1))
          CALL FMMPY_R1(MZ01(2),MXY(2))
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  120 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(1)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL ZMEXIT(MZ01,MB,NDSAVE,MXSAVE,KOVUN)
      KRAD = KRSAVE
      RETURN
      END SUBROUTINE ZMCOSH

      SUBROUTINE ZMCSSN(MA,MB,MC)

!  MB = COS(MA),    MC = SIN(MA).

!  If both the sine and cosine are needed, this routine is faster than calling both ZMCOS and ZMSIN.

!  MB and MC must be distinct arrays.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2),MC(2)
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,KL,KOVUN,KR_RETRY,KRESLT,KRSAVE,NCSAVE,NDSAVE
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB,MC
      TYPE(MULTI) :: MZ01(2),MZ02(2),MZ03(2),MZ04(2),MZ05(2)

      IF (.NOT. ALLOCATED(MB(1)%MP)) THEN
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(1)%MP)
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MB(2)%MP)) THEN
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(2)%MP)
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MC(1)%MP)) THEN
          ALLOCATE(MC(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MC(1)%MP)
          ALLOCATE(MC(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MC(2)%MP)) THEN
          ALLOCATE(MC(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MC(2)%MP)
          ALLOCATE(MC(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NCSAVE = NCALL
      CALL ZMENTR('ZMCSSN   ',MA,MA,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      NCALL = NCSAVE + 1
      IF (KRESLT /= 0) THEN
          CALL ZMEQ(MB,MC)
          IF (NTRACE /= 0) THEN
              CALL ZMNTR(1,MB,MB,1)
              IF (ABS(NTRACE) >= 1 .AND. NCALL <= LVLTRC) THEN
                  IF (NTRACE < 0) THEN
                      CALL ZMNTRJ(MC,NDIG)
                  ELSE
                      CALL ZMPRNT(MC)
                  ENDIF
              ENDIF
          ENDIF
          NCALL = NCALL - 1
          RETURN
      ENDIF
      KR_RETRY = 0
      KRSAVE = KRAD
      KRAD = 1

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL ZMEQU(MA,MZ04,NDSAVE,NDIG)

!             Check for special cases.

      IF (MA(1)%MP(3) == 0 .AND. MA(2)%MP(3) == 0) THEN
          CALL ZMI2M(1,MZ01)
          CALL ZMI2M(0,MZ05)
          GO TO 120
      ELSE IF (MA(2)%MP(3) == 0) THEN
          CALL FMCSSN(MZ04(1),MZ01(1),MZ05(1))
          CALL FMI2M(0,MZ01(2))
          CALL FMI2M(0,MZ05(2))
          GO TO 120
      ELSE IF (MA(1)%MP(3) == 0) THEN
          CALL FMCHSH(MZ04(2),MZ01(1),MZ05(2))
          CALL FMI2M(0,MZ01(2))
          CALL FMI2M(0,MZ05(1))
          GO TO 120
      ENDIF

!             Find SIN(REAL(MA)) and COS(REAL(MA)).

      CALL FMCSSN(MZ04(1),MZ02(1),MZ02(2))

!             Find SINH(IMAG(MA)) and COSH(IMAG(MA)).

      CALL FMCHSH(MZ04(2),MZ03(1),MZ03(2))

!             COS(MA) =  COS(REAL(MA))*COSH(IMAG(MA)) - SIN(REAL(MA))*SINH(IMAG(MA)) i

      CALL FMMPY(MZ02(1),MZ03(1),MZ01(1))
      CALL FMMPY(MZ02(2),MZ03(2),MZ01(2))
      IF (MZ01(2)%MP(2) /= MUNKNO .AND. MZ01(2)%MP(3) /= 0)  &
          MZ01(2)%MP(1) = -MZ01(2)%MP(1)

!             SIN(MA) =  SIN(REAL(MA))*COSH(IMAG(MA)) + COS(REAL(MA))*SINH(IMAG(MA)) i

      CALL FMMPY(MZ02(2),MZ03(1),MZ05(1))
      CALL FMMPY(MZ02(1),MZ03(2),MZ05(2))

      IF (MZ05(1)%MP(2) == MUNKNO .OR. MZ05(2)%MP(2) == MUNKNO) THEN
          CALL ZMCOS(MZ04,MZ01)
          CALL ZMSIN(MZ04,MZ05)
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  120 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(1)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ05(1)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ05(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL ZMEQU(MZ05,MC,NDIG,NDSAVE)
      CALL ZMEXIT(MZ01,MB,NDSAVE,MXSAVE,KOVUN)
      IF (NTRACE /= 0) THEN
          IF (ABS(NTRACE) >= 1 .AND. NCALL+1 <= LVLTRC) THEN
              IF (NTRACE < 0) THEN
                  CALL ZMNTRJ(MC,NDIG)
              ELSE
                  CALL ZMPRNT(MC)
              ENDIF
          ENDIF
      ENDIF
      KRAD = KRSAVE
      RETURN
      END SUBROUTINE ZMCSSN

      SUBROUTINE ZMDIV(MA,MB,MC)

!  MC = MA / MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2),MC(2)
      REAL (KIND(1.0D0)) :: MAXEXP,MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,KL,KOVUN,KR_RETRY,KRESLT,KWRNSV,NDGSV2,NDSAVE,NTRSAV
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      TYPE(MULTI) :: MXY(8),MZ01(2),MZ02(2)
      LOGICAL, EXTERNAL :: FMCOMP

      IF (.NOT. ALLOCATED(MC(1)%MP)) THEN
          ALLOCATE(MC(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MC(1)%MP)
          ALLOCATE(MC(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MC(2)%MP)) THEN
          ALLOCATE(MC(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MC(2)%MP)
          ALLOCATE(MC(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (ABS(MA(1)%MP(2)) > MEXPAB .OR. ABS(MA(2)%MP(2)) > MEXPAB .OR.  &
          ABS(MB(1)%MP(2)) > MEXPAB .OR. ABS(MB(2)%MP(2)) > MEXPAB .OR.  &
          KDEBUG >= 1) THEN
          CALL ZMENTR('ZMDIV    ',MA,MB,2,MC,KRESLT,NDSAVE,MXSAVE,KOVUN)
          IF (KRESLT /= 0) THEN
              RETURN
          ENDIF
      ELSE
          NCALL = NCALL + 1
          IF (NTRACE /= 0) THEN
              NAMEST(NCALL) = 'ZMDIV'
              CALL ZMNTR(2,MA,MB,2)
          ENDIF
          NDSAVE = NDIG
          NDIG = MAX(NDIG+NGRD52,2)
          IF (MBASE >= 100*ABS(MA(1)%MP(3)) .OR. MBASE >= 100*ABS(MA(2)%MP(3))) THEN
              NDIG = NDIG + 1
          ELSE IF (MBASE >= 100*ABS(MB(1)%MP(3)) .OR.  &
              MBASE >= 100*ABS(MB(2)%MP(3))) THEN
              NDIG = NDIG + 1
          ENDIF
          MXSAVE = MXEXP
          MXEXP = MXEXP2
          KOVUN = 0
      ENDIF

      NTRSAV = NTRACE
      NTRACE = 0
      KR_RETRY = 0
      KWRNSV = KWARN
      KWARN = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL FMEQU(MA(1),MXY(5),NDSAVE,NDIG)
      CALL FMEQU(MA(2),MXY(6),NDSAVE,NDIG)
      CALL FMEQU(MB(1),MXY(7),NDSAVE,NDIG)
      CALL FMEQU(MB(2),MXY(8),NDSAVE,NDIG)

!             Check for special cases.

      IF (MA(1)%MP(2) == MB(1)%MP(2) .AND.  &
          MA(1)%MP(3) == MB(1)%MP(3) .AND.  &
          MA(1)%MP(1) == MB(1)%MP(1)) THEN
          IF (MA(2)%MP(2) == MB(2)%MP(2) .AND.  &
              MA(2)%MP(3) == MB(2)%MP(3) .AND.  &
              MA(2)%MP(1) == MB(2)%MP(1)) THEN
              DO J = 3, NDSAVE+1
                 IF (MA(1)%MP(J+1) /= MB(1)%MP(J+1)) GO TO 120
                 IF (MA(2)%MP(J+1) /= MB(2)%MP(J+1)) GO TO 120
              ENDDO
              IF (ABS(MA(1)%MP(2)) < MEXPOV .AND. ABS(MA(2)%MP(2)) < MEXPOV  &
                  .AND. ABS(MB(1)%MP(2)) < MEXPOV .AND.                      &
                  ABS(MB(2)%MP(2)) < MEXPOV) THEN
                  CALL ZMI2M(1,MZ01)
                  GO TO 140
              ENDIF
          ENDIF
      ENDIF
      IF (MA(1)%MP(2) == MB(1)%MP(2) .AND.  &
          MA(1)%MP(3) == MB(1)%MP(3) .AND.  &
          (-MA(1)%MP(1)) == MB(1)%MP(1)) THEN
          IF (MA(2)%MP(2) == MB(2)%MP(2) .AND.  &
              MA(2)%MP(3) == MB(2)%MP(3) .AND.  &
              (-MA(2)%MP(1)) == MB(2)%MP(1)) THEN
              DO J = 3, NDSAVE+1
                 IF (MA(1)%MP(J+1) /= MB(1)%MP(J+1)) GO TO 120
                 IF (MA(2)%MP(J+1) /= MB(2)%MP(J+1)) GO TO 120
              ENDDO
              IF (ABS(MA(1)%MP(2)) < MEXPOV .AND. ABS(MA(2)%MP(2)) < MEXPOV  &
                  .AND. ABS(MB(1)%MP(2)) < MEXPOV .AND.                      &
                  ABS(MB(2)%MP(2)) < MEXPOV) THEN
                  CALL ZMI2M(-1,MZ01)
                  GO TO 140
              ENDIF
          ENDIF
      ENDIF
  120 IF (MXY(5)%MP(2) == MEXPUN .OR. MXY(6)%MP(2) == MEXPUN .OR.  &
          MXY(7)%MP(2) == MEXPUN .OR. MXY(8)%MP(2) == MEXPUN .OR.  &
          MXY(5)%MP(2) == MEXPOV .OR. MXY(6)%MP(2) == MEXPOV .OR.  &
          MXY(7)%MP(2) == MEXPOV .OR. MXY(8)%MP(2) == MEXPOV ) THEN
          CALL ZMI2M(0,MZ01)
          CALL ZMDIV_UNOV(MXY(5),MXY(6),MXY(7),MXY(8),MZ01)
          GO TO 140
      ENDIF

!             Method for  ( a + b i ) / ( c + d i ):

!             If  abs(c) <= abs(d)  Then

!                 P = c / d
!                 result = ( a*P + b )/( c*P + d ) + ( b*P - a )/( c*P + d ) i

!             Else

!                 P = d / c
!                 result = ( b*P + a )/( d*P + c ) + ( b - a*P )/( d*P + c ) i


      IF (MXY(7)%MP(3) == 0) THEN
          CALL FMI2M(0,MXY(4))
          J = 1
      ELSE IF (MXY(8)%MP(3) == 0) THEN
          CALL FMI2M(0,MXY(4))
          J = 2
      ELSE IF (MXY(7)%MP(2) < MXY(8)%MP(2)) THEN
          CALL FMDIV(MXY(7),MXY(8),MXY(4))
          J = 1
      ELSE IF (MXY(8)%MP(2) < MXY(7)%MP(2)) THEN
          CALL FMDIV(MXY(8),MXY(7),MXY(4))
          J = 2
      ELSE IF (MXY(7)%MP(3) < MXY(8)%MP(3)) THEN
          CALL FMDIV(MXY(7),MXY(8),MXY(4))
          J = 1
      ELSE IF (MXY(8)%MP(3) < MXY(7)%MP(3)) THEN
          CALL FMDIV(MXY(8),MXY(7),MXY(4))
          J = 2
      ELSE IF (MXY(7)%MP(4) < MXY(8)%MP(4)) THEN
          CALL FMDIV(MXY(7),MXY(8),MXY(4))
          J = 1
      ELSE IF (MXY(8)%MP(4) < MXY(7)%MP(4)) THEN
          CALL FMDIV(MXY(8),MXY(7),MXY(4))
          J = 2
      ELSE IF (FMCOMP(MXY(7),'<=',MXY(8))) THEN
          CALL FMDIV(MXY(7),MXY(8),MXY(4))
          J = 1
      ELSE
          CALL FMDIV(MXY(8),MXY(7),MXY(4))
          J = 2
      ENDIF

      IF (J == 1) THEN
          CALL FMDIV(MXY(7),MXY(8),MXY(4))
          CALL FMMPYE(MXY(4),MXY(5),MXY(6),MXY(7),MZ01(1),MZ01(2),MXY(3))
          CALL FMADD_R1(MXY(3),MXY(8))
          MAXEXP = MAX(MXY(6)%MP(2),MZ01(1)%MP(2))
          CALL FMADD_R2(MXY(6),MZ01(1))
          IF (MZ01(1)%MP(2) < MAXEXP) GO TO 130
          MAXEXP = MAX(MZ01(2)%MP(2),MXY(5)%MP(2))
          CALL FMSUB_R1(MZ01(2),MXY(5))
          IF (MZ01(2)%MP(2) < MAXEXP) GO TO 130
          CALL FMDIVD(MZ01(1),MZ01(2),MXY(3),MZ02(1),MZ02(2))
          CALL ZMEQ(MZ02,MZ01)
          GO TO 140
      ELSE
          CALL FMDIV(MXY(8),MXY(7),MXY(4))
          CALL FMMPYE(MXY(4),MXY(6),MXY(5),MXY(8), MZ01(1),MZ01(2),MXY(3))
          CALL FMADD_R1(MXY(3),MXY(7))
          MAXEXP = MAX(MXY(5)%MP(2),MZ01(1)%MP(2))
          CALL FMADD_R2(MXY(5),MZ01(1))
          IF (MZ01(1)%MP(2) < MAXEXP) GO TO 130
          MAXEXP = MAX(MZ01(2)%MP(2),MXY(6)%MP(2))
          CALL FMSUB_R2(MXY(6),MZ01(2))
          IF (MZ01(2)%MP(2) < MAXEXP) GO TO 130
          CALL FMDIVD(MZ01(1),MZ01(2),MXY(3),MZ02(1),MZ02(2))
          CALL ZMEQ(MZ02,MZ01)
          GO TO 140
      ENDIF

!             When there was cancellation error above, raise precision and use
!             the more stable formula.
!             ( a*c + b*d ) / ( c*c + d*d ) + ( b*c - a*d ) / ( c*c + d*d ) i

  130 CALL FMEQU_R1(MXY(5),NDIG,2*NDIG)
      CALL FMEQU_R1(MXY(6),NDIG,2*NDIG)
      CALL FMEQU_R1(MXY(7),NDIG,2*NDIG)
      CALL FMEQU_R1(MXY(8),NDIG,2*NDIG)
      NDIG = 2*NDIG
      CALL FMMPYE(MXY(7),MXY(5),MXY(6),MXY(7), MZ01(1),MZ01(2),MXY(3))
      CALL FMMPYE(MXY(8),MXY(6),MXY(5),MXY(8), MXY(1),MXY(2),MXY(4))
      CALL FMADD_R2(MXY(3),MXY(4))
      CALL FMADD_R1(MZ01(1),MXY(1))
      CALL FMSUB_R1(MZ01(2),MXY(2))
      CALL FMDIVD(MZ01(1),MZ01(2),MXY(4),MZ02(1),MZ02(2))
      CALL ZMEQ(MZ02,MZ01)

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  140 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(1)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      MXEXP = MXSAVE
      NTRACE = NTRSAV
      NDGSV2 = NDIG
      NDIG = NDSAVE
      KWARN = KWRNSV
      CALL ZMEQU(MZ01,MC,NDGSV2,NDSAVE)
      IF (MC(1)%MP(2) >= MEXPOV .OR. MC(1)%MP(2) <= -MEXPOV .OR.  &
          MC(2)%MP(2) >= MEXPOV .OR. MC(2)%MP(2) <= -MEXPOV) THEN
          IF (MC(1)%MP(2) == MUNKNO .OR. MC(2)%MP(2) == MUNKNO) THEN
              KFLAG = -4
          ELSE IF (MC(1)%MP(2) == MEXPOV .OR. MC(2)%MP(2) == MEXPOV) THEN
              KFLAG = -5
          ELSE IF (MC(1)%MP(2) == MEXPUN .OR. MC(2)%MP(2) == MEXPUN) THEN
              KFLAG = -6
          ENDIF
          IF ((MC(1)%MP(2) == MUNKNO) .OR. (MC(2)%MP(2) == MUNKNO)  &
             .OR. (MC(1)%MP(2) == MEXPUN .AND. KOVUN == 0)          &
             .OR. (MC(2)%MP(2) == MEXPUN .AND. KOVUN == 0)          &
             .OR. (MC(1)%MP(2) == MEXPOV .AND. KOVUN == 0)          &
             .OR. (MC(2)%MP(2) == MEXPOV .AND. KOVUN == 0)) THEN
                  NAMEST(NCALL) = 'ZMDIV'
                  CALL ZMWARN
          ENDIF
      ENDIF
      IF (NTRACE /= 0) CALL ZMNTR(1,MC,MC,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE ZMDIV

      SUBROUTINE ZMDIV_UNOV(MA,MB,MC,MD,MZ)

!  Check special cases where at least one of MA, MB, MC, MD is underflow or overflow.

!  Return MZ as the result.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC,MD,MZ(2)
      INTENT (INOUT) :: MZ
      INTENT (IN) :: MA,MB,MC,MD

      TYPE(MULTI) :: MXY(12), MZ01(2)
      INTEGER :: K_EPS, KS, N_OV
      REAL (KIND(1.0D0)) :: M_EXPUN, M_EXPOV, M_L1, M_L2, M_R1, M_R2
      LOGICAL, EXTERNAL :: FMCOMP

      M_EXPUN = -AINT( MAX_EXPONENT / 2.01D0 + 0.5D0 ) - 1
      M_EXPOV = AINT( MAX_EXPONENT / 2.01D0 + 0.5D0 ) + 2
      N_OV = 0
      IF (MA%MP(2) == MEXPOV) THEN
          N_OV = 1
          IF (MB%MP(2) >= M_EXPOV-NDIG) THEN
              CALL ZMST2M('UNKNOWN+UNKNOWN*i',MZ)
              GO TO 110
          ENDIF
      ENDIF
      IF (MB%MP(2) == MEXPOV) THEN
          IF (N_OV == 0) THEN
              N_OV = 2
              IF (MA%MP(2) >= M_EXPOV-NDIG) THEN
                  CALL ZMST2M('UNKNOWN+UNKNOWN*i',MZ)
                  GO TO 110
              ENDIF
          ELSE
              CALL ZMST2M('UNKNOWN+UNKNOWN*i',MZ)
              GO TO 110
          ENDIF
      ENDIF
      IF (MC%MP(2) == MEXPOV) THEN
          IF (N_OV == 0) THEN
              N_OV = 3
              IF (MD%MP(2) >= M_EXPOV-NDIG) THEN
                  CALL ZMST2M('UNKNOWN+UNKNOWN*i',MZ)
                  GO TO 110
              ENDIF
          ELSE
              CALL ZMST2M('UNKNOWN+UNKNOWN*i',MZ)
              GO TO 110
          ENDIF
      ENDIF
      IF (MD%MP(2) == MEXPOV) THEN
          IF (N_OV == 0) THEN
              N_OV = 4
              IF (MC%MP(2) >= M_EXPOV-NDIG) THEN
                  CALL ZMST2M('UNKNOWN+UNKNOWN*i',MZ)
                  GO TO 110
              ENDIF
          ELSE
              CALL ZMST2M('UNKNOWN+UNKNOWN*i',MZ)
              GO TO 110
          ENDIF
      ENDIF
      IF (N_OV > 0) THEN
          IF (N_OV <= 2) THEN
              CALL FMST2M('OVERFLOW',MXY(2))
              CALL FMSQR(MC,MXY(4))
              CALL FMSQR(MD,MXY(5))
              CALL FMADD(MXY(4),MXY(5),MXY(6))
              CALL FMSQRT(MXY(6),MXY(3))
          ELSE
              CALL FMST2M('OVERFLOW',MXY(3))
              CALL FMSQR(MA,MXY(4))
              CALL FMSQR(MB,MXY(5))
              CALL FMADD(MXY(4),MXY(5),MXY(6))
              CALL FMSQRT(MXY(6),MXY(2))
          ENDIF
          CALL FMDIV(MXY(2),MXY(3),MXY(1))
          IF (MXY(1)%MP(2) == MUNKNO) THEN
              CALL ZMST2M('UNKNOWN+UNKNOWN*i',MZ)
              GO TO 110
          ENDIF
          IF (N_OV <= 2) THEN
              IF (N_OV == 1) THEN
                  IF (MA%MP(1) == 1) THEN
                      CALL FMI2M(0,MXY(3))
                      IF (MB%MP(1) ==  1) K_EPS =  1
                      IF (MB%MP(1) == -1) K_EPS = -1
                      IF (MB%MP(3) == 0) K_EPS = 0
                  ELSE
                      CALL FMI2M(180,MXY(3))
                      IF (MB%MP(1) ==  1) K_EPS = -1
                      IF (MB%MP(1) == -1) K_EPS =  1
                      IF (MB%MP(3) == 0) K_EPS = 0
                  ENDIF
              ELSE
                  IF (MB%MP(1) == 1) THEN
                      CALL FMI2M(90,MXY(3))
                      IF (MA%MP(1) ==  1) K_EPS = -1
                      IF (MA%MP(1) == -1) K_EPS =  1
                      IF (MA%MP(3) == 0) K_EPS = 0
                  ELSE
                      CALL FMI2M(-90,MXY(3))
                      IF (MA%MP(1) ==  1) K_EPS =  1
                      IF (MA%MP(1) == -1) K_EPS = -1
                      IF (MA%MP(3) == 0) K_EPS = 0
                  ENDIF
              ENDIF
              CALL FMEQ(MC,MZ01(1))
              CALL FMEQ(MD,MZ01(2))
              KS = KRAD
              KRAD = 0
              CALL ZMARG(MZ01,MXY(4))
              KRAD = KS
              CALL FMSUB(MXY(3),MXY(4),MXY(2))
          ELSE
              IF (N_OV == 3) THEN
                  IF (MC%MP(1) == 1) THEN
                      CALL FMI2M(0,MXY(4))
                      IF (MD%MP(1) ==  1) K_EPS = -1
                      IF (MD%MP(1) == -1) K_EPS =  1
                      IF (MD%MP(3) == 0) K_EPS = 0
                  ELSE
                      CALL FMI2M(180,MXY(4))
                      IF (MD%MP(1) ==  1) K_EPS =  1
                      IF (MD%MP(1) == -1) K_EPS = -1
                      IF (MD%MP(3) == 0) K_EPS = 0
                  ENDIF
              ELSE
                  IF (MB%MP(1) == 1) THEN
                      CALL FMI2M(90,MXY(4))
                      IF (MC%MP(1) ==  1) K_EPS =  1
                      IF (MC%MP(1) == -1) K_EPS = -1
                      IF (MC%MP(3) == 0) K_EPS = 0
                  ELSE
                      CALL FMI2M(-90,MXY(4))
                      IF (MC%MP(1) ==  1) K_EPS = -1
                      IF (MC%MP(1) == -1) K_EPS =  1
                      IF (MC%MP(3) == 0) K_EPS = 0
                  ENDIF
              ENDIF
              CALL FMEQ(MA,MZ01(1))
              CALL FMEQ(MB,MZ01(2))
              KS = KRAD
              KRAD = 0
              CALL ZMARG(MZ01,MXY(3))
              KRAD = KS
              CALL FMSUB(MXY(3),MXY(4),MXY(2))
          ENDIF
          IF (MXY(2)%MP(1) < 0) THEN
              CALL FMI2M(360,MXY(5))
              CALL FMADD_R1(MXY(2),MXY(5))
          ENDIF
          CALL FMI2M(0,MXY(5))
          IF (FMCOMP(MXY(2),'==',MXY(5))) THEN
              IF (K_EPS == -1) THEN
                  IF (N_OV <= 2) THEN
                      CALL ZMST2M('OVERFLOW-OVERFLOW*i',MZ)
                      GO TO 110
                  ELSE
                      CALL ZMST2M('UNDERFLOW-UNDERFLOW*i',MZ)
                      GO TO 110
                  ENDIF
              ENDIF
              IF (K_EPS == 0) THEN
                  IF (N_OV <= 2) THEN
                      CALL ZMST2M('OVERFLOW',MZ)
                      GO TO 110
                  ELSE
                      CALL ZMST2M('UNDERFLOW',MZ)
                      GO TO 110
                  ENDIF
              ENDIF
              IF (K_EPS == 1) THEN
                  IF (N_OV <= 2) THEN
                      CALL ZMST2M('OVERFLOW+OVERFLOW*i',MZ)
                      GO TO 110
                  ELSE
                      CALL ZMST2M('UNDERFLOW+UNDERFLOW*i',MZ)
                      GO TO 110
                  ENDIF
              ENDIF
          ENDIF
          CALL FMI2M(90,MXY(5))
          IF (FMCOMP(MXY(2),'==',MXY(5))) THEN
              IF (K_EPS == -1) THEN
                  IF (N_OV <= 2) THEN
                      CALL ZMST2M('OVERFLOW+OVERFLOW*i',MZ)
                      GO TO 110
                  ELSE
                      CALL ZMST2M('UNDERFLOW+UNDERFLOW*i',MZ)
                      GO TO 110
                  ENDIF
              ENDIF
              IF (K_EPS == 0) THEN
                  IF (N_OV <= 2) THEN
                      CALL ZMST2M('0.0+OVERFLOW*i',MZ)
                      GO TO 110
                  ELSE
                      CALL ZMST2M('0.0+UNDERFLOW*i',MZ)
                      GO TO 110
                  ENDIF
              ENDIF
              IF (K_EPS == 1) THEN
                  IF (N_OV <= 2) THEN
                      CALL ZMST2M('-OVERFLOW+OVERFLOW*i',MZ)
                      GO TO 110
                  ELSE
                      CALL ZMST2M('-UNDERFLOW+UNDERFLOW*i',MZ)
                      GO TO 110
                  ENDIF
              ENDIF
          ENDIF
          CALL FMI2M(180,MXY(5))
          IF (FMCOMP(MXY(2),'==',MXY(5))) THEN
              IF (K_EPS == -1) THEN
                  IF (N_OV <= 2) THEN
                      CALL ZMST2M('-OVERFLOW+OVERFLOW*i',MZ)
                      GO TO 110
                  ELSE
                      CALL ZMST2M('-UNDERFLOW+UNDERFLOW*i',MZ)
                      GO TO 110
                  ENDIF
              ENDIF
              IF (K_EPS == 0) THEN
                  IF (N_OV <= 2) THEN
                      CALL ZMST2M('-OVERFLOW',MZ)
                      GO TO 110
                  ELSE
                      CALL ZMST2M('-UNDERFLOW',MZ)
                      GO TO 110
                  ENDIF
              ENDIF
              IF (K_EPS == 1) THEN
                  IF (N_OV <= 2) THEN
                      CALL ZMST2M('-OVERFLOW-OVERFLOW*i',MZ)
                      GO TO 110
                  ELSE
                      CALL ZMST2M('-UNDERFLOW-UNDERFLOW*i',MZ)
                      GO TO 110
                  ENDIF
              ENDIF
          ENDIF
          CALL FMI2M(270,MXY(5))
          IF (FMCOMP(MXY(2),'==',MXY(5))) THEN
              IF (K_EPS == -1) THEN
                  IF (N_OV <= 2) THEN
                      CALL ZMST2M('-OVERFLOW-OVERFLOW*i',MZ)
                      GO TO 110
                  ELSE
                      CALL ZMST2M('-UNDERFLOW-UNDERFLOW*i',MZ)
                      GO TO 110
                  ENDIF
              ENDIF
              IF (K_EPS == 0) THEN
                  IF (N_OV <= 2) THEN
                      CALL ZMST2M('0.0-OVERFLOW*i',MZ)
                      GO TO 110
                  ELSE
                      CALL ZMST2M('0.0-UNDERFLOW*i',MZ)
                      GO TO 110
                  ENDIF
              ENDIF
              IF (K_EPS == 1) THEN
                  IF (N_OV <= 2) THEN
                      CALL ZMST2M('OVERFLOW-OVERFLOW*i',MZ)
                      GO TO 110
                  ELSE
                      CALL ZMST2M('UNDERFLOW-UNDERFLOW*i',MZ)
                      GO TO 110
                  ENDIF
              ENDIF
          ENDIF
          CALL FMI2M(90,MXY(5))
          IF (FMCOMP(MXY(2),'<=',MXY(5))) THEN
              IF (N_OV <= 2) THEN
                  CALL ZMST2M('OVERFLOW+OVERFLOW*i',MZ)
                  GO TO 110
              ELSE
                  CALL ZMST2M('UNDERFLOW+UNDERFLOW*i',MZ)
                  GO TO 110
              ENDIF
          ENDIF
          CALL FMI2M(90,MXY(5))
          IF (FMCOMP(MXY(2),'<=',MXY(5))) THEN
              IF (N_OV <= 2) THEN
                  CALL ZMST2M('OVERFLOW+OVERFLOW*i',MZ)
                  GO TO 110
              ELSE
                  CALL ZMST2M('UNDERFLOW+UNDERFLOW*i',MZ)
                  GO TO 110
              ENDIF
          ENDIF
          CALL FMI2M(90,MXY(5))
          IF (FMCOMP(MXY(2),'<=',MXY(5))) THEN
              IF (N_OV <= 2) THEN
                  CALL ZMST2M('OVERFLOW+OVERFLOW*i',MZ)
                  GO TO 110
              ELSE
                  CALL ZMST2M('UNDERFLOW+UNDERFLOW*i',MZ)
                  GO TO 110
              ENDIF
          ENDIF
      ENDIF
      CALL FMSQR(MC,MXY(1))
      CALL FMSQR(MD,MXY(2))
      CALL FMMPY(MA,MC,MXY(3))
      CALL FMMPY(MB,MD,MXY(4))
      CALL FMMPY(MB,MC,MXY(5))
      CALL FMMPY(MA,MD,MXY(6))
      CALL FMMPYI_R1(MXY(6),-1)
      CALL FMADD(MXY(1),MXY(2),MXY(7))
      IF (MXY(7)%MP(2) == MUNKNO) THEN
          CALL FMST2M('UNDERFLOW',MXY(7))
      ENDIF

      CALL FMADD(MXY(3),MXY(4),MXY(8))
      IF (MXY(8)%MP(2) == MUNKNO .AND. MXY(3)%MP(3) /= 0 .AND.  &
          MXY(4)%MP(3) /= 0) THEN
          IF (MXY(3)%MP(2) == MUNKNO) THEN
              IF (MA%MP(2) == MEXPUN .AND. MC%MP(2) == MEXPOV) THEN
                  CALL ZMST2M('UNKNOWN+UNKNOWN*i',MZ)
                  GO TO 110
              ENDIF
              IF (MA%MP(2) == MEXPOV .AND. MC%MP(2) == MEXPUN) THEN
                  CALL ZMST2M('UNKNOWN+UNKNOWN*i',MZ)
                  GO TO 110
              ENDIF
          ENDIF
          IF (MXY(3)%MP(2) == MUNKNO) THEN
              IF (MB%MP(2) == MEXPUN .AND. MD%MP(2) == MEXPOV) THEN
                  CALL ZMST2M('UNKNOWN+UNKNOWN*i',MZ)
                  GO TO 110
              ENDIF
              IF (MB%MP(2) == MEXPOV .AND. MD%MP(2) == MEXPUN) THEN
                  CALL ZMST2M('UNKNOWN+UNKNOWN*i',MZ)
                  GO TO 110
              ENDIF
          ENDIF
          M_L1 = MAX( MIN( MA%MP(2) , M_EXPOV) , M_EXPUN )
          M_L2 = MAX( MIN( MC%MP(2) , M_EXPOV) , M_EXPUN )
          M_R1 = MAX( MIN( MB%MP(2) , M_EXPOV) , M_EXPUN )
          M_R2 = MAX( MIN( MD%MP(2) , M_EXPOV) , M_EXPUN )
          IF (M_L1+M_L2+NDIG < M_R1+M_R2) THEN
              CALL FMEQ(MXY(4),MXY(8))
          ELSE IF (M_R1+M_R2+NDIG < M_L1+M_L2) THEN
              CALL FMEQ(MXY(3),MXY(8))
          ELSE
              IF (MA%MP(1)*MC%MP(1) == MB%MP(1)*MD%MP(1)) THEN
                  IF ( (MA%MP(2) == MEXPOV .OR. MC%MP(2) == MEXPOV) .AND.  &
                       (MB%MP(2) == MEXPOV .OR. MD%MP(2) == MEXPOV) ) THEN
                      IF (MA%MP(2) == MEXPOV) THEN
                          CALL FMABS(MC,MXY(10))
                      ELSE
                          CALL FMABS(MA,MXY(10))
                      ENDIF
                      IF (MB%MP(2) == MEXPOV) THEN
                          CALL FMABS(MD,MXY(11))
                      ELSE
                          CALL FMABS(MB,MXY(11))
                      ENDIF
                      CALL FMADD(MXY(10),MXY(11),MXY(12))
                      IF (MXY(12)%MP(2) >= 1) THEN
                          CALL FMST2M('OVERFLOW',MXY(8))
                          MXY(8)%MP(1) = MA%MP(1)*MC%MP(1)
                      ENDIF
                  ENDIF
              ENDIF
          ENDIF
      ENDIF

      CALL FMADD(MXY(5),MXY(6),MXY(9))
      IF (MXY(9)%MP(2) == MUNKNO .AND. MXY(5)%MP(3) /= 0 .AND.  &
          MXY(6)%MP(3) /= 0) THEN
          IF (MXY(5)%MP(2) == MUNKNO) THEN
              IF (MB%MP(2) == MEXPUN .AND. MC%MP(2) == MEXPOV) THEN
                  CALL ZMST2M('UNKNOWN+UNKNOWN*i',MZ)
                  GO TO 110
              ENDIF
              IF (MB%MP(2) == MEXPOV .AND. MC%MP(2) == MEXPUN) THEN
                  CALL ZMST2M('UNKNOWN+UNKNOWN*i',MZ)
                  GO TO 110
              ENDIF
          ENDIF
          IF (MXY(5)%MP(2) == MUNKNO) THEN
              IF (MA%MP(2) == MEXPUN .AND. MD%MP(2) == MEXPOV) THEN
                  CALL ZMST2M('UNKNOWN+UNKNOWN*i',MZ)
                  GO TO 110
              ENDIF
              IF (MA%MP(2) == MEXPOV .AND. MD%MP(2) == MEXPUN) THEN
                  CALL ZMST2M('UNKNOWN+UNKNOWN*i',MZ)
                  GO TO 110
              ENDIF
          ENDIF
          M_L1 = MAX( MIN( MB%MP(2) , M_EXPOV) , M_EXPUN )
          M_L2 = MAX( MIN( MC%MP(2) , M_EXPOV) , M_EXPUN )
          M_R1 = MAX( MIN( MA%MP(2) , M_EXPOV) , M_EXPUN )
          M_R2 = MAX( MIN( MD%MP(2) , M_EXPOV) , M_EXPUN )
          IF (M_L1+M_L2+NDIG < M_R1+M_R2) THEN
              CALL FMEQ(MXY(6),MXY(9))
          ELSE IF (M_R1+M_R2+NDIG < M_L1+M_L2) THEN
              CALL FMEQ(MXY(5),MXY(9))
          ELSE
              IF (MB%MP(1)*MC%MP(1) == -MA%MP(1)*MD%MP(1)) THEN
                  IF ( (MB%MP(2) == MEXPOV .OR. MC%MP(2) == MEXPOV) .AND.  &
                       (MA%MP(2) == MEXPOV .OR. MD%MP(2) == MEXPOV) ) THEN
                      IF (MB%MP(2) == MEXPOV) THEN
                          CALL FMABS(MC,MXY(10))
                      ELSE
                          CALL FMABS(MB,MXY(10))
                      ENDIF
                      IF (MA%MP(2) == MEXPOV) THEN
                          CALL FMABS(MD,MXY(11))
                      ELSE
                          CALL FMABS(MA,MXY(11))
                      ENDIF
                      CALL FMADD(MXY(10),MXY(11),MXY(12))
                      IF (MXY(12)%MP(2) >= 1) THEN
                          CALL FMST2M('OVERFLOW',MXY(9))
                          MXY(9)%MP(1) = MB%MP(1)*MC%MP(1)
                      ENDIF
                  ENDIF
              ENDIF
          ENDIF
      ENDIF

      CALL FMDIV(MXY(8),MXY(7),MZ(1))
      CALL FMDIV(MXY(9),MXY(7),MZ(2))

  110 RETURN
      END SUBROUTINE ZMDIV_UNOV

      SUBROUTINE ZMDIV_R1(MA,MB)

!  MA = MA / MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (INOUT) :: MA
      INTENT (IN) :: MB

      TYPE(MULTI) :: MXY(2)


      CALL ZMDIV(MA,MB,MXY)
      CALL ZMEQ(MXY,MA)


      END SUBROUTINE ZMDIV_R1

      SUBROUTINE ZMDIV_R2(MA,MB)

!  MB = MA / MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB

      TYPE(MULTI) :: MXY(2)


      CALL ZMDIV(MA,MB,MXY)
      CALL ZMEQ(MXY,MB)


      END SUBROUTINE ZMDIV_R2

      SUBROUTINE ZMDIVI(MA,INTEG,MB)

!  MB = MA / INTEG        Divide by one-word (real) integer.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      INTEGER :: INTEG

      INTEGER :: KOVUN,KRESLT,KWRNSV,NDSAVE,NTRSAV
      REAL (KIND(1.0D0)) :: MXSAVE
      INTENT (IN) :: MA,INTEG
      INTENT (INOUT) :: MB

      IF (.NOT. ALLOCATED(MB(1)%MP)) THEN
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(1)%MP)
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MB(2)%MP)) THEN
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(2)%MP)
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (ABS(MA(1)%MP(2)) > MEXPAB .OR. ABS(MA(2)%MP(2)) > MEXPAB .OR.  &
          KDEBUG >= 1) THEN
          NTRSAV = NTRACE
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'ZMDIVI'
              CALL ZMNTR(2,MA,MA,1)
              CALL FMNTRI(2,INTEG,0)
              NCALL = NCALL - 1
          ENDIF
          NTRACE = 0
          CALL ZMENTR('ZMDIVI   ',MA,MA,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
          NTRACE = NTRSAV
          IF (KRESLT /= 0) THEN
              NCALL = NCALL + 1
              IF (NTRACE /= 0) CALL ZMNTR(1,MB,MB,1)
              NCALL = NCALL - 1
              RETURN
          ENDIF
          NDIG = NDSAVE
          MXEXP = MXSAVE
      ELSE
          NCALL = NCALL + 1
          IF (NTRACE /= 0) THEN
              NAMEST(NCALL) = 'ZMDIVI'
              CALL ZMNTR(2,MA,MA,1)
              CALL FMNTRI(2,INTEG,0)
          ENDIF
          KOVUN = 0
      ENDIF

!             Force FMDIVI to use more guard digits for user calls.

      NCALL = NCALL - 1
      NTRSAV = NTRACE
      NTRACE = 0
      KWRNSV = KWARN
      KWARN = 0

      CALL FMDIVI(MA(1),INTEG,MB(1))
      CALL FMDIVI(MA(2),INTEG,MB(2))

      NTRACE = NTRSAV
      KWARN = KWRNSV
      NCALL = NCALL + 1
      IF (NTRACE /= 0) NAMEST(NCALL) = 'ZMDIVI'
      IF (MB(1)%MP(2) == MUNKNO .OR. MB(2)%MP(2) == MUNKNO) THEN
          KFLAG = -4
      ELSE IF (MB(1)%MP(2) == MEXPOV .OR. MB(2)%MP(2) == MEXPOV) THEN
          KFLAG = -5
      ELSE IF (MB(1)%MP(2) == MEXPUN .OR. MB(2)%MP(2) == MEXPUN) THEN
          KFLAG = -6
      ENDIF
      IF ((MB(1)%MP(2) == MUNKNO) .OR. (MB(2)%MP(2) == MUNKNO)  &
         .OR. (MB(1)%MP(2) == MEXPUN .AND. KOVUN == 0)          &
         .OR. (MB(2)%MP(2) == MEXPUN .AND. KOVUN == 0)          &
         .OR. (MB(1)%MP(2) == MEXPOV .AND. KOVUN == 0)          &
         .OR. (MB(2)%MP(2) == MEXPOV .AND. KOVUN == 0)) THEN
          NAMEST(NCALL) = 'ZMDIVI'
          CALL ZMWARN
      ENDIF
      IF (NTRACE /= 0) CALL ZMNTR(1,MB,MB,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE ZMDIVI

      SUBROUTINE ZMDIVI_R1(MA,IVAL)

!  MA = MA / IVAL

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2)
      INTEGER :: IVAL
      INTENT (INOUT) :: MA
      INTENT (IN) :: IVAL

      TYPE(MULTI) :: MXY(2)


      CALL ZMDIVI(MA,IVAL,MXY)
      CALL ZMEQ(MXY,MA)


      END SUBROUTINE ZMDIVI_R1

      SUBROUTINE ZMENTR(NROUTN,MA,MB,NARGS,MC,KRESLT,NDSAVE,MXSAVE,KOVUN)

!  Do the argument checking and increasing of precision, overflow threshold, etc., upon entry to
!  a ZM routine.

!  NROUTN - routine name of calling routine
!  MA     - first input argument
!  MB     - second input argument (optional)
!  NARGS  - number of input arguments
!  MC     - result argument
!  KRESLT - returned nonzero if the input arguments give the result immediately
!           (e.g., MA*0 or OVERFLOW*MB)
!  NDSAVE - saves the value of NDIG after NDIG is increased
!  MXSAVE - saves the value of MXEXP
!  KOVUN  - returned nonzero if an input argument is (+ or -) overflow or underflow.

      USE ModLib_FMVALS
      IMPLICIT NONE

      CHARACTER(9) :: NROUTN
      TYPE(MULTI) :: MA(2),MB(2),MC(2)
      REAL (KIND(1.0D0)) :: MXSAVE
      INTEGER :: NARGS,KRESLT,NDSAVE,KOVUN

      REAL (KIND(1.0D0)) :: MBS
      INTEGER :: J,KWRNSV,NDS
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC

      KRESLT = 0
      NCALL = NCALL + 1
      KFLAG = 0
      NAMEST(NCALL) = NROUTN
      IF (NTRACE /= 0) CALL ZMNTR(2,MA,MB,NARGS)

      IF (MBLOGS /= MBASE) CALL FMCONS
      KOVUN = 0
      IF (MA(1)%MP(2) == MEXPOV .OR. MA(1)%MP(2) == MEXPUN .OR.  &
          MA(2)%MP(2) == MEXPOV .OR. MA(2)%MP(2) == MEXPUN) KOVUN = 1
      IF (NARGS == 2) THEN
          IF (MB(1)%MP(2) == MEXPOV .OR. MB(1)%MP(2) == MEXPUN .OR.  &
          MB(2)%MP(2) == MEXPOV .OR. MB(2)%MP(2) == MEXPUN) KOVUN = 1
      ENDIF
      MXSAVE = MXEXP

!             Check the validity of parameters if this is a user call.

      IF (NCALL > 1 .AND. KDEBUG == 0) GO TO 130

!             Check NDIG.

      IF (NDIG < 2) THEN
          KFLAG = -1
          CALL ZMWARN
          NDS = NDIG
          IF (NDIG < 2) NDIG = 2
          WRITE (KW,                                                      &
                 "(' NDIG was',I10,'.  It has been changed to',I10,'.')"  &
                ) NDS,NDIG
          KRESLT = 12
          GO TO 130
      ENDIF

!             Check MBASE.

      IF (MBASE < 2 .OR. MBASE > MXBASE) THEN
          KFLAG = -2
          CALL ZMWARN
          MBS = MBASE
          IF (MBASE < 2) MBASE = 2
          IF (MBASE > MXBASE) MBASE = MXBASE
          WRITE (KW,                                                       &
                 "(' MBASE was',I10,'.  It has been changed to',I10,'.')"  &
                ) INT(MBS),INT(MBASE)
          CALL FMCONS
          KRESLT = 12
          GO TO 130
      ENDIF

!             Check exponent range.

      IF (MA(1)%MP(2) > MXEXP+1 .OR. MA(1)%MP(2) < -MXEXP) THEN
          IF ((ABS(MA(1)%MP(2)) /= MEXPOV .AND. ABS(MA(1)%MP(2)) /= MUNKNO) .OR.  &
              ABS(MA(1)%MP(3)) /= 1) THEN
              KFLAG = -3
              CALL ZMWARN
              KRESLT = 12
              GO TO 130
          ENDIF
      ENDIF
      IF (MA(2)%MP(2) > MXEXP+1 .OR. MA(2)%MP(2) < -MXEXP) THEN
          IF ((ABS(MA(2)%MP(2)) /= MEXPOV .AND. ABS(MA(2)%MP(2)) /= MUNKNO) .OR.  &
              ABS(MA(2)%MP(3)) /= 1) THEN
              KFLAG = -3
              CALL ZMWARN
              KRESLT = 12
              GO TO 130
          ENDIF
      ENDIF
      IF (NARGS == 2) THEN
          IF (MB(1)%MP(2) > MXEXP+1 .OR. MB(1)%MP(2) < -MXEXP) THEN
              IF ((ABS(MB(1)%MP(2)) /= MEXPOV .AND.  &
                   ABS(MB(1)%MP(2)) /= MUNKNO) .OR. ABS(MB(1)%MP(3)) /= 1) THEN
                  KFLAG = -3
                  CALL ZMWARN
                  KRESLT = 12
                  GO TO 130
              ENDIF
          ENDIF
          IF (MB(2)%MP(2) > MXEXP+1 .OR. MB(2)%MP(2) < -MXEXP) THEN
              IF ((ABS(MB(2)%MP(2)) /= MEXPOV .AND.  &
                   ABS(MB(2)%MP(2)) /= MUNKNO) .OR. ABS(MB(2)%MP(3)) /= 1) THEN
                  KFLAG = -3
                  CALL ZMWARN
                  KRESLT = 12
                  GO TO 130
              ENDIF
          ENDIF
      ENDIF

!             Check for properly normalized digits in the input arguments.

      IF (ABS(MA(1)%MP(2)-INT(MA(1)%MP(2))) /= 0) KFLAG = 1
      IF (ABS(MA(2)%MP(2)-INT(MA(2)%MP(2))) /= 0) KFLAG = 1 + NDIG + 1
      IF (MA(1)%MP(3) <= (-1) .OR. MA(1)%MP(3) >= MBASE .OR.  &
          ABS(MA(1)%MP(3)-INT(MA(1)%MP(3))) /= 0) KFLAG = 2
      IF (MA(2)%MP(3) <= (-1) .OR. MA(2)%MP(3) >= MBASE .OR.  &
          ABS(MA(2)%MP(3)-INT(MA(2)%MP(3))) /= 0) KFLAG = 2 + NDIG + 1
      IF (KDEBUG == 0) GO TO 110
      DO J = 3, NDIG+1
         IF (MA(1)%MP(J+1) < 0 .OR. MA(1)%MP(J+1) >= MBASE .OR.  &
             ABS(MA(1)%MP(J+1)-INT(MA(1)%MP(J+1))) /= 0) THEN
             KFLAG = J
             GO TO 110
         ENDIF
      ENDDO
      DO J = 3, NDIG+1
         IF (MA(2)%MP(J+1) < 0 .OR. MA(2)%MP(J+1) >= MBASE .OR.  &
             ABS(MA(2)%MP(J+1)-INT(MA(2)%MP(J+1))) /= 0) THEN
             KFLAG = J + NDIG + 1
             GO TO 110
         ENDIF
      ENDDO
  110 IF (KFLAG /= 0) THEN
          J = KFLAG
          KFLAG = -4
          KWRNSV = KWARN
          IF (KWARN >= 2) KWARN = 1
          CALL ZMWARN
          KWARN = KWRNSV
          IF (KWARN >= 1) THEN
              IF (J < NDIG+1) THEN
                  WRITE (KW,*) ' First invalid array element:  MA(',  &
                               J,',1) = ',MA(1)%MP(J+1)
              ELSE
                  WRITE (KW,*) ' First invalid array element:  MA(',  &
                               J-NDIG-1,',2) = ',MA(2)%MP(J+1)
              ENDIF
          ENDIF
          IF (KWARN >= 2) THEN
              STOP
          ENDIF
          KRESLT = 12
          GO TO 130
      ENDIF
      IF (NARGS == 2) THEN
          IF (ABS(MB(1)%MP(2)-INT(MB(1)%MP(2))) /= 0) KFLAG = 1
          IF (ABS(MB(2)%MP(2)-INT(MB(2)%MP(2))) /= 0) KFLAG = 1 + NDIG + 1
          IF (MB(1)%MP(3) <= (-1) .OR. MB(1)%MP(3) >= MBASE .OR.  &
              ABS(MB(1)%MP(3)-INT(MB(1)%MP(3))) /= 0) KFLAG = 2
          IF (MB(2)%MP(3) <= (-1) .OR. MB(2)%MP(3) >= MBASE .OR.  &
              ABS(MB(2)%MP(3)-INT(MB(2)%MP(3))) /= 0) KFLAG = 2 + NDIG + 1
          IF (KDEBUG == 0) GO TO 120
          DO J = 3, NDIG+1
             IF (MB(1)%MP(J+1) < 0 .OR. MB(1)%MP(J+1) >= MBASE .OR.  &
                 ABS(MB(1)%MP(J+1)-INT(MB(1)%MP(J+1))) /= 0) THEN
                 KFLAG = J
                 GO TO 120
             ENDIF
          ENDDO
          DO J = 3, NDIG+1
             IF (MB(2)%MP(J+1) < 0 .OR. MB(2)%MP(J+1) >= MBASE .OR.  &
                 ABS(MB(2)%MP(J+1)-INT(MB(2)%MP(J+1))) /= 0) THEN
                 KFLAG = J + NDIG + 1
                 GO TO 120
             ENDIF
          ENDDO
  120     IF (KFLAG /= 0) THEN
              J = KFLAG
              KFLAG = -4
              KWRNSV = KWARN
              IF (KWARN >= 2) KWARN = 1
              CALL ZMWARN
              KWARN = KWRNSV
              IF (KWARN >= 1) THEN
                  IF (J < NDIG+1) THEN
                      WRITE (KW,*) ' First invalid array element:  MB(',  &
                                   J,',1) = ',MB(1)%MP(J+1)
                  ELSE
                      WRITE (KW,*) ' First invalid array element:  MB(',  &
                                   J-NDIG-1,',2) = ',MB(2)%MP(J+1)
                  ENDIF
              ENDIF
              IF (KWARN >= 2) THEN
                  STOP
              ENDIF
              KRESLT = 12
              GO TO 130
          ENDIF
      ENDIF

!             Increase the working precision.

  130 NDSAVE = NDIG
      IF (NCALL == 1) THEN
          NDIG = MAX(NDIG+NGRD52,2)
          IF (MBASE >= 100*ABS(MA(1)%MP(3)) .OR. MBASE >= 100*ABS(MA(2)%MP(3))) THEN
              NDIG = NDIG + 1
          ELSE IF (NARGS == 2 .AND. (MBASE >= 100*ABS(MB(1)%MP(3)) .OR.  &
              MBASE >= 100*ABS(MB(2)%MP(3)))) THEN
              NDIG = NDIG + 1
          ENDIF
      ENDIF
      IF ((MA(1)%MP(2) == MUNKNO .AND. MA(2)%MP(2) == MUNKNO) .OR.  &
          (MB(1)%MP(2) == MUNKNO .AND. MB(2)%MP(2) == MUNKNO)) THEN
          KFLAG = -4
          KRESLT = 12
      ENDIF
      IF (KRESLT /= 0) THEN
          NDIG = NDSAVE
          CALL ZMRSLT(MC,KRESLT)
          IF (NTRACE /= 0) CALL ZMNTR(1,MC,MC,1)
          NCALL = NCALL - 1
          RETURN
      ENDIF

!             Extend the overflow/underflow threshold.

      MXEXP = MXEXP2
      RETURN
      END SUBROUTINE ZMENTR

      SUBROUTINE ZMEQ(MA,MB)

!  MB = MA

!  This is the standard form of equality, where MA and MB both have precision NDIG.
!  Use ZMEQU for assignments that also change precision.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB

      CALL FMEQ(MA(1),MB(1))
      CALL FMEQ(MA(2),MB(2))
      RETURN
      END SUBROUTINE ZMEQ

      SUBROUTINE ZMEQU(MA,MB,NDA,NDB)

!  Set MB (having NDB digits) equal to MA (having NDA digits).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      INTEGER :: NDA,NDB
      INTENT (IN) :: NDA,NDB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB

      CALL FMEQU(MA(1),MB(1),NDA,NDB)
      CALL FMEQU(MA(2),MB(2),NDA,NDB)
      RETURN
      END SUBROUTINE ZMEQU

      SUBROUTINE ZMEQU_R1(MA,NDA,NDB)

!  Change precision of MA from NDA digits on input to NDB digits on output.

!  If NDB is less than NDA the result is rounded to NDB digits.

!  If NDB is greater than NDA the result has zero digits padded on the right.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2)
      INTEGER :: NDA,NDB
      INTENT (IN) :: NDA,NDB
      INTENT (INOUT) :: MA

      CALL FMEQU_R1(MA(1),NDA,NDB)
      CALL FMEQU_R1(MA(2),NDA,NDB)
      RETURN
      END SUBROUTINE ZMEQU_R1

      SUBROUTINE ZMEXIT(MT,MC,NDSAVE,MXSAVE,KOVUN)

!  Upon exit from an ZM routine the result MT (having precision NDIG) is rounded and returned in
!  MC (having precision NDSAVE).
!  The values of NDIG, MXEXP, and KACCSW are restored to the values NDSAVE,MXSAVE.
!  KOVUN is nonzero if one of the routine's input arguments was overflow or underflow.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MT(2),MC(2)
      REAL (KIND(1.0D0)) :: MXSAVE
      INTEGER :: NDSAVE,KOVUN

      INTEGER :: KFSAVE,KWRNSV
      INTENT (IN) :: MT
      INTENT (INOUT) :: MC,NDSAVE,MXSAVE,KOVUN

      KWRNSV = KWARN
      KWARN = 0
      MXEXP = MXSAVE
      KFSAVE = KFLAG
      CALL ZMEQU(MT,MC,NDIG,NDSAVE)
      IF (KFLAG /= -5 .AND. KFLAG /= -6) KFLAG = KFSAVE
      NDIG = NDSAVE
      KWARN = KWRNSV
      IF (KFLAG == 1) KFLAG = 0
      IF (MC(1)%MP(2) == MEXPUN .OR. MC(2)%MP(2) == MEXPUN) KFLAG = -6
      IF (MC(1)%MP(2) == MEXPOV .OR. MC(2)%MP(2) == MEXPOV) KFLAG = -5
      IF (MC(1)%MP(2) == MUNKNO .OR. MC(2)%MP(2) == MUNKNO) THEN
          IF (KFLAG /= -9) KFLAG = -4
      ENDIF
      IF ((MC(1)%MP(2) == MUNKNO .AND. KFLAG /= -9) .OR.  &
          (MC(2)%MP(2) == MUNKNO .AND. KFLAG /= -9) .OR.  &
          (MC(1)%MP(2) == MEXPUN .AND. KOVUN == 0)  .OR.  &
          (MC(2)%MP(2) == MEXPUN .AND. KOVUN == 0)  .OR.  &
          (MC(1)%MP(2) == MEXPOV .AND. KOVUN == 0)  .OR.  &
          (MC(2)%MP(2) == MEXPOV .AND. KOVUN == 0)) CALL ZMWARN
      IF (NTRACE /= 0) CALL ZMNTR(1,MC,MC,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE ZMEXIT

      SUBROUTINE ZMEXI2(MXFM,MYFM,NDSAVE,MXSAVE,KOVUN)

!  This routine is used upon exit for complex functions that return real FM results.
!  Round MXFM and return the result in MYFM.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MXFM,MYFM
      REAL (KIND(1.0D0)) :: MXSAVE
      INTEGER :: NDSAVE,KOVUN
      INTENT (IN) :: MXFM
      INTENT (INOUT) :: MYFM,NDSAVE,MXSAVE,KOVUN

      INTEGER :: KFSAVE,KWRNSV

      KWRNSV = KWARN
      KWARN = 0
      MXEXP = MXSAVE
      KFSAVE = KFLAG
      CALL FMEQU(MXFM,MYFM,NDIG,NDSAVE)
      IF (KFLAG /= -5 .AND. KFLAG /= -6) KFLAG = KFSAVE
      NDIG = NDSAVE
      KWARN = KWRNSV
      IF (KFLAG == 1) KFLAG = 0
      IF (MYFM%MP(2) == MUNKNO) THEN
          IF (KFLAG >= 0) KFLAG = -4
      ELSE IF (MYFM%MP(2) == MEXPOV) THEN
          KFLAG = -5
      ELSE IF (MYFM%MP(2) == MEXPUN) THEN
          KFLAG = -6
      ENDIF
      IF ((MYFM%MP(2) == MUNKNO .AND. KFLAG /= -9)     &
         .OR. (MYFM%MP(2) == MEXPUN .AND. KOVUN == 0)  &
         .OR. (MYFM%MP(2) == MEXPOV .AND. KOVUN == 0)) CALL ZMWARN
      IF (NTRACE /= 0) CALL ZMNTR2(1,MYFM,MYFM,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE ZMEXI2

      SUBROUTINE ZMEXP(MA,MB)

!  MB = EXP(MA).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,KL,KOVUN,KR_RETRY,KRESLT,KRSAVE,KWRNSV,NDSAVE
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(2),MZ01(2),MZ02(2)

      IF (.NOT. ALLOCATED(MB(1)%MP)) THEN
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(1)%MP)
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MB(2)%MP)) THEN
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(2)%MP)
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL ZMENTR('ZMEXP    ',MA,MA,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF
      KR_RETRY = 0
      KRSAVE = KRAD
      KRAD = 1

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL ZMEQU(MA,MZ02,NDSAVE,NDIG)

!             Check for special cases.

      IF (MA(1)%MP(3) == 0 .AND. MA(2)%MP(3) == 0) THEN
          CALL ZMI2M(1,MZ01)
          GO TO 120
      ELSE IF (MA(1)%MP(3) == 0) THEN
          CALL FMI2M(1,MXY(1))
      ELSE
          CALL FMEXP(MZ02(1),MXY(1))
          IF (MA(2)%MP(3) == 0) THEN
              CALL FMEQ(MXY(1),MZ01(1))
              CALL FMI2M(0,MZ01(2))
              GO TO 120
          ENDIF
      ENDIF

      CALL FMCSSN(MZ02(2),MZ01(1),MZ01(2))

      KWRNSV = KWARN
      KWARN = 0
      IF (MXY(1)%MP(2) == MEXPOV) THEN
          CALL FMEQ(MZ02(1),MZ02(2))
          CALL FMABS(MZ01(1),MXY(1))
          CALL FMLN(MXY(1),MXY(2))
          CALL FMADD_R2(MZ02(2),MXY(2))
          CALL FMEXP(MXY(2),MZ02(1))
          IF (MZ01(1)%MP(1) < 0) CALL FMMPYI_R1(MZ02(1),-1)

          CALL FMABS(MZ01(2),MXY(1))
          CALL FMLN(MXY(1),MXY(2))
          CALL FMADD_R2(MZ02(2),MXY(2))
          CALL FMEXP(MXY(2),MZ02(2))
          IF (MZ01(2)%MP(1) < 0) CALL FMMPYI_R1(MZ02(2),-1)
      ELSE
          CALL FMMPYD(MXY(1),MZ01(1),MZ01(2),MZ02(1),MZ02(2))
      ENDIF
      CALL ZMEQ(MZ02,MZ01)
      KWARN = KWRNSV

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  120 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(1)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL ZMEXIT(MZ01,MB,NDSAVE,MXSAVE,KOVUN)
      KRAD = KRSAVE
      RETURN
      END SUBROUTINE ZMEXP

      SUBROUTINE ZMFORM(FORM1,FORM2,MA,STRING)

!  Convert MA to STRING using FORM1 format for the real part and FORM2 format for the
!  imaginary part.

      USE ModLib_FMVALS
      IMPLICIT NONE

      CHARACTER(*) :: FORM1,FORM2,STRING
      TYPE(MULTI) :: MA(2)

      INTEGER :: J,KWIDIM,KWIDRE,LAST,LSIGN
      INTENT (IN) :: MA,FORM1,FORM2
      INTENT (INOUT) :: STRING
      TYPE(MULTI) :: MXY(2)


      NCALL = NCALL + 1
      NAMEST(NCALL) = 'ZMFORM'
      STRING = ' '
      CALL ZMFPCZ(FORM1,MA(1),KWIDRE)
      CALL FMEQ(MA(2),MXY(1))
      IF (MXY(1)%MP(1) > 0) THEN
          LSIGN = 1
      ELSE
          LSIGN = -1
          IF (MXY(1)%MP(2) /= MUNKNO .AND. MXY(1)%MP(3) /= 0)  &
              MXY(1)%MP(1) = -MXY(1)%MP(1)
      ENDIF
      CALL ZMFPCM(FORM2,MXY(1),KWIDIM)

      IF (KWIDRE+KWIDIM+50 > LMBUFZ) THEN
          IF (LMBUFZ > 0) DEALLOCATE(CMBUFZ)
          ALLOCATE(CMBUFZ(KWIDRE+KWIDIM+50),STAT=J)
          IF (J /= 0) THEN
              CALL FMDEFINE_ERROR
          ENDIF
          LMBUFZ = KWIDRE + KWIDIM + 50
          CALL ZMFPCZ(FORM1,MA(1),KWIDRE)
      ENDIF

      CMBUFZ(KWIDRE+1) = ' '
      IF (LSIGN == 1) THEN
          CMBUFZ(KWIDRE+2) = '+'
      ELSE
          CMBUFZ(KWIDRE+2) = '-'
      ENDIF
      CMBUFZ(KWIDRE+3) = ' '
      DO J = 1, KWIDIM
         CMBUFZ(KWIDRE+3+J) = CMBUFF(J)
      ENDDO
      CMBUFZ(KWIDRE+4+KWIDIM) = ' '
      CMBUFZ(KWIDRE+5+KWIDIM) = 'i'
      IF (JFORMZ == 2) CMBUFZ(KWIDRE+5+KWIDIM) = 'I'
      LAST = KWIDRE + KWIDIM + 5

      IF (MXY(1)%MP(2) == MEXPOV .OR. MXY(1)%MP(2) == MEXPUN) THEN
          DO J = KWIDRE+3, LAST
             IF (CMBUFZ(J) == 'O' .OR. CMBUFZ(J) == 'U') THEN
                 CMBUFZ(J-2) = ' '
                 EXIT
             ENDIF
          ENDDO
      ENDIF

      IF (LAST <= LEN(STRING)) THEN
          DO J = 1, LAST
             STRING(J:J) = CMBUFZ(J)
          ENDDO
      ELSE
          DO J = 1, LAST
             STRING(J:J) = '*'
          ENDDO
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE ZMFORM

      SUBROUTINE ZMFPCM(FORM,MA,KWI)

!  Internal routine to convert MA to base 10 using FORM format. The result is returned in CMBUFF and
!  the field width is KWI.

      USE ModLib_FMVALS
      IMPLICIT NONE

      CHARACTER(*) :: FORM
      TYPE(MULTI) :: MA
      INTEGER :: KWI
      DOUBLE PRECISION :: VAL
      INTEGER :: J,JF1SAV,JF2SAV,JPT,K1,K2,K3,KD,KWD,KSAVE,LAST,LB,LENGFM,LFIRST,ND,NEXP
      INTENT (IN) :: MA,FORM
      INTENT (INOUT) :: KWI
      TYPE(MULTI) :: MXY(2)


      KSAVE = KFLAG
      JF1SAV = JFORM1
      JF2SAV = JFORM2
      LENGFM = LEN(FORM)
      KWI = 75
      KWD = 40
      IF (INDEX(FORM,'I') > 0 .OR. INDEX(FORM,'i') > 0) THEN
          K1 = MAX(INDEX(FORM,'I'),INDEX(FORM,'i')) + 1
          K2 = LENGFM
          IF (K2 >= K1) THEN
              CALL FMST2D(FORM(K1:K2),VAL)
              KWI = NINT(VAL)
          ELSE
              KWI = 50
          ENDIF
          KWI = MAX(1,KWI)
          JFORM1 = 2
          JFORM2 = 0
          KWD = KWI + 11
          IF (KWD+50 > LMBUFF) THEN
              IF (LMBUFF > 0) DEALLOCATE(CMBUFF)
              ALLOCATE(CMBUFF(KWD+50),STAT=J)
              IF (J /= 0) THEN
                  CALL FMDEFINE_ERROR
              ENDIF
              LMBUFF = KWD + 50
          ENDIF
          CALL FMNINT(MA,MXY(1))
          IF (MXY(1)%MP(3) /= 0) THEN
              CALL FMOUT(MXY(1),CMBUFF,KWD)
          ELSE
              DO J = 1, KWD
                 CMBUFF(J) = ' '
              ENDDO
              CMBUFF(2) = '0'
          ENDIF
          LFIRST = 1
          LAST = 1
          DO J = 1, KWD
             IF (CMBUFF(KWD+1-J) /= ' ') LFIRST = KWD+1-J
             IF (CMBUFF(J) /= ' ') LAST = J
          ENDDO
          JPT = 1
          IF (LAST-LFIRST+1 > KWI) GO TO 110
          IF (LAST <= KWI) THEN
              DO J = LAST, LFIRST, -1
                 JPT = KWI - LAST + J
                 CMBUFF(JPT) = CMBUFF(J)
              ENDDO
              DO J = 1, JPT-1
                 CMBUFF(J) = ' '
              ENDDO
          ELSE
              DO J = LFIRST, LAST
                 JPT = KWI - LAST + J
                 CMBUFF(JPT) = CMBUFF(J)
              ENDDO
          ENDIF
      ELSE IF (INDEX(FORM,'F') > 0 .OR. INDEX(FORM,'f') > 0) THEN
          K1 = MAX(INDEX(FORM,'F'),INDEX(FORM,'f')) + 1
          K2 = INDEX(FORM(1:LENGFM),'.')
          K3 = LENGFM
          IF (K2 > K1) THEN
              CALL FMST2D(FORM(K1:K2-1),VAL)
              KWI = NINT(VAL)
          ELSE
              KWI = 50
          ENDIF
          IF (K3 > K2) THEN
              CALL FMST2D(FORM(K2+1:K3),VAL)
              KD = NINT(VAL)
          ELSE
              KD = 0
          ENDIF
          KWI = MAX(1,KWI)
          KD = MAX(0,MIN(KD,KWI-2))
          JFORM1 = 2
          JFORM2 = KD
          ND = INT(REAL(NDIG)*LOG10(REAL(MBASE))) + 1
          IF (ND < 2) ND = 2
          NEXP = INT(2.0*LOG10(REAL(MXBASE))) + 16
          LB = MAX(JFORM2+NEXP,ND+NEXP)
          KWD = LB
          IF (KWD+50 > LMBUFF) THEN
              IF (LMBUFF > 0) DEALLOCATE(CMBUFF)
              ALLOCATE(CMBUFF(KWD+50),STAT=J)
              IF (J /= 0) THEN
                  CALL FMDEFINE_ERROR
              ENDIF
              LMBUFF = KWD + 50
          ENDIF
          CALL FMOUT(MA,CMBUFF,KWD)
          LFIRST = 1
          LAST = 1
          DO J = 1, KWD
             IF (CMBUFF(KWD+1-J) /= ' ') LFIRST = KWD+1-J
             IF (CMBUFF(J) /= ' ') LAST = J
          ENDDO
          IF (LAST-LFIRST+1 > KWI) THEN

!             Not enough room for this F format, or FMOUT converted it to E format to avoid
!             showing no significant digits.  See if a shortened form will fit in E format.

              NEXP = INT(LOG10((ABS(MA%MP(2))+1)*LOG10(DBLE(MBASE))+1)+1)
              ND = KWI - NEXP - 5
              IF (ND < 1) THEN
                  GO TO 110
              ELSE
                  JFORM1 = 0
                  JFORM2 = ND
                  IF (KWI+50 > LMBUFF) THEN
                      IF (LMBUFF > 0) DEALLOCATE(CMBUFF)
                      ALLOCATE(CMBUFF(KWI+50),STAT=J)
                      IF (J /= 0) THEN
                          CALL FMDEFINE_ERROR
                      ENDIF
                      LMBUFF = KWI + 50
                  ENDIF
                  CALL FMOUT(MA,CMBUFF,KWI)
                  LFIRST = 1
                  LAST = 1
                  DO J = 1, KWI
                     IF (CMBUFF(KWI+1-J) /= ' ') LFIRST = KWI+1-J
                     IF (CMBUFF(J) /= ' ') LAST = J
                  ENDDO
              ENDIF
          ENDIF
          JPT = 1
          IF (LAST <= KWI) THEN
              DO J = LAST, LFIRST, -1
                 JPT = KWI - LAST + J
                 CMBUFF(JPT) = CMBUFF(J)
              ENDDO
              DO J = 1, JPT-1
                 CMBUFF(J) = ' '
              ENDDO
          ELSE
              DO J = LFIRST, LAST
                 JPT = KWI - LAST + J
                 CMBUFF(JPT) = CMBUFF(J)
              ENDDO
          ENDIF
      ELSE IF (INDEX(FORM,'1PE') > 0 .OR. INDEX(FORM,'1pe') > 0 .OR. INDEX(FORM,'ES') > 0 .OR.  &
               INDEX(FORM,'es') > 0) THEN
          IF (INDEX(FORM,'1PE') > 0 .OR. INDEX(FORM,'1pe') > 0) THEN
              K1 = MAX(INDEX(FORM,'E'),INDEX(FORM,'e')) + 1
          ELSE
              K1 = MAX(INDEX(FORM,'S'),INDEX(FORM,'s')) + 1
          ENDIF
          K2 = INDEX(FORM(1:LENGFM),'.')
          K3 = LENGFM
          IF (K2 > K1) THEN
              CALL FMST2D(FORM(K1:K2-1),VAL)
              KWI = NINT(VAL)
          ELSE
              KWI = 50
          ENDIF
          IF (K3 > K2) THEN
              CALL FMST2D(FORM(K2+1:K3),VAL)
              KD = NINT(VAL)
          ELSE
              KD = 0
          ENDIF
          KWI = MAX(1,KWI)
          KD = MAX(0,MIN(KD,KWI-2))
          JFORM1 = 1
          JFORM2 = KD + 1
          IF (KWI+50 > LMBUFF) THEN
              IF (LMBUFF > 0) DEALLOCATE(CMBUFF)
              ALLOCATE(CMBUFF(KWI+50),STAT=J)
              IF (J /= 0) THEN
                  CALL FMDEFINE_ERROR
              ENDIF
              LMBUFF = KWI + 50
          ENDIF
          CALL FMOUT(MA,CMBUFF,KWI)
      ELSE IF (INDEX(FORM,'E') > 0 .OR. INDEX(FORM,'e') > 0) THEN
          K1 = MAX(INDEX(FORM,'E'),INDEX(FORM,'e')) + 1
          K2 = INDEX(FORM(1:LENGFM),'.')
          K3 = LENGFM
          IF (K2 > K1) THEN
              CALL FMST2D(FORM(K1:K2-1),VAL)
              KWI = NINT(VAL)
          ELSE
              KWI = 50
          ENDIF
          IF (K3 > K2) THEN
              CALL FMST2D(FORM(K2+1:K3),VAL)
              KD = NINT(VAL)
          ELSE
              KD = 0
          ENDIF
          KWI = MAX(1,KWI)
          KD = MAX(0,MIN(KD,KWI-2))
          JFORM1 = 0
          JFORM2 = KD
          IF (KWI+50 > LMBUFF) THEN
              IF (LMBUFF > 0) DEALLOCATE(CMBUFF)
              ALLOCATE(CMBUFF(KWI+50),STAT=J)
              IF (J /= 0) THEN
                  CALL FMDEFINE_ERROR
              ENDIF
              LMBUFF = KWI + 50
          ENDIF
          CALL FMOUT(MA,CMBUFF,KWI)
      ELSE
          GO TO 110
      ENDIF

      JFORM1 = JF1SAV
      JFORM2 = JF2SAV
      KFLAG = KSAVE
      RETURN

!             Error condition.

  110 KFLAG = -8
      DO J = 1, KWI
         CMBUFF(J) = '*'
      ENDDO
      JFORM1 = JF1SAV
      JFORM2 = JF2SAV
      KFLAG = KSAVE
      RETURN
      END SUBROUTINE ZMFPCM

      SUBROUTINE ZMFPCZ(FORM,MA,KWI)

!  Internal routine to convert MA to base 10 using FORM format. The result is returned in CMBUFZ and
!  the field width is KWI.

      USE ModLib_FMVALS
      IMPLICIT NONE

      CHARACTER(*) :: FORM
      TYPE(MULTI) :: MA
      INTEGER :: KWI
      DOUBLE PRECISION :: VAL
      INTEGER :: J,JF1SAV,JF2SAV,JPT,K1,K2,K3,KD,KWD,KSAVE,LAST,LB,LENGFM,LFIRST,ND,NEXP
      INTENT (IN) :: MA,FORM
      INTENT (INOUT) :: KWI
      TYPE(MULTI) :: MXY(2)


      KSAVE = KFLAG
      JF1SAV = JFORM1
      JF2SAV = JFORM2
      LENGFM = LEN(FORM)
      KWI = 75
      KWD = 40
      IF (INDEX(FORM,'I') > 0 .OR. INDEX(FORM,'i') > 0) THEN
          K1 = MAX(INDEX(FORM,'I'),INDEX(FORM,'i')) + 1
          K2 = LENGFM
          IF (K2 >= K1) THEN
              CALL FMST2D(FORM(K1:K2),VAL)
              KWI = NINT(VAL)
          ELSE
              KWI = 50
          ENDIF
          KWI = MAX(1,KWI)
          JFORM1 = 2
          JFORM2 = 0
          KWD = KWI + 11
          IF (KWD+50 > LMBUFZ) THEN
              IF (LMBUFZ > 0) DEALLOCATE(CMBUFZ)
              ALLOCATE(CMBUFZ(KWD+50),STAT=J)
              IF (J /= 0) THEN
                  CALL FMDEFINE_ERROR
              ENDIF
              LMBUFZ = KWD + 50
          ENDIF
          CALL FMNINT(MA,MXY(1))
          IF (MXY(1)%MP(3) /= 0) THEN
              CALL FMOUT(MXY(1),CMBUFZ,KWD)
          ELSE
              DO J = 1, KWD
                 CMBUFZ(J) = ' '
              ENDDO
              CMBUFZ(2) = '0'
          ENDIF
          LFIRST = 1
          LAST = 1
          DO J = 1, KWD
             IF (CMBUFZ(KWD+1-J) /= ' ') LFIRST = KWD+1-J
             IF (CMBUFZ(J) /= ' ') LAST = J
          ENDDO
          JPT = 1
          IF (LAST-LFIRST+1 > KWI) GO TO 110
          IF (LAST <= KWI) THEN
              DO J = LAST, LFIRST, -1
                 JPT = KWI - LAST + J
                 CMBUFZ(JPT) = CMBUFZ(J)
              ENDDO
              DO J = 1, JPT-1
                 CMBUFZ(J) = ' '
              ENDDO
          ELSE
              DO J = LFIRST, LAST
                 JPT = KWI - LAST + J
                 CMBUFZ(JPT) = CMBUFZ(J)
              ENDDO
          ENDIF
      ELSE IF (INDEX(FORM,'F') > 0 .OR. INDEX(FORM,'f') > 0) THEN
          K1 = MAX(INDEX(FORM,'F'),INDEX(FORM,'f')) + 1
          K2 = INDEX(FORM(1:LENGFM),'.')
          K3 = LENGFM
          IF (K2 > K1) THEN
              CALL FMST2D(FORM(K1:K2-1),VAL)
              KWI = NINT(VAL)
          ELSE
              KWI = 50
          ENDIF
          IF (K3 > K2) THEN
              CALL FMST2D(FORM(K2+1:K3),VAL)
              KD = NINT(VAL)
          ELSE
              KD = 0
          ENDIF
          KWI = MAX(1,KWI)
          KD = MAX(0,MIN(KD,KWI-2))
          JFORM1 = 2
          JFORM2 = KD
          ND = INT(REAL(NDIG)*LOG10(REAL(MBASE))) + 1
          IF (ND < 2) ND = 2
          NEXP = INT(2.0*LOG10(REAL(MXBASE))) + 16
          LB = MAX(JFORM2+NEXP,ND+NEXP)
          KWD = LB
          IF (KWD+50 > LMBUFZ) THEN
              IF (LMBUFZ > 0) DEALLOCATE(CMBUFZ)
              ALLOCATE(CMBUFZ(KWD+50),STAT=J)
              IF (J /= 0) THEN
                  CALL FMDEFINE_ERROR
              ENDIF
              LMBUFZ = KWD + 50
          ENDIF
          CALL FMOUT(MA,CMBUFZ,KWD)
          LFIRST = 1
          LAST = 1
          DO J = 1, KWD
             IF (CMBUFZ(KWD+1-J) /= ' ') LFIRST = KWD+1-J
             IF (CMBUFZ(J) /= ' ') LAST = J
          ENDDO
          IF (LAST-LFIRST+1 > KWI) THEN

!             Not enough room for this F format, or FMOUT converted it to E format to avoid
!             showing no significant digits.  See if a shortened form will fit in E format.

              NEXP = INT(LOG10((ABS(MA%MP(2))+1)*LOG10(DBLE(MBASE))+1)+1)
              ND = KWI - NEXP - 5
              IF (ND < 1) THEN
                  GO TO 110
              ELSE
                  JFORM1 = 0
                  JFORM2 = ND
                  IF (KWI+50 > LMBUFZ) THEN
                      IF (LMBUFZ > 0) DEALLOCATE(CMBUFZ)
                      ALLOCATE(CMBUFZ(KWI+50),STAT=J)
                      IF (J /= 0) THEN
                          CALL FMDEFINE_ERROR
                      ENDIF
                      LMBUFZ = KWI + 50
                  ENDIF
                  CALL FMOUT(MA,CMBUFZ,KWI)
                  LFIRST = 1
                  LAST = 1
                  DO J = 1, KWI
                     IF (CMBUFZ(KWI+1-J) /= ' ') LFIRST = KWI+1-J
                     IF (CMBUFZ(J) /= ' ') LAST = J
                  ENDDO
              ENDIF
          ENDIF
          JPT = 1
          IF (LAST <= KWI) THEN
              DO J = LAST, LFIRST, -1
                 JPT = KWI - LAST + J
                 CMBUFZ(JPT) = CMBUFZ(J)
              ENDDO
              DO J = 1, JPT-1
                 CMBUFZ(J) = ' '
              ENDDO
          ELSE
              DO J = LFIRST, LAST
                 JPT = KWI - LAST + J
                 CMBUFZ(JPT) = CMBUFZ(J)
              ENDDO
          ENDIF
      ELSE IF (INDEX(FORM,'1PE') > 0 .OR. INDEX(FORM,'1pe') > 0 .OR. INDEX(FORM,'ES') > 0 .OR.  &
               INDEX(FORM,'es') > 0) THEN
          IF (INDEX(FORM,'1PE') > 0 .OR. INDEX(FORM,'1pe') > 0) THEN
              K1 = MAX(INDEX(FORM,'E'),INDEX(FORM,'e')) + 1
          ELSE
              K1 = MAX(INDEX(FORM,'S'),INDEX(FORM,'s')) + 1
          ENDIF
          K2 = INDEX(FORM(1:LENGFM),'.')
          K3 = LENGFM
          IF (K2 > K1) THEN
              CALL FMST2D(FORM(K1:K2-1),VAL)
              KWI = NINT(VAL)
          ELSE
              KWI = 50
          ENDIF
          IF (K3 > K2) THEN
              CALL FMST2D(FORM(K2+1:K3),VAL)
              KD = NINT(VAL)
          ELSE
              KD = 0
          ENDIF
          KWI = MAX(1,KWI)
          KD = MAX(0,MIN(KD,KWI-2))
          JFORM1 = 1
          JFORM2 = KD + 1
          IF (KWI+50 > LMBUFZ) THEN
              IF (LMBUFZ > 0) DEALLOCATE(CMBUFZ)
              ALLOCATE(CMBUFZ(KWI+50),STAT=J)
              IF (J /= 0) THEN
                  CALL FMDEFINE_ERROR
              ENDIF
              LMBUFZ = KWI + 50
          ENDIF
          CALL FMOUT(MA,CMBUFZ,KWI)
      ELSE IF (INDEX(FORM,'E') > 0 .OR. INDEX(FORM,'e') > 0) THEN
          K1 = MAX(INDEX(FORM,'E'),INDEX(FORM,'e')) + 1
          K2 = INDEX(FORM(1:LENGFM),'.')
          K3 = LENGFM
          IF (K2 > K1) THEN
              CALL FMST2D(FORM(K1:K2-1),VAL)
              KWI = NINT(VAL)
          ELSE
              KWI = 50
          ENDIF
          IF (K3 > K2) THEN
              CALL FMST2D(FORM(K2+1:K3),VAL)
              KD = NINT(VAL)
          ELSE
              KD = 0
          ENDIF
          KWI = MAX(1,KWI)
          KD = MAX(0,MIN(KD,KWI-2))
          JFORM1 = 0
          JFORM2 = KD
          IF (KWI+50 > LMBUFZ) THEN
              IF (LMBUFZ > 0) DEALLOCATE(CMBUFZ)
              ALLOCATE(CMBUFZ(KWI+50),STAT=J)
              IF (J /= 0) THEN
                  CALL FMDEFINE_ERROR
              ENDIF
              LMBUFZ = KWI + 50
          ENDIF
          CALL FMOUT(MA,CMBUFZ,KWI)
      ELSE
          GO TO 110
      ENDIF

      JFORM1 = JF1SAV
      JFORM2 = JF2SAV
      KFLAG = KSAVE
      RETURN

!             Error condition.

  110 KFLAG = -8
      DO J = 1, KWI
         CMBUFZ(J) = '*'
      ENDDO
      JFORM1 = JF1SAV
      JFORM2 = JF2SAV
      KFLAG = KSAVE
      RETURN
      END SUBROUTINE ZMFPCZ

      SUBROUTINE ZMFPRT(FORM1,FORM2,MA)

!  Print MA in base 10 using FORM1 format for the real part and FORM2 format for the imaginary part.

      USE ModLib_FMVALS
      IMPLICIT NONE

      CHARACTER(*) :: FORM1,FORM2
      TYPE(MULTI) :: MA(2)

      CHARACTER(20) :: FORM
      INTEGER :: J,K,KWIDIM,KWIDRE,LAST,LSIGN
      INTENT (IN) :: MA,FORM1,FORM2
      TYPE(MULTI) :: MXY(2)


      NCALL = NCALL + 1
      NAMEST(NCALL) = 'ZMFPRT'

      CALL ZMFPCZ(FORM1,MA(1),KWIDRE)
      CALL FMEQ(MA(2),MXY(1))
      IF (MXY(1)%MP(1) >= 0) THEN
          LSIGN = 1
      ELSE
          LSIGN = -1
          IF (MXY(1)%MP(2) /= MUNKNO .AND. MXY(1)%MP(3) /= 0)  &
              MXY(1)%MP(1) = -MXY(1)%MP(1)
      ENDIF
      CALL ZMFPCM(FORM2,MXY(1),KWIDIM)

      IF (KWIDRE+KWIDIM+50 > LMBUFZ) THEN
          IF (LMBUFZ > 0) DEALLOCATE(CMBUFZ)
          ALLOCATE(CMBUFZ(KWIDRE+KWIDIM+50),STAT=J)
          IF (J /= 0) THEN
              CALL FMDEFINE_ERROR
          ENDIF
          LMBUFZ = KWIDRE + KWIDIM + 50
          CALL ZMFPCZ(FORM1,MA(1),KWIDRE)
      ENDIF

      CMBUFZ(KWIDRE+1) = ' '
      IF (LSIGN == 1) THEN
          CMBUFZ(KWIDRE+2) = '+'
      ELSE
          CMBUFZ(KWIDRE+2) = '-'
      ENDIF
      CMBUFZ(KWIDRE+3) = ' '
      DO J = 1, KWIDIM
         CMBUFZ(KWIDRE+3+J) = CMBUFF(J)
      ENDDO
      CMBUFZ(KWIDRE+4+KWIDIM) = ' '
      CMBUFZ(KWIDRE+5+KWIDIM) = 'i'
      IF (JFORMZ == 2) CMBUFZ(KWIDRE+5+KWIDIM) = 'I'
      LAST = KWIDRE + KWIDIM + 5

      IF (MXY(1)%MP(2) == MEXPOV .OR. MXY(1)%MP(2) == MEXPUN) THEN
          DO J = KWIDRE+3, LAST
             IF (CMBUFZ(J) == 'O' .OR. CMBUFZ(J) == 'U') THEN
                 CMBUFZ(J-2) = ' '
                 EXIT
             ENDIF
          ENDDO
      ENDIF

      WRITE (FORM,"(' (6X,',I3,'A1) ')") KSWIDE-7
      WRITE (KW,FORM) (CMBUFZ(K),K=1,LAST)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE ZMFPRT

      SUBROUTINE ZMI2M(INTEG,MA)

!  MA = INTEG

!  The real part of MA is set to the one word integer value INTEG.
!  The imaginary part is set to zero.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: INTEG
      TYPE(MULTI) :: MA(2)
      INTENT (IN) :: INTEG
      INTENT (INOUT) :: MA

      NCALL = NCALL + 1
      NAMEST(NCALL) = 'ZMI2M'
      IF (NTRACE /= 0) CALL ZMNTRI(2,INTEG,1)

      CALL FMI2M(INTEG,MA(1))
      CALL FMI2M(0,MA(2))

      IF (NTRACE /= 0) CALL ZMNTR(1,MA,MA,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE ZMI2M

      SUBROUTINE ZM2I2M(INTEG1,INTEG2,MA)

!  MA = INTEG1 + INTEG2 i

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: INTEG1,INTEG2
      TYPE(MULTI) :: MA(2)
      INTENT (IN) :: INTEG1,INTEG2
      INTENT (INOUT) :: MA

      NCALL = NCALL + 1
      NAMEST(NCALL) = 'ZM2I2M'
      IF (NTRACE /= 0) THEN
          CALL ZMNTRI(2,INTEG1,1)
          CALL ZMNTRI(2,INTEG2,0)
      ENDIF

      CALL FMI2M(INTEG1,MA(1))
      CALL FMI2M(INTEG2,MA(2))

      IF (NTRACE /= 0) CALL ZMNTR(1,MA,MA,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE ZM2I2M

      SUBROUTINE ZMIMAG(MA,MBFM)

!  MBFM = IMAG(MA)        imaginary part of MA

!  MA is a complex ZM number, MBFM is a real FM number.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MBFM
      INTENT (IN) :: MA
      INTENT (INOUT) :: MBFM

      KFLAG = 0
      NCALL = NCALL + 1
      NAMEST(NCALL) = 'ZMIMAG'
      IF (NTRACE /= 0) CALL ZMNTR(2,MA,MA,1)

      CALL FMEQ(MA(2),MBFM)

      IF (NTRACE /= 0) CALL FMNTR(1,MBFM,MBFM,1,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE ZMIMAG

      SUBROUTINE ZMINP(LINE,MA,LA,LB)

!  Convert an A1 character string to floating point multiple precision complex format.

!  LINE is an A1 character array of length LB to be converted to ZM format and returned in MA.
!  LA is a pointer telling the routine where in the array to begin the conversion.  This allows more
!     than one number to be stored in an array and converted in place.
!  LB is a pointer to the last character of the field for that number.

!  The input numbers may be in integer or any real format.
!  In exponential format the 'E' may also be 'D', 'Q', or 'M'.

!  The following are all valid input strings:

!  1.23 + 4.56 I
!  1.23 + 4.56*I
!  2 + i
!  -i
!  1.23
!  4.56i
!  ( 1.23 , 4.56 )

!  So that ZMINP will convert any output from ZMOUT, LINE is tested to see if the input contains any
!  of the special symbols +OVERFLOW, -OVERFLOW, +UNDERFLOW, -UNDERFLOW, or UNKNOWN.
!  For user input the abbreviations OVFL, UNFL, UNKN may be used.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: LA,LB
      CHARACTER :: LINE(LB)
      TYPE(MULTI) :: MA(2)
      INTEGER :: J,JSTATE,K,KDIGFL,KFLAG1,KIFLAG,KPT,KRSAVE,KSIGN,KSTART,KSTOP,KSTOPI,  &
                 KSTOPR,KSTRTI,KSTRTR,KTYPE,KVAL,NDSAVE,NTRSAV

!  Simulate a finite-state automaton to scan the input line and build the number.
!  States 2-8 refer to the real part, states 10-16 refer to the imaginary part.
!  States of the machine:

!   1.  Initial entry to the subroutine
!   2.  Sign of the number
!   3.  Scanning digits before a decimal point
!   4.  Decimal point
!   5.  Scanning digits after a decimal point
!   6.  E, D, Q, or M - precision indicator before the exponent
!   7.  Sign of the exponent
!   8.  Scanning exponent
!   9.  Comma between the real and imaginary part
!  10.  Sign of the number
!  11.  Scanning digits before a decimal point
!  12.  Decimal point
!  13.  Scanning digits after a decimal point
!  14.  E, D, Q, or M - precision indicator before the exponent
!  15.  Sign of the exponent
!  16.  Scanning exponent
!  17.  Syntax error

!  Character types recognized by the machine:

!  1.  Sign (+,-)
!  2.  Numeral (0,1,...,9)
!  3.  Decimal point (.)
!  4.  Precision indicator (E,D,Q,M)
!  5.  Illegal character for number
!  6.  Comma (,)
!  7.  Character to be ignored   ' '    '('    ')'    '*'

!  All blanks are ignored.  The analysis of the number proceeds as follows:  If the simulated
!  machine is in state JSTATE and a character of type JTYPE is encountered the new state of the
!  machine is given by JTRANS(JSTATE,JTYPE).

!  State  1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16

      INTEGER :: JTRANS(16,4) = RESHAPE(  (/                                  &
          2, 17, 10, 10, 10,  7, 17, 10, 10, 17, 17, 17, 17, 15, 17, 17,      &
          3,  3,  3,  5,  5,  8,  8,  8, 11, 11, 11, 13, 13, 16, 16, 16,      &
          4,  4,  4, 17, 17, 17, 17, 17, 12, 12, 12, 17, 17, 17, 17, 17,      &
          6,  6,  6,  6,  6,  8, 17, 17, 14, 14, 14, 14, 14, 16, 17, 17   /)  &
        , (/ 16,4 /) )
      INTENT (IN) :: LINE,LA,LB
      INTENT (INOUT) :: MA
      TYPE(MULTI) :: MXY(2)

      IF (.NOT. ALLOCATED(MA(1)%MP)) THEN
          ALLOCATE(MA(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MA(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MA(1)%MP)
          ALLOCATE(MA(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MA(2)%MP)) THEN
          ALLOCATE(MA(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MA(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MA(2)%MP)
          ALLOCATE(MA(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS
      NCALL = NCALL + 1
      NAMEST(NCALL) = 'ZMINP'
      NDSAVE = NDIG
      KRSAVE = KROUND
      KROUND = 1
      KFLAG = 0

!             Initialize two hash tables that are used for character look-up during
!             input conversion.

      IF (LHASH == 0) CALL FMHTBL

!             Since arithmetic tracing is not usually desired during I/O conversion, disable
!             tracing during this routine.

      NTRSAV = NTRACE
      NTRACE = 0

!             Increase the working precision.

      IF (NCALL <= 2) THEN
          K = NGRD52
          NDIG = MAX(NDIG+K,2)
      ENDIF
      KSTART = LA
      KSTOP = LB
      JSTATE = 1
      KSTRTR = 0
      KSTOPR = 0
      KSTRTI = 0
      KSTOPI = 0
      KDIGFL = 0
      KIFLAG = 0
      KSIGN = 1

!             Scan the number.

      DO J = KSTART, KSTOP
         IF (LINE(J) == ' ' .OR. LINE(J) == '(' .OR. LINE(J) == ')'  &
             .OR. LINE(J) == '*') CYCLE
         IF (LINE(J) == 'I' .OR. LINE(J) == 'i') THEN
             KIFLAG = 1
             IF (KSTRTI == 0) THEN
                 KSTRTI = KSTRTR
                 KSTOPI = KSTOPR
                 KSTRTR = 0
                 KSTOPR = 0
             ENDIF
             CYCLE
         ENDIF

         KPT = ICHAR(LINE(J))
         IF (KPT < LHASH1 .OR. KPT > LHASH2) THEN
             WRITE (KW,                                                       &
                "(/' Error in input conversion.'/"                        //  &
                "' ICHAR function was out of range for the current',"     //  &
                "' dimensions.'/' ICHAR(''',A,''') gave the value ',"     //  &
                "I12,', which is outside the currently'/' dimensioned',"  //  &
                "' bounds of (',I5,':',I5,') for variables KHASHT ',"     //  &
                "'and KHASHV.'/' Re-define the two parameters ',"         //  &
                "'LHASH1 and LHASH2 so the dimensions will'/' contain',"  //  &
                "' all possible output values from ICHAR.'//)"                &
                   ) LINE(J),KPT,LHASH1,LHASH2
             KTYPE = 5
             KVAL  = 0
         ELSE
             KTYPE = KHASHT(KPT)
             KVAL  = KHASHV(KPT)
         ENDIF
         IF (KTYPE == 2 .OR. KTYPE == 5) KDIGFL = 1
         IF (LINE(J) == ',') THEN
             IF (JSTATE < 9) THEN
                 JSTATE = 9
             ELSE
                 GO TO 110
             ENDIF
         ELSE
             IF (KTYPE >= 5) KTYPE = 2
             IF (JSTATE < 17) JSTATE = JTRANS(JSTATE,KTYPE)
         ENDIF
         IF (JSTATE == 9 .OR. JSTATE == 10) KDIGFL = 0
         IF (JSTATE == 2 .OR. JSTATE == 10) KSIGN = KVAL

         IF (JSTATE >= 2 .AND. JSTATE <= 8) THEN
             IF (KSTRTR == 0) KSTRTR = J
             KSTOPR = J
         ENDIF
         IF (JSTATE >= 10 .AND. JSTATE <= 16) THEN
             IF (KSTRTI == 0) KSTRTI = J
             KSTOPI = J
         ENDIF

      ENDDO

!             Form the number and return.

      IF (KSTRTR > 0) THEN
          NCALL = NCALL - 1
          CALL FMINP(LINE,MXY(1),KSTRTR,KSTOPR)
          NCALL = NCALL + 1
      ELSE
          CALL FMIM(0,MXY(1))
      ENDIF
      KFLAG1 = KFLAG

      IF (KSTRTI > 0) THEN
          IF (KIFLAG == 1 .AND. KDIGFL == 0) THEN
              CALL FMIM(KSIGN,MXY(2))
          ELSE
              NCALL = NCALL - 1
              CALL FMINP(LINE,MXY(2),KSTRTI,KSTOPI)
              NCALL = NCALL + 1
          ENDIF
      ELSE IF (KIFLAG == 1) THEN
          CALL FMIM(1,MXY(2))
      ELSE
          CALL FMIM(0,MXY(2))
      ENDIF

      IF (KFLAG1 /= 0 .OR. KFLAG /= 0 .OR. JSTATE == 17) GO TO 110
      CALL FMEQU(MXY(1),MA(1),NDIG,NDSAVE)
      CALL FMEQU(MXY(2),MA(2),NDIG,NDSAVE)
      GO TO 120

!             Error in converting the number.

  110 KFLAG = -7
      CALL ZMWARN
      MA(1)%MP(1) = 1
      MA(1)%MP(2) = MUNKNO
      MA(1)%MP(3) = 1
      MA(2)%MP(1) = 1
      MA(2)%MP(2) = MUNKNO
      MA(2)%MP(3) = 1
      DO J = 2, NDSAVE
         MA(1)%MP(J+2) = 0
         MA(2)%MP(J+2) = 0
      ENDDO

  120 NDIG = NDSAVE
      NTRACE = NTRSAV
      KROUND = KRSAVE
      IF (KFLAG /= -7) KFLAG = 0
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE ZMINP

      SUBROUTINE ZMINT(MA,MB)

!  MB = INT(MA)

!  The integer parts of both real and imaginary values are returned.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB

      NCALL = NCALL + 1
      NAMEST(NCALL) = 'ZMINT'
      IF (NTRACE /= 0) CALL ZMNTR(2,MA,MA,1)

      CALL FMINT(MA(1),MB(1))
      CALL FMINT(MA(2),MB(2))

      IF (NTRACE /= 0) CALL ZMNTR(1,MB,MB,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE ZMINT

      SUBROUTINE ZMIPWR(MA,IVAL,MB)

!  MB = MA ** IVAL

!  Raise a ZM number to an integer power.
!  The binary multiplication method used requires an average of 1.5 * LOG2(IVAL) multiplications.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: IVAL
      TYPE(MULTI) :: MA(2),MB(2)
      REAL (KIND(1.0D0)) :: MA2,MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: I2N,J,K,KL,KOVUN,KR_RETRY,KWRNSV,LVLSAV,NDSAVE
      REAL :: XVAL
      INTENT (IN) :: MA,IVAL
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MZ02(2),MZ03(2),MZ04(2),MZ05(2)

      IF (.NOT. ALLOCATED(MB(1)%MP)) THEN
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(1)%MP)
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MB(2)%MP)) THEN
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(2)%MP)
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NCALL = NCALL + 1
      NAMEST(NCALL) = 'ZMIPWR'
      NDSAVE = NDIG
      IF (NTRACE /= 0) THEN
          CALL ZMNTR(2,MA,MA,1)
          CALL FMNTRI(2,IVAL,0)
      ENDIF
      KOVUN = 0
      KR_RETRY = 0
      IF (MA(1)%MP(2) == MEXPOV .OR. MA(1)%MP(2) == MEXPUN .OR.  &
          MA(2)%MP(2) == MEXPOV .OR. MA(2)%MP(2) == MEXPUN) KOVUN = 1

      IF (MBLOGS /= MBASE) CALL FMCONS
      KFLAG = 0
      MXSAVE = MXEXP
      MXEXP = MXEXP2

!             Check for special cases.

      IF (IVAL == 1) THEN
          NCALL = NCALL - 1
          J = NTRACE
          NTRACE = 0
          CALL ZMEQ(MA,MZ05)
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'ZMIPWR'
          NTRACE = J
          GO TO 140
      ELSE IF (IVAL == 2) THEN
          NCALL = NCALL - 1
          J = NTRACE
          NTRACE = 0
          K = KWARN
          KWARN = 0
          CALL ZMSQR(MA,MZ05)
          NCALL = NCALL + 1
          NAMEST(NCALL) = 'ZMIPWR'
          NTRACE = J
          KWARN = K
          GO TO 140
      ENDIF
      IF (MA(1)%MP(2) == MUNKNO .OR. MA(2)%MP(2) == MUNKNO .OR.  &
          (IVAL <= 0 .AND. MA(1)%MP(3) == 0 .AND. MA(2)%MP(3) == 0)) THEN
          MA2 = MA(1)%MP(3)
          KFLAG = -4
          IF (IVAL <= 0 .AND. MA2 == 0) CALL ZMWARN
          CALL ZMST2M('UNKNOWN+UNKNOWN*i',MB)
          IF (NTRACE /= 0) CALL ZMNTR(1,MB,MB,1)
          NCALL = NCALL - 1
          MXEXP = MXSAVE
          RETURN
      ENDIF

      IF (IVAL == 0) THEN
          CALL ZMI2M(1,MB)
          IF (NTRACE /= 0) CALL ZMNTR(1,MB,MB,1)
          NCALL = NCALL - 1
          MXEXP = MXSAVE
          RETURN
      ENDIF

      IF (MA(1)%MP(3) == 0 .AND. MA(2)%MP(3) == 0) THEN
          CALL ZMI2M(0,MB)
          IF (NTRACE /= 0) CALL ZMNTR(1,MB,MB,1)
          NCALL = NCALL - 1
          MXEXP = MXSAVE
          RETURN
      ENDIF

      IF (MA(2)%MP(3) == 0) THEN
          NCALL = NCALL - 1
          LVLSAV = LVLTRC
          LVLTRC = LVLTRC - 1
          CALL FMIPWR(MA(1),IVAL,MB(1))
          CALL FMIM(0,MB(2))
          NCALL = NCALL + 1
          LVLTRC = LVLSAV
          IF (NTRACE /= 0) THEN
              NAMEST(NCALL) = 'ZMIPWR'
              CALL ZMNTR(1,MB,MB,1)
          ENDIF
          NCALL = NCALL - 1
          MXEXP = MXSAVE
          RETURN
      ENDIF

      IF (MA(1)%MP(3) == 0) THEN
          NCALL = NCALL - 1
          LVLSAV = LVLTRC
          LVLTRC = LVLTRC - 1
          IF (IVAL >= 0) THEN
              I2N = MOD(IVAL,4)
          ELSE
              I2N = MOD(4 - MOD(ABS(IVAL),4),4)
          ENDIF
          IF (I2N == 0) THEN
              CALL FMIPWR(MA(2),IVAL,MB(1))
              CALL FMIM(0,MB(2))
          ELSE IF (I2N == 1) THEN
              CALL FMIPWR(MA(2),IVAL,MB(2))
              CALL FMIM(0,MB(1))
          ELSE IF (I2N == 2) THEN
              CALL FMIPWR(MA(2),IVAL,MB(1))
              CALL FMIM(0,MB(2))
              IF (MB(1)%MP(2) /= MUNKNO .AND. MB(1)%MP(3) /= 0)  &
                  MB(1)%MP(1) = -MB(1)%MP(1)
          ELSE IF (I2N == 3) THEN
              CALL FMIPWR(MA(2),IVAL,MB(2))
              CALL FMIM(0,MB(1))
              IF (MB(2)%MP(2) /= MUNKNO .AND. MB(2)%MP(3) /= 0)  &
                  MB(2)%MP(1) = -MB(2)%MP(1)
          ENDIF
          NCALL = NCALL + 1
          LVLTRC = LVLSAV
          IF (NTRACE /= 0) THEN
              NAMEST(NCALL) = 'ZMIPWR'
              CALL ZMNTR(1,MB,MB,1)
          ENDIF
          NCALL = NCALL - 1
          MXEXP = MXSAVE
          RETURN
      ENDIF

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(2*NDIG,2*NDSAVE+10)
      ENDIF
      IF (ABS(IVAL) == 1) THEN
          KWRNSV = KWARN
          KWARN = 0
          IF (IVAL == 1) THEN
              CALL ZMEQ(MA,MZ05)
          ELSE
              K = INT((5.0D0*DLOGTN)/DLOGMB + 2.0D0)
              NDIG = MAX(NDIG+K,2)
              CALL ZMI2M(1,MZ02)
              CALL ZMEQU(MA,MZ03,NDSAVE,NDIG)
              CALL ZMDIV(MZ02,MZ03,MZ05)
          ENDIF
          KWARN = KWRNSV
          GO TO 130
      ENDIF

!             Increase the working precision.

      IF (NCALL == 1) THEN
          XVAL = ABS(IVAL) + 1
          K = INT((5.0*REAL(DLOGTN) + 2.5*LOG(XVAL))/ALOGMB + 3.0)
          IF (MBASE <= 1000) K = 2*K
          NDIG = MAX(NDIG+K,2)
      ELSE
          XVAL = ABS(IVAL) + 1
          K = INT(LOG(XVAL)/ALOGMB + 1.0)
          NDIG = NDIG + K
      ENDIF

!             Initialize.

      KWRNSV = KWARN
      KWARN = 0
      K = ABS(IVAL)

      CALL ZMEQU(MA,MZ02,NDSAVE,NDIG)

      IF (MOD(K,2) == 0) THEN
          CALL ZMI2M(1,MZ05)
      ELSE
          CALL ZMEQ(MZ02,MZ05)
      ENDIF

!             This is the multiplication loop.

  120 K = K/2
      CALL ZMSQR(MZ02,MZ04)
      CALL ZMEQ(MZ04,MZ02)
      IF (MOD(K,2) == 1) THEN
          CALL ZMMPY(MZ02,MZ05,MZ04)
          CALL ZMEQ(MZ04,MZ05)
      ENDIF
      IF (K > 1) GO TO 120

!             Invert if the exponent is negative.

      IF (IVAL < 0) THEN
          CALL ZMI2M(1,MZ02)
          CALL ZMDIV(MZ02,MZ05,MZ04)
          CALL ZMEQ(MZ04,MZ05)
      ENDIF
      KWARN = KWRNSV

!             Round the result and return.

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  130 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ05(1)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ05(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
  140 CALL ZMEXIT(MZ05,MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE ZMIPWR

      SUBROUTINE ZMLG10(MA,MB)

!  MB = LOG10(MA).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,KL,KOVUN,KR_RETRY,KRESLT,KRSAVE,NDSAVE
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(2),MZ01(2),MZ02(2),MZ03(2)

      IF (.NOT. ALLOCATED(MB(1)%MP)) THEN
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(1)%MP)
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MB(2)%MP)) THEN
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(2)%MP)
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL ZMENTR('ZMLG10   ',MA,MA,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF
      KR_RETRY = 0
      KRSAVE = KRAD
      KRAD = 1

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL ZMEQU(MA,MZ03,NDSAVE,NDIG)
      CALL ZMLN(MZ03,MZ02)
      CALL FMLNI(10,MXY(1))
      CALL FMDIVD(MZ02(1),MZ02(2),MXY(1),MZ01(1),MZ01(2))

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(1)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL ZMEXIT(MZ01,MB,NDSAVE,MXSAVE,KOVUN)
      KRAD = KRSAVE
      RETURN
      END SUBROUTINE ZMLG10

      SUBROUTINE ZMLN(MA,MB)

!  MB = LN(MA).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,KF1,KL,KOVUN,KR_RETRY,KRESLT,KRSAVE,NDSAVE
      LOGICAL, EXTERNAL :: FMCOMP
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(4),MZ01(2),MZ02(2)

      IF (.NOT. ALLOCATED(MB(1)%MP)) THEN
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(1)%MP)
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MB(2)%MP)) THEN
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(2)%MP)
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL ZMENTR('ZMLN     ',MA,MA,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF
      KR_RETRY = 0
      KRSAVE = KRAD
      KRAD = 1

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL ZMEQU(MA,MZ02,NDSAVE,NDIG)

!             Check for special cases.

      IF (MA(1)%MP(3) == 0 .AND. MA(2)%MP(3) == 0) THEN
          KFLAG = -4
          CALL ZMST2M('UNKNOWN+UNKNOWN*i',MZ01)
          GO TO 120
      ELSE IF (MA(2)%MP(3) == 0) THEN
          IF (MA(1)%MP(1) < 0) THEN
              CALL FMEQ(MZ02(1),MZ01(1))
              IF (MZ01(1)%MP(2) /= MUNKNO .AND. MZ01(1)%MP(3) /= 0)  &
                  MZ01(1)%MP(1) = -MZ01(1)%MP(1)
              CALL FMLN(MZ01(1),MXY(4))
              CALL FMEQ(MXY(4),MZ01(1))
              CALL FMPI(MZ01(2))
          ELSE
              CALL FMLN(MZ02(1),MZ01(1))
              CALL FMI2M(0,MZ01(2))
          ENDIF
          GO TO 120
      ELSE IF (MA(1)%MP(3) == 0) THEN
          IF (MA(2)%MP(1) < 0) THEN
              CALL FMEQ(MZ02(2),MZ01(1))
              IF (MZ01(1)%MP(2) /= MUNKNO .AND. MZ01(1)%MP(3) /= 0)  &
                  MZ01(1)%MP(1) = -MZ01(1)%MP(1)
              CALL FMLN(MZ01(1),MXY(4))
              CALL FMEQ(MXY(4),MZ01(1))
              CALL FMPI(MZ01(2))
              CALL FMDIVI_R1(MZ01(2),-2)
          ELSE
              CALL FMLN(MZ02(2),MZ01(1))
              CALL FMPI(MZ01(2))
              CALL FMDIVI_R1(MZ01(2),2)
          ENDIF
          GO TO 120
      ENDIF

!             Ln(a + b i) = Ln(Abs(a + b i)) + Arg(a + b i) i.

      CALL FMABS(MZ02(1),MXY(1))
      CALL FMABS(MZ02(2),MXY(2))

!             Check for cancellation in Ln(x).

      CALL FMI2M(1,MXY(3))
      KF1 = 0
      IF (FMCOMP(MXY(1),'==',MXY(3)) .AND. MXY(2)%MP(2) <= (-NDIG)) KF1 = 1
      IF (FMCOMP(MXY(2),'==',MXY(3)) .AND. MXY(1)%MP(2) <= (-NDIG)) KF1 = 1

      IF (FMCOMP(MXY(1),'>=',MXY(2))) THEN
          CALL FMSUB(MZ02(1),MXY(3),MXY(1))
          CALL FMADD(MZ02(1),MXY(3),MXY(2))
          CALL FMMPY_R1(MXY(1),MXY(2))
          CALL FMSQR(MZ02(2),MXY(2))
          CALL FMADD_R2(MXY(1),MXY(2))
      ELSE
          CALL FMSUB(MZ02(2),MXY(3),MXY(1))
          CALL FMADD(MZ02(2),MXY(3),MXY(2))
          CALL FMMPY_R1(MXY(1),MXY(2))
          CALL FMSQR(MZ02(1),MXY(2))
          CALL FMADD_R2(MXY(1),MXY(2))
      ENDIF
      CALL ZMABS(MZ02,MZ01(1))
      CALL FMADD(MZ01(1),MXY(3),MXY(1))
      CALL FMDIV_R2(MXY(2),MXY(1))
      IF (KF1 == 1) THEN
          CALL FMEQ(MXY(1),MZ01(1))
          CALL FMATN2(MZ02(2),MZ02(1),MZ01(2))
          GO TO 120
      ELSE IF (MXY(1)%MP(2) < 0) THEN
          NDIG = NDIG - INT(MXY(1)%MP(2))
          CALL ZMEQU_R1(MZ02,NDSAVE,NDIG)
          CALL ZMABS(MZ02,MZ01(1))
      ENDIF

      CALL FMLN(MZ01(1),MXY(4))
      CALL FMEQ(MXY(4),MZ01(1))
      CALL FMATN2(MZ02(2),MZ02(1),MZ01(2))

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  120 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(1)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL ZMEXIT(MZ01,MB,NDSAVE,MXSAVE,KOVUN)
      KRAD = KRSAVE
      RETURN
      END SUBROUTINE ZMLN

      SUBROUTINE ZMM2I(MA,INTEG)

!  INTEG = MA

!  INTEG is set to the integer value of the real part of MA

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2)
      INTEGER :: INTEG
      INTENT (IN) :: MA
      INTENT (INOUT) :: INTEG

      NCALL = NCALL + 1
      NAMEST(NCALL) = 'ZMM2I'
      IF (NTRACE /= 0) CALL ZMNTR(2,MA,MA,1)

      CALL FMM2I(MA(1),INTEG)

      IF (NTRACE /= 0) CALL ZMNTRI(1,INTEG,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE ZMM2I

      SUBROUTINE ZMM2Z(MA,ZVAL)

!  ZVAL = MA

!  Complex variable ZVAL is set to MA.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2)
      COMPLEX :: ZVAL

      REAL :: DI,DR
      INTENT (IN) :: MA
      INTENT (INOUT) :: ZVAL

      NCALL = NCALL + 1
      NAMEST(NCALL) = 'ZMM2Z'
      IF (NTRACE /= 0) CALL ZMNTR(2,MA,MA,1)

      CALL FMM2SP(MA(1),DR)
      CALL FMM2SP(MA(2),DI)
      ZVAL = CMPLX(DR,DI)

      IF (NTRACE /= 0) CALL ZMNTRZ(1,ZVAL,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE ZMM2Z

      SUBROUTINE ZMMPY(MA,MB,MC)

!  MC = MA * MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2),MC(2)
      REAL (KIND(1.0D0)) :: MAXEXP,MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,KL,KMETHD,KOVUN,KR_RETRY,KRESLT,KWRNSV,NDGSV2,NDSAVE,NTRSAV
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      TYPE(MULTI) :: MXY(7),MZ01(2)

      IF (.NOT. ALLOCATED(MC(1)%MP)) THEN
          ALLOCATE(MC(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MC(1)%MP)
          ALLOCATE(MC(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MC(2)%MP)) THEN
          ALLOCATE(MC(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MC(2)%MP)
          ALLOCATE(MC(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (ABS(MA(1)%MP(2)) > MEXPAB .OR. ABS(MA(2)%MP(2)) > MEXPAB .OR.  &
          ABS(MB(1)%MP(2)) > MEXPAB .OR. ABS(MB(2)%MP(2)) > MEXPAB .OR.  &
          KDEBUG >= 1) THEN
          CALL ZMENTR('ZMMPY    ',MA,MB,2,MC,KRESLT,NDSAVE,MXSAVE,KOVUN)
          IF (KRESLT /= 0) THEN
              RETURN
          ENDIF
      ELSE
          NCALL = NCALL + 1
          IF (NTRACE /= 0) THEN
              NAMEST(NCALL) = 'ZMMPY'
              CALL ZMNTR(2,MA,MB,2)
          ENDIF
          NDSAVE = NDIG
          NDIG = MAX(NDIG+NGRD52,2)
          IF (MBASE >= 100*ABS(MA(1)%MP(3)) .OR. MBASE >= 100*ABS(MA(2)%MP(3))) THEN
              NDIG = NDIG + 1
          ELSE IF (MBASE >= 100*ABS(MB(1)%MP(3)) .OR.  &
              MBASE >= 100*ABS(MB(2)%MP(3))) THEN
              NDIG = NDIG + 1
          ENDIF
          MXSAVE = MXEXP
          MXEXP = MXEXP2
          KOVUN = 0
      ENDIF

      KR_RETRY = 0
      NTRSAV = NTRACE
      NTRACE = 0
      KWRNSV = KWARN
      KWARN = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL FMEQU(MA(1),MXY(4),NDSAVE,NDIG)
      CALL FMEQU(MA(2),MXY(5),NDSAVE,NDIG)
      CALL FMEQU(MB(1),MXY(6),NDSAVE,NDIG)
      CALL FMEQU(MB(2),MXY(7),NDSAVE,NDIG)

!             Check for special cases.

      KMETHD = 1

!             If precision is low or a retry is being done due to cancellation,
!             use a slower but more stable form of the multiplication formula.

      IF (NDIG >= 35) KMETHD = 2

      IF (MXY(4)%MP(2) == MEXPUN .OR. MXY(5)%MP(2) == MEXPUN .OR.  &
          MXY(6)%MP(2) == MEXPUN .OR. MXY(7)%MP(2) == MEXPUN .OR.  &
          MXY(4)%MP(2) == MEXPOV .OR. MXY(5)%MP(2) == MEXPOV .OR.  &
          MXY(6)%MP(2) == MEXPOV .OR. MXY(7)%MP(2) == MEXPOV ) THEN
          CALL ZMI2M(0,MZ01)
          CALL ZMMPY_UNOV(MXY(4),MXY(5),MXY(6),MXY(7),MZ01)
          GO TO 140
      ELSE IF (KMETHD == 1) THEN

!             Method 1 for  ( a + b i ) * ( c + d i )

!             result = a*c - b*d + ( a*d + b*c ) i

          CALL FMMPYD(MXY(4),MXY(6),MXY(7),MZ01(1),MZ01(2))
          CALL FMMPYD(MXY(5),MXY(7),MXY(6),MXY(1),MXY(2))
          MAXEXP = MAX(MZ01(1)%MP(2),MXY(1)%MP(2))
          CALL FMSUB_R1(MZ01(1),MXY(1))
          IF (MZ01(1)%MP(2) < MAXEXP) GO TO 120
          MAXEXP = MAX(MZ01(2)%MP(2),MXY(2)%MP(2))
          CALL FMADD_R1(MZ01(2),MXY(2))
          IF (MZ01(2)%MP(2) < MAXEXP) GO TO 120
          GO TO 130
      ELSE

!             Method 2 for  ( a + b i ) * ( c + d i )

!             P = ( a + b )*( c + d )
!             result = a*c - b*d + ( P - a*c - b*d ) i

          CALL FMADD(MXY(4),MXY(5),MXY(1))
          CALL FMADD(MXY(6),MXY(7),MXY(2))
          CALL FMMPY_R1(MXY(1),MXY(2))

          CALL FMMPY(MXY(4),MXY(6),MXY(2))
          CALL FMMPY(MXY(5),MXY(7),MXY(3))

          MAXEXP = MAX(MXY(2)%MP(2),MXY(3)%MP(2))
          CALL FMSUB(MXY(2),MXY(3),MZ01(1))
          IF (MZ01(1)%MP(2) < MAXEXP) GO TO 120
          MAXEXP = MAX(MXY(1)%MP(2),MXY(2)%MP(2),MXY(3)%MP(2))
          CALL FMSUB(MXY(1),MXY(2),MZ01(2))
          CALL FMSUB_R1(MZ01(2),MXY(3))
          IF (MZ01(2)%MP(2) < MAXEXP) GO TO 120
          GO TO 130
      ENDIF

  120 NDIG = 2*NDIG
      CALL FMEQU(MA(1),MXY(4),NDSAVE,NDIG)
      CALL FMEQU(MA(2),MXY(5),NDSAVE,NDIG)
      CALL FMEQU(MB(1),MXY(6),NDSAVE,NDIG)
      CALL FMEQU(MB(2),MXY(7),NDSAVE,NDIG)
      CALL FMMPYD(MXY(4),MXY(6),MXY(7),MZ01(1),MZ01(2))
      CALL FMMPYD(MXY(5),MXY(7),MXY(6),MXY(1),MXY(2))
      CALL FMSUB_R1(MZ01(1),MXY(1))
      CALL FMADD_R1(MZ01(2),MXY(2))
      NDIG = NDIG / 2
      GO TO 140

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  130 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(1)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF

  140 MXEXP = MXSAVE
      NTRACE = NTRSAV
      NDGSV2 = NDIG
      NDIG = NDSAVE
      KWARN = KWRNSV
      CALL ZMEQU(MZ01,MC,NDGSV2,NDSAVE)
      IF (MC(1)%MP(2) >= MEXPOV .OR. MC(1)%MP(2) <= -MEXPOV .OR.  &
          MC(2)%MP(2) >= MEXPOV .OR. MC(2)%MP(2) <= -MEXPOV) THEN
          IF (MC(1)%MP(2) == MUNKNO .OR. MC(2)%MP(2) == MUNKNO) THEN
              KFLAG = -4
          ELSE IF (MC(1)%MP(2) == MEXPOV .OR. MC(2)%MP(2) == MEXPOV) THEN
              KFLAG = -5
          ELSE IF (MC(1)%MP(2) == MEXPUN .OR. MC(2)%MP(2) == MEXPUN) THEN
              KFLAG = -6
          ENDIF
          IF ((MC(1)%MP(2) == MUNKNO) .OR. (MC(2)%MP(2) == MUNKNO)  &
             .OR. (MC(1)%MP(2) == MEXPUN .AND. KOVUN == 0)          &
             .OR. (MC(2)%MP(2) == MEXPUN .AND. KOVUN == 0)          &
             .OR. (MC(1)%MP(2) == MEXPOV .AND. KOVUN == 0)          &
             .OR. (MC(2)%MP(2) == MEXPOV .AND. KOVUN == 0)) THEN
              NAMEST(NCALL) = 'ZMMPY'
              CALL ZMWARN
          ENDIF
      ENDIF
      IF (NTRACE /= 0) CALL ZMNTR(1,MC,MC,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE ZMMPY

      SUBROUTINE ZMMPY_UNOV(MA,MB,MC,MD,MZ)

!  Check special cases where at least one of MA, MB, MC, MD is underflow or overflow.

!  Return MZ as the result.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC,MD,MZ(2)
      INTENT (INOUT) :: MZ
      INTENT (IN) :: MA,MB,MC,MD

      TYPE(MULTI) :: MXY(7)
      REAL (KIND(1.0D0)) :: M_EXPUN, M_EXPOV, M_L1, M_L2, M_R1, M_R2


      M_EXPUN = -AINT( MAX_EXPONENT / 2.01D0 + 0.5D0 ) - 1
      M_EXPOV = AINT( MAX_EXPONENT / 2.01D0 + 0.5D0 ) + 2

      CALL FMMPY(MA,MC,MXY(1))
      CALL FMMPY(MB,MD,MXY(2))
      CALL FMMPY(MA,MD,MXY(3))
      CALL FMMPY(MB,MC,MXY(4))
      IF (MXY(1)%MP(2) /= MUNKNO .AND. MXY(2)%MP(2) /= MUNKNO .AND.  &
          MXY(3)%MP(2) /= MUNKNO .AND. MXY(4)%MP(2) /= MUNKNO) THEN
          CALL FMSUB(MXY(1),MXY(2),MZ(1))
          CALL FMADD(MXY(3),MXY(4),MZ(2))
          GO TO 120
      ENDIF

      IF (MXY(1)%MP(2) /= MUNKNO .AND. MXY(2)%MP(2) /= MUNKNO) THEN
          CALL FMSUB(MXY(1),MXY(2),MZ(1))
      ELSE
          IF (MXY(1)%MP(2) == MUNKNO .AND. MXY(2)%MP(2) /= MUNKNO .AND.  &
              (MA%MP(2) == MEXPOV .OR. MC%MP(2) == MEXPOV)) THEN
              CALL FMST2M('UNKNOWN',MZ(1))
              GO TO 110
          ENDIF
          IF (MXY(2)%MP(2) == MUNKNO .AND. MXY(1)%MP(2) /= MUNKNO .AND.  &
              (MB%MP(2) == MEXPOV .OR. MD%MP(2) == MEXPOV)) THEN
              CALL FMST2M('UNKNOWN',MZ(1))
              GO TO 110
          ENDIF
          IF (MA%MP(2) == MEXPOV .AND. MB%MP(2) == MEXPOV) THEN
              IF (MA%MP(1)*MC%MP(1) == -MB%MP(1)*MD%MP(1)) THEN
                  CALL FMEQ(MC,MXY(6))
                  MXY(6)%MP(1) = MA%MP(1)*MC%MP(1)
                  CALL FMEQ(MD,MXY(7))
                  MXY(7)%MP(1) = MB%MP(1)*MD%MP(1)
                  CALL FMSUB(MXY(6),MXY(7),MXY(5))
                  CALL FMST2M('OVERFLOW',MXY(6))
                  CALL FMMPY(MXY(5),MXY(6),MZ(1))
                  GO TO 110
              ELSE
                  CALL FMST2M('UNKNOWN',MZ(1))
                  GO TO 110
              ENDIF
          ENDIF
          IF (MA%MP(2) == MEXPOV .AND. MD%MP(2) == MEXPOV) THEN
              IF (MA%MP(1)*MC%MP(1) == -MB%MP(1)*MD%MP(1)) THEN
                  CALL FMEQ(MC,MXY(6))
                  MXY(6)%MP(1) = MA%MP(1)*MC%MP(1)
                  CALL FMEQ(MB,MXY(7))
                  MXY(7)%MP(1) = MB%MP(1)*MD%MP(1)
                  CALL FMSUB(MXY(6),MXY(7),MXY(5))
                  CALL FMST2M('OVERFLOW',MXY(6))
                  CALL FMMPY(MXY(5),MXY(6),MZ(1))
                  GO TO 110
              ELSE
                  CALL FMST2M('UNKNOWN',MZ(1))
                  GO TO 110
              ENDIF
          ENDIF
          IF (MC%MP(2) == MEXPOV .AND. MB%MP(2) == MEXPOV) THEN
              IF (MA%MP(1)*MC%MP(1) == -MB%MP(1)*MD%MP(1)) THEN
                  CALL FMEQ(MA,MXY(6))
                  MXY(6)%MP(1) = MA%MP(1)*MC%MP(1)
                  CALL FMEQ(MD,MXY(7))
                  MXY(7)%MP(1) = MB%MP(1)*MD%MP(1)
                  CALL FMSUB(MXY(6),MXY(7),MXY(5))
                  CALL FMST2M('OVERFLOW',MXY(6))
                  CALL FMMPY(MXY(5),MXY(6),MZ(1))
                  GO TO 110
              ELSE
                  CALL FMST2M('UNKNOWN',MZ(1))
                  GO TO 110
              ENDIF
          ENDIF
          IF (MC%MP(2) == MEXPOV .AND. MD%MP(2) == MEXPOV) THEN
              IF (MA%MP(1)*MC%MP(1) == -MB%MP(1)*MD%MP(1)) THEN
                  CALL FMEQ(MA,MXY(6))
                  MXY(6)%MP(1) = MA%MP(1)*MC%MP(1)
                  CALL FMEQ(MB,MXY(7))
                  MXY(7)%MP(1) = MB%MP(1)*MD%MP(1)
                  CALL FMSUB(MXY(6),MXY(7),MXY(5))
                  CALL FMST2M('OVERFLOW',MXY(6))
                  CALL FMMPY(MXY(5),MXY(6),MZ(1))
                  GO TO 110
              ELSE
                  CALL FMST2M('UNKNOWN',MZ(1))
                  GO TO 110
              ENDIF
          ENDIF
          M_L1 = MAX( MIN( MA%MP(2) , M_EXPOV) , M_EXPUN )
          M_L2 = MAX( MIN( MC%MP(2) , M_EXPOV) , M_EXPUN )
          M_R1 = MAX( MIN( MB%MP(2) , M_EXPOV) , M_EXPUN )
          M_R2 = MAX( MIN( MD%MP(2) , M_EXPOV) , M_EXPUN )
          IF (M_L1+M_L2+NDIG < M_R1+M_R2 .AND. MXY(2)%MP(3) /= 0) THEN
              CALL FMMPYI(MXY(2),-1,MZ(1))
              GO TO 110
          ELSE IF (M_R1+M_R2+NDIG < M_L1+M_L2 .AND. MXY(1)%MP(3) /= 0) THEN
              CALL FMEQ(MXY(1),MZ(1))
              GO TO 110
          ENDIF
          CALL FMSUB(MXY(1),MXY(2),MZ(1))
      ENDIF

  110 IF (MXY(3)%MP(2) /= MUNKNO .AND. MXY(4)%MP(2) /= MUNKNO) THEN
          CALL FMSUB(MXY(3),MXY(4),MZ(2))
      ELSE
          IF (MXY(3)%MP(2) == MUNKNO .AND. MXY(4)%MP(2) /= MUNKNO .AND.  &
              (MA%MP(2) == MEXPOV .OR. MD%MP(2) == MEXPOV)) THEN
              CALL FMST2M('UNKNOWN',MZ(2))
              GO TO 120
          ENDIF
          IF (MXY(4)%MP(2) == MUNKNO .AND. MXY(3)%MP(2) /= MUNKNO .AND.  &
              (MB%MP(2) == MEXPOV .OR. MC%MP(2) == MEXPOV)) THEN
              CALL FMST2M('UNKNOWN',MZ(2))
              GO TO 120
          ENDIF
          IF (MA%MP(2) == MEXPOV .AND. MB%MP(2) == MEXPOV) THEN
              IF (MA%MP(1)*MD%MP(1) == MB%MP(1)*MC%MP(1)) THEN
                  CALL FMEQ(MD,MXY(6))
                  MXY(6)%MP(1) = MA%MP(1)*MD%MP(1)
                  CALL FMEQ(MC,MXY(7))
                  MXY(7)%MP(1) = MB%MP(1)*MC%MP(1)
                  CALL FMSUB(MXY(6),MXY(7),MXY(5))
                  CALL FMST2M('OVERFLOW',MXY(6))
                  CALL FMMPY(MXY(5),MXY(6),MZ(2))
                  GO TO 120
              ELSE
                  CALL FMST2M('UNKNOWN',MZ(2))
                  GO TO 120
              ENDIF
          ENDIF
          IF (MA%MP(2) == MEXPOV .AND. MC%MP(2) == MEXPOV) THEN
              IF (MA%MP(1)*MD%MP(1) == MB%MP(1)*MC%MP(1)) THEN
                  CALL FMEQ(MD,MXY(6))
                  MXY(6)%MP(1) = MA%MP(1)*MD%MP(1)
                  CALL FMEQ(MB,MXY(7))
                  MXY(7)%MP(1) = MB%MP(1)*MC%MP(1)
                  CALL FMSUB(MXY(6),MXY(7),MXY(5))
                  CALL FMST2M('OVERFLOW',MXY(6))
                  CALL FMMPY(MXY(5),MXY(6),MZ(2))
                  GO TO 120
              ELSE
                  CALL FMST2M('UNKNOWN',MZ(2))
                  GO TO 120
              ENDIF
          ENDIF
          IF (MD%MP(2) == MEXPOV .AND. MB%MP(2) == MEXPOV) THEN
              IF (MA%MP(1)*MD%MP(1) == MB%MP(1)*MC%MP(1)) THEN
                  CALL FMEQ(MA,MXY(6))
                  MXY(6)%MP(1) = MA%MP(1)*MD%MP(1)
                  CALL FMEQ(MC,MXY(7))
                  MXY(7)%MP(1) = MB%MP(1)*MC%MP(1)
                  CALL FMSUB(MXY(6),MXY(7),MXY(5))
                  CALL FMST2M('OVERFLOW',MXY(6))
                  CALL FMMPY(MXY(5),MXY(6),MZ(2))
                  GO TO 120
              ELSE
                  CALL FMST2M('UNKNOWN',MZ(2))
                  GO TO 120
              ENDIF
          ENDIF
          IF (MD%MP(2) == MEXPOV .AND. MC%MP(2) == MEXPOV) THEN
              IF (MA%MP(1)*MD%MP(1) == MB%MP(1)*MC%MP(1)) THEN
                  CALL FMEQ(MA,MXY(6))
                  MXY(6)%MP(1) = MA%MP(1)*MD%MP(1)
                  CALL FMEQ(MB,MXY(7))
                  MXY(7)%MP(1) = MB%MP(1)*MC%MP(1)
                  CALL FMSUB(MXY(6),MXY(7),MXY(5))
                  CALL FMST2M('OVERFLOW',MXY(6))
                  CALL FMMPY(MXY(5),MXY(6),MZ(2))
                  GO TO 120
              ELSE
                  CALL FMST2M('UNKNOWN',MZ(2))
                  GO TO 120
              ENDIF
          ENDIF
          M_L1 = MAX( MIN( MA%MP(2) , M_EXPOV) , M_EXPUN )
          M_L2 = MAX( MIN( MD%MP(2) , M_EXPOV) , M_EXPUN )
          M_R1 = MAX( MIN( MB%MP(2) , M_EXPOV) , M_EXPUN )
          M_R2 = MAX( MIN( MC%MP(2) , M_EXPOV) , M_EXPUN )
          IF (M_L1+M_L2+NDIG < M_R1+M_R2 .AND. MXY(4)%MP(3) /= 0) THEN
              CALL FMEQ(MXY(4),MZ(2))
              GO TO 120
          ELSE IF (M_R1+M_R2+NDIG < M_L1+M_L2 .AND. MXY(3)%MP(3) /= 0) THEN
              CALL FMEQ(MXY(3),MZ(2))
              GO TO 120
          ENDIF
          CALL FMSUB(MXY(3),MXY(4),MZ(2))
      ENDIF

  120 RETURN
      END SUBROUTINE ZMMPY_UNOV

      SUBROUTINE ZMMPY_R1(MA,MB)

!  MA = MA * MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (INOUT) :: MA
      INTENT (IN) :: MB

      TYPE(MULTI) :: MXY(2)


      CALL ZMMPY(MA,MB,MXY)
      CALL ZMEQ(MXY,MA)


      END SUBROUTINE ZMMPY_R1

      SUBROUTINE ZMMPY_R2(MA,MB)

!  MB = MA * MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB

      TYPE(MULTI) :: MXY(2)


      CALL ZMMPY(MA,MB,MXY)
      CALL ZMEQ(MXY,MB)


      END SUBROUTINE ZMMPY_R2

      SUBROUTINE ZMMPYI(MA,INTEG,MB)

!  MB = MA * INTEG        Multiply by one-word (real) integer.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      INTEGER :: INTEG
      INTEGER :: KOVUN,KRESLT,KWRNSV,NDSAVE,NTRSAV
      REAL (KIND(1.0D0)) :: MXSAVE
      INTENT (IN) :: MA,INTEG
      INTENT (INOUT) :: MB

      IF (ABS(MA(1)%MP(2)) > MEXPAB .OR. ABS(MA(2)%MP(2)) > MEXPAB .OR.  &
          KDEBUG >= 1) THEN
          NTRSAV = NTRACE
          IF (NTRACE /= 0) THEN
              NCALL = NCALL + 1
              NAMEST(NCALL) = 'ZMMPYI'
              CALL ZMNTR(2,MA,MA,1)
              CALL FMNTRI(2,INTEG,0)
              NCALL = NCALL - 1
          ENDIF
          NTRACE = 0
          CALL ZMENTR('ZMMPYI   ',MA,MA,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
          NTRACE = NTRSAV
          IF (KRESLT /= 0) THEN
              NCALL = NCALL + 1
              IF (NTRACE /= 0) CALL ZMNTR(1,MB,MB,1)
              NCALL = NCALL - 1
              RETURN
          ENDIF
          NDIG = NDSAVE
          MXEXP = MXSAVE
          NTRSAV = NTRACE
      ELSE
          NCALL = NCALL + 1
          IF (NTRACE /= 0) THEN
              NAMEST(NCALL) = 'ZMMPYI'
              CALL ZMNTR(2,MA,MA,1)
              CALL FMNTRI(2,INTEG,0)
          ENDIF
          KOVUN = 0
      ENDIF

!             Force FMMPYI to use more guard digits for user calls.

      NCALL = NCALL - 1
      NTRSAV = NTRACE
      NTRACE = 0
      KWRNSV = KWARN
      KWARN = 0

      CALL FMMPYI(MA(1),INTEG,MB(1))
      CALL FMMPYI(MA(2),INTEG,MB(2))

      NTRACE = NTRSAV
      KWARN = KWRNSV
      NCALL = NCALL + 1
      IF (NTRACE /= 0) NAMEST(NCALL) = 'ZMMPYI'
      IF (MB(1)%MP(2) == MUNKNO .OR. MB(2)%MP(2) == MUNKNO) THEN
          KFLAG = -4
      ELSE IF (MB(1)%MP(2) == MEXPOV .OR. MB(2)%MP(2) == MEXPOV) THEN
          KFLAG = -5
      ELSE IF (MB(1)%MP(2) == MEXPUN .OR. MB(2)%MP(2) == MEXPUN) THEN
          KFLAG = -6
      ENDIF
      IF ((MB(1)%MP(2) == MUNKNO) .OR. (MB(2)%MP(2) == MUNKNO)  &
         .OR. (MB(1)%MP(2) == MEXPUN .AND. KOVUN == 0)          &
         .OR. (MB(2)%MP(2) == MEXPUN .AND. KOVUN == 0)          &
         .OR. (MB(1)%MP(2) == MEXPOV .AND. KOVUN == 0)          &
         .OR. (MB(2)%MP(2) == MEXPOV .AND. KOVUN == 0)) THEN
          NAMEST(NCALL) = 'ZMMPYI'
          CALL ZMWARN
      ENDIF
      IF (NTRACE /= 0) CALL ZMNTR(1,MB,MB,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE ZMMPYI

      SUBROUTINE ZMMPYI_R1(MA,IVAL)

!  MA = MA / IVAL

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2)
      INTEGER :: IVAL
      INTENT (INOUT) :: MA
      INTENT (IN) :: IVAL

      TYPE(MULTI) :: MXY(2)


      CALL ZMMPYI(MA,IVAL,MXY)
      CALL ZMEQ(MXY,MA)


      END SUBROUTINE ZMMPYI_R1

      SUBROUTINE ZMNINT(MA,MB)

!  MB = NINT(MA)

!  The nearest integers to both real and imaginary parts are returned.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB

      NCALL = NCALL + 1
      NAMEST(NCALL) = 'ZMNINT'
      IF (NTRACE /= 0) CALL ZMNTR(2,MA,MA,1)

      CALL FMNINT(MA(1),MB(1))
      CALL FMNINT(MA(2),MB(2))

      IF (NTRACE /= 0) CALL ZMNTR(1,MB,MB,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE ZMNINT

      SUBROUTINE ZMNTR(NTR,MA,MB,NARG)

!  Print ZM numbers in base 10 format using ZMOUT for conversion.
!  This is used for trace output from the ZM routines.

!  NTR =  1 if a result of an ZM call is to be printed.
!      =  2 to print input argument(s) to an ZM call.

!  MA  -  the ZM number to be printed.

!  MB  -  an optional second ZM number to be printed.

!  NARG - the number of arguments.  NARG = 1 if only MA is to be
!         printed, and NARG = 2 if both MA and MB are to be printed.


!  NTRACE and LVLTRC (in module FMVALS) control trace printout.

!  NTRACE = 0        No printout except warnings and errors.

!  NTRACE = 1        The result of each call to one of the routines
!                    is printed in base 10, using ZMOUT.

!  NTRACE = -1       The result of each call to one of the routines
!                    is printed in internal base MBASE format.

!  NTRACE = 2        The input arguments and result of each call to one
!                    of the routines is printed in base 10, using ZMOUT.

!  NTRACE = -2       The input arguments and result of each call to one
!                    of the routines is printed in base MBASE format.

!  LVLTRC defines the call level to which the trace is done.  LVLTRC = 1 means only FM routines
!         called directly by the user are traced, LVLTRC = K prints traces for ZM or FM routines
!         with call levels up to and including level K.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      INTEGER :: NTR,NARG
      CHARACTER(9) :: NAME
      INTENT (IN) :: MA,MB

      IF (NTRACE == 0) RETURN
      IF (NCALL > LVLTRC) RETURN
      IF (NTR == 2 .AND. ABS(NTRACE) == 1) RETURN

      IF (NTR == 2) THEN
          NAME = NAMEST(NCALL)
          IF (KROUND == 1) THEN
              WRITE (KW,"(' Input to ',A)") TRIM(NAME)
          ELSE IF (KROUND == 2) THEN
              WRITE (KW,"(' Input to ',A,A)") NAME,'      (round toward +infinity)'
          ELSE IF (KROUND == 0) THEN
              WRITE (KW,"(' Input to ',A,A)") NAME,'      (round toward zero)'
          ELSE IF (KROUND == -1) THEN
              WRITE (KW,"(' Input to ',A,A)") NAME,'      (round toward -infinity)'
          ELSE
              WRITE (KW,"(' Input to ',A)") TRIM(NAME)
          ENDIF
      ELSE
          NAME = NAMEST(NCALL)
          IF (KFLAG == 0) THEN
              WRITE (KW,                                               &
                     "(' ',A,12X,'Call level =',I2,5X,'MBASE =',"  //  &
                     "I10,5X,'NDIG =',I10)"                            &
                    ) NAME,NCALL,INT(MBASE),NDIG
          ELSE
              WRITE (KW,                                              &
                     "(' ',A,3X,'Call level =',I2,4X,'MBASE =',"  //  &
                     "I10,4X,'NDIG =',I10,4X,'KFLAG =',I3)"           &
                    ) NAME,NCALL,INT(MBASE),NDIG,KFLAG
          ENDIF
      ENDIF

!             Check for base MBASE internal format trace.

      IF (NTRACE < 0) THEN
          CALL ZMNTRJ(MA,NDIG)
          IF (NARG == 2) CALL ZMNTRJ(MB,NDIG)
      ENDIF

!             Check for base 10 trace using ZMOUT.

      IF (NTRACE > 0) THEN
          CALL ZMPRNT(MA)

          IF (NARG == 2) THEN
              CALL ZMPRNT(MB)
          ENDIF
      ENDIF

      RETURN
      END SUBROUTINE ZMNTR

      SUBROUTINE ZMNTR2(NTR,MAFM,MBFM,NARG)

!  Print real FM numbers in base 10 format using FMOUT for conversion.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MAFM,MBFM
      INTEGER :: NTR,NARG

      CHARACTER(9) :: NAME
      INTENT (IN) :: NTR,MAFM,MBFM,NARG

      IF (NTRACE == 0) RETURN
      IF (NCALL > LVLTRC) RETURN
      IF (NTR == 2 .AND. ABS(NTRACE) == 1) RETURN

      IF (NTR == 2) THEN
          NAME = NAMEST(NCALL)
          IF (KROUND == 1) THEN
              WRITE (KW,"(' Input to ',A)") TRIM(NAME)
          ELSE IF (KROUND == 2) THEN
              WRITE (KW,"(' Input to ',A,A)") NAME,'      (round toward +infinity)'
          ELSE IF (KROUND == 0) THEN
              WRITE (KW,"(' Input to ',A,A)") NAME,'      (round toward zero)'
          ELSE IF (KROUND == -1) THEN
              WRITE (KW,"(' Input to ',A,A)") NAME,'      (round toward -infinity)'
          ELSE
              WRITE (KW,"(' Input to ',A)") TRIM(NAME)
          ENDIF
      ELSE
          NAME = NAMEST(NCALL)
          IF (KFLAG == 0) THEN
              WRITE (KW,                                               &
                     "(' ',A,12X,'Call level =',I2,5X,'MBASE =',"  //  &
                     "I10,5X,'NDIG =',I10)"                            &
                    ) NAME,NCALL,INT(MBASE),NDIG
          ELSE
              WRITE (KW,                                              &
                     "(' ',A,3X,'Call level =',I2,4X,'MBASE =',"  //  &
                     "I10,4X,'NDIG =',I10,4X,'KFLAG =',I3)"           &
                    ) NAME,NCALL,INT(MBASE),NDIG,KFLAG
          ENDIF
      ENDIF

!             Check for base MBASE internal format trace.

      IF (NTRACE < 0) THEN
          CALL FMNTRJ(MAFM,NDIG)
          IF (NARG == 2) CALL FMNTRJ(MBFM,NDIG)
      ENDIF

!             Check for base 10 trace using FMOUT.

      IF (NTRACE > 0) THEN
          CALL FMPRNT(MAFM)
          IF (NARG == 2) THEN
              CALL FMPRNT(MBFM)
          ENDIF
      ENDIF

      RETURN
      END SUBROUTINE ZMNTR2

      SUBROUTINE ZMNTRI(NTR,N,KNAM)

!  Internal routine for trace output of integer variables.

!  NTR = 1 for output values
!        2 for input values

!  N     Integer to be printed.

!  KNAM  is positive if the routine name is to be printed.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: NTR,N,KNAM

      CHARACTER(9) :: NAME
      INTENT (IN) :: NTR,N,KNAM

      IF (NTRACE == 0) RETURN
      IF (NCALL > LVLTRC) RETURN
      IF (NTR == 2 .AND. ABS(NTRACE) == 1) RETURN

      IF (NTR == 2 .AND. KNAM > 0) THEN
          NAME = NAMEST(NCALL)
          IF (KROUND == 1) THEN
              WRITE (KW,"(' Input to ',A)") TRIM(NAME)
          ELSE IF (KROUND == 2) THEN
              WRITE (KW,"(' Input to ',A,A)") NAME,'      (round toward +infinity)'
          ELSE IF (KROUND == 0) THEN
              WRITE (KW,"(' Input to ',A,A)") NAME,'      (round toward zero)'
          ELSE IF (KROUND == -1) THEN
              WRITE (KW,"(' Input to ',A,A)") NAME,'      (round toward -infinity)'
          ELSE
              WRITE (KW,"(' Input to ',A)") TRIM(NAME)
          ENDIF
      ENDIF
      IF (NTR == 1 .AND. KNAM > 0) THEN
          NAME = NAMEST(NCALL)
          IF (KFLAG == 0) THEN
              WRITE (KW,                                               &
                     "(' ',A,12X,'Call level =',I2,5X,'MBASE =',"  //  &
                     "I10,5X,'NDIG =',I10)"                            &
                    ) NAME,NCALL,INT(MBASE),NDIG
          ELSE
              WRITE (KW,                                              &
                     "(' ',A,3X,'Call level =',I2,4X,'MBASE =',"  //  &
                     "I10,4X,'NDIG =',I10,4X,'KFLAG =',I3)"           &
                    ) NAME,NCALL,INT(MBASE),NDIG,KFLAG
          ENDIF
      ENDIF

      WRITE (KW,"(1X,I20)") N

      RETURN
      END SUBROUTINE ZMNTRI

      SUBROUTINE ZMNTRJ(MA,ND)

!  Print trace output in internal base MBASE format.  The number to be printed is in MA.

!  ND is the number of base MBASE digits to be printed.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2)
      INTEGER :: ND

      CHARACTER(99) :: FORM
      CHARACTER(40), EXTERNAL :: FMFI
      CHARACTER(40) :: ST1,ST2
      INTEGER :: J,L,N
      INTENT (IN) :: MA,ND

      L = INT(LOG10(DBLE(MBASE-1))) + 2
      N = (KSWIDE-23)/L
      IF (N > 10) N = 5*(N/5)
      IF (ND <= N) THEN
          WRITE (FORM,"(' (1X,I19,I',I2,',',I3,'I',I2,') ')") L+2, N-1, L
      ELSE
          WRITE (FORM,                                  &
                 "(' (1X,I19,I',I2,',',I3,'I',I2,"  //  &
                 "'/(22X,',I3,'I',I2,')) ')"            &
                ) L+2, N-1, L, N, L
      ENDIF
      ST1 = FMFI(INT(MA(1)%MP(1)))
      ST2 = FMFI(INT(MA(1)%MP(2)))
      WRITE (KW,"(A,A,A,A,A)") '            Sign = ',TRIM(ST1),  &
                                   '   Exponent = ',TRIM(ST2),'   Digits of real part:'
      WRITE (FORM,*) '(13X,', N, 'I', L, ')'
      WRITE (KW,FORM) (INT(MA(1)%MP(J)),J=3,ND+2)

      ST1 = FMFI(INT(MA(2)%MP(1)))
      ST2 = FMFI(INT(MA(2)%MP(2)))
      WRITE (KW,"(A,A,A,A,A,A,A)") '            Sign = ',TRIM(ST1),  &
                                   '   Exponent = ',TRIM(ST2),'   Digits of imaginary part:'
      WRITE (KW,FORM) (INT(MA(2)%MP(J)),J=3,ND+2)

      RETURN
      END SUBROUTINE ZMNTRJ

      SUBROUTINE ZMNTRZ(NTR,X,KNAM)

!  Internal routine for trace output of complex variables.

!  NTR - 1 for output values
!        2 for input values

!  X   - Complex value to be printed if NX == 1

!  KNAM - Positive if the routine name is to be printed.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: NTR,KNAM
      COMPLEX :: X

      CHARACTER(9) :: NAME
      DOUBLE PRECISION :: XREAL,XIMAG
      INTENT (IN) :: NTR,X,KNAM

      IF (NTRACE == 0) RETURN
      IF (NCALL > LVLTRC) RETURN
      IF (NTR == 2 .AND. ABS(NTRACE) == 1) RETURN

      IF (NTR == 2 .AND. KNAM > 0) THEN
          NAME = NAMEST(NCALL)
          IF (KROUND == 1) THEN
              WRITE (KW,"(' Input to ',A)") TRIM(NAME)
          ELSE IF (KROUND == 2) THEN
              WRITE (KW,"(' Input to ',A,A)") NAME,'      (round toward +infinity)'
          ELSE IF (KROUND == 0) THEN
              WRITE (KW,"(' Input to ',A,A)") NAME,'      (round toward zero)'
          ELSE IF (KROUND == -1) THEN
              WRITE (KW,"(' Input to ',A,A)") NAME,'      (round toward -infinity)'
          ELSE
              WRITE (KW,"(' Input to ',A)") TRIM(NAME)
          ENDIF
      ENDIF
      IF (NTR == 1 .AND. KNAM > 0) THEN
          NAME = NAMEST(NCALL)
          IF (KFLAG == 0) THEN
              WRITE (KW,                                               &
                     "(' ',A,12X,'Call level =',I2,5X,'MBASE =',"  //  &
                     "I10,5X,'NDIG =',I10)"                            &
                    ) NAME,NCALL,INT(MBASE),NDIG
          ELSE
              WRITE (KW,                                              &
                     "(' ',A,3X,'Call level =',I2,4X,'MBASE =',"  //  &
                     "I10,4X,'NDIG =',I10,4X,'KFLAG =',I3)"           &
                    ) NAME,NCALL,INT(MBASE),NDIG,KFLAG
          ENDIF
      ENDIF

      XREAL = DBLE(X)
      XIMAG = DBLE(AIMAG(X))
      IF (XIMAG >= 0.0D0) THEN
          WRITE (KW,"(1X,D30.20,' +',D30.20,' i')") XREAL,XIMAG
      ELSE
          WRITE (KW,"(1X,D30.20,' -',D30.20,' i')") XREAL,ABS(XIMAG)
      ENDIF

      RETURN
      END SUBROUTINE ZMNTRZ

      SUBROUTINE ZMOUT(MA,LINE,LB,LAST1,LAST2)

!  Convert a floating multiple precision number to a character array for output.

!  MA    is an ZM number to be converted to an A1 character array in base 10 format
!  LINE  is the character(1) array in which the result is returned.
!  LB    is the length of LINE.
!  LAST1 is returned as the position of the last nonblank character of the real part of the
!        number in LINE.
!  LAST2 is returned as the position of the last nonblank character of the imaginary part of
!        the number in LINE.

!  JFORM1 and JFORM2 determine the format of the two FM numbers making up the complex value MA.
!  See FMOUT for details.

!  JFORMZ determines the format of the real and imaginary parts.

!  JFORMZ = 1  normal setting :       1.23 - 4.56 i
!         = 2  use capital I  :       1.23 - 4.56 I
!         = 3  parenthesis format   ( 1.23 , -4.56 )

!  LINE should be dimensioned at least 4*(LOG10(MBASE)*NDIG + 15) on a 32-bit machine to allow
!  for up to 10 digit exponents.  Replace 15 by 20 if 48-bit integers are used, 25 for 64-bit
!  integers, etc.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2)
      INTEGER :: LB,LAST1,LAST2
      CHARACTER :: LINE(LB)

      REAL (KIND(1.0D0)) :: MAIMS
      INTEGER :: J,KPT,LB2,ND,NEXP
      INTENT (IN) :: MA,LB
      INTENT (INOUT) :: LINE,LAST1,LAST2
      TYPE(MULTI) :: MXY(2)


      NCALL = NCALL + 1
      NAMEST(NCALL) = 'ZMOUT'
      DO J = 1, LB
         LINE(J) = ' '
      ENDDO
      ND = INT(REAL(NDIG)*LOG10(REAL(MBASE))) + 1
      IF (ND < 2) ND = 2
      NEXP = INT(2.0*LOG10(REAL(MXBASE))) + 16
      KPT = 1
      IF (JFORMZ == 3) KPT = 3
      LB2 = MAX(JFORM2+NEXP,ND+NEXP)
      LB2 = MIN(LB+1-KPT,LB2)
      CALL FMOUT(MA(1),LINE(KPT),LB2)

      IF (JFORMZ == 3) LINE(1) = '('
      LAST1 = 1
      DO J = LB2, 1, -1
         IF (LINE(J) /= ' ') THEN
             LAST1 = J
             GO TO 110
         ENDIF
      ENDDO

  110 MAIMS = MA(2)%MP(1)
      CALL FMI2M(0,MXY(1))
      DO J = 1, NDIG+2
         MXY(1)%MP(J) = MA(2)%MP(J)
      ENDDO
      LINE(LAST1+1) = ' '
      IF (JFORMZ == 3) THEN
          LINE(LAST1+2) = ','
      ELSE
          IF (MAIMS < 0) THEN
              MXY(1)%MP(1) = 1
              LINE(LAST1+2) = '-'
          ELSE
              LINE(LAST1+2) = '+'
          ENDIF
      ENDIF

      KPT = LAST1 + 3
      LB2 = MAX(JFORM2+NEXP,ND+NEXP)
      LB2 = MIN(LB+1-KPT,LB2+2)
      CALL FMOUT(MXY(1),LINE(KPT),LB2)
      LAST1 = KPT
      DO J = LB2+KPT-1, KPT, -1
         IF (LINE(J) /= ' ') THEN
             LAST2 = J
             GO TO 120
         ENDIF
      ENDDO

  120 LAST2 = LAST2 + 2
      LINE(LAST2) = 'i'
      IF (JFORMZ == 2) LINE(LAST2) = 'I'
      IF (JFORMZ == 3) LINE(LAST2) = ')'

      IF (LINE(KPT) == ' ' .AND. LINE(KPT+1) == '+') THEN
          DO J = KPT+2, LAST2
             LINE(J-2) = LINE(J)
          ENDDO
          LINE(LAST2-1) = ' '
          LINE(LAST2) = ' '
          LAST2 = LAST2 - 2
      ENDIF

      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE ZMOUT

      SUBROUTINE ZMPACK(MA,MP)

!  MA is packed two base MBASE digits per word and returned in MP.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MP(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MP
      CALL FMPACK(MA(1),MP(1))
      CALL FMPACK(MA(2),MP(2))
      RETURN
      END SUBROUTINE ZMPACK

      SUBROUTINE ZMPRNT(MA)

!  Print MA in base 10 format.

!  ZMPRNT can be called directly by the user for easy output in M format.
!  MA is converted using ZMOUT and printed.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2)

      CHARACTER(20) :: FORM
      INTEGER :: J,K,KSAVE,LAST1,LAST2,LB,LBZ,ND,NEXP
      INTENT (IN) :: MA

      KSAVE = KFLAG
      ND = INT(REAL(NDIG)*LOG10(REAL(MBASE))) + 1
      IF (ND < 2) ND = 2
      NEXP = INT(2.0*LOG10(REAL(MXBASE))) + 16
      LB = MAX(JFORM2+NEXP,ND+NEXP)

      IF (JPRNTZ == 1) THEN
          LBZ = 2*LB + 7
          IF (LBZ > LMBUFZ) THEN
              IF (LMBUFZ > 0) DEALLOCATE(CMBUFZ)
              ALLOCATE(CMBUFZ(LBZ),STAT=J)
              IF (J /= 0) THEN
                  CALL FMDEFINE_ERROR
              ENDIF
              LMBUFZ = LBZ
          ENDIF
          CALL ZMOUT(MA,CMBUFZ,LBZ,LAST1,LAST2)
          WRITE (FORM,"(' (6X,',I3,'A1) ')") KSWIDE-7
          WRITE (KW,FORM) (CMBUFZ(K),K=1,LAST2)
      ELSE
          CALL FMPRNT(MA(1))
          CALL FMPRNT(MA(2))
      ENDIF
      KFLAG = KSAVE
      RETURN
      END SUBROUTINE ZMPRNT

      SUBROUTINE ZMPWR(MA,MB,MC)

!  MC = MA ** MB.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2),MC(2)

      REAL (KIND(1.0D0)) :: MXSAVE,MTEMP
      DOUBLE PRECISION :: ERR
      INTEGER :: IEXTRA,INTMB,J,JSIN,JCOS,JSWAP,K,KL,KOVUN,KR_RETRY,KRADSV,  &
                 KRESLT,KWRNSV,NDSAVE
      LOGICAL, EXTERNAL :: FMCOMP
      REAL :: XVAL
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      TYPE(MULTI) :: MXY(5),MZ01(2),MZ02(2),MZ03(2),MZ04(2),MZ05(2),MZ06(2)

      IF (.NOT. ALLOCATED(MC(1)%MP)) THEN
          ALLOCATE(MC(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MC(1)%MP)
          ALLOCATE(MC(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MC(2)%MP)) THEN
          ALLOCATE(MC(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MC(2)%MP)
          ALLOCATE(MC(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL ZMENTR('ZMPWR    ',MA,MB,2,MC,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF
      KR_RETRY = 0
      K = MAX(MB(1)%MP(2),MB(2)%MP(2))
      IF (K < 25000) THEN
          NDIG = NDIG + MAX(K,0) + 1
      ELSE
          NDIG = NDIG + 1
      ENDIF

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL ZMEQU(MA,MZ04,NDSAVE,NDIG)
      CALL ZMEQU(MB,MZ05,NDSAVE,NDIG)

!             Check for special cases.

      IF (MA(1)%MP(3) == 0 .AND. MA(2)%MP(3) == 0) THEN
          IF (MB(1)%MP(1) > 0 .AND. MB(2)%MP(3) == 0) THEN
              CALL ZMI2M(0,MZ02)
              GO TO 130
          ELSE
              KFLAG = -4
              CALL ZMST2M('UNKNOWN+UNKNOWN*i',MZ02)
              GO TO 130
          ENDIF
      ENDIF
      IF (MB(2)%MP(3) == 0) THEN
          KWRNSV = KWARN
          KWARN = 0
          CALL FMMI(MZ05(1),INTMB)
          KWARN = KWRNSV
          IF (KFLAG == 0) THEN
              IF (NCALL == 1) THEN
                  XVAL = ABS(INTMB) + 1
                  K = INT((1.5*LOG(XVAL))/ALOGMB + 2.0)
                  NDIG = MAX(NDIG+K,2)
                  IF (MBASE >= 100*ABS(MA(1)%MP(3)) .OR.  &
                      MBASE >= 100*ABS(MA(2)%MP(3))) THEN
                      NDIG = NDIG + 1
                  ENDIF
              ENDIF
              CALL ZMEQU_R1(MZ04,NDSAVE,NDIG)
              CALL ZMIPWR(MZ04,INTMB,MZ03)
              CALL ZMEQ(MZ03,MZ02)
              GO TO 120
          ENDIF
      ENDIF

!             Check for cases where ABS(MA) is very close to 1, and avoid cancellation.

      CALL FMABS(MZ04(1),MXY(1))
      CALL FMABS(MZ04(2),MXY(2))
      CALL FMI2M(1,MXY(3))
      IF (FMCOMP(MXY(1),'==',MXY(3)) .AND.  &
          (MXY(2)%MP(2) <= (-NDIG).OR.MXY(2)%MP(3) == 0)) THEN
          IF (MA(1)%MP(1) > 0) THEN

!                 (1+c)**b = 1 + b*c + ...

              CALL ZMI2M(1,MZ02)
              CALL ZMSUB(MZ04,MZ02,MZ06)
              CALL ZMMPY(MZ05,MZ06,MZ02)
              CALL FMADD_R1(MZ02(1),MXY(3))
          ELSE

!                 (-1+c)**b = (-1)**b * (1 - b*c + ... )

              CALL ZMI2M(-1,MZ02)
              CALL ZMSUB(MZ04,MZ02,MZ01)
              CALL ZMMPY(MZ05,MZ01,MZ06)
              CALL ZMMPYI(MZ06,-1,MZ02)
              CALL FMADD_R1(MZ02(1),MXY(3))
              KRADSV = KRAD
              KRAD = 0
              IF (MA(2)%MP(1) >= 0) THEN
                  CALL FMMPYI(MZ05(1),180,MXY(4))
              ELSE
                  CALL FMMPYI(MZ05(1),-180,MXY(4))
              ENDIF
              CALL FMCSSN(MXY(4),MZ03(1),MZ03(2))
              KRAD = KRADSV
              CALL FMPI(MXY(3))
              CALL FMMPY_R1(MXY(3),MZ05(2))
              IF (MA(2)%MP(1) >= 0) CALL FMMPYI_R1(MXY(3),-1)
              CALL FMEXP(MXY(3),MXY(5))
              CALL FMEQ(MXY(5),MXY(3))
              CALL FMMPYD(MXY(3),MZ03(1),MZ03(2),MZ06(1),MZ06(2))
              CALL ZMMPY(MZ02,MZ06,MZ01)
              CALL ZMEQ(MZ01,MZ02)
          ENDIF
          GO TO 120
      ENDIF
      IF (FMCOMP(MXY(2),'==',MXY(3)) .AND.  &
          (MXY(1)%MP(2) <= (-NDIG).OR.MXY(1)%MP(3) == 0)) THEN
          IF (MA(2)%MP(1) > 0) THEN

!                 (i+c)**b = i**b * (1 - b*c*i - ... )

              CALL ZM2I2M(0,1,MZ02)
              CALL ZMSUB(MZ04,MZ02,MZ06)
              CALL ZMMPY(MZ05,MZ06,MZ02)
              DO J = 1, NDIG+2
                 MTEMP = MZ02(1)%MP(J)
                 MZ02(1)%MP(J) = MZ02(2)%MP(J)
                 MZ02(2)%MP(J) = MTEMP
              ENDDO
              IF (MZ02(2)%MP(2) /= MUNKNO .AND. MZ02(2)%MP(3) /= 0)  &
                  MZ02(2)%MP(1) = -MZ02(2)%MP(1)
              CALL FMADD_R1(MZ02(1),MXY(3))
              KRADSV = KRAD
              KRAD = 0
              CALL FMMPYI(MZ05(1),90,MXY(4))
              CALL FMCSSN(MXY(4),MZ03(1),MZ03(2))
              KRAD = KRADSV
              CALL FMPI(MXY(3))
              CALL FMMPY_R1(MXY(3),MZ05(2))
              CALL FMDIVI_R1(MXY(3),-2)
              CALL FMEXP(MXY(3),MXY(5))
              CALL FMEQ(MXY(5),MXY(3))
              CALL FMMPYD(MXY(3),MZ03(1),MZ03(2),MZ06(1),MZ06(2))
              CALL ZMMPY(MZ02,MZ06,MZ01)
              CALL ZMEQ(MZ01,MZ02)
          ELSE

!                 (-i+c)**b = (-i)**b * (1 + b*c*i - ... )

              CALL ZM2I2M(0,-1,MZ02)
              CALL ZMSUB(MZ04,MZ02,MZ06)
              CALL ZMMPY(MZ05,MZ06,MZ02)
              DO J = 1, NDIG+2
                 MTEMP = MZ02(1)%MP(J)
                 MZ02(1)%MP(J) = MZ02(2)%MP(J)
                 MZ02(2)%MP(J) = MTEMP
              ENDDO
              IF (MZ02(1)%MP(2) /= MUNKNO .AND. MZ02(1)%MP(3) /= 0)  &
                  MZ02(1)%MP(1) = -MZ02(1)%MP(1)
              CALL FMADD_R1(MZ02(1),MXY(3))
              KRADSV = KRAD
              KRAD = 0
              CALL FMMPYI(MZ05(1),-90,MXY(4))
              CALL FMCSSN(MXY(4),MZ03(1),MZ03(2))
              KRAD = KRADSV
              CALL FMPI(MXY(3))
              CALL FMMPY_R1(MXY(3),MZ05(2))
              CALL FMDIVI_R1(MXY(3),2)
              CALL FMEXP(MXY(3),MXY(5))
              CALL FMEQ(MXY(5),MXY(3))
              CALL FMMPYD(MXY(3),MZ03(1),MZ03(2),MZ06(1),MZ06(2))
              CALL ZMMPY(MZ02,MZ06,MZ01)
              CALL ZMEQ(MZ01,MZ02)
          ENDIF
          GO TO 120
      ENDIF

      CALL ZMLN(MZ04,MZ06)
      CALL ZMMPY(MZ05,MZ06,MZ02)
      CALL FMEQ(MZ02(2),MZ01(1))
      IF (MZ01(1)%MP(2) > 25000) THEN
          KFLAG = -4
          CALL ZMST2M('UNKNOWN+UNKNOWN*i',MZ02)
          GO TO 130
      ENDIF
      KWRNSV = KWARN
      KWARN = 0
      CALL FMRDC(MZ01(1),JSIN,JCOS,JSWAP)
      KWARN = KWRNSV
      IEXTRA = INT(MZ02(2)%MP(2) - MZ01(1)%MP(2))
      IF (NDIG >= NDSAVE+NGRD52+MAX(0,IEXTRA)) IEXTRA = 0
      IF (IEXTRA > 1) THEN
          NDIG = NDIG + IEXTRA
          CALL ZMEQU_R1(MZ04,NDSAVE,NDIG)
          CALL ZMEQU_R1(MZ05,NDSAVE,NDIG)
          CALL ZMLN(MZ04,MZ06)
          CALL ZMMPY(MZ05,MZ06,MZ02)
      ENDIF

      CALL ZMEXP(MZ02,MZ04)
      CALL ZMEQ(MZ04,MZ02)


!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  120 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ02(1)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ02(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
  130 CALL ZMEXIT(MZ02,MC,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE ZMPWR

      SUBROUTINE ZMREAD(KREAD,MA)

!  Read MA on unit KREAD.  Multi-line numbers will have '&' as the last nonblank character on all
!  but the last line.  Only one number is allowed on the line(s).

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: KREAD
      TYPE(MULTI) :: MA(2)

      CHARACTER :: LINE(80)
      INTEGER :: J,K,L2,LB
      INTENT (IN) :: KREAD
      INTENT (INOUT) :: MA

      NCALL = NCALL + 1
      NAMEST(NCALL) = 'ZMREAD'
      LB = 0

  110 READ (KREAD,"(80A1)",ERR=120,END=120) LINE

!             Scan the line and look for '&'

      DO J = 1, 80
         IF (LINE(J) == '&') GO TO 110
         IF (LINE(J) /= ' ') THEN
             LB = LB + 1
             IF (LB > LMBUFZ) THEN

!                If CMBUFZ runs out of space, try to re-allocate it with a bigger size.

                 IF (LMBUFZ > 0) THEN
                     ALLOCATE(MOVE_CMBUFF(LMBUFZ),STAT=K)
                     IF (K /= 0) THEN
                         CALL FMDEFINE_ERROR
                     ENDIF
                     DO K = 1, LMBUFZ
                        MOVE_CMBUFF(K) = CMBUFZ(K)
                     ENDDO
                     DEALLOCATE(CMBUFZ)
                     L2 = MAX(10000,2*LMBUFZ)
                     ALLOCATE(CMBUFZ(L2),STAT=K)
                     IF (K /= 0) THEN
                         CALL FMDEFINE_ERROR
                     ENDIF
                     DO K = 1, L2
                        CMBUFZ(K) = ' '
                     ENDDO
                     DO K = 1, LMBUFZ
                        CMBUFZ(K) = MOVE_CMBUFF(K)
                     ENDDO
                     DEALLOCATE(MOVE_CMBUFF)
                     LMBUFZ = L2
                 ELSE
                     ALLOCATE(CMBUFZ(10000),STAT=K)
                     IF (K /= 0) THEN
                         CALL FMDEFINE_ERROR
                     ENDIF
                     LMBUFZ = 10000
                 ENDIF
             ENDIF
             CMBUFZ(LB) = LINE(J)
          ENDIF
      ENDDO

      NCALL = NCALL - 1
      CALL ZMINP(CMBUFZ,MA,1,LB)

      RETURN

!             If there is an error, return UNKNOWN.

  120 KFLAG = -4
      CALL ZMWARN
      CALL ZMST2M('UNKNOWN+UNKNOWN*i',MA)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE ZMREAD

      SUBROUTINE ZMREAL(MA,MBFM)

!  MBFM = REAL(MA)

!  MA is a complex ZM number, MBFM is a real FM number.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MBFM
      INTENT (IN) :: MA
      INTENT (INOUT) :: MBFM

      KFLAG = 0
      NCALL = NCALL + 1
      NAMEST(NCALL) = 'ZMREAL'
      IF (NTRACE /= 0) CALL ZMNTR(2,MA,MA,1)

      CALL FMEQ(MA(1),MBFM)

      IF (NTRACE /= 0) CALL FMNTR(1,MBFM,MBFM,1,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE ZMREAL

      SUBROUTINE ZMRPWR(MA,IVAL,JVAL,MB)

!  MB = MA ** (IVAL/JVAL)

!  Raise a ZM number to a rational power.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      INTEGER :: IVAL,JVAL
      REAL (KIND(1.0D0)) :: MA2,MR1,MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: IJSIGN,INVERT,IVAL2,J,JVAL2,K,KL,KOVUN,KR_RETRY,KST,L,LVAL,NDSAVE
      REAL :: XVAL

      DOUBLE PRECISION :: AR,BR,F,THETA,X
      INTEGER :: NSTACK(49)
      INTENT (IN) :: MA,IVAL,JVAL
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(2),MZ01(2),MZ02(2),MZ03(2),MZ04(2)

      IF (.NOT. ALLOCATED(MB(1)%MP)) THEN
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(1)%MP)
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MB(2)%MP)) THEN
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(2)%MP)
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NCALL = NCALL + 1
      NAMEST(NCALL) = 'ZMRPWR'
      NDSAVE = NDIG
      IF (NTRACE /= 0) THEN
          CALL ZMNTR(2,MA,MA,1)
          CALL FMNTRI(2,IVAL,0)
          CALL FMNTRI(2,JVAL,0)
      ENDIF
      KR_RETRY = 0
      KOVUN = 0
      IF (MA(1)%MP(2) == MEXPOV .OR. MA(1)%MP(2) == MEXPUN .OR.  &
          MA(2)%MP(2) == MEXPOV .OR. MA(2)%MP(2) == MEXPUN) KOVUN = 1

      IF (MBLOGS /= MBASE) CALL FMCONS
      KFLAG = 0
      IJSIGN = 1
      IVAL2 = ABS(IVAL)
      JVAL2 = ABS(JVAL)
      IF (IVAL > 0 .AND. JVAL < 0) IJSIGN = -1
      IF (IVAL < 0 .AND. JVAL > 0) IJSIGN = -1
      IF (IVAL2 > 0 .AND. JVAL2 > 0) CALL FMGCDI(IVAL2,JVAL2)

!             Check for special cases.

      IF (MA(1)%MP(2) == MUNKNO .OR. MA(2)%MP(2) == MUNKNO .OR.             &
          (IJSIGN <= 0 .AND. MA(1)%MP(3) == 0 .AND. MA(2)%MP(3) == 0) .OR.  &
          JVAL == 0) THEN
          MA2 = MA(1)%MP(3)
          KFLAG = -4
          IF (IVAL <= 0 .AND. MA2 == 0) CALL ZMWARN
          CALL ZMST2M('UNKNOWN+UNKNOWN*i',MB)
          IF (NTRACE /= 0) CALL ZMNTR(1,MB,MB,1)
          NCALL = NCALL - 1
          RETURN
      ENDIF

      IF (IVAL == 0) THEN
          CALL ZMI2M(1,MB)
          IF (NTRACE /= 0) CALL ZMNTR(1,MB,MB,1)
          NCALL = NCALL - 1
          RETURN
      ENDIF
      MXSAVE = MXEXP
      MXEXP = MXEXP2

!             Increase the working precision.

  110 IF (NCALL == 1) THEN
          XVAL = MAX(ABS(IVAL),ABS(JVAL)) + 1
          K = INT((5.0*REAL(DLOGTN) + LOG(XVAL))/ALOGMB + 2.0)
          NDIG = MAX(NDIG+K,2)
          IF (KR_RETRY >= 1) THEN
              NDIG = MAX(NDIG,2*NDSAVE+10)
          ENDIF
      ELSE
          XVAL = MAX(ABS(IVAL),ABS(JVAL)) + 1
          K = INT(LOG(XVAL)/ALOGMB + 1.0)
          NDIG = NDIG + K
      ENDIF
      IF (MBASE >= 100*ABS(MA(1)%MP(3)) .OR.  &
          MBASE >= 100*ABS(MA(2)%MP(3))) THEN
          NDIG = NDIG + 1
      ENDIF

      CALL ZMEQU(MA,MZ02,NDSAVE,NDIG)
      IF (IVAL2 == 1 .AND. JVAL2 == 2) THEN
          CALL ZMSQRT(MZ02,MZ04)
          IF (IJSIGN < 0) THEN
              CALL ZMI2M(1,MZ01)
              CALL ZMDIV(MZ01,MZ04,MZ02)
              CALL ZMEQ(MZ02,MZ04)
          ENDIF
          GO TO 120
      ENDIF

!             Generate the first approximation to MA**(1/JVAL2).

      CALL ZMI2M(0,MZ04)
      CALL FMDIG(NSTACK,KST)
      NDIG = NSTACK(1)
      CALL FMSQR(MZ02(1),MZ01(1))
      CALL FMSQR(MZ02(2),MXY(1))
      CALL FMADD_R1(MZ01(1),MXY(1))
      CALL FMSQRT_R1(MZ01(1))
      IF (MZ01(1)%MP(2) >= MEXPOV) THEN
          KFLAG = -4
          CALL ZMWARN
          MXEXP = MXSAVE
          NDIG = NDSAVE
          CALL ZMST2M('UNKNOWN+UNKNOWN*i',MB)
          IF (NTRACE /= 0) CALL ZMNTR(1,MB,MB,1)
          NCALL = NCALL - 1
          RETURN
      ENDIF

!             Invert MA if ABS(MA) >= 1 and IVAL or JVAL is large.

      INVERT = 0
      IF (IVAL > 5 .OR. JVAL > 5) THEN
          IF (MZ01(1)%MP(2) > 0 .AND. (MZ02(2)%MP(3) /= 0 .OR.  &
              MZ02(1)%MP(1) > 0)) THEN
              INVERT = 1
              NDIG = NSTACK(KST)
              CALL ZMI2M(1,MZ04)
              CALL ZMDIV(MZ04,MZ02,MZ03)
              CALL ZMEQ(MZ03,MZ02)
              NDIG = NSTACK(1)
              CALL FMDIV_R2(MZ04(1),MZ01(1))
          ENDIF
      ENDIF

      CALL FMDIV(MZ02(1),MZ01(1),MXY(1))
      IF (MXY(1)%MP(2) == MUNKNO) THEN
          KFLAG = -4
          CALL ZMWARN
          MXEXP = MXSAVE
          NDIG = NDSAVE
          CALL ZMST2M('UNKNOWN+UNKNOWN*i',MB)
          IF (NTRACE /= 0) CALL ZMNTR(1,MB,MB,1)
          NCALL = NCALL - 1
          RETURN
      ENDIF
      CALL FMM2DP(MXY(1),AR)
      CALL FMDIV(MZ02(2),MZ01(1),MXY(1))
      IF (MXY(1)%MP(2) == MUNKNO) THEN
          KFLAG = -4
          CALL ZMWARN
          MXEXP = MXSAVE
          NDIG = NDSAVE
          CALL ZMST2M('UNKNOWN+UNKNOWN*i',MB)
          IF (NTRACE /= 0) CALL ZMNTR(1,MB,MB,1)
          NCALL = NCALL - 1
          RETURN
      ENDIF
      CALL FMM2DP(MXY(1),BR)
      MR1 = MZ01(1)%MP(2)
      MZ01(1)%MP(2) = 0
      CALL FMM2DP(MZ01(1),X)
      L = INT(MR1/JVAL2)
      F = MR1/DBLE(JVAL2) - L
      X = X**(1.0D0/JVAL2) * DBLE(MBASE)**F
      CALL FMDPM(X,MXY(1))
      MXY(1)%MP(2) = MXY(1)%MP(2) + L

      THETA = ATAN2(BR,AR)
      X = COS(THETA/JVAL2)
      CALL FMDPM(X,MZ04(1))
      X = SIN(THETA/JVAL2)
      CALL FMDPM(X,MZ04(2))
      CALL FMMPY_R2(MXY(1),MZ04(1))
      CALL FMMPY_R2(MXY(1),MZ04(2))

!             Newton iteration.

      DO J = 1, KST
         NDIG = NSTACK(J)
         IF (J < KST) NDIG = NDIG + 1
         LVAL = JVAL2 - 1
         CALL ZMIPWR(MZ04,LVAL,MZ01)
         CALL ZMDIV(MZ02,MZ01,MZ03)
         CALL ZMMPYI(MZ04,LVAL,MZ01)
         CALL ZMADD(MZ01,MZ03,MZ04)
         CALL ZMDIVI(MZ04,JVAL2,MZ03)
         CALL ZMEQ(MZ03,MZ04)
      ENDDO

      CALL ZMIPWR(MZ03,IJSIGN*IVAL2,MZ04)
      IF (INVERT == 1) THEN
          CALL ZMI2M(1,MZ01)
          CALL ZMDIV(MZ01,MZ04,MZ03)
          CALL ZMEQ(MZ03,MZ04)
      ENDIF

!             Round the result and return.

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  120 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ04(1)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ04(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL ZMEXIT(MZ04,MB,NDSAVE,MXSAVE,KOVUN)
      IF (KFLAG == 1) KFLAG = 0
      RETURN
      END SUBROUTINE ZMRPWR

      SUBROUTINE ZMRSLT(MC,KRESLT)

!  Handle results that are special cases, such as overflow, underflow, and unknown.

!  MC is the result that is returned

!  KRESLT is the result code.  Result codes handled here:

!   0 - Perform the normal operation
!  12 - The result is 'UNKNOWN'

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MC(2)
      INTEGER :: KRESLT

      INTEGER :: KFSAVE
      INTENT (IN) :: KRESLT
      INTENT (INOUT) :: MC

      KFSAVE = KFLAG

      IF (KRESLT == 12 .OR. KRESLT < 0 .OR. KRESLT > 15) THEN
          CALL ZMST2M('UNKNOWN+UNKNOWN*i',MC)
          KFLAG = KFSAVE
          RETURN
      ENDIF

      RETURN
      END SUBROUTINE ZMRSLT

      SUBROUTINE ZMSIN(MA,MB)

!  MB = SIN(MA).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,KL,KOVUN,KR_RETRY,KRESLT,KRSAVE,NDSAVE
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(5),MZ01(2),MZ02(2)

      IF (.NOT. ALLOCATED(MB(1)%MP)) THEN
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(1)%MP)
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MB(2)%MP)) THEN
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(2)%MP)
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL ZMENTR('ZMSIN    ',MA,MA,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF
      KR_RETRY = 0
      KRSAVE = KRAD
      KRAD = 1

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL ZMEQU(MA,MZ02,NDSAVE,NDIG)

!             Check for special cases.

      IF (MA(1)%MP(3) == 0 .AND. MA(2)%MP(3) == 0) THEN
          CALL ZMI2M(0,MZ01)
          GO TO 120
      ELSE IF (MA(1)%MP(2) < (-NDIG) .AND. MA(2)%MP(2) < (-NDIG)) THEN
          CALL ZMEQ(MZ02,MZ01)
          GO TO 120
      ELSE IF (MA(2)%MP(3) == 0) THEN
          CALL FMSIN(MZ02(1),MZ01(1))
          CALL FMI2M(0,MZ01(2))
          GO TO 120
      ELSE IF (MA(1)%MP(3) == 0) THEN
          CALL FMSINH(MZ02(2),MZ01(2))
          CALL FMI2M(0,MZ01(1))
          GO TO 120
      ENDIF

!             Find COS(REAL(MA)) and SIN(REAL(MA)).

      CALL FMCSSN(MZ02(1),MZ01(2),MZ01(1))

!             Find COSH(IMAG(MA)) and SINH(IMAG(MA)).

      CALL FMCHSH(MZ02(2),MXY(1),MXY(2))

!             SIN(MA) =  SIN(REAL(MA))*COSH(IMAG(MA)) + COS(REAL(MA))*SINH(IMAG(MA)) i

      IF (MXY(1)%MP(2) == MEXPOV) THEN
          CALL FMABS(MZ01(1),MXY(1))
          CALL FMDIVI_R1(MXY(1),2)
          CALL FMLN(MXY(1),MXY(2))
          CALL FMABS(MZ02(2),MXY(3))
          CALL FMADD(MXY(2),MXY(3),MXY(5))
          CALL FMEXP(MXY(5),MXY(4))
          IF (MZ01(1)%MP(1) < 0) CALL FMMPYI_R1(MXY(4),-1)

          CALL FMABS(MZ01(2),MXY(1))
          CALL FMDIVI_R1(MXY(1),2)
          CALL FMLN(MXY(1),MXY(2))
          CALL FMADD(MXY(2),MXY(3),MXY(1))
          CALL FMEXP(MXY(1),MXY(5))
          IF (MZ02(2)%MP(1) < 0) CALL FMMPYI_R1(MXY(5),-1)
          IF (MZ01(2)%MP(1) < 0) CALL FMMPYI_R1(MXY(5),-1)

          CALL FMEQ(MXY(4),MZ01(1))
          CALL FMEQ(MXY(5),MZ01(2))
      ELSE
          CALL FMMPY_R1(MZ01(1),MXY(1))
          CALL FMMPY_R1(MZ01(2),MXY(2))
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  120 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(1)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL ZMEXIT(MZ01,MB,NDSAVE,MXSAVE,KOVUN)
      KRAD = KRSAVE
      RETURN
      END SUBROUTINE ZMSIN

      SUBROUTINE ZMSINH(MA,MB)

!  MB = SINH(MA).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,KL,KOVUN,KR_RETRY,KRESLT,KRSAVE,NDSAVE
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(5),MZ01(2),MZ02(2)

      IF (.NOT. ALLOCATED(MB(1)%MP)) THEN
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(1)%MP)
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MB(2)%MP)) THEN
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(2)%MP)
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL ZMENTR('ZMSINH   ',MA,MA,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF
      KR_RETRY = 0
      KRSAVE = KRAD
      KRAD = 1

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL ZMEQU(MA,MZ02,NDSAVE,NDIG)

!             Check for special cases.

      IF (MA(1)%MP(3) == 0 .AND. MA(2)%MP(3) == 0) THEN
          CALL ZMI2M(0,MZ01)
          GO TO 120
      ELSE IF (MA(1)%MP(2) < (-NDIG) .AND. MA(2)%MP(2) < (-NDIG)) THEN
          CALL ZMEQ(MZ02,MZ01)
          GO TO 120
      ELSE IF (MA(1)%MP(3) == 0) THEN
          CALL FMSIN(MZ02(2),MZ01(2))
          CALL FMI2M(0,MZ01(1))
          GO TO 120
      ELSE IF (MA(2)%MP(3) == 0) THEN
          CALL FMSINH(MZ02(1),MZ01(1))
          CALL FMI2M(0,MZ01(2))
          GO TO 120
      ENDIF

!             Find SIN(IMAG(MA)) and COS(IMAG(MA)).

      CALL FMCSSN(MZ02(2),MZ01(1),MZ01(2))

!             Find SINH(REAL(MA)) and COSH(REAL(MA)).

      CALL FMCHSH(MZ02(1),MXY(1),MXY(2))

!             SINH(MA) =  SINH(REAL(MA))*COS(IMAG(MA)) + COSH(REAL(MA))*SIN(IMAG(MA)) i

      IF (MXY(1)%MP(2) == MEXPOV) THEN
          CALL FMABS(MZ01(1),MXY(1))
          CALL FMDIVI_R1(MXY(1),2)
          CALL FMLN(MXY(1),MXY(2))
          CALL FMABS(MZ02(1),MXY(3))
          CALL FMADD(MXY(2),MXY(3),MXY(5))
          CALL FMEXP(MXY(5),MXY(4))
          IF (MZ01(1)%MP(1) < 0) CALL FMMPYI_R1(MXY(4),-1)
          IF (MZ02(1)%MP(1) < 0) CALL FMMPYI_R1(MXY(4),-1)

          CALL FMABS(MZ01(2),MXY(1))
          CALL FMDIVI_R1(MXY(1),2)
          CALL FMLN(MXY(1),MXY(2))
          CALL FMADD(MXY(2),MXY(3),MXY(1))
          CALL FMEXP(MXY(1),MXY(5))
          IF (MZ01(2)%MP(1) < 0) CALL FMMPYI_R1(MXY(5),-1)

          CALL FMEQ(MXY(4),MZ01(1))
          CALL FMEQ(MXY(5),MZ01(2))
      ELSE
          CALL FMMPY_R1(MZ01(1),MXY(2))
          CALL FMMPY_R1(MZ01(2),MXY(1))
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  120 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(1)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      CALL ZMEXIT(MZ01,MB,NDSAVE,MXSAVE,KOVUN)
      KRAD = KRSAVE
      RETURN
      END SUBROUTINE ZMSINH

      SUBROUTINE ZMSQR(MA,MB)

!  MB = MA * MA

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,KL,KOVUN,KR_RETRY,KRESLT,KWRNSV,NDGSV2,NDSAVE,NTRSAV
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(2),MZ01(2),MZ02(2)

      IF (.NOT. ALLOCATED(MB(1)%MP)) THEN
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(1)%MP)
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MB(2)%MP)) THEN
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(2)%MP)
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (ABS(MA(1)%MP(2)) > MEXPAB .OR. ABS(MA(2)%MP(2)) > MEXPAB .OR.  &
          KDEBUG >= 1) THEN
          CALL ZMENTR('ZMSQR    ',MA,MA,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
          IF (KRESLT /= 0) THEN
              RETURN
          ENDIF
      ELSE
          NCALL = NCALL + 1
          IF (NTRACE /= 0) THEN
              NAMEST(NCALL) = 'ZMSQR'
              CALL ZMNTR(2,MA,MA,1)
          ENDIF
          NDSAVE = NDIG
          IF (NCALL == 1) THEN
              NDIG = MAX(NDIG+NGRD52,2)
              IF (MBASE >= 100*ABS(MA(1)%MP(3)) .OR.  &
                  MBASE >= 100*ABS(MA(2)%MP(3))) THEN
                  NDIG = NDIG + 1
              ENDIF
          ENDIF
          MXSAVE = MXEXP
          MXEXP = MXEXP2
          KOVUN = 0
      ENDIF

      KR_RETRY = 0
      NTRSAV = NTRACE
      NTRACE = 0
      KWRNSV = KWARN
      KWARN = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL ZMEQU(MA,MZ02,NDSAVE,NDIG)

!             Check for special cases.

      IF (MA(2)%MP(3) == 0) THEN
          CALL FMSQR(MZ02(1),MZ01(1))
          CALL FMI2M(0,MZ01(2))
      ELSE IF (MA(1)%MP(3) == 0) THEN
          CALL FMSQR(MZ02(2),MZ01(1))
          IF (MZ01(1)%MP(2) /= MUNKNO .AND. MZ01(1)%MP(3) /= 0)  &
              MZ01(1)%MP(1) = -MZ01(1)%MP(1)
          CALL FMI2M(0,MZ01(2))
      ELSE
          CALL FMADD(MZ02(1),MZ02(2),MXY(1))
          CALL FMSUB(MZ02(1),MZ02(2),MXY(2))
          CALL FMMPY(MXY(1),MXY(2),MZ01(1))
          CALL FMMPY(MZ02(1),MZ02(2),MXY(2))
          CALL FMADD(MXY(2),MXY(2),MZ01(2))
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(1)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF

      MXEXP = MXSAVE
      NTRACE = NTRSAV
      NDGSV2 = NDIG
      NDIG = NDSAVE
      KWARN = KWRNSV
      CALL ZMEQU(MZ01,MB,NDGSV2,NDSAVE)
      IF (MB(1)%MP(2) >= MEXPOV .OR. MB(1)%MP(2) <= -MEXPOV .OR.  &
          MB(2)%MP(2) >= MEXPOV .OR. MB(2)%MP(2) <= -MEXPOV) THEN
          IF (MB(1)%MP(2) == MUNKNO .OR. MB(2)%MP(2) == MUNKNO) THEN
              KFLAG = -4
          ELSE IF (MB(1)%MP(2) == MEXPOV .OR. MB(2)%MP(2) == MEXPOV) THEN
              KFLAG = -5
          ELSE IF (MB(1)%MP(2) == MEXPUN .OR. MB(2)%MP(2) == MEXPUN) THEN
              KFLAG = -6
          ENDIF
          IF ((MB(1)%MP(2) == MUNKNO) .OR. (MB(2)%MP(2) == MUNKNO)  &
             .OR. (MB(1)%MP(2) == MEXPUN .AND. KOVUN == 0)          &
             .OR. (MB(2)%MP(2) == MEXPUN .AND. KOVUN == 0)          &
             .OR. (MB(1)%MP(2) == MEXPOV .AND. KOVUN == 0)          &
             .OR. (MB(2)%MP(2) == MEXPOV .AND. KOVUN == 0)) THEN
              NAMEST(NCALL) = 'ZMSQR'
              CALL ZMWARN
          ENDIF
      ENDIF
      IF (NTRACE /= 0) CALL ZMNTR(1,MB,MB,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE ZMSQR

      SUBROUTINE ZMSQRT(MA,MB)

!  MB = SQRT(MA).  Principal Square Root.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      REAL (KIND(1.0D0)) :: MXEXP1,MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: J,KL,KOVUN,KR_RETRY,KRESLT,KWRNSV,NDSAVE,NTRSAV

      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(3),MZ01(2),MZ02(2)

      IF (.NOT. ALLOCATED(MB(1)%MP)) THEN
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(1)%MP)
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MB(2)%MP)) THEN
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(2)%MP)
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (ABS(MA(1)%MP(2)) > MEXPAB .OR. ABS(MA(2)%MP(2)) > MEXPAB .OR.  &
          KDEBUG >= 1) THEN
          CALL ZMENTR('ZMSQRT   ',MA,MA,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
          IF (KRESLT /= 0) THEN
              RETURN
          ENDIF
      ELSE
          NCALL = NCALL + 1
          IF (NTRACE /= 0) THEN
              NAMEST(NCALL) = 'ZMSQRT'
              CALL ZMNTR(2,MA,MA,1)
          ENDIF
          NDSAVE = NDIG
          IF (NCALL == 1) THEN
              NDIG = MAX(NDIG+NGRD52,2)
              IF (MBASE >= 100*ABS(MA(1)%MP(3)) .OR.  &
                  MBASE >= 100*ABS(MA(2)%MP(3))) THEN
                  NDIG = NDIG + 1
              ENDIF
          ENDIF
          MXSAVE = MXEXP
          MXEXP = MXEXP2
          KOVUN = 0
      ENDIF

      NTRSAV = NTRACE
      NTRACE = 0
      KWRNSV = KWARN
      KWARN = 0
      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL ZMEQU(MA,MZ02,NDSAVE,NDIG)

!             Check for special cases.

      MXEXP1 = INT(MXEXP2/2.01D0)
      IF (MA(1)%MP(3) == 0 .AND. MA(2)%MP(3) == 0) THEN
          CALL ZMI2M(0,MZ01)
          GO TO 120
      ELSE IF (MA(1)%MP(3) == 0) THEN
          CALL FMABS(MZ02(2),MXY(1))
          CALL FMDIVI(MXY(1),2,MXY(3))
          CALL FMSQRT_R1(MXY(3))
      ELSE IF (MA(2)%MP(3) == 0) THEN
          CALL FMABS(MZ02(1),MXY(3))
          CALL FMSQRT_R1(MXY(3))
      ELSE IF (MA(1)%MP(2) == MEXPUN) THEN
          IF (MA(2)%MP(2) <= -MXEXP1+NDIG+1) THEN
              CALL ZMST2M('UNKNOWN + UNKNOWN i',MZ01)
              GO TO 120
          ENDIF
      ELSE IF (MA(2)%MP(2) == MEXPUN) THEN
          IF (MA(1)%MP(2) <= -MXEXP1+NDIG+1) THEN
              CALL ZMST2M('UNKNOWN + UNKNOWN i',MZ01)
              GO TO 120
          ENDIF
          CALL FMSQR(MZ02(1),MXY(1))
          CALL FMSQR(MZ02(2),MXY(2))
          CALL FMADD(MXY(1),MXY(2),MXY(3))
          CALL FMSQRT_R1(MXY(3))
          IF (MXY(3)%MP(2) == MUNKNO) THEN
              CALL FMABS(MZ02(1),MXY(1))
              CALL FMABS(MZ02(2),MXY(2))
              CALL FMMAX(MXY(1),MXY(2),MZ01(1))
              CALL FMMIN(MXY(1),MXY(2),MZ01(2))
              CALL FMDIV(MZ01(2),MZ01(1),MXY(3))
              CALL FMI2M(1,MXY(1))
              CALL FMADD(MXY(1),MXY(3),MXY(2))
              CALL FMSQRT_R1(MXY(2))
              CALL FMMPY(MZ01(1),MXY(2),MXY(3))
          ENDIF
          CALL FMABS(MZ02(1),MXY(2))
          CALL FMADD_R2(MXY(2),MXY(3))
          CALL FMDIVI_R1(MXY(3),2)
          CALL FMSQRT_R1(MXY(3))
      ELSE
          CALL FMSQR(MZ02(1),MXY(1))
          CALL FMSQR(MZ02(2),MXY(2))
          CALL FMADD(MXY(1),MXY(2),MXY(3))
          CALL FMSQRT_R1(MXY(3))
          IF (MXY(3)%MP(2) == MUNKNO) THEN
              CALL FMABS(MZ02(1),MXY(1))
              CALL FMABS(MZ02(2),MXY(2))
              CALL FMMAX(MXY(1),MXY(2),MZ01(1))
              CALL FMMIN(MXY(1),MXY(2),MZ01(2))
              CALL FMDIV(MZ01(2),MZ01(1),MXY(3))
              CALL FMI2M(1,MXY(1))
              CALL FMADD(MXY(1),MXY(3),MXY(2))
              CALL FMSQRT_R1(MXY(2))
              CALL FMMPY(MZ01(1),MXY(2),MXY(3))
          ENDIF
          CALL FMABS(MZ02(1),MXY(2))
          CALL FMADD_R2(MXY(2),MXY(3))
          CALL FMDIVI_R1(MXY(3),2)
          CALL FMSQRT_R1(MXY(3))
      ENDIF

      CALL FMADD(MXY(3),MXY(3),MXY(2))
      IF (MA(1)%MP(1) >= 0) THEN
          CALL FMDIV(MZ02(2),MXY(2),MZ01(2))
          CALL FMEQ(MXY(3),MZ01(1))
      ELSE
          IF (MA(2)%MP(1) >= 0) THEN
              CALL FMDIV(MZ02(2),MXY(2),MZ01(1))
              CALL FMEQ(MXY(3),MZ01(2))
          ELSE
              CALL FMDIV(MZ02(2),MXY(2),MZ01(1))
              CALL FMEQ(MXY(3),MZ01(2))
              IF (MZ01(1)%MP(2) /= MUNKNO .AND. MZ01(1)%MP(3) /= 0)  &
                  MZ01(1)%MP(1) = -MZ01(1)%MP(1)
              IF (MZ01(2)%MP(2) /= MUNKNO .AND. MZ01(2)%MP(3) /= 0)  &
                  MZ01(2)%MP(1) = -MZ01(2)%MP(1)
          ENDIF
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  120 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(1)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(2)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      MXEXP = MXSAVE
      CALL ZMEQU(MZ01,MB,NDIG,NDSAVE)

      IF (MB(1)%MP(2) == MUNKNO .OR. MB(2)%MP(2) == MUNKNO) THEN
          KFLAG = -4
      ELSE IF (MB(1)%MP(2) == MEXPOV .OR. MB(2)%MP(2) == MEXPOV) THEN
          KFLAG = -5
      ELSE IF (MB(1)%MP(2) == MEXPUN .OR. MB(2)%MP(2) == MEXPUN) THEN
          KFLAG = -6
      ENDIF
      NTRACE = NTRSAV
      NDIG = NDSAVE
      KWARN = KWRNSV
      IF ((MB(1)%MP(2) == MUNKNO) .OR. (MB(2)%MP(2) == MUNKNO)  &
         .OR. (MB(1)%MP(2) == MEXPUN .AND. KOVUN == 0)          &
         .OR. (MB(2)%MP(2) == MEXPUN .AND. KOVUN == 0)          &
         .OR. (MB(1)%MP(2) == MEXPOV .AND. KOVUN == 0)          &
         .OR. (MB(2)%MP(2) == MEXPOV .AND. KOVUN == 0)) THEN
          NAMEST(NCALL) = 'ZMSQRT'
          CALL ZMWARN
      ENDIF
      IF (NTRACE /= 0) CALL ZMNTR(1,MB,MB,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE ZMSQRT

      SUBROUTINE ZMST2M(STRING,MA)

!  MA = STRING

!  Convert a character string to FM format.
!  This is often more convenient than using ZMINP, which converts an array of character(1) values.

      USE ModLib_FMVALS
      IMPLICIT NONE

      CHARACTER(*) :: STRING
      TYPE(MULTI) :: MA(2)

      INTEGER :: J,LB,KFSAVE
      INTENT (IN) :: STRING
      INTENT (INOUT) :: MA

      NCALL = NCALL + 1
      NAMEST(NCALL) = 'ZMST2M'
      LB = LEN(STRING)
      KFSAVE = KFLAG

      IF (LB > LMBUFZ) THEN
          IF (LMBUFZ > 0) DEALLOCATE(CMBUFZ)
          ALLOCATE(CMBUFZ(LB),STAT=J)
          IF (J /= 0) THEN
              CALL FMDEFINE_ERROR
          ENDIF
          LMBUFZ = LB
      ENDIF
      DO J = 1, LB
         CMBUFZ(J) = STRING(J:J)
      ENDDO

      NCALL = NCALL - 1
      CALL ZMINP(CMBUFZ,MA,1,LB)

      IF (KFSAVE /= 0) KFLAG = KFSAVE
      RETURN
      END SUBROUTINE ZMST2M

      SUBROUTINE ZMSUB(MA,MB,MC)

!  MC = MA - MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2),MC(2)
      INTEGER :: KF1,KOVUN,KRESLT,KWRNSV,NDSAVE,NTRSAV
      REAL (KIND(1.0D0)) :: MXSAVE
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC

      IF (ABS(MA(1)%MP(2)) > MEXPAB .OR. ABS(MA(2)%MP(2)) > MEXPAB .OR.  &
          ABS(MB(1)%MP(2)) > MEXPAB .OR. ABS(MB(2)%MP(2)) > MEXPAB .OR.  &
          KDEBUG >= 1) THEN
          CALL ZMENTR('ZMSUB    ',MA,MB,2,MC,KRESLT,NDSAVE,MXSAVE,KOVUN)
          IF (KRESLT /= 0) THEN
              RETURN
          ENDIF
          NDIG = NDSAVE
          MXEXP = MXSAVE
      ELSE
          NCALL = NCALL + 1
          IF (NTRACE /= 0) THEN
              NAMEST(NCALL) = 'ZMSUB'
              CALL ZMNTR(2,MA,MB,2)
          ENDIF
          KOVUN = 0
      ENDIF

!             Force FMSUB to use more guard digits for user calls.

      NCALL = NCALL - 1
      NTRSAV = NTRACE
      NTRACE = 0
      KWRNSV = KWARN
      KWARN = 0

      CALL FMSUB(MA(1),MB(1),MC(1))
      KF1 = KFLAG
      CALL FMSUB(MA(2),MB(2),MC(2))

      NTRACE = NTRSAV
      KWARN = KWRNSV
      NCALL = NCALL + 1
      IF (NTRACE /= 0) NAMEST(NCALL) = 'ZMSUB'
      IF (KFLAG == 1) KFLAG = KF1

      IF (MC(1)%MP(2) == MUNKNO .OR. MC(2)%MP(2) == MUNKNO) THEN
          KFLAG = -4
      ELSE IF (MC(1)%MP(2) == MEXPOV .OR. MC(2)%MP(2) == MEXPOV) THEN
          KFLAG = -5
      ELSE IF (MC(1)%MP(2) == MEXPUN .OR. MC(2)%MP(2) == MEXPUN) THEN
          KFLAG = -6
      ENDIF
      IF ((MC(1)%MP(2) == MUNKNO) .OR. (MC(2)%MP(2) == MUNKNO)  &
         .OR. (MC(1)%MP(2) == MEXPUN .AND. KOVUN == 0)          &
         .OR. (MC(2)%MP(2) == MEXPUN .AND. KOVUN == 0)          &
         .OR. (MC(1)%MP(2) == MEXPOV .AND. KOVUN == 0)          &
         .OR. (MC(2)%MP(2) == MEXPOV .AND. KOVUN == 0)) THEN
          NAMEST(NCALL) = 'ZMSUB'
          CALL ZMWARN
      ENDIF
      IF (NTRACE /= 0) CALL ZMNTR(1,MC,MC,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE ZMSUB

      SUBROUTINE ZMSUB_R1(MA,MB)

!  MA = MA - MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (INOUT) :: MA
      INTENT (IN) :: MB

      TYPE(MULTI) :: MXY(2)


      CALL ZMSUB(MA,MB,MXY)
      CALL ZMEQ(MXY,MA)


      END SUBROUTINE ZMSUB_R1

      SUBROUTINE ZMSUB_R2(MA,MB)

!  MB = MA - MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB

      TYPE(MULTI) :: MXY(2)


      CALL ZMSUB(MA,MB,MXY)
      CALL ZMEQ(MXY,MB)


      END SUBROUTINE ZMSUB_R2

      SUBROUTINE ZMTAN(MA,MB)

!  MB = TAN(MA).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: IEXTRA,J,K,KL,KOVUN,KR_RETRY,KRESLT,KRSAVE,NDSAVE,NGOAL,N_ACC
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(3),MZ01(2),MZ02(2),MZ03(2)

      IF (.NOT. ALLOCATED(MB(1)%MP)) THEN
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(1)%MP)
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MB(2)%MP)) THEN
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(2)%MP)
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL ZMENTR('ZMTAN    ',MA,MA,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF
      KR_RETRY = 0
      KRSAVE = KRAD
      KRAD = 1

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL ZMEQU(MA,MZ02,NDSAVE,NDIG)
      N_ACC = NINT(NDIG*ALOGM2)

!             Check for special cases.

      IF (MA(1)%MP(3) == 0 .AND. MA(2)%MP(3) == 0) THEN
          CALL ZMI2M(0,MZ01)
          GO TO 120
      ELSE IF (MA(1)%MP(2) < (-NDIG) .AND. MA(2)%MP(2) < (-NDIG)) THEN
          CALL ZMEQ(MZ02,MZ01)
          GO TO 120
      ELSE IF (MA(2)%MP(3) == 0) THEN
          CALL FMTAN(MZ02(1),MZ01(1))
          CALL FMI2M(0,MZ01(2))
          GO TO 120
      ELSE IF (MA(1)%MP(3) == 0) THEN
          CALL FMTANH(MZ02(2),MZ01(2))
          CALL FMI2M(0,MZ01(1))
          GO TO 120
      ENDIF

!             Find SIN(2*REAL(MA)) and COS(2*REAL(MA)).

      CALL FMADD(MZ02(1),MZ02(1),MZ01(1))
      CALL FMCSSN(MZ01(1),MZ01(2),MXY(2))
      CALL FMEQ(MXY(2),MZ01(1))

!             Find SINH(2*IMAG(MA)) and COSH(2*IMAG(MA)).

      CALL FMADD(MZ02(2),MZ02(2),MXY(2))
      CALL FMCHSH(MXY(2),MXY(1),MXY(3))
      CALL FMEQ(MXY(3),MXY(2))

!             TAN(MA) =  SIN(2*REAL(MA))  / (COS(2*REAL(MA))+COSH(2*IMAG(MA)) +
!                        SINH(2*IMAG(MA)) / (COS(2*REAL(MA))+COSH(2*IMAG(MA)) i

      CALL FMADD(MZ01(2),MXY(1),MXY(3))
      CALL FMCANCEL(MZ01(2),MXY(1),MXY(3),K)
      N_ACC = N_ACC - K
      CALL FMEQ(MXY(3),MXY(1))
      IF (MXY(1)%MP(3) == 0) THEN
          NGOAL = INT(REAL(NDSAVE)*ALOGM2) + 7
          GO TO 130
      ELSE IF (MXY(1)%MP(2) == MEXPOV) THEN
          CALL FMDIV_R1(MZ01(1),MXY(1))
          CALL FMIM(1,MZ01(2))
          IF (MXY(2)%MP(1) < 0 .AND. MZ01(2)%MP(2) /= MUNKNO .AND.  &
              MZ01(2)%MP(3) /= 0) MZ01(2)%MP(1) = -MZ01(2)%MP(1)
      ELSE
          CALL FMDIVD(MZ01(1),MXY(2),MXY(1),MZ03(1),MZ03(2))
          CALL ZMEQ(MZ03,MZ01)
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  120 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(1)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(2)%MP(J+NDSAVE+2)) / MBASE
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
          NGOAL = INT(REAL(NDSAVE)*ALOGM2) + 7
      ELSE
          NGOAL = INT(-MXEXP2)
      ENDIF
  130 IF (N_ACC <= NGOAL) THEN
          IEXTRA = INT(REAL(NGOAL-N_ACC)/ALOGM2 + 23.03/ALOGMB) + 1
          NDIG = NDIG + IEXTRA
          GO TO 110
      ENDIF

      CALL ZMEXIT(MZ01,MB,NDSAVE,MXSAVE,KOVUN)
      KRAD = KRSAVE
      RETURN
      END SUBROUTINE ZMTAN

      SUBROUTINE ZMTANH(MA,MB)

!  MB = TANH(MA).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR
      INTEGER :: IEXTRA,J,K,KL,KOVUN,KR_RETRY,KRESLT,KRSAVE,NDSAVE,NGOAL,N_ACC
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(3),MZ01(2),MZ02(2),MZ03(2)

      IF (.NOT. ALLOCATED(MB(1)%MP)) THEN
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(1)%MP)
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MB(2)%MP)) THEN
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(2)%MP)
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL ZMENTR('ZMTANH   ',MA,MA,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF
      KR_RETRY = 0
      KRSAVE = KRAD
      KRAD = 1

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL ZMEQU(MA,MZ02,NDSAVE,NDIG)
      N_ACC = NINT(NDIG*ALOGM2)

!             Check for special cases.

      IF (MA(1)%MP(3) == 0 .AND. MA(2)%MP(3) == 0) THEN
          CALL ZMI2M(0,MZ01)
          GO TO 120
      ELSE IF (MA(1)%MP(2) < (-NDIG) .AND. MA(2)%MP(2) < (-NDIG)) THEN
          CALL ZMEQ(MZ02,MZ01)
          GO TO 120
      ELSE IF (MA(1)%MP(3) == 0) THEN
          CALL FMTAN(MZ02(2),MZ01(2))
          CALL FMI2M(0,MZ01(1))
          GO TO 120
      ELSE IF (MA(2)%MP(3) == 0) THEN
          CALL FMTANH(MZ02(1),MZ01(1))
          CALL FMI2M(0,MZ01(2))
          GO TO 120
      ENDIF

!             Find SIN(2*IMAG(MA)) and COS(2*IMAG(MA)).

      CALL FMADD(MZ02(2),MZ02(2),MZ01(1))
      CALL FMCSSN(MZ01(1),MZ01(2),MXY(2))
      CALL FMEQ(MXY(2),MZ01(1))

!             Find SINH(2*REAL(MA)) and COSH(2*REAL(MA)).

      CALL FMADD(MZ02(1),MZ02(1),MXY(2))
      CALL FMCHSH(MXY(2),MXY(1),MXY(3))
      CALL FMEQ(MXY(3),MXY(2))

!             TANH(MA) =  SINH(2*REAL(MA)) / (COS(2*IMAG(MA))+COSH(2*REAL(MA)) +
!                         SIN(2*IMAG(MA))  / (COS(2*IMAG(MA))+COSH(2*REAL(MA)) i

      CALL FMADD(MZ01(2),MXY(1),MXY(3))
      CALL FMCANCEL(MZ01(2),MXY(1),MXY(3),K)
      N_ACC = N_ACC - K
      CALL FMEQ(MXY(3),MXY(1))
      IF (MXY(1)%MP(3) == 0) THEN
          NGOAL = INT(REAL(NDSAVE)*ALOGM2) + 7
          GO TO 130
      ELSE IF (MXY(1)%MP(2) == MEXPOV) THEN
          CALL FMDIV(MZ01(1),MXY(1),MZ01(2))
          CALL FMIM(1,MZ01(1))
          IF (MXY(2)%MP(1) < 0) MZ01(1)%MP(1) = -1
      ELSE
          CALL FMDIVD(MZ01(1),MXY(2),MXY(1),MZ03(2),MZ03(1))
          CALL ZMEQ(MZ03,MZ01)
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

  120 IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(1)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR > 0.498 .AND. ERR < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR > 0.998 .OR. ERR < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (NCALL >= 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MZ01(2)%MP(J+NDSAVE+2)) / MBASE
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
          NGOAL = INT(REAL(NDSAVE)*ALOGM2) + 7
      ELSE
          NGOAL = INT(-MXEXP2)
      ENDIF
  130 IF (N_ACC <= NGOAL) THEN
          IEXTRA = INT(REAL(NGOAL-N_ACC)/ALOGM2 + 23.03/ALOGMB) + 1
          NDIG = NDIG + IEXTRA
          GO TO 110
      ENDIF

      CALL ZMEXIT(MZ01,MB,NDSAVE,MXSAVE,KOVUN)
      KRAD = KRSAVE
      RETURN
      END SUBROUTINE ZMTANH

      SUBROUTINE ZMUNPK(MP,MA)

!  MP is unpacked and the value returned in MA.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MP(2)
      INTENT (IN) :: MP
      INTENT (INOUT) :: MA

      CALL FMUNPK(MP(1),MA(1))
      CALL FMUNPK(MP(2),MA(2))
      RETURN
      END SUBROUTINE ZMUNPK

      SUBROUTINE ZMWARN

!  Called by one of the ZM routines to print a warning message if any error condition arises in
!  that routine.

      USE ModLib_FMVALS
      IMPLICIT NONE

      CHARACTER(9) :: NAME

      INTEGER :: NCS

      IF (KFLAG >= 0 .OR. NCALL /= 1 .OR. KWARN <= 0) RETURN
      NCS = NCALL
      NAME = NAMEST(NCALL)
      WRITE (KW,"(/' Error of type KFLAG =',I3,"   //  &
                "' in FM package in routine ',A/)"     &
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
      ELSE IF (KFLAG == -8 .AND. NAME == 'ZMOUT') THEN
          WRITE (KW,                                                          &
                 "(' The result array is not big enough to hold the',"    //  &
                 "' output character string'/' in the current format.'/"  //  &
                 "' The result ''***...***'' has been returned.'/)"           &
                )
      ELSE IF (KFLAG == -8 .AND. NAME == 'ZMREAD') THEN
          WRITE (KW,                                                        &
                 "(' The CMBUFF array is not big enough to hold the',"  //  &
                 "' input character string'/"                           //  &
                 "' UNKNOWN has been returned.'/)"                          &
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
      ENDIF

      NCALL = NCS
      IF (KWARN >= 2) THEN
          STOP
      ENDIF
      RETURN
      END SUBROUTINE ZMWARN

      SUBROUTINE ZMWRIT(KWRITE,MA)

!  Write MA on unit KWRITE under the current format.  Multi-line numbers will have '&' as the last
!  nonblank character on all but the last line of the real part and the imaginary part.
!  These numbers can then be read easily using ZMREAD.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: KWRITE
      TYPE(MULTI) :: MA(2)

      INTEGER :: J,K,KSAVE,L,LAST,LAST1,LAST2,LB,ND,NEXP
      INTENT (IN) :: MA,KWRITE

      NCALL = NCALL + 1
      NAMEST(NCALL) = 'ZMWRIT'
      KSAVE = KFLAG
      ND = INT(REAL(NDIG)*LOG10(REAL(MBASE))) + 1
      IF (ND < 2) ND = 2
      NEXP = INT(2.0*LOG10(REAL(MXBASE))) + 16
      LB = 2*MAX(JFORM2+NEXP,ND+NEXP) + 3
      IF (LB > LMBUFZ) THEN
          IF (LMBUFZ > 0) DEALLOCATE(CMBUFZ)
          ALLOCATE(CMBUFZ(LB),STAT=J)
          IF (J /= 0) THEN
              CALL FMDEFINE_ERROR
          ENDIF
          LMBUFZ = LB
      ENDIF
      CALL ZMOUT(MA,CMBUFZ,LB,LAST1,LAST2)
      KFLAG = KSAVE
      LAST = LAST2 + 1
      DO J = 1, LAST2
         IF (CMBUFZ(LAST-J) /= ' ' .OR. J == LAST2) THEN
             L = LAST - J
             IF (MOD(L,73) /= 0) THEN
                 WRITE (KWRITE,"(4X,73A1,' &')") (CMBUFZ(K),K=1,L)
             ELSE
                 WRITE (KWRITE,"(4X,73A1,' &')") (CMBUFZ(K),K=1,L-73)
                 WRITE (KWRITE,"(4X,73A1)") (CMBUFZ(K),K=L-72,L)
             ENDIF
             NCALL = NCALL - 1
             RETURN
         ENDIF
      ENDDO
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE ZMWRIT

      SUBROUTINE ZMZ2M(ZVAL,MA)

!  MA = ZVAL

!  ZVAL is complex and is converted to ZM form.

      USE ModLib_FMVALS
      IMPLICIT NONE

      COMPLEX :: ZVAL
      TYPE(MULTI) :: MA(2)
      INTENT (IN) :: ZVAL
      INTENT (INOUT) :: MA

      NCALL = NCALL + 1
      NAMEST(NCALL) = 'ZMZ2M'
      IF (NTRACE /= 0) CALL ZMNTRZ(2,ZVAL,1)

      CALL FMSP2M(REAL(ZVAL),MA(1))
      CALL FMSP2M(AIMAG(ZVAL),MA(2))

      IF (NTRACE /= 0) CALL ZMNTR(1,MA,MA,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE ZMZ2M


!  Here are the routines which work with packed ZM numbers.
!  All names are the same as unpacked versions with 'ZM' replaced by 'ZP'.

!  This packed format is not available when using the FM, IM, or ZM derived types.

      SUBROUTINE ZPABS(MA,MBFM)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MBFM
      INTENT (IN) :: MA
      INTENT (INOUT) :: MBFM
      CALL ZMUNPK(MA,MPX)
      CALL ZMABS(MPX,MPA)
      CALL FMPACK(MPA,MBFM)
      RETURN
      END SUBROUTINE ZPABS

      SUBROUTINE ZPACOS(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMACOS(MPX,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPACOS

      SUBROUTINE ZPACOSH(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMACOSH(MPX,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPACOSH

      SUBROUTINE ZPADD(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2),MC(2)
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL ZMUNPK(MA,MPX)
      CALL ZMUNPK(MB,MPY)
      CALL ZMADD(MPX,MPY,MPZ)
      CALL ZMPACK(MPZ,MC)
      RETURN
      END SUBROUTINE ZPADD

      SUBROUTINE ZPADDI(MA,INTEG)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2)
      INTEGER :: INTEG
      INTENT (IN) :: INTEG
      INTENT (INOUT) :: MA
      CALL ZMUNPK(MA,MPX)
      CALL ZMADDI(MPX,INTEG)
      CALL ZMPACK(MPX,MA)
      RETURN
      END SUBROUTINE ZPADDI

      SUBROUTINE ZPARG(MA,MBFM)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MBFM
      INTENT (IN) :: MA
      INTENT (INOUT) :: MBFM
      CALL ZMUNPK(MA,MPX)
      CALL ZMARG(MPX,MPA)
      CALL FMPACK(MPA,MBFM)
      RETURN
      END SUBROUTINE ZPARG

      SUBROUTINE ZPASIN(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMASIN(MPX,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPASIN

      SUBROUTINE ZPASINH(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMASINH(MPX,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPASINH

      SUBROUTINE ZPATAN(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMATAN(MPX,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPATAN

      SUBROUTINE ZPATANH(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMATANH(MPX,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPATANH

      SUBROUTINE ZPCHSH(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2),MC(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB,MC
      CALL ZMUNPK(MA,MPX)
      CALL ZMCHSH(MPX,MPY,MPZ)
      CALL ZMPACK(MPY,MB)
      CALL ZMPACK(MPZ,MC)
      RETURN
      END SUBROUTINE ZPCHSH

      SUBROUTINE ZPCMPX(MAFM,MBFM,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MAFM,MBFM,MC(2)
      INTENT (IN) :: MAFM,MBFM
      INTENT (INOUT) :: MC
      CALL FMUNPK(MAFM,MPA)
      CALL FMUNPK(MBFM,MPB)
      CALL ZMCMPX(MPA,MPB,MPX)
      CALL ZMPACK(MPX,MC)
      RETURN
      END SUBROUTINE ZPCMPX

      SUBROUTINE ZPCONJ(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMCONJ(MPX,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPCONJ

      SUBROUTINE ZPCOS(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMCOS(MPX,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPCOS

      SUBROUTINE ZPCOSH(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMCOSH(MPX,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPCOSH

      SUBROUTINE ZPCSSN(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2),MC(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB,MC
      CALL ZMUNPK(MA,MPX)
      CALL ZMCSSN(MPX,MPY,MPZ)
      CALL ZMPACK(MPY,MB)
      CALL ZMPACK(MPZ,MC)
      RETURN
      END SUBROUTINE ZPCSSN

      SUBROUTINE ZPDIV(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2),MC(2)
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL ZMUNPK(MA,MPX)
      CALL ZMUNPK(MB,MPY)
      CALL ZMDIV(MPX,MPY,MPZ)
      CALL ZMPACK(MPZ,MC)
      RETURN
      END SUBROUTINE ZPDIV

      SUBROUTINE ZPDIVI(MA,INTEG,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTEGER :: INTEG
      INTENT (IN) :: MA,INTEG
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMDIVI(MPX,INTEG,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPDIVI

      SUBROUTINE ZPEQ(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FPEQ(MA(1),MB(1))
      CALL FPEQ(MA(2),MB(2))
      RETURN
      END SUBROUTINE ZPEQ

      SUBROUTINE ZPEQU(MA,MB,NDA,NDB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTEGER :: NDA,NDB
      INTENT (IN) :: MA,NDA,NDB
      INTENT (INOUT) :: MB
      CALL FPEQU(MA(1),MB(1),NDA,NDB)
      CALL FPEQU(MA(2),MB(2),NDA,NDB)
      RETURN
      END SUBROUTINE ZPEQU

      SUBROUTINE ZPEQU_R1(MA,NDA,NDB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2)
      INTEGER :: NDA,NDB
      INTENT (IN) :: NDA,NDB
      INTENT (INOUT) :: MA
      CALL FPEQU_R1(MA(1),NDA,NDB)
      CALL FPEQU_R1(MA(2),NDA,NDB)
      RETURN
      END SUBROUTINE ZPEQU_R1

      SUBROUTINE ZPEXP(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMEXP(MPX,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPEXP

      SUBROUTINE ZPFORM(FORM1,FORM2,MA,STRING)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2)
      CHARACTER(*) :: FORM1,FORM2,STRING
      INTENT (IN) :: MA,FORM1,FORM2
      INTENT (INOUT) :: STRING
      CALL ZMUNPK(MA,MPX)
      CALL ZMFORM(FORM1,FORM2,MPX,STRING)
      RETURN
      END SUBROUTINE ZPFORM

      SUBROUTINE ZPFPRT(FORM1,FORM2,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2)
      CHARACTER(*) :: FORM1,FORM2
      INTENT (IN) :: MA,FORM1,FORM2
      CALL ZMUNPK(MA,MPX)
      CALL ZMFPRT(FORM1,FORM2,MPX)
      RETURN
      END SUBROUTINE ZPFPRT

      SUBROUTINE ZP2I2M(INTEG1,INTEG2,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER :: INTEG1,INTEG2
      TYPE(MULTI) :: MA(2)
      INTENT (IN) :: INTEG1,INTEG2
      INTENT (INOUT) :: MA
      CALL ZM2I2M(INTEG1,INTEG2,MPX)
      CALL ZMPACK(MPX,MA)
      RETURN
      END SUBROUTINE ZP2I2M

      SUBROUTINE ZPI2M(INTEG,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER :: INTEG
      TYPE(MULTI) :: MA(2)
      INTENT (IN) :: INTEG
      INTENT (INOUT) :: MA
      CALL ZMI2M(INTEG,MPX)
      CALL ZMPACK(MPX,MA)
      RETURN
      END SUBROUTINE ZPI2M

      SUBROUTINE ZPIMAG(MA,MBFM)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MBFM
      INTENT (IN) :: MA
      INTENT (INOUT) :: MBFM
      CALL ZMUNPK(MA,MPX)
      CALL ZMIMAG(MPX,MPA)
      CALL FMPACK(MPA,MBFM)
      RETURN
      END SUBROUTINE ZPIMAG

      SUBROUTINE ZPINT(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMINT(MPX,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPINT

      SUBROUTINE ZPINP(LINE,MA,LA,LB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER :: LA,LB
      CHARACTER :: LINE(LB)
      TYPE(MULTI) :: MA(2)
      INTENT (IN) :: LINE,LA,LB
      INTENT (INOUT) :: MA
      CALL ZMINP(LINE,MPX,LA,LB)
      CALL ZMPACK(MPX,MA)
      RETURN
      END SUBROUTINE ZPINP

      SUBROUTINE ZPIPWR(MA,INTEG,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTEGER :: INTEG
      INTENT (IN) :: MA,INTEG
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMIPWR(MPX,INTEG,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPIPWR

      SUBROUTINE ZPLG10(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMLG10(MPX,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPLG10

      SUBROUTINE ZPLN(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMLN(MPX,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPLN

      SUBROUTINE ZPM2I(MA,INTEG)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2)
      INTEGER :: INTEG
      INTENT (IN) :: MA
      INTENT (INOUT) :: INTEG
      CALL ZMUNPK(MA,MPX)
      CALL ZMM2I(MPX,INTEG)
      RETURN
      END SUBROUTINE ZPM2I

      SUBROUTINE ZPM2Z(MA,ZVAL)
      USE ModLib_FMVALS
      IMPLICIT NONE
      COMPLEX :: ZVAL
      TYPE(MULTI) :: MA(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: ZVAL
      CALL ZMUNPK(MA,MPX)
      CALL ZMM2Z(MPX,ZVAL)
      RETURN
      END SUBROUTINE ZPM2Z

      SUBROUTINE ZPMPY(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2),MC(2)
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL ZMUNPK(MA,MPX)
      CALL ZMUNPK(MB,MPY)
      CALL ZMMPY(MPX,MPY,MPZ)
      CALL ZMPACK(MPZ,MC)
      RETURN
      END SUBROUTINE ZPMPY

      SUBROUTINE ZPMPYI(MA,INTEG,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTEGER :: INTEG
      INTENT (IN) :: MA,INTEG
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMMPYI(MPX,INTEG,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPMPYI

      SUBROUTINE ZPNINT(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMNINT(MPX,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPNINT

      SUBROUTINE ZPOUT(MA,LINE,LB,LAST1,LAST2)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2)
      INTEGER :: LB,LAST1,LAST2
      CHARACTER :: LINE(LB)
      INTENT (IN) :: MA,LB
      INTENT (INOUT) :: LINE,LAST1,LAST2
      CALL ZMUNPK(MA,MPX)
      CALL ZMOUT(MPX,LINE,LB,LAST1,LAST2)
      RETURN
      END SUBROUTINE ZPOUT

      SUBROUTINE ZPPRNT(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2)
      INTENT (IN) :: MA
      CALL ZMUNPK(MA,MPX)
      CALL ZMPRNT(MPX)
      RETURN
      END SUBROUTINE ZPPRNT

      SUBROUTINE ZPPWR(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2),MC(2)
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL ZMUNPK(MA,MPX)
      CALL ZMUNPK(MB,MPY)
      CALL ZMPWR(MPX,MPY,MPZ)
      CALL ZMPACK(MPZ,MC)
      RETURN
      END SUBROUTINE ZPPWR

      SUBROUTINE ZPREAD(KREAD,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER :: KREAD
      TYPE(MULTI) :: MA(2)
      INTENT (IN) :: KREAD
      INTENT (INOUT) :: MA
      CALL ZMREAD(KREAD,MPX)
      CALL ZMPACK(MPX,MA)
      RETURN
      END SUBROUTINE ZPREAD

      SUBROUTINE ZPREAL(MA,MBFM)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MBFM
      INTENT (IN) :: MA
      INTENT (INOUT) :: MBFM
      CALL ZMUNPK(MA,MPX)
      CALL ZMREAL(MPX,MPA)
      CALL FMPACK(MPA,MBFM)
      RETURN
      END SUBROUTINE ZPREAL

      SUBROUTINE ZPRPWR(MA,IVAL,JVAL,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTEGER :: IVAL,JVAL
      INTENT (IN) :: MA,IVAL,JVAL
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMRPWR(MPX,IVAL,JVAL,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPRPWR

      SUBROUTINE ZPSET(NPREC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER :: NPREC
      INTENT (IN) :: NPREC
      CALL ZMSET(NPREC)
      RETURN
      END SUBROUTINE ZPSET

      SUBROUTINE ZPSIN(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMSIN(MPX,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPSIN

      SUBROUTINE ZPSINH(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMSINH(MPX,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPSINH

      SUBROUTINE ZPSQR(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMSQR(MPX,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPSQR

      SUBROUTINE ZPSQRT(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMSQRT(MPX,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPSQRT

      SUBROUTINE ZPST2M(STRING,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      CHARACTER(*) :: STRING
      TYPE(MULTI) :: MA(2)
      INTENT (IN) :: STRING
      INTENT (INOUT) :: MA
      CALL ZMST2M(STRING,MPX)
      CALL ZMPACK(MPX,MA)
      RETURN
      END SUBROUTINE ZPST2M

      SUBROUTINE ZPSUB(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2),MC(2)
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL ZMUNPK(MA,MPX)
      CALL ZMUNPK(MB,MPY)
      CALL ZMSUB(MPX,MPY,MPZ)
      CALL ZMPACK(MPZ,MC)
      RETURN
      END SUBROUTINE ZPSUB

      SUBROUTINE ZPTAN(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMTAN(MPX,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPTAN

      SUBROUTINE ZPTANH(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMTANH(MPX,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPTANH

      SUBROUTINE ZPWRIT(KWRITE,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER :: KWRITE
      TYPE(MULTI) :: MA(2)
      INTENT (IN) :: MA,KWRITE
      CALL ZMUNPK(MA,MPX)
      CALL ZMWRIT(KWRITE,MPX)
      RETURN
      END SUBROUTINE ZPWRIT

      SUBROUTINE ZPZ2M(ZVAL,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      COMPLEX :: ZVAL
      TYPE(MULTI) :: MA(2)
      INTENT (IN) :: ZVAL
      INTENT (INOUT) :: MA
      CALL ZMZ2M(ZVAL,MPX)
      CALL ZMPACK(MPX,MA)
      RETURN
      END SUBROUTINE ZPZ2M


!  These are the longer and more readable routine names, equivalent to the older names.

      SUBROUTINE ZMCOSH_SINH(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2),MC(2)
      CALL ZMCHSH(MA,MB,MC)
      RETURN
      END SUBROUTINE ZMCOSH_SINH

      SUBROUTINE ZMCOMPLEX(MAFM,MBFM,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MAFM,MBFM,MC(2)
      CALL ZMCMPX(MAFM,MBFM,MC)
      RETURN
      END SUBROUTINE ZMCOMPLEX

      SUBROUTINE ZMCONJUGATE(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      CALL ZMCONJ(MA,MB)
      RETURN
      END SUBROUTINE ZMCONJUGATE

      SUBROUTINE ZMCOS_SIN(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2),MC(2)
      CALL ZMCSSN(MA,MB,MC)
      RETURN
      END SUBROUTINE ZMCOS_SIN

      SUBROUTINE ZMFPRINT(FORM1,FORM2,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      CHARACTER(*) :: FORM1,FORM2
      TYPE(MULTI) :: MA(2)
      CALL ZMFPRT(FORM1,FORM2,MA)
      RETURN
      END SUBROUTINE ZMFPRINT

      SUBROUTINE ZMIPOWER(MA,IVAL,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTEGER :: IVAL
      CALL ZMIPWR(MA,IVAL,MB)
      RETURN
      END SUBROUTINE ZMIPOWER

      SUBROUTINE ZMLOG10(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      CALL ZMLG10(MA,MB)
      RETURN
      END SUBROUTINE ZMLOG10

      SUBROUTINE ZMPRINT(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2)
      CALL ZMPRNT(MA)
      RETURN
      END SUBROUTINE ZMPRINT

      SUBROUTINE ZMPOWER(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2),MC(2)
      CALL ZMPWR(MA,MB,MC)
      RETURN
      END SUBROUTINE ZMPOWER

      SUBROUTINE ZMRATIONAL_POWER(MA,IVAL,JVAL,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTEGER :: IVAL,JVAL
      CALL ZMRPWR(MA,IVAL,JVAL,MB)
      RETURN
      END SUBROUTINE ZMRATIONAL_POWER

      SUBROUTINE ZMWRITE(KWRITE,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER :: KWRITE
      TYPE(MULTI) :: MA(2)
      CALL ZMWRIT(KWRITE,MA)
      RETURN
      END SUBROUTINE ZMWRITE

      SUBROUTINE ZPCOSH_SINH(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2),MC(2)
      CALL ZPCHSH(MA,MB,MC)
      RETURN
      END SUBROUTINE ZPCOSH_SINH

      SUBROUTINE ZPCOMPLEX(MAFM,MBFM,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MAFM,MBFM,MC(2)
      CALL ZPCMPX(MAFM,MBFM,MC)
      RETURN
      END SUBROUTINE ZPCOMPLEX

      SUBROUTINE ZPCONJUGATE(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      CALL ZPCONJ(MA,MB)
      RETURN
      END SUBROUTINE ZPCONJUGATE

      SUBROUTINE ZPCOS_SIN(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2),MC(2)
      CALL ZPCSSN(MA,MB,MC)
      RETURN
      END SUBROUTINE ZPCOS_SIN

      SUBROUTINE ZPFPRINT(FORM1,FORM2,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      CHARACTER(*) :: FORM1,FORM2
      TYPE(MULTI) :: MA(2)
      CALL ZPFPRT(FORM1,FORM2,MA)
      RETURN
      END SUBROUTINE ZPFPRINT

      SUBROUTINE ZPIPOWER(MA,IVAL,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTEGER :: IVAL
      CALL ZPIPWR(MA,IVAL,MB)
      RETURN
      END SUBROUTINE ZPIPOWER

      SUBROUTINE ZPLOG10(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      CALL ZPLG10(MA,MB)
      RETURN
      END SUBROUTINE ZPLOG10

      SUBROUTINE ZPPRINT(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2)
      CALL ZPPRNT(MA)
      RETURN
      END SUBROUTINE ZPPRINT

      SUBROUTINE ZPPOWER(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2),MC(2)
      CALL ZPPWR(MA,MB,MC)
      RETURN
      END SUBROUTINE ZPPOWER

      SUBROUTINE ZPRATIONAL_POWER(MA,IVAL,JVAL,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTEGER :: IVAL,JVAL
      CALL ZPRPWR(MA,IVAL,JVAL,MB)
      RETURN
      END SUBROUTINE ZPRATIONAL_POWER

      SUBROUTINE ZPWRITE(KWRITE,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER :: KWRITE
      TYPE(MULTI) :: MA(2)
      CALL ZPWRIT(KWRITE,MA)
      RETURN
      END SUBROUTINE ZPWRITE

!     ZM complex special functions

!  Here is a list of the routines that are designed to be called by the user.
!  All arguments are zm complex numbers.


!  ZMERF(MA,MB)         MB = Erf(MA)          Error function

!  ZMERFC(MA,MB)        MB = Erfc(MA)         Complimentary Error function

!  ZMERFC_SC(MA,MB)     MB = Erfc_scaled(MA)  Scaled complimentary Error function

!  ZMLNGM(MA,MB)        MB = Log_Gamma(MA)    Log_Gamma function

!  ZMGAM(MA,MB)         MB = Gamma(MA)        Gamma function

!  ZMFACT(MA,MB)        MB = MA!              Factorial function


      SUBROUTINE ZMERF(MA,MB)

!  MB = Erf(MA)    Error function.

!  2/Sqrt(pi) * Integral from 0 to MA of e**(-t**2) dt.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: C1,C2,C4,ERR,ERR2,X,XK
      DOUBLE PRECISION, EXTERNAL :: FMNTERMS
      INTEGER :: IEXTRA,J,J2,K,KFLAGX,KL,KOVUN,KR_RETRY,KRESLT,  &
                 NDSAVE,NDSAV1,NDSAV2,NMETHD,NTERM,NTERMS
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(2,15), MJSUMS(2,LJSUMS)
      LOGICAL, EXTERNAL :: FMCOMP

!             If MA is real, use FMERF.

      IF (MA(2)%MP(3) == 0) THEN
          CALL FMERF(MA(1),MB(1))
          CALL FMI2M(0,MB(2))
          RETURN
      ENDIF

      IF (.NOT. ALLOCATED(MB(1)%MP)) THEN
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(1)%MP)
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MB(2)%MP)) THEN
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(2)%MP)
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL ZMENTR('ZMERF    ',MA,MA,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF

      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL ZMEQU(MA,MXY(1:2,1),NDSAVE,NDIG)

!             Check for special cases.

      IF (MA(1)%MP(3) == 0 .AND. MA(2)%MP(3) == 0) THEN
          CALL ZMEQ(MXY(1:2,1),MXY(1:2,3))
          GO TO 210
      ENDIF
      IF (MXY(1,1)%MP(2) == MEXPUN .OR. MXY(2,1)%MP(2) == MEXPUN) THEN
          CALL ZMST2M('UNKNOWN+UNKNOWN*i',MXY(1:2,3))
          KFLAG = -4
          GO TO 210
      ENDIF

!             X is a double precision approximation to the size of input argument to this function.

      CALL ZMABS(MXY(1:2,1),MXY(1,4))
      CALL FMM2DP(MXY(1,4),X)
      KFLAGX = KFLAG
      IF (KFLAG /= 0 .AND. MXY(1,4)%MP(2) < 0) THEN
          X = 1.0D0/DPMAX**0.33D0
          KFLAGX = 0
      ELSE IF (KFLAG == 0 .AND. X < 1.0D0/DPMAX**0.33D0) THEN
          X = 1.0D0/DPMAX**0.33D0
      ENDIF
      IF (KFLAG /= 0 .AND. MXY(1,4)%MP(2) > 0) THEN
          X = DPMAX**0.33D0
          KFLAGX = 0
      ELSE IF (KFLAG == 0 .AND. X > DPMAX**0.33D0) THEN
          X = DPMAX**0.33D0
      ENDIF

!             Determine which method to use.

!             NMETHD = 1 use the convergent series,
!                    = 2 use a 2nd convergent series,
!                    = 3 use a continued fraction expansion,
!                    = 4 use an asymptotic series.

      XK = MAX(3.0D0,(2*X**2 + 1)/2)
      XK = -XK + LOG(2*XK-1)*(2*XK-0.5D0) + LOG(XK-1)*(-XK+0.5D0) - LOG(2.0D0)*(2*XK-1) -  &
           LOG(X)*(2*XK+1)
      IF (-XK/DLOGMB > NDIG+(3+NDIG/20)*NGRD52) THEN
          NMETHD = 4
      ELSE
          C4 = 5.0
          XK = 0.0
          DO WHILE (-XK/DLOGMB <= NDIG+(3+NDIG/20)*NGRD52)
             C4 = 2 * C4
             XK = MAX(3.0D0,(2*C4**2 + 1)/2)
             XK = -XK + LOG(2*XK-1)*(2*XK-0.5D0) + LOG(XK-1)*(-XK+0.5D0) - LOG(2.0D0)*(2*XK-1) -  &
                  LOG(C4)*(2*XK+1)
          ENDDO
          C1 = C4/2
          DO WHILE (C4-C1 > 0.1)
             C2 = (C1 + C4) / 2
             XK = MAX(3.0D0,(2*C2**2 + 1)/2)
             XK = -XK + LOG(2*XK-1)*(2*XK-0.5D0) + LOG(XK-1)*(-XK+0.5D0) - LOG(2.0D0)*(2*XK-1) -  &
                  LOG(C2)*(2*XK+1)
             IF (-XK/DLOGMB > NDIG+(3+NDIG/20)*NGRD52) THEN
                 C4 = C2
             ELSE
                 C1 = C2
             ENDIF
          ENDDO
          IF (X > 0.75*C4 .AND. X > C4 - 5) THEN
              NMETHD = 3
          ELSE
              CALL FMDIV(MXY(2,1),MXY(1,1),MXY(1,15))
              CALL FMABS(MXY(1,15),MXY(2,15))
              CALL FMDP2M(1.5D0,MXY(1,15))
              IF (FMCOMP(MXY(2,15),'>',MXY(1,15))) THEN
                  IF (X > 0.70*C4) THEN
                      NMETHD = 2
                  ELSE
                      NMETHD = 1
                  ENDIF
              ELSE
                  IF (X > 6.0) THEN
                      NMETHD = 2
                  ELSE
                      NMETHD = 1
                  ENDIF
              ENDIF
          ENDIF
      ENDIF
      IF (NMETHD /= 1) GO TO 140


!             Method 1.  Use the (-1)**n x**(2n+1) / n! / (2n+1) series.

!                        = ( 2 / sqrt(pi) ) * ( x - x^3/1! + x^5/2! - x^7/3! + ... )

      IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG+NGRD52)
      ENDIF
      CALL ZMEQU(MA,MXY(1:2,1),NDSAVE,NDIG)

!             If MA is large in magnitude or close to the real line, use more guard digits.

      CALL FMSQR(MXY(1,1),MXY(1,15))
      CALL FMSQR(MXY(2,1),MXY(2,15))
      CALL FMMPY(MXY(1,1),MXY(2,1),MXY(1,14))
      CALL FMST2M('-4.409869',MXY(1,13))

      CALL FMST2M(' 0.000918',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(1,1))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMST2M(' 0.002513',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(2,1))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMST2M(' 0.124040',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(1,15))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMST2M('-0.000288',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(1,14))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMST2M(' 0.001638',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(2,15))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMM2I(MXY(1,13),IEXTRA)
      IEXTRA = MAX(0,NINT((IEXTRA+3) * 16.11809565D0 / DLOGMB)+1)
      IF (IEXTRA > 0) THEN
          CALL ZMEQU_R1(MXY(1:2,1),NDIG,NDIG+IEXTRA)
      ENDIF
      NDIG = NDIG + IEXTRA
      IF (KFLAGX == 0) THEN
          J2 = INT(0.66*SQRT(FMNTERMS(X**2,1,0,0,1)) - 0.8)
          J2 = MAX(2,MIN(J2+MOD(J2,2),LJSUMS))
      ELSE
          J2 = 2
      ENDIF
      NDSAV1 = NDIG

!             Split into J2 concurrent sums.

      NTERM = 1
      CALL ZMI2M(1,MXY(1:2,6))
      CALL ZMI2M(1,MJSUMS(1:2,1))

      DO J = 2, J2
         IF (NTERM > 1) THEN
             CALL ZMDIVI_R1(MXY(1:2,6),NTERM)
         ENDIF
         CALL ZMDIVI(MXY(1:2,6),2*NTERM+1,MJSUMS(1:2,J))
         NTERM = NTERM + 1
      ENDDO
      CALL ZMABS(MXY(1:2,1),MXY(1,15))
      IF (MXY(1,15)%MP(2) < -NDIG) GO TO 130
      CALL ZMIPWR(MXY(1:2,1),2*J2,MXY(1:2,5))

  120 CALL ZMMPY_R1(MXY(1:2,6),MXY(1:2,5))
      DO J = 1, J2
         CALL ZMDIVI_R1(MXY(1:2,6),NTERM)
         CALL ZMDIVI(MXY(1:2,6),2*NTERM+1,MXY(1:2,4))
         NDIG = NDSAV1
         CALL ZMADD_R1(MJSUMS(1:2,J),MXY(1:2,4))
         IF (KFLAG /= 0) GO TO 130
         NDIG = NDSAV1 - INT(MIN(MJSUMS(1,J)%MP(2)-MXY(1,4)%MP(2),  &
                                 MJSUMS(2,J)%MP(2)-MXY(2,4)%MP(2)))
         NDIG = MIN(NDSAV1,NDIG)
         IF (NDIG < NGRD22) NDIG = NGRD22
         NTERM = NTERM + 1
      ENDDO
      GO TO 120

!             Put the J2 separate sums back together.

  130 KFLAG = 0
      CALL ZMSQR(MXY(1:2,1),MXY(1:2,4))
      CALL ZMMPYI_R1(MXY(1:2,4),-1)
      CALL ZMEQ(MJSUMS(1:2,J2),MXY(1:2,5))

      DO J = 2, J2
         CALL ZMMPY(MXY(1:2,5),MXY(1:2,4),MXY(1:2,15))
         CALL ZMADD(MXY(1:2,15),MJSUMS(1:2,J2-J+1),MXY(1:2,5))
      ENDDO

      CALL ZMMPY_R1(MXY(1:2,5),MXY(1:2,1))

      CALL FMPI(MXY(1,4))
      CALL FMI2M(0,MXY(2,4))
      CALL ZMSQRT(MXY(1:2,4),MXY(1:2,3))
      CALL ZMMPYI(MXY(1:2,5),2,MXY(1:2,4))
      CALL ZMDIV_R2(MXY(1:2,4),MXY(1:2,3))

      GO TO 210

  140 IF (NMETHD /= 2) GO TO 170


!             Method 2.  Use the x**(2n+1) * 2**(n+1) / (1*3*5*...*(2n+1)) series.

      IEXTRA = 0
      CALL FMSQR(MXY(1,1),MXY(1,15))
      CALL FMSQR(MXY(2,1),MXY(2,15))
      CALL FMMPY(MXY(1,1),MXY(2,1),MXY(1,14))
      CALL FMST2M('-2.156685',MXY(1,13))

      CALL FMST2M('-0.000490',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(1,1))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMST2M('-0.000013',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(2,1))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMST2M(' 0.062094',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(1,15))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMST2M(' 0.000027',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(1,14))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMST2M(' 0.062030',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(2,15))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMM2I(MXY(1,13),IEXTRA)
      IEXTRA = MAX(0,NINT((IEXTRA+3) * 16.11809565D0 / DLOGMB)+1)
      NDIG = NDIG + IEXTRA
      IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG) + 1
      ENDIF
      CALL ZMEQU(MA,MXY(1:2,1),NDSAVE,NDIG)
      IF (KFLAGX == 0) THEN
          J2 = INT(0.51*SQRT(FMNTERMS(2*X,2,-1,0,1)))
      ELSE
          J2 = 1
      ENDIF
      J2 = MAX(1,MIN(LJSUMS,J2))
      NDSAV1 = NDIG

!             Split into J2 concurrent sums.

      NTERM = 1
      CALL ZMMPYI(MXY(1:2,1),2,MXY(1:2,6))
      CALL ZMEQ(MXY(1:2,6),MJSUMS(1:2,1))
      DO J = 2, J2
         CALL ZMMPYI_R1(MXY(1:2,6),2)
         CALL ZMDIVI_R1(MXY(1:2,6),2*NTERM+1)
         CALL ZMEQ(MXY(1:2,6),MJSUMS(1:2,J))
         NTERM = NTERM + 1
      ENDDO
      CALL ZMABS(MXY(1:2,1),MXY(1,15))
      IF (MXY(1,15)%MP(2) < -NDIG) GO TO 160
      CALL ZMIPWR(MXY(1:2,1),2*J2,MXY(1:2,5))

  150 CALL ZMMPY_R1(MXY(1:2,6),MXY(1:2,5))
      DO J = 1, J2
         CALL ZMMPYI(MXY(1:2,6),2,MXY(1:2,15))
         CALL ZMDIVI(MXY(1:2,15),2*NTERM+1,MXY(1:2,6))
         NDIG = NDSAV1
         CALL ZMADD_R1(MJSUMS(1:2,J),MXY(1:2,6))
         IF (KFLAG /= 0) GO TO 160
         NDIG = NDSAV1 - INT(MIN(MJSUMS(1,J)%MP(2)-MXY(1,6)%MP(2),  &
                                 MJSUMS(2,J)%MP(2)-MXY(2,6)%MP(2)))
         NDIG = MIN(NDSAV1,NDIG)
         IF (NDIG < NGRD22) NDIG = NGRD22
         NTERM = NTERM + 1
      ENDDO
      GO TO 150

!             Put the J2 separate sums back together.

  160 KFLAG = 0
      CALL ZMSQR(MXY(1:2,1),MXY(1:2,6))
      CALL ZMEQ(MJSUMS(1:2,J2),MXY(1:2,5))
      DO J = 2, J2
         CALL ZMMPY(MXY(1:2,5),MXY(1:2,6),MXY(1:2,15))
         CALL ZMADD(MXY(1:2,15),MJSUMS(1:2,J2-J+1),MXY(1:2,5))
      ENDDO

      CALL FMPI(MXY(1,4))
      CALL FMI2M(0,MXY(2,4))
      CALL ZMSQRT(MXY(1:2,4),MXY(1:2,3))
      CALL ZMDIV_R2(MXY(1:2,5),MXY(1:2,3))
      CALL ZMEXP(MXY(1:2,6),MXY(1:2,4))
      CALL ZMDIV_R1(MXY(1:2,3),MXY(1:2,4))

      GO TO 210


  170 IF (NMETHD /= 3) GO TO 180


!             Method 3.  Continued fraction expansion (asymptotic).
!                        Used for Re(x) > 0, erf(x) = -erf(-x) otherwise.

!                                  erf(x) = 1 + (-exp(-x^2)/sqrt(pi)) /
!                                                                   x + 1 /
!                                                                     2*x + 2 /
!                                                                           x + 3 /
!                                                                             2*x + 4 /
!                                                                                   x + 5 ...

      IEXTRA = 0
      NDIG = NDIG + IEXTRA
      IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG) + 1
      ENDIF
      CALL ZMEQU(MA,MXY(1:2,1),NDSAVE,NDIG)
      NDSAV1 = NDIG
      IF (MA(1)%MP(1) < 0) THEN
          CALL ZMMPYI_R1(MXY(1:2,1),-1)
      ENDIF

      CALL ZMEQ(MXY(1:2,1),MXY(1:2,2))

      CALL ZMSQR(MXY(1:2,2),MXY(1:2,4))
      CALL ZMMPYI(MXY(1:2,4),-1,MXY(1:2,15))
      CALL ZMEXP(MXY(1:2,15),MXY(1:2,10))
      CALL ZMMPYI(MXY(1:2,10),-1,MXY(1:2,15))
      CALL FMPI(MXY(1,10))
      CALL FMSQRT_R1(MXY(1,10))
      CALL FMI2M(0,MXY(2,10))
      CALL ZMDIV_R2(MXY(1:2,15),MXY(1:2,10))
      CALL ZMDIV_R1(MXY(1:2,10),MXY(1:2,2))
      CALL ZMI2M(-31,MXY(1:2,13))

      CALL FMI2M(1,MXY(1,15))
      CALL FMULP(MXY(1,15),MXY(2,15))
      CALL FMDIV(MXY(1,1),MXY(2,1),MXY(1,15))
      CALL FMSQR_R1(MXY(1,15))
      IF (FMCOMP(MXY(1,15),'<',MXY(2,15))) THEN
          CALL ZMI2M(0,MXY(1:2,15))
      ELSE
          CALL ZMI2M(1,MXY(1:2,15))
      ENDIF
      CALL ZMADD(MXY(1:2,15),MXY(1:2,10),MXY(1:2,14))
      CALL ZMI2M(1,MXY(1:2,8))
      CALL ZMEQ(MXY(1:2,2),MXY(1:2,9))
      NTERMS = INT(INTMAX/10)

!             Continued fraction loop.

      NDSAV2 = NDIG
      DO K = 3, NTERMS
         CALL ZMMPYI(MXY(1:2,2),1+MOD(K,2),MXY(1:2,11))
         CALL ZMMPY_R1(MXY(1:2,11),MXY(1:2,9))
         CALL ZMMPYI(MXY(1:2,8),K-2,MXY(1:2,12))
         CALL ZMADD(MXY(1:2,11),MXY(1:2,12),MXY(1:2,7))
         CALL ZMMPY_R1(MXY(1:2,10),MXY(1:2,12))
         CALL ZMDIV_R1(MXY(1:2,10),MXY(1:2,7))
         CALL ZMMPYI_R1(MXY(1:2,10),-1)
         CALL ZMEQ(MXY(1:2,9),MXY(1:2,8))
         CALL ZMEQ(MXY(1:2,7),MXY(1:2,9))
         NDIG = NDSAV2
         CALL ZMADD_R1(MXY(1:2,14),MXY(1:2,10))

!             Check for convergence.

         IF (KFLAG == 1 .AND. K > 2) THEN
             EXIT
         ENDIF
         KL = MAX( 1000.0, 2 * ALOGMT * NDSAVE )
         IF (K > KL) THEN
             NMETHD = 2
             GO TO 140
         ENDIF
         CALL ZMEQ(MXY(1:2,10),MXY(1:2,13))
         NDIG = MIN(NDSAV2,MAX(NGRD22,NDSAV2-INT(                           &
                                      MIN(MXY(1,14)%MP(2)-MXY(1,10)%MP(2),  &
                                          MXY(2,14)%MP(2)-MXY(2,10)%MP(2)))))
      ENDDO

      CALL ZMEQ(MXY(1:2,14),MXY(1:2,3))
      IF (MA(1)%MP(1) < 0) THEN
          CALL ZMMPYI_R1(MXY(1:2,3),-1)
      ENDIF
      NDIG = NDSAV1
      GO TO 210


!             Method 4.  Asymptotic series
!                        1 - ( Exp(-x^2) / Sqrt(Pi) ) *
!                            Sum_{k=0}^Infinity (  (-1)^k * poch(1/2,k) / x^(2*k+1) )

!             The series is  1/x - (1/2) / x^3  +  ( 1*3 )/(2^2) / x^5 - ( 1*3*5 )/(2^3) / x^7
!                            + ...

  180 IEXTRA = 0
      NDIG = NDIG + IEXTRA
      IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG-1)
      ENDIF
      CALL ZMEQU(MA,MXY(1:2,1),NDSAVE,NDIG)
      IF (MA(1)%MP(1) < 0) THEN
          CALL ZMMPYI_R1(MXY(1:2,1),-1)
      ENDIF
      IF (KFLAGX == 0) THEN
          J2 = INT(0.66*SQRT(FMNTERMS(X**2,1,0,0,0)) - 0.8)
          J2 = MAX(2,MIN(J2+MOD(J2,2),LJSUMS))
      ELSE
          J2 = 2
      ENDIF
      NDSAV1 = NDIG

!             Split into J2 concurrent sums.

      NTERM = 1
      CALL ZMSQR(MXY(1:2,1),MXY(1:2,15))
      CALL ZMMPYI(MXY(1:2,15),2,MXY(1:2,7))
      CALL ZMI2M(1,MXY(1:2,8))
      CALL ZMDIV(MXY(1:2,8),MXY(1:2,7),MXY(1:2,6))
      CALL ZMI2M(1,MJSUMS(1:2,1))
      DO J = 2, J2
         NTERM = NTERM + 1
         CALL ZMMPYI_R1(MXY(1:2,8),2*NTERM-3)
         CALL ZMEQ(MXY(1:2,8),MJSUMS(1:2,J))
      ENDDO
      CALL ZMABS(MXY(1:2,1),MXY(1,15))
      IF (MXY(1,15)%MP(2) < -NDIG) GO TO 130
      CALL ZMIPWR(MXY(1:2,6),J2,MXY(1:2,7))
      CALL ZMI2M(-31,MXY(1:2,13))

  190 CALL ZMMPY_R2(MXY(1:2,7),MXY(1:2,8))
      DO J = 1, J2
         NTERM = NTERM + 1
         CALL ZMMPYI_R1(MXY(1:2,8),2*NTERM-3)
         NDIG = NDSAV1
         CALL ZMADD_R1(MJSUMS(1:2,J),MXY(1:2,8))
         IF (KFLAG /= 0) GO TO 200
         KL = MAX( 19000.0, 6 * ALOGMT * NDSAVE )
         IF (NTERM > KL) THEN
             CALL ZMABS(MXY(1:2,8),MXY(1,15))
             CALL ZMABS(MXY(1:2,13),MXY(2,15))
             IF (FMCOMP(MXY(1,15),'>',MXY(2,15))) THEN
                 NMETHD = 2
                 GO TO 140
             ENDIF
         ENDIF
         CALL ZMEQ(MXY(1:2,8),MXY(1:2,13))
         NDIG = NDSAV1 - INT(MIN(MJSUMS(1,J)%MP(2)-MXY(1,8)%MP(2),  &
                                 MJSUMS(2,J)%MP(2)-MXY(2,8)%MP(2)))
         NDIG = MIN(NDSAV1,NDIG)
         IF (NDIG < NGRD22) NDIG = NGRD22
      ENDDO
      GO TO 190

!             Put the J2 separate sums back together.

  200 KFLAG = 0
      CALL ZMMPYI(MXY(1:2,6),-1,MXY(1:2,4))
      CALL ZMI2M(1,MXY(1:2,5))
      DO J = 2, J2
         CALL ZMMPY_R1(MXY(1:2,5),MXY(1:2,4))
         CALL ZMMPY(MXY(1:2,5),MJSUMS(1:2,J),MXY(1:2,15))
         CALL ZMADD(MXY(1:2,15),MJSUMS(1:2,1),MXY(1:2,8))
         CALL ZMEQ(MXY(1:2,8),MJSUMS(1:2,1))
      ENDDO
      CALL FMMPY(MXY(1,1),MXY(2,1),MXY(2,15))
      CALL FMMPYI_R1(MXY(2,15),-2)
      CALL FMI2M(0,MXY(1,15))
      CALL ZMEXP(MXY(1:2,15),MXY(1:2,14))
      CALL ZMMPY(MJSUMS(1:2,1),MXY(1:2,14),MXY(1:2,15))
      CALL FMPI(MXY(1,4))
      CALL FMSQRT(MXY(1,4),MXY(1,8))
      CALL FMI2M(0,MXY(2,8))
      CALL ZMMPY(MXY(1:2,8),MXY(1:2,1),MXY(1:2,14))
      CALL ZMDIV(MXY(1:2,15),MXY(1:2,14),MXY(1:2,13))
      CALL FMADD(MXY(2,1),MXY(1,1),MXY(1,12))
      CALL FMSUB(MXY(2,1),MXY(1,1),MXY(1,11))
      CALL FMMPY(MXY(1,12),MXY(1,11),MXY(1,10))
      CALL FMEXP(MXY(1,10),MXY(1,12))
      CALL FMMPY(MXY(1,13),MXY(1,12),MXY(1,4))
      CALL FMMPY(MXY(2,13),MXY(1,12),MXY(2,4))

      IF (MXY(1,4)%MP(2) == MUNKNO) THEN
          CALL FMABS(MXY(1,13),MXY(1,14))
          CALL FMLN(MXY(1,14),MXY(1,15))
          CALL FMADD(MXY(1,10),MXY(1,15),MXY(1,14))
          CALL FMEXP(MXY(1,14),MXY(1,4))
          MXY(1,4)%MP(1) = MXY(1,13)%MP(1)
      ENDIF
      IF (MXY(2,4)%MP(2) == MUNKNO) THEN
          CALL FMABS(MXY(2,13),MXY(1,14))
          CALL FMLN(MXY(1,14),MXY(1,15))
          CALL FMADD(MXY(1,10),MXY(1,15),MXY(1,14))
          CALL FMEXP(MXY(1,14),MXY(2,4))
          MXY(2,4)%MP(1) = MXY(2,13)%MP(1)
      ENDIF
      CALL FMI2M(1,MXY(1,15))
      CALL FMULP(MXY(1,15),MXY(2,15))
      CALL FMDIV(MXY(1,1),MXY(2,1),MXY(1,15))
      CALL FMSQR_R1(MXY(1,15))
      IF (FMCOMP(MXY(1,15),'<',MXY(2,15))) THEN
          CALL ZMI2M(0,MXY(1:2,15))
          CALL ZMSUB(MXY(1:2,15),MXY(1:2,4),MXY(1:2,3))
      ELSE
          CALL ZMI2M(1,MXY(1:2,15))
          CALL ZMSUB(MXY(1:2,15),MXY(1:2,4),MXY(1:2,3))
      ENDIF

      IF (MA(1)%MP(1) < 0) THEN
          CALL ZMMPYI_R1(MXY(1:2,3),-1)
      ENDIF

  210 NAMEST(NCALL) = 'ZMERF'

      IF (MA(1)%MP(3) == 0) THEN
          IF (MXY(2,3)%MP(2) /= MUNKNO) THEN
              CALL FMI2M(0,MXY(1,3))
          ENDIF
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL == 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(1,3)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          ERR2 = 0
          DO J = KL, 1, -1
             ERR2 = (ERR2 + MXY(2,3)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR   > 0.498 .AND. ERR  < 0.502) .OR.  &
               (KROUND == 1 .AND. ERR2  > 0.498 .AND. ERR2 < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR  > 0.998 .OR. ERR  < 0.002)) .OR.  &
               (KROUND /= 1 .AND. (ERR2 > 0.998 .OR. ERR2 < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (MXY(1,3)%MP(2) == MUNKNO .OR. MXY(2,3)%MP(2) == MUNKNO) THEN
          CALL ZMST2M('UNKNOWN + UNKNOWN i',MXY(1:2,3))
      ENDIF

      CALL ZMEXIT(MXY(1:2,3),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE ZMERF

      SUBROUTINE ZMERFC(MA,MB)

!  MB = Erfc(MA)    Complimentary error function.

!  1 - 2/Sqrt(pi) * Integral from 0 to MA of e**(-t**2) dt.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: C1,C2,C4,ERR,ERR2,X,XK
      DOUBLE PRECISION, EXTERNAL :: FMNTERMS
      INTEGER :: IEXTRA,J,J2,K,KFLAGX,KL,KOVUN,KR_RETRY,KRESLT,  &
                 NDSAVE,NDSAV1,NDSAV2,NMETHD,NTERM,NTERMS
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(2,15), MJSUMS(2,LJSUMS)
      LOGICAL, EXTERNAL :: FMCOMP

!             If MA is real, use FMERFC.

      IF (MA(2)%MP(3) == 0) THEN
          CALL FMERFC(MA(1),MB(1))
          CALL FMI2M(0,MB(2))
          RETURN
      ENDIF

      IF (.NOT. ALLOCATED(MB(1)%MP)) THEN
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(1)%MP)
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MB(2)%MP)) THEN
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(2)%MP)
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL ZMENTR('ZMERFC   ',MA,MA,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF

      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL ZMEQU(MA,MXY(1:2,1),NDSAVE,NDIG)

!             Check for special cases.

      IF (MA(1)%MP(3) == 0 .AND. MA(2)%MP(3) == 0) THEN
          CALL ZMEQ(MXY(1:2,1),MXY(1:2,3))
          GO TO 210
      ENDIF
      IF (MXY(1,1)%MP(2) == MEXPUN .OR. MXY(2,1)%MP(2) == MEXPUN) THEN
          CALL ZMST2M('UNKNOWN+UNKNOWN*i',MXY(1:2,3))
          KFLAG = -4
          GO TO 210
      ENDIF

!             X is a double precision approximation to the size of input argument to this function.

      CALL ZMABS(MXY(1:2,1),MXY(1,4))
      CALL FMM2DP(MXY(1,4),X)
      KFLAGX = KFLAG
      IF (KFLAG /= 0 .AND. MXY(1,4)%MP(2) < 0) THEN
          X = 1.0D0/DPMAX**0.33D0
          KFLAGX = 0
      ELSE IF (KFLAG == 0 .AND. X < 1.0D0/DPMAX**0.33D0) THEN
          X = 1.0D0/DPMAX**0.33D0
      ENDIF
      IF (KFLAG /= 0 .AND. MXY(1,4)%MP(2) > 0) THEN
          X = DPMAX**0.33D0
          KFLAGX = 0
      ELSE IF (KFLAG == 0 .AND. X > DPMAX**0.33D0) THEN
          X = DPMAX**0.33D0
      ENDIF

!             Determine which method to use.

!             NMETHD = 1 use the convergent series,
!                    = 2 use a 2nd convergent series,
!                    = 3 use a continued fraction expansion,
!                    = 4 use an asymptotic series.

      XK = MAX(3.0D0,(2*X**2 + 1)/2)
      XK = -XK + LOG(2*XK-1)*(2*XK-0.5D0) + LOG(XK-1)*(-XK+0.5D0) - LOG(2.0D0)*(2*XK-1) -  &
           LOG(X)*(2*XK+1)
      IF (-XK/DLOGMB > NDIG+(3+NDIG/20)*NGRD52) THEN
          NMETHD = 4
      ELSE
          C4 = 5.0
          XK = 0.0
          DO WHILE (-XK/DLOGMB <= NDIG+(3+NDIG/20)*NGRD52)
             C4 = 2 * C4
             XK = MAX(3.0D0,(2*C4**2 + 1)/2)
             XK = -XK + LOG(2*XK-1)*(2*XK-0.5D0) + LOG(XK-1)*(-XK+0.5D0) - LOG(2.0D0)*(2*XK-1) -  &
                  LOG(C4)*(2*XK+1)
          ENDDO
          C1 = C4/2
          DO WHILE (C4-C1 > 0.1)
             C2 = (C1 + C4) / 2
             XK = MAX(3.0D0,(2*C2**2 + 1)/2)
             XK = -XK + LOG(2*XK-1)*(2*XK-0.5D0) + LOG(XK-1)*(-XK+0.5D0) - LOG(2.0D0)*(2*XK-1) -  &
                  LOG(C2)*(2*XK+1)
             IF (-XK/DLOGMB > NDIG+(3+NDIG/20)*NGRD52) THEN
                 C4 = C2
             ELSE
                 C1 = C2
             ENDIF
          ENDDO
          CALL FMDIV(MXY(2,1),MXY(1,1),MXY(1,15))
          CALL FMABS(MXY(1,15),MXY(2,15))
          CALL FMDP2M(1.5D0,MXY(1,15))
          IF (FMCOMP(MXY(2,15),'>',MXY(1,15)) .OR. MXY(1,1)%MP(3) == 0) THEN
              IF (X > 0.8*C4) THEN
                  NMETHD = 3
              ELSE
                  NMETHD = 1
              ENDIF
          ELSE
              IF (X > 0.6*C4) THEN
                  NMETHD = 3
              ELSE IF (X > 6.0) THEN
                  NMETHD = 2
              ELSE
                  NMETHD = 1
              ENDIF
          ENDIF
      ENDIF
      IF (NMETHD /= 1) GO TO 140


!             Method 1.  Use the 1 - (-1)**n x**(2n+1) / n! / (2n+1) series.

!                        = 1 - ( 2 / sqrt(pi) ) * ( x - x^3/1! + x^5/2! - x^7/3! + ... )

      IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG+NGRD52)
      ENDIF
      CALL ZMEQU(MA,MXY(1:2,1),NDSAVE,NDIG)

!             If MA is large in magnitude or close to the real line, use more guard digits.

      CALL FMSQR(MXY(1,1),MXY(1,15))
      CALL FMSQR(MXY(2,1),MXY(2,15))
      CALL FMMPY(MXY(1,1),MXY(2,1),MXY(1,14))
      CALL FMST2M('-4.408443',MXY(1,13))

      CALL FMST2M(' 0.000832',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(1,1))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMST2M(' 0.002632',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(2,1))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMST2M(' 0.124034',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(1,15))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMST2M('-0.000291',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(1,14))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMST2M(' 0.001637',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(2,15))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMM2I(MXY(1,13),IEXTRA)
      IEXTRA = MAX(0,NINT((IEXTRA+3) * 16.11809565D0 / DLOGMB)+1)
      IF (IEXTRA > 0) THEN
          CALL ZMEQU_R1(MXY(1:2,1),NDIG,NDIG+IEXTRA)
      ENDIF
      NDIG = NDIG + IEXTRA
      IF (KFLAGX == 0) THEN
          J2 = INT(0.66*SQRT(FMNTERMS(X**2,1,0,0,1)) - 0.8)
          J2 = MAX(2,MIN(J2+MOD(J2,2),LJSUMS))
      ELSE
          J2 = 2
      ENDIF
      NDSAV1 = NDIG

!             Split into J2 concurrent sums.

      NTERM = 1
      CALL ZMI2M(1,MXY(1:2,6))
      CALL ZMI2M(1,MJSUMS(1:2,1))

      DO J = 2, J2
         IF (NTERM > 1) THEN
             CALL ZMDIVI_R1(MXY(1:2,6),NTERM)
         ENDIF
         CALL ZMDIVI(MXY(1:2,6),2*NTERM+1,MJSUMS(1:2,J))
         NTERM = NTERM + 1
      ENDDO
      CALL ZMABS(MXY(1:2,1),MXY(1,15))
      IF (MXY(1,15)%MP(2) < -NDIG) GO TO 130
      CALL ZMIPWR(MXY(1:2,1),2*J2,MXY(1:2,5))

  120 CALL ZMMPY_R1(MXY(1:2,6),MXY(1:2,5))
      DO J = 1, J2
         CALL ZMDIVI_R1(MXY(1:2,6),NTERM)
         CALL ZMDIVI(MXY(1:2,6),2*NTERM+1,MXY(1:2,4))
         NDIG = NDSAV1
         CALL ZMADD_R1(MJSUMS(1:2,J),MXY(1:2,4))
         IF (KFLAG /= 0) GO TO 130
         NDIG = NDSAV1 - INT(MIN(MJSUMS(1,J)%MP(2)-MXY(1,4)%MP(2),  &
                                 MJSUMS(2,J)%MP(2)-MXY(2,4)%MP(2)))
         NDIG = MIN(NDSAV1,NDIG)
         IF (NDIG < NGRD22) NDIG = NGRD22
         NTERM = NTERM + 1
      ENDDO
      GO TO 120

!             Put the J2 separate sums back together.

  130 KFLAG = 0
      CALL ZMSQR(MXY(1:2,1),MXY(1:2,4))
      CALL ZMMPYI_R1(MXY(1:2,4),-1)
      CALL ZMEQ(MJSUMS(1:2,J2),MXY(1:2,5))

      DO J = 2, J2
         CALL ZMMPY(MXY(1:2,5),MXY(1:2,4),MXY(1:2,15))
         CALL ZMADD(MXY(1:2,15),MJSUMS(1:2,J2-J+1),MXY(1:2,5))
      ENDDO

      CALL ZMMPY_R1(MXY(1:2,5),MXY(1:2,1))

      CALL FMPI(MXY(1,4))
      CALL FMI2M(0,MXY(2,4))
      CALL ZMSQRT(MXY(1:2,4),MXY(1:2,3))
      CALL ZMMPYI(MXY(1:2,5),2,MXY(1:2,4))
      CALL ZMDIV_R2(MXY(1:2,4),MXY(1:2,3))

      CALL ZMI2M(1,MXY(1:2,15))
      CALL ZMSUB_R2(MXY(1:2,15),MXY(1:2,3))

      GO TO 210

  140 IF (NMETHD /= 2) GO TO 170


!             Method 2.  Use the 1 - x**(2n+1) * 2**(n+1) / (1*3*5*...*(2n+1)) series.

      IEXTRA = 0
      CALL FMSQR(MXY(1,1),MXY(1,15))
      CALL FMSQR(MXY(2,1),MXY(2,15))
      CALL FMMPY(MXY(1,1),MXY(2,1),MXY(1,14))
      CALL FMST2M('-1.966130',MXY(1,13))

      CALL FMST2M(' 0.002853',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(1,1))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMST2M('-0.001447',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(2,1))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMST2M(' 0.062040',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(1,15))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMST2M('-0.000063',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(1,14))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMST2M(' 0.061968',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(2,15))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMM2I(MXY(1,13),IEXTRA)
      IEXTRA = MAX(0,NINT((IEXTRA+3) * 16.11809565D0 / DLOGMB)+1)
      NDIG = NDIG + IEXTRA
      IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG) + 1
      ENDIF
      CALL ZMEQU(MA,MXY(1:2,1),NDSAVE,NDIG)
      IF (KFLAGX == 0) THEN
          J2 = INT(0.51*SQRT(FMNTERMS(2*X,2,-1,0,1)))
      ELSE
          J2 = 1
      ENDIF
      J2 = MAX(1,MIN(LJSUMS,J2))
      NDSAV1 = NDIG

!             Split into J2 concurrent sums.

      NTERM = 1
      CALL ZMMPYI(MXY(1:2,1),2,MXY(1:2,6))
      CALL ZMEQ(MXY(1:2,6),MJSUMS(1:2,1))
      DO J = 2, J2
         CALL ZMMPYI_R1(MXY(1:2,6),2)
         CALL ZMDIVI_R1(MXY(1:2,6),2*NTERM+1)
         CALL ZMEQ(MXY(1:2,6),MJSUMS(1:2,J))
         NTERM = NTERM + 1
      ENDDO
      CALL ZMABS(MXY(1:2,1),MXY(1,15))
      IF (MXY(1,15)%MP(2) < -NDIG) GO TO 160
      CALL ZMIPWR(MXY(1:2,1),2*J2,MXY(1:2,5))

  150 CALL ZMMPY_R1(MXY(1:2,6),MXY(1:2,5))
      DO J = 1, J2
         CALL ZMMPYI(MXY(1:2,6),2,MXY(1:2,15))
         CALL ZMDIVI(MXY(1:2,15),2*NTERM+1,MXY(1:2,6))
         NDIG = NDSAV1
         CALL ZMADD_R1(MJSUMS(1:2,J),MXY(1:2,6))
         IF (KFLAG /= 0) GO TO 160
         NDIG = NDSAV1 - INT(MIN(MJSUMS(1,J)%MP(2)-MXY(1,6)%MP(2),  &
                                 MJSUMS(2,J)%MP(2)-MXY(2,6)%MP(2)))
         NDIG = MIN(NDSAV1,NDIG)
         IF (NDIG < NGRD22) NDIG = NGRD22
         NTERM = NTERM + 1
      ENDDO
      GO TO 150

!             Put the J2 separate sums back together.

  160 KFLAG = 0
      CALL ZMSQR(MXY(1:2,1),MXY(1:2,6))
      CALL ZMEQ(MJSUMS(1:2,J2),MXY(1:2,5))
      DO J = 2, J2
         CALL ZMMPY(MXY(1:2,5),MXY(1:2,6),MXY(1:2,15))
         CALL ZMADD(MXY(1:2,15),MJSUMS(1:2,J2-J+1),MXY(1:2,5))
      ENDDO

      CALL FMPI(MXY(1,4))
      CALL FMI2M(0,MXY(2,4))
      CALL ZMSQRT(MXY(1:2,4),MXY(1:2,3))
      CALL ZMDIV_R2(MXY(1:2,5),MXY(1:2,3))
      CALL ZMEXP(MXY(1:2,6),MXY(1:2,4))
      CALL ZMDIV_R1(MXY(1:2,3),MXY(1:2,4))

      CALL ZMI2M(1,MXY(1:2,15))
      CALL ZMSUB_R2(MXY(1:2,15),MXY(1:2,3))

      GO TO 210


  170 IF (NMETHD /= 3) GO TO 180


!             Method 3.  Continued fraction expansion (asymptotic).
!                        Used for Re(x) > 0.
!                        For Re(x) < 0, use   erfc(x) = 1 - erf(x) = 1 + erf(-x) = 2 - erfc(-x).

!                                  erfc(x) = 0 + exp(-x^2)/sqrt(pi) /
!                                                                 x + 1 /
!                                                                   2*x + 2 /
!                                                                         x + 3 /
!                                                                           2*x + 4 /
!                                                                                 x + 5 ...

      IEXTRA = 0
      NDIG = NDIG + IEXTRA
      IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG) + 1
      ENDIF
      CALL ZMEQU(MA,MXY(1:2,1),NDSAVE,NDIG)
      NDSAV1 = NDIG
      IF (MA(1)%MP(1) < 0) THEN
          CALL ZMMPYI_R1(MXY(1:2,1),-1)
      ENDIF

      CALL ZMEQ(MXY(1:2,1),MXY(1:2,2))

      CALL ZMSQR(MXY(1:2,2),MXY(1:2,4))
      CALL ZMMPYI(MXY(1:2,4),-1,MXY(1:2,15))
      CALL ZMEXP(MXY(1:2,15),MXY(1:2,10))
      CALL ZMEQ(MXY(1:2,10),MXY(1:2,15))
      CALL FMPI(MXY(1,10))
      CALL FMSQRT_R1(MXY(1,10))
      CALL FMI2M(0,MXY(2,10))
      CALL ZMDIV_R2(MXY(1:2,15),MXY(1:2,10))
      CALL ZMDIV_R1(MXY(1:2,10),MXY(1:2,2))
      CALL ZMI2M(-31,MXY(1:2,13))

      CALL ZMEQ(MXY(1:2,10),MXY(1:2,14))
      CALL ZMI2M(1,MXY(1:2,8))
      CALL ZMEQ(MXY(1:2,2),MXY(1:2,9))
      NTERMS = INT(INTMAX/10)

!             Continued fraction loop.

      NDSAV2 = NDIG
      DO K = 3, NTERMS
         CALL ZMMPYI(MXY(1:2,2),1+MOD(K,2),MXY(1:2,11))
         CALL ZMMPY_R1(MXY(1:2,11),MXY(1:2,9))
         CALL ZMMPYI(MXY(1:2,8),K-2,MXY(1:2,12))
         CALL ZMADD(MXY(1:2,11),MXY(1:2,12),MXY(1:2,7))
         CALL ZMMPY_R1(MXY(1:2,10),MXY(1:2,12))
         CALL ZMDIV_R1(MXY(1:2,10),MXY(1:2,7))
         CALL ZMMPYI_R1(MXY(1:2,10),-1)
         CALL ZMEQ(MXY(1:2,9),MXY(1:2,8))
         CALL ZMEQ(MXY(1:2,7),MXY(1:2,9))
         NDIG = NDSAV2
         CALL ZMADD_R1(MXY(1:2,14),MXY(1:2,10))

!             Check for convergence.

         IF (KFLAG == 1 .AND. K > 2) THEN
             EXIT
         ENDIF
         KL = MAX( 1000.0, 5 * ALOGMT * NDSAVE )
         IF (K > KL) THEN
             NMETHD = 2
             GO TO 140
         ENDIF
         CALL ZMEQ(MXY(1:2,10),MXY(1:2,13))
         NDIG = MIN(NDSAV2,MAX(NGRD22,NDSAV2-INT(                           &
                                      MIN(MXY(1,14)%MP(2)-MXY(1,10)%MP(2),  &
                                          MXY(2,14)%MP(2)-MXY(2,10)%MP(2)))))
      ENDDO

      CALL FMI2M(1,MXY(1,15))
      CALL FMULP(MXY(1,15),MXY(2,15))
      CALL FMDIV(MXY(1,1),MXY(2,1),MXY(1,15))
      CALL FMSQR_R1(MXY(1,15))
      IF (FMCOMP(MXY(1,15),'<',MXY(2,15))) THEN
          CALL FMI2M(1,MXY(1,15))
          CALL FMADD_R2(MXY(1,15),MXY(1,14))
      ENDIF
      CALL ZMEQ(MXY(1:2,14),MXY(1:2,3))
      IF (MA(1)%MP(1) < 0) THEN
          CALL ZMI2M(2,MXY(1:2,15))
          CALL ZMSUB_R2(MXY(1:2,15),MXY(1:2,3))
      ENDIF
      NDIG = NDSAV1
      GO TO 210


!             Method 4.  Asymptotic series
!
!                        For Re(x) >= 0,
!                        erfc(x) = ( Exp(-x^2) / Sqrt(Pi) ) *
!                                  Sum_{k=0}^Infinity (  (-1)^k * poch(1/2,k) / x^(2*k+1) )

!                        For Re(x) < 0, use   erfc(x) = 1 - erf(x) = 1 + erf(-x) = 2 - erfc(-x).

!             The series is  1/x - (1/2) / x^3  +  ( 1*3 )/(2^2) / x^5 - ( 1*3*5 )/(2^3) / x^7
!                            + ...

  180 IEXTRA = 0
      NDIG = NDIG + IEXTRA
      IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG-1)
      ENDIF
      CALL ZMEQU(MA,MXY(1:2,1),NDSAVE,NDIG)
      IF (MA(1)%MP(1) < 0) THEN
          CALL ZMMPYI_R1(MXY(1:2,1),-1)
      ENDIF
      IF (KFLAGX == 0) THEN
          J2 = INT(0.66*SQRT(FMNTERMS(X**2,1,0,0,0)) - 0.8)
          J2 = MAX(2,MIN(J2+MOD(J2,2),LJSUMS))
      ELSE
          J2 = 2
      ENDIF
      NDSAV1 = NDIG

!             Split into J2 concurrent sums.

      NTERM = 1
      CALL ZMSQR(MXY(1:2,1),MXY(1:2,15))
      CALL ZMMPYI(MXY(1:2,15),2,MXY(1:2,7))
      CALL ZMI2M(1,MXY(1:2,8))
      CALL ZMDIV(MXY(1:2,8),MXY(1:2,7),MXY(1:2,6))
      CALL ZMI2M(1,MJSUMS(1:2,1))
      DO J = 2, J2
         NTERM = NTERM + 1
         CALL ZMMPYI_R1(MXY(1:2,8),2*NTERM-3)
         CALL ZMEQ(MXY(1:2,8),MJSUMS(1:2,J))
      ENDDO
      CALL ZMABS(MXY(1:2,1),MXY(1,15))
      IF (MXY(1,15)%MP(2) < -NDIG) GO TO 130
      CALL ZMIPWR(MXY(1:2,6),J2,MXY(1:2,7))
      CALL ZMI2M(-31,MXY(1:2,13))

  190 CALL ZMMPY_R2(MXY(1:2,7),MXY(1:2,8))
      DO J = 1, J2
         NTERM = NTERM + 1
         CALL ZMMPYI_R1(MXY(1:2,8),2*NTERM-3)
         NDIG = NDSAV1
         CALL ZMADD_R1(MJSUMS(1:2,J),MXY(1:2,8))
         IF (KFLAG /= 0) GO TO 200
         KL = MAX( 19000.0, 6 * ALOGMT * NDSAVE )
         IF (NTERM > KL) THEN
             CALL ZMABS(MXY(1:2,8),MXY(1,15))
             CALL ZMABS(MXY(1:2,13),MXY(2,15))
             IF (FMCOMP(MXY(1,15),'>',MXY(2,15))) THEN
                 NMETHD = 2
                 GO TO 140
             ENDIF
         ENDIF
         CALL ZMEQ(MXY(1:2,8),MXY(1:2,13))
         NDIG = NDSAV1 - INT(MIN(MJSUMS(1,J)%MP(2)-MXY(1,8)%MP(2),  &
                                 MJSUMS(2,J)%MP(2)-MXY(2,8)%MP(2)))
         NDIG = MIN(NDSAV1,NDIG)
         IF (NDIG < NGRD22) NDIG = NGRD22
      ENDDO
      GO TO 190

!             Put the J2 separate sums back together.

  200 KFLAG = 0
      CALL ZMMPYI(MXY(1:2,6),-1,MXY(1:2,4))
      CALL ZMI2M(1,MXY(1:2,5))
      DO J = 2, J2
         CALL ZMMPY_R1(MXY(1:2,5),MXY(1:2,4))
         CALL ZMMPY(MXY(1:2,5),MJSUMS(1:2,J),MXY(1:2,15))
         CALL ZMADD(MXY(1:2,15),MJSUMS(1:2,1),MXY(1:2,8))
         CALL ZMEQ(MXY(1:2,8),MJSUMS(1:2,1))
      ENDDO
      CALL FMMPY(MXY(1,1),MXY(2,1),MXY(2,15))
      CALL FMMPYI_R1(MXY(2,15),-2)
      CALL FMI2M(0,MXY(1,15))
      CALL ZMEXP(MXY(1:2,15),MXY(1:2,14))
      CALL ZMMPY(MJSUMS(1:2,1),MXY(1:2,14),MXY(1:2,15))
      CALL FMPI(MXY(1,4))
      CALL FMSQRT(MXY(1,4),MXY(1,8))
      CALL FMI2M(0,MXY(2,8))
      CALL ZMMPY(MXY(1:2,8),MXY(1:2,1),MXY(1:2,14))
      CALL ZMDIV(MXY(1:2,15),MXY(1:2,14),MXY(1:2,13))
      CALL FMADD(MXY(2,1),MXY(1,1),MXY(1,12))
      CALL FMSUB(MXY(2,1),MXY(1,1),MXY(1,11))
      CALL FMMPY(MXY(1,12),MXY(1,11),MXY(1,10))
      CALL FMEXP(MXY(1,10),MXY(1,12))
      CALL FMMPY(MXY(1,13),MXY(1,12),MXY(1,4))
      CALL FMMPY(MXY(2,13),MXY(1,12),MXY(2,4))
      IF (MXY(1,4)%MP(2) == MUNKNO) THEN
          CALL FMABS(MXY(1,13),MXY(1,14))
          CALL FMLN(MXY(1,14),MXY(1,15))
          CALL FMADD(MXY(1,10),MXY(1,15),MXY(1,14))
          CALL FMEXP(MXY(1,14),MXY(1,4))
          MXY(1,4)%MP(1) = MXY(1,13)%MP(1)
      ENDIF
      IF (MXY(2,4)%MP(2) == MUNKNO) THEN
          CALL FMABS(MXY(2,13),MXY(1,14))
          CALL FMLN(MXY(1,14),MXY(1,15))
          CALL FMADD(MXY(1,10),MXY(1,15),MXY(1,14))
          CALL FMEXP(MXY(1,14),MXY(2,4))
          MXY(2,4)%MP(1) = MXY(2,13)%MP(1)
      ENDIF

      CALL FMI2M(1,MXY(1,15))
      CALL FMULP(MXY(1,15),MXY(2,15))
      CALL FMDIV(MXY(1,1),MXY(2,1),MXY(1,15))
      CALL FMSQR_R1(MXY(1,15))
      IF (FMCOMP(MXY(1,15),'<',MXY(2,15))) THEN
          CALL FMI2M(1,MXY(1,15))
          CALL FMADD_R2(MXY(1,15),MXY(1,4))
      ENDIF

      IF (MA(1)%MP(1) < 0) THEN
          CALL ZMI2M(2,MXY(1:2,15))
          CALL ZMSUB(MXY(1:2,15),MXY(1:2,4),MXY(1:2,3))
      ELSE
          CALL ZMEQ(MXY(1:2,4),MXY(1:2,3))
      ENDIF

  210 NAMEST(NCALL) = 'ZMERFC'
      IF (MA(1)%MP(3) == 0) THEN
          IF (MXY(2,3)%MP(2) /= MUNKNO) THEN
              CALL FMI2M(1,MXY(1,3))
          ENDIF
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL == 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(1,3)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          ERR2 = 0
          DO J = KL, 1, -1
             ERR2 = (ERR2 + MXY(2,3)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR   > 0.498 .AND. ERR  < 0.502) .OR.  &
               (KROUND == 1 .AND. ERR2  > 0.498 .AND. ERR2 < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR  > 0.998 .OR. ERR  < 0.002)) .OR.  &
               (KROUND /= 1 .AND. (ERR2 > 0.998 .OR. ERR2 < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (MXY(1,3)%MP(2) == MUNKNO .OR. MXY(2,3)%MP(2) == MUNKNO) THEN
          CALL ZMST2M('UNKNOWN + UNKNOWN i',MXY(1:2,3))
      ENDIF

      CALL ZMEXIT(MXY(1:2,3),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE ZMERFC

      SUBROUTINE ZMERFC_SC(MA,MB)

!  MB = Exp(MA**2) * Erfc(MA)    Scaled complimentary error function.

!  Erfc(z) underflows for relatively small magnitude |z| in the right half-plane (Re(z) >= 0).
!  Multiplying by Exp(z**2) allows the scaled complimentary error function to be computed even
!  when |z| is large and Re(z) >= 0.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: C1,C2,C4,ERR,ERR2,X,XK
      DOUBLE PRECISION, EXTERNAL :: FMNTERMS
      INTEGER :: IEXTRA,J,J2,K,KFLAGX,KL,KOVUN,KR_RETRY,KRESLT,  &
                 NDSAVE,NDSAV1,NDSAV2,NMETHD,NTERM,NTERMS
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(2,15), MJSUMS(2,LJSUMS)
      LOGICAL, EXTERNAL :: FMCOMP

      IF (.NOT. ALLOCATED(MB(1)%MP)) THEN
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(1)%MP)
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MB(2)%MP)) THEN
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(2)%MP)
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL ZMENTR('ZMERFC_SC',MA,MA,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF

      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL ZMEQU(MA,MXY(1:2,1),NDSAVE,NDIG)

!             Check for special cases.

      IF (MA(1)%MP(3) == 0 .AND. MA(2)%MP(3) == 0) THEN
          CALL ZMEQ(MXY(1:2,1),MXY(1:2,3))
          GO TO 210
      ENDIF
      IF (MXY(1,1)%MP(2) == MEXPUN .OR. MXY(2,1)%MP(2) == MEXPUN) THEN
          CALL ZMST2M('UNKNOWN+UNKNOWN*i',MXY(1:2,3))
          KFLAG = -4
          GO TO 210
      ENDIF

!             X is a double precision approximation to the size of input argument to this function.

      CALL ZMABS(MXY(1:2,1),MXY(1,4))
      CALL FMM2DP(MXY(1,4),X)
      KFLAGX = KFLAG
      IF (KFLAG /= 0 .AND. MXY(1,4)%MP(2) < 0) THEN
          X = 1.0D0/DPMAX**0.33D0
          KFLAGX = 0
      ELSE IF (KFLAG == 0 .AND. X < 1.0D0/DPMAX**0.33D0) THEN
          X = 1.0D0/DPMAX**0.33D0
      ENDIF
      IF (KFLAG /= 0 .AND. MXY(1,4)%MP(2) > 0) THEN
          X = DPMAX**0.33D0
          KFLAGX = 0
      ELSE IF (KFLAG == 0 .AND. X > DPMAX**0.33D0) THEN
          X = DPMAX**0.33D0
      ENDIF

!             Determine which method to use.

!             NMETHD = 1 use the convergent series,
!                    = 2 use a 2nd convergent series,
!                    = 3 use a continued fraction expansion,
!                    = 4 use an asymptotic series.

      XK = MAX(3.0D0,(2*X**2 + 1)/2)
      XK = -XK + LOG(2*XK-1)*(2*XK-0.5D0) + LOG(XK-1)*(-XK+0.5D0) - LOG(2.0D0)*(2*XK-1) -  &
           LOG(X)*(2*XK+1)
      IF (-XK/DLOGMB > NDIG+(3+NDIG/20)*NGRD52) THEN
          NMETHD = 4
      ELSE
          C4 = 5.0
          XK = 0.0
          DO WHILE (-XK/DLOGMB <= NDIG+(3+NDIG/20)*NGRD52)
             C4 = 2 * C4
             XK = MAX(3.0D0,(2*C4**2 + 1)/2)
             XK = -XK + LOG(2*XK-1)*(2*XK-0.5D0) + LOG(XK-1)*(-XK+0.5D0) - LOG(2.0D0)*(2*XK-1) -  &
                  LOG(C4)*(2*XK+1)
          ENDDO
          C1 = C4/2
          DO WHILE (C4-C1 > 0.1)
             C2 = (C1 + C4) / 2
             XK = MAX(3.0D0,(2*C2**2 + 1)/2)
             XK = -XK + LOG(2*XK-1)*(2*XK-0.5D0) + LOG(XK-1)*(-XK+0.5D0) - LOG(2.0D0)*(2*XK-1) -  &
                  LOG(C2)*(2*XK+1)
             IF (-XK/DLOGMB > NDIG+(3+NDIG/20)*NGRD52) THEN
                 C4 = C2
             ELSE
                 C1 = C2
             ENDIF
          ENDDO
          CALL FMDIV(MXY(2,1),MXY(1,1),MXY(1,15))
          CALL FMABS(MXY(1,15),MXY(2,15))
          CALL FMDP2M(1.5D0,MXY(1,15))
          IF (FMCOMP(MXY(2,15),'>',MXY(1,15)) .OR. MXY(1,1)%MP(3) == 0) THEN
              IF (X > 0.8*C4) THEN
                  NMETHD = 3
              ELSE
                  NMETHD = 1
              ENDIF
          ELSE
              IF (X > 0.6*C4) THEN
                  NMETHD = 3
              ELSE IF (X > 6.0) THEN
                  NMETHD = 2
              ELSE
                  NMETHD = 1
              ENDIF
          ENDIF
      ENDIF
      IF (NMETHD /= 1) GO TO 140


!             Method 1.  Use the 1 - (-1)**n x**(2n+1) / n! / (2n+1) series.

!                        exp(x^2) * ( 1 - (2/sqrt(pi))*( x - x^3/1! + x^5/2! - x^7/3! + ... ) )

      IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG+NGRD52)
      ENDIF
      CALL ZMEQU(MA,MXY(1:2,1),NDSAVE,NDIG)

!             If MA is large in magnitude or close to the real line, use more guard digits.

      CALL FMSQR(MXY(1,1),MXY(1,15))
      CALL FMSQR(MXY(2,1),MXY(2,15))
      CALL FMMPY(MXY(1,1),MXY(2,1),MXY(1,14))
      CALL FMST2M('-4.408443',MXY(1,13))

      CALL FMST2M(' 0.000832',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(1,1))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMST2M(' 0.002632',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(2,1))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMST2M(' 0.124034',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(1,15))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMST2M('-0.000291',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(1,14))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMST2M(' 0.001637',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(2,15))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMM2I(MXY(1,13),IEXTRA)
      IEXTRA = MAX(0,NINT((IEXTRA+3) * 16.11809565D0 / DLOGMB)+1)
      IF (IEXTRA > 0) THEN
          CALL ZMEQU_R1(MXY(1:2,1),NDIG,NDIG+IEXTRA)
      ENDIF
      NDIG = NDIG + IEXTRA
      IF (KFLAGX == 0) THEN
          J2 = INT(0.66*SQRT(FMNTERMS(X**2,1,0,0,1)) - 0.8)
          J2 = MAX(2,MIN(J2+MOD(J2,2),LJSUMS))
      ELSE
          J2 = 2
      ENDIF
      NDSAV1 = NDIG

!             Split into J2 concurrent sums.

      NTERM = 1
      CALL ZMI2M(1,MXY(1:2,6))
      CALL ZMI2M(1,MJSUMS(1:2,1))

      DO J = 2, J2
         IF (NTERM > 1) THEN
             CALL ZMDIVI_R1(MXY(1:2,6),NTERM)
         ENDIF
         CALL ZMDIVI(MXY(1:2,6),2*NTERM+1,MJSUMS(1:2,J))
         NTERM = NTERM + 1
      ENDDO
      CALL ZMABS(MXY(1:2,1),MXY(1,15))
      IF (MXY(1,15)%MP(2) < -NDIG) GO TO 130
      CALL ZMIPWR(MXY(1:2,1),2*J2,MXY(1:2,5))

  120 CALL ZMMPY_R1(MXY(1:2,6),MXY(1:2,5))
      DO J = 1, J2
         CALL ZMDIVI_R1(MXY(1:2,6),NTERM)
         CALL ZMDIVI(MXY(1:2,6),2*NTERM+1,MXY(1:2,4))
         NDIG = NDSAV1
         CALL ZMADD_R1(MJSUMS(1:2,J),MXY(1:2,4))
         IF (KFLAG /= 0) GO TO 130
         NDIG = NDSAV1 - INT(MIN(MJSUMS(1,J)%MP(2)-MXY(1,4)%MP(2),  &
                                 MJSUMS(2,J)%MP(2)-MXY(2,4)%MP(2)))
         NDIG = MIN(NDSAV1,NDIG)
         IF (NDIG < NGRD22) NDIG = NGRD22
         NTERM = NTERM + 1
      ENDDO
      GO TO 120

!             Put the J2 separate sums back together.

  130 KFLAG = 0
      CALL ZMSQR(MXY(1:2,1),MXY(1:2,4))
      CALL ZMMPYI_R1(MXY(1:2,4),-1)
      CALL ZMEQ(MJSUMS(1:2,J2),MXY(1:2,5))

      DO J = 2, J2
         CALL ZMMPY(MXY(1:2,5),MXY(1:2,4),MXY(1:2,15))
         CALL ZMADD(MXY(1:2,15),MJSUMS(1:2,J2-J+1),MXY(1:2,5))
      ENDDO

      CALL ZMMPY_R1(MXY(1:2,5),MXY(1:2,1))

      CALL FMPI(MXY(1,4))
      CALL FMI2M(0,MXY(2,4))
      CALL ZMSQRT(MXY(1:2,4),MXY(1:2,3))
      CALL ZMMPYI(MXY(1:2,5),2,MXY(1:2,4))
      CALL ZMDIV_R2(MXY(1:2,4),MXY(1:2,3))

      CALL ZMI2M(1,MXY(1:2,15))
      CALL ZMSUB_R2(MXY(1:2,15),MXY(1:2,3))
      CALL ZMSQR(MXY(1:2,1),MXY(1:2,15))
      CALL ZMEXP(MXY(1:2,15),MXY(1:2,14))
      CALL ZMMPY_R1(MXY(1:2,3),MXY(1:2,14))
      GO TO 210

  140 IF (NMETHD /= 2) GO TO 170


!             Method 2.  Use the 1 - x**(2n+1) * 2**(n+1) / (1*3*5*...*(2n+1)) series.

!                        exp(x^2) * ( 1 - x**(2n+1) * 2**(n+1) / (1*3*5*...*(2n+1)) )

      IEXTRA = 0
      CALL FMSQR(MXY(1,1),MXY(1,15))
      CALL FMSQR(MXY(2,1),MXY(2,15))
      CALL FMMPY(MXY(1,1),MXY(2,1),MXY(1,14))
      CALL FMST2M('-1.966130',MXY(1,13))

      CALL FMST2M(' 0.002853',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(1,1))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMST2M('-0.001447',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(2,1))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMST2M(' 0.062040',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(1,15))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMST2M('-0.000063',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(1,14))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMST2M(' 0.061968',MXY(2,13))
      CALL FMMPY_R1(MXY(2,13),MXY(2,15))
      CALL FMADD_R1(MXY(1,13),MXY(2,13))

      CALL FMM2I(MXY(1,13),IEXTRA)
      IEXTRA = MAX(0,NINT((IEXTRA+3) * 16.11809565D0 / DLOGMB)+1)
      NDIG = NDIG + IEXTRA
      IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG) + 1
      ENDIF
      CALL ZMEQU(MA,MXY(1:2,1),NDSAVE,NDIG)
      IF (KFLAGX == 0) THEN
          J2 = INT(0.51*SQRT(FMNTERMS(2*X,2,-1,0,1)))
      ELSE
          J2 = 1
      ENDIF
      J2 = MAX(1,MIN(LJSUMS,J2))
      NDSAV1 = NDIG

!             Split into J2 concurrent sums.

      NTERM = 1
      CALL ZMMPYI(MXY(1:2,1),2,MXY(1:2,6))
      CALL ZMEQ(MXY(1:2,6),MJSUMS(1:2,1))
      DO J = 2, J2
         CALL ZMMPYI_R1(MXY(1:2,6),2)
         CALL ZMDIVI_R1(MXY(1:2,6),2*NTERM+1)
         CALL ZMEQ(MXY(1:2,6),MJSUMS(1:2,J))
         NTERM = NTERM + 1
      ENDDO
      CALL ZMABS(MXY(1:2,1),MXY(1,15))
      IF (MXY(1,15)%MP(2) < -NDIG) GO TO 160
      CALL ZMIPWR(MXY(1:2,1),2*J2,MXY(1:2,5))

  150 CALL ZMMPY_R1(MXY(1:2,6),MXY(1:2,5))
      DO J = 1, J2
         CALL ZMMPYI(MXY(1:2,6),2,MXY(1:2,15))
         CALL ZMDIVI(MXY(1:2,15),2*NTERM+1,MXY(1:2,6))
         NDIG = NDSAV1
         CALL ZMADD_R1(MJSUMS(1:2,J),MXY(1:2,6))
         IF (KFLAG /= 0) GO TO 160
         NDIG = NDSAV1 - INT(MIN(MJSUMS(1,J)%MP(2)-MXY(1,6)%MP(2),  &
                                 MJSUMS(2,J)%MP(2)-MXY(2,6)%MP(2)))
         NDIG = MIN(NDSAV1,NDIG)
         IF (NDIG < NGRD22) NDIG = NGRD22
         NTERM = NTERM + 1
      ENDDO
      GO TO 150

!             Put the J2 separate sums back together.

  160 KFLAG = 0
      CALL ZMSQR(MXY(1:2,1),MXY(1:2,6))
      CALL ZMEQ(MJSUMS(1:2,J2),MXY(1:2,5))
      DO J = 2, J2
         CALL ZMMPY(MXY(1:2,5),MXY(1:2,6),MXY(1:2,15))
         CALL ZMADD(MXY(1:2,15),MJSUMS(1:2,J2-J+1),MXY(1:2,5))
      ENDDO

      CALL FMPI(MXY(1,4))
      CALL FMI2M(0,MXY(2,4))
      CALL ZMSQRT(MXY(1:2,4),MXY(1:2,3))
      CALL ZMDIV_R2(MXY(1:2,5),MXY(1:2,3))
      CALL ZMEXP(MXY(1:2,6),MXY(1:2,4))
      CALL ZMDIV_R1(MXY(1:2,3),MXY(1:2,4))

      CALL ZMI2M(1,MXY(1:2,15))
      CALL ZMSUB_R2(MXY(1:2,15),MXY(1:2,3))

      CALL ZMSQR(MXY(1:2,1),MXY(1:2,15))
      CALL ZMEXP(MXY(1:2,15),MXY(1:2,14))
      CALL ZMMPY_R1(MXY(1:2,3),MXY(1:2,14))
      GO TO 210


  170 IF (NMETHD /= 3) GO TO 180


!             Method 3.  Continued fraction expansion (asymptotic).
!                        Used for Re(x) > 0.

!                               erfc_sc(x) = 0 + 1/sqrt(pi) /
!                                                         x + 1 /
!                                                           2*x + 2 /
!                                                                 x + 3 /
!                                                                   2*x + 4 /
!                                                                         x + 5 ...
!
!                        For Re(x) < 0, use   erfc_sc(x) = exp(x**2) * (1 - erf(x))
!                                                        = exp(x**2) * (1 + erf(-x))
!                                                        = exp(x**2) * (2 - erfc(-x))
!                                                        = 2*exp(x**2) - erfc_sc(-x)

      IEXTRA = 0
      NDIG = NDIG + IEXTRA
      IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG) + 1
      ENDIF
      CALL ZMEQU(MA,MXY(1:2,1),NDSAVE,NDIG)
      NDSAV1 = NDIG
      IF (MA(1)%MP(1) < 0) THEN
          CALL ZMMPYI_R1(MXY(1:2,1),-1)
      ENDIF

      CALL ZMEQ(MXY(1:2,1),MXY(1:2,2))

      CALL ZMI2M(1,MXY(1:2,15))
      CALL FMPI(MXY(1,10))
      CALL FMSQRT_R1(MXY(1,10))
      CALL FMI2M(0,MXY(2,10))
      CALL ZMDIV_R2(MXY(1:2,15),MXY(1:2,10))
      CALL ZMDIV_R1(MXY(1:2,10),MXY(1:2,2))
      CALL ZMI2M(-31,MXY(1:2,13))

      CALL ZMEQ(MXY(1:2,10),MXY(1:2,14))
      CALL ZMI2M(1,MXY(1:2,8))
      CALL ZMEQ(MXY(1:2,2),MXY(1:2,9))
      NTERMS = INT(INTMAX/10)

!             Continued fraction loop.

      NDSAV2 = NDIG
      DO K = 3, NTERMS
         CALL ZMMPYI(MXY(1:2,2),1+MOD(K,2),MXY(1:2,11))
         CALL ZMMPY_R1(MXY(1:2,11),MXY(1:2,9))
         CALL ZMMPYI(MXY(1:2,8),K-2,MXY(1:2,12))
         CALL ZMADD(MXY(1:2,11),MXY(1:2,12),MXY(1:2,7))
         CALL ZMMPY_R1(MXY(1:2,10),MXY(1:2,12))
         CALL ZMDIV_R1(MXY(1:2,10),MXY(1:2,7))
         CALL ZMMPYI_R1(MXY(1:2,10),-1)
         CALL ZMEQ(MXY(1:2,9),MXY(1:2,8))
         CALL ZMEQ(MXY(1:2,7),MXY(1:2,9))
         NDIG = NDSAV2
         CALL ZMADD_R1(MXY(1:2,14),MXY(1:2,10))

!             Check for convergence.

         IF (KFLAG == 1 .AND. K > 2) THEN
             EXIT
         ENDIF
         KL = MAX( 1000.0, 5 * ALOGMT * NDSAVE )
         IF (K > KL) THEN
             NMETHD = 2
             GO TO 140
         ENDIF
         CALL ZMEQ(MXY(1:2,10),MXY(1:2,13))
         NDIG = MIN(NDSAV2,MAX(NGRD22,NDSAV2-INT(                           &
                                      MIN(MXY(1,14)%MP(2)-MXY(1,10)%MP(2),  &
                                          MXY(2,14)%MP(2)-MXY(2,10)%MP(2)))))
      ENDDO

      CALL FMI2M(1,MXY(1,15))
      CALL FMULP(MXY(1,15),MXY(2,15))
      CALL FMDIV(MXY(1,1),MXY(2,1),MXY(1,15))
      CALL FMSQR_R1(MXY(1,15))
      IF (FMCOMP(MXY(1,15),'<',MXY(2,15))) THEN
          CALL ZMSQR(MXY(1:2,1),MXY(1:2,13))
          CALL ZMEXP(MXY(1:2,13),MXY(1:2,15))
          CALL ZMADD_R2(MXY(1:2,15),MXY(1,14))
      ENDIF
      CALL ZMEQ(MXY(1:2,14),MXY(1:2,3))
      IF (MA(1)%MP(1) < 0) THEN
          CALL ZMSQR(MXY(1:2,1),MXY(1:2,15))
          CALL ZMEXP(MXY(1:2,15),MXY(1:2,14))
          CALL ZMMPYI(MXY(1:2,14),2,MXY(1:2,15))
          CALL ZMSUB_R2(MXY(1:2,15),MXY(1:2,3))
      ENDIF
      NDIG = NDSAV1
      GO TO 210


!             Method 4.  Asymptotic series
!
!                        For Re(x) >= 0,
!                        erfc_sc(x) = ( 1 / Sqrt(Pi) ) *
!                                  Sum_{k=0}^Infinity (  (-1)^k * poch(1/2,k) / x^(2*k+1) )

!                        The series is  1/x - (1/2) / x^3  +  ( 1*3 )/(2^2) / x^5 -
!                                       ( 1*3*5 )/(2^3) / x^7 + ...

!                        For Re(x) < 0, use   erfc(x) = 1 - erf(x) = 1 + erf(-x) = 2 - erfc(-x).
!                                       erfc_sc(x) = Exp(x^2) * (2 - erfc(-x))
!                                                  = 2*Exp(x^2) - erfc_sc(-x)

  180 IEXTRA = 0
      NDIG = NDIG + IEXTRA
      IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG-1)
      ENDIF
      CALL ZMEQU(MA,MXY(1:2,1),NDSAVE,NDIG)
      IF (MA(1)%MP(1) < 0) THEN
          CALL ZMMPYI_R1(MXY(1:2,1),-1)
      ENDIF
      IF (KFLAGX == 0) THEN
          J2 = INT(0.66*SQRT(FMNTERMS(X**2,1,0,0,0)) - 0.8)
          J2 = MAX(2,MIN(J2+MOD(J2,2),LJSUMS))
      ELSE
          J2 = 2
      ENDIF
      NDSAV1 = NDIG

!             Split into J2 concurrent sums.

      NTERM = 1
      CALL ZMSQR(MXY(1:2,1),MXY(1:2,15))
      CALL ZMMPYI(MXY(1:2,15),2,MXY(1:2,7))
      CALL ZMI2M(1,MXY(1:2,8))
      CALL ZMDIV(MXY(1:2,8),MXY(1:2,7),MXY(1:2,6))
      CALL ZMI2M(1,MJSUMS(1:2,1))
      DO J = 2, J2
         NTERM = NTERM + 1
         CALL ZMMPYI_R1(MXY(1:2,8),2*NTERM-3)
         CALL ZMEQ(MXY(1:2,8),MJSUMS(1:2,J))
      ENDDO
      CALL ZMABS(MXY(1:2,1),MXY(1,15))
      IF (MXY(1,15)%MP(2) < -NDIG) GO TO 130
      CALL ZMIPWR(MXY(1:2,6),J2,MXY(1:2,7))
      CALL ZMI2M(-31,MXY(1:2,13))

  190 CALL ZMMPY_R2(MXY(1:2,7),MXY(1:2,8))
      DO J = 1, J2
         NTERM = NTERM + 1
         CALL ZMMPYI_R1(MXY(1:2,8),2*NTERM-3)
         NDIG = NDSAV1
         CALL ZMADD_R1(MJSUMS(1:2,J),MXY(1:2,8))
         IF (KFLAG /= 0) GO TO 200
         KL = MAX( 19000.0, 6 * ALOGMT * NDSAVE )
         IF (NTERM > KL) THEN
             CALL ZMABS(MXY(1:2,8),MXY(1,15))
             CALL ZMABS(MXY(1:2,13),MXY(2,15))
             IF (FMCOMP(MXY(1,15),'>',MXY(2,15))) THEN
                 NMETHD = 2
                 GO TO 140
             ENDIF
         ENDIF
         CALL ZMEQ(MXY(1:2,8),MXY(1:2,13))
         NDIG = NDSAV1 - INT(MIN(MJSUMS(1,J)%MP(2)-MXY(1,8)%MP(2),  &
                                 MJSUMS(2,J)%MP(2)-MXY(2,8)%MP(2)))
         NDIG = MIN(NDSAV1,NDIG)
         IF (NDIG < NGRD22) NDIG = NGRD22
      ENDDO
      GO TO 190

!             Put the J2 separate sums back together.

  200 KFLAG = 0
      CALL ZMMPYI(MXY(1:2,6),-1,MXY(1:2,4))
      CALL ZMI2M(1,MXY(1:2,5))
      DO J = 2, J2
         CALL ZMMPY_R1(MXY(1:2,5),MXY(1:2,4))
         CALL ZMMPY(MXY(1:2,5),MJSUMS(1:2,J),MXY(1:2,15))
         CALL ZMADD(MXY(1:2,15),MJSUMS(1:2,1),MXY(1:2,8))
         CALL ZMEQ(MXY(1:2,8),MJSUMS(1:2,1))
      ENDDO

      CALL FMPI(MXY(1,4))
      CALL FMSQRT(MXY(1,4),MXY(1,8))
      CALL FMI2M(0,MXY(2,8))
      CALL ZMDIV(MJSUMS(1:2,1),MXY(1:2,8),MXY(1:2,4))
      CALL ZMDIV_R1(MXY(1:2,4),MXY(1:2,1))

      CALL FMI2M(1,MXY(1,15))
      CALL FMULP(MXY(1,15),MXY(2,15))
      CALL FMDIV(MXY(1,1),MXY(2,1),MXY(1,15))
      CALL FMSQR_R1(MXY(1,15))
      IF (FMCOMP(MXY(1,15),'<',MXY(2,15))) THEN
          CALL ZMSQR(MXY(1:2,1),MXY(1:2,13))
          CALL ZMEXP(MXY(1:2,13),MXY(1:2,15))
          CALL ZMADD_R2(MXY(1:2,15),MXY(1:2,4))
      ENDIF
      IF (MA(1)%MP(1) < 0) THEN
          CALL ZMSQR(MXY(1:2,1),MXY(1:2,15))
          CALL FMI2M(2,MXY(1,14))
          CALL FMLN(MXY(1,14),MXY(1,13))
          CALL FMADD_R1(MXY(1,15),MXY(1,13))
          CALL ZMEXP(MXY(1:2,15),MXY(1:2,14))
          CALL ZMSUB(MXY(1:2,14),MXY(1:2,4),MXY(1:2,3))
      ELSE
          CALL ZMEQ(MXY(1:2,4),MXY(1:2,3))
      ENDIF

  210 NAMEST(NCALL) = 'ZMERFC_SC'

      IF (MA(1)%MP(3) == 0) THEN
          IF (MXY(2,3)%MP(2) /= MUNKNO) THEN
              CALL FMSQR(MXY(2,1),MXY(1,4))
              CALL FMMPYI_R1(MXY(1,4),-1)
              CALL FMEXP(MXY(1,4),MXY(1,3))
          ENDIF
      ENDIF

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL == 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(1,3)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          ERR2 = 0
          DO J = KL, 1, -1
             ERR2 = (ERR2 + MXY(2,3)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR   > 0.498 .AND. ERR  < 0.502) .OR.  &
               (KROUND == 1 .AND. ERR2  > 0.498 .AND. ERR2 < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR  > 0.998 .OR. ERR  < 0.002)) .OR.  &
               (KROUND /= 1 .AND. (ERR2 > 0.998 .OR. ERR2 < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (MXY(1,3)%MP(2) == MUNKNO .OR. MXY(2,3)%MP(2) == MUNKNO) THEN
          CALL ZMST2M('UNKNOWN + UNKNOWN i',MXY(1:2,3))
      ENDIF

      CALL ZMEXIT(MXY(1:2,3),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE ZMERFC_SC
      SUBROUTINE ZMLNGM(MA,MB)

!  MB = LogGamma(MA)

!  In the complex plane this Log Gamma function is not identical to Ln(Gamma(MA)), because of
!  branch points in the complex log function.

!  This function returns the function value of the principal branch of the log gamma function.
!  It is the same as Ln(Gamma(MA)) when MA is a positive real number, but it can differ from
!  Ln(Gamma(MA)) by an integer multiple of 2*pi*i elsewhere.  Note that we still have
!  Gamma(z) = exp(LogGamma(z)) = exp(Ln(Gamma(z))) even when they differ.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR,ERR2,X,XK,XS
      INTEGER :: J,J1,J2,JC,JS,KL,KOVUN,KR_RETRY,KRESLT,KZ,LARGE,  &
                 NDSAVE,NDSAV1,NTERM
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(2,15), MJSUMS(2,LJSUMS)
      LOGICAL, EXTERNAL :: FMCOMP

!             If MA is real and positive, use FMLNGM.

      IF (MA(2)%MP(3) == 0) THEN
          IF (MA(1)%MP(1) > 0 .AND. MA(1)%MP(3) > 0) THEN
              CALL FMLNGM(MA(1),MB(1))
              CALL FMI2M(0,MB(2))
              IF (MB(1)%MP(2) == MUNKNO) THEN
                  CALL ZMST2M('UNKNOWN+UNKNOWN*i',MB)
              ENDIF
              RETURN
          ENDIF
      ENDIF

      IF (.NOT. ALLOCATED(MB(1)%MP)) THEN
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(1)%MP)
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MB(2)%MP)) THEN
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(2)%MP)
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL ZMENTR('ZMLNGM   ',MA,MA,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF

      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL ZMEQU(MA,MXY(1:2,1),NDSAVE,NDIG)

!             Check for special cases.

      IF (MA(2)%MP(3) == 0) THEN
          CALL FMINT(MXY(1,1),MXY(1,15))
          IF (FMCOMP(MXY(1,1),'==',MXY(1,15))) THEN
              CALL ZMST2M('UNKNOWN+UNKNOWN*i',MXY(1:2,3))
              KFLAG = -4
              GO TO 140
          ENDIF
      ENDIF
      IF (MXY(1,1)%MP(2) == MEXPUN .OR. MXY(2,1)%MP(2) == MEXPUN) THEN
          CALL ZMST2M('UNKNOWN+UNKNOWN*i',MXY(1:2,3))
          KFLAG = -4
          GO TO 140
      ENDIF
      CALL ZMABS(MXY(1:2,1),MXY(1,2))
      CALL FMI2M(1,MXY(1,4))
      IF (FMCOMP(MXY(1,2),'<',MXY(1,4))) THEN
          CALL ZMLN(MXY(1:2,1),MXY(1:2,4))
          CALL ZMMPYI_R1(MXY(1:2,4),-1)
          CALL FMEULER(MXY(1,5))
          CALL FMI2M(0,MXY(2,5))
          CALL ZMMPY_R1(MXY(1:2,5),MXY(1:2,1))
          CALL ZMSUB_R1(MXY(1:2,4),MXY(1:2,5))
          CALL FMPI(MXY(1,3))
          CALL FMSQR_R1(MXY(1,3))
          CALL FMDIVI_R1(MXY(1,3),12)
          CALL FMI2M(0,MXY(2,3))
          CALL ZMSQR(MXY(1:2,1),MXY(1:2,5))
          CALL ZMMPY(MXY(1:2,3),MXY(1:2,5),MXY(1:2,6))
          CALL ZMADD_R1(MXY(1:2,4),MXY(1:2,6))
          CALL ZMMPY_R1(MXY(1:2,5),MXY(1:2,1))
          CALL ZMMPYI(MXY(1:2,5),4,MXY(1:2,6))
          CALL ZMDIVI_R1(MXY(1:2,6),10)
          CALL ZMSUB(MXY(1,4),MXY(1,6),MXY(1,3))
          IF (KFLAG == 1) THEN
              GO TO 140
          ENDIF
      ENDIF

!             X is a double precision approximation to the size of input argument to this function.

      CALL ZMABS(MXY(1:2,1),MXY(1,4))
      CALL FMM2DP(MXY(1,4),X)
      IF (KFLAG /= 0 .AND. MXY(1,4)%MP(2) < 0) THEN
          X = 1.0D0/DPMAX**0.33D0
      ELSE IF (KFLAG == 0 .AND. X < 1.0D0/DPMAX**0.33D0) THEN
          X = 1.0D0/DPMAX**0.33D0
      ENDIF
      IF (KFLAG /= 0 .AND. MXY(1,4)%MP(2) > 0) THEN
          X = DPMAX**0.33D0
      ELSE IF (KFLAG == 0 .AND. X > DPMAX**0.33D0) THEN
          X = DPMAX**0.33D0
      ENDIF

!             Determine the smallest shift JS for which the asymptotic series for ln(gamma(x+js))
!             converges for the current precison.

      CALL ZMEQ(MXY(1:2,1),MXY(1:2,2))
      MXY(1,2)%MP(1) = 1
      XK = 1.5D0*LOG(ABS(X)) - 2*3.14159*ABS(X)
      IF (ABS(X) < 2) XK = 0
      IF (-XK/DLOGMB > NDIG+(3+NDIG/20)*NGRD52) THEN
          JS = 0
      ELSE
          J1 = 1
          J2 = J1
          DO WHILE (-XK/DLOGMB <= NDIG+(3+NDIG/20)*NGRD52)
             J2 = 2*J2
             CALL ZMI2M(J2,MXY(1:2,5))
             CALL ZMADD(MXY(1:2,2),MXY(1:2,5),MXY(1:2,6))
             CALL ZMABS(MXY(1:2,6),MXY(1,4))
             CALL FMM2DP(MXY(1,4),XS)
             XK = 1.5D0*LOG(XS) - 2*3.14159*XS
          ENDDO
          JS = J2
          DO WHILE (ABS(J2-J1) > 1)
             JS = (J1 + J2) / 2
             CALL ZMI2M(JS,MXY(1:2,5))
             CALL ZMADD(MXY(1:2,2),MXY(1:2,5),MXY(1:2,6))
             CALL ZMABS(MXY(1:2,6),MXY(1,4))
             CALL FMM2DP(MXY(1,4),XS)
             XK = 1.5D0*LOG(XS) - 2*3.14159*XS
             IF (-XK/DLOGMB > NDIG+(3+NDIG/20)*NGRD52) THEN
                 J2 = JS
             ELSE
                 J1 = JS
             ENDIF
          ENDDO
      ENDIF

!             Use the asymptotic series after replacing input z by 1-z via the reflection formula
!             if re(z) < 0, by conjugate(z) if im(z) < 0, and by z+js if needed for enough accuracy.

      CALL ZMEQ(MXY(1:2,1),MXY(1:2,2))
      IF (MA(1)%MP(1) < 0) THEN
          CALL ZMI2M(1,MXY(1:2,15))
          CALL ZMSUB_R2(MXY(1:2,15),MXY(1:2,2))
      ENDIF
      JC = 0
      IF (MXY(2,2)%MP(1) < 0) THEN
          JC = 1
          CALL FMMPYI_R1(MXY(2,2),-1)
      ENDIF
      CALL ZMI2M(JS,MXY(1:2,5))
      CALL ZMADD_R1(MXY(1:2,2),MXY(1:2,5))
      IF (KR_RETRY <= 0 .AND. NCALL <= 1) THEN
          NDIG = MAX(NDSAVE+NGRD52,NDIG-1)
      ENDIF
      J2 = INT(0.3*ALOGMB + 0.2*SQRT(REAL(NDIG)))
      J2 = MAX(1,MIN(LJSUMS,J2))

      NDSAV1 = NDIG

!             Split into J2 concurrent sums.

      CALL ZMSQR(MXY(1:2,2),MXY(1:2,6))
      CALL ZMI2M(1,MXY(1:2,15))
      CALL ZMDIV_R2(MXY(1:2,15),MXY(1:2,6))
      CALL ZMIPWR(MXY(1:2,6),J2,MXY(1:2,7))
      CALL ZMEQ(MXY(1:2,2),MXY(1:2,8))

      NTERM = 0
      CALL ZMLN(MXY(1:2,2),MXY(1:2,10))
      CALL ZMI2M(1,MXY(1:2,11))
      CALL ZMDIVI_R1(MXY(1:2,11),2)
      CALL ZMSUB(MXY(1:2,2),MXY(1:2,11),MXY(1:2,12))
      CALL ZMMPY(MXY(1:2,10),MXY(1:2,12),MXY(1:2,11))
      CALL ZMSUB(MXY(1:2,11),MXY(1:2,2),MXY(1:2,10))
      CALL FMPI(MXY(1,11))
      CALL FMMPYI_R1(MXY(1,11),2)
      CALL FMLN(MXY(1,11),MXY(1,12))
      CALL FMDIVI_R1(MXY(1,12),2)
      CALL FMI2M(0,MXY(2,12))
      CALL ZMADD(MXY(1:2,10),MXY(1:2,12),MJSUMS(1:2,1))
      DO J = 2, J2
         NTERM = NTERM + 1
         CALL FMBERN(2*NTERM,MXY(1,8),MXY(1,15))
         CALL FMBERN(2*NTERM,MXY(2,8),MXY(2,15))
         CALL ZMDIVI(MXY(1:2,15),(2*NTERM-1)*2*NTERM,MJSUMS(1:2,J))
      ENDDO

      CALL ZMI2M(0,MXY(1:2,9))
      CALL ZMI2M(0,MXY(1:2,13))
      CALL ZMEQ(MJSUMS(1:2,J2),MXY(1:2,13))

  120 CALL ZMMPY_R2(MXY(1:2,7),MXY(1:2,8))
      DO J = 1, J2
         NTERM = NTERM + 1
         LARGE = INT(INTMAX/(2*NTERM))/(2*NTERM)
         CALL FMBERN(2*NTERM,MXY(1,8),MXY(1,15))
         CALL FMBERN(2*NTERM,MXY(2,8),MXY(2,15))
         IF (NTERM < LARGE) THEN
             CALL ZMDIVI_R1(MXY(1:2,15),(2*NTERM-1)*2*NTERM)
         ELSE
             CALL ZMDIVI_R1(MXY(1:2,15),2*NTERM-1)
             CALL ZMDIVI_R1(MXY(1:2,15),2*NTERM)
         ENDIF
         NDIG = NDSAV1
         CALL ZMADD_R1(MJSUMS(1:2,J),MXY(1:2,15))
         IF (KFLAG /= 0) GO TO 130
         IF (J == J2) THEN
             CALL ZMEQ(MXY(1:2,9),MXY(1:2,13))
             CALL ZMEQ(MXY(1:2,15),MXY(1:2,9))
         ENDIF
         KL = MAX( 19000.0, 6 * ALOGMT * NDSAVE )
         IF (NTERM > KL) THEN
             CALL ZMABS(MXY(1:2,9),MXY(1,15))
             CALL ZMABS(MXY(1:2,13),MXY(2,15))
             IF (FMCOMP(MXY(1,15),'>',MXY(2,15))) THEN
                 CALL ZMST2M('UNKNOWN + UNKNOWN i',MXY(1:2,3))
                 GO TO 140
             ENDIF
         ENDIF
         NDIG = NDSAV1 - INT(MIN(MJSUMS(1,J)%MP(2)-MXY(1,15)%MP(2),  &
                                 MJSUMS(2,J)%MP(2)-MXY(2,15)%MP(2)))
         NDIG = MIN(NDSAV1,NDIG)
         IF (NDIG < NGRD22) NDIG = NGRD22
      ENDDO
      GO TO 120

!             Put the J2 separate sums back together.

  130 KFLAG = 0
      CALL ZMI2M(1,MXY(1:2,5))
      DO J = 2, J2
         CALL ZMMPY_R1(MXY(1:2,5),MXY(1:2,6))
         CALL ZMMPY(MXY(1:2,5),MJSUMS(1:2,J),MXY(1:2,15))
         CALL ZMADD_R2(MXY(1:2,15),MJSUMS(1:2,1))
      ENDDO
      CALL ZMEQ(MJSUMS(1:2,1),MXY(1:2,3))

!             Reverse the effects of modifying the original argument.

      KZ = 0
      KL = 1
      IF (JS /= 0) THEN
          CALL ZMEQ(MXY(1:2,1),MXY(1:2,14))
          IF (MA(1)%MP(1) < 0) THEN
              CALL ZMI2M(1,MXY(1:2,15))
              CALL ZMSUB_R2(MXY(1:2,15),MXY(1:2,14))
          ENDIF
          IF (MXY(2,14)%MP(1) < 0) THEN
              CALL FMMPYI_R1(MXY(2,14),-1)
          ENDIF
          CALL ZMI2M(1,MXY(1:2,15))
          DO J = 1, JS
             CALL ZMMPY_R1(MXY(1:2,15),MXY(1:2,14))
             IF (MXY(2,15)%MP(1) < 0 .AND. KL == 1) KZ = KZ + 1
             KL = MXY(2,15)%MP(1)
             CALL FMADDI(MXY(1,14),1)
          ENDDO
          CALL ZMLN(MXY(1:2,15),MXY(1:2,14))
          CALL ZMSUB_R1(MXY(1:2,3),MXY(1:2,14))
          CALL FMPI(MXY(2,14))
          CALL FMMPYI_R1(MXY(2,14),2*KZ)
          CALL FMSUB_R1(MXY(2,3),MXY(2,14))
      ENDIF

      IF (JC == 1) THEN
          CALL FMMPYI_R1(MXY(2,3),-1)
      ENDIF
      IF (MA(1)%MP(1) < 0) THEN
          CALL FMPI(MXY(1,14))
          CALL FMI2M(0,MXY(2,14))
          CALL ZMMPY(MXY(1:2,14),MXY(1:2,1),MXY(1:2,15))
          CALL ZMSIN(MXY(1:2,15),MXY(1:2,13))
          IF (ABS(MXY(1,13)%MP(2)) >= MEXPOV .OR. ABS(MXY(2,13)%MP(2)) >= MEXPOV) THEN
              CALL FMCOS_SIN(MXY(1,15),MXY(2,10),MXY(1,10))
              IF (MXY(2,15)%MP(1) < 0) CALL FMMPYI_R1(MXY(2,10),-1)
              CALL ZMARG(MXY(1:2,10),MXY(2,13))
              CALL FMMPYI_R1(MXY(2,13),-1)
              CALL FMMPYI(MXY(1,14),2,MXY(1,12))
              CALL FMLN(MXY(1,12),MXY(1,13))
              CALL FMABS(MXY(2,15),MXY(1,12))
              CALL FMSUB_R1(MXY(1,13),MXY(1,12))
          ELSE
              CALL ZMDIV(MXY(1:2,14),MXY(1:2,13),MXY(1:2,15))
              CALL ZMLN(MXY(1:2,15),MXY(1:2,13))
          ENDIF
          CALL ZMSUB_R2(MXY(1:2,13),MXY(1:2,3))
          CALL FMDIVI(MXY(1,1),2,MXY(1,15))
          IF (MA(2)%MP(3) == 0) THEN
              CALL FMI2M(4,MXY(1,13))
          ELSE
              CALL FMI2M(3,MXY(1,13))
          ENDIF
          CALL FMDIVI_R1(MXY(1,13),4)
          CALL FMSUB_R1(MXY(1,15),MXY(1,13))
          CALL FMINT(MXY(1,15),MXY(1,11))
          CALL FMSUB(MXY(1,15),MXY(1,11),MXY(1,12))
          IF (MXY(1,12)%MP(3) == 0) THEN
              CALL FMEQ(MXY(1,15),MXY(1,13))
          ELSE IF (MXY(1,15)%MP(1) > 0) THEN
              CALL FMADDI(MXY(1,11),1)
              CALL FMEQ(MXY(1,11),MXY(1,13))
          ELSE
              CALL FMEQ(MXY(1,11),MXY(1,13))
          ENDIF
          CALL FMMPY(MXY(1,13),MXY(1,14),MXY(1,11))
          IF (MA(2)%MP(1) < 0) THEN
              CALL FMMPYI_R1(MXY(1,11),-2)
          ELSE
              CALL FMMPYI_R1(MXY(1,11),2)
          ENDIF
          CALL FMADD_R1(MXY(2,3),MXY(1,11))
      ENDIF

  140 NAMEST(NCALL) = 'ZMLNGM'

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL == 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(1,3)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          ERR2 = 0
          DO J = KL, 1, -1
             ERR2 = (ERR2 + MXY(2,3)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR   > 0.498 .AND. ERR  < 0.502) .OR.  &
               (KROUND == 1 .AND. ERR2  > 0.498 .AND. ERR2 < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR  > 0.998 .OR. ERR  < 0.002)) .OR.  &
               (KROUND /= 1 .AND. (ERR2 > 0.998 .OR. ERR2 < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (MXY(1,3)%MP(2) == MUNKNO .OR. MXY(2,3)%MP(2) == MUNKNO) THEN
          CALL ZMST2M('UNKNOWN + UNKNOWN i',MXY(1:2,3))
      ENDIF

      CALL ZMEXIT(MXY(1:2,3),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE ZMLNGM

      SUBROUTINE ZMGAM(MA,MB)

!  MB = Gamma(MA)

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR,ERR2
      INTEGER :: J,KL,KOVUN,KR_RETRY,KRESLT,NDSAVE
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(2,15)
      LOGICAL, EXTERNAL :: FMCOMP

!             If MA is real, use FMGAM.

      IF (MA(2)%MP(3) == 0) THEN
          CALL FMGAM(MA(1),MB(1))
          CALL FMI2M(0,MB(2))
          IF (MB(1)%MP(2) == MUNKNO) THEN
              CALL ZMST2M('UNKNOWN+UNKNOWN*i',MB)
          ENDIF
          RETURN
      ENDIF

      IF (.NOT. ALLOCATED(MB(1)%MP)) THEN
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(1)%MP)
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MB(2)%MP)) THEN
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(2)%MP)
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL ZMENTR('ZMGAM    ',MA,MA,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF

      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL ZMEQU(MA,MXY(1:2,1),NDSAVE,NDIG)

!             Check for special cases.

      IF (MA(2)%MP(3) == 0) THEN
          CALL FMINT(MXY(1,1),MXY(1,15))
          IF (FMCOMP(MXY(1,1),'==',MXY(1,15))) THEN
              CALL ZMST2M('UNKNOWN+UNKNOWN*i',MXY(1:2,3))
              KFLAG = -4
              GO TO 120
          ENDIF
      ENDIF
      IF (MXY(1,1)%MP(2) == MEXPUN .OR. MXY(2,1)%MP(2) == MEXPUN) THEN
          CALL ZMST2M('UNKNOWN+UNKNOWN*i',MXY(1:2,3))
          KFLAG = -4
          GO TO 120
      ENDIF
      CALL ZMABS(MXY(1:2,1),MXY(1,2))
      CALL FMIPWR(MXY(1,2),3,MXY(1,3))
      CALL FMI2M(1,MXY(1,4))
      CALL FMULP(MXY(1,4),MXY(1,5))
      IF (FMCOMP(MXY(1,3),'<=',MXY(1,5))) THEN
          CALL FMI2M(0,MXY(2,4))
          CALL ZMDIV(MXY(1:2,4),MXY(1:2,1),MXY(1:2,5))
          CALL FMEULER(MXY(1,4))
          CALL FMSUB_R1(MXY(1,5),MXY(1,4))
          CALL FMSQR_R1(MXY(1,4))
          CALL FMMPYI_R1(MXY(1,4),6)
          CALL FMPI(MXY(1,3))
          CALL FMSQR_R1(MXY(1,3))
          CALL FMADD_R1(MXY(1,4),MXY(1,3))
          CALL FMDIVI_R1(MXY(1,4),12)
          CALL ZMMPY(MXY(1:2,1),MXY(1:2,4),MXY(1:2,3))
          CALL ZMADD_R2(MXY(1:2,5),MXY(1:2,3))
      ELSE
          CALL ZMLNGM(MXY(1:2,1),MXY(1:2,2))
          CALL ZMEXP(MXY(1:2,2),MXY(1:2,3))
      ENDIF

  120 NAMEST(NCALL) = 'ZMGAM'

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL == 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(1,3)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          ERR2 = 0
          DO J = KL, 1, -1
             ERR2 = (ERR2 + MXY(2,3)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR   > 0.498 .AND. ERR  < 0.502) .OR.  &
               (KROUND == 1 .AND. ERR2  > 0.498 .AND. ERR2 < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR  > 0.998 .OR. ERR  < 0.002)) .OR.  &
               (KROUND /= 1 .AND. (ERR2 > 0.998 .OR. ERR2 < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (MXY(1,3)%MP(2) == MUNKNO .OR. MXY(2,3)%MP(2) == MUNKNO) THEN
          CALL ZMST2M('UNKNOWN + UNKNOWN i',MXY(1:2,3))
      ENDIF

      CALL ZMEXIT(MXY(1:2,3),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE ZMGAM

      SUBROUTINE ZMFACT(MA,MB)

!  MB = MA!     Factorial for complex input.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA(2),MB(2)
      REAL (KIND(1.0D0)) :: MXSAVE
      DOUBLE PRECISION :: ERR,ERR2
      INTEGER :: J,KL,KOVUN,KR_RETRY,KRESLT,NDSAVE
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(2,15)
      LOGICAL, EXTERNAL :: FMCOMP

!             If MA is real, use FMFACT.

      IF (MA(2)%MP(3) == 0) THEN
          CALL FMFACT(MA(1),MB(1))
          CALL FMI2M(0,MB(2))
          IF (MB(1)%MP(2) == MUNKNO) THEN
              CALL ZMST2M('UNKNOWN+UNKNOWN*i',MB)
          ENDIF
          RETURN
      ENDIF

      IF (.NOT. ALLOCATED(MB(1)%MP)) THEN
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(1)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(1)%MP)
          ALLOCATE(MB(1)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (.NOT. ALLOCATED(MB(2)%MP)) THEN
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB(2)%MP) < NDIG+2) THEN
          DEALLOCATE(MB(2)%MP)
          ALLOCATE(MB(2)%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      CALL ZMENTR('ZMFACT   ',MA,MA,1,MB,KRESLT,NDSAVE,MXSAVE,KOVUN)
      IF (KRESLT /= 0) THEN
          RETURN
      ENDIF

      KR_RETRY = 0

  110 IF (KR_RETRY >= 1) THEN
          IF (NCALL == 1) NDIG = MAX(NDIG,2*NDSAVE+10)
      ENDIF
      CALL ZMEQU(MA,MXY(1:2,1),NDSAVE,NDIG)

!             Check for special cases.

      IF (MA(2)%MP(3) == 0) THEN
          CALL FMINT(MXY(1,1),MXY(1,15))
          IF (FMCOMP(MXY(1,1),'==',MXY(1,15))) THEN
              CALL ZMST2M('UNKNOWN+UNKNOWN*i',MXY(1:2,3))
              KFLAG = -4
              GO TO 120
          ENDIF
      ENDIF
      IF (MXY(1,1)%MP(2) == MEXPUN .OR. MXY(2,1)%MP(2) == MEXPUN) THEN
          CALL ZMST2M('UNKNOWN+UNKNOWN*i',MXY(1:2,3))
          KFLAG = -4
          GO TO 120
      ENDIF

!             Use  x! = gamma(x+1).

      CALL ZMI2M(1,MXY(1:2,2))
      CALL ZMADD_R2(MXY(1:2,1),MXY(1:2,2))
      CALL ZMGAM(MXY(1:2,2),MXY(1:2,3))

  120 NAMEST(NCALL) = 'ZMFACT'

!             Try again with more guard digits if the current guard digits are too close to 1/2 ulp.

      IF (NCALL == 1) THEN
          KL = MIN(NDIG-NDSAVE,INT(3*DLOGTN/DLOGMB + 1.5))
          ERR = 0
          DO J = KL, 1, -1
             ERR = (ERR + MXY(1,3)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          ERR2 = 0
          DO J = KL, 1, -1
             ERR2 = (ERR2 + MXY(2,3)%MP(J+NDSAVE+2)) / MBASE
          ENDDO
          IF ( (KROUND == 1 .AND. ERR   > 0.498 .AND. ERR  < 0.502) .OR.  &
               (KROUND == 1 .AND. ERR2  > 0.498 .AND. ERR2 < 0.502) .OR.  &
               (KROUND /= 1 .AND. (ERR  > 0.998 .OR. ERR  < 0.002)) .OR.  &
               (KROUND /= 1 .AND. (ERR2 > 0.998 .OR. ERR2 < 0.002)) ) KR_RETRY = KR_RETRY + 1
      ENDIF
      IF (KR_RETRY == 1 .AND. NDIG < 2*NDSAVE+10) THEN
          KR_RETRY = 2
          GO TO 110
      ENDIF
      IF (MXY(1,3)%MP(2) == MUNKNO .OR. MXY(2,3)%MP(2) == MUNKNO) THEN
          CALL ZMST2M('UNKNOWN + UNKNOWN i',MXY(1:2,3))
      ENDIF

      CALL ZMEXIT(MXY(1:2,3),MB,NDSAVE,MXSAVE,KOVUN)
      RETURN
      END SUBROUTINE ZMFACT

      SUBROUTINE ZPERF(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMERF(MPX,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPERF

      SUBROUTINE ZPERFC(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMERFC(MPX,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPERFC

      SUBROUTINE ZPERFC_SC(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMERFC_SC(MPX,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPERFC_SC


      SUBROUTINE ZPLNGM(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMLNGM(MPX,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPLNGM


      SUBROUTINE ZPGAM(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMGAM(MPX,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPGAM


      SUBROUTINE ZPFACT(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA(2),MB(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL ZMUNPK(MA,MPX)
      CALL ZMFACT(MPX,MPY)
      CALL ZMPACK(MPY,MB)
      RETURN
      END SUBROUTINE ZPFACT
