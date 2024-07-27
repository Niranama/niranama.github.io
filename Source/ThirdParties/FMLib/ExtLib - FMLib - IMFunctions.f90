
!  The IM routines perform integer multiple-precision arithmetic.

      SUBROUTINE IMABS(MA,MB)

!  MB = ABS(MA)

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB

      INTEGER :: KWRNSV,NDSAVE,RESULT_SIZE
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB

      RESULT_SIZE = MAX(5,INT(MA%MP(2)+3))
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 5
      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NCALL = NCALL + 1
      IF (KDEBUG == 1) CALL IMARGS('IMABS    ',1,MA,MA)
      NDSAVE = NDIG
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'IMABS'
          CALL IMNTR(2,MA,MA,1)
      ENDIF

      KFLAG = 0
      KWRNSV = KWARN
      KWARN = 0
      CALL IMEQ(MA,MB)
      MB%MP(1) = 1
      KWARN = KWRNSV

      IF (NTRACE /= 0) CALL IMNTR(1,MB,MB,1)
      NCALL = NCALL - 1
      NDIG = NDSAVE
      RETURN
      END SUBROUTINE IMABS

      FUNCTION IMABS_GREATER_THAN(U, V)

!  Return true if abs(U) > abs(V) for IM numbers U, V.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: J
      TYPE(MULTI) :: U, V
      INTENT (IN) :: U, V
      LOGICAL :: IMABS_GREATER_THAN

      IF (U%MP(2) > V%MP(2)) THEN
          IMABS_GREATER_THAN = .TRUE.
          RETURN
      ELSE IF (U%MP(2) < V%MP(2)) THEN
          IMABS_GREATER_THAN = .FALSE.
          RETURN
      ENDIF

      IMABS_GREATER_THAN = .FALSE.
      DO J = 1, INT(U%MP(2))
         IF (U%MP(J+2) > V%MP(J+2)) THEN
             IMABS_GREATER_THAN = .TRUE.
             RETURN
         ELSE IF(U%MP(J+2) < V%MP(J+2)) THEN
             IMABS_GREATER_THAN = .FALSE.
             RETURN
         ENDIF
      ENDDO

      END FUNCTION IMABS_GREATER_THAN

      SUBROUTINE IMADD(MA,MB,MC)

!  MC = MA + MB

!  This routine performs the trace printing.  IMADD2 is used to do the arithmetic.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC

      NCALL = NCALL + 1
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'IMADD'
          CALL IMNTR(2,MA,MB,2)

          CALL IMADD2(MA,MB,MC)

          CALL IMNTR(1,MC,MC,1)
      ELSE
          CALL IMADD2(MA,MB,MC)
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE IMADD

      SUBROUTINE IMADD2(MA,MB,MC)

!  MC = MA + MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC

      REAL (KIND(1.0D0)) :: MDA,MDAB,MDB
      INTEGER :: NDSAVE,RESULT_SIZE
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC

      RESULT_SIZE = MAX(MA%MP(2),MB%MP(2)) + 4
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 5
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (KDEBUG == 1) CALL IMARGS('IMADD    ',2,MA,MB)
      NDSAVE = NDIG
      KFLAG = 0

      IF (MA%MP(2) <= 2) THEN
          IF (MB%MP(2) > 2 .OR. MA%MP(2) < 0 .OR. MB%MP(2) < 0) GO TO 110
          IF (MA%MP(2) <= 1) THEN
              MDA = MA%MP(1) * MA%MP(3)
          ELSE
              MDA = MA%MP(1) * (MA%MP(3)*MBASE + MA%MP(4))
          ENDIF
          IF (MB%MP(2) <= 1) THEN
              MDB = MB%MP(1) * MB%MP(3)
          ELSE
              MDB = MB%MP(1) * (MB%MP(3)*MBASE + MB%MP(4))
          ENDIF
          MDAB = MDA + MDB
          IF (ABS(MDAB) < MBASE) THEN
              MC%MP(2) = 1
              IF (MDAB == 0) MC%MP(2) = 0
              IF (MDAB < 0) THEN
                  MC%MP(3) = -MDAB
                  MC%MP(1) = -1
              ELSE
                  MC%MP(3) = MDAB
                  MC%MP(1) = 1
              ENDIF
              MC%MP(4) = 0
              IF (MDA == 0 .OR. MDB == 0) KFLAG = 1
              GO TO 120
          ELSE IF (ABS(MDAB) < MBASE*MBASE) THEN
              MC%MP(2) = 2
              IF (MDAB < 0) THEN
                  MC%MP(3) = AINT (-MDAB/MBASE)
                  MC%MP(4) = ABS(-MDAB - MBASE*MC%MP(3))
                  MC%MP(1) = -1
              ELSE
                  MC%MP(3) = AINT (MDAB/MBASE)
                  MC%MP(4) = ABS(MDAB - MBASE*MC%MP(3))
                  MC%MP(1) = 1
              ENDIF
              IF (MDA == 0 .OR. MDB == 0) KFLAG = 1
              GO TO 120
          ENDIF
      ENDIF

!             Check for special cases.

  110 IF (MA%MP(2) < 0 .OR. MB%MP(2) < 0 .OR.  &
          MA%MP(2) >= MEXPOV .OR. MB%MP(2) >= MEXPOV) THEN
          IF (MA%MP(2) == MUNKNO .OR. MB%MP(2) == MUNKNO) THEN
              CALL IMI2M2(0,MC)
              MC%MP(2) = MUNKNO
              MC%MP(3) = 1
              KFLAG = -4
              GO TO 120
          ENDIF
          IF (MA%MP(2) == MEXPOV) THEN
              IF (MA%MP(1) == MB%MP(1) .OR. MB%MP(3) == 0) THEN
                  MC%MP(1) = MA%MP(1)
                  MC%MP(2) = MA%MP(2)
                  MC%MP(3) = MA%MP(3)
                  MC%MP(4) = MA%MP(4)
                  KFLAG = -5
                  GO TO 120
              ELSE
                  KFLAG = -4
                  NAMEST(NCALL) = 'IMADD'
                  CALL FMWARN
                  CALL IMI2M2(0,MC)
                  MC%MP(2) = MUNKNO
                  MC%MP(3) = 1
                  GO TO 120
              ENDIF
          ENDIF
          IF (MB%MP(2) == MEXPOV) THEN
              IF (MB%MP(1) == MA%MP(1) .OR. MA%MP(3) == 0) THEN
                  MC%MP(1) = MB%MP(1)
                  MC%MP(2) = MB%MP(2)
                  MC%MP(3) = MB%MP(3)
                  MC%MP(4) = MB%MP(4)
                  KFLAG = -5
                  GO TO 120
              ELSE
                  KFLAG = -4
                  NAMEST(NCALL) = 'IMADD'
                  CALL FMWARN
                  CALL IMI2M2(0,MC)
                  MC%MP(2) = MUNKNO
                  MC%MP(3) = 1
                  GO TO 120
              ENDIF
          ENDIF
          KFLAG = -4
          NAMEST(NCALL) = 'IMADD'
          CALL FMWARN
          CALL IMI2M2(0,MC)
          MC%MP(2) = MUNKNO
          MC%MP(3) = 1
          GO TO 120
      ENDIF

      CALL IMADD3(MA,MB,MC)

  120 IF (MC%MP(2) <= 1) MC%MP(4) = 0
      NDIG = NDSAVE
      RETURN
      END SUBROUTINE IMADD2

      SUBROUTINE IMADD3(MA,MB,MC)

!  Internal addition routine.  MC = MA + MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC

      REAL (KIND(1.0D0)) :: MAS,MBS
      INTEGER :: J,JCOMP,JSIGN,N1
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      IF (MBLOGS /= MBASE) CALL FMCONS
      IF (MA%MP(3) == 0) THEN
          CALL IMEQ(MB,MC)
          KFLAG = 1
          IF (KSUB == 1) THEN
              IF (MC%MP(2) /= MUNKNO .AND. MC%MP(3) /= 0)  &
                  MC%MP(1) = -MC%MP(1)
              KFLAG = 0
          ENDIF
          RETURN
      ENDIF
      IF (MB%MP(3) == 0) THEN
          CALL IMEQ(MA,MC)
          KFLAG = 1
          RETURN
      ENDIF

      KFLAG = 0
      N1 = MAX(MA%MP(2),MB%MP(2)) + 1

!             JSIGN is the sign of the result of MA + MB.

      JSIGN = 1
      MAS = MA%MP(1)
      MBS = MB%MP(1)
      IF (KSUB == 1) MBS = -MBS

!             See which one is larger in absolute value.

      JCOMP = 2
      IF (MA%MP(2) > MB%MP(2)) THEN
          JCOMP = 1
      ELSE IF (MB%MP(2) > MA%MP(2)) THEN
          JCOMP = 3
      ELSE
          DO J = 2, N1
             IF (MA%MP(J+1) > MB%MP(J+1)) THEN
                 JCOMP = 1
                 EXIT
             ENDIF
             IF (MB%MP(J+1) > MA%MP(J+1)) THEN
                 JCOMP = 3
                 EXIT
             ENDIF
          ENDDO
      ENDIF

      IF (JCOMP < 3) THEN
          IF (MAS < 0) JSIGN = -1
          IF (MAS*MBS > 0) THEN
              CALL IMADDP(MA,MB)
          ELSE
              CALL IMADDN(MA,MB)
          ENDIF
      ELSE
          IF (MBS < 0) JSIGN = -1
          IF (MAS*MBS > 0) THEN
              CALL IMADDP(MB,MA)
          ELSE
              CALL IMADDN(MB,MA)
          ENDIF
      ENDIF

!             Transfer to MC and fix the sign of the result.

      NDIG = MWA%MP(2)
      IF (NDIG < 2) NDIG = 2
      CALL FMMOVE(MWA,MC)
      MC%MP(1) = 1
      IF (JSIGN < 0 .AND. MC%MP(3) /= 0) MC%MP(1) = -1

      IF (KFLAG < 0) THEN
          IF (KSUB == 1) THEN
              NAMEST(NCALL) = 'IMSUB'
          ELSE
              NAMEST(NCALL) = 'IMADD'
          ENDIF
          CALL FMWARN
      ENDIF

      RETURN
      END SUBROUTINE IMADD3

      SUBROUTINE IMADDN(MA,MB)

!  Internal addition routine.  MWA = MA - MB
!  The arguments are such that MA >= MB >= 0.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MK
      INTEGER :: J,K,KL,KP1,KP2,KPT,KSH,N1,RESULT_SIZE
      INTENT (IN) :: MA,MB

      RESULT_SIZE = 2*MA%MP(2) + 30
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 32
      RESULT_SIZE = MAX(32,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (MA%MP(2) == MEXPOV .OR. MB%MP(2) == MEXPOV) THEN
          KFLAG = -4
          MWA%MP(2) = MUNKNO
          MWA%MP(3) = 1
          MWA%MP(4) = 0
          RETURN
      ENDIF

      N1 = MA%MP(2) + 1
      MK = MA%MP(2) - MB%MP(2)
      K = INT(MK)

!             Subtract MB from MA.

      KP1 = MIN(N1,K+1)
      DO J = 1, KP1
         MWA%MP(J+1) = MA%MP(J+1)
      ENDDO
      KP2 = K + 2

!             (Inner Loop)

      DO J = KP2+1, N1+1
         MWA%MP(J) = MA%MP(J) - MB%MP(J-K)
      ENDDO
      MWA%MP(1+N1+1) = 0

!             Normalize.  Fix the sign of any negative digit.

      IF (K > 0) THEN
          DO J = N1, KP2, -1
             IF (MWA%MP(J+1) < 0) THEN
                 MWA%MP(J+1) = MWA%MP(J+1) + MBASE
                 MWA%MP(J) = MWA%MP(J) - 1
             ENDIF
          ENDDO
          KPT = KP2 - 1
  110     IF (MWA%MP(KPT+1) < 0 .AND. KPT >= 3) THEN
              MWA%MP(KPT+1) = MWA%MP(KPT+1) + MBASE
              MWA%MP(KPT) = MWA%MP(KPT) - 1
              KPT = KPT - 1
              GO TO 110
          ENDIF
      ELSE
          DO J = N1, 3, -1
             IF (MWA%MP(J+1) < 0) THEN
                 MWA%MP(J+1) = MWA%MP(J+1) + MBASE
                 MWA%MP(J) = MWA%MP(J) - 1
             ENDIF
          ENDDO
      ENDIF

!             Shift left if there are any leading zeros in the mantissa.

      DO J = 2, N1
         IF (MWA%MP(J+1) > 0) THEN
             KSH = J - 2
             GO TO 120
         ENDIF
      ENDDO
      MWA%MP(2) = 0
      MWA%MP(4) = 0
      RETURN

  120 IF (KSH > 0) THEN
          KL = N1 - KSH
          DO J = 2, KL
             MWA%MP(J+1) = MWA%MP(J+KSH+1)
          ENDDO
          DO J = KL+1, N1
             MWA%MP(J+1) = 0
          ENDDO
          MWA%MP(2) = MWA%MP(2) - KSH
      ENDIF

      RETURN
      END SUBROUTINE IMADDN

      SUBROUTINE IMADDP(MA,MB)

!  Internal addition routine.  MWA = MA + MB
!  The arguments are such that MA >= MB >= 0.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MK
      INTEGER :: J,K,KP2,KPT,N1,RESULT_SIZE
      INTENT (IN) :: MA,MB

      RESULT_SIZE = 2*(MA%MP(2)+1) + 30
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 32
      RESULT_SIZE = MAX(32,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      N1 = MA%MP(2) + 1
      MK = MA%MP(2) - MB%MP(2)
      K = INT(MK)

!             Add MA and MB.

      MWA%MP(2) = MA%MP(2) + 1
      MWA%MP(3) = 0
      DO J = 2, K+1
         MWA%MP(J+2) = MA%MP(J+1)
      ENDDO
      KP2 = K + 2

!             (Inner Loop)

      DO J = KP2+1, N1+1
         MWA%MP(J+1) = MA%MP(J) + MB%MP(J-K)
      ENDDO
      MWA%MP(N1+3) = 0

!             Normalize.  Fix any digit not less than MBASE.

      IF (K > 0) THEN
          DO J = N1+1, KP2, -1
             IF (MWA%MP(J+1) >= MBASE) THEN
                 MWA%MP(J+1) = MWA%MP(J+1) - MBASE
                 MWA%MP(J) = MWA%MP(J) + 1
             ENDIF
          ENDDO
          KPT = KP2 - 1
  110     IF (MWA%MP(KPT+1) >= MBASE .AND. KPT >= 3) THEN
              MWA%MP(KPT+1) = MWA%MP(KPT+1) - MBASE
              MWA%MP(KPT) = MWA%MP(KPT) + 1
              KPT = KPT - 1
              GO TO 110
          ENDIF
      ELSE
          DO J = N1+1, 3, -1
             IF (MWA%MP(J+1) >= MBASE) THEN
                 MWA%MP(J+1) = MWA%MP(J+1) - MBASE
                 MWA%MP(J) = MWA%MP(J) + 1
             ENDIF
          ENDDO
      ENDIF

      RETURN
      END SUBROUTINE IMADDP

      SUBROUTINE IMARGS(KROUTN,NARGS,MA,MB)

!  Check the input arguments to a routine for special cases.

!  KROUTN - Name of the subroutine that was called
!  NARGS  - The number of input arguments (1 or 2)
!  MA     - First input argument
!  MB     - Second input argument (if NARGS is 2)

      USE ModLib_FMVALS
      IMPLICIT NONE

      CHARACTER(9) :: KROUTN
      TYPE(MULTI) :: MA,MB
      INTEGER :: NARGS

      REAL (KIND(1.0D0)) :: MBS
      INTEGER :: J,KWRNSV,LAST
      INTENT (IN) :: KROUTN,NARGS,MA,MB

      KFLAG = -4
      IF (MA%MP(2) == MUNKNO) THEN
          RETURN
      ENDIF
      IF (NARGS == 2) THEN
          IF (MB%MP(2) == MUNKNO) THEN
              RETURN
          ENDIF
      ENDIF
      IF (MBLOGS /= MBASE) CALL FMCONS
      KFLAG = 0

!             Check the validity of parameters.

      IF (NCALL > 1 .AND. KDEBUG == 0) THEN
          RETURN
      ENDIF
      NAMEST(NCALL) = KROUTN

!             Check MBASE.

      IF (MBASE < 2 .OR. MBASE > MXBASE) THEN
          KFLAG = -2
          CALL FMWARN
          MBS = MBASE
          IF (MBASE < 2) MBASE = 2
          IF (MBASE > MXBASE) MBASE = MXBASE
          WRITE (KW,                                                       &
                 "(' MBASE was',I10,'.  It has been changed to',I10,'.')"  &
                ) INT(MBS),INT(MBASE)
          CALL FMCONS
          RETURN
      ENDIF

!             Check exponent range.

      IF (MA%MP(2) < 0) THEN
          IF (ABS(MA%MP(2)) /= MEXPOV .OR. ABS(MA%MP(3)) /= 1) THEN
              KFLAG = -3
              CALL FMWARN
              RETURN
          ENDIF
      ENDIF
      IF (NARGS == 2) THEN
          IF (MB%MP(2) < 0) THEN
              IF (ABS(MB%MP(2)) /= MEXPOV .OR. ABS(MB%MP(3)) /= 1) THEN
                  KFLAG = -3
                  CALL FMWARN
                  RETURN
              ENDIF
          ENDIF
      ENDIF

!             Check for properly normalized digits in the input arguments.

      IF (ABS(MA%MP(2)-INT(MA%MP(2))) /= 0) KFLAG = 1
      IF (MA%MP(3) <= (-1) .OR. MA%MP(3) >= MBASE .OR.  &
          ABS(MA%MP(3)-INT(MA%MP(3))) /= 0) KFLAG = 2
      IF (KDEBUG == 0) GO TO 110
      LAST = INT(MA%MP(2)) + 1
      IF (MA%MP(2) > SIZE(MA%MP)-2) LAST = 3
      DO J = 3, LAST
         IF (MA%MP(J+1) < 0 .OR. MA%MP(J+1) >= MBASE .OR.  &
             ABS(MA%MP(J+1)-INT(MA%MP(J+1))) /= 0) THEN
             KFLAG = J
             GO TO 110
         ENDIF
      ENDDO
  110 IF (KFLAG /= 0) THEN
          J = KFLAG
          KFLAG = -4
          KWRNSV = KWARN
          IF (KWARN >= 2) KWARN = 1
          CALL FMWARN
          KWARN = KWRNSV
          IF (KWARN >= 1) THEN
              WRITE (KW,*) ' First invalid array element:  MA(',J,') = ',MA%MP(J+1)
          ENDIF
          IF (KWARN >= 2) THEN
              STOP
          ENDIF
          RETURN
      ENDIF
      IF (NARGS == 2) THEN
          IF (ABS(MB%MP(2)-INT(MB%MP(2))) /= 0) KFLAG = 1
          IF (MB%MP(3) <= (-1) .OR. MB%MP(3) >= MBASE .OR.  &
              ABS(MB%MP(3)-INT(MB%MP(3))) /= 0) KFLAG = 2
          IF (KDEBUG == 0) GO TO 120
          LAST = INT(MB%MP(2)) + 1
          IF (MB%MP(2) > SIZE(MB%MP)-2) LAST = 3
          DO J = 3, LAST
             IF (MB%MP(J+1) < 0 .OR. MB%MP(J+1) >= MBASE .OR.  &
                 ABS(MB%MP(J+1)-INT(MB%MP(J+1))) /= 0) THEN
                 KFLAG = J
                 GO TO 120
             ENDIF
          ENDDO
  120     IF (KFLAG /= 0) THEN
              J = KFLAG
              KFLAG = -4
              KWRNSV = KWARN
              IF (KWARN >= 2) KWARN = 1
              CALL FMWARN
              KWARN = KWRNSV
              IF (KWARN >= 1) THEN
                  WRITE (KW,*) ' First invalid array element:  MB(',J,') = ',MB%MP(J+1)
              ENDIF
              IF (KWARN >= 2) THEN
                  STOP
              ENDIF
              RETURN
          ENDIF
      ENDIF
      RETURN
      END SUBROUTINE IMARGS

      SUBROUTINE IMBIG(MA)

!  MA = A very large IM integer.

!  Before version 1.3 of FM, this routine returned the largest representable IM integer.
!  Starting with version 1.3 the size of IM integers is limited only by the space available
!  in the working array MWK in FMSAVE.f95.
!  But if this routine set MA as large as possible, it would leave no room for other
!  FM/IM/ZM numbers.  So a result of 10**(10**6) is returned here.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTEGER :: L,RESULT_SIZE
      INTENT (INOUT) :: MA
      TYPE(MULTI) :: MXY(2)

      L = 1.05D+6*LOG(10.0D0)/LOG(DBLE(MBASE)) + 3
      RESULT_SIZE = L + 3
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MA%MP)) THEN
          ALLOCATE(MA%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MA%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MA%MP)
          ALLOCATE(MA%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      NCALL = NCALL + 1
      NAMEST(NCALL) = 'IMBIG'

      IF (MBLOGS /= MBASE) CALL FMCONS
      KFLAG = 0
      CALL IMI2M(10,MXY(1))
      CALL IMI2M(10**6,MXY(2))
      CALL IMPWR(MXY(1),MXY(2),MA)

      IF (NTRACE /= 0 .AND. NCALL <= LVLTRC) THEN
          WRITE (KW,                                               &
                 "(' ',A,12X,'Call level =',I2,5X,'MBASE =',I10)"  &
                ) 'IMBIG',NCALL,INT(MBASE)
          WRITE (KW,*) '       1.0M+1000000'
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE IMBIG

      SUBROUTINE IMCOMB(MA,MB,MC)

!  Binomial coefficients for integers.  MA, MB, MC, are all multi-precision integers.

!  MC = MA choose MB = MA! / ( MB! * (MA-MB)! )

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
      TYPE(MULTI) :: MXY(3)
      INTEGER :: K,KMOD2,L,N
      TYPE(MULTI) :: MT
      INTEGER :: J
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC

      NCALL = NCALL + 1
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'IMCOMB'
          CALL IMNTR(2,MA,MB,2)
      ENDIF

      CALL IMM2I(MA,N)
      IF (KFLAG == 0) THEN
          CALL IMM2I(MB,K)
          IF (KFLAG == 0) THEN
              CALL IMCOMBI(N,K,MT)
              CALL IMEQ(MT,MC)
              IF (NTRACE /= 0) CALL IMNTR(1,MC,MC,1)
              NCALL = NCALL - 1
              RETURN
          ENDIF
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS

      IF (MA%MP(1) < 0) THEN
          CALL IMSUB(MB,MA,MXY(1))
          CALL IMI2M(1,MXY(2))
          CALL IMSUB(MXY(1),MXY(2),MXY(3))
      ELSE
          CALL IMEQ(MA,MXY(3))
      ENDIF
      CALL IMSUB(MXY(3),MB,MXY(1))
      CALL IMMIN(MB,MXY(1),MXY(2))
      IF (MXY(2)%MP(1) < 0) THEN
          CALL IMI2M(0,MT)
          GO TO 110
      ELSE IF (MXY(2)%MP(3) == 0) THEN
          CALL IMI2M(1,MT)
          GO TO 110
      ENDIF
      CALL IMM2I(MXY(2),L)

!             If L = min(k,n-k) is too big to represent as a machine integer, return unknown,
!             since the binomial result mc would have over half a billion digits.

      IF (KFLAG /= 0) THEN
          CALL IMST2M('UNKNOWN',MT)
          GO TO 110
      ENDIF
      IF (L == 1) THEN
          CALL IMEQ(MXY(3),MT)
          GO TO 110
      ENDIF
      IF (L > 10) THEN
          CALL IMCOMB2(MXY(3),MXY(2),MT)
          GO TO 110
      ENDIF

!             Compute the binomial coefficient.

      CALL IMEQ(MXY(3),MT)
      CALL IMI2M(1,MXY(1))
      DO J = 2, L
         CALL IMSUB(MXY(3),MXY(1),MXY(2))
         CALL IMEQ(MXY(2),MXY(3))
         CALL IMMPY(MXY(3),MT,MXY(2))
         CALL IMDIVI(MXY(2),J,MT)
      ENDDO

  110 CALL IMMODI(MB,2,KMOD2)
      IF (MA%MP(1) < 0 .AND. KMOD2 == 1 .AND.  &
          MT%MP(2) /= MUNKNO .AND. MT%MP(3) /= 0) THEN
          MT%MP(1) = -MT%MP(1)
      ENDIF
      CALL IMEQ(MT,MC)
      IF (NTRACE /= 0) CALL IMNTR(1,MC,MC,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE IMCOMB

      SUBROUTINE IMCOMB2(MA,MB,MC)

!  Binomial coefficients for integers.  MA, MB, MC, are all multi-precision integers.

!  MC = MA choose MB = MA! / ( MB! * (MA-MB)! )

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC
      TYPE(MULTI) :: MXY(3)
      INTEGER :: J,J1,J1FAC,KMOD2,KPT,KPT2,L,R
      TYPE(MULTI), ALLOCATABLE :: TOP(:)
      TYPE(MULTI) :: MT
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC

      NCALL = NCALL + 1
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'IMCOMB2'
          CALL IMNTR(2,MA,MB,2)
      ENDIF

      IF (MBLOGS /= MBASE) CALL FMCONS

      IF (MA%MP(1) < 0) THEN
          CALL IMSUB(MB,MA,MXY(1))
          CALL IMI2M(1,MXY(2))
          CALL IMSUB(MXY(1),MXY(2),MXY(3))
      ELSE
          CALL IMEQ(MA,MXY(3))
      ENDIF
      CALL IMSUB(MXY(3),MB,MXY(1))
      CALL IMMIN(MB,MXY(1),MXY(2))
      IF (MXY(2)%MP(1) < 0) THEN
          CALL IMI2M(0,MT)
          GO TO 110
      ELSE IF (MXY(2)%MP(3) == 0) THEN
          CALL IMI2M(1,MT)
          GO TO 110
      ENDIF
      CALL IMM2I(MXY(2),L)

!             If L = min(k,n-k) is too big to represent as a machine integer, return unknown,
!             since the binomial result mc would have over half a billion digits.

      IF (KFLAG /= 0) THEN
          CALL IMST2M('UNKNOWN',MT)
          GO TO 110
      ENDIF
      IF (L == 1) THEN
          CALL IMEQ(MXY(3),MT)
          GO TO 110
      ENDIF

!             Compute the binomial coefficient by making a list of the numerator terms
!             N, N-1, ..., N-L+1.  Then divide out each of the denominator terms 2, 3, ..., L.


      ALLOCATE(TOP(L),STAT=J)
      IF (J /= 0) THEN
          CALL FMDEFINE_ERROR
      ENDIF
      CALL IMI2M(1,MXY(1))
      CALL IMEQ(MXY(3),TOP(1))
      DO J = 2, L
         CALL IMSUB(TOP(J-1),MXY(1),TOP(J))
      ENDDO

      DO J = L, 2, -1
         CALL IMMODI(MXY(3),J,R)
         KPT = R + 1
         DO WHILE (KPT <= L)
            CALL IMMODI(TOP(KPT),J,R)
            IF (R == 0) THEN
                CALL IMDIVI(TOP(KPT),J,MXY(2))
                CALL IMEQ(MXY(2),TOP(KPT))
                EXIT
            ENDIF
            KPT = KPT + J
            IF (KPT > L) THEN
                J1 = J
                DO WHILE (J1 > 1)
                   CALL IMCOMB2_FACTOR(J1,J1FAC)
                   CALL IMMODI(MXY(3),J1FAC,R)
                   KPT2 = R + 1
                   DO WHILE (KPT2 <= L)
                      CALL IMMODI(TOP(KPT2),J1FAC,R)
                      IF (R == 0) THEN
                          CALL IMDIVI(TOP(KPT2),J1FAC,MXY(2))
                          CALL IMEQ(MXY(2),TOP(KPT2))
                          EXIT
                      ENDIF
                      KPT2 = KPT2 + J1FAC
                   ENDDO
                ENDDO
            ENDIF
         ENDDO
      ENDDO

!             Form the product of the remaining TOP terms.

      CALL IMEQ(TOP(1),MT)
      DO J = 2, L
         IF (TOP(J)%MP(2) > 1 .OR.  &
            (TOP(J)%MP(2) == 1 .AND. TOP(J)%MP(3) > 1)) THEN
             CALL IMMPY(TOP(J),MT,MXY(2))
             CALL IMEQ(MXY(2),MT)
         ENDIF
      ENDDO

      DEALLOCATE(TOP)

  110 CALL IMI2M(2,MXY(1))
      CALL IMMOD(MB,MXY(1),MXY(2))
      CALL IMM2I(MXY(2),KMOD2)
      IF (MA%MP(1) < 0 .AND. KMOD2 == 1 .AND.  &
          MT%MP(2) /= MUNKNO .AND. MT%MP(3) /= 0) THEN
          MT%MP(1) = -MT%MP(1)
      ENDIF
      CALL IMEQ(MT,MC)
      IF (NTRACE /= 0) CALL IMNTR(1,MC,MC,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE IMCOMB2

      SUBROUTINE IMCOMB2_FACTOR(J1,J1FAC)

!  Return J1FAC as one prime factor of J1, and return J1 with the value J1 / J1FAC

      IMPLICIT NONE
      INTEGER :: J,J1,J1FAC

      IF (MOD(J1,2) == 0) THEN
          J1FAC = 2
          J1 = J1 / J1FAC
          RETURN
      ENDIF

      IF (MOD(J1,3) == 0) THEN
          J1FAC = 3
          J1 = J1 / J1FAC
          RETURN
      ENDIF

      DO J = 5, INT(SQRT(DBLE(J1)))+1, 6
         J1FAC = J
         IF (MOD(J1,J1FAC) == 0) THEN
             J1 = J1 / J1FAC
             RETURN
         ENDIF
         J1FAC = J + 2
         IF (MOD(J1,J1FAC) == 0) THEN
             J1 = J1 / J1FAC
             RETURN
         ENDIF
      ENDDO

      J1FAC = J1
      J1 = 1

      END SUBROUTINE IMCOMB2_FACTOR

      SUBROUTINE IMCOMBI(N,K,MA)

!  Binomial coefficients for integers.
!  N, K, are machine precision integers, MA is a multi-precision integer.

!  MA = N choose K = N! / ( K! * (N-K)! )

!  See the comments in IMCOMB about results for negative N, etc.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: N,K
      TYPE(MULTI) :: MA
      TYPE(MULTI) :: MXY(2)
      INTEGER :: L,N1,NMETHD
      INTEGER :: J,KSTART,KT,LARGE,LARGED,NDIV,NEXTD,NEXTN,NMPY,NTD,NTN,RESULT_SIZE
      DOUBLE PRECISION :: CN,CK,CNK,E,LOGN,LOGK,LOGNK,PI
      INTENT (IN) :: N,K
      INTENT (INOUT) :: MA

      IF (MBLOGS /= MBASE) CALL FMCONS

      N1 = N
      IF (N < 0) N1 = -N + K - 1
      CN = MAX(1,N1)
      CK = MAX(1,K)
      CK = MIN(CK,CN)
      CNK = MAX(1,N1-K)
      CNK = MIN(CNK,CN)
      E = EXP(1.0D0)
      PI = ACOS(-1.0D0)
      LOGN = (LOG( 2*PI*CN )/2) + CN*LOG( CN/E ) + LOG( 1 + 1/(12.0D0*CN) )
      LOGK = (LOG( 2*PI*CK )/2) + CK*LOG( CK/E ) + LOG( 1 + 1/(12.0D0*CK) )
      LOGNK = (LOG( 2*PI*CNK )/2) + CNK*LOG( CNK/E ) + LOG( 1 + 1/(12.0D0*CNK) )
      L = MIN(K,N1-K)
      IF (LOGN < 1.0D+10) THEN
          RESULT_SIZE = (LOGN - LOGK - LOGNK) / DLOGMB + 7
      ELSE
          RESULT_SIZE = (L*LOG(CN) - MIN(LOGK,LOGNK)) / DLOGMB + 7
      ENDIF
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 5
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MA%MP)) THEN
          ALLOCATE(MA%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MA%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MA%MP)
          ALLOCATE(MA%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (L < 0) THEN
          CALL IMI2M(0,MA)
          GO TO 110
      ELSE IF (L == 0) THEN
          CALL IMI2M(1,MA)
          GO TO 110
      ELSE IF (L == 1) THEN
          CALL IMI2M(N1,MA)
          GO TO 110
      ENDIF

!             Determine which method to use.

      NMETHD = 2
      IF (N1 >= 2100) THEN
          IF (N1 >= 10**7) THEN
              IF (L >= 8) NMETHD = 1
          ELSE
              IF (L > EXP(10.7D0 - 0.53D0*LOG(CN))) NMETHD = 1
          ENDIF
      ENDIF

      IF (NMETHD == 1) THEN
          CALL IMFACT_P(N1-L+1,N1,MXY(1))
          CALL IMFACT_P(2,L,MXY(2))
          CALL IMDIV(MXY(1),MXY(2),MA)
          GO TO 110
      ENDIF

!             Find the largest value for N1 choose J using integers.

      NTN = N1
      NTD = 1
      LARGE = INT(INTMAX/N1)
      DO J = 2, L
         IF (NTN <= LARGE) THEN
             NTN = (NTN*((N1+1)-J))/J
         ELSE
             CALL IMI2M(NTN,MA)
             NTN = (N1+1) - J
             NTD = J
             EXIT
         ENDIF
      ENDDO

      IF (NTD == 1) THEN
          CALL IMI2M(NTN,MA)
          GO TO 110
      ENDIF

      NEXTN = NTN
      NEXTD = NTD
      KSTART = NTD + 1

!             Compute the rest of N1 choose K.

      LARGED = MIN(LARGE,INT(MXBASE))
      DO KT = KSTART, L
         NEXTN = NEXTN - 1
         NEXTD = NEXTD + 1
         IF (NTN >= LARGE .OR. NTD >= LARGED) THEN
             CALL IMMPYI(MA,NTN,MXY(1))
             CALL IMDIVI(MXY(1),NTD,MA)
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
                 CALL IMMPYI(MA,NTN,MXY(1))
                 CALL IMDIVI(MXY(1),NTD,MA)
                 NTN = NEXTN
                 NTD = NEXTD
             ENDIF
         ENDIF
      ENDDO
      CALL FMGCDI(NTN,NTD)
      CALL IMMPYI(MA,NTN,MXY(1))
      CALL IMDIVI(MXY(1),NTD,MA)

  110 IF (N < 0 .AND. MOD(K,2) == 1 .AND.  &
          MA%MP(2) /= MUNKNO .AND. MA%MP(3) /= 0) THEN
          MA%MP(1) = -MA%MP(1)
      ENDIF
      RETURN
      END SUBROUTINE IMCOMBI

      FUNCTION IMCOMP(MA,LREL,MB)

!  Logical comparison of FM numbers MA and MB.

!  LREL is a CHARACTER description of the comparison to be done:
!  LREL = 'EQ' returns IMCOMP = .TRUE. if MA == MB
!       = 'NE', 'GE', 'GT', 'LE', 'LT' also work like a logical IF.
!       = '==', '/=', '<', '<=', '>', '>=' may be used.

      USE ModLib_FMVALS
      IMPLICIT NONE

      LOGICAL :: IMCOMP
      CHARACTER(*) :: LREL
      CHARACTER(2) :: JREL
      TYPE(MULTI) :: MA,MB

      INTEGER :: J,JCOMP,NDSAVE,NLAST,NTRSAV
      INTENT (IN) :: MA,LREL,MB

      NCALL = NCALL + 1
      IF (KDEBUG == 1) CALL IMARGS('IMCOMP   ',2,MA,MB)
      NAMEST(NCALL) = 'IMCOMP'

      IF (NCALL <= LVLTRC .AND. ABS(NTRACE) >= 2) THEN
          WRITE (KW,"(' Input to IMCOMP')")
          NDSAVE = NDIG
          IF (NTRACE > 0) THEN
              CALL IMPRNT(MA)
              IF (INDEX('=/<>',LREL(1:1)) > 0) THEN
                  WRITE (KW,"(8X,A)") LREL
              ELSE
                  WRITE (KW,"(7X,'.',A,'.')") LREL
              ENDIF
              CALL IMPRNT(MB)
          ELSE
              NDIG = MAX(2,INT(MA%MP(2)))
              NTRSAV = NTRACE
              CALL IMNTRJ(MA,NDIG)
              IF (INDEX('=/<>',LREL(1:1)) > 0) THEN
                  WRITE (KW,"(8X,A)") LREL
              ELSE
                  WRITE (KW,"(7X,'.',A,'.')") LREL
              ENDIF
              NDIG = MAX(2,INT(MB%MP(2)))
              CALL IMNTRJ(MB,NDIG)
              NTRACE = NTRSAV
          ENDIF
          NDIG = NDSAVE
      ENDIF

!             JCOMP will be 1 if MA > MB
!                           2 if MA == MB
!                           3 if MA < MB

!             Check for special cases.

      JREL = LREL
      IF (LREL /= 'EQ' .AND. LREL /= 'NE' .AND. LREL /= 'LT' .AND.  &
          LREL /= 'GT' .AND. LREL /= 'LE' .AND. LREL /= 'GE') THEN
          IF (LREL == 'eq' .OR. LREL == '==') THEN
              JREL = 'EQ'
          ELSE IF (LREL == 'ne' .OR. LREL == '/=') THEN
              JREL = 'NE'
          ELSE IF (LREL == 'lt' .OR. LREL == '<') THEN
              JREL = 'LT'
          ELSE IF (LREL == 'gt' .OR. LREL == '>') THEN
              JREL = 'GT'
          ELSE IF (LREL == 'le' .OR. LREL == '<=') THEN
              JREL = 'LE'
          ELSE IF (LREL == 'ge' .OR. LREL == '>=') THEN
              JREL = 'GE'
          ELSE
              IMCOMP = .FALSE.
              KFLAG = -4
              IF (NCALL /= 1 .OR. KWARN <= 0) GO TO 120
              IF (KWARN <= 0) GO TO 120
              WRITE (KW,                                                     &
                     "(/' Error of type KFLAG = -4 in FM package in',"   //  &
                     "' routine IMCOMP'//1X,A,' is not one of the six'," //  &
                     "' recognized comparisons.'//' .FALSE. has been',"  //  &
                     "' returned.'/)"                                        &
                    ) LREL
              IF (KWARN >= 2) THEN
                  STOP
              ENDIF
              GO TO 120
          ENDIF
      ENDIF

      IF (MA%MP(2) == MUNKNO .OR. MB%MP(2) == MUNKNO) THEN
          IMCOMP = .FALSE.
          KFLAG = -4
          GO TO 120
      ENDIF

      IF (ABS(MA%MP(2)) == MEXPOV .AND. MA%MP(2) == MB%MP(2) .AND.  &
          MA%MP(3) == MB%MP(3) .AND. MA%MP(1) == MB%MP(1)) THEN
          IMCOMP = .FALSE.
          KFLAG = -4
          IF (NCALL /= 1 .OR. KWARN <= 0) GO TO 120
          IF (KWARN <= 0) GO TO 120
          WRITE (KW,                                                     &
                 "(/' Error of type KFLAG = -4 in FM package in ',"  //  &
                 "'routine IMCOMP'//' Two numbers in the same ',"    //  &
                 "'overflow category cannot be compared.'//"         //  &
                 "' .FALSE. has been returned.'/)"                       &
                )
          IF (KWARN >= 2) THEN
              STOP
          ENDIF
          GO TO 120
      ENDIF

!             Check for zero.

      KFLAG = 0
      IF (MA%MP(3) == 0) THEN
          JCOMP = 2
          IF (MB%MP(3) == 0) GO TO 110
          IF (MB%MP(1) < 0) JCOMP = 1
          IF (MB%MP(1) > 0) JCOMP = 3
          GO TO 110
      ENDIF
      IF (MB%MP(3) == 0) THEN
          JCOMP = 1
          IF (MA%MP(1) < 0) JCOMP = 3
          GO TO 110
      ENDIF

!             Check for opposite signs.

      IF (MA%MP(1) > 0 .AND. MB%MP(1) < 0) THEN
          JCOMP = 1
          GO TO 110
      ENDIF
      IF (MB%MP(1) > 0 .AND. MA%MP(1) < 0) THEN
          JCOMP = 3
          GO TO 110
      ENDIF

!             See which one is larger in absolute value.

      IF (MA%MP(2) > MB%MP(2)) THEN
          JCOMP = 1
          GO TO 110
      ENDIF
      IF (MB%MP(2) > MA%MP(2)) THEN
          JCOMP = 3
          GO TO 110
      ENDIF
      NLAST = INT(MA%MP(2)) + 1

      DO J = 2, NLAST
         IF (ABS(MA%MP(J+1)) > ABS(MB%MP(J+1))) THEN
             JCOMP = 1
             GO TO 110
         ENDIF
         IF (ABS(MB%MP(J+1)) > ABS(MA%MP(J+1))) THEN
             JCOMP = 3
             GO TO 110
         ENDIF
      ENDDO

      JCOMP = 2

!             Now match the JCOMP value to the requested comparison.

  110 IF (JCOMP == 1 .AND. MA%MP(1) < 0) THEN
          JCOMP = 3
      ELSE IF (JCOMP == 3 .AND. MB%MP(1) < 0) THEN
          JCOMP = 1
      ENDIF

      IMCOMP = .FALSE.
      IF (JCOMP == 1 .AND. (JREL == 'GT' .OR. JREL == 'GE' .OR. JREL == 'NE')) IMCOMP = .TRUE.
      IF (JCOMP == 2 .AND. (JREL == 'EQ' .OR. JREL == 'GE' .OR. JREL == 'LE')) IMCOMP = .TRUE.
      IF (JCOMP == 3 .AND. (JREL == 'NE' .OR. JREL == 'LT' .OR. JREL == 'LE')) IMCOMP = .TRUE.

  120 CONTINUE
      IF (NTRACE /= 0) THEN
          IF (NCALL <= LVLTRC .AND. ABS(NTRACE) >= 1) THEN
              IF (KFLAG == 0) THEN
                  WRITE (KW,                                         &
                         "(' IMCOMP',15X,'Call level =',I2,5X,"  //  &
                         "'MBASE =',I10)"                            &
                        ) NCALL,INT(MBASE)
              ELSE
                  WRITE (KW,                                        &
                         "(' IMCOMP',6X,'Call level =',I2,4X,"  //  &
                         "'MBASE =',I10,4X,'KFLAG =',I3)"           &
                        ) NCALL,INT(MBASE),KFLAG
              ENDIF
              IF (IMCOMP) THEN
                  WRITE (KW,"(7X,'.TRUE.')")
              ELSE
                  WRITE (KW,"(7X,'.FALSE.')")
              ENDIF
          ENDIF
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END FUNCTION IMCOMP

      SUBROUTINE IMDIM(MA,MB,MC)

!  MC = DIM(MA,MB)

!  Positive difference.  MC = MA - MB  if MA >= MB,
!                           = 0        otherwise.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC

      INTEGER :: KOVFL,RESULT_SIZE
      LOGICAL, EXTERNAL :: IMCOMP
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC

      RESULT_SIZE = MAX(MA%MP(2),MB%MP(2)) + 3
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 5
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      KFLAG = 0
      NCALL = NCALL + 1
      IF (KDEBUG == 1) CALL IMARGS('IMDIM    ',2,MA,MB)
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'IMDIM'
          CALL IMNTR(2,MA,MB,2)
      ENDIF

      IF (MA%MP(2) == MUNKNO .OR. MB%MP(2) == MUNKNO) THEN
          CALL IMST2M('UNKNOWN',MC)
          KFLAG = -4
          GO TO 110
      ENDIF
      IF (MA%MP(2) < 0 .OR. MB%MP(2) < 0) THEN
          KFLAG = -4
          NAMEST(NCALL) = 'IMDIM'
          CALL FMWARN
          CALL IMST2M('UNKNOWN',MC)
          GO TO 110
      ENDIF
      KOVFL = 0
      IF (MA%MP(2) == MEXPOV .OR. MB%MP(2) == MEXPOV) THEN
          KOVFL = 1
          IF (MA%MP(2) == MEXPOV .AND. MB%MP(2) == MEXPOV .AND.  &
              MA%MP(3) == MB%MP(3) .AND. MA%MP(1) == MB%MP(1)) THEN
              KFLAG = -4
              NAMEST(NCALL) = 'IMDIM'
              CALL FMWARN
              CALL IMST2M('UNKNOWN',MC)
              GO TO 110
          ENDIF
      ENDIF

      IF (IMCOMP(MA,'>=',MB)) THEN
          CALL IMSUB(MA,MB,MC)
          IF (KFLAG == 1) KFLAG = 0
      ELSE
          MC%MP(2) = 0
          MC%MP(3) = 0
          MC%MP(4) = 0
          MC%MP(1) = 1
      ENDIF

      IF (MC%MP(2) >= MEXPOV) THEN
          IF (MC%MP(2) == MUNKNO) THEN
              KFLAG = -4
              NAMEST(NCALL) = 'IMDIM'
              CALL FMWARN
          ELSE IF (NCALL == 1 .OR. MC%MP(2) >= MEXPOV) THEN
              IF (MC%MP(1) > 0) THEN
                  CALL IMST2M('OVERFLOW',MC)
              ELSE
                  CALL IMST2M('-OVERFLOW',MC)
              ENDIF
              KFLAG = -5
              NAMEST(NCALL) = 'IMDIM'
              IF (KOVFL /= 1) CALL FMWARN
          ENDIF
      ENDIF

  110 IF (MC%MP(2) <= 1) MC%MP(4) = 0
      IF (NTRACE /= 0) CALL IMNTR(1,MC,MC,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE IMDIM

      SUBROUTINE IMDIV(MA,MB,MC)

!  MC = INT(MA/MB)

!  Use IMDIVR if both INT(MA/MB) and MOD(MA,MB) are needed.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC
      INTEGER :: NDSAVE,RESULT_SIZE
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      TYPE(MULTI) :: MXY(2)

      IF (MA%MP(2) >= MEXPOV .OR. MB%MP(2) >= MEXPOV) THEN
          RESULT_SIZE = 5
      ELSE
          RESULT_SIZE = MA%MP(2) - MB%MP(2) + 6
      ENDIF
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NCALL = NCALL + 1
      IF (KDEBUG == 1) CALL IMARGS('IMDIV    ',2,MA,MB)
      KFLAG = 0
      NDSAVE = NDIG
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'IMDIV'
          CALL IMNTR(2,MA,MB,2)
      ENDIF

      IF (MA%MP(2) == MUNKNO .OR. MB%MP(2) == MUNKNO) THEN
          CALL IMST2M('UNKNOWN',MC)
          KFLAG = -4
          GO TO 110
      ENDIF

      KREM = 0
      CALL IMDIVR(MA,MB,MC,MXY(1))
      KREM = 1

      IF (MC%MP(2) == MUNKNO) THEN
          KFLAG = -4
          NAMEST(NCALL) = 'IMDIV'
          CALL FMWARN
      ENDIF

  110 IF (MC%MP(2) <= 1) MC%MP(4) = 0
      IF (NTRACE /= 0) CALL IMNTR(1,MC,MC,1)
      NCALL = NCALL - 1
      NDIG = NDSAVE
      RETURN
      END SUBROUTINE IMDIV

      SUBROUTINE IMDIVI(MA,IDIV,MB)

!  MB = INT(MA/IDIV)

!  Use IMDVIR if both INT(MA/IDIV) and MOD(MA,IDIV) are needed.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTEGER :: IDIV,IREM,NDSAVE,RESULT_SIZE
      INTENT (IN) :: MA,IDIV
      INTENT (INOUT) :: MB

      RESULT_SIZE = MAX(5,INT(MA%MP(2)+3))
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 5
      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NCALL = NCALL + 1
      IF (KDEBUG == 1) CALL IMARGS('IMDIVI   ',1,MA,MA)
      KFLAG = 0
      NDSAVE = NDIG
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'IMDIVI'
          CALL IMNTR(2,MA,MA,1)
          CALL IMNTRI(2,IDIV,0)
      ENDIF

      IF (MA%MP(2) == MUNKNO) THEN
          CALL IMST2M('UNKNOWN',MB)
          KFLAG = -4
          GO TO 110
      ENDIF

      CALL IMDVIR(MA,IDIV,MB,IREM)

      IF (MB%MP(2) == MUNKNO) THEN
          KFLAG = -4
          NAMEST(NCALL) = 'IMDIVI'
          CALL FMWARN
      ENDIF

  110 IF (MB%MP(2) <= 1) MB%MP(4) = 0
      IF (NTRACE /= 0) CALL IMNTR(1,MB,MB,1)
      NCALL = NCALL - 1
      NDIG = NDSAVE
      RETURN
      END SUBROUTINE IMDIVI

      SUBROUTINE IMDIVR(MA,MB,MC,MD)

!  MC = INT(MA / MB),    MD = Remainder from the division.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC,MD
      REAL (KIND(1.0D0)) :: MDA,MDAB,MDB,MDR
      DOUBLE PRECISION :: XB,XBR,XBASE,XMWA
      REAL (KIND(1.0D0)) :: MAS,MAXMWA,MB1,MBP1,MBS,MCARRY,MKT,MLMAX,MQD
      INTEGER :: J,JB,JL,K,KA,KB,KL,KLTFLG,KPTMWA,LCRRCT,NA1,NB1,ND,NDSAVE,NGUARD,NL,  &
                 NMBWDS,NMETHD,NTRSAV,RESULT_SIZE
      LOGICAL, EXTERNAL :: IMCOMP
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC,MD
      TYPE(MULTI) :: MXY(4)

      IF (MA%MP(2) >= MEXPOV .OR. MB%MP(2) >= MEXPOV) THEN
          RESULT_SIZE = 5
      ELSE
          RESULT_SIZE = MA%MP(2) - MB%MP(2) + 6
      ENDIF
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      RESULT_SIZE = MAX(5,INT(MB%MP(2)+3)) + 2
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = MA%MP(2) + 5
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 5
      IF (.NOT. ALLOCATED(MD%MP)) THEN
          ALLOCATE(MD%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MD%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MD%MP)
          ALLOCATE(MD%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      RESULT_SIZE = 2*MA%MP(2) + 30
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 32
      RESULT_SIZE = MAX(32,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NCALL = NCALL + 1
      IF (KDEBUG == 1) CALL IMARGS('IMDIVR   ',2,MA,MB)
      NDSAVE = NDIG
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'IMDIVR'
          CALL IMNTR(2,MA,MB,2)
      ENDIF
      KFLAG = 0
      NTRSAV = NTRACE
      NTRACE = 0
      IF (MBLOGS /= MBASE) CALL FMCONS

!             Check for special cases.

      IF (MB%MP(2) == 1 .AND. MA%MP(2) /= MUNKNO) THEN
          IF (MB%MP(1)*MB%MP(3) == 1) THEN
              CALL IMEQ(MA,MC)
              MD%MP(2) = 0
              MD%MP(3) = 0
              MD%MP(4) = 0
              MD%MP(1) = 1
              GO TO 170
          ELSE IF (MB%MP(1)*MB%MP(3) == -1) THEN
              CALL IMEQ(MA,MC)
              IF (MC%MP(2) /= MUNKNO .AND. MC%MP(3) /= 0)  &
                  MC%MP(1) = -MC%MP(1)
              MD%MP(2) = 0
              MD%MP(3) = 0
              MD%MP(4) = 0
              MD%MP(1) = 1
              GO TO 170
          ENDIF
      ENDIF
      IF (MA%MP(2) < MB%MP(2) .AND. MB%MP(2) /= MUNKNO) GO TO 110
      IF (MA%MP(2) < 0 .OR. MB%MP(2) < 0 .OR. MB%MP(3) == 0 .OR.  &
          MA%MP(2) >= MEXPOV .OR. MB%MP(2) >= MEXPOV) THEN
          KFLAG = -4
          IF (MA%MP(2) /= MUNKNO .AND. MB%MP(2) /= MUNKNO) THEN
              NAMEST(NCALL) = 'IMDIVR'
              CALL FMWARN
          ENDIF
          CALL IMST2M('UNKNOWN',MC)
          CALL IMST2M('UNKNOWN',MD)
          GO TO 170
      ENDIF
      IF (MA%MP(2) <= 2) THEN
          IF (MB%MP(2) > 2) GO TO 110
          IF (MB%MP(3) == 0) GO TO 110
          IF (MA%MP(2) <= 1) THEN
              MDA = MA%MP(1) * MA%MP(3)
          ELSE
              MDA = MA%MP(1) * (MA%MP(3)*MBASE + MA%MP(4))
          ENDIF
          IF (MB%MP(2) <= 1) THEN
              MDB = MB%MP(1) * MB%MP(3)
          ELSE
              MDB = MB%MP(1) * (MB%MP(3)*MBASE + MB%MP(4))
          ENDIF
          MDAB = AINT (MDA / MDB)
          MDR = MDA - MDAB*MDB
          IF (ABS(MDAB) < MBASE) THEN
              MC%MP(2) = 1
              IF (MDAB == 0) MC%MP(2) = 0
              IF (MDAB >= 0) THEN
                  MC%MP(3) = MDAB
                  MC%MP(1) = 1
              ELSE
                  MC%MP(3) = -MDAB
                  MC%MP(1) = -1
              ENDIF
              MC%MP(4) = 0
          ELSE IF (ABS(MDAB) < MBASE*MBASE) THEN
              MC%MP(2) = 2
              IF (MDAB >= 0) THEN
                  MC%MP(3) = AINT (MDAB/MBASE)
                  MC%MP(4) = ABS(MDAB - MBASE*MC%MP(3))
                  MC%MP(1) = 1
              ELSE
                  MC%MP(3) = AINT (-MDAB/MBASE)
                  MC%MP(4) = ABS(-MDAB - MBASE*MC%MP(3))
                  MC%MP(1) = -1
              ENDIF
          ELSE
              GO TO 110
          ENDIF
          IF (ABS(MDR) < MBASE) THEN
              MD%MP(2) = 1
              IF (MDR == 0) MD%MP(2) = 0
              IF (MDR >= 0) THEN
                  MD%MP(3) = MDR
                  MD%MP(1) = 1
              ELSE
                  MD%MP(3) = -MDR
                  MD%MP(1) = -1
              ENDIF
              MD%MP(4) = 0
              GO TO 170
          ELSE IF (ABS(MDR) < MBASE*MBASE) THEN
              MD%MP(2) = 2
              IF (MDR >= 0) THEN
                  MD%MP(3) = AINT (MDR/MBASE)
                  MD%MP(4) = ABS(MDR - MBASE*MD%MP(3))
                  MD%MP(1) = 1
              ELSE
                  MD%MP(3) = AINT (-MDR/MBASE)
                  MD%MP(4) = ABS(-MDR - MBASE*MD%MP(3))
                  MD%MP(1) = -1
              ENDIF
              GO TO 170
          ENDIF
      ENDIF

  110 KLTFLG = 0
      MAS = MA%MP(1)
      MBS = MB%MP(1)
      KL = INT(MB%MP(2))
      IF (ABS(KL) >= MEXPOV) KL = 2
      RESULT_SIZE = ABS(MB%MP(2)) + 4
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 5
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MXY(1)%MP)) THEN
          ALLOCATE(MXY(1)%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MXY(1)%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MXY(1)%MP)
          ALLOCATE(MXY(1)%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      DO J = 0, KL+1
         MXY(1)%MP(J+1) = MB%MP(J+1)
      ENDDO
      MXY(1)%MP(1) = 1
      IF (KL == 1) MXY(1)%MP(4) = 0
      IF (MA%MP(2) == MXY(1)%MP(2) .AND.  &
          ABS(MA%MP(3)) <= MXY(1)%MP(3)) THEN
          DO J = 2, KL+1
             IF (MA%MP(J+1) /= MXY(1)%MP(J+1)) GO TO 120
          ENDDO
          KLTFLG = 2
  120     IF (KLTFLG == 0) THEN
              DO J = 2, KL+1
                 IF (MA%MP(J+1) < MXY(1)%MP(J+1)) THEN
                     KLTFLG = 1
                     EXIT
                 ELSE IF (MA%MP(J+1) > MXY(1)%MP(J+1)) THEN
                     EXIT
                 ENDIF
              ENDDO
          ENDIF
      ENDIF
      IF (MA%MP(2) < MB%MP(2) .OR. KLTFLG >= 1) THEN
          IF (KLTFLG /= 2) THEN
              CALL IMEQ(MA,MD)
              MD%MP(1) = ABS(MD%MP(1))
              CALL IMI2M(0,MC)
          ELSE
              CALL IMI2M(1,MC)
              CALL IMI2M(0,MD)
          ENDIF
          GO TO 160
      ENDIF

      NDIG = INT(MA%MP(2)) + 2
      IF (NDIG < 2) NDIG = 2

!             Check for using an FFT-based method if precision is very high.

      ND = 1000
      IF (INT(MA%MP(2))-INT(MB%MP(2)) >= ND) THEN
          NMETHD = 2
      ELSE
          NMETHD = 1
      ENDIF
      IF (NMETHD == 2) THEN
          CALL IMI2FM(MA,MXY(1))
          MXY(1)%MP(1) = 1
          CALL IMI2FM(MB,MXY(2))
          MXY(2)%MP(1) = 1
          CALL FMDIV(MXY(1),MXY(2),MXY(3))
          CALL IMFM2I(MXY(3),MC)
          IF (KREM /= 1) THEN
              CALL IMI2M(0,MD)
          ELSE
              CALL IMABS(MA,MXY(1))
              CALL IMABS(MB,MXY(2))
              CALL IMMPY(MC,MXY(2),MXY(3))
              CALL IMSUB(MXY(1),MXY(3),MXY(4))
              CALL IMEQ(MXY(4),MD)
              IF (MD%MP(1) == -1) THEN
                  CALL IMI2M(1,MXY(3))
                  CALL IMSUB(MC,MXY(3),MXY(4))
                  CALL IMEQ(MXY(4),MC)
                  CALL IMADD(MD,MXY(2),MXY(4))
                  CALL IMEQ(MXY(4),MD)
              ELSE IF (IMCOMP(MD,'>=',MXY(2))) THEN
                  CALL IMI2M(1,MXY(3))
                  CALL IMADD(MC,MXY(3),MXY(4))
                  CALL IMEQ(MXY(4),MC)
                  CALL IMSUB(MD,MXY(2),MXY(4))
                  CALL IMEQ(MXY(4),MD)
              ENDIF
          ENDIF
          GO TO 160
      ENDIF

!             NGUARD is the number of guard digits used.

      NGUARD = 1
      NA1 = INT(MA%MP(2)) + 1
      NB1 = INT(MB%MP(2)) + 1

!             Copy MA into the working array.

      DO J = 3, NA1
         MWA%MP(J+2) = MA%MP(J+1)
      ENDDO
      MWA%MP(2) = MA%MP(2) - MB%MP(2) + 1
      MWA%MP(3) = 0
      NL = NA1 + NGUARD + 3
      DO J = NA1+2, NL
         MWA%MP(J+1) = 0
      ENDDO

!             Save the sign of MA and MB and then work only with positive numbers.

      MAS = MA%MP(1)
      MB1 = MB%MP(2)
      MBS = MB%MP(1)
      MWA%MP(4) = MA%MP(3)

!             NMBWDS is the number of words of MB used to compute the estimated quotient digit MQD.

      NMBWDS = 4
      IF (MBASE < 100) NMBWDS = 7

!             XB is an approximation of MB used in estimating the quotient digits.

      XBASE = DBLE(MBASE)
      XB = 0
      JL = NMBWDS
      IF (JL <= NB1) THEN
          DO J = 2, JL
             XB = XB*XBASE + DBLE(MB%MP(J+1))
          ENDDO
      ELSE
          DO J = 2, JL
             IF (J <= NB1) THEN
                 XB = XB*XBASE + DBLE(MB%MP(J+1))
             ELSE
                 XB = XB*XBASE
             ENDIF
          ENDDO
      ENDIF
      IF (JL+1 <= NB1) THEN
          XB = XB + DBLE(MB%MP(JL+2))/XBASE
      ENDIF
      XBR = 1.0D0/XB

!             MLMAX determines when to normalize all of MWA.

      MBP1 = MBASE + 1
      MLMAX = MAXINT/MBP1
      MKT = INTMAX - MBASE
      MLMAX = MIN(MLMAX,MKT)

!             MAXMWA is an upper bound on the size of values in MWA divided by MBASE-1.
!             It is used to determine whether normalization can be postponed.

      MAXMWA = 0

!             KPTMWA points to the next digit in the quotient.

      KPTMWA = 2

!             This is the start of the division loop.

!             XMWA is an approximation of the active part of MWA used in estimating quotient digits.

  130 KL = KPTMWA + NMBWDS - 1
      IF (KL <= NL) THEN
          XMWA = ((DBLE(MWA%MP(KPTMWA+1))*XBASE + DBLE(MWA%MP(KPTMWA+2)))*XBASE  &
                 + DBLE(MWA%MP(KPTMWA+3)))*XBASE + DBLE(MWA%MP(KPTMWA+4))
          DO J = KPTMWA+4, KL
             XMWA = XMWA*XBASE + DBLE(MWA%MP(J+1))
          ENDDO
      ELSE
          XMWA = DBLE(MWA%MP(KPTMWA+1))
          DO J = KPTMWA+1, KL
             IF (J <= NL) THEN
                 XMWA = XMWA*XBASE + DBLE(MWA%MP(J+1))
             ELSE
                 XMWA = XMWA*XBASE
             ENDIF
          ENDDO
      ENDIF

!             MQD is the estimated quotient digit.

      MQD = AINT(XMWA*XBR)
      IF (MQD < 0) MQD = MQD - 1

      IF (MQD > 0) THEN
          MAXMWA = MAXMWA + MQD
      ELSE
          MAXMWA = MAXMWA - MQD
      ENDIF

!             See if MWA must be normalized.

      KA = KPTMWA + 1
      KB = KA + INT(MB1) - 1
      IF (MAXMWA >= MLMAX) THEN
          DO J = KB, KA, -1
             IF (MWA%MP(J+1) < 0) THEN
                 MCARRY = INT((-MWA%MP(J+1)-1)/MBASE) + 1
                 MWA%MP(J+1) = MWA%MP(J+1) + MCARRY*MBASE
                 MWA%MP(J) = MWA%MP(J) - MCARRY
             ELSE IF (MWA%MP(J+1) >= MBASE) THEN
                 MCARRY = -INT(MWA%MP(J+1)/MBASE)
                 MWA%MP(J+1) = MWA%MP(J+1) + MCARRY*MBASE
                 MWA%MP(J) = MWA%MP(J) - MCARRY
             ENDIF
          ENDDO
          XMWA = 0
          IF (KL <= NL) THEN
              DO J = KPTMWA, KL
                 XMWA = XMWA*XBASE + DBLE(MWA%MP(J+1))
              ENDDO
          ELSE
              DO J = KPTMWA, KL
                 IF (J <= NL) THEN
                     XMWA = XMWA*XBASE + DBLE(MWA%MP(J+1))
                 ELSE
                     XMWA = XMWA*XBASE
                 ENDIF
              ENDDO
          ENDIF
          MQD = AINT(XMWA*XBR)
          IF (MQD < 0) MQD = MQD - 1
          IF (MQD > 0) THEN
              MAXMWA = MQD
          ELSE
              MAXMWA = -MQD
          ENDIF
      ENDIF

!             Subtract MQD*MB from MWA.

      JB = KA - 2
      IF (MQD /= 0) THEN

!             Major (Inner Loop)

          DO J = KA+1, KB+1
             MWA%MP(J) = MWA%MP(J) - MQD*MB%MP(J-JB)
          ENDDO
      ENDIF

      MWA%MP(KA+1) = MWA%MP(KA+1) + MWA%MP(KA)*MBASE
      MWA%MP(KPTMWA+1) = MQD

      KPTMWA = KPTMWA + 1
      IF (KPTMWA-2 < MWA%MP(2)) GO TO 130

!             Final normalization.

      KPTMWA = KPTMWA - 1
      DO J = KPTMWA, 3, -1
         IF (MWA%MP(J+1) < 0) THEN
             MCARRY = INT((-MWA%MP(J+1)-1)/MBASE) + 1
             MWA%MP(J+1) = MWA%MP(J+1) + MCARRY*MBASE
             MWA%MP(J) = MWA%MP(J) - MCARRY
         ELSE IF (MWA%MP(J+1) >= MBASE) THEN
             MCARRY = -INT(MWA%MP(J+1)/MBASE)
             MWA%MP(J+1) = MWA%MP(J+1) + MCARRY*MBASE
             MWA%MP(J) = MWA%MP(J) - MCARRY
         ENDIF
      ENDDO

      LCRRCT = 0
  140 DO J = KPTMWA+INT(MB1), KPTMWA+2, -1
         IF (MWA%MP(J+1) < 0) THEN
             MCARRY = INT((-MWA%MP(J+1)-1)/MBASE) + 1
             MWA%MP(J+1) = MWA%MP(J+1) + MCARRY*MBASE
             MWA%MP(J) = MWA%MP(J) - MCARRY
         ELSE IF (MWA%MP(J+1) >= MBASE) THEN
             MCARRY = -INT(MWA%MP(J+1)/MBASE)
             MWA%MP(J+1) = MWA%MP(J+1) + MCARRY*MBASE
             MWA%MP(J) = MWA%MP(J) - MCARRY
         ENDIF
      ENDDO

!             Due to rounding, the remainder may not be between 0 and ABS(MB) here.
!             Correct if necessary.

      IF (MWA%MP(KA+1) < 0) THEN
          LCRRCT = LCRRCT - 1
          DO J = KA, KB
             MWA%MP(J+1) = MWA%MP(J+1) + MB%MP(J-JB+1)
          ENDDO
          GO TO 140
      ELSE IF (MWA%MP(KA+1) >= MBASE) THEN
          LCRRCT = LCRRCT + 1
          DO J = KA, KB
             MWA%MP(J+1) = MWA%MP(J+1) - MB%MP(J-JB+1)
          ENDDO
          GO TO 140
      ENDIF
      IF (MWA%MP(3) /= 0 .OR. KPTMWA == 2) THEN
          DO J = 1, INT(MWA%MP(2))+1
             MC%MP(J+1) = MWA%MP(J+1)
          ENDDO
      ELSE
          DO J = 3, INT(MWA%MP(2))+1
             MC%MP(J) = MWA%MP(J+1)
          ENDDO
          IF (MC%MP(3) /= 0) THEN
              MC%MP(2) = MWA%MP(2) - 1
          ELSE
              MC%MP(2) = 0
          ENDIF
      ENDIF
      IF (MC%MP(2) <= 1) MC%MP(4) = 0
      MC%MP(1) = 1

      IF (MWA%MP(KPTMWA+2) /= 0) THEN
          DO J = 1, INT(MB1)
             MD%MP(J+2) = MWA%MP(J+KPTMWA+1)
          ENDDO
          MD%MP(2) = MB1
      ELSE
          DO J = 1, INT(MB1)
             IF (MWA%MP(J+KPTMWA+1) /= 0) THEN
                 DO K = J, INT(MB1)
                    MD%MP(K-J+3) = MWA%MP(K+KPTMWA+1)
                 ENDDO
                 MD%MP(2) = MB1 + 1 - J
                 GO TO 150
             ENDIF
          ENDDO
          MD%MP(2) = 0
          MD%MP(3) = 0
      ENDIF
  150 IF (MD%MP(2) <= 1) MD%MP(4) = 0
      MD%MP(1) = 1

!             If the remainder had to be corrected, make the corresponding adjustment in
!             the quotient.

      IF (MD%MP(2) > MXY(1)%MP(2) .OR.    &
         (MD%MP(2) == MXY(1)%MP(2) .AND.  &
          ABS(MD%MP(3)) >= MXY(1)%MP(3))) THEN
          IF (IMCOMP(MD,'>=',MXY(1))) THEN
              CALL IMSUB(MD,MXY(1),MXY(3))
              CALL IMEQ(MXY(3),MD)
              LCRRCT = LCRRCT + 1
          ENDIF
      ENDIF
      IF (LCRRCT /= 0) THEN
          CALL IMI2M(LCRRCT,MXY(2))
          CALL IMADD(MXY(2),MC,MXY(3))
          CALL IMEQ(MXY(3),MC)
      ENDIF

  160 MC%MP(1) = 1
      MD%MP(1) = 1
      IF (MAS < 0 .AND. MBS > 0) THEN
          IF (MC%MP(2) /= MUNKNO .AND. MC%MP(3) /= 0) MC%MP(1) = -1
          IF (MD%MP(2) /= MUNKNO .AND. MD%MP(3) /= 0) MD%MP(1) = -1
      ELSE IF (MAS > 0 .AND. MBS < 0)  THEN
          IF (MC%MP(2) /= MUNKNO .AND. MC%MP(3) /= 0) MC%MP(1) = -1
      ELSE IF (MAS < 0 .AND. MBS < 0)  THEN
          IF (MD%MP(2) /= MUNKNO .AND. MD%MP(3) /= 0) MD%MP(1) = -1
      ENDIF

  170 IF (MC%MP(2) <= 1) MC%MP(4) = 0
      IF (MD%MP(2) <= 1) MD%MP(4) = 0
      NTRACE = NTRSAV
      IF (NTRACE /= 0) THEN
          CALL IMNTR(1,MC,MC,1)
          IF (ABS(NTRACE) >= 1 .AND. NCALL <= LVLTRC) THEN
              IF (NTRACE < 0) THEN
                  NDIG = MAX(2,INT(MD%MP(2)))
                  NTRSAV = NTRACE
                  CALL IMNTRJ(MD,NDIG)
                  NTRACE = NTRSAV
              ELSE
                  CALL IMPRNT(MD)
              ENDIF
          ENDIF
      ENDIF
      NCALL = NCALL - 1
      NDIG = NDSAVE
      RETURN
      END SUBROUTINE IMDIVR

      SUBROUTINE IMDVIR(MA,IDIV,MB,IREM)

!  MB = INT(MA / IDIV),    IREM = Remainder from the division.

!  Division by a one word integer.  The remainder is also a one word integer.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MAS,MDA,MDAB,MDB,MDR,MKT,MODINT,MVALP
      INTEGER :: IDIV,IREM,J,JDIV,KA,KL,KLTFLG,KPT,N1,NDSAVE,NMVAL,NTRSAV,NV2,RESULT_SIZE
      INTENT (IN) :: MA,IDIV
      INTENT (INOUT) :: MB,IREM
      TYPE(MULTI) :: MXY(3)

      RESULT_SIZE = MAX(5,INT(MA%MP(2)+5))
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 5
      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      RESULT_SIZE = 2*MA%MP(2) + 30
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 32
      RESULT_SIZE = MAX(32,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NCALL = NCALL + 1
      IF (KDEBUG == 1) CALL IMARGS('IMDVIR   ',1,MA,MA)
      KFLAG = 0
      NDSAVE = NDIG
      KLTFLG = 0
      NTRSAV = NTRACE
      NTRACE = 0
      MKT = ABS(IDIV)
      IF (MKT < MBASE) THEN
          IF (.NOT. ALLOCATED(MXY(1)%MP)) THEN
              ALLOCATE(MXY(1)%MP(5),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MXY(1)%MP) < 5) THEN
              DEALLOCATE(MXY(1)%MP)
              ALLOCATE(MXY(1)%MP(5),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          MXY(1)%MP(2) = 1
          MXY(1)%MP(3) = ABS(IDIV)
          MXY(1)%MP(4) = 0
          MXY(1)%MP(1) = 1
          IF (IDIV < 0) MXY(1)%MP(1) = -1
      ELSE IF (MKT < MBASE*MBASE) THEN
          IF (.NOT. ALLOCATED(MXY(1)%MP)) THEN
              ALLOCATE(MXY(1)%MP(6),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MXY(1)%MP) < 6) THEN
              DEALLOCATE(MXY(1)%MP)
              ALLOCATE(MXY(1)%MP(6),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          MXY(1)%MP(2) = 2
          MXY(1)%MP(3) = INT(MKT/MBASE)
          MXY(1)%MP(4) = MKT - MXY(1)%MP(3)*MBASE
          MXY(1)%MP(5) = 0
          MXY(1)%MP(1) = 1
          IF (IDIV < 0) MXY(1)%MP(1) = -1
      ELSE
          CALL IMI2M(IDIV,MXY(1))
      ENDIF
      NTRACE = NTRSAV
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'IMDVIR'
          CALL IMNTR(2,MA,MA,1)
          CALL IMNTRI(2,IDIV,0)
      ENDIF
      JDIV = ABS(IDIV)

!             Check for special cases.

      IF (MA%MP(2) < 0) THEN
          IREM = IUNKNO
          KFLAG = -4
          NAMEST(NCALL) = 'IMDVIR'
          CALL FMWARN
          CALL IMST2M('UNKNOWN',MB)
          GO TO 150
      ENDIF
      IF (JDIV == 1 .AND. MA%MP(2) /= MUNKNO) THEN
          IF (IDIV == 1) THEN
              CALL IMEQ(MA,MB)
              IREM = 0
              GO TO 150
          ELSE
              CALL IMEQ(MA,MB)
              IF (MB%MP(2) /= MUNKNO .AND. MB%MP(3) /= 0)  &
                  MB%MP(1) = -MB%MP(1)
              IREM = 0
              GO TO 150
          ENDIF
      ENDIF
      IF (MA%MP(2) >= MEXPOV .OR. IDIV == 0) THEN
          KFLAG = -4
          IF (MA%MP(2) /= MUNKNO) THEN
              NAMEST(NCALL) = 'IMDVIR'
              CALL FMWARN
          ENDIF
          CALL IMST2M('UNKNOWN',MB)
          IREM = IUNKNO
          GO TO 150
      ENDIF
      IF (MA%MP(2) <= 2) THEN
          IF (MA%MP(2) <= 1) THEN
              MDA = MA%MP(1) * MA%MP(3)
          ELSE
              MDA = MA%MP(1) * (MA%MP(3)*MBASE + MA%MP(4))
          ENDIF
          MDB = IDIV
          MDAB = AINT (MDA/MDB)
          MDR = MDA - MDAB*MDB
          IF (ABS(MDAB) < MBASE) THEN
              MB%MP(2) = 1
              IF (MDAB == 0) MB%MP(2) = 0
              IF (MDAB < 0) THEN
                  MB%MP(3) = -MDAB
                  MB%MP(1) = -1
              ELSE
                  MB%MP(3) = MDAB
                  MB%MP(1) = 1
              ENDIF
              MB%MP(4) = 0
          ELSE IF (ABS(MDAB) < MBASE*MBASE) THEN
              MB%MP(2) = 2
              IF (MDAB < 0) THEN
                  MB%MP(3) = AINT (-MDAB/MBASE)
                  MB%MP(4) = ABS(-MDAB - MBASE*MB%MP(3))
                  MB%MP(1) = -1
              ELSE
                  MB%MP(3) = AINT (MDAB/MBASE)
                  MB%MP(4) = ABS(MDAB - MBASE*MB%MP(3))
                  MB%MP(1) = 1
              ENDIF
          ELSE
              GO TO 110
          ENDIF
          IREM = INT(MDR)
          GO TO 150
      ENDIF

  110 MAS = MA%MP(1)
      MXY(1)%MP(1) = 1
      KL = MXY(1)%MP(2)
      IF (MA%MP(2) <= MXY(1)%MP(2)) THEN
          IF (MA%MP(2) == MXY(1)%MP(2) .AND.  &
              ABS(MA%MP(3)) <= MXY(1)%MP(3)) THEN
              DO J = 2, KL+1
                 IF (MA%MP(J+1) /= MXY(1)%MP(J+1)) THEN
                     IF (MA%MP(J+1) < MXY(1)%MP(J+1)) KLTFLG = 1
                     GO TO 120
                 ENDIF
              ENDDO
              KLTFLG = 2
          ENDIF
  120     IF (MA%MP(2) < MXY(1)%MP(2) .OR. KLTFLG >= 1) THEN
              IF (KLTFLG /= 2) THEN
                  CALL IMM2I(MA,IREM)
                  IREM = ABS(IREM)
                  CALL IMI2M(0,MB)
              ELSE
                  CALL IMI2M(1,MB)
                  IREM = 0
              ENDIF
              GO TO 140
          ENDIF
      ENDIF
      NDIG = INT(MA%MP(2))
      IF (NDIG < 2) NDIG = 2
      N1 = INT(MA%MP(2)) + 1

!             If ABS(IDIV) >= MXBASE use IMDIVR.

      MVALP = ABS(IDIV)
      NMVAL = INT(MVALP)
      NV2 = NMVAL - 1
      IF (ABS(IDIV) > MXBASE .OR. NMVAL /= ABS(IDIV) .OR. NV2 /= ABS(IDIV)-1) THEN
          CALL IMI2M(IDIV,MXY(2))
          CALL IMDIVR(MA,MXY(2),MXY(1),MXY(3))
          CALL IMEQ(MXY(1),MB)
          CALL IMEQ(MXY(3),MXY(2))
          CALL IMM2I(MXY(2),IREM)
          GO TO 150
      ENDIF

!             Find the first significant digit of the quotient.

      MKT = MA%MP(3)
      IF (MKT >= MVALP) THEN
          KPT = 2
          GO TO 130
      ENDIF
      DO J = 3, N1
         MKT = MKT*MBASE + MA%MP(J+1)
         IF (MKT >= MVALP) THEN
             KPT = J
             GO TO 130
         ENDIF
      ENDDO

      CALL IMM2I(MA,IREM)
      CALL IMI2M(0,MB)
      GO TO 150

!             Do the rest of the division.

  130 KA = KPT + 1
      MWA%MP(2) = MA%MP(2) + 2 - KPT
      MWA%MP(3) = INT (MKT/MVALP)
      MODINT = MKT - MWA%MP(3)*MVALP
      IF (KA <= N1) THEN
          KL = 3 - KA

!             (Inner Loop)

          DO J = KA+1, N1+1
             MKT = MODINT*MBASE + MA%MP(J)
             MWA%MP(KL+J) = INT (MKT/MVALP)
             MODINT = MKT - MWA%MP(KL+J)*MVALP
          ENDDO
      ENDIF

      DO J = 1, INT(MWA%MP(2))+1
         MB%MP(J+1) = MWA%MP(J+1)
      ENDDO
      IREM = INT(MODINT)

  140 MB%MP(1) = 1
      IF (MAS < 0 .AND. IDIV > 0) THEN
          IF (MB%MP(2) /= MUNKNO .AND. MB%MP(3) /= 0) MB%MP(1) = -1
          IREM = -IREM
      ELSE IF (MAS > 0 .AND. IDIV < 0)  THEN
          IF (MB%MP(2) /= MUNKNO .AND. MB%MP(3) /= 0) MB%MP(1) = -1
      ELSE IF (MAS < 0 .AND. IDIV < 0)  THEN
          IREM = -IREM
      ENDIF

  150 IF (MB%MP(2) <= 1) MB%MP(4) = 0
      IF (NTRACE /= 0 .AND. NCALL <= LVLTRC) THEN
          CALL IMNTR(1,MB,MB,1)
          CALL IMNTRI(1,IREM,0)
      ENDIF

      NCALL = NCALL - 1
      NDIG = NDSAVE
      RETURN
      END SUBROUTINE IMDVIR

      SUBROUTINE IMEQ(MA,MB)

!  MB = MA

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB

      INTEGER :: J,KDG
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      INTEGER :: RESULT_SIZE

      RESULT_SIZE = MA%MP(2)+3
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 5
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      KDG = MAX(2,INT(MA%MP(2))) + 2
      IF (KDG > SIZE(MA%MP)) KDG = 4
      DO J = 1, KDG
         MB%MP(J) = MA%MP(J)
      ENDDO
      RETURN
      END SUBROUTINE IMEQ

      SUBROUTINE IMFACT(N,MA)

!  MA = N!   (N factorial)  N is a machine precision integer and MA is an IM number.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTEGER :: N
      INTENT (IN) :: N
      INTENT (INOUT) :: MA
      INTEGER :: J,K,NDSAVE,RESULT_SIZE

      K = MAX(N,2)
      RESULT_SIZE = 10 + (LOG(2.0D0*DPPI*K)/2 + K*LOG(DBLE(K)) - K) / DLOGMB
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 5
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MA%MP)) THEN
          ALLOCATE(MA%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MA%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MA%MP)
          ALLOCATE(MA%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NCALL = NCALL + 1
      KFLAG = 0
      NDSAVE = NDIG
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'IMFACT'
          CALL  IMNTRI(2,N,1)
      ENDIF

!             Check for special cases.

      IF (N < 0) THEN
          CALL IMST2M('UNKNOWN',MA)
          KFLAG = -4
          GO TO 110
      ELSE IF (N <= 12) THEN
          K = 1
          DO J = 2, N
             K = K*J
          ENDDO
          CALL IMI2M(K,MA)
          GO TO 110
      ENDIF

      CALL IMFACT_P(2,N,MA)

  110 IF (MA%MP(2) <= 1) MA%MP(4) = 0
      IF (NTRACE /= 0) CALL IMNTR(1,MA,MA,1)
      NCALL = NCALL - 1
      NDIG = NDSAVE
      RETURN
      END SUBROUTINE IMFACT

      RECURSIVE SUBROUTINE IMFACT_P(A,B,MP)

!  This routine does the binary splitting for computing N!
!  MP = A * (A+1) * ... * B.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MP
      INTEGER :: A,B
      INTENT (IN) :: A,B
      INTENT (INOUT) :: MP
      TYPE(MULTI) :: MXY(2)
      INTEGER :: J,M,RESULT_SIZE
      REAL (KIND(0.0D0)) :: DA,DB,DLA,DLB

      DA = A
      DB = B
      IF ((DB+0.5D0)*LOG(DB+1) > 1.0D+10) THEN
          RESULT_SIZE = (B-A+1)*LOG(DB) / DLOGMB + 7 + NGRD52
      ELSE
          IF (A >= 2) THEN
              DLA = (DA+0.5D0)*LOG(DA) - DA + 1/(12*(DA))
          ELSE
              DLA = 0
          ENDIF
          IF (B >= 2) THEN
              DLB = (DB+0.5D0)*LOG(DB) - DB + 1/(12*(DB))
          ELSE
              DLB = 0
          ENDIF
          RESULT_SIZE = ( DLB - DLA ) / DLOGMB + 7 + NGRD52
      ENDIF
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MP%MP)) THEN
          ALLOCATE(MP%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MP%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MP%MP)
          ALLOCATE(MP%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (B-A < 25) THEN
          IF (.NOT. ALLOCATED(MXY(1)%MP)) THEN
              ALLOCATE(MXY(1)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MXY(1)%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MXY(1)%MP)
              ALLOCATE(MXY(1)%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
          CALL IMI2M(A,MP)
          DO J = A+1, B-1, 2
             CALL IMMPYI(MP,J,MXY(1))
             CALL IMMPYI(MXY(1),J+1,MP)
          ENDDO
          IF (MOD(B-A,2) == 1) THEN
              CALL IMMPYI(MP,B,MXY(1))
              CALL IMEQ(MXY(1),MP)
          ENDIF
          GO TO 110
      ENDIF

      M = A/2 + B/2 + MOD(A,2)*MOD(B,2)
      CALL IMFACT_P(A,M,MXY(1))
      CALL IMFACT_P(M+1,B,MXY(2))
      CALL IMMPY(MXY(1),MXY(2),MP)

  110 RETURN
      END SUBROUTINE IMFACT_P

      SUBROUTINE IMFM2I(MA,MB)

!  MB = INT(MA)

!  Convert from real (FM) format to integer (IM) format.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB

      INTEGER :: J,NTRSAV,NDGSAV,RESULT_SIZE
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(2)

      RESULT_SIZE = MAX(5,INT(MA%MP(2)+4))
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 5
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NCALL = NCALL + 1
      KFLAG = 0
      NTRSAV = NTRACE
      NTRACE = 0
      NDGSAV = NDIG
      IF (ABS(MA%MP(2)+3) >= MEXPOV) THEN
          NDIG = 2
          CALL FMEQ(MA,MB)
          NDIG = NDGSAV
      ELSE
          NDIG = MIN(NDIG,RESULT_SIZE-3)
          CALL FMINT(MA,MXY(1))
          CALL FMEQ(MXY(1),MB)
          NDIG = NDGSAV
          DO J = NDIG+2, INT(MA%MP(2))+1
             MB%MP(J+1) = 0
          ENDDO
          IF (MB%MP(2) <= 1) MB%MP(4) = 0
      ENDIF
      NTRACE = NTRSAV
      NCALL = NCALL - 1

      RETURN
      END SUBROUTINE IMFM2I

      SUBROUTINE IMFORM(FORM,MA,STRING)

!  Convert an IM number (MA) to a character string base 10 (STRING) using character string
!  FORM format.

!  FORM can be one of these types:  Iw,  Fw.d,  Ew.d,  ESw.d,  1PEw.d  for positive integers w,d.

      USE ModLib_FMVALS
      IMPLICIT NONE

      CHARACTER(*) :: FORM,STRING
      TYPE(MULTI) :: MA
      INTEGER :: NDSAVE
      INTENT (IN) :: FORM,MA
      INTENT (INOUT) :: STRING

      NCALL = NCALL + 1
      IF (KDEBUG == 1) CALL IMARGS('IMFORM   ',1,MA,MA)
      KFLAG = 0
      NAMEST(NCALL) = 'IMFORM'
      NDSAVE = NDIG
      NDIG = INT(MA%MP(2))
      IF (NDIG < 2) NDIG = 2
      IF (ABS(MA%MP(2)) >= MEXPOV) NDIG = 2

      CALL FMFORM(FORM,MA,STRING)

      NCALL = NCALL - 1
      NDIG = NDSAVE
      RETURN
      END SUBROUTINE IMFORM

      SUBROUTINE IMFPRT(FORM,MA)

!  Print an IM number (MA) on unit KW using character string FORM format.

!  FORM can be one of these types:  Iw,  Fw.d,  Ew.d,  ESw.d,  1PEw.d  for positive integers w,d.

      USE ModLib_FMVALS
      IMPLICIT NONE

      CHARACTER(*) :: FORM
      TYPE(MULTI) :: MA
      INTEGER :: NDSAVE
      INTENT (IN) :: FORM,MA

      NCALL = NCALL + 1
      IF (KDEBUG == 1) CALL IMARGS('IMFPRT   ',1,MA,MA)
      KFLAG = 0
      NAMEST(NCALL) = 'IMFPRT'
      NDSAVE = NDIG
      NDIG = INT(MA%MP(2))
      IF (NDIG < 2) NDIG = 2
      IF (ABS(MA%MP(2)) >= MEXPOV) NDIG = 2

      CALL FMFPRT(FORM,MA)

      NCALL = NCALL - 1
      NDIG = NDSAVE
      RETURN
      END SUBROUTINE IMFPRT

      SUBROUTINE IMGCD(MA,MB,MC)

!  Lehmer's GCD algorithm for MC = GCD(MA,MB).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA, MB, MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      INTEGER :: J, K
      DOUBLE PRECISION :: ABCD_MAX, DPBASE, A1, B1, C1, D1, Q1A, Q1B, T1, T2, T3, U1, V1
      CHARACTER(25) :: ST
      LOGICAL, EXTERNAL :: IMABS_GREATER_THAN
      TYPE(MULTI) :: MXY(4)
      INTEGER :: NDSAVE,RESULT_SIZE

      RESULT_SIZE = MAX(MA%MP(2),MB%MP(2)) + 3
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 5
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NCALL = NCALL + 1
      IF (KDEBUG == 1) CALL IMARGS('IMGCD    ',2,MA,MB)
      KFLAG = 0
      NDSAVE = NDIG
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'IMGCD'
          CALL IMNTR(2,MA,MB,2)
      ENDIF

!             Check for special cases.

      IF (MA%MP(2) == MUNKNO .OR. MB%MP(2) == MUNKNO) THEN
          CALL IMST2M('UNKNOWN',MC)
          KFLAG = -4
          GO TO 110
      ELSE IF (MB%MP(3) == 0) THEN
          CALL IMABS(MA,MC)
          GO TO 110
      ELSE IF (MA%MP(3) == 0) THEN
          CALL IMABS(MB,MC)
          GO TO 110
      ELSE IF (MB%MP(2) == 1 .AND. ABS(MB%MP(3)) == 1) THEN
          CALL IMI2M(1,MC)
          GO TO 110
      ELSE IF (MA%MP(2) == 1 .AND. ABS(MA%MP(3)) == 1) THEN
          CALL IMI2M(1,MC)
          GO TO 110
      ELSE IF (MA%MP(2) < 0 .OR. MB%MP(2) < 0 .OR.  &
          MA%MP(2) >= MEXPOV .OR. MB%MP(2) >= MEXPOV) THEN
          KFLAG = -4
          NAMEST(NCALL) = 'IMGCD'
          CALL FMWARN
          CALL IMST2M('UNKNOWN',MC)
          GO TO 110
      ENDIF

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
      CALL IMABS(MA,MXY(3))
      CALL IMABS(MB,MXY(4))
      IF (IMABS_GREATER_THAN(MXY(3),MXY(4))) THEN
          CALL IMEQ(MXY(3),MXY(1))
          CALL IMEQ(MXY(4),MXY(2))
      ELSE
          CALL IMEQ(MXY(4),MXY(1))
          CALL IMEQ(MXY(3),MXY(2))
      ENDIF

      IF (MBASE < 1000) THEN
          CALL IMGCD2(MA,MB,MC)
          GO TO 110
      ENDIF

      ABCD_MAX = (MAXINT / MBASE) / 2
      DPBASE = MBASE
      T1 = 1.0D-3 / EPSILON(1.0D0)
      CALL IMI2M(1,MXY(3))
      T2 = DPBASE
      K = 1
      DO WHILE (T2 < T1)
         MXY(3)%MP(2) = MXY(3)%MP(2) + 1
         K = K + 1
         MXY(3)%MP(K+2) = 0
         T2 = T2 * DPBASE
      ENDDO
      MXY(3)%MP(3) = AINT( DPBASE * T1 / T2 )

!             MXY(1) >= MXY(2) > 0 during the algorithm.

      DO WHILE (IMABS_GREATER_THAN(MXY(1),MXY(3)))

         IF (MXY(1)%MP(2) == MXY(2)%MP(2)) THEN
             U1 = MXY(1)%MP(3)
             V1 = MXY(2)%MP(3)
             DO J = 2, INT(MXY(1)%MP(2))
                IF (U1 < MAXINT / (10 * DPBASE)) THEN
                    U1 = U1 * DPBASE + MXY(1)%MP(J+2)
                    V1 = V1 * DPBASE + MXY(2)%MP(J+2)
                ELSE
                    T1 = (MXY(1)%MP(J+2) + 1.0D-2) / DPBASE
                    T2 = (MXY(2)%MP(J+2) + 1.0D-2) / DPBASE
                    DO WHILE (U1 < MAXINT / 100)
                       T1 = T1 * 10
                       K = T1
                       T1 = T1 - K
                       U1 = U1 * 10 + K

                       T2 = T2 * 10
                       K = T2
                       T2 = T2 - K
                       V1 = V1 * 10 + K
                    ENDDO
                    EXIT
                ENDIF
             ENDDO
         ELSE IF (MXY(1)%MP(2) == MXY(2)%MP(2) + 1) THEN
             U1 = MXY(1)%MP(3)
             V1 = 0
             DO J = 2, INT(MXY(1)%MP(2))
                IF (U1 < MAXINT / (10 * DPBASE)) THEN
                    U1 = U1 * DPBASE + MXY(1)%MP(J+2)
                    V1 = V1 * DPBASE + MXY(2)%MP(J+1)
                ELSE
                    T1 = (MXY(1)%MP(J+2)   + 1.0D-2) / DPBASE
                    T2 = (MXY(2)%MP(J+1) + 1.0D-2) / DPBASE
                    DO WHILE (U1 < MAXINT / 100)
                       T1 = T1 * 10
                       K = T1
                       T1 = T1 - K
                       U1 = U1 * 10 + K

                       T2 = T2 * 10
                       K = T2
                       T2 = T2 - K
                       V1 = V1 * 10 + K
                    ENDDO
                    EXIT
                ENDIF
             ENDDO
         ELSE
             U1 = 1
             V1 = 0
         ENDIF

         A1 = 1
         B1 = 0
         C1 = 0
         D1 = 1
         Q1A = 0
         Q1B = 0

         DO WHILE (Q1A == Q1B)
            IF (V1 + C1 /= 0.0D0 .AND. V1 + D1 /= 0.0D0) THEN
                Q1A = AINT( (U1 + A1) / (V1 + C1) )
                Q1B = AINT( (U1 + B1) / (V1 + D1) )
                IF (Q1A == Q1B) THEN
                    T1 = A1 - Q1A*C1
                    T2 = B1 - Q1A*D1
                    T3 = U1 - Q1A*V1
                    IF (ABS(T1) > ABCD_MAX .OR. ABS(T2) > ABCD_MAX) THEN
                        EXIT
                    ELSE
                        A1 = C1
                        C1 = T1
                        B1 = D1
                        D1 = T2
                        U1 = V1
                        V1 = T3
                    ENDIF
                ENDIF
            ELSE
                EXIT
            ENDIF
         ENDDO

         IF (B1 == 0) THEN
             CALL IMMOD(MXY(1),MXY(2),MXY(4))
             CALL IMEQ(MXY(2),MXY(1))
             CALL IMEQ(MXY(4),MXY(2))
             IF (MXY(2)%MP(3) == 0) THEN
                 CALL IMEQ(MXY(1),MC)
                 GO TO 110
             ENDIF
         ELSE
             CALL IMGCD_REDUCE_UV(MXY(1), MXY(2), MXY(4), A1, B1, C1, D1)
         ENDIF
      ENDDO

!             Finish the gcd using double precision.

      IF (MXY(2)%MP(3) == 0) THEN
          CALL IMEQ(MXY(1),MC)
          GO TO 110
      ENDIF
      U1 = 0
      DO J = 1, INT(MXY(1)%MP(2))
         U1 = U1 * DPBASE + MXY(1)%MP(J+2)
      ENDDO
      V1 = 0
      DO J = 1, INT(MXY(2)%MP(2))
         V1 = V1 * DPBASE + MXY(2)%MP(J+2)
      ENDDO
      DO WHILE (V1 > 0)
         T1 = MOD(U1,V1)
         U1 = V1
         V1 = T1
      ENDDO
      IF (U1 < DPBASE) THEN
          MXY(1)%MP(2) = 1
          MXY(1)%MP(3) = U1
          MXY(1)%MP(4) = 0
      ELSE IF (U1 < DPBASE*DPBASE) THEN
          MXY(1)%MP(2) = 2
          MXY(1)%MP(3) = AINT(U1/DPBASE)
          MXY(1)%MP(4) = MOD(U1,DPBASE)
      ELSE
          IF (ABS(U1) < HUGE(1)) THEN
              K = U1
              CALL IMI2M(K,MXY(1))
          ELSE
              WRITE (ST,'(E25.16)') U1
              CALL IMST2M(ST,MXY(1))
          ENDIF
      ENDIF

      CALL IMEQ(MXY(1),MC)

      IF (MC%MP(2) == MUNKNO) THEN
          KFLAG = -4
          NAMEST(NCALL) = 'IMGCD'
          CALL FMWARN
      ENDIF

  110 IF (MC%MP(2) <= 1) MC%MP(4) = 0
      IF (NTRACE /= 0) CALL IMNTR(1,MC,MC,1)
      NCALL = NCALL - 1
      NDIG = NDSAVE
      RETURN
      END SUBROUTINE IMGCD

      SUBROUTINE IMGCD_REDUCE_UV(MU, MV, MV_SHIFT, A1, B1, C1, D1)

!  Return  MU = A1 * MU + B1 * MV,
!  and     MV = C1 * MU + D1 * MV.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MU, MV, MV_SHIFT
      DOUBLE PRECISION :: A1, B1, C1, D1, U1, V1
      INTENT (INOUT) :: MU, MV
      INTENT (IN) :: A1, B1, C1, D1
      INTEGER :: J, K, N_U, N_V

!             Do the operations.

      N_U = MU%MP(2)
      N_V = MV%MP(2)
      K = N_U - N_V
      IF (K == 0) THEN
          DO J = 3, N_U+2
             U1 = MU%MP(J)
             V1 = MV%MP(J)
             MU%MP(J-1) = A1 * U1 + B1 * V1
             MV%MP(J-1) = C1 * U1 + D1 * V1
          ENDDO
      ELSE
          DO J = 3, K+2
             MV_SHIFT%MP(J) = 0
          ENDDO
          DO J = 3, N_V+2
             MV_SHIFT%MP(J+K) = MV%MP(J)
          ENDDO
          N_V = N_U
          DO J = 3, N_U+2
             U1 = MU%MP(J)
             V1 = MV_SHIFT%MP(J)
             MU%MP(J-1) = A1 * U1 + B1 * V1
             MV%MP(J-1) = C1 * U1 + D1 * V1
          ENDDO
      ENDIF

!             Normalize the digits in MU and MV.

      DO J = N_U+1, 2, -1
         IF (MU%MP(J) < 0) THEN
             K = (-MU%MP(J)-1) / MBASE + 1
             MU%MP(J) = MU%MP(J) + K * MBASE
             MU%MP(J-1) = MU%MP(J-1) - K
         ELSE
             K = MU%MP(J) / MBASE
             MU%MP(J) = MU%MP(J) - K * MBASE
             MU%MP(J-1) = MU%MP(J-1) + K
         ENDIF
      ENDDO

      DO J = N_V+1, 2, -1
         IF (MV%MP(J) < 0) THEN
             K = (-MV%MP(J)-1) / MBASE + 1
             MV%MP(J) = MV%MP(J) + K * MBASE
             MV%MP(J-1) = MV%MP(J-1) - K
         ELSE
             K = MV%MP(J) / MBASE
             MV%MP(J) = MV%MP(J) - K * MBASE
             MV%MP(J-1) = MV%MP(J-1) + K
         ENDIF
      ENDDO

!             Normalize the numbers if there are leading zeros.

      IF (MU%MP(2) == 0) THEN
          MU%MP(2) = N_U - 1
      ELSE
          DO J = N_U+2, 3, -1
             MU%MP(J) = MU%MP(J-1)
          ENDDO
          MU%MP(2) = N_U
      ENDIF
      K = 0
      DO J = 3, N_U+2
         IF (MU%MP(J) == 0) THEN
             K = K + 1
         ELSE
             EXIT
         ENDIF
      ENDDO
      IF (K > 0) THEN
          MU%MP(2) = MU%MP(2) - K
          DO J = 3, INT(MU%MP(2))+2
             MU%MP(J) = MU%MP(J+K)
          ENDDO
          DO J = 0, K-1
             MU%MP(2+N_U-J) = 0
          ENDDO
      ENDIF

      IF (MV%MP(2) == 0) THEN
          MV%MP(2) = N_V - 1
      ELSE
          DO J = N_V+2, 3, -1
             MV%MP(J) = MV%MP(J-1)
          ENDDO
          MV%MP(2) = N_V
      ENDIF
      K = 0
      DO J = 3, N_V+2
         IF (MV%MP(J) == 0) THEN
             K = K + 1
         ELSE
             EXIT
         ENDIF
      ENDDO
      IF (K > 0) THEN
          MV%MP(2) = MV%MP(2) - K
          DO J = 3, INT(MV%MP(2))+2
             MV%MP(J) = MV%MP(J+K)
          ENDDO
          DO J = 0, K-1
             MV%MP(2+N_V-J) = 0
          ENDDO
      ENDIF

      END SUBROUTINE IMGCD_REDUCE_UV

      SUBROUTINE IMGCD2(MA,MB,MC)

!  MC is returned as the greatest common divisor of MA and MB.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC
      INTEGER :: NDSAVE,RESULT_SIZE
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      TYPE(MULTI) :: MXY(4)

      RESULT_SIZE = MAX(MA%MP(2),MB%MP(2)) + 3
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 5
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NCALL = NCALL + 1
      IF (KDEBUG == 1) CALL IMARGS('IMGCD2   ',2,MA,MB)
      KFLAG = 0
      NDSAVE = NDIG
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'IMGCD2'
          CALL IMNTR(2,MA,MB,2)
      ENDIF

!             Check for special cases.

      IF (MA%MP(2) == MUNKNO .OR. MB%MP(2) == MUNKNO) THEN
          CALL IMST2M('UNKNOWN',MC)
          KFLAG = -4
          GO TO 120
      ELSE IF (MB%MP(3) == 0) THEN
          CALL IMABS(MA,MC)
          GO TO 120
      ELSE IF (MA%MP(3) == 0) THEN
          CALL IMABS(MB,MC)
          GO TO 120
      ELSE IF (MB%MP(2) == 1 .AND. ABS(MB%MP(3)) == 1) THEN
          CALL IMI2M(1,MC)
          GO TO 120
      ELSE IF (MA%MP(2) == 1 .AND. ABS(MA%MP(3)) == 1) THEN
          CALL IMI2M(1,MC)
          GO TO 120
      ELSE IF (MA%MP(2) < 0 .OR. MB%MP(2) < 0 .OR.  &
          MA%MP(2) >= MEXPOV .OR. MB%MP(2) >= MEXPOV) THEN
          KFLAG = -4
          NAMEST(NCALL) = 'IMGCD2'
          CALL FMWARN
          CALL IMST2M('UNKNOWN',MC)
          GO TO 120
      ENDIF

      CALL IMABS(MA,MXY(3))
      CALL IMABS(MB,MXY(2))
      CALL IMMAX(MXY(3),MXY(2),MXY(1))
      CALL IMMIN(MXY(3),MXY(2),MXY(4))
      CALL IMEQ(MXY(4),MXY(2))
  110 CALL IMDIVR(MXY(1),MXY(2),MXY(4),MXY(3))
      IF (MXY(3)%MP(3) /= 0) THEN
          CALL IMEQ(MXY(2),MXY(1))
          CALL IMEQ(MXY(3),MXY(2))
          GO TO 110
      ENDIF
      CALL IMEQ(MXY(2),MC)

      IF (MC%MP(2) == MUNKNO) THEN
          KFLAG = -4
          NAMEST(NCALL) = 'IMGCD2'
          CALL FMWARN
      ENDIF

  120 IF (MC%MP(2) <= 1) MC%MP(4) = 0
      IF (NTRACE /= 0) CALL IMNTR(1,MC,MC,1)
      NCALL = NCALL - 1
      NDIG = NDSAVE
      RETURN
      END SUBROUTINE IMGCD2

      SUBROUTINE IMI2FM(MA,MB)

!  MB = MA

!  Convert from integer (IM) format to real (FM) format.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      INTEGER :: KDG
      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NCALL = NCALL + 1
      IF (KDEBUG == 1) CALL IMARGS('IMI2FM   ',1,MA,MA)
      KFLAG = 0
      KDG = MAX(2,INT(MA%MP(2)))
      IF (MA%MP(2) >= MEXPOV) KDG = 2
      CALL FMEQU(MA,MB,KDG,NDIG)
      NCALL = NCALL - 1

      RETURN
      END SUBROUTINE IMI2FM

      SUBROUTINE IMI2M(IVAL,MA)

!  MA = IVAL

!  This routine performs the trace printing.  IMI2M2 is used to do the conversion.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTEGER :: IVAL
      INTENT (IN) :: IVAL
      INTENT (INOUT) :: MA

      NCALL = NCALL + 1
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'IMI2M'
          CALL IMNTRI(2,IVAL,1)

          CALL IMI2M2(IVAL,MA)

          CALL IMNTR(1,MA,MA,1)
      ELSE
          CALL IMI2M2(IVAL,MA)
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE IMI2M

      SUBROUTINE IMI2M2(IVAL,MA)

!  MA = IVAL

!  Convert a one word integer to IM format.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTEGER :: IVAL

      INTEGER :: NDSAVE,RESULT_SIZE
      DOUBLE PRECISION :: DT
      INTENT (IN) :: IVAL
      INTENT (INOUT) :: MA

      KFLAG = 0
      NDSAVE = NDIG
      IF (IVAL == 0) THEN
          NDIG = 2
      ELSE
          DT = LOG(DBLE(ABS(IVAL)))/DLOGMB + 2
          NDIG = MAX(2,INT(DT))
      ENDIF
      RESULT_SIZE = NDIG + 3
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 5
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MA%MP)) THEN
          ALLOCATE(MA%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MA%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MA%MP)
          ALLOCATE(MA%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      CALL FMIM(IVAL,MA)
      IF (MA%MP(2) <= 1) MA%MP(4) = 0
      NDIG = NDSAVE
      RETURN
      END SUBROUTINE IMI2M2

      SUBROUTINE IMINP(LINE,MA,LA,LB)

!  Convert an array of characters to multiple precision integer format.

!  LINE is an A1 character array of length LB to be converted to IM format and returned in MA.
!  LA is a pointer telling the routine where in the array to begin the conversion.
!  LB is a pointer to the last character of the field for that number.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: KFSAVE,NDSAVE,LA,LB
      CHARACTER :: LINE(LB)
      TYPE(MULTI) :: MA
      INTENT (IN) :: LINE,LA,LB
      INTENT (INOUT) :: MA


      NCALL = NCALL + 1
      KFLAG = 0
      NDSAVE = NDIG
      NAMEST(NCALL) = 'IMINP'

      NDIG = 50
      NCALL = NCALL - 1
      CALL FMINP(LINE,MWI,LA,LB)
      NCALL = NCALL + 1
      IF (MWI%MP(2) > 50 .AND. ABS(MWI%MP(2)) < MEXPOV) THEN
          NDIG = MWI%MP(2) + 1
          NCALL = NCALL - 1
          CALL FMINP(LINE,MWI,LA,LB)
          NCALL = NCALL + 1
      ENDIF
      KFSAVE = KFLAG
      CALL IMFM2I(MWI,MA)
      KFLAG = KFSAVE

      IF (MA%MP(2) <= 1) MA%MP(4) = 0
      NDIG = NDSAVE
      IF (NTRACE /= 0) CALL IMNTR(1,MA,MA,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE IMINP

      SUBROUTINE IMM2DP(MA,X)

!  X = MA

!  Convert an IM number to double precision.

!  If KFLAG = -4 is returned for a value of MA that is in the range of the machine's double
!  precision number system, change the definition of DPMAX in routine FMSET to reflect the
!  current machine's range.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      DOUBLE PRECISION :: X

      INTEGER :: KRESLT,ND2,NDSAVE
      INTENT (IN) :: MA
      INTENT (INOUT) :: X

      NCALL = NCALL + 1
      KFLAG = 0
      NAMEST(NCALL) = 'IMM2DP'
      KRESLT = 0
      IF (ABS(MA%MP(2)) > MEXPAB) THEN
          CALL FMARGS('IMM2DP   ',1,MA,MA,KRESLT)
      ENDIF
      IF (NTRACE /= 0) CALL IMNTR(2,MA,MA,1)
      IF (KRESLT /= 0) THEN

!             Here no valid result can be returned.  Set X to some value that the user is likely
!             to recognize as wrong.

          X = DBLE(RUNKNO)
          KFLAG = -4
          IF (MA%MP(2) /= MUNKNO) CALL FMWARN
          IF (NTRACE /= 0) CALL IMNTRR(1,X,1)
          NCALL = NCALL - 1
          RETURN
      ENDIF

      NDSAVE = NDIG
      NDIG = MAX(2,INT(MA%MP(2)))
      ND2 = 2 - LOG(EPSILON(1.0D0))/DLOGMB
      IF (NDIG >= ND2) NDIG = ND2
      CALL FMMD(MA,X)

      IF (NTRACE /= 0) CALL IMNTRR(1,X,1)
      NDIG = NDSAVE
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE IMM2DP

      SUBROUTINE IMM2I(MA,IVAL)

!  IVAL = MA

!  Convert an IM number to a one word integer.

!  KFLAG =  0 is returned if the conversion is exact.
!        = -4 is returned if MA is larger than INTMAX in magnitude.  IVAL = IUNKNO is returned as
!             an indication that IVAL could not be computed without integer overflow.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA

      INTEGER :: IVAL,NDSAVE
      INTENT (IN) :: MA
      INTENT (INOUT) :: IVAL

      NCALL = NCALL + 1
      IF (KDEBUG == 1) CALL IMARGS('IMM2I    ',1,MA,MA)
      NDSAVE = NDIG
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'IMM2I'
          CALL IMNTR(2,MA,MA,1)
      ENDIF

      NDIG = INT(MA%MP(2))
      IF (NDIG < 2) NDIG = 2
      IF (ABS(MA%MP(2)) >= MEXPOV) NDIG = 2
      KFLAG = 0
      CALL FMM2I(MA,IVAL)

      IF (ABS(NTRACE) >= 1 .AND. NCALL <= LVLTRC) THEN
          CALL IMNTRI(1,IVAL,1)
      ENDIF
      NCALL = NCALL - 1
      NDIG = NDSAVE
      RETURN
      END SUBROUTINE IMM2I

      SUBROUTINE IMM2SP(MA,X)

!  X = MA

!  Convert an IM number to single precision.

!  If KFLAG = -4 is returned for a value of MA that is in the range of the machine's single
!  precision number system, change the definition of SPMAX in routine FMSET to reflect the
!  current machine's range.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      REAL :: X

      DOUBLE PRECISION :: Y
      INTEGER :: KRESLT,ND2,NDSAVE
      INTENT (IN) :: MA
      INTENT (INOUT) :: X

      NCALL = NCALL + 1
      KFLAG = 0
      NAMEST(NCALL) = 'IMM2SP'
      KRESLT = 0
      IF (ABS(MA%MP(2)) > MEXPAB) THEN
          CALL FMARGS('IMM2SP   ',1,MA,MA,KRESLT)
      ENDIF
      IF (NTRACE /= 0) CALL IMNTR(2,MA,MA,1)
      IF (KRESLT /= 0) THEN

!             Here no valid result can be returned.  Set X to some value that the user is likely
!             to recognize as wrong.

          X = RUNKNO
          KFLAG = -4
          IF (MA%MP(2) /= MUNKNO) CALL FMWARN
          Y = X
          IF (NTRACE /= 0) CALL IMNTRR(1,Y,1)
          NCALL = NCALL - 1
          RETURN
      ENDIF

      NDSAVE = NDIG
      NDIG = MAX(2,INT(MA%MP(2)))
      ND2 = 2 - LOG(EPSILON(1.0D0))/DLOGMB
      IF (NDIG >= ND2) NDIG = ND2
      CALL FMMD(MA,Y)
      X = Y

      IF (NTRACE /= 0) CALL IMNTRR(1,Y,1)
      NDIG = NDSAVE
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE IMM2SP

      SUBROUTINE IMMAX(MA,MB,MC)

!  MC = MAX(MA,MB)

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC

      INTEGER :: KWRNSV
      LOGICAL, EXTERNAL :: IMCOMP
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC

      KFLAG = 0
      NCALL = NCALL + 1
      IF (KDEBUG == 1) CALL IMARGS('IMMAX    ',2,MA,MB)
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'IMMAX'
          CALL IMNTR(2,MA,MB,2)
      ENDIF

      KWRNSV = KWARN
      KWARN = 0
      IF (MA%MP(2) == MUNKNO .OR. MB%MP(2) == MUNKNO) THEN
          CALL IMST2M('UNKNOWN',MC)
          KFLAG = -4
      ELSE IF (IMCOMP(MA,'<',MB)) THEN
          CALL IMEQ(MB,MC)
      ELSE
          CALL IMEQ(MA,MC)
      ENDIF

      IF (MC%MP(2) <= 1) MC%MP(4) = 0
      KWARN = KWRNSV
      IF (NTRACE /= 0) CALL IMNTR(1,MC,MC,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE IMMAX

      SUBROUTINE IMMIN(MA,MB,MC)

!  MC = MIN(MA,MB)

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC

      INTEGER :: KWRNSV
      LOGICAL, EXTERNAL :: IMCOMP
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC

      KFLAG = 0
      NCALL = NCALL + 1
      IF (KDEBUG == 1) CALL IMARGS('IMMIN    ',2,MA,MB)
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'IMMIN'
          CALL IMNTR(2,MA,MB,2)
      ENDIF

      KWRNSV = KWARN
      KWARN = 0
      IF (MA%MP(2) == MUNKNO .OR. MB%MP(2) == MUNKNO) THEN
          CALL IMST2M('UNKNOWN',MC)
          KFLAG = -4
      ELSE IF (IMCOMP(MA,'>',MB)) THEN
          CALL IMEQ(MB,MC)
      ELSE
          CALL IMEQ(MA,MC)
      ENDIF

      IF (MC%MP(2) <= 1) MC%MP(4) = 0
      KWARN = KWRNSV
      IF (NTRACE /= 0) CALL IMNTR(1,MC,MC,1)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE IMMIN

      SUBROUTINE IMMOD(MA,MB,MC)

!  MC = MOD(MA,MB)

!  Use IMDIVR if both INT(MA/MB) and MOD(MA,MB) are needed.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC
      INTEGER :: NDSAVE,RESULT_SIZE
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      TYPE(MULTI) :: MXY(2)

      RESULT_SIZE = MIN(MA%MP(2),MB%MP(2)) + 3
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 5
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NCALL = NCALL + 1
      IF (KDEBUG == 1) CALL IMARGS('IMMOD    ',2,MA,MB)
      KFLAG = 0
      NDSAVE = NDIG
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'IMMOD'
          CALL IMNTR(2,MA,MB,2)
      ENDIF

      IF (MA%MP(2) == MUNKNO .OR. MB%MP(2) == MUNKNO) THEN
          CALL IMST2M('UNKNOWN',MC)
          KFLAG = -4
          GO TO 110
      ENDIF

      CALL IMDIVR(MA,MB,MXY(1),MXY(2))
      CALL IMEQ(MXY(2),MC)

      IF (MC%MP(2) == MUNKNO) THEN
          KFLAG = -4
          NAMEST(NCALL) = 'IMMOD'
          CALL FMWARN
      ENDIF

  110 IF (MC%MP(2) <= 1) MC%MP(4) = 0
      IF (NTRACE /= 0) CALL IMNTR(1,MC,MC,1)
      NCALL = NCALL - 1
      NDIG = NDSAVE
      RETURN
      END SUBROUTINE IMMOD

      SUBROUTINE IMMODI(MA,IMOD,IREM)

!  Internal routine to return integer IREM = mod( MA , IMOD ).
!  ABS(IMOD) should be less than MAXINT/MBASE for faster mod calculation.

!  MA is a multiple precision IM integer.
!  IMOD and IREM are one-word integers.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,T1,T2
      INTEGER :: IMOD,IREM,J,N
      REAL (KIND(1.0D0)) :: MMOD, MREM

      MMOD = ABS(IMOD)
      N = MA%MP(2)

!             If abs(imod) is more than than MAXINT/MBASE, do it the hard way, since the
!             MBASE*MREM in the fast method could be too big to represent exactly in d.p.

      IF (MMOD >= MAXINT/MBASE) THEN
          CALL IMI2M(ABS(IMOD),T1)
          CALL IMMOD(MA,T1,T2)
          CALL IMM2I(T2,IREM)
      ELSE
          MREM = 0
          DO J = 3, N+2
             MREM = MOD( MBASE*MREM + MA%MP(J) , MMOD )
          ENDDO
      ENDIF

      IREM = MREM
      IF (MA%MP(1) < 0) IREM = -IREM

      END SUBROUTINE IMMODI

      SUBROUTINE IMMPY(MA,MB,MC)

!  MC = MA * MB

!  This routine performs the trace printing.  IMMPY2 is used to do the arithmetic.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC

      NCALL = NCALL + 1
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'IMMPY'
          CALL IMNTR(2,MA,MB,2)

          CALL IMMPY2(MA,MB,MC)

          CALL IMNTR(1,MC,MC,1)
      ELSE
          CALL IMMPY2(MA,MB,MC)
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE IMMPY

      SUBROUTINE IMMPY2(MA,MB,MC)

!  MC = MA * MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC

      REAL (KIND(1.0D0)) :: MDAB
      INTEGER :: NDSAVE,RESULT_SIZE
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC

      IF (MA%MP(2) >= MEXPOV .OR. MB%MP(2) >= MEXPOV) THEN
          RESULT_SIZE = 5
      ELSE
          RESULT_SIZE = MA%MP(2) + MB%MP(2) + 4
      ENDIF
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      IF (KDEBUG == 1) CALL IMARGS('IMMPY    ',2,MA,MB)
      KFLAG = 0
      NDSAVE = NDIG

      IF (MA%MP(2) <= 1) THEN
          IF (MB%MP(2) > 1) GO TO 110
          MDAB = MA%MP(1) * MA%MP(3) * MB%MP(1) * MB%MP(3)
          IF (ABS(MDAB) < MBASE) THEN
              MC%MP(2) = 1
              IF (MDAB == 0) MC%MP(2) = 0
              IF (MDAB >= 0) THEN
                  MC%MP(3) = MDAB
                  MC%MP(1) = 1
              ELSE
                  MC%MP(3) = -MDAB
                  MC%MP(1) = -1
              ENDIF
              MC%MP(4) = 0
              GO TO 120
          ELSE IF (ABS(MDAB) < MBASE*MBASE) THEN
              MC%MP(2) = 2
              IF (MDAB >= 0) THEN
                  MC%MP(3) = AINT (MDAB/MBASE)
                  MC%MP(4) = ABS(MDAB - MBASE*MC%MP(3))
                  MC%MP(1) = 1
              ELSE
                  MC%MP(3) = AINT (-MDAB/MBASE)
                  MC%MP(4) = ABS(-MDAB - MBASE*MC%MP(3))
                  MC%MP(1) = -1
              ENDIF
              GO TO 120
          ENDIF
      ENDIF

!             Check for special cases.

  110 IF (MA%MP(2) == MUNKNO .OR. MB%MP(2) == MUNKNO) THEN
          KFLAG = -4
          CALL IMI2M2(0,MC)
          MC%MP(2) = MUNKNO
          MC%MP(3) = 1
          GO TO 120
      ENDIF
      IF (MA%MP(3) == 0 .OR. MB%MP(3) == 0) THEN
          MC%MP(1) = 1
          MC%MP(2) = 0
          MC%MP(3) = 0
          MC%MP(4) = 0
          GO TO 120
      ENDIF
      IF (MA%MP(2) == MEXPOV .OR. MB%MP(2) == MEXPOV) THEN
          KFLAG = -5
          IF (MA%MP(1)*MB%MP(1) < 0) THEN
              CALL IMI2M2(-1,MC)
              MC%MP(2) = MEXPOV
              MC%MP(3) = 1
          ELSE
              CALL IMI2M2(1,MC)
              MC%MP(2) = MEXPOV
              MC%MP(3) = 1
          ENDIF
          GO TO 120
      ENDIF
      IF (MA%MP(2) < 0 .OR. MB%MP(2) < 0) THEN
          KFLAG = -4
          NAMEST(NCALL) = 'IMMPY'
          CALL FMWARN
          CALL IMI2M2(0,MC)
          MC%MP(2) = MUNKNO
          MC%MP(3) = 1
          GO TO 120
      ENDIF
      IF (MB%MP(2) == 1 .AND. MB%MP(3) == 1 .AND. MB%MP(1) == 1) THEN
          CALL IMEQ(MA,MC)
          GO TO 120
      ELSE IF (MB%MP(2) == 1 .AND. MB%MP(3) == 1 .AND. MB%MP(1) == -1) THEN
          CALL IMEQ(MA,MC)
          IF (MC%MP(2) /= MUNKNO .AND. MC%MP(3) /= 0)  &
              MC%MP(1) = -MC%MP(1)
          GO TO 120
      ELSE IF (MA%MP(2) == 1 .AND. MA%MP(3) == 1 .AND. MA%MP(1) == 1) THEN
          CALL IMEQ(MB,MC)
          GO TO 120
      ELSE IF (MA%MP(2) == 1 .AND. MA%MP(3) == 1 .AND. MA%MP(1) == -1) THEN
          CALL IMEQ(MB,MC)
          IF (MC%MP(2) /= MUNKNO .AND. MC%MP(3) /= 0)  &
              MC%MP(1) = -MC%MP(1)
          GO TO 120
      ENDIF
      NDIG = INT(MA%MP(2) + MB%MP(2))

      IF (NDIG < 2) NDIG = 2
      CALL IMMPY3(MA,MB)

!             Transfer to MC and fix the sign of the result.

      NDIG = MWA%MP(2)
      IF (NDIG < 2) NDIG = 2
      IF (MA%MP(1)*MB%MP(1) < 0) THEN
          CALL FMMOVE(MWA,MC)
          MC%MP(1) = -1
      ELSE
          CALL FMMOVE(MWA,MC)
          MC%MP(1) = 1
      ENDIF

  120 IF (MC%MP(2) <= 1) MC%MP(4) = 0
      NDIG = NDSAVE
      RETURN
      END SUBROUTINE IMMPY2

      SUBROUTINE IMMPY3(MA,MB)

!  Internal multiplication of MA*MB.  The result is returned in MWA.  Both MA and MB are positive.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB

      REAL (KIND(1.0D0)) :: MAXMWA,MBJ,MBP1,MKT,MMAX
      INTEGER :: J,JM1,K,KB,KL,KLMA,KLMB,N1,ND,NMETHD,NZDA,NZDB,RESULT_SIZE
      TYPE(MULTI) :: MXY(2)
      INTENT (IN) :: MA,MB

      RESULT_SIZE = 2*(MA%MP(2)+MB%MP(2)) + 30
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 32
      RESULT_SIZE = MAX(32,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      N1 = NDIG + 1
      MWA%MP(2) = MA%MP(2) + MB%MP(2)
      MWA%MP(N1+2) = 0

!             Check for using an FFT-based method if precision is very high.

      ND = MAX(MA%MP(2),MB%MP(2))
      IF (LOG(7.0D0*ND/3.0D0) < LOG(7.0D3/3.0D0)*MIN(MA%MP(2),MB%MP(2))/1.0D3) THEN
          NZDA = INT(MA%MP(2))
          NZDB = INT(MB%MP(2))
          DO J = 2, INT(MB%MP(2))
             IF (MB%MP(J+2) == 0) NZDB = NZDB - 1
          ENDDO
          IF (REAL(NZDA)*NZDB < 65.0*ND*LOG(REAL(ND))) THEN
              NMETHD = 1
          ELSE
              NMETHD = 2
          ENDIF
      ELSE
          NMETHD = 1
      ENDIF
      IF (NMETHD == 2) THEN
          ND = NDIG
          NDIG = MAX(MA%MP(2),MB%MP(2))
          CALL IMI2FM(MA,MXY(1))
          CALL IMI2FM(MB,MXY(2))
          CALL FMMPYFFT(MXY(1),MXY(2))
          NDIG = ND
          RETURN
      ENDIF

!             The multiplication loop begins here.

!             MAXMWA is an upper bound on the size of values in MWA divided by (MBASE-1).  It is
!                    used to determine whether to normalize before the next digit is multiplied.

      MBP1 = MBASE + 1
      MMAX = INTMAX - MBASE
      MMAX = MIN((MAXINT/MBP1 - MBP1),MMAX)
      MBJ = MB%MP(3)
      MWA%MP(3) = 0
      KLMA = INT(MA%MP(2))
      DO K = KLMA+3, N1
         MWA%MP(K+1) = 0
      ENDDO

!             (Inner Loop)

      DO K = 3, KLMA+2
         MWA%MP(K+1) = MA%MP(K)*MBJ
      ENDDO
      MAXMWA = MBJ
      IF (MAXMWA > MMAX) THEN
          MAXMWA = 0
          KL = KLMA + 1
          DO KB = KL+1, 3, -1
             MKT = INT (MWA%MP(KB+1)/MBASE)
             MWA%MP(KB) = MWA%MP(KB) + MKT
             MWA%MP(KB+1) = MWA%MP(KB+1) - MKT*MBASE
          ENDDO
      ENDIF
      KLMB = INT(MB%MP(2))
      DO J = 3, KLMB+1
         MBJ = MB%MP(J+1)
         IF (MBJ /= 0) THEN
             MAXMWA = MAXMWA + MBJ
             JM1 = J - 1
             KL = KLMA + 1

!                       Major (Inner Loop)

             DO K = J+2, J+KLMA+1
                MWA%MP(K) = MWA%MP(K) + MA%MP(K-JM1)*MBJ
             ENDDO
         ENDIF

         IF (MAXMWA > MMAX) THEN
             MAXMWA = 0

!                       Here normalization is only required for the range of digits currently
!                       changing in MWA.

             DO KB = JM1+KL, JM1+2, -1
                MKT = INT (MWA%MP(KB+1)/MBASE)
                MWA%MP(KB) = MWA%MP(KB) + MKT
                MWA%MP(KB+1) = MWA%MP(KB+1) - MKT*MBASE
             ENDDO
         ENDIF
      ENDDO

!             Perform the final normalization.  (Inner Loop)

      DO KB = N1+1, 4, -1
         MKT = INT (MWA%MP(KB)/MBASE)
         MWA%MP(KB-1) = MWA%MP(KB-1) + MKT
         MWA%MP(KB) = MWA%MP(KB) - MKT*MBASE
      ENDDO

      RETURN
      END SUBROUTINE IMMPY3

      SUBROUTINE IMMPYI(MA,IVAL,MB)

!  MB = MA * IVAL

!  This routine performs the trace printing.  IMMPYI2 is used to do the arithmetic.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTEGER :: IVAL
      INTENT (IN) :: MA,IVAL
      INTENT (INOUT) :: MB

      NCALL = NCALL + 1
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'IMMPYI'
          CALL IMNTR(2,MA,MA,1)
          CALL IMNTRI(2,IVAL,0)

          CALL IMMPYI2(MA,IVAL,MB)

          CALL IMNTR(1,MB,MB,1)
      ELSE
          CALL IMMPYI2(MA,IVAL,MB)
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE IMMPYI

      SUBROUTINE IMMPYI2(MA,IVAL,MB)

!  MB = MA * IVAL

!  Multiplication by a one word integer.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      REAL (KIND(1.0D0)) :: MAS,MCARRY,MDAB,MKT,MVAL
      DOUBLE PRECISION :: DT
      INTEGER :: IVAL,J,KA,KB,KC,KSHIFT,N1,NDSAVE,NMVAL,NV2,RESULT_SIZE
      INTENT (IN) :: MA,IVAL
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: MXY(2)

      DT = LOG(DBLE(ABS(IVAL)+1))/DLOGMB + 1
      RESULT_SIZE = MA%MP(2) + DT + 4
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 5
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      RESULT_SIZE = 2*RESULT_SIZE + 30
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 32
      RESULT_SIZE = MAX(32,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      IF (KDEBUG == 1) CALL IMARGS('IMMPYI   ',1,MA,MA)
      KFLAG = 0
      NDSAVE = NDIG
      MAS = MA%MP(1)

      IF (MA%MP(2) <= 1) THEN
          MDAB = MA%MP(1) * MA%MP(3) * IVAL
          IF (ABS(MDAB) < MBASE) THEN
              MB%MP(2) = 1
              IF (MDAB == 0) MB%MP(2) = 0
              MB%MP(1) = 1
              IF (MDAB < 0) MB%MP(1) = -1
              MB%MP(3) = ABS(MDAB)
              MB%MP(4) = 0
              GO TO 120
          ELSE IF (ABS(MDAB) < MBASE*MBASE) THEN
              MB%MP(2) = 2
              MB%MP(1) = 1
              IF (MDAB < 0) MB%MP(1) = -1
              MDAB = ABS(MDAB)
              MB%MP(3) = AINT (MDAB/MBASE)
              MB%MP(4) = MDAB - MBASE*MB%MP(3)
              GO TO 120
          ENDIF
      ENDIF

!             Check for special cases.

      IF (MA%MP(2) < 0) THEN
          KFLAG = -4
          NAMEST(NCALL) = 'IMMPYI'
          CALL FMWARN
          CALL IMI2M2(0,MB)
          MB%MP(2) = MUNKNO
          MB%MP(3) = 1
          GO TO 120
      ENDIF
      IF (MA%MP(2) == MUNKNO) THEN
          KFLAG = -4
          CALL IMI2M2(0,MB)
          MB%MP(2) = MUNKNO
          MB%MP(3) = 1
          GO TO 120
      ELSE IF (IVAL == 0) THEN
          CALL IMI2M2(0,MB)
          GO TO 120
      ELSE IF (IVAL == 1) THEN
          CALL IMEQ(MA,MB)
          GO TO 120
      ELSE IF (IVAL == -1) THEN
          CALL IMEQ(MA,MB)
          IF (MB%MP(2) /= MUNKNO .AND. MB%MP(3) /= 0)  &
              MB%MP(1) = -MB%MP(1)
          GO TO 120
      ELSE IF (MA%MP(2) == 1 .AND. MA%MP(3)*MA%MP(1) == 1) THEN
          CALL IMI2M2(IVAL,MB)
          GO TO 120
      ELSE IF (MA%MP(2) == 1 .AND. MA%MP(3)*MA%MP(1) == -1) THEN
          CALL IMI2M2(IVAL,MB)
          IF (MB%MP(2) /= MUNKNO .AND. MB%MP(3) /= 0)  &
              MB%MP(1) = -MB%MP(1)
          GO TO 120
      ELSE IF (MA%MP(2) == MEXPOV) THEN
          KFLAG = -5
          CALL IMI2M2(1,MB)
          MB%MP(2) = MEXPOV
          MB%MP(3) = 1
          GO TO 110
      ENDIF

!             Work with positive numbers.

      MVAL = ABS(IVAL)
      NMVAL = INT(MVAL)
      NV2 = NMVAL - 1
      NDIG = INT(MA%MP(2))
      IF (NDIG < 2) NDIG = 2
      N1 = NDIG + 1

!             To leave room for normalization, shift the product to the right KSHIFT places in MWA.

      KSHIFT = INT((LOG(DBLE(MA%MP(3)+1)*DBLE(MVAL)))/DLOGMB)

!             If IVAL is too big, use IMMPY2.

      IF (KSHIFT > NDIG .OR. MVAL > MAXINT/MBASE .OR.  &
          NMVAL /= ABS(IVAL) .OR. NV2 /= ABS(IVAL)-1) THEN
          CALL IMI2M2(IVAL,MXY(1))
          CALL IMMPY2(MA,MXY(1),MB)
          GO TO 120
      ENDIF

      MWA%MP(2) = MA%MP(2) + KSHIFT
      KA = 2 + KSHIFT
      KB = N1 + KSHIFT
      KC = NDIG + 5
      DO J = KB, KC
         MWA%MP(J+1) = 0
      ENDDO

      MCARRY = 0

!             This is the main multiplication loop.

      DO J = KB, KA, -1
         MKT = MA%MP(J-KSHIFT+1)*MVAL + MCARRY
         MCARRY = INT (MKT/MBASE)
         MWA%MP(J+1) = MKT - MCARRY*MBASE
      ENDDO

!             Resolve the final carry.

      DO J = KA-1, 2, -1
         MKT = INT (MCARRY/MBASE)
         MWA%MP(J+1) = MCARRY - MKT*MBASE
         MCARRY = MKT
      ENDDO

!             Now the first significant digit in the product is in
!             MWA%MP(3) or MWA%MP(4).

      IF (MWA%MP(3) == 0) THEN
          MB%MP(2) = MWA%MP(2) - 1
          DO J = 3, KB
             MB%MP(J) = MWA%MP(J+1)
          ENDDO
      ELSE
          MB%MP(2) = MWA%MP(2)
          DO J = 2, KB
             MB%MP(J+1) = MWA%MP(J+1)
          ENDDO
      ENDIF

!             Put the sign on the result.

  110 MB%MP(1) = 1
      IF ((IVAL > 0 .AND. MAS < 0) .OR. (IVAL < 0 .AND.MAS > 0)) MB%MP(1) = -1

  120 IF (MB%MP(2) <= 1) MB%MP(4) = 0
      NDIG = NDSAVE
      RETURN
      END SUBROUTINE IMMPYI2

      SUBROUTINE IMMPYM(MA,MB,MC,MD)

!  MD = MA * MB mod MC

!  This routine is slightly faster than calling IMMPY and IMMOD separately.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC,MD
      REAL (KIND(1.0D0)) :: MAS,MAXMWA,MBP1,MBS,MC1,MCARRY,MDC,MDAB,MKT,MLMAX,MQD
      DOUBLE PRECISION :: XB,XBASE,XBR,XMWA
      INTEGER :: J,JB,JL,K,KA,KB,KL,KLTFLG,KPTMWA,N1,NA1,NC1,NDSAVE,NGUARD,NL,NMCWDS,NTRSAV
      INTEGER :: RESULT_SIZE
      LOGICAL, EXTERNAL :: IMCOMP
      INTENT (IN) :: MA,MB,MC
      INTENT (INOUT) :: MD
      TYPE(MULTI) :: MXY(3)

      RESULT_SIZE = MC%MP(2) + 4
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 5
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MD%MP)) THEN
          ALLOCATE(MD%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MD%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MD%MP)
          ALLOCATE(MD%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      RESULT_SIZE = 2*(MA%MP(2)+MB%MP(2)) + 30
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 32
      RESULT_SIZE = MAX(32,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NCALL = NCALL + 1
      IF (KDEBUG == 1) CALL IMARGS('IMMPYM   ',2,MA,MB)
      NDSAVE = NDIG
      KFLAG = 0
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'IMMPYM'
          CALL IMNTR(2,MA,MB,2)
          IF (ABS(NTRACE) >= 2 .AND. NCALL <= LVLTRC) THEN
              IF (NTRACE < 0) THEN
                  NDIG = MAX(2,INT(MC%MP(2)))
                  IF (ABS(MA%MP(2)) >= MEXPOV) NDIG = 2
                  NTRSAV = NTRACE
                  CALL IMNTRJ(MC,NDIG)
                  NTRACE = NTRSAV
                  NDIG = NDSAVE
              ELSE
                  CALL IMPRNT(MC)
              ENDIF
          ENDIF
      ENDIF

      IF (MA%MP(2) <= 1) THEN
          IF (MB%MP(2) > 1) GO TO 110
          IF (MA%MP(2) < 0 .OR. MB%MP(2) < 0) GO TO 110
          MDAB = MA%MP(1) * MA%MP(3) * MB%MP(1) * MB%MP(3)
          IF (MC%MP(2) <= 2) THEN
              IF (MC%MP(3) == 0) GO TO 110
              IF (MC%MP(2) <= 1) THEN
                  MDC = MC%MP(1) * MC%MP(3)
              ELSE
                  MDC = MC%MP(1) * (MC%MP(3)*MBASE + MC%MP(4))
              ENDIF
              MDAB = MOD(MDAB,MDC)
          ENDIF
          IF (ABS(MDAB) < MBASE) THEN
              MD%MP(2) = 1
              IF (MDAB == 0) MD%MP(2) = 0
              MD%MP(1) = 1
              IF (MDAB < 0) MD%MP(1) = -1
              MD%MP(3) = ABS(MDAB)
              MD%MP(4) = 0
              GO TO 160
          ELSE IF (ABS(MDAB) < MBASE*MBASE) THEN
              MD%MP(2) = 2
              MD%MP(1) = 1
              IF (MDAB < 0) MD%MP(1) = -1
              MDAB = ABS(MDAB)
              MD%MP(3) = AINT (MDAB/MBASE)
              MD%MP(4) = MDAB - MBASE*MD%MP(3)
              GO TO 160
          ENDIF
      ENDIF

!             Check for special cases.

  110 IF (MA%MP(2) == MUNKNO .OR. MB%MP(2) == MUNKNO .OR.  &
          MC%MP(2) == MUNKNO) THEN
          KFLAG = -4
          CALL IMST2M('UNKNOWN',MD)
          GO TO 170
      ELSE IF (MC%MP(3) == 0 .OR. MA%MP(2) < 0 .OR. MB%MP(2) < 0 .OR.  &
               MC%MP(2) < 0 .OR. MA%MP(2) >= MEXPOV .OR.               &
               MB%MP(2) >= MEXPOV .OR. MC%MP(2) >= MEXPOV) THEN
          KFLAG = -4
          NAMEST(NCALL) = 'IMMPYM'
          CALL FMWARN
          CALL IMST2M('UNKNOWN',MD)
          GO TO 170
      ELSE IF (MA%MP(3) == 0 .OR. MB%MP(3) == 0) THEN
          CALL IMI2M(0,MD)
          GO TO 170
      ELSE IF (MC%MP(2) == 1 .AND. MC%MP(3) == 1) THEN
          CALL IMI2M(0,MD)
          GO TO 170
      ELSE IF (MB%MP(2) == 1 .AND. MB%MP(3) == 1 .AND. MB%MP(1) == 1) THEN
          CALL IMMOD(MA,MC,MD)
          GO TO 160
      ELSE IF (MB%MP(2) == 1 .AND. MB%MP(3) == 1 .AND. MB%MP(1) == -1) THEN
          CALL IMMOD(MA,MC,MD)
          IF (MD%MP(2) /= MUNKNO .AND. MD%MP(3) /= 0)  &
              MD%MP(1) = -MD%MP(1)
          GO TO 160
      ELSE IF (MA%MP(2) == 1 .AND. MA%MP(3) == 1 .AND. MA%MP(1) == 1) THEN
          CALL IMMOD(MB,MC,MD)
          GO TO 160
      ELSE IF (MA%MP(2) == 1 .AND. MA%MP(3) == 1 .AND. MA%MP(1) == -1) THEN
          CALL IMMOD(MB,MC,MD)
          IF (MD%MP(2) /= MUNKNO .AND. MD%MP(3) /= 0)  &
              MD%MP(1) = -MD%MP(1)
          GO TO 160
      ENDIF

      NDIG = INT(MA%MP(2) + MB%MP(2))
      IF (NDIG < 2) NDIG = 2

!             Check for using an FFT-based method if precision is very high.

      IF (NDIG >= 1000) THEN
          CALL IMMPY(MA,MB,MXY(1))
          CALL IMMOD(MXY(1),MC,MXY(2))
          CALL IMEQ(MXY(2),MD)
          GO TO 160
      ENDIF

!             Save the sign of MA and MB and then work only with positive numbers.

      MAS = MA%MP(1)
      MBS = MB%MP(1)

      N1 = NDIG + 1

!             It is faster if the second argument is the one with fewer digits.

      IF (MA%MP(2) < MB%MP(2)) THEN
          CALL IMMPY3(MB,MA)
      ELSE
          CALL IMMPY3(MA,MB)
      ENDIF

!             Now do the division to find MWA mod MC.

      KLTFLG = 0
      IF (MWA%MP(3) == 0) THEN
          MWA%MP(2) = MWA%MP(2) - 1
      ELSE
          DO J = N1, 2, -1
             MWA%MP(J+2) = MWA%MP(J+1)
          ENDDO
          MWA%MP(3) = 0
      ENDIF
      KL = INT(MC%MP(2))
      RESULT_SIZE = MAX(MA%MP(2),MB%MP(2),MC%MP(2)) + 4
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 5
      RESULT_SIZE = MAX(5,RESULT_SIZE)
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
      DO J = 1, KL+2
         MXY(1)%MP(J) = MC%MP(J)
      ENDDO
      MXY(1)%MP(1) = 1
      IF (MWA%MP(2) == MXY(1)%MP(2) .AND.  &
          ABS(MWA%MP(4)) <= MXY(1)%MP(3)) THEN
          DO J = 4, N1+1
             MXY(2)%MP(J) = MWA%MP(J+1)
          ENDDO
          MXY(2)%MP(3) = ABS(MWA%MP(4))
          MXY(2)%MP(2) = MWA%MP(2)
          MXY(2)%MP(1) = 1
          IF (IMCOMP(MXY(2),'==',MXY(1))) THEN
              KLTFLG = 2
          ELSE IF (IMCOMP(MXY(2),'<',MXY(1))) THEN
              KLTFLG = 1
          ENDIF
      ENDIF
      IF (MWA%MP(2) < MC%MP(2) .OR. KLTFLG >= 1) THEN
          IF (KLTFLG /= 2) THEN
              DO J = 3, N1+1
                 MD%MP(J) = MWA%MP(J+1)
              ENDDO
              MD%MP(2) = MWA%MP(2)
          ELSE
              CALL IMI2M(0,MD)
          ENDIF
          GO TO 150
      ENDIF

      NDIG = INT(MWA%MP(2))
      IF (NDIG < 2) NDIG = 2

!             NGUARD is the number of guard digits used.

      NGUARD = 1
      NA1 = INT(MWA%MP(2)) + 1
      NC1 = INT(MC%MP(2)) + 1
      MWA%MP(2) = MWA%MP(2) - MC%MP(2) + 1
      NL = NA1 + NGUARD + 3
      DO J = NA1+2, NL
         MWA%MP(J+1) = 0
      ENDDO

!             Work only with positive numbers.

      MC1 = MC%MP(2)

!             NMCWDS is the number of words of MC used to compute the estimated quotient digit MQD.

      NMCWDS = 4
      IF (MBASE < 100) NMCWDS = 7

!             XB is an approximation of MC used in estimating the quotient digits.

      XBASE = DBLE(MBASE)
      XB = 0
      JL = NMCWDS
      IF (JL <= NC1) THEN
          DO J = 2, JL
             XB = XB*XBASE + DBLE(MC%MP(J+1))
          ENDDO
      ELSE
          DO J = 2, JL
             IF (J <= NC1) THEN
                 XB = XB*XBASE + DBLE(MC%MP(J+1))
             ELSE
                 XB = XB*XBASE
             ENDIF
          ENDDO
      ENDIF
      IF (JL+1 <= NC1) THEN
          XB = XB + DBLE(MC%MP(JL+2))/XBASE
      ENDIF
      XBR = 1.0D0/XB

!             MLMAX determines when to normalize all of MWA.

      MBP1 = MBASE + 1
      MLMAX = MAXINT/MBP1
      MKT = INTMAX - MBASE
      MLMAX = MIN(MLMAX,MKT)

!             MAXMWA is an upper bound on the size of values in MWA divided by MBASE-1.
!             It is used to determine whether normalization can be postponed.

      MAXMWA = 0

!             KPTMWA points to the next digit in the quotient.

      KPTMWA = 2

!             This is the start of the division loop.

!             XMWA is an approximation of the active part of MWA used in estimating quotient digits.

  120 KL = KPTMWA + NMCWDS - 1
      IF (KL <= NL) THEN
          XMWA = ((DBLE(MWA%MP(KPTMWA+1))*XBASE + DBLE(MWA%MP(KPTMWA+2)))*XBASE  &
                 + DBLE(MWA%MP(KPTMWA+3)))*XBASE + DBLE(MWA%MP(KPTMWA+4))
          DO J = KPTMWA+4, KL
             XMWA = XMWA*XBASE + DBLE(MWA%MP(J+1))
          ENDDO
      ELSE
          XMWA = DBLE(MWA%MP(KPTMWA+1))
          DO J = KPTMWA+1, KL
             IF (J <= NL) THEN
                 XMWA = XMWA*XBASE + DBLE(MWA%MP(J+1))
             ELSE
                 XMWA = XMWA*XBASE
             ENDIF
          ENDDO
      ENDIF

!             MQD is the estimated quotient digit.

      MQD = AINT(XMWA*XBR)
      IF (MQD < 0) MQD = MQD - 1

      IF (MQD > 0) THEN
          MAXMWA = MAXMWA + MQD
      ELSE
          MAXMWA = MAXMWA - MQD
      ENDIF

!             See if MWA must be normalized.

      KA = KPTMWA + 1
      KB = KA + INT(MC1) - 1
      IF (MAXMWA >= MLMAX) THEN
          DO J = KB, KA, -1
             IF (MWA%MP(J+1) < 0) THEN
                 MCARRY = INT((-MWA%MP(J+1)-1)/MBASE) + 1
                 MWA%MP(J+1) = MWA%MP(J+1) + MCARRY*MBASE
                 MWA%MP(J) = MWA%MP(J) - MCARRY
             ELSE IF (MWA%MP(J+1) >= MBASE) THEN
                 MCARRY = -INT(MWA%MP(J+1)/MBASE)
                 MWA%MP(J+1) = MWA%MP(J+1) + MCARRY*MBASE
                 MWA%MP(J) = MWA%MP(J) - MCARRY
             ENDIF
          ENDDO
          XMWA = 0
          IF (KL <= NL) THEN
              DO J = KPTMWA, KL
                 XMWA = XMWA*XBASE + DBLE(MWA%MP(J+1))
              ENDDO
          ELSE
              DO J = KPTMWA, KL
                 IF (J <= NL) THEN
                     XMWA = XMWA*XBASE + DBLE(MWA%MP(J+1))
                 ELSE
                     XMWA = XMWA*XBASE
                 ENDIF
              ENDDO
          ENDIF
          MQD = AINT(XMWA*XBR)
          IF (MQD < 0) MQD = MQD - 1
          IF (MQD > 0) THEN
              MAXMWA = MQD
          ELSE
              MAXMWA = -MQD
          ENDIF
      ENDIF

!             Subtract MQD*MC from MWA.

      JB = KA - 2
      IF (MQD /= 0) THEN

!             Major (Inner Loop)

          DO J = KA+1, KB+1
             MWA%MP(J) = MWA%MP(J) - MQD*MC%MP(J-JB)
          ENDDO
      ENDIF

      MWA%MP(KA+1) = MWA%MP(KA+1) + MWA%MP(KA)*MBASE
      MWA%MP(KPTMWA+1) = MQD

      KPTMWA = KPTMWA + 1
      IF (KPTMWA-2 < MWA%MP(2)) GO TO 120

!             Final normalization.

      KPTMWA = KPTMWA - 1
      DO J = KPTMWA, 3, -1
         IF (MWA%MP(J+1) < 0) THEN
             MCARRY = INT((-MWA%MP(J+1)-1)/MBASE) + 1
             MWA%MP(J+1) = MWA%MP(J+1) + MCARRY*MBASE
             MWA%MP(J) = MWA%MP(J) - MCARRY
         ELSE IF (MWA%MP(J+1) >= MBASE) THEN
             MCARRY = -INT(MWA%MP(J+1)/MBASE)
             MWA%MP(J+1) = MWA%MP(J+1) + MCARRY*MBASE
             MWA%MP(J) = MWA%MP(J) - MCARRY
         ENDIF
      ENDDO

  130 DO J = KPTMWA+INT(MC1), KPTMWA+2, -1
         IF (MWA%MP(J+1) < 0) THEN
             MCARRY = INT((-MWA%MP(J+1)-1)/MBASE) + 1
             MWA%MP(J+1) = MWA%MP(J+1) + MCARRY*MBASE
             MWA%MP(J) = MWA%MP(J) - MCARRY
         ELSE IF (MWA%MP(J+1) >= MBASE) THEN
             MCARRY = -INT(MWA%MP(J+1)/MBASE)
             MWA%MP(J+1) = MWA%MP(J+1) + MCARRY*MBASE
             MWA%MP(J) = MWA%MP(J) - MCARRY
         ENDIF
      ENDDO

!             Due to rounding, the remainder may not be between 0 and ABS(MC) here.
!             Correct if necessary.

      IF (MWA%MP(KA+1) < 0) THEN
          DO J = KA, KB
             MWA%MP(J+1) = MWA%MP(J+1) + MC%MP(J-JB+1)
          ENDDO
          GO TO 130
      ELSE IF (MWA%MP(KA+1) >= MBASE) THEN
          DO J = KA, KB
             MWA%MP(J+1) = MWA%MP(J+1) - MC%MP(J-JB+1)
          ENDDO
          GO TO 130
      ENDIF

      IF (MWA%MP(KPTMWA+2) /= 0) THEN
          DO J = 1, INT(MC1)
             MD%MP(J+2) = MWA%MP(J+KPTMWA+1)
          ENDDO
          MD%MP(2) = MC1
      ELSE
          DO J = 1, INT(MC1)
             IF (MWA%MP(J+KPTMWA+1) /= 0) THEN
                 DO K = J, INT(MC1)
                    MD%MP(K-J+3) = MWA%MP(K+KPTMWA+1)
                 ENDDO
                 MD%MP(2) = MC1 + 1 - J
                 GO TO 140
             ENDIF
          ENDDO
          MD%MP(2) = 0
          MD%MP(3) = 0
      ENDIF
  140 IF (MD%MP(2) <= 1) MD%MP(4) = 0

      IF (MD%MP(2) > MXY(1)%MP(2) .OR.    &
         (MD%MP(2) == MXY(1)%MP(2) .AND.  &
          ABS(MD%MP(3)) >= MXY(1)%MP(3))) THEN
          MD%MP(1) = 1
          IF (IMCOMP(MD,'>=',MXY(1))) THEN
              CALL IMSUB(MD,MXY(1),MXY(3))
              CALL IMEQ(MXY(3),MD)
          ENDIF
      ENDIF

  150 MD%MP(1) = 1
      IF (MAS*MBS < 0) THEN
          IF (MD%MP(2) /= MUNKNO .AND. MD%MP(3) /= 0)  &
              MD%MP(1) = -MD%MP(1)
      ENDIF

  160 IF (MD%MP(2) == MUNKNO) THEN
          KFLAG = -4
          NAMEST(NCALL) = 'IMMPYM'
          CALL FMWARN
      ENDIF

  170 IF (MD%MP(2) <= 1) MD%MP(4) = 0
      IF (NTRACE /= 0) CALL IMNTR(1,MD,MD,1)
      NCALL = NCALL - 1
      NDIG = NDSAVE
      RETURN
      END SUBROUTINE IMMPYM

      SUBROUTINE IMNTR(NTR,MA,MB,NARG)

!  Print IM numbers in base 10 format.  This is used for trace output from the IM routines.

!  NTR =  1 if a result of an IM call is to be printed.
!      =  2 to print input argument(s) to an IM call.

!  MA  -  the IM number to be printed.

!  MB  -  an optional second IM number to be printed.

!  NARG - the number of arguments.  NARG = 1 if only MA is to be printed, and NARG = 2 if both
!         MA and MB are to be printed.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTEGER :: NARG,NDSAVE,NTR,NTRSAV
      CHARACTER(9) :: NAME
      INTENT (IN) :: NTR,MA,MB,NARG

      IF (NTRACE == 0) RETURN
      IF (NCALL > LVLTRC) RETURN
      IF (NTR == 2 .AND. ABS(NTRACE) == 1) RETURN

      IF (NTR == 2) THEN
          NAME = NAMEST(NCALL)
          WRITE (KW,"(' Input to ',A)") TRIM(NAME)
      ELSE
          NAME = NAMEST(NCALL)
          IF (KFLAG == 0) THEN
              WRITE (KW,                                               &
                     "(' ',A,12X,'Call level =',I2,5X,'MBASE =',"  //  &
                     "I10)"                                            &
                    ) NAME,NCALL,INT(MBASE)
          ELSE
              WRITE (KW,                                              &
                     "(' ',A,3X,'Call level =',I2,4X,'MBASE =',"  //  &
                     "I10,4X,'KFLAG =',I3)"                           &
                    ) NAME,NCALL,INT(MBASE),KFLAG
          ENDIF
      ENDIF

      NDSAVE = NDIG
      IF (NTRACE < 0) THEN
          NDIG = MAX(2,INT(MA%MP(2)))
          IF (ABS(MA%MP(2)) >= MEXPOV) NDIG = 2
          NTRSAV = NTRACE
          CALL IMNTRJ(MA,NDIG)
          IF (NARG == 2) THEN
              NDIG = MAX(2,INT(MB%MP(2)))
              IF (ABS(MA%MP(2)) >= MEXPOV) NDIG = 2
              CALL IMNTRJ(MB,NDIG)
          ENDIF
          NTRACE = NTRSAV
      ENDIF

      IF (NTRACE > 0) THEN
          CALL IMPRNT(MA)
          IF (NARG == 2) CALL IMPRNT(MB)
      ENDIF

      NDIG = NDSAVE
      RETURN
      END SUBROUTINE IMNTR

      SUBROUTINE IMNTRI(NTR,N,KNAM)

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
          WRITE (KW,"(' Input to ',A)") TRIM(NAME)
      ENDIF
      IF (NTR == 1 .AND. KNAM > 0) THEN
          NAME = NAMEST(NCALL)
          IF (KFLAG == 0) THEN
              WRITE (KW,"(' ',A,12X,'Call level =',I2,5X,'MBASE =',I10)")  &
                     NAME,NCALL,INT(MBASE)
          ELSE
              WRITE (KW,                                              &
                     "(' ',A,3X,'Call level =',I2,4X,'MBASE =',"  //  &
                     "I10,4X,'KFLAG =',I3)"                           &
                    ) NAME,NCALL,INT(MBASE),KFLAG
          ENDIF
      ENDIF

      WRITE (KW,"(1X,I20)") N

      RETURN
      END SUBROUTINE IMNTRI

      SUBROUTINE IMNTRJ(MA,ND)

!  Print trace output in internal base MBASE format.  The number to be printed is in MA.

!  ND is the number of base MBASE digits to be printed.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTEGER :: ND

      CHARACTER(99) :: FORM
      INTEGER :: J,L,N
      INTENT (IN) :: MA,ND

      L = INT(LOG10(DBLE(MBASE-1))) + 2
      N = (KSWIDE-23)/L
      IF (N > 10) N = 5*(N/5)
      IF (ND <= N) THEN
          WRITE (FORM,"(' (1X,I19,I',I2,',',I3,'I',I2,') ')") L+2, N-1, L
      ELSE
          WRITE (FORM,                                                       &
                 "(' (1X,I19,I',I2,',',I3,'I',I2,'/(22X,',I3,'I',I2,')) ')"  &
                ) L+2, N-1, L, N, L
      ENDIF
      IF (INT(MA%MP(2)) >= 2) THEN
          WRITE (KW,*) '            Sign = ',INT(MA%MP(1)),  &
                       '  Exponent = ',INT(MA%MP(2)),'  Digits:'
          WRITE (FORM,*) '(13X,', N, 'I', L, ')'
          WRITE (KW,FORM) (INT(MA%MP(J)),J=3,ND+2)
      ELSE
          WRITE (KW,*) '            Sign = ',INT(MA%MP(1)),  &
                       '  Exponent = ',INT(MA%MP(2)),'  Digits:'
          WRITE (FORM,*) '(13X,', N, 'I', L, ')'
          WRITE (KW,FORM) INT(MA%MP(3)),(0,J=4,ND+2)
      ENDIF

      RETURN
      END SUBROUTINE IMNTRJ

      SUBROUTINE IMNTRR(NTR,X,KNAM)

!  Internal routine for trace output of real variables.

!  NTR - 1 for output values
!        2 for input values

!  X   - Double precision value to be printed if NX == 1

!  KNAM - Positive if the routine name is to be printed.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: NTR,KNAM
      DOUBLE PRECISION :: X

      CHARACTER(9) :: NAME
      INTENT (IN) :: NTR,X,KNAM

      IF (NTRACE == 0) RETURN
      IF (NCALL > LVLTRC) RETURN
      IF (NTR == 2 .AND. ABS(NTRACE) == 1) RETURN

      IF (NTR == 2 .AND. KNAM > 0) THEN
          NAME = NAMEST(NCALL)
          WRITE (KW,"(' Input to ',A)") TRIM(NAME)
      ENDIF
      IF (NTR == 1 .AND. KNAM > 0) THEN
          NAME = NAMEST(NCALL)
          IF (KFLAG == 0) THEN
              WRITE (KW,"(' ',A,12X,'Call level =',I2,5X,'MBASE =',I10)") NAME,NCALL,INT(MBASE)
          ELSE
              WRITE (KW,                                              &
                     "(' ',A,3X,'Call level =',I2,4X,'MBASE =',"  //  &
                     "I10,4X,'KFLAG =',I3)"                           &
                    ) NAME,NCALL,INT(MBASE),KFLAG
          ENDIF
      ENDIF

      WRITE (KW,"(1X,D30.20)") X

      RETURN
      END SUBROUTINE IMNTRR

      SUBROUTINE IMOUT(MA,LINE,LB)

!  Convert an integer multiple precision number to a character array for output.

!  MA   is an IM number to be converted to an A1 character array in base 10 format.
!  LINE is the character(1) array in which the result is returned.
!  LB   is the length of LINE.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: JF1SAV,JF2SAV,LB,NDSAVE
      CHARACTER :: LINE(LB)
      TYPE(MULTI) :: MA
      INTENT (IN) :: MA,LB
      INTENT (INOUT) :: LINE

      NCALL = NCALL + 1
      IF (KDEBUG == 1) CALL IMARGS('IMOUT    ',1,MA,MA)
      KFLAG = 0
      NDSAVE = NDIG
      NAMEST(NCALL) = 'IMOUT'

      NDSAVE = NDIG
      JF1SAV = JFORM1
      JF2SAV = JFORM2
      JFORM1 = 2
      JFORM2 = 0
      NDIG = MAX(2,INT(MA%MP(2)))
      IF (ABS(MA%MP(2)) >= MEXPOV) NDIG = 2
      CALL FMOUT(MA,LINE,LB)

      NDIG = NDSAVE
      JFORM1 = JF1SAV
      JFORM2 = JF2SAV
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE IMOUT

      SUBROUTINE IMPACK(MA,MP)

!  MA is packed two base MBASE digits per word and returned in MP.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MP

      INTEGER :: J,KP,KMA1,RESULT_SIZE
      INTENT (IN) :: MA
      INTENT (INOUT) :: MP

      RESULT_SIZE = MA%MP(2)/2 + 4
      IF (ABS(RESULT_SIZE) >= MEXPOV/2) RESULT_SIZE = 5
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MP%MP)) THEN
          ALLOCATE(MP%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MP%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MP%MP)
          ALLOCATE(MP%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      KMA1 = INT(MA%MP(2))
      IF (KMA1 <= 2) KMA1 = 2
      IF (ABS(KMA1) >= MEXPOV) KMA1 = 2
      KP = 2
      MP%MP(2) = MA%MP(2)
      MP%MP(3) = ABS(MA%MP(3))*MBASE + MA%MP(4)
      MP%MP(1) = 1
      IF (MA%MP(1) < 0) MP%MP(1) = -1
      IF (KMA1 >= 4) THEN
          DO J = 4, KMA1, 2
             KP = KP + 1
             MP%MP(KP+1) = MA%MP(J+1)*MBASE + MA%MP(J+2)
          ENDDO
      ENDIF
      IF (MOD(KMA1,2) == 1) MP%MP(KP+2) = MA%MP(KMA1+2)*MBASE
      RETURN
      END SUBROUTINE IMPACK

      SUBROUTINE IMPMOD(MA,MB,MC,MD)

!  MD = MOD(MA**MB,MC)

!  The binary multiplication method used requires an average of 1.5 * LOG2(MB) operations.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC,MD
      REAL (KIND(1.0D0)) :: MBS
      INTEGER :: IREM,KWRNSV,NDSAVE,NTRSAV,RESULT_SIZE
      INTENT (IN) :: MA,MB,MC
      INTENT (INOUT) :: MD
      TYPE(MULTI) :: MXY(6)

      RESULT_SIZE = MC%MP(2) + 3
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 5
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MD%MP)) THEN
          ALLOCATE(MD%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MD%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MD%MP)
          ALLOCATE(MD%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      KFLAG = 0
      NCALL = NCALL + 1
      IF (KDEBUG == 1) CALL IMARGS('IMPMOD   ',2,MA,MB)
      IF (KDEBUG == 1) CALL IMARGS('IMPMOD   ',1,MC,MC)
      NDSAVE = NDIG
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'IMPMOD'
          CALL IMNTR(2,MA,MB,2)
          IF (ABS(NTRACE) >= 2 .AND. NCALL <= LVLTRC) THEN
              IF (NTRACE < 0) THEN
                  NDIG = MAX(2,INT(MC%MP(2)))
                  IF (ABS(MA%MP(2)) >= MEXPOV) NDIG = 2
                  NTRSAV = NTRACE
                  CALL IMNTRJ(MC,NDIG)
                  NTRACE = NTRSAV
                  NDIG = NDSAVE
              ELSE
                  CALL IMPRNT(MC)
              ENDIF
          ENDIF
      ENDIF
      MBS = MB%MP(1)

!             Check for special cases.

      IF (MA%MP(2) == MUNKNO .OR. MB%MP(2) == MUNKNO .OR.        &
          MC%MP(2) == MUNKNO .OR. MA%MP(2) == MEXPOV .OR.        &
          MB%MP(2) == MEXPOV .OR. MC%MP(2) == MEXPOV .OR.        &
          MA%MP(2) < 0 .OR. MB%MP(2) < 0 .OR. MC%MP(2) < 0 .OR.  &
          (MB%MP(1)*MB%MP(3) <= 0 .AND. MA%MP(3) == 0) .OR.      &
          MC%MP(3) == 0) THEN
          KFLAG = -4
          IF (MA%MP(2) /= MUNKNO .AND. MB%MP(2) /= MUNKNO .AND.  &
              MC%MP(2) /=MUNKNO) THEN
              NAMEST(NCALL) = 'IMPMOD'
              CALL FMWARN
          ENDIF
          CALL IMST2M('UNKNOWN',MD)
          IF (NTRACE /= 0) CALL IMNTR(1,MD,MD,1)
          NDIG = NDSAVE
          NCALL = NCALL - 1
          RETURN
      ENDIF

      IF (MB%MP(3) == 0) THEN
          CALL IMI2M(1,MD)
          IF (NTRACE /= 0) CALL IMNTR(1,MD,MD,1)
          NDIG = NDSAVE
          NCALL = NCALL - 1
          RETURN
      ENDIF

      IF (MB%MP(2) == 1 .AND. ABS(MB%MP(3)) == 1) THEN
          KWRNSV = KWARN
          KWARN = 0
          IF (MB%MP(1) == 1) THEN
              CALL IMMOD(MA,MC,MXY(6))
          ELSE
              CALL IMI2M(1,MXY(2))
              CALL IMDIVR(MXY(2),MA,MXY(1),MXY(3))
              CALL IMMOD(MXY(1),MC,MXY(6))
          ENDIF
          CALL IMEQ(MXY(6),MD)
          IF (NTRACE /= 0) CALL IMNTR(1,MD,MD,1)
          NDIG = NDSAVE
          NCALL = NCALL - 1
          KWARN = KWRNSV
          RETURN
      ENDIF

      IF (MA%MP(3) == 0) THEN
          CALL IMI2M(0,MD)
          IF (NTRACE /= 0) CALL IMNTR(1,MD,MD,1)
          NDIG = NDSAVE
          NCALL = NCALL - 1
          RETURN
      ENDIF

!             Initialize.

      KWRNSV = KWARN
      KWARN = 0
      CALL IMABS(MB,MXY(3))
      CALL IMDIVR(MA,MC,MXY(1),MXY(2))
      CALL IMEQ(MC,MXY(1))
      CALL IMDVIR(MXY(3),2,MXY(6),IREM)
      IF (IREM == 0) THEN
          CALL IMI2M(1,MXY(6))
      ELSE
          CALL IMEQ(MXY(2),MXY(6))
      ENDIF
      CALL IMDVIR(MXY(3),2,MXY(4),IREM)
      CALL IMEQ(MXY(4),MXY(3))

!             This is the multiplication loop.

  110 CALL IMDVIR(MXY(3),2,MXY(4),IREM)
      CALL IMEQ(MXY(4),MXY(3))
      CALL IMMPYM(MXY(2),MXY(2),MXY(1),MXY(5))
      CALL IMEQ(MXY(5),MXY(2))
      IF (IREM == 1) THEN
          CALL IMMPYM(MXY(2),MXY(6),MXY(1),MXY(5))
          CALL IMEQ(MXY(5),MXY(6))
      ENDIF
      IF (MXY(3)%MP(3) > 0 .AND. MXY(6)%MP(3) /= 0) GO TO 110

      IF (MBS < 0) THEN
          CALL IMI2M(1,MXY(2))
          KREM = 0
          CALL IMDIVR(MXY(2),MXY(6),MXY(4),MXY(3))
          KREM = 1
          CALL IMEQ(MXY(4),MXY(6))
      ENDIF
      KWARN = KWRNSV
      CALL IMEQ(MXY(6),MD)
      IF (KFLAG < 0) THEN
          NAMEST(NCALL) = 'IMPMOD'
          CALL FMWARN
      ENDIF
      IF (MD%MP(2) <= 1) MD%MP(4) = 0
      IF (NTRACE /= 0) CALL IMNTR(1,MD,MD,1)
      NDIG = NDSAVE
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE IMPMOD

      SUBROUTINE IMPRNT(MA)

!  Print MA in base 10 format.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA

      INTEGER :: JF1SAV,JF2SAV,NDSAVE
      INTENT (IN) :: MA

      NDSAVE = NDIG
      JF1SAV = JFORM1
      JF2SAV = JFORM2
      JFORM1 = 2
      JFORM2 = 0
      NDIG = MAX(2,INT(MA%MP(2)))
      IF (ABS(MA%MP(2)) >= MEXPOV) NDIG = 2
      CALL FMPRNT(MA)
      JFORM1 = JF1SAV
      JFORM2 = JF2SAV
      NDIG = NDSAVE
      RETURN
      END SUBROUTINE IMPRNT

      SUBROUTINE IMPWR(MA,MB,MC)

!  MC = MA ** MB

!  The binary multiplication method used requires an average of 1.5 * LOG2(MB) multiplications.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC
      REAL (KIND(1.0D0)) :: MAS,MBS
      DOUBLE PRECISION :: DA,DB
      INTEGER :: IREM,IREMB,JSIGN,KOVFL,KWRNSV,RESULT_SIZE
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      TYPE(MULTI) :: MXY(5)

      IF ((MA%MP(2) == 1 .AND. MA%MP(3) == 1) .OR.  &
          MA%MP(3) == 0 .OR. MB%MP(3) == 0) THEN
          RESULT_SIZE = 5
      ELSE
          RESULT_SIZE = 5
          KOVFL = 1
          KWRNSV = KWARN
          KWARN = 0
          CALL IMM2DP(MA,DA)
          IF (KFLAG == 0) THEN
              CALL IMM2DP(MB,DB)
              IF (KFLAG == 0) THEN
                  RESULT_SIZE = ABS(DB)*LOG(ABS(DA)+1)/DLOGMB + 5
                  KOVFL = 0
                  IF (ABS(RESULT_SIZE) >= MEXPOV) THEN
                      RESULT_SIZE = 5
                      KOVFL = 1
                  ENDIF
              ENDIF
          ELSE
              CALL IMM2DP(MB,DB)
              IF (KFLAG == 0) THEN
                  RESULT_SIZE = MIN(HUGE(1)/10.0D0,ABS(DB)*(ABS(MA%MP(2))+1) + 5)
                  KOVFL = 0
                  IF (ABS(RESULT_SIZE) >= MEXPOV .OR. ABS(MA%MP(2)) >= MEXPOV) THEN
                      RESULT_SIZE = 5
                      KOVFL = 1
                  ENDIF
              ENDIF
          ENDIF
          KWARN = KWRNSV
      ENDIF
      IF (MB%MP(1) < 0 .AND. MA%MP(3) /= 0) THEN
          RESULT_SIZE = 5
      ENDIF
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 5
      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      KFLAG = 0
      NCALL = NCALL + 1
      IF (KDEBUG == 1) CALL IMARGS('IMPWR    ',2,MA,MB)
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'IMPWR'
          CALL IMNTR(2,MA,MB,2)
      ENDIF
      MAS = MA%MP(1)
      MBS = MB%MP(1)
      KWRNSV = KWARN

!             Check for special cases.

      IF (MA%MP(2) == MUNKNO .OR. MB%MP(2) == MUNKNO .OR.          &
          MA%MP(2) < 0 .OR. MB%MP(2) < 0 .OR. ((MB%MP(1) < 0 .OR.  &
          MB%MP(3) == 0) .AND. MA%MP(3) == 0)) THEN
          KFLAG = -4
          IF (MA%MP(2) /= MUNKNO .AND. MB%MP(2) /= MUNKNO) THEN
              KWARN = KWRNSV
              NAMEST(NCALL) = 'IMPWR'
              CALL FMWARN
          ENDIF
          CALL IMST2M('UNKNOWN',MC)
          GO TO 130
      ENDIF

      IF (MB%MP(3) == 0) THEN
          CALL IMI2M(1,MC)
          GO TO 130
      ENDIF

      IF (MA%MP(2) == 1 .AND. MA%MP(3) == 1) THEN
          KWARN = 0
          IF (MAS == 1) THEN
              CALL IMI2M(1,MC)
          ELSE
              CALL IMI2M(2,MXY(1))
              CALL IMDIVR(MB,MXY(1),MXY(4),MXY(2))
              CALL IMEQ(MXY(4),MXY(1))
              IF (MXY(2)%MP(2) == MUNKNO) THEN
                  KFLAG = -4
                  KWARN = KWRNSV
                  NAMEST(NCALL) = 'IMPWR'
                  CALL FMWARN
                  CALL IMST2M('UNKNOWN',MC)
              ELSE IF (MXY(2)%MP(3) == 0) THEN
                  CALL IMI2M(1,MC)
              ELSE
                  CALL IMI2M(-1,MC)
              ENDIF
          ENDIF
          GO TO 130
      ENDIF

      IF (MB%MP(2) == 1 .AND. MB%MP(3) == 1) THEN
          KWARN = 0
          IF (MBS == 1) THEN
              CALL IMEQ(MA,MC)
          ELSE
              CALL IMI2M(1,MXY(1))
              KREM = 0
              CALL IMDIVR(MXY(1),MA,MXY(3),MXY(2))
              CALL IMEQ(MXY(3),MC)
              KREM = 1
          ENDIF
          GO TO 130
      ENDIF

      IF (MA%MP(3) == 0) THEN
          CALL IMI2M(0,MC)
          GO TO 130
      ENDIF

      IF (MB%MP(1) < 0) THEN
          CALL IMI2M(0,MC)
          GO TO 130
      ENDIF

      IF (MB%MP(2) == MEXPOV) THEN
          IF (MBS < 0) THEN
              CALL IMI2M(0,MC)
          ELSE IF (MAS > 0) THEN
              CALL IMST2M('OVERFLOW',MC)
              KFLAG = -5
          ELSE
              KFLAG = -4
              KWARN = KWRNSV
              NAMEST(NCALL) = 'IMPWR'
              CALL FMWARN
              CALL IMST2M('UNKNOWN',MC)
          ENDIF
          GO TO 130
      ENDIF

      IF (MA%MP(2) == MEXPOV) THEN
          JSIGN = 1
          IF (MA%MP(1) < 0) JSIGN = -1
          IF (MBS > 0) THEN
              CALL IMDVIR(MB,2,MXY(1),IREM)
              CALL IMST2M('OVERFLOW',MC)
              MC%MP(1) = JSIGN**IREM
              KFLAG = -5
          ELSE
              CALL IMI2M(0,MC)
          ENDIF
          GO TO 130
      ENDIF

      IF (KOVFL == 1) THEN
          IF (MBS <= 0) THEN
              CALL IMI2M(0,MC)
          ELSE IF (MA%MP(1) >= 0) THEN
              CALL IMST2M('OVERFLOW',MC)
              KFLAG = -5
              KWARN = KWRNSV
              NAMEST(NCALL) = 'IMPWR'
              CALL FMWARN
          ELSE
              CALL IMDVIR(MB,2,MXY(1),IREM)
              CALL IMST2M('OVERFLOW',MC)
              MC%MP(1) = (-1)**IREM
              KFLAG = -5
              KWARN = KWRNSV
              NAMEST(NCALL) = 'IMPWR'
              CALL FMWARN
          ENDIF
          GO TO 130
      ENDIF

!             Initialize.

      IF (.NOT. ALLOCATED(MXY(1)%MP)) THEN
          ALLOCATE(MXY(1)%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MXY(1)%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MXY(1)%MP)
          ALLOCATE(MXY(1)%MP(RESULT_SIZE),STAT=K_STAT)
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
      IF (.NOT. ALLOCATED(MXY(5)%MP)) THEN
          ALLOCATE(MXY(5)%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MXY(5)%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MXY(5)%MP)
          ALLOCATE(MXY(5)%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF
      KWARN = 0
      CALL IMABS(MB,MXY(2))

      CALL IMEQ(MA,MXY(1))

      CALL IMDVIR(MB,2,MXY(4),IREMB)
      CALL IMEQ(MXY(4),MC)
      IF (IREMB == 0) THEN
          CALL IMI2M(1,MC)
      ELSE
          CALL IMEQ(MXY(1),MC)
      ENDIF
      CALL IMDVIR(MXY(2),2,MXY(4),IREM)
      CALL IMEQ(MXY(4),MXY(2))

!             This is the multiplication loop.

  110 CALL IMDVIR(MXY(2),2,MXY(4),IREM)
      CALL IMEQ(MXY(4),MXY(2))
      CALL IMSQR(MXY(1),MXY(5))
      CALL IMEQ(MXY(5),MXY(1))
      IF (IREM == 1) THEN
          CALL IMMPY(MXY(1),MC,MXY(3))
          CALL IMEQ(MXY(3),MC)
      ENDIF
      IF (MXY(1)%MP(2) == MEXPOV) THEN
          CALL IMEQ(MXY(1),MC)
          IF (MAS < 0 .AND. IREMB == 1) MC%MP(1) = -1
          GO TO 120
      ENDIF
      IF (MXY(2)%MP(3) > 0) GO TO 110

  120 IF (MBS < 0) THEN
          CALL IMI2M(1,MXY(1))
          KREM = 0
          CALL IMDIVR(MXY(1),MC,MXY(4),MXY(2))
          KREM = 1
          CALL IMEQ(MXY(4),MC)
      ENDIF

      IF (MC%MP(2) >= MEXPOV) THEN
          IF (NCALL == 1 .OR. MC%MP(2) >= MEXPOV) THEN
              IF (MC%MP(1) > 0) THEN
                  CALL IMST2M('OVERFLOW',MC)
              ELSE
                  CALL IMST2M('-OVERFLOW',MC)
              ENDIF
              KFLAG = -5
              KWARN = KWRNSV
              NAMEST(NCALL) = 'IMPWR'
              CALL FMWARN
          ENDIF
      ENDIF

  130 IF (MC%MP(2) <= 1) MC%MP(4) = 0
      KWARN = KWRNSV
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'IMPWR'
          CALL IMNTR(1,MC,MC,1)
      ENDIF
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE IMPWR

      SUBROUTINE IMREAD(KREAD,MA)

!  Read MA on unit KREAD.  Multi-line numbers will have '&' as the last nonblank character on all
!  but the last line.  Only one number is allowed on the line(s).

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA
      INTEGER :: KREAD

      CHARACTER :: LINE(132)
      INTEGER :: J,K,L2,LB
      INTENT (IN) :: KREAD
      INTENT (INOUT) :: MA

      NCALL = NCALL + 1
      NAMEST(NCALL) = 'IMREAD'
      LB = 0

  110 READ (KREAD,"(132A1)",ERR=120,END=120) LINE

!             Scan the line and look for '&'

      DO J = 1, 132
         IF (LINE(J) == '&') GO TO 110
         IF (LINE(J) /= ' ') THEN
             LB = LB + 1
             IF (LB > LMBUFF) THEN

!                If CMBUFF runs out of space, try to re-allocate it with a bigger size.

                 IF (LMBUFF > 0) THEN
                     ALLOCATE(MOVE_CMBUFF(LMBUFF),STAT=K)
                     IF (K /= 0) THEN
                         CALL FMDEFINE_ERROR
                     ENDIF
                     DO K = 1, LMBUFF
                        MOVE_CMBUFF(K) = CMBUFF(K)
                     ENDDO
                     DEALLOCATE(CMBUFF)
                     L2 = MAX(10000,2*LMBUFF)
                     ALLOCATE(CMBUFF(L2),STAT=K)
                     IF (K /= 0) THEN
                         CALL FMDEFINE_ERROR
                     ENDIF
                     DO K = 1, L2
                        CMBUFF(K) = ' '
                     ENDDO
                     DO K = 1, LMBUFF
                        CMBUFF(K) = MOVE_CMBUFF(K)
                     ENDDO
                     DEALLOCATE(MOVE_CMBUFF)
                     LMBUFF = L2
                 ELSE
                     ALLOCATE(CMBUFF(10000),STAT=K)
                     IF (K /= 0) THEN
                         CALL FMDEFINE_ERROR
                     ENDIF
                     LMBUFF = 10000
                 ENDIF
             ENDIF
             CMBUFF(LB) = LINE(J)
         ENDIF
      ENDDO

      CALL IMINP(CMBUFF,MA,1,LB)
      NCALL = NCALL - 1
      RETURN

!             If there is an error, return UNKNOWN.

  120 KFLAG = -4
      CALL FMWARN
      CALL FMST2M('UNKNOWN',MA)
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE IMREAD

      SUBROUTINE IMSIGN(MA,MB,MC)

!  MC = SIGN(MA,MB)

!  MC is set to ABS(MA) if MB is positive or zero, or -ABS(MA) if MB is negative.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC

      INTEGER :: KWRNSV,NDSAVE,RESULT_SIZE
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC

      RESULT_SIZE = MA%MP(2) + 3
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 5
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      KFLAG = 0
      NCALL = NCALL + 1
      IF (KDEBUG == 1) CALL IMARGS('IMSIGN   ',2,MA,MB)
      NDSAVE = NDIG
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'IMSIGN'
          CALL IMNTR(2,MA,MB,2)
      ENDIF

      NDIG = INT(MA%MP(2))
      IF (NDIG < 2) NDIG = 2
      KWRNSV = KWARN
      KWARN = 0
      IF (MA%MP(2) == MUNKNO .OR. MB%MP(2) == MUNKNO) THEN
          CALL IMST2M('UNKNOWN',MC)
          KFLAG = -4
      ELSE IF (MA%MP(2) < 0 .OR. MB%MP(2) < 0) THEN
          KFLAG = -4
          NAMEST(NCALL) = 'IMSIGN'
          CALL FMWARN
          CALL IMST2M('UNKNOWN',MC)
      ELSE IF (MB%MP(1) >= 0) THEN
          CALL IMEQ(MA,MC)
          MC%MP(1) = 1
      ELSE
          CALL IMEQ(MA,MC)
          IF (MC%MP(2) /= MUNKNO .AND. MC%MP(3) /= 0) MC%MP(1) = -1
      ENDIF

      IF (MC%MP(2) <= 1) MC%MP(4) = 0
      KWARN = KWRNSV
      IF (NTRACE /= 0) CALL IMNTR(1,MC,MC,1)
      NCALL = NCALL - 1
      NDIG = NDSAVE
      RETURN
      END SUBROUTINE IMSIGN

      SUBROUTINE IMSQR(MA,MB)

!  MB = MA * MA

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB

      REAL (KIND(1.0D0)) :: MDAB
      INTEGER :: NDSAVE,RESULT_SIZE
      RESULT_SIZE = 2*MA%MP(2) + 4
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 5
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MB%MP)) THEN
          ALLOCATE(MB%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MB%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MB%MP)
          ALLOCATE(MB%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NCALL = NCALL + 1
      IF (KDEBUG == 1) CALL IMARGS('IMSQR    ',1,MA,MA)
      KFLAG = 0
      NDSAVE = NDIG
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'IMSQR'
          CALL IMNTR(2,MA,MA,1)
      ENDIF

      IF (MA%MP(2) <= 1) THEN
          IF (MA%MP(2) < 0) GO TO 110
          MDAB = MA%MP(3) * MA%MP(3)
          IF (ABS(MDAB) < MBASE) THEN
              MB%MP(2) = 1
              IF (MDAB == 0) MB%MP(2) = 0
              MB%MP(3) = MDAB
              MB%MP(4) = 0
              GO TO 120
          ELSE IF (ABS(MDAB) < MBASE*MBASE) THEN
              MB%MP(2) = 2
              MB%MP(3) = AINT (MDAB/MBASE)
              MB%MP(4) = MDAB - MBASE*MB%MP(3)
              GO TO 120
          ENDIF
      ENDIF

!             Check for special cases.

  110 IF (MA%MP(2) == MUNKNO) THEN
          KFLAG = -4
          CALL IMST2M('UNKNOWN',MB)
          GO TO 120
      ENDIF
      IF (MA%MP(3) == 0) THEN
          MB%MP(1) = 1
          MB%MP(2) = 0
          MB%MP(3) = 0
          MB%MP(4) = 0
          GO TO 120
      ENDIF
      IF (MA%MP(2) == MEXPOV) THEN
          KFLAG = -5
          CALL IMST2M('OVERFLOW',MB)
          GO TO 120
      ENDIF
      IF (MA%MP(2) == 1 .AND. ABS(MA%MP(3)) == 1) THEN
          CALL IMI2M(1,MB)
          GO TO 120
      ELSE IF (MA%MP(2) < 0) THEN
          KFLAG = -4
          NAMEST(NCALL) = 'IMSQR'
          CALL FMWARN
          CALL IMST2M('UNKNOWN',MB)
          GO TO 120
      ENDIF

      NDIG = INT(MA%MP(2) + MA%MP(2))

      IF (NDIG < 2) NDIG = 2

      CALL IMSQR2(MA,MB)

  120 IF (MB%MP(2) <= 1) MB%MP(4) = 0
      MB%MP(1) = 1
      IF (NTRACE /= 0) CALL IMNTR(1,MB,MB,1)
      NCALL = NCALL - 1
      NDIG = NDSAVE
      RETURN
      END SUBROUTINE IMSQR

      SUBROUTINE IMSQR2(MA,MB)

!  MB = MA*MA.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB

      REAL (KIND(1.0D0)) :: MAXMAX,MAXMWA,MBJ,MBKJ,MBNORM,MBP1,MK,MKA,MKT,MMAX,MT
      INTEGER :: J,JM1,K,KB,KI,KJ,KL,KNZ,KOVUN,KWA,L,N1,ND,NMETHD,NZDA,RESULT_SIZE
      TYPE(MULTI) :: MXY(2)
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB

      RESULT_SIZE = 2*MA%MP(2) + 30
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 32
      RESULT_SIZE = MAX(32,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MWA%MP)) THEN
          ALLOCATE(MWA%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MWA%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MWA%MP)
          ALLOCATE(MWA%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

!             KSQR is used to tell FMMPYFFT that only one input fft is needed.

      KSQR = 1
      IF (MBLOGS /= MBASE) CALL FMCONS
      IF (KDEBUG == 1 .OR. MBASE*MBASE <= MXBASE/(4*MBASE)) THEN
          KOVUN = 0
          IF (MA%MP(2) == MEXPOV .OR. MA%MP(2) == MEXPUN) KOVUN = 1
          IF (MA%MP(2) == MUNKNO) KOVUN = 2
          NCALL = NCALL + 1
          CALL IMMPY(MA,MA,MB)
          NCALL = NCALL - 1
          IF ((KFLAG < 0 .AND. KOVUN == 0) .OR. (KFLAG == -4 .AND. KOVUN == 1)) THEN
              NAMEST(NCALL) = 'IMSQR'
              CALL FMWARN
          ENDIF
          GO TO 130
      ELSE IF (MA%MP(3) == 0) THEN
          CALL IMEQ(MA,MB)
          GO TO 130
      ENDIF
      KFLAG = 0
      MAXMAX = 0
      N1 = INT(MA%MP(2)) + 1
      MWA%MP(2) = MA%MP(2) + MA%MP(2)

!             Check for using an FFT-based method if precision is very high.

      ND = 1000
      IF (NDIG >= ND) THEN
          NZDA = 0
          DO J = 2, INT(MA%MP(2))
             IF (MA%MP(J+2) == 0) NZDA = NZDA + 1
          ENDDO
          IF (MA%MP(2)-NZDA < 50 .OR. REAL(NZDA)/MA%MP(2) > 0.8) THEN
              NMETHD = 1
          ELSE
              NMETHD = 2
          ENDIF
      ELSE
          NMETHD = 1
      ENDIF
      IF (NMETHD == 2) THEN
          ND = NDIG
          NDIG = MA%MP(2)
          CALL IMI2FM(MA,MXY(1))
          CALL IMI2FM(MA,MXY(2))
          CALL FMMPYFFT(MXY(1),MXY(2))
          NDIG = ND
          GO TO 120
      ENDIF

      L = N1 + INT(MA%MP(2))
      MWA%MP(L+2) = 0

!             The multiplication loop begins here.

!             MBNORM is the minimum number of digits that can be multiplied before normalization
!                    is required.
!             MAXMWA is an upper bound on the size of values in MWA divided by (MBASE-1).  It is
!                    used to determine whether to normalize before the next digit is multiplied.

      MBP1 = MBASE + 1
      MBNORM = (MAXINT/(MBP1*MBP1))
      MMAX = INTMAX - MBASE
      MMAX = MIN((MAXINT/MBP1 - MBP1),MMAX)
      IF (MBNORM >= 2) THEN
          MBJ = MA%MP(3)

!             Count the trailing zeros in MA.

          IF (MA%MP(N1+1) /= 0) THEN
              KNZ = N1
          ELSE
              DO J = INT(MA%MP(2)), 2, -1
                 IF (MA%MP(J+1) /= 0) THEN
                     KNZ = J
                     GO TO 110
                 ENDIF
              ENDDO
          ENDIF

  110     MWA%MP(3) = 0
          MWA%MP(4) = 0
          DO K = N1+1, L
             MWA%MP(K+1) = 0
          ENDDO

!             (Inner Loop)

          DO K = 4, N1+1
             MWA%MP(K+1) = MA%MP(K)*MBJ
          ENDDO
          MAXMWA = MBJ
          DO J = 3, N1
             MBJ = MA%MP(J+1)
             IF (MBJ /= 0) THEN
                 MAXMWA = MAXMWA + MBJ
                 JM1 = J - 1
                 KL = MIN(KNZ,L-JM1)

!                       Major (Inner Loop)

                 DO K = 2*J+1, JM1+KL+1
                    MWA%MP(K) = MWA%MP(K) + MA%MP(K-JM1)*MBJ
                 ENDDO
             ENDIF

             IF (MAXMWA > MMAX) THEN
                 MAXMAX = MAX(MAXMAX,MAXMWA)
                 MAXMWA = 0
                 JM1 = J - 1
                 KL = MIN(KNZ,L-JM1)

!                       Normalization is only required for the range of digits currently
!                       changing in MWA.

                 DO KB = JM1+KL, 2*J, -1
                    MKT = INT (MWA%MP(KB+1)/MBASE)
                    MWA%MP(KB) = MWA%MP(KB) + MKT
                    MWA%MP(KB+1) = MWA%MP(KB+1) - MKT*MBASE
                 ENDDO
             ENDIF
          ENDDO

!             Double MWA, add the square terms, and perform the final normalization.  (Inner Loop)

          IF (2*MAX(MAXMAX,MAXMWA)+MBASE > MMAX) THEN
              DO KB = L+1, 5, -1
                 MKT = INT (MWA%MP(KB)/MBASE)
                 MWA%MP(KB-1) = MWA%MP(KB-1) + MKT
                 MWA%MP(KB) = MWA%MP(KB) - MKT*MBASE
              ENDDO
          ENDIF

          DO J = 4, L, 2
             IF (J/2 <= N1) THEN
                 MKA = MA%MP(1+J/2)
                 MWA%MP(J) = 2*MWA%MP(J) + MKA*MKA
                 MWA%MP(J+1) = 2*MWA%MP(J+1)
             ELSE
                 MWA%MP(J) = 2*MWA%MP(J)
                 MWA%MP(J+1) = 2*MWA%MP(J+1)
             ENDIF
          ENDDO
          IF (MOD(L,2) == 1) THEN
              IF ((L+1)/2 <= N1) THEN
                  MKA = MA%MP(1+(L+1)/2)
                  MWA%MP(L+1) = 2*MWA%MP(L+1) + MKA*MKA
              ELSE
                  MWA%MP(L+1) = 2*MWA%MP(L+1)
              ENDIF
          ENDIF

          DO KB = L+1, 4, -1
             MKT = INT (MWA%MP(KB)/MBASE)
             MWA%MP(KB-1) = MWA%MP(KB-1) + MKT
             MWA%MP(KB) = MWA%MP(KB) - MKT*MBASE
          ENDDO

      ELSE

!             If normalization must be done for each digit, combine the two loops and normalize
!             as the digits are multiplied.

          DO J = 2, L
             MWA%MP(J+1) = 0
          ENDDO
          KJ = MA%MP(2) + 2
          DO J = 2, N1
             KJ = KJ - 1
             MBKJ = MA%MP(KJ+1)
             IF (MBKJ == 0) CYCLE
             KL = L - KJ + 1
             IF (KL > N1) KL = N1
             KI = KL + 2
             KWA = KL+ KJ + 1
             MK = 0
             DO K = 2, KL
                MT = MA%MP(KI-K+1)*MBKJ + MWA%MP(KWA-K+1) + MK
                MK = INT (MT/MBASE)
                MWA%MP(KWA-K+1) = MT - MBASE*MK
             ENDDO
             MWA%MP(KWA-KL) = MK
          ENDDO

      ENDIF

!             The multiplication is complete.

  120 NDIG = MWA%MP(2)
      IF (NDIG < 2) NDIG = 2
      CALL FMMOVE(MWA,MB)

      IF (KFLAG < 0) THEN
          NAMEST(NCALL) = 'IMSQR'
          CALL FMWARN
      ENDIF

  130 MB%MP(1) = 1
      KSQR = 0
      RETURN
      END SUBROUTINE IMSQR2

      SUBROUTINE IMST2M(STRING,MA)

!  MA = STRING

!  Convert a character string to IM format.

      USE ModLib_FMVALS
      IMPLICIT NONE

      CHARACTER(*) :: STRING
      TYPE(MULTI) :: MA

      INTEGER :: J,LB,KFSAVE
      INTENT (IN) :: STRING
      INTENT (INOUT) :: MA

      IF (MBLOGS /= MBASE) CALL FMCONS
      NCALL = NCALL + 1
      NAMEST(NCALL) = 'IMST2M'
      LB = LEN(STRING)
      IF (LB > LMBUFF) THEN
          IF (LMBUFF > 0) DEALLOCATE(CMBUFF)
          ALLOCATE(CMBUFF(LB),STAT=J)
          IF (J /= 0) THEN
              CALL FMDEFINE_ERROR
          ENDIF
          LMBUFF = LB
      ENDIF
      KFSAVE = KFLAG

      DO J = 1, LB
         CMBUFF(J) = STRING(J:J)
      ENDDO

      CALL IMINP(CMBUFF,MA,1,LB)

      IF (MA%MP(2) <= 1) MA%MP(4) = 0
      IF (KFSAVE /= 0) KFLAG = KFSAVE
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE IMST2M

      SUBROUTINE IMSUB(MA,MB,MC)

!  MC = MA - MB

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC

      REAL (KIND(1.0D0)) :: MDA,MDAB,MDB
      INTEGER :: NDSAVE,RESULT_SIZE
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC

      RESULT_SIZE = MAX(MA%MP(2),MB%MP(2)) + 4
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 5
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MC%MP)) THEN
          ALLOCATE(MC%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MC%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MC%MP)
          ALLOCATE(MC%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      NCALL = NCALL + 1
      IF (KDEBUG == 1) CALL IMARGS('IMSUB    ',2,MA,MB)
      KFLAG = 0
      NDSAVE = NDIG
      IF (NTRACE /= 0) THEN
          NAMEST(NCALL) = 'IMSUB'
          CALL IMNTR(2,MA,MB,2)
      ENDIF

      IF (MA%MP(2) <= 2) THEN
          IF (MB%MP(2) > 2 .OR. MA%MP(2) < 0 .OR. MB%MP(2) < 0) GO TO 110
          IF (MA%MP(2) <= 1) THEN
              MDA = MA%MP(1) * MA%MP(3)
          ELSE
              MDA = MA%MP(1) * (MA%MP(3)*MBASE + MA%MP(4))
          ENDIF
          IF (MB%MP(2) <= 1) THEN
              MDB = MB%MP(1) * MB%MP(3)
          ELSE
              MDB = MB%MP(1) * (MB%MP(3)*MBASE + MB%MP(4))
          ENDIF
          MDAB = MDA - MDB
          IF (ABS(MDAB) < MBASE) THEN
              MC%MP(2) = 1
              IF (MDAB == 0) MC%MP(2) = 0
              MC%MP(1) = 1
              IF (MDAB < 0) MC%MP(1) = -1
              MC%MP(3) = ABS(MDAB)
              MC%MP(4) = 0
              IF (MDA == 0 .OR. MDB == 0) KFLAG = 1
              GO TO 120
          ELSE IF (ABS(MDAB) < MBASE*MBASE) THEN
              MC%MP(2) = 2
              MC%MP(1) = 1
              IF (MDAB < 0) MC%MP(1) = -1
              MDAB = ABS(MDAB)
              MC%MP(3) = AINT (MDAB/MBASE)
              MC%MP(4) = MDAB - MBASE*MC%MP(3)
              IF (MDA == 0 .OR. MDB == 0) KFLAG = 1
              GO TO 120
          ENDIF
      ENDIF

!             Check for special cases.

  110 IF (MA%MP(2) < 0 .OR. MB%MP(2) < 0 .OR.  &
          MA%MP(2) >= MEXPOV .OR. MB%MP(2) >= MEXPOV) THEN
          IF (MA%MP(2) == MUNKNO .OR. MB%MP(2) == MUNKNO) THEN
              CALL IMST2M('UNKNOWN',MC)
              KFLAG = -4
              GO TO 120
          ENDIF
          IF (MA%MP(2) == MEXPOV) THEN
              IF (MA%MP(1) == -MB%MP(1) .OR. MB%MP(3) == 0) THEN
                  MC%MP(1) = MA%MP(1)
                  MC%MP(2) = MA%MP(2)
                  MC%MP(3) = MA%MP(3)
                  MC%MP(4) = MA%MP(4)
                  KFLAG = -5
                  GO TO 120
              ELSE
                  KFLAG = -4
                  NAMEST(NCALL) = 'IMSUB'
                  CALL FMWARN
                  CALL IMST2M('UNKNOWN',MC)
                  GO TO 120
              ENDIF
          ENDIF
          IF (MB%MP(2) == MEXPOV) THEN
              IF (-MB%MP(1) == MA%MP(1) .OR. MA%MP(3) == 0) THEN
                  MC%MP(1) = -MB%MP(1)
                  MC%MP(2) = MB%MP(2)
                  MC%MP(3) = MB%MP(3)
                  MC%MP(4) = MB%MP(4)
                  KFLAG = -5
                  GO TO 120
              ELSE
                  KFLAG = -4
                  NAMEST(NCALL) = 'IMSUB'
                  CALL FMWARN
                  CALL IMST2M('UNKNOWN',MC)
                  GO TO 120
              ENDIF
          ENDIF
          KFLAG = -4
          NAMEST(NCALL) = 'IMSUB'
          CALL FMWARN
          CALL IMST2M('UNKNOWN',MC)
          GO TO 120
      ENDIF

!             IMADD3 will negate MB and add.

      KSUB = 1
      CALL IMADD3(MA,MB,MC)
      KSUB = 0

  120 IF (MC%MP(2) <= 1) MC%MP(4) = 0
      IF (NTRACE /= 0) CALL IMNTR(1,MC,MC,1)
      NCALL = NCALL - 1
      NDIG = NDSAVE
      RETURN
      END SUBROUTINE IMSUB

      SUBROUTINE IMUNPK(MP,MA)

!  MP is unpacked and the value returned in MA.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MP

      INTEGER :: J,KP,KMA1,RESULT_SIZE
      INTENT (IN) :: MP
      INTENT (INOUT) :: MA

      RESULT_SIZE = MP%MP(2) + 3
      IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 5
      RESULT_SIZE = MAX(5,RESULT_SIZE)
      IF (.NOT. ALLOCATED(MA%MP)) THEN
          ALLOCATE(MA%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ELSE IF (SIZE(MA%MP) < RESULT_SIZE) THEN
          DEALLOCATE(MA%MP)
          ALLOCATE(MA%MP(RESULT_SIZE),STAT=K_STAT)
          IF (K_STAT /= 0) CALL FMDEFINE_ERROR
      ENDIF

      KMA1 = INT(MP%MP(2))
      IF (KMA1 <= 2) KMA1 = 2
      IF (ABS(KMA1) >= MEXPOV) KMA1 = 2
      KP = 2
      MA%MP(2) = MP%MP(2)
      MA%MP(3) = AINT (ABS(MP%MP(3))/MBASE)
      MA%MP(4) = ABS(MP%MP(3)) - MA%MP(3)*MBASE
      MA%MP(1) = 1
      IF (MP%MP(1) < 0) MA%MP(1) = -1
      IF (KMA1 >= 4) THEN
          DO J = 4, KMA1, 2
             KP = KP + 1
             MA%MP(J+1) = AINT (MP%MP(KP+1)/MBASE)
             MA%MP(J+2) = MP%MP(KP+1) - MA%MP(J+1)*MBASE
          ENDDO
      ENDIF
      IF (MOD(KMA1,2) == 1) MA%MP(KMA1+2) = AINT (MP%MP(KP+2)/MBASE)
      RETURN
      END SUBROUTINE IMUNPK

      SUBROUTINE IMWRIT(KWRITE,MA)

!  Write MA on unit KWRITE.  Multi-line numbers will have '&' as the last nonblank character on all
!  but the last line.  These numbers can then be read easily using IMREAD.

      USE ModLib_FMVALS
      IMPLICIT NONE

      INTEGER :: KWRITE
      TYPE(MULTI) :: MA

      INTEGER :: J,K,KSAVE,L,LAST,LB,ND,NDSAVE,NEXP
      INTENT (IN) :: KWRITE,MA

      NCALL = NCALL + 1
      IF (KDEBUG == 1) CALL IMARGS('IMWRIT   ',1,MA,MA)
      NAMEST(NCALL) = 'IMWRIT'
      NDSAVE = NDIG
      NDIG = MAX(2,INT(MA%MP(2)))
      IF (MA%MP(2) >= MEXPOV) NDIG = 2

      KSAVE = KFLAG
      ND = INT(REAL(NDIG)*LOG10(REAL(MBASE))) + 1
      IF (ND < 2) ND = 2
      NEXP = INT(2.0*LOG10(REAL(MXBASE))) + 16
      LB = ND + NEXP
      IF (LB > LMBUFF) THEN
          IF (LMBUFF > 0) DEALLOCATE(CMBUFF)
          ALLOCATE(CMBUFF(LB),STAT=J)
          IF (J /= 0) THEN
              CALL FMDEFINE_ERROR
          ENDIF
          LMBUFF = LB
      ENDIF

      CALL IMOUT(MA,CMBUFF,LB)

      KFLAG = KSAVE
      NDIG = NDSAVE
      LAST = LB + 1
      DO J = 1, LB
         IF (CMBUFF(LAST-J) /= ' ' .OR. J == LB) THEN
             L = LAST - J
             IF (MOD(L,73) /= 0) THEN
                 WRITE (KWRITE,"(4X,73A1,' &')") (CMBUFF(K),K=1,L)
             ELSE
                 IF (L > 73) WRITE (KWRITE,"(4X,73A1,' &')") (CMBUFF(K),K=1,L-73)
                 WRITE (KWRITE,"(4X,73A1)") (CMBUFF(K),K=L-72,L)
             ENDIF
             NCALL = NCALL - 1
             RETURN
         ENDIF
      ENDDO
      NCALL = NCALL - 1
      RETURN
      END SUBROUTINE IMWRIT

      SUBROUTINE IM_OR_FM_ADD(MA,MB,MC)

!  Internal routine used by binary splitting algorithms.
!  MA and MB are integers, but if they get bigger than the current precision can represent exactly,
!  it is faster to use FM routines in place of IM routines.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      TYPE(MULTI) :: M1,M2
      INTEGER :: RESULT_SIZE

      IF (MAX(MA%MP(2),MB%MP(2)) > NDIG) THEN
          IF (.NOT. ALLOCATED(MC%MP)) THEN
              ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MC%MP) < NDIG+2) THEN
              DEALLOCATE(MC%MP)
              ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
      ELSE
          RESULT_SIZE = MAX(MA%MP(2),MB%MP(2)) + 4
          IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 5
          RESULT_SIZE = MAX(5,RESULT_SIZE)
          IF (.NOT. ALLOCATED(MC%MP)) THEN
              ALLOCATE(MC%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MC%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MC%MP)
              ALLOCATE(MC%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
      ENDIF

      IF (MAX(MA%MP(2),MB%MP(2)) > NDIG) THEN
          IF (MA%MP(2) >= NDIG .AND. MB%MP(2) >= NDIG) THEN
              CALL FMADD(MA,MB,MC)
          ELSE IF (MA%MP(2) >= NDIG .AND. MB%MP(2) < NDIG) THEN
              CALL IMI2FM(MB,M2)
              CALL FMADD(MA,M2,MC)
          ELSE IF (MA%MP(2) < NDIG .AND. MB%MP(2) >= NDIG) THEN
              CALL IMI2FM(MA,M1)
              CALL FMADD(M1,MB,MC)
          ELSE IF (MA%MP(2) < NDIG .AND. MB%MP(2) < NDIG) THEN
              CALL IMI2FM(MA,M1)
              CALL IMI2FM(MB,M2)
              CALL FMADD(M1,M2,MC)
          ENDIF
      ELSE
          CALL IMADD(MA,MB,MC)
      ENDIF

      RETURN
      END SUBROUTINE IM_OR_FM_ADD

      SUBROUTINE IM_OR_FM_EQ(MA,MB)

!  Internal routine used by binary splitting algorithms.
!  MA is an integer, but if it gets bigger than the current precision can represent exactly,
!  it is faster to use FM routines in place of IM routines.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB

      IF (MA%MP(2) >= NDIG) THEN
          CALL FMEQ(MA,MB)
      ELSE
          CALL IMEQ(MA,MB)
      ENDIF
      RETURN
      END SUBROUTINE IM_OR_FM_EQ

      SUBROUTINE IM_OR_FM_MPY(MA,MB,MC)

!  Internal routine used by binary splitting algorithms.
!  MA and MB are integers, but if they get bigger than the current precision can represent exactly,
!  it is faster to use FM routines in place of IM routines.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      TYPE(MULTI) :: M1,M2
      INTEGER :: RESULT_SIZE

      IF (MA%MP(2)+MB%MP(2) > NDIG) THEN
          IF (.NOT. ALLOCATED(MC%MP)) THEN
              ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MC%MP) < NDIG+2) THEN
              DEALLOCATE(MC%MP)
              ALLOCATE(MC%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
      ELSE
          IF (MA%MP(2) >= MEXPOV .OR. MB%MP(2) >= MEXPOV) THEN
              RESULT_SIZE = 5
          ELSE
              RESULT_SIZE = MA%MP(2) + MB%MP(2) + 4
          ENDIF
          RESULT_SIZE = MAX(5,RESULT_SIZE)
          IF (.NOT. ALLOCATED(MC%MP)) THEN
              ALLOCATE(MC%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MC%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MC%MP)
              ALLOCATE(MC%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
      ENDIF

      IF (MA%MP(2)+MB%MP(2) > NDIG) THEN
          IF (MA%MP(2) >= NDIG .AND. MB%MP(2) >= NDIG) THEN
              CALL FMMPY(MA,MB,MC)
          ELSE IF (MA%MP(2) >= NDIG .AND. MB%MP(2) < NDIG) THEN
              CALL IMI2FM(MB,M2)
              CALL FMMPY(MA,M2,MC)
          ELSE IF (MA%MP(2) < NDIG .AND. MB%MP(2) >= NDIG) THEN
              CALL IMI2FM(MA,M1)
              CALL FMMPY(M1,MB,MC)
          ELSE IF (MA%MP(2) < NDIG .AND. MB%MP(2) < NDIG) THEN
              CALL IMI2FM(MA,M1)
              CALL IMI2FM(MB,M2)
              CALL FMMPY(M1,M2,MC)
          ENDIF
      ELSE
          CALL IMMPY(MA,MB,MC)
      ENDIF

      RETURN
      END SUBROUTINE IM_OR_FM_MPY

      SUBROUTINE IM_OR_FM_SQR(MA,MB)

!  Internal routine used by binary splitting algorithms.
!  MA and MB are integers, but if they get bigger than the current precision can represent exactly,
!  it is faster to use FM routines in place of IM routines.

      USE ModLib_FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      TYPE(MULTI) :: M1
      INTEGER :: RESULT_SIZE

      IF (2*MA%MP(2) > NDIG) THEN
          IF (.NOT. ALLOCATED(MB%MP)) THEN
              ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MB%MP) < NDIG+2) THEN
              DEALLOCATE(MB%MP)
              ALLOCATE(MB%MP(NDIG+2),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
      ELSE
          RESULT_SIZE = 2*MA%MP(2) + 4
          IF (ABS(RESULT_SIZE) >= MEXPOV) RESULT_SIZE = 5
          RESULT_SIZE = MAX(5,RESULT_SIZE)
          IF (.NOT. ALLOCATED(MB%MP)) THEN
              ALLOCATE(MB%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ELSE IF (SIZE(MB%MP) < RESULT_SIZE) THEN
              DEALLOCATE(MB%MP)
              ALLOCATE(MB%MP(RESULT_SIZE),STAT=K_STAT)
              IF (K_STAT /= 0) CALL FMDEFINE_ERROR
          ENDIF
      ENDIF

      IF (2*MA%MP(2) > NDIG) THEN
          IF (MA%MP(2) >= NDIG) THEN
              CALL FMSQR(MA,MB)
          ELSE
              CALL IMI2FM(MA,M1)
              CALL FMSQR(M1,MB)
          ENDIF
      ELSE
          CALL IMSQR(MA,MB)
      ENDIF

      RETURN
      END SUBROUTINE IM_OR_FM_SQR


!  These are the longer and more readable routine names, equivalent to the older names.

      FUNCTION IMCOMPARE(MA,LREL,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      LOGICAL :: IMCOMPARE
      LOGICAL, EXTERNAL :: IMCOMP
      CHARACTER(*) :: LREL
      TYPE(MULTI) :: MA,MB
      IMCOMPARE = IMCOMP(MA,LREL,MB)
      RETURN
      END FUNCTION IMCOMPARE

      SUBROUTINE IMFPRINT(FORM,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      CHARACTER(*) :: FORM
      TYPE(MULTI) :: MA
      CALL IMFPRT(FORM,MA)
      RETURN
      END SUBROUTINE IMFPRINT

      SUBROUTINE IMMPY_MOD(MA,MB,MC,MD)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC,MD
      CALL IMMPYM(MA,MB,MC,MD)
      RETURN
      END SUBROUTINE IMMPY_MOD

      SUBROUTINE IMPOWER_MOD(MA,MB,MC,MD)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC,MD
      CALL IMPMOD(MA,MB,MC,MD)
      RETURN
      END SUBROUTINE IMPOWER_MOD

      SUBROUTINE IMPRINT(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      CALL IMPRNT(MA)
      RETURN
      END SUBROUTINE IMPRINT

      SUBROUTINE IMPOWER(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      CALL IMPWR(MA,MB,MC)
      RETURN
      END SUBROUTINE IMPOWER

      SUBROUTINE IMWRITE(KWRITE,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER :: KWRITE
      TYPE(MULTI) :: MA
      CALL IMWRIT(KWRITE,MA)
      RETURN
      END SUBROUTINE IMWRITE

!  These versions of the IM routines use packed IM numbers.

      SUBROUTINE IPABS(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL IMUNPK(MA,MPA)
      CALL IMABS(MPA,MPB)
      CALL IMPACK(MPB,MB)
      RETURN
      END SUBROUTINE IPABS

      SUBROUTINE IPADD(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL IMUNPK(MA,MPA)
      CALL IMUNPK(MB,MPB)
      CALL IMADD(MPA,MPB,MPC)
      CALL IMPACK(MPC,MC)
      RETURN
      END SUBROUTINE IPADD

      SUBROUTINE IPBIG(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      INTENT (INOUT) :: MA
      CALL IMBIG(MPB)
      CALL IMPACK(MPB,MA)
      RETURN
      END SUBROUTINE IPBIG

      FUNCTION IPCOMP(MA,LREL,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      LOGICAL :: IPCOMP
      LOGICAL, EXTERNAL ::IMCOMP
      CHARACTER(*) :: LREL
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA,MB,LREL
      CALL IMUNPK(MA,MPA)
      CALL IMUNPK(MB,MPB)
      IPCOMP = IMCOMP(MPA,LREL,MPB)
      RETURN
      END FUNCTION IPCOMP

      SUBROUTINE IPDIM(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL IMUNPK(MA,MPA)
      CALL IMUNPK(MB,MPB)
      CALL IMDIM(MPA,MPB,MPC)
      CALL IMPACK(MPC,MC)
      RETURN
      END SUBROUTINE IPDIM

      SUBROUTINE IPDIV(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL IMUNPK(MA,MPA)
      CALL IMUNPK(MB,MPB)
      CALL IMDIV(MPA,MPB,MPC)
      CALL IMPACK(MPC,MC)
      RETURN
      END SUBROUTINE IPDIV

      SUBROUTINE IPDIVI(MA,IVAL,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTEGER :: IVAL
      INTENT (IN) :: MA,IVAL
      INTENT (INOUT) :: MB
      CALL IMUNPK(MA,MPA)
      CALL IMDIVI(MPA,IVAL,MPB)
      CALL IMPACK(MPB,MB)
      RETURN
      END SUBROUTINE IPDIVI

      SUBROUTINE IPDIVR(MA,MB,MC,MD)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC,MD
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC,MD
      CALL IMUNPK(MA,MPA)
      CALL IMUNPK(MB,MPB)
      CALL IMDIVR(MPA,MPB,MPC,MPD)
      CALL IMPACK(MPC,MC)
      CALL IMPACK(MPD,MD)
      RETURN
      END SUBROUTINE IPDIVR

      SUBROUTINE IPDVIR(MA,IVAL,MB,IREM)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTEGER :: IVAL,IREM
      INTENT (IN) :: MA,IVAL
      INTENT (INOUT) :: MB,IREM
      CALL IMUNPK(MA,MPA)
      CALL IMDVIR(MPA,IVAL,MPB,IREM)
      CALL IMPACK(MPB,MB)
      RETURN
      END SUBROUTINE IPDVIR

      SUBROUTINE IPEQ(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL IMUNPK(MA,MPA)
      CALL IMEQ(MPA,MPB)
      CALL IMPACK(MPB,MB)
      RETURN
      END SUBROUTINE IPEQ

      SUBROUTINE IPFACT(IVAL,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER :: IVAL
      TYPE(MULTI) :: MA
      INTENT (IN) :: IVAL
      INTENT (INOUT) :: MA
      CALL IMFACT(IVAL,MPA)
      CALL IMPACK(MPA,MA)
      RETURN
      END SUBROUTINE IPFACT

      SUBROUTINE IPFM2I(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL FMUNPK(MA,MPA)
      CALL IMFM2I(MPA,MPB)
      CALL IMPACK(MPB,MB)
      RETURN
      END SUBROUTINE IPFM2I

      SUBROUTINE IPFORM(FORM,MA,STRING)
      USE ModLib_FMVALS
      IMPLICIT NONE
      CHARACTER(*) :: FORM,STRING
      TYPE(MULTI) :: MA
      INTENT (IN) :: MA,FORM
      INTENT (INOUT) :: STRING
      CALL IMUNPK(MA,MPA)
      CALL IMFORM(FORM,MPA,STRING)
      RETURN
      END SUBROUTINE IPFORM

      SUBROUTINE IPFPRT(FORM,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      CHARACTER(*) :: FORM
      TYPE(MULTI) :: MA
      INTENT (IN) :: MA,FORM
      CALL IMUNPK(MA,MPA)
      CALL IMFPRT(FORM,MPA)
      RETURN
      END SUBROUTINE IPFPRT

      SUBROUTINE IPGCD(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL IMUNPK(MA,MPA)
      CALL IMUNPK(MB,MPB)
      CALL IMGCD(MPA,MPB,MPC)
      CALL IMPACK(MPC,MC)
      RETURN
      END SUBROUTINE IPGCD

      SUBROUTINE IPI2FM(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL IMUNPK(MA,MPA)
      CALL IMI2FM(MPA,MPB)
      CALL FMPACK(MPB,MB)
      RETURN
      END SUBROUTINE IPI2FM

      SUBROUTINE IPI2M(IVAL,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      INTEGER :: IVAL
      INTENT (IN) :: IVAL
      INTENT (INOUT) :: MA
      CALL IMI2M(IVAL,MPA)
      CALL IMPACK(MPA,MA)
      RETURN
      END SUBROUTINE IPI2M

      SUBROUTINE IPINP(LINE,MA,LA,LB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER :: LA,LB
      CHARACTER :: LINE(LB)
      TYPE(MULTI) :: MA
      INTENT (IN) :: LINE,LA,LB
      INTENT (INOUT) :: MA
      CALL IMINP(LINE,MPA,LA,LB)
      CALL IMPACK(MPA,MA)
      RETURN
      END SUBROUTINE IPINP

      SUBROUTINE IPM2DP(MA,DVAL)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      DOUBLE PRECISION :: DVAL
      INTENT (IN) :: MA
      INTENT (INOUT) :: DVAL
      CALL IMUNPK(MA,MPA)
      CALL IMM2DP(MPA,DVAL)
      RETURN
      END SUBROUTINE IPM2DP

      SUBROUTINE IPM2I(MA,IVAL)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      INTEGER :: IVAL
      INTENT (IN) :: MA
      INTENT (INOUT) :: IVAL
      CALL IMUNPK(MA,MPA)
      CALL IMM2I(MPA,IVAL)
      RETURN
      END SUBROUTINE IPM2I

      SUBROUTINE IPMAX(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL IMUNPK(MA,MPA)
      CALL IMUNPK(MB,MPB)
      CALL IMMAX(MPA,MPB,MPC)
      CALL IMPACK(MPC,MC)
      RETURN
      END SUBROUTINE IPMAX

      SUBROUTINE IPMIN(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL IMUNPK(MA,MPA)
      CALL IMUNPK(MB,MPB)
      CALL IMMIN(MPA,MPB,MPC)
      CALL IMPACK(MPC,MC)
      RETURN
      END SUBROUTINE IPMIN

      SUBROUTINE IPMOD(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL IMUNPK(MA,MPA)
      CALL IMUNPK(MB,MPB)
      CALL IMMOD(MPA,MPB,MPC)
      CALL IMPACK(MPC,MC)
      RETURN
      END SUBROUTINE IPMOD

      SUBROUTINE IPMPY(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL IMUNPK(MA,MPA)
      CALL IMUNPK(MB,MPB)
      CALL IMMPY(MPA,MPB,MPC)
      CALL IMPACK(MPC,MC)
      RETURN
      END SUBROUTINE IPMPY

      SUBROUTINE IPMPYI(MA,IVAL,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTEGER :: IVAL
      INTENT (IN) :: MA,IVAL
      INTENT (INOUT) :: MB
      CALL IMUNPK(MA,MPA)
      CALL IMMPYI(MPA,IVAL,MPB)
      CALL IMPACK(MPB,MB)
      RETURN
      END SUBROUTINE IPMPYI

      SUBROUTINE IPMPYM(MA,MB,MC,MD)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC,MD
      INTENT (IN) :: MA,MB,MC
      INTENT (INOUT) :: MD
      CALL IMUNPK(MA,MPA)
      CALL IMUNPK(MB,MPB)
      CALL IMUNPK(MC,MPC)
      CALL IMMPYM(MPA,MPB,MPC,MPD)
      CALL IMPACK(MPD,MD)
      RETURN
      END SUBROUTINE IPMPYM

      SUBROUTINE IPOUT(MA,LINE,LB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      INTEGER :: LB
      CHARACTER :: LINE(LB)
      INTENT (IN) :: MA,LB
      INTENT (INOUT) :: LINE
      CALL IMUNPK(MA,MPA)
      CALL IMOUT(MPA,LINE,LB)
      RETURN
      END SUBROUTINE IPOUT

      SUBROUTINE IPPMOD(MA,MB,MC,MD)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC,MD
      INTENT (IN) :: MA,MB,MC
      INTENT (INOUT) :: MD
      CALL IMUNPK(MA,MPA)
      CALL IMUNPK(MB,MPB)
      CALL IMUNPK(MC,MPC)
      CALL IMPMOD(MPA,MPB,MPC,MPD)
      CALL IMPACK(MPD,MD)
      RETURN
      END SUBROUTINE IPPMOD

      SUBROUTINE IPPRNT(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      INTENT (IN) :: MA
      CALL IMUNPK(MA,MPA)
      CALL IMPRNT(MPA)
      RETURN
      END SUBROUTINE IPPRNT

      SUBROUTINE IPPWR(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL IMUNPK(MA,MPA)
      CALL IMUNPK(MB,MPB)
      CALL IMPWR(MPA,MPB,MPC)
      CALL IMPACK(MPC,MC)
      RETURN
      END SUBROUTINE IPPWR

      SUBROUTINE IPREAD(KREAD,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      INTEGER :: KREAD
      INTENT (IN) :: KREAD
      INTENT (INOUT) :: MA
      CALL IMREAD(KREAD,MPA)
      CALL IMPACK(MPA,MA)
      RETURN
      END SUBROUTINE IPREAD

      SUBROUTINE IPSIGN(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL IMUNPK(MA,MPA)
      CALL IMUNPK(MB,MPB)
      CALL IMSIGN(MPA,MPB,MPC)
      CALL IMPACK(MPC,MC)
      RETURN
      END SUBROUTINE IPSIGN

      SUBROUTINE IPSQR(MA,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL IMUNPK(MA,MPA)
      CALL IMSQR(MPA,MPB)
      CALL IMPACK(MPB,MB)
      RETURN
      END SUBROUTINE IPSQR

      SUBROUTINE IPST2M(STRING,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      CHARACTER(*) :: STRING
      TYPE(MULTI) :: MA
      INTENT (IN) :: STRING
      INTENT (INOUT) :: MA
      CALL IMST2M(STRING,MPA)
      CALL IMPACK(MPA,MA)
      RETURN
      END SUBROUTINE IPST2M

      SUBROUTINE IPSUB(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL IMUNPK(MA,MPA)
      CALL IMUNPK(MB,MPB)
      CALL IMSUB(MPA,MPB,MPC)
      CALL IMPACK(MPC,MC)
      RETURN
      END SUBROUTINE IPSUB

      SUBROUTINE IPWRIT(KWRITE,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER :: KWRITE
      TYPE(MULTI) :: MA
      INTENT (IN) :: MA,KWRITE
      CALL IMUNPK(MA,MPA)
      CALL IMWRIT(KWRITE,MPA)
      RETURN
      END SUBROUTINE IPWRIT


!  These are the longer and more readable routine names, equivalent to the older names.

      FUNCTION IPCOMPARE(MA,LREL,MB)
      USE ModLib_FMVALS
      IMPLICIT NONE
      LOGICAL :: IPCOMPARE
      LOGICAL, EXTERNAL :: IPCOMP
      CHARACTER(*) :: LREL
      TYPE(MULTI) :: MA,MB
      IPCOMPARE = IPCOMP(MA,LREL,MB)
      RETURN
      END FUNCTION IPCOMPARE

      SUBROUTINE IPFPRINT(FORM,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      CHARACTER(*) :: FORM
      TYPE(MULTI) :: MA
      CALL IPFPRT(FORM,MA)
      RETURN
      END SUBROUTINE IPFPRINT

      SUBROUTINE IPMPY_MOD(MA,MB,MC,MD)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC,MD
      CALL IPMPYM(MA,MB,MC,MD)
      RETURN
      END SUBROUTINE IPMPY_MOD

      SUBROUTINE IPPOWER_MOD(MA,MB,MC,MD)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC,MD
      CALL IPPMOD(MA,MB,MC,MD)
      RETURN
      END SUBROUTINE IPPOWER_MOD

      SUBROUTINE IPPRINT(MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA
      CALL IPPRNT(MA)
      RETURN
      END SUBROUTINE IPPRINT

      SUBROUTINE IPPOWER(MA,MB,MC)
      USE ModLib_FMVALS
      IMPLICIT NONE
      TYPE(MULTI) :: MA,MB,MC
      CALL IPPWR(MA,MB,MC)
      RETURN
      END SUBROUTINE IPPOWER

      SUBROUTINE IPWRITE(KWRITE,MA)
      USE ModLib_FMVALS
      IMPLICIT NONE
      INTEGER :: KWRITE
      TYPE(MULTI) :: MA
      CALL IPWRIT(KWRITE,MA)
      RETURN
      END SUBROUTINE IPWRITE
