!  ************************************************************************************************************
!
!                                        ____  ____  ______  ______________
!                                       / __ \/ __ \/  _/  |/  / ____/ ___/
!                                      / /_/ / /_/ // // /|_/ / __/  \__ \
!                                     / ____/ _, _// // /  / / /___ ___/ /
!                                    /_/   /_/ |_/___/_/  /_/_____//____/
!
!                                         Constants and parameters
!
!  MIT License
!
!  Copyright (c) 2014-2023 Federico Perini
!  Parts of this code Copyright (c) the Primes.jl contributors and
!  All codes published under https://people.ksp.sk/~misof/primes/
!  are available under the CC BY-NC 4.0 Int'l license.
!
!  ************************************************************************************************************
MODULE ModLib_PrimeNumbers

    USE ISO_FORTRAN_ENV
    USE ModLib_PrimeConstants2
    USE ModBase_SInt128

    IMPLICIT NONE
    PRIVATE

    ! prime(n) returns the n-th prime number
    PUBLIC :: PRIME

    ! next_prime(n, i) returns the i-th prime number next to n (n not included)
    PUBLIC :: NEXT_PRIME

    ! call prime_factors(n, factors): return all prime factors, and their multiplicities, of an integer.
    ! factors(FACTORS_PRIME,:) -> all prime factors
    ! factors(FACTORS_POWER,:) -> their powers
    PUBLIC :: PRIME_FACTORS

    ! Check if an integer is prime
    PUBLIC :: IS_PRIME
    INTERFACE IS_PRIME
        MODULE PROCEDURE IS_PRIME32
        MODULE PROCEDURE IS_PRIME64
    END INTERFACE IS_PRIME

    ! Return a list of primes within a given integer range (or upper bound)
    PUBLIC :: PRIMES
    INTERFACE PRIMES
        MODULE PROCEDURE PRIMES_BOUNDS
        MODULE PROCEDURE PRIMES_LIMIT
    END INTERFACE PRIMES

    ! Return a logical mask of primes within a given integer range (or upper bound)
    PUBLIC :: PRIMES_MASK
    INTERFACE PRIMES_MASK
        MODULE PROCEDURE PRIMES_MASK_HI
        MODULE PROCEDURE PRIMES_MASK_BOUNDS
    END INTERFACE PRIMES_MASK

    ! Module parameters
    CHARACTER(*), PARAMETER :: THIS_MODULE = "[prime_numbers]"
    CHARACTER(*), PARAMETER :: FMT_ER = "(1x,a,' Overflow detected: n=',I9,' not between 1 and ',I9)"
    CHARACTER(*), PARAMETER :: FMT_ER2 = "(1x,a,' Overflow detected: ',I9,' not in [2,',I9,']')"

    ! Internal
    PUBLIC :: GENERATE_MIN_FACTORS

    CONTAINS

    ! Return the n-th prime number
    FUNCTION PRIME(N) RESULT(PRIME_NUMBER)
        INTEGER(IP), INTENT(IN) :: N
        INTEGER(IP)             :: PRIME_NUMBER,MORE

        SELECT CASE (N)
           CASE (        1:  CHUNK); PRIME_NUMBER = P1_TO_10000(N)
           CASE (  CHUNK+1:2*CHUNK); PRIME_NUMBER = P10001_TO_20000(N-  CHUNK)
           CASE (2*CHUNK+1:3*CHUNK); PRIME_NUMBER = P20001_TO_30000(N-2*CHUNK)
           CASE (3*CHUNK+1:4*CHUNK); PRIME_NUMBER = P30001_TO_40000(N-3*CHUNK)
           CASE (4*CHUNK+1:5*CHUNK); PRIME_NUMBER = P40001_TO_50000(N-4*CHUNK)
           CASE (5*CHUNK+1:)
               ! Use next-prime outside of the table
               MORE = N-5*CHUNK
               PRIME_NUMBER = NEXT_PRIME(P40001_TO_50000(CHUNK),MORE)
           CASE DEFAULT
               ! Negative/invalid
               PRIME_NUMBER = -HUGE(PRIME_NUMBER)
        END SELECT

    END FUNCTION PRIME

    ! Return a list of the first i-th prime numbers
    FUNCTION PRIMES_LIMIT(LIMIT) RESULT(LIST)
        INTEGER(IP), INTENT(IN) :: LIMIT
        INTEGER(IP), ALLOCATABLE :: LIST(:)
        LIST = PRIMES_BOUNDS(1,LIMIT)
    END FUNCTION PRIMES_LIMIT

    ! Return a list of the i-th prime numbers (from lo- to hi-)
    FUNCTION PRIMES_BOUNDS(LO,HI) RESULT(LIST)
        INTEGER(IP), INTENT(IN) :: LO,HI
        INTEGER(IP), ALLOCATABLE :: LIST(:)

        INTEGER(IP), PARAMETER :: I_235(3) = [INTEGER(IP) :: 2,3,5]
        INTEGER(IP) :: I,LOSAFE,LWI,NMASK,NSHORT,LAST
        LOGICAL(LP), ALLOCATABLE :: MASK(:)
        LOGICAL(LP) :: HAS_235(3)

        IF (LO>HI) THEN
            ! Invalid range
            ALLOCATE(LIST(0))
            RETURN
        ELSE

            ! Check small primes
            HAS_235 = LO<=I_235 .AND. HI>=I_235

            IF (HI<7) THEN
                LIST = PACK(I_235,HAS_235)
            ELSE

                LOSAFE = MAX(LO,2_IP)

                MASK   = PRIMES_MASK_GE7_BOUNDS(MAX(7,LO), HI)
                NMASK  = COUNT(MASK)
                NSHORT = COUNT(HAS_235)

                ALLOCATE(LIST(NMASK+NSHORT))

                IF (NSHORT>0) LIST(1:NSHORT) = PACK(I_235,HAS_235)
                LAST = NSHORT
                LWI = WHEEL_INDEX(LOSAFE-1)

                DO I=1,SIZE(MASK,KIND=IP)
                    IF (MASK(I)) THEN
                        LIST(LAST+1) = WHEEL_PRIME(I+LWI)
                        LAST = LAST+1
                    END IF
                END DO

            END IF

        END IF

    END FUNCTION PRIMES_BOUNDS

    ! Detect if an integer number is prime
    LOGICAL FUNCTION IS_PRIME32(N) RESULT(IS_PRIME)
       INTEGER(IP), INTENT(IN) :: N
       IS_PRIME = IS_PRIME64(INT(N,WP))
    END FUNCTION IS_PRIME32

    LOGICAL FUNCTION IS_PRIME64(N) RESULT(IS_PRIME)
       INTEGER(WP), INTENT(IN) :: N

       INTEGER(WP), PARAMETER :: SIMPLE_FACTORS(*) = [3,5,7,11,13,17,19,23]
       INTEGER(IP) :: W

       IF (N<2) THEN
          IS_PRIME = .FALSE.
       ELSEIF (MOD(N,2_8)==0) THEN
          ! Even
          IS_PRIME = N==2
       ELSEIF (N<N_SMALL) THEN
          ! Look-up table
          IS_PRIME = ODD_MIN_FACTOR(N)==N
       ELSEIF (ANY(MOD(N,SIMPLE_FACTORS)==0)) THEN
          ! Very large number. Test simple factors first
          IS_PRIME = .FALSE.
       ELSEIF (N<2_WP**32) THEN
          ! Perform Miller-Rabin test
          IS_PRIME = IS_PRIME_32(INT(N,IP), INT(WITNESSES(N),IP))
       ELSE

          IF (.NOT.IS_SPRP64(N,2_WP)) THEN
              IS_PRIME = .FALSE.
              RETURN
          ELSE
              W = WITNESSES_FOR_64(N)
              IS_PRIME =       IS_SPRP64(N,INT(IAND(W,4095_IP),WP)) &
                         .AND. IS_SPRP64(N,INT(SHIFTA(W,12_IP),WP))
          END IF

       END IF

    END FUNCTION IS_PRIME64

    ! Return the minimum factor of n for 1) n odd; 2) 1<n<N_SMALL
    ELEMENTAL INTEGER(WP) FUNCTION ODD_MIN_FACTOR(N)
       INTEGER(WP), INTENT(IN) :: N
       INTEGER(WP) :: M
       M = INT(MIN_FACTOR(SHIFTA(N,1)),WP)
       ODD_MIN_FACTOR = MERGE(N,M,M==1_IP)
    END FUNCTION ODD_MIN_FACTOR

    ! Find position of stored point x in list, list *already* sorted in ascending order
    PURE RECURSIVE SUBROUTINE QUICKFIND_INT(LIST,X,IT,BOUNDS)
       INTEGER(IP), INTENT(IN)              :: LIST(:)
       INTEGER(IP), INTENT(IN)              :: X
       INTEGER(IP), INTENT(INOUT)           :: IT
       INTEGER(IP), INTENT(IN), OPTIONAL    :: BOUNDS(2)
       INTEGER(IP), PARAMETER :: MAX_QUICKFIND_SIZE = 16_IP

       ! Local variables
       INTEGER(IP) :: I,N,L,U

       N = SIZE(LIST,KIND=IP)

       ! Define current global range bounds
       IF (PRESENT(BOUNDS)) THEN
         L = BOUNDS(1)
         U = BOUNDS(2)
       ELSE
         L = LBOUND(LIST,1,KIND=IP)
         U = UBOUND(LIST,1,KIND=IP)
       END IF
       IF (N<=MAX_QUICKFIND_SIZE) THEN
          ! Point is lower than lowest value
          IT = -HUGE(IT)
          IF (X<LIST(1)) RETURN

          DO I=1,N
             IF (X==LIST(I)) THEN
                IT = L-1+I
                RETURN
             END IF
          END DO

          ! Point is bigger than largest value
          IT = HUGE(IT)
       ELSE
          ! Set is big, use quicksort
          I = INT(N/2,KIND=IP)
          SPLIT_LIST: IF (X>=LIST(I)) THEN
             CALL QUICKFIND_INT(LIST(I:),X,IT,[L-1+I,U])
          ELSE SPLIT_LIST
             CALL QUICKFIND_INT(LIST(:I),X,IT,[L,L-1+I])
          END IF SPLIT_LIST
       END IF
    END SUBROUTINE QUICKFIND_INT

    ! http://people.ksp.sk/~misof/primes
    ELEMENTAL LOGICAL(LP) FUNCTION IS_PRIME_32(N, A)
        INTEGER(IP), INTENT(IN) :: N,A

        INTEGER(IP) :: D,S,R,AA
        INTEGER(WP) :: CUR,PW

        AA = A
        D  = N-1_IP

        ! Find trailing zeros and shift by them
        S  = TRAILZ(D)
        D  = SHIFTA(D,S)

        CUR = 1_WP
        PW  = D

        DO WHILE (PW/=0)
            IF (IAND(PW,1_WP)/=0) CUR = MOD(CUR*AA,N)
            AA = INT(MOD(INT(AA,WP)**2,N),IP)
            PW = SHIFTA(PW,1)
        END DO

        IF (CUR==1_WP) THEN
            IS_PRIME_32 = .TRUE._LP
            RETURN
        END IF

        DO R=0,S-1
            IF (CUR==N-1) THEN
                IS_PRIME_32 = .TRUE._LP
                RETURN
            END IF
            CUR = MOD(CUR**2,N)
        END DO
        IS_PRIME_32 = .FALSE._LP
        RETURN
    END FUNCTION IS_PRIME_32

    ELEMENTAL INTEGER(WP) FUNCTION WITNESSES(N)
       INTEGER(WP), INTENT(IN) :: N
       INTEGER(WP) :: I
       I =      IEOR(SHIFTA(N,16), N) * INT(Z'45d9f3b',WP)
       I =      IEOR(SHIFTA(I,16), I) * INT(Z'45d9f3b',WP)
       I = IAND(IEOR(SHIFTA(I,16), I), 255_WP)
       WITNESSES = INT(WITNESSES32(I+1_WP),WP)
    END FUNCTION WITNESSES

    ELEMENTAL INTEGER(IP) FUNCTION WITNESSES_FOR_64(N)
       INTEGER(WP), INTENT(IN) :: N
       INTEGER(WP) :: I
       I =      IEOR(SHIFTA(N,32), N) * INT(Z'45d9f3b3335b369',WP)
       I =      IEOR(SHIFTA(I,32), I) * INT(Z'3335b36945d9f3b',WP)
       I = IAND(IEOR(SHIFTA(I,32), I), 16383_WP)
       WITNESSES_FOR_64 = WITNESSES64(I+1_WP)
    END FUNCTION WITNESSES_FOR_64

    ELEMENTAL INTEGER(IP) FUNCTION WHEEL_INDEX(N)
       INTEGER(IP), INTENT(IN) :: N
       INTEGER(IP) :: D,R
       D = INT((N-1)/30,IP)
       R = MOD(N-1,30)
       WHEEL_INDEX = 8*D+WHEEL_INDICES(R+2)
    END FUNCTION WHEEL_INDEX

    ELEMENTAL INTEGER(IP) FUNCTION WHEEL_PRIME(N)
       INTEGER(IP), INTENT(IN) :: N
       INTEGER(IP) :: D,R
       D = SHIFTA(N-1,3_IP)
       R = IAND(N-1,7_IP)
       WHEEL_PRIME = 30*D+WHEEL_PRIMES(R+1)
    END FUNCTION WHEEL_PRIME

    FUNCTION GENERATE_MIN_FACTORS(LIMIT) RESULT(RES)
       INTEGER(IP), INTENT(IN) :: LIMIT
       INTEGER(IP), ALLOCATABLE :: RES(:)

       INTEGER(IP) :: I,M,N

       ! Only include odd numbers
       ALLOCATE(RES(MAX(0,(LIMIT-3)/2+1)))

       N = 0
       DO I=3,LIMIT,2
          N = N+1
          M = MIN_FACTOR(I)
          RES(N) = MERGE(1_IP,M,M==I)
       END DO
       IF (N/=SIZE(RES)) THEN
          PRINT *, N, SIZE(RES)
          STOP 'catastrophic! n/=size(Res)'
       ENDIF

       CONTAINS

          INTEGER(IP) FUNCTION MIN_FACTOR(N)
             INTEGER(IP), INTENT(IN) :: N
             IF (N<4) THEN
                MIN_FACTOR = N
                RETURN
             END IF
             DO MIN_FACTOR=3,ISQRT(N),2
                IF (MOD(N,MIN_FACTOR)==0) RETURN
             END DO
             MIN_FACTOR = N
          END FUNCTION MIN_FACTOR

    END FUNCTION GENERATE_MIN_FACTORS

    ! Largest integer for which isqrt**2<=n
    ELEMENTAL INTEGER(IP) FUNCTION ISQRT(N)
       INTEGER(IP), INTENT(IN) :: N
       INTRINSIC :: SQRT
       ISQRT = FLOOR(SQRT(REAL(N,WP)),IP)
    END FUNCTION ISQRT


    ! Return a list of prime numbers between low and hi.
    PURE FUNCTION PRIMES_MASK_BOUNDS(LO,HI) RESULT(MASK)
       INTEGER(IP), INTENT(IN) :: LO,HI
       LOGICAL(LP), ALLOCATABLE :: MASK(:)

       INTEGER(IP), PARAMETER :: I_235(3) = [INTEGER(IP) :: 2,3,5]
       INTEGER(IP) :: LSI,LWI,I
       LOGICAL(LP) :: HAS_235(3)
       LOGICAL(LP), ALLOCATABLE :: WHEEL_SIEVE(:)

       ! Invalid inputs
       IF (.NOT.(HI>=LO .AND. LO>0 .AND. HI<HUGE(HI))) THEN
           ALLOCATE(MASK(0))
           RETURN
       END IF

       ! Internally use flexible bounds, will return a [1:hi-lo+1] array
       ALLOCATE(MASK(LO:HI),SOURCE=.FALSE._LP)

       ! Check small primes
       HAS_235 = LO<=I_235 .AND. HI>=I_235

       IF (HAS_235(1)) MASK(2) = .TRUE._LP
       IF (HAS_235(2)) MASK(3) = .TRUE._LP
       IF (HAS_235(3)) MASK(5) = .TRUE._LP

       IF (HI<7) RETURN

       WHEEL_SIEVE = PRIMES_MASK_GE7_BOUNDS(MAX(7, LO), HI)
       LSI = LO - 1
       LWI = WHEEL_INDEX(LSI)
       DO I = 1,SIZE(WHEEL_SIEVE,KIND=IP)
           MASK(WHEEL_PRIME(I+LWI)) = WHEEL_SIEVE(I)
       END DO
       RETURN
    END FUNCTION PRIMES_MASK_BOUNDS

    PURE FUNCTION PRIMES_MASK_HI(LIMIT) RESULT(MASK)
       INTEGER(IP), INTENT(IN) :: LIMIT
       LOGICAL(LP), ALLOCATABLE :: MASK(:)
       MASK = PRIMES_MASK_BOUNDS(1,LIMIT)
    END FUNCTION PRIMES_MASK_HI

    PURE FUNCTION PRIMES_MASK_GE7_HI(LIMIT) RESULT(MASK)
       INTEGER(IP), INTENT(IN) :: LIMIT
       LOGICAL(LP), ALLOCATABLE :: MASK(:)

       INTEGER(IP) :: M,N,P,Q,I,J

       ! This is an internal function that NEEDS to operate with limit>=7
       IF (.NOT.LIMIT>=7) THEN
          ALLOCATE(MASK(0))
          RETURN
       END IF

       N = WHEEL_INDEX(LIMIT)
       M = WHEEL_PRIME(M)
       ALLOCATE(MASK(N),SOURCE=.TRUE._LP)

       DO I=1,WHEEL_INDEX(ISQRT(LIMIT))
          IF (MASK(I)) THEN
             P = WHEEL_PRIME(I)
             Q = P**2
             J = IAND(I-1,7_IP)+1_IP
             DO WHILE (Q<=M)
                IF (WHEEL_INDEX(Q)<=N) MASK(WHEEL_INDEX(Q)) = .FALSE._LP
                Q = Q+WHEEL(J)*P
                J = IAND(J,7_IP)+1_IP
             END DO
          ENDIF
       END DO

    END FUNCTION PRIMES_MASK_GE7_HI

    PURE FUNCTION PRIMES_MASK_GE7_BOUNDS(LO,HI) RESULT(MASK)
       INTEGER(IP), INTENT(IN) :: LO,HI
       LOGICAL(LP), ALLOCATABLE :: MASK(:)

       INTEGER(IP) :: WLO,WHI,M,I,J,P,Q
       INTEGER(WP) :: R
       LOGICAL(LP), ALLOCATABLE :: SMALL_MASK(:)

       ! This is an internal function that needs to be called with min(lo,hi)>=7
       IF (.NOT.MIN(LO,HI)>=7) THEN
           ALLOCATE(MASK(0))
           RETURN
       ENDIF

       WLO = WHEEL_INDEX(LO-1)
       WHI = WHEEL_INDEX(HI)
       M   = WHEEL_PRIME(WHI)

       ALLOCATE(MASK(WHI-WLO), SOURCE=.TRUE._LP)
       IF (HI<49) RETURN

       SMALL_MASK = PRIMES_MASK_GE7_HI(ISQRT(HI))

       DO I=1,SIZE(SMALL_MASK)
          IF (SMALL_MASK(I)) THEN
            P = WHEEL_PRIME(I)
            J = WHEEL_INDEX(2*INT((LO-P-1)/(2*P),IP)+1)

            ! Use wide integer to agoid r<=m due to overflow
            R = INT(P,WP)*INT(WHEEL_PRIME(J+1),WP)
            IF (R>M) CYCLE

            J = IAND(J,7_IP) + 1_IP
            Q = INT(R,IP)

            ! q < 0 indicates that overflow occurred incrementing q
            DO WHILE (Q>=0_IP .AND. Q<=M)
                MASK(WHEEL_INDEX(Q) - WLO) = .FALSE._LP
                Q = Q + WHEEL(J) * P
                J = IAND(J,7_IP) + 1_IP
            END DO

          END IF
       END DO

    END FUNCTION PRIMES_MASK_GE7_BOUNDS

    ! given 0 <= a,b,n < 2^64, computes (a*b)%n without overflow
    INTEGER(WP) FUNCTION SAFE_MUL(A,B,N)
       INTEGER(WP), INTENT(IN) :: A,B,N

       TYPE(SInt128) :: QA,QN

       ! 128bit
       QA = SInt128(A)
       QN = SInt128(N)

       SAFE_MUL = MOD(QA*B,QN)
    END FUNCTION SAFE_MUL

    ! given 0 <= a,b,n < 2^64, computes (a^b)%n without overflow
    INTEGER(WP) FUNCTION SAFE_EXP(A,B,N) RESULT(RES)
       INTEGER(WP), INTENT(IN) :: A,B,N

       INTEGER(WP) :: PW,QA
       INTEGER(IP) :: I

       RES = 1
       PW  = 1
       QA  = A

       I   = 0
       DO WHILE (I<64 .AND. PW<=B)
          IF (IAND(B,PW)/=0) RES = SAFE_MUL(RES,QA,N)
          QA = SAFE_MUL(QA,QA,N)
          PW = SHIFTL(PW,1)
          I = I+1
       END DO

    END FUNCTION SAFE_EXP

    LOGICAL(LP) FUNCTION IS_SPRP64(N,A) RESULT(IS_PRIME)
       INTEGER(WP), INTENT(IN) :: N,A

       INTEGER(WP) :: D,CUR
       INTEGER(IP) :: S,R

       IF (N==A) THEN
          IS_PRIME = .TRUE._LP
          RETURN
       ELSEIF (MOD(N,A)==0) THEN
          IS_PRIME = .FALSE.
          RETURN
       END IF

       ! Find trailing zero bits and shift by them
       D = N-1_WP
       S = TRAILZ(D)
       D = SHIFTA(D,S)

       CUR = SAFE_EXP(A,D,N)
       IF (CUR==1_WP) THEN
          IS_PRIME = .TRUE._LP
          RETURN
       END IF

       R = 0;
       DO WHILE (R<S)
          IF (CUR==N-1_WP) THEN
             IS_PRIME = .TRUE._LP
             RETURN
          END IF
          CUR = SAFE_MUL(CUR,CUR,N)
          R = R+1
       END DO
       IS_PRIME = .FALSE._LP
    END FUNCTION IS_SPRP64


    ! Find the i-th prime number next (right) to n. If i is not provided, find the immediate next prime (i=1)
    INTEGER(IP) FUNCTION NEXT_PRIME(N, I)
       INTEGER(IP), INTENT(IN) :: N
       INTEGER(IP), OPTIONAL, INTENT(IN) :: I

       INTEGER :: USEN,USEI

       USEI = 1_IP; IF (PRESENT(I)) USEI = I
       USEN = MAX(N,1_IP)

       ! If possible, locate the next prime using the look-up table
       IF (USEI<=0) THEN !stop 'i-th next prime to be found must be >=1'
           NEXT_PRIME = -HUGE(NEXT_PRIME)
           RETURN
       END IF

       ! Handle 2
       IF (USEN<2_IP) THEN
          ! Start from 2
          USEN = 2_IP
          USEI = USEI-1
       ENDIF

       IF (USEI<=0) THEN
          NEXT_PRIME = USEN
          RETURN
       END IF

       ! Ensure that the iteration starts from a non-prime number
       IF (MOD(USEN,2_IP)/=0) USEN = USEN + 1_IP

       ! While I still have to find more
       MORE_NEEDED: DO WHILE (USEI>0)

           DO WHILE (USEN==2_IP .OR. .NOT.IS_PRIME(USEN))
              USEN = USEN+1_IP  ! Skip all even numbers
           END DO

           ! Prime found
           USEI = USEI-1_IP
           IF (USEI <= 0) EXIT

           ! Position to the next candidate
           USEN = USEN+1_IP
       END DO MORE_NEEDED

       NEXT_PRIME = USEN
    END FUNCTION NEXT_PRIME



    ! Return all prime factors, and their multiplicities, of an integer
    SUBROUTINE PRIME_FACTORS(N,FACTORS)
        INTEGER(IP), INTENT(IN) :: N
        INTEGER(IP), ALLOCATABLE, INTENT(OUT) :: FACTORS(:,:)

        ! This buffer should be large enough that the product of the 1st 4196 primes
        ! is surely out of the current precision
        INTEGER(IP) :: BUFFER(2,FACTORS_CHUNKSIZE),NFACTORS,REMAINDER,PRIME

        NFACTORS  = 1
        BUFFER(FACTORS_POWER,1) = 0
        REMAINDER = ABS(N)
        PRIME     = 2_IP

        ! The easy test
        IF (REMAINDER<2_IP) THEN
           ALLOCATE(FACTORS(2,0))
           RETURN
        ELSEIF (IS_PRIME(REMAINDER)) THEN
           ALLOCATE(FACTORS(2,1))
           FACTORS(FACTORS_PRIME,1) = REMAINDER
           FACTORS(FACTORS_POWER,1) = 1_IP
           RETURN
        END IF

        DO WHILE (REMAINDER>0_IP)

            ! Prime is a factor
            IF (MOD(REMAINDER,PRIME)==0) THEN
                REMAINDER = REMAINDER/PRIME

                ! Add to multiplicity
                BUFFER(FACTORS_POWER,NFACTORS) = BUFFER(FACTORS_POWER,NFACTORS)+1_IP

                IF (REMAINDER==1_IP) THEN
                    BUFFER(FACTORS_PRIME,NFACTORS) = PRIME
                    EXIT
                ENDIF
            ELSE

                ! Should we close a previous factor?
                IF (BUFFER(FACTORS_POWER,NFACTORS)>0) THEN
                    BUFFER(FACTORS_PRIME,NFACTORS)   = PRIME
                    BUFFER(FACTORS_POWER,NFACTORS+1) = 0
                    NFACTORS = NFACTORS+1
                END IF

                ! Try another prime
                PRIME = NEXT_PRIME(PRIME)
            END IF

        END DO

        ALLOCATE(FACTORS(2,NFACTORS),SOURCE=BUFFER(:,1:NFACTORS))

    END SUBROUTINE PRIME_FACTORS

END MODULE ModLib_PrimeNumbers

