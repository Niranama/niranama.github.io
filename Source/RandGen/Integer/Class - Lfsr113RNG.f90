
MODULE Class_Lfsr113RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Lfsr113RNG* type and its related routines.
!   The *Lfsr113RNG* type is an *Integer* PRNG type that directly extends
!   the *IntegerRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by an *Integer* PRNG type.  <br>
!   In particular, the *Lfsr113RNG* type provides an implementation of the
!   *NextIntegerImpl* deferred procedure based on the 32-bit *LFSR113*
!   (composite linear feedback shift register) algorithm by Pierre L'Ecuyer. <br>
!   The *LFSR113* PRNG has four 32-bit states combined by a bitwise xor.
!   Its period length is approximatively 2<sup>113</sup>. <br>
!   It is important to note that the *LFSR113* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  Also, it should be noted that the implementation of
!   this PRNG is based on references #2-3. <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://www.ams.org/journals/mcom/1999-68-225/S0025-5718-99-01039-X/">
!       L'Ecuyer, P. 1999. Tables of maximally equidistributed combined LFSR
!       generators. Mathematics of Computation, 68(225): 261-269. </a> <br>
!   [2] <a href="https://wp.csiro.au/alanmiller/random/lfsr113.f90">
!       Lin_Feedback_Shift_Reg: a Fortran 90 module implementation of the LFSR113
!       generator by Alan Miller. </a> <br>
!   [3] <a href="http://umontreal-simul.github.io/ssj/docs/master/classumontreal_1_1ssj_1_1rng_1_1LFSR113.html">
!       Package umontreal.ssj.rng: LFSR113 Class Reference. </a>

!** USE STATEMENTS:
    USE ModBase_Common
    USE Class_BaseRNG
    USE Class_IntegerRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Lfsr113RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'Class_Lfsr113RNG'

!** DERIVED TYPE DEFINITIONS
    !> The *Lfsr113RNG* type is an *Integer* PRNG type based on 32-bit composite
    !  linear feedback shift register (LFSR) algorithm by Pierre L'Ecuyer.
    TYPE, EXTENDS(IntegerRNG)  :: Lfsr113RNG
        PRIVATE
        tInteger    :: State1 =  153587801
        tInteger    :: State2 = -759022222
        tInteger    :: State3 =  1288503317
        tInteger    :: State4 = -1718083407
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => Lfsr113RNG_BaseInit
        !> *NextIntegerImpl* is a deferred procedure. <br>
        !  Use the *NextInteger* method in place of the *NextIntegerImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE       :: NextIntegerImpl  => Lfsr113RNG_NextInteger
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => Lfsr113RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => Lfsr113RNG_GetSeedSize
        ! ---------------------------------------------------------------------
        ! -----                 Overridden Procedures                     -----
        ! ---------------------------------------------------------------------
        !> *NextDoubleImpl* is an overridden procedure. <br>
        !  Use the *NextDouble* method in place of the *NextDoubleImpl* method
        !  to generate a 64-bit real number.
        PROCEDURE   :: NextDoubleImpl               => Lfsr113RNG_NextDouble
        !> *Default_NextIntegerLimits* is an overridden procedure. <br>
        !  Use the *NextInteger* method in place of the *Default_NextIntegerLimits*
        !  method to generate a 32-bit integer number between the given bound.
        PROCEDURE   :: Default_NextIntegerLimits    => Lfsr113RNG_NextIntegerLimits
        ! ---------------------------------------------------------------------
    END TYPE Lfsr113RNG

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Lfsr113RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'Lfsr113RNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Lfsr113RNG), INTENT(INOUT)    :: RNG      !! 'Lfsr113RNG' object
    tInteger,          INTENT(IN)       :: Seed(:)  !! seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! set initial seeds
    SELECT CASE (SIZE(Seed))
    CASE (1)
        RNG%State1 = IEOR(RNG%State1, Seed(1))
    CASE (2)
        RNG%State1 = IEOR(RNG%State1, Seed(1))
        RNG%State2 = IEOR(RNG%State2, Seed(2))
    CASE (3)
        RNG%State1 = IEOR(RNG%State1, Seed(1))
        RNG%State2 = IEOR(RNG%State2, Seed(2))
        RNG%State3 = IEOR(RNG%State3, Seed(3))
    CASE (4)
        RNG%State1 = IEOR(RNG%State1, Seed(1))
        RNG%State2 = IEOR(RNG%State2, Seed(2))
        RNG%State3 = IEOR(RNG%State3, Seed(3))
        RNG%State4 = IEOR(RNG%State4, Seed(4))
    END SELECT

    IF (IAND(RNG%State1, -2)   == 0) RNG%State1 = RNG%State1 - 1023
    IF (IAND(RNG%State2, -8)   == 0) RNG%State2 = RNG%State2 - 1023
    IF (IAND(RNG%State3, -16)  == 0) RNG%State3 = RNG%State3 - 1023
    IF (IAND(RNG%State4, -128) == 0) RNG%State4 = RNG%State4 - 1023

    RETURN

END SUBROUTINE Lfsr113RNG_BaseInit

!******************************************************************************

FUNCTION Lfsr113RNG_NextInteger(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 32-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Lfsr113RNG), INTENT(INOUT)    :: RNG      !! 'Lfsr113RNG' object
    tInteger                            :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: B

! FLOW
    
    B          = SHIFTR(IEOR(SHIFTL(RNG%State1,  6), RNG%State1), 13)
    RNG%State1 = IEOR(SHIFTL(IAND(RNG%State1,   -2), 18), B)
    B          = SHIFTR(IEOR(SHIFTL(RNG%State2,  2), RNG%State2), 27)
    RNG%State2 = IEOR(SHIFTL(IAND(RNG%State2,   -8), 2), B)
    B          = SHIFTR(IEOR(SHIFTL(RNG%State3, 13), RNG%State3), 21)
    RNG%State3 = IEOR(SHIFTL(IAND(RNG%State3,  -16), 7), B)
    B          = SHIFTR(IEOR(SHIFTL(RNG%State4,  3), RNG%State4), 12)
    RNG%State4 = IEOR(SHIFTL(IAND(RNG%State4, -128), 13), B)
    
    RandNum = IEOR(IEOR(IEOR(RNG%State1, RNG%State2), RNG%State3), RNG%State4)
    
    RETURN

END FUNCTION Lfsr113RNG_NextInteger

!******************************************************************************

FUNCTION Lfsr113RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Lfsr113RNG), INTENT(IN)   :: RNG      !! 'Lfsr113RNG' object
    tCharAlloc                      :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'Lfsr113RNG'
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Lfsr113RNG_GetName

!******************************************************************************

FUNCTION Lfsr113RNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Lfsr113RNG), INTENT(IN)   :: RNG      !! 'Lfsr113RNG' object
    tIndex                          :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = 4
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Lfsr113RNG_GetSeedSize

!******************************************************************************

FUNCTION Lfsr113RNG_NextDouble(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random floating-point value.  This routine
    !  overrides the default implementation.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Lfsr113RNG), INTENT(INOUT)    :: RNG      !! 'Lfsr113RNG' object
    tDouble                             :: RandNum  !! random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble, PARAMETER  :: InvTwo24 = 5.9604644775390625E-8_kDouble     ! 2**(-24)
    tDouble, PARAMETER  :: Epsilon  = 5.5511151231257827E-17_kDouble    ! 2**(-54)
    ! generator constant: make sure that double values 0 and 1 never occur
    tLong,   PARAMETER  :: TwoPow32 = ToLong(Z'0000000100000000')   ! 2**32
    tDouble, PARAMETER  :: DNorm    = 1.0_kDouble/(TwoPow32+1)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: LongRnd(2)
    tIndex      :: I

! FLOW
    
    DO I = 1, 2
        LongRnd(I) = ToLong(RNG%NextInteger())
        IF (LongRnd(I) <= 0_kLong) LongRnd(I) = LongRnd(I) + TwoPow32
    END DO

    ! Make sure that double values 0 and 1 never occur
    RandNum = LongRnd(1)*DNorm
    
    RandNum = MOD((RandNum + (LongRnd(2)*DNorm) * InvTwo24), 1.0_kDouble) + Epsilon
    
    RETURN

END FUNCTION Lfsr113RNG_NextDouble

!******************************************************************************

FUNCTION Lfsr113RNG_NextIntegerLimits(RNG, Bound1, Bound2) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To generate a random 32-bit-integer value in the specified range.
    !  If *Bound2* is not specified, the value is in the range between
    !  0 and *Bound1*.  Otherwise, the value is in the range between
    !  *Bound1* and *Bound2*. <br>
    !  It should be noted that both *Bound1* and *Bound2* arguments can
    !  have either a positive or a negative value.  The returned value
    !  is always in between the lower limit (inclusive) and the upper
    !  limit (exclusive). <br>
    !  This routine overrides the default implementation.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Lfsr113RNG),  INTENT(INOUT)   :: RNG      !! 'Lfsr113RNG' object
    tInteger,           INTENT(IN)      :: Bound1   !! a required limit
    tInteger, OPTIONAL, INTENT(IN)      :: Bound2   !! an optional limit
    tInteger                            :: RandNum  !! random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong,   PARAMETER  :: TwoPow32 = ToLong(Z'0000000100000000')   ! 2**32
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: Lower, Diff
    tLong       :: D, Q, R, LongRnd

! FLOW
    
    ! check specified input and set Diff and Lower values
    IF (PRESENT(Bound2)) THEN
        Diff  = ABS(Bound1 - Bound2)
        Lower = MIN(Bound1, Bound2)
    ELSE
        Diff  = ABS(Bound1)
        Lower = MIN(Bound1, 0_kInteger)
    END IF
    
    ! return quickly if Diff is zero
    IF (Diff == 0_kInteger) THEN
        RandNum = Bound1
        RETURN
    END IF

    D = Diff + 1_kLong
    Q = TwoPow32 / D
    R = MOD(TwoPow32, D)
    DO
        LongRnd = ToLong(RNG%NextInteger())
        IF (LongRnd <= 0_kLong) LongRnd = LongRnd + TwoPow32
        IF (LongRnd < TwoPow32 - R) EXIT
    END DO

    RandNum = ToInteger(LongRnd / Q)  + Lower

    RETURN

END FUNCTION Lfsr113RNG_NextIntegerLimits

!******************************************************************************

END MODULE Class_Lfsr113RNG
    
!******************************************************************************
