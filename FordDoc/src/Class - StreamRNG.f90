
MODULE Class_StreamRNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *StreamRNG* type and its related routines.
!   The *StreamRNG* type is an abstract PRNG type that directly extends
!   the *BaseRNG* type.   It provides an additional API for a so-called
!   *Stream* PRNG.  It also provides default implementations of some
!   deferred procedures required by a PRNG where other deferred procedures
!   must be implemented by its subtypes.  <br>
!   By design, the *StreamRNG* type is provided as a base type for a *Stream*
!   PRNG, which can be considered as a virtual PRNG.  The full period of
!   a conventional PRNG is cut into a number of adjacent streams where a
!   *Stream* PRNG represents one of these streams.  A *Stream* PRNG can be
!   initialized into one of these streams depending on specified seed(s),
!   which represent a starting point of the current stream.  If a *Stream*
!   PRNG is initialized without providing seed(s), the starting point of
!   the current stream depends on default seed(s), which varies depending
!   on a specific PRNG.  A new *Stream* PRNG can be created by using the
!   *NewStream* method.  This new PRNG represents a stream next to the
!   current one.  For the concept of a stream PRNG and its sub-streams,
!   see references.  All so-called *Stream* PRNGs should extend from
!   this base type. <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://dl.acm.org/doi/10.1287/opre.50.6.1073.358">
!       Pierre L'Ecuyer, et. al. 2002. An Object-Oriented Random-Number Package with Many
!       Long Streams and Substreams.  Operations Research, 50(6), pp. 1073-1075. </a> <br>
!   [2] <a href="http://www.iro.umontreal.ca/~lecuyer/myftp/streams00/">RngStreams:
!       An Object-Oriented Random-Number Generator Package with Many Long Streams
!       and Substreams. Versions C, C++ and Java. </a> <br>
!   [3] <a href="http://simul.iro.umontreal.ca/ssj/indexe.html">SSJ: Stochastic
!       Simulation in Java</a>

!** USE STATEMENTS:
    USE ISO_C_BINDING,              ONLY: C_LOC, C_F_POINTER, C_NULL_PTR
    USE ModBase_Common
    USE ModBase_SIntUtil,           ONLY: MIN_I32, MAX_I32
    USE ModBase_Error_Handlers
    USE Class_BaseRNG
    USE Class_IntegerRNG,           ONLY: MaskL, I128_To_R128

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: StreamRNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'Class_StreamRNG'

!** DERIVED TYPE DEFINITIONS
    !> The *StreamRNG* type is an abstract PRNG type that directly extends
    !  the *BaseRNG* type.  It provides an additional API for a PRNG that
    !  can be used as a stream PRNG.  Therefore, all so-called *Stream*
    !  PRNGs should extend from this base type.
    TYPE, ABSTRACT, EXTENDS(BaseRNG)  :: StreamRNG
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----               Additional Deferred Procedure               -----
        ! ---------------------------------------------------------------------
        !> *NewStream* is a binding name of the *NewRNG* deferred procedure. <br>
        ! **Type-Bound Function**: NewStream <br>
        !  **Purpose**:  To return a clone of the specified PRNG initialized to the
        !                initial state of the next stream of the given PRNG, which
        !                represents the current stream. <br>
        !  **Usage**: <br>
        !   --->    NewRNG = PRNG%NewStream()
        PROCEDURE(NewRNG),   DEFERRED   :: NewStream
        !> *StartCurrSub* is a binding name of the *ResetSub* deferred procedure. <br>
        !  **Type-Bound Subroutine**: StartCurrSub <br>
        !  **Purpose**:  To reset the PRNG to the beginning state of its current
        !                sub-stream. <br>
        !  **Usage**: <br>
        !   --->    CALL PRNG%StartCurrSub()
        PROCEDURE(ResetSub), DEFERRED   :: StartCurrSub
        !> *StartNextSub* is a binding name of the *ResetNxt* deferred procedure. <br>
        !  **Type-Bound Subroutine**: StartNextSub <br>
        !  **Purpose**:  To reset the PRNG to the beginning state of its next
        !                sub-stream. <br>
        !  **Usage**: <br>
        !   --->    CALL PRNG%StartNextSub()
        PROCEDURE(ResetNxt), DEFERRED   :: StartNextSub
        ! ---------------------------------------------------------------------
        ! -----               Deferred Procedures Implemented             -----
        ! ---------------------------------------------------------------------
        !> *NextIntegerImpl* is a deferred procedure. <br>
        !  Use the *NextInteger* method in place of the *NextIntegerImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE       :: NextIntegerImpl  => Default_NextInteger
        !> *NextLongImpl* is a deferred procedure. <br>
        !  Use the *NextLong* method in place of the *NextLongImpl* method
        !  to generate a 64-bit integer number.
        PROCEDURE       :: NextLongImpl     => Default_NextLong
        !> *NextQuadImpl* is a deferred procedure. <br>
        !  Use the *NextQuad* method in place of the *NextQuadImpl* method
        !  to generate a 128-bit real number.
        PROCEDURE       :: NextQuadImpl     => Default_NextQuad
        ! ---------------------------------------------------------------------
        ! -----             Private (Overridden) Procedures               -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: Default_NextIntegerUpper => StreamRNG_NextIntegerUpper
        PROCEDURE, PRIVATE  :: Default_NextIntegerBound => StreamRNG_NextIntegerBound
        ! ---------------------------------------------------------------------
    END TYPE StreamRNG

!** INTERFACE DEFINITIONS:
    ! abstract interface for deferred procedure
    ABSTRACT INTERFACE
        !> NewRNG is a deferred procedure to return a new StreamRNG initialized
        !  to the initial state of the next stream of the given StreamRNG, which
        !  represents the current stream.
        FUNCTION NewRNG(RNG) RESULT(RNGNew)
            IMPORT
            !% stream generator
            CLASS(StreamRNG), INTENT(INOUT) :: RNG
            !% new stream generator
            CLASS(StreamRNG), ALLOCATABLE   :: RNGNew
        END FUNCTION NewRNG
        !> ResetSub is a deferred procedure to reset the generator to the beginning
        !  state of its current sub-stream. <br>
        SUBROUTINE ResetSub(RNG)
            IMPORT
            !% stream generator
            CLASS(StreamRNG), INTENT(INOUT) :: RNG
        END SUBROUTINE
        !> ResetNxt is a deferred procedure to reset the generator to the beginning
        !  state of its next sub-stream. <br>
        SUBROUTINE ResetNxt(RNG)
            IMPORT
            !% stream generator
            CLASS(StreamRNG), INTENT(INOUT) :: RNG
        END SUBROUTINE
    END INTERFACE
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

FUNCTION Default_NextInteger(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 32-bit random integer value.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StreamRNG), INTENT(INOUT) :: RNG      !! 'StreamRNG' object
    tInteger                        :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    RandNum = MIN_I32 + ToInteger(RNG%NextDouble()*(MAX_I32 - MIN_I32 + 1.0_kDouble))
    
    RETURN

END FUNCTION Default_NextInteger

!******************************************************************************

FUNCTION Default_NextLong(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random integer value.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StreamRNG), INTENT(INOUT) :: RNG      !! 'StreamRNG' object
    tLong                           :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    RandNum = IOR(SHIFTL(ToLong(RNG%NextInteger()), 32), &
                  IAND(ToLong(RNG%NextInteger()), MaskL))
    
    RETURN

END FUNCTION Default_NextLong

!******************************************************************************

FUNCTION Default_NextQuad(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a random 128-bit-floating-point value between zero (inclusive)
    !  and one (exclusive).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StreamRNG), INTENT(INOUT) :: RNG      !! 'StreamRNG' object
    tQuad                           :: RandNum  !! random number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! although these two parameters use different formulas, they are essentially the same.
    tQuad,   PARAMETER  :: QNorm1 = 2.0_kQuad**(-113)
    tQuad,   PARAMETER  :: QNorm2 = 0.5_kQuad*EPSILON(1.0_kQuad)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: IntVal(4)
    tInteger    :: Hi1, Hi2, Lo1, Lo2
    tUInt64     :: I128Hi, I128Lo
    tQuad       :: R128

! FLOW
    
    ! get four random integer values
    Hi1 = SHIFTR(RNG%NextInteger(), 4)  ! use the most significant 28 bits
    Hi2 = SHIFTR(RNG%NextInteger(), 4)  ! use the most significant 28 bits
    Lo1 = SHIFTR(RNG%NextInteger(), 4)  ! use the most significant 28 bits
    Lo2 = SHIFTR(RNG%NextInteger(), 3)  ! use the most significant 29 bits
    
    ! join the most significant 57 bits of Low and 56 bits of High
    IntVal = 0
    CALL MVBITS(Lo2,  0, 29, IntVal(1),  0)
    CALL MVBITS(Lo1,  0,  3, IntVal(1), 29)
    CALL MVBITS(Lo1,  3, 25, IntVal(2),  0)
    CALL MVBITS(Hi2,  0,  7, IntVal(2), 25)
    CALL MVBITS(Hi2,  7, 21, IntVal(3),  0)
    CALL MVBITS(Hi1,  0, 11, IntVal(3), 21)
    CALL MVBITS(Hi1, 11, 17, IntVal(4),  0)
    
    ! get upper and lower 64 bits of 128-bit integer number
    I128Lo = IOR(SHIFTL(ToInt64(IntVal(2)), 32), ToInt64(IntVal(1)))    ! treated as unsigned
    I128Hi = IOR(SHIFTL(ToInt64(IntVal(4)), 32), ToInt64(IntVal(3)))    ! treated as signed
    
    ! convert 128-bit integer number to 128-bit real number
    R128 = I128_To_R128(I128Hi, I128Lo)

    ! normalize the 128-bit real random number
    ! Note: Although the above block treats 128-bit integer as signed number,
    !       it is always positive because only the lower 113 bits are set and
    !       the higher 15 bits are all zero.
    RandNum = R128*QNorm1
    
    RETURN

END FUNCTION Default_NextQuad

!******************************************************************************

FUNCTION StreamRNG_NextIntegerUpper(RNG, Upper) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To generate a random 32-bit-integer value in the range between
    !  0 (inclusive) and the specified upper value (exclusive).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StreamRNG), INTENT(INOUT) :: RNG      !! 'StreamRNG' object
    tInteger,         INTENT(IN)    :: Upper    !! upper bound of the generated value; must be positive
    tInteger                        :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check upper bound value
    IF (Upper <= 0) THEN
        CALL Handle_ErrLevel('StreamRNG_NextIntegerUpper', ModName, ErrWarning, &
                             'The upper-bound value must be positive.')
        RandNum = 0
        RETURN
    END IF

    RandNum = ToInteger(RNG%NextDouble()*(Upper + 1.0_kDouble))

    RETURN

END FUNCTION StreamRNG_NextIntegerUpper

!******************************************************************************

FUNCTION StreamRNG_NextIntegerBound(RNG, Lower, Upper) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To generate a random 32-bit-integer value in the range between the specified
    !  lower value (inclusive) and the specified upper value (exclusive).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StreamRNG), INTENT(INOUT) :: RNG      !! 'StreamRNG' object
    tInteger,         INTENT(IN)    :: Lower    !! lower bound of the generated value
    tInteger,         INTENT(IN)    :: Upper    !! upper bound of the generated value; must be greater than the lower bound
    tInteger                        :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check bound values
    IF (Lower >= Upper) THEN
        CALL Handle_ErrLevel('StreamRNG_NextIntegerBound', ModName, ErrWarning, &
                             'The upper-bound value must be greater than the lower bound.')
        RandNum = Lower
        RETURN
    END IF
    
    RandNum = Lower + ToInteger(RNG%NextDouble()*(Upper - Lower + 1.0_kDouble))

    RETURN

END FUNCTION StreamRNG_NextIntegerBound

!******************************************************************************

END MODULE Class_StreamRNG
    
!******************************************************************************
