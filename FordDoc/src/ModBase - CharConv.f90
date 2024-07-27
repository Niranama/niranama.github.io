
MODULE ModBase_CharConv

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains routines that perform a conversion between a number
!   and a (decimal) string.  The routines may be categorized into 4 groups: <br>
!   - real-number-to-string conversion routines, <br>
!   - real-number-from-string conversion routines, <br>
!   - integer-number-to-string conversion routines, and <br>
!   - integer-number-from-string conversion routines. <br>
!   For real numbers, routines for all three common precisions (including
!   single-precision, double-precision and quadruple-precision) are provided.
!   For integer numbers, routines for only 32-bit and 64-bit integers are provided. <br>
!   See technical information regarding the integer-number-from-string conversion routines
!   in the <a href="../module/subbase_intfromchar.html">SubBase_IntFromChar</a> submodule. <br>
!   Also, see technical information regarding the conversion routines for real numbers in
!   the <a href="../module/subbase_realdpconv.html">SubBase_RealDPConv</a> submodule.

!** USE STATEMENTS:
    USE ModBase_Common

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    !--------------------------------------------------
    !   Single-Precision Real-Character Conversions
    !--------------------------------------------------
    ! Real-To-String
    PUBLIC :: RealSP_ToString_DragonBox,    RealSP_ToString_Ryu
    PUBLIC :: RealSP_ToString_Schubfach
    ! Real-From-String
    PUBLIC :: RealSP_FromString_FastFloat,  RealSP_FromString_LibC
    PUBLIC :: RealSP_FromString_YY,         RealSP_FromString_Lemire
    !--------------------------------------------------
    !   Double-Precision Real-Character Conversions
    !--------------------------------------------------
    ! Real-To-String
    PUBLIC :: RealDP_ToString_DragonBox,    RealDP_ToString_Ryu
    PUBLIC :: RealDP_ToString_Schubfach
    ! Real-From-String
    PUBLIC :: RealDP_FromString_FastFloat,  RealDP_FromString_LibC
    PUBLIC :: RealDP_FromString_YY,         RealDP_FromString_Lemire
    !--------------------------------------------------
    !   Quad-Precision Real-Character Conversions
    !--------------------------------------------------
    ! Real-To-String
    PUBLIC :: RealQP_ToString_DragonBox,    RealQP_ToString_Ryu
    PUBLIC :: RealQP_ToString_Schubfach
    ! Real-From-String
    PUBLIC :: RealQP_FromString_FastFloat,  RealQP_FromString_LibC
    PUBLIC :: RealQP_FromString_YY,         RealQP_FromString_Lemire
    !--------------------------------------------------
    !   32-Bit Integer ToChar Procedures
    !--------------------------------------------------
    PUBLIC :: I32_ToChar_Basic,             I32_ToChar_CC
    PUBLIC :: I32_ToChar_YY,                I32_ToChar_YYLL
    PUBLIC :: I32_ToChar_JEA
    !--------------------------------------------------
    !   64-Bit Integer ToChar Procedures
    !--------------------------------------------------
    PUBLIC :: I64_ToChar_Basic,             I64_ToChar_CC
    PUBLIC :: I64_ToChar_YY,                I64_ToChar_YYLL
    PUBLIC :: I64_ToChar_JEA
    !--------------------------------------------------
    !   32-Bit Integer FromChar Procedures
    !--------------------------------------------------
    PUBLIC :: I32_FromChar_CC_FortNum,      I32_FromChar_CC_FortPlus
    PUBLIC :: I32_FromChar_CC_JsonNum,      I32_FromChar_Lemire_FortPlus
    PUBLIC :: I32_FromChar_YY_JsonNum
    !--------------------------------------------------
    !   64-Bit Integer FromChar Procedures
    !--------------------------------------------------
    PUBLIC :: I64_FromChar_CC_FortNum,      I64_FromChar_CC_FortPlus
    PUBLIC :: I64_FromChar_CC_JsonNum,      I64_FromChar_Lemire_FortPlus
    PUBLIC :: I64_FromChar_YY_JsonNum

    PRIVATE          ! by default, hide all data and routines except those declared explicitly
    
!** MACRO DEFINITIONS:
#include    "../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! -----------------------------------------------------------------
    ! -----     options for type of number to be parsed           -----
    ! -----------------------------------------------------------------
    tSInt32, PARAMETER, PUBLIC  :: FortNum  = 1     ! strict Fortran number
    tSInt32, PARAMETER, PUBLIC  :: FPlusNum = 2     ! relaxed Fortran number
    tSInt32, PARAMETER, PUBLIC  :: JsonNum  = 3     ! JSON number

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE/GENERIC DEFINITIONS:
    !--------------------------------------------------
    !   Single-Precision Real-Character Conversions
    !--------------------------------------------------
    INTERFACE
        MODULE FUNCTION RealSP_ToString_DragonBox(Number, IsScientific) RESULT(cStr)
            !^ To convert a single-precision floating-point value to a character
            !  (decimal) string using the DragonBox algorithm.
            tSingle,            INTENT(IN)  :: Number       !! number
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ format flag <br>
            ! - true  if to write the given number in scientific format. <br>
            ! - false if to write the given number in general format. <br>
            ! - default is false.
            tCharAlloc                      :: cStr         !! character string
        END FUNCTION RealSP_ToString_DragonBox
        !----------------------------------------------------------------------
        MODULE FUNCTION RealSP_ToString_Ryu(Number, IsScientific) RESULT(cStr)
            !^ To convert a single-precision floating-point value to a character
            !  (decimal) string using the Ryu algorithm.
            tSingle,            INTENT(IN)  :: Number       !! number
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ format flag <br>
            ! - true  if to write the given number in scientific format. <br>
            ! - false if to write the given number in general format. <br>
            ! - default is false.
            tCharAlloc                      :: cStr         !! character string
        END FUNCTION RealSP_ToString_Ryu
        !----------------------------------------------------------------------
        MODULE FUNCTION RealSP_ToString_Schubfach(Number, IsScientific) RESULT(cStr)
            !^ To convert a single-precision floating-point value to a character
            !  (decimal) string using the Schubfach algorithm.
            tSingle,            INTENT(IN)  :: Number       !! number
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ format flag <br>
            ! - true  if to write the given number in scientific format. <br>
            ! - false if to write the given number in general format. <br>
            ! - default is false.
            tCharAlloc                      :: cStr         !! character string
        END FUNCTION RealSP_ToString_Schubfach
        !----------------------------------------------------------------------
        MODULE FUNCTION RealSP_FromString_FastFloat(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a character (decimal) string to a single-precision
            !  floating-point value using the FastFloat algorithm.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
            !^ flag indicating how to interpret the input string <br>
            !   - *FortNum* (or 1): interpreted as a strict Fortran number, <br>
            !   - *FPlusNum* (or 2): interpreted as a relaxed Fortran number, or <br>
            !   - *JsonNum* (or 3): interpreted as a JSON number.
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if error occurred
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tSingle                             :: Number   !! floating-point number
        END FUNCTION RealSP_FromString_FastFloat
        !----------------------------------------------------------------------
        MODULE FUNCTION RealSP_FromString_LibC(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a character (decimal) string to a single-precision
            !  floating-point value using the LibC algorithm.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
            !^ flag indicating how to interpret the input string <br>
            !   - *FortNum* (or 1): interpreted as a strict Fortran number, <br>
            !   - *FPlusNum* (or 2): interpreted as a relaxed Fortran number, or <br>
            !   - *JsonNum* (or 3): interpreted as a JSON number.
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if error occurred
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tSingle                             :: Number   !! floating-point number
        END FUNCTION RealSP_FromString_LibC
        !----------------------------------------------------------------------
        MODULE FUNCTION RealSP_FromString_YY(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a character (decimal) string to a single-precision
            !  floating-point value using the YY algorithm.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
            !^ flag indicating how to interpret the input string <br>
            !   - *FortNum* (or 1): interpreted as a strict Fortran number, <br>
            !   - *FPlusNum* (or 2): interpreted as a relaxed Fortran number, or <br>
            !   - *JsonNum* (or 3): interpreted as a JSON number.
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if error occurred
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tSingle                             :: Number   !! floating-point number
        END FUNCTION RealSP_FromString_YY
        !----------------------------------------------------------------------
        MODULE FUNCTION RealSP_FromString_Lemire(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a character (decimal) string to a single-precision
            !  floating-point value using the Lemire algorithm.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
            !^ flag indicating how to interpret the input string <br>
            !   - *FortNum* (or 1): interpreted as a strict Fortran number, <br>
            !   - *FPlusNum* (or 2): interpreted as a relaxed Fortran number, or <br>
            !   - *JsonNum* (or 3): interpreted as a JSON number.
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if error occurred
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tSingle                             :: Number   !! floating-point number
        END FUNCTION RealSP_FromString_Lemire
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------
    !   Double-Precision Real-Character Conversions
    !--------------------------------------------------
    INTERFACE
        MODULE FUNCTION RealDP_ToString_DragonBox(Number, IsScientific) RESULT(cStr)
            !^ To convert a double-precision floating-point value to a character
            !  (decimal) string using the DragonBox algorithm.
            tDouble,            INTENT(IN)  :: Number       !! number
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ format flag <br>
            ! - true  if to write the given number in scientific format. <br>
            ! - false if to write the given number in general format. <br>
            ! - default is false.
            tCharAlloc                      :: cStr         !! character string
        END FUNCTION RealDP_ToString_DragonBox
        !----------------------------------------------------------------------
        MODULE FUNCTION RealDP_ToString_Ryu(Number, IsScientific) RESULT(cStr)
            !^ To convert a double-precision floating-point value to a character
            !  (decimal) string using the Ryu algorithm.
            tDouble,            INTENT(IN)  :: Number       !! number
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ format flag <br>
            ! - true  if to write the given number in scientific format. <br>
            ! - false if to write the given number in general format. <br>
            ! - default is false.
            tCharAlloc                      :: cStr         !! character string
        END FUNCTION RealDP_ToString_Ryu
        !----------------------------------------------------------------------
        MODULE FUNCTION RealDP_ToString_Schubfach(Number, IsScientific) RESULT(cStr)
            !^ To convert a double-precision floating-point value to a character
            !  (decimal) string using the Schubfach algorithm.
            tDouble,            INTENT(IN)  :: Number       !! number
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ format flag <br>
            ! - true  if to write the given number in scientific format. <br>
            ! - false if to write the given number in general format. <br>
            ! - default is false.
            tCharAlloc                      :: cStr         !! character string
        END FUNCTION RealDP_ToString_Schubfach
        !----------------------------------------------------------------------
        MODULE FUNCTION RealDP_FromString_FastFloat(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a character (decimal) string to a double-precision
            !  floating-point value using the FastFloat algorithm.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
            !^ flag indicating how to interpret the input string <br>
            !   - *FortNum* (or 1): interpreted as a strict Fortran number, <br>
            !   - *FPlusNum* (or 2): interpreted as a relaxed Fortran number, or <br>
            !   - *JsonNum* (or 3): interpreted as a JSON number.
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if error occurred
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tDouble                             :: Number   !! floating-point number
        END FUNCTION RealDP_FromString_FastFloat
        !----------------------------------------------------------------------
        MODULE FUNCTION RealDP_FromString_LibC(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a character (decimal) string to a double-precision
            !  floating-point value using the LibC algorithm.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
            !^ flag indicating how to interpret the input string <br>
            !   - *FortNum* (or 1): interpreted as a strict Fortran number, <br>
            !   - *FPlusNum* (or 2): interpreted as a relaxed Fortran number, or <br>
            !   - *JsonNum* (or 3): interpreted as a JSON number.
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if error occurred
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tDouble                             :: Number   !! floating-point number
        END FUNCTION RealDP_FromString_LibC
        !----------------------------------------------------------------------
        MODULE FUNCTION RealDP_FromString_YY(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a character (decimal) string to a double-precision
            !  floating-point value using the YY algorithm.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
            !^ flag indicating how to interpret the input string <br>
            !   - *FortNum* (or 1): interpreted as a strict Fortran number, <br>
            !   - *FPlusNum* (or 2): interpreted as a relaxed Fortran number, or <br>
            !   - *JsonNum* (or 3): interpreted as a JSON number.
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if error occurred
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tDouble                             :: Number   !! floating-point number
        END FUNCTION RealDP_FromString_YY
        !----------------------------------------------------------------------
        MODULE FUNCTION RealDP_FromString_Lemire(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a character (decimal) string to a double-precision
            !  floating-point value using the Lemire algorithm.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
            !^ flag indicating how to interpret the input string <br>
            !   - *FortNum* (or 1): interpreted as a strict Fortran number, <br>
            !   - *FPlusNum* (or 2): interpreted as a relaxed Fortran number, or <br>
            !   - *JsonNum* (or 3): interpreted as a JSON number.
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if error occurred
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tDouble                             :: Number   !! floating-point number
        END FUNCTION RealDP_FromString_Lemire
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------
    !   Quad-Precision Real-Character Conversions
    !--------------------------------------------------
    INTERFACE
        MODULE FUNCTION RealQP_ToString_DragonBox(Number, IsScientific) RESULT(cStr)
            !^ To convert a quadruple-precision floating-point value to a character
            !  (decimal) string using the DragonBox algorithm.
            tQuad,              INTENT(IN)  :: Number       !! number
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ format flag <br>
            ! - true  if to write the given number in scientific format. <br>
            ! - false if to write the given number in general format. <br>
            ! - default is false.
            tCharAlloc                      :: cStr         !! character string
        END FUNCTION RealQP_ToString_DragonBox
        !----------------------------------------------------------------------
        MODULE FUNCTION RealQP_ToString_Ryu(Number, IsScientific) RESULT(cStr)
            !^ To convert a quadruple-precision floating-point value to a character
            !  (decimal) string using the Ryu algorithm.
            tQuad,              INTENT(IN)  :: Number       !! number
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ format flag <br>
            ! - true  if to write the given number in scientific format. <br>
            ! - false if to write the given number in general format. <br>
            ! - default is false.
            tCharAlloc                      :: cStr         !! character string
        END FUNCTION RealQP_ToString_Ryu
        !----------------------------------------------------------------------
        MODULE FUNCTION RealQP_ToString_Schubfach(Number, IsScientific) RESULT(cStr)
            !^ To convert a quadruple-precision floating-point value to a character
            !  (decimal) string using the Schubfach algorithm.
            tQuad,              INTENT(IN)  :: Number       !! number
            tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
            !^ format flag <br>
            ! - true  if to write the given number in scientific format. <br>
            ! - false if to write the given number in general format. <br>
            ! - default is false.
            tCharAlloc                      :: cStr         !! character string
        END FUNCTION RealQP_ToString_Schubfach
        !----------------------------------------------------------------------
        MODULE FUNCTION RealQP_FromString_FastFloat(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a character (decimal) string to a quadruple-precision
            !  floating-point value using the FastFloat algorithm.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
            !^ flag indicating how to interpret the input string <br>
            !   - *FortNum* (or 1): interpreted as a strict Fortran number, <br>
            !   - *FPlusNum* (or 2): interpreted as a relaxed Fortran number, or <br>
            !   - *JsonNum* (or 3): interpreted as a JSON number.
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if error occurred
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tQuad                               :: Number   !! floating-point number
        END FUNCTION RealQP_FromString_FastFloat
        !----------------------------------------------------------------------
        MODULE FUNCTION RealQP_FromString_LibC(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a character (decimal) string to a quadruple-precision
            !  floating-point value using the LibC algorithm.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
            !^ flag indicating how to interpret the input string <br>
            !   - *FortNum* (or 1): interpreted as a strict Fortran number, <br>
            !   - *FPlusNum* (or 2): interpreted as a relaxed Fortran number, or <br>
            !   - *JsonNum* (or 3): interpreted as a JSON number.
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if error occurred
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tQuad                               :: Number   !! floating-point number
        END FUNCTION RealQP_FromString_LibC
        !----------------------------------------------------------------------
        MODULE FUNCTION RealQP_FromString_YY(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a character (decimal) string to a quadruple-precision
            !  floating-point value using the YY algorithm.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
            !^ flag indicating how to interpret the input string <br>
            !   - *FortNum* (or 1): interpreted as a strict Fortran number, <br>
            !   - *FPlusNum* (or 2): interpreted as a relaxed Fortran number, or <br>
            !   - *JsonNum* (or 3): interpreted as a JSON number.
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if error occurred
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tQuad                               :: Number   !! floating-point number
        END FUNCTION RealQP_FromString_YY
        !----------------------------------------------------------------------
        MODULE FUNCTION RealQP_FromString_Lemire(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a character (decimal) string to a quadruple-precision
            !  floating-point value using the Lemire algorithm.
            tCharStar,            INTENT(IN)    :: cStr     !! character string
            tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
            !^ flag indicating how to interpret the input string <br>
            !   - *FortNum* (or 1): interpreted as a strict Fortran number, <br>
            !   - *FPlusNum* (or 2): interpreted as a relaxed Fortran number, or <br>
            !   - *JsonNum* (or 3): interpreted as a JSON number.
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if error occurred
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! error message
            tQuad                               :: Number   !! floating-point number
        END FUNCTION RealQP_FromString_Lemire
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------
    !   32-Bit Integer ToChar Procedures
    !--------------------------------------------------
    INTERFACE
        MODULE FUNCTION I32_ToChar_Basic(Number) RESULT(cStr)
            !^ To convert a 32-bit integer number to a character (decimal)
            !  string using the basic algorithm.
            tSInt32, INTENT(IN) :: Number   !! number
            tCharAlloc          :: cStr     !! character string
        END FUNCTION I32_ToChar_Basic
        !----------------------------------------------------------------------
        MODULE FUNCTION I32_ToChar_CC(Number) RESULT(cStr)
            !^ To convert a 32-bit integer number to a character (decimal)
            !  string using the CC algorithm.
            tSInt32, INTENT(IN) :: Number   !! number
            tCharAlloc          :: cStr     !! character string
        END FUNCTION I32_ToChar_CC
        !----------------------------------------------------------------------
        MODULE FUNCTION I32_ToChar_YY(Number) RESULT(cStr)
            !^ To convert a 32-bit integer number to a character (decimal)
            !  string using the YY algorithm.
            tSInt32, INTENT(IN) :: Number   !! number
            tCharAlloc          :: cStr     !! character string
        END FUNCTION I32_ToChar_YY
        !----------------------------------------------------------------------
        MODULE FUNCTION I32_ToChar_YYLL(Number) RESULT(cStr)
            !^ To convert a 32-bit integer number to a character (decimal)
            !  string using the YY algorithm with large tables.
            tSInt32, INTENT(IN) :: Number   !! number
            tCharAlloc          :: cStr     !! character string
        END FUNCTION I32_ToChar_YYLL
        !----------------------------------------------------------------------
        MODULE FUNCTION I32_ToChar_JEA(Number) RESULT(cStr)
            !^ To convert a 32-bit integer number to a character (decimal)
            !  string using the JEA algorithm.
            tSInt32, INTENT(IN) :: Number   !! number
            tCharAlloc          :: cStr     !! character string
        END FUNCTION I32_ToChar_JEA
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------
    !   64-Bit Integer ToChar Procedures
    !--------------------------------------------------
    INTERFACE
        MODULE FUNCTION I64_ToChar_Basic(Number) RESULT(cStr)
            !^ To convert a 64-bit integer number to a character (decimal)
            !  string using the basic algorithm.
            tSInt64, INTENT(IN) :: Number   !! number
            tCharAlloc          :: cStr     !! character string
        END FUNCTION I64_ToChar_Basic
        !----------------------------------------------------------------------
        MODULE FUNCTION I64_ToChar_CC(Number) RESULT(cStr)
            !^ To convert a 64-bit integer number to a character (decimal)
            !  string using the CC algorithm.
            tSInt64, INTENT(IN) :: Number   !! number
            tCharAlloc          :: cStr     !! character string
        END FUNCTION I64_ToChar_CC
        !----------------------------------------------------------------------
        MODULE FUNCTION I64_ToChar_YY(Number) RESULT(cStr)
            !^ To convert a 64-bit integer number to a character (decimal)
            !  string using the YY algorithm.
            tSInt64, INTENT(IN) :: Number   !! number
            tCharAlloc          :: cStr     !! character string
        END FUNCTION I64_ToChar_YY
        !----------------------------------------------------------------------
        MODULE FUNCTION I64_ToChar_YYLL(Number) RESULT(cStr)
            !^ To convert a 64-bit integer number to a character (decimal)
            !  string using the YY algorithm with large tables.
            tSInt64, INTENT(IN) :: Number   !! number
            tCharAlloc          :: cStr     !! character string
        END FUNCTION I64_ToChar_YYLL
        !----------------------------------------------------------------------
        MODULE FUNCTION I64_ToChar_JEA(Number) RESULT(cStr)
            !^ To convert a 64-bit integer number to a character (decimal)
            !  string using the JEA algorithm.
            tSInt64, INTENT(IN) :: Number   !! number
            tCharAlloc          :: cStr     !! character string
        END FUNCTION I64_ToChar_JEA
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------
    !   32-Bit Integer FromChar Procedures
    !--------------------------------------------------
    INTERFACE
        MODULE FUNCTION I32_FromChar_CC_FortNum(cStr, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a decimal string to a 32-bit integer value using the
            !  CC algorithm where the string is interpreted as a Fortran number.
            tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tSInt32                             :: Number   !! number
        END FUNCTION I32_FromChar_CC_FortNum
        !----------------------------------------------------------------------
        MODULE FUNCTION I32_FromChar_CC_FortPlus(cStr, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a decimal string to a 32-bit integer value using the
            !  CC algorithm where the string is interpreted as a FPlus number.
            tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tSInt32                             :: Number   !! number
        END FUNCTION I32_FromChar_CC_FortPlus
        !----------------------------------------------------------------------
        MODULE FUNCTION I32_FromChar_CC_JsonNum(cStr, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a decimal string to a 32-bit integer value using the
            !  CC algorithm where the string is interpreted as a JSON number.
            tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tSInt32                             :: Number   !! number
        END FUNCTION I32_FromChar_CC_JsonNum
        !----------------------------------------------------------------------
        MODULE FUNCTION I32_FromChar_Lemire_FortPlus(cStr, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a decimal string to a 32-bit integer value using the
            !  Lemire algorithm where the string is interpreted as a FPlus number.
            tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tSInt32                             :: Number   !! number
        END FUNCTION I32_FromChar_Lemire_FortPlus
        !----------------------------------------------------------------------
        MODULE FUNCTION I32_FromChar_YY_JsonNum(cStr, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a decimal string to a 32-bit integer value using the
            !  YY algorithm where the string is interpreted as a JSON number.
            tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tSInt32                             :: Number   !! number
        END FUNCTION I32_FromChar_YY_JsonNum
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------
    !   64-Bit Integer FromChar Procedures
    !--------------------------------------------------
    INTERFACE
        MODULE FUNCTION I64_FromChar_CC_FortNum(cStr, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a decimal string to a 64-bit integer value using the
            !  CC algorithm where the string is interpreted as a Fortran number.
            tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tSInt64                             :: Number   !! number
        END FUNCTION I64_FromChar_CC_FortNum
        !----------------------------------------------------------------------
        MODULE FUNCTION I64_FromChar_CC_FortPlus(cStr, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a decimal string to a 64-bit integer value using the
            !  CC algorithm where the string is interpreted as a FPlus number.
            tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tSInt64                             :: Number   !! number
        END FUNCTION I64_FromChar_CC_FortPlus
        !----------------------------------------------------------------------
        MODULE FUNCTION I64_FromChar_CC_JsonNum(cStr, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a decimal string to a 64-bit integer value using the
            !  CC algorithm where the string is interpreted as a JSON number.
            tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tSInt64                             :: Number   !! number
        END FUNCTION I64_FromChar_CC_JsonNum
        !----------------------------------------------------------------------
        MODULE FUNCTION I64_FromChar_Lemire_FortPlus(cStr, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a decimal string to a 64-bit integer value using the
            !  Lemire algorithm where the string is interpreted as a FPlus number.
            tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tSInt64                             :: Number   !! number
        END FUNCTION I64_FromChar_Lemire_FortPlus
        !----------------------------------------------------------------------
        MODULE FUNCTION I64_FromChar_YY_JsonNum(cStr, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a decimal string to a 64-bit integer value using the
            !  YY algorithm where the string is interpreted as a JSON number.
            tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tSInt64                             :: Number   !! number
        END FUNCTION I64_FromChar_YY_JsonNum
        !----------------------------------------------------------------------
    END INTERFACE

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!******************************************************************************

END MODULE ModBase_CharConv

!******************************************************************************
