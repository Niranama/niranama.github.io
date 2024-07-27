
SUBMODULE (ModBase_CharConv) SubBase_IntToChar

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation for routines relating to a conversion
!   from an integer number to a decimal string. <br>
!   <br>
!  **REFERENCES**: <br>
!   [1] <a href="https://github.com/ibireme/c_numconv_benchmark">Number Conversion
!       Benchmark in C.</a> <br>
!   [2] <a href="https://github.com/jeaiii/itoa">jeaiii_to_text</a>

!** USE STATEMENTS:
    USE ModBase_UIntUtil,       ONLY: U128_Multiply_High => UMul128_Upper64
    USE ModBase_LargeTables,    ONLY: Char2Digits, Char4Digits

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    tSInt32, PARAMETER  :: MinI32 = ToInt32(Z'80000000')            ! -2147483648
    tSInt64, PARAMETER  :: MinI64 = ToInt64(Z'8000000000000000')    ! -9223372036854775808

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!------------------------------------------------------------------------------
!
!                           32-BIT INTEGER ROUTINES
!
!------------------------------------------------------------------------------

MODULE FUNCTION I32_ToChar_Basic(Number) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert an integer number to character string

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN) :: Number   ! number
    tCharAlloc          :: cStr     ! character string

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: MaxLen = 10
    tSInt32, PARAMETER  :: Base   = 10
    tChar,   PARAMETER  :: NumStr(0:9) = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(MaxLen)    :: wStr     ! working string
    tSInt32             :: PosNum   ! positive number (working number)
    tSInt32             :: CurNum   ! current (saved) working number
    tSInt32             :: RemNum   ! remainder number
    tSInt32             :: Indx

!** FLOW

    ! check whether the number is zero
    IF (Number == 0) THEN
        cStr = '0'
        RETURN
    END IF
    IF (Number < 0) THEN
        IF (Number == MinI32) THEN
            cStr = '-2147483648'
            RETURN
        END IF
        PosNum = ABS(Number)
    ELSE
        PosNum = Number
    END IF
    Indx = MaxLen
    
    ! start the conversion
    DO
        ! save current number
        CurNum = PosNum
        ! compute the next round of working number
        PosNum = PosNum/Base
        ! compute the remainder
        RemNum = CurNum - PosNum*Base
        ! convert the remainder to a working string
        wStr(Indx:Indx) = NumStr(RemNum)
        Indx = Indx - 1
        IF (PosNum == 0) EXIT
    END DO
    
    ! allocate the resulting string and transfer
    ! characters from the working string
    Indx = Indx + 1
    IF (Number < 0) THEN
        cStr = '-' // wStr(Indx:MaxLen)
    ELSE
        cStr = wStr(Indx:MaxLen)
    END IF
    
    RETURN

END FUNCTION I32_ToChar_Basic

!******************************************************************************

MODULE FUNCTION I32_ToChar_CC(Number) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert an integer number to character string

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN) :: Number   ! number
    tCharAlloc          :: cStr     ! character string

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER :: MaxLen = 10
    tSInt32, PARAMETER :: ShiftPos = 45
    tSInt64, PARAMETER :: Multiplier = ToInt64(Z'00000000D1B71759')
    tSInt32, PARAMETER :: Divisor = 10000
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(MaxLen)    :: wStr     ! working string
    tSInt32             :: PosNum   ! positive number (working number)
    tSInt32             :: NxtNum   ! next round of positive number
    tSInt32             :: RemNum   ! remainder number
    tSInt32             :: Start, Finish

!** FLOW

    ! set positive number
    PosNum = ABS(Number)
    
    ! start the conversion
    IF (PosNum < 10000) THEN
        Start  = 1
        IF (PosNum < 100) THEN
            wStr(1:2) = Char2Digits(PosNum)
            Finish = 2
            IF (wStr(Start:Start) == '0') Start = 2
        ELSE
            wStr(1:4) = Char4Digits(PosNum)
            Finish = 4
            IF (wStr(Start:Start) == '0') Start = 2
        END IF
    ELSE
        ! compute the next round of working number
        NxtNum = ToInt32(SHIFTR(PosNum*Multiplier, ShiftPos))   ! NxtNum = PosNum/10000
        ! compute the remainder
        RemNum = PosNum - NxtNum*Divisor                        ! RemNum = MOD(PosNum, 10000)
        ! convert the remainder to a working string
        wStr(7:10) = Char4Digits(RemNum)
        Finish = 10
        PosNum = NxtNum
        IF (PosNum < 10000) THEN
            IF (PosNum < 100) THEN
                wStr(5:6) = Char2Digits(PosNum)
                Start  = 5
                IF (wStr(Start:Start) == '0') Start = 6
            ELSE
                wStr(3:6) = Char4Digits(PosNum)
                Start  = 3
                IF (wStr(Start:Start) == '0') Start = 4
            END IF
        ELSE
            ! compute the next round of working number
            NxtNum = ToInt32(SHIFTR(PosNum*Multiplier, ShiftPos))   ! NxtNum = PosNum/10000
            ! compute the remainder
            RemNum = PosNum - NxtNum*Divisor                        ! RemNum = MOD(PosNum, 10000)
            ! convert the remainder to a working string
            wStr(3:6) = Char4Digits(RemNum)
            IF (NxtNum > 0) THEN
                IF (NxtNum < 10) THEN
                    wStr(2:2) = Char2Digits(NxtNum)(2:2)
                    Start = 2
                ELSE
                    wStr(1:2) = Char2Digits(NxtNum)
                    Start = 1
                END IF
            ELSE
                Start = 3
            END IF
        END IF
    END IF
    
    IF (Number < 0) THEN
        IF (Number == MinI32) THEN
            cStr = '-2147483648'
            RETURN
        END IF
        cStr = '-' // wStr(Start:Finish)
    ELSE
        cStr = wStr(Start:Finish)
    END IF
    
    RETURN
    
END FUNCTION I32_ToChar_CC

!******************************************************************************

MODULE FUNCTION I32_ToChar_YY(Number) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert an integer number to character string

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN) :: Number   ! number
    tCharAlloc          :: cStr     ! character string

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: MaxLen = 10
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(MaxLen)    :: wStr
    tSInt32             :: PosNum
    tSInt32             :: Finish, Start
    tSInt32             :: AA, BB, CC, DD
    tSInt32             :: AABB, BBCC, CCDD
    tSInt32             :: AABBCC, DDEE, EE

!** FLOW
    
    ! set positive number
    PosNum = ABS(Number)
    
    ! start the conversion
    IF (PosNum < 100) THEN                                      ! 1-2 digits
        wStr(1:2) = Char2Digits(PosNum)
        Finish = 2
    ELSEIF (PosNum < 10000) THEN                                ! 3-4 digits
        AA = ToInt32(SHIFTR(PosNum*5243, 19))                   ! PosNum / 100
        BB = PosNum - AA*100                                    ! MOD(PosNum, 100)
        wStr(1:2) = Char2Digits(AA)
        wStr(3:4) = Char2Digits(BB)
        Finish = 4
    ELSEIF (PosNum < 1000000) THEN                              ! 5-6 digits
        AA = ToInt32(SHIFTR(PosNum*429497_kInt64, 32))          ! PosNum / 10000
        BBCC = PosNum - AA*10000                                ! MOD(PosNum, 10000)
        BB = SHIFTR(BBCC*5243, 19)                              ! BBCC / 100
        CC = BBCC - BB*100                                      ! MOD(BBCC, 100)
        wStr(1:2) = Char2Digits(AA)
        wStr(3:4) = Char2Digits(BB)
        wStr(5:6) = Char2Digits(CC)
        Finish = 6
    ELSEIF (PosNum < 100000000) THEN                            ! 7-8 digits
        AABB = ToInt32(SHIFTR(PosNum*109951163_kInt64, 40))     ! PosNum / 10000
        CCDD = PosNum - AABB*10000                              ! MOD(PosNum, 10000)
        AA = SHIFTR(AABB*5243, 19)                              ! AABB / 100
        CC = SHIFTR(CCDD*5243, 19)                              ! CCDD / 100
        BB = AABB - AA*100                                      ! MOD(AABB, 100)
        DD = CCDD - CC*100                                      ! MOD(CCDD, 100)
        wStr(1:2) = Char2Digits(AA)
        wStr(3:4) = Char2Digits(BB)
        wStr(5:6) = Char2Digits(CC)
        wStr(7:8) = Char2Digits(DD)
        Finish = 8
    ELSE                                                        ! 9-10 digits
        AABBCC = ToInt32(SHIFTR(PosNum*3518437209_kInt64, 45))  ! PosNum / 10000
        AA   = ToInt32(SHIFTR(AABBCC*429497_kInt64, 32))        ! aabbcc / 10000
        DDEE = PosNum - AABBCC*10000                            ! MOD(PosNum, 10000)
        BBCC = AABBCC - AA*10000                                ! MOD(aabbcc, 10000)
        BB = SHIFTR(BBCC*5243, 19)                              ! bbcc / 100
        DD = SHIFTR(DDEE*5243, 19)                              ! ddee / 100
        CC = BBCC - BB*100                                      ! MOD(bbcc, 100)
        EE = DDEE - DD*100                                      ! MOD(ddee, 100)
        wStr(1:2)  = Char2Digits(AA)
        wStr(3:4)  = Char2Digits(BB)
        wStr(5:6)  = Char2Digits(CC)
        wStr(7:8)  = Char2Digits(DD)
        wStr(9:10) = Char2Digits(EE)
        Finish = 10
    END IF

    Start = 1
    IF (wStr(1:1) == '0') Start = 2
        
    IF (Number < 0) THEN
        IF (Number == MinI32) THEN
            cStr = '-2147483648'
            RETURN
        END IF
        cStr = '-' // wStr(Start:Finish)
    ELSE
        cStr = wStr(Start:Finish)
    END IF
    
    RETURN

END FUNCTION I32_ToChar_YY

!******************************************************************************

MODULE FUNCTION I32_ToChar_YYLL(Number) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert an integer number to character string

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN) :: Number   ! number
    tCharAlloc          :: cStr     ! character string

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: MaxLen = 10
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(MaxLen)    :: wStr
    tSInt32             :: PosNum
    tSInt32             :: Finish, Start
    tSInt32             :: AA
    tSInt32             :: AABB, BBCC, CCDD
    tSInt32             :: BBCCDDEE, DDEE

!** FLOW
    
    ! set positive number
    PosNum = ABS(Number)
    Start = 1
    
    ! start the conversion
    IF (PosNum < 10000) THEN                                    ! 1-4 digits
        wStr(1:4) = Char4Digits(PosNum)
        IF (wStr(1:1) == '0') THEN
            Start = 2
            IF (wStr(2:2) == '0') THEN
                Start = 3
                IF (wStr(3:3) == '0') THEN
                    Start = 4
                END IF
            END IF
        END IF
        Finish = 4
    ELSEIF (PosNum < 100000000) THEN                            ! 5-8 digits
        AABB = ToInt32(SHIFTR(PosNum*109951163_kInt64, 40))     ! PosNum / 10000
        CCDD = PosNum - AABB*10000                              ! MOD(PosNum, 10000)
        wStr(1:4) = Char4Digits(AABB)
        wStr(5:8) = Char4Digits(CCDD)
        IF (wStr(1:1) == '0') THEN
            Start = 2
            IF (wStr(2:2) == '0') THEN
                Start = 3
                IF (wStr(3:3) == '0') THEN
                    Start = 4
                END IF
            END IF
        END IF
        Finish = 8
    ELSE                                                        ! 9-10 digits
        AA = ToInt32(SHIFTR(PosNum*1441151881_kInt64, 57))      ! PosNum / 100000000
        BBCCDDEE = PosNum - AA*100000000                        ! MOD(PosNum, 100000000)
        BBCC = ToInt32(SHIFTR(BBCCDDEE*109951163_kInt64, 40))   ! BBCCDDEE / 10000
        DDEE = BBCCDDEE - BBCC*10000                            ! MOD(BBCCDDEE, 10000)
        wStr(1:2)  = Char2Digits(AA)
        wStr(3:6)  = Char4Digits(BBCC)
        wStr(7:10) = Char4Digits(DDEE)
        IF (wStr(1:1) == '0') THEN
            Start = 2
        END IF
        Finish = 10
    END IF
        
    IF (Number < 0) THEN
        IF (Number == MinI32) THEN
            cStr = '-2147483648'
            RETURN
        END IF
        cStr = '-' // wStr(Start:Finish)
    ELSE
        cStr = wStr(Start:Finish)
    END IF
    
    RETURN

END FUNCTION I32_ToChar_YYLL

!******************************************************************************

MODULE FUNCTION I32_ToChar_JEA(Number) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert an integer number to character string

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN) :: Number   ! number
    tCharAlloc          :: cStr     ! character string

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER :: MaxLen = 10
    tQuad,   PARAMETER :: R1  = 1.0_kQuad
    tQuad,   PARAMETER :: R2  = 2.0_kQuad
    tSInt64, PARAMETER :: K24 = ToInt64(R2**24/1.0E2_kQuad + R1)
    tSInt64, PARAMETER :: K32 = ToInt64(R2**32/1.0E4_kQuad + R1)
    tSInt64, PARAMETER :: K48 = ToInt64(R2**48/1.0E6_kQuad + R1)
    tSInt64, PARAMETER :: K57 = ToInt64(R2**57/1.0E8_kQuad + R1)
    tSInt64, PARAMETER :: M24 = SHIFTL(1_kInt64, 24) - 1_kInt64
    tSInt64, PARAMETER :: M32 = SHIFTL(1_kInt64, 32) - 1_kInt64
    tSInt64, PARAMETER :: M57 = SHIFTL(1_kInt64, 57) - 1_kInt64
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(MaxLen)    :: wStr
    tSInt64             :: PosNum
    tSInt32             :: Finish, Start
    tSInt64             :: F0, F2, F4, F6, F8

!** FLOW
    
    ! set positive number
    PosNum = ABS(Number)
    
    ! start the conversion
    IF (PosNum < 100_kInt64) THEN                   ! 1-2 digits
        wStr(1:2) = Char2Digits(PosNum)
        Finish = 2
    ELSEIF (PosNum < 10000_kInt64) THEN             ! 3-4 digits
        F0 = K24*PosNum
        F2 = IAND(F0, M24)*100_kInt64
        wStr(1:2) = Char2Digits(SHIFTR(F0, 24))
        wStr(3:4) = Char2Digits(SHIFTR(F2, 24))
        Finish = 4
    ELSEIF (PosNum < 1000000_kInt64) THEN           ! 5-6 digits
        F0 = K32*PosNum
        F2 = IAND(F0, M32)*100_kInt64
        F4 = IAND(F2, M32)*100_kInt64
        wStr(1:2) = Char2Digits(SHIFTR(F0, 32))
        wStr(3:4) = Char2Digits(SHIFTR(F2, 32))
        wStr(5:6) = Char2Digits(SHIFTR(F4, 32))
        Finish = 6
    ELSEIF (PosNum < 100000000_kInt64) THEN         ! 7-8 digits
        F0 = SHIFTR(K48*PosNum, 16)
        F2 = IAND(F0, M32)*100_kInt64
        F4 = IAND(F2, M32)*100_kInt64
        F6 = IAND(F4, M32)*100_kInt64
        wStr(1:2) = Char2Digits(SHIFTR(F0, 32))
        wStr(3:4) = Char2Digits(SHIFTR(F2, 32))
        wStr(5:6) = Char2Digits(SHIFTR(F4, 32))
        wStr(7:8) = Char2Digits(SHIFTR(F6, 32))
        Finish = 8
    ELSE                                            ! 9-10 digits
        F0 = K57*PosNum
        F2 = IAND(F0, M57)*100_kInt64
        F4 = IAND(F2, M57)*100_kInt64
        F6 = IAND(F4, M57)*100_kInt64
        F8 = IAND(F6, M57)*100_kInt64
        wStr(1:2)  = Char2Digits(SHIFTR(F0, 57))
        wStr(3:4)  = Char2Digits(SHIFTR(F2, 57))
        wStr(5:6)  = Char2Digits(SHIFTR(F4, 57))
        wStr(7:8)  = Char2Digits(SHIFTR(F6, 57))
        wStr(9:10) = Char2Digits(SHIFTR(F8, 57))
        Finish = 10
    END IF

    Start = 1
    IF (wStr(1:1) == '0') Start = 2
        
    IF (Number < 0) THEN
        IF (Number == MinI32) THEN
            cStr = '-2147483648'
            RETURN
        END IF
        cStr = '-' // wStr(Start:Finish)
    ELSE
        cStr = wStr(Start:Finish)
    END IF
    
    RETURN

END FUNCTION I32_ToChar_JEA

!------------------------------------------------------------------------------
!
!                           64-BIT INTEGER ROUTINES
!
!------------------------------------------------------------------------------

MODULE FUNCTION I64_ToChar_Basic(Number) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert an integer number to character string

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(IN) :: Number   ! number
    tCharAlloc          :: cStr     ! character string

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: MaxLen = 19
    tSInt64, PARAMETER  :: Base   = 10_kInt64
    tChar,   PARAMETER  :: NumStr(0:9) = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(MaxLen)    :: wStr     ! working string
    tSInt64             :: PosNum   ! positive number (working number)
    tSInt64             :: CurNum   ! current (saved) working number
    tSInt64             :: RemNum   ! remainder number
    tSInt32             :: Indx

!** FLOW

    ! check whether the number is zero
    IF (Number == 0_kInt64) THEN
        cStr = '0'
        RETURN
    END IF
    IF (Number < 0_kInt64) THEN
        IF (Number == MinI64) THEN
            cStr = '-9223372036854775808'
            RETURN
        END IF
        PosNum = ABS(Number)
    ELSE
        PosNum = Number
    END IF
    Indx = MaxLen
    
    ! start the conversion
    DO
        ! save current number
        CurNum = PosNum
        ! compute the next round of working number
        PosNum = PosNum/Base
        ! compute the remainder
        RemNum = CurNum - PosNum*Base
        ! convert the remainder to a working string
        wStr(Indx:Indx) = NumStr(RemNum)
        Indx = Indx - 1
        IF (PosNum == 0_kInt64) EXIT
    END DO
    
    ! allocate the resulting string and transfer
    ! characters from the working string
    Indx = Indx + 1
    IF (Number < 0_kInt64) THEN
        cStr = '-' // wStr(Indx:MaxLen)
    ELSE
        cStr = wStr(Indx:MaxLen)
    END IF
    
    RETURN

END FUNCTION I64_ToChar_Basic

!******************************************************************************

MODULE FUNCTION I64_ToChar_CC(Number) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert an integer number to character string

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(IN) :: Number   ! number
    tCharAlloc          :: cStr     ! character string

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: MaxLen = 19
    tSInt64, PARAMETER  :: Div1E8 = 100000000_kInt64
    ! multiplier and shift for 19 digits and divisor of 1.0E8
    tSInt64, PARAMETER  :: M90 = ToInt64(Z'ABCC77118461CEFD')
    tSInt32, PARAMETER  :: S90 = 90 - 64
    ! multiplier for 11 digits and divisor of 1.0E8
    tSInt64, PARAMETER  :: M64 = ToInt64(Z'0000002AF31DC462')
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(MaxLen)    :: wStr
    tSInt64             :: PosNum
    tSInt64             :: NxtNum, RemNum
    tSInt32             :: Start

!** FLOW

    ! set positive number
    PosNum = ABS(Number)
    
    ! start the conversion
    IF (PosNum < 1000000000_kInt64) THEN
        ! utilize I32_ToChar_CC routine
        cStr = I32_ToChar_CC(ToInt32(Number))
        RETURN
    ELSE
        ! compute NxtNum = PosNum/100000000
        NxtNum = SHIFTR(U128_Multiply_High(PosNum, M90), S90)
        ! compute RemNum = MOD(PosNum, 100000000)
        RemNum = PosNum - NxtNum*Div1E8
        ! convert the remainder to a working string
        CALL Write_8_Digits(ToInt32(RemNum), wStr(12:19))

        PosNum = NxtNum
        IF (PosNum > Div1E8) THEN
            ! compute NxtNum = PosNum/100000000
            NxtNum = U128_Multiply_High(PosNum, M64)
            ! compute RemNum = MOD(PosNum, 100000000)
            RemNum = PosNum - NxtNum*Div1E8
            ! convert the remainder to a working string
            CALL Write_8_Digits(ToInt32(RemNum), wStr(4:11))

            ! convert the rest
            IF (NxtNum < 10) THEN
                wStr(3:3) = Char2Digits(NxtNum)(2:2)
                Start = 3
            ELSEIF (NxtNum < 100) THEN
                wStr(2:3) = Char2Digits(NxtNum)
                Start = 2
            ELSE
                wStr(1:3) = Char4Digits(NxtNum)(2:4)
                Start = 1
            END IF
        ELSE
            ! convert the rest
            Start = 3 + Write_1_to_8_Digits(ToInt32(PosNum), wStr(4:11))
        END IF
        ! transfer to output string
        IF (Number < 0_kInt64) THEN
            IF (Number == MinI64) THEN
                cStr = '-9223372036854775808'
                RETURN
            END IF
            cStr = '-' // wStr(Start:MaxLen)
        ELSE
            cStr = wStr(Start:MaxLen)
        END IF
    END IF
    
    RETURN
    
    CONTAINS

    SUBROUTINE Write_8_Digits(Number, cStr)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 8

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: Number   ! number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string

    !** SUBROUTINE PARAMETER DECLARATIONS:
        ! multiplier and shift for 4-byte integer and divisor of 10000
        tSInt32, PARAMETER  :: ShiftPos = 45
        tSInt64, PARAMETER  :: Multiplier = ToInt64(Z'00000000D1B71759')
        tSInt32, PARAMETER  :: Divisor = 10000

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: PosNum, NxtNum, RemNum

    !** FLOW
    
        ! set working number
        PosNum = Number
        ! compute NxtNum = PosNum/10000
        NxtNum = ToInt32(SHIFTR(PosNum*Multiplier, ShiftPos))
        ! compute RemNum = MOD(PosNum, 10000)
        RemNum = PosNum - NxtNum*Divisor
        ! convert the remainder to a working string
        cStr(5:8) = Char4Digits(RemNum)
        ! convert the rest
        cStr(1:4) = Char4Digits(NxtNum)
        
        RETURN

    END SUBROUTINE Write_8_Digits

    !**************************************************************************

    FUNCTION Write_1_to_8_Digits(Number, cStr) RESULT(Start)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 1 to 8

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: Number   ! number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string
        tSInt32                     :: Start

    !** SUBROUTINE PARAMETER DECLARATIONS:
        ! multiplier and shift for 4-byte integer and divisor of 10000
        tSInt32, PARAMETER  :: ShiftPos = 45
        tSInt64, PARAMETER  :: Multiplier = ToInt64(Z'00000000D1B71759')
        tSInt32, PARAMETER  :: Divisor = 10000

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: PosNum, NxtNum, RemNum

    !** FLOW
        
        PosNum = Number
        IF (PosNum < 10000) THEN
            IF (PosNum < 100) THEN
                cStr(7:8) = Char2Digits(PosNum)
                Start = 7
                IF (cStr(Start:Start) == '0') Start = 8
            ELSE
                cStr(5:8) = Char4Digits(PosNum)
                Start = 5
                IF (cStr(Start:Start) == '0') Start = 6
            END IF
        ELSE
            ! compute NxtNum = PosNum/10000
            NxtNum = ToInt32(SHIFTR(PosNum*Multiplier, ShiftPos))
            ! compute RemNum = MOD(PosNum, 10000)
            RemNum = PosNum - NxtNum*Divisor
            ! convert the remainder to a working string
            cStr(5:8) = Char4Digits(RemNum)
            IF (NxtNum < 100) THEN
                cStr(3:4) = Char2Digits(NxtNum)
                Start = 3
                IF (cStr(Start:Start) == '0') Start = 4
            ELSE
                cStr(1:4) = Char4Digits(NxtNum)
                Start  = 1
                IF (cStr(Start:Start) == '0') Start = 2
            END IF
        END IF
        
        RETURN

    END FUNCTION Write_1_to_8_Digits

    !**************************************************************************

END FUNCTION I64_ToChar_CC

!******************************************************************************

MODULE FUNCTION I64_ToChar_YY(Number) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert an integer number to character string

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(IN) :: Number   ! number
    tCharAlloc          :: cStr     ! character string

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: MaxLen = 20
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(MaxLen)    :: wStr
    tSInt64             :: PosNum, TmpNum, HiNum, LoNum, MidNum
    tSInt32             :: Start

!** FLOW

    ! set positive number
    PosNum = ABS(Number)
    
    ! start conversion and store digits in working string
    IF (PosNum < 100000000_kInt64) THEN                     ! 1-8 digits
        Start = 12 + Write_1_to_8_Digits(ToInt32(PosNum), wStr(13:20))
    ELSEIF (PosNum < 10000000000000000_kInt64) THEN         ! 9-16 digits
        HiNum = PosNum / 100000000_kInt64
        LoNum = PosNum - HiNum * 100000000_kInt64           ! MOD(PosNum, 100000000)
        CALL Write_8_Digits(ToInt32(LoNum), wStr(13:20))
        Start = 4 + Write_1_to_8_Digits(ToInt32(HiNum), wStr(5:12))
    ELSE                                                    ! 17-20 digits
        TmpNum = PosNum / 100000000_kInt64
        LoNum = PosNum - TmpNum * 100000000_kInt64          ! MOD(PosNum, 100000000)
        HiNum = TmpNum / 10000_kInt64
        MidNum = TmpNum - HiNum * 10000_kInt64              ! MOD(TmpNum, 10000)
        CALL Write_8_Digits(ToInt32(LoNum), wStr(13:20))
        CALL Write_4_Digits(ToInt32(MidNum), wStr(9:12))
        Start = Write_5_to_8_Digits(ToInt32(HiNum), wStr(1:8))
    END IF
    
    ! transfer to output string
    IF (Number < 0_kInt64) THEN
        IF (Number == MinI64) THEN
            cStr = '-9223372036854775808'
            RETURN
        END IF
        cStr = '-' // wStr(Start:MaxLen)
    ELSE
        cStr = wStr(Start:MaxLen)
    END IF
    
    RETURN
    
    CONTAINS

    SUBROUTINE Write_8_Digits(Number, cStr)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 8

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: Number   ! number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: AA, BB, CC, DD   ! working indices
        tSInt32     :: AABB, CCDD       ! working variables

    !** FLOW
    
        AABB = ToInt32(SHIFTR(ToInt64(Number)*109951163_kInt64, 40))    ! Number / 10000
        CCDD = Number - AABB*10000                                      ! MOD(Number, 10000)
        AA = SHIFTR(AABB*5243, 19)                                      ! AABB / 100
        CC = SHIFTR(CCDD*5243, 19)                                      ! CCDD / 100
        BB = AABB - AA*100                                              ! MOD(AABB, 100)
        DD = CCDD - CC*100                                              ! MOD(CCDD, 100)
        cStr(1:2) = Char2Digits(AA)
        cStr(3:4) = Char2Digits(BB)
        cStr(5:6) = Char2Digits(CC)
        cStr(7:8) = Char2Digits(DD)
        
        RETURN

    END SUBROUTINE Write_8_Digits

    !**************************************************************************

    SUBROUTINE Write_4_Digits(Number, cStr)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 8

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: Number   ! number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: AA, BB   ! working indices

    !** FLOW
    
        AA = SHIFTR(Number*5243, 19)            ! Number / 100
        BB = Number - AA*100                    ! MOD(Number, 100)
        cStr(1:2) = Char2Digits(AA)
        cStr(3:4) = Char2Digits(BB)
        
        RETURN

    END SUBROUTINE Write_4_Digits

    !**************************************************************************

    FUNCTION Write_1_to_8_Digits(Number, cStr) RESULT(Start)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 1 to 8

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: Number   ! number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string
        tSInt32                     :: Start

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: AA, BB, CC, DD   ! working indices
        tSInt32     :: AABB, BBCC, CCDD ! working variables

    !** FLOW
    
        IF (Number < 100) THEN                                          ! 1-2 digits
            AA = Number
            IF (AA < 10) THEN
                cStr(8:8) = Char2Digits(AA)(2:2)
                Start = 8
            ELSE
                cStr(7:8) = Char2Digits(AA)
                Start = 7
            END IF
        ELSEIF (Number < 10000) THEN                                    ! 3-4 digits
            AA = ToInt32(SHIFTR(ToInt64(Number)*5243_kInt64, 19))       ! Number / 100
            BB = Number - AA*100                                        ! MOD(Number, 100)
            IF (AA < 10) THEN
                cStr(6:6) = Char2Digits(AA)(2:2)
                cStr(7:8) = Char2Digits(BB)
                Start = 6
            ELSE
                cStr(5:6) = Char2Digits(AA)
                cStr(7:8) = Char2Digits(BB)
                Start = 5
            END IF
        ELSEIF (Number < 1000000) THEN                                  ! 5-6 digits
            AA = ToInt32(SHIFTR(ToInt64(Number)*429497_kInt64, 32))     ! Number / 10000
            BBCC = Number - AA*10000                                    ! MOD(Number, 10000)
            BB = SHIFTR(BBCC*5243, 19)                                  ! BBCC / 100
            CC = BBCC - BB*100                                          ! MOD(BBCC, 100)
            IF (AA < 10) THEN
                cStr(4:4) = Char2Digits(AA)(2:2)
                cStr(5:6) = Char2Digits(BB)
                cStr(7:8) = Char2Digits(CC)
                Start = 4
            ELSE
                cStr(3:4) = Char2Digits(AA)
                cStr(5:6) = Char2Digits(BB)
                cStr(7:8) = Char2Digits(CC)
                Start = 3
            END IF
        ELSEIF (Number < 100000000) THEN                                    ! 7-8 digits
            AABB = ToInt32(SHIFTR(ToInt64(Number)*109951163_kInt64, 40))    ! Number / 10000
            CCDD = Number - AABB*10000                                      ! MOD(Number, 10000)
            AA = SHIFTR(AABB*5243, 19)                                      ! AABB / 100
            CC = SHIFTR(CCDD*5243, 19)                                      ! CCDD / 100
            BB = AABB - AA*100                                              ! MOD(AABB, 100)
            DD = CCDD - CC*100                                              ! MOD(CCDD, 100)
            IF (AA < 10) THEN
                cStr(2:2) = Char2Digits(AA)(2:2)
                cStr(3:4) = Char2Digits(BB)
                cStr(5:6) = Char2Digits(CC)
                cStr(7:8) = Char2Digits(DD)
                Start = 2
            ELSE
                cStr(1:2) = Char2Digits(AA)
                cStr(3:4) = Char2Digits(BB)
                cStr(5:6) = Char2Digits(CC)
                cStr(7:8) = Char2Digits(DD)
                Start = 1
            END IF
        END IF
        
        RETURN

    END FUNCTION Write_1_to_8_Digits

    !**************************************************************************

    FUNCTION Write_5_to_8_Digits(Number, cStr) RESULT(Start)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 5 to 8

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: Number   ! number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string
        tSInt32                     :: Start

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: AA, BB, CC, DD   ! working indices
        tSInt32     :: AABB, BBCC, CCDD ! working variables

    !** FLOW
    
        IF (Number < 1000000) THEN                                      ! 5-6 digits
            AA = ToInt32(SHIFTR(ToInt64(Number)*429497_kInt64, 32))     ! Number / 10000
            BBCC = Number - AA*10000                                    ! MOD(Number, 10000)
            BB = SHIFTR(BBCC*5243, 19)                                  ! BBCC / 100
            CC = BBCC - BB*100                                          ! MOD(BBCC, 100)
            IF (AA < 10) THEN
                cStr(4:4) = Char2Digits(AA)(2:2)
                cStr(5:6) = Char2Digits(BB)
                cStr(7:8) = Char2Digits(CC)
                Start = 4
            ELSE
                cStr(3:4) = Char2Digits(AA)
                cStr(5:6) = Char2Digits(BB)
                cStr(7:8) = Char2Digits(CC)
                Start = 3
            END IF
        ELSEIF (Number < 100000000) THEN                                    ! 7-8 digits
            AABB = ToInt32(SHIFTR(ToInt64(Number)*109951163_kInt64, 40))    ! Number / 10000
            CCDD = Number - AABB*10000                                      ! MOD(Number, 10000)
            AA = SHIFTR(AABB*5243, 19)                                      ! AABB / 100
            CC = SHIFTR(CCDD*5243, 19)                                      ! CCDD / 100
            BB = AABB - AA*100                                              ! MOD(AABB, 100)
            DD = CCDD - CC*100                                              ! MOD(CCDD, 100)
            IF (AA < 10) THEN
                cStr(2:2) = Char2Digits(AA)(2:2)
                cStr(3:4) = Char2Digits(BB)
                cStr(5:6) = Char2Digits(CC)
                cStr(7:8) = Char2Digits(DD)
                Start = 2
            ELSE
                cStr(1:2) = Char2Digits(AA)
                cStr(3:4) = Char2Digits(BB)
                cStr(5:6) = Char2Digits(CC)
                cStr(7:8) = Char2Digits(DD)
                Start = 1
            END IF
        END IF
        
        RETURN

    END FUNCTION Write_5_to_8_Digits

    !**************************************************************************

END FUNCTION I64_ToChar_YY

!******************************************************************************

MODULE FUNCTION I64_ToChar_YYLL(Number) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert an integer number to character string

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(IN) :: Number   ! number
    tCharAlloc          :: cStr     ! character string

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: MaxLen = 20
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(MaxLen)    :: wStr
    tSInt64             :: PosNum, TmpNum, HiNum, LoNum, MidNum
    tSInt32             :: Start

!** FLOW

    ! set positive number
    PosNum = ABS(Number)
    
    ! start conversion and store digits in working string
    IF (PosNum < 100000000_kInt64) THEN                     ! 1-8 digits
        Start = 12 + Write_1_to_8_Digits(ToInt32(PosNum), wStr(13:20))
    ELSEIF (PosNum < 10000000000000000_kInt64) THEN         ! 9-16 digits
        HiNum = PosNum / 100000000_kInt64
        LoNum = PosNum - HiNum * 100000000_kInt64           ! MOD(PosNum, 100000000)
        CALL Write_8_Digits(ToInt32(LoNum), wStr(13:20))
        Start = 4 + Write_1_to_8_Digits(ToInt32(HiNum), wStr(5:12))
    ELSE                                                    ! 17-20 digits
        TmpNum = PosNum / 100000000_kInt64
        LoNum = PosNum - TmpNum * 100000000_kInt64          ! MOD(PosNum, 100000000)
        HiNum = TmpNum / 10000_kInt64
        MidNum = TmpNum - HiNum * 10000_kInt64              ! MOD(TmpNum, 10000)
        CALL Write_8_Digits(ToInt32(LoNum), wStr(13:20))
        CALL Write_4_Digits(ToInt32(MidNum), wStr(9:12))
        Start = Write_5_to_8_Digits(ToInt32(HiNum), wStr(1:8))
    END IF
    
    ! transfer to output string
    IF (Number < 0_kInt64) THEN
        IF (Number == MinI64) THEN
            cStr = '-9223372036854775808'
            RETURN
        END IF
        cStr = '-' // wStr(Start:MaxLen)
    ELSE
        cStr = wStr(Start:MaxLen)
    END IF
    
    RETURN
    
    CONTAINS

    SUBROUTINE Write_8_Digits(Number, cStr)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 8

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: Number   ! number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: AABB, CCDD       ! working variables

    !** FLOW
    
        AABB = ToInt32(SHIFTR(ToInt64(Number)*109951163_kInt64, 40))    ! Number / 10000
        CCDD = Number - AABB*10000                                      ! MOD(Number, 10000)
        cStr(1:4) = Char4Digits(AABB)
        cStr(5:8) = Char4Digits(CCDD)
        
        RETURN

    END SUBROUTINE Write_8_Digits

    !**************************************************************************

    SUBROUTINE Write_4_Digits(Number, cStr)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 8

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: Number   ! number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW
    
        cStr(1:4) = Char4Digits(Number)
        
        RETURN

    END SUBROUTINE Write_4_Digits

    !**************************************************************************

    FUNCTION Write_1_to_8_Digits(Number, cStr) RESULT(Start)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 1 to 8

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: Number   ! number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string
        tSInt32                     :: Start

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: AABB, CCDD

    !** FLOW
    
        IF (Number < 10000) THEN                                            ! 1-4 digits
            cStr(5:8) = Char4Digits(Number)
            Start = 5
            IF (cStr(Start:Start) == '0') THEN
                Start = 6
                IF (cStr(Start:Start) == '0') THEN
                    Start = 7
                    IF (cStr(Start:Start) == '0') THEN
                        Start = 8
                    END IF
                END IF
            END IF
        ELSE                                                                ! 5-8 digits
            AABB = ToInt32(SHIFTR(ToInt64(Number)*109951163_kInt64, 40))    ! Number / 10000
            CCDD = Number - AABB*10000                                      ! MOD(Number, 10000)
            cStr(1:4) = Char4Digits(AABB)
            cStr(5:8) = Char4Digits(CCDD)
            Start = 1
            IF (cStr(Start:Start) == '0') THEN
                Start = 2
                IF (cStr(Start:Start) == '0') THEN
                    Start = 3
                    IF (cStr(Start:Start) == '0') THEN
                        Start = 4
                    END IF
                END IF
            END IF
        END IF
        
        RETURN

    END FUNCTION Write_1_to_8_Digits

    !**************************************************************************

    FUNCTION Write_5_to_8_Digits(Number, cStr) RESULT(Start)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 5 to 8

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: Number   ! number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string
        tSInt32                     :: Start

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: AABB, CCDD

    !** FLOW
    
        AABB = ToInt32(SHIFTR(ToInt64(Number)*109951163_kInt64, 40))    ! Number / 10000
        CCDD = Number - AABB*10000                                      ! MOD(Number, 10000)
        cStr(1:4) = Char4Digits(AABB)
        cStr(5:8) = Char4Digits(CCDD)
        Start = 1
        IF (cStr(Start:Start) == '0') THEN
            Start = 2
            IF (cStr(Start:Start) == '0') THEN
                Start = 3
                IF (cStr(Start:Start) == '0') THEN
                    Start = 4
                END IF
            END IF
        END IF
        
        RETURN

    END FUNCTION Write_5_to_8_Digits

    !**************************************************************************

END FUNCTION I64_ToChar_YYLL

!******************************************************************************

MODULE FUNCTION I64_ToChar_JEA(Number) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert an integer number to character string

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(IN) :: Number   ! number
    tCharAlloc          :: cStr     ! character string

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: MaxLen = 20
    tQuad,   PARAMETER  :: R1  = 1.0_kQuad
    tQuad,   PARAMETER  :: R2  = 2.0_kQuad
    tSInt64, PARAMETER  :: K24 = ToInt64(R2**24/1.0E2_kQuad + R1)
    tSInt64, PARAMETER  :: K32 = ToInt64(R2**32/1.0E4_kQuad + R1)
    tSInt64, PARAMETER  :: K48 = ToInt64(R2**48/1.0E6_kQuad + R1)
    tSInt64, PARAMETER  :: K57 = ToInt64(R2**57/1.0E8_kQuad + R1)
    tSInt64, PARAMETER  :: M24 = SHIFTL(1_kInt64, 24) - 1_kInt64
    tSInt64, PARAMETER  :: M32 = SHIFTL(1_kInt64, 32) - 1_kInt64
    tSInt64, PARAMETER  :: M57 = SHIFTL(1_kInt64, 57) - 1_kInt64
    tSInt64, PARAMETER  :: MaxU32 = ToInt64(Z'00000000FFFFFFFF') !  4,294,967,296
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(MaxLen)    :: wStr
    tSInt64             :: PosNum
    tSInt32             :: Finish, Start
    tSInt64             :: F0, F2, F4, F6, F8
    tSInt64             :: Z, Y

!** FLOW
    
    ! set positive number
    PosNum = ABS(Number)
    
    ! start the conversion
    IF (PosNum < 100_kInt64) THEN                   ! 1-2 digits
        wStr(1:2) = Char2Digits(PosNum)
        Finish = 2
    ELSEIF (PosNum < 1000000_kInt64) THEN           ! 3-6 digits
        IF (PosNum < 10000_kInt64) THEN             ! 3-4 digits
            CALL Write_4_Digits(PosNum, wStr(1:4))
            Finish = 4
        ELSE                                        ! 5-6 digits
            CALL Write_6_Digits(PosNum, wStr(1:6))
            Finish = 6
        END IF
    ELSEIF (PosNum < MaxU32)    THEN                ! 7-10 digits
        IF (PosNum < 100000000_kInt64) THEN         ! 7-8 digits
            CALL Write_8_Digits(PosNum, wStr(1:8))
            Finish = 8
        ELSE                                        ! 9-10 digits
            CALL Write_10_Digits(PosNum, wStr(1:10))
            Finish = 10
        END IF
    ELSE
        Z = MOD(PosNum, 100000000_kInt64)
        PosNum = PosNum/100000000_kInt64
        IF (PosNum < 100_kInt64) THEN               ! 1-2 digits
            wStr(1:2) = Char2Digits(PosNum)
            Start = 3
        ELSEIF (PosNum < 1000000_kInt64) THEN       ! 3-6 digits
            IF (PosNum < 10000_kInt64) THEN         ! 3-4 digits
                CALL Write_4_Digits(PosNum, wStr(1:4))
                Start = 5
            ELSE                                    ! 5-6 digits
                CALL Write_6_Digits(PosNum, wStr(1:6))
                Start = 7
            END IF
        ELSEIF (PosNum < 100000000_kInt64) THEN     ! 7-8 digits
            CALL Write_8_Digits(PosNum, wStr(1:8))
            Start = 9
        ELSEIF (PosNum < MaxU32)    THEN            ! 9-10 digits
            CALL Write_10_Digits(PosNum, wStr(1:10))
            Start = 11
        ELSE
            Y = MOD(PosNum, 100000000_kInt64)
            PosNum = PosNum/100000000_kInt64
            ! n is 2, 3, or 4 digits (if n < 10 it would have been handled above)
            IF (PosNum < 100_kInt64) THEN           ! 1-2 digits
                wStr(1:2) = Char2Digits(PosNum)
                Start = 3
            ELSE
                F0 = K24*PosNum
                F2 = IAND(F0, M24)*100_kInt64
                wStr(1:2) = Char2Digits(SHIFTR(F0, 24))
                wStr(3:4) = Char2Digits(SHIFTR(F2, 24))
                Start = 5
            END IF
            ! do 8 digits
            Finish = Start + 7
            CALL Write_8_Digits(Y, wStr(Start:Finish))
            Start = Finish + 1
        END IF
        ! do 8 digits
        Finish = Start + 7
        CALL Write_8_Digits(Z, wStr(Start:Finish))
    END IF

    Start = 1
    IF (wStr(1:1) == '0') Start = 2
        
    IF (Number < 0) THEN
        IF (Number == MinI64) THEN
            cStr = '-9223372036854775808'
            RETURN
        END IF
        cStr = '-' // wStr(Start:Finish)
    ELSE
        cStr = wStr(Start:Finish)
    END IF
    
    RETURN
    
    CONTAINS

    SUBROUTINE Write_4_Digits(Number, cStr)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 8

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt64,   INTENT(IN)       :: Number   ! number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW
    
        F0 = K24*Number
        F2 = IAND(F0, M24)*100_kInt64
        cStr(1:2) = Char2Digits(SHIFTR(F0, 24))
        cStr(3:4) = Char2Digits(SHIFTR(F2, 24))
        
        RETURN

    END SUBROUTINE Write_4_Digits

    !**************************************************************************

    SUBROUTINE Write_6_Digits(Number, cStr)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 6

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt64,   INTENT(IN)       :: Number   ! number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW
    
        F0 = K32*Number
        F2 = IAND(F0, M32)*100_kInt64
        F4 = IAND(F2, M32)*100_kInt64
        cStr(1:2) = Char2Digits(SHIFTR(F0, 32))
        cStr(3:4) = Char2Digits(SHIFTR(F2, 32))
        cStr(5:6) = Char2Digits(SHIFTR(F4, 32))
        
        RETURN

    END SUBROUTINE Write_6_Digits

    !**************************************************************************

    SUBROUTINE Write_8_Digits(Number, cStr)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 8

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt64,   INTENT(IN)       :: Number   ! number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: AABB, CCDD       ! working variables

    !** FLOW
    
        AABB = ToInt32(SHIFTR(Number*109951163_kInt64, 40))     ! Number / 10000
        CCDD = Number - AABB*10000                              ! MOD(Number, 10000)
        cStr(1:4) = Char4Digits(AABB)
        cStr(5:8) = Char4Digits(CCDD)
        
        ! Note: the following statements (original code by JEA algorithm)
        !       do not write correct numbers sometimes
!        F0 = SHIFTR(K48*Number, 16)
!        F2 = IAND(F0, M32)*100_kInt64
!        F4 = IAND(F2, M32)*100_kInt64
!        F6 = IAND(F4, M32)*100_kInt64
!        cStr(1:2) = Char2Digits(SHIFTR(F0, 32))
!        cStr(3:4) = Char2Digits(SHIFTR(F2, 32))
!        cStr(5:6) = Char2Digits(SHIFTR(F4, 32))
!        cStr(7:8) = Char2Digits(SHIFTR(F6, 32))
        
        RETURN

    END SUBROUTINE Write_8_Digits

    !**************************************************************************

    SUBROUTINE Write_10_Digits(Number, cStr)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 8

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt64,   INTENT(IN)       :: Number   ! number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW
    
        F0 = K57*Number
        F2 = IAND(F0, M57)*100_kInt64
        F4 = IAND(F2, M57)*100_kInt64
        F6 = IAND(F4, M57)*100_kInt64
        F8 = IAND(F6, M57)*100_kInt64
        cStr(1:2)  = Char2Digits(SHIFTR(F0, 57))
        cStr(3:4)  = Char2Digits(SHIFTR(F2, 57))
        cStr(5:6)  = Char2Digits(SHIFTR(F4, 57))
        cStr(7:8)  = Char2Digits(SHIFTR(F6, 57))
        cStr(9:10) = Char2Digits(SHIFTR(F8, 57))
        
        RETURN

    END SUBROUTINE Write_10_Digits

    !**************************************************************************

END FUNCTION I64_ToChar_JEA

!******************************************************************************

END SUBMODULE SubBase_IntToChar

!******************************************************************************
