! Macro - Util Definitions.f90
 
! Fortran preprocessor must be enabled: -fpp.

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++                  SWAP/EXCHANGE MACROS                +++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#define SWAP_SCALAR(A, B) \
    Temp = A; \
    A = B; \
    B = Temp;

#define SWAP_DOUBLE(A, B, IA, IB) \
    Temp = A; \
    A = B; \
    B = Temp; \
    ITemp = IA; \
    IA = IB; \
    IB = ITemp;

#define SWAP_ARRAY(A, B, IBegin, IEnd) \
    DO I = IBegin, IEnd; \
        Temp = A(I); \
        A(I) = B(I); \
        B(I) = Temp;\
    END DO;

#define EXCHANGE(A, I, J) \
    Temp = A(I); \
    A(I) = A(J); \
    A(J) = Temp;

#define EXCHANGE_DOUBLE(AVal, APos, I, J) \
    Temp = AVal(I); \
    AVal(I) = AVal(J); \
    AVal(J) = Temp; \
    ITemp = APos(I); \
    APos(I) = APos(J); \
    APos(J) = ITemp;

#define EXCHANGE_RECORD_ITEMS(A, I, J) \
    Temp%Val = A%Val(I); \
    Temp%Pos = A%Pos(I); \
    A%Val(I) = A%Val(J); \
    A%Pos(I) = A%Pos(J); \
    A%Val(J) = Temp%Val; \
    A%Pos(J) = Temp%Pos;

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++                   ASSIGN/COPY MACROS                 +++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#define ASSIGN_DOUBLE(AVal, APos, I, J) \
    AVal(I) = AVal(J); \
    APos(I) = APos(J);

#define COPY_DOUBLE_S2A(AVal, APos, I, BVal, BPos) \
    AVal(I) = BVal; \
    APos(I) = BPos;

#define COPY_DOUBLE_A2S(AVal, APos, BVal, BPos, I) \
    AVal = BVal(I); \
    APos = BPos(I);

#define COPY_ARRAY_ZERO_BASED(SrcArr, SrcBase, DstArr, DstBase, ALen) \
    DO Index = 0_kIP, ALen-1_kIP; \
        DstArr(DstBase+Index) = SrcArr(SrcBase+Index); \
    END DO;

#define COPY_ARRAY_ZERO_BASED_REVERSE(SrcArr, SrcBase, DstArr, DstBase, ALen) \
    DO Index = ALen-1_kIP, 0_kIP, -1_kIP; \
        DstArr(DstBase+Index) = SrcArr(SrcBase+Index); \
    END DO;


!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++                    ASSERTION MACROS                  +++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

! show general error message if assert condition is false
#ifdef DebugMode
#define ASSERT(Condition, SubName) \
    IF (.NOT.Condition) THEN; \
        WRITE(*,'(A,A,A,I10)') 'Assertion failed in file ', __FILE__,', at line number', __LINE__  ; \
        CALL Handle_ErrLevel(SubName, ModName, ErrWarning, 'Assert condition is false.'); \
        RETURN; \
    END IF;
#else
#define ASSERT(Condition, SubName)
#endif

! show specified error message if assert condition is false
#ifdef DebugMode
#define ASSERT_MSG(Condition, SubName, Message) \
    IF (.NOT.Condition) THEN; \
        CALL Handle_ErrLevel(SubName, ModName, ErrWarning, Message); \
        RETURN; \
    END IF;
#else
#define ASSERT_MSG(Condition, SubName, Message)
#endif

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++                  MISCELLANEOUS MACROS                +++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#define SET_OPTION(Param, Default, Option) \
    Param = Default; \
    IF (PRESENT(Option)) Param = Option;

#define SET_OPTION_WITH_LIMITS(Param, Default, Option, MinVal, MaxVal) \
    Param = Default; \
    IF (PRESENT(Option)) THEN;\
        IF ((Option >= MinVal).AND.(Option <= MaxVal)) THEN;\
            Param = Option;\
        END IF;\
    END IF;

#define IF_THEN_ELSE(Condition, TrueVal, FalseVal, ResVal) \
    IF (Condition) THEN;\
        ResVal = TrueVal;\
    ELSE;\
        ResVal = FalseVal;\
    END IF;

#define IN_RANGE(Val,LoVal,HiVal)       (Val >= LoVal).AND.(Val <= HiVal)
#define NOT_IN_RANGE(Val,LoVal,HiVal)   (Val < LoVal).OR.(Val > HiVal)

! Macros for bitwise operation routines. 
#define RotateLeft(V,P)     ISHFTC(V,  P)
#define RotateRight(V,P)    ISHFTC(V, -P)
