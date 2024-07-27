
MODULE Class_GroestlS

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *GroestlS* type and its related routines.
!   The *GroestlS* type is a *digest* type that directly extends from the
!   <a href="../module/class_mdengine.html#type-mdengine">MDEngine</a>
!   type.  As a *concrete* derived type, it provides all remaining
!   deferred procedures required by all its abstract parent types. <br>
!   The *GroestlS* type implements an incremental cryptographic hash function
!   by employing either the *Groestl-224* or the *Groestl-256 message-digest*
!   algorithm [1].  The implementation here is based mainly on the *SPHLIB*
!   implementation [2].  <br>
!   By default, the *GroestlS* type employs the *Groestl-256 message-digest*
!   algorithm.  However, a user can specify the *IsGroestl224* flag to
!   true when initializing the digest object (by calling the *Create*
!   method) in order to use the *Groestl-224 message-digest* algorithm
!   instead of the default one. <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="http://www.groestl.info/">Grostl - a SHA-3 candidate. </a> <br>
!   [2] <a href="https://github.com/pornin/sphlib">SPHLIB 3.0: A Set of
!       Implementations of Various Hash Functions, Both in C and in Java. </a> <br>

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_BytePack,           ONLY: BytePackBE, ByteUnpackBE
    USE Class_BaseDigest
    USE Class_MDEngine

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: GroestlS

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define BX64(X, Pos)    ToInteger(SHIFTR(X, Pos))
#define BY64(X, Pos)    IAND(ToInteger(SHIFTR(X, Pos)), ToInteger(Z'000000FF'))
#define RSTT(A, I0, I1, I2, I3, I4, I5, I6, I7) \
    IEOR(IEOR(IEOR(IEOR(IEOR(IEOR(IEOR(T0(BX64(A(I0), 56)),  \
                                       T1(BY64(A(I1), 48))), \
                                       T2(BY64(A(I2), 40))), \
                                       T3(BY64(A(I3), 32))), \
                                       T4(BY64(A(I4), 24))), \
                                       T5(BY64(A(I5), 16))), \
                                       T6(BY64(A(I6),  8))), \
                                       T7(BY64(A(I7),  0)))
#define PC64(A, J, R)   IEOR(A, SHIFTL(ToLong(J + R), 56))
#define QC64(A, J, R)   IEOR(A, IEOR(-ToLong(J), ToLong(R)))

!** MODULE PARAMETERS:
    tIndex, PARAMETER           :: BlockLen = 64_kIndex
    tIndex, PARAMETER           :: DLen224  = 28_kIndex
    tIndex, PARAMETER           :: DLen256  = 32_kIndex
    tLong,  PARAMETER, PUBLIC   :: T0(0:255) = [                      &
            ToLong(Z'C632F4A5F497A5C6'), ToLong(Z'F86F978497EB84F8'), &
            ToLong(Z'EE5EB099B0C799EE'), ToLong(Z'F67A8C8D8CF78DF6'), &
            ToLong(Z'FFE8170D17E50DFF'), ToLong(Z'D60ADCBDDCB7BDD6'), &
            ToLong(Z'DE16C8B1C8A7B1DE'), ToLong(Z'916DFC54FC395491'), &
            ToLong(Z'6090F050F0C05060'), ToLong(Z'0207050305040302'), &
            ToLong(Z'CE2EE0A9E087A9CE'), ToLong(Z'56D1877D87AC7D56'), &
            ToLong(Z'E7CC2B192BD519E7'), ToLong(Z'B513A662A67162B5'), &
            ToLong(Z'4D7C31E6319AE64D'), ToLong(Z'EC59B59AB5C39AEC'), &
            ToLong(Z'8F40CF45CF05458F'), ToLong(Z'1FA3BC9DBC3E9D1F'), &
            ToLong(Z'8949C040C0094089'), ToLong(Z'FA68928792EF87FA'), &
            ToLong(Z'EFD03F153FC515EF'), ToLong(Z'B29426EB267FEBB2'), &
            ToLong(Z'8ECE40C94007C98E'), ToLong(Z'FBE61D0B1DED0BFB'), &
            ToLong(Z'416E2FEC2F82EC41'), ToLong(Z'B31AA967A97D67B3'), &
            ToLong(Z'5F431CFD1CBEFD5F'), ToLong(Z'456025EA258AEA45'), &
            ToLong(Z'23F9DABFDA46BF23'), ToLong(Z'535102F702A6F753'), &
            ToLong(Z'E445A196A1D396E4'), ToLong(Z'9B76ED5BED2D5B9B'), &
            ToLong(Z'75285DC25DEAC275'), ToLong(Z'E1C5241C24D91CE1'), &
            ToLong(Z'3DD4E9AEE97AAE3D'), ToLong(Z'4CF2BE6ABE986A4C'), &
            ToLong(Z'6C82EE5AEED85A6C'), ToLong(Z'7EBDC341C3FC417E'), &
            ToLong(Z'F5F3060206F102F5'), ToLong(Z'8352D14FD11D4F83'), &
            ToLong(Z'688CE45CE4D05C68'), ToLong(Z'515607F407A2F451'), &
            ToLong(Z'D18D5C345CB934D1'), ToLong(Z'F9E1180818E908F9'), &
            ToLong(Z'E24CAE93AEDF93E2'), ToLong(Z'AB3E9573954D73AB'), &
            ToLong(Z'6297F553F5C45362'), ToLong(Z'2A6B413F41543F2A'), &
            ToLong(Z'081C140C14100C08'), ToLong(Z'9563F652F6315295'), &
            ToLong(Z'46E9AF65AF8C6546'), ToLong(Z'9D7FE25EE2215E9D'), &
            ToLong(Z'3048782878602830'), ToLong(Z'37CFF8A1F86EA137'), &
            ToLong(Z'0A1B110F11140F0A'), ToLong(Z'2FEBC4B5C45EB52F'), &
            ToLong(Z'0E151B091B1C090E'), ToLong(Z'247E5A365A483624'), &
            ToLong(Z'1BADB69BB6369B1B'), ToLong(Z'DF98473D47A53DDF'), &
            ToLong(Z'CDA76A266A8126CD'), ToLong(Z'4EF5BB69BB9C694E'), &
            ToLong(Z'7F334CCD4CFECD7F'), ToLong(Z'EA50BA9FBACF9FEA'), &
            ToLong(Z'123F2D1B2D241B12'), ToLong(Z'1DA4B99EB93A9E1D'), &
            ToLong(Z'58C49C749CB07458'), ToLong(Z'3446722E72682E34'), &
            ToLong(Z'3641772D776C2D36'), ToLong(Z'DC11CDB2CDA3B2DC'), &
            ToLong(Z'B49D29EE2973EEB4'), ToLong(Z'5B4D16FB16B6FB5B'), &
            ToLong(Z'A4A501F60153F6A4'), ToLong(Z'76A1D74DD7EC4D76'), &
            ToLong(Z'B714A361A37561B7'), ToLong(Z'7D3449CE49FACE7D'), &
            ToLong(Z'52DF8D7B8DA47B52'), ToLong(Z'DD9F423E42A13EDD'), &
            ToLong(Z'5ECD937193BC715E'), ToLong(Z'13B1A297A2269713'), &
            ToLong(Z'A6A204F50457F5A6'), ToLong(Z'B901B868B86968B9'), &
            ToLong(Z'0000000000000000'), ToLong(Z'C1B5742C74992CC1'), &
            ToLong(Z'40E0A060A0806040'), ToLong(Z'E3C2211F21DD1FE3'), &
            ToLong(Z'793A43C843F2C879'), ToLong(Z'B69A2CED2C77EDB6'), &
            ToLong(Z'D40DD9BED9B3BED4'), ToLong(Z'8D47CA46CA01468D'), &
            ToLong(Z'671770D970CED967'), ToLong(Z'72AFDD4BDDE44B72'), &
            ToLong(Z'94ED79DE7933DE94'), ToLong(Z'98FF67D4672BD498'), &
            ToLong(Z'B09323E8237BE8B0'), ToLong(Z'855BDE4ADE114A85'), &
            ToLong(Z'BB06BD6BBD6D6BBB'), ToLong(Z'C5BB7E2A7E912AC5'), &
            ToLong(Z'4F7B34E5349EE54F'), ToLong(Z'EDD73A163AC116ED'), &
            ToLong(Z'86D254C55417C586'), ToLong(Z'9AF862D7622FD79A'), &
            ToLong(Z'6699FF55FFCC5566'), ToLong(Z'11B6A794A7229411'), &
            ToLong(Z'8AC04ACF4A0FCF8A'), ToLong(Z'E9D9301030C910E9'), &
            ToLong(Z'040E0A060A080604'), ToLong(Z'FE66988198E781FE'), &
            ToLong(Z'A0AB0BF00B5BF0A0'), ToLong(Z'78B4CC44CCF04478'), &
            ToLong(Z'25F0D5BAD54ABA25'), ToLong(Z'4B753EE33E96E34B'), &
            ToLong(Z'A2AC0EF30E5FF3A2'), ToLong(Z'5D4419FE19BAFE5D'), &
            ToLong(Z'80DB5BC05B1BC080'), ToLong(Z'0580858A850A8A05'), &
            ToLong(Z'3FD3ECADEC7EAD3F'), ToLong(Z'21FEDFBCDF42BC21'), &
            ToLong(Z'70A8D848D8E04870'), ToLong(Z'F1FD0C040CF904F1'), &
            ToLong(Z'63197ADF7AC6DF63'), ToLong(Z'772F58C158EEC177'), &
            ToLong(Z'AF309F759F4575AF'), ToLong(Z'42E7A563A5846342'), &
            ToLong(Z'2070503050403020'), ToLong(Z'E5CB2E1A2ED11AE5'), &
            ToLong(Z'FDEF120E12E10EFD'), ToLong(Z'BF08B76DB7656DBF'), &
            ToLong(Z'8155D44CD4194C81'), ToLong(Z'18243C143C301418'), &
            ToLong(Z'26795F355F4C3526'), ToLong(Z'C3B2712F719D2FC3'), &
            ToLong(Z'BE8638E13867E1BE'), ToLong(Z'35C8FDA2FD6AA235'), &
            ToLong(Z'88C74FCC4F0BCC88'), ToLong(Z'2E654B394B5C392E'), &
            ToLong(Z'936AF957F93D5793'), ToLong(Z'55580DF20DAAF255'), &
            ToLong(Z'FC619D829DE382FC'), ToLong(Z'7AB3C947C9F4477A'), &
            ToLong(Z'C827EFACEF8BACC8'), ToLong(Z'BA8832E7326FE7BA'), &
            ToLong(Z'324F7D2B7D642B32'), ToLong(Z'E642A495A4D795E6'), &
            ToLong(Z'C03BFBA0FB9BA0C0'), ToLong(Z'19AAB398B3329819'), &
            ToLong(Z'9EF668D16827D19E'), ToLong(Z'A322817F815D7FA3'), &
            ToLong(Z'44EEAA66AA886644'), ToLong(Z'54D6827E82A87E54'), &
            ToLong(Z'3BDDE6ABE676AB3B'), ToLong(Z'0B959E839E16830B'), &
            ToLong(Z'8CC945CA4503CA8C'), ToLong(Z'C7BC7B297B9529C7'), &
            ToLong(Z'6B056ED36ED6D36B'), ToLong(Z'286C443C44503C28'), &
            ToLong(Z'A72C8B798B5579A7'), ToLong(Z'BC813DE23D63E2BC'), &
            ToLong(Z'1631271D272C1D16'), ToLong(Z'AD379A769A4176AD'), &
            ToLong(Z'DB964D3B4DAD3BDB'), ToLong(Z'649EFA56FAC85664'), &
            ToLong(Z'74A6D24ED2E84E74'), ToLong(Z'1436221E22281E14'), &
            ToLong(Z'92E476DB763FDB92'), ToLong(Z'0C121E0A1E180A0C'), &
            ToLong(Z'48FCB46CB4906C48'), ToLong(Z'B88F37E4376BE4B8'), &
            ToLong(Z'9F78E75DE7255D9F'), ToLong(Z'BD0FB26EB2616EBD'), &
            ToLong(Z'43692AEF2A86EF43'), ToLong(Z'C435F1A6F193A6C4'), &
            ToLong(Z'39DAE3A8E372A839'), ToLong(Z'31C6F7A4F762A431'), &
            ToLong(Z'D38A593759BD37D3'), ToLong(Z'F274868B86FF8BF2'), &
            ToLong(Z'D583563256B132D5'), ToLong(Z'8B4EC543C50D438B'), &
            ToLong(Z'6E85EB59EBDC596E'), ToLong(Z'DA18C2B7C2AFB7DA'), &
            ToLong(Z'018E8F8C8F028C01'), ToLong(Z'B11DAC64AC7964B1'), &
            ToLong(Z'9CF16DD26D23D29C'), ToLong(Z'49723BE03B92E049'), &
            ToLong(Z'D81FC7B4C7ABB4D8'), ToLong(Z'ACB915FA1543FAAC'), &
            ToLong(Z'F3FA090709FD07F3'), ToLong(Z'CFA06F256F8525CF'), &
            ToLong(Z'CA20EAAFEA8FAFCA'), ToLong(Z'F47D898E89F38EF4'), &
            ToLong(Z'476720E9208EE947'), ToLong(Z'1038281828201810'), &
            ToLong(Z'6F0B64D564DED56F'), ToLong(Z'F073838883FB88F0'), &
            ToLong(Z'4AFBB16FB1946F4A'), ToLong(Z'5CCA967296B8725C'), &
            ToLong(Z'38546C246C702438'), ToLong(Z'575F08F108AEF157'), &
            ToLong(Z'732152C752E6C773'), ToLong(Z'9764F351F3355197'), &
            ToLong(Z'CBAE6523658D23CB'), ToLong(Z'A125847C84597CA1'), &
            ToLong(Z'E857BF9CBFCB9CE8'), ToLong(Z'3E5D6321637C213E'), &
            ToLong(Z'96EA7CDD7C37DD96'), ToLong(Z'611E7FDC7FC2DC61'), &
            ToLong(Z'0D9C9186911A860D'), ToLong(Z'0F9B9485941E850F'), &
            ToLong(Z'E04BAB90ABDB90E0'), ToLong(Z'7CBAC642C6F8427C'), &
            ToLong(Z'712657C457E2C471'), ToLong(Z'CC29E5AAE583AACC'), &
            ToLong(Z'90E373D8733BD890'), ToLong(Z'06090F050F0C0506'), &
            ToLong(Z'F7F4030103F501F7'), ToLong(Z'1C2A36123638121C'), &
            ToLong(Z'C23CFEA3FE9FA3C2'), ToLong(Z'6A8BE15FE1D45F6A'), &
            ToLong(Z'AEBE10F91047F9AE'), ToLong(Z'69026BD06BD2D069'), &
            ToLong(Z'17BFA891A82E9117'), ToLong(Z'9971E858E8295899'), &
            ToLong(Z'3A5369276974273A'), ToLong(Z'27F7D0B9D04EB927'), &
            ToLong(Z'D991483848A938D9'), ToLong(Z'EBDE351335CD13EB'), &
            ToLong(Z'2BE5CEB3CE56B32B'), ToLong(Z'2277553355443322'), &
            ToLong(Z'D204D6BBD6BFBBD2'), ToLong(Z'A9399070904970A9'), &
            ToLong(Z'07878089800E8907'), ToLong(Z'33C1F2A7F266A733'), &
            ToLong(Z'2DECC1B6C15AB62D'), ToLong(Z'3C5A66226678223C'), &
            ToLong(Z'15B8AD92AD2A9215'), ToLong(Z'C9A96020608920C9'), &
            ToLong(Z'875CDB49DB154987'), ToLong(Z'AAB01AFF1A4FFFAA'), &
            ToLong(Z'50D8887888A07850'), ToLong(Z'A52B8E7A8E517AA5'), &
            ToLong(Z'03898A8F8A068F03'), ToLong(Z'594A13F813B2F859'), &
            ToLong(Z'09929B809B128009'), ToLong(Z'1A2339173934171A'), &
            ToLong(Z'651075DA75CADA65'), ToLong(Z'D784533153B531D7'), &
            ToLong(Z'84D551C65113C684'), ToLong(Z'D003D3B8D3BBB8D0'), &
            ToLong(Z'82DC5EC35E1FC382'), ToLong(Z'29E2CBB0CB52B029'), &
            ToLong(Z'5AC3997799B4775A'), ToLong(Z'1E2D3311333C111E'), &
            ToLong(Z'7B3D46CB46F6CB7B'), ToLong(Z'A8B71FFC1F4BFCA8'), &
            ToLong(Z'6D0C61D661DAD66D'), ToLong(Z'2C624E3A4E583A2C')]
    tInteger                    :: Indx
    tLong,  PARAMETER, PUBLIC   :: T1(0:255) = [(RotateLeft(T0(Indx), 56), Indx = 0, 255)]
    tLong,  PARAMETER, PUBLIC   :: T2(0:255) = [(RotateLeft(T0(Indx), 48), Indx = 0, 255)]
    tLong,  PARAMETER, PUBLIC   :: T3(0:255) = [(RotateLeft(T0(Indx), 40), Indx = 0, 255)]
    tLong,  PARAMETER, PUBLIC   :: T4(0:255) = [(RotateLeft(T0(Indx), 32), Indx = 0, 255)]
    tLong,  PARAMETER, PUBLIC   :: T5(0:255) = [(RotateLeft(T0(Indx), 24), Indx = 0, 255)]
    tLong,  PARAMETER, PUBLIC   :: T6(0:255) = [(RotateLeft(T0(Indx), 16), Indx = 0, 255)]
    tLong,  PARAMETER, PUBLIC   :: T7(0:255) = [(RotateLeft(T0(Indx),  8), Indx = 0, 255)]

!** DERIVED TYPE DEFINITIONS
    !> *GroestlS* is a concrete *digest* type that implements an
    !  incremental cryptographic hash function by employing either
    !  the *Groestl-224* or the *Groestl-256 message-digest* algorithm.
    TYPE, EXTENDS(MDEngine) :: GroestlS
        PRIVATE
        !% buffer array used to store input data
        tByte       :: BufArr(0:BlockLen-1) = 0_kByte
        !% state variable
        tLong       :: H(0:7) = 0_kLong
        !% flag indicating whether the Groestl-224 algorithm is employed or not.
        tLogical    :: IsGroestl224 = FalseVal
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                    Private Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *InitializeWFlag* method to
        !  initialize the *digest* object with specified flag.
        PROCEDURE, PRIVATE  :: InitializeWFlag  => GroestlS_Initialize_wFlag
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object with default algorithm (Groestl-256).
        PROCEDURE       :: Initialize   => GroestlS_Initialize
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE       :: Reset        => GroestlS_Reset
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone     => GroestlS_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName      => GroestlS_GetName
        !> **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the natural hash function output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE       :: GetDigestLen => GroestlS_GetDigestLen
        !> *GetBlockLen* is a procedure to return the block length of the digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: GetBlockLen  => GroestlS_GetBlockLen
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: SetBufPtr    => GroestlS_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: ProcessBlock => GroestlS_ProcessBlock
        !> *DoPadding* is a procedure to perform padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: DoPadding    => GroestlS_DoPadding
        !> *AddBitsNPad* is a procedure to add the final partial byte and then perform
        !  padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: AddBitsNPad  => GroestlS_AddBitsNPad
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Create <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object. <br>
        !  **Usage**: <br>
        !   ! initialize the object to employ the default (Groestl-256) algorithm <br>
        !   --->    CALL MD%Create() <br>
        !   ! initialize the object to employ the Groestl-224 algorithm <br>
        !   --->    CALL MD%Create(IsGroestl224=.TRUE.) <br>
        GENERIC         :: Create       => InitializeWFlag
        ! ---------------------------------------------------------------------
    END TYPE GroestlS

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE GroestlS_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with default algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlS), INTENT(INOUT)  :: MD    !! 'GroestlS' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! to employ the Groestl-256 algorithm
    CALL MD%Create(FalseVal)
   
    RETURN

END SUBROUTINE GroestlS_Initialize

!******************************************************************************

SUBROUTINE GroestlS_Initialize_wFlag(MD, IsGroestl224)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with the specified flag.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlS), INTENT(INOUT)  :: MD           !! 'GroestlS' object
    tLogical,        INTENT(IN)     :: IsGroestl224
    !^ flag indicating whether the Groestl-224 algorithm is employed or not. <br>
    !  - If true, use the Groestl-224 algorithm. <br>
    !  - Otherwise, use the Groestl-256 algorithm. <br>

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    MD%IsGroestl224 = IsGroestl224
    CALL MD%Reset()
   
    RETURN

END SUBROUTINE GroestlS_Initialize_wFlag

!******************************************************************************

SUBROUTINE GroestlS_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the hash algorithm state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlS), INTENT(INOUT)  :: MD   !! 'GroestlS' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    MD%BufArr = 0_kByte
    MD%H(0:6) = 0_kLong
    MD%H(7)   = SHIFTL(ToLong(MD%GetDigestLen()), 3)
    CALL MD%EngineReset()

    RETURN

END SUBROUTINE GroestlS_Reset

!******************************************************************************

SUBROUTINE GroestlS_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlS),                INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(GroestlS :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (GroestlS)
        CALL Dst%Create(Src%IsGroestl224)
        Dst%H      = Src%H
        Dst%BufArr = Src%BufArr
        CALL Src%EngineClone(Dst)
    END SELECT
        
    RETURN

END SUBROUTINE GroestlS_GetClone

!******************************************************************************

FUNCTION GroestlS_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlS), INTENT(IN) :: MD       !! 'GroestlS' object
    tCharAlloc                  :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsGroestl224) THEN
        Name = 'Groestl-224'
    ELSE
        Name = 'Groestl-256'
    END IF

    RETURN

END FUNCTION GroestlS_GetName

!******************************************************************************

FUNCTION GroestlS_GetDigestLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the natural hash function output length (in bytes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlS), INTENT(IN) :: MD       !! 'GroestlS' object
    tIndex                      :: Length   !! the digest length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsGroestl224) THEN
        Length = DLen224
    ELSE
        Length = DLen256
    END IF

    RETURN

END FUNCTION GroestlS_GetDigestLen

!******************************************************************************

FUNCTION GroestlS_GetBlockLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the block length for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlS), INTENT(IN) :: MD       !! 'GroestlS' object
    tIndex                      :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Length = BlockLen
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION GroestlS_GetBlockLen

!******************************************************************************

SUBROUTINE GroestlS_SetBufPtr(MD, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlS), TARGET, INTENT(INOUT)  :: MD           !! 'GroestlS' object
    tByte,          POINTER, INTENT(INOUT)  :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => MD%BufArr

    RETURN

END SUBROUTINE GroestlS_SetBufPtr

!******************************************************************************

SUBROUTINE GroestlS_ProcessBlock(MD, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlS), INTENT(INOUT)  :: MD           !! 'GroestlS' object
    tByte,           INTENT(IN)     :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: G(0:7)
    tLong       :: MS(0:7)
    tIndex      :: I

! FLOW

    ! input block
    CALL BytePackBE(BytesIn, 0_kIndex, MS)
    DO I = 0, 7
        G(I) = IEOR(MS(I), MD%H(I))
    END DO

    ! perform permutations
    CALL DoPermP(G)
    CALL DoPermQ(MS)

    ! get output states
    DO I = 0, 7
        MD%H(I) = IEOR(MD%H(I), IEOR(G(I), MS(I)))
    END DO

    RETURN

CONTAINS

    SUBROUTINE DoPermQ(X)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform permutation Q.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(INOUT)    :: X(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: T(0:7)
        tInteger    :: R, RP1

    ! FLOW

        DO R = 0, 9, 2
            X(0) = QC64(X(0), ToInteger(Z'00000001'), R)
            X(1) = QC64(X(1), ToInteger(Z'00000011'), R)
            X(2) = QC64(X(2), ToInteger(Z'00000021'), R)
            X(3) = QC64(X(3), ToInteger(Z'00000031'), R)
            X(4) = QC64(X(4), ToInteger(Z'00000041'), R)
            X(5) = QC64(X(5), ToInteger(Z'00000051'), R)
            X(6) = QC64(X(6), ToInteger(Z'00000061'), R)
            X(7) = QC64(X(7), ToInteger(Z'00000071'), R)
            T(0) = RSTT(X, 1, 3, 5, 7, 0, 2, 4, 6)
            T(1) = RSTT(X, 2, 4, 6, 0, 1, 3, 5, 7)
            T(2) = RSTT(X, 3, 5, 7, 1, 2, 4, 6, 0)
            T(3) = RSTT(X, 4, 6, 0, 2, 3, 5, 7, 1)
            T(4) = RSTT(X, 5, 7, 1, 3, 4, 6, 0, 2)
            T(5) = RSTT(X, 6, 0, 2, 4, 5, 7, 1, 3)
            T(6) = RSTT(X, 7, 1, 3, 5, 6, 0, 2, 4)
            T(7) = RSTT(X, 0, 2, 4, 6, 7, 1, 3, 5)
            RP1 = R + 1
            T(0) = QC64(T(0), ToInteger(Z'00000001'), RP1)
            T(1) = QC64(T(1), ToInteger(Z'00000011'), RP1)
            T(2) = QC64(T(2), ToInteger(Z'00000021'), RP1)
            T(3) = QC64(T(3), ToInteger(Z'00000031'), RP1)
            T(4) = QC64(T(4), ToInteger(Z'00000041'), RP1)
            T(5) = QC64(T(5), ToInteger(Z'00000051'), RP1)
            T(6) = QC64(T(6), ToInteger(Z'00000061'), RP1)
            T(7) = QC64(T(7), ToInteger(Z'00000071'), RP1)
            X(0) = RSTT(T, 1, 3, 5, 7, 0, 2, 4, 6)
            X(1) = RSTT(T, 2, 4, 6, 0, 1, 3, 5, 7)
            X(2) = RSTT(T, 3, 5, 7, 1, 2, 4, 6, 0)
            X(3) = RSTT(T, 4, 6, 0, 2, 3, 5, 7, 1)
            X(4) = RSTT(T, 5, 7, 1, 3, 4, 6, 0, 2)
            X(5) = RSTT(T, 6, 0, 2, 4, 5, 7, 1, 3)
            X(6) = RSTT(T, 7, 1, 3, 5, 6, 0, 2, 4)
            X(7) = RSTT(T, 0, 2, 4, 6, 7, 1, 3, 5)
        END DO

        RETURN

    END SUBROUTINE DoPermQ

    !**************************************************************************

END SUBROUTINE GroestlS_ProcessBlock

!******************************************************************************

SUBROUTINE DoPermP(X)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform permutation P.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(INOUT)    :: X(0:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: T(0:7)
    tInteger    :: R, RP1

! FLOW

    DO R = 0, 9, 2
        X(0) = PC64(X(0), ToInteger(Z'00000000'), R)
        X(1) = PC64(X(1), ToInteger(Z'00000010'), R)
        X(2) = PC64(X(2), ToInteger(Z'00000020'), R)
        X(3) = PC64(X(3), ToInteger(Z'00000030'), R)
        X(4) = PC64(X(4), ToInteger(Z'00000040'), R)
        X(5) = PC64(X(5), ToInteger(Z'00000050'), R)
        X(6) = PC64(X(6), ToInteger(Z'00000060'), R)
        X(7) = PC64(X(7), ToInteger(Z'00000070'), R)
        T(0) = RSTT(X, 0, 1, 2, 3, 4, 5, 6, 7)
        T(1) = RSTT(X, 1, 2, 3, 4, 5, 6, 7, 0)
        T(2) = RSTT(X, 2, 3, 4, 5, 6, 7, 0, 1)
        T(3) = RSTT(X, 3, 4, 5, 6, 7, 0, 1, 2)
        T(4) = RSTT(X, 4, 5, 6, 7, 0, 1, 2, 3)
        T(5) = RSTT(X, 5, 6, 7, 0, 1, 2, 3, 4)
        T(6) = RSTT(X, 6, 7, 0, 1, 2, 3, 4, 5)
        T(7) = RSTT(X, 7, 0, 1, 2, 3, 4, 5, 6)
        RP1 = R + 1
        T(0) = PC64(T(0), ToInteger(Z'00000000'), RP1)
        T(1) = PC64(T(1), ToInteger(Z'00000010'), RP1)
        T(2) = PC64(T(2), ToInteger(Z'00000020'), RP1)
        T(3) = PC64(T(3), ToInteger(Z'00000030'), RP1)
        T(4) = PC64(T(4), ToInteger(Z'00000040'), RP1)
        T(5) = PC64(T(5), ToInteger(Z'00000050'), RP1)
        T(6) = PC64(T(6), ToInteger(Z'00000060'), RP1)
        T(7) = PC64(T(7), ToInteger(Z'00000070'), RP1)
        X(0) = RSTT(T, 0, 1, 2, 3, 4, 5, 6, 7)
        X(1) = RSTT(T, 1, 2, 3, 4, 5, 6, 7, 0)
        X(2) = RSTT(T, 2, 3, 4, 5, 6, 7, 0, 1)
        X(3) = RSTT(T, 3, 4, 5, 6, 7, 0, 1, 2)
        X(4) = RSTT(T, 4, 5, 6, 7, 0, 1, 2, 3)
        X(5) = RSTT(T, 5, 6, 7, 0, 1, 2, 3, 4)
        X(6) = RSTT(T, 6, 7, 0, 1, 2, 3, 4, 5)
        X(7) = RSTT(T, 7, 0, 1, 2, 3, 4, 5, 6)
    END DO

    RETURN

END SUBROUTINE DoPermP

!******************************************************************************

SUBROUTINE GroestlS_DoPadding(MD, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the final padding and store the result in the
    !  provided buffer.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlS), INTENT(INOUT)  :: MD           !! 'GroestlS' object
    tByte,           INTENT(INOUT)  :: BytesOut(0:) !! the output buffer
    tIndex,          INTENT(IN)     :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! use the 'AddBitsNPad' method with no partial last byte
    CALL MD%AddBitsNPad(0_kByte, 0_kByte, BytesOut, Offset)

    RETURN

END SUBROUTINE GroestlS_DoPadding

!******************************************************************************

SUBROUTINE GroestlS_AddBitsNPad(MD, LastByte, NBits, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then perform the final padding and
    !  store the result in the provided buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlS), INTENT(INOUT)  :: MD           !! 'GroestlS' object
    tByte,           INTENT(IN)     :: LastByte     !! the last byte
    tByte,           INTENT(IN)     :: NBits        !! number of bits in the last byte
    tByte,           INTENT(INOUT)  :: BytesOut(0:) !! the output buffer
    tIndex,          INTENT(IN)     :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: G(0:7)
    tIndex      :: I, Ptr, DLen
    tLong       :: Count
    tByte       :: Z

! FLOW

    ASSOCIATE(TmpBuf => MD%BufArr)
        Ptr = MD%GetBufLen()
        Z = SHIFTR(FByte80, NBits)
        TmpBuf(Ptr) = IAND(IOR(IAND(LastByte, -Z), Z), FByteFF)
        Ptr = Ptr + 1_kIndex
        Count = MD%GetBlockCount()
        IF (Ptr <= 56_kIndex) THEN
            TmpBuf(Ptr:55) = FByte00
            Count = Count + 1_kLong
        ELSE
            TmpBuf(Ptr:63) = FByte00
            CALL MD%ProcessBlock(TmpBuf)
            TmpBuf(0:55) = FByte00
            Count = Count + 2_kLong
        END IF
        CALL ByteUnpackBE(Count, TmpBuf, 56_kIndex)
        CALL MD%ProcessBlock(TmpBuf)
        G = MD%H
        CALL DoPermP(G)
        DO I = 0_kIndex, 3_kIndex
            CALL ByteUnpackBE(IEOR(MD%H(I+4_kIndex), G(I+4_kIndex)), TmpBuf, I*8_kIndex)
        END DO
        DLen = MD%GetDigestLen()
        BytesOut(Offset:Offset+DLen-1) = TmpBuf(32-DLen:31)
    END ASSOCIATE
        
    RETURN

END SUBROUTINE GroestlS_AddBitsNPad

!******************************************************************************

END MODULE Class_GroestlS
    
!******************************************************************************
