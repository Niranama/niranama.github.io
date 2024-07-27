
MODULE Class_JHDigest

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *JHDigest* type and its related routines.
!   The *JHDigest* type is a *digest* type that directly extends from the
!   <a href="../module/class_mdengine.html#type-mdengine">MDEngine</a>
!   type.  As a *concrete* derived type, it provides all remaining
!   deferred procedures required by all its abstract parent types. <br>
!   The *JHDigest* type implements an incremental cryptographic hash
!   function by employing the *JH message-digest* algorithm [1].  The
!   implementation here is mainly based on the references [2]. <br>
!   The *JHDigest* type represents four cryptographic hash functions:
!   the *JH-224*, *JH-256*, *JH-384*, and *JH-512* hash functions.  By
!   default, the *JHDigest* type represents the *JH-256* hash function.
!   However, a user can specify the *Security* argument (to one of the
!   four applicable values: 224, 256, 384 and 512) when initializing the
!   digest object in order to use a different hash function and get a
!   different hash output size. <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://www3.ntu.edu.sg/home/wuhj/research/jh/">Hash Function JH. </a> <br>
!   [2] <a href="https://github.com/pornin/sphlib">SPHLIB 3.0: A Set of
!       Implementations of Various Hash Functions, Both in C and in Java. </a> <br>

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_SIntUtil,           ONLY: ToDecStrSigned
    USE ModBase_BytePack,           ONLY: BytePackBE, ByteUnpackBE
    USE Class_BaseDigest
    USE Class_MDEngine

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: JHDigest

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tIndex, PARAMETER   :: BlockLen = 64_kIndex
    tIndex, PARAMETER   :: DLen224  = 28_kIndex
    tIndex, PARAMETER   :: DLen256  = 32_kIndex
    tIndex, PARAMETER   :: DLen384  = 48_kIndex
    tIndex, PARAMETER   :: DLen512  = 64_kIndex
    tLong,  PARAMETER   :: IV224(0:15) = [                            &
            ToLong(Z'2DFEDD62F99A98AC'), ToLong(Z'AE7CACD619D634E7'), &
            ToLong(Z'A4831005BC301216'), ToLong(Z'B86038C6C9661494'), &
            ToLong(Z'66D9899F2580706F'), ToLong(Z'CE9EA31B1D9B1ADC'), &
            ToLong(Z'11E8325F7B366E10'), ToLong(Z'F994857F02FA06C1'), &
            ToLong(Z'1B4F1B5CD8C840B3'), ToLong(Z'97F6A17F6E738099'), &
            ToLong(Z'DCDF93A5ADEAA3D3'), ToLong(Z'A431E8DEC9539A68'), &
            ToLong(Z'22B4A98AEC86A1E4'), ToLong(Z'D574AC959CE56CF0'), &
            ToLong(Z'15960DEAB5AB2BBF'), ToLong(Z'9611DCF0DD64EA6E')]
    tLong,  PARAMETER   :: IV256(0:15) = [                            &
            ToLong(Z'EB98A3412C20D3EB'), ToLong(Z'92CDBE7B9CB245C1'), &
            ToLong(Z'1C93519160D4C7FA'), ToLong(Z'260082D67E508A03'), &
            ToLong(Z'A4239E267726B945'), ToLong(Z'E0FB1A48D41A9477'), &
            ToLong(Z'CDB5AB26026B177A'), ToLong(Z'56F024420FFF2FA8'), &
            ToLong(Z'71A396897F2E4D75'), ToLong(Z'1D144908F77DE262'), &
            ToLong(Z'277695F776248F94'), ToLong(Z'87D5B6574780296C'), &
            ToLong(Z'5C5E272DAC8E0D6C'), ToLong(Z'518450C657057A0F'), &
            ToLong(Z'7BE4D367702412EA'), ToLong(Z'89E3AB13D31CD769')]
    tLong,  PARAMETER   :: IV384(0:15) = [                            &
            ToLong(Z'481E3BC6D813398A'), ToLong(Z'6D3B5E894ADE879B'), &
            ToLong(Z'63FAEA68D480AD2E'), ToLong(Z'332CCB21480F8267'), &
            ToLong(Z'98AEC84D9082B928'), ToLong(Z'D455EA3041114249'), &
            ToLong(Z'36F555B2924847EC'), ToLong(Z'C7250A93BAF43CE1'), &
            ToLong(Z'569B7F8A27DB454C'), ToLong(Z'9EFCBD496397AF0E'), &
            ToLong(Z'589FC27D26AA80CD'), ToLong(Z'80C08B8C9DEB2EDA'), &
            ToLong(Z'8A7981E8F8D5373A'), ToLong(Z'F43967ADDDD17A71'), &
            ToLong(Z'A9B4D3BDA475D394'), ToLong(Z'976C3FBA9842737F')]
    tLong,  PARAMETER   :: IV512(0:15) = [                            &
            ToLong(Z'6FD14B963E00AA17'), ToLong(Z'636A2E057A15D543'), &
            ToLong(Z'8A225E8D0C97EF0B'), ToLong(Z'E9341259F2B3C361'), &
            ToLong(Z'891DA0C1536F801E'), ToLong(Z'2AA9056BEA2B6D80'), &
            ToLong(Z'588ECCDB2075BAA6'), ToLong(Z'A90F3A76BAF83BF7'), &
            ToLong(Z'0169E60541E34A69'), ToLong(Z'46B58A8E2E6FE65A'), &
            ToLong(Z'1047A7D0C1843C24'), ToLong(Z'3B6E71B12D5AC199'), &
            ToLong(Z'CF57F6EC9DB1F856'), ToLong(Z'A706887C5716B156'), &
            ToLong(Z'E3C2FCDFE68517FB'), ToLong(Z'545A4678CC8CDD4B')]
    tLong,  PARAMETER   :: CParam(0:167) = [                          &
            ToLong(Z'72D5DEA2DF15F867'), ToLong(Z'7B84150AB7231557'), &
            ToLong(Z'81ABD6904D5A87F6'), ToLong(Z'4E9F4FC5C3D12B40'), &
            ToLong(Z'EA983AE05C45FA9C'), ToLong(Z'03C5D29966B2999A'), &
            ToLong(Z'660296B4F2BB538A'), ToLong(Z'B556141A88DBA231'), &
            ToLong(Z'03A35A5C9A190EDB'), ToLong(Z'403FB20A87C14410'), &
            ToLong(Z'1C051980849E951D'), ToLong(Z'6F33EBAD5EE7CDDC'), &
            ToLong(Z'10BA139202BF6B41'), ToLong(Z'DC786515F7BB27D0'), &
            ToLong(Z'0A2C813937AA7850'), ToLong(Z'3F1ABFD2410091D3'), &
            ToLong(Z'422D5A0DF6CC7E90'), ToLong(Z'DD629F9C92C097CE'), &
            ToLong(Z'185CA70BC72B44AC'), ToLong(Z'D1DF65D663C6FC23'), &
            ToLong(Z'976E6C039EE0B81A'), ToLong(Z'2105457E446CECA8'), &
            ToLong(Z'EEF103BB5D8E61FA'), ToLong(Z'FD9697B294838197'), &
            ToLong(Z'4A8E8537DB03302F'), ToLong(Z'2A678D2DFB9F6A95'), &
            ToLong(Z'8AFE7381F8B8696C'), ToLong(Z'8AC77246C07F4214'), &
            ToLong(Z'C5F4158FBDC75EC4'), ToLong(Z'75446FA78F11BB80'), &
            ToLong(Z'52DE75B7AEE488BC'), ToLong(Z'82B8001E98A6A3F4'), &
            ToLong(Z'8EF48F33A9A36315'), ToLong(Z'AA5F5624D5B7F989'), &
            ToLong(Z'B6F1ED207C5AE0FD'), ToLong(Z'36CAE95A06422C36'), &
            ToLong(Z'CE2935434EFE983D'), ToLong(Z'533AF974739A4BA7'), &
            ToLong(Z'D0F51F596F4E8186'), ToLong(Z'0E9DAD81AFD85A9F'), &
            ToLong(Z'A7050667EE34626A'), ToLong(Z'8B0B28BE6EB91727'), &
            ToLong(Z'47740726C680103F'), ToLong(Z'E0A07E6FC67E487B'), &
            ToLong(Z'0D550AA54AF8A4C0'), ToLong(Z'91E3E79F978EF19E'), &
            ToLong(Z'8676728150608DD4'), ToLong(Z'7E9E5A41F3E5B062'), &
            ToLong(Z'FC9F1FEC4054207A'), ToLong(Z'E3E41A00CEF4C984'), &
            ToLong(Z'4FD794F59DFA95D8'), ToLong(Z'552E7E1124C354A5'), &
            ToLong(Z'5BDF7228BDFE6E28'), ToLong(Z'78F57FE20FA5C4B2'), &
            ToLong(Z'05897CEFEE49D32E'), ToLong(Z'447E9385EB28597F'), &
            ToLong(Z'705F6937B324314A'), ToLong(Z'5E8628F11DD6E465'), &
            ToLong(Z'C71B770451B920E7'), ToLong(Z'74FE43E823D4878A'), &
            ToLong(Z'7D29E8A3927694F2'), ToLong(Z'DDCB7A099B30D9C1'), &
            ToLong(Z'1D1B30FB5BDC1BE0'), ToLong(Z'DA24494FF29C82BF'), &
            ToLong(Z'A4E7BA31B470BFFF'), ToLong(Z'0D324405DEF8BC48'), &
            ToLong(Z'3BAEFC3253BBD339'), ToLong(Z'459FC3C1E0298BA0'), &
            ToLong(Z'E5C905FDF7AE090F'), ToLong(Z'947034124290F134'), &
            ToLong(Z'A271B701E344ED95'), ToLong(Z'E93B8E364F2F984A'), &
            ToLong(Z'88401D63A06CF615'), ToLong(Z'47C1444B8752AFFF'), &
            ToLong(Z'7EBB4AF1E20AC630'), ToLong(Z'4670B6C5CC6E8CE6'), &
            ToLong(Z'A4D5A456BD4FCA00'), ToLong(Z'DA9D844BC83E18AE'), &
            ToLong(Z'7357CE453064D1AD'), ToLong(Z'E8A6CE68145C2567'), &
            ToLong(Z'A3DA8CF2CB0EE116'), ToLong(Z'33E906589A94999A'), &
            ToLong(Z'1F60B220C26F847B'), ToLong(Z'D1CEAC7FA0D18518'), &
            ToLong(Z'32595BA18DDD19D3'), ToLong(Z'509A1CC0AAA5B446'), &
            ToLong(Z'9F3D6367E4046BBA'), ToLong(Z'F6CA19AB0B56EE7E'), &
            ToLong(Z'1FB179EAA9282174'), ToLong(Z'E9BDF7353B3651EE'), &
            ToLong(Z'1D57AC5A7550D376'), ToLong(Z'3A46C2FEA37D7001'), &
            ToLong(Z'F735C1AF98A4D842'), ToLong(Z'78EDEC209E6B6779'), &
            ToLong(Z'41836315EA3ADBA8'), ToLong(Z'FAC33B4D32832C83'), &
            ToLong(Z'A7403B1F1C2747F3'), ToLong(Z'5940F034B72D769A'), &
            ToLong(Z'E73E4E6CD2214FFD'), ToLong(Z'B8FD8D39DC5759EF'), &
            ToLong(Z'8D9B0C492B49EBDA'), ToLong(Z'5BA2D74968F3700D'), &
            ToLong(Z'7D3BAED07A8D5584'), ToLong(Z'F5A5E9F0E4F88E65'), &
            ToLong(Z'A0B8A2F436103B53'), ToLong(Z'0CA8079E753EEC5A'), &
            ToLong(Z'9168949256E8884F'), ToLong(Z'5BB05C55F8BABC4C'), &
            ToLong(Z'E3BB3B99F387947B'), ToLong(Z'75DAF4D6726B1C5D'), &
            ToLong(Z'64AEAC28DC34B36D'), ToLong(Z'6C34A550B828DB71'), &
            ToLong(Z'F861E2F2108D512A'), ToLong(Z'E3DB643359DD75FC'), &
            ToLong(Z'1CACBCF143CE3FA2'), ToLong(Z'67BBD13C02E843B0'), &
            ToLong(Z'330A5BCA8829A175'), ToLong(Z'7F34194DB416535C'), &
            ToLong(Z'923B94C30E794D1E'), ToLong(Z'797475D7B6EEAF3F'), &
            ToLong(Z'EAA8D4F7BE1A3921'), ToLong(Z'5CF47E094C232751'), &
            ToLong(Z'26A32453BA323CD2'), ToLong(Z'44A3174A6DA6D5AD'), &
            ToLong(Z'B51D3EA6AFF2C908'), ToLong(Z'83593D98916B3C56'), &
            ToLong(Z'4CF87CA17286604D'), ToLong(Z'46E23ECC086EC7F6'), &
            ToLong(Z'2F9833B3B1BC765E'), ToLong(Z'2BD666A5EFC4E62A'), &
            ToLong(Z'06F4B6E8BEC1D436'), ToLong(Z'74EE8215BCEF2163'), &
            ToLong(Z'FDC14E0DF453C969'), ToLong(Z'A77D5AC406585826'), &
            ToLong(Z'7EC1141606E0FA16'), ToLong(Z'7E90AF3D28639D3F'), &
            ToLong(Z'D2C9F2E3009BD20C'), ToLong(Z'5FAACE30B7D40C30'), &
            ToLong(Z'742A5116F2E03298'), ToLong(Z'0DEB30D8E3CEF89A'), &
            ToLong(Z'4BC59E7BB5F17992'), ToLong(Z'FF51E66E048668D3'), &
            ToLong(Z'9B234D57E6966731'), ToLong(Z'CCE6A6F3170A7505'), &
            ToLong(Z'B17681D913326CCE'), ToLong(Z'3C175284F805A262'), &
            ToLong(Z'F42BCBB378471547'), ToLong(Z'FF46548223936A48'), &
            ToLong(Z'38DF58074E5E6565'), ToLong(Z'F2FC7C89FC86508E'), &
            ToLong(Z'31702E44D00BCA86'), ToLong(Z'F04009A23078474E'), &
            ToLong(Z'65A0EE39D1F73883'), ToLong(Z'F75EE937E42C3ABD'), &
            ToLong(Z'2197B2260113F86F'), ToLong(Z'A344EDD1EF9FDEE7'), &
            ToLong(Z'8BA0DF15762592D9'), ToLong(Z'3C85F7F612DC42BE'), &
            ToLong(Z'D8A7EC7CAB27B07E'), ToLong(Z'538D7DDAAA3EA8DE'), &
            ToLong(Z'AA25CE93BD0269D8'), ToLong(Z'5AF643FD1A7308F9'), &
            ToLong(Z'C05FEFDA174A19A5'), ToLong(Z'974D66334CFD216A'), &
            ToLong(Z'35B49831DB411570'), ToLong(Z'EA1E0FBBEDCD549B'), &
            ToLong(Z'9AD063A151974072'), ToLong(Z'F6759DBF91476FE2')]

!** DERIVED TYPE DEFINITIONS
    !> *JHDigest* is a concrete *digest* type that implements an incremental
    !  cryptographic hash function based on the JH hash functions.
    TYPE, EXTENDS(MDEngine) :: JHDigest
        PRIVATE
        !% buffer array used to store input data
        tByte       :: BufArr(0:BlockLen-1) = 0_kByte
        !% state
        tLong       :: State(0:15) = IV256(0:15)
        !% security strength in bits
        tInteger    :: Security = 256
        !% length of hash output in bytes
        tIndex      :: DigestLen = DLen256
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *InitializeWSecurity* method to
        !  initialize the *digest* object with specified security.
        PROCEDURE, PRIVATE  :: InitializeWSecurity  => JHDigest_Initialize_wSecurity
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object with default algorithm (JH-256).
        PROCEDURE       :: Initialize   => JHDigest_Initialize
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE       :: Reset        => JHDigest_Reset
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone     => JHDigest_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName      => JHDigest_GetName
        !> **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the natural hash function output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE       :: GetDigestLen => JHDigest_GetDigestLen
        !> *GetBlockLen* is a procedure to return the block length of the digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: GetBlockLen  => JHDigest_GetBlockLen
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: SetBufPtr    => JHDigest_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: ProcessBlock => JHDigest_ProcessBlock
        !> *DoPadding* is a procedure to perform padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: DoPadding    => JHDigest_DoPadding
        !> *AddBitsNPad* is a procedure to add the final partial byte and then perform
        !  padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: AddBitsNPad  => JHDigest_AddBitsNPad
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Create <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object. <br>
        !  **Usage**: <br>
        !   ! initialize the object to employ the default (JH-256) algorithm <br>
        !   --->    CALL MD%Create() <br>
        !   ! initialize the object to employ the JH-512 algorithm <br>
        !   --->    CALL MD%Create(512) <br>
        GENERIC         :: Create       => InitializeWSecurity
    END TYPE JHDigest

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE JHDigest_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with default algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(JHDigest), INTENT(INOUT)  :: MD    !! 'JHDigest' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    MD%Security  = 256
    MD%DigestLen = DLen512
    CALL MD%Reset()
   
    RETURN

END SUBROUTINE JHDigest_Initialize

!******************************************************************************

SUBROUTINE JHDigest_Initialize_wSecurity(MD, Security)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with the specified Security.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(JHDigest), INTENT(INOUT)  :: MD           !! 'JHDigest' object
    tInteger,        INTENT(IN)     :: Security
    !^ Strength of security in bits with four possible values: 224, 256, 384 and 512.
    !  If the specified value is NOT valid, it is set to the default (256) value.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT CASE (Security)
    CASE (224, 256, 384, 512)
        MD%Security = Security
    CASE DEFAULT
        MD%Security = 256
    END SELECT
    
    SELECT CASE (MD%Security)
    CASE (224)
        MD%DigestLen = DLen224
    CASE (256)
        MD%DigestLen = DLen256
    CASE (384)
        MD%DigestLen = DLen384
    CASE (512)
        MD%DigestLen = DLen512
    END SELECT
    
    CALL MD%Reset()
   
    RETURN

END SUBROUTINE JHDigest_Initialize_wSecurity

!******************************************************************************

SUBROUTINE JHDigest_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the hash algorithm state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(JHDigest), INTENT(INOUT)  :: MD   !! 'JHDigest' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    MD%BufArr = 0_kByte
    SELECT CASE (MD%Security)
    CASE (224)
        MD%State = IV224
    CASE (256)
        MD%State = IV256
    CASE (384)
        MD%State = IV384
    CASE (512)
        MD%State = IV512
    END SELECT
    CALL MD%EngineReset()

    RETURN

END SUBROUTINE JHDigest_Reset

!******************************************************************************

SUBROUTINE JHDigest_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(JHDigest),                INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(JHDigest :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (JHDigest)
        CALL Dst%Create(Src%Security)
        Dst%State  = Src%State
        Dst%BufArr = Src%BufArr
        CALL Src%EngineClone(Dst)
    END SELECT
        
    RETURN

END SUBROUTINE JHDigest_GetClone

!******************************************************************************

FUNCTION JHDigest_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(JHDigest), INTENT(IN) :: MD       !! 'JHDigest' object
    tCharAlloc                  :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'JH-' // ToDecStrSigned(MD%Security)

    RETURN

END FUNCTION JHDigest_GetName

!******************************************************************************

FUNCTION JHDigest_GetDigestLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the natural hash function output length (in bytes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(JHDigest), INTENT(IN) :: MD       !! 'JHDigest' object
    tIndex                      :: Length   !! the digest length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Length = MD%DigestLen

    RETURN

END FUNCTION JHDigest_GetDigestLen

!******************************************************************************

FUNCTION JHDigest_GetBlockLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the block length for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(JHDigest), INTENT(IN) :: MD       !! 'JHDigest' object
    tIndex                      :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Length = BlockLen
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION JHDigest_GetBlockLen

!******************************************************************************

SUBROUTINE JHDigest_SetBufPtr(MD, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(JHDigest), TARGET, INTENT(INOUT)  :: MD           !! 'JHDigest' object
    tByte,          POINTER, INTENT(INOUT)  :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => MD%BufArr

    RETURN

END SUBROUTINE JHDigest_SetBufPtr

!******************************************************************************

SUBROUTINE JHDigest_ProcessBlock(MD, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(JHDigest), INTENT(INOUT)  :: MD           !! 'JHDigest' object
    tByte,           INTENT(IN)     :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: MS(0:7)
    tIndex      :: I
    tInteger    :: Round

! FLOW

    ! input block
    CALL BytePackBE(BytesIn, 0_kIndex, MS)
    
    ! initialize state values
    DO I = 0, 7
        MD%State(I) = IEOR(MD%State(I), MS(I))
    END DO
    
    ! process data
    Round = 0
    DO WHILE (Round < 42)
        CALL DoS(MD%State, Round)
        CALL DoL(MD%State)
        CALL DoWgen(MD%State, ToLong(Z'5555555555555555'),  1)
        CALL DoS(MD%State, Round + 1)
        CALL DoL(MD%State)
        CALL DoWgen(MD%State, ToLong(Z'3333333333333333'),  2)
        CALL DoS(MD%State, Round + 2)
        CALL DoL(MD%State)
        CALL DoWgen(MD%State, ToLong(Z'0F0F0F0F0F0F0F0F'),  4)
        CALL DoS(MD%State, Round + 3)
        CALL DoL(MD%State)
        CALL DoWgen(MD%State, ToLong(Z'00FF00FF00FF00FF'),  8)
        CALL DoS(MD%State, Round + 4)
        CALL DoL(MD%State)
        CALL DoWgen(MD%State, ToLong(Z'0000FFFF0000FFFF'), 16)
        CALL DoS(MD%State, Round + 5)
        CALL DoL(MD%State)
        CALL DoWgen(MD%State, ToLong(Z'00000000FFFFFFFF'), 32)
        CALL DoS(MD%State, Round + 6)
        CALL DoL(MD%State)
        CALL DoW6(MD%State)
        Round = Round + 7
    END DO
    
    ! return state values
    DO I = 0, 7
        MD%State(I+8) = IEOR(MD%State(I+8), MS(I))
    END DO

    RETURN

CONTAINS

    SUBROUTINE DoS(H, R)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong,    INTENT(INOUT) :: H(0:15)
        tInteger, INTENT(IN)    :: R

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: X0, X1, X2, X3, CC, Tmp
        tInteger    :: RShiftL, I

    ! FLOW
    
        RShiftL = SHIFTL(R, 2)

        DO I = 0, 3
            CC  = CPaRam(RShiftL + I)
            X0  = H(I)
            X1  = H(I+4)
            X2  = H(I+8)
            X3  = H(I+12)
            X3  = NOT(X3)
            X0  = IEOR(X0, IAND(CC, NOT(X2)))
            Tmp = IEOR(CC, IAND(X0, X1))
            X0  = IEOR(X0, IAND(X2, X3))
            X3  = IEOR(X3, IAND(NOT(X1), X2))
            X1  = IEOR(X1, IAND(X0, X2))
            X2  = IEOR(X2, IAND(X0, NOT(X3)))
            X0  = IEOR(X0, IOR(X1, X3))
            X3  = IEOR(X3, IAND(X1, X2))
            X1  = IEOR(X1, IAND(Tmp, X0))
            X2  = IEOR(X2, Tmp)
            H(I)    = X0
            H(I+4)  = X1
            H(I+8)  = X2
            H(I+12) = X3
        END DO

        RETURN

    END SUBROUTINE DoS

    !**************************************************************************

    SUBROUTINE DoL(H)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(INOUT)    :: H(0:15)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: X0, X1, X2, X3, X4, X5, X6, X7

    ! FLOW
    
        X0 = H( 0)
        X1 = H( 4)
        X2 = H( 8)
        X3 = H(12)
        X4 = H( 2)
        X5 = H( 6)
        X6 = H(10)
        X7 = H(14)
        X4 = IEOR(X4, X1)
        X5 = IEOR(X5, X2)
        X6 = IEOR(X6, IEOR(X3, X0))
        X7 = IEOR(X7, X0)
        X0 = IEOR(X0, X5)
        X1 = IEOR(X1, X6)
        X2 = IEOR(X2, IEOR(X7, X4))
        X3 = IEOR(X3, X4)
        H( 0) = X0
        H( 4) = X1
        H( 8) = X2
        H(12) = X3
        H( 2) = X4
        H( 6) = X5
        H(10) = X6
        H(14) = X7

        X0 = H( 1)
        X1 = H( 5)
        X2 = H( 9)
        X3 = H(13)
        X4 = H( 3)
        X5 = H( 7)
        X6 = H(11)
        X7 = H(15)
        X4 = IEOR(X4, X1)
        X5 = IEOR(X5, X2)
        X6 = IEOR(X6, IEOR(X3, X0))
        X7 = IEOR(X7, X0)
        X0 = IEOR(X0, X5)
        X1 = IEOR(X1, X6)
        X2 = IEOR(X2, IEOR(X7, X4))
        X3 = IEOR(X3, X4)
        H( 1) = X0
        H( 5) = X1
        H( 9) = X2
        H(13) = X3
        H( 3) = X4
        H( 7) = X5
        H(11) = X6
        H(15) = X7
        
        RETURN

    END SUBROUTINE DoL

    !**************************************************************************

    SUBROUTINE DoWGen(H, C, N)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong,    INTENT(INOUT) :: H(0:15)
        tLong,    INTENT(IN)    :: C
        tInteger, INTENT(IN)    :: N

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    ! FLOW
    
#define WGen(X, C, N)   IOR(SHIFTL(IAND(X, C), N), IAND(SHIFTR(X, N), C))

        H( 2) = WGen(H( 2), C, N)
        H( 3) = WGen(H( 3), C, N)
        H( 6) = WGen(H( 6), C, N)
        H( 7) = WGen(H( 7), C, N)
        H(10) = WGen(H(10), C, N)
        H(11) = WGen(H(11), C, N)
        H(14) = WGen(H(14), C, N)
        H(15) = WGen(H(15), C, N)

#undef WGen
        
        RETURN

    END SUBROUTINE DoWGen

    !**************************************************************************

    SUBROUTINE DoW6(H)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(INOUT)    :: H(0:15)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: Temp

    ! FLOW
    
        EXCHANGE(H,  2,  3)
        EXCHANGE(H,  6,  7)
        EXCHANGE(H, 10, 11)
        EXCHANGE(H, 14, 15)
        
        RETURN

    END SUBROUTINE DoW6

    !**************************************************************************

END SUBROUTINE JHDigest_ProcessBlock

!******************************************************************************

SUBROUTINE JHDigest_DoPadding(MD, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the final padding and store the result in the
    !  provided buffer.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(JHDigest), INTENT(INOUT)  :: MD           !! 'JHDigest' object
    tByte,           INTENT(INOUT)  :: BytesOut(0:) !! the output buffer
    tIndex,          INTENT(IN)     :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! use the 'AddBitsNPad' method with no partial last byte
    CALL MD%AddBitsNPad(0_kByte, 0_kByte, BytesOut, Offset)

    RETURN

END SUBROUTINE JHDigest_DoPadding

!******************************************************************************

SUBROUTINE JHDigest_AddBitsNPad(MD, LastByte, NBits, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then perform the final padding and
    !  store the result in the provided buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(JHDigest), INTENT(INOUT)  :: MD           !! 'JHDigest' object
    tByte,           INTENT(IN)     :: LastByte     !! the last byte
    tByte,           INTENT(IN)     :: NBits        !! number of bits in the last byte
    tByte,           INTENT(INOUT)  :: BytesOut(0:) !! the output buffer
    tIndex,          INTENT(IN)     :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte       :: TmpBuf(0:127)
    tIndex      :: Rem, NumZ, I, DLen
    tLong       :: BC
    tByte       :: Z

! FLOW

    Rem = MD%GetBufLen()
    BC = MD%GetBlockCount()
    IF ((Rem == 0_kIndex).AND.(NBits == 0_kByte)) THEN
        NumZ = 47_kIndex
    ELSE
        NumZ = 111_kIndex - Rem
    END IF
    Z = SHIFTR(FByte80, NBits)
    TmpBuf(0) = IAND(IOR(IAND(LastByte, -Z), Z), FByteFF)
    TmpBuf(1:NumZ) = FByte00
    CALL ByteUnpackBE(SHIFTR(BC, 55), TmpBuf, NumZ + 1_kIndex)
    CALL ByteUnpackBE(SHIFTL(BC, 9)+SHIFTL(Rem, 3)+NBits, TmpBuf, NumZ + 9_kIndex)
    CALL MD%Update(TmpBuf, 0_kIndex, NumZ + 17_kIndex)
    DO I = 0, 7
        CALL ByteUnpackBE(MD%State(I+8), TmpBuf, SHIFTL(I, 3))
    END DO
    DLen = MD%GetDigestLen()
    BytesOut(Offset:Offset+DLen-1) = TmpBuf(64-DLen:63)
        
    RETURN

END SUBROUTINE JHDigest_AddBitsNPad

!******************************************************************************

END MODULE Class_JHDigest
    
!******************************************************************************
