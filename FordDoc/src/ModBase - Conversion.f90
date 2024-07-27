
MODULE ModBase_Conversion

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains parameters and routines for converting
!   values from SI unit to IP unit (and vice versa).

!** USE STATEMENTS:
    USE ModBase_Common

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! procedures
    PUBLIC :: DegreeToRadian
    PUBLIC :: RadianToDegree
    PUBLIC :: CelciusToFahrenheit
    PUBLIC :: FahrenheitToCelcius
    PUBLIC :: PsiToKPa
    PUBLIC :: KPaToPsi
    ! parameters
    PUBLIC :: CFC, CFD, CFK, CFL, CFU, CFR

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    tReal, PARAMETER    :: CFC = 4.184_kFloat           !! Specific Heat, (kJ/kg-K)/(Btu/lb-F)
    tReal, PARAMETER    :: CFD = 16.01846337_kFloat     !! Density, (kg/m3)/(lb/ft3)
    tReal, PARAMETER    :: CFK = 1.729577206_kFloat     !! Conductivity, (W/m-K)/(Btu/hr-ft-F)
    tReal, PARAMETER    :: CFL = 0.3048_kFloat          !! Length, (m)/(ft)
    tReal, PARAMETER    :: CFU = 5.678263_kFloat        !! U-value, (W/m2-K)/(Btu/ft2-F-hr)
    tReal, PARAMETER    :: CFR = 0.1762280394_kFloat    !! R-value, (m2-K/W)/(ft2-F-hr/Btu)

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

ELEMENTAL FUNCTION DegreeToRadian(DegAng) RESULT(RadAng)

!** PURPOSE OF THIS SUBROUTINE:
    !^  To convert an angle from degree to radian

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tReal, INTENT(IN)   :: DegAng  !! angle, degrees
    tReal               :: RadAng  !! angle, radians

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    RadAng = DegAng * Pi / 180.0_kFloat

    RETURN

END FUNCTION DegreeToRadian

!******************************************************************************

ELEMENTAL FUNCTION RadianToDegree(RadAng) RESULT(DegAng)

!** PURPOSE OF THIS SUBROUTINE:
    !^  To convert an angle from radian to degree

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tReal, INTENT(IN)   :: RadAng  !! angle, radians
    tReal               :: DegAng  !! angle, degrees

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    DegAng = RadAng * 180.0_kFloat / Pi

    RETURN

END FUNCTION RadianToDegree

!******************************************************************************

ELEMENTAL FUNCTION CelciusToFahrenheit(TC) RESULT(TF)

!** PURPOSE OF THIS SUBROUTINE:
    !^  To convert temperature from deg C to deg F

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tReal, INTENT(IN)   :: TC   !! temperature in deg C
    tReal               :: TF   !! temperature in deg F

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    TF = TC*1.8_kFloat + 32.0_kFloat

    RETURN

END FUNCTION CelciusToFahrenheit

!******************************************************************************

ELEMENTAL FUNCTION FahrenheitToCelcius(TF) RESULT(TC)

!** PURPOSE OF THIS SUBROUTINE:
    !^  To convert temperature from deg F to deg C

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tReal, INTENT(IN)   :: TF   !! temperature in deg F
    tReal               :: TC   !! temperature in deg C

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    TC = (TF - 32.0_kFloat)/1.8_kFloat

    RETURN

END FUNCTION FahrenheitToCelcius

!******************************************************************************

ELEMENTAL FUNCTION PsiToKPa(P_ip) RESULT(P_si)

!** PURPOSE OF THIS SUBROUTINE:
    !^  To convert pressure from psi to kPa

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tReal, INTENT(IN)   :: P_ip     !! pressure in psi
    tReal               :: P_si     !! pressure in kPa

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    P_si = P_ip*6.8948_kFloat

    RETURN

END FUNCTION PsiToKPa

!******************************************************************************

ELEMENTAL FUNCTION KPaToPsi(P_si) RESULT(P_ip)

!** PURPOSE OF THIS SUBROUTINE:
    !^  To convert pressure from kPa to psi

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tReal, INTENT(IN)   :: P_si     !! pressure in kPa
    tReal               :: P_ip     !! pressure in psi

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    P_ip = P_si/6.8948_kFloat

    RETURN

END FUNCTION KPaToPsi

!******************************************************************************

END MODULE ModBase_Conversion

!******************************************************************************
