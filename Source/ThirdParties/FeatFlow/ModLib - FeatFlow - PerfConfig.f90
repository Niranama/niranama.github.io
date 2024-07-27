!!##############################################################################
!!# ****************************************************************************
!!# <name> perfconfig </name>
!!# ****************************************************************************
!!#
!!# <purpose>
!!#   This module provides basic structures for performance configurations
!!#
!!# The following routines can be found in this module:
!!#
!!# 1.) pcfg_initFromParameterList
!!#     -> Initialises a performance configuration by the values of a
!!#        given parameter list
!!#
!!# 2.) pcfg_readPerfConfig
!!#     -> Reads a performance configuration from file
!!#
!!# 3.) pcfg_writePerfConfig
!!#     -> Writes a performance configuration to file
!!# </purpose>
!!##############################################################################

MODULE ModLib_FeatFlow_PerfConfig

!$ use omp_lib
  USE ModLib_FeatFlow_System
  USE ModLib_FeatFlow_ParamList

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: t_perfconfig
  PUBLIC :: pcfg_initPerfConfig
  PUBLIC :: pcfg_initFromParameterList
  PUBLIC :: pcfg_readPerfConfig
  PUBLIC :: pcfg_writePerfConfig

!<types>

!<typeblock>

  ! Global performance configuration
  TYPE t_perfconfig

    ! Number of equations to be handled simultaneously
    INTEGER :: NEQSIM    = 32

    ! Number of matrix entries to be handled simultaneously
    INTEGER :: NASIM     = 32

    ! Number of edges to be handled simultaneously
    INTEGER :: NEDGESIM  = 32

    ! Number of elements to be handled simultaneously
    INTEGER :: NELEMSIM  = 128

    ! Number of patches to be handled simultaneously
    INTEGER :: NPATCHSIM = 100

    ! Number of items to be handles simultaneously
    INTEGER :: NITEMSIM  = 256

    ! OpenMP-Extension: the following settings are lower bounds which
    ! must be satisfied before OpenMP-parallelisation is activated

    ! Minimal number of equations
    !$ integer :: NEQMIN_OMP    = 1000

    ! Minimal number of matrix entries
    !$ integer :: NAMIN_OMP     = 1000

    ! Minimal number of edges
    !$ integer :: NEDGEMIN_OMP  = 1000

    ! Minimal number of elements
    !$ integer :: NELEMMIN_OMP  = 1000

    ! Minimal number of patches
    !$ integer :: NPATCHMIN_OMP = 1000

    ! Minimal number of items
    !$ integer :: NITEMMIN_OMP  = 1000

  END TYPE

!</typeblock>

!</types>

CONTAINS

  !****************************************************************************

!<subroutine>

  SUBROUTINE pcfg_initPerfConfig(rperfconfig)

!<description>
    ! This routine initialises the global performance configuration
!</description>

!<output>
    ! Performance configuration to be initialised
    TYPE(t_perfconfig), INTENT(out) :: rperfconfig
!</output>
!</subroutine>

    rperfconfig%NEQSIM    = 32
    rperfconfig%NASIM     = 32
    rperfconfig%NEDGESIM  = 32
    rperfconfig%NELEMSIM  = 128
    rperfconfig%NPATCHSIM = 100
    rperfconfig%NITEMSIM  = 256

    ! OpenMP-Extension
    !$ rperfconfig%NEQMIN_OMP    = 1000
    !$ rperfconfig%NAMIN_OMP     = 1000
    !$ rperfconfig%NEDGEMIN_OMP  = 1000
    !$ rperfconfig%NELEMMIN_OMP  = 1000
    !$ rperfconfig%NPATCHMIN_OMP = 1000
    !$ rperfconfig%NITEMMIN_OMP  = 1000

  END SUBROUTINE pcfg_initPerfConfig

  !****************************************************************************

!<subroutine>

  SUBROUTINE pcfg_initFromParameterlist(rparlist, ssectionName, rperfconfig)

!<description>
    ! This subroutine initialises a performance configuration by the
    ! values from a given parameter list
!</description>

!<input>
    ! parameter list
    TYPE(t_parlist), INTENT(in)    :: rparlist

    ! Section name of the parameter list
    CHARACTER(LEN=*), INTENT(in)   :: ssectionName
!</input>

!<output>
  ! Performance configuration to be initialised
    TYPE(t_perfconfig), INTENT(out) :: rperfconfig
!</output>
!</subroutine>

    ! Initialise xxxSIM settings
    CALL parlst_getvalue_int(rparlist, ssectionName,&
        "NEQSIM", rperfconfig%NEQSIM, rperfconfig%NEQSIM)
    CALL parlst_getvalue_int(rparlist, ssectionName,&
        "NASIM", rperfconfig%NASIM, rperfconfig%NASIM)
    CALL parlst_getvalue_int(rparlist, ssectionName,&
        "NEDGESIM", rperfconfig%NEDGESIM, rperfconfig%NEDGESIM)
    CALL parlst_getvalue_int(rparlist, ssectionName,&
        "NELEMSIM", rperfconfig%NELEMSIM, rperfconfig%NELEMSIM)
    CALL parlst_getvalue_int(rparlist, ssectionName,&
        "NPATCHSIM", rperfconfig%NPATCHSIM, rperfconfig%NPATCHSIM)
    CALL parlst_getvalue_int(rparlist, ssectionName,&
        "NITEMSIM", rperfconfig%NITEMSIM, rperfconfig%NITEMSIM)

    ! OpenMP-Extension: xxxMIN_OMP
    !$ call parlst_getvalue_int(rparlist, ssectionName,&
    !$    "NEQMIN_OMP", rperfconfig%NEQMIN_OMP, rperfconfig%NEQMIN_OMP)
    !$ call parlst_getvalue_int(rparlist, ssectionName,&
    !$    "NAMIN_OMP", rperfconfig%NAMIN_OMP, rperfconfig%NAMIN_OMP)
    !$ call parlst_getvalue_int(rparlist, ssectionName,&
    !$    "NEDGEMIN_OMP", rperfconfig%NEDGEMIN_OMP, rperfconfig%NEDGEMIN_OMP)
    !$ call parlst_getvalue_int(rparlist, ssectionName,&
    !$    "NELEMMIN_OMP", rperfconfig%NELEMMIN_OMP, rperfconfig%NELEMMIN_OMP)
    !$ call parlst_getvalue_int(rparlist, ssectionName,&
    !$    "NPATCHMIN_OMP", rperfconfig%NPATCHMIN_OMP, rperfconfig%NPATCHMIN_OMP)
    !$ call parlst_getvalue_int(rparlist, ssectionName,&
    !$    "NITEMMIN_OMP", rperfconfig%NITEMMIN_OMP, rperfconfig%NITEMMIN_OMP)

  END SUBROUTINE pcfg_initFromParameterlist

  !****************************************************************************

!<subroutine>

  SUBROUTINE pcfg_readPerfConfig(sfilename, ssectionName, rperfconfig)

!<description>
  ! This routine reads a performance configuration from an INI-file
!</description>

!<input>
    ! File name from which the configuration is read
    CHARACTER(LEN=*), INTENT(in) :: sfilename

    ! Section name of the INI-file
    CHARACTER(LEN=*), INTENT(in)   :: ssectionName
!</input>

!<output>
    ! Performance configuration to be initialised
    TYPE(t_perfconfig), INTENT(out) :: rperfconfig
!</output>
!</subroutine>

    ! local variables
    TYPE(t_parlist) :: rparlist

    ! Read parameterlist from file
    CALL parlst_init(rparlist)
    CALL parlst_readfromfile(rparlist, sfilename)

    ! Initialise performance configuration
    CALL pcfg_initFromParameterList(rparlist, ssectionName, rperfconfig)

    ! Release parameter list
    CALL parlst_done(rparlist)

  END SUBROUTINE pcfg_readPerfConfig

  !****************************************************************************

!<subroutine>

  SUBROUTINE pcfg_writePerfConfig(rperfconfig, sfilename, ssectionName, cflag)

!<description>
  ! This routine writes a performance configuration to an INI-file
!</description>

!<input>
    ! Performance configuration to be initialised
    TYPE(t_perfconfig), INTENT(in) :: rperfconfig

    ! File name from which the configuration is read
    CHARACTER(LEN=*), INTENT(in) :: sfilename

    ! Section name of the INI-file
    CHARACTER(LEN=*), INTENT(in)   :: ssectionName

    ! mode: SYS_APPEND or SYS_REPLACE
    INTEGER, INTENT(in) :: cflag
!</input>
!</subroutine>

    ! local variables
    TYPE(t_parlist) :: rparlist

    ! Initialise parameter list and add section
    CALL parlst_init(rparlist)
    CALL parlst_addsection(rparlist, ssectionname)

    ! Add xxxSIM constants
    CALL parlst_addvalue(rparlist, ssectionname,&
                         "NEQSIM", sys_siL(rperfconfig%NEQSIM,10))
    CALL parlst_addvalue(rparlist, ssectionname,&
                         "NASIM", sys_siL(rperfconfig%NASIM,10))
    CALL parlst_addvalue(rparlist, ssectionname,&
                         "NEDGESIM", sys_siL(rperfconfig%NEDGESIM,10))
    CALL parlst_addvalue(rparlist, ssectionname,&
                         "NELEMSIM", sys_siL(rperfconfig%NELEMSIM,10))
    CALL parlst_addvalue(rparlist, ssectionname,&
                         "NPATCHSIM", sys_siL(rperfconfig%NPATCHSIM,10))
    CALL parlst_addvalue(rparlist, ssectionname,&
                         "NITEMSIM", sys_siL(rperfconfig%NITEMSIM,10))

    ! OpenMP-Extension: xxxMIN_OMP constants
    !$ call parlst_addvalue(rparlist, ssectionname,&
    !$                      "NEQMIN_OMP", sys_siL(rperfconfig%NEQMIN_OMP,10))
    !$ call parlst_addvalue(rparlist, ssectionname,&
    !$                      "NAMIN_OMP", sys_siL(rperfconfig%NAMIN_OMP,10))
    !$ call parlst_addvalue(rparlist, ssectionname,&
    !$                      "NEDGEMIN_OMP", sys_siL(rperfconfig%NEDGEMIN_OMP,10))
    !$ call parlst_addvalue(rparlist, ssectionname,&
    !$                      "NELEMMIN_OMP", sys_siL(rperfconfig%NELEMMIN_OMP,10))
    !$ call parlst_addvalue(rparlist, ssectionname,&
    !$                      "NPATCHMIN_OMP", sys_siL(rperfconfig%NPATCHMIN_OMP,10))
    !$ call parlst_addvalue(rparlist, ssectionname,&
    !$                      "NITEMMIN_OMP", sys_siL(rperfconfig%NITEMMIN_OMP,10))

    ! Dump parameter list to file
    CALL parlst_dumptofile(rparlist, sfilename, cflag)

    ! Release parameter list
    CALL parlst_done(rparlist)

  END SUBROUTINE pcfg_writePerfConfig

END MODULE ModLib_FeatFlow_PerfConfig
