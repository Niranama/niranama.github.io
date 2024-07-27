!!##############################################################################
!!# ****************************************************************************
!!# <name> paramlist </name>
!!# ****************************************************************************
!!#
!!# <purpose>
!!# This module realises a dynamic parameter list in the memory. It allows
!!# to read .INI parameter from disc, parses it and saves the content into a
!!# structure. The parameters can later be accessed by the program.
!!#
!!# A typical .INI file has the following structure:
!!# <verb>
!!# -------------------------snip------------------------------
!!# # this is a data file
!!#
!!# parameter1 = data1
!!# parameter2 = data2
!!#
!!# # now a section follows
!!#
!!# [Section1]
!!# parameter3 = 'This is a string of ''multiple'' words'
!!# parameter4 = data3
!!#
!!# [section2]
!!# parameter5 = data4    # This is a comment
!!#
!!# [section3]
!!# parameterlist(4)=     # An array consisting of 4 strings
!!#   data-line1
!!#   data-line2
!!#   data-line3
!!#   data-line4
!!# -------------------------snip------------------------------
!!# </verb>
!!# the .INI file is build up by different sections.
!!# Each section starts with the section name enclosed by
!!# brackets ('[...]'), followed by a list of parameters
!!# if the form "name=parameter". There is one unnamed parameter
!!# block at the beginning of the file.
!!#
!!# There is at most one parameter per line. The parameter names
!!# are case-insensitive. Allowed characters for parameter names
!!# are: 'A'..'Z', '0'..'9', '-', '_', '(', ')'.
!!#
!!# The parameter data is always a string consisting of
!!# - one word without spaces or
!!# - multiple words, enclosed by apostrophes.
!!# Spaces in the parameter data are ignored except when they
!!# are enclosed by apostrophes.
!!# A parameter name followed by "(n)" is assumed to be an array.
!!# The following n nonempty lines define string values for all
!!# n entries in the array.
!!#
!!# Empty lines are ignored. When there is a '#' character in
!!# a line not enclosed by apostrophes, the rest of the line is
!!# ignored as a comment.
!!#
!!# The subvariables feature \\
!!# ======================== \\
!!# The parameter list also allows to specify variables as subvariables
!!# of other variables. Take a look at the following example:
!!#
!!# <verb>
!!# -------------------------snip------------------------------
!!# NLMIN=1
!!# ACOMPLEXSTRING=! %{NLMIN} ! %{imainelement} ! %{SECTION1.ilist:3}
!!# imainelement=%{SECTION1.ielement}
!!#
!!# [SECTION1]
!!# ielement=5
!!#
!!# ilist(4)=
!!#   abc
!!#   def
!!#   ghi
!!#   jkl
!!# -------------------------snip------------------------------
!!# </verb>
!!# When reading the file, the parameter "ACOMPLEXSTRING" is automatically
!!# expanded to the value "! 1 ! 5 ! ghi" by using other variables from
!!# the parameter file. The following syntax is allowed here to refer
!!# to other parameters:
!!#
!!# <verb>
!!#  %{NAME}               - A variable from the unnamed section
!!#  %{NAME:idx}           - Value number idx of variable NAME
!!#                          from the unnamed section
!!#  %{SECTION.NAME}       - Variable NAME from section SECTION
!!#  %{SECTION.NAME:idx}   - Value number idx of variable NAME
!!#                          from section SECTION
!!# </verb>
!!#
!!# The environment variable feature \\
!!# ================================ \\
!!# DAT files may refer to environment variables. Example:
!!# <verb>
!!# -------------------------snip------------------------------
!!# NLMIN=1
!!# NLMAX=$NLMAXENV
!!# -------------------------snip------------------------------
!!# </verb>
!!# In this case, it is assumed that $NLMAXENV is an environment
!!# variable of the system where the program runs. So when NLMAX
!!# is requested, the value of the environment variable $NLMAXENV
!!# is returned.
!!#
!!# The subfile feature \\
!!# =================== \\
!!# An INI file may contain references to subfiles. Subfiles must
!!# be specified at the beginning of an INI file with the following
!!# syntax:
!!# <verb>
!!# -------------------------snip------------------------------
!!# # this is a data file which imports a couple of subfiles
!!#
!!# simportdatafiles(4) =
!!#   "subfilename1.ini"
!!#   "subfilename2.ini"
!!#   "subfilename3.ini"
!!#   "subfilename4.ini"
!!#
!!# # The rest of the file overwrites the data in the subfiles.
!!#
!!# parameter1 = data1
!!# parameter2 = data2
!!#
!!# # now a section follows; if one of the child data files above
!!# # also contains that section and the parameter 'parameter3',
!!# # that data is overwritten here!
!!#
!!# [Section1]
!!# parameter3 = 'this string replaces parameter3 from the child-files'
!!# ...
!!# -------------------------snip------------------------------
!!# </verb>
!!#
!!# parlst_readfromfile will at first read all subfiles and then
!!# evaluate the data in the main file. Data in the main file will
!!# overwrite data in the subfiles. Subfiles may contain references
!!# to other subfiles. That way, a 'master.ini' file may define
!!# a couple of files which are read at first and then overwrite
!!# some parameters.
!!#
!!# The following routines can be used to maintain a parameter
!!# list:
!!#  1.) parlst_init
!!#       -> Initialises an empty parameter list
!!#
!!#  2.) parlst_readfromfile
!!#      -> Reads the content of a .INI file into a parameter list.
!!#
!!#  3.) parlst_clear
!!#      -> Cleans up a parameter list, removes all parameters.
!!#
!!#  4.) parlst_done
!!#      -> Cleans up a parameter list, releases all allocated memory
!!#         from the heap
!!#
!!#  5.) parlst_querysection
!!#      -> Determines whether or not a section exists
!!#
!!#  6.) parlst_addsection
!!#      -> Adds a new section
!!#
!!#  7.) parlst_queryvalue
!!#      -> Determines whether or not a parameter exists
!!#
!!#  8.) parlst_querysubstrings
!!#      -> Returns the number of substrings in a parameter
!!#
!!#  9.) parlst_getvalue_string
!!#      parlst_getvalue_int
!!#      parlst_getvalue_single
!!#      parlst_getvalue_double
!!#      parlst_getvalue_logical
!!#      -> Get the string/int/real value of a parameter from the parameter list
!!#
!!# 10.) parlst_addvalue
!!#     -> Adds a new parameter to the parameter list
!!#
!!# 11.) parlst_setvalue
!!#      -> Modifies the value of a parameter in the list
!!#
!!# 12.) parlst_getStringRepresentation
!!#      -> Creates a string representation of the parameter list.
!!#
!!# 13.) parlst_info
!!#      -> Print the parameter list to the terminal
!!#
!!# 14.) parlst_findvalue
!!#      -> Checks if a given value exists and returns its substring index
!!#
!!# Auxiliary routines
!!#
!!# 1.) parlst_readfromsinglefile
!!#     -> Reads a single INI file without checking for subfiles.
!!#
!!# 2.) parlst_expandEnvVariables
!!#     -> expands all environment variables in the parameter list
!!#
!!# 3.) parlst_expandSubvars
!!#     -> expands all subvariables in the parameter list
!!#
!!# </purpose>
!!##############################################################################

MODULE ModLib_FeatFlow_ParamList

!$ use omp_lib
  USE ModLib_FeatFlow_System
  USE ModLib_FeatFlow_GenOutput
  USE ModLib_FeatFlow_IO

  IMPLICIT NONE

  PRIVATE

!<constants>

  !<constantblock>

  ! Maximum length of a section name.
  INTEGER, PARAMETER, PUBLIC :: PARLST_MLSECTION = 64

  ! Maximum length of parameter names: 32 characters
  INTEGER, PARAMETER, PUBLIC :: PARLST_MLNAME = 32

  ! Default length of parameter data: 256 characters
  INTEGER, PARAMETER, PUBLIC :: PARLST_MLDATA = 256

  ! Minimum number of free parameter 'slots' per parameter section.
  ! If there are too many parameters in a parameter section, the
  ! structure is dynamically extended in terms of PARLST_NPARSPERBLOCK
  ! entries.
  INTEGER, PARAMETER, PUBLIC :: PARLST_NPARSPERBLOCK = 32

  ! Minimum number of parameter sections.
  ! If there are too many parameter sections in a parameter block, the
  ! structure is dynamically extended in terms of PARLST_NSECTIONS
  ! entries.
  INTEGER, PARAMETER, PUBLIC :: PARLST_NSECTIONS = 8

  ! Maximum length of a line in a INI file. Lines longer than this
  ! are truncated.
  INTEGER, PARAMETER, PUBLIC :: PARLST_LENLINEBUF = 1024

  ! Comment character
  CHARACTER, PARAMETER, PUBLIC :: PARLST_COMMENT = "#"

  !</constantblock>

!</constants>

!<types>

  !<typeblock>

  ! This structure realises a value associated to a parameter name.
  ! A value consists of one string or an array of strings.

  TYPE t_parlstValue

    PRIVATE

    ! Number of strings. If set to 0, the value consists of one
    ! string, to be found in svalue. If > 0, there are nsize
    ! strings to be found in p_SentryList.
    INTEGER :: nsize = 0

    ! Single string; contains the value in case nsize=0
    CHARACTER, DIMENSION(:), POINTER :: p_sentry => NULL()

    ! Array of strings in case nsize>0
    CHARACTER, DIMENSION(:,:), POINTER :: p_SentryList => NULL()

  END TYPE

  PUBLIC :: t_parlstValue

  !</typeblock>

  !<typeblock>

  ! This structure realises a parameter section. It contains an
  ! array with parameter names and an array with parameter values
  ! to these names. The arrays are dynamically allocated.

  TYPE t_parlstSection

    ! The name of the section.
    CHARACTER(LEN=PARLST_MLSECTION) :: ssectionName = ''

    ! Actual number of parameters in this section.
    INTEGER :: iparamCount = 0

    ! A list of parameter names. Each name contains PARLST_MLNAME
    ! characters.
    CHARACTER(LEN=PARLST_MLNAME), DIMENSION(:), POINTER :: p_Sparameters => NULL()

    ! A list of t_parlstValue structures corresponding to the parameters
    ! in p_Sparameters.
    TYPE(t_parlstValue), DIMENSION(:), POINTER :: p_Rvalues

  END TYPE

  PUBLIC :: t_parlstSection

  !</typeblock>

  !<typeblock>

  ! This structure realises a parameter list. Parameters can be read into
  ! it from a file. Parameters can be obtained from the structure using
  ! the query/get routines.

  TYPE t_parlist

    PRIVATE

    ! Actual number of sections in the parameter list. There is at least
    ! one section - the unnamed section. If this value is =0, the parameter
    ! list is not initialised.
    INTEGER :: isectionCount = 0

    ! A list of sections. The first section is always the unnamed section.
    TYPE(t_parlstSection), DIMENSION(:), POINTER :: p_Rsections => NULL()

  END TYPE

  PUBLIC :: t_parlist

  !</typeblock>

!</types>

  PRIVATE :: parlst_initsection, parlst_reallocsection, parlst_realloclist
  PRIVATE :: parlst_reallocSubVariables
  PRIVATE :: parlst_fetchparameter,parlst_readlinefromfile,parlst_parseline

  INTERFACE parlst_queryvalue
    MODULE PROCEDURE parlst_queryvalue_direct
    MODULE PROCEDURE parlst_queryvalue_indir
  END INTERFACE

  INTERFACE parlst_querysubstrings
    MODULE PROCEDURE parlst_querysubstrings_direct
    MODULE PROCEDURE parlst_querysubstrings_indir
  END INTERFACE

  INTERFACE parlst_addvalue
    MODULE PROCEDURE parlst_addvalue_direct
    MODULE PROCEDURE parlst_addvalue_indir
  END INTERFACE

  INTERFACE parlst_setvalue
    MODULE PROCEDURE parlst_setvalue_fetch
    MODULE PROCEDURE parlst_setvalue_indir
    MODULE PROCEDURE parlst_setvalue_direct
  END INTERFACE

  INTERFACE parlst_getvalue_string
    MODULE PROCEDURE parlst_getvalue_string_fetch
    MODULE PROCEDURE parlst_getvalue_string_indir
    MODULE PROCEDURE parlst_getvalue_string_direct
  END INTERFACE

  INTERFACE parlst_getvalue_int
    MODULE PROCEDURE parlst_getvalue_int8_fetch
    MODULE PROCEDURE parlst_getvalue_int16_fetch
    MODULE PROCEDURE parlst_getvalue_int32_fetch
    MODULE PROCEDURE parlst_getvalue_int64_fetch
    MODULE PROCEDURE parlst_getvalue_int8_indir
    MODULE PROCEDURE parlst_getvalue_int16_indir
    MODULE PROCEDURE parlst_getvalue_int32_indir
    MODULE PROCEDURE parlst_getvalue_int64_indir
    MODULE PROCEDURE parlst_getvalue_int8_direct
    MODULE PROCEDURE parlst_getvalue_int16_direct
    MODULE PROCEDURE parlst_getvalue_int32_direct
    MODULE PROCEDURE parlst_getvalue_int64_direct
  END INTERFACE

  INTERFACE parlst_getvalue_single
    MODULE PROCEDURE parlst_getvalue_single_fetch
    MODULE PROCEDURE parlst_getvalue_single_indir
    MODULE PROCEDURE parlst_getvalue_single_direct
  END INTERFACE

  INTERFACE parlst_getvalue_double
    MODULE PROCEDURE parlst_getvalue_double_fetch
    MODULE PROCEDURE parlst_getvalue_double_indir
    MODULE PROCEDURE parlst_getvalue_double_direct
  END INTERFACE parlst_getvalue_double

  INTERFACE parlst_getvalue_logical
    MODULE PROCEDURE parlst_getvalue_logical_fetch
    MODULE PROCEDURE parlst_getvalue_logical_indir
    MODULE PROCEDURE parlst_getvalue_logical_direct
  END INTERFACE

  INTERFACE parlst_findvalue
    MODULE PROCEDURE parlst_findvalue_direct
    MODULE PROCEDURE parlst_findvalue_indir
  END INTERFACE

  PUBLIC :: parlst_init
  PUBLIC :: parlst_readfromfile
  PUBLIC :: parlst_clear
  PUBLIC :: parlst_done
  PUBLIC :: parlst_querysection
  PUBLIC :: parlst_addsection
  PUBLIC :: parlst_queryvalue
  PUBLIC :: parlst_querysubstrings
  PUBLIC :: parlst_getvalue_string
  PUBLIC :: parlst_getvalue_int
  PUBLIC :: parlst_getvalue_single
  PUBLIC :: parlst_getvalue_double
  PUBLIC :: parlst_getvalue_logical
  PUBLIC :: parlst_addvalue
  PUBLIC :: parlst_setvalue
  PUBLIC :: parlst_getStringRepresentation
  PUBLIC :: parlst_info
  PUBLIC :: parlst_readfromsinglefile
  PUBLIC :: parlst_expandEnvVariables
  PUBLIC :: parlst_expandSubvars
  PUBLIC :: parlst_dumptofile
  PUBLIC :: parlst_findvalue

CONTAINS

  ! ***************************************************************************

  ! Internal subroutine: Initialise a newly created parameter section.

  SUBROUTINE parlst_initsection (rparlstSection,sname)

  TYPE(t_parlstSection), INTENT(inout) :: rparlstSection
  CHARACTER(LEN=*), INTENT(in) :: sname

  ! Simply allocate the pointers with an empty list
  ALLOCATE(rparlstSection%p_Sparameters(PARLST_NPARSPERBLOCK))
  ALLOCATE(rparlstSection%p_Rvalues(PARLST_NPARSPERBLOCK))

  ! and set the section name
  rparlstSection%ssectionName = sname

  END SUBROUTINE

  ! ***************************************************************************

  ! Internal subroutine: Reallocate a section.
  ! This increases the size of a parameter section by reallocation of the
  ! arrays.

  SUBROUTINE parlst_reallocsection (rparlstSection, inewsize)

  ! The section to reallocate.
  TYPE(t_parlstSection), INTENT(inout) :: rparlstSection

  ! The new 'size' of the section, i.e. the new number of parameters,
  ! the section should be able to handle.
  INTEGER, INTENT(in) :: inewsize

  ! local variables

  INTEGER :: sz,oldsize

  ! Pointers to new lists for replacing the old.
  CHARACTER(LEN=PARLST_MLNAME), DIMENSION(:), POINTER :: p_Sparameters
  TYPE(t_parlstValue), DIMENSION(:), POINTER :: p_Rvalues

  oldsize = SIZE(rparlstSection%p_Sparameters)
  sz = MAX(oldsize,inewsize)

  IF (SIZE(rparlstSection%p_Sparameters) .eq. sz) RETURN ! nothing to do

  ! Allocate the pointers for the new lists
  ALLOCATE(p_Sparameters(sz))
  ALLOCATE(p_Rvalues(sz))

  ! Copy the content of the old ones
  p_Sparameters(1:oldsize) = rparlstSection%p_Sparameters (1:oldsize)
  p_Rvalues(1:oldsize) = rparlstSection%p_Rvalues (1:oldsize)

  ! Throw away the old arrays, replace by the new ones
  DEALLOCATE(rparlstSection%p_Rvalues)
  DEALLOCATE(rparlstSection%p_Sparameters)

  rparlstSection%p_Sparameters => p_Sparameters
  rparlstSection%p_Rvalues => p_Rvalues

  END SUBROUTINE

  ! ***************************************************************************

  ! Internal subroutine: Reallocates a sub-parameter list
  ! This increases the size of the sub-parameters of a parameter.
  ! Old strings are copied.

  SUBROUTINE parlst_reallocSubVariables (rvalue, inewsize)

  ! THe parameter item where to reallocate sub-parameters.
  TYPE(t_parlstValue), INTENT(inout) :: rvalue

  ! The new 'length' of the sub-parameters.
  INTEGER, INTENT(in) :: inewsize

    ! local variables
    INTEGER :: i,iold
    CHARACTER, DIMENSION(:,:), POINTER :: p_Sdata

    IF (UBOUND(rvalue%p_SentryList,2) .eq. 0) RETURN ! nothing to do

    ! Old size
    iold = UBOUND(rvalue%p_SentryList,1)

    ! Allocate memory for the strings.
    ALLOCATE(p_Sdata(inewsize,UBOUND(rvalue%p_SentryList,2)))
    p_Sdata(:,:) = ' '

    ! Copy the substrings.
    DO i=1,UBOUND(rvalue%p_SentryList,2)
      p_Sdata(1:MIN(inewsize,iold),i) = rvalue%p_SentryList(1:MIN(inewsize,iold),i)
    END DO

    ! Replace the data.
    DEALLOCATE(rvalue%p_SentryList)
    rvalue%p_SentryList => p_Sdata

  END SUBROUTINE

  ! ***************************************************************************

  ! Internal subroutine: Release a section.
  ! Removes all temporary memory that is allocated by a section.

  SUBROUTINE parlst_releasesection (rparlstSection)

  ! The section to release.
  TYPE(t_parlstSection), INTENT(inout) :: rparlstSection

  ! local variables
  INTEGER :: i

  ! Loop through all values in the current section if there is
  ! an array-value. Release them.
  DO i=SIZE(rparlstSection%p_Rvalues),1,-1
    IF (ASSOCIATED(rparlstSection%p_Rvalues(i)%p_sentry)) THEN
      DEALLOCATE(rparlstSection%p_Rvalues(i)%p_sentry)
    END IF
    IF (rparlstSection%p_Rvalues(i)%nsize .gt. 0) THEN
      DEALLOCATE(rparlstSection%p_Rvalues(i)%p_SentryList)
    END IF
  END DO

  ! Remove the content of the section.
  DEALLOCATE(rparlstSection%p_Rvalues)
  DEALLOCATE(rparlstSection%p_Sparameters)
  rparlstSection%iparamCount = 0

  END SUBROUTINE

  ! ***************************************************************************

  ! Internal subroutine: Reallocate the section list
  ! This increases the size of a section list by reallocation of the
  ! arrays.

  SUBROUTINE parlst_realloclist (rparlist, inewsize)

  ! The section list to reallocate.
  TYPE(t_parlist), INTENT(inout) :: rparlist

  ! The new 'size' of the section, i.e. the new number of parameters,
  ! the section should be able to handle.
  INTEGER, INTENT(in) :: inewsize

  ! local variables

  INTEGER :: sz

  ! Pointers to new lists for replacing the old.
  TYPE(t_parlstSection), DIMENSION(:), POINTER :: p_Rsections

  ! Allocate the pointers for the new lists
  ALLOCATE(p_Rsections(inewsize))

  sz = MIN(SIZE(rparlist%p_Rsections),inewsize)

  ! Copy the content of the old ones
  p_Rsections(1:sz) = rparlist%p_Rsections (1:sz)

  ! Throw away the old arrays, replace by the new ones
  DEALLOCATE(rparlist%p_Rsections)

  rparlist%p_Rsections => p_Rsections

  END SUBROUTINE

  ! ***************************************************************************

  ! Internal subroutine: Search in a section for a parameter
  ! and return the index - or 0 if the parameter does not exist.

  SUBROUTINE parlst_fetchparameter(rsection, sname, iparamnum)

  ! The section.
  TYPE(t_parlstSection), INTENT(in) :: rsection

  ! The parameter name to look for. Must be uppercase.
  CHARACTER(LEN=*), INTENT(in) :: sname

  ! The number of the parameter in the list or 0 if it does not exist.
  INTEGER, INTENT(out) :: iparamnum

  ! local variables
  INTEGER :: i

  iparamnum = 0

  ! If the parameter list is empty, the section does not exist for sure
  IF (rsection%iparamCount .eq. 0) RETURN

  ! Loop through all sections to see if the section exists
  DO i=1,rsection%iparamCount
    IF (rsection%p_Sparameters(i) .eq. sname) THEN
      iparamnum = i
      RETURN
    END IF
  END DO

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>

  SUBROUTINE parlst_init (rparlist)

!<description>

  ! This routine initialises a parameter list. It must be applied to a
  ! parameter list structure before doing anything to it, just to initialise.

!</description>

!<inputoutput>

  ! The parameter list to initialise.
  TYPE(t_parlist), INTENT(inout) :: rparlist

!</inputoutput>

!</subroutine>

  ! Set the section-count to 1.
  rparlist%isectionCount = 1

  ! Allocate a first set of sections
  ALLOCATE(rparlist%p_Rsections(PARLST_NSECTIONS))

  ! Initialise the first section - it is the unnamed one.
  CALL parlst_initsection (rparlist%p_Rsections(1),'')

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>

  SUBROUTINE parlst_clear (rparlist)

!<description>
  ! This routine cleans up a parameter list. All parameters in rparlist are
  ! removed.
!</description>

!<inputoutput>
  ! The parameter list to clean up.
  TYPE(t_parlist), INTENT(inout) :: rparlist
!</inputoutput>

!</subroutine>

    ! Clean up = done+reinit. We make that simple here...
    CALL parlst_done (rparlist)
    CALL parlst_init (rparlist)

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>

  SUBROUTINE parlst_done (rparlist)

!<description>

  ! This routine releases a parameter list. All memory allocated by the
  ! parameter list is released.

!</description>

!<inputoutput>

  ! The parameter list to release.
  TYPE(t_parlist), INTENT(inout) :: rparlist

!</inputoutput>

!</subroutine>

  ! local variables
  INTEGER :: i

  ! Probably nothing to do
  IF (rparlist%isectionCount .eq. 0) RETURN

  ! Loop through the parameter lists and release the content
  DO i=rparlist%isectionCount,1,-1
    CALL parlst_releasesection (rparlist%p_Rsections(i))
  END DO

  ! Release all sections
  DEALLOCATE(rparlist%p_Rsections)

  ! Mark the structure as 'empty', finish
  rparlist%isectionCount = 0

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>

  SUBROUTINE parlst_querysection(rparlist, sname, p_rsection)

!<description>

  ! Searches for a section and return a pointer to it -
  ! or NULL() of the section does not exist.

!</description>

!<input>

  ! The parameter list to scan for the section.
  TYPE(t_parlist), INTENT(in) :: rparlist

  ! The section name to look for.
  CHARACTER(LEN=*), INTENT(in) :: sname

!</input>

!<output>

  ! A pointer to the section.
  TYPE(t_parlstSection), POINTER :: p_rsection

!</output>

!</subroutine>

  ! local variables
  INTEGER :: i
  CHARACTER(LEN=PARLST_MLSECTION) :: sectionname

  NULLIFY(p_rsection)

  ! If the parameter list is empty, the section does not exist for sure
  IF (rparlist%isectionCount .eq. 0) RETURN

  ! If the section name is '', return a pointer to the first section.
  IF (sname .eq. '') THEN
    p_rsection => rparlist%p_Rsections(1)
    RETURN
  END IF

  ! Create the upper-case section name
  sectionname = ADJUSTL(sname)
  CALL sys_toupper (sectionname)

  ! Loop through all sections to see if the section exists
  DO i=1,rparlist%isectionCount
    IF (rparlist%p_Rsections(i)%ssectionName .eq. sectionname) THEN
      p_rsection => rparlist%p_Rsections(i)
      RETURN
    END IF
  END DO

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>

  SUBROUTINE parlst_addsection (rparlist, sname)

!<description>

  ! Adds a section with the name sname to the list of sections in the
  ! parameter list rparlist. The name must NOT contain brackets ('[',']')
  ! in front and at the end!

!</description>

!<inputoutput>

  ! The parameter list where to add the section.
  TYPE(t_parlist), INTENT(inout) :: rparlist

!</inputoutput>

!<input>

  ! The section name to add - without brackets in front and at the end!
  CHARACTER(LEN=*), INTENT(in) :: sname

!</input>

!</subroutine>

  ! local variables
  CHARACTER(LEN=PARLST_MLSECTION) :: sectionname

  ! Cancel if the list is not initialised.
  IF (rparlist%isectionCount .eq. 0) THEN
    CALL output_line ('Parameter list not initialised!', &
            OU_CLASS_ERROR,OU_MODE_STD,'parlst_addsection')
    CALL sys_halt()
  END IF

  ! Create the upper-case section name
  sectionname = ADJUSTL(sname)
  CALL sys_toupper (sectionname)

  ! Add a new section - reallocate the section list if necessary
  IF (rparlist%isectionCount .eq. SIZE(rparlist%p_Rsections)) THEN
    CALL parlst_realloclist (rparlist, SIZE(rparlist%p_Rsections)+PARLST_NSECTIONS)
  END IF
  rparlist%isectionCount = rparlist%isectionCount + 1

  ! Initialise the new section.
  CALL parlst_initsection(rparlist%p_Rsections(rparlist%isectionCount),sectionname)

  END SUBROUTINE

  ! ***************************************************************************

!<function>

  INTEGER FUNCTION parlst_queryvalue_indir (rsection, sparameter) &
               RESULT (exists)

!<description>
  ! Checks whether a parameter sparameter exists in the section rsection.
!</description>

!<result>
  ! The index of the parameter in the section ssection or =0, if the
  ! parameter does not exist within the section.
!</result>

!<input>

  ! The section where to search for the parameter
  TYPE(t_parlstSection), INTENT(in) :: rsection

  ! The parameter name to search for.
  CHARACTER(LEN=*), INTENT(in) :: sparameter

!</input>

!</function>

  ! local variables
  CHARACTER(LEN=PARLST_MLNAME) :: paramname

  exists = 0

  IF (sparameter .eq. '') THEN
    CALL output_line ('Empty parameter name!', &
        OU_CLASS_ERROR,OU_MODE_STD,'parlst_queryvalue_indir')
    CALL sys_halt()
  END IF

  ! Create the upper-case parameter name
  paramname = ADJUSTL(sparameter)
  CALL sys_toupper (paramname)

  ! Get the parameter index into 'exists', finish.
  CALL parlst_fetchparameter(rsection, paramname, exists)

  END FUNCTION

  ! ***************************************************************************

!<function>

  INTEGER FUNCTION parlst_queryvalue_direct (rparlist, ssectionName, sparameter) &
               RESULT (exists)

!<description>
  ! Checks whether a parameter sparameter exists in the section ssectionname
  ! in the parameter list rparlist.
!</description>

!<result>
  ! The index of the parameter in the section ssectionName or =0, if the
  ! parameter does not exist within the section.
!</result>

!<input>

  ! The parameter list.
  TYPE(t_parlist), INTENT(in) :: rparlist

  ! The section name - '' identifies the unnamed section.
  CHARACTER(LEN=*), INTENT(in) :: ssectionName

  ! The parameter name to search for.
  CHARACTER(LEN=*), INTENT(in) :: sparameter

!</input>

!</function>

  ! local variables
  TYPE(t_parlstSection), POINTER :: p_rsection

  exists = 0

  ! Cancel if the list is not initialised.
  IF (rparlist%isectionCount .eq. 0) THEN
    CALL output_line ('Parameter list not initialised!', &
        OU_CLASS_ERROR,OU_MODE_STD,'parlst_queryvalue_direct')
    CALL sys_halt()
  END IF

  ! Get the section
  CALL parlst_querysection(rparlist, ssectionName, p_rsection)
  IF (.NOT. ASSOCIATED(p_rsection)) THEN
    CALL output_line ('Section not found: '//TRIM(ssectionName), &
        OU_CLASS_ERROR,OU_MODE_STD,'parlst_queryvalue_direct')
    RETURN
  END IF

  ! Search for the parameter
  exists = parlst_queryvalue_indir (p_rsection, sparameter)

  END FUNCTION

  ! ***************************************************************************

!<function>

  INTEGER FUNCTION parlst_querysubstrings_indir (rsection, sparameter) &
               RESULT (iresult)

!<description>
  ! Returns the number of substrings of a parameter.
!</description>

!<result>
  ! The number of substrings of parameter sparameter in section rsection.
!</result>

!<input>

  ! The section where to search for the parameter
  TYPE(t_parlstSection), INTENT(in) :: rsection

  ! The parameter name to search for.
  CHARACTER(LEN=*), INTENT(in) :: sparameter

!</input>

!</function>

  ! local variables
  INTEGER :: idx
  CHARACTER(LEN=PARLST_MLNAME) :: paramname

  IF (sparameter .eq. '') THEN
    CALL output_line ('Empty parameter name!', &
        OU_CLASS_ERROR,OU_MODE_STD,'parlst_querysubstrings_indir')
    CALL sys_halt()
  END IF

  ! Create the upper-case parameter name
  paramname = ADJUSTL(sparameter)
  CALL sys_toupper (paramname)

  ! Get the parameter index into 'idx', finish.
  CALL parlst_fetchparameter(rsection, paramname, idx)

  ! Return number of substrings
  IF (idx .eq. 0) THEN
    iresult = 0
  ELSE
    iresult = rsection%p_Rvalues(idx)%nsize
  END IF

  END FUNCTION

  ! ***************************************************************************

!<function>

  INTEGER FUNCTION parlst_querysubstrings_direct (rparlist, ssectionName, sparameter) &
               RESULT (iresult)

!<description>
  ! Checks whether a parameter sparameter exists in the section ssectionname
  ! in the parameter list rparlist.
!</description>

!<result>
  ! The index of the parameter in the section ssectionName or =0, if the
  ! parameter does not exist within the section.
!</result>

!<input>

  ! The parameter list.
  TYPE(t_parlist), INTENT(in) :: rparlist

  ! The section name - '' identifies the unnamed section.
  CHARACTER(LEN=*), INTENT(in) :: ssectionName

  ! The parameter name to search for.
  CHARACTER(LEN=*), INTENT(in) :: sparameter

!</input>

!</function>

  ! local variables
  INTEGER :: idx
  TYPE(t_parlstSection), POINTER :: p_rsection

  ! Cancel if the list is not initialised.
  IF (rparlist%isectionCount .eq. 0) THEN
    CALL output_line ('Parameter list not initialised!', &
        OU_CLASS_ERROR,OU_MODE_STD,'parlst_querysubstrings_direct')
    CALL sys_halt()
  END IF

  ! Get the section
  CALL parlst_querysection(rparlist, ssectionName, p_rsection)
  IF (.NOT. ASSOCIATED(p_rsection)) THEN
    CALL output_line ('Section not found: '//TRIM(ssectionName), &
        OU_CLASS_ERROR,OU_MODE_STD,'parlst_querysubstrings_direct')
    RETURN
  END IF

  ! Get the parameter index
  idx = parlst_queryvalue_indir (p_rsection, sparameter)

  ! Return number of substrings
  IF (idx .eq. 0) THEN
    iresult = 0
  ELSE
    iresult = p_rsection%p_Rvalues(idx)%nsize
  END IF

  END FUNCTION

  ! ***************************************************************************

!<subroutine>
  SUBROUTINE parlst_getvalue_string_indir (rsection, sparameter, svalue, &
                                           sdefault, isubstring, bdequote)
!<description>

  ! Returns the value of a parameter in the section ssection.
  ! If the value does not exist, sdefault is returned.
  ! If sdefault is not given, an error will be thrown.
  !
  ! If the value is an array of strings, the optional parameter isubstring>=0
  ! allows to specify the number of the substring to be returned;
  ! isubstring=0 returns the value directly
  ! behind the '=' sign in the line of the parameter, isubstring>0 returns
  ! the array-entry in the lines below the parameter.
  !
  ! When omitting isubstring, the value directly behind the '=' sign
  ! is returned.

!</description>

!<input>

  ! The section where to search for the parameter
  TYPE(t_parlstSection), INTENT(in) :: rsection

  ! The parameter name.
  CHARACTER(LEN=*), INTENT(in) :: sparameter

  ! OPTIONAL: A default value
  CHARACTER(LEN=*), INTENT(in), OPTIONAL :: sdefault

  ! OPTIONAL: The number of the substring to be returned.
  ! =0: returns the string directly behind the '=' sign in the line
  !     'name=value'.
  ! >0: returns substring isubstring.
  INTEGER, INTENT(in), OPTIONAL :: isubstring

  ! OPTIONAL: De-quote the string.
  ! This provides a save way of removing quotation marks around a string in case
  ! the parameter contains exactly one string.
  ! =false: Return the string as it is (standard)
  ! =true: Re-read the string and remove any leading and trailing quotation marks
  !   (if there are any).
  LOGICAL, INTENT(in), OPTIONAL :: bdequote
!</input>

!<output>

  ! The value of the parameter
  CHARACTER(LEN=*), INTENT(out) :: svalue

!</output>

!</subroutine>

  ! local variables
  INTEGER :: i,isub
  CHARACTER(LEN=PARLST_MLNAME) :: paramname

  IF (sparameter .eq. '') THEN
    CALL output_line ('Empty parameter name!', &
        OU_CLASS_ERROR,OU_MODE_STD,'parlst_getvalue_string_indir')
    CALL sys_halt()
  END IF

  ! Create the upper-case parameter name
  paramname = ADJUSTL(sparameter)
  CALL sys_toupper (paramname)

  ! Get the parameter index into 'exists', finish.
  CALL parlst_fetchparameter(rsection, paramname, i)

  IF (i .eq. 0) THEN
    IF (PRESENT(sdefault)) THEN
      svalue = sdefault
    ELSE
      CALL output_line ('Parameter not found: '//TRIM(paramname), &
          OU_CLASS_ERROR,OU_MODE_STD,'parlst_getvalue_string_indir')
      CALL sys_halt()
    END IF
  ELSE
    ! Depending on isubstring, return either the 'headline' or one
    ! of the substrings.
    isub = 0
    IF (PRESENT(isubstring)) isub = isubstring

    IF ((isub .le. 0) .OR. (isub .gt. rsection%p_Rvalues(i)%nsize)) THEN
      CALL sys_chararraytostring(rsection%p_Rvalues(i)%p_sentry,svalue)
    ELSE
      CALL sys_chararraytostring(rsection%p_Rvalues(i)%p_SentryList(:,isub),svalue)
    END IF
  END IF

  IF (PRESENT(bdequote)) THEN
    IF (bdequote) THEN
      CALL sys_dequote(svalue)
    END IF
  END IF

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>
  SUBROUTINE parlst_getvalue_string_fetch (rsection, iparameter, svalue,&
                                           bexists, isubstring, bdequote)
!<description>

  ! Returns the value of a parameter in the section rsection.
  ! iparameter specifies the number of the parameter in section rsection.
  ! If bexists does not appear, an error is thrown if a nonexisting
  ! parameter is accessed.
  !
  ! If bexists is given, it will be set to TRUE if the parameter number
  ! iparameter exists, otherwise it will be set to FALSE and svalue=''.
  !
  ! If the value is an array of strings, the optional parameter isubstring>=0
  ! allows to specify the number of the substring to be returned;
  ! isubstring=0 returns the value directly
  ! behind the '=' sign in the line of the parameter, isubstring>0 returns
  ! the array-entry in the lines below the parameter.
  !
  ! When omitting isubstring, the value directly behind the '=' sign
  ! is returned.

!</description>

!<input>

  ! The section where to search for the parameter
  TYPE(t_parlstSection), INTENT(in) :: rsection

  ! The number of the parameter.
  INTEGER, INTENT(in) :: iparameter

  ! OPTIONAL: The number of the substring to be returned.
  ! =0: returns the string directly behind the '=' sign in the line
  !     'name=value'.
  ! >0: returns substring isubstring.
  INTEGER, INTENT(in), OPTIONAL :: isubstring

  ! OPTIONAL: De-quote the string.
  ! This provides a save way of removing quotation marks around a string in case
  ! the parameter contains exactly one string.
  ! =false: Return the string as it is (standard)
  ! =true: Re-read the string and remove any leading and trailing quotation marks
  !   (if there are any).
  LOGICAL, INTENT(in), OPTIONAL :: bdequote
!</input>

!<output>

  ! The value of the parameter
  CHARACTER(LEN=*), INTENT(out) :: svalue

  ! OPTIONAL: Parameter existance check
  ! Is set to TRUE/FALSE, depending on whether the parameter exists.
  LOGICAL, INTENT(out), OPTIONAL :: bexists

!</output>

!</subroutine>

  INTEGER :: isub

  ! Check if iparameter is out of bounds. If yes, probably
  ! throw an error.

  IF ((iparameter .lt. 0) .OR. (iparameter .gt. rsection%iparamCount)) THEN

    IF (.NOT. PRESENT(bexists)) THEN
      CALL output_line ('Error. Parameter '//TRIM(sys_siL(iparameter,10))//&
          ' does not exist!', &
          OU_CLASS_ERROR,OU_MODE_STD,'parlst_getvalue_string_fetch')
      CALL sys_halt()
    ELSE
      svalue = ''
      bexists = .false.
      RETURN
    END IF

  END IF

  ! Get the parameter value.
  ! Depending on isubstring, return either the 'headline' or one
  ! of the substrings.
  isub = 0
  IF (PRESENT(isubstring)) isub = isubstring

  IF ((isub .le. 0) .OR. &
      (isub .gt. rsection%p_Rvalues(iparameter)%nsize)) THEN
    CALL sys_charArrayToString(rsection%p_Rvalues(iparameter)%p_sentry,svalue)
  ELSE
    CALL sys_charArrayToString(rsection%p_Rvalues(iparameter)%p_SentryList(:,isub),svalue)
  END IF

  IF (PRESENT(bexists)) bexists = .true.

  IF (PRESENT(bdequote)) THEN
    IF (bdequote) THEN
      CALL sys_dequote(svalue)
    END IF
  END IF

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>
  SUBROUTINE parlst_getvalue_string_direct (rparlist, ssectionName, &
                                            sparameter, svalue, sdefault,&
                                            isubstring,bdequote)
!<description>

  ! Returns the value of a parameter in the section ssection.
  ! If the value does not exist, sdefault is returned.
  ! If sdefault is not given, an error will be thrown.
  !
  ! If the value is an array of strings, the optional parameter isubstring>=0
  ! allows to specify the number of the substring to be returned;
  ! isubstring=0 returns the value directly
  ! behind the '=' sign in the line of the parameter, isubstring>0 returns
  ! the array-entry in the lines below the parameter.
  !
  ! When omitting isubstring, the value directly behind the '=' sign
  ! is returned.

!</description>

!<input>

  ! The parameter list.
  TYPE(t_parlist), INTENT(in) :: rparlist

  ! The section name - '' identifies the unnamed section.
  CHARACTER(LEN=*), INTENT(in) :: ssectionName

  ! The parameter name.
  CHARACTER(LEN=*), INTENT(in) :: sparameter

  ! OPTIONAL: A default value
  CHARACTER(LEN=*), INTENT(in), OPTIONAL :: sdefault

  ! OPTIONAL: The number of the substring to be returned.
  ! =0: returns the string directly behind the '=' sign in the line
  !     'name=value'.
  ! >0: returns substring isubstring.
  INTEGER, INTENT(in), OPTIONAL :: isubstring

  ! OPTIONAL: De-quote the string.
  ! This provides a save way of removing quotation marks around a string in case
  ! the parameter contains exactly one string.
  ! =false: Return the string as it is (standard)
  ! =true: Re-read the string and remove any leading and trailing quotation marks
  !   (if there are any).
  LOGICAL, INTENT(in), OPTIONAL :: bdequote

!</input>

!<output>

  ! The value of the parameter
  CHARACTER(LEN=*), INTENT(out) :: svalue

!</output>

!</subroutine>

  ! local variables
  TYPE(t_parlstSection), POINTER :: p_rsection

  ! Cancel if the list is not initialised.
  IF (rparlist%isectionCount .eq. 0) THEN
    CALL output_line ('Parameter list not initialised!', &
        OU_CLASS_ERROR,OU_MODE_STD,'parlst_getvalue_string_fetch')
    CALL sys_halt()
  END IF

  ! Get the section
  CALL parlst_querysection(rparlist, ssectionName, p_rsection)
  IF (.NOT. ASSOCIATED(p_rsection)) THEN
    IF (PRESENT(sdefault)) THEN
      svalue = sdefault
      RETURN
    ELSE
      CALL output_line ('Section not found: '//TRIM(ssectionName), &
          OU_CLASS_ERROR,OU_MODE_STD,'parlst_getvalue_string_fetch')
      CALL sys_halt()
    END IF
  END IF

  ! Get the value
  CALL parlst_getvalue_string_indir (p_rsection, sparameter, svalue, sdefault,&
                                     isubstring,bdequote)

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>
  SUBROUTINE parlst_getvalue_single_indir (rsection, sparameter, fvalue, &
                                           fdefault, iarrayindex)
!<description>

  ! Returns the value of a parameter in the section ssection.
  ! If the value does not exist, idefault is returned.
  ! If idefault is not given, an error will be thrown.
  !
  ! If the value is an array of singles, the optional parameter
  ! iarrayindex>=0 allows to specify the number of the single to be
  ! returned; iarrayindex=0 returns the value directly behind the '='
  ! sign in the line of the parameter, iarrayindex>0 returns the
  ! array-entry in the lines below the parameter.
  !
  ! When omitting iarrayindex, the value directly behind the '=' sign
  ! is returned.

!</description>

!<input>

  ! The section where to search for the parameter
  TYPE(t_parlstSection), INTENT(in) :: rsection

  ! The parameter name.
  CHARACTER(LEN=*), INTENT(in) :: sparameter

  ! OPTIONAL: A default value
  REAL(SP), INTENT(in), OPTIONAL :: fdefault

  ! OPTIONAL: The number of the arrayindex to be returned.
  ! =0: returns the integer directly behind the '=' sign in the line
  !     'name=value'.
  ! >0: returns array index  iarrayindex.
  INTEGER, INTENT(in), OPTIONAL :: iarrayindex

!</input>

!<output>

  ! The value of the parameter
  REAL(SP), INTENT(out) :: fvalue

!</output>

!</subroutine>

  ! local variables
  CHARACTER (LEN=PARLST_LENLINEBUF) :: sdefault,svalue

  ! Call the string routine, perform a conversion afterwards.
  IF (PRESENT(fdefault)) THEN
    WRITE (sdefault,'(E17.10E2)') fdefault
    CALL parlst_getvalue_string_indir (rsection, sparameter, svalue, &
                                       sdefault, iarrayindex)
  ELSE
    CALL parlst_getvalue_string_indir (rsection, sparameter, svalue, &
                                       isubstring=iarrayindex)
  END IF

  fvalue = sys_StringToSingle(svalue,'(E17.10E2)')

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>

  SUBROUTINE parlst_getvalue_single_fetch (rsection, iparameter, fvalue, &
                                           bexists, iarrayindex)

!<description>

  ! Returns the value of a parameter in the section rsection.
  ! iparameter specifies the number of the parameter in section rsection.
  !
  ! If bexists does not appear, an error is thrown if a nonexisting
  ! parameter is accessed.
  ! If bexists is given, it will be set to TRUE if the parameter number
  ! iparameter exists, otherwise it will be set to FALSE and ivalue=0.
  !
  ! If the value is an array of singles, the optional parameter
  ! iarrayindex>=0 allows to specify the number of the single to be
  ! returned; iarrayindex=0 returns the value directly behind the '='
  ! sign in the line of the parameter, iarrayindex>0 returns the
  ! array-entry in the lines below the parameter.
  !
  ! When omitting iarrayindex, the value directly behind the '=' sign
  ! is returned.

!</description>

!<input>

  ! The section where to search for the parameter
  TYPE(t_parlstSection), INTENT(in) :: rsection

  ! The number of the parameter.
  INTEGER, INTENT(in) :: iparameter

  ! OPTIONAL: The number of the arrayindex to be returned.
  ! =0: returns the integer directly behind the '=' sign in the line
  !     'name=value'.
  ! >0: returns array index  iarrayindex.
  INTEGER, INTENT(in), OPTIONAL :: iarrayindex

!</input>

!<output>

  ! The value of the parameter
  REAL(SP), INTENT(out) :: fvalue

  ! OPTIONAL: Parameter existance check
  ! Is set to TRUE/FALSE, depending on whether the parameter exists.
  LOGICAL, INTENT(out), OPTIONAL :: bexists

!</output>

!</subroutine>

  ! local variables
  CHARACTER (LEN=PARLST_LENLINEBUF) :: svalue

  svalue = '0.0E0'
  CALL parlst_getvalue_string_fetch (rsection, iparameter, svalue, &
                                     bexists, iarrayindex)

  fvalue = sys_StringToSingle(svalue,'(E17.10E2)')

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>
  SUBROUTINE parlst_getvalue_single_direct (rparlist, ssectionName, &
                                            sparameter, fvalue, fdefault,&
                                            iarrayindex)
!<description>

  ! Returns the value of a parameter in the section ssection. If the
  ! value does not exist, ddefault is returned.  If fdefault is not
  ! given, an error will be thrown.
  !
  ! If the value is an array of singles, the optional parameter
  ! iarrayindex>=0 allows to specify the number of the single to be
  ! returned; iarrayindex=0 returns the value directly behind the '='
  ! sign in the line of the parameter, iarrayindex>0 returns the
  ! array-entry in the lines below the parameter.
  !
  ! When omitting iarrayindex, the value directly behind the '=' sign
  ! is returned.
!</description>

!<input>

  ! The parameter list.
  TYPE(t_parlist), INTENT(in) :: rparlist

  ! The section name - '' identifies the unnamed section.
  CHARACTER(LEN=*), INTENT(in) :: ssectionName

  ! The parameter name.
  CHARACTER(LEN=*), INTENT(in) :: sparameter

  ! OPTIONAL: A default value
  REAL(SP), INTENT(in), OPTIONAL :: fdefault

  ! OPTIONAL: The number of the arrayindex to be returned.
  ! =0: returns the integer directly behind the '=' sign in the line
  !     'name=value'.
  ! >0: returns array index  iarrayindex.
  INTEGER, INTENT(in), OPTIONAL :: iarrayindex

!</input>

!<output>

  ! The value of the parameter
  REAL(SP), INTENT(out) :: fvalue

!</output>

!</subroutine>

  ! local variables
  CHARACTER (LEN=PARLST_LENLINEBUF) :: sdefault,svalue

  ! Call the string routine, perform a conversion afterwards.
  IF (PRESENT(fdefault)) THEN
    WRITE (sdefault,'(E17.10E2)') fdefault
    CALL parlst_getvalue_string_direct (rparlist, ssectionName, &
                                        sparameter, svalue, sdefault, &
                                        iarrayindex)
  ELSE
    CALL parlst_getvalue_string_direct (rparlist, ssectionName, &
                                        sparameter, svalue, &
                                        isubstring=iarrayindex)
  END IF

  fvalue = sys_StringToSingle(svalue,'(E17.10E2)')

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>
  SUBROUTINE parlst_getvalue_double_indir (rsection, sparameter, dvalue, &
                                           ddefault, iarrayindex)
!<description>

  ! Returns the value of a parameter in the section ssection.
  ! If the value does not exist, ddefault is returned.
  ! If ddefault is not given, an error will be thrown.
  !
  ! If the value is an array of doubles, the optional parameter
  ! iarrayindex>=0 allows to specify the number of the double to be
  ! returned; iarrayindex=0 returns the value directly behind the '='
  ! sign in the line of the parameter, iarrayindex>0 returns the
  ! array-entry in the lines below the parameter.
  !
  ! When omitting iarrayindex, the value directly behind the '=' sign
  ! is returned.

!</description>

!<input>

  ! The section where to search for the parameter
  TYPE(t_parlstSection), INTENT(in) :: rsection

  ! The parameter name.
  CHARACTER(LEN=*), INTENT(in) :: sparameter

  ! OPTIONAL: A default value
  REAL(DP), INTENT(in), OPTIONAL :: ddefault

  ! OPTIONAL: The number of the arrayindex to be returned.
  ! =0: returns the integer directly behind the '=' sign in the line
  !     'name=value'.
  ! >0: returns array index  iarrayindex.
  INTEGER, INTENT(in), OPTIONAL :: iarrayindex

!</input>

!<output>

  ! The value of the parameter
  REAL(DP), INTENT(out) :: dvalue

!</output>

!</subroutine>

  ! local variables
  CHARACTER (LEN=PARLST_LENLINEBUF) :: sdefault,svalue

  ! Call the string routine, perform a conversion afterwards.
  IF (PRESENT(ddefault)) THEN
    WRITE (sdefault,'(E27.19E3)') ddefault
    CALL parlst_getvalue_string_indir (rsection, sparameter, svalue, &
                                       sdefault, iarrayindex)
  ELSE
    CALL parlst_getvalue_string_indir (rsection, sparameter, svalue, &
                                       isubstring=iarrayindex)
  END IF

  dvalue = sys_StringToDouble(svalue,'(E27.19E3)')

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>

  SUBROUTINE parlst_getvalue_double_fetch (rsection, iparameter, dvalue, &
                                           bexists, iarrayindex)

!<description>

  ! Returns the value of a parameter in the section rsection.
  ! iparameter specifies the number of the parameter in section rsection.
  !
  ! If bexists does not appear, an error is thrown if a nonexisting
  ! parameter is accessed.
  ! If bexists is given, it will be set to TRUE if the parameter number
  ! iparameter exists, otherwise it will be set to FALSE and ivalue=0.
  !
  ! If the value is an array of doubles, the optional parameter
  ! iarrayindex>=0 allows to specify the number of the double to be
  ! returned; iarrayindex=0 returns the value directly behind the '='
  ! sign in the line of the parameter, iarrayindex>0 returns the
  ! array-entry in the lines below the parameter.
  !
  ! When omitting iarrayindex, the value directly behind the '=' sign
  ! is returned.

!</description>

!<input>

  ! The section where to search for the parameter
  TYPE(t_parlstSection), INTENT(in) :: rsection

  ! The number of the parameter.
  INTEGER, INTENT(in) :: iparameter

  ! OPTIONAL: The number of the arrayindex to be returned.
  ! =0: returns the integer directly behind the '=' sign in the line
  !     'name=value'.
  ! >0: returns array index  iarrayindex.
  INTEGER, INTENT(in), OPTIONAL :: iarrayindex

!</input>

!<output>

  ! The value of the parameter
  REAL(DP), INTENT(out) :: dvalue

  ! OPTIONAL: Parameter existance check
  ! Is set to TRUE/FALSE, depending on whether the parameter exists.
  LOGICAL, INTENT(out), OPTIONAL :: bexists

!</output>

!</subroutine>

  ! local variables
  CHARACTER (LEN=PARLST_LENLINEBUF) :: svalue

  svalue = '0.0E0'
  CALL parlst_getvalue_string_fetch (rsection, iparameter, svalue, &
                                     bexists, iarrayindex)

  dvalue = sys_StringToDouble(svalue,'(E27.19E3)')

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>
  SUBROUTINE parlst_getvalue_double_direct (rparlist, ssectionName, &
                                            sparameter, dvalue, ddefault,&
                                            iarrayindex)
!<description>

  ! Returns the value of a parameter in the section ssection.
  ! If the value does not exist, ddefault is returned.
  ! If ddefault is not given, an error will be thrown.
  !
  ! If the value is an array of doubles, the optional parameter
  ! iarrayindex>=0 allows to specify the number of the double to be
  ! returned; iarrayindex=0 returns the value directly behind the '='
  ! sign in the line of the parameter, iarrayindex>0 returns the
  ! array-entry in the lines below the parameter.
  !
  ! When omitting iarrayindex, the value directly behind the '=' sign
  ! is returned.

!</description>

!<input>

  ! The parameter list.
  TYPE(t_parlist), INTENT(in) :: rparlist

  ! The section name - '' identifies the unnamed section.
  CHARACTER(LEN=*), INTENT(in) :: ssectionName

  ! The parameter name.
  CHARACTER(LEN=*), INTENT(in) :: sparameter

  ! OPTIONAL: A default value
  REAL(DP), INTENT(in), OPTIONAL :: ddefault

  ! OPTIONAL: The number of the arrayindex to be returned.
  ! =0: returns the integer directly behind the '=' sign in the line
  !     'name=value'.
  ! >0: returns array index  iarrayindex.
  INTEGER, INTENT(in), OPTIONAL :: iarrayindex

!</input>

!<output>

  ! The value of the parameter
  REAL(DP), INTENT(out) :: dvalue

!</output>

!</subroutine>

  ! local variables
  CHARACTER (LEN=PARLST_LENLINEBUF) :: sdefault,svalue

  ! Call the string routine, perform a conversion afterwards.
  IF (PRESENT(ddefault)) THEN
    WRITE (sdefault,'(E27.19E3)') ddefault
    CALL parlst_getvalue_string_direct (rparlist, ssectionName, &
                                        sparameter, svalue, sdefault, &
                                        iarrayindex)
  ELSE
    CALL parlst_getvalue_string_direct (rparlist, ssectionName, &
                                        sparameter, svalue, &
                                        isubstring=iarrayindex)
  END IF

  dvalue = sys_StringToDouble(svalue,'(E27.19E3)')

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>
  SUBROUTINE parlst_getvalue_logical_indir (rsection, sparameter, bvalue, &
                                            bdefault, iarrayindex)
!<description>

  ! Returns the value of a parameter in the section ssection.
  ! If the value does not exist, bdefault is returned.
  ! If bdefault is not given, an error will be thrown.
  !
  ! If the value is an array of logicals, the optional parameter
  ! iarrayindex>=0 allows to specify the number of the logical to be
  ! returned; iarrayindex=0 returns the value directly behind the '='
  ! sign in the line of the parameter, iarrayindex>0 returns the
  ! array-entry in the lines below the parameter.
  !
  ! When omitting iarrayindex, the value directly behind the '=' sign
  ! is returned.

!</description>

!<input>

  ! The section where to search for the parameter
  TYPE(t_parlstSection), INTENT(in) :: rsection

  ! The parameter name.
  CHARACTER(LEN=*), INTENT(in) :: sparameter

  ! OPTIONAL: A default value
  LOGICAL, INTENT(in), OPTIONAL :: bdefault

  ! OPTIONAL: The number of the arrayindex to be returned.
  ! =0: returns the integer directly behind the '=' sign in the line
  !     'name=value'.
  ! >0: returns array index  iarrayindex.
  INTEGER, INTENT(in), OPTIONAL :: iarrayindex

!</input>

!<output>

  ! The value of the parameter
  LOGICAL, INTENT(out) :: bvalue

!</output>

!</subroutine>

  ! local variables
  CHARACTER (LEN=PARLST_LENLINEBUF) :: sdefault,svalue

  ! Call the string routine, perform a conversion afterwards.
  IF (PRESENT(bdefault)) THEN
    WRITE (sdefault,'(L1)') bdefault
    CALL parlst_getvalue_string_indir (rsection, sparameter, svalue, &
                                       sdefault, iarrayindex)
  ELSE
    CALL parlst_getvalue_string_indir (rsection, sparameter, svalue, &
                                       isubstring=iarrayindex)
  END IF

  READ (svalue,'(L1)') bvalue

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>

  SUBROUTINE parlst_getvalue_logical_fetch (rsection, iparameter, bvalue, &
                                            bexists, iarrayindex)

!<description>

  ! Returns the value of a parameter in the section rsection.
  ! iparameter specifies the number of the parameter in section rsection.
  !
  ! If bexists does not appear, an error is thrown if a nonexisting
  ! parameter is accessed.
  ! If bexists is given, it will be set to TRUE if the parameter number
  ! iparameter exists, otherwise it will be set to FALSE and ivalue=0.
  !
  ! If the value is an array of logicals, the optional parameter
  ! iarrayindex>=0 allows to specify the number of the logical to be
  ! returned; iarrayindex=0 returns the value directly behind the '='
  ! sign in the line of the parameter, iarrayindex>0 returns the
  ! array-entry in the lines below the parameter.
  !
  ! When omitting iarrayindex, the value directly behind the '=' sign
  ! is returned.

!</description>

!<input>

  ! The section where to search for the parameter
  TYPE(t_parlstSection), INTENT(in) :: rsection

  ! The number of the parameter.
  INTEGER, INTENT(in) :: iparameter

  ! OPTIONAL: The number of the arrayindex to be returned.
  ! =0: returns the integer directly behind the '=' sign in the line
  !     'name=value'.
  ! >0: returns array index  iarrayindex.
  INTEGER, INTENT(in), OPTIONAL :: iarrayindex

!</input>

!<output>

  ! The value of the parameter
  LOGICAL, INTENT(out) :: bvalue

  ! OPTIONAL: Parameter existance check
  ! Is set to TRUE/FALSE, depending on whether the parameter exists.
  LOGICAL, INTENT(out), OPTIONAL :: bexists

!</output>

!</subroutine>

  ! local variables
  CHARACTER (LEN=PARLST_LENLINEBUF) :: svalue

  WRITE (svalue,'(L1)') .false.
  CALL parlst_getvalue_string_fetch (rsection, iparameter, svalue, &
                                     bexists, iarrayindex)

  READ (svalue,'(L1)') bvalue

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>
  SUBROUTINE parlst_getvalue_logical_direct (rparlist, ssectionName, &
                                             sparameter, bvalue, bdefault,&
                                             iarrayindex)
!<description>

  ! Returns the value of a parameter in the section ssection.
  ! If the value does not exist, bdefault is returned.
  ! If bdefault is not given, an error will be thrown.
  !
  ! If the value is an array of logicals, the optional parameter
  ! iarrayindex>=0 allows to specify the number of the logical to be
  ! returned; iarrayindex=0 returns the value directly behind the '='
  ! sign in the line of the parameter, iarrayindex>0 returns the
  ! array-entry in the lines below the parameter.
  !
  ! When omitting iarrayindex, the value directly behind the '=' sign
  ! is returned.

!</description>

!<input>

  ! The parameter list.
  TYPE(t_parlist), INTENT(in) :: rparlist

  ! The section name - '' identifies the unnamed section.
  CHARACTER(LEN=*), INTENT(in) :: ssectionName

  ! The parameter name.
  CHARACTER(LEN=*), INTENT(in) :: sparameter

  ! OPTIONAL: A default value
  LOGICAL, INTENT(in), OPTIONAL :: bdefault

  ! OPTIONAL: The number of the arrayindex to be returned.
  ! =0: returns the integer directly behind the '=' sign in the line
  !     'name=value'.
  ! >0: returns array index  iarrayindex.
  INTEGER, INTENT(in), OPTIONAL :: iarrayindex

!</input>

!<output>

  ! The value of the parameter
  LOGICAL, INTENT(out) :: bvalue

!</output>

!</subroutine>

  ! local variables
  CHARACTER (LEN=PARLST_LENLINEBUF) :: sdefault,svalue

  ! Call the string routine, perform a conversion afterwards.
  IF (PRESENT(bdefault)) THEN
    WRITE (sdefault,'(L1)') bdefault
    CALL parlst_getvalue_string_direct (rparlist, ssectionName, &
                                        sparameter, svalue, sdefault, &
                                        iarrayindex)
  ELSE
    CALL parlst_getvalue_string_direct (rparlist, ssectionName, &
                                        sparameter, svalue, &
                                        isubstring=iarrayindex)
  END IF

  READ (svalue,'(L1)') bvalue

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>
  SUBROUTINE parlst_getvalue_int8_indir (rsection, sparameter, ivalue, &
                                         idefault, iarrayindex)
!<description>

  ! Returns the value of a parameter in the section ssection.
  ! If the value does not exist, idefault is returned.
  ! If idefault is not given, an error will be thrown.
  !
  ! If the value is an array of integers, the optional parameter
  ! iarrayindex>=0 allows to specify the number of the integer to be
  ! returned; iarrayindex=0 returns the value directly behind the '='
  ! sign in the line of the parameter, iarrayindex>0 returns the
  ! array-entry in the lines below the parameter.
  !
  ! When omitting iarrayindex, the value directly behind the '=' sign
  ! is returned.
!</description>

!<input>

  ! The section where to search for the parameter
  TYPE(t_parlstSection), INTENT(in) :: rsection

  ! The parameter name.
  CHARACTER(LEN=*), INTENT(in) :: sparameter

  ! OPTIONAL: A default value
  INTEGER(I8), INTENT(in), OPTIONAL :: idefault

  ! OPTIONAL: The number of the arrayindex to be returned.
  ! =0: returns the integer directly behind the '=' sign in the line
  !     'name=value'.
  ! >0: returns array index  iarrayindex.
  INTEGER, INTENT(in), OPTIONAL :: iarrayindex

!</input>

!<output>

  ! The value of the parameter
  INTEGER(I8), INTENT(out) :: ivalue

!</output>

!</subroutine>

  ! local variables
  CHARACTER (LEN=PARLST_LENLINEBUF) :: sdefault,svalue

  ! Call the string routine, perform a conversion afterwards.
  IF (PRESENT(idefault)) THEN
    WRITE (sdefault,*) idefault
    CALL parlst_getvalue_string_indir (rsection, sparameter, svalue, &
                                       sdefault, iarrayindex)
  ELSE
    CALL parlst_getvalue_string_indir (rsection, sparameter, svalue, &
                                       isubstring=iarrayindex)
  END IF

  READ(svalue,*) ivalue

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>
  SUBROUTINE parlst_getvalue_int8_fetch (rsection, iparameter, ivalue, &
                                         bexists, iarrayindex)
!<description>

  ! Returns the value of a parameter in the section rsection.
  ! iparameter specifies the number of the parameter in section rsection.
  ! If bexists does not appear, an error is thrown if a nonexisting
  ! parameter is accessed.
  !
  ! If bexists is given, it will be set to TRUE if the parameter number
  ! iparameter exists, otherwise it will be set to FALSE and ivalue=0.
  !
  ! If the value is an array of integers, the optional parameter
  ! iarrayindex>=0 allows to specify the number of the integer to be
  ! returned; iarrayindex=0 returns the value directly behind the '='
  ! sign in the line of the parameter, iarrayindex>0 returns the
  ! array-entry in the lines below the parameter.
  !
  ! When omitting iarrayindex, the value directly behind the '=' sign
  ! is returned.

!</description>

!<input>

  ! The section where to search for the parameter
  TYPE(t_parlstSection), INTENT(in) :: rsection

  ! The number of the parameter.
  INTEGER, INTENT(in) :: iparameter

  ! OPTIONAL: The number of the arrayindex to be returned.
  ! =0: returns the integer directly behind the '=' sign in the line
  !     'name=value'.
  ! >0: returns array index  iarrayindex.
  INTEGER, INTENT(in), OPTIONAL :: iarrayindex

!</input>

!<output>

  ! The value of the parameter
  INTEGER(I8), INTENT(out) :: ivalue

  ! OPTIONAL: Parameter existance check
  ! Is set to TRUE/FALSE, depending on whether the parameter exists.
  LOGICAL, INTENT(out), OPTIONAL :: bexists

!</output>

!</subroutine>

  ! local variables
  CHARACTER (LEN=PARLST_LENLINEBUF) :: svalue

  svalue = '0'
  CALL parlst_getvalue_string_fetch (rsection, iparameter, svalue, &
                                     bexists, iarrayindex)
  READ(svalue,*) ivalue

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>
  SUBROUTINE parlst_getvalue_int8_direct (rparlist, ssectionName, &
                                          sparameter, ivalue, idefault,&
                                          iarrayindex)
!<description>

  ! Returns the value of a parameter in the section ssection. If the
  ! value does not exist, idefault is returned.  If idefault is not
  ! given, an error will be thrown.
  !
  ! If the value is an array of integers, the optional parameter
  ! iarrayindex>=0 allows to specify the number of the integer to be
  ! returned; iarrayindex=0 returns the value directly behind the '='
  ! sign in the line of the parameter, iarrayindex>0 returns the
  ! array-entry in the lines below the parameter.
  !
  ! When omitting iarrayindex, the value directly behind the '=' sign
  ! is returned.

!</description>

!<input>

  ! The parameter list.
  TYPE(t_parlist), INTENT(in) :: rparlist

  ! The section name - '' identifies the unnamed section.
  CHARACTER(LEN=*), INTENT(in) :: ssectionName

  ! The parameter name.
  CHARACTER(LEN=*), INTENT(in) :: sparameter

  ! OPTIONAL: A default value
  INTEGER(I8), INTENT(in), OPTIONAL :: idefault

  ! OPTIONAL: The number of the arrayindex to be returned.
  ! =0: returns the integer directly behind the '=' sign in the line
  !     'name=value'.
  ! >0: returns array index  iarrayindex.
  INTEGER, INTENT(in), OPTIONAL :: iarrayindex

!</input>

!<output>

  ! The value of the parameter
  INTEGER(I8), INTENT(out) :: ivalue

!</output>

!</subroutine>

  ! local variables
  CHARACTER (LEN=PARLST_LENLINEBUF) :: sdefault,svalue

  ! Call the string routine, perform a conversion afterwards.
  IF (PRESENT(idefault)) THEN
    WRITE (sdefault,*) idefault
    CALL parlst_getvalue_string_direct (rparlist, ssectionName, &
                                        sparameter, svalue, sdefault, &
                                        iarrayindex)
  ELSE
    CALL parlst_getvalue_string_direct (rparlist, ssectionName, &
                                        sparameter, svalue, &
                                        isubstring=iarrayindex)
  END IF

  READ(svalue,*) ivalue

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>
  SUBROUTINE parlst_getvalue_int16_indir (rsection, sparameter, ivalue, &
                                          idefault, iarrayindex)
!<description>

  ! Returns the value of a parameter in the section ssection.
  ! If the value does not exist, idefault is returned.
  ! If idefault is not given, an error will be thrown.
  !
  ! If the value is an array of integers, the optional parameter
  ! iarrayindex>=0 allows to specify the number of the integer to be
  ! returned; iarrayindex=0 returns the value directly behind the '='
  ! sign in the line of the parameter, iarrayindex>0 returns the
  ! array-entry in the lines below the parameter.
  !
  ! When omitting iarrayindex, the value directly behind the '=' sign
  ! is returned.
!</description>

!<input>

  ! The section where to search for the parameter
  TYPE(t_parlstSection), INTENT(in) :: rsection

  ! The parameter name.
  CHARACTER(LEN=*), INTENT(in) :: sparameter

  ! OPTIONAL: A default value
  INTEGER(I16), INTENT(in), OPTIONAL :: idefault

  ! OPTIONAL: The number of the arrayindex to be returned.
  ! =0: returns the integer directly behind the '=' sign in the line
  !     'name=value'.
  ! >0: returns array index  iarrayindex.
  INTEGER, INTENT(in), OPTIONAL :: iarrayindex

!</input>

!<output>

  ! The value of the parameter
  INTEGER(I16), INTENT(out) :: ivalue

!</output>

!</subroutine>

  ! local variables
  CHARACTER (LEN=PARLST_LENLINEBUF) :: sdefault,svalue

  ! Call the string routine, perform a conversion afterwards.
  IF (PRESENT(idefault)) THEN
    WRITE (sdefault,*) idefault
    CALL parlst_getvalue_string_indir (rsection, sparameter, svalue, &
                                       sdefault, iarrayindex)
  ELSE
    CALL parlst_getvalue_string_indir (rsection, sparameter, svalue, &
                                       isubstring=iarrayindex)
  END IF

  READ(svalue,*) ivalue

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>
  SUBROUTINE parlst_getvalue_int16_fetch (rsection, iparameter, ivalue, &
                                          bexists, iarrayindex)
!<description>

  ! Returns the value of a parameter in the section rsection.
  ! iparameter specifies the number of the parameter in section rsection.
  ! If bexists does not appear, an error is thrown if a nonexisting
  ! parameter is accessed.
  !
  ! If bexists is given, it will be set to TRUE if the parameter number
  ! iparameter exists, otherwise it will be set to FALSE and ivalue=0.
  !
  ! If the value is an array of integers, the optional parameter
  ! iarrayindex>=0 allows to specify the number of the integer to be
  ! returned; iarrayindex=0 returns the value directly behind the '='
  ! sign in the line of the parameter, iarrayindex>0 returns the
  ! array-entry in the lines below the parameter.
  !
  ! When omitting iarrayindex, the value directly behind the '=' sign
  ! is returned.

!</description>

!<input>

  ! The section where to search for the parameter
  TYPE(t_parlstSection), INTENT(in) :: rsection

  ! The number of the parameter.
  INTEGER, INTENT(in) :: iparameter

  ! OPTIONAL: The number of the arrayindex to be returned.
  ! =0: returns the integer directly behind the '=' sign in the line
  !     'name=value'.
  ! >0: returns array index  iarrayindex.
  INTEGER, INTENT(in), OPTIONAL :: iarrayindex

!</input>

!<output>

  ! The value of the parameter
  INTEGER(I16), INTENT(out) :: ivalue

  ! OPTIONAL: Parameter existance check
  ! Is set to TRUE/FALSE, depending on whether the parameter exists.
  LOGICAL, INTENT(out), OPTIONAL :: bexists

!</output>

!</subroutine>

  ! local variables
  CHARACTER (LEN=PARLST_LENLINEBUF) :: svalue

  svalue = '0'
  CALL parlst_getvalue_string_fetch (rsection, iparameter, svalue, &
                                     bexists, iarrayindex)
  READ(svalue,*) ivalue

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>
  SUBROUTINE parlst_getvalue_int16_direct (rparlist, ssectionName, &
                                           sparameter, ivalue, idefault,&
                                           iarrayindex)
!<description>

  ! Returns the value of a parameter in the section ssection. If the
  ! value does not exist, idefault is returned.  If idefault is not
  ! given, an error will be thrown.
  !
  ! If the value is an array of integers, the optional parameter
  ! iarrayindex>=0 allows to specify the number of the integer to be
  ! returned; iarrayindex=0 returns the value directly behind the '='
  ! sign in the line of the parameter, iarrayindex>0 returns the
  ! array-entry in the lines below the parameter.
  !
  ! When omitting iarrayindex, the value directly behind the '=' sign
  ! is returned.

!</description>

!<input>

  ! The parameter list.
  TYPE(t_parlist), INTENT(in) :: rparlist

  ! The section name - '' identifies the unnamed section.
  CHARACTER(LEN=*), INTENT(in) :: ssectionName

  ! The parameter name.
  CHARACTER(LEN=*), INTENT(in) :: sparameter

  ! OPTIONAL: A default value
  INTEGER(I16), INTENT(in), OPTIONAL :: idefault

  ! OPTIONAL: The number of the arrayindex to be returned.
  ! =0: returns the integer directly behind the '=' sign in the line
  !     'name=value'.
  ! >0: returns array index  iarrayindex.
  INTEGER, INTENT(in), OPTIONAL :: iarrayindex

!</input>

!<output>

  ! The value of the parameter
  INTEGER(I16), INTENT(out) :: ivalue

!</output>

!</subroutine>

  ! local variables
  CHARACTER (LEN=PARLST_LENLINEBUF) :: sdefault,svalue

  ! Call the string routine, perform a conversion afterwards.
  IF (PRESENT(idefault)) THEN
    WRITE (sdefault,*) idefault
    CALL parlst_getvalue_string_direct (rparlist, ssectionName, &
                                        sparameter, svalue, sdefault, &
                                        iarrayindex)
  ELSE
    CALL parlst_getvalue_string_direct (rparlist, ssectionName, &
                                        sparameter, svalue, &
                                        isubstring=iarrayindex)
  END IF

  READ(svalue,*) ivalue

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>
  SUBROUTINE parlst_getvalue_int32_indir (rsection, sparameter, ivalue, &
                                          idefault, iarrayindex)
!<description>

  ! Returns the value of a parameter in the section ssection.
  ! If the value does not exist, idefault is returned.
  ! If idefault is not given, an error will be thrown.
  !
  ! If the value is an array of integers, the optional parameter
  ! iarrayindex>=0 allows to specify the number of the integer to be
  ! returned; iarrayindex=0 returns the value directly behind the '='
  ! sign in the line of the parameter, iarrayindex>0 returns the
  ! array-entry in the lines below the parameter.
  !
  ! When omitting iarrayindex, the value directly behind the '=' sign
  ! is returned.
!</description>

!<input>

  ! The section where to search for the parameter
  TYPE(t_parlstSection), INTENT(in) :: rsection

  ! The parameter name.
  CHARACTER(LEN=*), INTENT(in) :: sparameter

  ! OPTIONAL: A default value
  INTEGER(I32), INTENT(in), OPTIONAL :: idefault

  ! OPTIONAL: The number of the arrayindex to be returned.
  ! =0: returns the integer directly behind the '=' sign in the line
  !     'name=value'.
  ! >0: returns array index  iarrayindex.
  INTEGER, INTENT(in), OPTIONAL :: iarrayindex

!</input>

!<output>

  ! The value of the parameter
  INTEGER(I32), INTENT(out) :: ivalue

!</output>

!</subroutine>

  ! local variables
  CHARACTER (LEN=PARLST_LENLINEBUF) :: sdefault,svalue

  ! Call the string routine, perform a conversion afterwards.
  IF (PRESENT(idefault)) THEN
    WRITE (sdefault,*) idefault
    CALL parlst_getvalue_string_indir (rsection, sparameter, svalue, &
                                       sdefault, iarrayindex)
  ELSE
    CALL parlst_getvalue_string_indir (rsection, sparameter, svalue, &
                                       isubstring=iarrayindex)
  END IF

  READ(svalue,*) ivalue

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>
  SUBROUTINE parlst_getvalue_int32_fetch (rsection, iparameter, ivalue, &
                                          bexists, iarrayindex)
!<description>

  ! Returns the value of a parameter in the section rsection.
  ! iparameter specifies the number of the parameter in section rsection.
  ! If bexists does not appear, an error is thrown if a nonexisting
  ! parameter is accessed.
  !
  ! If bexists is given, it will be set to TRUE if the parameter number
  ! iparameter exists, otherwise it will be set to FALSE and ivalue=0.
  !
  ! If the value is an array of integers, the optional parameter
  ! iarrayindex>=0 allows to specify the number of the integer to be
  ! returned; iarrayindex=0 returns the value directly behind the '='
  ! sign in the line of the parameter, iarrayindex>0 returns the
  ! array-entry in the lines below the parameter.
  !
  ! When omitting iarrayindex, the value directly behind the '=' sign
  ! is returned.

!</description>

!<input>

  ! The section where to search for the parameter
  TYPE(t_parlstSection), INTENT(in) :: rsection

  ! The number of the parameter.
  INTEGER, INTENT(in) :: iparameter

  ! OPTIONAL: The number of the arrayindex to be returned.
  ! =0: returns the integer directly behind the '=' sign in the line
  !     'name=value'.
  ! >0: returns array index  iarrayindex.
  INTEGER, INTENT(in), OPTIONAL :: iarrayindex

!</input>

!<output>

  ! The value of the parameter
  INTEGER(I32), INTENT(out) :: ivalue

  ! OPTIONAL: Parameter existance check
  ! Is set to TRUE/FALSE, depending on whether the parameter exists.
  LOGICAL, INTENT(out), OPTIONAL :: bexists

!</output>

!</subroutine>

  ! local variables
  CHARACTER (LEN=PARLST_LENLINEBUF) :: svalue

  svalue = '0'
  CALL parlst_getvalue_string_fetch (rsection, iparameter, svalue, &
                                     bexists, iarrayindex)
  READ(svalue,*) ivalue

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>
  SUBROUTINE parlst_getvalue_int32_direct (rparlist, ssectionName, &
                                           sparameter, ivalue, idefault,&
                                           iarrayindex)
!<description>

  ! Returns the value of a parameter in the section ssection. If the
  ! value does not exist, idefault is returned.  If idefault is not
  ! given, an error will be thrown.
  !
  ! If the value is an array of integers, the optional parameter
  ! iarrayindex>=0 allows to specify the number of the integer to be
  ! returned; iarrayindex=0 returns the value directly behind the '='
  ! sign in the line of the parameter, iarrayindex>0 returns the
  ! array-entry in the lines below the parameter.
  !
  ! When omitting iarrayindex, the value directly behind the '=' sign
  ! is returned.

!</description>

!<input>

  ! The parameter list.
  TYPE(t_parlist), INTENT(in) :: rparlist

  ! The section name - '' identifies the unnamed section.
  CHARACTER(LEN=*), INTENT(in) :: ssectionName

  ! The parameter name.
  CHARACTER(LEN=*), INTENT(in) :: sparameter

  ! OPTIONAL: A default value
  INTEGER(I32), INTENT(in), OPTIONAL :: idefault

  ! OPTIONAL: The number of the arrayindex to be returned.
  ! =0: returns the integer directly behind the '=' sign in the line
  !     'name=value'.
  ! >0: returns array index  iarrayindex.
  INTEGER, INTENT(in), OPTIONAL :: iarrayindex

!</input>

!<output>

  ! The value of the parameter
  INTEGER(I32), INTENT(out) :: ivalue

!</output>

!</subroutine>

  ! local variables
  CHARACTER (LEN=PARLST_LENLINEBUF) :: sdefault,svalue

  ! Call the string routine, perform a conversion afterwards.
  IF (PRESENT(idefault)) THEN
    WRITE (sdefault,*) idefault
    CALL parlst_getvalue_string_direct (rparlist, ssectionName, &
                                        sparameter, svalue, sdefault, &
                                        iarrayindex)
  ELSE
    CALL parlst_getvalue_string_direct (rparlist, ssectionName, &
                                        sparameter, svalue, &
                                        isubstring=iarrayindex)
  END IF

  READ(svalue,*) ivalue

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>
  SUBROUTINE parlst_getvalue_int64_indir (rsection, sparameter, ivalue, &
                                          idefault, iarrayindex)
!<description>

  ! Returns the value of a parameter in the section ssection.
  ! If the value does not exist, idefault is returned.
  ! If idefault is not given, an error will be thrown.
  !
  ! If the value is an array of integers, the optional parameter
  ! iarrayindex>=0 allows to specify the number of the integer to be
  ! returned; iarrayindex=0 returns the value directly behind the '='
  ! sign in the line of the parameter, iarrayindex>0 returns the
  ! array-entry in the lines below the parameter.
  !
  ! When omitting iarrayindex, the value directly behind the '=' sign
  ! is returned.
!</description>

!<input>

  ! The section where to search for the parameter
  TYPE(t_parlstSection), INTENT(in) :: rsection

  ! The parameter name.
  CHARACTER(LEN=*), INTENT(in) :: sparameter

  ! OPTIONAL: A default value
  INTEGER(I64), INTENT(in), OPTIONAL :: idefault

  ! OPTIONAL: The number of the arrayindex to be returned.
  ! =0: returns the integer directly behind the '=' sign in the line
  !     'name=value'.
  ! >0: returns array index  iarrayindex.
  INTEGER, INTENT(in), OPTIONAL :: iarrayindex

!</input>

!<output>

  ! The value of the parameter
  INTEGER(I64), INTENT(out) :: ivalue

!</output>

!</subroutine>

  ! local variables
  CHARACTER (LEN=PARLST_LENLINEBUF) :: sdefault,svalue

  ! Call the string routine, perform a conversion afterwards.
  IF (PRESENT(idefault)) THEN
    WRITE (sdefault,*) idefault
    CALL parlst_getvalue_string_indir (rsection, sparameter, svalue, &
                                       sdefault, iarrayindex)
  ELSE
    CALL parlst_getvalue_string_indir (rsection, sparameter, svalue, &
                                       isubstring=iarrayindex)
  END IF

  READ(svalue,*) ivalue

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>
  SUBROUTINE parlst_getvalue_int64_fetch (rsection, iparameter, ivalue, &
                                          bexists, iarrayindex)
!<description>

  ! Returns the value of a parameter in the section rsection.
  ! iparameter specifies the number of the parameter in section rsection.
  ! If bexists does not appear, an error is thrown if a nonexisting
  ! parameter is accessed.
  !
  ! If bexists is given, it will be set to TRUE if the parameter number
  ! iparameter exists, otherwise it will be set to FALSE and ivalue=0.
  !
  ! If the value is an array of integers, the optional parameter
  ! iarrayindex>=0 allows to specify the number of the integer to be
  ! returned; iarrayindex=0 returns the value directly behind the '='
  ! sign in the line of the parameter, iarrayindex>0 returns the
  ! array-entry in the lines below the parameter.
  !
  ! When omitting iarrayindex, the value directly behind the '=' sign
  ! is returned.

!</description>

!<input>

  ! The section where to search for the parameter
  TYPE(t_parlstSection), INTENT(in) :: rsection

  ! The number of the parameter.
  INTEGER, INTENT(in) :: iparameter

  ! OPTIONAL: The number of the arrayindex to be returned.
  ! =0: returns the integer directly behind the '=' sign in the line
  !     'name=value'.
  ! >0: returns array index  iarrayindex.
  INTEGER, INTENT(in), OPTIONAL :: iarrayindex

!</input>

!<output>

  ! The value of the parameter
  INTEGER(I64), INTENT(out) :: ivalue

  ! OPTIONAL: Parameter existance check
  ! Is set to TRUE/FALSE, depending on whether the parameter exists.
  LOGICAL, INTENT(out), OPTIONAL :: bexists

!</output>

!</subroutine>

  ! local variables
  CHARACTER (LEN=PARLST_LENLINEBUF) :: svalue

  svalue = '0'
  CALL parlst_getvalue_string_fetch (rsection, iparameter, svalue, &
                                     bexists, iarrayindex)
  READ(svalue,*) ivalue

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>
  SUBROUTINE parlst_getvalue_int64_direct (rparlist, ssectionName, &
                                           sparameter, ivalue, idefault,&
                                           iarrayindex)
!<description>

  ! Returns the value of a parameter in the section ssection. If the
  ! value does not exist, idefault is returned.  If idefault is not
  ! given, an error will be thrown.
  !
  ! If the value is an array of integers, the optional parameter
  ! iarrayindex>=0 allows to specify the number of the integer to be
  ! returned; iarrayindex=0 returns the value directly behind the '='
  ! sign in the line of the parameter, iarrayindex>0 returns the
  ! array-entry in the lines below the parameter.
  !
  ! When omitting iarrayindex, the value directly behind the '=' sign
  ! is returned.

!</description>

!<input>

  ! The parameter list.
  TYPE(t_parlist), INTENT(in) :: rparlist

  ! The section name - '' identifies the unnamed section.
  CHARACTER(LEN=*), INTENT(in) :: ssectionName

  ! The parameter name.
  CHARACTER(LEN=*), INTENT(in) :: sparameter

  ! OPTIONAL: A default value
  INTEGER(I64), INTENT(in), OPTIONAL :: idefault

  ! OPTIONAL: The number of the arrayindex to be returned.
  ! =0: returns the integer directly behind the '=' sign in the line
  !     'name=value'.
  ! >0: returns array index  iarrayindex.
  INTEGER, INTENT(in), OPTIONAL :: iarrayindex

!</input>

!<output>

  ! The value of the parameter
  INTEGER(I64), INTENT(out) :: ivalue

!</output>

!</subroutine>

  ! local variables
  CHARACTER (LEN=PARLST_LENLINEBUF) :: sdefault,svalue

  ! Call the string routine, perform a conversion afterwards.
  IF (PRESENT(idefault)) THEN
    WRITE (sdefault,*) idefault
    CALL parlst_getvalue_string_direct (rparlist, ssectionName, &
                                        sparameter, svalue, sdefault, &
                                        iarrayindex)
  ELSE
    CALL parlst_getvalue_string_direct (rparlist, ssectionName, &
                                        sparameter, svalue, &
                                        isubstring=iarrayindex)
  END IF

  READ(svalue,*) ivalue

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>

  SUBROUTINE parlst_addvalue_indir (rsection, sparameter, svalue, nsubstrings)

!<description>
  ! Adds a parameter to a section rsection.
  ! If the parameter exists, it is overwritten.
!</description>

!<inputoutput>

  ! The section where to arr the parameter
  TYPE(t_parlstSection), INTENT(inout) :: rsection

!</inputoutput>

!<input>

  ! The parameter name.
  CHARACTER(LEN=*), INTENT(in) :: sparameter

  ! The value of the parameter
  CHARACTER(LEN=*), INTENT(in) :: svalue

  ! OPTIONAL: Number of substrings. This allows a parameter to have
  ! multiple substrings, which can be accessed via the 'isubstring'
  ! parameter in the GET-routines.
  INTEGER, INTENT(in), OPTIONAL :: nsubstrings

!</input>

!</subroutine>

    ! local variables
    CHARACTER(LEN=PARLST_MLNAME) :: paramname
    INTEGER :: i,j

    ! Create the upper-case parameter name
    paramname = ADJUSTL(sparameter)
    CALL sys_toupper (paramname)

    ! Get the parameter index into 'exists', finish.
    CALL parlst_fetchparameter(rsection, paramname, i)

    IF (i .eq. 0) THEN

      ! Does not exist. Append.
      !
      ! Enough space free? Otherwise reallocate the parameter list
      IF (rsection%iparamCount .eq. SIZE(rsection%p_Sparameters)) THEN
        CALL parlst_reallocsection (rsection, SIZE(rsection%p_Sparameters)+PARLST_NPARSPERBLOCK)
      END IF

      ! Add the parameter - without any adjustment of the 'value' string
      rsection%iparamCount = rsection%iparamCount + 1

      ! Set i to the index of the parameter
      i = rsection%iparamCount

    ELSE

      ! Check if there are substrings. If yes, deallocate.
      ! Will be allocated later if necessary.
      IF (ASSOCIATED(rsection%p_Rvalues(i)%p_SentryList)) THEN
        DEALLOCATE(rsection%p_Rvalues(i)%p_SentryList)
        rsection%p_Rvalues(i)%nsize = 0
      END IF

      ! Deallocate memory for the value, will be reallocated later.
      IF (ASSOCIATED(rsection%p_Rvalues(i)%p_sentry)) THEN
        DEALLOCATE(rsection%p_Rvalues(i)%p_sentry)
      END IF

    END IF

    rsection%p_Sparameters(i) = paramname
    j = LEN_TRIM(svalue)
    ALLOCATE(rsection%p_Rvalues(i)%p_sentry(MAX(1,j)))
    rsection%p_Rvalues(i)%p_sentry(:) = ' '
    CALL sys_stringtochararray(svalue,rsection%p_Rvalues(i)%p_sentry,j)

    ! Add a list for the substrings if the parameter should have substrings.
    IF (PRESENT(nsubstrings)) THEN
      IF (nsubstrings .gt. 0) THEN
        ALLOCATE(rsection%p_Rvalues(i)%p_SentryList(MAX(1,j),nsubstrings))
        rsection%p_Rvalues(i)%p_SentryList(:,:) = ' '
        rsection%p_Rvalues(i)%nsize = nsubstrings
      ELSE
        NULLIFY(rsection%p_Rvalues(i)%p_SentryList)
        rsection%p_Rvalues(i)%nsize = 0
      END IF
    ELSE
      NULLIFY(rsection%p_Rvalues(i)%p_SentryList)
      rsection%p_Rvalues(i)%nsize = 0
    END IF

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>
  SUBROUTINE parlst_addvalue_direct (rparlist, ssectionName, sparameter, svalue,&
                                     nsubstrings)
!<description>

  ! Adds a parameter to a section with name ssectionName in the parameter list
  ! rparlist. If ssectionName='', the parameter is added to the unnamed
  ! section.

!</description>

!<inputoutput>

  ! The parameter list.
  TYPE(t_parlist), INTENT(inout) :: rparlist

!</inputoutput>

!<input>

  ! The section name - '' identifies the unnamed section.
  CHARACTER(LEN=*), INTENT(in) :: ssectionName

  ! The parameter name.
  CHARACTER(LEN=*), INTENT(in) :: sparameter

  ! The value of the parameter
  CHARACTER(LEN=*), INTENT(in) :: svalue

  ! OPTIONAL: Number of substrings. This allows a parameter to have
  ! multiple substrings, which can be accessed via the 'isubstring'
  ! parameter in the GET-routines.
  INTEGER, INTENT(in), OPTIONAL :: nsubstrings

!</input>

!</subroutine>

  ! local variables
  TYPE(t_parlstSection), POINTER :: p_rsection

  ! Cancel if the list is not initialised.
  IF (rparlist%isectionCount .eq. 0) THEN
    CALL output_line ('Parameter list not initialised!', &
        OU_CLASS_ERROR,OU_MODE_STD,'parlst_getvalue_string_fetch')
    CALL sys_halt()
  END IF

  ! Get the section
  CALL parlst_querysection(rparlist, ssectionName, p_rsection)
  IF (.NOT. ASSOCIATED(p_rsection)) THEN
    CALL output_line ('Section not found: '//TRIM(ssectionName), &
        OU_CLASS_ERROR,OU_MODE_STD,'parlst_addvalue_direct')
    RETURN
  END IF

  ! Add the parameter

  CALL parlst_addvalue_indir (p_rsection, sparameter, svalue, nsubstrings)

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>

  SUBROUTINE parlst_setvalue_fetch (rsection, iparameter, svalue, iexists,&
                                    isubstring)

!<description>

  ! Modifies the value of a parameter in the section rsection.
  ! The value of parameter iparameter in the section rsection is modified.
  ! If iexists does not appear, an error is thrown if a nonexisting
  ! parameter is accessed.
  ! If iexists is given, it will be set to YES if the parameter number
  ! iparameter exists and was modified, otherwise it will be set to NO.
  !
  ! isubstring allows to specify the numer of a substring of the parameter to
  ! change. If omitted or = 0, the 'headline' directly behind the '='
  ! sign of the line 'name=value' is modified. Otherwise, the corresponding
  ! substring is changed.

!</description>

!<inputoutput>

  ! The section where to arr the parameter
  TYPE(t_parlstSection), INTENT(inout) :: rsection

!</inputoutput>

!<input>

  ! The parameter name.
  INTEGER, INTENT(in) :: iparameter

  ! The new value of the parameter
  CHARACTER(LEN=*), INTENT(in) :: svalue

  ! OPTIONAL: The number of the substring to be changed.
  ! =0: changes the string directly behind the '=' sign in the line
  !     'name=value'.
  ! >0: changes substring isubstring.
  INTEGER, INTENT(in), OPTIONAL :: isubstring

!</input>

!<output>

  ! Optional parameter. Is set to YES/NO, depending on whether
  ! the parameter exists.
  INTEGER, INTENT(out), OPTIONAL :: iexists

!</output>

!</subroutine>

  INTEGER :: isub,j

  ! Check if iparameter is out of bounds. If yes, probably
  ! throw an error.

  IF ((iparameter .lt. 0) .OR. (iparameter .gt. rsection%iparamCount)) THEN

    IF (.NOT. PRESENT(iexists)) THEN
      CALL output_line ('Error. Parameter '//TRIM(sys_siL(iparameter,10))//&
          ' does not exist!', &
          OU_CLASS_ERROR,OU_MODE_STD,'parlst_setvalue_fetch')
      CALL sys_halt()
    ELSE
      iexists = NO
      RETURN
    END IF

  END IF

  ! Depending on isubstring, change either the 'headline' or one
  ! of the substrings.
  isub = 0
  IF (PRESENT(isubstring)) isub = isubstring

  j = LEN_TRIM(svalue)
  IF ((isub .le. 0) .OR. &
      (isub .gt. rsection%p_Rvalues(iparameter)%nsize)) THEN
    ! Reallocate memory
    DEALLOCATE(rsection%p_Rvalues(iparameter)%p_sentry)
    ALLOCATE(rsection%p_Rvalues(iparameter)%p_sentry(MAX(1,j)))
    rsection%p_Rvalues(iparameter)%p_sentry(:) = ' '
    CALL sys_stringToCharArray(svalue,rsection%p_Rvalues(iparameter)%p_sentry,j)
  ELSE
    ! Check that there is enough memory to save the string.
    IF (UBOUND(rsection%p_Rvalues(iparameter)%p_SentryList,1) .le. j) THEN
      CALL parlst_reallocSubVariables(rsection%p_Rvalues(iparameter),j)
    END IF
    CALL sys_stringToCharArray(svalue,rsection%p_Rvalues(iparameter)%p_SentryList(:,isub),j)
  END IF

  IF (PRESENT(iexists)) iexists = YES

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>

  SUBROUTINE parlst_setvalue_indir (rsection, sparameter, svalue, isubstring)

!<description>

  ! Modifies the value of a parameter in the section rsection.
  ! If the parameter does not exist, an error is thrown.
  !
  ! isubstring allows to specify the numer of a substring of the parameter to
  ! change. If omitted or = 0, the 'headline' directly behind the '='
  ! sign of the line 'name=value' is modified. Otherwise, the corresponding
  ! substring is changed.

!</description>

!<inputoutput>

  ! The section where to arr the parameter
  TYPE(t_parlstSection), INTENT(inout) :: rsection

!</inputoutput>

!<input>

  ! The parameter name.
  CHARACTER(LEN=*), INTENT(in) :: sparameter

  ! The new value of the parameter
  CHARACTER(LEN=*), INTENT(in) :: svalue

  ! OPTIONAL: The number of the substring to be changed.
  ! =0: changes the string directly behind the '=' sign in the line
  !     'name=value'.
  ! >0: changes substring isubstring.
  INTEGER, INTENT(in), OPTIONAL :: isubstring

!</input>

!</subroutine>

  ! local variables
  INTEGER :: i,isub,j
  CHARACTER(LEN=PARLST_MLNAME) :: paramname

  ! Create the upper-case parameter name
  paramname = ADJUSTL(sparameter)
  CALL sys_toupper (paramname)

  ! Get the parameter position
  i = parlst_queryvalue_indir (rsection, paramname)

  IF (i .eq. 0) THEN
    CALL output_line ('Parameter '//TRIM(paramname)//&
        ' does not exist, cannot be modified!', &
        OU_CLASS_ERROR,OU_MODE_STD,'parlst_setvalue_indir')
    CALL sys_halt()
  ELSE

    ! Depending on isubstring, change either the 'headline' or one
    ! of the substrings.
    isub = 0
    IF (PRESENT(isubstring)) isub = isubstring

    j = LEN_TRIM(svalue)
    IF ((isub .le. 0) .OR. (isub .gt. rsection%p_Rvalues(i)%nsize)) THEN
      ! Reallocate memory
      DEALLOCATE(rsection%p_Rvalues(i)%p_sentry)
      ALLOCATE(rsection%p_Rvalues(i)%p_sentry(MAX(1,j)))
      rsection%p_Rvalues(i)%p_sentry(:) = ' '
      CALL sys_stringToCharArray(svalue,rsection%p_Rvalues(i)%p_sentry,j)
    ELSE
      ! Check that there is enough memory to save the string.
      IF (UBOUND(rsection%p_Rvalues(i)%p_SentryList,1) .le. j) THEN
        CALL parlst_reallocSubVariables(rsection%p_Rvalues(i),j)
      END IF
      CALL sys_stringToCharArray(svalue,rsection%p_Rvalues(i)%p_SentryList(:,isub),j)
    END IF

  END IF

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>
  SUBROUTINE parlst_setvalue_direct (rparlist, ssectionName, sparameter, svalue,&
                                     isubstring)
!<description>

  ! Modifies the value of a parameter in the section with name ssectionName
  ! in the parameter list rparlist.
  ! If the parameter does not exist, an error is thrown.
  !
  ! isubstring allows to specify the numer of a substring of the parameter to
  ! change. If omitted or = 0, the 'headline' directly behind the '='
  ! sign of the line 'name=value' is modified. Otherwise, the corresponding
  ! substring is changed.

!</description>

!<inputoutput>

  ! The parameter list.
  TYPE(t_parlist), INTENT(inout) :: rparlist

!</inputoutput>

!<input>

  ! The section name - '' identifies the unnamed section.
  CHARACTER(LEN=*), INTENT(in) :: ssectionName

  ! The parameter name.
  CHARACTER(LEN=*), INTENT(in) :: sparameter

  ! The new value of the parameter
  CHARACTER(LEN=*), INTENT(in) :: svalue

  ! OPTIONAL: The number of the substring to be changed.
  ! =0: changes the string directly behind the '=' sign in the line
  !     'name=value'.
  ! >0: changes substring isubstring.
  INTEGER, INTENT(in), OPTIONAL :: isubstring

!</input>

!</subroutine>

  ! local variables
  TYPE(t_parlstSection), POINTER :: p_rsection

  ! Cancel if the list is not initialised.
  IF (rparlist%isectionCount .eq. 0) THEN
    CALL output_line ('Parameter list not initialised!', &
        OU_CLASS_ERROR,OU_MODE_STD,'parlst_setvalue_direct')
    CALL sys_halt()
  END IF

  ! Get the section
  CALL parlst_querysection(rparlist, ssectionName, p_rsection)
  IF (.NOT. ASSOCIATED(p_rsection)) THEN
    CALL output_line ('Section not found: '//TRIM(ssectionName), &
        OU_CLASS_ERROR,OU_MODE_STD,'parlst_setvalue_direct')
    RETURN
  END IF

  ! Set the parameter

  CALL parlst_setvalue_indir (p_rsection, sparameter, svalue, isubstring)

  END SUBROUTINE

  ! ***************************************************************************

  ! Internal subroutine: Read a line from a text file.

  SUBROUTINE parlst_readlinefromfile (iunit, sdata, ilinelen, ios)

  ! The unit where to read from; must be connected to a file.
  INTEGER, INTENT(in) :: iunit

  ! The string where to write data to
  CHARACTER(LEN=*), INTENT(out) :: sdata

  ! Length of the output
  INTEGER, INTENT(out) :: ilinelen

  ! Status of the reading process. Set to a value <> 0 if the end
  ! of the file is reached.
  INTEGER, INTENT(out) :: ios

  ! local variables
  CHARACTER :: c

  sdata = ''
  ilinelen = 0

  ! Read the data - as long as the line/file does not end.
  DO

    ! Read a character.
    ! Unfortunately, Fortran forces me to use this dirty GOTO
    ! to decide processor-independently whether the line or
    ! the record ends.
    READ (unit=iunit, fmt='(A1)', iostat=ios, advance='NO',&
          END=10, eor=20) c

    ! Do not do anything in case of an error
    IF (ios .eq. 0) THEN

      ilinelen = ilinelen + 1
      sdata (ilinelen:ilinelen) = c

    END IF

    ! Proceed to next character
    CYCLE

    ! End of file.
10  ios = -1
    EXIT

    ! End of record = END OF LINE.
20  ios = 0
    EXIT

  END DO

  END SUBROUTINE

  ! ***************************************************************************

  ! Internal subroutine: Parse a text line.
  ! This parses the text line sdata.
  ! Return values:
  !  ityp = 0 -> The line is a comment
  !  ityp = 1 -> The line is a section. ssecname is the name of the section
  !              without '[]'
  !  ityp = 2 -> The line is a parameter. sparamname is the uppercase
  !              parameter name. svalue is the value of the parameter,
  !              trimmed and left adjusted.
  !  ityp = 3 -> Line is the beginning of a multi-valued parameter.
  !              The next isubstring lines contain additional substrings.
  !  ityp = 4 -> Line is a substring of a multi-valued parameter.

  SUBROUTINE parlst_parseline (sdata, ityp, isubstring, ilinenum, &
                               ssecname, sparamname, svalue, sfilename)

  ! The line to be parsed
  CHARACTER(LEN=*), INTENT(in) :: sdata

  ! The typ of the line
  INTEGER, INTENT(out) :: ityp

  ! input: =0: parse line as parameter. isubstring is changed to a value > 0
  !            is the parameter has multiple values attached.
  !        >0: parse line as substring of a multi-valued parameter, not
  !            containing a leading 'name='.
  ! output: If the 'headline' of a multi-valued parameter is read, isubstring is
  !         changed to the number of substrings (the k in 'name(k)=...').
  !         Otherwise unchanged.
  INTEGER, INTENT(inout) :: isubstring

  ! Line number
  INTEGER, INTENT(in) :: ilinenum

  ! Section name, if it is a section
  CHARACTER(LEN=*), INTENT(inout) :: ssecname

  ! Parameter name, if it is a parameter
  CHARACTER(LEN=*), INTENT(inout) :: sparamname

  ! Parameter value, if it is a parameter
  CHARACTER(LEN=*), INTENT(inout) :: svalue

  ! OPTIONAL: Filename of the file to be parsed.
  ! Will be printed out in error messages if present.
  CHARACTER(LEN=*), INTENT(in), OPTIONAL :: sfilename

  ! local variables
  INTEGER :: i,j1,j2,ltr
  CHARACTER(LEN=PARLST_LENLINEBUF) :: sbuf,slen

    ityp = 0

    ! Do we have data in sdata?
    IF (sdata .eq. '') RETURN

    ! Copy the input string - left adjusted - and get the string length
    sbuf = ADJUSTL(sdata)

    ! Should we parse the line as first line of a parameter or as substring
    ! of a multi-valued parameter?
    IF (isubstring .eq. 0) THEN

      ! Standard parameter or section header.
      !
      ! Do we start with '[' and end with ']'?
      IF (sbuf(1:1) .eq. "[") THEN

        ! Find the final ']'.
        DO ltr = 1,LEN(sbuf)
          IF (sbuf(ltr:ltr) .eq. "]") EXIT
        END DO

        IF (sbuf(ltr:ltr) .ne. ']') THEN
          IF (PRESENT(sfilename)) THEN
            CALL output_line ('File: '//TRIM(sfilename),&
                OU_CLASS_ERROR,OU_MODE_STD,'parlst_parseline')
          END IF
          CALL output_line ('Wrong syntax of section name. Line '//&
              TRIM(sys_siL(ilinenum,10))//':', &
              OU_CLASS_ERROR,OU_MODE_STD,'parlst_parseline')
          CALL output_line (sbuf, &
              OU_CLASS_ERROR,OU_MODE_STD,'parlst_parseline')
          CALL sys_halt()
        END IF

        ! Get the section name
        ssecname = sbuf(2:ltr-1)
        ityp = 1
        RETURN

      ELSE IF (sbuf(1:1) .eq. PARLST_COMMENT) THEN

        ! Comment sign
        RETURN

      ELSE

        ! Must be a parameter. Get the length of the string without comment
        ! at the end.
        CALL linelength(sbuf, ltr)

        ! ltr=0 means: empty line. Ignore that.
        IF (ltr .eq. 0) RETURN

        ! Is there a '(..)' that is indicating a multi-valued parameter?
        j1 = INDEX(sbuf(1:ltr),'(')
        j2 = INDEX(sbuf(1:ltr),')')

        ! Is there a '=' sign?
        i = INDEX(sbuf(1:ltr),'=')

        IF (i .eq. 0) THEN
          IF (PRESENT(sfilename)) THEN
            CALL output_line ('File: '//TRIM(sfilename),&
                OU_CLASS_ERROR,OU_MODE_STD,'parlst_parseline')
          END IF
          CALL output_line ('Invalid parameter syntax. Line '&
              //TRIM(sys_siL(ilinenum,10))//':', &
              OU_CLASS_ERROR,OU_MODE_STD,'parlst_parseline')
          CALL output_line (TRIM(sbuf), &
              OU_CLASS_ERROR,OU_MODE_STD,'parlst_parseline')
          CALL sys_halt()
        END IF

        IF ((j1 .eq. 0) .OR. (j2 .le. j1) .OR. (i .le. j1)) THEN

          ityp = 2

          ! Get the name of the parameter
          sparamname = ADJUSTL(sbuf(1:i-1))

          ! Get the parameter value
          svalue = ADJUSTL(sbuf(i+1:ltr))

        ELSE

          ! Probably multi-valued parameter with substrings in the
          ! following lines.

          ! Get the name of the parameter
          sparamname = ADJUSTL(sbuf(1:j1-1))

          ! Get the parameter value
          svalue = ADJUSTL(sbuf(i+1:ltr))

          ! Get the length of the parameter list.
          slen = sbuf (j1+1:MIN(j2-1,LEN(slen)))

          isubstring = 0
          READ(slen,*) isubstring

          IF (isubstring .le. 0) THEN
            ! Oh, only one line. User wants to cheat :-)
            isubstring = 0

            ityp = 2
          ELSE
            ! Real multi-valued parameter.
            ityp = 3
          END IF

        END IF

      END IF

    ELSE

      ! Substring of a multi-valued parameter.
      IF (sbuf(1:1) .eq. PARLST_COMMENT) THEN

        ! Comment sign
        RETURN

      ELSE

        ! Must be a parameter. Get the length of the string without comment
        ! at the end.
        CALL linelength(sbuf, ltr)

        ! ltr=0 means: empty line. Ignore that.
        IF (ltr .eq. 0) RETURN

        ityp = 4

        ! Get the parameter value. Do not get a parameter name; there is none.
        svalue = ADJUSTL(sbuf(1:ltr))

      END IF

    END IF

  CONTAINS

    ! Sub-subroutine: find the length of the line, removing comments
    ! at the end.

    SUBROUTINE linelength (sdata, l)

    ! The string to parse. Must not be ''!
    CHARACTER(LEN=*), INTENT(in) :: sdata

    ! The index of the last character without any comment at the end.
    INTEGER, INTENT(out) :: l

    ! local variables
    LOGICAL :: bflag   ! Set to true if we are in apostroph mode
    INTEGER :: lsdata

    bflag = .false.

    ! Go through all characters
    l = 0
    lsdata = LEN(sdata)
    DO WHILE (l .lt. lsdata)

      ! next character
      l = l+1

      ! A comment character while we are not in apostroph mode? Stop.
      IF ((.NOT. bflag) .AND. (sdata(l:l) .eq. PARLST_COMMENT)) THEN
        l = l-1
        EXIT
      END IF

      ! An apostroph?
      IF (sdata(l:l) .eq. "'") THEN

        ! Switch the apostroph mode.
        ! Btw.: Two subsequent apostrophes will switch the mode off and on again.
        bflag = .NOT. bflag

      END IF

    END DO

    END SUBROUTINE

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>

  SUBROUTINE parlst_readfromfile (rparlist, sfile, sdirectory, bexpandVars)

!<description>

  ! This routine parses a text file for data of the INI-file form.
  ! sfile must be the name of a file on the hard disc.
  ! The file may have references to subfiles in the unnamed section:
  ! If there is a parameter "simportdatafiles(n)" present in the
  ! unnamed section, the routine expects a list of filenames in this
  ! parameter. All the files listed there are read in as well.
  ! Parameters at the end of the master file will overwrite
  ! the parameters from the files in simportdatafiles.
  !
  ! Sub-files specified in the main file are searched in the following
  ! directories:
  ! 1.) sdirectory (if specified)
  ! 2.) the directory that contains sfile (if sfile specifies
  !     a directory)
  ! 3.) current directory
  !
  ! The parameters read from the file(s) are added to the parameter list
  ! rparlist, which has to be initialised with parlst_init before
  ! calling the routine.
  !
  ! Remark: When adding parameters/sections to rparlist, the routine
  !   checks whether the parameters/sections already exist.
  !   Adding a parameter/section which exists does not result in an error -
  !   the first instance of the parameter/section will just be overwritten.

!</description>

!<inputoutput>

  ! The parameter list which is filled with data from the file
  TYPE(t_parlist), INTENT(inout) :: rparlist

!</inputoutput>

!<input>

  ! The filename of the file to read.
  CHARACTER(LEN=*), INTENT(in) :: sfile

  ! OPTIONAL: Directory containing other data files in case
  ! the main file sfile contains references to subfiles.
  CHARACTER(LEN=*), INTENT(in), OPTIONAL :: sdirectory

  ! OPTIONAL: Expand references to subvariables.
  ! TRUE: Subvariables like "%{section.varname} are expanded to actual values.
  !       This is the standard setting.
  ! FALSE: Subvariables are left as they are.
  LOGICAL, INTENT(in), OPTIONAL :: bexpandVars
!</input>

    ! local variables
    INTEGER :: iidxsubfiles,icurrentsubfile
    CHARACTER(LEN=PARLST_LENLINEBUF), DIMENSION(:), POINTER :: p_Ssubfiles,p_SsubfilesTemp
    CHARACTER(LEN=PARLST_LENLINEBUF) :: sstring,smainfile
    INTEGER :: nsubfiles,nnewsubfiles,j,ilensubf
    LOGICAL :: bexists,bmainpath,babsolute
    CHARACTER(LEN=PARLST_LENLINEBUF) :: smainpath,sfilepath,sfilename

    ! Filename/path of the master dat file.
    ! Search at first in the specified path.
    bexists = .false.
    IF (PRESENT(sdirectory)) THEN
      smainfile = TRIM(sdirectory)//"/"//sfile
      INQUIRE(file=smainfile, exist=bexists)
    END IF

    IF (.NOT. bexists) THEN
      smainfile = sfile
      INQUIRE(file=smainfile, exist=bexists)
    END IF

    IF (.NOT. bexists) THEN
      ! Cancel if the file does not exist.
      RETURN
    END IF

    ! Get the main path/name of the file.
    CALL io_pathExtract (smainfile, smainpath)
    bmainpath = smainpath .ne. ""

    ! Create a list of files to be read.
    ! They contain filenames including the directory.
    ALLOCATE(p_Ssubfiles(1))
    p_Ssubfiles(1) = smainfile

    icurrentsubfile = 0
    nsubfiles = 1

    ! Now read all files in the list. Append new files to the list if necessary.
    DO WHILE (icurrentsubfile .lt. nsubfiles)

      ! Read the unnamed section from the next file.
      icurrentsubfile = icurrentsubfile + 1

      ! Get the filename including the path.
      bexists = .false.

      CALL io_pathExtract (TRIM(p_Ssubfiles(icurrentsubfile)), sfilepath, sfilename, babsolute)

      IF (babsolute) THEN
        ! Ok, we have an absolute path given. Test it.
        sstring = TRIM(p_Ssubfiles(icurrentsubfile))
        INQUIRE(file=sstring, exist=bexists)
      ELSE
        ! Path is relative -- a little bit more complicated.
        ! Is there a directory given?
        IF (sfilepath .ne. "") THEN
          ! Directory specified. We add "sdirectory" if we have it.
          IF (PRESENT(sdirectory)) THEN
            sstring = TRIM(sdirectory)//"/"//TRIM(sfilepath)//"/"//TRIM(sfilename)
            INQUIRE(file=sstring, exist=bexists)
          END IF

          IF (.NOT. bexists) THEN
            ! No, not there. Then take the path directly.
            sstring = TRIM(sfilepath)//"/"//TRIM(sfilename)
            INQUIRE(file=sstring, exist=bexists)
          END IF

          IF (bmainpath .AND. (.NOT. bexists)) THEN
            ! No, not there. Add the master directory and test there.
            sstring = TRIM(smainpath)//"/"//TRIM(sfilepath)//"/"//TRIM(sfilename)
            INQUIRE(file=sstring, exist=bexists)
          END IF
        ELSE
          ! No directory given. Then we search in the directory
          ! of the master file...
          IF (bmainpath .AND. (.NOT. bexists)) THEN
            sstring = TRIM(smainpath)//"/"//TRIM(sfilename)
            INQUIRE(file=sstring, exist=bexists)
          END IF

          ! And in the current directory.
          IF (.NOT. bexists) THEN
            sstring = TRIM(sfilename)
            INQUIRE(file=sstring, exist=bexists)
          END IF
        END IF
      END IF

      IF (bexists) THEN
        CALL parlst_readfromsinglefile (rparlist, sstring, .false., .false.)

        ! Replace the filename with the string including the path.
        ! Then the actual read process at the end of the routine can be
        ! handled easier.
        p_Ssubfiles(icurrentsubfile) = TRIM(sstring)

      ELSE
        CALL output_line ('Specified data-subfile does not exist: '//sstring, &
            OU_CLASS_WARNING,OU_MODE_STD,'parlst_readfromfile')
      END IF

      ! Check if there is a parameter "simportdatafiles" available in the
      ! parameter list. It must be present in the unnamed section.
      CALL parlst_fetchparameter(rparlist%p_Rsections(1), "SIMPORTDATAFILES", iidxsubfiles)

      IF (iidxsubfiles .ne. 0) THEN
        ! Append the new files to the file list.
        !
        ! Get the number of new files. The parameter definitely exists as
        ! it was created when reading the 'master' INI file.
        nnewsubfiles = parlst_querysubstrings (rparlist%p_Rsections(1), &
            "SIMPORTDATAFILES")

        ! if nnewsubfiles=0, there is (hopefully) only one string here.
        IF (nnewsubfiles .eq. 0) THEN
          CALL parlst_getvalue_string(rparlist%p_Rsections(1), iidxsubfiles, sstring,bdequote=.true.)
          IF (TRIM(sstring) .ne. "") THEN
            ! Append the data.
            ALLOCATE(p_SsubfilesTemp(nsubfiles+1))
            p_SsubfilesTemp(1:nsubfiles) = p_Ssubfiles(:)
            DEALLOCATE(p_Ssubfiles)
            p_Ssubfiles => p_SsubfilesTemp
            NULLIFY(p_SsubfilesTemp)

            ! Expand subvariables and environment variables here.
            ! This point is independent of a parameter bexpandVars
            ! as subfiles may otherwise not be found!
            CALL parlst_expandEnvVariable(sstring)
            CALL parlst_expandSubvariable(rparlist,sstring)

            p_Ssubfiles(nsubfiles+1) = sstring
            nsubfiles = nsubfiles + 1
          END IF
        ELSE
          ! Get all the filenames.
          ALLOCATE(p_SsubfilesTemp(nsubfiles+nnewsubfiles))
          p_SsubfilesTemp(1:nsubfiles) = p_Ssubfiles(:)
          DEALLOCATE(p_Ssubfiles)
          p_Ssubfiles => p_SsubfilesTemp
          NULLIFY(p_SsubfilesTemp)

          DO j=1,nnewsubfiles
            CALL parlst_getvalue_string(rparlist%p_Rsections(1), iidxsubfiles, &
                p_Ssubfiles(nsubfiles+j),isubstring=j,bdequote=.true.)

            ! Expand subvariables and environment variables here.
            ! This point is independent of a parameter bexpandVars
            ! as subfiles may otherwise not be found!
            CALL parlst_expandEnvVariable(p_Ssubfiles(nsubfiles+j))
            CALL parlst_expandSubvariable(rparlist,p_Ssubfiles(nsubfiles+j))
          END DO

          nsubfiles = nsubfiles + nnewsubfiles
        END IF

        ! Remove all substrings from the simportdatafiles parameter, so the parameter
        ! is filled with new data upon the next read statement.
        CALL parlst_addvalue(rparlist%p_Rsections(1), "SIMPORTDATAFILES", "")

      END IF

    END DO

    ! Ok, at that point we know which files to read -- so read them, one after
    ! the other. The 'master' file must be read at last!
    ilensubf = 1
    DO icurrentsubfile = 2,nsubfiles
      sstring = TRIM(p_Ssubfiles(icurrentsubfile))
      ilensubf = MAX(ilensubf,LEN_TRIM(sstring))
      INQUIRE(file=sstring, exist=bexists)

      IF (bexists) THEN
        ! Read the sub-files...
        ! Do not yet expand variables.
        CALL parlst_readfromsinglefile (rparlist, sstring, .true., .false.)
      END IF
    END DO

    ! ... and the master
    icurrentsubfile = 1
    sstring = TRIM(p_Ssubfiles(icurrentsubfile))
    INQUIRE(file=sstring, exist=bexists)

    IF (bexists) THEN
      ! Do not yet expand variables.
      CALL parlst_readfromsinglefile (rparlist, sstring, .true., .false.)
    END IF

    IF (nsubfiles .gt. 1) THEN
      ! There have a couple of subfiles been read from disc.
      ! All subfiles can be found in p_Ssubfiles.

      ! Incorporate the complete list to the parameter "SIMPORTDATAFILES".
      IF (ASSOCIATED(rparlist%p_Rsections(1)%p_Rvalues(iidxsubfiles)%p_SentryList)) THEN
        DEALLOCATE(rparlist%p_Rsections(1)%p_Rvalues(iidxsubfiles)%p_SentryList)
      END IF

      ALLOCATE(rparlist%p_Rsections(1)%p_Rvalues(iidxsubfiles)%p_SentryList(ilensubf+2,nsubfiles-1))
      rparlist%p_Rsections(1)%p_Rvalues(iidxsubfiles)%p_SentryList(:,:) = ' '
      DO icurrentsubfile = 1,nsubfiles-1
        CALL sys_stringToCharArray('"'//TRIM(p_Ssubfiles(1+icurrentsubfile))//'"',&
          rparlist%p_Rsections(1)%p_Rvalues(iidxsubfiles)%p_SentryList(:,icurrentsubfile));
      END DO
      rparlist%p_Rsections(1)%p_Rvalues(iidxsubfiles)%nsize = nsubfiles-1
    END IF

    ! Release memory, finish
    DEALLOCATE(p_Ssubfiles)

    ! Now expand all subvariables and environment variables to the actual values.
    IF (.NOT. PRESENT(bexpandVars)) THEN
      CALL parlst_expandEnvVariables(rparlist)
      CALL parlst_expandSubvars(rparlist)
    ELSE IF (bexpandVars) THEN
      CALL parlst_expandEnvVariables(rparlist)
      CALL parlst_expandSubvars(rparlist)
    END IF

  END SUBROUTINE

  ! ***************************************************************************

  SUBROUTINE parlst_readfromsinglefile (rparlist, sfilename, bimportSections, bexpandVars)

!<description>

  ! This routine parses a text file for data of the INI-file form.
  ! sfilename must be the name of a file on the hard disc.
  ! The parameters read from the file are added to the parameter list
  ! rparlist, which has to be initialised with parlst_init before
  ! calling the routine.
  ! Remark: When adding parameters/sections to rparlist, the routine
  !   checks whether the parameters/sections already exist.
  !   Adding a parameter/section which exists does not result in an error -
  !   the first instance of the parameter/section will just be overwritten.

!</description>

!<inputoutput>

  ! The parameter list which is filled with data from the file
  TYPE(t_parlist), INTENT(inout) :: rparlist

!</inputoutput>

!<input>

  ! The filename of the file to read.
  CHARACTER(LEN=*), INTENT(in) :: sfilename

  ! TRUE: Import all sections in the DAT file.
  ! FALSE: Import only the main (unnamed) section and ignore all other
  ! sections.#
  LOGICAL, INTENT(in) :: bimportSections

  ! OPTIONAL: Expand references to subvariables.
  ! TRUE: Subvariables like "%{section.varname} are expanded to actual values.
  !       This is the standard setting.
  ! FALSE: Subvariables are left as they are.
  LOGICAL, INTENT(in), OPTIONAL :: bexpandVars

!</input>

    ! local variables
    INTEGER :: iunit,ios,isbuflen,ityp,ilinenum,isubstring,nsubstrings,iparpos
    TYPE(t_parlstSection), POINTER :: p_currentsection
    CHARACTER(LEN=PARLST_LENLINEBUF) :: sdata
    CHARACTER(LEN=PARLST_MLSECTION) :: ssectionname
    CHARACTER(LEN=PARLST_MLNAME) :: sparname
    CHARACTER(LEN=PARLST_LENLINEBUF) :: svalue

    ! Try to open the file
    CALL io_openFileForReading(sfilename, iunit)

    ! Oops...
    IF (iunit .eq. -1) THEN
      CALL output_line ('Error opening .INI file: '//TRIM(sfilename), &
          OU_CLASS_ERROR,OU_MODE_STD,'parlst_readfromsinglefile')
      CALL sys_halt()
    END IF

    ! Start adding parameters to the unnamed section
    p_currentsection => rparlist%p_Rsections(1)

    ! Read all lines from the file
    ios = 0
    ilinenum = 0
    isubstring = 0
    nsubstrings = 0
    DO WHILE (ios .eq. 0)

      ! Read a line from the file into sbuf
      CALL parlst_readlinefromfile (iunit, sdata, isbuflen, ios)
      ilinenum = ilinenum + 1

      IF (isbuflen .ne. 0) THEN

        ! Parse the line
        CALL parlst_parseline (sdata, ityp, nsubstrings, ilinenum, ssectionname, &
                              sparname, svalue,sfilename)

        SELECT CASE (ityp)
        CASE (1)
          ! Stop parsing the file here if bimportSections tells us to do so.
          ! When the first section starts, the unnamed section is finished.
          IF (.NOT. bimportSections) EXIT

          ! Check if the section exists; if not, create a new one.
          CALL parlst_querysection(rparlist, ssectionname, p_currentsection)

          IF (.NOT. ASSOCIATED(p_currentsection)) THEN
            ! A new section name. Add a section, set the current section
            ! to the new one.
            CALL parlst_addsection (rparlist, ssectionname)
            p_currentsection => rparlist%p_Rsections(rparlist%isectionCount)
          END IF

        CASE (2)
          ! A new parameter. Add it to the current section.
          CALL parlst_addvalue (p_currentsection, sparname, svalue)

        CASE (3)
          ! 'Headline' of a multi-valued parameter. Add the parameter with
          ! isubstring subvalues
          CALL parlst_addvalue (p_currentsection, sparname, svalue, nsubstrings)

          ! Fetch the parameter for later adding of subvalues.
          iparpos = parlst_queryvalue(p_currentsection, sparname)

          ! isubstring counts the current readed substring.
          ! Set it to 0, it will be increased up to nsubstrings in 'case 4'.
          isubstring = 0

        CASE (4)
          ! Increase number of current substring
          isubstring = isubstring + 1

          ! Sub-parameter of a multi-valued parameter. Add the value to
          ! the last parameter that was added in case 3.
          CALL parlst_setvalue_fetch (p_currentsection, iparpos, svalue, &
                                      isubstring=isubstring)

          ! Decrement the substring counter. If we reach 0, parlst_parseline
          ! continues to parse standard parameters.
          nsubstrings = nsubstrings - 1

        ! Other cases: comment.
        END SELECT

      END IF

    END DO

    ! Close the file.
    CLOSE (iunit)

    IF (.NOT. PRESENT(bexpandVars)) THEN
      ! Expand all subvariables and environment variables to the actual values.
      CALL parlst_expandEnvVariables(rparlist)
      CALL parlst_expandSubvars(rparlist)
    ELSE IF (bexpandVars) THEN
      ! Expand all subvariables and environment variables to the actual values.
      CALL parlst_expandEnvVariables(rparlist)
      CALL parlst_expandSubvars(rparlist)
    END IF

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>

  SUBROUTINE parlst_getStringRepresentation (rparlist, p_sconfiguration)

!<description>
  ! Creates a string representation of the given parameter list rparlist.
  ! p_sconfiguration will be created as "array[1..*] of char" on
  ! the heap containing this representation. The memory must be manually
  ! released by the caller using DEALLOCATE when finished using the string
  ! representation.
!</description>

!<input>
  ! The parameter list which is filled with data from the file
  TYPE(t_parlist), INTENT(in) :: rparlist
!</input>

!<output>
  ! A pointer to a character array containing all lines of the parameter list.
  ! Points to NULL() if there is no data in the parameter list.
  ! Each line is terminated by NEWLINE.
  ! If there is data, a new pointer is allocated for this on the heap.
  ! The user must manually release the memory when finished using it.
  CHARACTER, DIMENSION(:), POINTER :: p_sconfiguration
!</output>

!</subroutine>

  INTEGER :: ilength,isection,ivalue,ientry,icount
  CHARACTER, DIMENSION(:), POINTER :: p_sbuf
  CHARACTER(LEN=PARLST_LENLINEBUF) :: sbuf

    IF (rparlist%isectionCount .eq. 0) THEN
      CALL output_line ('Parameter list not initialised', &
          OU_CLASS_ERROR,OU_MODE_STD,'parlst_getStringRepresentation')
      CALL sys_halt()
    END IF

    NULLIFY(p_sbuf)

    ! Number of characters in the buffer
    ilength = 0

    ! Loop through all sections
    DO isection = 1,rparlist%isectionCount

      ! Append the section name. May be empty for the unnamed section,
      ! which is always the first one.
      IF (isection .gt. 1) THEN
        ! Empty line before
        IF (ilength .gt. 0) CALL appendString(p_sbuf,ilength,'')
        CALL appendString(p_sbuf,ilength,&
          '['//TRIM(rparlist%p_Rsections(isection)%ssectionName)//']')
      END IF

      ! Loop through the values in the section
      DO ivalue = 1,rparlist%p_Rsections(isection)%iparamCount

        ! Do we have one or multiple entries to that parameter?
        icount = rparlist%p_Rsections(isection)%p_Rvalues(ivalue)%nsize
        IF (icount .eq. 0) THEN
          ! Write "name=value"
          CALL sys_charArrayToString(&
              rparlist%p_Rsections(isection)%p_Rvalues(ivalue)%p_sentry,sbuf)
          CALL appendString(p_sbuf,ilength,&
              TRIM(rparlist%p_Rsections(isection)%p_Sparameters(ivalue)) &
              //"="//TRIM(sbuf))
        ELSE
          ! Write "name(icount)="
          CALL appendString(p_sbuf,ilength,&
            TRIM(rparlist%p_Rsections(isection)%p_Sparameters(ivalue)) &
            //"("//TRIM(sys_siL(icount, 10))//")=")
          ! Write all the entries of that value, one each line.
          DO ientry = 1,icount
            CALL sys_charArrayToString(&
                rparlist%p_Rsections(isection)%p_Rvalues(ivalue)%p_SentryList(:,ientry),sbuf)
            CALL appendString(p_sbuf,ilength,TRIM(sbuf))
          END DO
        END IF

      END DO ! ivalue

    END DO ! isection

    ! Allocate a new character array with the correct size, copy p_sbuf to
    ! that ald release the old p_sbuf.
    ! Return NULL() if there is no data.
    NULLIFY(p_sconfiguration)
    IF (ilength .gt. 0) THEN
      ALLOCATE(p_sconfiguration(ilength))
      p_sconfiguration = p_sbuf(1:ilength)
    END IF

    ! Release our temp buffer
    IF (ASSOCIATED(p_sbuf)) DEALLOCATE(p_sbuf)

  CONTAINS

    ! Makes sure, the character buffer points to a character memory block of
    ! size nsize. If not, the block is reallocated to have that size.
    SUBROUTINE assumeBufSize(p_sconfig,nsize)

    CHARACTER, DIMENSION(:), POINTER :: p_sconfig
    INTEGER, INTENT(in) :: nsize

    CHARACTER, DIMENSION(:), POINTER :: p_sconfignew

      IF (.NOT. ASSOCIATED(p_sconfig)) THEN
        ALLOCATE(p_sconfig(nsize))
      ELSE IF (SIZE(p_sconfig) .lt. nsize) THEN
        ALLOCATE(p_sconfignew(nsize))
        p_sconfignew(1:SIZE(p_sconfig)) = p_sconfig
        DEALLOCATE(p_sconfig)
        p_sconfig => p_sconfignew
      END IF

    END SUBROUTINE

    ! Appends sstring to the buffer p_sconfig, followed by a NEWLINE
    ! character. Reallocates memory if necessary.
    SUBROUTINE appendString(p_sconfig,iconfigLength,sstring)

    ! Pointer to character data
    CHARACTER, DIMENSION(:), POINTER :: p_sconfig

    ! In: Current length of data stream in p_sconfig.
    ! Out: New length of data stream in p_sconfig
    INTEGER, INTENT(inout) :: iconfigLength

    ! The string to be added.
    CHARACTER(LEN=*), INTENT(in) :: sstring

      INTEGER :: nblocks,nblocksneeded,i

      ! How many memory blocks do we need for the current configuration?
      ! We work block-wise to prevent too often reallocation.
      IF (.NOT. ASSOCIATED(p_sconfig)) THEN
        nblocks = 0
      ELSE
        nblocks = SIZE(p_sconfig) / SYS_STRLEN
      END IF
      nblocksneeded = 1 + (iconfigLength+LEN(sstring)+1) / SYS_STRLEN
      IF (nblocksneeded .gt. nblocks) THEN
        CALL assumeBufSize(p_sconfig,nblocksneeded*SYS_STRLEN)
      END IF

      ! Append the data
      DO i=1,LEN(sstring)
        iconfigLength = iconfigLength+1
        p_sconfig(iconfigLength) = sstring(i:i)
      END DO

      ! Append NEWLINE as line-end character
      iconfigLength = iconfigLength+1
      p_sconfig(iconfigLength) = NEWLINE

    END SUBROUTINE

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>

  SUBROUTINE parlst_info (rparlist)

!<description>
  ! Prints the parameter list rparlist to the terminal.
!</description>

!<input>
  ! The parameter list which is to be printed to the terminal.
  TYPE(t_parlist), INTENT(in) :: rparlist
!</input>

!</subroutine>


  INTEGER :: isection,ivalue,ientry,icount
  CHARACTER(LEN=PARLST_LENLINEBUF) :: sbuf

    IF (rparlist%isectionCount .eq. 0) THEN
      CALL output_line ('Parameter list not initialised', &
          OU_CLASS_ERROR,OU_MODE_STD,'parlst_info')
      CALL sys_halt()
    END IF

    ! Loop through all sections
    DO isection = 1,rparlist%isectionCount

      ! Append the section name. May be empty for the unnamed section,
      ! which is always the first one.
      IF (isection .gt. 1) THEN
        ! Empty line before
        CALL output_lbrk()
        CALL output_line('['//TRIM(rparlist%p_Rsections(isection)%ssectionName)//']')
      END IF

      ! Loop through the values in the section
      DO ivalue = 1,rparlist%p_Rsections(isection)%iparamCount

        ! Do we have one or multiple entries to that parameter?
        icount = rparlist%p_Rsections(isection)%p_Rvalues(ivalue)%nsize
        IF (icount .eq. 0) THEN
          ! Write "name=value"
          CALL sys_charArrayToString(&
              rparlist%p_Rsections(isection)%p_Rvalues(ivalue)%p_sentry,sbuf)
          CALL output_line(&
            TRIM(rparlist%p_Rsections(isection)%p_Sparameters(ivalue)) &
            //"="//TRIM(sbuf))
        ELSE
          ! Write "name(icount)="
          CALL output_line(&
            TRIM(rparlist%p_Rsections(isection)%p_Sparameters(ivalue)) &
            //"("//TRIM(sys_siL(icount, 10))//")=")
          ! Write all the entries of that value, one each line.
          DO ientry = 1,icount
            CALL sys_charArrayToString(&
                rparlist%p_Rsections(isection)%p_Rvalues(ivalue)% &
                   p_SentryList(:,ientry),sbuf)
            CALL output_line(TRIM(sbuf))
          END DO
        END IF

      END DO ! ivalue

    END DO ! isection

  END SUBROUTINE

  ! ***************************************************************************

!<function>

  INTEGER FUNCTION parlst_findvalue_indir (rsection, sparameter, svalue) &
               RESULT (isubstring)

!<description>
  ! Checks whether the parameter sparameter in the section rsection
  ! has the given value svalue.
!</description>

!<result>
  ! The index of the substring in the parameter sparameter which has value
  ! svalue or =-1, if the parameter does not exist within the section.
!</result>

!<input>

  ! The section where to search for the parameter
  TYPE(t_parlstSection), INTENT(in) :: rsection

  ! The parameter name to search for.
  CHARACTER(LEN=*), INTENT(in) :: sparameter

  ! The value to search for
  CHARACTER(LEN=*), INTENT(in) :: svalue

!</input>

!</function>

    ! local variables
    INTEGER :: idx
    CHARACTER(LEN=PARLST_MLNAME) :: paramname
    CHARACTER(LEN(svalue)) :: sbuf

    IF (sparameter .eq. '') THEN
      CALL output_line ('Empty parameter name!', &
          OU_CLASS_ERROR,OU_MODE_STD,'parlst_lookfor_indir')
      CALL sys_halt()
    END IF

    ! Create the upper-case parameter name
    paramname = ADJUSTL(sparameter)
    CALL sys_toupper (paramname)

    ! Get the parameter index into 'idx', finish.
    CALL parlst_fetchparameter(rsection, paramname, idx)

    ! Check if value svalue exists in some substring and return its
    ! index; of the value does not exist return -1
    IF (idx .eq. 0) THEN
      CALL sys_charArrayToString(&
          rsection%p_Rvalues(idx)%p_sentry, sbuf)
      IF (TRIM(sbuf) .eq. TRIM(svalue)) THEN
        isubstring = 0
      ELSE
        isubstring = -1
      END IF
    ELSE
      DO isubstring = 0, rsection%p_Rvalues(idx)%nsize
        CALL sys_charArrayToString(&
            rsection%p_Rvalues(idx)%p_SentryList(:,isubstring), sbuf)
        IF (TRIM(sbuf) .eq. TRIM(svalue)) THEN
          RETURN
        END IF
      END DO

      ! We did not find the desired value
      isubstring = -1
    END IF

  END FUNCTION

  ! ***************************************************************************

!<function>

  INTEGER FUNCTION parlst_findvalue_direct (rparlist, ssectionName, sparameter, svalue) &
               RESULT (isubstring)

!<description>
  ! Checks whether the parameter sparameter in the section ssectionname
  ! in the parameter list rparlist has the given value.
!</description>

!<result>
  ! The index of the substring in the parameter sparameter which has value
  ! svalue or =-1, if the parameter does not exist within the section.
!</result>

!<input>

  ! The parameter list.
  TYPE(t_parlist), INTENT(in) :: rparlist

  ! The section name - '' identifies the unnamed section.
  CHARACTER(LEN=*), INTENT(in) :: ssectionName

  ! The parameter name to search for.
  CHARACTER(LEN=*), INTENT(in) :: sparameter

  ! The value to search for
  CHARACTER(LEN=*), INTENT(in) :: svalue

!</input>

!</function>

    ! local variables
    INTEGER :: idx
    TYPE(t_parlstSection), POINTER :: p_rsection
    CHARACTER(LEN(svalue)) :: sbuf

    ! Cancel if the list is not initialised.
    IF (rparlist%isectionCount .eq. 0) THEN
      CALL output_line ('Parameter list not initialised!', &
          OU_CLASS_ERROR,OU_MODE_STD,'parlst_findvalue_direct')
      CALL sys_halt()
    END IF

    ! Get the section
    CALL parlst_querysection(rparlist, ssectionName, p_rsection)
    IF (.NOT. ASSOCIATED(p_rsection)) THEN
      CALL output_line ('Section not found: '//TRIM(ssectionName), &
          OU_CLASS_ERROR,OU_MODE_STD,'parlst_findvalue_direct')
      RETURN
    END IF

    ! Get the parameter index
    idx = parlst_queryvalue_indir (p_rsection, sparameter)

    ! Check if value svalue exists in some substring and return its
    ! index; of the value does not exist return -1
    IF (idx .eq. 0) THEN
      CALL sys_charArrayToString(&
          p_rsection%p_Rvalues(idx)%p_sentry, sbuf)
      IF (TRIM(sbuf) .eq. TRIM(svalue)) THEN
        isubstring = 0
      ELSE
        isubstring = -1
      END IF
    ELSE
      DO isubstring = 0, p_rsection%p_Rvalues(idx)%nsize
        CALL sys_charArrayToString(&
            p_rsection%p_Rvalues(idx)%p_SentryList(:,isubstring), sbuf)
        IF (TRIM(sbuf) .eq. TRIM(svalue)) THEN
          RETURN
        END IF
      END DO

      ! We did not find the desired value
      isubstring = -1
    END IF

  END FUNCTION

  ! ***************************************************************************

!<subroutine>

  SUBROUTINE parlst_expandEnvVariables(rparlist)

!<description>
  ! This subroutine expands variables when referring to subvariables:
  ! The value of a parameter may refer to another variable in the parameter
  ! list. This can be specified by tokens of the form "%{NAME}" or "{SECTION.NAME}",
  ! depending on whether it is part of the main section or not.
  ! Example: "NLMIN = %{NLMAX}"; in this case, NLMIN is always set to
  ! the same value as NLMAX.
  !
  ! The routine parses all variables in the DAT file to resolve such
  ! references. Note that recursive definitions are not allowed!
!</description>

!<inputoutput>
  ! The parameter list which is filled with data from the file
  TYPE(t_parlist), INTENT(inout) :: rparlist
!</inputoutput>

!</subroutine>

    INTEGER :: isection,ivalue,ientry,icount,j
    CHARACTER(LEN=PARLST_LENLINEBUF) :: sbuf

    IF (rparlist%isectionCount .eq. 0) THEN
      CALL output_line ('Parameter list not initialised', &
          OU_CLASS_ERROR,OU_MODE_STD,'parlst_expandEnvVariables')
      CALL sys_halt()
    END IF

    ! Loop through all sections
    DO isection = 1,rparlist%isectionCount

      ! Loop through the values in the section
      DO ivalue = 1,rparlist%p_Rsections(isection)%iparamCount

        ! Do we have one or multiple entries to that parameter?
        icount = rparlist%p_Rsections(isection)%p_Rvalues(ivalue)%nsize
        IF (icount .eq. 0) THEN
          ! Expand the value if is refers to subvalues.
          CALL sys_charArrayToString(&
              rparlist%p_Rsections(isection)%p_Rvalues(ivalue)%p_sentry,sbuf)
          CALL parlst_expandEnvVariable(sbuf)

          j = LEN_TRIM(sbuf)
          DEALLOCATE(rparlist%p_Rsections(isection)%p_Rvalues(ivalue)%p_sentry)
          ALLOCATE(rparlist%p_Rsections(isection)%p_Rvalues(ivalue)%p_sentry(j))
          rparlist%p_Rsections(isection)%p_Rvalues(ivalue)%p_sentry(:) = ' '
          CALL sys_stringToCharArray(sbuf,&
              rparlist%p_Rsections(isection)%p_Rvalues(ivalue)%p_sentry,j)
        ELSE
          ! Loop through the subvalues.
          DO ientry = 1,icount
            ! Expand the value if is refers to subvalues.
            CALL sys_charArrayToString(&
                rparlist%p_Rsections(isection)%p_Rvalues(ivalue)%p_SentryList(:,ientry),sbuf)
            CALL parlst_expandEnvVariable(sbuf)

            ! Reallocate before writing back if necessary
            j = LEN_TRIM(sbuf)
            IF (j .gt. UBOUND(rparlist%p_Rsections(isection)%p_Rvalues(ivalue)%p_SentryList,1)) THEN
              CALL parlst_reallocSubVariables(rparlist%p_Rsections(isection)%p_Rvalues(ivalue),j)
            END IF
            CALL sys_stringToCharArray(sbuf,&
                rparlist%p_Rsections(isection)%p_Rvalues(ivalue)%p_SentryList(:,ientry),j)
          END DO
        END IF

      END DO ! ivalue

    END DO ! isection

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>

  SUBROUTINE parlst_expandEnvVariable(sbuffer)

  !<description>
    ! This subroutine recursively expands all environment variables in the given
    ! string sbuffer.
  !</description>

  !<input>
    ! string; all environment variables in here are replaced
    CHARACTER(LEN=*), INTENT(inout) :: sbuffer
  !</input>
!</subroutine>

    ! flag
    LOGICAL :: bfoundInEnv

    ! start and end position of variable
    INTEGER(I32) :: istartPos, istopPosRelative

    ! variable to expand environment variable on-the-fly to if found
    CHARACTER(LEN=SYS_STRLEN) :: sauxEnv

    ! Buffer for the result
    CHARACTER(LEN=PARLST_LENLINEBUF) :: sresult

    ! Initialise return value
    sresult = TRIM(sbuffer)

    ! check for a $ character
    istartPos = INDEX(sresult, "$")
    DO WHILE (istartPos .gt. 0)
      ! Detect end of variable: a variable ends at the first character that
      ! is neither in '[A-Z]', '[0-9]' nor '_'.
      istopPosRelative = VERIFY(sresult(istartPos+1:), &
                          "abcdefghijklmnopqrstuvwxyz" // &
                          "ABCDEFGHIJKLMNOPQRSTUVWXYZ" // &
                          "0123456789_")

      bfoundInEnv = .false.
      ! Retrieve value of environment variable
      ! (Do not forget to cut the dollar sign.)
      bfoundInEnv = &
          sys_getenv_string(TRIM(&
              sresult(istartPos + 1 : istartPos + istopPosRelative - 1)), sauxEnv)
      IF (bfoundInEnv) THEN
        ! Replace environment variable by its content
        sresult = sresult(1:istartPos-1) // &
                  TRIM(sauxEnv) // &
                  TRIM(sresult(istartPos + istopPosRelative:))
      ELSE
        CALL output_line ('Environment variable <'//&
            TRIM(sresult(istartPos + 1 : istartPos + istopPosRelative - 1))//&
            '> not found!',&
            OU_CLASS_ERROR,OU_MODE_STD,'parlst_expandEnvVariables')
        CALL sys_halt()
      END IF

      ! check for next $ character
      istartPos = INDEX(sresult, "$")
    ENDDO

    ! Replace by the result
    sbuffer = sresult

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>

  SUBROUTINE parlst_expandSubvars(rparlist)

!<description>
  ! This subroutine expands variables when referring to subvariables:
  ! The value of a parameter may refer to another variable in the parameter
  ! list. This can be specified by tokens of the form "%{NAME}" or "{SECTION.NAME}",
  ! depending on whether it is part of the main section or not.
  ! Example: "NLMIN = %{NLMAX}"; in this case, NLMIN is always set to
  ! the same value as NLMAX.
  !
  ! The routine parses all variables in the DAT file to resolve such
  ! references. Note that recursive definitions are not allowed!
!</description>

!<inputoutput>
  ! The parameter list which is filled with data from the file
  TYPE(t_parlist), INTENT(inout) :: rparlist
!</inputoutput>

!</subroutine>

    INTEGER :: isection,ivalue,ientry,icount,j
    CHARACTER(LEN=PARLST_LENLINEBUF) :: sbuf

    IF (rparlist%isectionCount .eq. 0) THEN
      CALL output_line ('Parameter list not initialised', &
          OU_CLASS_ERROR,OU_MODE_STD,'parlst_expandSubvars')
      CALL sys_halt()
    END IF

    ! Loop through all sections
    DO isection = 1,rparlist%isectionCount

      ! Loop through the values in the section
      DO ivalue = 1,rparlist%p_Rsections(isection)%iparamCount

        ! Do we have one or multiple entries to that parameter?
        icount = rparlist%p_Rsections(isection)%p_Rvalues(ivalue)%nsize

        IF (icount .eq. 0) THEN
          ! Expand the value if is refers to subvalues.
          CALL sys_charArrayToString(&
              rparlist%p_Rsections(isection)%p_Rvalues(ivalue)%p_sentry,sbuf)
          CALL parlst_expandSubvariable(rparlist,sbuf)

          j = LEN_TRIM(sbuf)
          DEALLOCATE(rparlist%p_Rsections(isection)%p_Rvalues(ivalue)%p_sentry)
          ALLOCATE(rparlist%p_Rsections(isection)%p_Rvalues(ivalue)%p_sentry(j))
          rparlist%p_Rsections(isection)%p_Rvalues(ivalue)%p_sentry(:) = ' '
          CALL sys_stringToCharArray(sbuf,&
              rparlist%p_Rsections(isection)%p_Rvalues(ivalue)%p_sentry,j)
        ELSE
          ! Loop through the subvalues.
          DO ientry = 1,icount
            ! Expand the value if is refers to subvalues.
            CALL sys_charArrayToString(&
                rparlist%p_Rsections(isection)%p_Rvalues(ivalue)%p_SentryList(:,ientry),sbuf)
            CALL parlst_expandSubvariable(rparlist,sbuf)

            ! Reallocate before writing back if necessary
            j = LEN_TRIM(sbuf)
            IF (j .gt. UBOUND(rparlist%p_Rsections(isection)%p_Rvalues(ivalue)%p_SentryList,1)) THEN
              CALL parlst_reallocSubVariables(rparlist%p_Rsections(isection)%p_Rvalues(ivalue),j)
            END IF
            CALL sys_stringToCharArray(sbuf,&
                rparlist%p_Rsections(isection)%p_Rvalues(ivalue)%p_SentryList(:,ientry),j)
          END DO

        END IF

      END DO ! ivalue

    END DO ! isection

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>

  SUBROUTINE parlst_findSubvariable(sstring,istart,iend,ssection,sname,ivalue)

!<description>
  ! Searchs in the string sstring for the first occurance of a subvariable.
  ! A subvariable has the form "%{NAME}" or "{SECTION.NAME}"
  ! or "%{NAME:INDEX}" or "%{SECTION.NAME:INDEX}"
  ! depending on whether it is part of the main section or not.
!</description>

!<input>
  ! The string where a subvariable is searched.
  CHARACTER(LEN=*), INTENT(in) :: sstring
!</input>

!<output>
  ! Returns the start of the variable in the string or 0 if no subvariable
  ! is found.
  INTEGER, INTENT(out) :: istart

  ! Returns the end of the variable in the string or 0 if no subvariable
  ! is found.
  INTEGER, INTENT(out) :: iend

  ! Returns the name of the section or "" if either no subvariable is found
  ! or the unnamed section is referred to.
  CHARACTER(LEN=*), INTENT(out) :: ssection

  ! Returns the name of the subvariable or "" if no subvariable is found.
  CHARACTER(LEN=*), INTENT(out) :: sname

  ! Returns the number/index INDEX of the subvalue or 0, if there is
  ! no index or if no subvariable is found.
  INTEGER, INTENT(out) :: ivalue
!</output>

!</subroutine>

  ! local variables
  INTEGER :: i,j,istrlen,idotpos,icolonpos
  LOGICAL :: bstartfound

    ! Ok, this is a parser. We have the following rules:
    ! "%%" means one "%" and is not interpreted as the begin of a
    ! token.
    ! "%{NAME}" is a token referring to a variable in the unnamed section.
    ! "%{NAME:INDEX}" is a token referring to a subvariable
    ! of variable NAME in section SECTION.
    ! "%{SECTION.NAME}" is a token referring to a name in a named section
    ! which must exist.
    ! "%{SECTION.NAME:INDEX}" is a token referring to a subvariable
    ! of variable NAME in section SECTION.
    !
    istart = 0
    iend = 0
    ivalue = 0
    bstartfound = .false.

    ! Lets loop through the characters.
    istrlen = LEN(sstring)
    DO i=1,istrlen
      IF (sstring(i:i) .eq. "%") THEN
        ! Did we already found the "%"?
        IF (bstartfound) THEN
          ! That is our escape sequence. Do not do anything, just
          ! return to 'normal' mode.
          bstartfound = .false.
        ELSE
          ! That is probably the beginning of a token.
          bstartfound = .true.
        END IF

        ! Go on.
        CYCLE
      END IF

      ! The next things only execute if we are close to a token...
      IF (bstartfound) THEN
        IF (sstring(i:I) .eq. "{") THEN
          ! Yes, that is a token.
          istart = i-1

          ! Find the end of the token and probably the dot/colon
          idotpos = 0
          icolonpos = 0
          DO j=istart+1,istrlen
            IF (sstring(j:J) .eq. ".") THEN
              ! Here is the dot.
              idotpos = j
            END IF

            IF (sstring(j:J) .eq. ":") THEN
              ! Here is the dot.
              icolonpos = j
            END IF

            IF (sstring(j:j) .eq. "}") THEN
              ! Here, the token ends.
              iend = j

              ! Extract name and probably the section
              IF (idotpos .eq. 0) THEN
                ssection = ""
                IF (icolonpos .eq. 0) THEN
                  sname = sstring(istart+2:iend-1)
                ELSE
                  sname = sstring(istart+2:icolonpos-1)

                  ! Get the value
                  READ(sstring(icolonpos+1:iend-1),*) ivalue
                END IF
              ELSE
                ssection = sstring(istart+2:idotpos-1)
                IF (icolonpos .eq. 0) THEN
                  sname = sstring(idotpos+1:iend-1)
                ELSE
                  sname = sstring(idotpos+1:icolonpos-1)

                  ! Get the value
                  READ(sstring(icolonpos+1:iend-1),*) ivalue
                END IF
              END IF

              ! That is it.
              RETURN

            END IF
          END DO
        END IF
      END IF
    END DO

    ! Nothing found.
    ssection = ""
    sname = ""

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>

  SUBROUTINE parlst_expandSubvariable(rparlist,sstring)

!<description>
  ! Expands the subvariables in sstring to a fully qualified string.
!</description>

!<input>
  ! The parameter list containing the variables that can be used
  ! as subvariables.
  TYPE(t_parlist), INTENT(in) :: rparlist
!</input>

!<inputoutput>
  ! The string to be expanded. Receives the expanded string upon return.
  CHARACTER(LEN=*), INTENT(inout) :: sstring
!</inputoutput>

!</subroutine>

    ! local variables
    INTEGER :: istart,iend,ivalue
    CHARACTER(LEN=PARLST_MLSECTION) :: ssection
    CHARACTER(LEN=PARLST_MLNAME) :: sname
    CHARACTER(LEN=LEN(sstring)) :: sbuffer
    CHARACTER(LEN=PARLST_LENLINEBUF) :: sdata

    ! Repeat until we found all subvariables
    istart = 1
    iend = 0
    DO WHILE (istart .ne. 0)

      ! Find the first subvariable
      CALL parlst_findSubvariable(sstring,istart,iend,ssection,sname,ivalue)

      IF (istart .ne. 0) THEN
        ! Copy the string to the buffer
        sbuffer = sstring

        ! Now copy back and replace the variable by the stuff from the
        ! parameter list.
        IF (ivalue .eq. 0) THEN
          CALL parlst_getvalue_string (rparlist, ssection, sname, sdata, bdequote=.true.)
        ELSE
          CALL parlst_getvalue_string (rparlist, ssection, sname, sdata, &
              isubstring=ivalue, bdequote=.true.)
        END IF
        sstring = sbuffer(1:istart-1)//TRIM(sdata)//sbuffer(iend+1:)

      END IF

    END DO

  END SUBROUTINE

  ! ***************************************************************************

!<subroutine>

  SUBROUTINE parlst_dumpToFile(rparlist, sfilename, cflag)

!<description>
   ! This subroutine dumps a given parameter list into a text file in
   ! INI-file form. Note that no additional comments are exported but
   ! just the tuples `parameter = value` in the corresponding sections.
!</description>

!<input>

  ! The parameter list.
  TYPE(t_parlist), INTENT(in) :: rparlist

  ! The name of the output file
  CHARACTER(*), INTENT(in) :: sfilename

  ! mode: SYS_APPEND or SYS_REPLACE
  INTEGER, INTENT(in) :: cflag

!</input>
!</subroutine>

    ! local variables
    CHARACTER(LEN=PARLST_LENLINEBUF) :: sbuf
    INTEGER :: iunit,isection,ivalue,ientry,icount

    IF (rparlist%isectionCount .eq. 0) THEN
      CALL output_line ('Parameter list not initialised', &
          OU_CLASS_ERROR,OU_MODE_STD,'parlst_dumpToFile')
      CALL sys_halt()
    END IF

    ! Open file for output
    CALL io_openFileForWriting(sfilename, iunit, cflag, bformatted=.true.)
    IF (iunit .eq. -1) THEN
      CALL output_line ('Unable to open file for output!', &
          OU_CLASS_ERROR,OU_MODE_STD,'parlst_dumpToFile')
      CALL sys_halt()
    END IF

    ! Loop through all sections
    DO isection = 1,rparlist%isectionCount

      ! Append the section name. May be empty for the unnamed section,
      ! which is always the first one.
      IF (isection .gt. 1) THEN
        ! Empty line before
        WRITE(iunit,*)
        WRITE(iunit,*) '['//TRIM(rparlist%p_Rsections(isection)%ssectionName)//']'
      END IF

      ! Loop through the values in the section
      DO ivalue = 1,rparlist%p_Rsections(isection)%iparamCount

        ! Do we have one or multiple entries to that parameter?
        icount = rparlist%p_Rsections(isection)%p_Rvalues(ivalue)%nsize
        IF (icount .eq. 0) THEN
          ! Write "name=value"
          CALL sys_charArrayToString(&
              rparlist%p_Rsections(isection)%p_Rvalues(ivalue)%p_sentry,sbuf)
          WRITE(iunit,*)&
              TRIM(rparlist%p_Rsections(isection)%p_Sparameters(ivalue)) &
              //" = "//TRIM(sbuf)
        ELSE
          ! Write "name(icount)="
          WRITE(iunit,*)&
              TRIM(rparlist%p_Rsections(isection)%p_Sparameters(ivalue)) &
              //"("//TRIM(sys_siL(icount, 10))//") = "
          ! Write all the entries of that value, one each line.
          DO ientry = 1,icount
            CALL sys_charArrayToString(&
                rparlist%p_Rsections(isection)%p_Rvalues(ivalue)% &
                p_SentryList(:,ientry),sbuf)
            WRITE(iunit,*) TRIM(sbuf)
          END DO
        END IF

      END DO ! ivalue

    END DO ! isection

    ! Close file
    CLOSE(iunit)

  END SUBROUTINE parlst_dumpToFile

END MODULE ModLib_FeatFlow_ParamList
