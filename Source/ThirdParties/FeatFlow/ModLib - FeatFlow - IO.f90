!!##############################################################################
!!# ****************************************************************************
!!# <name> io </name>
!!# ****************************************************************************
!!#
!!# <purpose>
!!# This module contains several routines for input/output purposes.
!!#
!!# The following routines can be found here:
!!#
!!# 1.) io_openFileForReading
!!#     -> Opens a file for reading; the file handle is automatically determined
!!#
!!# 2.) io_openFileForWriting
!!#     -> Opens a file for writing; the file handle is automatically determined
!!#
!!# 3.) io_readlinefromfile
!!#     -> Reads one line from an opend file
!!#
!!# 4.) io_deleteFile
!!#     -> Deletes a file.
!!#
!!# 5.) io_pathExtract
!!#     -> Extracts path information from a filename
!!#
!!# 6.) io_pathConcat
!!#     -> Concatenates a filename to a path.
!!#
!!# 7.) io_isDirectory
!!#     -> Checks whether a given string is a directory
!!#
!!# 8.) io_makeDirectory
!!#     -> Creates a directory (including parent directories if needed)
!!#
!!# </purpose>
!!##############################################################################

MODULE ModLib_FeatFlow_IO

!$ use omp_lib
  USE ModLib_FeatFlow_System
  USE ModLib_FeatFlow_GenOutput

  IMPLICIT NONE

  PRIVATE

!<constants>

  !<constantblock description="Input/output block type identifiers">

  ! defines the default value for files
  INTEGER, PARAMETER, PUBLIC :: IO_UNKNOWN = 0

  ! defines that a file must already exist
  INTEGER, PARAMETER, PUBLIC :: IO_OLD = 1

  ! defines that a file must not exist
  INTEGER, PARAMETER, PUBLIC :: IO_NEW = 2

  ! defines that an existing file should be replaced
  INTEGER, PARAMETER, PUBLIC :: IO_REPLACE = 3

  ! defines that a temporary file should be deleted when closed
  INTEGER, PARAMETER, PUBLIC :: IO_SCRATCH = 4

  !</constantblock>

!</constants>

  PUBLIC :: io_openFileForReading
  PUBLIC :: io_openFileForWriting
  PUBLIC :: io_readlinefromfile
  PUBLIC :: io_deleteFile
  PUBLIC :: io_pathExtract
  PUBLIC :: io_pathConcat
  PUBLIC :: io_isDirectory

CONTAINS

!************************************************************************************

!<subroutine>
  SUBROUTINE io_openFileForReading(sfilename, iunit, bformatted)

!<description>
    !This routine tries to open a file for reading. If succesful, on can read from it
    !via unit "iunit". Otherwise, iunit is -1.
!</description>

!<input>

    !filename
    CHARACTER(*), INTENT(in) :: sfilename

    ! OPTIONAL:
    ! TRUE : Open the file formatted, i.e. in human readable form
    ! FALSE: Open the file in unformatted, machine dependent form
    ! If not specified, the default system dependent setting is used.
    LOGICAL, INTENT(in), OPTIONAL :: bformatted

!</input>

!<output>

    !number of unit
    INTEGER, INTENT(out) :: iunit
!</output>
!</subroutine>

    LOGICAL :: bexists !true, if a file with name sfilename exists
    INTEGER :: istatus !status variable for opening. 0, if opening succesful


    IF (TRIM(sfilename) .eq. "") THEN
      CALL output_line('File name <'//TRIM(ADJUSTL(sfilename))//'> empty.', &
                       OU_CLASS_ERROR,OU_MODE_STD,'io_openFileForReading')
      RETURN
    END IF

    iunit = sys_getFreeUnit()
    IF (iunit .eq. -1) THEN
      CALL output_line('No free unit found. Not able to open the file.', &
                       OU_CLASS_ERROR,OU_MODE_STD,'io_openFileForReading')
      !give it up
      RETURN
    END IF

    INQUIRE(file=TRIM(sfilename), exist=bexists)

    IF (bexists) THEN

      IF (.NOT. PRESENT(bformatted)) THEN
        OPEN(unit=iunit, file=TRIM(sfilename), iostat=istatus, action="read")
      ELSE IF (bformatted) THEN
        OPEN(unit=iunit, file=TRIM(sfilename), iostat=istatus, action="read",&
             form="formatted")
      ELSE
        OPEN(unit=iunit, file=TRIM(sfilename), iostat=istatus, action="read",&
             form="unformatted")
      END IF
      IF (istatus .ne. 0) THEN
        CALL output_line('Error while opening file <'//TRIM(ADJUSTL(sfilename))//'> for reading.', &
                         OU_CLASS_ERROR,OU_MODE_STD,'io_openFileForReading')
        iunit = -1
      END IF

    ELSE

      CALL output_line('File <'//TRIM(ADJUSTL(sfilename))//'> does not exist.', &
                       OU_CLASS_ERROR,OU_MODE_STD,'io_openFileForReading')
      CALL sys_halt()
    END IF

  END SUBROUTINE io_openFileForReading

!************************************************************************


!<subroutine>
  SUBROUTINE io_openFileForWriting(sfilename, iunit, cflag, bfileExists, bformatted)

!<description>
    !This routine tries to open a file for writing. If succesful, one can write to it
    !via unit "iunit". Otherwise, iunit is -1. cflag specifies if an already existing file
    !should be replaced or if the output should be appended. bfileExists is an optional
    !parameter, which will be set to true, if the file already existed, otherwise false.
!</description>

!<input>

    !filename
    CHARACTER(*), INTENT(in) :: sfilename

    !mode: SYS_APPEND or SYS_REPLACE
    INTEGER, INTENT(in) :: cflag

    ! OPTIONAL:
    ! TRUE : Open the file formatted, i.e. in human readable form
    ! FALSE: Open the file in unformatted, machine dependent form
    ! If not specified, the default system dependent setting is used.
    LOGICAL, INTENT(in), OPTIONAL :: bformatted
!</input>

!<output>

    ! unit of the opened file
    INTEGER, INTENT(out) :: iunit

    ! optional parameter (see description)
    LOGICAL, INTENT(out), OPTIONAL :: bfileExists

!</output>
!</subroutine>

    LOGICAL :: bexists !true, if the file to be written in exists
    INTEGER :: istatus !status variable for opening procedure

    ! the result "dirname(sfilename)" would yield
    CHARACTER(LEN=LEN(sfilename)) :: sfilepath

    IF (LEN_TRIM(sfilename) .eq. 0) THEN
      CALL output_line('File name <'//TRIM(ADJUSTL(sfilename))//'> empty.', &
                       OU_CLASS_ERROR,OU_MODE_STD,'io_openFileForWriting')
      RETURN
    END IF

    iunit = sys_getFreeUnit()
    IF (iunit .eq. -1) THEN
      CALL output_line('No free unit found. Not able to open the file.', &
                       OU_CLASS_ERROR,OU_MODE_STD,'io_openFileForWriting')
      RETURN
    END IF

    INQUIRE(file=TRIM(sfilename), exist=bexists)
    IF (.NOT. PRESENT(bformatted)) THEN
      IF (bexists .AND. cflag .eq. SYS_REPLACE) THEN
        OPEN(unit=iunit, file=TRIM(sfilename), iostat=istatus, status="replace", &
            action="write")
      ELSE
        ! Ensure that the path up to the given file name does exist
        CALL io_pathExtract(sfilename, sfilepath)
        IF (.NOT. io_isDirectory(sfilepath)) THEN
          CALL io_makeDirectory(sfilepath)
        END IF
        OPEN(unit=iunit, file=TRIM(sfilename), iostat=istatus, action="write", &
            position="append")
      END IF
    ELSE
      IF (bexists .AND. cflag .eq. SYS_REPLACE) THEN
        IF (bformatted) THEN
          OPEN(unit=iunit, file=TRIM(sfilename), iostat=istatus, status="replace", &
              action="write", form="formatted")
        ELSE
          OPEN(unit=iunit, file=TRIM(sfilename), iostat=istatus, status="replace", &
              action="write", form="unformatted")
        END IF
      ELSE
        ! Ensure that the path up to the given file name does exist. If it does not,
        ! create it
        CALL io_pathExtract(sfilename, sfilepath)
        IF (.NOT. io_isDirectory(sfilepath)) THEN
          CALL io_makeDirectory(sfilepath)
        END IF
        IF (bformatted) THEN
          OPEN(unit=iunit, file=TRIM(sfilename), iostat=istatus, action="write", &
              position="append", form="formatted")
        ELSE
          OPEN(unit=iunit, file=TRIM(sfilename), iostat=istatus, action="write", &
              position="append", form="unformatted")
        END IF
      END IF
    END IF
    IF (PRESENT(bfileExists)) THEN
      bfileExists = bexists
    END IF
    IF (istatus .ne. 0) THEN
      CALL output_line('Error while opening file <'//TRIM(ADJUSTL(sfilename))//'> for writing.', &
                         OU_CLASS_ERROR,OU_MODE_STD,'io_openFileForWriting')
      iunit = -1
    END IF

  END SUBROUTINE io_openFileForWriting

! ***************************************************************************

!<subroutine>
  SUBROUTINE io_deleteFile(sfilename)

!<description>
  ! this routione deletes a file sfilename.
!</description>

!<input>
  ! filename
  CHARACTER(*), INTENT(in) :: sfilename
!</input>
!</subroutine>

    INTEGER :: iunit

    ! Open the file for writing, overwrite the old one.
    CALL io_openFileForWriting(sfilename, iunit, SYS_REPLACE)

    ! Close the file and delete it.
    CLOSE (iunit, STATUS='DELETE')

  END SUBROUTINE io_deleteFile

! ***************************************************************************

!<subroutine>

  SUBROUTINE io_readlinefromfile (iunit, sdata, ilinelen, ios)

!<description>
  !This routine reads a line from a text file
!</description>

!<input>
    ! The unit where to read from; must be connected to a file.
    INTEGER, INTENT(in) :: iunit
!</input>

!<output>
    ! The string where to write data to
    CHARACTER(LEN=*), INTENT(out) :: sdata

    ! Length of the output
    INTEGER, INTENT(out) :: ilinelen

    ! Status of the reading process. Set to a value <> 0 if the end
    ! of the file is reached.
    INTEGER, INTENT(out) :: ios
!</output>
!</subroutine>

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
10    ios = -1
      EXIT

      ! End of record = END OF LINE.
20    ios = 0
      EXIT

    END DO

  END SUBROUTINE io_readlinefromfile

  ! ***************************************************************************

  !<subroutine>

  SUBROUTINE io_pathExtract (sfile, sfilepath, sfilename, babsolute)

  !<description>
    ! Extracts the path of a file from a path+filename string.
  !</description>

  !<input>
    ! Filename + path of a specific file (or directory).
    CHARACTER(LEN=*), INTENT(in) :: sfile
  !</input>

  !<output>
    ! OPTIONAL: Receives the directory that contains the specific file,
    ! or "" if no directory was specified in sfile.
    CHARACTER(LEN=*), INTENT(out), OPTIONAL :: sfilepath

    ! OPTIONAL: Receives the name of the file without a probably preceding
    ! directory string.
    CHARACTER(LEN=*), INTENT(out), OPTIONAL :: sfilename

    ! OPTIONAL: Returns TRUE if the path specification in sfile points to an
    ! absolute path. Returns FALSE if the path in sfile is relative.
    LOGICAL, INTENT(out), OPTIONAL :: babsolute
  !</output>

  !</subroutine>

    INTEGER :: i
    CHARACTER(LEN=10) :: ssubpath

    ! Find the last "/" or "\" in sfile.                                (!" cpp fix)
    ! Note that we specified "\\" and not "\" because the PGI compiler  (!" cpp fix)
    ! (stupid thing) would otherwise use the backslash to escape the quote
    ! character. So PGI sees "/\" and other compiler see "/\\", but this (!" cpp fix)
    ! does not matter since the string must only contain a couple of
    ! delimiters which may occur more than once in the string.
    i = SCAN(sfile,"/\\",.true.)
    IF (i .ne. 0) THEN
      ! Directory ends at position i.
      IF (PRESENT(sfilepath)) sfilepath = sfile(1:i-1)
      IF (PRESENT(sfilename)) sfilename = sfile(i+1:)
    ELSE
      ! No directory specified.
      IF (PRESENT(sfilepath)) sfilepath = ""
      IF (PRESENT(sfilename)) sfilename = sfile
    END IF

    IF (PRESENT(babsolute)) THEN
      ! Take a look if this is an absolute or relative path.
      i = SCAN(TRIM(ADJUSTL(sfile)),"/\\",.false.)
      babsolute = i .eq. 1

      ! In Windows environments, the path is also absolute if
      ! a volume descriptor like "C:" precedes the (back-)slash.
      IF (.NOT. babsolute) THEN
        IF (i .eq. 3) THEN
          ! Extract the first 10 characters and check
          ssubpath = TRIM(ADJUSTL(sfile))
          IF (ssubpath(2:2) .eq. ":") THEN
            babsolute = .true.
          END IF
        END IF
      END IF
    END IF

  END SUBROUTINE

  ! ***************************************************************************

  !<function>

  FUNCTION io_pathConcat (spath,sfilename) RESULT (sfile)

  !<description>
    ! Concatenates a filename to a path specifier.
  !</description>

  !<input>
    ! Path to the file.
    CHARACTER(LEN=*), INTENT(in) :: spath

    ! Name of the file (or directory)
    CHARACTER(LEN=*), INTENT(in) :: sfilename
  !</input>

  !<result>
    ! Path + filename to a specific file (or directory).
    CHARACTER(LEN=LEN_TRIM(spath)+LEN_TRIM(sfilename)+1) :: sfile
  !</result>

  !</function>

    sfile = TRIM(spath)//"/"//TRIM(sfilename)

  END FUNCTION


  ! ***************************************************************************


!<function>
  FUNCTION io_isDirectory (spath) RESULT (bisDir)

  !<description>
    ! Checks whether a given string is a directory
  !</description>

  !<input>
    ! Path to the file.
    CHARACTER(LEN=*), INTENT(in) :: spath
  !</input>

  !<result>
    ! Is .TRUE. if the given string is an existing directory
    LOGICAL :: bisDir
  !</result>

!</function>

    INTEGER(I32) :: ierr
    INTEGER(I32) :: iisdir

    ! use wrapper for C system call stat() in kernel/System/isdirectory.c
!    EXTERNAL isdirectory
    ierr = 0
    bisDir = .false.
!    CALL isdirectory(TRIM(spath) // ACHAR(0), iisdir, ierr)
    IF (ierr .ge. 0) THEN
      bisDir = (iisdir .gt. 0)
    END IF

  END FUNCTION io_isDirectory


  ! ***************************************************************************


!<subroutine>
  SUBROUTINE io_makeDirectory (spath)

  !<description>
    ! Portably creates directories (even recursively)
  !</description>

  !<input>
    ! Path to the file.
    CHARACTER(LEN=*), INTENT(in) :: spath
  !</input>

!</subroutine>

!    EXTERNAL mkdir_recursive

    ! status variable for (recursive) directory creation
    ! > 0: number of subdirectories created
    ! < 0: -errno
    INTEGER :: istatus


    IF (LEN_TRIM(spath) .eq. 0) THEN
      CALL output_line('Path name <'//TRIM(ADJUSTL(spath))//'> empty.', &
                       OU_CLASS_ERROR,OU_MODE_STD,'io_makeDirectory')
      RETURN
    END IF

    ! Ensure to explicitly NULL-terminate the string when calling C function!
!    CALL mkdir_recursive(TRIM(spath) // ACHAR(0), istatus)
    IF (istatus .lt. 0) THEN
      CALL output_line('Could not (auto-)create path <'//TRIM(ADJUSTL(spath))//'>.', &
                       OU_CLASS_ERROR,OU_MODE_STD,'io_makeDirectory')
      CALL sys_halt()
    END IF

  END SUBROUTINE io_makeDirectory

END MODULE ModLib_FeatFlow_IO
