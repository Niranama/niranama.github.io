!!##############################################################################
!!# ****************************************************************************
!!# <name> genoutput </name>
!!# ****************************************************************************
!!#
!!# <purpose>
!!# This module contains several routines for input/output of messages to the
!!# terminal and/or to log files.
!!#
!!# The following routines can be found here:
!!#
!!# 1.) output_init
!!#     -> Initialises the output. Opens a log file and assigns it an
!!#        output channel OU_LOG.
!!#
!!# 2.) output_line
!!#     -> Writes a message to the terminal and/or the log file.
!!#
!!# 3.) output_separator
!!#     -> Writes a separator line to the terminal and/or the log file.
!!#
!!# 4.) output_lbrk
!!#     -> Writes an empty line / line break to the terminal and/or the log
!!#        file.
!!#
!!# 5.) output_simple
!!#     -> Writes a message to the terminal and/or the log file.
!!#        For old FEAT compatibility. Uses a priority identifier to decide on
!!#        whether to write only to the log file or to both, log file
!!#        and terminal.
!!#
!!# 6.) output_simple_sep
!!#     -> Writes a separator line to the terminal and/or the log file.
!!#        For old FEAT compatibility. Uses a priority identifier to decide on
!!#        whether to write only to the log file or to both, log file
!!#        and terminal.
!!#
!!# 7.) output_multiline
!!#     -> Writes a formatted message to the terminal and/or the log file.
!!#
!!# 8.) output_done
!!#     -> Closes the log file, releases all ressources in use.
!!#
!!# HOW TO USE:
!!# -----------
!!# As long as output_init is not called, all output is redirected only to the
!!# standard output channel. Directly after program start, output_init should
!!# be called so that the log file is written.
!!#
!!# Proceed as follows:
!!#
!!# <verb>
!!# 1.) call output_init ()
!!#     -> Initialise the output system for output on the terminal
!!#
!!#     Alternatively one can use:
!!#
!!#     call output_init ('mylogfile.txt')
!!#     -> Opens a log file 'mylogfile.txt' for the output additional to
!!#        terminal output
!!#
!!#     Alternatively one can use
!!#
!!#     call output_init ('mylogfile.txt','myerrorlogfile.txt')
!!#     -> Opens a log file 'mylogfile.txt' for the output and
!!#        'myerrorlogfile.txt' for error output (both additionally to
!!#        terminal output).
!!#
!!# 2.) call output_line ('This is a message')
!!#
!!#     -> Writes a message to the terminal. If output_init was used
!!#        before, output_line_std will also write the message to the log file.
!!#
!!#     Alternatively, one can specify where to write output to by using
!!#     different variants of output_line:
!!#
!!#     a) call output_lbrk ()
!!#
!!#     -> Writes an empty line to the terminal and log file. This is the same
!!#        as call output_line ('').
!!#
!!#     b) call output_line('A message only to the terminal.', &
!!#                         OU_CLASS_MSG,OU_MODE_TERM)
!!#
!!#     -> Writes a message only to the terminal
!!#
!!#     c) call output_line ('A message only to the log file.', &
!!#                          OU_CLASS_MSG,OU_MODE_LOG)
!!#
!!#     -> Writes a message only to the log file
!!#
!!#     d) call output_line ('A special debug message.', &
!!#                          OU_CLASS_TRACE1,OU_MODE_STD,'mysubroutine')
!!#
!!#     -> Writes a debug message with '*** (mysubroutine):' in front to the
!!#        terminal and the log file. This is usually used for debug purposes.
!!#
!!#     e) call output_line ('This is an error message.', &
!!#                          OU_CLASS_ERROR,OU_MODE_STD,'mysubroutine')
!!#
!!#     -> Writes a debug message with 'Error (mysubroutine):' in front to the
!!#        error log file and error output channel.
!!#
!!#       or even simpler:
!!#
!!#        call output_line ('This is an error message.',OU_CLASS_ERROR)
!!#        call output_line ('This is an warning message.',OU_CLASS_WARNING)
!!#
!!#     f) call output_separator (OU_SEP_MINUS)
!!#
!!#     -> Writes a separation line with '-' signs to the terminal / log file
!!#
!!#     g) MT = 1
!!#        call output_simple (MT,'A log file message.')
!!#
!!#     -> Writes a message to the log file, not to the terminal. FEAT1.0
!!#        compatibility routine for "MT=1"
!!#
!!#     h) MT = 2
!!#        call output_simple (MT,'A log file message')
!!#
!!#     -> Writes a message to the terminal and to the log file. FEAT1.0
!!#        compatibility routine for "MT=2".
!!#
!!#     i) MT = 2
!!#        call output_simple (MT)
!!#
!!#     -> Writes an empty line to the terminal and to the log file. FEAT1.0
!!#        compatibility routine for "MT=2".
!!#
!!#     i) MT = 2
!!#        call output_simple_sep (MT,OU_SEP_MINUS)
!!#
!!#     -> Writes a separation line with '-' signs to the terminal and
!!#        to the log file. FEAT1.0 compatibility routine for "MT=2".
!!#
!!#     For futher possibilities, consider the documentation out output_line.
!!#
!!# 3.) call output_done()
!!#
!!#     -> Closes the output channel(s).
!!# </code>
!!#
!!# Logging benchmark data
!!# ----------------------
!!# The output library furthermore supports the (semi-)automatic output of
!!# deterministic data to a so callen 'benchmark log file'. This is typically
!!# used to write out data to a specific file which is compared to reference
!!# data in regression tests. Such data is meant to be deterministic,
!!# reproducable in every run and usually formatted in a 'nice' style that
!!# differences to reference results can easily be checked.
!!#
!!# To activate the benchmark log file, one has to specify an additional
!!# parameter in the call to output_init:
!!#
!!# <code>
!!#    call output_init ('mylogfile.txt','myerrorlogfile.txt','benchmarkresultfile')
!!# </code>
!!#
!!# This opens a file 'benchmarkresultfile' where benchmark data is written to.
!!# To write a string to this file, the application has to call output_line
!!# with an extended output-mode:
!!#
!!# <code>
!!#   call output_line('A message only to the benchmark log.', &
!!#                    OU_CLASS_MSG,OU_MODE_BENCHLOG)
!!# </code>
!!#
!!# This writes a message directly to the benchmark log file. It is also possible
!!# to write out data to the standard terminal plus the benchmark log file
!!# by combining the output mode constants:
!!#
!!# <code>
!!#   call output_line('A message only to the benchmark log.', &
!!#                    OU_CLASS_MSG,OU_MODE_STD+OU_MODE_BENCHLOG)
!!# </code>
!!#
!!# In all cases, the additional constant OU_MODE_BENCHLOG must be manually
!!# specified as logging to the benchmark log file is not done automatically.
!!#
!!# Adding date/time to the output
!!# ------------------------------
!!# The output module supports automatic adding of the current date and/or
!!# time to the output. This helps to keep track of the execution time
!!# and allows basic profiling of the application. To enable the output
!!# of date/time data, it can be activated using the global
!!# cdefaultDateTimeLogPolicy variable. Using cdatetimeLogFormat allows to
!!# change the format of the output.
!!#
!!# Example: The following code enables printing of date and time:
!!#
!!# <code>
!!#    call output_init()
!!#    cdefaultDateTimeLogPolicy = OU_DTP_ADDDATETIME
!!# </code>
!!#
!!# Using the optional variable cdateTimeLogPolicy in the output subroutines
!!# allows to manually define whether to print date/time or not. This can be
!!# used to switch off the output if some lines are printed without line break.
!!# Example:
!!#
!!# <code>
!!#    call output_init()
!!#    cdefaultDateTimeLogPolicy = OU_DTP_ADDDATETIME
!!#
!!#    call output_line ("Hello")
!!#
!!#    call output_line ("This is some output.")
!!#
!!#    call output_line ("This sentence is incorrectly printed ", bnolinebreak=.true.)
!!#    call output_line ("in two statements.")
!!#
!!#    call output_line ("This sentence is correctly printed ", bnolinebreak=.true.)
!!#    call output_line ("in two statements.", cdateTimeLogPolicy = OU_DTP_NONE)
!!# </code>
!!#
!!# This gives the output:
!!#
!!# <verb>
!!#    02-06-2010 16:21:00: Hello.
!!#    02-06-2010 16:21:00: This is some output.
!!#    02-06-2010 16:21:00: This sentence is incorrectly printed 02-06-2010 16:21:00: in two statements.
!!#    02-06-2010 16:21:00: This sentence is correctly printed in two statements.
!!# </verb>
!!#
!!# If cdefaultDateTimeLogPolicy is not set, specifying cdateTimeLogPolicy=OU_DTP_NONE
!!# as parameter in the output does not harm since it has no effect.
!!#
!!# </purpose>
!!##############################################################################

MODULE ModLib_FeatFlow_GenOutput

!$ use omp_lib
  USE ModLib_FeatFlow_System

  IMPLICIT NONE

  PRIVATE

!<constants>

!<constantblock description="Output mode. Decides on where the output is written to.">

  ! Output mode: Write to main log file / error log file (depending on whether
  ! a message is considered as an error or not by OU_CLASS_ERROR/OU_CLASS_WARNING)
  INTEGER(I32), PARAMETER, PUBLIC :: OU_MODE_LOG      = 2_I32**0

  ! Output mode: Write to terminal
  INTEGER(I32), PARAMETER, PUBLIC :: OU_MODE_TERM     = 2_I32**1

  ! Output mode: Write to benchmark log file
  INTEGER(I32), PARAMETER, PUBLIC :: OU_MODE_BENCHLOG = 2_I32**2

  ! Output mode: Write to both, log file and terminal
  INTEGER(I32), PARAMETER, PUBLIC :: OU_MODE_STD      = OU_MODE_LOG+OU_MODE_TERM

!</constantblock>

!<constantblock description="Output classification. Prints an additional classification string\\
!                            when writing a string to a log file / terminal">

  ! Output classification: Standard message
  INTEGER, PARAMETER, PUBLIC :: OU_CLASS_MSG      = 0

  ! Output classification: Trace information, level 1
  INTEGER, PARAMETER, PUBLIC :: OU_CLASS_TRACE1   = 1

  ! Output classification: Trace information, level 2
  INTEGER, PARAMETER, PUBLIC :: OU_CLASS_TRACE2   = 2

  ! Output classification: Trace information, level 3
  INTEGER, PARAMETER, PUBLIC :: OU_CLASS_TRACE3   = 3

  ! Output classification: System/Timer message
  INTEGER, PARAMETER, PUBLIC :: OU_CLASS_SYSTEM   = 4

  ! Output classification: Error message.
  INTEGER, PARAMETER, PUBLIC :: OU_CLASS_ERROR    = 5

  ! Output classification: Warning message.
  INTEGER, PARAMETER, PUBLIC :: OU_CLASS_WARNING  = 6

!</constantblock>

!<constantblock description="Type of separator line">

  ! Separator line: MINUS character
  INTEGER, PARAMETER, PUBLIC :: OU_SEP_MINUS  = 0

  ! Separator line: STAR character
  INTEGER, PARAMETER, PUBLIC :: OU_SEP_STAR   = 1

  ! Separator line: EQUAL character
  INTEGER, PARAMETER, PUBLIC :: OU_SEP_EQUAL  = 2

  ! Separator line: DOLLAR character
  INTEGER, PARAMETER, PUBLIC :: OU_SEP_DOLLAR = 3

  ! Separator line: @ character
  INTEGER, PARAMETER, PUBLIC :: OU_SEP_AT     = 4

  ! Separator line: + character
  INTEGER, PARAMETER, PUBLIC :: OU_SEP_PLUS   = 5

  ! Separator line: ~ character
  INTEGER, PARAMETER, PUBLIC :: OU_SEP_TILDE  = 6

  ! Separator line: & character
  INTEGER, PARAMETER, PUBLIC :: OU_SEP_AMPAND = 7

  ! Separator line: % character
  INTEGER, PARAMETER, PUBLIC :: OU_SEP_PERC   = 8

  ! Separator line: # character
  INTEGER, PARAMETER, PUBLIC :: OU_SEP_HASH   = 9

!</constantblock>

!<constantblock>

  ! Length of a line on the terminal / in the log file.
  ! Standard value = 80 characters
  INTEGER, PUBLIC :: OU_LINE_LENGTH         = 80

  ! Global device number for terminal output
  INTEGER, PUBLIC :: OU_TERMINAL            = 6

  ! Global device number for terminal input
  INTEGER, PUBLIC :: IN_TERMINAL            = 5

  ! Global device number for errors on terminal
  INTEGER, PUBLIC :: OU_ERROR               = 6

  ! Global device number for log file. The log file is opened in output_init.
  ! <=0: no log file output.
  INTEGER, PUBLIC :: OU_LOG                 = 0

  ! Global device number for error log file. The error log file is opened
  ! in output_init and usually coincides with OU_LOG.
  ! <=0: write error messages to standard log file.
  INTEGER, PUBLIC :: OU_ERRORLOG            = 0

  ! Global device number for benchmark log file. The error log file is opened
  ! in output_init and usually coincides with OU_LOG.
  ! <=0: write error messages to standard log file.
  INTEGER, PUBLIC :: OU_BENCHLOG            = 0

!</constantblock>

!<constantblock description = "Constants for setting up adding date/time to each message.">

  ! Do not add date/time.
  INTEGER, PARAMETER, PUBLIC :: OU_DTP_NONE            = 0

  ! Add date.
  INTEGER, PARAMETER, PUBLIC :: OU_DTP_ADDDATE         = 1

  ! Add time.
  INTEGER, PARAMETER, PUBLIC :: OU_DTP_ADDTIME         = 2

  ! Add date + time.
  INTEGER, PARAMETER, PUBLIC :: OU_DTP_ADDDATETIME     = 3

!</constantblock>

!</constants>

!<publicvars>

  ! Current benchmark file logging policy.
  ! =0: None. Do not log anything to the benchmark log file.
  ! =1: Standard. Write data to benchmark log file if OU_BENCHLOG is specified
  !     in coutputMode.
  ! =2: Full. Write everything into the benchmark log file independent on whether
  !     OU_BENCHLOG is specified in coutputMode or not.
  INTEGER, PUBLIC, SAVE :: cbenchLogPolicy = 1

  ! Default date/time appending flag. Allows to configure the
  ! output submodule to automatically add the current date/time to the output.
  ! =OU_DTP_NONE:    do not add date/time (standard).
  ! =OU_DTP_ADDDATE: add time to the output.
  ! =OU_DTP_ADDTIME:    add date to the output.
  ! =OU_DTP_ADDDATETIME:    add both, date and time to the output.
  INTEGER, PUBLIC, SAVE :: cdefaultDateTimeLogPolicy = OU_DTP_NONE

  ! Date/Time format.
  ! =0: english: MM-DD-YYYY HH:MM:SS
  ! =1: german: DD.MM.YYYY HH:MM:SS
  INTEGER, PUBLIC, SAVE :: cdatetimeLogFormat = 0

  ! Defines an automatic indention by spaces which is put in front of
  ! all messages (After a possible date).
  ! Must be in the range 0..255.
  INTEGER, PUBLIC, SAVE :: output_iautoOutputIndent = 0

!</publicvars>

  ! Empty string used for indented output.
  ! By Fortran convention, this automatically expands to spaces.
  CHARACTER(LEN=256), SAVE :: semptystring = ""

  INTERFACE output_line
    MODULE PROCEDURE output_line_std
    MODULE PROCEDURE output_line_feast
  END INTERFACE

  INTERFACE output_init
    MODULE PROCEDURE output_init_simple
    MODULE PROCEDURE output_init_logfile
    MODULE PROCEDURE output_init_standard
  END INTERFACE

  PUBLIC :: output_init
  PUBLIC :: output_line
  PUBLIC :: output_separator
  PUBLIC :: output_lbrk
  PUBLIC :: output_simple
  PUBLIC :: output_simple_sep
  PUBLIC :: output_multiline
  PUBLIC :: output_done

CONTAINS

!************************************************************************

!<subroutine>
  SUBROUTINE output_openLogfile(sfilename, iunit)

  !<description>
    ! This routine opens a file sfilename for writing. This is
    ! usually used to create a log file. The routine will return a
    ! handle iunit with an output channel where text can be written to.
    ! If the file exists, it is overwritten.
    !
    ! As long as this routine is not called, all output is redirected
    ! only to the standard output channel.
  !</description>

  !<input>

    ! Name of the file to overwrite.
    CHARACTER(*), INTENT(in) :: sfilename

  !</input>

  !<output>
    !unit of the opened file
    INTEGER, INTENT(out) :: iunit
  !</output>

!</subroutine>

    INTEGER :: i
    INTEGER :: istatus ! status variable for opening procedure
    CHARACTER(LEN=LEN(sfilename)) :: sfilepath

    ! use wrapper for C system call stat() in kernel/System/isdirectory.c
!    EXTERNAL isdirectory
    ! use wrapper for C system call mkdir() in kernel/System/mkdir_recursive.c
!    EXTERNAL mkdir_recursive


    IF (LEN_TRIM(sfilename) .eq. 0) THEN
      WRITE (*,'(A)') 'Error: output_openLogfile. sfilename undefined!'
      RETURN
    END IF

    iunit = sys_getFreeUnit()
    IF (iunit .eq. -1) THEN
      WRITE (*,'(A)') 'Error: output_openLogfile. No output channel available!'
      RETURN
    END IF

    ! Ensure that the path up to the given file name does exist
    ! In short, this could be realised by
    !    call io_pathExtract(sfilename, sfilepath)
    !    if (.not. io_isDirectory(sfilepath)) then
    !      call io_makeDirectory(sfilepath)
    !    end if
    ! but as this would introduce circular module dependencies inline ethe relevant code
    ! here manually.
    i = SCAN(sfilename, "/\\", .true.)
    IF (i .ne. 0) THEN
      ! Directory ends at position i.
      sfilepath = sfilename(1:i-1)
!      CALL isdirectory(TRIM(sfilepath) // ACHAR(0), i, istatus)
      IF (istatus .ge. 0) THEN
        IF (i .le. 0) THEN
          ! Path does not exist, create it
!          CALL mkdir_recursive(TRIM(sfilepath) // ACHAR(0), istatus)
          IF (istatus .lt. 0) THEN
            WRITE (*,'(A)') 'Error: output_openLogfile. Could not (auto-)create path to',&
                            ' output file "', TRIM(sfilename), '". ***'
          END IF
        END IF
      END IF

    ELSE
      ! No subdirectory contained in log file, no path needs creation
    END IF

    OPEN(unit=iunit, file=TRIM(sfilename), iostat=istatus, status="replace", &
          action="write")

    IF (istatus .ne. 0) THEN
      WRITE (unit=*, fmt=*) &
          'Error: output_openLogfile. Error while opening file "',&
          TRIM(sfilename), '". ***'
      iunit = -1
    END IF

  END SUBROUTINE

!************************************************************************************

!<subroutine>

  SUBROUTINE output_init_simple ()

!<description>
  ! Initialises the output system. The output system is configured to show all
  ! messages on the terminal, no log file is opened.
!</description>

!</subroutine>

    CALL output_init_standard ("","")

  END SUBROUTINE

!************************************************************************************

!<subroutine>

  SUBROUTINE output_init_logfile (slogFilename)

!<description>
  ! Initialises the output system.
  ! If sfilename is given, a log file with that name is opened and all log and error
  ! messages are written to it. If not given, all output is directed to the
  ! terminal only.
!</description>

!<input>

  ! Name of a log file for standard messages. If "" is specified,
  ! output messages are written to the standard output.
  CHARACTER(LEN=*), INTENT(in) :: slogFilename

!</input>

!</subroutine>

    CALL output_init_standard (slogFilename,"")

  END SUBROUTINE

!************************************************************************************

!<subroutine>

  SUBROUTINE output_init_standard (slogFilename,serrorFilename,sbenchLogfile)

!<description>
  ! Initialises the output system.
  ! If sfilename is given, a log file with that name is opened and all log
  ! messages are written to it. If not given, all output is directed to the
  ! terminal only.
  ! If serrorFilename is given, an error log file with filename serrorFilename
  ! is opened and all error messages are redirected to it. If not given,
  ! the error messages are directed to the log file -- or to the terminal, if
  ! slogFilename does not exist.
!</description>

!<input>

  ! Name of a log file for standard messages. If ""
  ! is specified, output messages are written to the standard output.
  CHARACTER(LEN=*), INTENT(in) :: slogFilename

  ! Name of an log file for error messages. If ""
  ! is specified, errors are written to the standard log file. The name of
  ! the file may also coincide with slogFilename.
  CHARACTER(LEN=*), INTENT(in) :: serrorFilename

  ! OPTIONAL: Name of an log file for deterministic benchmark messages. If
  ! not present or set to "", benchmark messages are not written out. The
  ! name of the file may also coincide with slogFilename.
  CHARACTER(LEN=*), INTENT(in), OPTIONAL :: sbenchLogfile

!</input>

!</subroutine>

    ! Close previously opened log files.
    CALL output_done ()

    ! Name of the standard logfile given?
    IF (slogFilename .ne. "") THEN

      ! Open a log file
      CALL output_openLogfile(slogFilename, OU_LOG)

    END IF

    ! Name of the error logfile given?
    IF (serrorFilename .ne. "") THEN

      ! Both filenames the same?
      IF (slogFilename .eq. serrorFilename) THEN
        OU_ERRORLOG = OU_LOG
      ELSE
        ! Open an error log file
        CALL output_openLogfile(serrorFilename, OU_ERRORLOG)
      END IF

    ELSE

      ! Write error messages to standard log file.
      OU_ERRORLOG = OU_LOG

    END IF

    ! Name of the benchmark logfile given?
    IF (PRESENT(sbenchLogfile)) THEN
      IF (sbenchLogfile .ne. "") THEN

        ! Both filenames the same?
        IF (slogFilename .eq. serrorFilename) THEN
          OU_BENCHLOG = OU_LOG
        ELSE
          ! Open a benchmark log file
          CALL output_openLogfile(sbenchLogfile, OU_BENCHLOG)
        END IF

      ELSE

        ! No benchmark output
        OU_BENCHLOG = 0

      END IF

    ELSE

      ! No benchmark output
      OU_BENCHLOG = 0

    END IF

  END SUBROUTINE

!************************************************************************************

!<subroutine>

  SUBROUTINE output_multiline (smessage, &
                               coutputClass, coutputMode, ssubroutine, &
                               bnolinebreak, bnotrim, cdateTimeLogPolicy)

!<description>
  ! Writes a formatted output message to the terminal, log file or error log
  ! file, depending on the input parameters.
  ! smessage is the message to be written out.
  ! coutputMode decides (if given) about whether the output if written to file,
  ! terminal or both.
  ! coutputClass classifies (if given) the message as standard message, trace
  ! or error message.
!</description>

!<input>
  ! The message to be written out.
  CHARACTER(LEN=*), INTENT(in) :: smessage

  ! OPTIONAL: Output mode. One of the OU_MODE_xxxx constants. If not specified,
  ! OU_MODE_STD is assumed.
  INTEGER(I32), INTENT(in), OPTIONAL :: coutputMode

  ! OPTIONAL: Output classification. One of the OU_CLASS_xxxx constants.
  ! If not specified, OU_CLASS_MSG is assumed.
  INTEGER, INTENT(in), OPTIONAL :: coutputClass

  ! OPTIONAL: Name of the subroutine that calls this function
  CHARACTER(LEN=*), INTENT(in), OPTIONAL :: ssubroutine

  ! OPTIONAL: When specifying bnolinebreak=TRUE, the output routine will
  ! not perform a line break after printing.
  LOGICAL, INTENT(in), OPTIONAL :: bnolinebreak

  ! OPTIONAL: When specifying bnotrim=TRUE, the output routine will
  ! not trim the string when printing. This does only work for
  ! coutputClass=OU_CLASS_MSG and coutputClass=OU_CLASS_ERROR
  ! (or if coutputClass is not specified)!
  LOGICAL, INTENT(in), OPTIONAL :: bnotrim

  ! OPTIONAL: Date/time appending flag. Allows to configure the
  ! output submodule to automatically add the current date/time to the output.
  ! =OU_DTP_NONE:    do not add date/time (standard).
  ! =OU_DTP_ADDDATE: add time to the output.
  ! =OU_DTP_ADDTIME:    add date to the output.
  ! =OU_DTP_ADDDATETIME:    add both, date and time to the output.
  ! If the parameter is not present, cdefaultDateTimeLogPolicy will be used
  ! as default parameter.
  INTEGER, INTENT(in), OPTIONAL :: cdateTimeLogPolicy

!</input>

!</subroutine>

  ! local variables
  INTEGER(I32) :: coMode
  INTEGER :: coClass, iofChannel, iotChannel
  INTEGER :: iindent,istart,ilinelen
  LOGICAL :: bntrim, bnnewline
  CHARACTER(LEN=LEN(smessage)+20+SYS_NAMELEN+10+8+3) :: smsg

    ! Get the actual parameters.

    coMode = OU_MODE_STD
    coClass = OU_CLASS_MSG
    bntrim = .false.
    bnnewline = .false.
    iindent = MIN(255,output_iautoOutputIndent)

    IF (PRESENT(coutputMode))  coMode = coutputMode
    IF (PRESENT(coutputClass)) coClass = coutputClass
    IF (PRESENT(bnotrim))      bntrim = bnotrim
    IF (PRESENT(bnolinebreak)) bnnewline = bnolinebreak

    ! Get the file and terminal output channel
    iotChannel = OU_TERMINAL
    IF ((coClass .eq. OU_CLASS_ERROR) .OR. &
        (coClass .eq. OU_CLASS_WARNING)) iotChannel = OU_ERROR

    iofChannel = OU_LOG
    IF ((coClass .eq. OU_CLASS_ERROR) .OR. &
        (coClass .eq. OU_CLASS_WARNING)) iofChannel = OU_ERRORLOG

    IF (bntrim .AND. &
        ((coClass .eq. OU_CLASS_MSG) .OR. &
         (coClass .eq. OU_CLASS_WARNING) .OR. &
         (coClass .eq. OU_CLASS_ERROR)) ) THEN

      ! Where to write the message to?
      IF ((IAND(coMode,OU_MODE_TERM) .ne. 0) .AND. (iotChannel .gt. 0)) THEN
        istart=1; ilinelen=INDEX(smessage,"\n")
        DO WHILE(ilinelen.ne.0)
          IF (ilinelen.eq.1) THEN
            WRITE (iotChannel,'(A)') ''
            istart=istart+ilinelen+1
          ELSE
            WRITE (iotChannel,'(A)',ADVANCE='NO') semptystring(1:iindent)//smessage(istart:istart+ilinelen-2)
            istart=istart+ilinelen-1
          END IF
          ilinelen=INDEX(smessage(istart:),"\n")
        END DO
        IF (bnnewline) THEN
          WRITE (iotChannel,'(A)',ADVANCE='NO') semptystring(1:iindent)//smessage(istart:)
        ELSE
          WRITE (iotChannel,'(A)') semptystring(1:iindent)//smessage(istart:)
        END IF
      END IF

      ! Output to log file?
      IF ((IAND(coMode,OU_MODE_LOG) .ne. 0) .AND. (iofChannel .gt. 0)) THEN
        istart=1; ilinelen=INDEX(smessage,"\n")
        DO WHILE(ilinelen.ne.0)
          IF (ilinelen.eq.1) THEN
            WRITE (iofChannel,'(A)') ''
            istart=istart+ilinelen+1
          ELSE
            WRITE (iofChannel,'(A)',ADVANCE='NO') semptystring(1:iindent)//smessage(istart:istart+ilinelen-2)
            istart=istart+ilinelen-1
          END IF
          ilinelen=INDEX(smessage(istart:),"\n")
        END DO
        IF (bnnewline) THEN
          WRITE (iofChannel,'(A)',ADVANCE='NO') semptystring(1:iindent)//smessage(istart:)
        ELSE
          WRITE (iofChannel,'(A)') semptystring(1:iindent)//smessage(istart:)
        END IF
      END IF

      ! Output to the benchmark log file?
      IF (OU_BENCHLOG .gt. 0) THEN
        IF ((cbenchLogPolicy .eq. 2) .OR. &
            (cbenchLogPolicy .eq. 1) .AND. (IAND(coMode,OU_MODE_BENCHLOG) .ne. 0)) THEN
          istart=1; ilinelen=INDEX(smessage,"\n")
          DO WHILE(ilinelen.ne.0)
            IF (ilinelen.eq.1) THEN
              WRITE (OU_BENCHLOG,'(A)') ''
              istart=istart+ilinelen+1
            ELSE
              WRITE (OU_BENCHLOG,'(A)',ADVANCE='NO') semptystring(1:iindent)//smessage(istart:istart+ilinelen-2)
              istart=istart+ilinelen-1
            END IF
            ilinelen=INDEX(smessage(istart:),"\n")
          END DO
          IF (bnnewline) THEN
            WRITE (OU_BENCHLOG,'(A)',ADVANCE='NO') semptystring(1:iindent)//smessage(istart:)
          ELSE
            WRITE (OU_BENCHLOG,'(A)') semptystring(1:iindent)//smessage(istart:)
          END IF
        END IF
      END IF

    ELSE

      istart=1; ilinelen=INDEX(smessage,"\n")
      DO WHILE(ilinelen.ne.0)
        IF (ilinelen.eq.1) THEN
          ! Build the actual error message
          smsg = output_reformatMsg (semptystring(1:iindent), &
              coutputClass, ssubroutine, cdateTimeLogPolicy)

          ! Where to write the new line to?
          IF ((IAND(coMode,OU_MODE_TERM) .ne. 0) .AND. (iotChannel .gt. 0)) THEN
            WRITE (iotChannel,'(A)') TRIM(smsg)
          END IF

          ! Output to log file?
          IF ((IAND(coMode,OU_MODE_LOG) .ne. 0) .AND. (iofChannel .gt. 0)) THEN
            WRITE (iofChannel,'(A)') TRIM(smsg)
          END IF

          ! Output to benchmark log file?
          IF (OU_BENCHLOG .gt. 0) THEN
            IF ((cbenchLogPolicy .eq. 2) .OR. &
                (cbenchLogPolicy .eq. 1) .AND. (IAND(coMode,OU_MODE_BENCHLOG) .ne. 0)) THEN
              WRITE (OU_BENCHLOG,'(A)') TRIM(smsg)
            END IF
          END IF
          istart=istart+ilinelen+1
        ELSE
          ! Build the actual error message
          smsg = output_reformatMsg (semptystring(1:iindent)//smessage(istart:istart+ilinelen-2), &
              coutputClass, ssubroutine, cdateTimeLogPolicy)

          ! Where to write the message to?
          IF ((IAND(coMode,OU_MODE_TERM) .ne. 0) .AND. (iotChannel .gt. 0)) THEN
            WRITE (iotChannel,'(A)',ADVANCE='NO') TRIM(smsg)
          END IF

          ! Output to log file?
          IF ((IAND(coMode,OU_MODE_LOG) .ne. 0) .AND. (iofChannel .gt. 0)) THEN
            WRITE (iofChannel,'(A)',ADVANCE='NO') TRIM(smsg)
          END IF

          ! Output to benchmark log file?
          IF (OU_BENCHLOG .gt. 0) THEN
            IF ((cbenchLogPolicy .eq. 2) .OR. &
                (cbenchLogPolicy .eq. 1) .AND. (IAND(coMode,OU_MODE_BENCHLOG) .ne. 0)) THEN
              WRITE (OU_BENCHLOG,'(A)',ADVANCE='NO') TRIM(smsg)
            END IF
          END IF
          istart=istart+ilinelen-1
        END IF
        ilinelen=INDEX(smessage(istart:),"\n")
      END DO

      ! Build the actual error message
      smsg = output_reformatMsg (semptystring(1:iindent)//smessage(istart:), &
          coutputClass, ssubroutine, cdateTimeLogPolicy)

      ! Where to write the message to?
      IF ((IAND(coMode,OU_MODE_TERM) .ne. 0) .AND. (iotChannel .gt. 0)) THEN
        IF (bnnewline) THEN
          WRITE (iotChannel,'(A)',ADVANCE='NO') TRIM(smsg)
        ELSE
          WRITE (iotChannel,'(A)') TRIM(smsg)
        END IF
      END IF

      ! Output to log file?
      IF ((IAND(coMode,OU_MODE_LOG) .ne. 0) .AND. (iofChannel .gt. 0)) THEN
        IF (bnnewline) THEN
          WRITE (iofChannel,'(A)',ADVANCE='NO') TRIM(smsg)
        ELSE
          WRITE (iofChannel,'(A)') TRIM(smsg)
        END IF
      END IF

      ! Output to benchmark log file?
      IF (OU_BENCHLOG .gt. 0) THEN
        IF ((cbenchLogPolicy .eq. 2) .OR. &
            (cbenchLogPolicy .eq. 1) .AND. (IAND(coMode,OU_MODE_BENCHLOG) .ne. 0)) THEN
          IF (bnnewline) THEN
            WRITE (OU_BENCHLOG,'(A)',ADVANCE='NO') TRIM(smsg)
          ELSE
            WRITE (OU_BENCHLOG,'(A)') TRIM(smsg)
          END IF
        END IF
      END IF

    END IF

  END SUBROUTINE

!************************************************************************************

!<subroutine>

  SUBROUTINE output_done ()

!<description>
  ! Closes all log files, cleans up output system.
!</description>

!</subroutine>

    ! Close all open channels.
    IF (OU_LOG .gt. 0) CLOSE(OU_LOG)
    IF (OU_ERRORLOG .gt. 0) CLOSE(OU_ERRORLOG)
    IF (OU_BENCHLOG .gt. 0) CLOSE(OU_BENCHLOG)

  END SUBROUTINE

!************************************************************************************

!<function>

  FUNCTION output_reformatMsg (smessage, coutputClass, ssubroutine, cdateTimeLogPolicy) RESULT (s)

!<description>
  ! Reformats an output message according to an output class.
!</description>

!<input>
  ! The message to reformat.
  CHARACTER(LEN=*), INTENT(in) :: smessage

  ! OPTIONAL: Output classification. One of the OU_CLASS_xxxx constants.
  ! If not specified, OU_CLASS_MSG is assumed.
  INTEGER, INTENT(in), OPTIONAL :: coutputClass

  ! OPTIONAL: Name of a subroutine to include in the message; may be "".
  CHARACTER(LEN=*), INTENT(in), OPTIONAL :: ssubroutine

  ! OPTIONAL: Date/time appending flag. Allows to configure the
  ! output submodule to automatically add the current date/time to the output.
  ! =OU_DTP_NONE:    do not add date/time (standard).
  ! =OU_DTP_ADDDATE: add time to the output.
  ! =OU_DTP_ADDTIME:    add date to the output.
  ! =OU_DTP_ADDDATETIME:    add both, date and time to the output.
  ! If the parameter is not present, cdefaultDateTimeLogPolicy will be used
  ! as default parameter.
  INTEGER, INTENT(in), OPTIONAL :: cdateTimeLogPolicy
!</input>

!<result>
  ! Reformatted string
!</result>

  CHARACTER(LEN=LEN(smessage)+20+SYS_NAMELEN+10+8+3) :: s

!</function>

    LOGICAL :: bsub
    INTEGER :: ioc,cdateTime,iindent
    CHARACTER(LEN=8) :: sdate
    CHARACTER(LEN=10) :: stime

    iindent = MIN(255,output_iautoOutputIndent)

    bsub = .false.
    IF (PRESENT (ssubroutine)) THEN
      bsub = (ssubroutine .ne. '')
    END IF

    cdateTime = cdefaultDateTimeLogPolicy
    IF (PRESENT(cdateTimeLogPolicy)) cdateTime = cdateTimeLogPolicy

    ioc = OU_CLASS_MSG
    IF (PRESENT(coutputClass)) ioc = coutputClass

    IF (.NOT. bsub) THEN

      SELECT CASE (ioc)
      CASE (OU_CLASS_TRACE1)
        s = semptystring(1:iindent)//'*** '//ADJUSTL(smessage)
      CASE (OU_CLASS_TRACE2)
        s = semptystring(1:iindent)//'***** '//ADJUSTL(smessage)
      CASE (OU_CLASS_TRACE3)
        s = semptystring(1:iindent)//'******* '//ADJUSTL(smessage)
      CASE (OU_CLASS_SYSTEM)
        s = semptystring(1:iindent)//'* System: '//ADJUSTL(smessage)
      CASE (OU_CLASS_ERROR)
        s = semptystring(1:iindent)//'* Error: '//ADJUSTL(smessage)
      CASE (OU_CLASS_WARNING)
        s = semptystring(1:iindent)//'* Warning: '//ADJUSTL(smessage)
      CASE default
        s = smessage
      END SELECT

    ELSE

      SELECT CASE (ioc)
      CASE (OU_CLASS_TRACE1)
        s = semptystring(1:iindent)//'*** '//TRIM(ssubroutine)//': '//ADJUSTL(smessage)
      CASE (OU_CLASS_TRACE2)
        s = semptystring(1:iindent)//'***** '//TRIM(ssubroutine)//': '//ADJUSTL(smessage)
      CASE (OU_CLASS_TRACE3)
        s = semptystring(1:iindent)//'******* '//TRIM(ssubroutine)//': '//ADJUSTL(smessage)
      CASE (OU_CLASS_SYSTEM)
        s = semptystring(1:iindent)//'* System ('//TRIM(ssubroutine)//'): '//ADJUSTL(smessage)
      CASE (OU_CLASS_ERROR)
        s = semptystring(1:iindent)//'* Error ('//TRIM(ssubroutine)//'): '//ADJUSTL(smessage)
      CASE (OU_CLASS_WARNING)
        s = semptystring(1:iindent)//'* Warning ('//TRIM(ssubroutine)//'): '//ADJUSTL(smessage)
      CASE default
        s = smessage
      END SELECT

    END IF

    IF (cdateTime .ne. OU_DTP_NONE) THEN

      ! Get date and time.
      CALL DATE_AND_TIME(sdate,stime)

      ! Reformat the message.
      SELECT CASE (cdatetimeLogFormat)
      CASE (1)
        SELECT CASE (cdateTime)
        CASE (OU_DTP_NONE)
        CASE (OU_DTP_ADDDATE)
          s = sdate(7:8)//"."//sdate(5:6)//"."//sdate(1:4)//": "//s
        CASE (OU_DTP_ADDTIME)
          s = stime(1:2)//":"//stime(3:4)//":"//stime(5:6)//": "//s
        CASE (OU_DTP_ADDDATETIME)
          s = sdate(7:8)//"."//sdate(5:6)//"."//sdate(1:4)//" "// &
              stime(1:2)//":"//stime(3:4)//":"//stime(5:6)//": "//s
        END SELECT
      CASE default
        SELECT CASE (cdateTime)
        CASE (OU_DTP_NONE)
        CASE (OU_DTP_ADDDATE)
          s = sdate(5:6)//"-"//sdate(7:8)//"-"//sdate(1:4)//": "//s
        CASE (OU_DTP_ADDTIME)
          s = stime(1:2)//":"//stime(3:4)//":"//stime(5:6)//": "//s
        CASE (OU_DTP_ADDDATETIME)
          s = sdate(5:6)//"-"//sdate(7:8)//"-"//sdate(1:4)//" "// &
              stime(1:2)//":"//stime(3:4)//":"//stime(5:6)//": "//s
        END SELECT
      END SELECT

    END IF


  END FUNCTION


!************************************************************************************

!<subroutine>

  SUBROUTINE output_line_std (smessage, &
                              coutputClass, coutputMode, ssubroutine, &
                              bnolinebreak, bnotrim, cdateTimeLogPolicy)

!<description>
  ! Writes an output message to the terminal, log file or error log file,
  ! depending on the input parameters.
  ! smessage is the message to be written out.
  ! coutputMode decides (if given) about whether the output if written to file,
  ! terminal or both.
  ! coutputClass classifies (if given) the message as standard message, trace
  ! or error message.
!</description>

!<input>
  ! The message to be written out.
  CHARACTER(LEN=*), INTENT(in) :: smessage

  ! OPTIONAL: Output mode. One of the OU_MODE_xxxx constants. If not specified,
  ! OU_MODE_STD is assumed.
  INTEGER(I32), INTENT(in), OPTIONAL :: coutputMode

  ! OPTIONAL: Output classification. One of the OU_CLASS_xxxx constants.
  ! If not specified, OU_CLASS_MSG is assumed.
  INTEGER, INTENT(in), OPTIONAL :: coutputClass

  ! OPTIONAL: Name of the subroutine that calls this function
  CHARACTER(LEN=*), INTENT(in), OPTIONAL :: ssubroutine

  ! OPTIONAL: When specifying bnolinebreak=TRUE, the output routine will
  ! not perform a line break after printing.
  LOGICAL, INTENT(in), OPTIONAL :: bnolinebreak

  ! OPTIONAL: When specifying bnotrim=TRUE, the output routine will
  ! not trim the string when printing. This does only work for
  ! coutputClass=OU_CLASS_MSG and coutputClass=OU_CLASS_ERROR
  ! (or if coutputClass is not specified)!
  LOGICAL, INTENT(in), OPTIONAL :: bnotrim

  ! OPTIONAL: Date/time appending flag. Allows to configure the
  ! output submodule to automatically add the current date/time to the output.
  ! =OU_DTP_NONE:    do not add date/time (standard).
  ! =OU_DTP_ADDDATE: add time to the output.
  ! =OU_DTP_ADDTIME:    add date to the output.
  ! =OU_DTP_ADDDATETIME:    add both, date and time to the output.
  ! If the parameter is not present, cdefaultDateTimeLogPolicy will be used
  ! as default parameter.
  INTEGER, INTENT(in), OPTIONAL :: cdateTimeLogPolicy

!</input>

!</subroutine>

  ! local variables
  INTEGER(I32) :: coMode
  INTEGER :: coClass, iofChannel, iotChannel
  INTEGER :: iindent
  LOGICAL :: bntrim, bnnewline
  CHARACTER(LEN=LEN(smessage)+20+SYS_NAMELEN+10+8+3) :: smsg

    ! Get the actual parameters.

    coMode = OU_MODE_STD
    coClass = OU_CLASS_MSG
    bntrim = .false.
    bnnewline = .false.
    iindent = MIN(255,output_iautoOutputIndent)

    IF (PRESENT(coutputMode))  coMode = coutputMode
    IF (PRESENT(coutputClass)) coClass = coutputClass
    IF (PRESENT(bnotrim))      bntrim = bnotrim
    IF (PRESENT(bnolinebreak)) bnnewline = bnolinebreak

    ! Get the file and terminal output channel
    iotChannel = OU_TERMINAL
    IF ((coClass .eq. OU_CLASS_ERROR) .OR. &
        (coClass .eq. OU_CLASS_WARNING)) iotChannel = OU_ERROR

    iofChannel = OU_LOG
    IF ((coClass .eq. OU_CLASS_ERROR) .OR. &
        (coClass .eq. OU_CLASS_WARNING)) iofChannel = OU_ERRORLOG

    IF (bntrim .AND. &
        ((coClass .eq. OU_CLASS_MSG) .OR. &
         (coClass .eq. OU_CLASS_WARNING) .OR. &
         (coClass .eq. OU_CLASS_ERROR)) ) THEN

      ! Where to write the message to?
      IF ((IAND(coMode,OU_MODE_TERM) .ne. 0) .AND. (iotChannel .gt. 0)) THEN
        IF (bnnewline) THEN
          WRITE (iotChannel,'(A)',ADVANCE='NO') semptystring(1:iindent)//smessage
        ELSE
          WRITE (iotChannel,'(A)') semptystring(1:iindent)//smessage
        END IF
      END IF

      ! Output to log file?
      IF ((IAND(coMode,OU_MODE_LOG) .ne. 0) .AND. (iofChannel .gt. 0)) THEN
        IF (bnnewline) THEN
          WRITE (iofChannel,'(A)',ADVANCE='NO') semptystring(1:iindent)//smessage
        ELSE
          WRITE (iofChannel,'(A)') semptystring(1:iindent)//smessage
        END IF
      END IF

      ! Output to the benchmark log file?
      IF (OU_BENCHLOG .gt. 0) THEN
        IF ((cbenchLogPolicy .eq. 2) .OR. &
            (cbenchLogPolicy .eq. 1) .AND. (IAND(coMode,OU_MODE_BENCHLOG) .ne. 0)) THEN
          IF (bnnewline) THEN
            WRITE (OU_BENCHLOG,'(A)',ADVANCE='NO') semptystring(1:iindent)//smessage
          ELSE
            WRITE (OU_BENCHLOG,'(A)') semptystring(1:iindent)//smessage
          END IF
        END IF
      END IF

    ELSE

      ! Build the actual error message
      smsg = output_reformatMsg (semptystring(1:iindent)//smessage, &
          coutputClass, ssubroutine, cdateTimeLogPolicy)

      ! Where to write the message to?
      IF ((IAND(coMode,OU_MODE_TERM) .ne. 0) .AND. (iotChannel .gt. 0)) THEN
        IF (bnnewline) THEN
          WRITE (iotChannel,'(A)',ADVANCE='NO') TRIM(smsg)
        ELSE
          WRITE (iotChannel,'(A)') TRIM(smsg)
        END IF
      END IF

      ! Output to log file?
      IF ((IAND(coMode,OU_MODE_LOG) .ne. 0) .AND. (iofChannel .gt. 0)) THEN
        IF (bnnewline) THEN
          WRITE (iofChannel,'(A)',ADVANCE='NO') TRIM(smsg)
        ELSE
          WRITE (iofChannel,'(A)') TRIM(smsg)
        END IF
      END IF

      ! Output to benchmark log file?
      IF (OU_BENCHLOG .gt. 0) THEN
        IF ((cbenchLogPolicy .eq. 2) .OR. &
            (cbenchLogPolicy .eq. 1) .AND. (IAND(coMode,OU_MODE_BENCHLOG) .ne. 0)) THEN
          IF (bnnewline) THEN
            WRITE (OU_BENCHLOG,'(A)',ADVANCE='NO') TRIM(smsg)
          ELSE
            WRITE (OU_BENCHLOG,'(A)') TRIM(smsg)
          END IF
        END IF
      END IF
    END IF

  END SUBROUTINE

!************************************************************************************

!<subroutine>

  SUBROUTINE output_line_feast (coutputClass, ssubroutine, smsg)

!<description>
  ! Writes an output message to the terminal, log file or error log file,
  ! depending on the input parameters.
  ! smsg is the message to be written out.
  ! coutputMode decides (if given) about whether the output if written to file,
  ! terminal or both.
  ! coutputClass classifies (if given) the message as standard message, trace
  ! or error message.
  !
  ! Compatibility function for FEAST routines.
!</description>

!<input>
  ! OPTIONAL: Output classification. One of the OU_CLASS_xxxx constants.
  ! If not specified, OU_CLASS_MSG is assumed.
  INTEGER, INTENT(in) :: coutputClass

  ! OPTIONAL: Name of the subroutine that calls this function
  CHARACTER(LEN=*), INTENT(in) :: ssubroutine

  ! The message to be written out.
  CHARACTER(LEN=*), INTENT(in) :: smsg

!</input>

!</subroutine>

    ! REMARK: Do not rename 'smsg' to 'smessage' -- this does not comply
    ! with the Fortran standard and will give you an ambigious interface
    ! in output_line as Fortran cannot distinguish between output_line_std
    ! and output_line_feast anymore in that case! (as all parameter names
    ! are the same).

    CALL output_line_std (smsg, coutputClass, OU_MODE_STD, ssubroutine)

  END SUBROUTINE

!************************************************************************************

!<subroutine>

  SUBROUTINE output_lbrk (coutputClass, coutputMode, ssubroutine, nlbrk, cdateTimeLogPolicy)

!<description>
  ! Writes a line break to the terminal, log file or error log file,
  ! depending on the input parameters.
  ! coutputMode decides (if given) about whether the output if written to file,
  ! terminal or both.
  ! coutputClass classifies (if given) the message as standard message, trace
  ! or error message.
!</description>

!<input>
  ! OPTIONAL: Output mode. One of the OU_MODE_xxxx constants. If not specified,
  ! OU_MODE_STD is assumed.
  INTEGER(I32), INTENT(in), OPTIONAL :: coutputMode

  ! OPTIONAL: Output classification. One of the OU_CLASS_xxxx constants.
  ! If not specified, OU_CLASS_MSG is assumed.
  INTEGER, INTENT(in), OPTIONAL :: coutputClass

  ! OPTIONAL: Name of the subroutine that calls this function
  CHARACTER(LEN=*), INTENT(in), OPTIONAL :: ssubroutine

  ! OPTIONAL: Number of linebreaks
  INTEGER, INTENT(in), OPTIONAL :: nlbrk

  ! OPTIONAL: Date/time appending flag. Allows to configure the
  ! output submodule to automatically add the current date/time to the output.
  ! =OU_DTP_NONE:    do not add date/time (standard).
  ! =OU_DTP_ADDDATE: add time to the output.
  ! =OU_DTP_ADDTIME:    add date to the output.
  ! =OU_DTP_ADDDATETIME:    add both, date and time to the output.
  ! If the parameter is not present, cdefaultDateTimeLogPolicy will be used
  ! as default parameter.
  INTEGER, INTENT(in), OPTIONAL :: cdateTimeLogPolicy
!</input>

!</subroutine>

    ! local variables
    INTEGER :: ilbrk

    IF (PRESENT(nlbrk)) THEN

      DO ilbrk = 1, nlbrk
        CALL output_line_std ('', coutputClass, coutputMode, ssubroutine,&
            cdateTimeLogPolicy=cdateTimeLogPolicy)
      END DO

    ELSE

      CALL output_line_std ('', coutputClass, coutputMode, ssubroutine,&
            cdateTimeLogPolicy=cdateTimeLogPolicy)

    END IF

  END SUBROUTINE

!************************************************************************************

!<subroutine>

  SUBROUTINE output_separator (csepType, coutputClass, coutputMode, ssubroutine, cdateTimeLogPolicy)

!<description>
  ! Writes a separator line to the terminal, log file or error log file,
  ! depending on the input parameters. The parameter csepType decides
  ! on the type of separator line.
  ! coutputMode decides (if given) about whether the output if written to file,
  ! terminal or both.
  ! coutputClass classifies (if given) the message as standard message, trace
  ! or error message.
!</description>

!<input>
  ! Type of the separator line. One of the OU_SEP_xxxx constants
  ! (OU_SEP_MINUS, OU_SEP_STAR, OU_SEP_EQUAL,...).
  INTEGER, INTENT(in) :: csepType

  ! OPTIONAL: Output mode. One of the OU_MODE_xxxx constants. If not specified,
  ! OU_MODE_STD is assumed.
  INTEGER(I32), INTENT(in), OPTIONAL :: coutputMode

  ! OPTIONAL: Output classification. One of the OU_CLASS_xxxx constants.
  ! If not specified, OU_CLASS_MSG is assumed.
  INTEGER, INTENT(in), OPTIONAL :: coutputClass

  ! OPTIONAL: Name of the subroutine that calls this function
  CHARACTER(LEN=*), INTENT(in), OPTIONAL :: ssubroutine

  ! OPTIONAL: Date/time appending flag. Allows to configure the
  ! output submodule to automatically add the current date/time to the output.
  ! =OU_DTP_NONE:    do not add date/time (standard).
  ! =OU_DTP_ADDDATE: add time to the output.
  ! =OU_DTP_ADDTIME:    add date to the output.
  ! =OU_DTP_ADDDATETIME:    add both, date and time to the output.
  ! If the parameter is not present, cdefaultDateTimeLogPolicy will be used
  ! as default parameter.
  INTEGER, INTENT(in), OPTIONAL :: cdateTimeLogPolicy
!</input>

!</subroutine>

    ! local variables
    CHARACTER(LEN=MAX(1,OU_LINE_LENGTH-1)) :: cstr
    CHARACTER(LEN=20) :: saux
    INTEGER :: isub,cdateTime

    cdateTime = cdefaultDateTimeLogPolicy
    IF (PRESENT(cdateTimeLogPolicy)) cdateTime = cdateTimeLogPolicy

    ! Determine how much to reduce the line length.
    isub = 0
    SELECT CASE (cdateTime)
    CASE (OU_DTP_NONE)
    CASE (OU_DTP_ADDDATE)
      isub = 12
    CASE (OU_DTP_ADDTIME)
      isub = 10
    CASE (OU_DTP_ADDDATETIME)
      isub = 21
    END SELECT

    ! Create the separator line
    SELECT CASE (csepType)
    CASE (OU_SEP_MINUS)
      WRITE (saux,'(A,I3,A)') '(',LEN(cstr)-isub,'(''-''))'
    CASE (OU_SEP_PLUS)
      WRITE (saux,'(A,I3,A)') '(',LEN(cstr)-isub,'(''+''))'
    CASE (OU_SEP_STAR)
      WRITE (saux,'(A,I3,A)') '(',LEN(cstr)-isub,'(''*''))'
    CASE (OU_SEP_EQUAL)
      WRITE (saux,'(A,I3,A)') '(',LEN(cstr)-isub,'(''=''))'
    CASE (OU_SEP_DOLLAR)
      WRITE (saux,'(A,I3,A)') '(',LEN(cstr)-isub,'(''$''))'
    CASE (OU_SEP_AT)
      WRITE (saux,'(A,I3,A)') '(',LEN(cstr)-isub,'(''@''))'
    CASE (OU_SEP_TILDE)
      WRITE (saux,'(A,I3,A)') '(',LEN(cstr)-isub,'(''~''))'
    CASE (OU_SEP_AMPAND)
      WRITE (saux,'(A,I3,A)') '(',LEN(cstr)-isub,'(''&''))'
    CASE (OU_SEP_PERC)
      WRITE (saux,'(A,I3,A)') '(',LEN(cstr)-isub,'(''%''))'
    CASE (OU_SEP_HASH)
      WRITE (saux,'(A,I3,A)') '(',LEN(cstr)-isub,'(''#''))'
    CASE default
      WRITE (unit=*, fmt=*) 'output_separator: Unknown separator type: ',&
                            csepType
      CALL sys_halt()
    END SELECT

    WRITE (cstr,saux)

    CALL output_line_std (TRIM(cstr), coutputClass, coutputMode, ssubroutine)

  END SUBROUTINE

!************************************************************************************

!<subroutine>

  SUBROUTINE output_simple (ioutputLevel, smsg, coutputClass, ssubroutine, cdateTimeLogPolicy)

!<description>
  ! Writes an output message to the terminal, log file or error log file,
  ! depending on the input parameters.
  ! smsg is the message to be written out.
  ! ioutputLevel decides on where the message is written to, to the terminal
  ! and/or the log file.
  ! coutputClass classifies (if given) the message as standard message, trace
  ! or error message.
!</description>

!<input>
  ! Output level. DEcides on where to write the message to.
  ! <=0: No output,
  !  =1: write to log file.
  ! >=2: write to log file and terminal.
  INTEGER, INTENT(in) :: ioutputLevel

  ! OPTIONAL: The message to be written out.
  CHARACTER(LEN=*), INTENT(in), OPTIONAL :: smsg

  ! OPTIONAL: Output classification. One of the OU_CLASS_xxxx constants.
  ! If not specified, OU_CLASS_MSG is assumed.
  INTEGER, INTENT(in), OPTIONAL :: coutputClass

  ! OPTIONAL: Name of the subroutine that calls this function
  CHARACTER(LEN=*), INTENT(in), OPTIONAL :: ssubroutine

  ! OPTIONAL: Date/time appending flag. Allows to configure the
  ! output submodule to automatically add the current date/time to the output.
  ! =OU_DTP_NONE:    do not add date/time (standard).
  ! =OU_DTP_ADDDATE: add time to the output.
  ! =OU_DTP_ADDTIME:    add date to the output.
  ! =OU_DTP_ADDDATETIME:    add both, date and time to the output.
  ! If the parameter is not present, cdefaultDateTimeLogPolicy will be used
  ! as default parameter.
  INTEGER, INTENT(in), OPTIONAL :: cdateTimeLogPolicy
!</input>

!</subroutine>

  INTEGER(I32) :: coMode

    SELECT CASE (ioutputLevel)
    CASE (:0)
      coMode = 0
    CASE (1)
      coMode = OU_MODE_LOG
    CASE (2:)
      coMode = OU_MODE_LOG+OU_MODE_TERM
    END SELECT

    IF (PRESENT(smsg)) THEN
      CALL output_line_std (smsg, coutputClass, coMode, ssubroutine, &
          cdateTimeLogPolicy=cdateTimeLogPolicy)
    ELSE
      CALL output_line_std ('', coutputClass, coMode, ssubroutine, &
          cdateTimeLogPolicy=cdateTimeLogPolicy)
    END IF

  END SUBROUTINE

!************************************************************************************

!<subroutine>

  SUBROUTINE output_simple_sep (ioutputLevel, csepType, coutputClass, ssubroutine, cdateTimeLogPolicy)

!<description>
  ! Writes a separator line to the terminal, log file or error log file,
  ! depending on the input parameters. The parameter csepType decides
  ! on the type of separator line.
  ! ioutputLevel decides on where the message is written to, to the terminal
  ! and/or the log file.
  ! coutputClass classifies (if given) the message as standard message, trace
  ! or error message.
!</description>

!<input>
  ! Output level. DEcides on where to write the message to.
  ! <=0: No output,
  !  =1: write to log file.
  ! >=2: write to log file and terminal.
  INTEGER, INTENT(in) :: ioutputLevel

  ! Type of the separator line. One of the OU_SEP_xxxx constants
  ! (OU_SEP_MINUS, OU_SEP_STAR or OU_SEP_EQUAL).
  INTEGER, INTENT(in) :: csepType

  ! OPTIONAL: Output classification. One of the OU_CLASS_xxxx constants.
  ! If not specified, OU_CLASS_MSG is assumed.
  INTEGER, INTENT(in), OPTIONAL :: coutputClass

  ! OPTIONAL: Name of the subroutine that calls this function
  CHARACTER(LEN=*), INTENT(in), OPTIONAL :: ssubroutine

  ! OPTIONAL: Date/time appending flag. Allows to configure the
  ! output submodule to automatically add the current date/time to the output.
  ! =OU_DTP_NONE:    do not add date/time (standard).
  ! =OU_DTP_ADDDATE: add time to the output.
  ! =OU_DTP_ADDTIME:    add date to the output.
  ! =OU_DTP_ADDDATETIME:    add both, date and time to the output.
  ! If the parameter is not present, cdefaultDateTimeLogPolicy will be used
  ! as default parameter.
  INTEGER, INTENT(in), OPTIONAL :: cdateTimeLogPolicy
!</input>

!</subroutine>

  INTEGER(I32) :: coMode

    SELECT CASE (ioutputLevel)
    CASE (:0)
      coMode = 0
    CASE (1)
      coMode = OU_MODE_LOG
    CASE (2:)
      coMode = OU_MODE_LOG+OU_MODE_TERM
    END SELECT

    CALL output_separator (csepType, coutputClass, coMode, ssubroutine, cdateTimeLogPolicy)

  END SUBROUTINE

END MODULE ModLib_FeatFlow_GenOutput
