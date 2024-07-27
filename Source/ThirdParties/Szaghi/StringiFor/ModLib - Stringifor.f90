!< StringiFor, Strings Fortran Manipulator with steroids.

MODULE ModLib_Stringifor
!< StringiFor, Strings Fortran Manipulator with steroids.
USE ModLib_Penf, ONLY : I1P, I2P, I4P, I8P, R4P, R8P, R16P
! use stringifor_string_t, only : adjustl, adjustr, count, index, len, len_trim, repeat, scan, trim, verify, CK, string
USE ModLib_StringiforString, ONLY : ADJUSTL, ADJUSTR, COUNT, INDEX, LEN_TRIM, REPEAT, SCAN, TRIM, VERIFY, CK, GLOB, STRING, STRJOIN

IMPLICIT NONE
PRIVATE
SAVE
! expose StingiFor objects
PUBLIC :: CK
PUBLIC :: GLOB
PUBLIC :: STRJOIN
PUBLIC :: STRING
! expose StingiFor overloaded builtins and operators
! public :: adjustl, adjustr, count, index, len, len_trim, repeat, scan, trim, verify
PUBLIC :: ADJUSTL, ADJUSTR, COUNT, INDEX, LEN_TRIM, REPEAT, SCAN, TRIM, VERIFY
! expose StingiFor new procedures
PUBLIC :: READ_FILE, READ_LINES, WRITE_FILE, WRITE_LINES
! expose PENF kinds
PUBLIC :: I1P, I2P, I4P, I8P, R4P, R8P, R16P

CONTAINS
   SUBROUTINE READ_FILE(FILE, LINES, FORM, IOSTAT, IOMSG)
   !< Read a file as a single string stream.
   !<
   !< The lines are returned as an array of strings that are read until the eof is reached.
   !< The line is read as an ascii stream read until the eor is reached.
   !<
   !< @note For unformatted read only `access='stream'` is supported with new_line as line terminator.
   !<
   !<```fortran
   !< type(string)              :: astring
   !< type(string), allocatable :: strings(:)
   !< type(string)              :: line(3)
   !< integer                   :: iostat
   !< character(len=99)         :: iomsg
   !< integer                   :: scratch
   !< integer                   :: l
   !< logical                   :: test_passed(8)
   !< line(1) = ' Hello World!   '
   !< line(2) = 'How are you?  '
   !< line(3) = '   All say: "Fine thanks"'
   !< open(newunit=scratch, file='read_file_test.tmp')
   !< write(scratch, "(A)") line(1)%chars()
   !< write(scratch, "(A)") line(2)%chars()
   !< write(scratch, "(A)") line(3)%chars()
   !< close(scratch)
   !< call read_file(file='read_file_test.tmp', lines=strings, iostat=iostat, iomsg=iomsg)
   !< test_passed(1) = (size(strings, dim=1)==size(line, dim=1))
   !< do l=1, size(strings, dim=1)
   !<   test_passed(l+1) = (strings(l)==line(l))
   !< enddo
   !< open(newunit=scratch, file='read_file_test.tmp', form='UNFORMATTED', access='STREAM')
   !< write(scratch) line(1)%chars()//new_line('a')
   !< write(scratch) line(2)%chars()//new_line('a')
   !< write(scratch) line(3)%chars()//new_line('a')
   !< close(scratch)
   !< call read_file(file='read_file_test.tmp', lines=strings, form='unformatted', iostat=iostat, iomsg=iomsg)
   !< test_passed(5) = (size(strings, dim=1)==size(line, dim=1))
   !< do l=1, size(strings, dim=1)
   !<   test_passed(l+5) = (strings(l)==line(l))
   !< enddo
   !< open(newunit=scratch, file='read_file_test.tmp', form='UNFORMATTED', access='STREAM')
   !< close(scratch, status='DELETE')
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CHARACTER(LEN=*), INTENT(IN)               :: FILE       !< File name.
   TYPE(STRING),     INTENT(OUT), ALLOCATABLE :: LINES(:)   !< The lines.
   CHARACTER(LEN=*), INTENT(IN),    OPTIONAL  :: FORM       !< Format of unit.
   INTEGER,          INTENT(OUT),   OPTIONAL  :: IOSTAT     !< IO status code.
   CHARACTER(LEN=*), INTENT(INOUT), OPTIONAL  :: IOMSG      !< IO status message.
   TYPE(STRING)                               :: FORM_      !< Format of unit, local variable.
   INTEGER                                    :: IOSTAT_    !< IO status code, local variable.
   CHARACTER(LEN=:), ALLOCATABLE              :: IOMSG_     !< IO status message, local variable.
   INTEGER                                    :: UNIT       !< Logical unit.
   LOGICAL                                    :: DOES_EXIST !< Check if file exist.

   IOMSG_ = REPEAT(' ', 99) ; IF (PRESENT(IOMSG)) IOMSG_ = IOMSG
   INQUIRE(FILE=FILE, IOMSG=IOMSG_, IOSTAT=IOSTAT_, EXIST=DOES_EXIST)
   IF (DOES_EXIST) THEN
      FORM_ = 'FORMATTED' ; IF (PRESENT(FORM)) FORM_ = FORM ; FORM_ = FORM_%UPPER()
      SELECT CASE(FORM_%CHARS())
      CASE('FORMATTED')
         OPEN(NEWUNIT=UNIT, FILE=FILE, STATUS='OLD', ACTION='READ', IOMSG=IOMSG_, IOSTAT=IOSTAT_, ERR=10)
      CASE('UNFORMATTED')
         OPEN(NEWUNIT=UNIT, FILE=FILE, STATUS='OLD', ACTION='READ', FORM='UNFORMATTED', ACCESS='STREAM', &
              IOMSG=IOMSG_, IOSTAT=IOSTAT_, ERR=10)
      ENDSELECT
      CALL READ_LINES(UNIT=UNIT, LINES=LINES, FORM=FORM, IOMSG=IOMSG_, IOSTAT=IOSTAT_)
      10 CLOSE(UNIT)
   ENDIF
   IF (PRESENT(IOSTAT)) IOSTAT = IOSTAT_
   IF (PRESENT(IOMSG)) IOMSG = IOMSG_
   ENDSUBROUTINE READ_FILE

   SUBROUTINE READ_LINES(UNIT, LINES, FORM, IOSTAT, IOMSG)
   !< Read lines (records) from a connected-formatted unit.
   !<
   !< @note The connected unit is rewinded. At a successful exit current record is at eof, at the beginning otHERWISE.
   !<
   !< The lines are returned as an array of strings that are read until the eof is reached.
   !< The line is read as an ascii stream read until the eor is reached.
   !<
   !< @note For unformatted read only `access='stream'` is supported with new_line as line terminator.
   !<
   !< @note There is no doctests, this being tested by means of [[read_file]] doctests.
   INTEGER,          INTENT(IN)               :: UNIT     !< Logical unit.
   TYPE(STRING),     INTENT(OUT), ALLOCATABLE :: LINES(:) !< The lines.
   CHARACTER(LEN=*), INTENT(IN),    OPTIONAL  :: FORM     !< Format of unit.
   INTEGER,          INTENT(OUT),   OPTIONAL  :: IOSTAT   !< IO status code.
   CHARACTER(LEN=*), INTENT(INOUT), OPTIONAL  :: IOMSG    !< IO status message.
   TYPE(STRING)                               :: FORM_    !< Format of unit, local variable.
   INTEGER                                    :: IOSTAT_  !< IO status code, local variable.
   CHARACTER(LEN=:), ALLOCATABLE              :: IOMSG_   !< IO status message, local variable.
   CHARACTER(KIND=CK, LEN=1)                  :: CH       !< Character storage.
   INTEGER                                    :: L        !< Counter.

   FORM_ = 'FORMATTED' ; IF (PRESENT(FORM)) FORM_ = FORM ; FORM_ = FORM_%UPPER()
   IOMSG_ = REPEAT(' ', 99) ; IF (PRESENT(IOMSG)) IOMSG_ = IOMSG
   REWIND(UNIT)
   SELECT CASE(FORM_%CHARS())
   CASE('FORMATTED')
      L = 0
      DO
         READ(UNIT, *, ERR=10, END=10)
         L = L + 1
      ENDDO
   CASE('UNFORMATTED')
      L = 0
      DO
         READ(UNIT, ERR=10, END=10) CH
         IF (CH==NEW_LINE('a')) L = L + 1
      ENDDO
   ENDSELECT
   10 REWIND(UNIT)
   IF (L>0) THEN
      ALLOCATE(LINES(1:L))
      L = 1
      IOSTAT_ = 0
      DO
         CALL LINES(L)%READ_LINE(UNIT=UNIT, FORM=FORM, IOSTAT=IOSTAT_, IOMSG=IOMSG_)
         IF ((IOSTAT_/=0.AND..NOT.IS_IOSTAT_EOR(IOSTAT_)).OR.(L>=SIZE(LINES, DIM=1))) THEN
            EXIT
         ENDIF
         L = L + 1
      ENDDO
   ENDIF
   IF (PRESENT(IOSTAT)) IOSTAT = IOSTAT_
   IF (PRESENT(IOMSG)) IOMSG = IOMSG_
   ENDSUBROUTINE READ_LINES

   SUBROUTINE WRITE_FILE(FILE, LINES, FORM, IOSTAT, IOMSG)
   !< Write a single string stream into file.
   !<
   !< @note For unformatted read only `access='stream'` is supported with new_line as line terminator.
   !<
   !<```fortran
   !< type(string)              :: astring
   !< type(string)              :: anotherstring
   !< type(string), allocatable :: strings(:)
   !< type(string)              :: line(3)
   !< integer                   :: iostat
   !< character(len=99)         :: iomsg
   !< integer                   :: scratch
   !< integer                   :: l
   !< logical                   :: test_passed(8)
   !< line(1) = ' Hello World!   '
   !< line(2) = 'How are you?  '
   !< line(3) = '   All say: "Fine thanks"'
   !< anotherstring = anotherstring%join(array=line, sep=new_line('a'))
   !< call write_file(file='write_file_test.tmp', lines=line, iostat=iostat, iomsg=iomsg)
   !< call astring%read_file(file='write_file_test.tmp', iostat=iostat, iomsg=iomsg)
   !< call astring%split(tokens=strings, sep=new_line('a'))
   !< test_passed(1) = (size(strings, dim=1)==size(line, dim=1))
   !< do l=1, size(strings, dim=1)
   !<   test_passed(l+1) = (strings(l)==line(l))
   !< enddo
   !< call write_file(file='write_file_test.tmp', lines=line, form='unformatted', iostat=iostat, iomsg=iomsg)
   !< call astring%read_file(file='write_file_test.tmp', form='unformatted', iostat=iostat, iomsg=iomsg)
   !< call astring%split(tokens=strings, sep=new_line('a'))
   !< test_passed(5) = (size(strings, dim=1)==size(line, dim=1))
   !< do l=1, size(strings, dim=1)
   !<   test_passed(l+5) = (strings(l)==line(l))
   !< enddo
   !< open(newunit=scratch, file='write_file_test.tmp')
   !< close(scratch, status='DELETE')
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CHARACTER(LEN=*), INTENT(IN)              :: FILE      !< File name.
   TYPE(STRING),     INTENT(IN)              :: LINES(1:) !< The lines.
   CHARACTER(LEN=*), INTENT(IN),    OPTIONAL :: FORM      !< Format of unit.
   INTEGER,          INTENT(OUT),   OPTIONAL :: IOSTAT    !< IO status code.
   CHARACTER(LEN=*), INTENT(INOUT), OPTIONAL :: IOMSG     !< IO status message.
   TYPE(STRING)                              :: FORM_     !< Format of unit, local variable.
   INTEGER                                   :: IOSTAT_   !< IO status code, local variable.
   CHARACTER(LEN=:), ALLOCATABLE             :: IOMSG_    !< IO status message, local variable.
   INTEGER                                   :: UNIT      !< Logical unit.

   IOMSG_ = REPEAT(' ', 99) ; IF (PRESENT(IOMSG)) IOMSG_ = IOMSG
   FORM_ = 'FORMATTED' ; IF (PRESENT(FORM)) FORM_ = FORM ; FORM_ = FORM_%UPPER()
   SELECT CASE(FORM_%CHARS())
   CASE('FORMATTED')
      OPEN(NEWUNIT=UNIT, FILE=FILE, ACTION='WRITE', IOMSG=IOMSG_, IOSTAT=IOSTAT_, ERR=10)
   CASE('UNFORMATTED')
      OPEN(NEWUNIT=UNIT, FILE=FILE, ACTION='WRITE', FORM='UNFORMATTED', ACCESS='STREAM', IOMSG=IOMSG_, IOSTAT=IOSTAT_, ERR=10)
   ENDSELECT
   CALL WRITE_LINES(UNIT=UNIT, LINES=LINES, FORM=FORM, IOMSG=IOMSG_, IOSTAT=IOSTAT_)
   10 CLOSE(UNIT)
   IF (PRESENT(IOSTAT)) IOSTAT = IOSTAT_
   IF (PRESENT(IOMSG)) IOMSG = IOMSG_
   ENDSUBROUTINE WRITE_FILE

   SUBROUTINE WRITE_LINES(UNIT, LINES, FORM, IOSTAT, IOMSG)
   !< Write lines (records) to a connected-formatted unit.
   !<
   !< @note There is no doctests, this being tested by means of [[write_file]] doctests.
   INTEGER,          INTENT(IN)              :: UNIT      !< Logical unit.
   TYPE(STRING),     INTENT(IN)              :: LINES(1:) !< The lines.
   CHARACTER(LEN=*), INTENT(IN),    OPTIONAL :: FORM      !< Format of unit.
   INTEGER,          INTENT(OUT),   OPTIONAL :: IOSTAT    !< IO status code.
   CHARACTER(LEN=*), INTENT(INOUT), OPTIONAL :: IOMSG     !< IO status message.
   INTEGER                                   :: L         !< Counter.

   DO L=1, SIZE(LINES, DIM=1)
      CALL LINES(L)%WRITE_LINE(UNIT=UNIT, FORM=FORM, IOSTAT=IOSTAT, IOMSG=IOMSG)
   ENDDO
   ENDSUBROUTINE WRITE_LINES
ENDMODULE ModLib_Stringifor
