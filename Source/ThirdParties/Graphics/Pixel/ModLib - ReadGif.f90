!>
!!##NAME
!!    readgif(3f) - [M_readgif] read a GIF file
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine readgif(filename, num_image, image, iostat, color_map, verbose)
!!
!!    character(len=*), intent(in) :: filename
!!    integer, intent(in)          :: num_image
!!    integer, intent(out), allocatable :: image(:,:)
!!    integer, intent(out)         :: iostat
!!    real   , allocatable, intent(out) :: color_map(:,:)
!!    logical, intent(in), optional :: verbose
!!
!!##DESCRIPTION
!!    read the num_image'th gif image from filename into arrays image and color_map
!!
!!##OPTIONS
!!    filename    input file
!!    num_image   number of image required
!!    image       Image data returned
!!    iostat      I/O error number, =0 if ok
!!    color_map   RGB for each level, range 0.0 to 1.0
!!    verbose     .true.for verbose output
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!       program demo_readgif
!!       use M_readgif, only : readgif
!!       use M_writegif, only : writegif
!!       implicit none
!!       character(len=*),parameter :: filename='boxes.gif'
!!       integer                    :: num_image=1
!!       integer,allocatable        :: image(:,:)
!!       integer                    :: iostat=0
!!       real,allocatable           :: color_map(:,:)
!!       integer,allocatable        :: color_map2(:,:)
!!       logical                    :: verbose=.true.
!!       integer                    :: i,ii,jj
!!       call readgif(filename,num_image,image,iostat,color_map,verbose)
!!       if(iostat.ne.0)then
!!          write(*,*)'*demo_readgif* could not read GIF file ',trim(filename)
!!          stop
!!       endif
!!
!!       write(*,*)'SIZE OF IMAGE =',size(image)
!!       do i=1,rank(image)
!!          write(*,*)'RANK OF IMAGE=',i,size(image,dim=i)
!!       enddo
!!
!!       write(*,*)'SIZE OF COLORMAP=',size(color_map)
!!       do i=1,rank(color_map)
!!          write(*,*)'RANK OF COLORMAP=',i,size(color_map,dim=i)
!!       enddo
!!
!!       ! convert between colormap types
!!       ! writegif uses an integer colormap, values 0 to 255
!!       ! readgif  uses real values 0.0 to 1.0
!!       ii=size(color_map,dim=1)
!!       jj=size(color_map,dim=2)
!!       allocate(color_map2(ii,0:jj-1))
!!       color_map2=255*color_map
!!
!!       ! change color and write standard gif file
!!       where (image.eq.1) image=4
!!       call writegif('boxes_new.gif',image,color_map2)
!!
!!       end program demo_readgif
!!
!!##AUTHORS
!!    Jos Bergervoet, Van Snyder, Maurizio Cremonesi, Clive Page, and others
!!##LICENSE
!!    This module contains a subroutine readgif(3f) which can read GIF files
!!    of types Gif87a and Gif89 (and maybe others). The code comes from
!!    various authors, see comments below. This version was put together
!!    by Clive Page who has put it into the public domain.
MODULE ModLib_ReadGif
! readgif2.f90   cgp 2010 Aug 28
! Original code from: http://it.geocities.com/butonoj/doc/gif-io/gifio.htm (now a dead link)
PUBLIC :: READGIF
PRIVATE
! *****     private stuff     ******************************************
INTEGER,PRIVATE, PARAMETER :: INTERLACE = 6    ! index of bit indicating interlaced
INTEGER,PRIVATE, PARAMETER :: MAX_LZW_BITS = 12
INTEGER,PRIVATE, PARAMETER :: USE_LOCAL_COLORMAP = 7 ! bit indicating to use local color map
INTEGER, PUBLIC, PARAMETER :: MAX_COLORMAP_SIZE = 256
INTEGER,PRIVATE, SAVE      :: LUN                    ! logical unit number
LOGICAL,PRIVATE            :: ZERO_DATA_BLOCK

TYPE, PUBLIC :: GIF_SCREEN_TYPE
    INTEGER :: ASPECT_RATIO, BACKGROUND
    INTEGER :: BIT_PIXEL                 ! size of colormap
    INTEGER :: COLOR_RESOLUTION
    INTEGER :: HEIGHT, WIDTH             ! shape(image) = (/width,height/)
    INTEGER :: COLOR_MAP_SIZE           ! size of local_colormap
    LOGICAL :: USE_LOCAL_COLORMAP       ! .true. if local color map, else global
END TYPE GIF_SCREEN_TYPE

TYPE(GIF_SCREEN_TYPE), PUBLIC :: GIF_SCREEN

TYPE, PUBLIC :: GIF89_TYPE
    INTEGER :: TRANSPARENT
    INTEGER :: DELAYTIME
    INTEGER :: INPUTFLAG
    INTEGER :: DISPOSAL
END TYPE GIF89_TYPE

TYPE(GIF89_TYPE), PUBLIC :: GIF89
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
CONTAINS
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE READGIF(FILENAME, NUM_IMAGE, IMAGE, IOSTAT, COLOR_MAP, VERBOSE)
! read the num_image'th gif image from filename into arrays image and color_map
CHARACTER(LEN=*), INTENT(IN) :: FILENAME                ! input file
INTEGER, INTENT(IN)          :: NUM_IMAGE               ! number of image required
INTEGER, INTENT(OUT), ALLOCATABLE :: IMAGE(:,:)         ! Image data returned
INTEGER, INTENT(OUT)         :: IOSTAT                  ! I/O error number, =0 if ok
REAL   , ALLOCATABLE, INTENT(OUT) :: COLOR_MAP(:,:)     ! RGB for each level, range 0.0 to 1.0
LOGICAL, INTENT(IN), OPTIONAL :: VERBOSE                ! .true.for verbose output
! -----     local variables     ------------------------------------
CHARACTER(LEN=16) :: BUF             ! input buffer
CHARACTER (LEN=1):: C                ! shorter input buffer
INTEGER :: IMAGE_COUNT               ! number of images processed so far
LOGICAL :: MY_VERBOSE
! -----     executable statements     ------------------------------
    ZERO_DATA_BLOCK = .FALSE.
    GIF89= GIF89_TYPE( -1, -1, -1, 0 )
    MY_VERBOSE = .FALSE.
    IF ( PRESENT(VERBOSE) ) MY_VERBOSE = VERBOSE
    CALL OPEN_GIF (FILENAME, IOSTAT, MY_VERBOSE, COLOR_MAP )
    IF (IOSTAT /= 0) RETURN
    IMAGE_COUNT = 0
    DO ! forever
      CALL READ_BUF(C, IOSTAT )
      IF (IOSTAT /= 0) THEN
        CALL IO_ERROR ( "reading file", IOSTAT, FILENAME )
        RETURN
      END IF
      IF ( C == ";" ) THEN ! gif image terminator
        IF ( IMAGE_COUNT < NUM_IMAGE ) THEN
          WRITE (*,*) "only", IMAGE_COUNT, "image(s) found in file"
          IOSTAT = -1
        END IF
        CLOSE ( UNIT=LUN )
        RETURN
      END IF
      IF ( C == "!" ) THEN
   ! gif extension
        CALL DO_EXTENSION (FILENAME, IOSTAT, MY_VERBOSE )
        IF (IOSTAT /= 0) RETURN
        CYCLE
      END IF
      IF ( C /= "," ) THEN
   ! not a valid start character
        WRITE (*,*) "ignoring bogus character ", ICHAR(C)
        CYCLE
      END IF
      IMAGE_COUNT = IMAGE_COUNT + 1
      IF (IMAGE_COUNT>NUM_IMAGE) RETURN
      CALL READ_BUF(BUF(1:9), IOSTAT )
      IF (IOSTAT /= 0) THEN
        CALL IO_ERROR("cannot read width/height", IOSTAT, FILENAME)
        RETURN
      END IF
!
! If local colour map exists: read it
!
      GIF_SCREEN%USE_LOCAL_COLORMAP = BTEST(ICHAR(BUF(9:9)),USE_LOCAL_COLORMAP)
      IF ( GIF_SCREEN%USE_LOCAL_COLORMAP ) THEN
        GIF_SCREEN%COLOR_MAP_SIZE = 2**(MODULO(ICHAR(BUF(9:9)),8)+1)
        IF(MY_VERBOSE) WRITE(*,*)'readgif error in local colour map, size=', &
                        GIF_SCREEN%COLOR_MAP_SIZE
        ALLOCATE(COLOR_MAP(3,GIF_SCREEN%COLOR_MAP_SIZE))
        CALL READ_COLORMAP(COLOR_MAP, IOSTAT )
        IF (IOSTAT /= 0) THEN
          CALL IO_ERROR ( " error reading local color map", IOSTAT, FILENAME )
          RETURN
        END IF
        CALL READ_IMAGE(BCINT2B(BUF(5:6)), BCINT2B(BUF(7:8)), &
                         BTEST(ICHAR(BUF(9:9)),INTERLACE), IMAGE_COUNT /= NUM_IMAGE, &
                         MY_VERBOSE, FILENAME,  IOSTAT, IMAGE)
        IF (IOSTAT /= 0) RETURN
      ELSE
        CALL READ_IMAGE(BCINT2B(BUF(5:6)), BCINT2B(BUF(7:8)), &
                         BTEST(ICHAR(BUF(9:9)),INTERLACE), IMAGE_COUNT /= NUM_IMAGE, &
                         MY_VERBOSE, FILENAME,  IOSTAT, IMAGE )
        IF (IOSTAT /= 0) RETURN
      END IF
    END DO
    CLOSE(UNIT=LUN)
    PRINT *,'closed unit', LUN, ' in readgif'
  END SUBROUTINE READGIF
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
! private module procedures
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
FUNCTION BCINT2B ( BUF ) RESULT (IRESULT)
! convert two bytes to an integer. The bytes are in little-endian
! order -- the first one is the low-order byte, and the second is
! the high-order byte
CHARACTER(LEN=*), INTENT(IN) :: BUF
INTEGER :: IRESULT
IRESULT = 256*ICHAR(BUF(2:2)) + ICHAR(BUF(1:1))
END FUNCTION BCINT2B
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE DO_EXTENSION (FILENAME, IOSTAT, VERBOSE )
CHARACTER(LEN=*),INTENT(IN) :: FILENAME             ! in case it is needed in a message
INTEGER, INTENT(OUT)        :: IOSTAT
LOGICAL, INTENT(IN)         :: VERBOSE
!
    CHARACTER(LEN=256), SAVE :: BUF      ! long input buffer
    CHARACTER (LEN=1) :: C                       ! short input buffer
    INTEGER :: COUNTX                     ! length of a data block
    CHARACTER(LEN=256) :: STR            ! part of a message, if verbose
!
    CALL READ_BUF(C, IOSTAT )
    IF (IOSTAT /= 0) THEN
      CALL IO_ERROR ( " error reading extension", IOSTAT, FILENAME )
      RETURN
    END IF
    SELECT CASE ( ICHAR(C) )
    CASE ( 1 )                           ! 0x01 -- plain text extension
      STR = "plain text extension"
    CASE ( 249 )                         ! 0xf9 -- graphic control extension
      STR = "graphic control extension"
      CALL GET_DATA_BLOCK(BUF, FILENAME, COUNTX, IOSTAT )
      IF (IOSTAT /= 0)  THEN
         RETURN
      END IF
      ! the gif89 structure isn't used. Why do we do this?
      GIF89%DISPOSAL = MODULO(ICHAR(BUF(1:1))/4, 7)
      GIF89%INPUTFLAG   = MODULO(ICHAR(BUF(1:1))/2, 1)
      GIF89%DELAYTIME   = BCINT2B(BUF(2:3))
      IF ( MODULO(ICHAR(BUF(1:1)),2) /= 0 ) THEN
         GIF89%TRANSPARENT = ICHAR(BUF(4:4))
      END IF
      DO
        CALL GET_DATA_BLOCK(BUF, FILENAME, COUNTX, IOSTAT )
        IF (IOSTAT /= 0) THEN
          RETURN
        END IF
        IF ( COUNTX == 0 ) THEN
          EXIT
        END IF
      END DO
    CASE ( 254 )                         ! 0xfe -- comment extension
      STR = "comment extension"
      DO
        CALL GET_DATA_BLOCK(BUF, FILENAME, COUNTX, IOSTAT )
        IF (IOSTAT /= 0) THEN
          RETURN
        END IF
        IF ( COUNTX == 0 ) THEN
           EXIT
        END IF
        IF (VERBOSE) WRITE (*,*) " gif comment: ", BUF(:COUNTX)
      END DO
    CASE ( 255 )                         ! 0xff -- application extension
      STR = "application extension"
    CASE DEFAULT                         ! oops
      WRITE (*,*) " unknown extension ", ICHAR(C), " label"
      STR = BUF
    END SELECT
    IF ( VERBOSE ) WRITE(*,*) 'readgif: extension ', TRIM(STR)
END SUBROUTINE DO_EXTENSION
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE GET_CODE ( RESULT,CODE_SIZE, FLAG, FILENAME, IOSTAT )
INTEGER, INTENT(OUT) :: RESULT
INTEGER, INTENT(IN) :: CODE_SIZE
LOGICAL, INTENT(IN) :: FLAG          ! first-time flag
CHARACTER (LEN=*), INTENT(IN) :: FILENAME        ! in case it is needed in a message
INTEGER, INTENT(OUT) :: IOSTAT
!
    INTEGER :: BINT                      ! bit an integer (not logical)
    CHARACTER(LEN=280), SAVE :: BUF      ! input buffer
    INTEGER :: COUNTX
    INTEGER, SAVE :: CURBIT
    LOGICAL, SAVE :: DONE
    INTEGER :: I, J, K
    INTEGER, SAVE :: LASTBIT
    INTEGER, SAVE :: LAST_BYTE
!
    IF ( FLAG ) THEN
      CURBIT = 0
      LASTBIT = 0
      LAST_BYTE = 2
      BUF(1:2) = "  " ! so it has a value the first time around
      DONE = .FALSE.
      RESULT = 0
      RETURN
    END IF
    IF ( CURBIT + CODE_SIZE >= LASTBIT ) THEN
      IF ( DONE ) THEN
        IF ( CURBIT >= LASTBIT ) WRITE (*,*) " ran off the end of my bits"
        RESULT = -1
        RETURN
      END IF
      BUF(1:2) = BUF(LAST_BYTE-1:LAST_BYTE)
      CALL GET_DATA_BLOCK(BUF(3:), FILENAME, COUNTX, IOSTAT )
      IF (IOSTAT /= 0) THEN
        RESULT = -1
        RETURN
      END IF
      IF ( COUNTX == 0 ) DONE = .TRUE.
      CURBIT = (CURBIT - LASTBIT) + 16
      LAST_BYTE = 2 + COUNTX
      LASTBIT = LAST_BYTE * 8
    END IF
    RESULT = 0
    I = CURBIT / 8 + 1
    K = MODULO(CURBIT, 8)
    DO J = 0, CODE_SIZE-1
      BINT = 0
      IF (BTEST(ICHAR(BUF(I:I)), K)) BINT = 1
      RESULT = IOR( RESULT, ISHFT(BINT,J) )
      CURBIT = CURBIT + 1
      K = K + 1
      IF ( K == 8 ) THEN
        K = 0
        I = I + 1
      END IF
    END DO
  END SUBROUTINE GET_CODE
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE GET_DATA_BLOCK (BUF, FILENAME, COUNTX, IOSTAT )
CHARACTER(LEN=*), INTENT(OUT) :: BUF
CHARACTER(LEN=*), INTENT(IN):: FILENAME ! in case it is needed in a message
INTEGER, INTENT(OUT) :: COUNTX        ! size of data block
INTEGER, INTENT(OUT) :: IOSTAT
!
    CHARACTER (LEN=1) :: C
    CALL READ_BUF(C, IOSTAT )
    IF (IOSTAT /= 0) THEN
      CALL IO_ERROR ( " error in count for data block", IOSTAT, FILENAME )
      RETURN
    END IF
    COUNTX = ICHAR(C)
    ZERO_DATA_BLOCK = COUNTX == 0
    IF ( COUNTX /= 0 ) THEN
      CALL READ_BUF(BUF(1:COUNTX), IOSTAT )
      IF (IOSTAT /= 0) &
        CALL IO_ERROR (" error reading data block", IOSTAT, FILENAME )
    END IF
  END SUBROUTINE GET_DATA_BLOCK
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE GET_LUN ( LUN )
INTEGER, INTENT(OUT) :: LUN    ! a free logical unit number
!
LOGICAL :: INUSE, EXISTS
!
DO LUN = 100, 6, -1
      INQUIRE (UNIT=LUN, OPENED=INUSE, EXIST=EXISTS )
      IF (EXISTS .AND. .NOT. INUSE) RETURN
END DO
WRITE(*,*) "get_lun: no free logical unit numbers"
LUN = -1
END SUBROUTINE GET_LUN
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE IO_ERROR ( MESSAGE, IOSTAT, FILENAME )
CHARACTER(LEN=*), INTENT(IN) :: MESSAGE
INTEGER, INTENT(IN)          :: IOSTAT
CHARACTER(LEN=*), INTENT(IN) :: FILENAME
!
WRITE (*,*) "readgif error ", TRIM(MESSAGE), ' in ', TRIM(FILENAME), &
            " code =", IOSTAT
END SUBROUTINE IO_ERROR
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE LZW_READ_BYTE ( RESULT, INPUT_CODE_SIZE, FLAG, FILENAME, IOSTAT )
INTEGER, INTENT (OUT) :: RESULT
INTEGER, INTENT(IN) :: INPUT_CODE_SIZE
LOGICAL, INTENT(IN) :: FLAG          ! first-time flag
CHARACTER(LEN=*), INTENT(IN) :: FILENAME             ! in case it is needed in a message
INTEGER, INTENT(OUT) :: IOSTAT
!
    CHARACTER(LEN=260) :: BUF
    INTEGER, SAVE :: CLEAR_CODE
    INTEGER :: CODE
    INTEGER, SAVE :: CODE_SIZE
    INTEGER :: COUNTX
    INTEGER, SAVE :: END_CODE
    INTEGER, SAVE :: FIRSTCODE
    LOGICAL, SAVE :: FRESH = .FALSE.
    INTEGER :: I
    INTEGER :: INCODE
    INTEGER, SAVE :: MAX_CODE, MAX_CODE_SIZE
    INTEGER, PARAMETER :: MAX_MAX_CODE = 2**MAX_LZW_BITS
    INTEGER, PARAMETER :: MAX_STACK = 2*MAX_MAX_CODE
    INTEGER, SAVE :: OLDCODE
    INTEGER, SAVE :: SET_CODE_SIZE
    INTEGER, DIMENSION(0:1,0:MAX_MAX_CODE-1), SAVE :: TABLE
    INTEGER,DIMENSION(MAX_STACK), SAVE :: STACK
    INTEGER, SAVE :: STACK_PTR
    RESULT = 0
    IF ( FLAG ) THEN                     ! setup
        SET_CODE_SIZE = INPUT_CODE_SIZE
        CLEAR_CODE = 2 ** SET_CODE_SIZE
        IF ( SET_CODE_SIZE > MAX_LZW_BITS ) THEN
           RESULT= -1
           RETURN
         END IF
         END_CODE = CLEAR_CODE + 1
         CODE_SIZE = SET_CODE_SIZE + 1
         MAX_CODE = CLEAR_CODE + 2
         MAX_CODE_SIZE = 2 * CLEAR_CODE
         STACK_PTR = 1
         TABLE(0,:) = 0
         DO I = 0, CLEAR_CODE-1
            TABLE(1,I) = I
         END DO
         TABLE(1,CLEAR_CODE:) = 0
         CALL GET_CODE(I, CODE_SIZE, .TRUE., FILENAME, IOSTAT )   ! initialize
         FRESH = .TRUE.
         RETURN
    END IF
    IF ( FRESH ) THEN
      FRESH = .FALSE.
      DO
         CALL GET_CODE(OLDCODE, CODE_SIZE, .FALSE., FILENAME, IOSTAT )
         FIRSTCODE = OLDCODE
         IF ( FIRSTCODE /= CLEAR_CODE ) EXIT
      END DO
      RESULT = FIRSTCODE
      RETURN
    END IF
    IF ( STACK_PTR > 1 ) THEN
      STACK_PTR = STACK_PTR - 1
      RESULT = STACK(STACK_PTR)
      RETURN
    END IF
    DO
      CALL GET_CODE(CODE, CODE_SIZE, .FALSE., FILENAME, IOSTAT )
      IF ( CODE < 0 ) EXIT
      IF ( CODE == CLEAR_CODE ) THEN
        CODE_SIZE = SET_CODE_SIZE + 1
        MAX_CODE = CLEAR_CODE + 2
        MAX_CODE_SIZE = 2 * CLEAR_CODE
        STACK_PTR = 1
        TABLE(0,:) = 0
        DO I = 0, CLEAR_CODE-1
          TABLE(1,I) = I
        END DO
        TABLE(1,CLEAR_CODE:) = 0
        CALL GET_CODE( OLDCODE, CODE_SIZE, .FALSE., FILENAME, IOSTAT )
        FIRSTCODE = OLDCODE
        RESULT = FIRSTCODE
        RETURN
      END IF
      IF ( CODE == END_CODE ) THEN
        RESULT = -2
        IF ( ZERO_DATA_BLOCK ) RETURN
        DO
          CALL GET_DATA_BLOCK(BUF, FILENAME, COUNTX, IOSTAT )
          IF (IOSTAT /= 0) RETURN
          IF ( COUNTX <= 0 ) EXIT
        END DO
        IF ( COUNTX /= 0 ) THEN
          WRITE (UNIT=*, FMT="(a)") "missing eod in data stream in file"
          WRITE (UNIT=*, FMT="(a)") TRIM(FILENAME)
          WRITE (UNIT=*, FMT="(a)") "(this is not unusual)"
        END IF
        RETURN
      END IF
      INCODE = CODE
      IF ( CODE >= MAX_CODE ) THEN
        STACK(STACK_PTR) = FIRSTCODE
        STACK_PTR = STACK_PTR + 1
        CODE = OLDCODE
      END IF
      DO
        IF ( CODE >= CLEAR_CODE ) THEN
           STACK(STACK_PTR) = TABLE(1,CODE)
           STACK_PTR = STACK_PTR + 1
           IF ( CODE == TABLE(0,CODE) ) THEN
             WRITE (UNIT=*, FMT="(a,i6,a,i6)") "code =", CODE,", table(0,code) =", TABLE(0,CODE)
             WRITE (UNIT=*, FMT="(a)") "circular table entry in file"
             WRITE (UNIT=*, FMT="(a)") TRIM(FILENAME)
             WRITE (UNIT=*, FMT="(a)") "this is a serious error."
             RESULT = -2
             RETURN
           END IF
           CODE = TABLE(0,CODE)
        ELSE
          EXIT
        END IF
      END DO
      FIRSTCODE = TABLE(1,CODE)
      STACK(STACK_PTR) = FIRSTCODE
      STACK_PTR = STACK_PTR + 1
      CODE = MAX_CODE
      IF ( CODE < MAX_MAX_CODE ) THEN
        TABLE(0,CODE) = OLDCODE
        TABLE(1,CODE) = FIRSTCODE
        MAX_CODE = MAX_CODE + 1
        IF ( MAX_CODE >= MAX_CODE_SIZE .AND. MAX_CODE_SIZE < MAX_MAX_CODE ) THEN
          MAX_CODE_SIZE = MAX_CODE_SIZE * 2
          CODE_SIZE = CODE_SIZE + 1
        END IF
      END IF
      OLDCODE = INCODE
      IF ( STACK_PTR > 1 ) THEN
        STACK_PTR = STACK_PTR - 1
        RESULT = STACK(STACK_PTR)
        RETURN
      END IF
    END DO
    RESULT = CODE
  END SUBROUTINE LZW_READ_BYTE
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE OPEN_GIF (FILENAME, IOSTAT, VERBOSE, COLOR_MAP)
! open a gif file, verify it is either gif87a or gif89a
! fill the gif_screen structure, including reading the color map if
! necessary.
! iostat > 0 is status from open or read, iostat = -1 means wrong format.
! a message will be printed if iostat /= 0.
CHARACTER(LEN=*), INTENT(IN) :: FILENAME
INTEGER, INTENT(OUT) :: IOSTAT
LOGICAL, INTENT(IN) :: VERBOSE
REAL, ALLOCATABLE, INTENT(OUT) :: COLOR_MAP(:,:)
!
CHARACTER(LEN=7) :: BUF              ! input buffer
!
CALL GET_LUN ( LUN )
IF (LUN<1) THEN
   IOSTAT = -1
   RETURN
END IF
IF ( VERBOSE ) WRITE (UNIT=*, FMT="(a,a)") "opening ", TRIM(FILENAME)
OPEN(UNIT=LUN, FILE=FILENAME, ACCESS="stream", STATUS="old", IOSTAT=IOSTAT)
IF (IOSTAT /= 0) THEN
    CALL IO_ERROR ( " failed to open", IOSTAT, FILENAME )
    RETURN
ELSE
    END IF
    CALL READ_BUF (BUF(1:6), IOSTAT )
    IF (IOSTAT /= 0) THEN
      CALL IO_ERROR ( " error reading 'magic number'", IOSTAT, FILENAME )
      RETURN
    END IF
    IF ( BUF(1:6) /= "GIF87a" .AND. BUF(1:6) /= "GIF89a" ) THEN
      WRITE (*,*) " invalid GIF format", BUF(1:6), " in ", TRIM(FILENAME),&
         " expected GIF87a or GIF89a"
      IOSTAT = -1
      CLOSE ( UNIT=LUN )
      RETURN
    END IF
    CALL READ_BUF (BUF(1:7), IOSTAT )
    IF (IOSTAT /= 0) THEN
      RETURN
    END IF
    GIF_SCREEN%WIDTH = BCINT2B(BUF(1:2))
    GIF_SCREEN%HEIGHT = BCINT2B(BUF(3:4))
    GIF_SCREEN%BIT_PIXEL = ISHFT(2,IAND(ICHAR(BUF(5:5)),7))
    GIF_SCREEN%COLOR_RESOLUTION = IAND(ISHFT(ICHAR(BUF(5:5)),-3),14)+1
    GIF_SCREEN%BACKGROUND = ICHAR(BUF(6:6))
    GIF_SCREEN%ASPECT_RATIO = ICHAR(BUF(7:7))
    IF ( BTEST(ICHAR(BUF(5:5)), USE_LOCAL_COLORMAP) ) THEN
        IF(VERBOSE) WRITE(*,*) &
           'readgif error in global colour map size', GIF_SCREEN%BIT_PIXEL
        ALLOCATE(COLOR_MAP(3,GIF_SCREEN%BIT_PIXEL))
        CALL READ_COLORMAP(COLOR_MAP, IOSTAT )
        IF (IOSTAT /= 0) THEN
           CALL IO_ERROR ( "error reading global colormap", IOSTAT, FILENAME )
           RETURN
        END IF
    END IF
    IF ( GIF_SCREEN%ASPECT_RATIO /= 0 .AND. GIF_SCREEN%ASPECT_RATIO /= 49 ) THEN
      WRITE (*,*) "readgif warning: non-square pixels, ratio=", &
                 ( GIF_SCREEN%ASPECT_RATIO + 15.0 ) / 64.0
    END IF
END SUBROUTINE OPEN_GIF
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE READ_BUF (BUF, IOSTAT )
! read the next len(buf) characters from the gif file opened by open_gif.
CHARACTER(LEN=*), INTENT(OUT) :: BUF
INTEGER, INTENT(OUT) :: IOSTAT   ! I/O status, =0 if no error
READ(UNIT=LUN, IOSTAT=IOSTAT) BUF
END SUBROUTINE READ_BUF
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE READ_COLORMAP (COLORMAP, IOSTAT )
! read the colormap.
REAL, DIMENSION(:), INTENT(OUT) :: COLORMAP(:,:)  ! 1st dimension: red/green/blue
INTEGER, INTENT(OUT) :: IOSTAT
!
INTEGER :: J
CHARACTER :: TRIPLET*3
!
DO J = 1, SIZE(COLORMAP,2)
    CALL READ_BUF(TRIPLET, IOSTAT )
    IF (IOSTAT /= 0) THEN
        WRITE(*,*) 'readgif: error reading colormap at j=', J, IOSTAT
        RETURN
    END IF
    COLORMAP(1,J) = ICHAR(TRIPLET(1:1)) / 255.0
    COLORMAP(2,J) = ICHAR(TRIPLET(2:2)) / 255.0
    COLORMAP(3,J) = ICHAR(TRIPLET(3:3)) / 255.0
END DO
END SUBROUTINE READ_COLORMAP
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE READ_IMAGE (LENGTH, HEIGHT, INTERLACE, IGNORE, &
                       VERBOSE, FILENAME, IOSTAT, IMAGE)
INTEGER, INTENT(IN) :: LENGTH
INTEGER, INTENT(IN) :: HEIGHT
LOGICAL, INTENT(IN) :: INTERLACE
LOGICAL, INTENT(IN) :: IGNORE
CHARACTER(LEN=*),INTENT(IN) :: FILENAME             ! so it can appear in messages
LOGICAL, INTENT(IN) :: VERBOSE
!    character (len=*),dimension(:,:), pointer :: image
INTEGER, INTENT(OUT) :: IOSTAT
INTEGER, INTENT(OUT), ALLOCATABLE :: IMAGE(:,:)
!
CHARACTER (LEN=1) :: C              ! input buffer
INTEGER :: PASS                     ! pass number, for interlaced images
INTEGER :: VBYTE                    ! output from lzw_read_byte
INTEGER :: XPOS, YPOS               ! coordinates of pixel
INTEGER :: RSLT
!
    CALL READ_BUF(C, IOSTAT )
    IF (IOSTAT /= 0) THEN
      CALL IO_ERROR ( "error reading code size byte", IOSTAT, FILENAME )
      RETURN
    END IF
    CALL LZW_READ_BYTE( RSLT, ICHAR(C), .TRUE., FILENAME, IOSTAT )
    IF ( RSLT < 0 ) THEN
      CALL IO_ERROR ( "reading image prefix", IOSTAT, FILENAME )
      RETURN
    END IF
    IF ( IGNORE ) THEN                   ! skip an uninteresting image
      IF ( VERBOSE ) THEN
         WRITE (UNIT=*, FMT="(a)") "skipping an image"
      END IF
      CALL LZW_READ_BYTE (RSLT, ICHAR(C), .FALSE., FILENAME, IOSTAT )
      DO
         IF ( RSLT >= 0 ) THEN
            CALL LZW_READ_BYTE (RSLT, ICHAR(C), .FALSE., FILENAME, IOSTAT )
         ELSE
            EXIT
         END IF
      END DO
      RETURN
    END IF
 !
    IF(VERBOSE) WRITE(UNIT=*,FMT=*) "Image size:",LENGTH,HEIGHT
    ALLOCATE ( IMAGE(LENGTH,HEIGHT), STAT=IOSTAT )
 !
    IF (IOSTAT /= 0) THEN
      CALL IO_ERROR ( "while allocating image", IOSTAT, FILENAME )
      RETURN
    END IF
    IF ( VERBOSE ) WRITE (*,*) " reading", LENGTH," by", HEIGHT, " GIF image from ", &
                   TRIM(FILENAME), "  ", MERGE("interlaced", "          ", INTERLACE)
    PASS = 0
    XPOS = 1
    YPOS = 1
    DO
      CALL LZW_READ_BYTE (VBYTE, ICHAR(C), .FALSE., FILENAME, IOSTAT )
      IF (VBYTE < 0) EXIT
      IMAGE(XPOS,YPOS) = VBYTE
      XPOS = XPOS + 1
      IF ( XPOS > LENGTH ) THEN
        XPOS = 1
        IF ( INTERLACE ) THEN
          SELECT CASE ( PASS )
          CASE ( 0, 1 )
            YPOS = YPOS + 8
          CASE ( 2 )
            YPOS = YPOS + 4
          CASE ( 3 )
            YPOS = YPOS + 2
          END SELECT
          IF ( YPOS > HEIGHT ) THEN
            PASS = PASS + 1
            SELECT CASE ( PASS )
            CASE ( 1 )
              YPOS = 5
            CASE ( 2 )
              YPOS = 3
            CASE ( 3 )
              YPOS = 2
            CASE DEFAULT
              EXIT
            END SELECT
          END IF
        ELSE
          YPOS = YPOS + 1
        END IF
      END IF
      IF ( YPOS > HEIGHT ) EXIT
    END DO
      CALL LZW_READ_BYTE ( RSLT,ICHAR(C), .FALSE., FILENAME, IOSTAT )
    IF ( RSLT >= 0 ) THEN
      WRITE (*,*) "readgif: too much input data, ignoring extra..."
      DO
        IF ( RSLT >= 0 ) THEN
          CALL LZW_READ_BYTE ( RSLT, ICHAR(C), .FALSE., FILENAME, IOSTAT )
        ELSE
          EXIT
        END IF
      END DO
    END IF
  END SUBROUTINE READ_IMAGE
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
END MODULE ModLib_ReadGif
