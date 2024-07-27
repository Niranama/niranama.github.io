!>
!!##NAME
!!    writegif(3f) - [M_writegif] Codes pixel-map with palette into GIF format. Optional transparent color
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine writegif (FileName, Pixel, ColorMap, Transparent)
!!
!!    character(len=*), intent(in)         :: FileName
!!    integer, intent(in), dimension(:,:)  :: Pixel
!!    integer, intent(in), dimension(:,0:) :: ColorMap
!!    integer, intent(in), optional        :: Transparent
!!##DESCRIPTION
!!    Write GIF file from pixel array and color map.
!!
!!##OPTIONS
!!    FileName       file to create or replace
!!    Pixel          Pixel values 0 to ncol
!!    ColorMap       Color map (RGB 0:255 for colours 0:ncol)
!!    Transparent    Optional
!!##EXAMPLE
!!
!!   Sample call:
!!
!!    program demo_writegif
!!    use M_writegif, only : writegif
!!    integer  :: Pixel(100,100)
!!    integer  :: Transparent = 0
!!    integer  :: ColorMap (3,0:7)
!!    colormap(:,0)=[255,255,255]
!!    colormap(:,1)=[255,  0,  0]
!!    colormap(:,2)=[  0,255,  0]
!!    colormap(:,3)=[  0,  0,255]
!!    colormap(:,4)=[255,255,  0]
!!    colormap(:,5)=[255,  0,255]
!!    colormap(:,6)=[  0,255,255]
!!    colormap(:,7)=[  0,  0,  0]
!!
!!    ! put some colored boxes into pixmap
!!    pixel(:,:)=0
!!    pixel(1:80,1:80)=1
!!    pixel(11:20,11:20)=2
!!    pixel(21:40,21:40)=3
!!
!!    ! write gif with a transparent background
!!    call writegif('boxes_t.gif',pixel,ColorMap,Transparent)
!!
!!    ! change background color and write standard gif file
!!    where (pixel.eq.0) pixel=4
!!    call writegif('boxes.gif',pixel,ColorMap)
!!
!!    end program demo_writegif
!!
!!##AUTHOR
!!    o Version 1.01, 1999 August: Written by Jos Bergervoet
!!    o Version 2, 2008 Jan 28: Modified by Clive Page to use stream I/O, array as colourmap.
!!    o Version 3, 2017 July 6: Modified by John Urban to make it easier to use with ModLib_Pixel(3f) module.
!!##LICENSE
!!    Public Domain.
!>
!! PRODUCT:        CLI library utilities and examples
!! PROGRAM:        M_writegif(3f)
!! DESCRIPTION:    This module can write a GIF file in GIF89 format from raster data
!!##VERSION:        1.01, 19990808
!! AUTHOR:         Jos Bergervoet
!!##VERSION:        2.00, 20080128
!! AUTHOR:         version by [[Clive Page]] makes use of Fortran stream I/O, array as colourmap
!!##VERSION:        3.00, 20170706
!! AUTHOR:         Modified by John Urban to make it easier to use with ModLib_Pixel(3f) module.
!! LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.
!!                 There is NO WARRANTY, to the extent permitted by law.
MODULE ModLib_WriteGif
IMPLICIT NONE         !  Check all declarations

! ident_1="@(#)This module can write a GIF file in GIF89 format from raster data"

PRIVATE               !  bin_io is used private, no transfer to main program
PUBLIC  :: WRITEGIF   !  Writes GIF89 image, given pixel array and color map

PRIVATE :: GIFLZW, SLICEWRITE, INITTABLE, FLUSHBUFFER
INTEGER, PARAMETER, PRIVATE     :: BUFEND=260
CHARACTER(LEN=BUFEND), PRIVATE  :: BUF
INTEGER, PRIVATE  ::  IBUF                            ! output buffer vars
INTEGER, PARAMETER, PRIVATE  ::    MAXCODE = 4095
INTEGER, PARAMETER, PRIVATE  ::    NOCODE = MAXCODE+1 ! definitions for LZW

! Define LZW code tables for hashing:
CHARACTER(LEN=1), PRIVATE, DIMENSION(0:MAXCODE+1)  :: ENDBYTE
INTEGER, PRIVATE, DIMENSION(0:MAXCODE)             :: FOLLOW, NEXT
  !
  ! For any code P, which codes for a sequence af pixel-values, endbyte(P)
  ! is the last pixel-value, follow(P) points to another code (if it exists)
  ! which codes for this same sequence, but with one more pixel-value
  ! appended.
  !   For each code P, next(P) points to another code which codes for a
  ! similar sequence with only the endbyte different. This is a hashing
  ! pointer, for fast look-up.
  !   All pointers are 'nocode' if they point to nothing
  !

INTEGER, PRIVATE :: NCOD, CURMAXCODE, EOI, CC, P, K, CHILD, &
                    MAXBASE, SKIP, SLEN, BLEN, ACCUM, NOUT   ! local vars

CONTAINS
!-----------------------------------------------------------------------------

! CHAR2 Converts the two least sig bytes of an integer to a 2-character string
CHARACTER(LEN=2) FUNCTION CHAR2(IVAL)
INTEGER, INTENT(IN) :: IVAL
CHAR2 = ACHAR(MOD(IVAL,256)) // ACHAR(MOD(IVAL/256,256))
END FUNCTION CHAR2
!-----------------------------------------------------------------------------

SUBROUTINE FLUSHBUFFER(F_UNIT)
! Flushes up to 255 bytes to output file if buffer contains data, keeping
! rest of data in buffer. If skip>0 there is a partially filled last byte
! in buf[ibuf]. This byte will be written only if ibuf<256. That should be
! the last call to flushbuffer.
INTEGER, INTENT(IN) :: F_UNIT   ! I/O unit to use
  INTEGER  :: BL    !   number of bytes to write (to be determined)

  IF (IBUF > 255) THEN
    BL = 255        !   we will write buf[1..255]
  ELSE IF (SKIP /= 0) THEN
    BL = IBUF       !   buf[ibuf] is partially used, write buf[1..ibuf]
  ELSE IF (IBUF > 1) THEN
    BL = IBUF-1     !   write buf[1..ibuf-1], there is no partial byte
  ELSE
    RETURN          !   nothing to write
  END IF

  WRITE(F_UNIT) CHAR(BL)
  WRITE(F_UNIT) BUF(1:BL)
  BUF(1:IBUF-BL) = BUF(BL+1:IBUF) ! shift down remaining data
  IBUF = IBUF - BL
  RETURN
END SUBROUTINE FLUSHBUFFER
!-----------------------------------------------------------------------------
SUBROUTINE GIFLZW(F_UNIT, PIXEL)          ! routine for LZW coding
  INTEGER, INTENT(IN)                 :: F_UNIT
  INTEGER, INTENT(IN), DIMENSION(:,:) :: PIXEL
  INTEGER                             :: I, J

  NOUT=0                          ! for counting the codes going out
  IF (BLEN<2) THEN
    BLEN=2                        ! pixel code-length, 2 is minimum for GIF
  END IF
  WRITE(F_UNIT) CHAR(BLEN)
  MAXBASE = 2**BLEN - 1
  CALL INITTABLE()
  CALL SLICEWRITE(F_UNIT, CC)

  DO J=1, UBOUND(PIXEL,2)
   DO I=1, UBOUND(PIXEL,1)
    K = MODULO(PIXEL(I,J), MAXBASE+1)    ! take next byte, prevent overflow
    IF (I==1 .AND. J==1) THEN
      P = K                       ! first raster byte has one-byte code P
      CYCLE                       ! for the first byte no further action
    END IF
                                  ! Now see if code exists for sequence [.P.]K
    CHILD = FOLLOW(P)             ! [.P.]K is "string coded by P" followed by K
    CHILDLOOP: DO
      IF ((CHILD == NOCODE) .OR. (ICHAR(ENDBYTE(CHILD)) == K)) THEN
        EXIT CHILDLOOP
      END IF
      CHILD = NEXT(CHILD)
    END DO CHILDLOOP

    IF (CHILD /= NOCODE) THEN     ! If code for [.P.]K was found, store it in P
      P = CHILD
    ELSE                          ! If not: output P and create code for [.P.]K
      CALL SLICEWRITE(F_UNIT, P)
      IF (NCOD > MAXCODE) THEN    ! check if a new code can be added
        CALL SLICEWRITE(F_UNIT, CC)       ! If not: tell listener to clear table
        CALL INITTABLE()          ! and clear our own table
      ELSE
        IF (NCOD > CURMAXCODE) THEN
          SLEN = SLEN+1                     ! New codes will be one bit longer
          CURMAXCODE = CURMAXCODE * 2 + 1   ! and more codes are possible
        END IF
        ENDBYTE(NCOD) = CHAR(K)   ! ncod is the new code for [.P.]K
        FOLLOW(NCOD) = NOCODE
        NEXT(NCOD) = FOLLOW(P)    ! include ncod in the hashing list
        FOLLOW(P) = NCOD          !     of codes with same start-sequence
        NCOD = NCOD+1
      END IF
      P = K
    END IF
   END DO
  END DO
  CALL SLICEWRITE(F_UNIT, P)              ! send the last code to buffer
  CALL SLICEWRITE(F_UNIT, EOI)            ! send 'end of image' to buffer
  CALL FLUSHBUFFER(F_UNIT)        ! extra flush, including partial last byte
  RETURN
END SUBROUTINE GIFLZW
!-----------------------------------------------------------------------------
SUBROUTINE INITTABLE()
  INTEGER :: I

  DO I=0,MAXBASE                  ! Start with defining the codes 0..maxbase
    ENDBYTE(I) = CHAR(I)          ! for one-pixel sequences (code=pixelvalue)
  END DO                          ! Initially no multi-pixel codes exist
  FOLLOW(0:MAXBASE) = NOCODE
  NEXT(0:MAXBASE) = NOCODE
  CC = MAXBASE+1                  ! `clear code-tabel', a control code
  EOI = MAXBASE+2                 ! `end of image', another control code
  NCOD = CC + 2                   ! ncod = number of currently defined codes
  SLEN = BLEN + 1                 ! current number of bits to write one code
  CURMAXCODE = 2**SLEN - 1        ! currently the highest, until slen increases
  RETURN
END SUBROUTINE INITTABLE
!-----------------------------------------------------------------------------
SUBROUTINE OPEN_FOR_WRITE(FNAME, FUNIT)
! Creates a new Stream I/O file returning I/O unit used
! CGP 2009 Jan 28
CHARACTER(LEN=*), INTENT(IN)  :: FNAME
INTEGER, INTENT(OUT)          :: FUNIT
!
LOGICAL :: EXISTS, OPEN
! Get free I/O unit number
DO FUNIT = 90, 7, -1
    INQUIRE(UNIT=FUNIT, EXIST=EXISTS, OPENED=OPEN)
    IF(EXISTS .AND. .NOT. OPEN) EXIT
END DO
IF(FUNIT < 7) STOP 'open_for_write failed - no free I/O units'
OPEN (UNIT=FUNIT, FILE=FNAME, ACCESS="STREAM", STATUS="REPLACE")
END SUBROUTINE OPEN_FOR_WRITE
!-----------------------------------------------------------------------------
SUBROUTINE SLICEWRITE(F_UNIT, CODE)       ! add some bits (a 'slice') to output buffer
  INTEGER, INTENT(IN)  :: F_UNIT
  INTEGER, INTENT(IN)  :: CODE

  IF (NOUT == 0) THEN             ! initiate output buffer
    IBUF = 1
    SKIP = 0
    ACCUM = 0
  END IF
  NOUT = NOUT+1

  ACCUM = ACCUM + CODE * 2**SKIP  ! add bits at correct position in accum
  SKIP = SKIP + SLEN              ! slen is current slice length, in bits

  SHIFTOUT: DO
    BUF(IBUF:IBUF) = CHAR(MODULO(ACCUM, 256))
    IF (SKIP<8) THEN
      EXIT SHIFTOUT
    END IF
    IBUF = IBUF+1                 ! last written buffer-byte is now permanent
    ACCUM = ACCUM / 256           ! remove that byte from accum
    SKIP = SKIP-8                 ! skip points to next bit to write in accum
  END DO SHIFTOUT

  IF (IBUF>255) THEN
    CALL FLUSHBUFFER(F_UNIT)            ! won't write unfinished byte in buf[ibuf]
  END IF
  RETURN                          ! at most 255 bytes will be left in buffer
END SUBROUTINE SLICEWRITE
!-----------------------------------------------------------------------------
SUBROUTINE WRITEGIF (FILENAME, PIXEL, COLORMAP, TRANSPARENT)
!
! Codes pixel-map with palette into GIF format. Optional transparent color
!
CHARACTER(LEN=*), INTENT(IN)            :: FILENAME ! file to create or replace
INTEGER, INTENT(IN), DIMENSION(:,:)     :: PIXEL    ! Pixel values 0 to ncol
INTEGER, INTENT(IN), DIMENSION(:,0:)    :: COLORMAP ! RGB 0:255 for colours 0:ncol
INTEGER, INTENT(IN), OPTIONAL           :: TRANSPARENT ! Optional

  CHARACTER(LEN=256) :: S
  INTEGER            :: INFOBYTE, NX, NY, CBLEN, HASMAP, MAXINCOL,  &
                        MAXGIFCOL, BACKGROUND, I, F_UNIT

  CALL OPEN_FOR_WRITE (FILENAME, F_UNIT)
  NX = UBOUND(PIXEL, 1)
  NY = UBOUND(PIXEL, 2)
  MAXINCOL = UBOUND(COLORMAP,2)
!!  print *,'image size', nx, ny, ' colours', maxincol
  DO I=1,8                           ! find the bitsize, blen, for pixels
    BLEN = I
    MAXGIFCOL = 2**BLEN - 1          ! Number of colors has to be power of 2
    IF (MAXGIFCOL >= MAXINCOL) THEN
      EXIT                           ! now blen and maxgifcol are correct
    END IF                           ! only op to 256 colors can be
  END DO
   WRITE(F_UNIT) "GIF89a"
!  Create information for screen descriptor
  BACKGROUND = 0
  IF (PRESENT(TRANSPARENT)) THEN
    BACKGROUND = TRANSPARENT
  END IF
  HASMAP = 1
  CBLEN = BLEN
  INFOBYTE = HASMAP * 128 + (CBLEN-1) * 16 + BLEN-1
!  Write the screen descriptor
  WRITE(F_UNIT) CHAR2(NX), CHAR2(NY), CHAR(INFOBYTE), CHAR(BACKGROUND), CHAR(0)
  DO I=0,MAXGIFCOL                                 ! write global colormap
    WRITE(F_UNIT) CHAR(COLORMAP(1,MIN(I,MAXINCOL))), &
                  CHAR(COLORMAP(2,MIN(I,MAXINCOL))), &
                  CHAR(COLORMAP(3,MIN(I,MAXINCOL)))
  END DO
  IF (PRESENT(TRANSPARENT)) THEN
    WRITE(UNIT=*,FMT=*) "Transparent color: ", TRANSPARENT
    S = "!" // CHAR(249) // CHAR(4) // CHAR(1) // CHAR(0) // CHAR(0)  &
            // CHAR(TRANSPARENT) // CHAR(0)
    WRITE(F_UNIT) S(1:8)                            ! GIF transparent extension
  END IF
   WRITE(F_UNIT) ","                                ! Announce image
!    Now create and write image descriptor
  HASMAP = 0
  INFOBYTE = HASMAP * 128 + BLEN-1                 ! add 64, if interlaced
!    x_margin, y_margin (not used), image dimensions
  WRITE(F_UNIT) CHAR2(0), CHAR2(0), CHAR2(NX), CHAR2(NY), CHAR(INFOBYTE)
  CALL GIFLZW (F_UNIT, PIXEL)                       ! now the raster data
  WRITE(F_UNIT) CHAR(0), ';'                    ! Terminating 0-block ; for GIF
  CLOSE(UNIT=F_UNIT)
  RETURN
END SUBROUTINE WRITEGIF
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
END MODULE ModLib_WriteGif
