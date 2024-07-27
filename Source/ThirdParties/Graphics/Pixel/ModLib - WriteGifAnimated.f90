!>
!!##NAME
!!    write_animated_gif(3f) - [ModLib_WriteGif_Animated] Codes pixel-maps with palette into animated GIF format. Optional transparent color
!!
!!##SYNOPSIS
!!
!!   subroutine write_animated_gif(filename,pixel,colormap,transparent,delay)
!!
!!    character(len=*),intent(in)         :: filename
!!    integer,intent(in),dimension(:,:,:) :: pixel
!!    integer,intent(in),dimension(:,0:)  :: colormap
!!    integer,intent(in),optional         :: transparent
!!    integer,intent(in),optional         :: delay
!!
!!##DESCRIPTION
!!    Writes gif89 image, given pixel array and color map.
!!    This version can create an animated gif.
!!
!!##OPTIONS
!!    FileName       file to create or replace
!!    Pixel          Pixel values 0 to ncol
!!    ColorMap       Color map (RGB 0:255 for colours 0:ncol)
!!    Transparent    Transparent color; optional
!!    Delay          Delay time [ 1/100 of seconds]; optional
!!
!!##EXAMPLE
!!
!!   Sample call:
!!
!!    program demo_write_animated_gif
!!    !***************************************************************************
!!    !> author: Jacob Williams
!!    !
!!    !  Use the gif module to create a sample animated gif.
!!    !
!!    !# See also
!!    !  * [Make a circle illusion animation](http://codegolf.stackexchange.com/questions/34887/make-a-circle-illusion-animation)
!!    !
!!    use, intrinsic :: iso_fortran_env, only: wp=>real64
!!    use ModLib_WriteGif_Animated, only : write_animated_gif
!!    implicit none
!!    !
!!    logical,parameter :: new = .true.
!!    !
!!    integer,parameter  :: n        = 200  !! size of image (square)
!!    real(wp),parameter :: rcircle  = n/2  !! radius of the big circle
!!    integer,parameter  :: time_sep = 5    !! deg
!!    !
!!    real(wp),parameter :: deg2rad = acos(-1.0_wp)/180.0_wp
!!    !
!!    integer,dimension(:,:,:),allocatable :: pixel    !! pixel values
!!    !
!!    real(wp),dimension(2) :: xy
!!    real(wp)              :: r,t
!!    integer               :: i,j,k,row,col,m,n_cases,ang_sep,iframe
!!    !
!!    integer,dimension(3,0:5)  :: colormap
!!    integer,parameter  :: white = 0
!!    integer,parameter  :: gray  = 1
!!    integer,parameter  :: red   = 2
!!    integer,parameter  :: green = 3
!!    integer,parameter  :: blue  = 4
!!    integer,parameter  :: black = 5
!!    !
!!    colormap(:,black) = [0,0,0]
!!    colormap(:,white) = [255,255,255]
!!    colormap(:,gray)  = [200,200,200]
!!    colormap(:,red)   = [255,0,0]
!!    colormap(:,green) = [0,255,0]
!!    colormap(:,blue)  = [0,0,255]
!!    !
!!    if (new) then
!!        ang_sep = 5
!!        n_cases = 3
!!    else
!!        ang_sep = 20
!!        n_cases = 0
!!    end if
!!    !
!!    !how many frames:
!!    iframe=0
!!    do k=0,355,time_sep
!!        iframe=iframe+1
!!    end do
!!    allocate(pixel(iframe,0:n,0:n))
!!    !
!!    iframe=0
!!    do k=0,355,time_sep
!!        !frame number:
!!        iframe=iframe+1
!!        !clear entire image:
!!        pixel(iframe,:,:) = white
!!        if (new) call draw_circle(n/2,n/2,red,n/2)
!!        !draw polar grid:
!!        do j=0,180-ang_sep,ang_sep
!!            do i=-n/2, n/2
!!                call spherical_to_cartesian(dble(i),dble(j)*deg2rad,xy)
!!                call convert(xy,row,col)
!!                if (new) then
!!                    pixel(iframe,row,col) = gray
!!                else
!!                    pixel(iframe,row,col) = black
!!                end if
!!            end do
!!        end do
!!        !draw dots:
!!        do m=0,n_cases
!!            do j=0,360-ang_sep,ang_sep
!!                r = sin(m*90.0_wp*deg2rad + (k + j)*deg2rad)*rcircle
!!                t = dble(j)*deg2rad
!!                call spherical_to_cartesian(r,t,xy)
!!                call convert(xy,row,col)
!!                if (new) then
!!                    !call draw_circle(row,col,black,10)    !v2
!!                    !call draw_circle(row,col,m,5)         !v2
!!                    call draw_circle(row,col,mod(j,3)+3,5) !v3
!!                else
!!                    call draw_square(row,col,red)          !v1
!!                end if
!!            end do
!!        end do
!!    end do
!!    !
!!    call write_animated_gif('circle_illusion.gif',pixel,colormap,delay=5)
!!    !
!!    deallocate(pixel)
!!    !
!!    contains
!!    !***************************************************************************
!!    !> author: Jacob Williams
!!    !
!!    !  Draw a square.
!!    !
!!    subroutine draw_square(r,c,icolor)
!!    implicit none
!!    integer,intent(in) :: r      !! row of center
!!    integer,intent(in) :: c      !! col of center
!!    integer,intent(in) :: icolor !! color value
!!    !
!!    integer,parameter :: d = 10 !square size
!!    !
!!        pixel(iframe,max(0,r-d):min(n,r+d),max(0,c-d):min(n,c+d)) = icolor
!!    !
!!    end subroutine draw_square
!!    !***************************************************************************
!!    !> author: Jacob Williams
!!    !
!!    !  Draw a circle.
!!    subroutine draw_circle(r,c,icolor,d)
!!    implicit none
!!    !
!!    integer,intent(in) :: r      !! row of center
!!    integer,intent(in) :: c      !! col of center
!!    integer,intent(in) :: icolor !! color value
!!    integer,intent(in) :: d      !! diameter
!!    !
!!    integer :: i,j
!!    !
!!        do i=max(0,r-d),min(n,r+d)
!!            do j=max(0,c-d),min(n,c+d)
!!                if (sqrt(dble(i-r)**2 + dble(j-c)**2)<=d) &
!!                    pixel(iframe,i,j) = icolor
!!            end do
!!        end do
!!    !
!!    end subroutine draw_circle
!!    !***************************************************************************
!!    !> author: Jacob Williams
!!    !
!!    !  Convert from x,y to row,col.
!!    subroutine convert(xy,row,col)
!!    implicit none
!!    !
!!    real(wp),dimension(2),intent(in) :: xy  !! coordinates
!!    integer,intent(out) :: row
!!    integer,intent(out) :: col
!!    !
!!       row = int(-xy(2) + n/2.0_wp)
!!       col = int( xy(1) + n/2.0_wp)
!!    !
!!    end subroutine convert
!!    !***************************************************************************
!!    !> author: Jacob Williams
!!    !
!!    !  Convert spherical to cartesian coordinates.
!!    subroutine spherical_to_cartesian(r,theta,xy)
!!    implicit none
!!    !
!!    real(wp),intent(in) :: r
!!    real(wp),intent(in) :: theta
!!    real(wp),dimension(2),intent(out) :: xy
!!    !
!!       xy(1) = r * cos(theta)
!!       xy(2) = r * sin(theta)
!!    !
!!    end subroutine spherical_to_cartesian
!!    !***************************************************************************
!!    end program demo_write_animated_gif
!!    !***************************************************************************
!!
!!##AUTHOR
!!    o Version 1.01, August 1999, Written by Jos Bergervoet
!!    o 2008 Jan 28: Modified by Clive Page to use stream I/O, array as colourmap.
!!    * Jacob Williams, 7/27/2014. Refactored, updated, added ability to export animated gifs.
!!    o Minor modifications to make more easily used with ModLib_Pixel(3f) module, 2017-July-06, John Urban
!!##LICENSE
!!   Copyright (c) 2014-2015, Jacob Williams.
!!   All rights reserved.
!!
!!   Redistribution and use in source and binary forms, with or without
!!   modification, are permitted provided that the following conditions are met:
!!
!!   * Redistributions of source code must retain the above copyright notice, this
!!     list of conditions and the following disclaimer.
!!
!!   * Redistributions in binary form must reproduce the above copyright notice,
!!     this list of conditions and the following disclaimer in the documentation
!!     and/or other materials provided with the distribution.
!!
!!   * Neither the name of the {organization} nor the names of its
!!     contributors may be used to endorse or promote products derived from
!!     this software without specific prior written permission.
!!
!!   This software is provided by the copyright holders and contributors "AS IS"
!!   and any express or implied warranties, including, but not limited to, the
!!   implied warranties of merchantability and fitness for a particular purpose are
!!   disclaimed. In no event shall the copyright holder or contributors be liable
!!   for any direct, indirect, incidental, special, exemplary, or consequential
!!   damages (including, but not limited to, procurement of substitute goods or
!!   services; loss of use, data, or profits; or business interruption) However
!!   caused and on any theory of liability, whether in contract, strict liability,
!!   or tort (Including negligence or otherwise) arising in any way out of the use
!!   of this software, even if advised of the possibility of such damage.
!>
!  Conversion of raster data to GIF89 format.
!
!# See also
!   * The original code (License: public domain) was from
!     [here](http://fortranwiki.org/fortran/show/writegif)
!
!# History
!   * Version 1.01, August 1999, Written by Jos Bergervoet
!   * 2008 Jan 28: Modified by Clive Page to use stream I/O, array as colourmap.
!   * Jacob Williams, 7/27/2014. Refactored, updated, added ability to export animated gifs.
!
!-----------------------------------------------------------------------------------------------------------------------------------
    MODULE ModLib_WriteGif_Animated
    IMPLICIT NONE
    PRIVATE

    PUBLIC :: WRITE_ANIMATED_GIF

    CONTAINS
!-----------------------------------------------------------------------------------------------------------------------------------
!> author: Jacob Williams
!  date: 7/27/2014
!
!  Writes gif89 image, given pixel array and color map
!  This version can create an animated gif:
!
!  * The pixel matrix is rank 3: image i is pixel(i,:,:)
!  * If size(pixel,1) is 1, then a regular gif is produced.
!
!# See also
!   1. [writegif](http://fortranwiki.org/fortran/show/writegif)
!   2. [GIF format](http://www.onicos.com/staff/iz/formats/gif.html#aeb)
!   3. [GIF File Format Summary](http://www.fileformat.info/format/gif/egff.htm)

    SUBROUTINE WRITE_ANIMATED_GIF(FILENAME,PIXEL,COLORMAP,TRANSPARENT,DELAY)

    CHARACTER(LEN=*),INTENT(IN)         :: FILENAME    !! file to create or replace
    INTEGER,INTENT(IN),DIMENSION(:,:,:) :: PIXEL       !! pixel values [0 to ncol]
    INTEGER,INTENT(IN),DIMENSION(:,0:)  :: COLORMAP    !! [r,g,b (0:255)] , [0:ncol] colors
    INTEGER,INTENT(IN),OPTIONAL         :: TRANSPARENT !! transparent color
    INTEGER,INTENT(IN),OPTIONAL         :: DELAY       !! delay time [1/100 of seconds]

    INTEGER,PARAMETER     :: BUFEND=260
    CHARACTER(LEN=BUFEND) :: BUF
    INTEGER               :: IBUF ! output buffer vars
    INTEGER,PARAMETER     :: MAXCODE = 4095
    INTEGER,PARAMETER     :: NOCODE = MAXCODE+1 !! definitions for lzw

    ! define lzw code tables for hashing:
    !
    ! for any code p, which codes for a sequence af pixel-values, endbyte(p)
    ! is the last pixel-value, follow(p) points to another code (if it exists)
    ! which codes for this same sequence, but with one more pixel-value
    ! appended.
    !   for each code p, next(p) points to another code which codes for a
    ! similar sequence with only the endbyte different. this is a hashing
    ! pointer, for fast look-up.
    !   all pointers are 'nocode' if they point to nothing
    !
    CHARACTER(LEN=1),DIMENSION(0:MAXCODE+1) :: ENDBYTE
    INTEGER,DIMENSION(0:MAXCODE) :: FOLLOW, NEXT
    INTEGER :: NCOD, CURMAXCODE, EOI, CC, P, K, CHILD, &
                    MAXBASE, SKIP, SLEN, BLEN, ACCUM, NOUT

    INTEGER :: INFOBYTE,NX,NY,CBLEN,HASMAP,MAXINCOL,ISTAT,&
                MAXGIFCOL,BACKGROUND,I,IUNIT,IFRAME,N,DT
    CHARACTER(LEN=1),DIMENSION(2) :: T

    !delay time:
    IF (PRESENT(DELAY)) THEN
        DT = DELAY
    ELSE
        DT = 1
    END IF

    !transparency info:
    IF (PRESENT(TRANSPARENT)) THEN
        T(1) = CHAR(1) !Reserved+Disposal Method+User Input Flag+Transparent Color Flag
        T(2) = CHAR(TRANSPARENT)
    ELSE
        T(1) = CHAR(0)
        T(2) = CHAR(0)
    END IF

    OPEN(    NEWUNIT=IUNIT,&
            FILE=TRIM(FILENAME),&
            ACCESS='STREAM',&
            STATUS='REPLACE',&
            IOSTAT=ISTAT)

    IF (ISTAT==0) THEN

        N = SIZE(PIXEL,1) !number of images
        NX = UBOUND(PIXEL, 2)
        NY = UBOUND(PIXEL, 3)
        MAXINCOL = UBOUND(COLORMAP,2)

        DO I=1,8                            ! find the bitsize, blen, for pixels
            BLEN = I
            MAXGIFCOL = 2**BLEN - 1         ! Number of colors has to be power of 2
            IF (MAXGIFCOL>=MAXINCOL) EXIT   ! now blen and maxgifcol are correct
                                            ! [only up to 256 colors]
        END DO

        !------------
        ! GIF Header
        !------------

        WRITE(IUNIT) 'GIF89a'

        ! create information for screen descriptor
        IF (PRESENT(TRANSPARENT)) THEN
            BACKGROUND = TRANSPARENT
        ELSE
            BACKGROUND = 0
        END IF
        HASMAP = 1
        CBLEN = BLEN
        INFOBYTE = HASMAP * 128 + (CBLEN-1) * 16 + BLEN-1

        ! write the screen descriptor
        WRITE(IUNIT)   CHAR2(NX),&         ! logical screen width
                        CHAR2(NY),&         ! logical screen height
                        CHAR(INFOBYTE),&    ! screen and color map information
                        CHAR(BACKGROUND),&  ! background color index
                        CHAR(0)             ! pixel aspect ratio

        ! write global colormap
        DO I=0,MAXGIFCOL
            WRITE(IUNIT)  CHAR(COLORMAP(1,MIN(I,MAXINCOL))), &
                          CHAR(COLORMAP(2,MIN(I,MAXINCOL))), &
                          CHAR(COLORMAP(3,MIN(I,MAXINCOL)))
        END DO

        IF (N>1) THEN    !it is an animated gif

            !-----------------------------
            ! Application Extension Block
            !-----------------------------

            ! See: http://odur.let.rug.nl/kleiweg/gif/netscape.html

            WRITE(IUNIT)   '!',&           ! Extension Introducer (0x21)
                            CHAR(255),&     ! GIF Extension code
                            CHAR(11),&      ! Length of Application Block
                            'NETSCAPE',&    ! Application Identifier
                            '2.0',&         ! Application Authentication Code
                            CHAR(3),&       ! Length of Data Sub-Block
                            CHAR(1),&        ! 1 (0x01)
                            CHAR(0),&       ! number of loop iterations
                            CHAR(0),&       ! Data Sub-Block Terminator
                            CHAR(0)         ! Block Terminator (0x00)

        END IF

        !each frame of the animated gif:
        DO IFRAME = 1,N

            !---------------------------------
            ! Graphic Control Extension Block
            !---------------------------------

            WRITE(IUNIT)   '!',&           ! Extension Introducer (0x21)
                            CHAR(249),&    ! Graphic Control Label (0xF9)
                            CHAR(4),&      ! Block Size (0x04)
                            T(1),&         !
                            CHAR2(DT),&    ! Delay Time
                            T(2),&         ! Transparent Color Index
                            CHAR(0)        ! Block Terminator (0x00)

            !-------------
            ! Image Block
            !-------------

            ! now create and write image descriptor
            HASMAP = 0
            INFOBYTE = HASMAP * 128 + BLEN-1    ! add 64, if interlaced

            WRITE(IUNIT)   ',',&            ! Image Separator (0x2C)
                            CHAR2(0),&      ! Image Left Position
                            CHAR2(0),&      ! Image Top Position
                            CHAR2(NX),&     ! Image Width
                            CHAR2(NY),&     ! Image Height
                            CHAR(INFOBYTE)  ! Image and Color Table Data Information

            CALL GIFLZW(IUNIT,PIXEL(IFRAME,:,:))  ! now the raster data

            WRITE(IUNIT) CHAR(0)    ! Block Terminator (0x00)

        END DO

        !---------
        ! Trailer
        !---------

        WRITE(IUNIT) ';'

    ELSE
        WRITE(*,*) 'Error opening :'//TRIM(FILENAME)
    END IF

    !close the gif file:
    CLOSE(UNIT=IUNIT,IOSTAT=ISTAT)

CONTAINS
!-----------------------------------------------------------------------------------------------------------------------------------
    !>
    !  Convert the two least sig bytes of an integer to a 2-character string

FUNCTION CHAR2(IVAL) RESULT(C)
INTEGER, INTENT(IN) :: IVAL
CHARACTER(LEN=2)    :: C

   C = ACHAR(MOD(IVAL,256)) // ACHAR(MOD(IVAL/256,256))

END FUNCTION CHAR2
!-----------------------------------------------------------------------------------------------------------------------------------
    !>
    !  Flushes up to 255 bytes to output file if buffer contains data, keeping
    !  rest of data in buffer. If skip>0 there is a partially filled last byte
    !  in buf[ibuf]. This byte will be written only if ibuf<256. That should be
    !  the last call to flushbuffer.

SUBROUTINE FLUSHBUFFER(IUNIT)
INTEGER, INTENT(IN) :: IUNIT   !! i/o unit to use
INTEGER :: BL   !! number of bytes to write (to be determined)

   IF (IBUF > 255) THEN     ! we will write buf[1..255]
      BL = 255
   ELSE IF (SKIP /= 0) THEN ! buf[ibuf] is partially used, write buf[1..ibuf]
      BL = IBUF
   ELSE IF (IBUF > 1) THEN  ! write buf[1..ibuf-1], there is no partial byte
      BL = IBUF-1
   ELSE                     ! nothing to write
      RETURN
   END IF

   WRITE(IUNIT) CHAR(BL)
   WRITE(IUNIT) BUF(1:BL)
   BUF(1:IBUF-BL) = BUF(BL+1:IBUF) ! shift down remaining data
   IBUF = IBUF - BL

END SUBROUTINE FLUSHBUFFER
!-----------------------------------------------------------------------------------------------------------------------------------
    !>
    !  routine for LZW coding

        SUBROUTINE GIFLZW(IUNIT, PIXEL)

        INTEGER, INTENT(IN)                 :: IUNIT
        INTEGER, INTENT(IN), DIMENSION(:,:) :: PIXEL
        INTEGER                             :: I
        INTEGER                             :: J

        NOUT=0                        ! for counting the codes going out
        IF (BLEN<2) THEN
            BLEN=2                    ! pixel code-length, 2 is minimum for gif
        END IF
        WRITE(IUNIT) CHAR(BLEN)
        MAXBASE = 2**BLEN - 1
        CALL INITTABLE()
        CALL SLICEWRITE(IUNIT, CC)

        DO J=1, UBOUND(PIXEL,2)
            DO I=1, UBOUND(PIXEL,1)
                K = MODULO(PIXEL(I,J), MAXBASE+1)    ! take next byte, prevent overflow
                IF (I==1 .AND. J==1) THEN
                    P = K                       ! first raster byte has one-byte code p
                    CYCLE                       ! for the first byte no further action
                END IF
                                            ! now see if code exists for sequence [.p.]k
                CHILD = FOLLOW(P)           ! [.p.]k is "string coded by p" followed by k
                CHILDLOOP: DO
                    IF ((CHILD == NOCODE) .OR. (ICHAR(ENDBYTE(CHILD)) == K)) THEN
                        EXIT CHILDLOOP
                    END IF
                    CHILD = NEXT(CHILD)
                END DO CHILDLOOP

                IF (CHILD /= NOCODE) THEN    ! if code for [.p.]k was found, store it in p
                    P = CHILD
                ELSE                         ! if not: output p and create code for [.p.]k
                    CALL SLICEWRITE(IUNIT, P)
                    IF (NCOD > MAXCODE) THEN        ! check if a new code can be added
                        CALL SLICEWRITE(IUNIT, CC)  ! if not: tell listener to clear table
                        CALL INITTABLE()            ! and clear our own table
                    ELSE
                        IF (NCOD > CURMAXCODE) THEN
                            SLEN = SLEN+1               ! new codes will be one bit longer
                            CURMAXCODE = CURMAXCODE * 2 + 1 ! and more codes are possible
                        END IF
                        ENDBYTE(NCOD) = CHAR(K)   ! ncod is the new code for [.p.]k
                        FOLLOW(NCOD) = NOCODE
                        NEXT(NCOD) = FOLLOW(P)    ! include ncod in the hashing list
                        FOLLOW(P) = NCOD          !     of codes with same start-sequence
                        NCOD = NCOD+1
                    END IF
                    P = K
                END IF
            END DO
        END DO

        CALL SLICEWRITE(IUNIT, P)      ! send the last code to buffer
        CALL SLICEWRITE(IUNIT, EOI)    ! send 'end of image' to buffer
        CALL FLUSHBUFFER(IUNIT)        ! extra flush, including partial last byte

        END SUBROUTINE GIFLZW
!-----------------------------------------------------------------------------------------------------------------------------------
    !>
    !  Initialize table.

        SUBROUTINE INITTABLE()

        INTEGER :: I

        DO I=0,MAXBASE                  ! start with defining the codes 0..maxbase
            ENDBYTE(I) = CHAR(I)        ! for one-pixel sequences (code=pixelvalue)
        END DO                          ! initially no multi-pixel codes exist
        FOLLOW(0:MAXBASE) = NOCODE
        NEXT(0:MAXBASE) = NOCODE
        CC = MAXBASE+1                  ! 'clear code-tabel', a control code
        EOI = MAXBASE+2                 ! 'end of image', another control code
        NCOD = CC + 2                   ! ncod = number of currently defined codes
        SLEN = BLEN + 1                 ! current number of bits to write one code
        CURMAXCODE = 2**SLEN - 1        ! currently the highest, until slen increases

        END SUBROUTINE INITTABLE
!-----------------------------------------------------------------------------------------------------------------------------------
    !>
    !  add some bits (a 'slice') to output buffer

        SUBROUTINE SLICEWRITE(IUNIT, CODE)

        INTEGER, INTENT(IN)  :: IUNIT
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
            BUF(IBUF:IBUF) = CHAR(MODULO(ACCUM,256))
            IF (SKIP<8) EXIT SHIFTOUT
            IBUF = IBUF+1               ! last written buffer-byte is now permanent
            ACCUM = ACCUM/256           ! remove that byte from accum
            SKIP = SKIP-8               ! skip points to next bit to write in accum
        END DO SHIFTOUT

        IF (IBUF>255) THEN
            CALL FLUSHBUFFER(IUNIT)    ! won't write unfinished byte in buf[ibuf]
        END IF

        ! at most 255 bytes will be left in buffer

        END SUBROUTINE SLICEWRITE
!-----------------------------------------------------------------------------------------------------------------------------------
    END SUBROUTINE WRITE_ANIMATED_GIF
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
    END MODULE ModLib_WriteGif_Animated
!-----------------------------------------------------------------------------------------------------------------------------------
