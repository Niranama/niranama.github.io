MODULE ModLib_FglDraw

   USE ModLib_FglImage

   IMPLICIT NONE

   TYPE POINT
      INTEGER :: X, Y
   END TYPE POINT

   TYPE BUCKET
      INTEGER :: YMAX, YMIN, X, SIGN, DX, DY, SUMM
   END TYPE BUCKET

   INTERFACE DRAW_LINE
      MODULE PROCEDURE DRAW_LINERGB
      MODULE PROCEDURE DRAW_LINERGBA
   END INTERFACE DRAW_LINE

   INTERFACE DRAW_RECTANGLE
      MODULE PROCEDURE DRAW_RECTANGLERGB
      MODULE PROCEDURE DRAW_RECTANGLERGB_A
      MODULE PROCEDURE DRAW_RECTANGLERGBA
   END INTERFACE

   INTERFACE DRAW_CIRCLE
      MODULE PROCEDURE DRAW_CIRCLERGB
      ! module procedure draw_circleRGB_A
      MODULE PROCEDURE DRAW_CIRCLERGBA
   END INTERFACE

   INTERFACE FLOOD_FILL
      MODULE PROCEDURE FLOOD_FILLRGB
      MODULE PROCEDURE FLOOD_FILLRGBA
   END INTERFACE

CONTAINS

   SUBROUTINE DRAW_LINERGB(IMG, P1, P2, COLOUR)

      IMPLICIT NONE

      TYPE(RGBIMAGE),      INTENT(INOUT) :: IMG
      TYPE(RGB), OPTIONAL, INTENT(IN)    :: COLOUR
      TYPE(POINT),         INTENT(IN)    :: P1, P2
      TYPE(RGB)                          :: C
      TYPE(POINT)                        :: P1T, P2T
      INTEGER                            :: X, Y, DX, DY, ERROR, YSTEP
      LOGICAL                            :: STEEP

      IF(.NOT.PRESENT(COLOUR))THEN
         C = RGB(0, 0, 0)
      ELSE
         C = COLOUR
      END IF

      P1T = P1
      P2T = P2

      STEEP = (ABS(P2T%Y - P1T%Y) > ABS(P2T%X -P1T%X))

      IF(STEEP)THEN
         CALL SWAP(P1T%X, P1T%Y)
         CALL SWAP(P2T%X, P2T%Y)
      END IF

      IF(P1T%X > P2T%X)THEN
         CALL SWAP(P1T%X, P2T%X)
         CALL SWAP(P1T%Y, P2T%Y)
      END IF

      DX = P2T%X - P1T%X
      DY = ABS(P2T%Y - P1T%Y)
      ERROR = DX/2
      Y = P1T%Y

      IF(P1T%Y < P2T%Y)THEN
         YSTEP = 1
      ELSE
         YSTEP = -1
      END IF

    DO X = P1T%X, P2T%X
       IF (STEEP) THEN
          CALL SET_PIXEL(IMG, Y, X, C)
       ELSE
          CALL SET_PIXEL(IMG, X, Y, C)
       END IF
       ERROR = ERROR - DY
       IF ( ERROR < 0 ) THEN
          Y = Y + YSTEP
          ERROR = ERROR + DX
       END IF
    END DO
   END SUBROUTINE DRAW_LINERGB

   SUBROUTINE DRAW_LINERGBA(IMG, P1, P2, COLOUR)

      IMPLICIT NONE

      TYPE(RGBAIMAGE),      INTENT(INOUT) :: IMG
      TYPE(RGBA), OPTIONAL, INTENT(IN)    :: COLOUR
      TYPE(POINT),         INTENT(IN)    :: P1, P2
      TYPE(RGBA)                          :: C
      TYPE(POINT)                        :: P1T, P2T
      INTEGER                            :: X, Y, DX, DY, ERROR, YSTEP
      LOGICAL                            :: STEEP

      IF(.NOT.PRESENT(COLOUR))THEN
         C = RGBA(0, 0, 0, 0)
      ELSE
         C = COLOUR
      END IF

      P1T = P1
      P2T = P2

      STEEP = (ABS(P2T%Y - P1T%Y) > ABS(P2T%X -P1T%X))

      IF(STEEP)THEN
         CALL SWAP(P1T%X, P1T%Y)
         CALL SWAP(P2T%X, P2T%Y)
      END IF

      IF(P1T%X > P2T%X)THEN
         CALL SWAP(P1T%X, P2T%X)
         CALL SWAP(P1T%Y, P2T%Y)
      END IF

      DX = P2T%X - P1T%X
      DY = ABS(P2T%Y - P1T%Y)
      ERROR = DX/2
      Y = P1T%Y

      IF(P1T%Y < P2T%Y)THEN
         YSTEP = 1
      ELSE
         YSTEP = -1
      END IF

    DO X = P1T%X, P2T%X
       IF (STEEP) THEN
          CALL SET_PIXEL(IMG, Y, X, C)
       ELSE
          CALL SET_PIXEL(IMG, X, Y, C)
       END IF
       ERROR = ERROR - DY
       IF ( ERROR < 0 ) THEN
          Y = Y + YSTEP
          ERROR = ERROR + DX
       END IF
    END DO
   END SUBROUTINE DRAW_LINERGBA


   SUBROUTINE DRAW_RECTANGLERGB(IMG, P1, P2, COLOUR, FILL)

      IMPLICIT NONE

      TYPE(RGBIMAGE),    INTENT(INOUT) :: IMG
      TYPE(RGB),         INTENT(IN)    :: COLOUR
      TYPE(POINT),       INTENT(INOUT) :: P1, P2
      LOGICAL, OPTIONAL, INTENT(IN)    :: FILL
      INTEGER                          :: I, J
      LOGICAL                          :: FLAG, XF, YF

      IF(PRESENT(FILL))THEN
         IF(FILL)THEN
            FLAG = .TRUE.
         ELSE
            FLAG = .FALSE.
         END IF
      ELSE
         FLAG = .FALSE.
      END IF

      IF(P1%X > P2%X)THEN
         CALL SWAP(P1%X, P2%X)
         CALL SWAP(P1%Y, P2%Y)
         XF = .TRUE.
      END IF

      IF(P1%Y > P2%Y.AND..NOT.XF)THEN
         CALL SWAP(P1%Y, P2%Y)
         YF = .TRUE.
      END IF

      IF(FLAG)THEN
         DO J = P1%Y , P2%Y
            DO I = P1%X , P2%X
               CALL SET_PIXEL(IMG, I, J, COLOUR)
            END DO
         END DO
      ELSE
         DO I = P1%X , P2%X
            CALL SET_PIXEL(IMG, I, P1%Y, COLOUR)
            CALL SET_PIXEL(IMG, I, P2%Y, COLOUR)
         END DO

         DO I = P1%Y , P2%Y
            CALL SET_PIXEL(IMG, P1%X, I, COLOUR)
            CALL SET_PIXEL(IMG, P2%X, I, COLOUR)
         END DO
      END IF

      IF(XF)THEN
         CALL SWAP(P1%X, P2%X)
         CALL SWAP(P1%Y, P2%Y)
      ELSEIF(YF.AND..NOT.XF)THEN
         CALL SWAP(P1%X, P2%X)
         CALL SWAP(P1%Y, P2%Y)
      END IF
   END SUBROUTINE DRAW_RECTANGLERGB


   SUBROUTINE DRAW_RECTANGLERGB_A(IMG, P1, P2, COLOUR, FILL)

      IMPLICIT NONE

      TYPE(RGBAIMAGE),    INTENT(INOUT) :: IMG
      TYPE(RGB),         INTENT(IN)     :: COLOUR
      TYPE(POINT),       INTENT(INOUT)  :: P1, P2
      LOGICAL, OPTIONAL, INTENT(IN)     :: FILL
      TYPE(RGBA)                        :: C_A
      INTEGER                           :: I, J
      LOGICAL                           :: FLAG, XF, YF

      C_A = RGBA(COLOUR%RED, COLOUR%GREEN, COLOUR%GREEN, 255)

      IF(PRESENT(FILL))THEN
         IF(FILL)THEN
            FLAG = .TRUE.
         ELSE
            FLAG = .FALSE.
         END IF
      ELSE
         FLAG = .FALSE.
      END IF

      IF(P1%X > P2%X)THEN
         CALL SWAP(P1%X, P2%X)
         CALL SWAP(P1%Y, P2%Y)
         XF = .TRUE.
      END IF

      IF(P1%Y > P2%Y.AND..NOT.XF)THEN
         CALL SWAP(P1%Y, P2%Y)
         YF = .TRUE.
      END IF

      IF(FLAG)THEN
         DO J = P1%Y , P2%Y
            DO I = P1%X , P2%X
               CALL SET_PIXEL(IMG, I, J, C_A)
            END DO
         END DO
      ELSE
         DO I = P1%X , P2%X
            CALL SET_PIXEL(IMG, I, P1%Y, C_A)
            CALL SET_PIXEL(IMG, I, P2%Y, C_A)
         END DO

         DO I = P1%Y , P2%Y
            CALL SET_PIXEL(IMG, P1%X, I, C_A)
            CALL SET_PIXEL(IMG, P2%X, I, C_A)
         END DO
      END IF

      IF(XF)THEN
         CALL SWAP(P1%X, P2%X)
         CALL SWAP(P1%Y, P2%Y)
      ELSEIF(YF.AND..NOT.XF)THEN
         CALL SWAP(P1%X, P2%X)
         CALL SWAP(P1%Y, P2%Y)
      END IF
   END SUBROUTINE DRAW_RECTANGLERGB_A


   SUBROUTINE DRAW_RECTANGLERGBA(IMG, P1, P2, COLOUR, FILL, BLEND)

      IMPLICIT NONE

      TYPE(RGBAIMAGE),   INTENT(INOUT) :: IMG
      TYPE(RGBA),        INTENT(IN)    :: COLOUR
      TYPE(POINT),       INTENT(INOUT) :: P1, P2
      LOGICAL, OPTIONAL, INTENT(IN)    :: FILL, BLEND
      TYPE(RGBA)                       :: C1, C2
      INTEGER                          :: I, J
      LOGICAL                          :: FLAG_F, FLAG_B, XF, YF

      IF(PRESENT(BLEND))THEN
         IF(BLEND)THEN
            FLAG_B = .TRUE.
         ELSE
            FLAG_B = .FALSE.
         END IF
      ELSE
         FLAG_B = .FALSE.
      END IF

      IF(PRESENT(FILL))THEN
         IF(FILL)THEN
            FLAG_F = .TRUE.
         ELSE
            FLAG_F = .FALSE.
         END IF
      ELSE
         FLAG_F = .FALSE.
      END IF

      IF(P1%X > P2%X)THEN
         CALL SWAP(P1%X, P2%X)
         CALL SWAP(P1%Y, P2%Y)
         XF = .TRUE.
      END IF

      IF(P1%Y > P2%Y.AND..NOT.XF)THEN
         CALL SWAP(P1%Y, P2%Y)
         YF = .TRUE.
      END IF

      IF(FLAG_F.AND.FLAG_B)THEN
         DO J = P1%Y , P2%Y
            DO I = P1%X , P2%X
               CALL GET_PIXEL(IMG, I, J, C1)
               C2 = ALPHA_COMP(COLOUR, C1)
               CALL SET_PIXEL(IMG, I, J, C2)
            END DO
         END DO
      ELSEIF(FLAG_F.AND..NOT.FLAG_B)THEN
         DO J = P1%Y , P2%Y
            DO I = P1%X , P2%X
               CALL SET_PIXEL(IMG, I, J, COLOUR)
            END DO
         END DO
      ELSE
         DO I = P1%X , P2%X
            CALL SET_PIXEL(IMG, I, P1%Y, COLOUR)
            CALL SET_PIXEL(IMG, I, P2%Y, COLOUR)
         END DO

         DO I = P1%Y , P2%Y
            CALL SET_PIXEL(IMG, P1%X, I, COLOUR)
            CALL SET_PIXEL(IMG, P2%X, I, COLOUR)
         END DO
      END IF

      IF(XF)THEN
         CALL SWAP(P1%X, P2%X)
         CALL SWAP(P1%Y, P2%Y)
      ELSEIF(YF.AND..NOT.XF)THEN
         CALL SWAP(P1%X, P2%X)
         CALL SWAP(P1%Y, P2%Y)
      END IF
   END SUBROUTINE DRAW_RECTANGLERGBA


   SUBROUTINE DRAW_CIRCLERGB(IMG, P, RADIUS, COLOUR, FILL)

      IMPLICIT NONE

      TYPE(RGBIMAGE),    INTENT(INOUT) :: IMG
      TYPE(RGB),         INTENT(IN)    :: COLOUR
      TYPE(POINT),       INTENT(IN)    :: P
      INTEGER,           INTENT(IN)    :: RADIUS
      LOGICAL, OPTIONAL, INTENT(IN)    :: FILL
      INTEGER                          :: X, Y, ERROR

      X = RADIUS
      Y = 0
      ERROR = 0

      DO WHILE( X >= Y)

         CALL SET_PIXEL(IMG, P%X + X, P%Y + Y , COLOUR)
         CALL SET_PIXEL(IMG, P%X + Y, P%Y + X , COLOUR)
         CALL SET_PIXEL(IMG, P%X - Y, P%Y + X , COLOUR)
         CALL SET_PIXEL(IMG, P%X - X, P%Y + Y , COLOUR)
         CALL SET_PIXEL(IMG, P%X - X, P%Y - Y , COLOUR)
         CALL SET_PIXEL(IMG, P%X - Y, P%Y - X , COLOUR)
         CALL SET_PIXEL(IMG, P%X + Y, P%Y - X , COLOUR)
         CALL SET_PIXEL(IMG, P%X + X, P%Y - Y , COLOUR)

         Y = Y + 1
         ERROR = ERROR + 1 + 2*Y
         IF(2*(ERROR-X) + 1 > 0)THEN
            X = X - 1
            ERROR = ERROR + (1 - 2*X)
         END IF
      END DO

      IF(PRESENT(FILL))THEN
         IF(FILL)THEN
            CALL FLOOD_FILL(IMG, P%X, P%Y, COLOUR, RGB(IMG%RED(X, Y), IMG%GREEN(X, Y), IMG%BLUE(X, Y)))
         END IF
      END IF

   END SUBROUTINE DRAW_CIRCLERGB


   SUBROUTINE DRAW_CIRCLERGBA(IMG, P, RADIUS, COLOUR, FILL, BLEND)

      IMPLICIT NONE

      TYPE(RGBAIMAGE),   INTENT(INOUT) :: IMG
      TYPE(RGBA),        INTENT(IN)    :: COLOUR
      TYPE(POINT),       INTENT(IN)    :: P
      INTEGER,           INTENT(IN)    :: RADIUS
      LOGICAL, OPTIONAL, INTENT(IN)    :: FILL, BLEND
      TYPE(RGBA)                       :: C1, C2
      INTEGER                          :: X, Y, ERROR,I,J

      X = RADIUS
      Y = 0
      ERROR = 0
      C1 = COLOUR

      IF(PRESENT(FILL).AND.PRESENT(BLEND))THEN
         IF(FILL.AND.BLEND)THEN

            DO I = -RADIUS, RADIUS, 1
               DO J = -RADIUS, RADIUS, 1
                  IF(I*I + J*J <= RADIUS*RADIUS)THEN
                     CALL GET_PIXELRGBA(IMG, P%X+I, P%Y+J, C2)
                     C1 = ALPHA_COMP(COLOUR, C2)
                     CALL SET_PIXEL(IMG, P%X+I, P%Y+J, C1)
                  END IF
               END DO
            END DO
         ELSEIF(FILL)THEN
            DO I = -RADIUS, RADIUS, 1
               DO J = -RADIUS, RADIUS, 1
                  IF(I*I + J*J <= RADIUS*RADIUS)THEN
                     CALL SET_PIXEL(IMG, P%X+I, P%Y+J, C1)
                  END IF
               END DO
            END DO
         ELSEIF(BLEND)THEN
            DO WHILE(X >= Y)

               CALL GET_PIXELRGBA(IMG, P%X+X, P%Y+Y, C2)
               C1 = ALPHA_COMP(COLOUR, C2)
               CALL SET_PIXEL(IMG, P%X + X, P%Y + Y , C1)

               CALL GET_PIXELRGBA(IMG, P%X+Y, P%Y+X, C2)
               C1 = ALPHA_COMP(COLOUR, C2)
               CALL SET_PIXEL(IMG, P%X + Y, P%Y + X , C1)

               CALL GET_PIXELRGBA(IMG, P%X-Y, P%Y+X, C2)
               C1 = ALPHA_COMP(COLOUR,C2)
               CALL SET_PIXEL(IMG, P%X - Y, P%Y + X , C1)

               CALL GET_PIXELRGBA(IMG, P%X-X, P%Y+Y, C2)
               C1 = ALPHA_COMP(COLOUR, C2)
               CALL SET_PIXEL(IMG, P%X - X, P%Y + Y , C1)

               CALL GET_PIXELRGBA(IMG, P%X-X, P%Y-Y, C2)
               C1 = ALPHA_COMP(COLOUR, C2)
               CALL SET_PIXEL(IMG, P%X - X, P%Y - Y , C1)

               CALL GET_PIXELRGBA(IMG, P%X-Y, P%Y-X, C2)
               C1 = ALPHA_COMP(COLOUR, C2)
               CALL SET_PIXEL(IMG, P%X - Y, P%Y - X , C1)

               CALL GET_PIXELRGBA(IMG, P%X+Y, P%Y-X, C2)
               C1 = ALPHA_COMP(COLOUR, C2)
               CALL SET_PIXEL(IMG, P%X + Y, P%Y - X , C1)

               CALL GET_PIXELRGBA(IMG, P%X+X, P%Y-Y, C2)
               C1 = ALPHA_COMP(COLOUR, C2)
               CALL SET_PIXEL(IMG, P%X + X, P%Y - Y , C1)

               Y = Y + 1
               ERROR = ERROR + 1 + 2*Y
               IF(2*(ERROR-X) + 1 > 0)THEN
                  X = X - 1
                  ERROR = ERROR + (1 - 2*X)
               END IF
            END DO
         ELSE
            DO WHILE(X >= Y)

               CALL SET_PIXEL(IMG, P%X + X, P%Y + Y , C1)
               CALL SET_PIXEL(IMG, P%X + Y, P%Y + X , C1)
               CALL SET_PIXEL(IMG, P%X - Y, P%Y + X , C1)
               CALL SET_PIXEL(IMG, P%X - X, P%Y + Y , C1)
               CALL SET_PIXEL(IMG, P%X - X, P%Y - Y , C1)
               CALL SET_PIXEL(IMG, P%X - Y, P%Y - X , C1)
               CALL SET_PIXEL(IMG, P%X + Y, P%Y - X , C1)
               CALL SET_PIXEL(IMG, P%X + X, P%Y - Y , C1)

               Y = Y + 1
               ERROR = ERROR + 1 + 2*Y
               IF(2*(ERROR-X) + 1 > 0)THEN
                  X = X - 1
                  ERROR = ERROR + (1 - 2*X)
               END IF
            END DO
         END IF
      ELSEIF(PRESENT(FILL))THEN
         IF(FILL)THEN
            DO I = -RADIUS, RADIUS, 1
               DO J = -RADIUS, RADIUS, 1
                  IF(I*I + J*J <= RADIUS*RADIUS)THEN
                     CALL GET_PIXELRGBA(IMG, P%X+I, P%Y+J, C2)
                     C1 = ALPHA_COMP(COLOUR, C2)
                     CALL SET_PIXEL(IMG, P%X+I, P%Y+J, C1)
                  END IF
               END DO
            END DO
         ELSE
            DO WHILE( X >= Y)

               CALL SET_PIXEL(IMG, P%X + X, P%Y + Y , COLOUR)
               CALL SET_PIXEL(IMG, P%X + Y, P%Y + X , COLOUR)
               CALL SET_PIXEL(IMG, P%X - Y, P%Y + X , COLOUR)
               CALL SET_PIXEL(IMG, P%X - X, P%Y + Y , COLOUR)
               CALL SET_PIXEL(IMG, P%X - X, P%Y - Y , COLOUR)
               CALL SET_PIXEL(IMG, P%X - Y, P%Y - X , COLOUR)
               CALL SET_PIXEL(IMG, P%X + Y, P%Y - X , COLOUR)
               CALL SET_PIXEL(IMG, P%X + X, P%Y - Y , COLOUR)

               Y = Y + 1
               ERROR = ERROR + 1 + 2*Y
               IF(2*(ERROR-X) + 1 > 0)THEN
                  X = X - 1
                  ERROR = ERROR + (1 - 2*X)
               END IF
            END DO
         END IF
      ELSE
         DO WHILE( X >= Y)

               CALL SET_PIXEL(IMG, P%X + X, P%Y + Y , COLOUR)
               CALL SET_PIXEL(IMG, P%X + Y, P%Y + X , COLOUR)
               CALL SET_PIXEL(IMG, P%X - Y, P%Y + X , COLOUR)
               CALL SET_PIXEL(IMG, P%X - X, P%Y + Y , COLOUR)
               CALL SET_PIXEL(IMG, P%X - X, P%Y - Y , COLOUR)
               CALL SET_PIXEL(IMG, P%X - Y, P%Y - X , COLOUR)
               CALL SET_PIXEL(IMG, P%X + Y, P%Y - X , COLOUR)
               CALL SET_PIXEL(IMG, P%X + X, P%Y - Y , COLOUR)

               Y = Y + 1
               ERROR = ERROR + 1 + 2*Y
               IF(2*(ERROR-X) + 1 > 0)THEN
                  X = X - 1
                  ERROR = ERROR + (1 - 2*X)
               END IF
         END DO
      END IF

   END SUBROUTINE DRAW_CIRCLERGBA

   SUBROUTINE DRAW_POLYGON(IMG, COLOUR, P1, P2, P3, FILL)

      IMPLICIT NONE

      TYPE(RGBAIMAGE),    INTENT(INOUT) :: IMG
      TYPE(RGBA),         INTENT(IN)    :: COLOUR
      TYPE(POINT),       INTENT(INOUT) :: P1, P2, P3
      LOGICAL, OPTIONAL, INTENT(IN)    :: FILL
      INTEGER                          :: X, Y
      TYPE(BUCKET)                     :: BUCK(3)

      CALL DRAW_LINE(IMG, P1, P2, COLOUR)
      CALL DRAW_LINE(IMG, P2, P3, COLOUR)
      CALL DRAW_LINE(IMG, P3, P1, COLOUR)

      DO X = 1 , 3
         BUCK(X) = BUCKET(MAX(P1%Y,P2%Y), MIN(P1%Y,P2%Y), X, 1, ABS(P2%X-P1%X), ABS(P2%Y-P1%Y), 0)
      END DO

      IF(PRESENT(FILL))THEN
         IF(FILL)THEN
            X = (P1%X + P2%X + P3%X)/3
            Y = (P1%Y + P2%Y + P3%Y)/3
            PRINT*,X,Y
!            call set_pixel(img, x,y,RGB(255,255,255))
            CALL FLOOD_FILL(IMG, X, Y, COLOUR, RGBA(IMG%RED(X, Y), IMG%GREEN(X, Y), IMG%BLUE(X, Y),IMG%ALPHA(X,Y)))
         END IF
      END IF

   END SUBROUTINE DRAW_POLYGON

   SUBROUTINE SWAP(A, B)

      IMPLICIT NONE

      INTEGER, INTENT(INOUT) :: A, B
      INTEGER                :: TMP

      TMP = A
      A = B
      B = TMP

   END SUBROUTINE SWAP


   ! subroutine fill_poly(img, colour)

   !    implicit none

   !    type(RGBAimage), intent(INOUT) :: img
   !    type(RGBA),      intent(IN)    :: colour



   ! end subroutine fill_poly


   RECURSIVE SUBROUTINE FLOOD_FILLRGB(IMG, X, Y, COLOUR, OLD)

      IMPLICIT NONE

      TYPE(RGBIMAGE), INTENT(INOUT) :: IMG
      TYPE(RGB),      INTENT(IN)    :: COLOUR, OLD
      INTEGER,        INTENT(IN)    :: X, Y
      INTEGER                       :: X1

      IF(OLD == COLOUR)RETURN
      IF( (RGB(IMG%RED(X, Y), IMG%GREEN(X, Y), IMG%BLUE(X, Y)) /= OLD))RETURN

      X1 = X
      DO WHILE(X1 < IMG%WIDTH .AND. RGB(IMG%RED(X1, Y), IMG%GREEN(X1, Y), &
                                             IMG%BLUE(X1, Y)) == OLD)
         CALL SET_PIXEL(IMG, X1, Y, COLOUR)
         X1 = X1 + 1
      END DO

      X1 = X - 1
      IF(X1/=0)THEN
         DO WHILE(X1 >= 1 .AND. RGB(IMG%RED(X1, Y), IMG%GREEN(X1, Y), &
                                                IMG%BLUE(X1, Y)) == OLD)
            CALL SET_PIXEL(IMG, X1, Y, COLOUR)

            X1 = X1 - 1
            IF(X1==0)EXIT
         END DO
      END IF

!      move up
      X1 = X
      DO WHILE(X1 < IMG%WIDTH .AND. RGB(IMG%RED(X1, Y), IMG%GREEN(X1, Y), &
                                             IMG%BLUE(X1, Y)) == COLOUR)
         IF(Y==1)EXIT
         IF(Y > 1 .AND. RGB(IMG%RED(X1, Y-1), IMG%GREEN(X1, Y-1), &
                                             IMG%BLUE(X1, Y-1))==OLD)THEN
            CALL FLOOD_FILL(IMG, X, Y-1, COLOUR, OLD)
         END IF
         X1 = X1 +1

      END DO

      X1 = X - 1
      IF(X1/=0)THEN
         DO WHILE(X1 >= 1 .AND. RGB(IMG%RED(X1, Y), IMG%GREEN(X1, Y), &
                                                IMG%BLUE(X1, Y)) == COLOUR)
            IF(X1==0 .OR. Y==1)EXIT
            IF(Y > 1 .AND. RGB(IMG%RED(X1, Y-1), IMG%GREEN(X1, Y-1), &
                                                IMG%BLUE(X1, Y-1))==OLD)THEN
               CALL FLOOD_FILL(IMG, X, Y-1, COLOUR, OLD)
            END IF
            X1 = X1 - 1
            IF(X1==0 .OR. Y==0)EXIT
         END DO
      END IF
      !move down

      X1 = X
      DO WHILE(X1 < IMG%WIDTH .AND. RGB(IMG%RED(X1, Y), IMG%GREEN(X1, Y), &
                                             IMG%BLUE(X1, Y)) == COLOUR)
         IF(Y==IMG%HEIGHT)EXIT
         IF(Y < IMG%HEIGHT-1 .AND. RGB(IMG%RED(X1, Y+1), IMG%GREEN(X1, Y+1), &
                                             IMG%BLUE(X1, Y+1))==OLD)THEN
            CALL FLOOD_FILL(IMG, X, Y+1, COLOUR, OLD)
         END IF
         X1 = X1 +1
      END DO

      X1 = X-1
      IF(X1/=0)THEN
         DO WHILE(X1 >= 1 .AND. RGB(IMG%RED(X1, Y), IMG%GREEN(X1, Y), &
                                                IMG%BLUE(X1, Y)) == COLOUR)
            IF(Y==IMG%HEIGHT)EXIT
            IF(Y < IMG%HEIGHT .AND. RGB(IMG%RED(X1, Y+1), IMG%GREEN(X1, Y+1), &
                                                IMG%BLUE(X1, Y+1))==OLD)THEN
               CALL FLOOD_FILL(IMG, X, Y+1, COLOUR, OLD)
            END IF
            X1 = X1 - 1
            IF(X1==0.OR.Y==0)EXIT
         END DO
      END IF
   END SUBROUTINE FLOOD_FILLRGB


   RECURSIVE SUBROUTINE FLOOD_FILLRGBA(IMG, X, Y, COLOUR, OLD)

      IMPLICIT NONE

      TYPE(RGBAIMAGE), INTENT(INOUT) :: IMG
      TYPE(RGBA),      INTENT(IN)    :: COLOUR, OLD
      INTEGER,         INTENT(IN)    :: X, Y
      TYPE(RGBA)                     :: C
      INTEGER                        :: X1

      IF(OLD == COLOUR)RETURN
      CALL GET_PIXELRGBA(IMG, X, Y, C)
      IF(C /= OLD)RETURN
      X1 = X
      DO WHILE(X1 < IMG%WIDTH .AND. RGBA(IMG%RED(X1, Y), IMG%GREEN(X1, Y), &
                                         IMG%BLUE(X1, Y), IMG%ALPHA(X1, Y)) == OLD)
         CALL SET_PIXEL(IMG, X1, Y, COLOUR)
         X1 = X1 + 1
      END DO

      X1 = X - 1
      IF(X1/=0)THEN
         DO WHILE(X1 >= 1 .AND. RGBA(IMG%RED(X1, Y), IMG%GREEN(X1, Y), &
                                     IMG%BLUE(X1, Y), IMG%ALPHA(X1, Y)) == OLD)
            CALL SET_PIXEL(IMG, X1, Y, COLOUR)

            X1 = X1 - 1
            IF(X1==0)EXIT
         END DO
      END IF

!      move up
      X1 = X
      DO WHILE(X1 < IMG%WIDTH .AND. RGBA(IMG%RED(X1, Y), IMG%GREEN(X1, Y), &
                                         IMG%BLUE(X1, Y), IMG%ALPHA(X1, Y)) == COLOUR)
         IF(Y==1)EXIT
         IF(Y > 1 .AND. RGBA(IMG%RED(X1, Y-1), IMG%GREEN(X1, Y-1), &
                             IMG%BLUE(X1, Y-1), IMG%ALPHA(X1, Y-1)) == OLD)THEN
            CALL FLOOD_FILL(IMG, X, Y-1, COLOUR, OLD)
         END IF
         X1 = X1 +1

      END DO

      X1 = X - 1
      IF(X1/=0)THEN
         DO WHILE(X1 >= 1 .AND. RGBA(IMG%RED(X1, Y), IMG%GREEN(X1, Y), &
                                     IMG%BLUE(X1, Y), IMG%ALPHA(X1, Y)) == COLOUR)
            IF(X1==0 .OR. Y==1)EXIT
            IF(Y > 1 .AND. RGBA(IMG%RED(X1, Y-1), IMG%GREEN(X1, Y-1), &
                                IMG%BLUE(X1, Y-1), IMG%ALPHA(X1, Y-1)) == OLD)THEN
               CALL FLOOD_FILL(IMG, X, Y-1, COLOUR, OLD)
            END IF
            X1 = X1 - 1
            IF(X1==0 .OR. Y==0)EXIT
         END DO
      END IF
      !move down

      X1 = X
      DO WHILE(X1 < IMG%WIDTH .AND. RGBA(IMG%RED(X1, Y), IMG%GREEN(X1, Y), &
                                         IMG%BLUE(X1, Y), IMG%ALPHA(X1, Y)) == COLOUR)
         IF(Y==IMG%HEIGHT)EXIT
         IF(Y < IMG%HEIGHT-1 .AND. RGBA(IMG%RED(X1, Y+1), IMG%GREEN(X1, Y+1), &
                                        IMG%BLUE(X1, Y+1), IMG%ALPHA(X1, Y+1)) == OLD)THEN
            CALL FLOOD_FILL(IMG, X, Y+1, COLOUR, OLD)
         END IF
         X1 = X1 +1
      END DO

      X1 = X-1
      IF(X1/=0)THEN
         DO WHILE(X1 >= 1 .AND. RGBA(IMG%RED(X1, Y), IMG%GREEN(X1, Y), &
                                     IMG%BLUE(X1, Y), IMG%ALPHA(X1, Y)) == COLOUR)
            IF(Y==IMG%HEIGHT)EXIT
            IF(Y < IMG%HEIGHT .AND. RGBA(IMG%RED(X1, Y+1), IMG%GREEN(X1, Y+1), &
                                         IMG%BLUE(X1, Y+1), IMG%ALPHA(X1, Y+1)) == OLD)THEN
               CALL FLOOD_FILL(IMG, X, Y+1, COLOUR, OLD)
            END IF
            X1 = X1 - 1
            IF(X1==0.OR.Y==0)EXIT
         END DO
      END IF

   END SUBROUTINE FLOOD_FILLRGBA


   FUNCTION ALPHA_COMP(CA, CB) RESULT(CO)

        IMPLICIT NONE

        TYPE(RGBA), INTENT(IN) :: CB, CA
        TYPE(RGBA)             :: CO
        REAL                   :: A_TMP, B_TMP

        A_TMP = CA%ALPHA/255.
        B_TMP = CB%ALPHA/255.

         CO%RED =  CLAMPINT(INT(CA%RED * A_TMP) + INT(CB%RED * (1. - A_TMP)), 0, 255)
         CO%GREEN =  CLAMPINT(INT(CA%GREEN * A_TMP) + INT(CB%GREEN * (1. - A_TMP)), 0, 255)
         CO%BLUE =  CLAMPINT(INT(CA%BLUE * A_TMP) + INT(CB%BLUE * (1. - A_TMP)), 0, 255)
         CO%ALPHA = CLAMPINT(INT((B_TMP + (1. - B_TMP) * A_TMP)*255.), 0, 255)

   END FUNCTION ALPHA_COMP

END MODULE ModLib_FglDraw
