!< FACE, Fortran Ansi Colors Environment.
MODULE ModLib_Face
!< FACE, Fortran Ansi Colors Environment.
USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT32

IMPLICIT NONE
PRIVATE
PUBLIC :: COLORIZE
PUBLIC :: COLORS_SAMPLES
PUBLIC :: STYLES_SAMPLES
PUBLIC :: ASCII
PUBLIC :: UCS4

INTERFACE COLORIZE
#if defined ASCII_SUPPORTED && defined ASCII_NEQ_DEFAULT
   MODULE PROCEDURE COLORIZE_ASCII
   MODULE PROCEDURE COLORIZE_DEFAULT
#else
   MODULE PROCEDURE COLORIZE_DEFAULT
#endif
#ifdef UCS4_SUPPORTED
   MODULE PROCEDURE COLORIZE_UCS4
#endif
ENDINTERFACE

! kind parameters
#ifdef ASCII_SUPPORTED
INTEGER, PARAMETER :: ASCII = SELECTED_CHAR_KIND('ascii')     !< ASCII character set kind.
#else
INTEGER, PARAMETER :: ASCII = SELECTED_CHAR_KIND('default')   !< ASCII character set kind.
#endif
#ifdef UCS4_SUPPORTED
INTEGER, PARAMETER :: UCS4  = SELECTED_CHAR_KIND('iso_10646') !< Unicode character set kind.
#else
INTEGER, PARAMETER :: UCS4  = SELECTED_CHAR_KIND('default')   !< Unicode character set kind.
#endif
! parameters
CHARACTER(26), PARAMETER :: UPPER_ALPHABET='ABCDEFGHIJKLMNOPQRSTUVWXYZ' !< Upper case alphabet.
CHARACTER(26), PARAMETER :: LOWER_ALPHABET='abcdefghijklmnopqrstuvwxyz' !< Lower case alphabet.
CHARACTER(1),  PARAMETER :: NL=NEW_LINE('a')                            !< New line character.
CHARACTER(1),  PARAMETER :: ESCAPE=ACHAR(27)                            !< "\" character.
! codes
CHARACTER(2), PARAMETER :: CODE_START=ESCAPE//'['               !< Start ansi code, "\[".
CHARACTER(1), PARAMETER :: CODE_END='m'                         !< End ansi code, "m".
CHARACTER(4), PARAMETER :: CODE_CLEAR=CODE_START//'0'//CODE_END !< Clear all styles, "\[0m".
! styles codes
CHARACTER(17), PARAMETER :: STYLES(1:2,1:16)=RESHAPE([&
             'BOLD_ON          ','1                ', &         !  Bold on.
             'ITALICS_ON       ','3                ', &         !  Italics on.
             'UNDERLINE_ON     ','4                ', &         !  Underline on.
             'INVERSE_ON       ','7                ', &         !  Inverse on: reverse foreground and backgrouND COLORS.
             'STRIKETHROUGH_ON ','9                ', &         !  Strikethrough on.
             'BOLD_OFF         ','22               ', &         !  Bold off.
             'ITALICS_OFF      ','23               ', &         !  Italics off.
             'UNDERLINE_OFF    ','24               ', &         !  Underline off.
             'INVERSE_OFF      ','27               ', &         !  Inverse off: reverse foreground and backgroUND COLORS.
             'STRIKETHROUGH_OFF','29               ', &         !  Strikethrough off.
             'FRAMED_ON        ','51               ', &         !  Framed on.
             'ENCIRCLED_ON     ','52               ', &         !  Encircled on.
             'OVERLINED_ON     ','53               ', &         !  Overlined on.
             'FRAMED_OFF       ','54               ', &         !  Framed off.
             'ENCIRCLED_OFF    ','54               ', &         !  Encircled off.
             'OVERLINED_OFF    ','55               '  &         !  Overlined off.
                                                     ], [2,16]) !< Styles.
! colors codes
CHARACTER(15), PARAMETER :: COLORS_FG(1:2,1:17)=RESHAPE([&
                    'BLACK          ','30             ', &         !  Black.
                    'RED            ','31             ', &         !  Red.
                    'GREEN          ','32             ', &         !  Green.
                    'YELLOW         ','33             ', &         !  Yellow.
                    'BLUE           ','34             ', &         !  Blue.
                    'MAGENTA        ','35             ', &         !  Magenta.
                    'CYAN           ','36             ', &         !  Cyan.
                    'WHITE          ','37             ', &         !  White.
                    'DEFAULT        ','39             ', &         !  Default (white).
                    'BLACK_INTENSE  ','90             ', &         !  Black intense.
                    'RED_INTENSE    ','91             ', &         !  Red intense.
                    'GREEN_INTENSE  ','92             ', &         !  Green intense.
                    'YELLOW_INTENSE ','93             ', &         !  Yellow intense.
                    'BLUE_INTENSE   ','94             ', &         !  Blue intense.
                    'MAGENTA_INTENSE','95             ', &         !  Magenta intense.
                    'CYAN_INTENSE   ','96             ', &         !  Cyan intense.
                    'WHITE_INTENSE  ','97             '  &         !  White intense.
                                                        ], [2,17]) !< Foreground colors.
CHARACTER(15), PARAMETER :: COLORS_BG(1:2,1:17)=RESHAPE([&
                    'BLACK          ','40             ', &         !  Black.
                    'RED            ','41             ', &         !  Red.
                    'GREEN          ','42             ', &         !  Green.
                    'YELLOW         ','43             ', &         !  Yellow.
                    'BLUE           ','44             ', &         !  Blue.
                    'MAGENTA        ','45             ', &         !  Magenta.
                    'CYAN           ','46             ', &         !  Cyan.
                    'WHITE          ','47             ', &         !  White.
                    'DEFAULT        ','49             ', &         !  Default (black).
                    'BLACK_INTENSE  ','100            ', &         !  Black intense.
                    'RED_INTENSE    ','101            ', &         !  Red intense.
                    'GREEN_INTENSE  ','102            ', &         !  Green intense.
                    'YELLOW_INTENSE ','103            ', &         !  Yellow intense.
                    'BLUE_INTENSE   ','104            ', &         !  Blue intense.
                    'MAGENTA_INTENSE','105            ', &         !  Magenta intense.
                    'CYAN_INTENSE   ','106            ', &         !  Cyan intense.
                    'WHITE_INTENSE  ','107            '  &         !  White intense.
                                                        ], [2,17]) !< Background colors.
CONTAINS
   ! public procedures
   SUBROUTINE COLORS_SAMPLES()
   !< Print to standard output all colors samples.
   INTEGER(INT32) :: C !< Counter.

   PRINT '(A)', COLORIZE('Foreground colors samples', COLOR_FG='red_intense')
   DO C=1, SIZE(COLORS_FG, DIM=2)
      PRINT '(A)', '  colorize("'//COLORS_FG(1, C)//'", color_fg="'//COLORS_FG(1, C)//'") => '//&
         COLORIZE(COLORS_FG(1, C), COLOR_FG=COLORS_FG(1, C))//&
         ' code: '//COLORIZE(TRIM(COLORS_FG(2, C)), COLOR_FG=COLORS_FG(1, C), STYLE='inverse_on')
   ENDDO
   PRINT '(A)', COLORIZE('Background colors samples', COLOR_FG='red_intense')
   DO C=1, SIZE(COLORS_BG, DIM=2)
      PRINT '(A)', '  colorize("'//COLORS_BG(1, C)//'", color_bg="'//COLORS_BG(1, C)//'") => '//&
         COLORIZE(COLORS_BG(1, C), COLOR_BG=COLORS_BG(1, C))//&
         ' code: '//COLORIZE(TRIM(COLORS_BG(2, C)), COLOR_BG=COLORS_BG(1, C), STYLE='inverse_on')
   ENDDO
   ENDSUBROUTINE COLORS_SAMPLES

   SUBROUTINE STYLES_SAMPLES()
   !< Print to standard output all styles samples.
   INTEGER(INT32) :: S !< Counter.

   PRINT '(A)', COLORIZE('Styles samples', COLOR_FG='red_intense')
   DO S=1, SIZE(STYLES, DIM=2)
      PRINT '(A)', '  colorize("'//STYLES(1, S)//'", style="'//STYLES(1, S)//'") => '//&
         COLORIZE(STYLES(1, S), STYLE=STYLES(1, S))//&
         ' code: '//COLORIZE(TRIM(STYLES(2, S)), COLOR_FG='magenta', STYLE='inverse_on')
   ENDDO
   ENDSUBROUTINE STYLES_SAMPLES

   ! private procedures
   PURE FUNCTION COLORIZE_ASCII(STRING, COLOR_FG, COLOR_BG, STYLE) RESULT(COLORIZED)
   !< Colorize and stylize strings, ASCII kind.
   CHARACTER(LEN=*, KIND=ASCII), INTENT(IN)           :: STRING    !< Input string.
   CHARACTER(LEN=*),             INTENT(IN), OPTIONAL :: COLOR_FG  !< Foreground color definition.
   CHARACTER(LEN=*),             INTENT(IN), OPTIONAL :: COLOR_BG  !< Background color definition.
   CHARACTER(LEN=*),             INTENT(IN), OPTIONAL :: STYLE     !< Style definition.
   CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE          :: COLORIZED !< Colorized string.
   CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE          :: BUFFER    !< Temporary buffer.
   INTEGER(INT32)                                     :: I         !< Counter.

   COLORIZED = STRING
   IF (PRESENT(COLOR_FG)) THEN
      I = COLOR_INDEX(UPPER(COLOR_FG))
      IF (I>0) THEN
         BUFFER = CODE_START//TRIM(COLORS_FG(2, I))//CODE_END
         COLORIZED = BUFFER//COLORIZED
         BUFFER = CODE_CLEAR
         COLORIZED = COLORIZED//BUFFER
      ENDIF
   ENDIF
   IF (PRESENT(COLOR_BG)) THEN
      I = COLOR_INDEX(UPPER(COLOR_BG))
      IF (I>0) THEN
         BUFFER = CODE_START//TRIM(COLORS_BG(2, I))//CODE_END
         COLORIZED = BUFFER//COLORIZED
         BUFFER = CODE_CLEAR
         COLORIZED = COLORIZED//BUFFER
      ENDIF
   ENDIF
   IF (PRESENT(STYLE)) THEN
      I = STYLE_INDEX(UPPER(STYLE))
      IF (I>0) THEN
         BUFFER = CODE_START//TRIM(STYLES(2, I))//CODE_END
         COLORIZED = BUFFER//COLORIZED
         BUFFER = CODE_CLEAR
         COLORIZED = COLORIZED//BUFFER
      ENDIF
   ENDIF
   ENDFUNCTION COLORIZE_ASCII

   PURE FUNCTION COLORIZE_DEFAULT(STRING, COLOR_FG, COLOR_BG, STYLE) RESULT(COLORIZED)
   !< Colorize and stylize strings, DEFAULT kind.
   CHARACTER(LEN=*), INTENT(IN)           :: STRING    !< Input string.
   CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: COLOR_FG  !< Foreground color definition.
   CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: COLOR_BG  !< Background color definition.
   CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: STYLE     !< Style definition.
   CHARACTER(LEN=:), ALLOCATABLE          :: COLORIZED !< Colorized string.
   INTEGER(INT32)                         :: I         !< Counter.

   COLORIZED = STRING
   IF (PRESENT(COLOR_FG)) THEN
      I = COLOR_INDEX(UPPER(COLOR_FG))
      IF (I>0) COLORIZED = CODE_START//TRIM(COLORS_FG(2, I))//CODE_END//COLORIZED//CODE_CLEAR
   ENDIF
   IF (PRESENT(COLOR_BG)) THEN
      I = COLOR_INDEX(UPPER(COLOR_BG))
      IF (I>0) COLORIZED = CODE_START//TRIM(COLORS_BG(2, I))//CODE_END//COLORIZED//CODE_CLEAR
   ENDIF
   IF (PRESENT(STYLE)) THEN
      I = STYLE_INDEX(UPPER(STYLE))
      IF (I>0) COLORIZED = CODE_START//TRIM(STYLES(2, I))//CODE_END//COLORIZED//CODE_CLEAR
   ENDIF
   ENDFUNCTION COLORIZE_DEFAULT

   PURE FUNCTION COLORIZE_UCS4(STRING, COLOR_FG, COLOR_BG, STYLE) RESULT(COLORIZED)
   !< Colorize and stylize strings, UCS4 kind.
   CHARACTER(LEN=*, KIND=UCS4), INTENT(IN)           :: STRING    !< Input string.
   CHARACTER(LEN=*),            INTENT(IN), OPTIONAL :: COLOR_FG  !< Foreground color definition.
   CHARACTER(LEN=*),            INTENT(IN), OPTIONAL :: COLOR_BG  !< Background color definition.
   CHARACTER(LEN=*),            INTENT(IN), OPTIONAL :: STYLE     !< Style definition.
   CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE          :: COLORIZED !< Colorized string.
   CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE          :: BUFFER    !< Temporary buffer.
   INTEGER(INT32)                                    :: I         !< Counter.

   COLORIZED = STRING
   IF (PRESENT(COLOR_FG)) THEN
      I = COLOR_INDEX(UPPER(COLOR_FG))
      IF (I>0) THEN
         BUFFER = CODE_START//TRIM(COLORS_FG(2, I))//CODE_END
         COLORIZED = BUFFER//COLORIZED
         BUFFER = CODE_CLEAR
         COLORIZED = COLORIZED//BUFFER
      ENDIF
   ENDIF
   IF (PRESENT(COLOR_BG)) THEN
      I = COLOR_INDEX(UPPER(COLOR_BG))
      IF (I>0) THEN
         BUFFER = CODE_START//TRIM(COLORS_BG(2, I))//CODE_END
         COLORIZED = BUFFER//COLORIZED
         BUFFER = CODE_CLEAR
         COLORIZED = COLORIZED//BUFFER
      ENDIF
   ENDIF
   IF (PRESENT(STYLE)) THEN
      I = STYLE_INDEX(UPPER(STYLE))
      IF (I>0) THEN
         BUFFER = CODE_START//TRIM(STYLES(2, I))//CODE_END
         COLORIZED = BUFFER//COLORIZED
         BUFFER = CODE_CLEAR
         COLORIZED = COLORIZED//BUFFER
      ENDIF
   ENDIF
   ENDFUNCTION COLORIZE_UCS4

   ELEMENTAL FUNCTION COLOR_INDEX(COLOR)
   !< Return the array-index corresponding to the queried color.
   !<
   !< @note Because Foreground and backround colors lists share the same name, no matter what array is used to FIND THE COLOR INDEX.
   !< Thus, the foreground array is used.
   CHARACTER(LEN=*), INTENT(IN) :: COLOR       !< Color definition.
   INTEGER(INT32)               :: COLOR_INDEX !< Index into the colors arrays.
   INTEGER(INT32)               :: C           !< Counter.

   COLOR_INDEX = 0
   DO C=1, SIZE(COLORS_FG, DIM=2)
      IF (TRIM(COLORS_FG(1, C))==TRIM(ADJUSTL(COLOR))) THEN
         COLOR_INDEX = C
         EXIT
      ENDIF
   ENDDO
   ENDFUNCTION COLOR_INDEX

   ELEMENTAL FUNCTION STYLE_INDEX(STYLE)
   !< Return the array-index corresponding to the queried style.
   CHARACTER(LEN=*), INTENT(IN) :: STYLE       !< Style definition.
   INTEGER(INT32)               :: STYLE_INDEX !< Index into the styles array.
   INTEGER(INT32)               :: S           !< Counter.

   STYLE_INDEX = 0
   DO S=1, SIZE(STYLES, DIM=2)
      IF (TRIM(STYLES(1, S))==TRIM(ADJUSTL(STYLE))) THEN
         STYLE_INDEX = S
         EXIT
      ENDIF
   ENDDO
   ENDFUNCTION STYLE_INDEX

   ELEMENTAL FUNCTION UPPER(STRING)
   !< Return a string with all uppercase characters.
   CHARACTER(LEN=*), INTENT(IN) :: STRING !< Input string.
   CHARACTER(LEN=LEN(STRING))   :: UPPER  !< Upper case string.
   INTEGER                      :: N1     !< Characters counter.
   INTEGER                      :: N2     !< Characters counter.

   UPPER = STRING
   DO N1=1, LEN(STRING)
      N2 = INDEX(LOWER_ALPHABET, STRING(N1:N1))
      IF (N2>0) UPPER(N1:N1) = UPPER_ALPHABET(N2:N2)
   ENDDO
   ENDFUNCTION UPPER
ENDMODULE ModLib_Face
