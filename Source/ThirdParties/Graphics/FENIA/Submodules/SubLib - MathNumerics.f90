
SUBMODULE (ModLib_FENIA) SubLib_MathNumerics

!** PURPOSE OF THIS SUBMODULE:
    ! This submodule contains routines that perform ...

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS
    ! na

!** GENERIC DECLARATIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):
!
MODULE FUNCTION BezierQuadraticControl(A,B,C)

! BezierQuadraticControl takes the x and y coordinates of two given points
! (a and b) and calculates the x and y coordinate of a control point with
! which a quadratic Bezier curve can be plotted that goes through the third
! given point (c).

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! a: real, array (1D) with 2 elements. The x and y coordinates (elements 1 and
!    2 respectively) of the point that is on the left or on the right of the
!    point represented by argument "c".
! b: real, array (1D) with 2 elements. The x and y coordinates (elements 1 and
!    2 respectively) of the point that is on the right or on the left of the
!    point represented by argument "c".
! c: real, array (1D) with 2 elements. The x and y coordinates (elements 1 and
!    2 respectively) of the point that is in between the points represented
!    by arguments "a" and "b".

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! BezierQuadraticControl: real array with 2 elements (1D). The x and y
!    coordinates (elements 1 and 2 respectively) of the control point that
!    together with points represented by arguments "a" and "b" draws a Bezier
!    curve that goes through the point represented by argument "c".

! OUTPUT (OPTIONAL):
! No optional output arguments. All arguments are required.

! Example of usage:

!interface
!function BezierQuadraticControl(a,b,c)
!use Config, only: srk
!implicit none
!real(srk),intent(in):: a(2),b(2),c(2)
!real(srk):: BezierQuadraticControl(2)
!end function BezierQuadraticControl
!end interface

!write(*,*)BezierQuadraticControl((/1.0,1.0/),(/2.0,2.0/),(/1.5,3.0/))

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk

IMPLICIT NONE

! Argument variables:
REAL(SRK),INTENT(IN):: A(2),B(2),C(2)
REAL(SRK):: BezierQuadraticControl(2)

! Private variables:
REAL(SRK),PARAMETER:: STEP=0.001_SRK
REAL(SRK):: REAL_I,T,LOCUS_POINTS(NINT(1.0_SRK/STEP)-1,2)
REAL(SRK):: DISTANCE(NINT(1.0_SRK/STEP)-1)
INTEGER:: I,CLOSEST_POINT(1)

! ------------------------------------------------------------------------------

! Since a point of the curve is known, solve the parametric formula for the
! quadratic Bezier curve in order to obtain all the possible control points.
DO I=1,SIZE(LOCUS_POINTS,DIM=1)
    REAL_I=I
    T=REAL_I*STEP
    LOCUS_POINTS(I,:)=(C-(T**2)*A-((1-T)**2)*B)/(2.0*T*(1-T))
END DO

! Calculate the distance of all possible control points from the given curve
! point.
DISTANCE(:)=SQRT((C(1)-LOCUS_POINTS(:,1))**2+(C(2)-LOCUS_POINTS(:,2))**2)

! The control point in question is the one that is closest to the given curve
! point.
CLOSEST_POINT=MINLOC(DISTANCE)
BezierQuadraticControl=LOCUS_POINTS(CLOSEST_POINT(1),:)

END FUNCTION BezierQuadraticControl

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE CrossPoint(X1,X2,Y1,Y2,XM,YM)

! CrossPoint gets two pairs of points that define two straight lines and
! calculates the coordinates of the point at the cross section of the two lines.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! x1: real, array (1D) with 2 elements. The x-coordinates of the first data pair
!    that defines a straight line.
! x2: real, array (1D) with 2 elements. The x-coordinates of the second data
!    pair that defines a straight line.
! y1: real, array (1D) with 2 elements. The y-coordinates of the first data pair
!    that defines a straight line.
! y2: real, array (1D) with 2 elements. The y-coordinates of the second data
!    pair that defines a straight line.

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! xm: real, scalar. The x-coordinate of the point at the cross section of the
!    two given lines.
! ym: real, scalar. The y-coordinate of the point at the cross section of the
!    two given lines.

! OUTPUT (OPTIONAL):
! No optional output arguments. All arguments are required.

! Example of usage:

!interface
!subroutine CrossPoint(x1,x2,y1,y2,xm,ym)
!use Config, only: srk
!implicit none
!real(srk),intent(in):: x1(2),x2(2),y1(2),y2(2)
!real(srk),intent(out):: xm,ym
!end subroutine CrossPoint
!end interface

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,warnings_pause

IMPLICIT NONE

! Argument variables:
REAL(SRK),INTENT(IN):: X1(2),X2(2),Y1(2),Y2(2)
REAL(SRK),INTENT(OUT):: XM,YM

! Private variables:
REAL(SRK):: A1,A2,B1,B2

! ------------------------------------------------------------------------------

IF ( & ! One of the two lines is actually a point:
    ((X1(1)==X1(2)).AND.(Y1(1)==Y1(2))).OR. &
    ((X2(1)==X2(2)).AND.(Y2(1)==Y2(2))) &
    ) THEN
WRITE(*,*)"CrossPoint"
WRITE(*,*)"ERROR: given point set does not define a single line."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ( & ! The two lines are on top of each other:
    (((X1(1)==X2(1)).AND.(Y1(1)==Y2(1))).AND. &
    ((X1(2)==X2(2)).AND.(Y1(2)==Y2(2)))) &
    .OR. &
    (((X1(2)==X2(1)).AND.(Y1(2)==Y2(1))).AND. &
    ((X1(1)==X2(2)).AND.(Y1(1)==Y2(2)))) &
    ) THEN
WRITE(*,*)"CrossPoint"
WRITE(*,*)"WARNING: given lines coincide."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
XM=(X1(1)+X1(2))/2.0
YM=(Y1(1)+Y1(2))/2.0
RETURN
END IF

IF ( & ! Both the lines are parallel to the vertical or the horizontal axis:
    ((X1(1)==X1(2)).AND.(X2(1)==X2(2))).OR. &
    ((Y1(1)==Y1(2)).AND.(Y2(1)==Y2(2))) &
    ) THEN
WRITE(*,*)"CrossPoint"
WRITE(*,*)"ERROR: given point sets define two parallel lines."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! ------------------------------------------------------------------------------

! Line "1" is vertical:
IF (X1(2)==X1(1)) THEN
    A2=(Y2(2)-Y2(1))/(X2(2)-X2(1))
    XM=X1(1)
    YM=A2*XM+B2
    RETURN
END IF

! Line "2" is vertical:
IF (X2(2)==X2(1)) THEN
    A1=(Y1(2)-Y1(1))/(X1(2)-X1(1))
    XM=X2(1)
    YM=A1*XM+B1
    RETURN
END IF

A1=(Y1(2)-Y1(1))/(X1(2)-X1(1))
A2=(Y2(2)-Y2(1))/(X2(2)-X2(1))

! The two lines are parallel:
IF (A1==A2) THEN
WRITE(*,*)"CrossPoint"
WRITE(*,*)"ERROR: equal slopes define parallel lines."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

B1=Y1(1)-A1*X1(1)
B2=Y2(1)-A2*X2(1)
XM=(B2-B1)/(A1-A2)
YM=A1*XM+B1

END SUBROUTINE CrossPoint

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE FUNCTION FACT(X)

! fact(x) calculates the factorial of x.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! x: real, scalar. The number whose factorial will be calculated

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! fact: real, scalar. The calculated factorial.

! OUTPUT (OPTIONAL):
! No optional output arguments. All arguments are required.

! Example of usage:

!interface
!function fact(x)
!use Config, only: srk
!implicit none
!real(srk),intent(in):: x
!real(srk):: fact
!end function fact
!end interface

!integer:: i
!real(srk):: j
!do i=1,100
!j=i-1
!write(*,*)j,fact(j)
!end do
!do i=1,200
!j=i-0.5
!write(*,*)j,fact(j)
!end do

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,pi

IMPLICIT NONE

! Argument variables:
REAL(SRK),INTENT(IN):: X
REAL(SRK):: FACT

! Private variables:
INTEGER:: I
REAL(SRK):: TEST

! Variable initialization:
! The factorial of 1 is 1, and the factorial of 0 is 1 by definition:
FACT=1

! ------------------------------------------------------------------------------

! Error control:

! The factorial of a negative integer number has no practical meaning.
! When n is a negative integer, it's factorial is equal to complex infinity.
IF ((X<0.0).AND.(X==INT(X))) THEN
WRITE(*,*)"fact"
WRITE(*,*)"ERROR: invalid argument in factorial."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! ------------------------------------------------------------------------------

IF (X==INT(X)) THEN
    IF (X>1) THEN
        DO I=1,INT(X)
            FACT=FACT*I
        END DO
    END IF
ELSE
! The factorial of a non-integer number can be expanded in a series.
    FACT=SQRT(2.0*PI)*(X**(X+0.5))*EXP(-X)* &
        & (1.0+(X**(-1))/12.0+(X**(-2))/288.0-139.0*(X**(-3))/51840.0)
END IF

TEST=FACT/FACT

IF (TEST/=TEST) THEN
WRITE(*,*)"fact"
WRITE(*,*)"ERROR: result is infinite."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF


END FUNCTION FACT

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE HeapsortCharacterLength(CHARACTER_ARRAY)

! HeapsortCharacterLength sorts a given character array into ascending order
! according to the length of the array elements.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! character_array: character, array (1D). The unsorted character array.

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! character_array: character, array (1D). The sorted character array.

! OUTPUT (OPTIONAL):
! No optional output arguments. All arguments are required.

! Example of usage:

!interface
!subroutine HeapsortCharacterLength(character_array)
!implicit none
!character,intent(inout):: character_array(:)*(*)
!end subroutine HeapsortCharacterLength
!end interface

!integer:: i
!character:: array(5)*(10)=(/'333  ','22   ','1    ','55555','4444 '/)
!call HeapsortCharacterLength(array)
!write(*,*)(trim(array(i))//' ',i=1,size(array))

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: warnings_pause

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(INOUT):: CHARACTER_ARRAY(:)*(*)

! Private variables:
INTEGER:: ELEMENT_LENGTH(SIZE(CHARACTER_ARRAY))
INTEGER:: SORTED_INDEX(SIZE(CHARACTER_ARRAY))
INTEGER:: I

! ------------------------------------------------------------------------------

! Error control:

IF (ALL(CHARACTER_ARRAY=='')) THEN
WRITE(*,*)"HeapsortCharacterLength"
WRITE(*,*)"WARNING: empty character array."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
RETURN
END IF

! ------------------------------------------------------------------------------

! Obtain an integer array with the trimmed lengths of the respective character
! array elements.
FORALL(I=1:SIZE(CHARACTER_ARRAY)) ELEMENT_LENGTH(I)=LEN_TRIM(CHARACTER_ARRAY(I))

! Sort the integer array and obtain its sorting index.
CALL HeapsortInteger(ELEMENT_LENGTH,SORTED_INDEX)

! Sort the character array according to the obtained index.
CHARACTER_ARRAY=CHARACTER_ARRAY(SORTED_INDEX)

END SUBROUTINE HeapsortCharacterLength

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE HeapsortInteger(ARRAY,SORTED_INDEX)

! HeapsortInteger sorts the elements of a given array into ascending order.

! Code based on:
!*******************************************************************************
! W. H. Press, S. A. Teukolsky, W. T. Vetterling, B. P. Flannery, Numerical    *
! Recipes in Fortran 90: The art of parallel scientific computing,             *
! 2nd Edition, Cambridge University Press, 1996, pg. 1171-1172.                *
!*******************************************************************************

! INPUT (REQUIRED):
! array: integer, array (1D). The unsorted array.

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! array: integer, array (1D). The sorted character array.

! OUTPUT (OPTIONAL):
! sorted_index: integer, array (1D) of size equal to size(array). The sorted
!    indexes of the original array.

! Example of usage:

!interface
!subroutine HeapsortInteger(array,sorted_index)
!implicit none
!integer,intent(inout):: array(:)
!integer,optional,intent(out):: sorted_index(:)
!end subroutine HeapsortInteger
!end interface

!integer:: array(9)=(/4,6,7,2,4,8,9,1,3/)
!integer:: sorted_index(9)
!call HeapsortInteger(array,sorted_index)
!write(*,*)"correct index : ",(/8,4,9,1,5,2,3,6,7/)
!write(*,*)array
!write(*,*)sorted_index

!integer:: array(10000)
!integer:: sorted_index(size(array)),i
!real:: time_begin,time_end
!real:: rand_num
!do i=1,size(array)
!call random_number(rand_num)
!array(i)=rand_num*10
!end do
!write(*,*)'start timing'
!call cpu_time(time_begin)
!call HeapsortInteger(array,sorted_index)
!call cpu_time(time_end)
!write(*,*)'time: ',time_end-time_begin,' seconds'

! ------------------------------------------------------------------------------

IMPLICIT NONE

! Arguement variables:
INTEGER,INTENT(INOUT):: ARRAY(:)
INTEGER,OPTIONAL,INTENT(OUT):: SORTED_INDEX(:)

! Private variables:
INTEGER:: I,N
LOGICAL:: PRESENT_SORTED_INDEX

! Variable initialization:
N=SIZE(ARRAY)
PRESENT_SORTED_INDEX=PRESENT(SORTED_INDEX)
IF (PRESENT_SORTED_INDEX) FORALL(I=1:N) SORTED_INDEX(I)=I

! ------------------------------------------------------------------------------

! Error control:

IF (PRESENT_SORTED_INDEX) THEN
IF (SIZE(SORTED_INDEX)/=SIZE(ARRAY)) THEN
WRITE(*,*)"HeapsortInteger"
WRITE(*,*)"ERROR: index should be as big as the array."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF
END IF

! ------------------------------------------------------------------------------

DO I=N/2,1,-1
    CALL SIFT_DOWN(I,N)
END DO

DO I=N,2,-1
    CALL SWAP_INTEGER(ARRAY(1),ARRAY(I))
    IF (PRESENT_SORTED_INDEX) &
        & CALL SWAP_INTEGER(SORTED_INDEX(1),SORTED_INDEX(I))
    CALL SIFT_DOWN(1,I-1)
END DO

! Contained subprograms:
CONTAINS

! ------------------------------------------------------------------------------

SUBROUTINE SIFT_DOWN(L,R)

IMPLICIT NONE

! Arguement variables:
INTEGER,INTENT(IN):: L,R

! Private variables:
INTEGER:: J,JOLD
INTEGER:: A,B

! Variable initialization:
A=ARRAY(L)
IF (PRESENT_SORTED_INDEX) B=SORTED_INDEX(L)
JOLD=L
J=L+L

DO
    IF (J>R) EXIT
    IF (J<R) THEN
        IF (ARRAY(J)<ARRAY(J+1)) J=J+1
    END IF
    IF (A>=ARRAY(J)) EXIT
    ARRAY(JOLD)=ARRAY(J)
    IF (PRESENT_SORTED_INDEX) SORTED_INDEX(JOLD)=SORTED_INDEX(J)
    JOLD=J
    J=J+J
END DO

ARRAY(JOLD)=A
IF (PRESENT_SORTED_INDEX) SORTED_INDEX(JOLD)=B

END SUBROUTINE SIFT_DOWN

! ------------------------------------------------------------------------------

SUBROUTINE SWAP_INTEGER(A,B)

IMPLICIT NONE

! Arguement variables:
INTEGER,INTENT(INOUT):: A,B

! Private variables:
INTEGER:: TEMP

TEMP=A
A=B
B=TEMP

END SUBROUTINE SWAP_INTEGER

END SUBROUTINE HeapsortInteger

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE FUNCTION LinearIntExp(X1,X2,Y1,Y2,X_GIVEN)

! LinearIntExp performs 2D linear interpolation or extrapolation between
! two given points and calculates the unknown dependent variable for a given
! value of the independent variable.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2008                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! x1: real, scalar. The x-coordinate of the first data pair
! x2: real, scalar. The x-coordinate of the second data pair
! y1: real, scalar. The y-coordinate of the first data pair
! y2: real, scalar. The y-coordinate of the second data pair
! x_given: real, scalar. The known x-coordinate of the third point.

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! LinearIntExp: real, scalar. The y-coordinate that was calculated.

! OUTPUT (OPTIONAL):
! No optional output arguments. All arguments are required.

! Example of usage:

!interface
!function LinearIntExp(x1,x2,y1,y2,x_given)
!use Config, only: srk
!implicit none
!real(srk),intent(in):: x1,x2,y1,y2,x_given
!real(srk):: LinearIntExp
!end function LinearIntExp
!end interface

!write(*,*)LinearIntExp(2.9_srk,5.5_srk,0.23_srk,0.99_srk,4.1_srk)

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: srk,warnings_pause

IMPLICIT NONE

! Argument variables:
REAL(SRK),INTENT(IN):: X1,X2,Y1,Y2,X_GIVEN
REAL(SRK):: LinearIntExp

! ------------------------------------------------------------------------------

! Error control:

IF ((X1==X2).AND.(Y1/=Y2)) THEN
WRITE(*,*)"LinearIntExp"
WRITE(*,*)"WARNING: infinite solutions in linear interpolation problem."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((X1==X2).AND.(Y1==Y2)) THEN
WRITE(*,*)"LinearIntExp"
WRITE(*,*)"WARNING: linear interpolation points coincide."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

! ------------------------------------------------------------------------------

! Vertical line:
IF ((X1==X2).AND.(Y1/=Y2)) LinearIntExp=(Y1+Y2)/2.0

! Coinciding points and horizontal line:
IF (((X1==X2).AND.(Y1==Y2)).OR.((X1/=X2).AND.(Y1==Y2))) LinearIntExp=Y1

! Line with slope other than zero:
IF ((X1/=X2).AND.(Y1/=Y2)) LinearIntExp=Y1+((Y2-Y1)/(X2-X1))*(X_GIVEN-X1)

END FUNCTION LinearIntExp

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------


END SUBMODULE SubLib_MathNumerics
