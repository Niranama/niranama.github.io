
SUBMODULE (ModLib_FENIA) SubLib_VisualizationToolkit

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
MODULE SUBROUTINE VTK_Initialize(OUTPUT_FILE)

! VTK_Initialize is called to initialize an output VTK PolyData file.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2009                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output VTK file. It must have
!	the ".vtp" extension (otherwise an error is generated).

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine VTK_Initialize(output_file)
!implicit none
!character,intent(in):: output_file*(*)
!end subroutine VTK_Initialize
!end interface

!! Create an empty VTK document:
!call VTK_Initialize(output_file='plot.vtp')
!call VTK_Finalize(output_file='plot.vtp')

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: warnings_pause

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*)

! Private variables:
INTEGER:: ERR,IO
LOGICAL:: CONNECTED

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)

! ------------------------------------------------------------------------------

! Error control:

IF (CONNECTED) THEN
WRITE(*,*)"VTK_Initialize"
WRITE(*,*)"WARNING: VTK file has already been accessed."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<5) THEN
WRITE(*,*)"VTK_Initialize"
WRITE(*,*)"ERROR: invalid filename."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-3:LEN_TRIM(OUTPUT_FILE))/='.vtp') THEN
WRITE(*,*)"VTK_Initialize"
WRITE(*,*)"ERROR: file must have the .vtp extension."
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! ------------------------------------------------------------------------------

WRITE(IO,'(A21)',IOSTAT=ERR)"<?xml version='1.0'?>"

IF (ERR/=0) THEN
WRITE(*,*)"VTK_Initialize"
WRITE(*,*)"ERROR: writing to file was not possible."
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

WRITE(IO,'(A39)')"<VTKFile type='PolyData' version='0.1'>"
WRITE(IO,'(A10)')"<PolyData>"
WRITE(IO,*)

END SUBROUTINE VTK_Initialize

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE VTK_Finalize(OUTPUT_FILE)

! VTK_Finalize is called to finalize an output VTK PolyData file.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2009                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output VTK file. It must have
!	the ".vtp" extension (otherwise an error is generated).

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine VTK_Finalize(output_file)
!implicit none
!character,intent(in):: output_file*(*)
!end subroutine VTK_Finalize
!end interface

!! Create an empty VTK document:
!call VTK_Finalize(output_file='plot.vtp')
!call VTK_Finalize(output_file='plot.vtp')

! ------------------------------------------------------------------------------

! External variables:
!use Config, only: warnings_pause

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*)

! Private variables:
INTEGER:: ERR,IO
LOGICAL:: CONNECTED

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)

! ------------------------------------------------------------------------------

! Error control:

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"VTK_Finalize"
WRITE(*,*)"WARNING: uninitialized VTK file."
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<5) THEN
WRITE(*,*)"VTK_Finalize"
WRITE(*,*)"ERROR: invalid filename."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-3:LEN_TRIM(OUTPUT_FILE))/='.vtp') THEN
WRITE(*,*)"VTK_Finalize"
WRITE(*,*)"ERROR: file must have the .vtp extension."
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! ------------------------------------------------------------------------------

WRITE(IO,'(A10)',IOSTAT=ERR)"<!--EOF-->"

IF (ERR/=0) THEN
WRITE(*,*)"VTK_Finalize"
WRITE(*,*)"ERROR: writing to file was not possible."
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

WRITE(IO,'(A11)')"</PolyData>"
WRITE(IO,'(A10)')"</VTKFile>"
WRITE(IO,*)

END SUBROUTINE VTK_Finalize

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

MODULE SUBROUTINE VTK_MultiLine(OUTPUT_FILE,X1,Y1,Z1,X2,Y2,Z2,SCALAR_NAME,SCALAR_VALUE)

! VTK_MultiLine is called to draw multiple lines in an VTK file. Prior to the
! call to subroutine VTK_MultiLine, a call to subroutine InitializeVTK must take
! place. In addition, after VTK_MultiLine is called, subroutine FinalizeVTK
! must be called.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2009                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character. The name of the output VTK file. It must have the
!    ".svg" extension (otherwise an error is generated).
! x1: real, array (2D). The x coordinates of the points of the first point
!    dataset. The elements are in 1-1 correspondance with the elements of the
!    other coordinate arrays.
! y1: real, array (2D). The y coordinates of the points of the first point
!    dataset. The elements are in 1-1 correspondance with the elements of the
!    other coordinate arrays.
! z1: real, array (2D). The z coordinates of the points of the first point
!    dataset. The elements are in 1-1 correspondance with the elements of the
!    other coordinate arrays.
! x2: real, array (2D). The x coordinates of the points of the second point
!    dataset. The elements are in 1-1 correspondance with the elements of the
!    other coordinate arrays.
! y2: real, array (2D). The y coordinates of the points of the second point
!    dataset. The elements are in 1-1 correspondance with the elements of the
!    other coordinate arrays.
! z2: real, array (2D). The z coordinates of the points of the second point
!    dataset. The elements are in 1-1 correspondance with the elements of the
!    other coordinate arrays.

! INPUT (OPTIONAL):
! scalar_name: character, array (1D). The names of the variables that will be
!    stored in the VTK object as XML variables. This variable may not
!    be omitted when variable "scalar_value" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_value" (dim=3) and their elements are 1-1
!    corresponding.
! scalar_value: real, array (3D). The values of the variables that will be
!    stored in the VTK object as XML variables. This variable may not
!    be omitted when variable "scalar_name" is included in the arguments
!    and vica versa. The size of the array (dim=3) must be equal to the size
!    of variable "scalar_name" and their elements are 1-1 corresponding.
!    The elements of the first two dimensions are in 1-1 correspondance with
!    the lines of the coordinate array.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine VTK_MultiLine(output_file,x1,y1,z1,x2,y2,z2, &
!    & scalar_name,scalar_value)
!!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*)
!real(srk),intent(in):: x1(:,:),y1(:,:),z1(:,:),x2(:,:),y2(:,:),z2(:,:)
!character,optional,intent(in):: scalar_name(:)*(*)
!real(srk),optional,intent(in):: scalar_value(:,:,:)
!end subroutine VTK_MultiLine
!end interface

! ----------------------------------------------------------------------------

! External dependencies:
!use Config, only: warnings_pause,srk

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*)
REAL(SRK),INTENT(IN):: X1(:,:),Y1(:,:),Z1(:,:),X2(:,:),Y2(:,:),Z2(:,:)
CHARACTER,OPTIONAL,INTENT(IN):: SCALAR_NAME(:)*(*)
REAL(SRK),OPTIONAL,INTENT(IN):: SCALAR_VALUE(:,:,:)

! Private variables:
INTEGER:: ERR,I,J,K,M,IO
REAL(SRK):: COORDINATE_ARRAY(2*3*SIZE(X1)),PROPERTY_ARRAY(SIZE(X1))
INTEGER:: ID(2,SIZE(X1,DIM=1),SIZE(X1,DIM=2))
INTEGER:: CONNECTIVITY_ARRAY(2*((SIZE(X1,DIM=1))*(SIZE(X1,DIM=2))))
INTEGER:: CONNECTIVITY_OFFSETS((SIZE(X1,DIM=1))*(SIZE(X1,DIM=2)))
LOGICAL:: CONNECTED

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)

! ----------------------------------------------------------------------------

! Error control:

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"VTK_MultiLine"
WRITE(*,*)"WARNING: uninitialized VTK file."
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<5) THEN
WRITE(*,*)"VTK_Path"
WRITE(*,*)"ERROR: invalid filename"
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-3:LEN_TRIM(OUTPUT_FILE))/='.vtp') THEN
WRITE(*,*)"VTK_Path"
WRITE(*,*)"ERROR: file must have the .vtp extension"
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((SIZE(X1)/=SIZE(Y1)).OR.(SIZE(X1)/=SIZE(Z1))) THEN
WRITE(*,*)"VTK_MultiLine"
WRITE(*,*)"ERROR: all coordinate arrays of matrix 1 must have equal amount of"
WRITE(*,*)"elements."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((SIZE(X2)/=SIZE(Y2)).OR.(SIZE(X2)/=SIZE(Z2))) THEN
WRITE(*,*)"VTK_MultiLine"
WRITE(*,*)"ERROR: all coordinate arrays of matrix 2 must have equal amount of"
WRITE(*,*)"elements."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (SIZE(X1)/=SIZE(X2)) THEN
WRITE(*,*)"VTK_MultiLine"
WRITE(*,*)"ERROR: matrixes must contain the same amount of points."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((ALL(X1==0.0)).AND.(ALL(Y1==0.0)).AND.(ALL(Z1==0.0))) THEN
WRITE(*,*)"VTK_MultiLine"
WRITE(*,*)"WARNING: missing data in matrix 1"
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((ALL(X2==0.0)).AND.(ALL(Y2==0.0)).AND.(ALL(Z2==0.0))) THEN
WRITE(*,*)"VTK_MultiLine"
WRITE(*,*)"WARNING: missing data in matrix 2"
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(.NOT.(PRESENT(SCALAR_VALUE)))) THEN
WRITE(*,*)"VTK_MultiLine"
WRITE(*,*)"ERROR: variable 'scalar_name' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_value'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_VALUE)).AND.(.NOT.(PRESENT(SCALAR_NAME)))) THEN
WRITE(*,*)"VTK_MultiLine"
WRITE(*,*)"ERROR: variable 'scalar_value' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_name'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
IF (SIZE(SCALAR_NAME)/=SIZE(SCALAR_VALUE,DIM=3)) THEN
WRITE(*,*)"VTK_MultiLine"
WRITE(*,*)"ERROR: variables 'scalar_name' and 'scalar_value' must have the"
WRITE(*,*)"same amount of columns."
WRITE(*,*)"size(scalar_name): ",SIZE(SCALAR_NAME)
WRITE(*,*)"size(scalar_value,dim=2): ",SIZE(SCALAR_VALUE,DIM=2)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF
END IF

IF (PRESENT(SCALAR_VALUE)) THEN
IF ((SIZE(SCALAR_VALUE,DIM=1)/=SIZE(X1,DIM=1)).OR. &
    & (SIZE(SCALAR_VALUE,DIM=2)/=SIZE(X1,DIM=2))) THEN
WRITE(*,*)"VTK_MultiLine"
WRITE(*,*)"ERROR: variables 'scalar_value' and 'points' must have the"
WRITE(*,*)"same size."
WRITE(*,*)"size(scalar_value,dim=1): ",SIZE(SCALAR_VALUE,DIM=1)
WRITE(*,*)"size(scalar_value,dim=2): ",SIZE(SCALAR_VALUE,DIM=2)
WRITE(*,*)"size(x1,dim=1): ",SIZE(X1,DIM=1)
WRITE(*,*)"size(x1,dim=2): ",SIZE(X1,DIM=2)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF
END IF

! ----------------------------------------------------------------------------

! Get the coordinates array.
J=1
M=0
DO I=1,SIZE(X1,DIM=1)
    DO K=1,SIZE(X1,DIM=2)
        COORDINATE_ARRAY(J:J+2)=(/X1(I,K),Y1(I,K),Z1(I,K)/)
        J=J+3
        ID(1,I,K)=M
        M=M+1
    END DO
END DO
DO I=1,SIZE(X1,DIM=1)
    DO K=1,SIZE(X1,DIM=2)
        COORDINATE_ARRAY(J:J+2)=(/X2(I,K),Y2(I,K),Z2(I,K)/)
        J=J+3
        ID(2,I,K)=M
        M=M+1
    END DO
END DO

! Get the connectivity array.
K=1
DO I=1,SIZE(X1,DIM=1)
    DO J=1,SIZE(X1,DIM=2)
        CONNECTIVITY_ARRAY(K:K+1)=(/ID(1,I,J),ID(2,I,J)/)
        K=K+2
    END DO
END DO

! Get the offsets array.
J=0
DO I=1,SIZE(CONNECTIVITY_OFFSETS)
    J=J+2
    CONNECTIVITY_OFFSETS(I)=J
END DO

! ----------------------------------------------------------------------------

WRITE(IO,'(A6)',IOSTAT=ERR)"<Piece"

IF (ERR/=0) THEN
WRITE(*,*)"VTK_Path"
WRITE(*,*)"ERROR: writing to file was not possible"
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

WRITE(IO,*)"NumberOfPoints='",2*SIZE(X1),"'"
WRITE(IO,*)"NumberOfVerts='",0,"'"
WRITE(IO,*)"NumberOfLines='",SIZE(X1),"'"
WRITE(IO,*)"NumberOfStrips='",0,"'"
WRITE(IO,*)"NumberOfPolys='",0,"'"
WRITE(IO,*)">"
WRITE(IO,*)

IF (PRESENT(SCALAR_NAME)) THEN
    WRITE(IO,*)"<PointData Scalars='PointProperty'>"
    DO M=1,SIZE(SCALAR_NAME)
        WRITE(IO,*)"<DataArray type='Float32' Name='", &
            & TRIM(SCALAR_NAME(M)),"' format='ascii'>"
        K=1 ! Get the property array.
        DO I=1,SIZE(X1,DIM=1)
            DO J=1,SIZE(X1,DIM=2)
                PROPERTY_ARRAY(K)=SCALAR_VALUE(I,J,M)
                K=K+1
            END DO
        END DO
        WRITE(IO,*)PROPERTY_ARRAY(:)
        WRITE(IO,*)"</DataArray>"
    END DO
    WRITE(IO,*)"</PointData>"
    WRITE(IO,*)
END IF

WRITE(IO,*)"<Points>"
WRITE(IO,*)"<DataArray type='Float32' NumberOfComponents='3' format='ascii'>"
WRITE(IO,*)COORDINATE_ARRAY
WRITE(IO,*)"</DataArray>"
WRITE(IO,*)"</Points>"
WRITE(IO,*)
WRITE(IO,*)"<Lines>"
WRITE(IO,*)
WRITE(IO,*)"<DataArray type='Int32' Name='connectivity' format='ascii'>"
WRITE(IO,*)CONNECTIVITY_ARRAY
WRITE(IO,*)"</DataArray>"
WRITE(IO,*)
WRITE(IO,*)"<DataArray type='Int32' Name='offsets' format='ascii'>"
WRITE(IO,*)CONNECTIVITY_OFFSETS
WRITE(IO,*)"</DataArray>"
WRITE(IO,*)
WRITE(IO,*)"</Lines>"
WRITE(IO,*)
WRITE(IO,'(A8)')"</Piece>"
WRITE(IO,*)

END SUBROUTINE VTK_MultiLine

! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------

MODULE SUBROUTINE VTK_Path(OUTPUT_FILE,POINTS,SCALAR_NAME,SCALAR_VALUE)

! VTK_Path is called to draw a pathline in an VTK file. Prior to the call
! to subroutine VTK_Path, a call to subroutine InitializeVTK must take
! place. In addition, after VTK_Path is called, subroutine FinalizeVTK
! must be called.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2009                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character. The name of the output VTK file. It must have the
!    ".vtp" extension (otherwise an error is generated).
! points: real, array (2D) with 3 columns. Each of the three columns
!    corresponds to the x, y and z coordinates respectively. It contains
!    the points in the order in which they will be connected in order to
!    form the path.

! INPUT (OPTIONAL):
! scalar_name: character, array (1D). The names of the variables that will be
!    stored in the VTK object as XML variables. This variable may not
!    be omitted when variable "scalar_value" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_value" and their elements are 1-1 corresponding.
! scalar_value: real, array (2D). The values of the variables that will be
!    stored in the VTK object as XML variables. This variable may not
!    be omitted when variable "scalar_name" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_name" and their elements are 1-1 corresponding.
!    The lines are in 1-1 correspondance with the lines of the coordinate
!    array.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine VTK_Path(output_file,points,scalar_name,scalar_value)
!!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*)
!real(srk),intent(in):: points(:,:)
!character,optional,intent(in):: scalar_name(:)*(*)
!real(srk),optional,intent(in):: scalar_value(:,:)
!end subroutine VTK_Path
!end interface

!real(srk):: p(4,3)

!p(1,1)=0 ; p(1,2)=0 ; p(1,3)=0
!p(2,1)=2 ; p(2,2)=2 ; p(2,3)=2
!p(3,1)=1 ; p(3,2)=2 ; p(3,3)=3
!p(4,1)=2 ; p(4,2)=1 ; p(4,3)=3

!call InitializeVTK('play.vtp')
!call VTK_Path('play.vtp',p)
!call FinalizeVTK('play.vtp')

! ----------------------------------------------------------------------------

! External variables:
!use Config, only: warnings_pause,srk

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*)
REAL(SRK),INTENT(IN):: POINTS(:,:)
CHARACTER,OPTIONAL,INTENT(IN):: SCALAR_NAME(:)*(*)
REAL(SRK),OPTIONAL,INTENT(IN):: SCALAR_VALUE(:,:)

! Private variables:
INTEGER:: ERR,I,J,K,IO
REAL(SRK):: COORDINATE_ARRAY(SIZE(POINTS))
INTEGER:: CONNECTIVITY_ARRAY((2*SIZE(POINTS,DIM=1))-2)
INTEGER:: CONNECTIVITY_OFFSETS(SIZE(POINTS,DIM=1)-1)
LOGICAL:: CONNECTED

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)

! ----------------------------------------------------------------------------

! Error control:

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"VTK_Path"
WRITE(*,*)"WARNING: uninitialized VTK file."
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<5) THEN
WRITE(*,*)"VTK_Path"
WRITE(*,*)"ERROR: invalid filename"
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-3:LEN_TRIM(OUTPUT_FILE))/='.vtp') THEN
WRITE(*,*)"VTK_Path"
WRITE(*,*)"ERROR: file must have the .vtp extension"
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (SIZE(POINTS,DIM=2)/=3) THEN
WRITE(*,*)"VTK_Path"
WRITE(*,*)"ERROR: the array with the path points must have 3 columns"
WRITE(*,*)"Each column corresponds to one dimension."
WRITE(*,*)"There were ",SIZE(POINTS,DIM=2)/=3," columns given"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(.NOT.(PRESENT(SCALAR_VALUE)))) THEN
WRITE(*,*)"VTK_Path"
WRITE(*,*)"ERROR: variable 'scalar_name' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_value'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_VALUE)).AND.(.NOT.(PRESENT(SCALAR_NAME)))) THEN
WRITE(*,*)"VTK_Path"
WRITE(*,*)"ERROR: variable 'scalar_value' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_name'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
IF (SIZE(SCALAR_NAME)/=SIZE(SCALAR_VALUE,DIM=2)) THEN
WRITE(*,*)"VTK_Path"
WRITE(*,*)"ERROR: variables 'scalar_name' and 'scalar_value' must have the"
WRITE(*,*)"same amount of columns."
WRITE(*,*)"size(scalar_name): ",SIZE(SCALAR_NAME)
WRITE(*,*)"size(scalar_value,dim=2): ",SIZE(SCALAR_VALUE,DIM=2)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF
END IF

IF (PRESENT(SCALAR_VALUE)) THEN
IF (SIZE(SCALAR_VALUE,DIM=1)/=SIZE(POINTS,DIM=1)) THEN
WRITE(*,*)"VTK_Path"
WRITE(*,*)"ERROR: variables 'scalar_value' and 'points' must have the"
WRITE(*,*)"same amount of lines."
WRITE(*,*)"size(scalar_value,dim=1): ",SIZE(SCALAR_VALUE,DIM=1)
WRITE(*,*)"size(points,dim=1): ",SIZE(POINTS,DIM=1)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF
END IF

! ----------------------------------------------------------------------------

! Get the coordinates array.
J=1
DO I=1,SIZE(POINTS,DIM=1)
    COORDINATE_ARRAY(J:J+2)=POINTS(I,:)
    J=J+3
END DO

! Get the connectivity array.
J=0
CONNECTIVITY_ARRAY(1)=J
DO I=2,SIZE(CONNECTIVITY_ARRAY)-1,2
    J=J+1
    CONNECTIVITY_ARRAY(I:I+1)=J
END DO
CONNECTIVITY_ARRAY(SIZE(CONNECTIVITY_ARRAY))=SIZE(POINTS,DIM=1)-1

! Get the offsets array.
J=0
DO I=1,SIZE(CONNECTIVITY_OFFSETS)
    J=J+2
    CONNECTIVITY_OFFSETS(I)=J
END DO

! ----------------------------------------------------------------------------

WRITE(IO,'(A6)',IOSTAT=ERR)"<Piece"

IF (ERR/=0) THEN
WRITE(*,*)"VTK_Path"
WRITE(*,*)"ERROR: writing to file was not possible"
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

WRITE(IO,*)"NumberOfPoints='",SIZE(POINTS,DIM=1),"'"
WRITE(IO,*)"NumberOfVerts='",0,"'"
WRITE(IO,*)"NumberOfLines='",SIZE(POINTS,DIM=1)-1,"'"
WRITE(IO,*)"NumberOfStrips='",0,"'"
WRITE(IO,*)"NumberOfPolys='",0,"'"
WRITE(IO,*)">"
WRITE(IO,*)

IF (PRESENT(SCALAR_NAME)) THEN
    WRITE(IO,*)"<PointData Scalars='PointProperty'>"
    DO I=1,SIZE(SCALAR_NAME)
        WRITE(IO,*)"<DataArray type='Float32' Name='", &
            & TRIM(SCALAR_NAME(I)),"' format='ascii'>"
        WRITE(IO,*)SCALAR_VALUE(:,I)
        WRITE(IO,*)"</DataArray>"
    END DO
    WRITE(IO,*)"</PointData>"
    WRITE(IO,*)
END IF

WRITE(IO,*)"<Points>"
WRITE(IO,*)"<DataArray type='Float32' NumberOfComponents='3' format='ascii'>"
WRITE(IO,*)COORDINATE_ARRAY
WRITE(IO,*)"</DataArray>"
WRITE(IO,*)"</Points>"
WRITE(IO,*)
WRITE(IO,*)"<Lines>"
WRITE(IO,*)
WRITE(IO,*)"<DataArray type='Int32' Name='connectivity' format='ascii'>"
WRITE(IO,*)CONNECTIVITY_ARRAY
WRITE(IO,*)"</DataArray>"
WRITE(IO,*)
WRITE(IO,*)"<DataArray type='Int32' Name='offsets' format='ascii'>"
WRITE(IO,*)CONNECTIVITY_OFFSETS
WRITE(IO,*)"</DataArray>"
WRITE(IO,*)
WRITE(IO,*)"</Lines>"
WRITE(IO,*)
WRITE(IO,'(A8)')"</Piece>"
WRITE(IO,*)

END SUBROUTINE VTK_Path

! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------

MODULE SUBROUTINE VTK_Sphere(OUTPUT_FILE,CENTER,DIAMETER,RADIAL_RES,AXIAL_RES)

! VTK_Sphere is called to create a sphere in an output VTK file of type
! PolyData.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2009                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character, scalar. The name of the output VTK file. It must have
!    the ".vtp" extension (otherwise an error is generated).
! center: real, array (1D) with 3 elements. The coordinates of the sphere
!    center. The three elements correspond to the x, y and z coordinates
!    respectively.
! diameter: real, scalar. The diameter of the sphere (dimensionless).
! radial_res: integer, scalar. Defines the radial resolution of the
!    sphere, i.e. how many points will every horizontal ring (see
!    "axial_res") have in order to approximate the sphere's
!    curvature on a horizontal surface.
! axial_res: integer, scalar. Defines the axial resolution of the
!    sphere, i.e. how many horizontal rings will be used to approximate
!    the sphere's curvature in the vertical direction.

! INPUT (OPTIONAL):
! No optional input arguments. All arguments are required.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!subroutine VTK_Sphere(output_file,center,diameter,radial_res,axial_res)
!!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*)
!real(srk),intent(in):: center(3),diameter
!integer,intent(in):: radial_res,axial_res
!end subroutine VTK_Sphere

!call VTK_Initialize(output_file='play.vtp')
!call VTK_Sphere(output_file='play.vtp', &
!    & center=(/0.0_srk,0.0_srk,0.0_srk/), &
!    & diameter=2.0_srk, &
!    & radial_res=10, &
!    & axial_res=10)
!call VTK_Finalize(output_file='play.vtp')

! ----------------------------------------------------------------------------

! External variables:
!use Config, only: warnings_pause,srk,pi

! External subprograms:
IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*)
REAL(SRK),INTENT(IN):: CENTER(3),DIAMETER
INTEGER,INTENT(IN):: RADIAL_RES,AXIAL_RES

! Private variables:
REAL(SRK),ALLOCATABLE:: RING_CENTER(:,:),RING_RADIUS(:),COORDINATE_ARRAY(:)
REAL(SRK):: AXIAL_INCREMENT
INTEGER,ALLOCATABLE:: CONNECTIVITY_ARRAY(:),CONNECTIVITY_OFFSETS(:)
INTEGER:: I,J,K,H,ERR,IO,AXIAL_RES_INTERNAL
LOGICAL:: CONNECTED

TYPE RINGPOINTREAL
REAL(SRK):: COORDINATE(3)
INTEGER:: ID
END TYPE RINGPOINTREAL

TYPE(RINGPOINTREAL):: NORTH_POLE,SOUTH_POLE
TYPE(RINGPOINTREAL),ALLOCATABLE:: RING_POINT(:,:)

! Variable initialization:

IO=FileManager(OUTPUT_FILE,CONNECTED)

! It is much more comfortable to have an odd number for axial resolution.
! Therefore, to avoid throwing an error if the axial resolution is an even
! number, an intenral, always-odd axial resolution is used instead.
IF (MOD(AXIAL_RES,2)==0.0) THEN
    AXIAL_RES_INTERNAL=AXIAL_RES+1
ELSE
    AXIAL_RES_INTERNAL=AXIAL_RES
END IF

IF (ALLOCATED(RING_CENTER)) DEALLOCATE(RING_CENTER)
IF (ALLOCATED(RING_RADIUS)) DEALLOCATE(RING_RADIUS)
IF (ALLOCATED(CONNECTIVITY_ARRAY)) DEALLOCATE(CONNECTIVITY_ARRAY)
IF (ALLOCATED(CONNECTIVITY_OFFSETS)) DEALLOCATE(CONNECTIVITY_OFFSETS)
IF (ALLOCATED(COORDINATE_ARRAY)) DEALLOCATE(COORDINATE_ARRAY)
IF (ALLOCATED(RING_POINT)) DEALLOCATE(RING_POINT)

ALLOCATE(RING_CENTER(AXIAL_RES_INTERNAL,3))
ALLOCATE(RING_RADIUS(AXIAL_RES_INTERNAL))
ALLOCATE(CONNECTIVITY_ARRAY &
    & (3*2*RADIAL_RES+4*RADIAL_RES*(AXIAL_RES_INTERNAL-1)))
ALLOCATE(CONNECTIVITY_OFFSETS &
    & (2*RADIAL_RES+RADIAL_RES*(AXIAL_RES_INTERNAL-1)))
ALLOCATE(COORDINATE_ARRAY(3*(RADIAL_RES*AXIAL_RES_INTERNAL+2)))
ALLOCATE(RING_POINT(AXIAL_RES_INTERNAL,RADIAL_RES))

! ----------------------------------------------------------------------------

! Error control:

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"VTK_Sphere"
WRITE(*,*)"WARNING: uninitialized VTK file."
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<5) THEN
WRITE(*,*)"VTK_Sphere"
WRITE(*,*)"ERROR: invalid filename"
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-3:LEN_TRIM(OUTPUT_FILE))/='.vtp') THEN
WRITE(*,*)"VTK_Sphere"
WRITE(*,*)"ERROR: file must have the .vtp extension"
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (AXIAL_RES<1) THEN
WRITE(*,*)"VTK_Sphere"
WRITE(*,*)"ERROR: axial resolution must be greater than 1"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (RADIAL_RES<3) THEN
WRITE(*,*)"VTK_Sphere"
WRITE(*,*)"ERROR: axial resolution must be greater than 3"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (DIAMETER<=0.0) THEN
WRITE(*,*)"VTK_Sphere"
WRITE(*,*)"ERROR: diameter must be greater than zero"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

! ----------------------------------------------------------------------------

! Calculate the coordinates of the poles:
NORTH_POLE%COORDINATE=CENTER
NORTH_POLE%COORDINATE(2)=NORTH_POLE%COORDINATE(2)+DIAMETER/2.0
SOUTH_POLE%COORDINATE=CENTER
SOUTH_POLE%COORDINATE(2)=SOUTH_POLE%COORDINATE(2)-DIAMETER/2.0

! Define the increment of the axial resolution:
AXIAL_INCREMENT=DIAMETER/(REAL(AXIAL_RES_INTERNAL+1))

DO I=1,AXIAL_RES_INTERNAL
    IF (I==AXIAL_RES_INTERNAL/2+1) THEN
! For the ring at the sphere center, directly attribute the given data to
! avoid numerical innacuracies.
        RING_CENTER(I,:)=CENTER
        RING_RADIUS(I)=DIAMETER/2.0
    ELSE
! Calculate the centers of the horizontal rings
        RING_CENTER(I,:)=NORTH_POLE%COORDINATE
        RING_CENTER(I,2)=RING_CENTER(I,2)-REAL(I)*AXIAL_INCREMENT
! Use the calculated centers of the horizontal rings to calculate the
! respective ring radii.
        RING_RADIUS(I)=SQRT((DIAMETER/2.0)**2- &
            & (SUM((CENTER(:)-RING_CENTER(I,:))**2)))
    END IF
END DO

! ----------------------------------------------------------------------------

! Initialize the id-numbers and give the id-number 0 to the north pole.
K=0
NORTH_POLE%ID=K

! Form the coordinate array according to the id-numbers so that the VTK
! id-numbers match the ones in this subroutine.
COORDINATE_ARRAY(3*K+1:3*K+3)=NORTH_POLE%COORDINATE

DO I=1,AXIAL_RES_INTERNAL
    DO J=1,RADIAL_RES
! Use the calculated radius of every ring to evenly distribute points on it.
        RING_POINT(I,J)%COORDINATE=(/ &
            & RING_CENTER(I,1)+REAL(RING_RADIUS(I))* &
            &    COS(REAL(J)*2.0*PI/REAL(RADIAL_RES)), &
            & RING_CENTER(I,2), &
            & RING_CENTER(I,3)+REAL(RING_RADIUS(I))* &
            &    SIN(REAL(J)*2.0*PI/REAL(RADIAL_RES))/)
! Give an id-number to every point.
        K=K+1
        RING_POINT(I,J)%ID=K
        COORDINATE_ARRAY(3*K+1:3*K+3)=RING_POINT(I,J)%COORDINATE
    END DO
END DO

! Give the last id number to the south pole.
K=K+1
SOUTH_POLE%ID=K
COORDINATE_ARRAY(3*K+1:3*K+3)=SOUTH_POLE%COORDINATE

! ----------------------------------------------------------------------------

! Get the connectivity array for the sphere sides (4 point surfaces):
K=1
H=0
DO I=1,AXIAL_RES_INTERNAL-1
    DO J=1,RADIAL_RES-1
        CONNECTIVITY_ARRAY(K:K+3)=(/ &
            & RING_POINT(I,J)%ID, &
            & RING_POINT(I,J+1)%ID, &
            & RING_POINT(I+1,J+1)%ID, &
            & RING_POINT(I+1,J)%ID/)
        K=K+4
        H=H+1
        CONNECTIVITY_OFFSETS(H)=K-1
    END DO

    CONNECTIVITY_ARRAY(K:K+3)=(/ &
        & RING_POINT(I,RADIAL_RES)%ID, &
        & RING_POINT(I,1)%ID, &
        & RING_POINT(I+1,1)%ID, &
        & RING_POINT(I+1,RADIAL_RES)%ID/)
    K=K+4
    H=H+1
    CONNECTIVITY_OFFSETS(H)=K-1
END DO

! Get the connectivity array for the sphere caps (3 point surfaces):
DO J=1,RADIAL_RES-1
    CONNECTIVITY_ARRAY(K:K+2)=(/ &
        & NORTH_POLE%ID, &
        & RING_POINT(1,J)%ID, &
        & RING_POINT(1,J+1)%ID/)
    K=K+3
    H=H+1
    CONNECTIVITY_OFFSETS(H)=K-1
    CONNECTIVITY_ARRAY(K:K+2)=(/ &
        & SOUTH_POLE%ID, &
        & RING_POINT(AXIAL_RES_INTERNAL,J)%ID, &
        & RING_POINT(AXIAL_RES_INTERNAL,J+1)%ID/)
    K=K+3
    H=H+1
    CONNECTIVITY_OFFSETS(H)=K-1
END DO

CONNECTIVITY_ARRAY(K:K+2)=(/ &
    & NORTH_POLE%ID, &
    & RING_POINT(1,RADIAL_RES)%ID, &
    & RING_POINT(1,1)%ID/)
K=K+3
H=H+1
CONNECTIVITY_OFFSETS(H)=K-1
CONNECTIVITY_ARRAY(K:K+2)=(/ &
    & SOUTH_POLE%ID, &
    & RING_POINT(AXIAL_RES_INTERNAL,RADIAL_RES)%ID, &
    & RING_POINT(AXIAL_RES_INTERNAL,1)%ID/)
K=K+3
H=H+1
CONNECTIVITY_OFFSETS(H)=K-1

! ----------------------------------------------------------------------------

WRITE(IO,'(A6)',IOSTAT=ERR)"<Piece"

IF (ERR/=0) THEN
WRITE(*,*)"VTK_Sphere"
WRITE(*,*)"ERROR: writing to file was not possible."
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

WRITE(IO,*)"NumberOfPoints='",SIZE(COORDINATE_ARRAY)/3,"'"
WRITE(IO,*)"NumberOfVerts='",0,"'"
WRITE(IO,*)"NumberOfLines='",0,"'"
WRITE(IO,*)"NumberOfStrips='",0,"'"
WRITE(IO,*)"NumberOfPolys='",SIZE(CONNECTIVITY_OFFSETS),"'"
WRITE(IO,*)">"
WRITE(IO,*)
WRITE(IO,*)"<Points>"
WRITE(IO,*)"<DataArray type='Float32' NumberOfComponents='3' format='ascii'>"
WRITE(IO,*)COORDINATE_ARRAY
WRITE(IO,*)"</DataArray>"
WRITE(IO,*)"</Points>"
WRITE(IO,*)
WRITE(IO,*)"<Polys>"
WRITE(IO,*)
WRITE(IO,*)"<DataArray type='Int32' Name='connectivity' format='ascii'>"
WRITE(IO,*)CONNECTIVITY_ARRAY
WRITE(IO,*)"</DataArray>"
WRITE(IO,*)
WRITE(IO,*)"<DataArray type='Int32' Name='offsets' format='ascii'>"
WRITE(IO,*)CONNECTIVITY_OFFSETS
WRITE(IO,*)"</DataArray>"
WRITE(IO,*)
WRITE(IO,*)"</Polys>"
WRITE(IO,*)
WRITE(IO,'(A8)')"</Piece>"
WRITE(IO,*)

END SUBROUTINE VTK_Sphere

! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------

MODULE SUBROUTINE VTK_Surface(OUTPUT_FILE,X,Y,Z,SCALAR_NAME,SCALAR_VALUE)

! VTK_Surface is called to draw a surface in an VTK file. Prior to
! the call to subroutine VTK_Surface, a call to subroutine
! InitializeVTK must take place. In addition, after VTK_Surface is
! called, subroutine FinalizeVTK must be called.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2009                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character. The name of the output VTK file. It must have the
!    ".svg" extension (otherwise an error is generated).
! x: real, array (2D). The x coordinates of the points. The elements are in
!    1-1 correspondance with the elements of the other coordinate arrays.
! y: real, array (2D). The y coordinates of the points. The elements are in
!    1-1 correspondance with the elements of the other coordinate arrays.
! z: real, array (2D). The z coordinates of the points. The elements are in
!    1-1 correspondance with the elements of the other coordinate arrays.

! INPUT (OPTIONAL):
! scalar_name: character, array (1D). The names of the variables that will be
!    stored in the VTK object as XML variables. This variable may not
!    be omitted when variable "scalar_value" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_value" (dim=3) and their elements are 1-1
!    corresponding.
! scalar_value: real, array (3D). The values of the variables that will be
!    stored in the VTK object as XML variables. This variable may not
!    be omitted when variable "scalar_name" is included in the arguments
!    and vica versa. The size of the array (dim=3) must be equal to the size
!    of variable "scalar_name" and their elements are 1-1 corresponding.
!    The elements of the first two dimensions are in 1-1 correspondance with
!    the lines of the coordinate array.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine VTK_Surface(output_file,x,y,z,scalar_name,scalar_value)
!!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*)
!real(srk),intent(in):: x(:,:),y(:,:),z(:,:)
!character,optional,intent(in):: scalar_name(:)*(*)
!real(srk),optional,intent(in):: scalar_value(:,:,:)
!end subroutine VTK_Surface
!end interface

! ----------------------------------------------------------------------------

! External dependencies:
!use Config, only: warnings_pause,srk

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*)
REAL(SRK),INTENT(IN):: X(:,:),Y(:,:),Z(:,:)
CHARACTER,OPTIONAL,INTENT(IN):: SCALAR_NAME(:)*(*)
REAL(SRK),OPTIONAL,INTENT(IN):: SCALAR_VALUE(:,:,:)

! Private variables:
INTEGER:: ERR,I,J,K,M,IO
REAL(SRK):: COORDINATE_ARRAY(3*SIZE(X)),PROPERTY_ARRAY(SIZE(X))
INTEGER:: ID(SIZE(X,DIM=1),SIZE(X,DIM=2))
INTEGER:: CONNECTIVITY_ARRAY(4*((SIZE(X,DIM=1)-1)*(SIZE(X,DIM=2)-1)))
INTEGER:: CONNECTIVITY_OFFSETS((SIZE(X,DIM=1)-1)*(SIZE(X,DIM=2)-1))
LOGICAL:: CONNECTED

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)

! ----------------------------------------------------------------------------

! Error control:

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"VTK_Surface"
WRITE(*,*)"WARNING: uninitialized VTK file."
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<5) THEN
WRITE(*,*)"VTK_Path"
WRITE(*,*)"ERROR: invalid filename"
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-3:LEN_TRIM(OUTPUT_FILE))/='.vtp') THEN
WRITE(*,*)"VTK_Path"
WRITE(*,*)"ERROR: file must have the .vtp extension"
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((SIZE(X)/=SIZE(Y)).OR.(SIZE(X)/=SIZE(Z))) THEN
WRITE(*,*)"VTK_Surface"
WRITE(*,*)"ERROR: all coordinate arrays must have equal amount of elements"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((ALL(X==0.0)).AND.(ALL(Y==0.0)).AND.(ALL(Z==0.0))) THEN
WRITE(*,*)"VTK_Surface"
WRITE(*,*)"WARNING: missing data"
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(.NOT.(PRESENT(SCALAR_VALUE)))) THEN
WRITE(*,*)"VTK_Surface"
WRITE(*,*)"ERROR: variable 'scalar_name' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_value'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_VALUE)).AND.(.NOT.(PRESENT(SCALAR_NAME)))) THEN
WRITE(*,*)"VTK_Surface"
WRITE(*,*)"ERROR: variable 'scalar_value' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_name'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
IF (SIZE(SCALAR_NAME)/=SIZE(SCALAR_VALUE,DIM=3)) THEN
WRITE(*,*)"VTK_Surface"
WRITE(*,*)"ERROR: variables 'scalar_name' and 'scalar_value' must have the"
WRITE(*,*)"same amount of columns."
WRITE(*,*)"size(scalar_name): ",SIZE(SCALAR_NAME)
WRITE(*,*)"size(scalar_value,dim=2): ",SIZE(SCALAR_VALUE,DIM=2)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF
END IF

IF (PRESENT(SCALAR_VALUE)) THEN
IF ((SIZE(SCALAR_VALUE,DIM=1)/=SIZE(X,DIM=1)).OR. &
    & (SIZE(SCALAR_VALUE,DIM=2)/=SIZE(X,DIM=2))) THEN
WRITE(*,*)"VTK_Surface"
WRITE(*,*)"ERROR: variables 'scalar_value' and 'points' must have the"
WRITE(*,*)"same size."
WRITE(*,*)"size(scalar_value,dim=1): ",SIZE(SCALAR_VALUE,DIM=1)
WRITE(*,*)"size(scalar_value,dim=2): ",SIZE(SCALAR_VALUE,DIM=2)
WRITE(*,*)"size(points,dim=1): ",SIZE(X,DIM=1)
WRITE(*,*)"size(points,dim=2): ",SIZE(X,DIM=2)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF
END IF

! ----------------------------------------------------------------------------

! Get the coordinates array.
J=1
M=0
DO I=1,SIZE(X,DIM=1)
    DO K=1,SIZE(X,DIM=2)
        COORDINATE_ARRAY(J:J+2)=(/X(I,K),Y(I,K),Z(I,K)/)
        J=J+3
        ID(I,K)=M
        M=M+1
    END DO
END DO

! Get the connectivity array.
K=1
DO I=1,SIZE(X,DIM=1)-1
    DO J=1,SIZE(X,DIM=2)-1
        CONNECTIVITY_ARRAY(K:K+3)= &
            & (/ID(I,J),ID(I,J+1),ID(I+1,J+1),ID(I+1,J)/)
        K=K+4
    END DO
END DO

! Get the offsets array.
J=0
DO I=1,SIZE(CONNECTIVITY_OFFSETS)
    J=J+4
    CONNECTIVITY_OFFSETS(I)=J
END DO

! ----------------------------------------------------------------------------

WRITE(IO,'(A6)',IOSTAT=ERR)"<Piece"

IF (ERR/=0) THEN
WRITE(*,*)"VTK_Path"
WRITE(*,*)"ERROR: writing to file was not possible"
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

WRITE(IO,*)"NumberOfPoints='",SIZE(X),"'"
WRITE(IO,*)"NumberOfVerts='",0,"'"
WRITE(IO,*)"NumberOfLines='",0,"'"
WRITE(IO,*)"NumberOfStrips='",0,"'"
WRITE(IO,*)"NumberOfPolys='",(SIZE(X,DIM=1)-1)*(SIZE(X,DIM=2)-1),"'"
WRITE(IO,*)">"
WRITE(IO,*)

IF (PRESENT(SCALAR_NAME)) THEN
    WRITE(IO,*)"<PointData Scalars='PointProperty'>"
    DO M=1,SIZE(SCALAR_NAME)
        WRITE(IO,*)"<DataArray type='Float32' Name='", &
            & TRIM(SCALAR_NAME(M)),"' format='ascii'>"
        K=1 ! Get the property array.
        DO I=1,SIZE(X,DIM=1)
            DO J=1,SIZE(X,DIM=2)
                PROPERTY_ARRAY(K)=SCALAR_VALUE(I,J,M)
                K=K+1
            END DO
        END DO
        WRITE(IO,*)PROPERTY_ARRAY(:)
        WRITE(IO,*)"</DataArray>"
    END DO
    WRITE(IO,*)"</PointData>"
    WRITE(IO,*)
END IF

WRITE(IO,*)"<Points>"
WRITE(IO,*)"<DataArray type='Float32' NumberOfComponents='3' format='ascii'>"
WRITE(IO,*)COORDINATE_ARRAY
WRITE(IO,*)"</DataArray>"
WRITE(IO,*)"</Points>"
WRITE(IO,*)
WRITE(IO,*)"<Polys>"
WRITE(IO,*)
WRITE(IO,*)"<DataArray type='Int32' Name='connectivity' format='ascii'>"
WRITE(IO,*)CONNECTIVITY_ARRAY
WRITE(IO,*)"</DataArray>"
WRITE(IO,*)
WRITE(IO,*)"<DataArray type='Int32' Name='offsets' format='ascii'>"
WRITE(IO,*)CONNECTIVITY_OFFSETS
WRITE(IO,*)"</DataArray>"
WRITE(IO,*)
WRITE(IO,*)"</Polys>"
WRITE(IO,*)
WRITE(IO,'(A8)')"</Piece>"
WRITE(IO,*)

END SUBROUTINE VTK_Surface

! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------

MODULE SUBROUTINE VTK_VectorField(OUTPUT_FILE,X,Y,Z,VX,VY,VZ,SCALAR_NAME,SCALAR_VALUE)

! VTK_VectorField is called to draw a vector field in an VTK file. Prior to
! the call to subroutine VTK_VectorField, a call to subroutine
! InitializeVTK must take place. In addition, after VTK_VectorField is
! called, subroutine FinalizeVTK must be called.

!*******************************************************************************
! Evangelos Bertakis, F.EN.I.A. Project, 2009                                  *
!*******************************************************************************

! INPUT (REQUIRED):
! output_file: character. The name of the output VTK file. It must have the
!    ".svg" extension (otherwise an error is generated).
! x: real, array (2D). The x coordinates of the points. The elements are in
!    1-1 correspondance with the elements of the other coordinate arrays.
! y: real, array (2D). The y coordinates of the points. The elements are in
!    1-1 correspondance with the elements of the other coordinate arrays.
! z: real, array (2D). The z coordinates of the points. The elements are in
!    1-1 correspondance with the elements of the other coordinate arrays.
! vx: real, array (2D). The x-components of the vectors.
! vy: real, array (2D). The x-components of the vectors.
! vz: real, array (2D). The x-components of the vectors.

! INPUT (OPTIONAL):
! scalar_name: character, array (1D). The names of the variables that will be
!    stored in the VTK object as XML variables. This variable may not
!    be omitted when variable "scalar_value" is included in the arguments
!    and vica versa. The size of the array must be equal to the size of
!    variable "scalar_value" (dim=3) and their elements are 1-1
!    corresponding.
! scalar_value: real, array (3D). The values of the variables that will be
!    stored in the VTK object as XML variables. This variable may not
!    be omitted when variable "scalar_name" is included in the arguments
!    and vica versa. The size of the array (dim=3) must be equal to the size
!    of variable "scalar_name" and their elements are 1-1 corresponding.
!    The elements of the first two dimensions are in 1-1 correspondance with
!    the lines of the coordinate array.

! OUTPUT (REQUIRED):
! This subroutine returns no output.

! OUTPUT (OPTIONAL):
! This subroutine returns no output.

! Example of usage:

!interface
!subroutine VTK_VectorField(output_file,x,y,z,vx,vy,vz,scalar_name,scalar_value)
!!use Config, only: srk
!implicit none
!character,intent(in):: output_file*(*)
!real(srk),intent(in):: x(:,:),y(:,:),z(:,:),vx(:,:),vy(:,:),vz(:,:)
!character,optional,intent(in):: scalar_name(:)*(*)
!real(srk),optional,intent(in):: scalar_value(:,:,:)
!end subroutine VTK_VectorField
!end interface

! ----------------------------------------------------------------------------

! External dependencies:
!use Config, only: warnings_pause,srk

IMPLICIT NONE

! Argument variables:
CHARACTER,INTENT(IN):: OUTPUT_FILE*(*)
REAL(SRK),INTENT(IN):: X(:,:),Y(:,:),Z(:,:),VX(:,:),VY(:,:),VZ(:,:)
CHARACTER,OPTIONAL,INTENT(IN):: SCALAR_NAME(:)*(*)
REAL(SRK),OPTIONAL,INTENT(IN):: SCALAR_VALUE(:,:,:)

! Private variables:
INTEGER:: ERR,I,J,K,M,IO
REAL(SRK):: COORDINATE_ARRAY(3*SIZE(X)),VECTOR_ARRAY(3*SIZE(X))
REAL(SRK):: PROPERTY_ARRAY(SIZE(X))
INTEGER:: ID(SIZE(X,DIM=1),SIZE(X,DIM=2))
INTEGER:: CONNECTIVITY_ARRAY(4*((SIZE(X,DIM=1)-1)*(SIZE(X,DIM=2)-1)))
INTEGER:: CONNECTIVITY_OFFSETS((SIZE(X,DIM=1)-1)*(SIZE(X,DIM=2)-1))
LOGICAL:: CONNECTED

! Variable initialization:
IO=FileManager(OUTPUT_FILE,CONNECTED)

! ----------------------------------------------------------------------------

! Error control:

IF (.NOT.CONNECTED) THEN
WRITE(*,*)"VTK_VectorField"
WRITE(*,*)"WARNING: uninitialized VTK file."
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF (LEN_TRIM(OUTPUT_FILE)<5) THEN
WRITE(*,*)"VTK_Path"
WRITE(*,*)"ERROR: invalid filename"
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF (OUTPUT_FILE(LEN_TRIM(OUTPUT_FILE)-3:LEN_TRIM(OUTPUT_FILE))/='.vtp') THEN
WRITE(*,*)"VTK_Path"
WRITE(*,*)"ERROR: file must have the .vtp extension"
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((SIZE(X)/=SIZE(Y)).OR.(SIZE(X)/=SIZE(Z))) THEN
WRITE(*,*)"VTK_VectorField"
WRITE(*,*)"ERROR: all coordinate arrays must have equal amount of elements"
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((ALL(X==0.0)).AND.(ALL(Y==0.0)).AND.(ALL(Z==0.0))) THEN
WRITE(*,*)"VTK_VectorField"
WRITE(*,*)"WARNING: missing coordinate data"
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((ALL(VX==0.0)).AND.(ALL(VY==0.0)).AND.(ALL(VZ==0.0))) THEN
WRITE(*,*)"VTK_VectorField"
WRITE(*,*)"WARNING: missing vector data"
WRITE(*,*)"Continue..."
IF (WARNINGS_PAUSE) READ(*,*)
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(.NOT.(PRESENT(SCALAR_VALUE)))) THEN
WRITE(*,*)"VTK_VectorField"
WRITE(*,*)"ERROR: variable 'scalar_name' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_value'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_VALUE)).AND.(.NOT.(PRESENT(SCALAR_NAME)))) THEN
WRITE(*,*)"VTK_VectorField"
WRITE(*,*)"ERROR: variable 'scalar_value' may not appear in the arguments"
WRITE(*,*)"without variable 'scalar_name'."
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

IF ((PRESENT(SCALAR_NAME)).AND.(PRESENT(SCALAR_VALUE))) THEN
IF (SIZE(SCALAR_NAME)/=SIZE(SCALAR_VALUE,DIM=3)) THEN
WRITE(*,*)"VTK_VectorField"
WRITE(*,*)"ERROR: variables 'scalar_name' and 'scalar_value' must have the"
WRITE(*,*)"same amount of columns."
WRITE(*,*)"size(scalar_name): ",SIZE(SCALAR_NAME)
WRITE(*,*)"size(scalar_value,dim=2): ",SIZE(SCALAR_VALUE,DIM=2)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF
END IF

IF (PRESENT(SCALAR_VALUE)) THEN
IF ((SIZE(SCALAR_VALUE,DIM=1)/=SIZE(X,DIM=1)).OR. &
    & (SIZE(SCALAR_VALUE,DIM=2)/=SIZE(X,DIM=2))) THEN
WRITE(*,*)"VTK_VectorField"
WRITE(*,*)"ERROR: variables 'scalar_value' and 'points' must have the"
WRITE(*,*)"same size."
WRITE(*,*)"size(scalar_value,dim=1): ",SIZE(SCALAR_VALUE,DIM=1)
WRITE(*,*)"size(scalar_value,dim=2): ",SIZE(SCALAR_VALUE,DIM=2)
WRITE(*,*)"size(points,dim=1): ",SIZE(X,DIM=1)
WRITE(*,*)"size(points,dim=2): ",SIZE(X,DIM=2)
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF
END IF

! ----------------------------------------------------------------------------

! Get the vector components array.
J=1
DO I=1,SIZE(X,DIM=1)
    DO K=1,SIZE(X,DIM=2)
        VECTOR_ARRAY(J:J+2)=(/VX(I,K),VY(I,K),VZ(I,K)/)
        J=J+3
    END DO
END DO

! Get the coordinates array.
J=1
M=0
DO I=1,SIZE(X,DIM=1)
    DO K=1,SIZE(X,DIM=2)
        COORDINATE_ARRAY(J:J+2)=(/X(I,K),Y(I,K),Z(I,K)/)
        J=J+3
        ID(I,K)=M
        M=M+1
    END DO
END DO

! Get the connectivity array.
K=1
DO I=1,SIZE(X,DIM=1)-1
    DO J=1,SIZE(X,DIM=2)-1
        CONNECTIVITY_ARRAY(K:K+3)= &
            & (/ID(I,J),ID(I,J+1),ID(I+1,J+1),ID(I+1,J)/)
        K=K+4
    END DO
END DO

! Get the offsets array.
J=0
DO I=1,SIZE(CONNECTIVITY_OFFSETS)
    J=J+4
    CONNECTIVITY_OFFSETS(I)=J
END DO

! ----------------------------------------------------------------------------

WRITE(IO,'(A6)',IOSTAT=ERR)"<Piece"

IF (ERR/=0) THEN
WRITE(*,*)"VTK_Path"
WRITE(*,*)"ERROR: writing to file was not possible"
WRITE(*,*)"filename : ",TRIM(OUTPUT_FILE)
WRITE(*,*)"error flag : ",ERR
WRITE(*,*)"Program terminated."
READ(*,*)
STOP
END IF

WRITE(IO,*)"NumberOfPoints='",SIZE(X),"'"
WRITE(IO,*)"NumberOfVerts='",0,"'"
WRITE(IO,*)"NumberOfLines='",0,"'"
WRITE(IO,*)"NumberOfStrips='",0,"'"
WRITE(IO,*)"NumberOfPolys='",(SIZE(X,DIM=1)-1)*(SIZE(X,DIM=2)-1),"'"
WRITE(IO,*)">"
WRITE(IO,*)

IF (PRESENT(SCALAR_NAME)) THEN
    WRITE(IO,*)"<PointData Scalars='PointProperty'>"
    DO M=1,SIZE(SCALAR_NAME)
        WRITE(IO,*)"<DataArray type='Float32' Name='", &
            & TRIM(SCALAR_NAME(M)),"' format='ascii'>"
        K=1 ! Get the property array.
        DO I=1,SIZE(X,DIM=1)
            DO J=1,SIZE(X,DIM=2)
                PROPERTY_ARRAY(K)=SCALAR_VALUE(I,J,M)
                K=K+1
            END DO
        END DO
        WRITE(IO,*)PROPERTY_ARRAY(:)
        WRITE(IO,*)"</DataArray>"
    END DO
    WRITE(IO,*)"</PointData>"
    WRITE(IO,*)
END IF

WRITE(IO,*)"<PointData Vectors='vectorfield'>"
WRITE(IO,*)"<DataArray type='Float32' NumberOfComponents='3' &
    &Name='vectorfield' format='ascii'>"
WRITE(IO,*)VECTOR_ARRAY
WRITE(IO,*)"</DataArray>"
WRITE(IO,*)"</PointData>"
WRITE(IO,*)
WRITE(IO,*)"<Points>"
WRITE(IO,*)"<DataArray type='Float32' NumberOfComponents='3' format='ascii'>"
WRITE(IO,*)COORDINATE_ARRAY
WRITE(IO,*)"</DataArray>"
WRITE(IO,*)"</Points>"
WRITE(IO,*)
WRITE(IO,*)"<Polys>"
WRITE(IO,*)
WRITE(IO,*)"<DataArray type='Int32' Name='connectivity' format='ascii'>"
WRITE(IO,*)CONNECTIVITY_ARRAY
WRITE(IO,*)"</DataArray>"
WRITE(IO,*)
WRITE(IO,*)"<DataArray type='Int32' Name='offsets' format='ascii'>"
WRITE(IO,*)CONNECTIVITY_OFFSETS
WRITE(IO,*)"</DataArray>"
WRITE(IO,*)
WRITE(IO,*)"</Polys>"
WRITE(IO,*)
WRITE(IO,'(A8)')"</Piece>"
WRITE(IO,*)

END SUBROUTINE VTK_VectorField

! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------


END SUBMODULE SubLib_VisualizationToolkit
