module stdlib_linalg
  !!Provides a support for various linear algebra procedures
  !! ([Specification](../page/specs/stdlib_linalg.html))
  use stdlib_kinds, only: xdp, int8, int16, int32, int64
  use stdlib_linalg_constants, only: sp, dp, qp, lk, ilp
  use stdlib_error, only: error_stop
  use stdlib_optval, only: optval
  use stdlib_linalg_state, only: linalg_state_type, linalg_error_handling
  implicit none
  private

  public :: det
  public :: operator(.det.)
  public :: diag
  public :: eye
  public :: lstsq
  public :: lstsq_space
  public :: solve
  public :: solve_lu  
  public :: solve_lstsq
  public :: trace
  public :: svd
  public :: svdvals
  public :: outer_product
  public :: kronecker_product
  public :: cross_product
  public :: is_square
  public :: is_diagonal
  public :: is_symmetric
  public :: is_skew_symmetric
  public :: is_hermitian
  public :: is_triangular
  public :: is_hessenberg
  
  ! Export linalg error handling
  public :: linalg_state_type, linalg_error_handling

  interface diag
    !! version: experimental
    !!
    !! Creates a diagonal array or extract the diagonal elements of an array
    !! ([Specification](../page/specs/stdlib_linalg.html#
    !! diag-create-a-diagonal-array-or-extract-the-diagonal-elements-of-an-array))
      !
      ! Vector to matrix
      !
      module function diag_rsp(v) result(res)
        real(sp), intent(in) :: v(:)
        real(sp) :: res(size(v),size(v))
      end function diag_rsp
      module function diag_rdp(v) result(res)
        real(dp), intent(in) :: v(:)
        real(dp) :: res(size(v),size(v))
      end function diag_rdp
      module function diag_rxdp(v) result(res)
        real(xdp), intent(in) :: v(:)
        real(xdp) :: res(size(v),size(v))
      end function diag_rxdp
      module function diag_rqp(v) result(res)
        real(qp), intent(in) :: v(:)
        real(qp) :: res(size(v),size(v))
      end function diag_rqp
      module function diag_csp(v) result(res)
        complex(sp), intent(in) :: v(:)
        complex(sp) :: res(size(v),size(v))
      end function diag_csp
      module function diag_cdp(v) result(res)
        complex(dp), intent(in) :: v(:)
        complex(dp) :: res(size(v),size(v))
      end function diag_cdp
      module function diag_cxdp(v) result(res)
        complex(xdp), intent(in) :: v(:)
        complex(xdp) :: res(size(v),size(v))
      end function diag_cxdp
      module function diag_cqp(v) result(res)
        complex(qp), intent(in) :: v(:)
        complex(qp) :: res(size(v),size(v))
      end function diag_cqp
      module function diag_iint8(v) result(res)
        integer(int8), intent(in) :: v(:)
        integer(int8) :: res(size(v),size(v))
      end function diag_iint8
      module function diag_iint16(v) result(res)
        integer(int16), intent(in) :: v(:)
        integer(int16) :: res(size(v),size(v))
      end function diag_iint16
      module function diag_iint32(v) result(res)
        integer(int32), intent(in) :: v(:)
        integer(int32) :: res(size(v),size(v))
      end function diag_iint32
      module function diag_iint64(v) result(res)
        integer(int64), intent(in) :: v(:)
        integer(int64) :: res(size(v),size(v))
      end function diag_iint64
      module function diag_rsp_k(v,k) result(res)
        real(sp), intent(in) :: v(:)
        integer, intent(in) :: k
        real(sp) :: res(size(v)+abs(k),size(v)+abs(k))
      end function diag_rsp_k
      module function diag_rdp_k(v,k) result(res)
        real(dp), intent(in) :: v(:)
        integer, intent(in) :: k
        real(dp) :: res(size(v)+abs(k),size(v)+abs(k))
      end function diag_rdp_k
      module function diag_rxdp_k(v,k) result(res)
        real(xdp), intent(in) :: v(:)
        integer, intent(in) :: k
        real(xdp) :: res(size(v)+abs(k),size(v)+abs(k))
      end function diag_rxdp_k
      module function diag_rqp_k(v,k) result(res)
        real(qp), intent(in) :: v(:)
        integer, intent(in) :: k
        real(qp) :: res(size(v)+abs(k),size(v)+abs(k))
      end function diag_rqp_k
      module function diag_csp_k(v,k) result(res)
        complex(sp), intent(in) :: v(:)
        integer, intent(in) :: k
        complex(sp) :: res(size(v)+abs(k),size(v)+abs(k))
      end function diag_csp_k
      module function diag_cdp_k(v,k) result(res)
        complex(dp), intent(in) :: v(:)
        integer, intent(in) :: k
        complex(dp) :: res(size(v)+abs(k),size(v)+abs(k))
      end function diag_cdp_k
      module function diag_cxdp_k(v,k) result(res)
        complex(xdp), intent(in) :: v(:)
        integer, intent(in) :: k
        complex(xdp) :: res(size(v)+abs(k),size(v)+abs(k))
      end function diag_cxdp_k
      module function diag_cqp_k(v,k) result(res)
        complex(qp), intent(in) :: v(:)
        integer, intent(in) :: k
        complex(qp) :: res(size(v)+abs(k),size(v)+abs(k))
      end function diag_cqp_k
      module function diag_iint8_k(v,k) result(res)
        integer(int8), intent(in) :: v(:)
        integer, intent(in) :: k
        integer(int8) :: res(size(v)+abs(k),size(v)+abs(k))
      end function diag_iint8_k
      module function diag_iint16_k(v,k) result(res)
        integer(int16), intent(in) :: v(:)
        integer, intent(in) :: k
        integer(int16) :: res(size(v)+abs(k),size(v)+abs(k))
      end function diag_iint16_k
      module function diag_iint32_k(v,k) result(res)
        integer(int32), intent(in) :: v(:)
        integer, intent(in) :: k
        integer(int32) :: res(size(v)+abs(k),size(v)+abs(k))
      end function diag_iint32_k
      module function diag_iint64_k(v,k) result(res)
        integer(int64), intent(in) :: v(:)
        integer, intent(in) :: k
        integer(int64) :: res(size(v)+abs(k),size(v)+abs(k))
      end function diag_iint64_k

      !
      ! Matrix to vector
      !
      module function diag_rsp_mat(A) result(res)
        real(sp), intent(in) :: A(:,:)
        real(sp) :: res(minval(shape(A)))
      end function diag_rsp_mat
      module function diag_rdp_mat(A) result(res)
        real(dp), intent(in) :: A(:,:)
        real(dp) :: res(minval(shape(A)))
      end function diag_rdp_mat
      module function diag_rxdp_mat(A) result(res)
        real(xdp), intent(in) :: A(:,:)
        real(xdp) :: res(minval(shape(A)))
      end function diag_rxdp_mat
      module function diag_rqp_mat(A) result(res)
        real(qp), intent(in) :: A(:,:)
        real(qp) :: res(minval(shape(A)))
      end function diag_rqp_mat
      module function diag_csp_mat(A) result(res)
        complex(sp), intent(in) :: A(:,:)
        complex(sp) :: res(minval(shape(A)))
      end function diag_csp_mat
      module function diag_cdp_mat(A) result(res)
        complex(dp), intent(in) :: A(:,:)
        complex(dp) :: res(minval(shape(A)))
      end function diag_cdp_mat
      module function diag_cxdp_mat(A) result(res)
        complex(xdp), intent(in) :: A(:,:)
        complex(xdp) :: res(minval(shape(A)))
      end function diag_cxdp_mat
      module function diag_cqp_mat(A) result(res)
        complex(qp), intent(in) :: A(:,:)
        complex(qp) :: res(minval(shape(A)))
      end function diag_cqp_mat
      module function diag_iint8_mat(A) result(res)
        integer(int8), intent(in) :: A(:,:)
        integer(int8) :: res(minval(shape(A)))
      end function diag_iint8_mat
      module function diag_iint16_mat(A) result(res)
        integer(int16), intent(in) :: A(:,:)
        integer(int16) :: res(minval(shape(A)))
      end function diag_iint16_mat
      module function diag_iint32_mat(A) result(res)
        integer(int32), intent(in) :: A(:,:)
        integer(int32) :: res(minval(shape(A)))
      end function diag_iint32_mat
      module function diag_iint64_mat(A) result(res)
        integer(int64), intent(in) :: A(:,:)
        integer(int64) :: res(minval(shape(A)))
      end function diag_iint64_mat
      module function diag_rsp_mat_k(A,k) result(res)
        real(sp), intent(in) :: A(:,:)
        integer, intent(in) :: k
        real(sp) :: res(minval(shape(A))-abs(k))
      end function diag_rsp_mat_k
      module function diag_rdp_mat_k(A,k) result(res)
        real(dp), intent(in) :: A(:,:)
        integer, intent(in) :: k
        real(dp) :: res(minval(shape(A))-abs(k))
      end function diag_rdp_mat_k
      module function diag_rxdp_mat_k(A,k) result(res)
        real(xdp), intent(in) :: A(:,:)
        integer, intent(in) :: k
        real(xdp) :: res(minval(shape(A))-abs(k))
      end function diag_rxdp_mat_k
      module function diag_rqp_mat_k(A,k) result(res)
        real(qp), intent(in) :: A(:,:)
        integer, intent(in) :: k
        real(qp) :: res(minval(shape(A))-abs(k))
      end function diag_rqp_mat_k
      module function diag_csp_mat_k(A,k) result(res)
        complex(sp), intent(in) :: A(:,:)
        integer, intent(in) :: k
        complex(sp) :: res(minval(shape(A))-abs(k))
      end function diag_csp_mat_k
      module function diag_cdp_mat_k(A,k) result(res)
        complex(dp), intent(in) :: A(:,:)
        integer, intent(in) :: k
        complex(dp) :: res(minval(shape(A))-abs(k))
      end function diag_cdp_mat_k
      module function diag_cxdp_mat_k(A,k) result(res)
        complex(xdp), intent(in) :: A(:,:)
        integer, intent(in) :: k
        complex(xdp) :: res(minval(shape(A))-abs(k))
      end function diag_cxdp_mat_k
      module function diag_cqp_mat_k(A,k) result(res)
        complex(qp), intent(in) :: A(:,:)
        integer, intent(in) :: k
        complex(qp) :: res(minval(shape(A))-abs(k))
      end function diag_cqp_mat_k
      module function diag_iint8_mat_k(A,k) result(res)
        integer(int8), intent(in) :: A(:,:)
        integer, intent(in) :: k
        integer(int8) :: res(minval(shape(A))-abs(k))
      end function diag_iint8_mat_k
      module function diag_iint16_mat_k(A,k) result(res)
        integer(int16), intent(in) :: A(:,:)
        integer, intent(in) :: k
        integer(int16) :: res(minval(shape(A))-abs(k))
      end function diag_iint16_mat_k
      module function diag_iint32_mat_k(A,k) result(res)
        integer(int32), intent(in) :: A(:,:)
        integer, intent(in) :: k
        integer(int32) :: res(minval(shape(A))-abs(k))
      end function diag_iint32_mat_k
      module function diag_iint64_mat_k(A,k) result(res)
        integer(int64), intent(in) :: A(:,:)
        integer, intent(in) :: k
        integer(int64) :: res(minval(shape(A))-abs(k))
      end function diag_iint64_mat_k
  end interface


  ! Matrix trace
  interface trace
    !! version: experimental
    !!
    !! Computes the trace of a matrix
    !! ([Specification](../page/specs/stdlib_linalg.html#
    !! trace-trace-of-a-matrix))
      module procedure trace_rsp
      module procedure trace_rdp
      module procedure trace_rxdp
      module procedure trace_rqp
      module procedure trace_csp
      module procedure trace_cdp
      module procedure trace_cxdp
      module procedure trace_cqp
      module procedure trace_iint8
      module procedure trace_iint16
      module procedure trace_iint32
      module procedure trace_iint64
  end interface


  ! Outer product (of two vectors)
  interface outer_product
    !! version: experimental
    !!
    !! Computes the outer product of two vectors, returning a rank-2 array
    !! ([Specification](../page/specs/stdlib_linalg.html#
    !! outer_product-computes-the-outer-product-of-two-vectors))
      pure module function outer_product_rsp(u, v) result(res)
        real(sp), intent(in) :: u(:), v(:)
        real(sp) :: res(size(u),size(v))
      end function outer_product_rsp
      pure module function outer_product_rdp(u, v) result(res)
        real(dp), intent(in) :: u(:), v(:)
        real(dp) :: res(size(u),size(v))
      end function outer_product_rdp
      pure module function outer_product_rxdp(u, v) result(res)
        real(xdp), intent(in) :: u(:), v(:)
        real(xdp) :: res(size(u),size(v))
      end function outer_product_rxdp
      pure module function outer_product_rqp(u, v) result(res)
        real(qp), intent(in) :: u(:), v(:)
        real(qp) :: res(size(u),size(v))
      end function outer_product_rqp
      pure module function outer_product_csp(u, v) result(res)
        complex(sp), intent(in) :: u(:), v(:)
        complex(sp) :: res(size(u),size(v))
      end function outer_product_csp
      pure module function outer_product_cdp(u, v) result(res)
        complex(dp), intent(in) :: u(:), v(:)
        complex(dp) :: res(size(u),size(v))
      end function outer_product_cdp
      pure module function outer_product_cxdp(u, v) result(res)
        complex(xdp), intent(in) :: u(:), v(:)
        complex(xdp) :: res(size(u),size(v))
      end function outer_product_cxdp
      pure module function outer_product_cqp(u, v) result(res)
        complex(qp), intent(in) :: u(:), v(:)
        complex(qp) :: res(size(u),size(v))
      end function outer_product_cqp
      pure module function outer_product_iint8(u, v) result(res)
        integer(int8), intent(in) :: u(:), v(:)
        integer(int8) :: res(size(u),size(v))
      end function outer_product_iint8
      pure module function outer_product_iint16(u, v) result(res)
        integer(int16), intent(in) :: u(:), v(:)
        integer(int16) :: res(size(u),size(v))
      end function outer_product_iint16
      pure module function outer_product_iint32(u, v) result(res)
        integer(int32), intent(in) :: u(:), v(:)
        integer(int32) :: res(size(u),size(v))
      end function outer_product_iint32
      pure module function outer_product_iint64(u, v) result(res)
        integer(int64), intent(in) :: u(:), v(:)
        integer(int64) :: res(size(u),size(v))
      end function outer_product_iint64
  end interface outer_product

  interface kronecker_product
    !! version: experimental
    !!
    !! Computes the Kronecker product of two arrays of size M1xN1, and of M2xN2, returning an (M1*M2)x(N1*N2) array
    !! ([Specification](../page/specs/stdlib_linalg.html#
    !! kronecker_product-computes-the-kronecker-product-of-two-matrices))
      pure module function kronecker_product_rsp(A, B) result(C)
        real(sp), intent(in) :: A(:,:), B(:,:)
        real(sp) :: C(size(A,dim=1)*size(B,dim=1),size(A,dim=2)*size(B,dim=2))
      end function kronecker_product_rsp
      pure module function kronecker_product_rdp(A, B) result(C)
        real(dp), intent(in) :: A(:,:), B(:,:)
        real(dp) :: C(size(A,dim=1)*size(B,dim=1),size(A,dim=2)*size(B,dim=2))
      end function kronecker_product_rdp
      pure module function kronecker_product_rxdp(A, B) result(C)
        real(xdp), intent(in) :: A(:,:), B(:,:)
        real(xdp) :: C(size(A,dim=1)*size(B,dim=1),size(A,dim=2)*size(B,dim=2))
      end function kronecker_product_rxdp
      pure module function kronecker_product_rqp(A, B) result(C)
        real(qp), intent(in) :: A(:,:), B(:,:)
        real(qp) :: C(size(A,dim=1)*size(B,dim=1),size(A,dim=2)*size(B,dim=2))
      end function kronecker_product_rqp
      pure module function kronecker_product_csp(A, B) result(C)
        complex(sp), intent(in) :: A(:,:), B(:,:)
        complex(sp) :: C(size(A,dim=1)*size(B,dim=1),size(A,dim=2)*size(B,dim=2))
      end function kronecker_product_csp
      pure module function kronecker_product_cdp(A, B) result(C)
        complex(dp), intent(in) :: A(:,:), B(:,:)
        complex(dp) :: C(size(A,dim=1)*size(B,dim=1),size(A,dim=2)*size(B,dim=2))
      end function kronecker_product_cdp
      pure module function kronecker_product_cxdp(A, B) result(C)
        complex(xdp), intent(in) :: A(:,:), B(:,:)
        complex(xdp) :: C(size(A,dim=1)*size(B,dim=1),size(A,dim=2)*size(B,dim=2))
      end function kronecker_product_cxdp
      pure module function kronecker_product_cqp(A, B) result(C)
        complex(qp), intent(in) :: A(:,:), B(:,:)
        complex(qp) :: C(size(A,dim=1)*size(B,dim=1),size(A,dim=2)*size(B,dim=2))
      end function kronecker_product_cqp
      pure module function kronecker_product_iint8(A, B) result(C)
        integer(int8), intent(in) :: A(:,:), B(:,:)
        integer(int8) :: C(size(A,dim=1)*size(B,dim=1),size(A,dim=2)*size(B,dim=2))
      end function kronecker_product_iint8
      pure module function kronecker_product_iint16(A, B) result(C)
        integer(int16), intent(in) :: A(:,:), B(:,:)
        integer(int16) :: C(size(A,dim=1)*size(B,dim=1),size(A,dim=2)*size(B,dim=2))
      end function kronecker_product_iint16
      pure module function kronecker_product_iint32(A, B) result(C)
        integer(int32), intent(in) :: A(:,:), B(:,:)
        integer(int32) :: C(size(A,dim=1)*size(B,dim=1),size(A,dim=2)*size(B,dim=2))
      end function kronecker_product_iint32
      pure module function kronecker_product_iint64(A, B) result(C)
        integer(int64), intent(in) :: A(:,:), B(:,:)
        integer(int64) :: C(size(A,dim=1)*size(B,dim=1),size(A,dim=2)*size(B,dim=2))
      end function kronecker_product_iint64
  end interface kronecker_product


  ! Cross product (of two vectors)
  interface cross_product
    !! version: experimental
    !!
    !! Computes the cross product of two vectors, returning a rank-1 and size-3 array
    !! ([Specification](../page/specs/stdlib_linalg.html#cross_product-computes-the-cross-product-of-two-3-d-vectors))
      pure module function cross_product_rsp(a, b) result(res)
        real(sp), intent(in) :: a(3), b(3)
        real(sp) :: res(3)
      end function cross_product_rsp
      pure module function cross_product_rdp(a, b) result(res)
        real(dp), intent(in) :: a(3), b(3)
        real(dp) :: res(3)
      end function cross_product_rdp
      pure module function cross_product_rxdp(a, b) result(res)
        real(xdp), intent(in) :: a(3), b(3)
        real(xdp) :: res(3)
      end function cross_product_rxdp
      pure module function cross_product_rqp(a, b) result(res)
        real(qp), intent(in) :: a(3), b(3)
        real(qp) :: res(3)
      end function cross_product_rqp
      pure module function cross_product_csp(a, b) result(res)
        complex(sp), intent(in) :: a(3), b(3)
        complex(sp) :: res(3)
      end function cross_product_csp
      pure module function cross_product_cdp(a, b) result(res)
        complex(dp), intent(in) :: a(3), b(3)
        complex(dp) :: res(3)
      end function cross_product_cdp
      pure module function cross_product_cxdp(a, b) result(res)
        complex(xdp), intent(in) :: a(3), b(3)
        complex(xdp) :: res(3)
      end function cross_product_cxdp
      pure module function cross_product_cqp(a, b) result(res)
        complex(qp), intent(in) :: a(3), b(3)
        complex(qp) :: res(3)
      end function cross_product_cqp
      pure module function cross_product_iint8(a, b) result(res)
        integer(int8), intent(in) :: a(3), b(3)
        integer(int8) :: res(3)
      end function cross_product_iint8
      pure module function cross_product_iint16(a, b) result(res)
        integer(int16), intent(in) :: a(3), b(3)
        integer(int16) :: res(3)
      end function cross_product_iint16
      pure module function cross_product_iint32(a, b) result(res)
        integer(int32), intent(in) :: a(3), b(3)
        integer(int32) :: res(3)
      end function cross_product_iint32
      pure module function cross_product_iint64(a, b) result(res)
        integer(int64), intent(in) :: a(3), b(3)
        integer(int64) :: res(3)
      end function cross_product_iint64
  end interface cross_product


  ! Check for squareness
  interface is_square
    !! version: experimental
    !!
    !! Checks if a matrix (rank-2 array) is square
    !! ([Specification](../page/specs/stdlib_linalg.html#
    !! is_square-checks-if-a-matrix-is-square))
      module procedure is_square_rsp
      module procedure is_square_rdp
      module procedure is_square_rxdp
      module procedure is_square_rqp
      module procedure is_square_csp
      module procedure is_square_cdp
      module procedure is_square_cxdp
      module procedure is_square_cqp
      module procedure is_square_iint8
      module procedure is_square_iint16
      module procedure is_square_iint32
      module procedure is_square_iint64
  end interface is_square


  ! Check for diagonality
  interface is_diagonal
    !! version: experimental
    !!
    !! Checks if a matrix (rank-2 array) is diagonal
    !! ([Specification](../page/specs/stdlib_linalg.html#
    !! is_diagonal-checks-if-a-matrix-is-diagonal))
      module procedure is_diagonal_rsp
      module procedure is_diagonal_rdp
      module procedure is_diagonal_rxdp
      module procedure is_diagonal_rqp
      module procedure is_diagonal_csp
      module procedure is_diagonal_cdp
      module procedure is_diagonal_cxdp
      module procedure is_diagonal_cqp
      module procedure is_diagonal_iint8
      module procedure is_diagonal_iint16
      module procedure is_diagonal_iint32
      module procedure is_diagonal_iint64
  end interface is_diagonal


  ! Check for symmetry
  interface is_symmetric
    !! version: experimental
    !!
    !! Checks if a matrix (rank-2 array) is symmetric
    !! ([Specification](../page/specs/stdlib_linalg.html#
    !! is_symmetric-checks-if-a-matrix-is-symmetric))
      module procedure is_symmetric_rsp
      module procedure is_symmetric_rdp
      module procedure is_symmetric_rxdp
      module procedure is_symmetric_rqp
      module procedure is_symmetric_csp
      module procedure is_symmetric_cdp
      module procedure is_symmetric_cxdp
      module procedure is_symmetric_cqp
      module procedure is_symmetric_iint8
      module procedure is_symmetric_iint16
      module procedure is_symmetric_iint32
      module procedure is_symmetric_iint64
  end interface is_symmetric


  ! Check for skew-symmetry
  interface is_skew_symmetric
    !! version: experimental
    !!
    !! Checks if a matrix (rank-2 array) is skew-symmetric
    !! ([Specification](../page/specs/stdlib_linalg.html#
    !! is_skew_symmetric-checks-if-a-matrix-is-skew-symmetric))
      module procedure is_skew_symmetric_rsp
      module procedure is_skew_symmetric_rdp
      module procedure is_skew_symmetric_rxdp
      module procedure is_skew_symmetric_rqp
      module procedure is_skew_symmetric_csp
      module procedure is_skew_symmetric_cdp
      module procedure is_skew_symmetric_cxdp
      module procedure is_skew_symmetric_cqp
      module procedure is_skew_symmetric_iint8
      module procedure is_skew_symmetric_iint16
      module procedure is_skew_symmetric_iint32
      module procedure is_skew_symmetric_iint64
  end interface is_skew_symmetric


  ! Check for Hermiticity
  interface is_hermitian
    !! version: experimental
    !!
    !! Checks if a matrix (rank-2 array) is Hermitian
    !! ([Specification](../page/specs/stdlib_linalg.html#
    !! is_hermitian-checks-if-a-matrix-is-hermitian))
      module procedure is_hermitian_rsp
      module procedure is_hermitian_rdp
      module procedure is_hermitian_rxdp
      module procedure is_hermitian_rqp
      module procedure is_hermitian_csp
      module procedure is_hermitian_cdp
      module procedure is_hermitian_cxdp
      module procedure is_hermitian_cqp
      module procedure is_hermitian_iint8
      module procedure is_hermitian_iint16
      module procedure is_hermitian_iint32
      module procedure is_hermitian_iint64
  end interface is_hermitian


  ! Check for triangularity
  interface is_triangular
    !! version: experimental
    !!
    !! Checks if a matrix (rank-2 array) is triangular
    !! ([Specification](../page/specs/stdlib_linalg.html#
    !! is_triangular-checks-if-a-matrix-is-triangular))
      module procedure is_triangular_rsp
      module procedure is_triangular_rdp
      module procedure is_triangular_rxdp
      module procedure is_triangular_rqp
      module procedure is_triangular_csp
      module procedure is_triangular_cdp
      module procedure is_triangular_cxdp
      module procedure is_triangular_cqp
      module procedure is_triangular_iint8
      module procedure is_triangular_iint16
      module procedure is_triangular_iint32
      module procedure is_triangular_iint64
  end interface is_triangular
  

  ! Check for matrix being Hessenberg
  interface is_hessenberg
    !! version: experimental
    !!
    !! Checks if a matrix (rank-2 array) is Hessenberg
    !! ([Specification](../page/specs/stdlib_linalg.html#
    !! is_hessenberg-checks-if-a-matrix-is-hessenberg))
      module procedure is_Hessenberg_rsp
      module procedure is_Hessenberg_rdp
      module procedure is_Hessenberg_rxdp
      module procedure is_Hessenberg_rqp
      module procedure is_Hessenberg_csp
      module procedure is_Hessenberg_cdp
      module procedure is_Hessenberg_cxdp
      module procedure is_Hessenberg_cqp
      module procedure is_Hessenberg_iint8
      module procedure is_Hessenberg_iint16
      module procedure is_Hessenberg_iint32
      module procedure is_Hessenberg_iint64
  end interface is_hessenberg

  ! Solve linear system system Ax=b.
  interface solve
     !! version: experimental 
     !!
     !! Solves the linear system \( A \cdot x = b \) for the unknown vector \( x \) from a square matrix \( A \). 
     !! ([Specification](../page/specs/stdlib_linalg.html#solve-solves-a-linear-matrix-equation-or-a-linear-system-of-equations))
     !!
     !!### Summary 
     !! Interface for solving a linear system arising from a general matrix.
     !!
     !!### Description
     !! 
     !! This interface provides methods for computing the solution of a linear matrix system.
     !! Supported data types include `real` and `complex`. No assumption is made on the matrix 
     !! structure. 
     !! The function can solve simultaneously either one (from a 1-d right-hand-side vector `b(:)`) 
     !! or several (from a 2-d right-hand-side vector `b(:,:)`) systems.
     !! 
     !!@note The solution is based on LAPACK's generic LU decomposition based solvers `*GESV`.
     !!@note BLAS/LAPACK backends do not currently support extended precision (``xdp``).
     !!    
     module function stdlib_linalg_s_solve_one(a,b,overwrite_a,err) result(x)
         !> Input matrix a[n,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:)     
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         real(sp), allocatable, target :: x(:)
     end function stdlib_linalg_s_solve_one
     pure module function stdlib_linalg_s_pure_solve_one(a,b) result(x)
         !> Input matrix a[n,n]
         real(sp), intent(in) :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(sp), allocatable, target :: x(:)
     end function stdlib_linalg_s_pure_solve_one
     module function stdlib_linalg_d_solve_one(a,b,overwrite_a,err) result(x)
         !> Input matrix a[n,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:)     
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         real(dp), allocatable, target :: x(:)
     end function stdlib_linalg_d_solve_one
     pure module function stdlib_linalg_d_pure_solve_one(a,b) result(x)
         !> Input matrix a[n,n]
         real(dp), intent(in) :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(dp), allocatable, target :: x(:)
     end function stdlib_linalg_d_pure_solve_one
     module function stdlib_linalg_q_solve_one(a,b,overwrite_a,err) result(x)
         !> Input matrix a[n,n]
         real(qp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(qp), intent(in) :: b(:)     
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         real(qp), allocatable, target :: x(:)
     end function stdlib_linalg_q_solve_one
     pure module function stdlib_linalg_q_pure_solve_one(a,b) result(x)
         !> Input matrix a[n,n]
         real(qp), intent(in) :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(qp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(qp), allocatable, target :: x(:)
     end function stdlib_linalg_q_pure_solve_one
     module function stdlib_linalg_c_solve_one(a,b,overwrite_a,err) result(x)
         !> Input matrix a[n,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:)     
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(sp), allocatable, target :: x(:)
     end function stdlib_linalg_c_solve_one
     pure module function stdlib_linalg_c_pure_solve_one(a,b) result(x)
         !> Input matrix a[n,n]
         complex(sp), intent(in) :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(sp), allocatable, target :: x(:)
     end function stdlib_linalg_c_pure_solve_one
     module function stdlib_linalg_z_solve_one(a,b,overwrite_a,err) result(x)
         !> Input matrix a[n,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:)     
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(dp), allocatable, target :: x(:)
     end function stdlib_linalg_z_solve_one
     pure module function stdlib_linalg_z_pure_solve_one(a,b) result(x)
         !> Input matrix a[n,n]
         complex(dp), intent(in) :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(dp), allocatable, target :: x(:)
     end function stdlib_linalg_z_pure_solve_one
     module function stdlib_linalg_w_solve_one(a,b,overwrite_a,err) result(x)
         !> Input matrix a[n,n]
         complex(qp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(qp), intent(in) :: b(:)     
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(qp), allocatable, target :: x(:)
     end function stdlib_linalg_w_solve_one
     pure module function stdlib_linalg_w_pure_solve_one(a,b) result(x)
         !> Input matrix a[n,n]
         complex(qp), intent(in) :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(qp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(qp), allocatable, target :: x(:)
     end function stdlib_linalg_w_pure_solve_one
     module function stdlib_linalg_s_solve_many(a,b,overwrite_a,err) result(x)
         !> Input matrix a[n,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:,:)     
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         real(sp), allocatable, target :: x(:,:)
     end function stdlib_linalg_s_solve_many
     pure module function stdlib_linalg_s_pure_solve_many(a,b) result(x)
         !> Input matrix a[n,n]
         real(sp), intent(in) :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(sp), allocatable, target :: x(:,:)
     end function stdlib_linalg_s_pure_solve_many
     module function stdlib_linalg_d_solve_many(a,b,overwrite_a,err) result(x)
         !> Input matrix a[n,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:,:)     
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         real(dp), allocatable, target :: x(:,:)
     end function stdlib_linalg_d_solve_many
     pure module function stdlib_linalg_d_pure_solve_many(a,b) result(x)
         !> Input matrix a[n,n]
         real(dp), intent(in) :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(dp), allocatable, target :: x(:,:)
     end function stdlib_linalg_d_pure_solve_many
     module function stdlib_linalg_q_solve_many(a,b,overwrite_a,err) result(x)
         !> Input matrix a[n,n]
         real(qp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(qp), intent(in) :: b(:,:)     
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         real(qp), allocatable, target :: x(:,:)
     end function stdlib_linalg_q_solve_many
     pure module function stdlib_linalg_q_pure_solve_many(a,b) result(x)
         !> Input matrix a[n,n]
         real(qp), intent(in) :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(qp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(qp), allocatable, target :: x(:,:)
     end function stdlib_linalg_q_pure_solve_many
     module function stdlib_linalg_c_solve_many(a,b,overwrite_a,err) result(x)
         !> Input matrix a[n,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:,:)     
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(sp), allocatable, target :: x(:,:)
     end function stdlib_linalg_c_solve_many
     pure module function stdlib_linalg_c_pure_solve_many(a,b) result(x)
         !> Input matrix a[n,n]
         complex(sp), intent(in) :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(sp), allocatable, target :: x(:,:)
     end function stdlib_linalg_c_pure_solve_many
     module function stdlib_linalg_z_solve_many(a,b,overwrite_a,err) result(x)
         !> Input matrix a[n,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:,:)     
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(dp), allocatable, target :: x(:,:)
     end function stdlib_linalg_z_solve_many
     pure module function stdlib_linalg_z_pure_solve_many(a,b) result(x)
         !> Input matrix a[n,n]
         complex(dp), intent(in) :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(dp), allocatable, target :: x(:,:)
     end function stdlib_linalg_z_pure_solve_many
     module function stdlib_linalg_w_solve_many(a,b,overwrite_a,err) result(x)
         !> Input matrix a[n,n]
         complex(qp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(qp), intent(in) :: b(:,:)     
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(qp), allocatable, target :: x(:,:)
     end function stdlib_linalg_w_solve_many
     pure module function stdlib_linalg_w_pure_solve_many(a,b) result(x)
         !> Input matrix a[n,n]
         complex(qp), intent(in) :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(qp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(qp), allocatable, target :: x(:,:)
     end function stdlib_linalg_w_pure_solve_many
  end interface solve

  ! Solve linear system Ax = b using LU decomposition (subroutine interface).
  interface solve_lu
     !! version: experimental 
     !!
     !! Solves the linear system \( A \cdot x = b \) for the unknown vector \( x \) from a square matrix \( A \). 
     !! ([Specification](../page/specs/stdlib_linalg.html#solve-lu-solves-a-linear-matrix-equation-or-a-linear-system-of-equations-subroutine-interface))
     !!
     !!### Summary 
     !! Subroutine interface for solving a linear system using LU decomposition.
     !!
     !!### Description
     !! 
     !! This interface provides methods for computing the solution of a linear matrix system using
     !! a subroutine. Supported data types include `real` and `complex`. No assumption is made on the matrix 
     !! structure. Preallocated space for the solution vector `x` is user-provided, and it may be provided
     !! for the array of pivot indices, `pivot`. If all pre-allocated work spaces are provided, no internal 
     !! memory allocations take place when using this interface.     
     !! The function can solve simultaneously either one (from a 1-d right-hand-side vector `b(:)`) 
     !! or several (from a 2-d right-hand-side vector `b(:,:)`) systems.
     !! 
     !!@note The solution is based on LAPACK's generic LU decomposition based solvers `*GESV`.
     !!@note BLAS/LAPACK backends do not currently support extended precision (``xdp``).
     !!        
     pure module subroutine stdlib_linalg_s_solve_lu_one(a,b,x,pivot,overwrite_a,err)     
         !> Input matrix a[n,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]     
         real(sp), intent(inout), contiguous, target :: x(:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
     end subroutine stdlib_linalg_s_solve_lu_one
     pure module subroutine stdlib_linalg_d_solve_lu_one(a,b,x,pivot,overwrite_a,err)     
         !> Input matrix a[n,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]     
         real(dp), intent(inout), contiguous, target :: x(:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
     end subroutine stdlib_linalg_d_solve_lu_one
     pure module subroutine stdlib_linalg_q_solve_lu_one(a,b,x,pivot,overwrite_a,err)     
         !> Input matrix a[n,n]
         real(qp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(qp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]     
         real(qp), intent(inout), contiguous, target :: x(:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
     end subroutine stdlib_linalg_q_solve_lu_one
     pure module subroutine stdlib_linalg_c_solve_lu_one(a,b,x,pivot,overwrite_a,err)     
         !> Input matrix a[n,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]     
         complex(sp), intent(inout), contiguous, target :: x(:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
     end subroutine stdlib_linalg_c_solve_lu_one
     pure module subroutine stdlib_linalg_z_solve_lu_one(a,b,x,pivot,overwrite_a,err)     
         !> Input matrix a[n,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]     
         complex(dp), intent(inout), contiguous, target :: x(:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
     end subroutine stdlib_linalg_z_solve_lu_one
     pure module subroutine stdlib_linalg_w_solve_lu_one(a,b,x,pivot,overwrite_a,err)     
         !> Input matrix a[n,n]
         complex(qp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(qp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]     
         complex(qp), intent(inout), contiguous, target :: x(:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
     end subroutine stdlib_linalg_w_solve_lu_one
     pure module subroutine stdlib_linalg_s_solve_lu_many(a,b,x,pivot,overwrite_a,err)     
         !> Input matrix a[n,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]     
         real(sp), intent(inout), contiguous, target :: x(:,:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
     end subroutine stdlib_linalg_s_solve_lu_many
     pure module subroutine stdlib_linalg_d_solve_lu_many(a,b,x,pivot,overwrite_a,err)     
         !> Input matrix a[n,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]     
         real(dp), intent(inout), contiguous, target :: x(:,:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
     end subroutine stdlib_linalg_d_solve_lu_many
     pure module subroutine stdlib_linalg_q_solve_lu_many(a,b,x,pivot,overwrite_a,err)     
         !> Input matrix a[n,n]
         real(qp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(qp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]     
         real(qp), intent(inout), contiguous, target :: x(:,:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
     end subroutine stdlib_linalg_q_solve_lu_many
     pure module subroutine stdlib_linalg_c_solve_lu_many(a,b,x,pivot,overwrite_a,err)     
         !> Input matrix a[n,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]     
         complex(sp), intent(inout), contiguous, target :: x(:,:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
     end subroutine stdlib_linalg_c_solve_lu_many
     pure module subroutine stdlib_linalg_z_solve_lu_many(a,b,x,pivot,overwrite_a,err)     
         !> Input matrix a[n,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]     
         complex(dp), intent(inout), contiguous, target :: x(:,:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
     end subroutine stdlib_linalg_z_solve_lu_many
     pure module subroutine stdlib_linalg_w_solve_lu_many(a,b,x,pivot,overwrite_a,err)     
         !> Input matrix a[n,n]
         complex(qp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(qp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]     
         complex(qp), intent(inout), contiguous, target :: x(:,:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
     end subroutine stdlib_linalg_w_solve_lu_many
  end interface solve_lu     
     
  ! Least squares solution to system Ax=b, i.e. such that the 2-norm abs(b-Ax) is minimized.
  interface lstsq
    !! version: experimental 
    !!
    !! Computes the squares solution to system \( A \cdot x = b \). 
    !! ([Specification](../page/specs/stdlib_linalg.html#lstsq-computes-the-least-squares-solution-to-a-linear-matrix-equation))
    !! 
    !!### Summary 
    !! Interface for computing least squares, i.e. the 2-norm \( || (b-A \cdot x ||_2 \) minimizing solution.
    !!
    !!### Description
    !! 
    !! This interface provides methods for computing the least squares of a linear matrix system.
    !! Supported data types include `real` and `complex`.
    !! 
    !!@note The solution is based on LAPACK's singular value decomposition `*GELSD` methods.
    !!@note BLAS/LAPACK backends do not currently support extended precision (``xdp``).
    !! 
      module function stdlib_linalg_s_lstsq_one(a,b,cond,overwrite_a,rank,err) result(x)
         !> Input matrix a[n,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(sp), optional, intent(in) :: cond
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         real(sp), allocatable, target :: x(:)
      end function stdlib_linalg_s_lstsq_one
      module function stdlib_linalg_d_lstsq_one(a,b,cond,overwrite_a,rank,err) result(x)
         !> Input matrix a[n,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(dp), optional, intent(in) :: cond
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         real(dp), allocatable, target :: x(:)
      end function stdlib_linalg_d_lstsq_one
      module function stdlib_linalg_q_lstsq_one(a,b,cond,overwrite_a,rank,err) result(x)
         !> Input matrix a[n,n]
         real(qp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(qp), intent(in) :: b(:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(qp), optional, intent(in) :: cond
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         real(qp), allocatable, target :: x(:)
      end function stdlib_linalg_q_lstsq_one
      module function stdlib_linalg_c_lstsq_one(a,b,cond,overwrite_a,rank,err) result(x)
         !> Input matrix a[n,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(sp), optional, intent(in) :: cond
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(sp), allocatable, target :: x(:)
      end function stdlib_linalg_c_lstsq_one
      module function stdlib_linalg_z_lstsq_one(a,b,cond,overwrite_a,rank,err) result(x)
         !> Input matrix a[n,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(dp), optional, intent(in) :: cond
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(dp), allocatable, target :: x(:)
      end function stdlib_linalg_z_lstsq_one
      module function stdlib_linalg_w_lstsq_one(a,b,cond,overwrite_a,rank,err) result(x)
         !> Input matrix a[n,n]
         complex(qp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(qp), intent(in) :: b(:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(qp), optional, intent(in) :: cond
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(qp), allocatable, target :: x(:)
      end function stdlib_linalg_w_lstsq_one
      module function stdlib_linalg_s_lstsq_many(a,b,cond,overwrite_a,rank,err) result(x)
         !> Input matrix a[n,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:,:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(sp), optional, intent(in) :: cond
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         real(sp), allocatable, target :: x(:,:)
      end function stdlib_linalg_s_lstsq_many
      module function stdlib_linalg_d_lstsq_many(a,b,cond,overwrite_a,rank,err) result(x)
         !> Input matrix a[n,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:,:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(dp), optional, intent(in) :: cond
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         real(dp), allocatable, target :: x(:,:)
      end function stdlib_linalg_d_lstsq_many
      module function stdlib_linalg_q_lstsq_many(a,b,cond,overwrite_a,rank,err) result(x)
         !> Input matrix a[n,n]
         real(qp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(qp), intent(in) :: b(:,:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(qp), optional, intent(in) :: cond
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         real(qp), allocatable, target :: x(:,:)
      end function stdlib_linalg_q_lstsq_many
      module function stdlib_linalg_c_lstsq_many(a,b,cond,overwrite_a,rank,err) result(x)
         !> Input matrix a[n,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:,:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(sp), optional, intent(in) :: cond
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(sp), allocatable, target :: x(:,:)
      end function stdlib_linalg_c_lstsq_many
      module function stdlib_linalg_z_lstsq_many(a,b,cond,overwrite_a,rank,err) result(x)
         !> Input matrix a[n,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:,:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(dp), optional, intent(in) :: cond
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(dp), allocatable, target :: x(:,:)
      end function stdlib_linalg_z_lstsq_many
      module function stdlib_linalg_w_lstsq_many(a,b,cond,overwrite_a,rank,err) result(x)
         !> Input matrix a[n,n]
         complex(qp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(qp), intent(in) :: b(:,:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(qp), optional, intent(in) :: cond
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(qp), allocatable, target :: x(:,:)
      end function stdlib_linalg_w_lstsq_many
  end interface lstsq

  ! Least squares solution to system Ax=b, i.e. such that the 2-norm abs(b-Ax) is minimized.
  interface solve_lstsq
    !! version: experimental 
    !!
    !! Computes the squares solution to system \( A \cdot x = b \). 
    !! ([Specification](../page/specs/stdlib_linalg.html#solve-lstsq-compute-the-least-squares-solution-to-a-linear-matrix-equation-subroutine-interface))
    !! 
    !!### Summary 
    !! Subroutine interface for computing least squares, i.e. the 2-norm \( || (b-A \cdot x ||_2 \) minimizing solution.
    !!
    !!### Description
    !! 
    !! This interface provides methods for computing the least squares of a linear matrix system using 
    !! a subroutine. Supported data types include `real` and `complex`. If pre-allocated work spaces 
    !! are provided, no internal memory allocations take place when using this interface.
    !! 
    !!@note The solution is based on LAPACK's singular value decomposition `*GELSD` methods.
    !!@note BLAS/LAPACK backends do not currently support extended precision (``xdp``).
    !! 
      module subroutine stdlib_linalg_s_solve_lstsq_one(a,b,x,real_storage,int_storage,&
                        cond,singvals,overwrite_a,rank,err) 
         !> Input matrix a[n,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(sp), intent(inout), contiguous, target :: x(:)     
         !> [optional] real working storage space
         real(sp), optional, intent(inout), target :: real_storage(:)
         !> [optional] integer working storage space
         integer(ilp), optional, intent(inout), target :: int_storage(:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(sp), optional, intent(in) :: cond
         !> [optional] list of singular values [min(m,n)], in descending magnitude order, returned by the SVD
         real(sp), optional, intent(out), target :: singvals(:)                  
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_s_solve_lstsq_one
      module subroutine stdlib_linalg_d_solve_lstsq_one(a,b,x,real_storage,int_storage,&
                        cond,singvals,overwrite_a,rank,err) 
         !> Input matrix a[n,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(dp), intent(inout), contiguous, target :: x(:)     
         !> [optional] real working storage space
         real(dp), optional, intent(inout), target :: real_storage(:)
         !> [optional] integer working storage space
         integer(ilp), optional, intent(inout), target :: int_storage(:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(dp), optional, intent(in) :: cond
         !> [optional] list of singular values [min(m,n)], in descending magnitude order, returned by the SVD
         real(dp), optional, intent(out), target :: singvals(:)                  
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_d_solve_lstsq_one
      module subroutine stdlib_linalg_q_solve_lstsq_one(a,b,x,real_storage,int_storage,&
                        cond,singvals,overwrite_a,rank,err) 
         !> Input matrix a[n,n]
         real(qp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(qp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(qp), intent(inout), contiguous, target :: x(:)     
         !> [optional] real working storage space
         real(qp), optional, intent(inout), target :: real_storage(:)
         !> [optional] integer working storage space
         integer(ilp), optional, intent(inout), target :: int_storage(:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(qp), optional, intent(in) :: cond
         !> [optional] list of singular values [min(m,n)], in descending magnitude order, returned by the SVD
         real(qp), optional, intent(out), target :: singvals(:)                  
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_q_solve_lstsq_one
      module subroutine stdlib_linalg_c_solve_lstsq_one(a,b,x,real_storage,int_storage,&
                        cmpl_storage,cond,singvals,overwrite_a,rank,err) 
         !> Input matrix a[n,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(sp), intent(inout), contiguous, target :: x(:)     
         !> [optional] real working storage space
         real(sp), optional, intent(inout), target :: real_storage(:)
         !> [optional] integer working storage space
         integer(ilp), optional, intent(inout), target :: int_storage(:)
         !> [optional] complex working storage space
         complex(sp), optional, intent(inout), target :: cmpl_storage(:)                  
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(sp), optional, intent(in) :: cond
         !> [optional] list of singular values [min(m,n)], in descending magnitude order, returned by the SVD
         real(sp), optional, intent(out), target :: singvals(:)                  
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_c_solve_lstsq_one
      module subroutine stdlib_linalg_z_solve_lstsq_one(a,b,x,real_storage,int_storage,&
                        cmpl_storage,cond,singvals,overwrite_a,rank,err) 
         !> Input matrix a[n,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(dp), intent(inout), contiguous, target :: x(:)     
         !> [optional] real working storage space
         real(dp), optional, intent(inout), target :: real_storage(:)
         !> [optional] integer working storage space
         integer(ilp), optional, intent(inout), target :: int_storage(:)
         !> [optional] complex working storage space
         complex(dp), optional, intent(inout), target :: cmpl_storage(:)                  
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(dp), optional, intent(in) :: cond
         !> [optional] list of singular values [min(m,n)], in descending magnitude order, returned by the SVD
         real(dp), optional, intent(out), target :: singvals(:)                  
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_z_solve_lstsq_one
      module subroutine stdlib_linalg_w_solve_lstsq_one(a,b,x,real_storage,int_storage,&
                        cmpl_storage,cond,singvals,overwrite_a,rank,err) 
         !> Input matrix a[n,n]
         complex(qp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(qp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(qp), intent(inout), contiguous, target :: x(:)     
         !> [optional] real working storage space
         real(qp), optional, intent(inout), target :: real_storage(:)
         !> [optional] integer working storage space
         integer(ilp), optional, intent(inout), target :: int_storage(:)
         !> [optional] complex working storage space
         complex(qp), optional, intent(inout), target :: cmpl_storage(:)                  
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(qp), optional, intent(in) :: cond
         !> [optional] list of singular values [min(m,n)], in descending magnitude order, returned by the SVD
         real(qp), optional, intent(out), target :: singvals(:)                  
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_w_solve_lstsq_one
      module subroutine stdlib_linalg_s_solve_lstsq_many(a,b,x,real_storage,int_storage,&
                        cond,singvals,overwrite_a,rank,err) 
         !> Input matrix a[n,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(sp), intent(inout), contiguous, target :: x(:,:)     
         !> [optional] real working storage space
         real(sp), optional, intent(inout), target :: real_storage(:)
         !> [optional] integer working storage space
         integer(ilp), optional, intent(inout), target :: int_storage(:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(sp), optional, intent(in) :: cond
         !> [optional] list of singular values [min(m,n)], in descending magnitude order, returned by the SVD
         real(sp), optional, intent(out), target :: singvals(:)                  
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_s_solve_lstsq_many
      module subroutine stdlib_linalg_d_solve_lstsq_many(a,b,x,real_storage,int_storage,&
                        cond,singvals,overwrite_a,rank,err) 
         !> Input matrix a[n,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(dp), intent(inout), contiguous, target :: x(:,:)     
         !> [optional] real working storage space
         real(dp), optional, intent(inout), target :: real_storage(:)
         !> [optional] integer working storage space
         integer(ilp), optional, intent(inout), target :: int_storage(:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(dp), optional, intent(in) :: cond
         !> [optional] list of singular values [min(m,n)], in descending magnitude order, returned by the SVD
         real(dp), optional, intent(out), target :: singvals(:)                  
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_d_solve_lstsq_many
      module subroutine stdlib_linalg_q_solve_lstsq_many(a,b,x,real_storage,int_storage,&
                        cond,singvals,overwrite_a,rank,err) 
         !> Input matrix a[n,n]
         real(qp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(qp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(qp), intent(inout), contiguous, target :: x(:,:)     
         !> [optional] real working storage space
         real(qp), optional, intent(inout), target :: real_storage(:)
         !> [optional] integer working storage space
         integer(ilp), optional, intent(inout), target :: int_storage(:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(qp), optional, intent(in) :: cond
         !> [optional] list of singular values [min(m,n)], in descending magnitude order, returned by the SVD
         real(qp), optional, intent(out), target :: singvals(:)                  
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_q_solve_lstsq_many
      module subroutine stdlib_linalg_c_solve_lstsq_many(a,b,x,real_storage,int_storage,&
                        cmpl_storage,cond,singvals,overwrite_a,rank,err) 
         !> Input matrix a[n,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(sp), intent(inout), contiguous, target :: x(:,:)     
         !> [optional] real working storage space
         real(sp), optional, intent(inout), target :: real_storage(:)
         !> [optional] integer working storage space
         integer(ilp), optional, intent(inout), target :: int_storage(:)
         !> [optional] complex working storage space
         complex(sp), optional, intent(inout), target :: cmpl_storage(:)                  
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(sp), optional, intent(in) :: cond
         !> [optional] list of singular values [min(m,n)], in descending magnitude order, returned by the SVD
         real(sp), optional, intent(out), target :: singvals(:)                  
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_c_solve_lstsq_many
      module subroutine stdlib_linalg_z_solve_lstsq_many(a,b,x,real_storage,int_storage,&
                        cmpl_storage,cond,singvals,overwrite_a,rank,err) 
         !> Input matrix a[n,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(dp), intent(inout), contiguous, target :: x(:,:)     
         !> [optional] real working storage space
         real(dp), optional, intent(inout), target :: real_storage(:)
         !> [optional] integer working storage space
         integer(ilp), optional, intent(inout), target :: int_storage(:)
         !> [optional] complex working storage space
         complex(dp), optional, intent(inout), target :: cmpl_storage(:)                  
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(dp), optional, intent(in) :: cond
         !> [optional] list of singular values [min(m,n)], in descending magnitude order, returned by the SVD
         real(dp), optional, intent(out), target :: singvals(:)                  
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_z_solve_lstsq_many
      module subroutine stdlib_linalg_w_solve_lstsq_many(a,b,x,real_storage,int_storage,&
                        cmpl_storage,cond,singvals,overwrite_a,rank,err) 
         !> Input matrix a[n,n]
         complex(qp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(qp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(qp), intent(inout), contiguous, target :: x(:,:)     
         !> [optional] real working storage space
         real(qp), optional, intent(inout), target :: real_storage(:)
         !> [optional] integer working storage space
         integer(ilp), optional, intent(inout), target :: int_storage(:)
         !> [optional] complex working storage space
         complex(qp), optional, intent(inout), target :: cmpl_storage(:)                  
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(qp), optional, intent(in) :: cond
         !> [optional] list of singular values [min(m,n)], in descending magnitude order, returned by the SVD
         real(qp), optional, intent(out), target :: singvals(:)                  
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_w_solve_lstsq_many
  end interface solve_lstsq

  ! Return the working array space required by the least squares solver
  interface lstsq_space
    !! version: experimental 
    !!
    !! Computes the integer, real [, complex] working space required by the least-squares solver
    !! ([Specification](../page/specs/stdlib_linalg.html#lstsq-space-compute-internal-working-space-requirements-for-the-least-squares-solver))
    !! 
    !!### Description
    !! 
    !! This interface provides sizes of integer, real [, complex] working spaces required by the 
    !! least-squares solver. These sizes can be used to pre-allocated working arrays in case several 
    !! repeated least-squares solutions to a same system are sought. If pre-allocated working arrays 
    !! are provided, no internal allocations will take place.
    !! 
      pure module subroutine stdlib_linalg_s_lstsq_space_one(a,b,lrwork,liwork)
         !> Input matrix a[m,n]
         real(sp), intent(in), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:)
         !> Size of the working space arrays                
         integer(ilp), intent(out) :: lrwork,liwork         
      end subroutine stdlib_linalg_s_lstsq_space_one
      pure module subroutine stdlib_linalg_d_lstsq_space_one(a,b,lrwork,liwork)
         !> Input matrix a[m,n]
         real(dp), intent(in), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:)
         !> Size of the working space arrays                
         integer(ilp), intent(out) :: lrwork,liwork         
      end subroutine stdlib_linalg_d_lstsq_space_one
      pure module subroutine stdlib_linalg_q_lstsq_space_one(a,b,lrwork,liwork)
         !> Input matrix a[m,n]
         real(qp), intent(in), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(qp), intent(in) :: b(:)
         !> Size of the working space arrays                
         integer(ilp), intent(out) :: lrwork,liwork         
      end subroutine stdlib_linalg_q_lstsq_space_one
      pure module subroutine stdlib_linalg_c_lstsq_space_one(a,b,lrwork,liwork,lcwork)
         !> Input matrix a[m,n]
         complex(sp), intent(in), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:)
         !> Size of the working space arrays                
         integer(ilp), intent(out) :: lrwork,liwork,lcwork         
      end subroutine stdlib_linalg_c_lstsq_space_one
      pure module subroutine stdlib_linalg_z_lstsq_space_one(a,b,lrwork,liwork,lcwork)
         !> Input matrix a[m,n]
         complex(dp), intent(in), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:)
         !> Size of the working space arrays                
         integer(ilp), intent(out) :: lrwork,liwork,lcwork         
      end subroutine stdlib_linalg_z_lstsq_space_one
      pure module subroutine stdlib_linalg_w_lstsq_space_one(a,b,lrwork,liwork,lcwork)
         !> Input matrix a[m,n]
         complex(qp), intent(in), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(qp), intent(in) :: b(:)
         !> Size of the working space arrays                
         integer(ilp), intent(out) :: lrwork,liwork,lcwork         
      end subroutine stdlib_linalg_w_lstsq_space_one
      pure module subroutine stdlib_linalg_s_lstsq_space_many(a,b,lrwork,liwork)
         !> Input matrix a[m,n]
         real(sp), intent(in), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:,:)
         !> Size of the working space arrays                
         integer(ilp), intent(out) :: lrwork,liwork         
      end subroutine stdlib_linalg_s_lstsq_space_many
      pure module subroutine stdlib_linalg_d_lstsq_space_many(a,b,lrwork,liwork)
         !> Input matrix a[m,n]
         real(dp), intent(in), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:,:)
         !> Size of the working space arrays                
         integer(ilp), intent(out) :: lrwork,liwork         
      end subroutine stdlib_linalg_d_lstsq_space_many
      pure module subroutine stdlib_linalg_q_lstsq_space_many(a,b,lrwork,liwork)
         !> Input matrix a[m,n]
         real(qp), intent(in), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(qp), intent(in) :: b(:,:)
         !> Size of the working space arrays                
         integer(ilp), intent(out) :: lrwork,liwork         
      end subroutine stdlib_linalg_q_lstsq_space_many
      pure module subroutine stdlib_linalg_c_lstsq_space_many(a,b,lrwork,liwork,lcwork)
         !> Input matrix a[m,n]
         complex(sp), intent(in), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:,:)
         !> Size of the working space arrays                
         integer(ilp), intent(out) :: lrwork,liwork,lcwork         
      end subroutine stdlib_linalg_c_lstsq_space_many
      pure module subroutine stdlib_linalg_z_lstsq_space_many(a,b,lrwork,liwork,lcwork)
         !> Input matrix a[m,n]
         complex(dp), intent(in), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:,:)
         !> Size of the working space arrays                
         integer(ilp), intent(out) :: lrwork,liwork,lcwork         
      end subroutine stdlib_linalg_z_lstsq_space_many
      pure module subroutine stdlib_linalg_w_lstsq_space_many(a,b,lrwork,liwork,lcwork)
         !> Input matrix a[m,n]
         complex(qp), intent(in), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(qp), intent(in) :: b(:,:)
         !> Size of the working space arrays                
         integer(ilp), intent(out) :: lrwork,liwork,lcwork         
      end subroutine stdlib_linalg_w_lstsq_space_many
  end interface lstsq_space

  interface det
    !! version: experimental 
    !!
    !! Computes the determinant of a square matrix
    !! ([Specification](../page/specs/stdlib_linalg.html#det-computes-the-determinant-of-a-square-matrix))
    !! 
    !!### Summary 
    !! Interface for computing matrix determinant.
    !!
    !!### Description
    !! 
    !! This interface provides methods for computing the determinant of a matrix.
    !! Supported data types include `real` and `complex`.
    !! 
    !!@note The provided functions are intended for square matrices only.          
    !!@note BLAS/LAPACK backends do not currently support extended precision (``xdp``).
    !! 
    !!### Example
    !!
    !!```fortran
    !!
    !!    real(sp) :: a(3,3), d
    !!    type(linalg_state_type) :: state  
    !!    a = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])
    !!
    !!    ! ...
    !!    d = det(a,err=state)
    !!    if (state%ok()) then 
    !!       print *, 'Success! det=',d
    !!    else
    !!       print *, state%print()
    !!    endif
    !!    ! ...
    !!```
    !!     
    module procedure stdlib_linalg_rspdeterminant
    module procedure stdlib_linalg_pure_rspdeterminant
    module procedure stdlib_linalg_rdpdeterminant
    module procedure stdlib_linalg_pure_rdpdeterminant
    module procedure stdlib_linalg_rqpdeterminant
    module procedure stdlib_linalg_pure_rqpdeterminant
    module procedure stdlib_linalg_cspdeterminant
    module procedure stdlib_linalg_pure_cspdeterminant
    module procedure stdlib_linalg_cdpdeterminant
    module procedure stdlib_linalg_pure_cdpdeterminant
    module procedure stdlib_linalg_cqpdeterminant
    module procedure stdlib_linalg_pure_cqpdeterminant
  end interface det

  interface operator(.det.)
    !! version: experimental 
    !!
    !! Determinant operator of a square matrix
    !! ([Specification](../page/specs/stdlib_linalg.html#det-determinant-operator-of-a-square-matrix))
    !!
    !!### Summary
    !! Pure operator interface for computing matrix determinant.
    !!
    !!### Description
    !! 
    !! This pure operator interface provides a convenient way to compute the determinant of a matrix.
    !! Supported data types include real and complex.
    !!
    !!@note The provided functions are intended for square matrices.
    !!@note BLAS/LAPACK backends do not currently support extended precision (``xdp``).
    !!
    !!### Example
    !!
    !!```fortran
    !!
    !!    ! ...
    !!    real(sp) :: matrix(3,3), d
    !!    matrix = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])
    !!    d = .det.matrix
    !!    ! ...
    !! 
    !!```
    !     
    module procedure stdlib_linalg_pure_rspdeterminant
    module procedure stdlib_linalg_pure_rdpdeterminant
    module procedure stdlib_linalg_pure_rqpdeterminant
    module procedure stdlib_linalg_pure_cspdeterminant
    module procedure stdlib_linalg_pure_cdpdeterminant
    module procedure stdlib_linalg_pure_cqpdeterminant
  end interface operator(.det.)

  interface
    module function stdlib_linalg_rspdeterminant(a,overwrite_a,err) result(det)
        !> Input matrix a[m,n]
        real(sp), intent(inout), target :: a(:,:)
        !> [optional] Can A data be overwritten and destroyed?
        logical(lk), optional, intent(in) :: overwrite_a
        !> State return flag. 
        type(linalg_state_type), intent(out) :: err
        !> Matrix determinant
        real(sp) :: det
    end function stdlib_linalg_rspdeterminant
    pure module function stdlib_linalg_pure_rspdeterminant(a) result(det)
        !> Input matrix a[m,n]
        real(sp), intent(in) :: a(:,:)
        !> Matrix determinant
        real(sp) :: det                
    end function stdlib_linalg_pure_rspdeterminant
    module function stdlib_linalg_rdpdeterminant(a,overwrite_a,err) result(det)
        !> Input matrix a[m,n]
        real(dp), intent(inout), target :: a(:,:)
        !> [optional] Can A data be overwritten and destroyed?
        logical(lk), optional, intent(in) :: overwrite_a
        !> State return flag. 
        type(linalg_state_type), intent(out) :: err
        !> Matrix determinant
        real(dp) :: det
    end function stdlib_linalg_rdpdeterminant
    pure module function stdlib_linalg_pure_rdpdeterminant(a) result(det)
        !> Input matrix a[m,n]
        real(dp), intent(in) :: a(:,:)
        !> Matrix determinant
        real(dp) :: det                
    end function stdlib_linalg_pure_rdpdeterminant
    module function stdlib_linalg_rqpdeterminant(a,overwrite_a,err) result(det)
        !> Input matrix a[m,n]
        real(qp), intent(inout), target :: a(:,:)
        !> [optional] Can A data be overwritten and destroyed?
        logical(lk), optional, intent(in) :: overwrite_a
        !> State return flag. 
        type(linalg_state_type), intent(out) :: err
        !> Matrix determinant
        real(qp) :: det
    end function stdlib_linalg_rqpdeterminant
    pure module function stdlib_linalg_pure_rqpdeterminant(a) result(det)
        !> Input matrix a[m,n]
        real(qp), intent(in) :: a(:,:)
        !> Matrix determinant
        real(qp) :: det                
    end function stdlib_linalg_pure_rqpdeterminant
    module function stdlib_linalg_cspdeterminant(a,overwrite_a,err) result(det)
        !> Input matrix a[m,n]
        complex(sp), intent(inout), target :: a(:,:)
        !> [optional] Can A data be overwritten and destroyed?
        logical(lk), optional, intent(in) :: overwrite_a
        !> State return flag. 
        type(linalg_state_type), intent(out) :: err
        !> Matrix determinant
        complex(sp) :: det
    end function stdlib_linalg_cspdeterminant
    pure module function stdlib_linalg_pure_cspdeterminant(a) result(det)
        !> Input matrix a[m,n]
        complex(sp), intent(in) :: a(:,:)
        !> Matrix determinant
        complex(sp) :: det                
    end function stdlib_linalg_pure_cspdeterminant
    module function stdlib_linalg_cdpdeterminant(a,overwrite_a,err) result(det)
        !> Input matrix a[m,n]
        complex(dp), intent(inout), target :: a(:,:)
        !> [optional] Can A data be overwritten and destroyed?
        logical(lk), optional, intent(in) :: overwrite_a
        !> State return flag. 
        type(linalg_state_type), intent(out) :: err
        !> Matrix determinant
        complex(dp) :: det
    end function stdlib_linalg_cdpdeterminant
    pure module function stdlib_linalg_pure_cdpdeterminant(a) result(det)
        !> Input matrix a[m,n]
        complex(dp), intent(in) :: a(:,:)
        !> Matrix determinant
        complex(dp) :: det                
    end function stdlib_linalg_pure_cdpdeterminant
    module function stdlib_linalg_cqpdeterminant(a,overwrite_a,err) result(det)
        !> Input matrix a[m,n]
        complex(qp), intent(inout), target :: a(:,:)
        !> [optional] Can A data be overwritten and destroyed?
        logical(lk), optional, intent(in) :: overwrite_a
        !> State return flag. 
        type(linalg_state_type), intent(out) :: err
        !> Matrix determinant
        complex(qp) :: det
    end function stdlib_linalg_cqpdeterminant
    pure module function stdlib_linalg_pure_cqpdeterminant(a) result(det)
        !> Input matrix a[m,n]
        complex(qp), intent(in) :: a(:,:)
        !> Matrix determinant
        complex(qp) :: det                
    end function stdlib_linalg_pure_cqpdeterminant
  end interface  

  ! Singular value decomposition  
  interface svd 
    !! version: experimental 
    !!
    !! Computes the singular value decomposition of a `real` or `complex` 2d matrix.
    !! ([Specification](../page/specs/stdlib_linalg.html#svd-compute-the-singular-value-decomposition-of-a-rank-2-array-matrix))
    !! 
    !!### Summary 
    !! Interface for computing the singular value decomposition of a `real` or `complex` 2d matrix.
    !!
    !!### Description
    !! 
    !! This interface provides methods for computing the singular value decomposition of a matrix.
    !! Supported data types include `real` and `complex`. The subroutine returns a `real` array of 
    !! singular values, and optionally, left- and right- singular vector matrices, `U` and `V`. 
    !! For a matrix `A` with size [m,n], full matrix storage for `U` and `V` should be [m,m] and [n,n]. 
    !! It is possible to use partial storage [m,k] and [k,n], `k=min(m,n)`, choosing `full_matrices=.false.`.
    !! 
    !!@note The solution is based on LAPACK's singular value decomposition `*GESDD` methods.
    !!@note BLAS/LAPACK backends do not currently support extended precision (``xdp``).    
    !! 
    !!### Example
    !!
    !!```fortran
    !!    real(sp) :: a(2,3), s(2), u(2,2), vt(3,3) 
    !!    a = reshape([3,2, 2,3, 2,-2],[2,3])
    !!
    !!    call svd(A,s,u,v)
    !!    print *, 'singular values = ',s
    !!```
    !!         
    module subroutine stdlib_linalg_svd_s(a,s,u,vt,overwrite_a,full_matrices,err)
     !!### Summary
     !! Compute singular value decomposition of a matrix \( A = U \cdot S \cdot \V^T \)
     !!
     !!### Description
     !!
     !! This function computes the singular value decomposition of a `real` or `complex` matrix \( A \), 
     !! and returns the array of singular values, and optionally the left matrix \( U \) containing the 
     !! left unitary singular vectors, and the right matrix \( V^T \), containing the right unitary 
     !! singular vectors.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: s Output `real` array of size [min(m,n)] returning a list of singular values.
     !! param: u [optional] Output left singular matrix of size [m,m] or [m,min(m,n)] (.not.full_matrices). Contains singular vectors as columns.
     !! param: vt [optional] Output right singular matrix of size [n,n] or [min(m,n),n] (.not.full_matrices). Contains singular vectors as rows.     
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: full_matrices [optional] If `.true.` (default), matrices \( U \) and \( V^T \) have size [m,m], [n,n]. Otherwise, they are [m,k], [k,n] with `k=min(m,n)`.
     !! param: err [optional] State return flag.      
     !!            
         !> Input matrix A[m,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Array of singular values
         real(sp), intent(out) :: s(:)
         !> The columns of U contain the left singular vectors 
         real(sp), optional, intent(out), target :: u(:,:)
         !> The rows of V^T contain the right singular vectors
         real(sp), optional, intent(out), target :: vt(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] full matrices have shape(u)==[m,m], shape(vh)==[n,n] (default); otherwise
         !> they are shape(u)==[m,k] and shape(vh)==[k,n] with k=min(m,n)
         logical(lk), optional, intent(in) :: full_matrices
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_svd_s
    module subroutine stdlib_linalg_svd_d(a,s,u,vt,overwrite_a,full_matrices,err)
     !!### Summary
     !! Compute singular value decomposition of a matrix \( A = U \cdot S \cdot \V^T \)
     !!
     !!### Description
     !!
     !! This function computes the singular value decomposition of a `real` or `complex` matrix \( A \), 
     !! and returns the array of singular values, and optionally the left matrix \( U \) containing the 
     !! left unitary singular vectors, and the right matrix \( V^T \), containing the right unitary 
     !! singular vectors.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: s Output `real` array of size [min(m,n)] returning a list of singular values.
     !! param: u [optional] Output left singular matrix of size [m,m] or [m,min(m,n)] (.not.full_matrices). Contains singular vectors as columns.
     !! param: vt [optional] Output right singular matrix of size [n,n] or [min(m,n),n] (.not.full_matrices). Contains singular vectors as rows.     
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: full_matrices [optional] If `.true.` (default), matrices \( U \) and \( V^T \) have size [m,m], [n,n]. Otherwise, they are [m,k], [k,n] with `k=min(m,n)`.
     !! param: err [optional] State return flag.      
     !!            
         !> Input matrix A[m,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Array of singular values
         real(dp), intent(out) :: s(:)
         !> The columns of U contain the left singular vectors 
         real(dp), optional, intent(out), target :: u(:,:)
         !> The rows of V^T contain the right singular vectors
         real(dp), optional, intent(out), target :: vt(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] full matrices have shape(u)==[m,m], shape(vh)==[n,n] (default); otherwise
         !> they are shape(u)==[m,k] and shape(vh)==[k,n] with k=min(m,n)
         logical(lk), optional, intent(in) :: full_matrices
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_svd_d
    module subroutine stdlib_linalg_svd_q(a,s,u,vt,overwrite_a,full_matrices,err)
     !!### Summary
     !! Compute singular value decomposition of a matrix \( A = U \cdot S \cdot \V^T \)
     !!
     !!### Description
     !!
     !! This function computes the singular value decomposition of a `real` or `complex` matrix \( A \), 
     !! and returns the array of singular values, and optionally the left matrix \( U \) containing the 
     !! left unitary singular vectors, and the right matrix \( V^T \), containing the right unitary 
     !! singular vectors.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: s Output `real` array of size [min(m,n)] returning a list of singular values.
     !! param: u [optional] Output left singular matrix of size [m,m] or [m,min(m,n)] (.not.full_matrices). Contains singular vectors as columns.
     !! param: vt [optional] Output right singular matrix of size [n,n] or [min(m,n),n] (.not.full_matrices). Contains singular vectors as rows.     
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: full_matrices [optional] If `.true.` (default), matrices \( U \) and \( V^T \) have size [m,m], [n,n]. Otherwise, they are [m,k], [k,n] with `k=min(m,n)`.
     !! param: err [optional] State return flag.      
     !!            
         !> Input matrix A[m,n]
         real(qp), intent(inout), target :: a(:,:)
         !> Array of singular values
         real(qp), intent(out) :: s(:)
         !> The columns of U contain the left singular vectors 
         real(qp), optional, intent(out), target :: u(:,:)
         !> The rows of V^T contain the right singular vectors
         real(qp), optional, intent(out), target :: vt(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] full matrices have shape(u)==[m,m], shape(vh)==[n,n] (default); otherwise
         !> they are shape(u)==[m,k] and shape(vh)==[k,n] with k=min(m,n)
         logical(lk), optional, intent(in) :: full_matrices
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_svd_q
    module subroutine stdlib_linalg_svd_c(a,s,u,vt,overwrite_a,full_matrices,err)
     !!### Summary
     !! Compute singular value decomposition of a matrix \( A = U \cdot S \cdot \V^T \)
     !!
     !!### Description
     !!
     !! This function computes the singular value decomposition of a `real` or `complex` matrix \( A \), 
     !! and returns the array of singular values, and optionally the left matrix \( U \) containing the 
     !! left unitary singular vectors, and the right matrix \( V^T \), containing the right unitary 
     !! singular vectors.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: s Output `real` array of size [min(m,n)] returning a list of singular values.
     !! param: u [optional] Output left singular matrix of size [m,m] or [m,min(m,n)] (.not.full_matrices). Contains singular vectors as columns.
     !! param: vt [optional] Output right singular matrix of size [n,n] or [min(m,n),n] (.not.full_matrices). Contains singular vectors as rows.     
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: full_matrices [optional] If `.true.` (default), matrices \( U \) and \( V^T \) have size [m,m], [n,n]. Otherwise, they are [m,k], [k,n] with `k=min(m,n)`.
     !! param: err [optional] State return flag.      
     !!            
         !> Input matrix A[m,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Array of singular values
         real(sp), intent(out) :: s(:)
         !> The columns of U contain the left singular vectors 
         complex(sp), optional, intent(out), target :: u(:,:)
         !> The rows of V^T contain the right singular vectors
         complex(sp), optional, intent(out), target :: vt(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] full matrices have shape(u)==[m,m], shape(vh)==[n,n] (default); otherwise
         !> they are shape(u)==[m,k] and shape(vh)==[k,n] with k=min(m,n)
         logical(lk), optional, intent(in) :: full_matrices
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_svd_c
    module subroutine stdlib_linalg_svd_z(a,s,u,vt,overwrite_a,full_matrices,err)
     !!### Summary
     !! Compute singular value decomposition of a matrix \( A = U \cdot S \cdot \V^T \)
     !!
     !!### Description
     !!
     !! This function computes the singular value decomposition of a `real` or `complex` matrix \( A \), 
     !! and returns the array of singular values, and optionally the left matrix \( U \) containing the 
     !! left unitary singular vectors, and the right matrix \( V^T \), containing the right unitary 
     !! singular vectors.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: s Output `real` array of size [min(m,n)] returning a list of singular values.
     !! param: u [optional] Output left singular matrix of size [m,m] or [m,min(m,n)] (.not.full_matrices). Contains singular vectors as columns.
     !! param: vt [optional] Output right singular matrix of size [n,n] or [min(m,n),n] (.not.full_matrices). Contains singular vectors as rows.     
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: full_matrices [optional] If `.true.` (default), matrices \( U \) and \( V^T \) have size [m,m], [n,n]. Otherwise, they are [m,k], [k,n] with `k=min(m,n)`.
     !! param: err [optional] State return flag.      
     !!            
         !> Input matrix A[m,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Array of singular values
         real(dp), intent(out) :: s(:)
         !> The columns of U contain the left singular vectors 
         complex(dp), optional, intent(out), target :: u(:,:)
         !> The rows of V^T contain the right singular vectors
         complex(dp), optional, intent(out), target :: vt(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] full matrices have shape(u)==[m,m], shape(vh)==[n,n] (default); otherwise
         !> they are shape(u)==[m,k] and shape(vh)==[k,n] with k=min(m,n)
         logical(lk), optional, intent(in) :: full_matrices
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_svd_z
    module subroutine stdlib_linalg_svd_w(a,s,u,vt,overwrite_a,full_matrices,err)
     !!### Summary
     !! Compute singular value decomposition of a matrix \( A = U \cdot S \cdot \V^T \)
     !!
     !!### Description
     !!
     !! This function computes the singular value decomposition of a `real` or `complex` matrix \( A \), 
     !! and returns the array of singular values, and optionally the left matrix \( U \) containing the 
     !! left unitary singular vectors, and the right matrix \( V^T \), containing the right unitary 
     !! singular vectors.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: s Output `real` array of size [min(m,n)] returning a list of singular values.
     !! param: u [optional] Output left singular matrix of size [m,m] or [m,min(m,n)] (.not.full_matrices). Contains singular vectors as columns.
     !! param: vt [optional] Output right singular matrix of size [n,n] or [min(m,n),n] (.not.full_matrices). Contains singular vectors as rows.     
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: full_matrices [optional] If `.true.` (default), matrices \( U \) and \( V^T \) have size [m,m], [n,n]. Otherwise, they are [m,k], [k,n] with `k=min(m,n)`.
     !! param: err [optional] State return flag.      
     !!            
         !> Input matrix A[m,n]
         complex(qp), intent(inout), target :: a(:,:)
         !> Array of singular values
         real(qp), intent(out) :: s(:)
         !> The columns of U contain the left singular vectors 
         complex(qp), optional, intent(out), target :: u(:,:)
         !> The rows of V^T contain the right singular vectors
         complex(qp), optional, intent(out), target :: vt(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] full matrices have shape(u)==[m,m], shape(vh)==[n,n] (default); otherwise
         !> they are shape(u)==[m,k] and shape(vh)==[k,n] with k=min(m,n)
         logical(lk), optional, intent(in) :: full_matrices
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_svd_w
  end interface svd

  ! Singular values
  interface svdvals
    !! version: experimental 
    !!
    !! Computes the singular values of a `real` or `complex` 2d matrix.
    !! ([Specification](../page/specs/stdlib_linalg.html#svdvals-compute-the-singular-values-of-a-rank-2-array-matrix))
    !! 
    !!### Summary 
    !! 
    !! Function interface for computing the array of singular values from the singular value decomposition 
    !! of a `real` or `complex` 2d matrix.
    !!
    !!### Description
    !! 
    !! This interface provides methods for computing the singular values a 2d matrix.
    !! Supported data types include `real` and `complex`. The function returns a `real` array of 
    !! singular values, with size [min(m,n)]. 
    !! 
    !!@note The solution is based on LAPACK's singular value decomposition `*GESDD` methods.
    !!@note BLAS/LAPACK backends do not currently support extended precision (``xdp``).    
    !! 
    !!### Example
    !!
    !!```fortran
    !!    real(sp) :: a(2,3), s(2)
    !!    a = reshape([3,2, 2,3, 2,-2],[2,3])
    !!
    !!    s = svdvals(A)
    !!    print *, 'singular values = ',s
    !!```
    !!   
    module function stdlib_linalg_svdvals_s(a,err) result(s)
     !!### Summary
     !! Compute singular values \(S \) from the singular-value decomposition of a matrix \( A = U \cdot S \cdot \V^T \).
     !!
     !!### Description
     !!
     !! This function returns the array of singular values from the singular value decomposition of a `real` 
     !! or `complex` matrix \( A = U \cdot S \cdot V^T \).
     !!
     !! param: a Input matrix of size [m,n].
     !! param: err [optional] State return flag.      
     !!
     !!### Return value
     !! 
     !! param: s `real` array of size [min(m,n)] returning a list of singular values.
     !!           
         !> Input matrix A[m,n]
         real(sp), intent(in), target :: a(:,:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Array of singular values
         real(sp), allocatable :: s(:)
      end function stdlib_linalg_svdvals_s
    module function stdlib_linalg_svdvals_d(a,err) result(s)
     !!### Summary
     !! Compute singular values \(S \) from the singular-value decomposition of a matrix \( A = U \cdot S \cdot \V^T \).
     !!
     !!### Description
     !!
     !! This function returns the array of singular values from the singular value decomposition of a `real` 
     !! or `complex` matrix \( A = U \cdot S \cdot V^T \).
     !!
     !! param: a Input matrix of size [m,n].
     !! param: err [optional] State return flag.      
     !!
     !!### Return value
     !! 
     !! param: s `real` array of size [min(m,n)] returning a list of singular values.
     !!           
         !> Input matrix A[m,n]
         real(dp), intent(in), target :: a(:,:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Array of singular values
         real(dp), allocatable :: s(:)
      end function stdlib_linalg_svdvals_d
    module function stdlib_linalg_svdvals_q(a,err) result(s)
     !!### Summary
     !! Compute singular values \(S \) from the singular-value decomposition of a matrix \( A = U \cdot S \cdot \V^T \).
     !!
     !!### Description
     !!
     !! This function returns the array of singular values from the singular value decomposition of a `real` 
     !! or `complex` matrix \( A = U \cdot S \cdot V^T \).
     !!
     !! param: a Input matrix of size [m,n].
     !! param: err [optional] State return flag.      
     !!
     !!### Return value
     !! 
     !! param: s `real` array of size [min(m,n)] returning a list of singular values.
     !!           
         !> Input matrix A[m,n]
         real(qp), intent(in), target :: a(:,:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Array of singular values
         real(qp), allocatable :: s(:)
      end function stdlib_linalg_svdvals_q
    module function stdlib_linalg_svdvals_c(a,err) result(s)
     !!### Summary
     !! Compute singular values \(S \) from the singular-value decomposition of a matrix \( A = U \cdot S \cdot \V^T \).
     !!
     !!### Description
     !!
     !! This function returns the array of singular values from the singular value decomposition of a `real` 
     !! or `complex` matrix \( A = U \cdot S \cdot V^T \).
     !!
     !! param: a Input matrix of size [m,n].
     !! param: err [optional] State return flag.      
     !!
     !!### Return value
     !! 
     !! param: s `real` array of size [min(m,n)] returning a list of singular values.
     !!           
         !> Input matrix A[m,n]
         complex(sp), intent(in), target :: a(:,:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Array of singular values
         real(sp), allocatable :: s(:)
      end function stdlib_linalg_svdvals_c
    module function stdlib_linalg_svdvals_z(a,err) result(s)
     !!### Summary
     !! Compute singular values \(S \) from the singular-value decomposition of a matrix \( A = U \cdot S \cdot \V^T \).
     !!
     !!### Description
     !!
     !! This function returns the array of singular values from the singular value decomposition of a `real` 
     !! or `complex` matrix \( A = U \cdot S \cdot V^T \).
     !!
     !! param: a Input matrix of size [m,n].
     !! param: err [optional] State return flag.      
     !!
     !!### Return value
     !! 
     !! param: s `real` array of size [min(m,n)] returning a list of singular values.
     !!           
         !> Input matrix A[m,n]
         complex(dp), intent(in), target :: a(:,:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Array of singular values
         real(dp), allocatable :: s(:)
      end function stdlib_linalg_svdvals_z
    module function stdlib_linalg_svdvals_w(a,err) result(s)
     !!### Summary
     !! Compute singular values \(S \) from the singular-value decomposition of a matrix \( A = U \cdot S \cdot \V^T \).
     !!
     !!### Description
     !!
     !! This function returns the array of singular values from the singular value decomposition of a `real` 
     !! or `complex` matrix \( A = U \cdot S \cdot V^T \).
     !!
     !! param: a Input matrix of size [m,n].
     !! param: err [optional] State return flag.      
     !!
     !!### Return value
     !! 
     !! param: s `real` array of size [min(m,n)] returning a list of singular values.
     !!           
         !> Input matrix A[m,n]
         complex(qp), intent(in), target :: a(:,:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Array of singular values
         real(qp), allocatable :: s(:)
      end function stdlib_linalg_svdvals_w
  end interface svdvals  

contains


    !> Version: experimental
    !>
    !> Constructs the identity matrix.
    !> ([Specification](../page/specs/stdlib_linalg.html#eye-construct-the-identity-matrix))
    pure function eye(dim1, dim2) result(result)

        integer, intent(in) :: dim1
        integer, intent(in), optional :: dim2
        integer(int8), allocatable :: result(:, :)

        integer :: dim2_
        integer :: i

        dim2_ = optval(dim2, dim1)
        allocate(result(dim1, dim2_))
        
        result = 0_int8
        do i = 1, min(dim1, dim2_)
            result(i, i) = 1_int8
        end do

    end function eye

      function trace_rsp(A) result(res)
        real(sp), intent(in) :: A(:,:)
        real(sp) :: res
        integer :: i
        res = 0
        do i = 1, minval(shape(A))
          res = res + A(i,i)
        end do
      end function trace_rsp
      function trace_rdp(A) result(res)
        real(dp), intent(in) :: A(:,:)
        real(dp) :: res
        integer :: i
        res = 0
        do i = 1, minval(shape(A))
          res = res + A(i,i)
        end do
      end function trace_rdp
      function trace_rxdp(A) result(res)
        real(xdp), intent(in) :: A(:,:)
        real(xdp) :: res
        integer :: i
        res = 0
        do i = 1, minval(shape(A))
          res = res + A(i,i)
        end do
      end function trace_rxdp
      function trace_rqp(A) result(res)
        real(qp), intent(in) :: A(:,:)
        real(qp) :: res
        integer :: i
        res = 0
        do i = 1, minval(shape(A))
          res = res + A(i,i)
        end do
      end function trace_rqp
      function trace_csp(A) result(res)
        complex(sp), intent(in) :: A(:,:)
        complex(sp) :: res
        integer :: i
        res = 0
        do i = 1, minval(shape(A))
          res = res + A(i,i)
        end do
      end function trace_csp
      function trace_cdp(A) result(res)
        complex(dp), intent(in) :: A(:,:)
        complex(dp) :: res
        integer :: i
        res = 0
        do i = 1, minval(shape(A))
          res = res + A(i,i)
        end do
      end function trace_cdp
      function trace_cxdp(A) result(res)
        complex(xdp), intent(in) :: A(:,:)
        complex(xdp) :: res
        integer :: i
        res = 0
        do i = 1, minval(shape(A))
          res = res + A(i,i)
        end do
      end function trace_cxdp
      function trace_cqp(A) result(res)
        complex(qp), intent(in) :: A(:,:)
        complex(qp) :: res
        integer :: i
        res = 0
        do i = 1, minval(shape(A))
          res = res + A(i,i)
        end do
      end function trace_cqp
      function trace_iint8(A) result(res)
        integer(int8), intent(in) :: A(:,:)
        integer(int8) :: res
        integer :: i
        res = 0
        do i = 1, minval(shape(A))
          res = res + A(i,i)
        end do
      end function trace_iint8
      function trace_iint16(A) result(res)
        integer(int16), intent(in) :: A(:,:)
        integer(int16) :: res
        integer :: i
        res = 0
        do i = 1, minval(shape(A))
          res = res + A(i,i)
        end do
      end function trace_iint16
      function trace_iint32(A) result(res)
        integer(int32), intent(in) :: A(:,:)
        integer(int32) :: res
        integer :: i
        res = 0
        do i = 1, minval(shape(A))
          res = res + A(i,i)
        end do
      end function trace_iint32
      function trace_iint64(A) result(res)
        integer(int64), intent(in) :: A(:,:)
        integer(int64) :: res
        integer :: i
        res = 0
        do i = 1, minval(shape(A))
          res = res + A(i,i)
        end do
      end function trace_iint64


      pure function is_square_rsp(A) result(res)
        real(sp), intent(in) :: A(:,:)
        logical :: res
        res = (size(A,1) == size(A,2))
      end function is_square_rsp
      pure function is_square_rdp(A) result(res)
        real(dp), intent(in) :: A(:,:)
        logical :: res
        res = (size(A,1) == size(A,2))
      end function is_square_rdp
      pure function is_square_rxdp(A) result(res)
        real(xdp), intent(in) :: A(:,:)
        logical :: res
        res = (size(A,1) == size(A,2))
      end function is_square_rxdp
      pure function is_square_rqp(A) result(res)
        real(qp), intent(in) :: A(:,:)
        logical :: res
        res = (size(A,1) == size(A,2))
      end function is_square_rqp
      pure function is_square_csp(A) result(res)
        complex(sp), intent(in) :: A(:,:)
        logical :: res
        res = (size(A,1) == size(A,2))
      end function is_square_csp
      pure function is_square_cdp(A) result(res)
        complex(dp), intent(in) :: A(:,:)
        logical :: res
        res = (size(A,1) == size(A,2))
      end function is_square_cdp
      pure function is_square_cxdp(A) result(res)
        complex(xdp), intent(in) :: A(:,:)
        logical :: res
        res = (size(A,1) == size(A,2))
      end function is_square_cxdp
      pure function is_square_cqp(A) result(res)
        complex(qp), intent(in) :: A(:,:)
        logical :: res
        res = (size(A,1) == size(A,2))
      end function is_square_cqp
      pure function is_square_iint8(A) result(res)
        integer(int8), intent(in) :: A(:,:)
        logical :: res
        res = (size(A,1) == size(A,2))
      end function is_square_iint8
      pure function is_square_iint16(A) result(res)
        integer(int16), intent(in) :: A(:,:)
        logical :: res
        res = (size(A,1) == size(A,2))
      end function is_square_iint16
      pure function is_square_iint32(A) result(res)
        integer(int32), intent(in) :: A(:,:)
        logical :: res
        res = (size(A,1) == size(A,2))
      end function is_square_iint32
      pure function is_square_iint64(A) result(res)
        integer(int64), intent(in) :: A(:,:)
        logical :: res
        res = (size(A,1) == size(A,2))
      end function is_square_iint64


      pure function is_diagonal_rsp(A) result(res)
        real(sp), intent(in) :: A(:,:)
        logical :: res
        real(sp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        do j = 1, n !loop over all columns
            o = min(j-1,m) !index of row above diagonal (or last row)
            do i = 1, o !loop over rows above diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
            do i = o+2, m !loop over rows below diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is diagonal
      end function is_diagonal_rsp
      pure function is_diagonal_rdp(A) result(res)
        real(dp), intent(in) :: A(:,:)
        logical :: res
        real(dp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        do j = 1, n !loop over all columns
            o = min(j-1,m) !index of row above diagonal (or last row)
            do i = 1, o !loop over rows above diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
            do i = o+2, m !loop over rows below diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is diagonal
      end function is_diagonal_rdp
      pure function is_diagonal_rxdp(A) result(res)
        real(xdp), intent(in) :: A(:,:)
        logical :: res
        real(xdp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        do j = 1, n !loop over all columns
            o = min(j-1,m) !index of row above diagonal (or last row)
            do i = 1, o !loop over rows above diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
            do i = o+2, m !loop over rows below diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is diagonal
      end function is_diagonal_rxdp
      pure function is_diagonal_rqp(A) result(res)
        real(qp), intent(in) :: A(:,:)
        logical :: res
        real(qp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        do j = 1, n !loop over all columns
            o = min(j-1,m) !index of row above diagonal (or last row)
            do i = 1, o !loop over rows above diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
            do i = o+2, m !loop over rows below diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is diagonal
      end function is_diagonal_rqp
      pure function is_diagonal_csp(A) result(res)
        complex(sp), intent(in) :: A(:,:)
        logical :: res
        complex(sp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        do j = 1, n !loop over all columns
            o = min(j-1,m) !index of row above diagonal (or last row)
            do i = 1, o !loop over rows above diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
            do i = o+2, m !loop over rows below diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is diagonal
      end function is_diagonal_csp
      pure function is_diagonal_cdp(A) result(res)
        complex(dp), intent(in) :: A(:,:)
        logical :: res
        complex(dp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        do j = 1, n !loop over all columns
            o = min(j-1,m) !index of row above diagonal (or last row)
            do i = 1, o !loop over rows above diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
            do i = o+2, m !loop over rows below diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is diagonal
      end function is_diagonal_cdp
      pure function is_diagonal_cxdp(A) result(res)
        complex(xdp), intent(in) :: A(:,:)
        logical :: res
        complex(xdp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        do j = 1, n !loop over all columns
            o = min(j-1,m) !index of row above diagonal (or last row)
            do i = 1, o !loop over rows above diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
            do i = o+2, m !loop over rows below diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is diagonal
      end function is_diagonal_cxdp
      pure function is_diagonal_cqp(A) result(res)
        complex(qp), intent(in) :: A(:,:)
        logical :: res
        complex(qp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        do j = 1, n !loop over all columns
            o = min(j-1,m) !index of row above diagonal (or last row)
            do i = 1, o !loop over rows above diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
            do i = o+2, m !loop over rows below diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is diagonal
      end function is_diagonal_cqp
      pure function is_diagonal_iint8(A) result(res)
        integer(int8), intent(in) :: A(:,:)
        logical :: res
        integer(int8), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        do j = 1, n !loop over all columns
            o = min(j-1,m) !index of row above diagonal (or last row)
            do i = 1, o !loop over rows above diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
            do i = o+2, m !loop over rows below diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is diagonal
      end function is_diagonal_iint8
      pure function is_diagonal_iint16(A) result(res)
        integer(int16), intent(in) :: A(:,:)
        logical :: res
        integer(int16), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        do j = 1, n !loop over all columns
            o = min(j-1,m) !index of row above diagonal (or last row)
            do i = 1, o !loop over rows above diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
            do i = o+2, m !loop over rows below diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is diagonal
      end function is_diagonal_iint16
      pure function is_diagonal_iint32(A) result(res)
        integer(int32), intent(in) :: A(:,:)
        logical :: res
        integer(int32), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        do j = 1, n !loop over all columns
            o = min(j-1,m) !index of row above diagonal (or last row)
            do i = 1, o !loop over rows above diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
            do i = o+2, m !loop over rows below diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is diagonal
      end function is_diagonal_iint32
      pure function is_diagonal_iint64(A) result(res)
        integer(int64), intent(in) :: A(:,:)
        logical :: res
        integer(int64), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        do j = 1, n !loop over all columns
            o = min(j-1,m) !index of row above diagonal (or last row)
            do i = 1, o !loop over rows above diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
            do i = o+2, m !loop over rows below diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is diagonal
      end function is_diagonal_iint64


      pure function is_symmetric_rsp(A) result(res)
        real(sp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j-1 !loop over all rows above diagonal
                if (A(i,j) /= A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is symmetric
      end function is_symmetric_rsp
      pure function is_symmetric_rdp(A) result(res)
        real(dp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j-1 !loop over all rows above diagonal
                if (A(i,j) /= A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is symmetric
      end function is_symmetric_rdp
      pure function is_symmetric_rxdp(A) result(res)
        real(xdp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j-1 !loop over all rows above diagonal
                if (A(i,j) /= A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is symmetric
      end function is_symmetric_rxdp
      pure function is_symmetric_rqp(A) result(res)
        real(qp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j-1 !loop over all rows above diagonal
                if (A(i,j) /= A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is symmetric
      end function is_symmetric_rqp
      pure function is_symmetric_csp(A) result(res)
        complex(sp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j-1 !loop over all rows above diagonal
                if (A(i,j) /= A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is symmetric
      end function is_symmetric_csp
      pure function is_symmetric_cdp(A) result(res)
        complex(dp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j-1 !loop over all rows above diagonal
                if (A(i,j) /= A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is symmetric
      end function is_symmetric_cdp
      pure function is_symmetric_cxdp(A) result(res)
        complex(xdp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j-1 !loop over all rows above diagonal
                if (A(i,j) /= A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is symmetric
      end function is_symmetric_cxdp
      pure function is_symmetric_cqp(A) result(res)
        complex(qp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j-1 !loop over all rows above diagonal
                if (A(i,j) /= A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is symmetric
      end function is_symmetric_cqp
      pure function is_symmetric_iint8(A) result(res)
        integer(int8), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j-1 !loop over all rows above diagonal
                if (A(i,j) /= A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is symmetric
      end function is_symmetric_iint8
      pure function is_symmetric_iint16(A) result(res)
        integer(int16), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j-1 !loop over all rows above diagonal
                if (A(i,j) /= A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is symmetric
      end function is_symmetric_iint16
      pure function is_symmetric_iint32(A) result(res)
        integer(int32), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j-1 !loop over all rows above diagonal
                if (A(i,j) /= A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is symmetric
      end function is_symmetric_iint32
      pure function is_symmetric_iint64(A) result(res)
        integer(int64), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j-1 !loop over all rows above diagonal
                if (A(i,j) /= A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is symmetric
      end function is_symmetric_iint64


      pure function is_skew_symmetric_rsp(A) result(res)
        real(sp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be skew-symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j !loop over all rows above diagonal (and diagonal)
                if (A(i,j) /= -A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is skew-symmetric
      end function is_skew_symmetric_rsp
      pure function is_skew_symmetric_rdp(A) result(res)
        real(dp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be skew-symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j !loop over all rows above diagonal (and diagonal)
                if (A(i,j) /= -A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is skew-symmetric
      end function is_skew_symmetric_rdp
      pure function is_skew_symmetric_rxdp(A) result(res)
        real(xdp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be skew-symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j !loop over all rows above diagonal (and diagonal)
                if (A(i,j) /= -A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is skew-symmetric
      end function is_skew_symmetric_rxdp
      pure function is_skew_symmetric_rqp(A) result(res)
        real(qp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be skew-symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j !loop over all rows above diagonal (and diagonal)
                if (A(i,j) /= -A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is skew-symmetric
      end function is_skew_symmetric_rqp
      pure function is_skew_symmetric_csp(A) result(res)
        complex(sp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be skew-symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j !loop over all rows above diagonal (and diagonal)
                if (A(i,j) /= -A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is skew-symmetric
      end function is_skew_symmetric_csp
      pure function is_skew_symmetric_cdp(A) result(res)
        complex(dp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be skew-symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j !loop over all rows above diagonal (and diagonal)
                if (A(i,j) /= -A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is skew-symmetric
      end function is_skew_symmetric_cdp
      pure function is_skew_symmetric_cxdp(A) result(res)
        complex(xdp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be skew-symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j !loop over all rows above diagonal (and diagonal)
                if (A(i,j) /= -A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is skew-symmetric
      end function is_skew_symmetric_cxdp
      pure function is_skew_symmetric_cqp(A) result(res)
        complex(qp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be skew-symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j !loop over all rows above diagonal (and diagonal)
                if (A(i,j) /= -A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is skew-symmetric
      end function is_skew_symmetric_cqp
      pure function is_skew_symmetric_iint8(A) result(res)
        integer(int8), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be skew-symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j !loop over all rows above diagonal (and diagonal)
                if (A(i,j) /= -A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is skew-symmetric
      end function is_skew_symmetric_iint8
      pure function is_skew_symmetric_iint16(A) result(res)
        integer(int16), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be skew-symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j !loop over all rows above diagonal (and diagonal)
                if (A(i,j) /= -A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is skew-symmetric
      end function is_skew_symmetric_iint16
      pure function is_skew_symmetric_iint32(A) result(res)
        integer(int32), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be skew-symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j !loop over all rows above diagonal (and diagonal)
                if (A(i,j) /= -A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is skew-symmetric
      end function is_skew_symmetric_iint32
      pure function is_skew_symmetric_iint64(A) result(res)
        integer(int64), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be skew-symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j !loop over all rows above diagonal (and diagonal)
                if (A(i,j) /= -A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is skew-symmetric
      end function is_skew_symmetric_iint64


      pure function is_hermitian_rsp(A) result(res)
        real(sp), intent(in) :: A(:,:)
        logical :: res
        res = is_symmetric(A) !symmetry and Hermiticity are equivalent for real matrices
      end function is_hermitian_rsp
      pure function is_hermitian_rdp(A) result(res)
        real(dp), intent(in) :: A(:,:)
        logical :: res
        res = is_symmetric(A) !symmetry and Hermiticity are equivalent for real matrices
      end function is_hermitian_rdp
      pure function is_hermitian_rxdp(A) result(res)
        real(xdp), intent(in) :: A(:,:)
        logical :: res
        res = is_symmetric(A) !symmetry and Hermiticity are equivalent for real matrices
      end function is_hermitian_rxdp
      pure function is_hermitian_rqp(A) result(res)
        real(qp), intent(in) :: A(:,:)
        logical :: res
        res = is_symmetric(A) !symmetry and Hermiticity are equivalent for real matrices
      end function is_hermitian_rqp
      pure function is_hermitian_iint8(A) result(res)
        integer(int8), intent(in) :: A(:,:)
        logical :: res
        res = is_symmetric(A) !symmetry and Hermiticity are equivalent for real matrices
      end function is_hermitian_iint8
      pure function is_hermitian_iint16(A) result(res)
        integer(int16), intent(in) :: A(:,:)
        logical :: res
        res = is_symmetric(A) !symmetry and Hermiticity are equivalent for real matrices
      end function is_hermitian_iint16
      pure function is_hermitian_iint32(A) result(res)
        integer(int32), intent(in) :: A(:,:)
        logical :: res
        res = is_symmetric(A) !symmetry and Hermiticity are equivalent for real matrices
      end function is_hermitian_iint32
      pure function is_hermitian_iint64(A) result(res)
        integer(int64), intent(in) :: A(:,:)
        logical :: res
        res = is_symmetric(A) !symmetry and Hermiticity are equivalent for real matrices
      end function is_hermitian_iint64
      pure function is_hermitian_csp(A) result(res)
        complex(sp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be Hermitian
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j !loop over all rows above diagonal (and diagonal)
                if (A(i,j) /= conjg(A(j,i))) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is Hermitian
      end function is_hermitian_csp
      pure function is_hermitian_cdp(A) result(res)
        complex(dp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be Hermitian
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j !loop over all rows above diagonal (and diagonal)
                if (A(i,j) /= conjg(A(j,i))) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is Hermitian
      end function is_hermitian_cdp
      pure function is_hermitian_cxdp(A) result(res)
        complex(xdp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be Hermitian
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j !loop over all rows above diagonal (and diagonal)
                if (A(i,j) /= conjg(A(j,i))) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is Hermitian
      end function is_hermitian_cxdp
      pure function is_hermitian_cqp(A) result(res)
        complex(qp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be Hermitian
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j !loop over all rows above diagonal (and diagonal)
                if (A(i,j) /= conjg(A(j,i))) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is Hermitian
      end function is_hermitian_cqp


      function is_triangular_rsp(A,uplo) result(res)
        real(sp), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        real(sp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper triangularity
          do j = 1, n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i = o+2, m !loop over rows below diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower triangularity
          do j=1,n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i=1,o !loop over rows above diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_triangular): second argument must be one of {'u','U','l','L'}")
        end if
     
        res = .true. !otherwise A is triangular of the requested type
      end function is_triangular_rsp
      function is_triangular_rdp(A,uplo) result(res)
        real(dp), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        real(dp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper triangularity
          do j = 1, n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i = o+2, m !loop over rows below diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower triangularity
          do j=1,n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i=1,o !loop over rows above diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_triangular): second argument must be one of {'u','U','l','L'}")
        end if
     
        res = .true. !otherwise A is triangular of the requested type
      end function is_triangular_rdp
      function is_triangular_rxdp(A,uplo) result(res)
        real(xdp), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        real(xdp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper triangularity
          do j = 1, n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i = o+2, m !loop over rows below diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower triangularity
          do j=1,n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i=1,o !loop over rows above diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_triangular): second argument must be one of {'u','U','l','L'}")
        end if
     
        res = .true. !otherwise A is triangular of the requested type
      end function is_triangular_rxdp
      function is_triangular_rqp(A,uplo) result(res)
        real(qp), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        real(qp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper triangularity
          do j = 1, n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i = o+2, m !loop over rows below diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower triangularity
          do j=1,n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i=1,o !loop over rows above diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_triangular): second argument must be one of {'u','U','l','L'}")
        end if
     
        res = .true. !otherwise A is triangular of the requested type
      end function is_triangular_rqp
      function is_triangular_csp(A,uplo) result(res)
        complex(sp), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        complex(sp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper triangularity
          do j = 1, n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i = o+2, m !loop over rows below diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower triangularity
          do j=1,n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i=1,o !loop over rows above diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_triangular): second argument must be one of {'u','U','l','L'}")
        end if
     
        res = .true. !otherwise A is triangular of the requested type
      end function is_triangular_csp
      function is_triangular_cdp(A,uplo) result(res)
        complex(dp), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        complex(dp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper triangularity
          do j = 1, n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i = o+2, m !loop over rows below diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower triangularity
          do j=1,n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i=1,o !loop over rows above diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_triangular): second argument must be one of {'u','U','l','L'}")
        end if
     
        res = .true. !otherwise A is triangular of the requested type
      end function is_triangular_cdp
      function is_triangular_cxdp(A,uplo) result(res)
        complex(xdp), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        complex(xdp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper triangularity
          do j = 1, n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i = o+2, m !loop over rows below diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower triangularity
          do j=1,n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i=1,o !loop over rows above diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_triangular): second argument must be one of {'u','U','l','L'}")
        end if
     
        res = .true. !otherwise A is triangular of the requested type
      end function is_triangular_cxdp
      function is_triangular_cqp(A,uplo) result(res)
        complex(qp), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        complex(qp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper triangularity
          do j = 1, n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i = o+2, m !loop over rows below diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower triangularity
          do j=1,n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i=1,o !loop over rows above diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_triangular): second argument must be one of {'u','U','l','L'}")
        end if
     
        res = .true. !otherwise A is triangular of the requested type
      end function is_triangular_cqp
      function is_triangular_iint8(A,uplo) result(res)
        integer(int8), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        integer(int8), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper triangularity
          do j = 1, n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i = o+2, m !loop over rows below diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower triangularity
          do j=1,n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i=1,o !loop over rows above diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_triangular): second argument must be one of {'u','U','l','L'}")
        end if
     
        res = .true. !otherwise A is triangular of the requested type
      end function is_triangular_iint8
      function is_triangular_iint16(A,uplo) result(res)
        integer(int16), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        integer(int16), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper triangularity
          do j = 1, n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i = o+2, m !loop over rows below diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower triangularity
          do j=1,n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i=1,o !loop over rows above diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_triangular): second argument must be one of {'u','U','l','L'}")
        end if
     
        res = .true. !otherwise A is triangular of the requested type
      end function is_triangular_iint16
      function is_triangular_iint32(A,uplo) result(res)
        integer(int32), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        integer(int32), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper triangularity
          do j = 1, n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i = o+2, m !loop over rows below diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower triangularity
          do j=1,n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i=1,o !loop over rows above diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_triangular): second argument must be one of {'u','U','l','L'}")
        end if
     
        res = .true. !otherwise A is triangular of the requested type
      end function is_triangular_iint32
      function is_triangular_iint64(A,uplo) result(res)
        integer(int64), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        integer(int64), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper triangularity
          do j = 1, n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i = o+2, m !loop over rows below diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower triangularity
          do j=1,n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i=1,o !loop over rows above diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_triangular): second argument must be one of {'u','U','l','L'}")
        end if
     
        res = .true. !otherwise A is triangular of the requested type
      end function is_triangular_iint64


      function is_hessenberg_rsp(A,uplo) result(res)
        real(sp), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        real(sp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = o+4, m !loop over rows two or more below main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = 1, o !loop over rows one or more above main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_hessenberg): second argument must be one of {'u','U','l','L'}")
        end if
        res = .true. !otherwise A is Hessenberg of the requested type
      end function is_hessenberg_rsp
      function is_hessenberg_rdp(A,uplo) result(res)
        real(dp), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        real(dp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = o+4, m !loop over rows two or more below main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = 1, o !loop over rows one or more above main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_hessenberg): second argument must be one of {'u','U','l','L'}")
        end if
        res = .true. !otherwise A is Hessenberg of the requested type
      end function is_hessenberg_rdp
      function is_hessenberg_rxdp(A,uplo) result(res)
        real(xdp), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        real(xdp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = o+4, m !loop over rows two or more below main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = 1, o !loop over rows one or more above main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_hessenberg): second argument must be one of {'u','U','l','L'}")
        end if
        res = .true. !otherwise A is Hessenberg of the requested type
      end function is_hessenberg_rxdp
      function is_hessenberg_rqp(A,uplo) result(res)
        real(qp), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        real(qp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = o+4, m !loop over rows two or more below main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = 1, o !loop over rows one or more above main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_hessenberg): second argument must be one of {'u','U','l','L'}")
        end if
        res = .true. !otherwise A is Hessenberg of the requested type
      end function is_hessenberg_rqp
      function is_hessenberg_csp(A,uplo) result(res)
        complex(sp), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        complex(sp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = o+4, m !loop over rows two or more below main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = 1, o !loop over rows one or more above main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_hessenberg): second argument must be one of {'u','U','l','L'}")
        end if
        res = .true. !otherwise A is Hessenberg of the requested type
      end function is_hessenberg_csp
      function is_hessenberg_cdp(A,uplo) result(res)
        complex(dp), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        complex(dp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = o+4, m !loop over rows two or more below main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = 1, o !loop over rows one or more above main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_hessenberg): second argument must be one of {'u','U','l','L'}")
        end if
        res = .true. !otherwise A is Hessenberg of the requested type
      end function is_hessenberg_cdp
      function is_hessenberg_cxdp(A,uplo) result(res)
        complex(xdp), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        complex(xdp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = o+4, m !loop over rows two or more below main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = 1, o !loop over rows one or more above main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_hessenberg): second argument must be one of {'u','U','l','L'}")
        end if
        res = .true. !otherwise A is Hessenberg of the requested type
      end function is_hessenberg_cxdp
      function is_hessenberg_cqp(A,uplo) result(res)
        complex(qp), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        complex(qp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = o+4, m !loop over rows two or more below main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = 1, o !loop over rows one or more above main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_hessenberg): second argument must be one of {'u','U','l','L'}")
        end if
        res = .true. !otherwise A is Hessenberg of the requested type
      end function is_hessenberg_cqp
      function is_hessenberg_iint8(A,uplo) result(res)
        integer(int8), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        integer(int8), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = o+4, m !loop over rows two or more below main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = 1, o !loop over rows one or more above main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_hessenberg): second argument must be one of {'u','U','l','L'}")
        end if
        res = .true. !otherwise A is Hessenberg of the requested type
      end function is_hessenberg_iint8
      function is_hessenberg_iint16(A,uplo) result(res)
        integer(int16), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        integer(int16), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = o+4, m !loop over rows two or more below main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = 1, o !loop over rows one or more above main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_hessenberg): second argument must be one of {'u','U','l','L'}")
        end if
        res = .true. !otherwise A is Hessenberg of the requested type
      end function is_hessenberg_iint16
      function is_hessenberg_iint32(A,uplo) result(res)
        integer(int32), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        integer(int32), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = o+4, m !loop over rows two or more below main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = 1, o !loop over rows one or more above main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_hessenberg): second argument must be one of {'u','U','l','L'}")
        end if
        res = .true. !otherwise A is Hessenberg of the requested type
      end function is_hessenberg_iint32
      function is_hessenberg_iint64(A,uplo) result(res)
        integer(int64), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        integer(int64), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = o+4, m !loop over rows two or more below main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = 1, o !loop over rows one or more above main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_hessenberg): second argument must be one of {'u','U','l','L'}")
        end if
        res = .true. !otherwise A is Hessenberg of the requested type
      end function is_hessenberg_iint64
    
end module stdlib_linalg