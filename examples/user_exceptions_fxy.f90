module user_exceptions_fxy
  implicit none

  ! Note: For throwable types, compiler should make a few checks:
  ! * They should not contain any pointer components
  ! * They should only have pure subroutines as type bound procedures (or maybe one should
  !   completely forbid type bound procedures?)

  ! Let's generate a generic fatal_error for our project
  type, throwable :: fatal_error
    character(:), allocatable :: message
  end type fatal_error


  ! An I/O error should also contain information about unit and file name
  type, throwable, extends(fatal_error) :: io_error
    character(:), allocatable :: filename
    integer :: unit = -1
  end type io_error


  ! A linalg error should carry the info flag of the underlying library
  type, throwable, extends(fatal_error) :: linalg_error
    integer :: info = 0
  end type linalg_error

end module user_exceptions_fxy
