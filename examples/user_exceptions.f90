module user_exceptions
  implicit none

  ! Let's generate a generic fatal_error for our project
  type :: fatal_error
    character(:), allocatable :: message
  end type fatal_error


  ! An I/O error should also contain information about unit and file name
  type, extends(fatal_error) :: io_error
    character(:), allocatable :: filename
    integer :: unit = -1
  end type io_error


  ! A linalg error should carry the info flag of the underlying library
  type, extends(fatal_error) :: linalg_error
    integer :: info = 0
  end type linalg_error

end module user_exceptions
