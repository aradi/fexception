module funcexpr_exception_fxy
  use user_exceptions_fxy, only : fatal_error, io_error, linalg_error
  implicit none

contains

  subroutine main()

    integer :: ii
    integer :: res

    do ii = 0, 3

      print "(a,i0)", "*** Error generation loop: #", ii
      print "(a)", "main: entering try-catch construct"

      try catch
        res = function_propagating_error(ii, internal_errorvar)
      catch (io_error as errorvar)
          print "(a)", "main: caught io_error"
          if (allocated(errorvar%message)) print "(2a)", "Message: ", errorvar%message
          if (allocated(errorvar%filename)) print "(2a)", "File name: ", errorvar%filename
          print "(a,i0)", "Unit: ", errorvar%unit
          res = -1
      catch all
          print "(a)", "main: obtained some error, but I did not care to obtain its details"
          res = -2
      end try

      print "(a)", "main: left try-catch construct"
      print "(a,i0)", "main: value of assignment: ", res

    end do

  end subroutine main


  function function_propagating_error(ierror) result(res)&
      & throws(io_error, linalg_error, fatal_error)
    integer, intent(in) :: ierror
    integer :: res

    try res = function_throwing_error1(ierror)&
        & + function_throwing_error2(ierror)

  end function function_propagating_error


  function function_throwing_error1(ierror) result(res) throws(io_error, linalg_error, fatal_error)
    integer, intent(in) :: ierror
    integer :: res

    select case (ierror)
    case (1)
      print "(a)", "function_throwing_error1: throwing io_error"
      throw io_error(message="I/O error happened", filename="test.dat", unit=12)
    case (2)
      print "(a)", "function_throwing_error1: throwing linalg_error"
      throw linalg_error(message="Linalg error happened", info=4)
    case (3)
      print "(a)", "function_throwing_error1: throwing fatal_error"
      throw fatal_error(message="Fatal error without further details happened")
    end select

    print "(a)", "function_throwing_error1: returning without any error"
    res = 42

  end function function_throwing_error1


function function_throwing_error2(ierror) result(res) throws(io_error, linalg_error, fatal_error)
  integer, intent(in) :: ierror
  integer :: res

  select case (ierror)
  case (2)
    print "(a)", "function_throwing_error2: throwing io_error"
    throw io_error(message="I/O error happened", filename="test.dat", unit=12)
  case (3)
    print "(a)", "function_throwing_error2: throwing linalg_error"
    throw linalg_error(message="Linalg error happened", info=4)
  case (0)
    print "(a)", "function_throwing_error2: throwing fatal_error"
    throw fatal_error(message="Fatal error without further details happened")
  end select

  print "(a)", "function_throwing_error2: returning without any error"
  res = -142

end function function_throwing_error2

end module funcexpr_exception_fxy


program program_funcexpr_exception_fxy
  use funcexpr_exception_fxy
  implicit none

  call main()

end program program_funcexpr_exception_fxy
