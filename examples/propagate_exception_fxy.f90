module propagate_exception_fxy
  use user_exceptions_fxy, only : fatal_error, io_error, linalg_error
  implicit none

contains

  subroutine main()

    integer :: ii

    do ii = 0, 3

      print "(a,i0)", "*** Error generation loop: #", ii
      print "(a)", "main: entering try-catch construct"

      try catch
        call subroutine_propagating_error(ii, internal_errorvar)
      catch (io_error as errorvar)
        print "(a)", "main: caught io_error"
        if (allocated(errorvar%message)) print "(2a)", "Message: ", errorvar%message
        if (allocated(errorvar%filename)) print "(2a)", "File name: ", errorvar%filename
        print "(a,i0)", "Unit: ", errorvar%unit
      catch all
        print "(a)", "main: obtained some error, but I did not care to obtain its details"
      end try

      print "(a)", "main: left try-catch construct"

    end do

  end subroutine main


  subroutine subroutine_propagating_error(ierror) throws(io_error, linalg_error, fatal_error)
    integer, intent(in) :: ierror

    print "(a)", "subroutine_propgating_error: Entering try construct"
    try call subroutine_throwing_error(ierror)
    print "(a)", "subroutine_propgating_error: Left try construct"

  end subroutine subroutine_propagating_error


  subroutine subroutine_throwing_error(ierror) throws(io_error, linalg_error, fatal_error)
    integer, intent(in) :: ierror

    select case (ierror)
    case (1)
      print "(a)", "subroutine_throwing_error: throwing io_error"
      throw io_error(message="I/O error happened", filename="test.dat", unit=12)
    case (2)
      print "(a)", "subroutine_throwing_error: throwing linalg_error"
      throw linalg_error(message="Linalg error happened", info=4)
    case (3)
      print "(a)", "subroutine_throwing_error: throwing fatal_error"
      throw fatal_error(message="Fatal error without further details happened")
    end select

    print "(a)", "subroutine_throwing_error: returning without any error"

  end subroutine subroutine_throwing_error

end module propagate_exception_fxy


program program_propagate_exception_fxy
  use propagate_exception_fxy
  implicit none

  call main()

end program program_propagate_exception_fxy
