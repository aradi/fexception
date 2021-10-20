module partially_propagate_exception_fxy
  use user_exceptions, only : fatal_error, io_error, linalg_error
  implicit none

contains

  subroutine main()

    integer :: ii

    do ii = 0, 3

      print "(a,i0)", "*** Error generation loop: #", ii
      print "(a)", "main: entering try-catch construct"

      try catch
        call subroutine_propagating_error(ii)
      catch (io_error)
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


  ! Note, since linalg_error is handled locally, this routine only throws io_error or fatal_error

  subroutine subroutine_propagating_error(ierror) throws(io_error, fatal_error)
    integer, intent(in) :: ierror

    print "(a)", "subroutine_propgating_error: entering try construct"
    try catch
      call subroutine_throwing_error(ierror, internal_errorvar)
    catch (linalg_error)
      print "(a)", "subroutine_propagating_error: caught linalg_error"
    end try
    print "(a)", "subroutine_propgating_error: left try-catch construct"

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

end module partially_propagate_exception_fxy


program program_partially_propagate_exception_fxy
  use partially_propagate_exception_fxy
  implicit none

  call main()

end program program_partially_propagate_exception_fxy
