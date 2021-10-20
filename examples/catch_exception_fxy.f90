module catch_exception_fxy
  use user_exceptions_fxy, only : fatal_error, io_error, linalg_error
  implicit none

contains

  subroutine main()

    integer :: ii

    do ii = 0, 3

      print "(a)", "main: entering try-catch construct"

      ! Note: Since main is declared not to throw errors, compiler must check, whether all errors
      ! which can be thrown in first block are caught by catch clauses. For example, commenting out
      ! the "catch all" clause below should raise a compiler error, as linalg_error and fatal_error
      ! have not had been handled then.

      ! Note: the "as errorvar" in the catch clause should be optional. A catch without knowing
      ! the details of the error can be declares as "catch (io_error)"

      try catch
        call subroutine_throwing_error(ii)
      catch (io_error as errorvar)
        print "(a)", "main: caught io_error"
        if (allocated(errorvar%message)) print "(2a)", "Message: ", errorvar%message
        if (allocated(errorvar%filename)) print "(2a)", "File name: ", errovar%filename
        print "(a,i0)", "Unit: ", errovar%unit
      catch all
        print "(a)", "main: obtained some error, but I did not care to obtain its details"
      end try
      print "(a)", "main: left try-catch construct"
    end do

  end subroutine main


  subroutine subroutine_throwing_error(ierror) throws(io_error, linalg_error, fatal_error)
    integer, intent(in) :: ierror

    ! Note: compiler must check, whether only the errors declared in the "throws()" clause
    ! above are indeed thrown by the subroutine.

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

end module catch_exception_fxy


program program_catch_exception_fxy
  use catch_exception_fxy
  implicit none

  call main()

end program program_catch_exception_fxy
