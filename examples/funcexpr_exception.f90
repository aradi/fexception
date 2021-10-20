module funcexpr_exception
  use user_exceptions, only : fatal_error, io_error, linalg_error
  implicit none

contains

  subroutine main()

    integer :: ii
    integer :: res

    do ii = 0, 3

      print "(a,i0)", "*** Error generation loop: #", ii
      print "(a)", "main: entering try-catch construct"

      try_catch: block
        class(*), allocatable, target :: internal_errorvar

        try: block
          res = function_propagating_error(ii, internal_errorvar)
          if (allocated(internal_errorvar)) exit try
          ! :
          exit try_catch
        end block try

        select type (internal_errorvar)
        type is (io_error)
        catch_io_error: block
            type(io_error), pointer :: errorvar
            errorvar => internal_errorvar
            print "(a)", "main: caught io_error"
            if (allocated(errorvar%message)) print "(2a)", "Message: ", errorvar%message
            if (allocated(errorvar%filename)) print "(2a)", "File name: ", errorvar%filename
            print "(a,i0)", "Unit: ", errorvar%unit
            res = -1
            exit try_catch
          end block catch_io_error
        class default
          print "(a)", "main: obtained some error, but I did not care to obtain its details"
          res = -2
        end select
      end block try_catch

      print "(a)", "main: left try-catch construct"
      print "(a,i0)", "main: value of assignment: ", res

    end do

  end subroutine main


  function function_propagating_error(ierror, internal_errorvar) result(res)
    integer, intent(in) :: ierror
    class(*), allocatable, intent(out) :: internal_errorvar
    integer :: res

    print "(a)", "function_propagating_error: Entering try construct"
    try: block
      integer :: tmpres1, tmpres2
      tmpres1 = function_throwing_error1(ierror, internal_errorvar)
      if (allocated(internal_errorvar)) return
      tmpres2 = function_throwing_error2(ierror, internal_errorvar)
      if (allocated(internal_errorvar)) return
      res = tmpres1 + tmpres2
    end block try
    print "(a)", "function_propagating_error: Left try construct"

  end function function_propagating_error


  function function_throwing_error1(ierror, internal_errorvar) result(res)
    integer, intent(in) :: ierror
    class(*), allocatable, intent(out) :: internal_errorvar
    integer :: res

    select case (ierror)
    case (1)
      print "(a)", "function_throwing_error1: throwing io_error"
      internal_errorvar = io_error(message="I/O error happened", filename="test.dat", unit=12)
      return
    case (2)
      print "(a)", "function_throwing_error1: throwing linalg_error"
      internal_errorvar = linalg_error(message="Linalg error happened", info=4)
      return
    case (3)
      print "(a)", "function_throwing_error1: throwing fatal_error"
      internal_errorvar = fatal_error(message="Fatal error without further details happened")
      return
    end select

    print "(a)", "function_throwing_error1: returning without any error"
    res = 42

  end function function_throwing_error1


  function function_throwing_error2(ierror, internal_errorvar) result(res)
    integer, intent(in) :: ierror
    class(*), allocatable, intent(out) :: internal_errorvar
    integer :: res

    select case (ierror)
    case (2)
      print "(a)", "function_throwing_error2: throwing io_error"
      internal_errorvar = io_error(message="I/O error happened", filename="test.dat", unit=12)
      return
    case (3)
      print "(a)", "function_throwing_error2: throwing linalg_error"
      internal_errorvar = linalg_error(message="Linalg error happened", info=4)
      return
    case (0)
      print "(a)", "function_throwing_error2: throwing fatal_error"
      internal_errorvar = fatal_error(message="Fatal error without further details happened")
      return
    end select

    print "(a)", "function_throwing_error2: returning without any error"
    res = -142

  end function function_throwing_error2


end module funcexpr_exception


program program_funcexpr_exception
  use funcexpr_exception
  implicit none

  call main()

end program program_funcexpr_exception
