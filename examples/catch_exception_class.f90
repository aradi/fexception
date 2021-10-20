! This should be only considered, if we allow the handling of exception classes additional
! to exception types

module catch_exception_class
  use user_exceptions, only : fatal_error, io_error, linalg_error
  implicit none

contains

  subroutine main()

    integer :: ii

    do ii = 0, 3

      print "(a,i0)", "*** Error generation loop: #", ii
      print "(a)", "main: entering try-catch construct"

      try_catch: block
        class(*), allocatable, target :: internal_errorvar

        try: block
          call subroutine_throwing_error(ii, internal_errorvar)
          if (allocated(internal_errorvar)) exit try
          ! call subroutine_throwing_error2(...)
          ! if (allocated(internal_errorvar)) exit try
          ! :
          exit try_catch
        end block try

        select type (internal_errorvar)
        type is (io_error)
          catch_io_error: block
            type(io_error), pointer :: errorvar
            errorvar => internal_errorvar
            print "(a)", "main: caught io_error type"
            if (allocated(errorvar%message)) print "(2a)", "Message: ", errorvar%message
            if (allocated(errorvar%filename)) print "(2a)", "File name: ", errorvar%filename
            print "(a,i0)", "Unit: ", errorvar%unit
            exit try_catch
          end block catch_io_error
        class is (fatal_error)
          catch_fatal_error: block
            class(fatal_error), pointer :: errorvar
            errorvar => internal_errorvar
            print "(a)", "main: caught fatal_error class"
            if (allocated(errorvar%message)) print "(2a)", "Message: ", errorvar%message
            exit try_catch
          end block catch_fatal_error
        ! "class default" not needed, as all errors had been already handled above
        end select
      end block try_catch

      print "(a)", "main: left try-catch construct"
    end do

  end subroutine main


  subroutine subroutine_throwing_error(ierror, internal_errorvar)
    integer, intent(in) :: ierror
    class(*), allocatable, intent(out) :: internal_errorvar

    select case (ierror)
    case (1)
      print "(a)", "subroutine_throwing_error: throwing io_error"
      internal_errorvar = io_error(message="I/O error happened", filename="test.dat", unit=12)
      return
    case (2)
      print "(a)", "subroutine_throwing_error: throwing linalg_error"
      internal_errorvar = linalg_error(message="Linalg error happened", info=4)
      return
    case (3)
      print "(a)", "subroutine_throwing_error: throwing fatal_error"
      internal_errorvar = fatal_error(message="Fatal error without further details happened")
      return
    end select

    print "(a)", "subroutine_throwing_error: returning without any error"

  end subroutine subroutine_throwing_error

end module catch_exception_class


program program_catch_exception_class
  use catch_exception_class
  implicit none

  call main()

end program program_catch_exception_class
