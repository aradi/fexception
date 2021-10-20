module partially_propagate_exception
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
          call subroutine_propagating_error(ii, internal_errorvar)
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
            exit try_catch
          end block catch_io_error
        class default
          print "(a)", "main: obtained some error, but I did not care to obtain its details"
        end select
      end block try_catch

      print "(a)", "main: left try-catch construct"

    end do

  end subroutine main



  subroutine subroutine_propagating_error(ierror, internal_errorvar)
    integer, intent(in) :: ierror
    class(*), allocatable, target, intent(out) :: internal_errorvar

    print "(a)", "subroutine_propgating_error: entering try construct"
    try_catch_env: block
      try_catch: block

        try: block
          call subroutine_throwing_error(ierror, internal_errorvar)
          if (allocated(internal_errorvar)) exit try
          exit try_catch
        end block try

        select type (internal_errorvar)
        type is (linalg_error)
          catch_linalg_error: block
            type(linalg_error), pointer :: errorvar
            errorvar => internal_errorvar
            print "(a)", "subroutine_propagating_error: caught linalg_error"
            if (allocated(errorvar%message)) print "(2a)", "Message: ", errorvar%message
            print "(a,i0)", "Info: ", errorvar%info
            exit try_catch
          end block catch_linalg_error
        ! propagate everything upwards, which was not handled here
        class default
          return
        end select
      end block try_catch

      ! We need to explicitely deallocate here, as the error had been handled when we arrive here
      if (allocated(internal_errorvar)) deallocate(internal_errorvar)

    end block try_catch_env

    print "(a)", "subroutine_propagating_error: left try-catch construct"

  end subroutine subroutine_propagating_error


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

end module partially_propagate_exception


program program_partially_propagate_exception
  use partially_propagate_exception
  implicit none

  call main()

end program program_partially_propagate_exception
