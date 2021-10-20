set(common-sources
    user_exceptions.f90)

set(examples
    function_exception
    catch_exception
    catch_exception_class
    funcexpr_exception
    propagate_exception
    partially_propagate_exception
    rethrow_exception)

add_library(common OBJECT ${common-sources})

foreach(example IN LISTS examples)
    add_executable(${example} ${example}.f90)
    target_link_libraries(${example} PRIVATE common)
endforeach()
