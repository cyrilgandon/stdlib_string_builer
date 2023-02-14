program tester
   use, intrinsic :: iso_fortran_env, only: error_unit
   use testdrive, only: run_testsuite, testsuite_type, new_testsuite
   use stdlib_string_builder_m_tests, only: collect_string_builder_m_tests => collect

   integer :: stat, is
   type(testsuite_type), allocatable :: testsuites(:)

   testsuites = [ &
                new_testsuite("string_builder_m_tests", collect_string_builder_m_tests) &
                ]

   stat = 0
   do is = 1, size(testsuites)
      write (error_unit, '("#", *(1x, a))') "Testing:", testsuites(is)%name
      call run_testsuite(testsuites(is)%collect, error_unit, stat)
   end do

   if (stat > 0) then
      write (error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
      error stop
   end if

end program tester
