module stdlib_string_builder_m_tests
   use stdlib_string_builder_m
   use testdrive, only: error_type, unittest_type, new_unittest, check
   implicit none
   private

   public :: collect

contains
   subroutine collect(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_push", test_push) &
                  , new_unittest("test_push_line", test_push) &
                  ]

   end subroutine

   subroutine test_push(error)
      type(error_type), allocatable, intent(out) :: error
      type(string_builder) :: sb1, sb2

      sb1 = string_builder("ABC"); 
      call check(error, sb1%to_chars(), "ABC")

      call sb1%push("DEF"); 
      call check(error, sb1%to_chars(), "ABCDEF")
      call check(error, sb1%length(), 6)

      sb2 = string_builder('')
      call check(error, sb2%length(), 0)
      call sb2%push("GHIJKL"); 
      call check(error, sb2%to_chars(), "GHIJKL")
      call sb1%push(sb2%to_chars()); 
      call check(error, sb1%to_chars(), "ABCDEFGHIJKL")
   end subroutine

   subroutine test_push_line(error)
      type(error_type), allocatable, intent(out) :: error
      type(string_builder) :: sb1, sb2

      sb1 = string_builder("ABC"); 
      call check(error, sb1%to_chars(), "ABC")

      call sb1%push_line("DEF"); 
      call check(error, sb1%to_chars(), "ABC"//new_line('a')//"DEF")
      call check(error, sb1%length(), 6)

      sb2 = string_builder('')
      call check(error, sb2%length(), 0)
      call sb2%push_line("GHIJKL"); 
      call check(error, sb2%to_chars(), "GHIJKL")
      call sb1%push_line(sb2%to_chars()); 
      call check(error, sb1%to_chars(), "ABC"//new_line('a')//"DEF"//new_line('a')//"GHIJKL")
   end subroutine
end module
