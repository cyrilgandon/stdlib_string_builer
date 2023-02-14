program main
   use, intrinsic :: iso_fortran_env, only: output_unit
   use stdlib_strings
   use stdlib_string_type
   use stdlib_string_builder_m
   implicit none

   real :: start_cpu_time, end_cpu_time

   call cpu_time(start_cpu_time)
   write (output_unit, *) "Program starting..."

   call cpu_time(end_cpu_time)
   write (output_unit, *) "Finished in "//to_string(int((end_cpu_time - start_cpu_time)*1000))//"ms"
end program main
