module stdlib_string_builder_m
   use stdlib_string_type

   implicit none

   private
   public string_builder

   type :: string_builder
      private
      character(len=:), allocatable :: chars
      integer :: m_length = 0
   contains
      procedure :: ensure_capacity
      procedure :: length
      procedure :: capacity
      generic :: push => push_chars, push_std_string_type
      generic :: push_line => push_line_chars, push_line_std_string_type
      procedure :: to_chars

      procedure, private :: push_chars
      procedure, private :: push_std_string_type
      procedure, private :: push_line_chars
      procedure, private :: push_line_std_string_type
   end type

   interface string_builder
      module procedure init
   end interface

contains

   type(string_builder) pure function init(chars) result(this)
      character(len=*), intent(in) :: chars
      integer :: n

      n = len(chars)
      call this%ensure_capacity(n)
      this%chars(:) = chars
      this%m_length = n
   end function

   pure subroutine ensure_capacity(this, cap)
      class(string_builder), intent(inout) :: this
      integer, intent(in) :: cap

      if (.not. allocated(this%chars)) then
         allocate (character(len=cap) :: this%chars)
      else if (this%capacity() < cap) then
         if (this%m_length == 0) then
            deallocate (this%chars)
            allocate (character(len=cap) :: this%chars)
         else
            block
               character(len=this%m_length) :: tmp

               tmp = this%chars(1:this%m_length)
               deallocate (this%chars)
               allocate (character(len=cap) :: this%chars)
               this%chars(1:this%m_length) = tmp
            end block
         end if
      end if
   end subroutine

   pure subroutine push_chars(this, chars)
      class(string_builder), intent(inout) :: this
      character(len=*), intent(in) :: chars

      integer :: n_new

      n_new = this%length() + len(chars)
      call this%ensure_capacity(n_new)
      this%chars(this%m_length + 1:) = chars
      this%m_length = n_new
   end subroutine

   pure subroutine push_std_string_type(this, str)
      class(string_builder), intent(inout) :: this
      type(string_type), intent(in) :: str

      call this%ensure_capacity(0)
      call this%push(char(str))
   end subroutine

   pure subroutine push_line_chars(this, chars)
      class(string_builder), intent(inout) :: this
      character(len=*), intent(in) :: chars
      call this%push(chars)
      call this%push(new_line('a'))
   end subroutine

   pure subroutine push_line_std_string_type(this, str)
      class(string_builder), intent(inout) :: this
      type(string_type), intent(in) :: str
      call this%push(str)
      call this%push(new_line('a'))
   end subroutine

   integer pure function length(this)
      class(string_builder), intent(in) :: this

      length = this%m_length
   end function

   integer pure function capacity(this)
      class(string_builder), intent(in) :: this

      if (allocated(this%chars)) then
         capacity = len(this%chars)
      else
         capacity = 0
      end if
   end function

   pure function to_chars(this) result(chars)
      class(string_builder), intent(in) :: this
      character(len=:), allocatable :: chars

      if (this%m_length > 0) then
         chars = this%chars(1:this%m_length)
      else
         chars = ''
      end if
   end function
end module
