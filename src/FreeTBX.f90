module FreeTBX
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, FreeTBX!"
  end subroutine say_hello
end module FreeTBX
