program FreeTBX
  use TBModel

  implicit none

  ! Catch arguments from terminal and load system
  call LoadSystem()
  call PathCalc()

end program
