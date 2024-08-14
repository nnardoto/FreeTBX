program FreeTBX
  use TBModel

  implicit none
  integer, dimension(3) :: RR
  RR = 0

  ! Catch arguments from terminal and load system
  call LoadSystem()
  print*, BandCalc(RR)*27.2114

end program
