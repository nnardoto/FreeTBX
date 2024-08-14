program FreeTBX
  use TBModel

  implicit none
  integer, dimension(3) :: RR
  real, allocatable :: EigVal(:)
  RR = 0

  ! Catch arguments from terminal and load system
  call LoadSystem()
  print*, "TAMO AI"
  call BandCalc(RR, EigVal)

end program
