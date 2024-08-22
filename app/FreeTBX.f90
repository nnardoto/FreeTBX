program FreeTBX
  use TBModel

  implicit none
  real BeginTime, StepTime, TotalTime

  TotalTime = 0.0d0
  call CPU_TIME(BeginTime)
  ! Catch arguments from terminal and load system
  call LoadSystem()
  call CPU_TIME(StepTime)
  print*, "Time: ", StepTime - BeginTime
  call PathCalc()
  call CPU_TIME(StepTime)
  print*, "Time: ", StepTime - BeginTime
  

end program
