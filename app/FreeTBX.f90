program FreeTBX
  use TBModel
  use TermTools

  implicit none
  real BeginTime, StepTime, TotalTime

  call MainTitle("FreeTBX: A Modern Fortran Tight Binding Exploration Code", "Nathanael N. Batista", "nnardoto@gmail.com", "v0.0.1")
  call CPU_TIME(BeginTime) 
  ! Catch arguments from terminal and load system
  call LoadSystem()
  call PathCalc()
  call CPU_TIME(TotalTime)
  call TitleBox("End Calculations") 
  print*, TotalTime - StepTime, 's'
end program
