submodule (TBModel) Configuration           
  contains                                       
                                                 
    ! main routine for load and build the system
    module procedure LoadSystem
      character(len=:), allocatable  :: FileName          
      integer                        :: N                 

      !------------------------------------------------ 
      ! load input file from command-line argument      
      !------------------------------------------------ 
      call get_command_argument(1, length = N)            

      if (N < 1) then
        print*, "THERE IS NO INPUT FILE"
        call exit()  
      endif

      allocate(character(N) :: FileName)
      call get_command_argument(1, FileName)
        
      
      ! -----------------------------------------------
      ! Load using Flexive Data Format Library
      ! -----------------------------------------------
      call fdf_init(FileName, "log.txt")      


      ! -----------------------------------------------
      ! grep all parameters from input file
      ! -----------------------------------------------
      SystemName = fdf_get("SystemName", "null")
      HFile      = fdf_get("HFile"     , "null")
      SFile      = fdf_get("SFile"     , "null")

      ! -----------------------------------------------
      ! grep to do calculations
      ! -----------------------------------------------



      ! Print Data
      print*, SystemName
      print*, HFile
      print*, SFile

    end procedure LoadSystem                               


    module procedure LoadHamiltonian
      integer :: InputFile

      open(action = 'read', file = HFile, newunit = InputFile)
      close(InputFile)

    end procedure

end submodule Configuration                      
