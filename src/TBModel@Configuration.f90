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
      call fdf_init(FileName, "log")      


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
      print*, trim(SystemName)
      print*, trim(HFile)
      print*, trim(SFile)
    end procedure LoadSystem                               


    module procedure LoadHamiltonian
      implicit none
      integer :: fp
      integer :: i, ii, jj, l, m, n
      real(kind = 4) :: R, Im
      
      ! todo passar para a definição do módulo
      integer :: MSize, nFock, N, nM, idx
      integer, allocatable :: Degen(:)
      integer, allocatable :: iRn(:,:)
      complex(kind = 4), allocatable :: H(:,:,:), S(:,:,:)

      open(action = 'read', file = HFile, newunit = fp)
        read(fp, *)
        read(fp, *) MSize
        read(fp, *) nFock

        print*, MSize, nFock

        allocate(Degen(nFock))
        allocate(H(nFock, MSize, MSize))
        allocate(S(nFock, MSize, MSize))
        allocate(iRn(nFock, 3))

        ! 15 elements by line
        do i = 1, (nFock / 15)
          print*, i
          read(fp, *) Degen((i - 1)*15 + 1:(i - 1)*15 + 15)
        enddo
       
        ! Last line of degenerecences
        read(fp, *) Degen((i - 1)*15 + 1:(i - 1)*15 + MOD(nFock, 15))


        ! Begin Hamiltonian Read
        nM = MSize * MSize
        N = nFock*nM
        do i = 1, N 
          read(fp, *) l, m, n, ii, jj, R, Im
          idx = i / nM + 1 - (nM - mod(i, nM))/nM
          iRn(idx, 1) = l
          iRn(idx, 2) = m
          iRn(idx, 3) = n
          H(idx, ii, jj) = complex(R, Im)
        enddo
      close(fp)
    end procedure

end submodule Configuration                      
