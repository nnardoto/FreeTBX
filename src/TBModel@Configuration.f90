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
      ! todo organizaton of calculations to do
      

      ! Allocation of Matrices of System
      call SysConfig()
    end procedure LoadSystem                               

    module procedure SysConfig
      implicit none

      integer :: hr_MSize, olp_MSize, hr_num, olp_num
      integer :: hrFile, olpFile

      ! =============================================================================
      !                          Minimal Consistence check
      ! =============================================================================
      open(action = 'read', file = HFile, newunit =  hrFile)
        read( hrFile, *)
        read( hrFile, *)  hr_MSize
        read( hrFile, *)  hr_num
        isOrthogonal = .true.
      close(hrFile)

      if(SFile /= "null") then 
        open(action = 'read', file = SFile, newunit = olpFile)
          read(olpFile, *)
          read(olpFile, *) olp_MSize
          read(olpFile, *) olp_num
          isOrthogonal = .false.
          print*, "Non Orthogonal System"
        close(olpFile)
      endif
        
      if(hr_MSize /= olp_MSize) then
        print*, "The Size of Overlap Matrices are not equal to Fock Matrices"
        call exit()
      endif

      if(hr_num /= olp_num) then                                          
        print*, "The Number of Overlap Matrices are not equal to Fock Matrices" 
        call exit()                                                           
      endif                                                                   

      ! ============================================================================= 
      !                             Allocation of Matrices                            
      ! ============================================================================= 
      print*, "Begin Allocation Process"
      MSize = hr_MSize
      nFock = hr_num
      allocate(Degen(nFock))           
      allocate(H(nFock, MSize, MSize)) 
      allocate(S(nFock, MSize, MSize)) 
      allocate(iRn(nFock, 3))         
      print*, "Allocation Successful"

      call LoadHamiltonian()
      if(isOrthogonal .eqv. .false.) then
        call LoadOverlap()
      endif

    end procedure SysConfig

    module procedure LoadHamiltonian
      implicit none
      integer :: fp
      integer :: i, ii, jj, l, m, n
      integer :: N, nM, idx
      real(kind = 4) :: R, Im
      
      open(action = 'read', file = HFile, newunit = fp)
        read(fp, *)
        read(fp, *) 
        read(fp, *) 

        ! 15 elements by line
        do i = 1, (nFock / 15)
          read(fp, *) Degen((i - 1)*15 + 1:(i - 1)*15 + 15)
        enddo
       
        ! Last line of degenerecences
        read(fp, *) Degen((i - 1)*15 + 1:(i - 1)*15 + MOD(nFock, 15))

        ! Begin Hamiltonian Read
        nM = MSize * MSize        ! Number of Elements in each matrix
        N = nFock*nM              ! Total Number of Elements

        do i = 1, N 
          read(fp, *) l, m, n, ii, jj, R, Im
          idx = i / nM + 1 - (nM - mod(i, nM))/nM
          iRn(idx, 1) = l
          iRn(idx, 2) = m
          iRn(idx, 3) = n
          H(idx, ii, jj) = complex(R, Im)
        enddo
      close(fp)
    end procedure LoadHamiltonian

    module procedure LoadOverlap                                  
      implicit none                                                   
      integer :: fp                                                   
      integer :: i, ii, jj, l, m, n                                   
      integer :: N, nM, idx                                           
      real(kind = 4) :: R, Im                                         
                                                                      
      open(action = 'read', file = HFile, newunit = fp)               
        read(fp, *)                                                   
        read(fp, *)                                              
        read(fp, *)                                              
                                                                      
        ! 15 elements by line                                         
        do i = 1, (nFock / 15)                                        
          read(fp, *) Degen((i - 1)*15 + 1:(i - 1)*15 + 15)           
        enddo                                                         
                                                                      
        ! Last line of degenerecences                                 
        read(fp, *) Degen((i - 1)*15 + 1:(i - 1)*15 + MOD(nFock, 15)) 
                                                                      
                                                                      
        ! Begin Hamiltonian Read                                      
        nM = MSize * MSize        ! Number of Elements in each matrix 
        N = nFock*nM              ! Total Number of Elements          

        do i = 1, N                                                   
          read(fp, *) l, m, n, ii, jj, R, Im                          
          idx = i / nM + 1 - (nM - mod(i, nM))/nM                     
          iRn(idx, 1) = l                                             
          iRn(idx, 2) = m                                             
          iRn(idx, 3) = n                                             
          S(idx, ii, jj) = complex(R, Im)                             
        enddo                                                         
      close(fp)                                                       
    end procedure LoadOverlap                                                     
                                                                      
end submodule Configuration                                           
