submodule (TBModel) BandCalculation
  contains

  module procedure BandCalc
    implicit none 
    complex(dp), parameter  :: JJ = (0.0d0, 2.0d0)
    integer :: Phi 
    integer  :: i, j, k, l, m, n
    
    complex(dp), allocatable    :: HH(:,:), SS(:,:), U(:,:), Ut(:,:)
    complex(dp), allocatable    :: lambda(:)
    real(dp)   , allocatable    :: rlambda(:)


  ! Allocation
  allocate(HH(MSize, MSize))
  allocate(SS(MSize, MSize))
  allocate( U(MSize, MSize))
  allocate(Ut(MSize, MSize))
  allocate(lambda(MSIZE))
  allocate(rlambda(MSIZE))
  allocate(EigVal(MSize))

  ! Initialize with zeros
  HH = (0.0d0, 0.0d0)
  SS = (0.0d0, 0.0d0)
  U  = (0.0d0, 0.0d0)
  Ut = (0.0d0, 0.0d0)


  do i = 1, nFock 
    l = iRn(i, 1)
    m = iRn(i, 2)
    n = iRn(i, 3)

    Phi = l * Kp(1) + m * Kp(2) + n * Kp(3)
    do j = 1, MSize
      do k = 1, MSize
        HH(j,k) = HH(j,k) + H(i, j,k)! * exp(J*Phi*pi)
        SS(j,k) = SS(j,k) + S(i, j,k)! * exp(J*Phi*pi)
      enddo
    enddo
  enddo

  ! todo Begin lowdin Diagonalization
  call eig(SS, lambda, right = U)   
  !Ut = TRANSPOSE(CONJG(U))              
  U  = TRANSPOSE(U) 
  Ut = .inv.TRANSPOSE(U)

  !Do SQRT(S) 
  SS = (0.0d0, 0.0d0)                   
  lambda = SQRT(lambda)               
  
  do i = 1, MSize                       
    SS(i,i) = lambda(i)                
  enddo                                 
  ! done SQRT(S) 
  SS = .inv.SS                          
                                        
  SS = MATMUL(MATMUL(U, SS), Ut)        
  HH = MATMUL(MATMUL(SS, HH), SS)       
  
  call eigh(HH, rlambda)                
  print*, rlambda
  end procedure BandCalc

end submodule BandCalculation
