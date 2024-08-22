submodule (TBModel) BandCalculation
  contains

  module procedure BandCalc
    implicit none 
    complex(dp), parameter  :: JJ = (0.0d0, 2.0d0)
    complex(dp) :: Phi 
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

    Phi = JJ*pi*(l * Kp(1) + m * Kp(2) + n * Kp(3))
    

    HH = HH + H(i, :,:) * exp(Phi)
    SS = SS + S(i, :,:) * exp(Phi)
  enddo

  ! todo Begin lowdin Diagonalization
  !icall eigh(HH, rlambda, vectors = U)   
!  SS = MATMUL(MATMUL(U, SS), Ut)        
!  HH = MATMUL(MATMUL(SS, HH), SS)       
  
  call eigh(HH, rlambda)                
  EigVal =  rlambda
  end procedure BandCalc

  function sqrtmat(A) result(B)
    complex(dp), allocatable :: A(:,:), B(:,:), L(:,:)
    real(dp), allocatable    :: LL(:,:)
    real(dp)     :: Lambda(MSize, 1)

  end function

end submodule BandCalculation
