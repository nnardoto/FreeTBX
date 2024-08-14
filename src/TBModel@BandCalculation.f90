submodule (TBModel) BandCalculation
  contains

  module procedure BandCalc
    implicit none 
    complex, parameter  :: J = (0.0d0, 2.0d0)
    integer :: Phi 
    integer  :: i, l, m, n
    
    complex, allocatable :: HH(:,:), SS(:,:), U(:,:), Ut(:,:)
    real, allocatable    :: lambda(:,:), LL(:,:)
    



  ! Allocation
  allocate(EigVal(MSize))
  allocate(HH(MSize, MSize))
  allocate(SS(MSize, MSize))
  allocate(U(MSize, MSize))
  allocate(Ut(MSize, MSize))
  allocate(lambda(MSIZE, 1))
  allocate(LL(MSIZE, MSIZE))

  ! Initialize with zeros
  HH = (0.0d0, 0.0d0)
  SS = (0.0d0, 0.0d0)

  do i = 1, MSIZE 
    l = iRn(1, i)
    m = iRn(2, i)
    n = iRn(3, i)

    Phi = Kp(1) * l + Kp(2) * m + Kp(3) * n
    
    HH = HH + H(:,:, i) * exp(J*Phi*pi)
    SS = SS + S(:,:, i) * exp(J*Phi*pi)
  enddo

  ! todo Begin lowdin Diagonalization
  call eigh(SS, lambda(:, 1), vectors = U)
  SS = SQRT(MATMUL(eye(MSIZE, MSIZE), lambda))
  Ut = TRANSPOSE(CONJG(U))
  SS = MATMUL(MATMUL(Ut, SS), U)
  SS = .inv.SS
  HH = MATMUL(MATMUL(SS, HH), SS)
  call eigh(HH, lambda(:, 1))
  
  print*, lambda
  
  end procedure BandCalc

end submodule BandCalculation
