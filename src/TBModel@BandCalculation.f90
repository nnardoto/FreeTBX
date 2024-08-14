submodule (TBModel) BandCalculation
  contains

  module procedure BandCalc
    use iso_fortran_env, only : dp=>real64
    use stdlib_linalg, only : eigh, operator(.inv.)

    implicit none 
    complex(dp), allocatable :: HH(:,:), SS(:,:), U(:,:), Ut(:,:), sqrtSS(:,:)
    real(dp), allocatable    :: lambda(:)
    
    integer :: Phi 
    integer  :: i, l, m, n
    complex, parameter  :: J = (0.0d0, 2.0d0)


  ! Allocation
  allocate(HH(MSize, MSize))
  allocate(SS(MSize, MSize))
 
  allocate(EigVal(MSize))

  allocate(U(MSize, MSize))
  allocate(Ut(MSize, MSize))
  allocate(sqrtSS(MSize, MSize))
  allocate(lambda(MSize))

  ! Initialize with zeros
  HH = (0.0d0, 0.0d0)
  SS = (0.0d0, 0.0d0)


  do i = 1, MSIZE 
    l = iRn(i, 1)
    m = iRn(i, 2)
    n = iRn(i, 3)

    Phi = Kp(1) * l + Kp(2) * m + Kp(3) * n
    
    HH = HH + H(i,:,:) * exp(J*Phi*pi)
    SS = SS + S(i,:,:) * exp(J*Phi*pi)
  enddo

  ! Begin lowdin Diagonalization
  call eigh(SS, lambda, vectors=U)
  Ut = transpose(.inv.U)
  sqrtSS = MATMUL(MATMUL(Ut, SS), U)
  sqrtSS = SQRT(sqrtSS)
  sqrtSS = MATMUL(MATMUL(U, sqrtSS), Ut)
  
  HH = MATMUL(MATMUL(sqrtSS, HH), sqrtSS)
  call eigh(HH, lambda)
  
  EigVal = lambda

  end procedure BandCalc

end submodule BandCalculation
