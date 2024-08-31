submodule (TBModel) BandCalculation
    contains
        module procedure BandCalc
            implicit none 
            complex(dp), parameter  :: JJ = (0.0d0, 2.0d0)
            complex(dp) :: Phi 
            complex(dp), allocatable :: HH(:,:)
            integer  :: i, j, k, l, m, n
        
            ! Allocation
            allocate(HH(MSize, MSize))
            allocate(EigVal(MSize)) 
            
            ! Initialize with zeros
            !    HH = (0.0d0, 0.0d0)
            do i = 1, nFock 
                l = iRn(i, 1)
                m = iRn(i, 2)
                n = iRn(i, 3)
                
                Phi = JJ*pi*(l * Kp(1) + m * Kp(2) + n * Kp(3))
                
                
                HH = HH + H(i, :,:) * exp(Phi)
            enddo
            ! todo lowdin Diagonalization

            ! Eigenvalue Calculation 
            call mfi_heevd(HH, EigVal) 
        end procedure BandCalc

        function sqrtmat(A) result(B)
            complex(dp), allocatable    :: A(:,:), B(:,:), L(:,:)
            real(dp),    allocatable    :: LL(:,:)
            real(dp)                    :: Lambda(MSize, 1)
        end function
end submodule BandCalculation
