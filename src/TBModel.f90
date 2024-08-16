module TBModel
  use fdf
  use iso_fortran_env, only : dp=>real64
  use stdlib_linalg, only: eig, eigh, eye, is_hermitian, operator(.inv.) 
  
  implicit none

  ! System information to load system
  public :: LoadSystem
  public :: LoadHamiltonian
  public :: BandCalc
  
  private
    real(dp), parameter  :: pi = 3.1415926535897932384626433832795028841971693
    character(len = 200) :: SystemName, HFile, SFile
    integer :: MSize, nFock
    logical :: isOrthogonal
    integer, allocatable :: Degen(:), iRn(:,:)
    complex(dp), allocatable :: H(:,:,:), S(:,:,:)

  interface
 
    ! load and build the tight binding model for system
    module subroutine LoadSystem()
    end subroutine

    module subroutine SysConfig() 
    end subroutine                      
    
    module subroutine LoadHamiltonian()
    end subroutine

    module subroutine LoadOverlap() 
    end subroutine                     

    ! To BandCalculation
    module subroutine BandCalc(Kp, EigVal)
      integer,  dimension(3)     :: kp
      real, allocatable      :: EigVal(:) 
    end subroutine

  end interface

end module
