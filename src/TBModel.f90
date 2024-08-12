module TBModel
  use fdf
  
  ! System information to load system
  character(len = 200) :: SystemName, HFile, SFile

  interface
 
    ! load and build the tight binding model for system
    module subroutine LoadSystem()
    end subroutine

    module subroutine LoadHamiltonian()
    end subroutine

  end interface

end module
