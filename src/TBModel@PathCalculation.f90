submodule (TBModel) PathCalculation
  contains

  module procedure PathCalc
    implicit none
    integer                ::  i, j, k, TotKp
    real(dp), dimension(3) ::  dk
    real(dp), allocatable  ::  Bands(:,:)

    ! Allocate Memory for BandCalculation
    TotKp = sum(nPath) - nPath(size(nPath)) + 1
    allocate(Bands(TotKp, MSize))
    k = 1
    do i = 1, size(nPath) - 1
      dk = (FullPath(i + 1, :) - FullPath(i, :)) / (nPath(i))
      do j = 1, nPath(i)
        Bands(k, :) = BandCalc(FullPath(i, :) + (j - 1)*dk)
        k = k + 1
      enddo
    enddo
    !print*, k
    !Bands(k, :) =  FullPath(size(nPath), :)

    !Escreve Bandas
    do j = 1, MSize
      do i = 1, TotKp
        print*, i, Bands(i, j)
      enddo
        print*, ''
    enddo

  end procedure PathCalc

end submodule PathCalculation
