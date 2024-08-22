submodule (TBModel) PathCalculation
  contains

  module procedure PathCalc
    implicit none
    integer                ::  i, j, k, TotKp, fp
    character(len=:), allocatable :: BandFileName
    real(dp), dimension(3) ::  dk
    real(dp), allocatable  ::  Bands(:,:)

    ! Allocate Memory for BandCalculation
    TotKp = sum(nPath) - nPath(size(nPath)) + 1
    print*, TotKp
    allocate(Bands(TotKp, MSize))
    k = 1
    do i = 1, size(nPath) - 1
      dk = (FullPath(i + 1, :) - FullPath(i, :)) / (nPath(i))
      do j = 1, nPath(i)
        Bands(k, :) = BandCalc(FullPath(i, :) + (j - 1)*dk)
        k = k + 1
      enddo
    enddo
    Bands(k, :) =  BandCalc(FullPath(size(nPath), :))

    !Escreve Bandas
    BandFileName = trim(SystemName) // "_band.dat"
    open(newunit = fp, file = BandFileName, action = 'write' )
    do j = 1, MSize
      do i = 1, TotKp
        write(fp, *), i, Bands(i, j)
    enddo
      write(fp, *), ''
    enddo
    close(fp)

  end procedure PathCalc

end submodule PathCalculation
