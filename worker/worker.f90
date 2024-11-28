program worker
  use mpi
  use ctca
  implicit none
!
  integer :: ierr, myrank, nprocs
  !エリアID
  integer :: phi_areaid
  integer :: i
  integer :: from_rank
  !リクエストを受け取るときのデータ
  integer ::req_params(10)
  !受け取るデータとそのサイズ
  real*8,allocatable    :: read_data(:)
  integer :: read_data_size
!
  call CTCAW_init(0, 1)
  call MPI_Comm_size(CTCA_subcomm, nprocs, ierr)
  call MPI_Comm_rank(CTCA_subcomm, myrank, ierr)
!
  print*, "worker: ", myrank, " / ", nprocs
!
! エリアIDを取得
  call CTCAW_regarea_real8(phi_areaid)
!
  do while( .true. )
    !リクエストを受けとる
    call CTCAW_pollreq(from_rank,req_params,size(req_params))
    if( CTCAW_isfin() ) exit
    !リクエスト時のデータをもとに受け取るデータの情報をみる
    from_rank = req_params(1)
    read_data_size = req_params(2)
    !初回のみ領域確保
    if(.not.allocated(read_data)) then
      allocate(read_data(read_data_size))
    end if

    !read_dataにデータを読み込む
    call CTCAW_readarea_real8(phi_areaid,from_rank,0,read_data_size,read_data)
    print*, "CTCAworker: read_data="
    do i = 1, read_data_size
      write(*, "(A,E)", advance="no") ",", read_data(i)
    end do

    call CTCAW_complete()
  end do

  print*, "worker is finalizing..."
  call CTCAW_finalize()
!
  stop
end program worker
