program coupler
  use mpi
  use ctca
  implicit none

  integer(kind=4) :: ierr, myrank, nprocs
  integer(kind=4) :: phi_areaid, recv_areaid
  integer(kind=4) :: reqinf(4), frmrank, progid
  integer(kind=4) :: req_params(10)
  integer(kind=8) :: phi_data_size, recv_data_size
  real(8), allocatable :: phi_data(:)
  real(8), allocatable :: recv_data(:)

  call CTCAC_init()
  call MPI_Comm_size(CTCA_subcomm, nprocs, ierr)
  call MPI_Comm_rank(CTCA_subcomm, myrank, ierr)

  ! エリア登録（requester/workerと同じ順序・個数で！）
  call CTCAC_regarea_real8(phi_areaid)
  call CTCAC_regarea_real8(recv_areaid)

  ! 必要に応じてサイズをセット
  phi_data_size = 35
  recv_data_size = 35
  allocate(phi_data(phi_data_size))
  allocate(recv_data(recv_data_size))

  do while (.true.)
    ! 1. requester→workerへのリクエスト転送
    call CTCAC_pollreq(reqinf, frmrank, req_params, size(req_params))
    if (CTCAC_isfin()) exit
    progid = 0
    call CTCAC_enqreq(reqinf, progid, req_params, size(req_params))

    ! 2. worker→requesterへの応答データ転送
    ! workerがrecv_areaidにデータを書き込んだら、それをrequesterに転送
    ! ここではrecv_areaidのデータを転送する例
    call CTCAC_readarea_real8(recv_areaid, frmrank, 0_8, recv_data_size, recv_data)
    call CTCAC_writearea_real8(recv_areaid, frmrank, 0_8, recv_data_size, recv_data)
  end do

  call CTCAC_finalize()
  stop
end program coupler
