program worker
  use mpi
  use ctca
  implicit none

  integer :: ierr, myrank, nprocs
  integer :: phi_areaid, recv_areaid
  integer :: i
  integer :: from_rank, req_params(10)
  integer :: read_data_size, send_data_size
  integer :: request_id
  real(8), allocatable :: read_data(:)
  real(8), allocatable :: send_data(:)

  call CTCAW_init(0, 1)
  call MPI_Comm_size(CTCA_subcomm, nprocs, ierr)
  call MPI_Comm_rank(CTCA_subcomm, myrank, ierr)
  print*, "worker: ", myrank, " / ", nprocs

  call CTCAW_regarea_real8(phi_areaid)
  call CTCAW_regarea_real8(recv_areaid)

  do while(.true.)
    call CTCAW_pollreq(from_rank, req_params, size(req_params))
    if (CTCAW_isfin()) exit

    ! リクエストIDを取得
    request_id = req_params(3)
    print *, "worker: received request_id=", request_id

    read_data_size = req_params(2)
    if (.not. allocated(read_data)) then
      allocate(read_data(read_data_size))
    end if

    call CTCAW_readarea_real8(phi_areaid, from_rank, 0, read_data_size, read_data)
    print*, "worker: read_data=", read_data

    send_data_size = read_data_size
    if (.not. allocated(send_data)) then
      allocate(send_data(send_data_size))
    end if
    do i = 1, send_data_size
      send_data(i) = 1000.0d0 + i + myrank
    end do

    ! 応答データを返す
    call CTCAW_writearea_real8(recv_areaid, from_rank, 0, send_data_size, send_data)
    print*, "worker: sent response to requester, send_data=", send_data, "for request_id=", request_id

    call CTCAW_complete()
  end do

  print*, "worker is finalizing..."
  call CTCAW_finalize()
  stop
end program worker
