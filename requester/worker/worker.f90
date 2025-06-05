program worker
  use ctca
  implicit none

  integer :: phi_areaid, recv_areaid
  integer(kind=8) :: phi_data_size, recv_data_size
  real(8), allocatable :: phi_data(:)
  real(8), allocatable :: send_data(:)
  integer :: i, j, from_rank

  call CTCAW_init(0, 1)

  phi_data_size = 10
  allocate(phi_data(phi_data_size))
  call CTCAW_regarea_real8(phi_areaid)

  recv_data_size = 10
  allocate(send_data(recv_data_size))
  call CTCAW_regarea_real8(recv_areaid)

  from_rank = 0 ! requesterのランク

  do i = 1, 10
      ! requesterからphiデータを受信
      call CTCAW_readarea_real8(phi_areaid, from_rank, 0, phi_data_size, phi_data)
      print *, "worker: received phi_data =", phi_data

      ! サンプルデータを作成して送り返す
      send_data = 1000.0d0 * i + [(j, j=1,recv_data_size)]
      call CTCAW_writearea_real8(recv_areaid, from_rank, 0, recv_data_size, send_data)
      print *, "worker: sent data to requester =", send_data
  end do

  call CTCAW_finalize()
  stop
end program worker
