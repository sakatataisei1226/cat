program coupler
  use mpi
  use ctca
  implicit none
!
  integer(kind=4) :: ierr, myrank, nprocs
  !エリアID
  integer(kind=4) :: phi_areaid
  integer(kind=4) :: reqinf(4)
  integer(kind=4) :: frmrank, progid
  integer(kind=4) :: req_params(10)
!
  call CTCAC_init()
  call MPI_Comm_size(CTCA_subcomm, nprocs, ierr)
  call MPI_Comm_rank(CTCA_subcomm, myrank, ierr)
!
! エリアIDを取得
  call CTCAC_regarea_real8(phi_areaid)
!
  do while (.true.)
    call CTCAC_pollreq(reqinf,frmrank,req_params,size(req_params))
    if( CTCAC_isfin() ) exit
    progid = 0
    call CTCAC_enqreq(reqinf,progid,req_params,size(req_params))
  end do
!
  call CTCAC_finalize()
!
  stop
end program coupler
