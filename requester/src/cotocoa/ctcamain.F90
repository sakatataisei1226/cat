module m_ctcamain
    use oh_type
    use ctca
    use paramt
    use allcom
#define OH_LIB_LEVEL 3
#include "ohhelp_f.h"
    implicit none
    private
    public cotocoa_init, cotocoa_mainstep, cotocoa_finalize

    !共有するphiの配列
    real*8,allocatable    :: phi_data(:)
    !共有する配列のサイズ
    integer(kind=8)       :: phi_data_size=35
    !共有領域のエリアID
    integer               :: phi_areaid
    !共有する受信データの配列
    real*8,allocatable    :: recv_data(:)
    !受信データのサイズ
    integer(kind=8)       :: recv_data_size=10

    integer :: recv_areaid

contains

    subroutine cotocoa_init
        !共有する配列の領域を確保
        allocate(phi_data(phi_data_size))
        allocate(recv_data(recv_data_size))
        !エリアIDを取得
        call CTCAR_regarea_real8(phi_data,phi_data_size,phi_areaid)
        call CTCAR_regarea_real8(recv_data,recv_data_size,recv_areaid)
    end subroutine cotocoa_init

subroutine cotocoa_mainstep
    implicit none

    integer(kind=4) :: req_params(10)
    integer(kind=4) :: x, y, z, phi_shape(5), i
    integer :: from_rank = 10
    integer(kind=8) :: req_hdl
    integer, save :: request_id = 0  ! リクエストIDカウンタ

    integer :: resp_hdl
    integer :: stat
    logical :: finished

    if (myid .eq. from_rank) then
        request_id = request_id + 1  ! リクエストごとにインクリメント

        phi_shape = shape(phi)
        x = phi_shape(2)
        y = phi_shape(3)
        z = phi_shape(4)
        phi_data = phi(1, 1:phi_data_size, y/2, z/2, 1)
        print *, "requester: phi_data=", phi_data

        req_params(1) = from_rank
        req_params(2) = phi_data_size
        req_params(3) = request_id   ! リクエストIDを埋め込む

        call CTCAR_sendreq_hdl(req_params, size(req_params), req_hdl)
        print *, "requester: sent request, req_hdl=", req_hdl, "request_id=", request_id

        finished = .false.
        do while (.not. finished)
            call CTCAR_wait(req_hdl)
            if (req_hdl == 0) then
                print *, "requester: received response from worker, recv_data=", recv_data
                print *, "requester: response for request_id=", request_id
                finished = .true.
            else
                call sleep(1)
            end if
        end do
    end if

end subroutine cotocoa_mainstep


    subroutine cotocoa_finalize
  
    end subroutine cotocoa_finalize

end module m_ctcamain
