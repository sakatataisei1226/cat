# EMSES-CoToCoA-sample
月面EMSESとCoToCoAを使うときのサンプルコード

クローン
```
git clone https://github.com/kmr-gks/EMSES-CoToCoA-sample.git
```


```
cd EMSES-CoToCoA-sample
```

EMSESのクローン
```
git clone https://github.com/Nkzono99/MPIEMSES3D.git
```


```
mv MPIEMSES3D requester
```


requesterの内のコードを一部変更する必要がある。

`src/main/esses.F90`
メインループ部分(`esses_mainstep`を呼び出す前か後にこのコードを追加する)

```fortran
if(usecotocoa.eq.1) call send_data
```

`src/cotocoa/ictcar.F90`
以下の内容に書き換える
```fortran
module m_ictcar
    use oh_type
    use paramt
    use allcom
#define OH_LIB_LEVEL 3
#include "ohhelp_f.h"
    implicit none
    private
    public ccinit, send_data

    !共有するphiの配列
    real*8,allocatable    :: phi_data(:)
    !共有する配列のサイズ
    integer(kind=8)               :: phi_data_size=35
    !共有領域のエリアID
    integer               :: phi_areaid

contains

!requesterは領域を登録することと、リクエストを送ることを行う
subroutine ccinit
!
!   ____________________________________________________________
!
!               S U B R O U T I N E   C C I N I T
!   ____________________________________________________________
!
!   ............................................................
!   .                                                          .
!   ............................................................
!
!-------------------- initialization for CoToCoA (ex. regarea)

    implicit none
!
    
! 

!-------------------- 
    !共有する配列の領域を確保
    allocate(phi_data(phi_data_size))

!-------------------- 
    !エリアIDを取得
    call CTCAR_regarea_real8(phi_data,phi_data_size,phi_areaid)

    return
end subroutine ccinit

!send data with cotocoa
subroutine send_data
    use allcom
    implicit none
    
    !リクエストを送るときのデータ
    integer(kind=4) ::req_params(10)
    integer(kind=4) ::x,y,z,phi_shape(5),i
    integer ::from_rank=10

    if(myid.eq.from_rank) then
        phi_shape=shape(phi)
        x=phi_shape(2)
        y=phi_shape(3)
        z=phi_shape(4)
        phi_data = phi(1,1:phi_data_size,y/2,z/2,1)
        print*, "CTCArequester: phi_data=", phi_data
        !リクエスト時のデータを設定
        req_params(1)=from_rank
        req_params(2)=phi_data_size
        !リクエスト時にデータを送ることができる
        call CTCAR_sendreq(req_params,size(req_params))
    end if
end subroutine send_data

end module m_ictcar
```


moonフォルダで`make`する

default-condition,dshield1,exp_surfaceのうちの一つ
シミュレーションフォルダに移動し、sbatchする


