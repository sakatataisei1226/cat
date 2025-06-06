!
  module wrapper
!
    use ssl2ex
    implicit none
    include 'fftw3.f'
    interface ranu0
      module procedure dranu0s,dranu0,dranu0l
    end interface ranu0
    interface rann0
      module procedure drann0s,drann0,drann0l
    end interface rann0

    integer(kind=4),private :: seed
    integer(kind=8),private :: fftplan(3,2)
    real(kind=8),allocatable,private :: workarray(:)

    contains
!
  subroutine randinit0(iseed)
!
!   ____________________________________________________________
!
!               S U B R O U T I N E   R A N D I N I T 0
!   ____________________________________________________________
!
!   ............................................................
!   .                                                          .
!   .   Initialization of Mersenne Twister, giving a seed      .
!   .   as a positive integer.                                 .
!   .   Designed for Cray XE6 system at Kyoto University.      .   
!   ............................................................
!
!-------------------- parameter and common blocks
  implicit none
!
  integer(kind=4),intent(in) :: iseed


!-------------------- 
      seed = iseed
      call MT_INIT(seed)


  return
  end subroutine randinit0



  subroutine dranu0s(random,nrandom,icon)
!
!   ____________________________________________________________
!
!               S U B R O U T I N E   D R A N U 0 S
!   ____________________________________________________________
!
!   ............................................................
!   .                                                          .
!   .   Wrapper for the uniform random number generation,      .
!   .   which internally invokes the Mersenne Twister.         .
!   .   Designed for Cray XE6 system at Kyoto University.      .   
!   ............................................................
!
!-------------------- parameter and common blocks
  implicit none
!
  integer(kind=4),intent(in) :: nrandom
  integer(kind=4),intent(out) :: icon
  real(kind=8),intent(out) :: random
!
  real(kind=8),external :: mtrand_real8
!


!-------------------- 
      random = mtrand_real8()
!
      if(nrandom.eq.1) then
        icon = 0
      else
        icon = 1
        print*, "Warning(RANU0): only one random number generated"
      end if


  return
  end subroutine dranu0s



  subroutine dranu0(random,nrandom,icon)
!
!   ____________________________________________________________
!
!               S U B R O U T I N E   D R A N U 0
!   ____________________________________________________________
!
!   ............................................................
!   .                                                          .
!   .   Wrapper for the uniform random number generation,      .
!   .   which internally invokes the Mersenne Twister.         .
!   .   Designed for Cray XE6 system at Kyoto University.      .   
!   ............................................................
!
!-------------------- parameter and common blocks
  implicit none
!
  integer(kind=4),intent(in) :: nrandom
  integer(kind=4),intent(out) :: icon
  real(kind=8),intent(out) :: random(nrandom)
!
  real(kind=8),external :: mtrand_real8
!
  integer(kind=4) :: i


!-------------------- 
      do i=1,nrandom
        random(i) = mtrand_real8()
      end do
      icon = 0


  return
  end subroutine dranu0



  subroutine dranu0l(random,nrandom,icon)
!
!   ____________________________________________________________
!
!               S U B R O U T I N E   D R A N U 0 L
!   ____________________________________________________________
!
!   ............................................................
!   .                                                          .
!   .   Wrapper for the uniform random number generation,      .
!   .   which internally invokes the Mersenne Twister.         .
!   .   Designed for Cray XE6 system at Kyoto University.      .   
!   ............................................................
!
!-------------------- parameter and common blocks
  implicit none
!
  integer(kind=8),intent(in) :: nrandom
  integer(kind=4),intent(out) :: icon
  real(kind=8),intent(out) :: random(nrandom)
!
  real(kind=8),external :: mtrand_real8
!
  integer(kind=8) :: i


!-------------------- 
      do i=1,nrandom
        random(i) = mtrand_real8()
      end do
      icon = 0


  return
  end subroutine dranu0l



  subroutine drann0s(am,sd,random,nrandom,icon)
!
!   ____________________________________________________________
!
!               S U B R O U T I N E   D R A N N 0 S
!   ____________________________________________________________
!
!   ............................................................
!   .                                                          .
!   .   Box-Muller normal random number generator, based on    .
!   .   uniform random numbers generated by RANU0.             .   
!   ............................................................
!
!-------------------- parameter and common blocks
  implicit none
!
  integer(kind=4),intent(in) :: nrandom
  integer(kind=4),intent(out) :: icon
  real(kind=8),intent(in) :: am
  real(kind=8),intent(in) :: sd
  real(kind=8),intent(out) :: random
!
  real(kind=8) :: work(2)
  real(kind=8) :: pei2=6.28318530717958646d0


!-------------------- 
      call DRANU0(work,2,icon)
      if(icon.ne.0) then
        print*, "Error(RANN0)a:", icon
        return
      end if
      random = am + sd*sqrt(-2.0d0*log(work(1)))*cos(pei2*work(2))
!
      if(nrandom.ne.1) then
        print*, "Warning(RANN0)d: only one random number generated"
      end if


  return
  end subroutine drann0s



  subroutine drann0(am,sd,random,nrandom,icon)
!
!   ____________________________________________________________
!
!               S U B R O U T I N E   D R A N N 0
!   ____________________________________________________________
!
!   ............................................................
!   .                                                          .
!   .   Box-Muller normal random number generator, based on    .
!   .   uniform random numbers generated by RANU0.             .   
!   ............................................................
!
!-------------------- parameter and common blocks
  implicit none
!
  integer(kind=4),intent(in) :: nrandom
  integer(kind=4),intent(out) :: icon
  real(kind=8),intent(in) :: am
  real(kind=8),intent(in) :: sd
  real(kind=8),intent(out) :: random(nrandom)
!
  integer(kind=4) :: i, id, nn
  real(kind=8) :: work(2)
  real(kind=8) :: pei2=6.28318530717958646d0


!-------------------- 
      if(nrandom.eq.1) then
        call DRANU0(work,2,icon)
        if(icon.ne.0) then
          print*, "Error(RANN0)a:", icon
          return
        end if
        random(nrandom) = am &
       &                + sd*sqrt(-2.0d0*log(work(1)))*cos(pei2*work(2))
      else
        id = 0
        if(nrandom.ne.int(nrandom/2)*2) id = 1
        nn = nrandom - id
        call DRANU0(random,nn,icon)
        if(icon.ne.0) then
          print*, "Error(RANN0)b:", icon
          return
        end if
        do i=1,nn,2
          work(1) = sd*sqrt(-2.0d0*log(random(i)))
          work(2) = pei2*random(i+1)
          random(i) = am + work(1)*cos(work(2))
          random(i+1) = am + work(1)*sin(work(2))
        end do
!
        if(id.eq.1) then
          call DRANU0(work,2,icon)
          if(icon.ne.0)  then
            print*, "Error(RANN0)c:", icon
            return
          end if
          random(nrandom) = am &
         &                + sd*sqrt(-2.0d0*log(work(1)))*cos(pei2*work(2))
        end if
      end if


  return
  end subroutine drann0



  subroutine drann0l(am,sd,random,nrandom,icon)
!
!   ____________________________________________________________
!
!               S U B R O U T I N E   D R A N N 0 L
!   ____________________________________________________________
!
!   ............................................................
!   .                                                          .
!   .   Box-Muller normal random number generator, based on    .
!   .   uniform random numbers generated by RANU0.             .   
!   ............................................................
!
!-------------------- parameter and common blocks
  implicit none
!
  integer(kind=8),intent(in) :: nrandom
  integer(kind=4),intent(out) :: icon
  real(kind=8),intent(in) :: am
  real(kind=8),intent(in) :: sd
  real(kind=8),intent(out) :: random(nrandom)
!
  integer(kind=8) :: i, id, nn
  real(kind=8) :: work(2)
  real(kind=8) :: pei2=6.28318530717958646d0


!-------------------- 
      if(nrandom.eq.1) then
        call DRANU0(work,2,icon)
        if(icon.ne.0) then
          print*, "Error(RANN0)a:", icon
          return
        end if
        random(nrandom) = am &
       &                + sd*sqrt(-2.0d0*log(work(1)))*cos(pei2*work(2))
      else
        id = 0
        if(nrandom.ne.int(nrandom/2)*2) id = 1
        nn = nrandom - id
        call DRANU0L(random,nn,icon)
        if(icon.ne.0) then
          print*, "Error(RANN0)b:", icon
          return
        end if
        do i=1,nn,2
          work(1) = sd*sqrt(-2.0d0*log(random(i)))
          work(2) = pei2*random(i+1)
          random(i) = am + work(1)*cos(work(2))
          random(i+1) = am + work(1)*sin(work(2))
        end do
!
        if(id.eq.1) then
          call DRANU0(work,2,icon)
          if(icon.ne.0)  then
            print*, "Error(RANN0)c:", icon
            return
          end if
          random(nrandom) = am &
         &                + sd*sqrt(-2.0d0*log(work(1)))*cos(pei2*work(2))
        end if
      end if


  return
  end subroutine drann0l



  subroutine fftinit0(rtab,stab,ctab,nxfft,nyfft,nzfft,vbound)
!
!   ____________________________________________________________
!
!               S U B R O U T I N E   F F T I N I T 0
!   ____________________________________________________________
!
!   ............................................................
!   .                                                          .
!   ............................................................
!
!-------------------- parameter and common blocks
  implicit none
!
  integer(kind=4),intent(in) :: nxfft,nyfft,nzfft, vbound(3)
  real(kind=8),intent(in) :: rtab(:,:),stab(:,:),ctab(:,:)


!-------------------- 
      if(size(rtab,1).lt.max(nxfft,nyfft,nzfft)) then
        print '(A,I5,",",\,A,I5,".")', &
       &  "Extent of 1st dim. of rtab should not be lower than ", &
       &  max(nxfft,nyfft,nzfft), &
       &  "  but actually size(rtab,1) = ", size(rtab,1)
      end if
      if(size(stab,1).lt.max(nxfft-1,nyfft-1,nzfft-1)) then
        print '(A,I5,",",/,A,I5,".")', &
       &  "Extent of 1st dim. of stab should not be lower than ", &
       &  max(nxfft-1,nyfft-1,nzfft-1), &
       &  "  but actually size(stab,1) = ", size(stab,1)
      end if
      if(size(ctab,1).lt.max(nxfft+1,nyfft+1,nzfft+1)) then
        print '(A,I5,",",/,A,I5,".")', &
       &  "Extent of 1st dim. of ctab should not be lower than ", &
       &  max(nxfft+1,nyfft+1,nzfft+1), &
       &  "  but actually size(ctab,1) = ", size(ctab,1)
      end if
      if(size(rtab,2).lt.3.or.size(stab,2).lt.3.or.size(ctab,2).lt.3) then
        print '(A,/,A,I3,", ",I3,", ",I3,".")', &
       &  "Extent of 2nd dim. of {r,s,c}tab should not be lower than 3,", &
       &  "  but actually size({r,s,c}tab,2) = ", &
       &  size(rtab,2), size(stab,2), size(ctab,2)
      end if


!-------------------- 
      allocate(workarray(-1:max(nxfft,nyfft,nzfft)+1))


!-------------------- making plans (forward)
      if(vbound(1).eq.0) then
        call dfftw_plan_r2r_1d(fftplan(1,1),nxfft,workarray(0:nxfft-1), &
       &                       rtab(1:nxfft,1),FFTW_R2HC,FFTW_MEASURE)
      else if(vbound(1).eq.1) then
        call dfftw_plan_r2r_1d(fftplan(1,1),nxfft-1,workarray(1:nxfft-1), &
       &                       stab(1:nxfft-1,1),FFTW_RODFT00,FFTW_MEASURE)
      else if(vbound(1).eq.2) then
        call dfftw_plan_r2r_1d(fftplan(1,1),nxfft+1,workarray(0:nxfft), &
       &                       ctab(1:nxfft+1,1),FFTW_REDFT00,FFTW_MEASURE)
      end if
!
      if(vbound(2).eq.0) then
        call dfftw_plan_r2r_1d(fftplan(2,1),nyfft,workarray(0:nyfft-1), &
       &                       rtab(1:nyfft,2),FFTW_R2HC,FFTW_MEASURE)
      else if(vbound(2).eq.1) then
        call dfftw_plan_r2r_1d(fftplan(2,1),nyfft-1,workarray(1:nyfft-1), &
       &                       stab(1:nyfft-1,2),FFTW_RODFT00,FFTW_MEASURE)
      else if(vbound(2).eq.2) then
        call dfftw_plan_r2r_1d(fftplan(2,1),nyfft+1,workarray(0:nyfft), &
       &                       ctab(1:nyfft+1,2),FFTW_REDFT00,FFTW_MEASURE)
      end if
!
      if(vbound(3).eq.0) then
        call dfftw_plan_r2r_1d(fftplan(3,1),nzfft,workarray(0:nzfft-1), &
       &                       rtab(1:nzfft,3),FFTW_R2HC,FFTW_MEASURE)
      else if(vbound(3).eq.1) then
        call dfftw_plan_r2r_1d(fftplan(3,1),nzfft-1,workarray(1:nzfft-1), &
       &                       stab(1:nzfft-1,3),FFTW_RODFT00,FFTW_MEASURE)
      else if(vbound(3).eq.2) then
        call dfftw_plan_r2r_1d(fftplan(3,1),nzfft+1,workarray(0:nzfft), &
       &                       ctab(1:nzfft+1,3),FFTW_REDFT00,FFTW_MEASURE)
      end if


!-------------------- making plans (backward)
      if(vbound(3).eq.0) then
        call dfftw_plan_r2r_1d(fftplan(3,2),nzfft,workarray(0:nzfft-1), &
       &                       rtab(1:nzfft,3),FFTW_HC2R,FFTW_MEASURE)
      else if(vbound(3).eq.1) then
        call dfftw_plan_r2r_1d(fftplan(3,2),nzfft-1,workarray(1:nzfft-1), &
       &                       stab(1:nzfft-1,3),FFTW_RODFT00,FFTW_MEASURE)
      else if(vbound(3).eq.2) then
        call dfftw_plan_r2r_1d(fftplan(3,2),nzfft+1,workarray(0:nzfft), &
       &                       ctab(1:nzfft+1,3),FFTW_REDFT00,FFTW_MEASURE)
      end if
!
      if(vbound(2).eq.0) then
        call dfftw_plan_r2r_1d(fftplan(2,2),nyfft,workarray(0:nyfft-1), &
       &                       rtab(1:nyfft,2),FFTW_HC2R,FFTW_MEASURE)
      else if(vbound(2).eq.1) then
        call dfftw_plan_r2r_1d(fftplan(2,2),nyfft-1,workarray(1:nyfft-1), &
       &                       stab(1:nyfft-1,2),FFTW_RODFT00,FFTW_MEASURE)
      else if(vbound(2).eq.2) then
        call dfftw_plan_r2r_1d(fftplan(2,2),nyfft+1,workarray(0:nyfft), &
       &                       ctab(1:nyfft+1,2),FFTW_REDFT00,FFTW_MEASURE)
      end if
!
      if(vbound(1).eq.0) then
        call dfftw_plan_r2r_1d(fftplan(1,2),nxfft,workarray(0:nxfft-1), &
       &                       rtab(1:nxfft,1),FFTW_HC2R,FFTW_MEASURE)
      else if(vbound(1).eq.1) then
        call dfftw_plan_r2r_1d(fftplan(1,2),nxfft-1,workarray(1:nxfft-1), &
       &                       stab(1:nxfft-1,1),FFTW_RODFT00,FFTW_MEASURE)
      else if(vbound(1).eq.2) then
        call dfftw_plan_r2r_1d(fftplan(1,2),nxfft+1,workarray(0:nxfft), &
       &                       ctab(1:nxfft+1,1),FFTW_REDFT00,FFTW_MEASURE)
      end if



  return
  end subroutine fftinit0



  subroutine fftfmulti0(fftarray,ffttab,xs,nxft,xdim,ydim,axs)
!
!   ____________________________________________________________
!
!               S U B R O U T I N E   F F T F M U L T I 0
!   ____________________________________________________________
!
!   ............................................................
!   .                                                          .
!   ............................................................
!
!-------------------- parameter and common blocks
  implicit none
!
  integer(kind=4),intent(in) :: xs, nxft, xdim,ydim, axs
  real(kind=8),intent(inout) :: fftarray(-1:xdim-2,ydim)
  real(kind=8),intent(inout) :: ffttab(:)
!
  integer(kind=4) :: j


!-------------------- 
      if(.not.allocated(workarray)) then
        print*, "FFTINIT0 should be called before using FFT."
        stop
      end if


!-------------------- 
      do j=1,ydim
        workarray(xs:xs+nxft-1) = fftarray(xs:xs+nxft-1,j)
        call dfftw_execute_r2r(fftplan(axs,1),workarray(xs:xs+nxft-1),ffttab(1:nxft))
        fftarray(xs:xs+nxft-1,j) = ffttab(1:nxft)
      end do


  return
  end subroutine fftfmulti0



  subroutine fftbmulti0(fftarray,ffttab,xs,nxft,xdim,ydim,axs)
!
!   ____________________________________________________________
!
!               S U B R O U T I N E   F F T B M U L T I 0
!   ____________________________________________________________
!
!   ............................................................
!   .                                                          .
!   ............................................................
!
!-------------------- parameter and common blocks
  implicit none
!
  integer(kind=4),intent(in) :: xs, nxft, xdim,ydim, axs
  real(kind=8),intent(inout) :: fftarray(-1:xdim-2,ydim)
  real(kind=8),intent(inout) :: ffttab(:)
!
  integer(kind=4) :: j


!-------------------- 
      if(.not.allocated(workarray)) then
        print*, "FFTINIT0 should be called before using IFFT."
        stop
      end if


!-------------------- 
      do j=1,ydim
        workarray(xs:xs+nxft-1) = fftarray(xs:xs+nxft-1,j)
        call dfftw_execute_r2r(fftplan(axs,2),workarray(xs:xs+nxft-1),ffttab(1:nxft))
        fftarray(xs:xs+nxft-1,j) = ffttab(1:nxft)
      end do


  return
  end subroutine fftbmulti0



  subroutine fstmulti0(fftarray,ffttab,xs,nxft,xdim,ydim,axs,fb)
!
!   ____________________________________________________________
!
!               S U B R O U T I N E   F S T M U L T I 0
!   ____________________________________________________________
!
!   ............................................................
!   .                                                          .
!   ............................................................
!
!-------------------- parameter and common blocks
  implicit none
!
  integer(kind=4),intent(in) :: xs, nxft, xdim,ydim, axs, fb
  real(kind=8),intent(inout) :: fftarray(-1:xdim-2,ydim)
  real(kind=8),intent(inout) :: ffttab(:)
!
  integer(kind=4) :: j


!-------------------- 
      if(.not.allocated(workarray)) then
        print*, "FFTINIT0 should be called before using FST."
        stop
      end if


!-------------------- 
      do j=1,ydim
        workarray(xs:xs+nxft-2) = fftarray(xs:xs+nxft-2,j)
        call dfftw_execute_r2r(fftplan(axs,fb),workarray(xs:xs+nxft-2), &
       &                       ffttab(1:nxft-1))
        fftarray(xs:xs+nxft-2,j) = ffttab(1:nxft-1)
        fftarray(xs+nxft-1,j) = 0.0d0
      end do


  return
  end subroutine fstmulti0



  subroutine fctmulti0(fftarray,ffttab,xs,nxft,xdim,ydim,axs,fb)
!
!   ____________________________________________________________
!
!               S U B R O U T I N E   F C T M U L T I 0
!   ____________________________________________________________
!
!   ............................................................
!   .                                                          .
!   ............................................................
!
!-------------------- parameter and common blocks
  implicit none
!
  integer(kind=4),intent(in) :: xs, nxft, xdim,ydim, axs, fb
  real(kind=8),intent(inout) :: fftarray(-1:xdim-2,ydim)
  real(kind=8),intent(inout) :: ffttab(:)
!
  integer(kind=4) :: j


!-------------------- 
      if(.not.allocated(workarray)) then
        print*, "FFTINIT0 should be called before using FCT."
        stop
      end if


!-------------------- 
      do j=1,ydim
        workarray(xs:xs+nxft) = fftarray(xs:xs+nxft,j)
        call dfftw_execute_r2r(fftplan(axs,fb),workarray(xs:xs+nxft), &
       &                       ffttab(1:nxft+1))
        fftarray(xs:xs+nxft,j) = ffttab(1:nxft+1)
      end do


  return
  end subroutine fctmulti0



  function mipslct(a,b)
    use mpi
    implicit none
    logical,intent(in) :: a
    real(kind=8),intent(in) :: b(:)
    real(kind=8) :: mipslct(size(b))

    if(a) then
      mipslct = MPI_IN_PLACE
    else
      mipslct = b
    end if

    return
  end function mipslct



  end module wrapper
