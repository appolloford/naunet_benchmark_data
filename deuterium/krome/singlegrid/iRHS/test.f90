
!###################################################
! WARNING:This is a test auto-generated by KROME, in order to
! show a bare-minimal code to call the KROME's subroutine.
! Most of the values could not be appropriate for your
! problem, since this test is only intended as a general
! purpose example.

program test
  ! include 'mpif.h'
  use krome_main !use krome (mandatory)
  use krome_user !use utility (for krome_idx_* constants and others)
  implicit none
  integer,parameter::nsp=krome_nmols !number of species (common)
  integer,parameter::numTimes=10046
  integer::i,spec
  real*8::Tgas,dt,x(nsp),spy,t,nH,Av,CRrate,times(numTimes)
  real*8::coef(krome_nrea)
  real*8,parameter :: pi=3.14159265
  real*8,parameter :: rD=1.0d-5
  real*8,parameter :: rhoD=3.0
  real*8,parameter :: DtoGM=7.09d-3
  real*8,parameter :: amH=1.66043d-24
  real*8 :: GtoDN
  real*8 :: OPRH2 = 0.1
  real*8 :: COdep
  real*8 :: Ndep
  real*8 :: starttime, endtime
  integer :: clock_rate, clock_start, clock_stop

  real*8 :: mass(nsp) = (/ &
      1d0, 1200d0, 1d0, 2d0, 12d0, 16d0, 26d0, 17d0, 18d0, 2d0, 2d0, &
      3d0, 4d0, 4d0, 1d0, 2d0, 1200d0, 4d0, 28d0, 12d0, 14d0, &
      16d0, 28d0, 15d0, 16d0, 30d0, 32d0, 17d0, 18d0, 25d0, 24d0, &
      26d0, 38d0, 26d0, 36d0, 40d0, 14d0, 16d0, 15d0, 44d0, 18d0, &
      20d0, 19d0, 27d0, 28d0, 29d0, 30d0, 27d0, 28d0, 31d0, 32d0, &
      44d0, 16d0, 18d0, 17d0, 46d0, 33d0, 34d0, 42d0, 13d0, 14d0, &
      3d0, 3d0, 4d0, 4d0, 5d0, 5d0, 6d0, 6d0, 1d0, 3d0, &
      4d0, 4d0, 4d0, 29d0, 30d0, 12d0, 2d0, 14d0, 16d0, 2d0, &
      2d0, 13d0, 14d0, 30d0, 32d0, 14d0, 16d0, 15d0, 31d0, 32d0, &
      16d0, 18d0, 17d0, 28d0, 24d0, 17d0, 18d0, 25d0, 26d0, 15d0, &
      16d0, 18d0, 20d0, 19d0, 26d0, 36d0, 40d0, 29d0, 30d0, 38d0, &
      38d0, 27d0, 28d0, 27d0, 28d0, 42d0, 29d0, 30d0, 33d0, 34d0, &
      5d0, 6d0, 28d0, 44d0, 46d0, 6d0, 19d0, 20d0, 21d0, 22d0 &
      /)




  open( unit=10, file="timeres.dat", status="OLD", action="READ")
  do i=1, numTimes
    read (10, *) times(i)
  enddo
  open( unit=11, file="evolution.dat")
  ! open( unit=12, file="dfrac.dat")
  open( unit=13, file="Kout.dat")
  open( unit=14, file="input.dat")
  open( unit=15, file="time.txt")

  read(14, *) 
  read(14, *) Av
  read(14, *) 
  read(14, *) CRrate
  read(14, *) 
  read(14, *) Ndep
  read(14, *)
  read(14, *) COdep
  
  close(14)
  spy = 3.65d2 * 2.4d1 * 3.6d3 !seconds per year

  call krome_init() !init krome (mandatory)

  nH = 1.0d5
  !nH = 1.97d5
  x(:) = 1d-40 !default abundances
  ! x(krome_idx_H) = 1.d4 !hydrogen initial abundance
  x(krome_idx_H2_PARA)   = 1.0   / (1.0 + OPRH2) * 0.5 * nH
  x(krome_idx_H2_ORTHO)  = OPRH2 / (1.0 + OPRH2) * 0.5 * nH
  !x(krome_idx_H2_PARA)   = 1.25e-1 * nH
  !x(krome_idx_H2_ORTHO)  = 3.75e-1 * nH
  x(krome_idx_HD)        = 1.5d-5 * nH
  x(krome_idx_HE)        = 1.0d-1 * nH
  x(krome_idx_N)         = 2.1d-5 * nH / Ndep
  x(krome_idx_O)         = 1.8d-4 * nH / COdep
  x(krome_idx_C)         = 7.3d-5 * nH / COdep
  x(krome_idx_GRAIN0)    = 1.3215d-12 * nH

  Tgas = 15.0 !gas temperature (K)
  dt = 1d2 * spy !time-step (s)
  !dt = 760 * spy !time-step (s)
  t = 0d0

  !Av = 30.0
  !CRrate = 2.5d-17
  GtoDN = (4.d0*pi*rhoD*rD*rD*rD)/(3.d0*DtoGM*amH)
  call krome_set_user_Av(Av)
  call krome_set_user_crflux(CRrate)
  call krome_set_user_GtoDN(GtoDN)
  print *,"Av,CR,GtoDn=",Av,CRrate,GtoDN

  !call the solver
  ! call krome(x(:), Tgas, dt) !call KROME
  coef = krome_get_coef(Tgas, x)
  do i=1, krome_nrea
    write(13, *) i, coef(i)
  enddo

  !mass(:) = krome_get_mass()

  write(11,'(132E17.8e3)') t,x(:)/nH
  do i=1, numTimes-1
     ! dt = dt * 1.1d0 !increase timestep
     dt = (times(i+1)-times(i)) * spy
     t = t + dt !increase time
     !if (t .gt. 1d7*spy) exit

     ! starttime = MPI_Wtime()
     call system_clock(count_rate=clock_rate)
     call system_clock(count=clock_start)
     call krome(x(:), Tgas, dt) !call KROME
     ! endtime = MPI_Wtime()
     call system_clock(count=clock_stop) 
     write(11,'(132E17.8e3)') t/31536000.0,x(:)/nH
     write(15,'(F8.5)') real(clock_stop-clock_start)/real(clock_rate)
     print *, "t = ",t/31536000.0," yr, ", "elapsed = ", real(clock_stop-clock_start)/real(clock_rate), " sec"
     ! if ( i==75 ) then
     !    open(unit=87, file="PrestellarCoreInitAbundance_t3e5.dat")
     !    write(87,9487) nH, OPRH2, t/31536000.0
     !    write(87,9488) CRrate, COdep, Ndep
     !    write(87,9489) x(krome_idx_H2_ORTHO)/x(krome_idx_H2_PARA), x(krome_idx_N2Dj)/x(krome_idx_N2Hj)
     !    do spec=1, nsp
     !       write(87,*) x(spec)/nH, mass(spec)
     !    enddo
     !    close(87)
     !    !exit
     ! else if (i==105) then 
     !    open(unit=87, file="PrestellarCoreInitAbundance_t6e5.dat")
     !    write(87,9487) nH, OPRH2, t/31536000.0
     !    write(87,9488) CRrate, COdep, Ndep
     !    write(87,9489) x(krome_idx_H2_ORTHO)/x(krome_idx_H2_PARA), x(krome_idx_N2Dj)/x(krome_idx_N2Hj)
     !    do spec=1, nsp
     !       write(87,*) x(spec)/nH, mass(spec)
     !    enddo
     !    close(87)
     ! endif
  end do

  9487 format("# Clump mean nH = ", 1pe10.3, ", Initial OPR = ", 1pe10.3,", year = ", 1pe10.3)
  9488 format("# CRIR = ", 1pe10.3, ", CO depletion = ", 1pe10.3, ", N  depletion = ", 1pe10.3)
  9489 format("# Current OPR = ", 1pe10.3, ", Dfrac_N2H+ = ", 1pe10.3)

  close(10)
  close(11)
  ! close(12)
  close(13)
  close(15)
  print *,"Test OK!"

end program test
