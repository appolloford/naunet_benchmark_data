
!############### MODULE ##############
module krome_subs
contains

  ! *************************************************************
  !  This file has been generated with:
  !  KROME 14.08.dev on 2021-04-07 16:17:06
  !  Changeset xxxxxxx
  !  see http://kromepackage.org
  !
  !  Written and developed by Tommaso Grassi and Stefano Bovino
  !
  !  Contributors:
  !  J.Boulangier, T.Frostholm, D.Galli, F.A.Gianturco, T.Haugboelle,
  !  A.Lupi, J.Prieto, J.Ramsey, D.R.G.Schleicher, D.Seifried, E.Simoncini,
  !  E.Tognelli
  !  KROME is provided "as it is", without any warranty.
  ! *************************************************************

  !************************
  !compute reaction rates cm^3(n-1)/s
  function coe(n)
    use krome_commons
    use krome_constants
    use krome_user_commons
    use krome_getphys
    use krome_grfuncs
    use krome_phfuncs
    use krome_fit
    implicit none
    real*8::coe(nrea),k(nrea),Tgas,n(nspec),kmax
    real*8::T32
    real*8::invT
    real*8::small,nmax
    integer::i
    real*8::Hnuclei  !preproc from coevar
    !Tgas is in K
    Tgas = max(n(idx_Tgas), phys_Tcmb)
    Tgas = min(Tgas,1d9)

    !maxn initialization can be removed and small can be
    ! replaced with a proper value according to the environment
    nmax = max(maxval(n(1:nmols)),1d0)
    small = 1d-40/(nmax*nmax*nmax*nmax)

    T32 = Tgas*0.0033333333333333335 !Tgas/(300 K) (#)
    invT = 1.d0/Tgas !inverse of T (1/K)

    Hnuclei = get_Hnuclei(n(:))

    k(:) = small !inizialize coefficients

    !H3+_PARA + H2_PARA -> H3+_PARA + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(1) = small + (8.160e-10&
          *exp(-1.649e+02*invT))
    end if

    !H3+_PARA + H2_PARA -> H3+_ORTHO + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(2) = small + (5.880e-10&
          *exp(-1.982e+02*invT))
    end if

    !H3+_PARA + H2_ORTHO -> H3+_PARA + H2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(3) = small + (2.980e-10&
          *exp(+6.900e-01*invT))
    end if

    !H3+_PARA + H2_ORTHO -> H3+_ORTHO + H2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(4) = small + (3.460e-10&
          *exp(+6.900e-01*invT))
    end if

    !H3+_PARA + H2_ORTHO -> H3+_ORTHO + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(5) = small + (8.030e-10&
          *exp(-3.260e+01*invT))
    end if

    !H3+_ORTHO + H2_PARA -> H3+_PARA + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(6) = small + (1.500e-09&
          *exp(-1.362e+02*invT))
    end if

    !H3+_ORTHO + H2_PARA -> H3+_ORTHO + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(7) = small + (8.840e-09&
          *exp(-1.700e+02*invT))
    end if

    !H3+_ORTHO + H2_ORTHO -> H3+_PARA + H2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(8) = small + (1.040e-10)
    end if

    !H3+_ORTHO + H2_ORTHO -> H3+_PARA + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(9) = small + (4.000e-10&
          *exp(+1.900e-01*invT))
    end if

    !H3+_ORTHO + H2_ORTHO -> H3+_ORTHO + H2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(10) = small + (9.670e-11&
          *exp(+1.400e-01*invT))
    end if

    !H3+_PARA + HD -> H3+_ORTHO + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(11) = small + (5.710e-11&
          *exp(-3.220e+01*invT))
    end if

    !H3+_PARA + HD -> H2D+_PARA + H2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(12) = small + (3.110e-10&
          *exp(+7.100e-01*invT))
    end if

    !H3+_PARA + HD -> H2D+_PARA + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(13) = small + (4.930e-10&
          *exp(-9.500e-01*invT))
    end if

    !H3+_PARA + HD -> H2D+_ORTHO + H2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(14) = small + (6.080e-10&
          *exp(+1.080e+00*invT))
    end if

    !H3+_PARA + HD -> H2D+_ORTHO + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(15) = small + (5.710e-10&
          *exp(-2.580e+01*invT))
    end if

    !H3+_ORTHO + HD -> H3+_PARA + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(16) = small + (2.870e-11&
          *exp(+3.800e-01*invT))
    end if

    !H3+_ORTHO + HD -> H2D+_PARA + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(17) = small + (1.700e-10&
          *exp(+4.400e-01*invT))
    end if

    !H3+_ORTHO + HD -> H2D+_ORTHO + H2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(18) = small + (2.220e-10&
          *exp(+4.700e-01*invT))
    end if

    !H3+_ORTHO + HD -> H2D+_ORTHO + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(19) = small + (1.110e-09&
          *exp(-3.500e-01*invT))
    end if

    !H2D+_PARA + H2_PARA -> H3+_PARA + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(20) = small + (2.460e-10&
          *exp(-2.265e+02*invT))
    end if

    !H2D+_PARA + H2_PARA -> H2D+_ORTHO + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(21) = small + (1.020e-09&
          *exp(-2.561e+02*invT))
    end if

    !H2D+_PARA + H2_ORTHO -> H3+_PARA + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(22) = small + (1.480e-10&
          *exp(-5.880e+01*invT))
    end if

    !H2D+_PARA + H2_ORTHO -> H3+_ORTHO + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(23) = small + (9.320e-09&
          *exp(-9.460e+01*invT))
    end if

    !H2D+_PARA + H2_ORTHO -> H2D+_ORTHO + H2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(24) = small + (1.260e-09&
          *exp(-6.000e-02*invT))
    end if

    !H2D+_PARA + H2_ORTHO -> H2D+_ORTHO + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(25) = small + (6.040e-10&
          *exp(-8.880e+01*invT))
    end if

    !H2D+_ORTHO + H2_PARA -> H3+_PARA + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(26) = small + (1.310e-10&
          *exp(-1.404e+02*invT))
    end if

    !H2D+_ORTHO + H2_PARA -> H2D+_PARA + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(27) = small + (5.580e-10&
          *exp(-8.270e+01*invT))
    end if

    !H2D+_ORTHO + H2_PARA -> H2D+_ORTHO + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(28) = small + (6.540e-10&
          *exp(-1.740e+02*invT))
    end if

    !H2D+_ORTHO + H2_ORTHO -> H3+_PARA + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(29) = small + (4.670e-11&
          *exp(+8.200e-01*invT))
    end if

    !H2D+_ORTHO + H2_ORTHO -> H3+_ORTHO + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(30) = small + (1.640e-10&
          *exp(-6.310e+00*invT))
    end if

    !H2D+_ORTHO + H2_ORTHO -> H2D+_PARA + H2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(31) = small + (8.310e-11&
          *exp(+9.200e-01*invT))
    end if

    !H2D+_ORTHO + H2_ORTHO -> H2D+_PARA + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(32) = small + (1.680e-10&
          *exp(+7.700e-01*invT))
    end if

    !H2D+_ORTHO + H2_ORTHO -> H2D+_ORTHO + H2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(33) = small + (2.190e-10&
          *exp(+7.200e-01*invT))
    end if

    !H3+_PARA + D2_PARA -> H2D+_PARA + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(34) = small + (3.500e-09&
          *exp(+4.100e-01*invT))
    end if

    !H3+_PARA + D2_PARA -> H2D+_ORTHO + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(35) = small + (5.080e-09&
          *exp(+8.000e-02*invT))
    end if

    !H3+_PARA + D2_PARA -> D2H+_PARA + H2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(36) = small + (3.020e-10&
          *exp(+1.200e-01*invT))
    end if

    !H3+_PARA + D2_PARA -> D2H+_PARA + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(37) = small + (4.080e-10&
          *exp(-6.200e-01*invT))
    end if

    !H3+_PARA + D2_ORTHO -> H2D+_PARA + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(38) = small + (3.060e-10&
          *exp(-5.900e-01*invT))
    end if

    !H3+_PARA + D2_ORTHO -> H2D+_ORTHO + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(39) = small + (2.420e-10&
          *exp(-8.000e-02*invT))
    end if

    !H3+_PARA + D2_ORTHO -> D2H+_ORTHO + H2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(40) = small + (4.810e-10&
          *exp(+4.200e-01*invT))
    end if

    !H3+_PARA + D2_ORTHO -> D2H+_ORTHO + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(41) = small + (5.390e-10&
          *exp(-6.000e-02*invT))
    end if

    !H3+_ORTHO + D2_PARA -> H2D+_ORTHO + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(42) = small + (8.020e-10&
          *exp(-9.000e-02*invT))
    end if

    !H3+_ORTHO + D2_PARA -> D2H+_PARA + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(43) = small + (7.500e-10&
          *exp(+1.000e-01*invT))
    end if

    !H3+_ORTHO + D2_ORTHO -> H2D+_ORTHO + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(44) = small + (5.590e-10&
          *exp(-2.490e+00*invT))
    end if

    !H3+_ORTHO + D2_ORTHO -> D2H+_ORTHO + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(45) = small + (1.030e-09&
          *exp(+8.600e-01*invT))
    end if

    !H2D+_PARA + HD -> H3+_PARA + D2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(46) = small + (7.830e-12&
          *exp(-2.378e+02*invT))
    end if

    !H2D+_PARA + HD -> H3+_PARA + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(47) = small + (9.480e-12&
          *exp(-1.466e+02*invT))
    end if

    !H2D+_PARA + HD -> H2D+_ORTHO + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(48) = small + (2.840e-10&
          *exp(-8.850e+01*invT))
    end if

    !H2D+_PARA + HD -> D2H+_PARA + H2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(49) = small + (4.120e-10&
          *exp(+5.000e-01*invT))
    end if

    !H2D+_PARA + HD -> D2H+_PARA + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(50) = small + (1.890e-10&
          *exp(-3.310e+01*invT))
    end if

    !H2D+_PARA + HD -> D2H+_ORTHO + H2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(51) = small + (7.320e-10&
          *exp(-3.000e-01*invT))
    end if

    !H2D+_PARA + HD -> D2H+_ORTHO + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(52) = small + (1.930e-10&
          *exp(+6.400e-01*invT))
    end if

    !H2D+_ORTHO + HD -> H3+_PARA + D2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(53) = small + (2.520e-12&
          *exp(-1.501e+02*invT))
    end if

    !H2D+_ORTHO + HD -> H3+_PARA + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(54) = small + (3.880e-12&
          *exp(-6.510e+01*invT))
    end if

    !H2D+_ORTHO + HD -> H3+_ORTHO + D2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(55) = small + (6.800e-12&
          *exp(-1.817e+02*invT))
    end if

    !H2D+_ORTHO + HD -> H3+_ORTHO + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(56) = small + (1.030e-10&
          *exp(-9.680e+01*invT))
    end if

    !H2D+_ORTHO + HD -> H2D+_PARA + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(57) = small + (8.640e-11&
          *exp(+3.800e-01*invT))
    end if

    !H2D+_ORTHO + HD -> D2H+_PARA + H2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(58) = small + (6.410e-11&
          *exp(-2.200e-01*invT))
    end if

    !H2D+_ORTHO + HD -> D2H+_PARA + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(59) = small + (3.020e-10&
          *exp(+6.000e-01*invT))
    end if

    !H2D+_ORTHO + HD -> D2H+_ORTHO + H2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(60) = small + (1.490e-10&
          *exp(+9.000e-01*invT))
    end if

    !H2D+_ORTHO + HD -> D2H+_ORTHO + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(61) = small + (5.240e-10&
          *exp(+5.600e-01*invT))
    end if

    !D2H+_PARA + H2_PARA -> H3+_PARA + D2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(62) = small + (2.020e-10&
          *exp(-3.550e+02*invT))
    end if

    !D2H+_PARA + H2_PARA -> H2D+_PARA + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(63) = small + (3.260e-10&
          *exp(-1.373e+02*invT))
    end if

    !D2H+_PARA + H2_PARA -> H2D+_ORTHO + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(64) = small + (4.490e-10&
          *exp(-2.314e+02*invT))
    end if

    !D2H+_PARA + H2_PARA -> D2H+_PARA + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(65) = small + (7.090e-10&
          *exp(-1.688e+02*invT))
    end if

    !D2H+_PARA + H2_ORTHO -> H3+_ORTHO + D2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(66) = small + (2.650e-11&
          *exp(-2.339e+02*invT))
    end if

    !D2H+_PARA + H2_ORTHO -> H2D+_PARA + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(67) = small + (7.330e-11&
          *exp(-1.580e+00*invT))
    end if

    !D2H+_PARA + H2_ORTHO -> H2D+_ORTHO + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(68) = small + (5.940e-10&
          *exp(-5.460e+01*invT))
    end if

    !D2H+_PARA + H2_ORTHO -> D2H+_PARA + H2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(69) = small + (2.840e-10&
          *exp(+5.800e-01*invT))
    end if

    !D2H+_ORTHO + H2_PARA -> H3+_PARA + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(70) = small + (1.560e-11&
          *exp(-3.252e+02*invT))
    end if

    !D2H+_ORTHO + H2_PARA -> H2D+_PARA + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(71) = small + (3.480e-10&
          *exp(-1.936e+02*invT))
    end if

    !D2H+_ORTHO + H2_PARA -> H2D+_ORTHO + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(72) = small + (4.610e-10&
          *exp(-2.817e+02*invT))
    end if

    !D2H+_ORTHO + H2_PARA -> D2H+_ORTHO + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(73) = small + (4.160e-10&
          *exp(-1.711e+02*invT))
    end if

    !D2H+_ORTHO + H2_ORTHO -> H3+_PARA + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(74) = small + (8.350e-12&
          *exp(-1.711e+02*invT))
    end if

    !D2H+_ORTHO + H2_ORTHO -> H3+_ORTHO + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(75) = small + (1.650e-11&
          *exp(-1.946e+02*invT))
    end if

    !D2H+_ORTHO + H2_ORTHO -> H2D+_PARA + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(76) = small + (8.150e-11&
          *exp(-1.560e+01*invT))
    end if

    !D2H+_ORTHO + H2_ORTHO -> H2D+_ORTHO + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(77) = small + (6.820e-10&
          *exp(-1.034e+02*invT))
    end if

    !D2H+_ORTHO + H2_ORTHO -> D2H+_ORTHO + H2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(78) = small + (4.170e-10&
          *exp(+3.600e-01*invT))
    end if

    !H2D+_PARA + D2_PARA -> H2D+_PARA + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(79) = small + (4.820e-11&
          *exp(+1.010e+00*invT))
    end if

    !H2D+_PARA + D2_PARA -> D2H+_PARA + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(80) = small + (6.780e-10&
          *exp(+2.300e-01*invT))
    end if

    !H2D+_PARA + D2_PARA -> D2H+_ORTHO + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(81) = small + (5.410e-10&
          *exp(-8.500e-01*invT))
    end if

    !H2D+_PARA + D2_PARA -> D3+_ORTHO + H2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(82) = small + (1.410e-10&
          *exp(+1.050e+00*invT))
    end if

    !H2D+_PARA + D2_ORTHO -> H2D+_PARA + D2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(83) = small + (2.070e-11&
          *exp(-8.630e+01*invT))
    end if

    !H2D+_PARA + D2_ORTHO -> D2H+_PARA + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(84) = small + (2.570e-10&
          *exp(+5.500e-01*invT))
    end if

    !H2D+_PARA + D2_ORTHO -> D2H+_ORTHO + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(85) = small + (7.490e-10&
          *exp(-6.000e-01*invT))
    end if

    !H2D+_PARA + D2_ORTHO -> D3+_META + H2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(86) = small + (2.270e-10&
          *exp(+8.600e-01*invT))
    end if

    !H2D+_PARA + D2_ORTHO -> D3+_ORTHO + H2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(87) = small + (1.600e-10&
          *exp(-1.100e-01*invT))
    end if

    !H2D+_ORTHO + D2_PARA -> H2D+_ORTHO + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(88) = small + (3.930e-11&
          *exp(-2.100e-01*invT))
    end if

    !H2D+_ORTHO + D2_PARA -> D2H+_PARA + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(89) = small + (6.640e-10&
          *exp(-2.000e-01*invT))
    end if

    !H2D+_ORTHO + D2_PARA -> D2H+_ORTHO + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(90) = small + (5.390e-10&
          *exp(+4.400e-01*invT))
    end if

    !H2D+_ORTHO + D2_PARA -> D3+_ORTHO + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(91) = small + (1.310e-10&
          *exp(-1.800e-01*invT))
    end if

    !H2D+_ORTHO + D2_ORTHO -> H2D+_ORTHO + D2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(92) = small + (3.950e-11&
          *exp(-8.850e+01*invT))
    end if

    !H2D+_ORTHO + D2_ORTHO -> D2H+_PARA + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(93) = small + (2.740e-10&
          *exp(+3.600e-01*invT))
    end if

    !H2D+_ORTHO + D2_ORTHO -> D2H+_ORTHO + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(94) = small + (8.750e-10&
          *exp(-5.300e-01*invT))
    end if

    !H2D+_ORTHO + D2_ORTHO -> D3+_META + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(95) = small + (1.630e-10&
          *exp(+1.570e+00*invT))
    end if

    !H2D+_ORTHO + D2_ORTHO -> D3+_ORTHO + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(96) = small + (8.010e-11&
          *exp(+9.400e-01*invT))
    end if

    !D2H+_PARA + HD -> H2D+_PARA + D2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(97) = small + (1.540e-11&
          *exp(-1.455e+02*invT))
    end if

    !D2H+_PARA + HD -> H2D+_PARA + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(98) = small + (1.170e-11&
          *exp(-5.700e+01*invT))
    end if

    !D2H+_PARA + HD -> H2D+_ORTHO + D2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(99) = small + (9.470e-11&
          *exp(-2.373e+02*invT))
    end if

    !D2H+_PARA + HD -> H2D+_ORTHO + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(100) = small + (4.680e-11&
          *exp(-1.462e+02*invT))
    end if

    !D2H+_PARA + HD -> D2H+_ORTHO + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(101) = small + (3.360e-10&
          *exp(-1.800e+00*invT))
    end if

    !D2H+_PARA + HD -> D3+_ORTHO + H2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(102) = small + (1.090e-10&
          *exp(+7.800e-01*invT))
    end if

    !D2H+_PARA + HD -> D3+_ORTHO + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(103) = small + (3.700e-10&
          *exp(-5.200e-01*invT))
    end if

    !D2H+_ORTHO + HD -> H2D+_PARA + D2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(104) = small + (7.830e-12&
          *exp(-2.022e+02*invT))
    end if

    !D2H+_ORTHO + HD -> H2D+_PARA + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(105) = small + (2.120e-11&
          *exp(-1.076e+02*invT))
    end if

    !D2H+_ORTHO + HD -> H2D+_ORTHO + D2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(106) = small + (3.590e-11&
          *exp(-2.851e+02*invT))
    end if

    !D2H+_ORTHO + HD -> H2D+_ORTHO + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(107) = small + (7.790e-11&
          *exp(-1.967e+02*invT))
    end if

    !D2H+_ORTHO + HD -> D2H+_PARA + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(108) = small + (2.900e-10&
          *exp(-4.830e+01*invT))
    end if

    !D2H+_ORTHO + HD -> D3+_META + H2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(109) = small + (1.360e-10&
          *exp(+1.500e-01*invT))
    end if

    !D2H+_ORTHO + HD -> D3+_META + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(110) = small + (2.070e-10&
          *exp(+1.000e-01*invT))
    end if

    !D2H+_ORTHO + HD -> D3+_ORTHO + H2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(111) = small + (1.100e-10&
          *exp(+2.700e-01*invT))
    end if

    !D2H+_ORTHO + HD -> D3+_ORTHO + H2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(112) = small + (2.840e-10&
          *exp(+3.800e-01*invT))
    end if

    !D3+_META + H2_PARA -> H2D+_PARA + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(113) = small + (1.650e-10&
          *exp(-3.449e+02*invT))
    end if

    !D3+_META + H2_PARA -> D2H+_ORTHO + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(114) = small + (9.570e-10&
          *exp(-2.393e+02*invT))
    end if

    !D3+_META + H2_ORTHO -> H2D+_ORTHO + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(115) = small + (1.900e-10&
          *exp(-2.627e+02*invT))
    end if

    !D3+_META + H2_ORTHO -> D2H+_ORTHO + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(116) = small + (1.530e-09&
          *exp(-6.560e+01*invT))
    end if

    !D3+_ORTHO + H2_PARA -> H2D+_PARA + D2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(117) = small + (1.070e-10&
          *exp(-3.939e+02*invT))
    end if

    !D3+_ORTHO + H2_PARA -> H2D+_PARA + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(118) = small + (7.850e-11&
          *exp(-2.969e+02*invT))
    end if

    !D3+_ORTHO + H2_PARA -> D2H+_PARA + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(119) = small + (9.430e-10&
          *exp(-2.374e+02*invT))
    end if

    !D3+_ORTHO + H2_PARA -> D2H+_ORTHO + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(120) = small + (6.890e-10&
          *exp(-1.897e+02*invT))
    end if

    !D3+_ORTHO + H2_ORTHO -> H2D+_ORTHO + D2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(121) = small + (1.530e-10&
          *exp(-3.038e+02*invT))
    end if

    !D3+_ORTHO + H2_ORTHO -> H2D+_ORTHO + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(122) = small + (9.600e-11&
          *exp(-2.136e+02*invT))
    end if

    !D3+_ORTHO + H2_ORTHO -> D2H+_PARA + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(123) = small + (9.060e-10&
          *exp(-6.620e+01*invT))
    end if

    !D3+_ORTHO + H2_ORTHO -> D2H+_ORTHO + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(124) = small + (7.700e-10&
          *exp(-1.700e+01*invT))
    end if

    !D2H+_PARA + D2_PARA -> D2H+_PARA + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(125) = small + (5.160e-11&
          *exp(-1.000e-01*invT))
    end if

    !D2H+_PARA + D2_PARA -> D2H+_ORTHO + D2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(126) = small + (4.360e-11&
          *exp(-3.110e+00*invT))
    end if

    !D2H+_PARA + D2_PARA -> D2H+_ORTHO + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(127) = small + (1.420e-10&
          *exp(-1.310e+00*invT))
    end if

    !D2H+_PARA + D2_PARA -> D3+_ORTHO + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(128) = small + (8.230e-10&
          *exp(+1.300e-01*invT))
    end if

    !D2H+_PARA + D2_ORTHO -> D2H+_PARA + D2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(129) = small + (3.740e-11&
          *exp(-8.590e+01*invT))
    end if

    !D2H+_PARA + D2_ORTHO -> D2H+_ORTHO + D2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(130) = small + (1.050e-10&
          *exp(-3.560e+01*invT))
    end if

    !D2H+_PARA + D2_ORTHO -> D2H+_ORTHO + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(131) = small + (9.120e-11&
          *exp(-3.650e+00*invT))
    end if

    !D2H+_PARA + D2_ORTHO -> D3+_META + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(132) = small + (2.930e-10&
          *exp(-1.600e-01*invT))
    end if

    !D2H+_PARA + D2_ORTHO -> D3+_ORTHO + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(133) = small + (7.590e-10&
          *exp(+5.200e-01*invT))
    end if

    !D2H+_ORTHO + D2_PARA -> D2H+_PARA + D2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(134) = small + (3.640e-11&
          *exp(-5.000e+01*invT))
    end if

    !D2H+_ORTHO + D2_PARA -> D2H+_PARA + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(135) = small + (1.920e-10&
          *exp(+7.000e-01*invT))
    end if

    !D2H+_ORTHO + D2_PARA -> D2H+_ORTHO + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(136) = small + (1.110e-10&
          *exp(+5.000e-01*invT))
    end if

    !D2H+_ORTHO + D2_PARA -> D3+_META + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(137) = small + (2.770e-10&
          *exp(+7.600e-01*invT))
    end if

    !D2H+_ORTHO + D2_PARA -> D3+_ORTHO + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(138) = small + (6.520e-10&
          *exp(-9.000e-01*invT))
    end if

    !D2H+_ORTHO + D2_ORTHO -> D2H+_PARA + D2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(139) = small + (5.750e-11&
          *exp(-1.377e+02*invT))
    end if

    !D2H+_ORTHO + D2_ORTHO -> D2H+_PARA + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(140) = small + (7.310e-11&
          *exp(-5.030e+01*invT))
    end if

    !D2H+_ORTHO + D2_ORTHO -> D2H+_ORTHO + D2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(141) = small + (4.280e-11&
          *exp(-8.550e+01*invT))
    end if

    !D2H+_ORTHO + D2_ORTHO -> D3+_META + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(142) = small + (5.820e-10&
          *exp(+8.000e-02*invT))
    end if

    !D2H+_ORTHO + D2_ORTHO -> D3+_ORTHO + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(143) = small + (5.810e-10&
          *exp(-4.000e-01*invT))
    end if

    !D3+_META + HD -> D2H+_PARA + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(144) = small + (1.080e-10&
          *exp(-2.067e+02*invT))
    end if

    !D3+_META + HD -> D2H+_ORTHO + D2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(145) = small + (8.740e-11&
          *exp(-2.513e+02*invT))
    end if

    !D3+_META + HD -> D2H+_ORTHO + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(146) = small + (2.650e-10&
          *exp(-1.543e+02*invT))
    end if

    !D3+_META + HD -> D3+_ORTHO + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(147) = small + (5.970e-10&
          *exp(-4.630e+01*invT))
    end if

    !D3+_ORTHO + HD -> D2H+_PARA + D2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(148) = small + (1.400e-10&
          *exp(-2.474e+02*invT))
    end if

    !D3+_ORTHO + HD -> D2H+_PARA + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(149) = small + (1.630e-10&
          *exp(-1.605e+02*invT))
    end if

    !D3+_ORTHO + HD -> D2H+_ORTHO + D2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(150) = small + (1.080e-10&
          *exp(-1.984e+02*invT))
    end if

    !D3+_ORTHO + HD -> D2H+_ORTHO + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(151) = small + (1.200e-10&
          *exp(-1.052e+02*invT))
    end if

    !D3+_ORTHO + HD -> D3+_META + HD
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(152) = small + (2.460e-10&
          *exp(+2.300e-01*invT))
    end if

    !D3+_META + D2_PARA -> D3+_META + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(153) = small + (1.480e-10&
          *exp(+4.900e-01*invT))
    end if

    !D3+_META + D2_PARA -> D3+_ORTHO + D2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(154) = small + (1.110e-10&
          *exp(-4.660e+01*invT))
    end if

    !D3+_META + D2_PARA -> D3+_ORTHO + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(155) = small + (6.120e-10&
          *exp(-4.500e-01*invT))
    end if

    !D3+_META + D2_ORTHO -> D3+_META + D2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(156) = small + (2.140e-10&
          *exp(-8.470e+01*invT))
    end if

    !D3+_META + D2_ORTHO -> D3+_ORTHO + D2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(157) = small + (1.630e-10&
          *exp(-1.305e+02*invT))
    end if

    !D3+_META + D2_ORTHO -> D3+_ORTHO + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(158) = small + (6.660e-10&
          *exp(-4.580e+01*invT))
    end if

    !D3+_ORTHO + D2_PARA -> D3+_META + D2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(159) = small + (4.470e-11&
          *exp(+2.600e-01*invT))
    end if

    !D3+_ORTHO + D2_PARA -> D3+_META + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(160) = small + (5.640e-11&
          *exp(-7.200e-01*invT))
    end if

    !D3+_ORTHO + D2_PARA -> D3+_ORTHO + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(161) = small + (5.870e-10&
          *exp(+1.900e-01*invT))
    end if

    !D3+_ORTHO + D2_ORTHO -> D3+_META + D2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(162) = small + (3.210e-10&
          *exp(-3.830e+01*invT))
    end if

    !D3+_ORTHO + D2_ORTHO -> D3+_META + D2_ORTHO
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(163) = small + (2.780e-10&
          *exp(+4.700e-01*invT))
    end if

    !D3+_ORTHO + D2_ORTHO -> D3+_ORTHO + D2_PARA
    if(Tgas.GE.5d0 .and. Tgas.LT.20d0) then
      k(164) = small + (3.240e-10&
          *exp(-8.520e+01*invT))
    end if

    !H + H -> H2_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(165) = small + (1.238e-17&
          *(T32)**(+5.000e-01)&
          /n(idx_H)*Hnuclei)
    end if

    !H + H -> H2_ORTHO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(166) = small + (3.713e-17&
          *(T32)**(+5.000e-01)&
          /n(idx_H)*Hnuclei)
    end if

    !H + D -> HD
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(167) = small + (2.475e-17&
          *(T32)**(+5.000e-01)&
          /(0.5*n(idx_H)+0.5*n(idx_D))&
          *user_GtoDN)
    end if

    !D + D -> D2_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(168) = small + (1.650e-22&
          *(T32)**(+5.000e-01)&
          /n(idx_D)*user_GtoDN)
    end if

    !D + D -> D2_ORTHO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(169) = small + (3.300e-22&
          *(T32)**(+5.000e-01)&
          /n(idx_D)*user_GtoDN)
    end if

    !GRAIN0 + E -> GRAIN-
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(170) = small + (7.428e-16&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !H+ + GRAIN- -> H + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(171) = small + (5.083e-15&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !HD+ + GRAIN- -> H + D + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(172) = small + (1.948e-15&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !HD+ + GRAIN- -> HD + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(173) = small + (1.017e-15&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !D2+_ORTHO + GRAIN- -> D + D + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(174) = small + (1.694e-15&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !D2+_ORTHO + GRAIN- -> D2_ORTHO + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(175) = small + (8.472e-16&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !D2+_PARA + GRAIN- -> D + D + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(176) = small + (1.694e-15&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !D2+_PARA + GRAIN- -> D2_PARA + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(177) = small + (8.472e-16&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !HE+ + GRAIN- -> HE + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(178) = small + (2.541e-15&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !H3+_PARA + GRAIN- -> H2_PARA + H + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(179) = small + (4.914e-16&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !H3+_PARA + GRAIN- -> H2_ORTHO + H + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(180) = small + (4.914e-16&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !H3+_PARA + GRAIN- -> H + H + H + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(181) = small + (1.948e-15&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !H3+_ORTHO + GRAIN- -> H2_ORTHO + H + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(182) = small + (1.017e-15&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !H3+_ORTHO + GRAIN- -> H + H + H + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(183) = small + (1.948e-15&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !H2D+_PARA + GRAIN- -> H2_PARA + D + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(184) = small + (4.236e-16&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !H2D+_PARA + GRAIN- -> HD + H + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(185) = small + (8.472e-16&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !H2D+_PARA + GRAIN- -> H + D + H + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(186) = small + (1.271e-15&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !H2D+_ORTHO + GRAIN- -> H + D + H + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(187) = small + (1.271e-15&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !H2D+_ORTHO + GRAIN- -> H2_ORTHO + D + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(188) = small + (4.236e-16&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !H2D+_ORTHO + GRAIN- -> HD + H + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(189) = small + (8.472e-16&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !D2H+_ORTHO + GRAIN- -> D + H + D + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(190) = small + (1.101e-15&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !D2H+_ORTHO + GRAIN- -> D2_ORTHO + H + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(191) = small + (3.727e-16&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !D2H+_ORTHO + GRAIN- -> HD + D + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(192) = small + (7.541e-16&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !D2H+_PARA + GRAIN- -> D2_PARA + H + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(193) = small + (3.727e-16&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !D2H+_PARA + GRAIN- -> HD + D + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(194) = small + (7.541e-16&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !D2H+_PARA + GRAIN- -> D + H + D + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(195) = small + (1.101e-15&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !D3+_META + GRAIN- -> D + D + D + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(196) = small + (1.355e-15&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !D3+_META + GRAIN- -> D2_ORTHO + D + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(197) = small + (6.863e-16&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !D3+_ORTHO + GRAIN- -> D + D + D + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(198) = small + (1.017e-15&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !D3+_ORTHO + GRAIN- -> D2_ORTHO + D + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(199) = small + (3.474e-16&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !D3+_ORTHO + GRAIN- -> D2_PARA + D + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(200) = small + (3.474e-16&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !HCO+ + GRAIN- -> H + CO + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(201) = small + (9.319e-16&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !DCO+ + GRAIN- -> D + CO + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(202) = small + (9.319e-16&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !C -> C+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(203) = small + (1.020e+03&
          *user_crflux)
    end if

    !H -> H+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(204) = small + (4.600e-01&
          *user_crflux)
    end if

    !D -> D+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(205) = small + (4.600e-01&
          *user_crflux)
    end if

    !HE -> HE+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(206) = small + (5.000e-01&
          *user_crflux)
    end if

    !N -> N+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(207) = small + (2.100e+00&
          *user_crflux)
    end if

    !O -> O+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(208) = small + (2.800e+00&
          *user_crflux)
    end if

    !H2_PARA -> H + H
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(209) = small + (1.000e-01&
          *user_crflux)
    end if

    !H2_ORTHO -> H + H
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(210) = small + (1.000e-01&
          *user_crflux)
    end if

    !D2_PARA -> D + D
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(211) = small + (1.000e-01&
          *user_crflux)
    end if

    !D2_ORTHO -> D + D
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(212) = small + (1.000e-01&
          *user_crflux)
    end if

    !HD -> H + D
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(213) = small + (1.000e-01&
          *user_crflux)
    end if

    !H2_PARA -> H + H+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(214) = small + (2.200e-02&
          *user_crflux)
    end if

    !H2_ORTHO -> H + H+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(215) = small + (2.200e-02&
          *user_crflux)
    end if

    !D2_PARA -> D + D+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(216) = small + (2.200e-02&
          *user_crflux)
    end if

    !D2_ORTHO -> D + D+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(217) = small + (2.200e-02&
          *user_crflux)
    end if

    !HD -> H + D+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(218) = small + (2.200e-02&
          *user_crflux)
    end if

    !HD -> D + H+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(219) = small + (2.200e-02&
          *user_crflux)
    end if

    !H2_PARA -> H+ + H-
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(220) = small + (3.000e-04&
          *user_crflux)
    end if

    !H2_ORTHO -> H+ + H-
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(221) = small + (3.000e-04&
          *user_crflux)
    end if

    !D2_PARA -> D+ + D-
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(222) = small + (3.000e-04&
          *user_crflux)
    end if

    !D2_ORTHO -> D+ + D-
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(223) = small + (3.000e-04&
          *user_crflux)
    end if

    !HD -> H+ + D-
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(224) = small + (3.000e-04&
          *user_crflux)
    end if

    !HD -> D+ + H-
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(225) = small + (3.000e-04&
          *user_crflux)
    end if

    !H2_PARA -> H2+_PARA + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(226) = small + (9.300e-01&
          *user_crflux)
    end if

    !H2_ORTHO -> H2+_ORTHO + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(227) = small + (9.300e-01&
          *user_crflux)
    end if

    !D2_PARA -> D2+_PARA + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(228) = small + (9.300e-01&
          *user_crflux)
    end if

    !D2_ORTHO -> D2+_ORTHO + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(229) = small + (9.300e-01&
          *user_crflux)
    end if

    !HD -> HD+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(230) = small + (9.300e-01&
          *user_crflux)
    end if

    !CH+ -> C + H+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(231) = small + (1.760e+02&
          *user_crflux)
    end if

    !CD+ -> C + D+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(232) = small + (1.760e+02&
          *user_crflux)
    end if

    !CH+ -> H + C+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(233) = small + (1.120e+02&
          *user_crflux)
    end if

    !CD+ -> D + C+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(234) = small + (1.120e+02&
          *user_crflux)
    end if

    !N2 -> N + N
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(235) = small + (5.000e+00&
          *user_crflux)
    end if

    !NH -> H + N
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(236) = small + (5.000e+02&
          *user_crflux)
    end if

    !ND -> D + N
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(237) = small + (5.000e+02&
          *user_crflux)
    end if

    !NO -> N + O
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(238) = small + (4.820e+02&
          *user_crflux)
    end if

    !NO -> NO+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(239) = small + (4.940e+02&
          *user_crflux)
    end if

    !O2 -> O + O
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(240) = small + (7.500e+02&
          *user_crflux)
    end if

    !O2 -> O2+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(241) = small + (1.170e+02&
          *user_crflux)
    end if

    !OH -> H + O
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(242) = small + (5.100e+02&
          *user_crflux)
    end if

    !OD -> D + O
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(243) = small + (5.100e+02&
          *user_crflux)
    end if

    !C2H -> H + C2
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(244) = small + (5.000e+03&
          *user_crflux)
    end if

    !C2D -> D + C2
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(245) = small + (5.000e+03&
          *user_crflux)
    end if

    !C2N -> C + CN
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(246) = small + (1.000e+03&
          *user_crflux)
    end if

    !C3 -> C + C2
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(247) = small + (1.120e+03&
          *user_crflux)
    end if

    !CCO -> O + C2
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(248) = small + (7.500e+02&
          *user_crflux)
    end if

    !CCO -> C + CO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(249) = small + (7.500e+02&
          *user_crflux)
    end if

    !CH2 -> CH2+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(250) = small + (5.000e+02&
          *user_crflux)
    end if

    !CD2 -> CD2+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(251) = small + (5.000e+02&
          *user_crflux)
    end if

    !CHD -> CHD+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(252) = small + (5.000e+02&
          *user_crflux)
    end if

    !CO2 -> O + CO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(253) = small + (1.710e+03&
          *user_crflux)
    end if

    !H2O -> H + OH
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(254) = small + (9.700e+02&
          *user_crflux)
    end if

    !D2O -> D + OD
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(255) = small + (9.700e+02&
          *user_crflux)
    end if

    !HDO -> H + OD
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(256) = small + (9.700e+02&
          *user_crflux)
    end if

    !HDO -> D + OH
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(257) = small + (9.700e+02&
          *user_crflux)
    end if

    !HCN -> H + CN
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(258) = small + (3.120e+03&
          *user_crflux)
    end if

    !DCN -> D + CN
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(259) = small + (3.120e+03&
          *user_crflux)
    end if

    !HCO -> H + CO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(260) = small + (4.210e+02&
          *user_crflux)
    end if

    !DCO -> D + CO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(261) = small + (4.210e+02&
          *user_crflux)
    end if

    !HCO -> HCO+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(262) = small + (1.170e+03&
          *user_crflux)
    end if

    !DCO -> DCO+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(263) = small + (1.170e+03&
          *user_crflux)
    end if

    !HNC -> H + CN
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(264) = small + (3.000e+03&
          *user_crflux)
    end if

    !DNC -> D + CN
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(265) = small + (3.000e+03&
          *user_crflux)
    end if

    !HNO -> HNO+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(266) = small + (1.000e+03&
          *user_crflux)
    end if

    !DNO -> DNO+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(267) = small + (1.000e+03&
          *user_crflux)
    end if

    !N2O -> N + NO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(268) = small + (1.500e+03&
          *user_crflux)
    end if

    !NH2 -> H + NH
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(269) = small + (8.000e+01&
          *user_crflux)
    end if

    !ND2 -> D + ND
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(270) = small + (8.000e+01&
          *user_crflux)
    end if

    !NHD -> H + ND
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(271) = small + (8.000e+01&
          *user_crflux)
    end if

    !NHD -> D + NH
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(272) = small + (8.000e+01&
          *user_crflux)
    end if

    !NH2 -> NH2+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(273) = small + (6.500e+02&
          *user_crflux)
    end if

    !ND2 -> ND2+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(274) = small + (6.500e+02&
          *user_crflux)
    end if

    !NHD -> NHD+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(275) = small + (6.500e+02&
          *user_crflux)
    end if

    !NO2 -> O + NO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(276) = small + (1.500e+03&
          *user_crflux)
    end if

    !O2H -> O + OH
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(277) = small + (7.500e+02&
          *user_crflux)
    end if

    !O2D -> O + OD
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(278) = small + (7.500e+02&
          *user_crflux)
    end if

    !O2H -> H + O2
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(279) = small + (7.500e+02&
          *user_crflux)
    end if

    !O2D -> D + O2
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(280) = small + (7.500e+02&
          *user_crflux)
    end if

    !OCN -> O + CN
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(281) = small + (1.500e+03&
          *user_crflux)
    end if

    !C2 -> C + C
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(282) = small + (2.370e+02&
          *user_crflux)
    end if

    !CH -> C + H
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(283) = small + (7.300e+02&
          *user_crflux)
    end if

    !CD -> C + D
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(284) = small + (7.300e+02&
          *user_crflux)
    end if

    !CN -> C + N
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(285) = small + (1.060e+04&
          *user_crflux)
    end if

    !CO -> C + O
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(286) = small + (5.000e+00&
          *user_crflux)
    end if

    !CO -> CO+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(287) = small + (3.000e+00&
          *user_crflux)
    end if

    !C2+ -> C + C+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(288) = small + (1.000e-11&
          *exp(-1.700e+00*user_Av))
    end if

    !CH+ -> H + C+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(289) = small + (4.600e-12&
          *exp(-3.000e+00*user_Av))
    end if

    !CD+ -> D + C+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(290) = small + (4.600e-12&
          *exp(-3.000e+00*user_Av))
    end if

    !H2+_PARA -> H + H+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(291) = small + (2.600e-10&
          *exp(-1.800e+00*user_Av))
    end if

    !H2+_ORTHO -> H + H+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(292) = small + (2.600e-10&
          *exp(-1.800e+00*user_Av))
    end if

    !D2+_PARA -> D + D+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(293) = small + (2.600e-10&
          *exp(-1.800e+00*user_Av))
    end if

    !D2+_ORTHO -> D + D+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(294) = small + (2.600e-10&
          *exp(-1.800e+00*user_Av))
    end if

    !HD+ -> H + D+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(295) = small + (2.600e-10&
          *exp(-1.800e+00*user_Av))
    end if

    !HD+ -> D + H+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(296) = small + (2.600e-10&
          *exp(-1.800e+00*user_Av))
    end if

    !OH+ -> O + H+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(297) = small + (7.200e-12&
          *exp(-1.800e+00*user_Av))
    end if

    !OD+ -> O + D+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(298) = small + (7.200e-12&
          *exp(-1.800e+00*user_Av))
    end if

    !C2H+ -> H + C2+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(299) = small + (1.000e-11&
          *exp(-2.000e+00*user_Av))
    end if

    !C2D+ -> D + C2+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(300) = small + (1.000e-11&
          *exp(-2.000e+00*user_Av))
    end if

    !CH2+ -> H + CH+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(301) = small + (1.700e-09&
          *exp(-1.700e+00*user_Av))
    end if

    !CD2+ -> D + CD+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(302) = small + (1.700e-09&
          *exp(-1.700e+00*user_Av))
    end if

    !CHD+ -> H + CD+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(303) = small + (1.700e-09&
          *exp(-1.700e+00*user_Av))
    end if

    !CHD+ -> D + CH+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(304) = small + (1.700e-09&
          *exp(-1.700e+00*user_Av))
    end if

    !H3+_ORTHO -> H2_PARA + H+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(305) = small + (2.000e-08&
          *exp(-1.800e+00*user_Av))
    end if

    !H3+_ORTHO -> H2_ORTHO + H+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(306) = small + (2.000e-08&
          *exp(-1.800e+00*user_Av))
    end if

    !H3+_PARA -> H2_PARA + H+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(307) = small + (2.000e-08&
          *exp(-1.800e+00*user_Av))
    end if

    !H3+_PARA -> H2_ORTHO + H+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(308) = small + (2.000e-08&
          *exp(-1.800e+00*user_Av))
    end if

    !D3+_ORTHO -> D2_PARA + D+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(309) = small + (2.000e-08&
          *exp(-1.800e+00*user_Av))
    end if

    !D3+_ORTHO -> D2_ORTHO + D+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(310) = small + (2.000e-08&
          *exp(-1.800e+00*user_Av))
    end if

    !D3+_META -> D2_PARA + D+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(311) = small + (2.000e-08&
          *exp(-1.800e+00*user_Av))
    end if

    !D3+_META -> D2_ORTHO + D+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(312) = small + (2.000e-08&
          *exp(-1.800e+00*user_Av))
    end if

    !H2D+_ORTHO -> H2_PARA + D+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(313) = small + (2.000e-08&
          *exp(-1.800e+00*user_Av))
    end if

    !H2D+_ORTHO -> H2_ORTHO + D+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(314) = small + (2.000e-08&
          *exp(-1.800e+00*user_Av))
    end if

    !H2D+_PARA -> H2_PARA + D+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(315) = small + (2.000e-08&
          *exp(-1.800e+00*user_Av))
    end if

    !H2D+_PARA -> H2_ORTHO + D+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(316) = small + (2.000e-08&
          *exp(-1.800e+00*user_Av))
    end if

    !H2D+_ORTHO -> HD + H+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(317) = small + (2.000e-08&
          *exp(-1.800e+00*user_Av))
    end if

    !H2D+_PARA -> HD + H+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(318) = small + (2.000e-08&
          *exp(-1.800e+00*user_Av))
    end if

    !D2H+_ORTHO -> D2_PARA + H+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(319) = small + (2.000e-08&
          *exp(-1.800e+00*user_Av))
    end if

    !D2H+_ORTHO -> D2_ORTHO + H+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(320) = small + (2.000e-08&
          *exp(-1.800e+00*user_Av))
    end if

    !D2H+_PARA -> D2_PARA + H+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(321) = small + (2.000e-08&
          *exp(-1.800e+00*user_Av))
    end if

    !D2H+_PARA -> D2_ORTHO + H+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(322) = small + (2.000e-08&
          *exp(-1.800e+00*user_Av))
    end if

    !D2H+_ORTHO -> HD + D+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(323) = small + (2.000e-08&
          *exp(-1.800e+00*user_Av))
    end if

    !D2H+_PARA -> HD + D+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(324) = small + (2.000e-08&
          *exp(-1.800e+00*user_Av))
    end if

    !H3+_ORTHO -> H + H2+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(325) = small + (7.900e-09&
          *exp(-2.300e+00*user_Av))
    end if

    !H3+_ORTHO -> H + H2+_ORTHO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(326) = small + (7.900e-09&
          *exp(-2.300e+00*user_Av))
    end if

    !H3+_PARA -> H + H2+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(327) = small + (7.900e-09&
          *exp(-2.300e+00*user_Av))
    end if

    !H3+_PARA -> H + H2+_ORTHO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(328) = small + (7.900e-09&
          *exp(-2.300e+00*user_Av))
    end if

    !D3+_ORTHO -> D + D2+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(329) = small + (7.900e-09&
          *exp(-2.300e+00*user_Av))
    end if

    !D3+_ORTHO -> D + D2+_ORTHO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(330) = small + (7.900e-09&
          *exp(-2.300e+00*user_Av))
    end if

    !D3+_META -> D + D2+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(331) = small + (7.900e-09&
          *exp(-2.300e+00*user_Av))
    end if

    !D3+_META -> D + D2+_ORTHO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(332) = small + (7.900e-09&
          *exp(-2.300e+00*user_Av))
    end if

    !H2D+_ORTHO -> H + HD+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(333) = small + (7.900e-09&
          *exp(-2.300e+00*user_Av))
    end if

    !H2D+_PARA -> H + HD+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(334) = small + (7.900e-09&
          *exp(-2.300e+00*user_Av))
    end if

    !H2D+_ORTHO -> D + H2+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(335) = small + (7.900e-09&
          *exp(-2.300e+00*user_Av))
    end if

    !H2D+_ORTHO -> D + H2+_ORTHO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(336) = small + (7.900e-09&
          *exp(-2.300e+00*user_Av))
    end if

    !H2D+_PARA -> D + H2+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(337) = small + (7.900e-09&
          *exp(-2.300e+00*user_Av))
    end if

    !H2D+_PARA -> D + H2+_ORTHO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(338) = small + (7.900e-09&
          *exp(-2.300e+00*user_Av))
    end if

    !D2H+_ORTHO -> H + D2+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(339) = small + (7.900e-09&
          *exp(-2.300e+00*user_Av))
    end if

    !D2H+_ORTHO -> H + D2+_ORTHO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(340) = small + (7.900e-09&
          *exp(-2.300e+00*user_Av))
    end if

    !D2H+_PARA -> H + D2+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(341) = small + (7.900e-09&
          *exp(-2.300e+00*user_Av))
    end if

    !D2H+_PARA -> H + D2+_ORTHO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(342) = small + (7.900e-09&
          *exp(-2.300e+00*user_Av))
    end if

    !D2H+_ORTHO -> D + HD+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(343) = small + (7.900e-09&
          *exp(-2.300e+00*user_Av))
    end if

    !D2H+_PARA -> D + HD+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(344) = small + (7.900e-09&
          *exp(-2.300e+00*user_Av))
    end if

    !C- -> C + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(345) = small + (2.400e-07&
          *exp(-9.000e-01*user_Av))
    end if

    !H- -> H + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(346) = small + (2.400e-07&
          *exp(-9.000e-01*user_Av))
    end if

    !D- -> D + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(347) = small + (2.400e-07&
          *exp(-9.000e-01*user_Av))
    end if

    !O- -> O + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(348) = small + (2.400e-07&
          *exp(-9.000e-01*user_Av))
    end if

    !CN- -> CN + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(349) = small + (2.400e-07&
          *exp(-9.000e-01*user_Av))
    end if

    !OH- -> OH + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(350) = small + (2.400e-07&
          *exp(-9.000e-01*user_Av))
    end if

    !OD- -> OD + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(351) = small + (2.400e-07&
          *exp(-9.000e-01*user_Av))
    end if

    !C -> C+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(352) = small + (2.160e-10&
          *exp(-2.610e+00*user_Av))
    end if

    !C2 -> C + C
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(353) = small + (4.700e-11&
          *exp(-2.600e+00*user_Av))
    end if

    !C2 -> C2+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(354) = small + (1.000e-10&
          *exp(-2.000e+00*user_Av))
    end if

    !CH -> C + H
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(355) = small + (1.400e-10&
          *exp(-1.500e+00*user_Av))
    end if

    !CD -> C + D
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(356) = small + (1.400e-10&
          *exp(-1.500e+00*user_Av))
    end if

    !CH -> CH+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(357) = small + (2.900e-10&
          *exp(-2.800e+00*user_Av))
    end if

    !CD -> CD+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(358) = small + (2.900e-10&
          *exp(-2.800e+00*user_Av))
    end if

    !CN -> C + N
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(359) = small + (1.000e-09&
          *exp(-2.800e+00*user_Av))
    end if

    !CO -> C + O
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(360) = small + (3.100e-11&
          *exp(-2.540e+00*user_Av))
    end if

    !H2_PARA -> H + H
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(361) = small + (3.400e-11&
          *exp(-2.500e+00*user_Av))
    end if

    !H2_ORTHO -> H + H
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(362) = small + (3.400e-11&
          *exp(-2.500e+00*user_Av))
    end if

    !D2_PARA -> D + D
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(363) = small + (3.400e-11&
          *exp(-2.500e+00*user_Av))
    end if

    !D2_ORTHO -> D + D
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(364) = small + (3.400e-11&
          *exp(-2.500e+00*user_Av))
    end if

    !HD -> H + D
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(365) = small + (3.400e-11&
          *exp(-2.500e+00*user_Av))
    end if

    !N2 -> N + N
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(366) = small + (5.000e-12&
          *exp(-3.000e+00*user_Av))
    end if

    !NH -> H + N
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(367) = small + (4.000e-10&
          *exp(-1.500e+00*user_Av))
    end if

    !ND -> D + N
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(368) = small + (4.000e-10&
          *exp(-1.500e+00*user_Av))
    end if

    !NH -> NH+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(369) = small + (1.000e-11&
          *exp(-2.000e+00*user_Av))
    end if

    !ND -> ND+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(370) = small + (1.000e-11&
          *exp(-2.000e+00*user_Av))
    end if

    !NO -> N + O
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(371) = small + (3.000e-10&
          *exp(-2.000e+00*user_Av))
    end if

    !NO -> NO+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(372) = small + (2.000e-10&
          *exp(-2.000e+00*user_Av))
    end if

    !O2 -> O + O
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(373) = small + (3.300e-10&
          *exp(-1.400e+00*user_Av))
    end if

    !O2 -> O2+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(374) = small + (6.200e-12&
          *exp(-3.100e+00*user_Av))
    end if

    !OH -> H + O
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(375) = small + (1.680e-10&
          *exp(-1.660e+00*user_Av))
    end if

    !OD -> D + O
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(376) = small + (1.680e-10&
          *exp(-1.660e+00*user_Av))
    end if

    !OH -> OH+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(377) = small + (1.600e-12&
          *exp(-3.100e+00*user_Av))
    end if

    !OD -> OD+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(378) = small + (1.600e-12&
          *exp(-3.100e+00*user_Av))
    end if

    !C2H -> H + C2
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(379) = small + (1.000e-09&
          *exp(-1.700e+00*user_Av))
    end if

    !C2D -> D + C2
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(380) = small + (1.000e-09&
          *exp(-1.700e+00*user_Av))
    end if

    !C2H -> C2H+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(381) = small + (1.000e-11&
          *exp(-2.000e+00*user_Av))
    end if

    !C2D -> C2D+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(382) = small + (1.000e-11&
          *exp(-2.000e+00*user_Av))
    end if

    !C2N -> N + C2
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(383) = small + (1.000e-10&
          *exp(-1.700e+00*user_Av))
    end if

    !C2N -> C + CN
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(384) = small + (1.000e-09&
          *exp(-1.700e+00*user_Av))
    end if

    !C3 -> C + C2
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(385) = small + (2.600e-10&
          *exp(-2.280e+00*user_Av))
    end if

    !CH2 -> H + CH
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(386) = small + (5.000e-11&
          *exp(-1.700e+00*user_Av))
    end if

    !CD2 -> D + CD
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(387) = small + (5.000e-11&
          *exp(-1.700e+00*user_Av))
    end if

    !CHD -> H + CD
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(388) = small + (5.000e-11&
          *exp(-1.700e+00*user_Av))
    end if

    !CHD -> D + CH
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(389) = small + (5.000e-11&
          *exp(-1.700e+00*user_Av))
    end if

    !CH2 -> CH2+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(390) = small + (1.000e-09&
          *exp(-2.300e+00*user_Av))
    end if

    !CD2 -> CD2+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(391) = small + (1.000e-09&
          *exp(-2.300e+00*user_Av))
    end if

    !CHD -> CHD+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(392) = small + (1.000e-09&
          *exp(-2.300e+00*user_Av))
    end if

    !CO2 -> O + CO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(393) = small + (3.130e-10&
          *exp(-2.030e+00*user_Av))
    end if

    !H2O -> H + OH
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(394) = small + (3.280e-10&
          *exp(-1.630e+00*user_Av))
    end if

    !D2O -> D + OD
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(395) = small + (3.280e-10&
          *exp(-1.630e+00*user_Av))
    end if

    !HDO -> H + OD
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(396) = small + (3.280e-10&
          *exp(-1.630e+00*user_Av))
    end if

    !HDO -> D + OH
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(397) = small + (3.280e-10&
          *exp(-1.630e+00*user_Av))
    end if

    !H2O -> H2O+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(398) = small + (2.100e-11&
          *exp(-3.100e+00*user_Av))
    end if

    !D2O -> D2O+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(399) = small + (2.100e-11&
          *exp(-3.100e+00*user_Av))
    end if

    !HDO -> HDO+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(400) = small + (2.100e-11&
          *exp(-3.100e+00*user_Av))
    end if

    !HCN -> H + CN
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(401) = small + (5.480e-10&
          *exp(-2.000e+00*user_Av))
    end if

    !DCN -> D + CN
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(402) = small + (5.480e-10&
          *exp(-2.000e+00*user_Av))
    end if

    !HCO -> H + CO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(403) = small + (5.870e-10&
          *exp(-5.300e-01*user_Av))
    end if

    !DCO -> D + CO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(404) = small + (5.870e-10&
          *exp(-5.300e-01*user_Av))
    end if

    !HCO -> HCO+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(405) = small + (2.460e-10&
          *exp(-2.110e+00*user_Av))
    end if

    !DCO -> DCO+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(406) = small + (2.460e-10&
          *exp(-2.110e+00*user_Av))
    end if

    !HNC -> H + CN
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(407) = small + (5.480e-10&
          *exp(-2.000e+00*user_Av))
    end if

    !DNC -> D + CN
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(408) = small + (5.480e-10&
          *exp(-2.000e+00*user_Av))
    end if

    !HNO -> H + NO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(409) = small + (1.700e-10&
          *exp(-5.300e-01*user_Av))
    end if

    !DNO -> D + NO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(410) = small + (1.700e-10&
          *exp(-5.300e-01*user_Av))
    end if

    !NH2 -> H + NH
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(411) = small + (2.110e-10&
          *exp(-1.520e+00*user_Av))
    end if

    !ND2 -> D + ND
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(412) = small + (2.110e-10&
          *exp(-1.520e+00*user_Av))
    end if

    !NHD -> H + ND
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(413) = small + (2.110e-10&
          *exp(-1.520e+00*user_Av))
    end if

    !NHD -> D + NH
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(414) = small + (2.110e-10&
          *exp(-1.520e+00*user_Av))
    end if

    !NH2 -> NH2+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(415) = small + (1.730e-10&
          *exp(-2.590e+00*user_Av))
    end if

    !ND2 -> ND2+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(416) = small + (1.730e-10&
          *exp(-2.590e+00*user_Av))
    end if

    !NHD -> NHD+ + E
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(417) = small + (1.730e-10&
          *exp(-2.590e+00*user_Av))
    end if

    !NO2 -> O + NO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(418) = small + (1.290e-09&
          *exp(-2.000e+00*user_Av))
    end if

    !OCN -> O + CN
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(419) = small + (1.000e-11&
          *exp(-2.000e+00*user_Av))
    end if

    !CH + C+ -> H + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(420) = small + (2.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD + C+ -> D + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(421) = small + (2.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !NH + C+ -> H + CN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(422) = small + (4.600e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND + C+ -> D + CN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(423) = small + (4.600e-09&
          *(T32)**(-5.000e-01))
    end if

    !O2 + C+ -> CO + O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(424) = small + (4.100e-10)
    end if

    !O2 + C+ -> O + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(425) = small + (7.500e-10)
    end if

    !OH + C+ -> H + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(426) = small + (2.900e-09&
          *(T32)**(-3.330e-01))
    end if

    !OD + C+ -> D + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(427) = small + (2.900e-09&
          *(T32)**(-3.330e-01))
    end if

    !C2H + C+ -> H + C3+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(428) = small + (2.600e-09&
          *(T32)**(-5.000e-01))
    end if

    !C2D + C+ -> D + C3+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(429) = small + (2.600e-09&
          *(T32)**(-5.000e-01))
    end if

    !CCO + C+ -> C + C2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(430) = small + (3.930e-09&
          *(T32)**(-5.000e-01))
    end if

    !CH2 + C+ -> H + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(431) = small + (4.340e-10&
          *(T32)**(-5.000e-01))
    end if

    !CD2 + C+ -> D + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(432) = small + (4.340e-10&
          *(T32)**(-5.000e-01))
    end if

    !CHD + C+ -> H + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(433) = small + (4.340e-10&
          *(T32)**(-5.000e-01))
    end if

    !CHD + C+ -> D + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(434) = small + (4.340e-10&
          *(T32)**(-5.000e-01))
    end if

    !CO2 + C+ -> CO + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(435) = small + (1.100e-09)
    end if

    !H2O + C+ -> H + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(436) = small + (8.900e-10&
          *(T32)**(-5.000e-01))
    end if

    !D2O + C+ -> D + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(437) = small + (8.900e-10&
          *(T32)**(-5.000e-01))
    end if

    !HDO + C+ -> H + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(438) = small + (8.900e-10&
          *(T32)**(-5.000e-01))
    end if

    !HDO + C+ -> D + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(439) = small + (8.900e-10&
          *(T32)**(-5.000e-01))
    end if

    !H2O + C+ -> H + HOC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(440) = small + (1.800e-09&
          *(T32)**(-5.000e-01))
    end if

    !D2O + C+ -> D + DOC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(441) = small + (1.800e-09&
          *(T32)**(-5.000e-01))
    end if

    !HDO + C+ -> H + DOC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(442) = small + (1.800e-09&
          *(T32)**(-5.000e-01))
    end if

    !HDO + C+ -> D + HOC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(443) = small + (1.800e-09&
          *(T32)**(-5.000e-01))
    end if

    !HCN + C+ -> H + C2N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(444) = small + (4.750e-09&
          *(T32)**(-5.000e-01))
    end if

    !DCN + C+ -> D + C2N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(445) = small + (4.750e-09&
          *(T32)**(-5.000e-01))
    end if

    !HCN + C+ -> H + CNC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(446) = small + (4.750e-09&
          *(T32)**(-5.000e-01))
    end if

    !DCN + C+ -> D + CNC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(447) = small + (4.750e-09&
          *(T32)**(-5.000e-01))
    end if

    !HCO + C+ -> CO + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(448) = small + (6.700e-10&
          *(T32)**(-5.000e-01))
    end if

    !DCO + C+ -> CO + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(449) = small + (6.700e-10&
          *(T32)**(-5.000e-01))
    end if

    !HNC + C+ -> H + C2N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(450) = small + (8.600e-09&
          *(T32)**(-5.000e-01))
    end if

    !DNC + C+ -> D + C2N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(451) = small + (8.600e-09&
          *(T32)**(-5.000e-01))
    end if

    !NH2 + C+ -> H + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(452) = small + (2.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND2 + C+ -> D + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(453) = small + (2.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !NHD + C+ -> H + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(454) = small + (2.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !NHD + C+ -> D + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(455) = small + (2.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !OCN + C+ -> CN + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(456) = small + (1.900e-09&
          *(T32)**(-5.000e-01))
    end if

    !H2_PARA + CO+ -> H + HOC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(457) = small + (7.500e-10)
    end if

    !H2_ORTHO + CO+ -> H + HOC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(458) = small + (7.500e-10)
    end if

    !D2_PARA + CO+ -> D + DOC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(459) = small + (7.500e-10)
    end if

    !D2_ORTHO + CO+ -> D + DOC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(460) = small + (7.500e-10)
    end if

    !HD + CO+ -> H + DOC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(461) = small + (3.750e-10)
    end if

    !HD + CO+ -> D + HOC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(462) = small + (3.750e-10)
    end if

    !H2_PARA + CO+ -> H + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(463) = small + (7.500e-10)
    end if

    !H2_ORTHO + CO+ -> H + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(464) = small + (7.500e-10)
    end if

    !D2_PARA + CO+ -> D + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(465) = small + (7.500e-10)
    end if

    !D2_ORTHO + CO+ -> D + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(466) = small + (7.500e-10)
    end if

    !HD + CO+ -> H + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(467) = small + (3.750e-10)
    end if

    !HD + CO+ -> D + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(468) = small + (3.750e-10)
    end if

    !H2_PARA + CN+ -> H + HNC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(469) = small + (7.500e-10)
    end if

    !H2_ORTHO + CN+ -> H + HNC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(470) = small + (7.500e-10)
    end if

    !D2_PARA + CN+ -> D + DNC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(471) = small + (7.500e-10)
    end if

    !D2_ORTHO + CN+ -> D + DNC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(472) = small + (7.500e-10)
    end if

    !HD + CN+ -> H + DNC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(473) = small + (3.750e-10)
    end if

    !HD + CN+ -> D + HNC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(474) = small + (3.750e-10)
    end if

    !H2_PARA + CN+ -> H + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(475) = small + (7.500e-10)
    end if

    !H2_ORTHO + CN+ -> H + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(476) = small + (7.500e-10)
    end if

    !D2_PARA + CN+ -> D + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(477) = small + (7.500e-10)
    end if

    !D2_ORTHO + CN+ -> D + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(478) = small + (7.500e-10)
    end if

    !HD + CN+ -> H + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(479) = small + (3.750e-10)
    end if

    !HD + CN+ -> D + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(480) = small + (3.750e-10)
    end if

    !NO + C- -> O + CN-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(481) = small + (1.000e-09)
    end if

    !O2 + C- -> CO + O-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(482) = small + (4.000e-10)
    end if

    !CO2 + C- -> CO + CO + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(483) = small + (4.700e-11)
    end if

    !H2O + H- -> H2_PARA + OH-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(484) = small + (3.800e-09)
    end if

    !H2O + D- -> H2_PARA + OD-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(485) = small + (1.900e-09)
    end if

    !H2O + D- -> HD + OH-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(486) = small + (1.900e-09)
    end if

    !D2O + H- -> D2_PARA + OH-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(487) = small + (1.900e-09)
    end if

    !D2O + H- -> HD + OD-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(488) = small + (1.900e-09)
    end if

    !D2O + D- -> D2_PARA + OD-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(489) = small + (3.800e-09)
    end if

    !HDO + H- -> H2_PARA + OD-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(490) = small + (1.900e-09)
    end if

    !HDO + H- -> HD + OH-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(491) = small + (1.900e-09)
    end if

    !HDO + D- -> D2_PARA + OH-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(492) = small + (1.900e-09)
    end if

    !HDO + D- -> HD + OD-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(493) = small + (1.900e-09)
    end if

    !HCN + H- -> H2_PARA + CN-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(494) = small + (3.800e-09)
    end if

    !HCN + D- -> HD + CN-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(495) = small + (3.800e-09)
    end if

    !DCN + H- -> HD + CN-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(496) = small + (3.800e-09)
    end if

    !DCN + D- -> D2_PARA + CN-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(497) = small + (3.800e-09)
    end if

    !H2_PARA + O- -> H + OH-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(498) = small + (3.000e-11)
    end if

    !H2_ORTHO + O- -> H + OH-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(499) = small + (3.000e-11)
    end if

    !D2_PARA + O- -> D + OD-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(500) = small + (3.000e-11)
    end if

    !D2_ORTHO + O- -> D + OD-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(501) = small + (3.000e-11)
    end if

    !HD + O- -> H + OD-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(502) = small + (1.500e-11)
    end if

    !HD + O- -> D + OH-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(503) = small + (1.500e-11)
    end if

    !HCN + O- -> OH + CN-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(504) = small + (1.200e-09)
    end if

    !DCN + O- -> OD + CN-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(505) = small + (1.200e-09)
    end if

    !HCN + OH- -> H2O + CN-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(506) = small + (1.200e-09)
    end if

    !HCN + OD- -> HDO + CN-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(507) = small + (1.200e-09)
    end if

    !DCN + OH- -> HDO + CN-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(508) = small + (1.200e-09)
    end if

    !DCN + OD- -> D2O + CN-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(509) = small + (1.200e-09)
    end if

    !O + CH2 -> H + H + CO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(510) = small + (1.200e-10)
    end if

    !O + CD2 -> D + D + CO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(511) = small + (1.200e-10)
    end if

    !O + CHD -> H + D + CO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(512) = small + (1.200e-10)
    end if

    !O + CH2 -> CO + H2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(513) = small + (8.000e-11)
    end if

    !O + CD2 -> CO + D2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(514) = small + (8.000e-11)
    end if

    !O + CHD -> CO + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(515) = small + (8.000e-11)
    end if

    !O + HCO -> H + CO2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(516) = small + (5.000e-11)
    end if

    !O + DCO -> D + CO2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(517) = small + (5.000e-11)
    end if

    !O + HCO -> CO + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(518) = small + (5.000e-11)
    end if

    !O + DCO -> CO + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(519) = small + (5.000e-11)
    end if

    !O + HCN -> H + OCN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(520) = small + (3.610e-13&
          *(T32)**(+2.100e+00)*exp(-3.080e+03*invT))
    end if

    !O + DCN -> D + OCN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(521) = small + (3.610e-13&
          *(T32)**(+2.100e+00)*exp(-3.080e+03*invT))
    end if

    !O + HNO -> NO + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(522) = small + (3.770e-11&
          *(T32)**(-7.600e-01))
    end if

    !O + DNO -> NO + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(523) = small + (3.770e-11&
          *(T32)**(-7.600e-01))
    end if

    !O + NH2 -> NH + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(524) = small + (7.100e-12&
          *(T32)**(-1.000e-01))
    end if

    !O + ND2 -> ND + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(525) = small + (7.100e-12&
          *(T32)**(-1.000e-01))
    end if

    !O + NHD -> NH + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(526) = small + (3.550e-12&
          *(T32)**(-1.000e-01))
    end if

    !O + NHD -> ND + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(527) = small + (3.550e-12&
          *(T32)**(-1.000e-01))
    end if

    !O + NH2 -> H + HNO
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(528) = small + (6.390e-11&
          *(T32)**(-1.000e-01))
    end if

    !O + ND2 -> D + DNO
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(529) = small + (6.390e-11&
          *(T32)**(-1.000e-01))
    end if

    !O + NHD -> H + DNO
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(530) = small + (3.195e-11&
          *(T32)**(-1.000e-01))
    end if

    !O + NHD -> D + HNO
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(531) = small + (3.195e-11&
          *(T32)**(-1.000e-01))
    end if

    !O + NO2 -> NO + O2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(532) = small + (1.000e-11)
    end if

    !O + O2H -> O2 + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(533) = small + (5.300e-11)
    end if

    !O + O2D -> O2 + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(534) = small + (5.300e-11)
    end if

    !O + OCN -> CO + NO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(535) = small + (1.500e-11&
          *exp(-2.000e+02*invT))
    end if

    !O + OCN -> CN + O2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(536) = small + (4.050e-10&
          *(T32)**(-1.430e+00)*exp(-3.500e+03*invT))
    end if

    !O + C3 -> C2 + CO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(537) = small + (5.000e-12&
          *exp(-9.000e+02*invT))
    end if

    !CH + NO -> O + HCN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(538) = small + (1.200e-11&
          *(T32)**(-1.300e-01))
    end if

    !CD + NO -> O + DCN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(539) = small + (1.200e-11&
          *(T32)**(-1.300e-01))
    end if

    !CH + O2 -> CO + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(540) = small + (3.800e-11&
          *(T32)**(-4.800e-01))
    end if

    !CD + O2 -> CO + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(541) = small + (3.800e-11&
          *(T32)**(-4.800e-01))
    end if

    !CH + HNO -> NO + CH2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(542) = small + (1.730e-11)
    end if

    !CH + DNO -> NO + CHD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(543) = small + (1.730e-11)
    end if

    !CD + HNO -> NO + CHD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(544) = small + (1.730e-11)
    end if

    !CD + DNO -> NO + CD2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(545) = small + (1.730e-11)
    end if

    !C2 + CH -> H + C3
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(546) = small + (1.000e-10)
    end if

    !C2 + CD -> D + C3
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(547) = small + (1.000e-10)
    end if

    !CN + NO -> CO + N2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(548) = small + (1.600e-13)
    end if

    !CN + O2 -> O + OCN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(549) = small + (2.400e-11&
          *(T32)**(-6.000e-01))
    end if

    !CN + HCO -> CO + HCN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(550) = small + (1.000e-10)
    end if

    !CN + DCO -> CO + DCN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(551) = small + (1.000e-10)
    end if

    !CN + OH -> H + OCN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(552) = small + (7.010e-11)
    end if

    !CN + OD -> D + OCN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(553) = small + (7.010e-11)
    end if

    !CN + OH -> O + HCN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(554) = small + (1.000e-11&
          *exp(-1.000e+03*invT))
    end if

    !CN + OD -> O + DCN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(555) = small + (1.000e-11&
          *exp(-1.000e+03*invT))
    end if

    !CN + HNO -> NO + HCN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(556) = small + (3.000e-11)
    end if

    !CN + DNO -> NO + DCN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(557) = small + (3.000e-11)
    end if

    !CO + OH -> H + CO2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(558) = small + (2.810e-13&
          *exp(-1.760e+02*invT))
    end if

    !CO + OD -> D + CO2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(559) = small + (2.810e-13&
          *exp(-1.760e+02*invT))
    end if

    !CO + HNO -> NH + CO2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(560) = small + (3.320e-12&
          *exp(-6.170e+03*invT))
    end if

    !CO + DNO -> ND + CO2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(561) = small + (3.320e-12&
          *exp(-6.170e+03*invT))
    end if

    !NH + NH -> H + H + N2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(562) = small + (1.000e-10)
    end if

    !NH + ND -> H + D + N2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(563) = small + (1.000e-10)
    end if

    !ND + ND -> D + D + N2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(564) = small + (1.000e-10)
    end if

    !NH + NO -> H + N2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(565) = small + (3.120e-11)
    end if

    !ND + NO -> D + N2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(566) = small + (3.120e-11)
    end if

    !NO + NH2 -> N2 + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(567) = small + (1.700e-11)
    end if

    !NO + ND2 -> N2 + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(568) = small + (1.700e-11)
    end if

    !NO + NHD -> N2 + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(569) = small + (1.700e-11)
    end if

    !OH + OH -> O + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(570) = small + (1.650e-12&
          *(T32)**(+1.140e+00)*exp(-5.000e+01*invT))
    end if

    !OH + OD -> O + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(571) = small + (1.650e-12&
          *(T32)**(+1.140e+00)*exp(-5.000e+01*invT))
    end if

    !OD + OD -> O + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(572) = small + (1.650e-12&
          *(T32)**(+1.140e+00)*exp(-5.000e+01*invT))
    end if

    !OH + HCO -> CO + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(573) = small + (1.690e-10)
    end if

    !OH + DCO -> CO + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(574) = small + (1.690e-10)
    end if

    !OD + HCO -> CO + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(575) = small + (1.690e-10)
    end if

    !OD + DCO -> CO + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(576) = small + (1.690e-10)
    end if

    !OH + HNO -> NO + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(577) = small + (8.000e-11&
          *exp(-5.000e+02*invT))
    end if

    !OH + DNO -> NO + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(578) = small + (8.000e-11&
          *exp(-5.000e+02*invT))
    end if

    !OD + HNO -> NO + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(579) = small + (8.000e-11&
          *exp(-5.000e+02*invT))
    end if

    !OD + DNO -> NO + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(580) = small + (8.000e-11&
          *exp(-5.000e+02*invT))
    end if

    !OH + NH2 -> NH + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(581) = small + (1.500e-12)
    end if

    !OH + ND2 -> NH + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(582) = small + (7.500e-13)
    end if

    !OH + ND2 -> ND + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(583) = small + (7.500e-13)
    end if

    !OH + NHD -> NH + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(584) = small + (7.500e-13)
    end if

    !OH + NHD -> ND + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(585) = small + (7.500e-13)
    end if

    !OD + NH2 -> NH + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(586) = small + (7.500e-13)
    end if

    !OD + NH2 -> ND + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(587) = small + (7.500e-13)
    end if

    !OD + ND2 -> ND + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(588) = small + (1.500e-12)
    end if

    !OD + NHD -> NH + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(589) = small + (7.500e-13)
    end if

    !OD + NHD -> ND + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(590) = small + (7.500e-13)
    end if

    !OH + O2H -> O2 + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(591) = small + (1.200e-10)
    end if

    !OH + O2D -> O2 + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(592) = small + (1.200e-10)
    end if

    !OD + O2H -> O2 + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(593) = small + (1.200e-10)
    end if

    !OD + O2D -> O2 + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(594) = small + (1.200e-10)
    end if

    !NO + CN+ -> N + NCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(595) = small + (1.900e-10)
    end if

    !O2 + CN+ -> CO + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(596) = small + (8.600e-11)
    end if

    !O2 + CN+ -> O + NCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(597) = small + (8.600e-11)
    end if

    !CO2 + CN+ -> NO + C2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(598) = small + (2.250e-10)
    end if

    !CO2 + CN+ -> CO + NCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(599) = small + (2.250e-10)
    end if

    !H2O + CN+ -> OH + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(600) = small + (1.600e-09)
    end if

    !D2O + CN+ -> OD + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(601) = small + (1.600e-09)
    end if

    !HDO + CN+ -> OH + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(602) = small + (1.600e-09)
    end if

    !HDO + CN+ -> OD + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(603) = small + (1.600e-09)
    end if

    !H2O + CN+ -> NH + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(604) = small + (1.600e-10)
    end if

    !D2O + CN+ -> ND + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(605) = small + (1.600e-10)
    end if

    !HDO + CN+ -> NH + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(606) = small + (1.600e-10)
    end if

    !HDO + CN+ -> ND + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(607) = small + (1.600e-10)
    end if

    !HCO + CN+ -> CO + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(608) = small + (3.700e-10)
    end if

    !DCO + CN+ -> CO + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(609) = small + (3.700e-10)
    end if

    !N + CO+ -> C + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(610) = small + (8.100e-11)
    end if

    !CH + CO+ -> C + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(611) = small + (3.200e-10)
    end if

    !CD + CO+ -> C + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(612) = small + (3.200e-10)
    end if

    !NH + CO+ -> N + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(613) = small + (3.200e-10)
    end if

    !ND + CO+ -> N + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(614) = small + (3.200e-10)
    end if

    !OH + CO+ -> O + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(615) = small + (3.100e-10)
    end if

    !OD + CO+ -> O + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(616) = small + (3.100e-10)
    end if

    !C2H + CO+ -> C2 + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(617) = small + (3.900e-10)
    end if

    !C2D + CO+ -> C2 + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(618) = small + (3.900e-10)
    end if

    !CH2 + CO+ -> CH + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(619) = small + (4.300e-10)
    end if

    !CD2 + CO+ -> CD + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(620) = small + (4.300e-10)
    end if

    !CHD + CO+ -> CH + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(621) = small + (4.300e-10)
    end if

    !CHD + CO+ -> CD + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(622) = small + (4.300e-10)
    end if

    !H2O + CO+ -> OH + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(623) = small + (8.800e-10)
    end if

    !D2O + CO+ -> OD + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(624) = small + (8.800e-10)
    end if

    !HDO + CO+ -> OH + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(625) = small + (8.800e-10)
    end if

    !HDO + CO+ -> OD + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(626) = small + (8.800e-10)
    end if

    !NH2 + CO+ -> NH + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(627) = small + (4.500e-10)
    end if

    !ND2 + CO+ -> ND + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(628) = small + (4.500e-10)
    end if

    !NHD + CO+ -> NH + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(629) = small + (4.500e-10)
    end if

    !NHD + CO+ -> ND + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(630) = small + (4.500e-10)
    end if

    !C + H2+_PARA -> H + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(631) = small + (2.400e-09)
    end if

    !C + H2+_ORTHO -> H + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(632) = small + (2.400e-09)
    end if

    !C + D2+_PARA -> D + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(633) = small + (2.400e-09)
    end if

    !C + D2+_ORTHO -> D + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(634) = small + (2.400e-09)
    end if

    !C + HD+ -> H + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(635) = small + (2.400e-09)
    end if

    !C + HD+ -> D + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(636) = small + (2.400e-09)
    end if

    !N + H2+_PARA -> H + NH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(637) = small + (1.900e-09)
    end if

    !N + H2+_ORTHO -> H + NH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(638) = small + (1.900e-09)
    end if

    !N + D2+_PARA -> D + ND+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(639) = small + (1.900e-09)
    end if

    !N + D2+_ORTHO -> D + ND+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(640) = small + (1.900e-09)
    end if

    !N + HD+ -> H + ND+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(641) = small + (1.900e-09)
    end if

    !N + HD+ -> D + NH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(642) = small + (1.900e-09)
    end if

    !O + H2+_PARA -> H + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(643) = small + (1.500e-09)
    end if

    !O + H2+_ORTHO -> H + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(644) = small + (1.500e-09)
    end if

    !O + D2+_PARA -> D + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(645) = small + (1.500e-09)
    end if

    !O + D2+_ORTHO -> D + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(646) = small + (1.500e-09)
    end if

    !O + HD+ -> H + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(647) = small + (1.500e-09)
    end if

    !O + HD+ -> D + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(648) = small + (1.500e-09)
    end if

    !C2 + H2+_PARA -> H + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(649) = small + (1.100e-09)
    end if

    !C2 + H2+_ORTHO -> H + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(650) = small + (1.100e-09)
    end if

    !C2 + D2+_PARA -> D + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(651) = small + (1.100e-09)
    end if

    !C2 + D2+_ORTHO -> D + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(652) = small + (1.100e-09)
    end if

    !C2 + HD+ -> H + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(653) = small + (1.100e-09)
    end if

    !C2 + HD+ -> D + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(654) = small + (1.100e-09)
    end if

    !CH + H2+_PARA -> H + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(655) = small + (7.100e-10)
    end if

    !CH + H2+_ORTHO -> H + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(656) = small + (7.100e-10)
    end if

    !CH + D2+_PARA -> H + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(657) = small + (7.100e-10)
    end if

    !CH + D2+_ORTHO -> H + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(658) = small + (7.100e-10)
    end if

    !CH + D2+_PARA -> D + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(659) = small + (7.100e-10)
    end if

    !CH + D2+_ORTHO -> D + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(660) = small + (7.100e-10)
    end if

    !CH + HD+ -> H + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(661) = small + (7.100e-10)
    end if

    !CH + HD+ -> D + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(662) = small + (7.100e-10)
    end if

    !CD + H2+_PARA -> H + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(663) = small + (7.100e-10)
    end if

    !CD + H2+_ORTHO -> H + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(664) = small + (7.100e-10)
    end if

    !CD + H2+_PARA -> D + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(665) = small + (7.100e-10)
    end if

    !CD + H2+_ORTHO -> D + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(666) = small + (7.100e-10)
    end if

    !CD + D2+_PARA -> D + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(667) = small + (7.100e-10)
    end if

    !CD + D2+_ORTHO -> D + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(668) = small + (7.100e-10)
    end if

    !CD + HD+ -> H + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(669) = small + (7.100e-10)
    end if

    !CD + HD+ -> D + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(670) = small + (7.100e-10)
    end if

    !CN + H2+_PARA -> H + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(671) = small + (1.200e-09)
    end if

    !CN + H2+_ORTHO -> H + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(672) = small + (1.200e-09)
    end if

    !CN + D2+_PARA -> D + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(673) = small + (1.200e-09)
    end if

    !CN + D2+_ORTHO -> D + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(674) = small + (1.200e-09)
    end if

    !CN + HD+ -> H + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(675) = small + (1.200e-09)
    end if

    !CN + HD+ -> D + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(676) = small + (1.200e-09)
    end if

    !CO + H2+_PARA -> H + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(677) = small + (2.200e-09)
    end if

    !CO + H2+_ORTHO -> H + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(678) = small + (2.200e-09)
    end if

    !CO + D2+_PARA -> D + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(679) = small + (2.200e-09)
    end if

    !CO + D2+_ORTHO -> D + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(680) = small + (2.200e-09)
    end if

    !CO + HD+ -> H + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(681) = small + (2.200e-09)
    end if

    !CO + HD+ -> D + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(682) = small + (2.200e-09)
    end if

    !H2_PARA + H2+_ORTHO -> H + H3+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(683) = small + (7.000e-10)
    end if

    !H2_ORTHO + H2+_PARA -> H + H3+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(684) = small + (7.000e-10)
    end if

    !H2_ORTHO + H2+_ORTHO -> H + H3+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(685) = small + (1.400e-09)
    end if

    !H2_PARA + D2+_PARA -> H + D2H+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(686) = small + (1.050e-09)
    end if

    !H2_PARA + D2+_ORTHO -> H + D2H+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(687) = small + (1.050e-09)
    end if

    !H2_ORTHO + D2+_PARA -> H + D2H+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(688) = small + (1.050e-09)
    end if

    !H2_ORTHO + D2+_ORTHO -> H + D2H+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(689) = small + (1.050e-09)
    end if

    !H2_PARA + D2+_PARA -> D + H2D+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(690) = small + (1.050e-09)
    end if

    !H2_PARA + D2+_ORTHO -> D + H2D+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(691) = small + (1.050e-09)
    end if

    !H2_ORTHO + D2+_PARA -> D + H2D+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(692) = small + (1.050e-09)
    end if

    !H2_ORTHO + D2+_ORTHO -> D + H2D+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(693) = small + (1.050e-09)
    end if

    !H2_PARA + HD+ -> H + H2D+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(694) = small + (5.250e-10)
    end if

    !H2_PARA + HD+ -> H + H2D+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(695) = small + (5.250e-10)
    end if

    !H2_ORTHO + HD+ -> H + H2D+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(696) = small + (5.250e-10)
    end if

    !H2_ORTHO + HD+ -> H + H2D+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(697) = small + (5.250e-10)
    end if

    !H2_PARA + HD+ -> D + H3+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(698) = small + (5.250e-10)
    end if

    !H2_ORTHO + HD+ -> D + H3+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(699) = small + (5.250e-10)
    end if

    !D2_PARA + H2+_PARA -> H + D2H+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(700) = small + (1.050e-09)
    end if

    !D2_PARA + H2+_ORTHO -> H + D2H+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(701) = small + (1.050e-09)
    end if

    !D2_PARA + H2+_ORTHO -> H + D2H+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(702) = small + (1.050e-09)
    end if

    !D2_ORTHO + H2+_PARA -> H + D2H+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(703) = small + (1.050e-09)
    end if

    !D2_ORTHO + H2+_PARA -> H + D2H+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(704) = small + (1.050e-09)
    end if

    !D2_ORTHO + H2+_ORTHO -> H + D2H+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(705) = small + (1.050e-09)
    end if

    !D2_ORTHO + H2+_ORTHO -> H + D2H+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(706) = small + (1.050e-09)
    end if

    !D2_PARA + H2+_PARA -> D + H2D+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(707) = small + (1.050e-09)
    end if

    !D2_PARA + H2+_ORTHO -> D + H2D+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(708) = small + (1.050e-09)
    end if

    !D2_ORTHO + H2+_PARA -> D + H2D+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(709) = small + (1.050e-09)
    end if

    !D2_ORTHO + H2+_ORTHO -> D + H2D+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(710) = small + (1.050e-09)
    end if

    !D2_PARA + D2+_PARA -> D + D3+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(711) = small + (2.100e-09)
    end if

    !D2_PARA + D2+_PARA -> D + D3+_META
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(712) = small + (2.100e-09)
    end if

    !D2_PARA + D2+_ORTHO -> D + D3+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(713) = small + (7.000e-10)
    end if

    !D2_PARA + D2+_ORTHO -> D + D3+_META
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(714) = small + (7.000e-10)
    end if

    !D2_ORTHO + D2+_PARA -> D + D3+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(715) = small + (7.000e-10)
    end if

    !D2_ORTHO + D2+_PARA -> D + D3+_META
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(716) = small + (7.000e-10)
    end if

    !D2_ORTHO + D2+_ORTHO -> D + D3+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(717) = small + (7.000e-10)
    end if

    !D2_ORTHO + D2+_ORTHO -> D + D3+_META
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(718) = small + (7.000e-09)
    end if

    !D2_PARA + HD+ -> H + D3+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(719) = small + (4.200e-10)
    end if

    !D2_PARA + HD+ -> H + D3+_META
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(720) = small + (4.200e-10)
    end if

    !D2_ORTHO + HD+ -> H + D3+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(721) = small + (4.200e-10)
    end if

    !D2_ORTHO + HD+ -> H + D3+_META
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(722) = small + (4.200e-10)
    end if

    !D2_PARA + HD+ -> D + D2H+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(723) = small + (4.200e-10)
    end if

    !D2_PARA + HD+ -> D + D2H+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(724) = small + (4.200e-10)
    end if

    !D2_ORTHO + HD+ -> D + D2H+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(725) = small + (4.200e-10)
    end if

    !D2_ORTHO + HD+ -> D + D2H+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(726) = small + (4.200e-10)
    end if

    !HD + H2+_PARA -> H + H2D+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(727) = small + (7.000e-10)
    end if

    !HD + H2+_PARA -> H + H2D+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(728) = small + (7.000e-10)
    end if

    !HD + H2+_ORTHO -> H + H2D+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(729) = small + (5.250e-10)
    end if

    !HD + H2+_ORTHO -> H + H2D+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(730) = small + (5.250e-10)
    end if

    !HD + H2+_ORTHO -> D + H3+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(731) = small + (5.250e-10)
    end if

    !HD + D2+_PARA -> H + D3+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(732) = small + (1.050e-09)
    end if

    !HD + D2+_ORTHO -> H + D3+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(733) = small + (7.000e-10)
    end if

    !HD + D2+_ORTHO -> H + D3+_META
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(734) = small + (7.000e-10)
    end if

    !HD + D2+_PARA -> D + D2H+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(735) = small + (1.050e-09)
    end if

    !HD + D2+_ORTHO -> D + D2H+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(736) = small + (7.000e-10)
    end if

    !HD + HD+ -> H + D2H+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(737) = small + (5.250e-10)
    end if

    !HD + HD+ -> H + D2H+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(738) = small + (5.250e-10)
    end if

    !HD + HD+ -> D + H2D+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(739) = small + (5.250e-10)
    end if

    !HD + HD+ -> D + H2D+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(740) = small + (5.250e-10)
    end if

    !N2 + H2+_PARA -> H + N2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(741) = small + (2.000e-09)
    end if

    !N2 + H2+_ORTHO -> H + N2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(742) = small + (2.000e-09)
    end if

    !N2 + D2+_PARA -> D + N2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(743) = small + (2.000e-09)
    end if

    !N2 + D2+_ORTHO -> D + N2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(744) = small + (2.000e-09)
    end if

    !N2 + HD+ -> H + N2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(745) = small + (2.000e-09)
    end if

    !N2 + HD+ -> D + N2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(746) = small + (2.000e-09)
    end if

    !NH + H2+_PARA -> H + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(747) = small + (7.600e-10)
    end if

    !NH + H2+_ORTHO -> H + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(748) = small + (7.600e-10)
    end if

    !NH + D2+_PARA -> H + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(749) = small + (7.600e-10)
    end if

    !NH + D2+_ORTHO -> H + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(750) = small + (7.600e-10)
    end if

    !NH + D2+_PARA -> D + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(751) = small + (7.600e-10)
    end if

    !NH + D2+_ORTHO -> D + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(752) = small + (7.600e-10)
    end if

    !NH + HD+ -> H + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(753) = small + (7.600e-10)
    end if

    !NH + HD+ -> D + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(754) = small + (7.600e-10)
    end if

    !ND + H2+_PARA -> H + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(755) = small + (7.600e-10)
    end if

    !ND + H2+_ORTHO -> H + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(756) = small + (7.600e-10)
    end if

    !ND + H2+_PARA -> D + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(757) = small + (7.600e-10)
    end if

    !ND + H2+_ORTHO -> D + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(758) = small + (7.600e-10)
    end if

    !ND + D2+_PARA -> D + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(759) = small + (7.600e-10)
    end if

    !ND + D2+_ORTHO -> D + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(760) = small + (7.600e-10)
    end if

    !ND + HD+ -> H + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(761) = small + (7.600e-10)
    end if

    !ND + HD+ -> D + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(762) = small + (7.600e-10)
    end if

    !NO + H2+_PARA -> H + HNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(763) = small + (1.100e-09)
    end if

    !NO + H2+_ORTHO -> H + HNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(764) = small + (1.100e-09)
    end if

    !NO + D2+_PARA -> D + DNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(765) = small + (1.100e-09)
    end if

    !NO + D2+_ORTHO -> D + DNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(766) = small + (1.100e-09)
    end if

    !NO + HD+ -> H + DNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(767) = small + (1.100e-09)
    end if

    !NO + HD+ -> D + HNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(768) = small + (1.100e-09)
    end if

    !O2 + H2+_PARA -> H + O2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(769) = small + (1.900e-09)
    end if

    !O2 + H2+_ORTHO -> H + O2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(770) = small + (1.900e-09)
    end if

    !O2 + D2+_PARA -> D + O2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(771) = small + (1.900e-09)
    end if

    !O2 + D2+_ORTHO -> D + O2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(772) = small + (1.900e-09)
    end if

    !O2 + HD+ -> H + O2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(773) = small + (1.900e-09)
    end if

    !O2 + HD+ -> D + O2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(774) = small + (1.900e-09)
    end if

    !OH + H2+_PARA -> H + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(775) = small + (7.600e-10)
    end if

    !OH + H2+_ORTHO -> H + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(776) = small + (7.600e-10)
    end if

    !OH + D2+_PARA -> H + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(777) = small + (7.600e-10)
    end if

    !OH + D2+_ORTHO -> H + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(778) = small + (7.600e-10)
    end if

    !OH + D2+_PARA -> D + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(779) = small + (7.600e-10)
    end if

    !OH + D2+_ORTHO -> D + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(780) = small + (7.600e-10)
    end if

    !OH + HD+ -> H + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(781) = small + (7.600e-10)
    end if

    !OH + HD+ -> D + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(782) = small + (7.600e-10)
    end if

    !OD + H2+_PARA -> H + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(783) = small + (7.600e-10)
    end if

    !OD + H2+_ORTHO -> H + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(784) = small + (7.600e-10)
    end if

    !OD + H2+_PARA -> D + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(785) = small + (7.600e-10)
    end if

    !OD + H2+_ORTHO -> D + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(786) = small + (7.600e-10)
    end if

    !OD + D2+_PARA -> D + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(787) = small + (7.600e-10)
    end if

    !OD + D2+_ORTHO -> D + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(788) = small + (7.600e-10)
    end if

    !OD + HD+ -> H + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(789) = small + (7.600e-10)
    end if

    !OD + HD+ -> D + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(790) = small + (7.600e-10)
    end if

    !CO2 + H2+_PARA -> H2O + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(791) = small + (1.400e-09)
    end if

    !CO2 + H2+_ORTHO -> H2O + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(792) = small + (1.400e-09)
    end if

    !CO2 + D2+_PARA -> D2O + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(793) = small + (1.400e-09)
    end if

    !CO2 + D2+_ORTHO -> D2O + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(794) = small + (1.400e-09)
    end if

    !CO2 + HD+ -> HDO + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(795) = small + (1.400e-09)
    end if

    !HCO + H2+_ORTHO -> CO + H3+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(796) = small + (5.000e-10)
    end if

    !HCO + D2+_PARA -> CO + D2H+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(797) = small + (1.000e-09)
    end if

    !HCO + D2+_ORTHO -> CO + D2H+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(798) = small + (5.000e-10)
    end if

    !HCO + D2+_ORTHO -> CO + D2H+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(799) = small + (5.000e-10)
    end if

    !HCO + HD+ -> CO + H2D+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(800) = small + (5.000e-10)
    end if

    !HCO + HD+ -> CO + H2D+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(801) = small + (5.000e-10)
    end if

    !DCO + H2+_PARA -> CO + H2D+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(802) = small + (1.000e-09)
    end if

    !DCO + H2+_ORTHO -> CO + H2D+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(803) = small + (5.000e-10)
    end if

    !DCO + H2+_ORTHO -> CO + H2D+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(804) = small + (5.000e-10)
    end if

    !DCO + D2+_PARA -> CO + D3+_META
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(805) = small + (5.000e-10)
    end if

    !DCO + D2+_ORTHO -> CO + D3+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(806) = small + (3.333e-10)
    end if

    !DCO + D2+_ORTHO -> CO + D3+_META
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(807) = small + (3.333e-10)
    end if

    !DCO + HD+ -> CO + D2H+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(808) = small + (5.000e-10)
    end if

    !DCO + HD+ -> CO + D2H+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(809) = small + (5.000e-10)
    end if

    !H + HEH+ -> HE + H2+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(810) = small + (9.000e-10)
    end if

    !H + HED+ -> HE + HD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(811) = small + (9.000e-10)
    end if

    !D + HEH+ -> HE + HD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(812) = small + (9.000e-10)
    end if

    !D + HED+ -> HE + D2+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(813) = small + (9.000e-10)
    end if

    !H2_ORTHO + HEH+ -> HE + H3+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(814) = small + (9.000e-10)
    end if

    !H2_PARA + HED+ -> HE + H2D+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(815) = small + (1.800e-09)
    end if

    !H2_ORTHO + HED+ -> HE + H2D+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(816) = small + (9.000e-10)
    end if

    !H2_ORTHO + HED+ -> HE + H2D+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(817) = small + (9.000e-10)
    end if

    !D2_PARA + HEH+ -> HE + D2H+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(818) = small + (1.800e-09)
    end if

    !D2_ORTHO + HEH+ -> HE + D2H+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(819) = small + (9.000e-10)
    end if

    !D2_ORTHO + HEH+ -> HE + D2H+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(820) = small + (9.000e-10)
    end if

    !D2_PARA + HED+ -> HE + D3+_META
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(821) = small + (9.000e-10)
    end if

    !D2_ORTHO + HED+ -> HE + D3+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(822) = small + (6.000e-10)
    end if

    !D2_ORTHO + HED+ -> HE + D3+_META
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(823) = small + (6.000e-10)
    end if

    !HD + HEH+ -> HE + H2D+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(824) = small + (9.000e-10)
    end if

    !HD + HEH+ -> HE + H2D+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(825) = small + (9.000e-10)
    end if

    !HD + HED+ -> HE + D2H+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(826) = small + (9.000e-10)
    end if

    !HD + HED+ -> HE + D2H+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(827) = small + (9.000e-10)
    end if

    !H3+_ORTHO + H- -> H2_PARA + H2_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(828) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !H3+_PARA + H- -> H2_PARA + H2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(829) = small + (1.150e-07&
          *(T32)**(-5.000e-01))
    end if

    !H3+_PARA + H- -> H2_PARA + H2_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(830) = small + (1.150e-07&
          *(T32)**(-5.000e-01))
    end if

    !H3+_ORTHO + D- -> H2_PARA + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(831) = small + (1.150e-07&
          *(T32)**(-5.000e-01))
    end if

    !H3+_ORTHO + D- -> H2_ORTHO + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(832) = small + (1.150e-07&
          *(T32)**(-5.000e-01))
    end if

    !H3+_PARA + D- -> H2_PARA + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(833) = small + (1.150e-07&
          *(T32)**(-5.000e-01))
    end if

    !H3+_PARA + D- -> H2_ORTHO + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(834) = small + (1.150e-07&
          *(T32)**(-5.000e-01))
    end if

    !D3+_ORTHO + D- -> D2_PARA + D2_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(835) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !D3+_META + D- -> D2_PARA + D2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(836) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !H2D+_ORTHO + H- -> H2_PARA + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(837) = small + (1.150e-07&
          *(T32)**(-5.000e-01))
    end if

    !H2D+_ORTHO + H- -> H2_ORTHO + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(838) = small + (1.150e-07&
          *(T32)**(-5.000e-01))
    end if

    !H2D+_PARA + H- -> H2_PARA + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(839) = small + (1.150e-07&
          *(T32)**(-5.000e-01))
    end if

    !H2D+_PARA + H- -> H2_ORTHO + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(840) = small + (1.150e-07&
          *(T32)**(-5.000e-01))
    end if

    !H2D+_ORTHO + D- -> H2_PARA + D2_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(841) = small + (7.667e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2D+_ORTHO + D- -> H2_ORTHO + D2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(842) = small + (7.667e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2D+_PARA + D- -> H2_PARA + D2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(843) = small + (5.750e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2D+_PARA + D- -> H2_PARA + D2_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(844) = small + (5.750e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2D+_PARA + D- -> H2_ORTHO + D2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(845) = small + (5.750e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2D+_ORTHO + D- -> HD + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(846) = small + (7.667e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2D+_PARA + D- -> HD + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(847) = small + (5.750e-08&
          *(T32)**(-5.000e-01))
    end if

    !D2H+_ORTHO + H- -> H2_PARA + D2_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(848) = small + (7.667e-08&
          *(T32)**(-5.000e-01))
    end if

    !D2H+_ORTHO + H- -> H2_ORTHO + D2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(849) = small + (7.667e-08&
          *(T32)**(-5.000e-01))
    end if

    !D2H+_PARA + H- -> H2_PARA + D2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(850) = small + (5.750e-08&
          *(T32)**(-5.000e-01))
    end if

    !D2H+_PARA + H- -> H2_PARA + D2_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(851) = small + (5.750e-08&
          *(T32)**(-5.000e-01))
    end if

    !D2H+_PARA + H- -> H2_ORTHO + D2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(852) = small + (5.750e-08&
          *(T32)**(-5.000e-01))
    end if

    !D2H+_ORTHO + H- -> HD + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(853) = small + (7.667e-08&
          *(T32)**(-5.000e-01))
    end if

    !D2H+_PARA + H- -> HD + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(854) = small + (5.750e-08&
          *(T32)**(-5.000e-01))
    end if

    !D2H+_ORTHO + D- -> D2_PARA + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(855) = small + (1.150e-07&
          *(T32)**(-5.000e-01))
    end if

    !D2H+_ORTHO + D- -> D2_ORTHO + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(856) = small + (1.150e-07&
          *(T32)**(-5.000e-01))
    end if

    !D2H+_PARA + D- -> D2_PARA + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(857) = small + (1.150e-07&
          *(T32)**(-5.000e-01))
    end if

    !D2H+_PARA + D- -> D2_ORTHO + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(858) = small + (1.150e-07&
          *(T32)**(-5.000e-01))
    end if

    !HCO+ + H- -> CO + H2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(859) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !HCO+ + D- -> CO + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(860) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !DCO+ + H- -> CO + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(861) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !DCO+ + D- -> CO + D2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(862) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !C2 + HE+ -> C + HE + C+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(863) = small + (1.600e-09)
    end if

    !CH + HE+ -> H + HE + C+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(864) = small + (3.830e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD + HE+ -> D + HE + C+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(865) = small + (3.830e-09&
          *(T32)**(-5.000e-01))
    end if

    !CN + HE+ -> HE + N + C+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(866) = small + (3.600e-09&
          *(T32)**(-5.000e-01))
    end if

    !CN + HE+ -> C + HE + N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(867) = small + (3.600e-09&
          *(T32)**(-5.000e-01))
    end if

    !CO + HE+ -> HE + O + C+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(868) = small + (1.600e-09)
    end if

    !H2_PARA + HE+ -> H + HE + H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(869) = small + (3.300e-15)
    end if

    !H2_ORTHO + HE+ -> H + HE + H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(870) = small + (3.300e-15)
    end if

    !D2_PARA + HE+ -> D + HE + D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(871) = small + (3.300e-15)
    end if

    !D2_ORTHO + HE+ -> D + HE + D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(872) = small + (3.300e-15)
    end if

    !HD + HE+ -> H + HE + D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(873) = small + (3.300e-15)
    end if

    !HD + HE+ -> D + HE + H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(874) = small + (3.300e-15)
    end if

    !N2 + HE+ -> HE + N + N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(875) = small + (8.000e-10)
    end if

    !NH + HE+ -> H + HE + N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(876) = small + (6.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND + HE+ -> D + HE + N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(877) = small + (6.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !NO + HE+ -> HE + O + N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(878) = small + (6.400e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO + HE+ -> HE + N + O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(879) = small + (1.000e-10&
          *(T32)**(-5.000e-01))
    end if

    !O2 + HE+ -> HE + O + O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(880) = small + (1.000e-09)
    end if

    !OH + HE+ -> H + HE + O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(881) = small + (8.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !OD + HE+ -> D + HE + O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(882) = small + (8.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !C2H + HE+ -> HE + CH + C+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(883) = small + (1.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !C2D + HE+ -> HE + CD + C+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(884) = small + (1.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !C2H + HE+ -> H + HE + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(885) = small + (1.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !C2D + HE+ -> D + HE + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(886) = small + (1.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !C2H + HE+ -> C + HE + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(887) = small + (1.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !C2D + HE+ -> C + HE + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(888) = small + (1.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !C2N + HE+ -> HE + CN + C+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(889) = small + (2.890e-09&
          *(T32)**(-5.000e-01))
    end if

    !C3 + HE+ -> HE + C2 + C+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(890) = small + (1.000e-09)
    end if

    !C3 + HE+ -> C + HE + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(891) = small + (1.000e-09)
    end if

    !CCO + HE+ -> HE + CO + C+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(892) = small + (6.260e-09&
          *(T32)**(-5.000e-01))
    end if

    !CH2 + HE+ -> HE + H2_PARA + C+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(893) = small + (6.250e-10&
          *(T32)**(-5.000e-01))
    end if

    !CD2 + HE+ -> HE + D2_PARA + C+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(894) = small + (6.250e-10&
          *(T32)**(-5.000e-01))
    end if

    !CHD + HE+ -> HE + HD + C+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(895) = small + (6.250e-10&
          *(T32)**(-5.000e-01))
    end if

    !CH2 + HE+ -> H + HE + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(896) = small + (6.250e-10&
          *(T32)**(-5.000e-01))
    end if

    !CD2 + HE+ -> D + HE + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(897) = small + (6.250e-10&
          *(T32)**(-5.000e-01))
    end if

    !CHD + HE+ -> H + HE + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(898) = small + (3.125e-10&
          *(T32)**(-5.000e-01))
    end if

    !CHD + HE+ -> D + HE + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(899) = small + (3.125e-10&
          *(T32)**(-5.000e-01))
    end if

    !CO2 + HE+ -> HE + O2 + C+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(900) = small + (4.000e-11)
    end if

    !CO2 + HE+ -> HE + CO + O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(901) = small + (2.000e-10)
    end if

    !CO2 + HE+ -> HE + O + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(902) = small + (8.000e-10)
    end if

    !CO2 + HE+ -> C + HE + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(903) = small + (1.100e-11)
    end if

    !H2O + HE+ -> HE + OH + H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(904) = small + (1.320e-09&
          *(T32)**(-5.000e-01))
    end if

    !D2O + HE+ -> HE + OD + D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(905) = small + (1.320e-09&
          *(T32)**(-5.000e-01))
    end if

    !HDO + HE+ -> HE + OH + D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(906) = small + (6.600e-10&
          *(T32)**(-5.000e-01))
    end if

    !HDO + HE+ -> HE + OD + H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(907) = small + (6.600e-10&
          *(T32)**(-5.000e-01))
    end if

    !H2O + HE+ -> H + HE + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(908) = small + (1.320e-09&
          *(T32)**(-5.000e-01))
    end if

    !D2O + HE+ -> D + HE + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(909) = small + (1.320e-09&
          *(T32)**(-5.000e-01))
    end if

    !HDO + HE+ -> H + HE + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(910) = small + (6.600e-10&
          *(T32)**(-5.000e-01))
    end if

    !HDO + HE+ -> D + HE + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(911) = small + (6.600e-10&
          *(T32)**(-5.000e-01))
    end if

    !HCN + HE+ -> H + HE + N + C+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(912) = small + (3.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !DCN + HE+ -> D + HE + N + C+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(913) = small + (3.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !HCN + HE+ -> HE + CH + N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(914) = small + (1.000e-09&
          *(T32)**(-5.000e-01))
    end if

    !DCN + HE+ -> HE + CD + N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(915) = small + (1.000e-09&
          *(T32)**(-5.000e-01))
    end if

    !HCN + HE+ -> HE + N + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(916) = small + (3.100e-09&
          *(T32)**(-5.000e-01))
    end if

    !DCN + HE+ -> HE + N + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(917) = small + (3.100e-09&
          *(T32)**(-5.000e-01))
    end if

    !HCN + HE+ -> H + HE + CN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(918) = small + (6.900e-09&
          *(T32)**(-5.000e-01))
    end if

    !DCN + HE+ -> D + HE + CN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(919) = small + (6.900e-09&
          *(T32)**(-5.000e-01))
    end if

    !HCO + HE+ -> HE + O + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(920) = small + (6.900e-10&
          *(T32)**(-5.000e-01))
    end if

    !DCO + HE+ -> HE + O + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(921) = small + (6.900e-10&
          *(T32)**(-5.000e-01))
    end if

    !HCO + HE+ -> H + HE + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(922) = small + (6.900e-10&
          *(T32)**(-5.000e-01))
    end if

    !DCO + HE+ -> D + HE + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(923) = small + (6.900e-10&
          *(T32)**(-5.000e-01))
    end if

    !HCO + HE+ -> CO + HEH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(924) = small + (6.900e-10&
          *(T32)**(-5.000e-01))
    end if

    !DCO + HE+ -> CO + HED+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(925) = small + (6.900e-10&
          *(T32)**(-5.000e-01))
    end if

    !HNC + HE+ -> H + HE + N + C+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(926) = small + (4.430e-09&
          *(T32)**(-5.000e-01))
    end if

    !DNC + HE+ -> D + HE + N + C+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(927) = small + (4.430e-09&
          *(T32)**(-5.000e-01))
    end if

    !HNC + HE+ -> H + HE + CN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(928) = small + (4.430e-09&
          *(T32)**(-5.000e-01))
    end if

    !DNC + HE+ -> D + HE + CN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(929) = small + (4.430e-09&
          *(T32)**(-5.000e-01))
    end if

    !HNC + HE+ -> C + HE + NH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(930) = small + (4.430e-09&
          *(T32)**(-5.000e-01))
    end if

    !DNC + HE+ -> C + HE + ND+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(931) = small + (4.430e-09&
          *(T32)**(-5.000e-01))
    end if

    !HNO + HE+ -> HE + NO + H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(932) = small + (1.800e-09&
          *(T32)**(-5.000e-01))
    end if

    !DNO + HE+ -> HE + NO + D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(933) = small + (1.800e-09&
          *(T32)**(-5.000e-01))
    end if

    !HNO + HE+ -> H + HE + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(934) = small + (1.800e-09&
          *(T32)**(-5.000e-01))
    end if

    !DNO + HE+ -> D + HE + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(935) = small + (1.800e-09&
          *(T32)**(-5.000e-01))
    end if

    !N2O + HE+ -> HE + NO + N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(936) = small + (8.450e-11&
          *(T32)**(-5.000e-01))
    end if

    !N2O + HE+ -> HE + N2 + O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(937) = small + (8.450e-11&
          *(T32)**(-5.000e-01))
    end if

    !N2O + HE+ -> HE + O + N2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(938) = small + (8.450e-11&
          *(T32)**(-5.000e-01))
    end if

    !N2O + HE+ -> HE + N + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(939) = small + (8.450e-11&
          *(T32)**(-5.000e-01))
    end if

    !NH2 + HE+ -> HE + H2_PARA + N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(940) = small + (2.000e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND2 + HE+ -> HE + D2_PARA + N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(941) = small + (2.000e-09&
          *(T32)**(-5.000e-01))
    end if

    !NHD + HE+ -> HE + HD + N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(942) = small + (2.000e-09&
          *(T32)**(-5.000e-01))
    end if

    !NH2 + HE+ -> H + HE + NH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(943) = small + (2.000e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND2 + HE+ -> D + HE + ND+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(944) = small + (2.000e-09&
          *(T32)**(-5.000e-01))
    end if

    !NHD + HE+ -> H + HE + ND+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(945) = small + (1.000e-09&
          *(T32)**(-5.000e-01))
    end if

    !NHD + HE+ -> D + HE + NH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(946) = small + (1.000e-09&
          *(T32)**(-5.000e-01))
    end if

    !OCN + HE+ -> HE + CN + O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(947) = small + (1.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !OCN + HE+ -> HE + O + CN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(948) = small + (1.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !C + C2H+ -> H + C3+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(949) = small + (1.100e-09)
    end if

    !C + C2D+ -> D + C3+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(950) = small + (1.100e-09)
    end if

    !N + C2H+ -> CN + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(951) = small + (9.000e-11)
    end if

    !N + C2D+ -> CN + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(952) = small + (9.000e-11)
    end if

    !N + C2H+ -> H + C2N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(953) = small + (1.000e-10)
    end if

    !N + C2D+ -> D + C2N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(954) = small + (1.000e-10)
    end if

    !O + C2H+ -> C + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(955) = small + (3.300e-10)
    end if

    !O + C2D+ -> C + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(956) = small + (3.300e-10)
    end if

    !CH + C2H+ -> C2 + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(957) = small + (3.200e-10)
    end if

    !CH + C2D+ -> C2 + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(958) = small + (3.200e-10)
    end if

    !CD + C2H+ -> C2 + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(959) = small + (3.200e-10)
    end if

    !CD + C2D+ -> C2 + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(960) = small + (3.200e-10)
    end if

    !H2O + C2N+ -> HCN + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(961) = small + (1.500e-09)
    end if

    !D2O + C2N+ -> DCN + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(962) = small + (1.500e-09)
    end if

    !HDO + C2N+ -> HCN + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(963) = small + (1.500e-09)
    end if

    !HDO + C2N+ -> DCN + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(964) = small + (1.500e-09)
    end if

    !C + CH2+ -> H + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(965) = small + (1.200e-09)
    end if

    !C + CD2+ -> D + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(966) = small + (1.200e-09)
    end if

    !C + CHD+ -> H + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(967) = small + (6.000e-10)
    end if

    !C + CHD+ -> D + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(968) = small + (6.000e-10)
    end if

    !N + CH2+ -> H + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(969) = small + (2.200e-10)
    end if

    !N + CD2+ -> D + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(970) = small + (2.200e-10)
    end if

    !N + CHD+ -> H + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(971) = small + (1.100e-10)
    end if

    !N + CHD+ -> D + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(972) = small + (1.100e-10)
    end if

    !O + CH2+ -> H + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(973) = small + (7.500e-10)
    end if

    !O + CD2+ -> D + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(974) = small + (7.500e-10)
    end if

    !O + CHD+ -> H + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(975) = small + (3.750e-10)
    end if

    !O + CHD+ -> D + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(976) = small + (3.750e-10)
    end if

    !O2 + CH2+ -> OH + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(977) = small + (9.100e-10)
    end if

    !O2 + CD2+ -> OD + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(978) = small + (9.100e-10)
    end if

    !O2 + CHD+ -> OH + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(979) = small + (4.550e-10)
    end if

    !O2 + CHD+ -> OD + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(980) = small + (4.550e-10)
    end if

    !H2O + CNC+ -> HCN + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(981) = small + (6.400e-11)
    end if

    !D2O + CNC+ -> DCN + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(982) = small + (6.400e-11)
    end if

    !HDO + CNC+ -> HCN + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(983) = small + (6.400e-11)
    end if

    !HDO + CNC+ -> DCN + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(984) = small + (6.400e-11)
    end if

    !H + CO2+ -> O + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(985) = small + (2.900e-10)
    end if

    !D + CO2+ -> O + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(986) = small + (2.900e-10)
    end if

    !O + CO2+ -> CO + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(987) = small + (1.640e-10)
    end if

    !C + H2O+ -> OH + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(988) = small + (1.100e-09)
    end if

    !C + D2O+ -> OD + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(989) = small + (1.100e-09)
    end if

    !C + HDO+ -> OH + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(990) = small + (5.500e-10)
    end if

    !C + HDO+ -> OD + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(991) = small + (5.500e-10)
    end if

    !N + H2O+ -> H + HNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(992) = small + (1.900e-10)
    end if

    !N + D2O+ -> D + DNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(993) = small + (1.900e-10)
    end if

    !N + HDO+ -> H + DNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(994) = small + (9.500e-11)
    end if

    !N + HDO+ -> D + HNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(995) = small + (9.500e-11)
    end if

    !O + H2O+ -> H2_PARA + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(996) = small + (4.000e-11)
    end if

    !O + D2O+ -> D2_PARA + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(997) = small + (4.000e-11)
    end if

    !O + HDO+ -> HD + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(998) = small + (4.000e-11)
    end if

    !C + HCN+ -> CN + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(999) = small + (1.100e-09)
    end if

    !C + DCN+ -> CN + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1000) = small + (1.100e-09)
    end if

    !C2 + HCN+ -> CN + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1001) = small + (8.400e-10)
    end if

    !C2 + DCN+ -> CN + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1002) = small + (8.400e-10)
    end if

    !CH + HCN+ -> CN + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1003) = small + (6.300e-10)
    end if

    !CH + DCN+ -> CN + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1004) = small + (6.300e-10)
    end if

    !CD + HCN+ -> CN + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1005) = small + (6.300e-10)
    end if

    !CD + DCN+ -> CN + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1006) = small + (6.300e-10)
    end if

    !CO + HCN+ -> CN + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1007) = small + (1.400e-10)
    end if

    !CO + DCN+ -> CN + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1008) = small + (1.400e-10)
    end if

    !NH + HCN+ -> CN + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1009) = small + (6.500e-10)
    end if

    !NH + DCN+ -> CN + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1010) = small + (6.500e-10)
    end if

    !ND + HCN+ -> CN + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1011) = small + (6.500e-10)
    end if

    !ND + DCN+ -> CN + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1012) = small + (6.500e-10)
    end if

    !OH + HCN+ -> CN + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1013) = small + (6.300e-10)
    end if

    !OH + DCN+ -> CN + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1014) = small + (6.300e-10)
    end if

    !OD + HCN+ -> CN + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1015) = small + (6.300e-10)
    end if

    !OD + DCN+ -> CN + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1016) = small + (6.300e-10)
    end if

    !C + HCO+ -> CO + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1017) = small + (1.100e-09)
    end if

    !C + DCO+ -> CO + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1018) = small + (1.100e-09)
    end if

    !C2 + HCO+ -> CO + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1019) = small + (8.300e-10)
    end if

    !C2 + DCO+ -> CO + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1020) = small + (8.300e-10)
    end if

    !CH + HCO+ -> CO + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1021) = small + (4.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !CH + DCO+ -> CO + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1022) = small + (4.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD + HCO+ -> CO + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1023) = small + (4.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD + DCO+ -> CO + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1024) = small + (4.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !NH + HCO+ -> CO + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1025) = small + (3.800e-09&
          *(T32)**(-5.000e-01))
    end if

    !NH + DCO+ -> CO + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1026) = small + (3.800e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND + HCO+ -> CO + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1027) = small + (3.800e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND + DCO+ -> CO + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1028) = small + (3.800e-09&
          *(T32)**(-5.000e-01))
    end if

    !OH + HCO+ -> CO + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1029) = small + (2.330e-09&
          *(T32)**(-5.000e-01))
    end if

    !OH + DCO+ -> CO + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1030) = small + (2.330e-09&
          *(T32)**(-5.000e-01))
    end if

    !OD + HCO+ -> CO + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1031) = small + (2.330e-09&
          *(T32)**(-5.000e-01))
    end if

    !OD + DCO+ -> CO + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1032) = small + (2.330e-09&
          *(T32)**(-5.000e-01))
    end if

    !O2 + C2H -> CO + HCO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1033) = small + (4.200e-11&
          *(T32)**(-3.200e-01))
    end if

    !O2 + C2D -> CO + DCO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1034) = small + (4.200e-11&
          *(T32)**(-3.200e-01))
    end if

    !C + CH2 -> CH + CH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1035) = small + (2.690e-12&
          *exp(-2.360e+04*invT))
    end if

    !C + CD2 -> CD + CD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1036) = small + (2.690e-12&
          *exp(-2.360e+04*invT))
    end if

    !C + CHD -> CH + CD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1037) = small + (2.690e-12&
          *exp(-2.360e+04*invT))
    end if

    !C + CH2 -> H + C2H
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1038) = small + (1.000e-10)
    end if

    !C + CD2 -> D + C2D
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1039) = small + (1.000e-10)
    end if

    !C + CHD -> H + C2D
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1040) = small + (1.000e-10)
    end if

    !C + CHD -> D + C2H
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1041) = small + (1.000e-10)
    end if

    !C + NH2 -> CH + NH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1042) = small + (9.610e-13&
          *exp(-1.050e+04*invT))
    end if

    !C + ND2 -> CD + ND
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1043) = small + (9.610e-13&
          *exp(-1.050e+04*invT))
    end if

    !C + NHD -> CH + ND
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1044) = small + (9.610e-13&
          *exp(-1.050e+04*invT))
    end if

    !C + NHD -> CD + NH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1045) = small + (9.610e-13&
          *exp(-1.050e+04*invT))
    end if

    !C + NH2 -> H + HNC
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1046) = small + (3.400e-11&
          *(T32)**(-3.600e-01))
    end if

    !C + ND2 -> D + DNC
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1047) = small + (3.400e-11&
          *(T32)**(-3.600e-01))
    end if

    !C + NHD -> H + DNC
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1048) = small + (3.400e-11&
          *(T32)**(-3.600e-01))
    end if

    !C + NHD -> D + HNC
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1049) = small + (3.400e-11&
          *(T32)**(-3.600e-01))
    end if

    !C + NH2 -> H + HCN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1050) = small + (3.400e-11&
          *(T32)**(-3.600e-01))
    end if

    !C + ND2 -> D + DCN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1051) = small + (3.400e-11&
          *(T32)**(-3.600e-01))
    end if

    !C + NHD -> H + DCN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1052) = small + (3.400e-11&
          *(T32)**(-3.600e-01))
    end if

    !C + NHD -> D + HCN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1053) = small + (3.400e-11&
          *(T32)**(-3.600e-01))
    end if

    !C + CN -> N + C2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1054) = small + (4.980e-10&
          *exp(-1.810e+04*invT))
    end if

    !C + N2 -> N + CN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1055) = small + (8.700e-11&
          *exp(-2.260e+04*invT))
    end if

    !C + CO -> O + C2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1056) = small + (1.000e-10&
          *exp(-5.280e+04*invT))
    end if

    !H + HCO -> CO + H2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1057) = small + (1.500e-10)
    end if

    !H + DCO -> CO + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1058) = small + (1.500e-10)
    end if

    !D + HCO -> CO + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1059) = small + (1.500e-10)
    end if

    !D + DCO -> CO + D2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1060) = small + (1.500e-10)
    end if

    !H + HCO -> O + CH2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1061) = small + (6.610e-11&
          *exp(-5.160e+04*invT))
    end if

    !H + DCO -> O + CHD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1062) = small + (6.610e-11&
          *exp(-5.160e+04*invT))
    end if

    !D + HCO -> O + CHD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1063) = small + (6.610e-11&
          *exp(-5.160e+04*invT))
    end if

    !D + DCO -> O + CD2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1064) = small + (6.610e-11&
          *exp(-5.160e+04*invT))
    end if

    !H + CH -> C + H2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.500d0) then
      k(1065) = small + (1.240e-10&
          *(T32)**(+2.600e-01))
    end if

    !H + CD -> C + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.500d0) then
      k(1066) = small + (1.240e-10&
          *(T32)**(+2.600e-01))
    end if

    !D + CH -> C + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.500d0) then
      k(1067) = small + (1.240e-10&
          *(T32)**(+2.600e-01))
    end if

    !D + CD -> C + D2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.500d0) then
      k(1068) = small + (1.240e-10&
          *(T32)**(+2.600e-01))
    end if

    !H + CH2 -> CH + H2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1069) = small + (2.200e-10)
    end if

    !H + CD2 -> CH + D2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1070) = small + (1.100e-10)
    end if

    !H + CD2 -> CD + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1071) = small + (1.100e-10)
    end if

    !H + CHD -> CH + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1072) = small + (1.100e-10)
    end if

    !H + CHD -> CD + H2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1073) = small + (1.100e-10)
    end if

    !D + CH2 -> CH + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1074) = small + (1.100e-10)
    end if

    !D + CH2 -> CD + H2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1075) = small + (1.100e-10)
    end if

    !D + CD2 -> CD + D2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1076) = small + (2.200e-10)
    end if

    !D + CHD -> CH + D2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1077) = small + (1.100e-10)
    end if

    !D + CHD -> CD + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1078) = small + (1.100e-10)
    end if

    !H + OH -> O + H2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1079) = small + (6.860e-14&
          *(T32)**(+2.800e+00)*exp(-1.950e+03*invT))
    end if

    !H + OD -> O + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1080) = small + (6.860e-14&
          *(T32)**(+2.800e+00)*exp(-1.950e+03*invT))
    end if

    !D + OH -> O + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1081) = small + (6.860e-14&
          *(T32)**(+2.800e+00)*exp(-1.950e+03*invT))
    end if

    !D + OD -> O + D2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1082) = small + (6.860e-14&
          *(T32)**(+2.800e+00)*exp(-1.950e+03*invT))
    end if

    !H + H2O -> H2_PARA + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1083) = small + (6.820e-12&
          *(T32)**(+1.600e+00)*exp(-9.720e+03*invT))
    end if

    !H + D2O -> D2_PARA + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1084) = small + (3.410e-12&
          *(T32)**(+1.600e+00)*exp(-9.720e+03*invT))
    end if

    !H + D2O -> HD + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1085) = small + (3.410e-12&
          *(T32)**(+1.600e+00)*exp(-9.720e+03*invT))
    end if

    !H + HDO -> H2_PARA + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1086) = small + (3.410e-12&
          *(T32)**(+1.600e+00)*exp(-9.720e+03*invT))
    end if

    !H + HDO -> HD + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1087) = small + (3.410e-12&
          *(T32)**(+1.600e+00)*exp(-9.720e+03*invT))
    end if

    !D + H2O -> H2_PARA + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1088) = small + (3.410e-12&
          *(T32)**(+1.600e+00)*exp(-9.720e+03*invT))
    end if

    !D + H2O -> HD + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1089) = small + (3.410e-12&
          *(T32)**(+1.600e+00)*exp(-9.720e+03*invT))
    end if

    !D + D2O -> D2_PARA + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1090) = small + (6.820e-12&
          *(T32)**(+1.600e+00)*exp(-9.720e+03*invT))
    end if

    !D + HDO -> D2_PARA + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1091) = small + (3.410e-12&
          *(T32)**(+1.600e+00)*exp(-9.720e+03*invT))
    end if

    !D + HDO -> HD + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1092) = small + (3.410e-12&
          *(T32)**(+1.600e+00)*exp(-9.720e+03*invT))
    end if

    !H + HCN -> CN + H2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1093) = small + (6.190e-10&
          *exp(-1.250e+04*invT))
    end if

    !H + DCN -> CN + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1094) = small + (6.190e-10&
          *exp(-1.250e+04*invT))
    end if

    !D + HCN -> CN + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1095) = small + (6.190e-10&
          *exp(-1.250e+04*invT))
    end if

    !D + DCN -> CN + D2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1096) = small + (6.190e-10&
          *exp(-1.250e+04*invT))
    end if

    !H + NO -> O + NH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1097) = small + (9.300e-10&
          *(T32)**(-1.000e-01)*exp(-3.520e+04*invT))
    end if

    !D + NO -> O + ND
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1098) = small + (9.300e-10&
          *(T32)**(-1.000e-01)*exp(-3.520e+04*invT))
    end if

    !H + NO -> N + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1099) = small + (3.600e-10&
          *exp(-2.490e+04*invT))
    end if

    !D + NO -> N + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1100) = small + (3.600e-10&
          *exp(-2.490e+04*invT))
    end if

    !H + HNO -> O + NH2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1101) = small + (1.050e-09&
          *(T32)**(-3.000e-01)*exp(-1.470e+04*invT))
    end if

    !H + DNO -> O + NHD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1102) = small + (1.050e-09&
          *(T32)**(-3.000e-01)*exp(-1.470e+04*invT))
    end if

    !D + HNO -> O + NHD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1103) = small + (1.050e-09&
          *(T32)**(-3.000e-01)*exp(-1.470e+04*invT))
    end if

    !D + DNO -> O + ND2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1104) = small + (1.050e-09&
          *(T32)**(-3.000e-01)*exp(-1.470e+04*invT))
    end if

    !H + HNO -> NH + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1105) = small + (2.410e-09&
          *(T32)**(-5.000e-01)*exp(-9.010e+03*invT))
    end if

    !H + DNO -> NH + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1106) = small + (1.205e-09&
          *(T32)**(-5.000e-01)*exp(-9.010e+03*invT))
    end if

    !H + DNO -> ND + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1107) = small + (1.205e-09&
          *(T32)**(-5.000e-01)*exp(-9.010e+03*invT))
    end if

    !D + HNO -> NH + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1108) = small + (1.205e-09&
          *(T32)**(-5.000e-01)*exp(-9.010e+03*invT))
    end if

    !D + HNO -> ND + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1109) = small + (1.205e-09&
          *(T32)**(-5.000e-01)*exp(-9.010e+03*invT))
    end if

    !D + DNO -> ND + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1110) = small + (2.410e-09&
          *(T32)**(-5.000e-01)*exp(-9.010e+03*invT))
    end if

    !H + O2 -> O + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1111) = small + (2.940e-10&
          *exp(-8.380e+03*invT))
    end if

    !D + O2 -> O + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1112) = small + (2.940e-10&
          *exp(-8.380e+03*invT))
    end if

    !H + O2H -> H2_PARA + O2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1113) = small + (5.600e-12)
    end if

    !H + O2D -> HD + O2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1114) = small + (5.600e-12)
    end if

    !D + O2H -> HD + O2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1115) = small + (5.600e-12)
    end if

    !D + O2D -> D2_PARA + O2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1116) = small + (5.600e-12)
    end if

    !H + O2H -> OH + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1117) = small + (7.210e-11)
    end if

    !H + O2D -> OH + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1118) = small + (7.210e-11)
    end if

    !D + O2H -> OH + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1119) = small + (7.210e-11)
    end if

    !D + O2D -> OD + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1120) = small + (7.210e-11)
    end if

    !H + O2H -> O + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1121) = small + (2.420e-12)
    end if

    !H + O2D -> O + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1122) = small + (2.420e-12)
    end if

    !D + O2H -> O + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1123) = small + (2.420e-12)
    end if

    !D + O2D -> O + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1124) = small + (2.420e-12)
    end if

    !H + CO2 -> CO + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1125) = small + (2.510e-10&
          *exp(-1.330e+04*invT))
    end if

    !D + CO2 -> CO + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1126) = small + (2.510e-10&
          *exp(-1.330e+04*invT))
    end if

    !H + N2O -> N2 + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1127) = small + (9.220e-14&
          *exp(-2.990e+03*invT))
    end if

    !D + N2O -> N2 + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1128) = small + (9.220e-14&
          *exp(-2.990e+03*invT))
    end if

    !H + NO2 -> NO + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1129) = small + (4.000e-10&
          *exp(-3.400e+02*invT))
    end if

    !D + NO2 -> NO + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1130) = small + (4.000e-10&
          *exp(-3.400e+02*invT))
    end if

    !C + H2_PARA -> H + CH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1131) = small + (6.640e-10&
          *exp(-1.170e+04*invT))
    end if

    !C + H2_ORTHO -> H + CH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1132) = small + (6.640e-10&
          *exp(-1.170e+04*invT))
    end if

    !C + D2_PARA -> D + CD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1133) = small + (6.640e-10&
          *exp(-1.170e+04*invT))
    end if

    !C + D2_ORTHO -> D + CD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1134) = small + (6.640e-10&
          *exp(-1.170e+04*invT))
    end if

    !C + HD -> H + CD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1135) = small + (3.320e-10&
          *exp(-1.170e+04*invT))
    end if

    !C + HD -> D + CH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1136) = small + (3.320e-10&
          *exp(-1.170e+04*invT))
    end if

    !CH + H2_PARA -> H + CH2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1137) = small + (3.750e-10&
          *exp(-1.660e+03*invT))
    end if

    !CH + H2_ORTHO -> H + CH2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1138) = small + (3.750e-10&
          *exp(-1.660e+03*invT))
    end if

    !CH + D2_PARA -> H + CD2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1139) = small + (1.875e-10&
          *exp(-1.660e+03*invT))
    end if

    !CH + D2_ORTHO -> H + CD2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1140) = small + (1.875e-10&
          *exp(-1.660e+03*invT))
    end if

    !CH + D2_PARA -> D + CHD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1141) = small + (1.875e-10&
          *exp(-1.660e+03*invT))
    end if

    !CH + D2_ORTHO -> D + CHD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1142) = small + (1.875e-10&
          *exp(-1.660e+03*invT))
    end if

    !CH + HD -> H + CHD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1143) = small + (1.875e-10&
          *exp(-1.660e+03*invT))
    end if

    !CH + HD -> D + CH2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1144) = small + (1.875e-10&
          *exp(-1.660e+03*invT))
    end if

    !CD + H2_PARA -> H + CHD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1145) = small + (1.875e-10&
          *exp(-1.660e+03*invT))
    end if

    !CD + H2_ORTHO -> H + CHD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1146) = small + (1.875e-10&
          *exp(-1.660e+03*invT))
    end if

    !CD + H2_PARA -> D + CH2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1147) = small + (1.875e-10&
          *exp(-1.660e+03*invT))
    end if

    !CD + H2_ORTHO -> D + CH2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1148) = small + (1.875e-10&
          *exp(-1.660e+03*invT))
    end if

    !CD + D2_PARA -> D + CD2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1149) = small + (3.750e-10&
          *exp(-1.660e+03*invT))
    end if

    !CD + D2_ORTHO -> D + CD2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1150) = small + (3.750e-10&
          *exp(-1.660e+03*invT))
    end if

    !CD + HD -> H + CD2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1151) = small + (1.875e-10&
          *exp(-1.660e+03*invT))
    end if

    !CD + HD -> D + CHD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1152) = small + (1.875e-10&
          *exp(-1.660e+03*invT))
    end if

    !O + H2_PARA -> H + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1153) = small + (3.440e-13&
          *(T32)**(+2.670e+00)*exp(-3.160e+03*invT))
    end if

    !O + H2_ORTHO -> H + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1154) = small + (3.440e-13&
          *(T32)**(+2.670e+00)*exp(-3.160e+03*invT))
    end if

    !O + D2_PARA -> D + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1155) = small + (3.440e-13&
          *(T32)**(+2.670e+00)*exp(-3.160e+03*invT))
    end if

    !O + D2_ORTHO -> D + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1156) = small + (3.440e-13&
          *(T32)**(+2.670e+00)*exp(-3.160e+03*invT))
    end if

    !O + HD -> H + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1157) = small + (1.720e-13&
          *(T32)**(+2.670e+00)*exp(-3.160e+03*invT))
    end if

    !O + HD -> D + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1158) = small + (1.720e-13&
          *(T32)**(+2.670e+00)*exp(-3.160e+03*invT))
    end if

    !H2_PARA + OH -> H + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1159) = small + (8.400e-13&
          *exp(-1.040e+03*invT))
    end if

    !H2_ORTHO + OH -> H + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1160) = small + (8.400e-13&
          *exp(-1.040e+03*invT))
    end if

    !H2_PARA + OD -> H + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1161) = small + (4.200e-13&
          *exp(-1.040e+03*invT))
    end if

    !H2_ORTHO + OD -> H + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1162) = small + (4.200e-13&
          *exp(-1.040e+03*invT))
    end if

    !H2_PARA + OD -> D + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1163) = small + (4.200e-13&
          *exp(-1.040e+03*invT))
    end if

    !H2_ORTHO + OD -> D + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1164) = small + (4.200e-13&
          *exp(-1.040e+03*invT))
    end if

    !D2_PARA + OH -> H + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1165) = small + (4.200e-13&
          *exp(-1.040e+03*invT))
    end if

    !D2_ORTHO + OH -> H + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1166) = small + (4.200e-13&
          *exp(-1.040e+03*invT))
    end if

    !D2_PARA + OH -> D + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1167) = small + (4.200e-13&
          *exp(-1.040e+03*invT))
    end if

    !D2_ORTHO + OH -> D + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1168) = small + (4.200e-13&
          *exp(-1.040e+03*invT))
    end if

    !D2_PARA + OD -> D + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1169) = small + (8.400e-13&
          *exp(-1.040e+03*invT))
    end if

    !D2_ORTHO + OD -> D + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1170) = small + (8.400e-13&
          *exp(-1.040e+03*invT))
    end if

    !HD + OH -> H + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1171) = small + (4.200e-13&
          *exp(-1.040e+03*invT))
    end if

    !HD + OH -> D + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1172) = small + (4.200e-13&
          *exp(-1.040e+03*invT))
    end if

    !HD + OD -> H + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1173) = small + (4.200e-13&
          *exp(-1.040e+03*invT))
    end if

    !HD + OD -> D + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1174) = small + (4.200e-13&
          *exp(-1.040e+03*invT))
    end if

    !N + H2_PARA -> H + NH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1175) = small + (4.650e-10&
          *exp(-1.660e+04*invT))
    end if

    !N + H2_ORTHO -> H + NH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1176) = small + (4.650e-10&
          *exp(-1.660e+04*invT))
    end if

    !N + D2_PARA -> D + ND
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1177) = small + (4.650e-10&
          *exp(-1.660e+04*invT))
    end if

    !N + D2_ORTHO -> D + ND
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1178) = small + (4.650e-10&
          *exp(-1.660e+04*invT))
    end if

    !N + HD -> H + ND
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1179) = small + (2.325e-10&
          *exp(-1.660e+04*invT))
    end if

    !N + HD -> D + NH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1180) = small + (2.325e-10&
          *exp(-1.660e+04*invT))
    end if

    !H2_PARA + NH -> H + NH2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1181) = small + (5.960e-11&
          *exp(-7.780e+03*invT))
    end if

    !H2_ORTHO + NH -> H + NH2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1182) = small + (5.960e-11&
          *exp(-7.780e+03*invT))
    end if

    !H2_PARA + ND -> H + NHD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1183) = small + (2.980e-11&
          *exp(-7.780e+03*invT))
    end if

    !H2_ORTHO + ND -> H + NHD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1184) = small + (2.980e-11&
          *exp(-7.780e+03*invT))
    end if

    !H2_PARA + ND -> D + NH2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1185) = small + (2.980e-11&
          *exp(-7.780e+03*invT))
    end if

    !H2_ORTHO + ND -> D + NH2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1186) = small + (2.980e-11&
          *exp(-7.780e+03*invT))
    end if

    !D2_PARA + NH -> H + ND2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1187) = small + (2.980e-11&
          *exp(-7.780e+03*invT))
    end if

    !D2_ORTHO + NH -> H + ND2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1188) = small + (2.980e-11&
          *exp(-7.780e+03*invT))
    end if

    !D2_PARA + NH -> D + NHD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1189) = small + (2.980e-11&
          *exp(-7.780e+03*invT))
    end if

    !D2_ORTHO + NH -> D + NHD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1190) = small + (2.980e-11&
          *exp(-7.780e+03*invT))
    end if

    !D2_PARA + ND -> D + ND2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1191) = small + (5.960e-11&
          *exp(-7.780e+03*invT))
    end if

    !D2_ORTHO + ND -> D + ND2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1192) = small + (5.960e-11&
          *exp(-7.780e+03*invT))
    end if

    !HD + NH -> H + NHD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1193) = small + (2.980e-11&
          *exp(-7.780e+03*invT))
    end if

    !HD + NH -> D + NH2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1194) = small + (2.980e-11&
          *exp(-7.780e+03*invT))
    end if

    !HD + ND -> H + ND2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1195) = small + (2.980e-11&
          *exp(-7.780e+03*invT))
    end if

    !HD + ND -> D + NHD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1196) = small + (2.980e-11&
          *exp(-7.780e+03*invT))
    end if

    !N + C2 -> C + CN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1197) = small + (5.000e-11)
    end if

    !N + CH -> H + CN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1198) = small + (1.660e-10&
          *(T32)**(-9.000e-02))
    end if

    !N + CD -> D + CN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1199) = small + (1.660e-10&
          *(T32)**(-9.000e-02))
    end if

    !N + CN -> C + N2
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1200) = small + (1.000e-10)
    end if

    !N + NH -> H + N2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1201) = small + (5.000e-11)
    end if

    !N + ND -> D + N2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1202) = small + (5.000e-11)
    end if

    !N + NO -> O + N2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1203) = small + (3.000e-11&
          *(T32)**(-6.000e-01))
    end if

    !N + OH -> H + NO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1204) = small + (7.500e-11&
          *(T32)**(-1.800e-01))
    end if

    !N + OD -> D + NO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1205) = small + (7.500e-11&
          *(T32)**(-1.800e-01))
    end if

    !N + O2 -> O + NO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1206) = small + (1.500e-11&
          *exp(-3.680e+03*invT))
    end if

    !N + CH2 -> H + HCN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1207) = small + (3.950e-11&
          *(T32)**(+1.670e-01))
    end if

    !N + CD2 -> D + DCN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1208) = small + (3.950e-11&
          *(T32)**(+1.670e-01))
    end if

    !N + CHD -> H + DCN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1209) = small + (1.975e-11&
          *(T32)**(+1.670e-01))
    end if

    !N + CHD -> D + HCN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1210) = small + (1.975e-11&
          *(T32)**(+1.670e-01))
    end if

    !N + CH2 -> H + HNC
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1211) = small + (3.950e-11&
          *(T32)**(+1.670e-01))
    end if

    !N + CD2 -> D + DNC
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1212) = small + (3.950e-11&
          *(T32)**(+1.670e-01))
    end if

    !N + CHD -> H + DNC
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1213) = small + (1.975e-11&
          *(T32)**(+1.670e-01))
    end if

    !N + CHD -> D + HNC
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1214) = small + (1.975e-11&
          *(T32)**(+1.670e-01))
    end if

    !N + HCO -> H + OCN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1215) = small + (1.000e-10)
    end if

    !N + DCO -> D + OCN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1216) = small + (1.000e-10)
    end if

    !N + NO2 -> N2 + O2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1217) = small + (1.000e-12)
    end if

    !N + NO2 -> NO + NO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1218) = small + (1.000e-12)
    end if

    !N + NO2 -> O + N2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1219) = small + (2.100e-11)
    end if

    !N + O2H -> NH + O2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1220) = small + (1.700e-13)
    end if

    !N + O2D -> ND + O2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1221) = small + (1.700e-13)
    end if

    !N + C2H -> H + C2N
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1222) = small + (1.000e-10)
    end if

    !N + C2D -> D + C2N
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1223) = small + (1.000e-10)
    end if

    !N + C2N -> CN + CN
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1224) = small + (1.000e-10)
    end if

    !N + CCO -> CN + CO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1225) = small + (5.500e-10)
    end if

    !N + C3 -> C2 + CN
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1226) = small + (1.000e-13)
    end if

    !O + C2 -> C + CO
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1227) = small + (2.000e-10&
          *(T32)**(-1.200e-01))
    end if

    !O + CH -> H + CO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1228) = small + (6.600e-11)
    end if

    !O + CD -> D + CO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1229) = small + (6.600e-11)
    end if

    !O + CN -> N + CO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1230) = small + (4.000e-11)
    end if

    !O + NH -> H + NO
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1231) = small + (6.600e-11)
    end if

    !O + ND -> D + NO
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1232) = small + (6.600e-11)
    end if

    !O + OH -> H + O2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1233) = small + (3.500e-11)
    end if

    !O + OD -> D + O2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1234) = small + (3.500e-11)
    end if

    !O + C2H -> CH + CO
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1235) = small + (1.000e-10)
    end if

    !O + C2D -> CD + CO
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1236) = small + (1.000e-10)
    end if

    !O + C2N -> CN + CO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1237) = small + (6.000e-12)
    end if

    !O + CCO -> CO + CO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1238) = small + (8.600e-11)
    end if

    !C2 + H2O+ -> OH + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1239) = small + (4.700e-10)
    end if

    !C2 + D2O+ -> OD + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1240) = small + (4.700e-10)
    end if

    !C2 + HDO+ -> OH + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1241) = small + (2.350e-10)
    end if

    !C2 + HDO+ -> OD + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1242) = small + (2.350e-10)
    end if

    !CH + H2O+ -> OH + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1243) = small + (3.400e-10)
    end if

    !CH + D2O+ -> OH + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1244) = small + (3.400e-10)
    end if

    !CH + D2O+ -> OD + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1245) = small + (3.400e-10)
    end if

    !CH + HDO+ -> OH + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1246) = small + (1.700e-10)
    end if

    !CH + HDO+ -> OD + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1247) = small + (1.700e-10)
    end if

    !CD + H2O+ -> OH + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1248) = small + (3.400e-10)
    end if

    !CD + H2O+ -> OD + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1249) = small + (3.400e-10)
    end if

    !CD + D2O+ -> OD + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1250) = small + (3.400e-10)
    end if

    !CD + HDO+ -> OH + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1251) = small + (1.700e-10)
    end if

    !CD + HDO+ -> OD + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1252) = small + (1.700e-10)
    end if

    !CO + H2O+ -> OH + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1253) = small + (5.000e-10)
    end if

    !CO + D2O+ -> OD + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1254) = small + (5.000e-10)
    end if

    !CO + HDO+ -> OH + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1255) = small + (2.500e-10)
    end if

    !CO + HDO+ -> OD + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1256) = small + (2.500e-10)
    end if

    !C + H3+_ORTHO -> H2_ORTHO + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1257) = small + (2.000e-09)
    end if

    !C + H3+_PARA -> H2_PARA + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1258) = small + (1.000e-09)
    end if

    !C + H3+_PARA -> H2_ORTHO + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1259) = small + (1.000e-09)
    end if

    !C + D3+_ORTHO -> D2_ORTHO + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1260) = small + (2.000e-09)
    end if

    !C + D3+_META -> D2_PARA + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1261) = small + (2.000e-09)
    end if

    !C + H2D+_ORTHO -> H2_ORTHO + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1262) = small + (6.667e-10)
    end if

    !C + H2D+_PARA -> H2_PARA + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1263) = small + (3.334e-10)
    end if

    !C + H2D+_PARA -> H2_ORTHO + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1264) = small + (3.334e-10)
    end if

    !C + H2D+_ORTHO -> HD + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1265) = small + (1.333e-09)
    end if

    !C + H2D+_PARA -> HD + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1266) = small + (1.333e-09)
    end if

    !C + D2H+_ORTHO -> D2_ORTHO + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1267) = small + (6.667e-10)
    end if

    !C + D2H+_PARA -> D2_PARA + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1268) = small + (3.334e-10)
    end if

    !C + D2H+_PARA -> D2_ORTHO + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1269) = small + (3.334e-10)
    end if

    !C + D2H+_ORTHO -> HD + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1270) = small + (1.333e-09)
    end if

    !C + D2H+_PARA -> HD + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1271) = small + (1.333e-09)
    end if

    !N + H3+_ORTHO -> H + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1272) = small + (1.000e-17)
    end if

    !N + H3+_PARA -> H + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1273) = small + (1.000e-17)
    end if

    !N + D3+_ORTHO -> D + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1274) = small + (1.000e-17)
    end if

    !N + D3+_META -> D + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1275) = small + (1.000e-17)
    end if

    !N + H2D+_ORTHO -> H + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1276) = small + (1.000e-17)
    end if

    !N + H2D+_PARA -> H + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1277) = small + (1.000e-17)
    end if

    !N + H2D+_ORTHO -> D + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1278) = small + (1.000e-17)
    end if

    !N + H2D+_PARA -> D + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1279) = small + (1.000e-17)
    end if

    !N + D2H+_ORTHO -> H + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1280) = small + (1.000e-17)
    end if

    !N + D2H+_PARA -> H + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1281) = small + (1.000e-17)
    end if

    !N + D2H+_ORTHO -> D + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1282) = small + (1.000e-17)
    end if

    !N + D2H+_PARA -> D + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1283) = small + (1.000e-17)
    end if

    !O + H3+_ORTHO -> H2_ORTHO + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1284) = small + (8.540e-10&
          *(T32)**(-2.100e-01))
    end if

    !O + H3+_PARA -> H2_PARA + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1285) = small + (4.270e-10&
          *(T32)**(-2.100e-01))
    end if

    !O + H3+_PARA -> H2_ORTHO + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1286) = small + (4.270e-10&
          *(T32)**(-2.100e-01))
    end if

    !O + D3+_ORTHO -> D2_ORTHO + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1287) = small + (8.540e-10&
          *(T32)**(-2.100e-01))
    end if

    !O + D3+_META -> D2_PARA + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1288) = small + (8.540e-10&
          *(T32)**(-2.100e-01))
    end if

    !O + H2D+_ORTHO -> H2_ORTHO + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1289) = small + (2.847e-10&
          *(T32)**(-2.100e-01))
    end if

    !O + H2D+_PARA -> H2_PARA + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1290) = small + (1.424e-10&
          *(T32)**(-2.100e-01))
    end if

    !O + H2D+_PARA -> H2_ORTHO + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1291) = small + (1.424e-10&
          *(T32)**(-2.100e-01))
    end if

    !O + H2D+_ORTHO -> HD + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1292) = small + (5.693e-10&
          *(T32)**(-2.100e-01))
    end if

    !O + H2D+_PARA -> HD + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1293) = small + (5.693e-10&
          *(T32)**(-2.100e-01))
    end if

    !O + D2H+_ORTHO -> D2_ORTHO + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1294) = small + (2.847e-10&
          *(T32)**(-2.100e-01))
    end if

    !O + D2H+_PARA -> D2_PARA + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1295) = small + (1.424e-10&
          *(T32)**(-2.100e-01))
    end if

    !O + D2H+_PARA -> D2_ORTHO + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1296) = small + (1.424e-10&
          *(T32)**(-2.100e-01))
    end if

    !O + D2H+_ORTHO -> HD + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1297) = small + (5.693e-10&
          *(T32)**(-2.100e-01))
    end if

    !O + D2H+_PARA -> HD + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1298) = small + (5.693e-10&
          *(T32)**(-2.100e-01))
    end if

    !O + H3+_ORTHO -> H + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1299) = small + (3.660e-10&
          *(T32)**(-2.100e-01))
    end if

    !O + H3+_PARA -> H + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1300) = small + (3.660e-10&
          *(T32)**(-2.100e-01))
    end if

    !O + D3+_ORTHO -> D + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1301) = small + (3.660e-10&
          *(T32)**(-2.100e-01))
    end if

    !O + D3+_META -> D + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1302) = small + (3.660e-10&
          *(T32)**(-2.100e-01))
    end if

    !O + H2D+_ORTHO -> H + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1303) = small + (3.660e-10&
          *(T32)**(-2.100e-01))
    end if

    !O + H2D+_PARA -> H + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1304) = small + (3.660e-10&
          *(T32)**(-2.100e-01))
    end if

    !O + H2D+_ORTHO -> D + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1305) = small + (3.660e-10&
          *(T32)**(-2.100e-01))
    end if

    !O + H2D+_PARA -> D + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1306) = small + (3.660e-10&
          *(T32)**(-2.100e-01))
    end if

    !O + D2H+_ORTHO -> H + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1307) = small + (3.660e-10&
          *(T32)**(-2.100e-01))
    end if

    !O + D2H+_PARA -> H + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1308) = small + (3.660e-10&
          *(T32)**(-2.100e-01))
    end if

    !O + D2H+_ORTHO -> D + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1309) = small + (3.660e-10&
          *(T32)**(-2.100e-01))
    end if

    !O + D2H+_PARA -> D + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(1310) = small + (3.660e-10&
          *(T32)**(-2.100e-01))
    end if

    !C2 + H3+_ORTHO -> H2_ORTHO + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1311) = small + (1.800e-09)
    end if

    !C2 + H3+_PARA -> H2_PARA + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1312) = small + (9.000e-10)
    end if

    !C2 + H3+_PARA -> H2_ORTHO + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1313) = small + (9.000e-10)
    end if

    !C2 + D3+_ORTHO -> D2_ORTHO + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1314) = small + (1.800e-09)
    end if

    !C2 + D3+_META -> D2_PARA + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1315) = small + (1.800e-09)
    end if

    !C2 + H2D+_ORTHO -> H2_ORTHO + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1316) = small + (6.000e-10)
    end if

    !C2 + H2D+_PARA -> H2_PARA + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1317) = small + (3.000e-10)
    end if

    !C2 + H2D+_PARA -> H2_ORTHO + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1318) = small + (3.000e-10)
    end if

    !C2 + H2D+_ORTHO -> HD + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1319) = small + (1.200e-09)
    end if

    !C2 + H2D+_PARA -> HD + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1320) = small + (1.200e-09)
    end if

    !C2 + D2H+_ORTHO -> D2_ORTHO + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1321) = small + (6.000e-10)
    end if

    !C2 + D2H+_PARA -> D2_PARA + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1322) = small + (3.000e-10)
    end if

    !C2 + D2H+_PARA -> D2_ORTHO + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1323) = small + (3.000e-10)
    end if

    !C2 + D2H+_ORTHO -> HD + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1324) = small + (1.200e-09)
    end if

    !C2 + D2H+_PARA -> HD + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1325) = small + (1.200e-09)
    end if

    !CH + H3+_ORTHO -> H2_ORTHO + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1326) = small + (8.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !CH + H3+_PARA -> H2_PARA + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1327) = small + (4.250e-09&
          *(T32)**(-5.000e-01))
    end if

    !CH + H3+_PARA -> H2_ORTHO + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1328) = small + (4.250e-09&
          *(T32)**(-5.000e-01))
    end if

    !CH + D3+_ORTHO -> HD + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1329) = small + (8.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !CH + D3+_META -> HD + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1330) = small + (8.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !CH + H2D+_ORTHO -> H2_ORTHO + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1331) = small + (2.833e-09&
          *(T32)**(-5.000e-01))
    end if

    !CH + H2D+_PARA -> H2_PARA + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1332) = small + (1.417e-09&
          *(T32)**(-5.000e-01))
    end if

    !CH + H2D+_PARA -> H2_ORTHO + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1333) = small + (1.417e-09&
          *(T32)**(-5.000e-01))
    end if

    !CH + H2D+_ORTHO -> HD + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1334) = small + (5.667e-09&
          *(T32)**(-5.000e-01))
    end if

    !CH + H2D+_PARA -> HD + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1335) = small + (5.667e-09&
          *(T32)**(-5.000e-01))
    end if

    !CH + D2H+_ORTHO -> D2_ORTHO + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1336) = small + (2.833e-09&
          *(T32)**(-5.000e-01))
    end if

    !CH + D2H+_PARA -> D2_PARA + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1337) = small + (1.417e-09&
          *(T32)**(-5.000e-01))
    end if

    !CH + D2H+_PARA -> D2_ORTHO + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1338) = small + (1.417e-09&
          *(T32)**(-5.000e-01))
    end if

    !CH + D2H+_ORTHO -> HD + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1339) = small + (5.667e-09&
          *(T32)**(-5.000e-01))
    end if

    !CH + D2H+_PARA -> HD + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1340) = small + (5.667e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD + H3+_ORTHO -> H2_ORTHO + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1341) = small + (8.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD + H3+_PARA -> H2_PARA + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1342) = small + (4.250e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD + H3+_PARA -> H2_ORTHO + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1343) = small + (4.250e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD + H3+_ORTHO -> HD + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1344) = small + (8.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD + H3+_PARA -> HD + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1345) = small + (8.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD + D3+_ORTHO -> D2_ORTHO + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1346) = small + (8.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD + D3+_META -> D2_PARA + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1347) = small + (8.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD + H2D+_ORTHO -> H2_ORTHO + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1348) = small + (2.833e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD + H2D+_PARA -> H2_PARA + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1349) = small + (1.417e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD + H2D+_PARA -> H2_ORTHO + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1350) = small + (1.417e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD + H2D+_ORTHO -> HD + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1351) = small + (5.667e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD + H2D+_PARA -> HD + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1352) = small + (5.667e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD + D2H+_ORTHO -> D2_ORTHO + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1353) = small + (2.833e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD + D2H+_PARA -> D2_PARA + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1354) = small + (1.417e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD + D2H+_PARA -> D2_ORTHO + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1355) = small + (1.417e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD + D2H+_ORTHO -> HD + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1356) = small + (5.667e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD + D2H+_PARA -> HD + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1357) = small + (5.667e-09&
          *(T32)**(-5.000e-01))
    end if

    !CN + H3+_ORTHO -> H2_ORTHO + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1358) = small + (8.100e-09&
          *(T32)**(-5.000e-01))
    end if

    !CN + H3+_PARA -> H2_PARA + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1359) = small + (4.050e-09&
          *(T32)**(-5.000e-01))
    end if

    !CN + H3+_PARA -> H2_ORTHO + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1360) = small + (4.050e-09&
          *(T32)**(-5.000e-01))
    end if

    !CN + D3+_ORTHO -> D2_ORTHO + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1361) = small + (8.100e-09&
          *(T32)**(-5.000e-01))
    end if

    !CN + D3+_META -> D2_PARA + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1362) = small + (8.100e-09&
          *(T32)**(-5.000e-01))
    end if

    !CN + H2D+_ORTHO -> H2_ORTHO + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1363) = small + (2.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !CN + H2D+_PARA -> H2_PARA + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1364) = small + (1.350e-09&
          *(T32)**(-5.000e-01))
    end if

    !CN + H2D+_PARA -> H2_ORTHO + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1365) = small + (1.350e-09&
          *(T32)**(-5.000e-01))
    end if

    !CN + H2D+_ORTHO -> HD + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1366) = small + (5.400e-09&
          *(T32)**(-5.000e-01))
    end if

    !CN + H2D+_PARA -> HD + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1367) = small + (5.400e-09&
          *(T32)**(-5.000e-01))
    end if

    !CN + D2H+_ORTHO -> D2_ORTHO + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1368) = small + (2.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !CN + D2H+_PARA -> D2_PARA + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1369) = small + (1.350e-09&
          *(T32)**(-5.000e-01))
    end if

    !CN + D2H+_PARA -> D2_ORTHO + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1370) = small + (1.350e-09&
          *(T32)**(-5.000e-01))
    end if

    !CN + D2H+_ORTHO -> HD + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1371) = small + (5.400e-09&
          *(T32)**(-5.000e-01))
    end if

    !CN + D2H+_PARA -> HD + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1372) = small + (5.400e-09&
          *(T32)**(-5.000e-01))
    end if

    !CO + H3+_ORTHO -> H2_ORTHO + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1373) = small + (1.610e-09)
    end if

    !CO + H3+_PARA -> H2_PARA + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1374) = small + (8.050e-10)
    end if

    !CO + H3+_PARA -> H2_ORTHO + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1375) = small + (8.050e-10)
    end if

    !CO + D3+_ORTHO -> D2_ORTHO + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1376) = small + (1.610e-09)
    end if

    !CO + D3+_META -> D2_PARA + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1377) = small + (1.610e-09)
    end if

    !CO + H2D+_ORTHO -> H2_ORTHO + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1378) = small + (5.367e-10)
    end if

    !CO + H2D+_PARA -> H2_PARA + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1379) = small + (2.684e-10)
    end if

    !CO + H2D+_PARA -> H2_ORTHO + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1380) = small + (2.684e-10)
    end if

    !CO + H2D+_ORTHO -> HD + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1381) = small + (1.073e-09)
    end if

    !CO + H2D+_PARA -> HD + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1382) = small + (1.073e-09)
    end if

    !CO + D2H+_ORTHO -> D2_ORTHO + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1383) = small + (5.367e-10)
    end if

    !CO + D2H+_PARA -> D2_PARA + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1384) = small + (2.684e-10)
    end if

    !CO + D2H+_PARA -> D2_ORTHO + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1385) = small + (2.684e-10)
    end if

    !CO + D2H+_ORTHO -> HD + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1386) = small + (1.073e-09)
    end if

    !CO + D2H+_PARA -> HD + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1387) = small + (1.073e-09)
    end if

    !CO + H3+_ORTHO -> H2_ORTHO + HOC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1388) = small + (9.440e-11)
    end if

    !CO + H3+_PARA -> H2_PARA + HOC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1389) = small + (4.720e-11)
    end if

    !CO + H3+_PARA -> H2_ORTHO + HOC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1390) = small + (4.720e-11)
    end if

    !CO + D3+_ORTHO -> D2_ORTHO + DOC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1391) = small + (9.440e-11)
    end if

    !CO + D3+_META -> D2_PARA + DOC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1392) = small + (9.440e-11)
    end if

    !CO + H2D+_ORTHO -> H2_ORTHO + DOC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1393) = small + (3.147e-11)
    end if

    !CO + H2D+_PARA -> H2_PARA + DOC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1394) = small + (1.574e-11)
    end if

    !CO + H2D+_PARA -> H2_ORTHO + DOC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1395) = small + (1.574e-11)
    end if

    !CO + H2D+_ORTHO -> HD + HOC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1396) = small + (6.294e-11)
    end if

    !CO + H2D+_PARA -> HD + HOC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1397) = small + (6.294e-11)
    end if

    !CO + D2H+_ORTHO -> D2_ORTHO + HOC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1398) = small + (3.147e-11)
    end if

    !CO + D2H+_PARA -> D2_PARA + HOC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1399) = small + (1.574e-11)
    end if

    !CO + D2H+_PARA -> D2_ORTHO + HOC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1400) = small + (1.574e-11)
    end if

    !CO + D2H+_ORTHO -> HD + DOC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1401) = small + (6.294e-11)
    end if

    !CO + D2H+_PARA -> HD + DOC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1402) = small + (6.294e-11)
    end if

    !N2 + H3+_ORTHO -> H2_ORTHO + N2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1403) = small + (1.700e-09)
    end if

    !N2 + H3+_PARA -> H2_PARA + N2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1404) = small + (8.500e-10)
    end if

    !N2 + H3+_PARA -> H2_ORTHO + N2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1405) = small + (8.500e-10)
    end if

    !N2 + D3+_ORTHO -> D2_ORTHO + N2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1406) = small + (1.700e-09)
    end if

    !N2 + D3+_META -> D2_PARA + N2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1407) = small + (1.700e-09)
    end if

    !N2 + H2D+_ORTHO -> H2_ORTHO + N2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1408) = small + (5.667e-10)
    end if

    !N2 + H2D+_PARA -> H2_PARA + N2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1409) = small + (2.834e-10)
    end if

    !N2 + H2D+_PARA -> H2_ORTHO + N2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1410) = small + (2.834e-10)
    end if

    !N2 + H2D+_ORTHO -> HD + N2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1411) = small + (1.133e-09)
    end if

    !N2 + H2D+_PARA -> HD + N2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1412) = small + (1.133e-09)
    end if

    !N2 + D2H+_ORTHO -> D2_ORTHO + N2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1413) = small + (5.667e-10)
    end if

    !N2 + D2H+_PARA -> D2_PARA + N2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1414) = small + (2.834e-10)
    end if

    !N2 + D2H+_PARA -> D2_ORTHO + N2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1415) = small + (2.834e-10)
    end if

    !N2 + D2H+_ORTHO -> HD + N2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1416) = small + (1.133e-09)
    end if

    !N2 + D2H+_PARA -> HD + N2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1417) = small + (1.133e-09)
    end if

    !NH + H3+_ORTHO -> H2_ORTHO + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1418) = small + (7.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !NH + H3+_PARA -> H2_PARA + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1419) = small + (3.750e-09&
          *(T32)**(-5.000e-01))
    end if

    !NH + H3+_PARA -> H2_ORTHO + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1420) = small + (3.750e-09&
          *(T32)**(-5.000e-01))
    end if

    !NH + D3+_ORTHO -> HD + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1421) = small + (7.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !NH + D3+_META -> HD + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1422) = small + (7.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !NH + H2D+_ORTHO -> H2_ORTHO + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1423) = small + (2.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !NH + H2D+_PARA -> H2_PARA + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1424) = small + (1.250e-09&
          *(T32)**(-5.000e-01))
    end if

    !NH + H2D+_PARA -> H2_ORTHO + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1425) = small + (1.250e-09&
          *(T32)**(-5.000e-01))
    end if

    !NH + H2D+_ORTHO -> HD + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1426) = small + (5.000e-09&
          *(T32)**(-5.000e-01))
    end if

    !NH + H2D+_PARA -> HD + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1427) = small + (5.000e-09&
          *(T32)**(-5.000e-01))
    end if

    !NH + D2H+_ORTHO -> D2_ORTHO + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1428) = small + (2.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !NH + D2H+_PARA -> D2_PARA + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1429) = small + (1.250e-09&
          *(T32)**(-5.000e-01))
    end if

    !NH + D2H+_PARA -> D2_ORTHO + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1430) = small + (1.250e-09&
          *(T32)**(-5.000e-01))
    end if

    !NH + D2H+_ORTHO -> HD + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1431) = small + (5.000e-09&
          *(T32)**(-5.000e-01))
    end if

    !NH + D2H+_PARA -> HD + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1432) = small + (5.000e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND + H3+_ORTHO -> H2_ORTHO + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1433) = small + (7.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND + H3+_PARA -> H2_PARA + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1434) = small + (3.750e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND + H3+_PARA -> H2_ORTHO + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1435) = small + (3.750e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND + H3+_ORTHO -> HD + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1436) = small + (7.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND + H3+_PARA -> HD + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1437) = small + (7.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND + D3+_ORTHO -> D2_ORTHO + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1438) = small + (7.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND + D3+_META -> D2_PARA + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1439) = small + (7.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND + H2D+_ORTHO -> H2_ORTHO + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1440) = small + (2.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND + H2D+_PARA -> H2_PARA + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1441) = small + (1.250e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND + H2D+_PARA -> H2_ORTHO + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1442) = small + (1.250e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND + H2D+_ORTHO -> HD + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1443) = small + (5.000e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND + H2D+_PARA -> HD + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1444) = small + (5.000e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND + D2H+_ORTHO -> D2_ORTHO + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1445) = small + (2.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND + D2H+_PARA -> D2_PARA + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1446) = small + (1.250e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND + D2H+_PARA -> D2_ORTHO + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1447) = small + (1.250e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND + D2H+_ORTHO -> HD + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1448) = small + (5.000e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND + D2H+_PARA -> HD + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1449) = small + (5.000e-09&
          *(T32)**(-5.000e-01))
    end if

    !NO + H3+_ORTHO -> H2_ORTHO + HNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1450) = small + (8.500e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO + H3+_PARA -> H2_PARA + HNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1451) = small + (4.250e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO + H3+_PARA -> H2_ORTHO + HNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1452) = small + (4.250e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO + D3+_ORTHO -> D2_ORTHO + DNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1453) = small + (8.500e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO + D3+_META -> D2_PARA + DNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1454) = small + (8.500e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO + H2D+_ORTHO -> H2_ORTHO + DNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1455) = small + (2.833e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO + H2D+_PARA -> H2_PARA + DNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1456) = small + (1.417e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO + H2D+_PARA -> H2_ORTHO + DNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1457) = small + (1.417e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO + H2D+_ORTHO -> HD + HNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1458) = small + (5.667e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO + H2D+_PARA -> HD + HNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1459) = small + (5.667e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO + D2H+_ORTHO -> D2_ORTHO + HNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1460) = small + (2.833e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO + D2H+_PARA -> D2_PARA + HNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1461) = small + (1.417e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO + D2H+_PARA -> D2_ORTHO + HNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1462) = small + (1.417e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO + D2H+_ORTHO -> HD + DNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1463) = small + (5.667e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO + D2H+_PARA -> HD + DNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1464) = small + (5.667e-10&
          *(T32)**(-5.000e-01))
    end if

    !O2 + H3+_ORTHO -> H2_ORTHO + O2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1465) = small + (6.400e-10)
    end if

    !O2 + H3+_PARA -> H2_PARA + O2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1466) = small + (3.200e-10)
    end if

    !O2 + H3+_PARA -> H2_ORTHO + O2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1467) = small + (3.200e-10)
    end if

    !O2 + D3+_ORTHO -> D2_ORTHO + O2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1468) = small + (6.400e-10)
    end if

    !O2 + D3+_META -> D2_PARA + O2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1469) = small + (6.400e-10)
    end if

    !O2 + H2D+_ORTHO -> H2_ORTHO + O2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1470) = small + (2.133e-10)
    end if

    !O2 + H2D+_PARA -> H2_PARA + O2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1471) = small + (1.067e-10)
    end if

    !O2 + H2D+_PARA -> H2_ORTHO + O2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1472) = small + (1.067e-10)
    end if

    !O2 + H2D+_ORTHO -> HD + O2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1473) = small + (4.267e-10)
    end if

    !O2 + H2D+_PARA -> HD + O2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1474) = small + (4.267e-10)
    end if

    !O2 + D2H+_ORTHO -> D2_ORTHO + O2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1475) = small + (2.133e-10)
    end if

    !O2 + D2H+_PARA -> D2_PARA + O2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1476) = small + (1.067e-10)
    end if

    !O2 + D2H+_PARA -> D2_ORTHO + O2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1477) = small + (1.067e-10)
    end if

    !O2 + D2H+_ORTHO -> HD + O2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1478) = small + (4.267e-10)
    end if

    !O2 + D2H+_PARA -> HD + O2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1479) = small + (4.267e-10)
    end if

    !OH + H3+_ORTHO -> H2_ORTHO + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1480) = small + (9.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !OH + H3+_PARA -> H2_PARA + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1481) = small + (4.750e-09&
          *(T32)**(-5.000e-01))
    end if

    !OH + H3+_PARA -> H2_ORTHO + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1482) = small + (4.750e-09&
          *(T32)**(-5.000e-01))
    end if

    !OH + D3+_ORTHO -> HD + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1483) = small + (9.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !OH + D3+_META -> HD + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1484) = small + (9.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !OH + H2D+_ORTHO -> H2_ORTHO + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1485) = small + (3.167e-09&
          *(T32)**(-5.000e-01))
    end if

    !OH + H2D+_PARA -> H2_PARA + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1486) = small + (1.583e-09&
          *(T32)**(-5.000e-01))
    end if

    !OH + H2D+_PARA -> H2_ORTHO + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1487) = small + (1.583e-09&
          *(T32)**(-5.000e-01))
    end if

    !OH + H2D+_ORTHO -> HD + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1488) = small + (6.333e-09&
          *(T32)**(-5.000e-01))
    end if

    !OH + H2D+_PARA -> HD + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1489) = small + (6.333e-09&
          *(T32)**(-5.000e-01))
    end if

    !OH + D2H+_ORTHO -> D2_ORTHO + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1490) = small + (3.167e-09&
          *(T32)**(-5.000e-01))
    end if

    !OH + D2H+_PARA -> D2_PARA + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1491) = small + (1.583e-09&
          *(T32)**(-5.000e-01))
    end if

    !OH + D2H+_PARA -> D2_ORTHO + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1492) = small + (1.583e-09&
          *(T32)**(-5.000e-01))
    end if

    !OH + D2H+_ORTHO -> HD + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1493) = small + (6.333e-09&
          *(T32)**(-5.000e-01))
    end if

    !OH + D2H+_PARA -> HD + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1494) = small + (6.333e-09&
          *(T32)**(-5.000e-01))
    end if

    !OD + H3+_ORTHO -> H2_ORTHO + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1495) = small + (9.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !OD + H3+_PARA -> H2_PARA + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1496) = small + (4.750e-09&
          *(T32)**(-5.000e-01))
    end if

    !OD + H3+_PARA -> H2_ORTHO + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1497) = small + (4.750e-09&
          *(T32)**(-5.000e-01))
    end if

    !OD + H3+_ORTHO -> HD + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1498) = small + (9.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !OD + H3+_PARA -> HD + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1499) = small + (9.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !OD + D3+_ORTHO -> D2_ORTHO + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1500) = small + (9.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !OD + D3+_META -> D2_PARA + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1501) = small + (9.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !OD + H2D+_ORTHO -> H2_ORTHO + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1502) = small + (3.167e-09&
          *(T32)**(-5.000e-01))
    end if

    !OD + H2D+_PARA -> H2_PARA + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1503) = small + (1.583e-09&
          *(T32)**(-5.000e-01))
    end if

    !OD + H2D+_PARA -> H2_ORTHO + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1504) = small + (1.583e-09&
          *(T32)**(-5.000e-01))
    end if

    !OD + H2D+_ORTHO -> HD + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1505) = small + (6.333e-09&
          *(T32)**(-5.000e-01))
    end if

    !OD + H2D+_PARA -> HD + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1506) = small + (6.333e-09&
          *(T32)**(-5.000e-01))
    end if

    !OD + D2H+_ORTHO -> D2_ORTHO + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1507) = small + (3.167e-09&
          *(T32)**(-5.000e-01))
    end if

    !OD + D2H+_PARA -> D2_PARA + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1508) = small + (1.583e-09&
          *(T32)**(-5.000e-01))
    end if

    !OD + D2H+_PARA -> D2_ORTHO + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1509) = small + (1.583e-09&
          *(T32)**(-5.000e-01))
    end if

    !OD + D2H+_ORTHO -> HD + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1510) = small + (6.333e-09&
          *(T32)**(-5.000e-01))
    end if

    !OD + D2H+_PARA -> HD + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1511) = small + (6.333e-09&
          *(T32)**(-5.000e-01))
    end if

    !NO2 + H3+_ORTHO -> H2_ORTHO + OH + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1512) = small + (7.280e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO2 + H3+_PARA -> H2_PARA + OH + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1513) = small + (3.640e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO2 + H3+_PARA -> H2_ORTHO + OH + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1514) = small + (3.640e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO2 + D3+_ORTHO -> D2_ORTHO + OD + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1515) = small + (7.280e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO2 + D3+_META -> D2_PARA + OD + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1516) = small + (7.280e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO2 + H2D+_ORTHO -> H2_ORTHO + OD + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1517) = small + (2.427e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO2 + H2D+_PARA -> H2_PARA + OD + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1518) = small + (1.213e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO2 + H2D+_PARA -> H2_ORTHO + OD + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1519) = small + (1.213e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO2 + H2D+_ORTHO -> HD + OH + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1520) = small + (4.853e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO2 + H2D+_PARA -> HD + OH + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1521) = small + (4.853e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO2 + D2H+_ORTHO -> D2_ORTHO + OH + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1522) = small + (2.427e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO2 + D2H+_PARA -> D2_PARA + OH + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1523) = small + (1.213e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO2 + D2H+_PARA -> D2_ORTHO + OH + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1524) = small + (1.213e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO2 + D2H+_ORTHO -> HD + OD + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1525) = small + (4.853e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO2 + D2H+_PARA -> HD + OD + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1526) = small + (4.853e-10&
          *(T32)**(-5.000e-01))
    end if

    !C2H + H+ -> H2_PARA + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1527) = small + (3.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !C2H + D+ -> HD + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1528) = small + (3.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !C2D + H+ -> HD + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1529) = small + (3.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !C2D + D+ -> D2_PARA + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1530) = small + (3.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !CCO + H+ -> H + C2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1531) = small + (1.200e-08&
          *(T32)**(-5.000e-01))
    end if

    !CCO + D+ -> D + C2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1532) = small + (1.200e-08&
          *(T32)**(-5.000e-01))
    end if

    !CH2 + H+ -> H2_PARA + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1533) = small + (1.140e-09&
          *(T32)**(-5.000e-01))
    end if

    !CH2 + D+ -> H2_PARA + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1534) = small + (5.700e-10&
          *(T32)**(-5.000e-01))
    end if

    !CH2 + D+ -> HD + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1535) = small + (5.700e-10&
          *(T32)**(-5.000e-01))
    end if

    !CD2 + H+ -> D2_PARA + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1536) = small + (5.700e-10&
          *(T32)**(-5.000e-01))
    end if

    !CD2 + H+ -> HD + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1537) = small + (5.700e-10&
          *(T32)**(-5.000e-01))
    end if

    !CD2 + D+ -> D2_PARA + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1538) = small + (1.140e-09&
          *(T32)**(-5.000e-01))
    end if

    !CHD + H+ -> H2_PARA + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1539) = small + (5.700e-10&
          *(T32)**(-5.000e-01))
    end if

    !CHD + H+ -> HD + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1540) = small + (5.700e-10&
          *(T32)**(-5.000e-01))
    end if

    !CHD + D+ -> D2_PARA + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1541) = small + (5.700e-10&
          *(T32)**(-5.000e-01))
    end if

    !CHD + D+ -> HD + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1542) = small + (5.700e-10&
          *(T32)**(-5.000e-01))
    end if

    !CO2 + H+ -> O + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1543) = small + (3.000e-09)
    end if

    !CO2 + D+ -> O + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1544) = small + (3.000e-09)
    end if

    !HCO + H+ -> H2_PARA + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1545) = small + (1.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !HCO + D+ -> HD + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1546) = small + (1.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !DCO + H+ -> HD + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1547) = small + (1.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !DCO + D+ -> D2_PARA + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1548) = small + (1.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !HCO + H+ -> CO + H2+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1549) = small + (1.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !HCO + D+ -> CO + HD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1550) = small + (1.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !DCO + H+ -> CO + HD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1551) = small + (1.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !DCO + D+ -> CO + D2+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1552) = small + (1.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !C + HNC+ -> CN + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1553) = small + (1.100e-09)
    end if

    !C + DNC+ -> CN + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1554) = small + (1.100e-09)
    end if

    !C2 + HNC+ -> CN + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1555) = small + (8.400e-10)
    end if

    !C2 + DNC+ -> CN + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1556) = small + (8.400e-10)
    end if

    !CH + HNC+ -> CN + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1557) = small + (6.300e-10)
    end if

    !CH + DNC+ -> CN + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1558) = small + (6.300e-10)
    end if

    !CD + HNC+ -> CN + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1559) = small + (6.300e-10)
    end if

    !CD + DNC+ -> CN + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1560) = small + (6.300e-10)
    end if

    !NH + HNC+ -> CN + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1561) = small + (6.500e-10)
    end if

    !NH + DNC+ -> CN + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1562) = small + (6.500e-10)
    end if

    !ND + HNC+ -> CN + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1563) = small + (6.500e-10)
    end if

    !ND + DNC+ -> CN + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1564) = small + (6.500e-10)
    end if

    !O2 + HNC+ -> HCO + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1565) = small + (9.000e-11)
    end if

    !O2 + DNC+ -> DCO + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1566) = small + (9.000e-11)
    end if

    !OH + HNC+ -> CN + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1567) = small + (6.300e-10)
    end if

    !OH + DNC+ -> CN + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1568) = small + (6.300e-10)
    end if

    !OD + HNC+ -> CN + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1569) = small + (6.300e-10)
    end if

    !OD + DNC+ -> CN + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1570) = small + (6.300e-10)
    end if

    !C + HNO+ -> NO + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1571) = small + (1.000e-09)
    end if

    !C + DNO+ -> NO + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1572) = small + (1.000e-09)
    end if

    !O + HNO+ -> H + NO2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1573) = small + (1.000e-12)
    end if

    !O + DNO+ -> D + NO2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1574) = small + (1.000e-12)
    end if

    !C2 + HNO+ -> NO + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1575) = small + (8.200e-10)
    end if

    !C2 + DNO+ -> NO + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1576) = small + (8.200e-10)
    end if

    !CH + HNO+ -> NO + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1577) = small + (4.400e-09&
          *(T32)**(-5.000e-01))
    end if

    !CH + DNO+ -> NO + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1578) = small + (4.400e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD + HNO+ -> NO + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1579) = small + (4.400e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD + DNO+ -> NO + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1580) = small + (4.400e-09&
          *(T32)**(-5.000e-01))
    end if

    !CO + HNO+ -> NO + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1581) = small + (2.700e-10&
          *(T32)**(-5.000e-01))
    end if

    !CO + DNO+ -> NO + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1582) = small + (2.700e-10&
          *(T32)**(-5.000e-01))
    end if

    !NH + HNO+ -> NO + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1583) = small + (3.800e-09&
          *(T32)**(-5.000e-01))
    end if

    !NH + DNO+ -> NO + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1584) = small + (3.800e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND + HNO+ -> NO + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1585) = small + (3.800e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND + DNO+ -> NO + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1586) = small + (3.800e-09&
          *(T32)**(-5.000e-01))
    end if

    !OH + HNO+ -> NO + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1587) = small + (4.600e-09&
          *(T32)**(-5.000e-01))
    end if

    !OH + DNO+ -> NO + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1588) = small + (4.600e-09&
          *(T32)**(-5.000e-01))
    end if

    !OD + HNO+ -> NO + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1589) = small + (4.600e-09&
          *(T32)**(-5.000e-01))
    end if

    !OD + DNO+ -> NO + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1590) = small + (4.600e-09&
          *(T32)**(-5.000e-01))
    end if

    !CO + HOC+ -> CO + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1591) = small + (4.000e-10)
    end if

    !CO + DOC+ -> CO + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1592) = small + (4.000e-10)
    end if

    !H2_PARA + HOC+ -> H2_PARA + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1593) = small + (1.000e-11)
    end if

    !H2_ORTHO + HOC+ -> H2_PARA + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1594) = small + (5.000e-12)
    end if

    !H2_ORTHO + HOC+ -> H2_ORTHO + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1595) = small + (5.000e-12)
    end if

    !H2_PARA + DOC+ -> H2_PARA + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1596) = small + (5.000e-12)
    end if

    !H2_ORTHO + DOC+ -> H2_PARA + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1597) = small + (3.330e-12)
    end if

    !H2_ORTHO + DOC+ -> H2_ORTHO + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1598) = small + (3.330e-12)
    end if

    !H2_PARA + DOC+ -> HD + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1599) = small + (5.000e-12)
    end if

    !H2_ORTHO + DOC+ -> HD + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1600) = small + (3.330e-12)
    end if

    !D2_PARA + HOC+ -> D2_PARA + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1601) = small + (5.000e-12)
    end if

    !D2_ORTHO + HOC+ -> D2_PARA + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1602) = small + (3.330e-12)
    end if

    !D2_ORTHO + HOC+ -> D2_ORTHO + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1603) = small + (3.330e-12)
    end if

    !D2_PARA + HOC+ -> HD + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1604) = small + (5.000e-12)
    end if

    !D2_ORTHO + HOC+ -> HD + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1605) = small + (3.330e-12)
    end if

    !D2_PARA + DOC+ -> D2_PARA + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1606) = small + (1.000e-11)
    end if

    !D2_ORTHO + DOC+ -> D2_PARA + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1607) = small + (5.000e-12)
    end if

    !D2_ORTHO + DOC+ -> D2_ORTHO + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1608) = small + (5.000e-12)
    end if

    !HD + HOC+ -> H2_PARA + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1609) = small + (1.000e-11)
    end if

    !HD + HOC+ -> H2_ORTHO + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1610) = small + (5.000e-12)
    end if

    !HD + HOC+ -> HD + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1611) = small + (5.000e-12)
    end if

    !HD + DOC+ -> D2_PARA + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1612) = small + (1.000e-11)
    end if

    !HD + DOC+ -> D2_ORTHO + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1613) = small + (5.000e-12)
    end if

    !HD + DOC+ -> HD + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1614) = small + (5.000e-12)
    end if

    !N2 + HOC+ -> CO + N2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1615) = small + (2.000e-09)
    end if

    !N2 + DOC+ -> CO + N2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1616) = small + (2.000e-09)
    end if

    !C + N2H+ -> N2 + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1617) = small + (1.100e-09)
    end if

    !C + N2D+ -> N2 + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1618) = small + (1.100e-09)
    end if

    !C2 + N2H+ -> N2 + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1619) = small + (8.300e-10)
    end if

    !C2 + N2D+ -> N2 + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1620) = small + (8.300e-10)
    end if

    !CH + N2H+ -> N2 + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1621) = small + (4.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !CH + N2D+ -> N2 + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1622) = small + (4.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD + N2H+ -> N2 + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1623) = small + (4.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD + N2D+ -> N2 + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1624) = small + (4.500e-09&
          *(T32)**(-5.000e-01))
    end if

    !CO + N2H+ -> N2 + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1625) = small + (8.800e-10)
    end if

    !CO + N2D+ -> N2 + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1626) = small + (8.800e-10)
    end if

    !NH + N2H+ -> N2 + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1627) = small + (3.800e-09&
          *(T32)**(-5.000e-01))
    end if

    !NH + N2D+ -> N2 + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1628) = small + (3.800e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND + N2H+ -> N2 + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1629) = small + (3.800e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND + N2D+ -> N2 + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1630) = small + (3.800e-09&
          *(T32)**(-5.000e-01))
    end if

    !OH + N2H+ -> N2 + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1631) = small + (4.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !OH + N2D+ -> N2 + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1632) = small + (4.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !OD + N2H+ -> N2 + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1633) = small + (4.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !OD + N2D+ -> N2 + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1634) = small + (4.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !CH + N+ -> H + CN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1635) = small + (3.600e-10)
    end if

    !CD + N+ -> D + CN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1636) = small + (3.600e-10)
    end if

    !H2_PARA + N+ -> H + NH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1637) = small + (1.000e-09&
          *exp(-8.500e+01*invT))
    end if

    !H2_ORTHO + N+ -> H + NH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1638) = small + (1.000e-09&
          *exp(-8.500e+01*invT))
    end if

    !D2_PARA + N+ -> D + ND+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1639) = small + (1.000e-09&
          *exp(-8.500e+01*invT))
    end if

    !D2_ORTHO + N+ -> D + ND+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1640) = small + (1.000e-09&
          *exp(-8.500e+01*invT))
    end if

    !HD + N+ -> H + ND+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1641) = small + (5.000e-10&
          *exp(-8.500e+01*invT))
    end if

    !HD + N+ -> D + NH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1642) = small + (5.000e-10&
          *exp(-8.500e+01*invT))
    end if

    !NH + N+ -> H + N2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1643) = small + (3.700e-10)
    end if

    !ND + N+ -> D + N2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1644) = small + (3.700e-10)
    end if

    !NO + N+ -> O + N2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1645) = small + (5.000e-11)
    end if

    !O2 + N+ -> NO + O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1646) = small + (3.600e-11)
    end if

    !O2 + N+ -> O + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1647) = small + (1.700e-10)
    end if

    !OH + N+ -> H + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1648) = small + (3.700e-10)
    end if

    !OD + N+ -> D + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1649) = small + (3.700e-10)
    end if

    !CO2 + N+ -> NO + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1650) = small + (2.500e-10)
    end if

    !HCO + N+ -> CO + NH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1651) = small + (4.500e-10)
    end if

    !DCO + N+ -> CO + ND+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1652) = small + (4.500e-10)
    end if

    !C2 + O+ -> C + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1653) = small + (4.800e-10)
    end if

    !CH + O+ -> H + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1654) = small + (3.500e-10)
    end if

    !CD + O+ -> D + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1655) = small + (3.500e-10)
    end if

    !CN + O+ -> C + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1656) = small + (1.000e-09)
    end if

    !H2_PARA + O+ -> H + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1657) = small + (1.600e-09)
    end if

    !H2_ORTHO + O+ -> H + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1658) = small + (1.600e-09)
    end if

    !D2_PARA + O+ -> D + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1659) = small + (1.600e-09)
    end if

    !D2_ORTHO + O+ -> D + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1660) = small + (1.600e-09)
    end if

    !HD + O+ -> H + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1661) = small + (8.000e-10)
    end if

    !HD + O+ -> D + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1662) = small + (8.000e-10)
    end if

    !N2 + O+ -> N + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1663) = small + (1.200e-12)
    end if

    !NH + O+ -> H + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1664) = small + (3.600e-10)
    end if

    !ND + O+ -> D + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1665) = small + (3.600e-10)
    end if

    !OH + O+ -> H + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1666) = small + (3.600e-10)
    end if

    !OD + O+ -> D + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1667) = small + (3.600e-10)
    end if

    !C2H + O+ -> CH + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1668) = small + (4.600e-10)
    end if

    !C2D + O+ -> CD + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1669) = small + (4.600e-10)
    end if

    !CO2 + O+ -> CO + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1670) = small + (1.100e-09)
    end if

    !HCN + O+ -> NH + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1671) = small + (1.200e-09)
    end if

    !DCN + O+ -> ND + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1672) = small + (1.200e-09)
    end if

    !HCN + O+ -> CH + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1673) = small + (1.200e-09)
    end if

    !DCN + O+ -> CD + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1674) = small + (1.200e-09)
    end if

    !HCN + O+ -> N + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1675) = small + (1.200e-09)
    end if

    !DCN + O+ -> N + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1676) = small + (1.200e-09)
    end if

    !HCO + O+ -> CO + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1677) = small + (4.300e-10)
    end if

    !DCO + O+ -> CO + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1678) = small + (4.300e-10)
    end if

    !N2O + O+ -> NO + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1679) = small + (6.300e-10)
    end if

    !HNC + H+ -> HCN + H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1680) = small + (2.510e-08&
          *(T32)**(-5.000e-01))
    end if

    !HNC + D+ -> HCN + D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1681) = small + (1.255e-08&
          *(T32)**(-5.000e-01))
    end if

    !HNC + D+ -> DCN + H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1682) = small + (1.255e-08&
          *(T32)**(-5.000e-01))
    end if

    !DNC + H+ -> HCN + D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1683) = small + (1.255e-08&
          *(T32)**(-5.000e-01))
    end if

    !DNC + H+ -> DCN + H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1684) = small + (1.255e-08&
          *(T32)**(-5.000e-01))
    end if

    !DNC + D+ -> DCN + D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1685) = small + (2.510e-08&
          *(T32)**(-5.000e-01))
    end if

    !HNO + H+ -> H2_PARA + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1686) = small + (6.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !HNO + D+ -> HD + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1687) = small + (6.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !DNO + H+ -> HD + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1688) = small + (6.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !DNO + D+ -> D2_PARA + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1689) = small + (6.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !N + C2+ -> CN + C+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1690) = small + (4.000e-11)
    end if

    !O + C2+ -> C + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1691) = small + (3.100e-10)
    end if

    !C2 + C2+ -> C + C3+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1692) = small + (8.700e-10)
    end if

    !CH + C2+ -> H + C3+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1693) = small + (3.200e-10)
    end if

    !CD + C2+ -> D + C3+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1694) = small + (3.200e-10)
    end if

    !H2_PARA + C2+ -> H + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1695) = small + (1.100e-09)
    end if

    !H2_ORTHO + C2+ -> H + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1696) = small + (1.100e-09)
    end if

    !D2_PARA + C2+ -> D + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1697) = small + (1.100e-09)
    end if

    !D2_ORTHO + C2+ -> D + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1698) = small + (1.100e-09)
    end if

    !HD + C2+ -> H + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1699) = small + (5.500e-10)
    end if

    !HD + C2+ -> D + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1700) = small + (5.500e-10)
    end if

    !NH + C2+ -> N + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1701) = small + (3.300e-10)
    end if

    !ND + C2+ -> N + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1702) = small + (3.300e-10)
    end if

    !NH + C2+ -> H + C2N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1703) = small + (3.300e-10)
    end if

    !ND + C2+ -> D + C2N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1704) = small + (3.300e-10)
    end if

    !O2 + C2+ -> CO + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1705) = small + (8.000e-10)
    end if

    !H2O + C2+ -> OH + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1706) = small + (4.400e-10)
    end if

    !D2O + C2+ -> OD + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1707) = small + (4.400e-10)
    end if

    !HDO + C2+ -> OH + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1708) = small + (4.400e-10)
    end if

    !HDO + C2+ -> OD + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1709) = small + (4.400e-10)
    end if

    !HCO + C2+ -> CO + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1710) = small + (3.800e-10)
    end if

    !DCO + C2+ -> CO + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1711) = small + (3.800e-10)
    end if

    !C + CH+ -> H + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1712) = small + (1.200e-09)
    end if

    !C + CD+ -> D + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1713) = small + (1.200e-09)
    end if

    !H + CH+ -> H2_PARA + C+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1714) = small + (7.500e-10)
    end if

    !H + CD+ -> HD + C+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1715) = small + (7.500e-10)
    end if

    !D + CH+ -> HD + C+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1716) = small + (7.500e-10)
    end if

    !D + CD+ -> D2_PARA + C+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1717) = small + (7.500e-10)
    end if

    !N + CH+ -> H + CN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1718) = small + (1.900e-10)
    end if

    !N + CD+ -> D + CN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1719) = small + (1.900e-10)
    end if

    !O + CH+ -> H + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1720) = small + (3.500e-10)
    end if

    !O + CD+ -> D + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1721) = small + (3.500e-10)
    end if

    !C2 + CH+ -> H + C3+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1722) = small + (1.000e-09)
    end if

    !C2 + CD+ -> D + C3+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1723) = small + (1.000e-09)
    end if

    !CH + CH+ -> H2_PARA + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1724) = small + (7.400e-10)
    end if

    !CH + CD+ -> HD + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1725) = small + (7.400e-10)
    end if

    !CD + CH+ -> HD + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1726) = small + (7.400e-10)
    end if

    !CD + CD+ -> D2_PARA + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1727) = small + (7.400e-10)
    end if

    !CN + CH+ -> H + C2N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1728) = small + (1.100e-09)
    end if

    !CN + CD+ -> D + C2N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1729) = small + (1.100e-09)
    end if

    !CN + CH+ -> H + CNC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1730) = small + (5.500e-10)
    end if

    !CN + CD+ -> D + CNC+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1731) = small + (5.500e-10)
    end if

    !H2_PARA + CH+ -> H + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1732) = small + (1.200e-09)
    end if

    !H2_ORTHO + CH+ -> H + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1733) = small + (1.200e-09)
    end if

    !H2_PARA + CD+ -> H + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1734) = small + (6.000e-10)
    end if

    !H2_ORTHO + CD+ -> H + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1735) = small + (6.000e-10)
    end if

    !H2_PARA + CD+ -> D + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1736) = small + (6.000e-10)
    end if

    !H2_ORTHO + CD+ -> D + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1737) = small + (6.000e-10)
    end if

    !D2_PARA + CH+ -> H + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1738) = small + (6.000e-10)
    end if

    !D2_ORTHO + CH+ -> H + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1739) = small + (6.000e-10)
    end if

    !D2_PARA + CH+ -> D + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1740) = small + (6.000e-10)
    end if

    !D2_ORTHO + CH+ -> D + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1741) = small + (6.000e-10)
    end if

    !D2_PARA + CD+ -> D + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1742) = small + (1.200e-09)
    end if

    !D2_ORTHO + CD+ -> D + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1743) = small + (1.200e-09)
    end if

    !HD + CH+ -> H + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1744) = small + (6.000e-10)
    end if

    !HD + CH+ -> D + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1745) = small + (6.000e-10)
    end if

    !HD + CD+ -> H + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1746) = small + (6.000e-10)
    end if

    !HD + CD+ -> D + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1747) = small + (6.000e-10)
    end if

    !NH + CH+ -> H2_PARA + CN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1748) = small + (7.600e-10)
    end if

    !NH + CD+ -> HD + CN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1749) = small + (7.600e-10)
    end if

    !ND + CH+ -> HD + CN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1750) = small + (7.600e-10)
    end if

    !ND + CD+ -> D2_PARA + CN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1751) = small + (7.600e-10)
    end if

    !O2 + CH+ -> HCO + O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1752) = small + (1.000e-11)
    end if

    !O2 + CD+ -> DCO + O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1753) = small + (1.000e-11)
    end if

    !O2 + CH+ -> OH + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1754) = small + (1.000e-11)
    end if

    !O2 + CD+ -> OD + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1755) = small + (1.000e-11)
    end if

    !O2 + CH+ -> O + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1756) = small + (9.700e-10)
    end if

    !O2 + CD+ -> O + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1757) = small + (9.700e-10)
    end if

    !OH + CH+ -> H2_PARA + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1758) = small + (7.500e-10)
    end if

    !OH + CD+ -> HD + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1759) = small + (7.500e-10)
    end if

    !OD + CH+ -> HD + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1760) = small + (7.500e-10)
    end if

    !OD + CD+ -> D2_PARA + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1761) = small + (7.500e-10)
    end if

    !C2H + CH+ -> H2_PARA + C3+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1762) = small + (9.800e-10)
    end if

    !C2H + CD+ -> HD + C3+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1763) = small + (9.800e-10)
    end if

    !C2D + CH+ -> HD + C3+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1764) = small + (9.800e-10)
    end if

    !C2D + CD+ -> D2_PARA + C3+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1765) = small + (9.800e-10)
    end if

    !CH2 + CH+ -> H2_PARA + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1766) = small + (1.000e-09)
    end if

    !CH2 + CD+ -> H2_PARA + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1767) = small + (5.000e-10)
    end if

    !CH2 + CD+ -> HD + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1768) = small + (5.000e-10)
    end if

    !CD2 + CH+ -> D2_PARA + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1769) = small + (5.000e-10)
    end if

    !CD2 + CH+ -> HD + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1770) = small + (5.000e-10)
    end if

    !CD2 + CD+ -> D2_PARA + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1771) = small + (1.000e-09)
    end if

    !CHD + CH+ -> H2_PARA + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1772) = small + (5.000e-10)
    end if

    !CHD + CH+ -> HD + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1773) = small + (5.000e-10)
    end if

    !CHD + CD+ -> D2_PARA + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1774) = small + (5.000e-10)
    end if

    !CHD + CD+ -> HD + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1775) = small + (5.000e-10)
    end if

    !CO2 + CH+ -> CO + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1776) = small + (1.600e-09)
    end if

    !CO2 + CD+ -> CO + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1777) = small + (1.600e-09)
    end if

    !H2O + CH+ -> H2_PARA + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1778) = small + (2.900e-09)
    end if

    !H2O + CD+ -> H2_PARA + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1779) = small + (1.450e-09)
    end if

    !H2O + CD+ -> HD + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1780) = small + (1.450e-09)
    end if

    !D2O + CH+ -> D2_PARA + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1781) = small + (1.450e-09)
    end if

    !D2O + CH+ -> HD + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1782) = small + (1.450e-09)
    end if

    !D2O + CD+ -> D2_PARA + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1783) = small + (2.900e-09)
    end if

    !HDO + CH+ -> H2_PARA + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1784) = small + (1.450e-09)
    end if

    !HDO + CH+ -> HD + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1785) = small + (1.450e-09)
    end if

    !HDO + CD+ -> D2_PARA + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1786) = small + (1.450e-09)
    end if

    !HDO + CD+ -> HD + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1787) = small + (1.450e-09)
    end if

    !HCN + CH+ -> H2_PARA + C2N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1788) = small + (3.600e-10)
    end if

    !HCN + CD+ -> HD + C2N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1789) = small + (3.600e-10)
    end if

    !DCN + CH+ -> HD + C2N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1790) = small + (3.600e-10)
    end if

    !DCN + CD+ -> D2_PARA + C2N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1791) = small + (3.600e-10)
    end if

    !HCO + CH+ -> CO + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1792) = small + (4.600e-10)
    end if

    !HCO + CD+ -> CO + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1793) = small + (4.600e-10)
    end if

    !DCO + CH+ -> CO + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1794) = small + (4.600e-10)
    end if

    !DCO + CD+ -> CO + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1795) = small + (4.600e-10)
    end if

    !NH2 + CH+ -> H2_PARA + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1796) = small + (1.100e-09)
    end if

    !NH2 + CD+ -> H2_PARA + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1797) = small + (5.500e-10)
    end if

    !NH2 + CD+ -> HD + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1798) = small + (5.500e-10)
    end if

    !ND2 + CH+ -> D2_PARA + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1799) = small + (5.500e-10)
    end if

    !ND2 + CH+ -> HD + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1800) = small + (5.500e-10)
    end if

    !ND2 + CD+ -> D2_PARA + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1801) = small + (1.100e-09)
    end if

    !NHD + CH+ -> H2_PARA + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1802) = small + (5.500e-10)
    end if

    !NHD + CH+ -> HD + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1803) = small + (5.500e-10)
    end if

    !NHD + CD+ -> D2_PARA + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1804) = small + (5.500e-10)
    end if

    !NHD + CD+ -> HD + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1805) = small + (5.500e-10)
    end if

    !O + N2+ -> N + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1806) = small + (1.400e-10)
    end if

    !H2_PARA + N2+ -> H + N2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1807) = small + (1.700e-09)
    end if

    !H2_ORTHO + N2+ -> H + N2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1808) = small + (1.700e-09)
    end if

    !D2_PARA + N2+ -> D + N2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1809) = small + (1.700e-09)
    end if

    !D2_ORTHO + N2+ -> D + N2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1810) = small + (1.700e-09)
    end if

    !HD + N2+ -> H + N2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1811) = small + (8.500e-10)
    end if

    !HD + N2+ -> D + N2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1812) = small + (8.500e-10)
    end if

    !H2O + N2+ -> OH + N2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1813) = small + (2.000e-09)
    end if

    !D2O + N2+ -> OD + N2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1814) = small + (2.000e-09)
    end if

    !HDO + N2+ -> OH + N2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1815) = small + (2.000e-09)
    end if

    !HDO + N2+ -> OD + N2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1816) = small + (2.000e-09)
    end if

    !HCO + N2+ -> CO + N2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1817) = small + (3.700e-10)
    end if

    !DCO + N2+ -> CO + N2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1818) = small + (3.700e-10)
    end if

    !C + NH+ -> N + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1819) = small + (1.600e-09)
    end if

    !C + ND+ -> N + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1820) = small + (1.600e-09)
    end if

    !N + NH+ -> H + N2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1821) = small + (1.300e-09)
    end if

    !N + ND+ -> D + N2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1822) = small + (1.300e-09)
    end if

    !O + NH+ -> N + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1823) = small + (1.000e-09)
    end if

    !O + ND+ -> N + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1824) = small + (1.000e-09)
    end if

    !C2 + NH+ -> N + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1825) = small + (4.900e-10)
    end if

    !C2 + ND+ -> N + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1826) = small + (4.900e-10)
    end if

    !C2 + NH+ -> H + C2N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1827) = small + (4.900e-10)
    end if

    !C2 + ND+ -> D + C2N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1828) = small + (4.900e-10)
    end if

    !C2 + NH+ -> C + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1829) = small + (4.900e-10)
    end if

    !C2 + ND+ -> C + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1830) = small + (4.900e-10)
    end if

    !CH + NH+ -> N + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1831) = small + (9.900e-10)
    end if

    !CH + ND+ -> N + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1832) = small + (9.900e-10)
    end if

    !CD + NH+ -> N + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1833) = small + (9.900e-10)
    end if

    !CD + ND+ -> N + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1834) = small + (9.900e-10)
    end if

    !CN + NH+ -> N + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1835) = small + (1.600e-09)
    end if

    !CN + ND+ -> N + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1836) = small + (1.600e-09)
    end if

    !CO + NH+ -> N + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1837) = small + (1.600e-09)
    end if

    !CO + ND+ -> N + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1838) = small + (1.600e-09)
    end if

    !CO + NH+ -> H + NCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1839) = small + (5.390e-10)
    end if

    !CO + ND+ -> D + NCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1840) = small + (5.390e-10)
    end if

    !H2_ORTHO + NH+ -> N + H3+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1841) = small + (1.125e-10)
    end if

    !H2_PARA + ND+ -> N + H2D+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1842) = small + (2.250e-10)
    end if

    !H2_ORTHO + ND+ -> N + H2D+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1843) = small + (1.125e-10)
    end if

    !H2_ORTHO + ND+ -> N + H2D+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1844) = small + (1.125e-10)
    end if

    !D2_PARA + NH+ -> N + D2H+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1845) = small + (2.250e-10)
    end if

    !D2_ORTHO + NH+ -> N + D2H+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1846) = small + (1.125e-10)
    end if

    !D2_ORTHO + NH+ -> N + D2H+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1847) = small + (1.125e-10)
    end if

    !D2_PARA + ND+ -> N + D3+_META
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1848) = small + (1.125e-10)
    end if

    !D2_ORTHO + ND+ -> N + D3+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1849) = small + (7.500e-11)
    end if

    !D2_ORTHO + ND+ -> N + D3+_META
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1850) = small + (7.500e-11)
    end if

    !HD + NH+ -> N + H2D+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1851) = small + (1.125e-10)
    end if

    !HD + NH+ -> N + H2D+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1852) = small + (1.125e-10)
    end if

    !HD + ND+ -> N + D2H+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1853) = small + (1.125e-10)
    end if

    !HD + ND+ -> N + D2H+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1854) = small + (1.125e-10)
    end if

    !H2_PARA + NH+ -> H + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1855) = small + (1.000e-09)
    end if

    !H2_ORTHO + NH+ -> H + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1856) = small + (1.000e-09)
    end if

    !H2_PARA + ND+ -> H + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1857) = small + (5.000e-10)
    end if

    !H2_ORTHO + ND+ -> H + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1858) = small + (5.000e-10)
    end if

    !H2_PARA + ND+ -> D + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1859) = small + (5.000e-10)
    end if

    !H2_ORTHO + ND+ -> D + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1860) = small + (5.000e-10)
    end if

    !D2_PARA + NH+ -> H + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1861) = small + (5.000e-10)
    end if

    !D2_ORTHO + NH+ -> H + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1862) = small + (5.000e-10)
    end if

    !D2_PARA + NH+ -> D + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1863) = small + (5.000e-10)
    end if

    !D2_ORTHO + NH+ -> D + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1864) = small + (5.000e-10)
    end if

    !D2_PARA + ND+ -> D + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1865) = small + (1.000e-09)
    end if

    !D2_ORTHO + ND+ -> D + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1866) = small + (1.000e-09)
    end if

    !HD + NH+ -> H + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1867) = small + (5.000e-10)
    end if

    !HD + NH+ -> D + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1868) = small + (5.000e-10)
    end if

    !HD + ND+ -> H + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1869) = small + (5.000e-10)
    end if

    !HD + ND+ -> D + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1870) = small + (5.000e-10)
    end if

    !N2 + NH+ -> N + N2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1871) = small + (1.500e-09)
    end if

    !N2 + ND+ -> N + N2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1872) = small + (1.500e-09)
    end if

    !NH + NH+ -> N + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1873) = small + (1.000e-09)
    end if

    !NH + ND+ -> N + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1874) = small + (1.000e-09)
    end if

    !ND + NH+ -> N + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1875) = small + (1.000e-09)
    end if

    !ND + ND+ -> N + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1876) = small + (1.000e-09)
    end if

    !NO + NH+ -> O + N2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1877) = small + (1.780e-10)
    end if

    !NO + ND+ -> O + N2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1878) = small + (1.780e-10)
    end if

    !O2 + NH+ -> OH + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1879) = small + (2.000e-10)
    end if

    !O2 + ND+ -> OD + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1880) = small + (2.000e-10)
    end if

    !O2 + NH+ -> N + O2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1881) = small + (1.600e-10)
    end if

    !O2 + ND+ -> N + O2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1882) = small + (1.600e-10)
    end if

    !OH + NH+ -> N + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1883) = small + (1.000e-09)
    end if

    !OH + ND+ -> N + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1884) = small + (1.000e-09)
    end if

    !OD + NH+ -> N + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1885) = small + (1.000e-09)
    end if

    !OD + ND+ -> N + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1886) = small + (1.000e-09)
    end if

    !CO2 + NH+ -> HCO + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1887) = small + (3.300e-10)
    end if

    !CO2 + ND+ -> DCO + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1888) = small + (3.300e-10)
    end if

    !CO2 + NH+ -> CO + HNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1889) = small + (3.850e-10)
    end if

    !CO2 + ND+ -> CO + DNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1890) = small + (3.850e-10)
    end if

    !H2O + NH+ -> H2_PARA + HNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1891) = small + (3.500e-10)
    end if

    !H2O + ND+ -> H2_PARA + DNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1892) = small + (1.750e-10)
    end if

    !H2O + ND+ -> HD + HNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1893) = small + (1.750e-10)
    end if

    !D2O + NH+ -> D2_PARA + HNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1894) = small + (1.750e-10)
    end if

    !D2O + NH+ -> HD + DNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1895) = small + (1.750e-10)
    end if

    !D2O + ND+ -> D2_PARA + DNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1896) = small + (3.500e-10)
    end if

    !HDO + NH+ -> H2_PARA + DNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1897) = small + (1.750e-10)
    end if

    !HDO + NH+ -> HD + HNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1898) = small + (1.750e-10)
    end if

    !HDO + ND+ -> D2_PARA + HNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1899) = small + (1.750e-10)
    end if

    !HDO + ND+ -> HD + DNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1900) = small + (1.750e-10)
    end if

    !H2O + NH+ -> OH + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1901) = small + (8.750e-10)
    end if

    !H2O + ND+ -> OH + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1902) = small + (4.375e-10)
    end if

    !H2O + ND+ -> OD + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1903) = small + (4.375e-10)
    end if

    !D2O + NH+ -> OH + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1904) = small + (4.375e-10)
    end if

    !D2O + NH+ -> OD + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1905) = small + (4.375e-10)
    end if

    !D2O + ND+ -> OD + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1906) = small + (8.750e-10)
    end if

    !HDO + NH+ -> OH + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1907) = small + (4.375e-10)
    end if

    !HDO + NH+ -> OD + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1908) = small + (4.375e-10)
    end if

    !HDO + ND+ -> OH + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1909) = small + (4.375e-10)
    end if

    !HDO + ND+ -> OD + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1910) = small + (4.375e-10)
    end if

    !C + O2+ -> O + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1911) = small + (5.200e-11)
    end if

    !N + O2+ -> O + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1912) = small + (1.800e-10)
    end if

    !C2 + O2+ -> CO + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1913) = small + (4.100e-10)
    end if

    !CH + O2+ -> O + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1914) = small + (3.100e-10)
    end if

    !CD + O2+ -> O + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1915) = small + (3.100e-10)
    end if

    !NH + O2+ -> O + HNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1916) = small + (3.200e-10)
    end if

    !ND + O2+ -> O + DNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1917) = small + (3.200e-10)
    end if

    !NH + O2+ -> H + NO2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1918) = small + (3.200e-10)
    end if

    !ND + O2+ -> D + NO2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1919) = small + (3.200e-10)
    end if

    !HCO + O2+ -> CO + O2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1920) = small + (3.600e-10)
    end if

    !DCO + O2+ -> CO + O2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1921) = small + (3.600e-10)
    end if

    !C + OH+ -> O + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1922) = small + (1.200e-09)
    end if

    !C + OD+ -> O + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1923) = small + (1.200e-09)
    end if

    !N + OH+ -> H + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1924) = small + (8.900e-10)
    end if

    !N + OD+ -> D + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1925) = small + (8.900e-10)
    end if

    !O + OH+ -> H + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1926) = small + (7.100e-10)
    end if

    !O + OD+ -> D + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1927) = small + (7.100e-10)
    end if

    !C2 + OH+ -> O + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1928) = small + (4.800e-10)
    end if

    !C2 + OD+ -> O + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1929) = small + (4.800e-10)
    end if

    !CH + OH+ -> O + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1930) = small + (3.500e-10)
    end if

    !CH + OD+ -> O + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1931) = small + (3.500e-10)
    end if

    !CD + OH+ -> O + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1932) = small + (3.500e-10)
    end if

    !CD + OD+ -> O + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1933) = small + (3.500e-10)
    end if

    !CN + OH+ -> O + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1934) = small + (1.000e-09)
    end if

    !CN + OD+ -> O + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1935) = small + (1.000e-09)
    end if

    !CO + OH+ -> O + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1936) = small + (1.000e-09)
    end if

    !CO + OD+ -> O + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1937) = small + (1.000e-09)
    end if

    !H2_PARA + OH+ -> H + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1938) = small + (1.100e-09)
    end if

    !H2_ORTHO + OH+ -> H + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1939) = small + (1.100e-09)
    end if

    !H2_PARA + OD+ -> H + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1940) = small + (5.500e-10)
    end if

    !H2_ORTHO + OD+ -> H + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1941) = small + (5.500e-10)
    end if

    !H2_PARA + OD+ -> D + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1942) = small + (5.500e-10)
    end if

    !H2_ORTHO + OD+ -> D + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1943) = small + (5.500e-10)
    end if

    !D2_PARA + OH+ -> H + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1944) = small + (5.500e-10)
    end if

    !D2_ORTHO + OH+ -> H + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1945) = small + (5.500e-10)
    end if

    !D2_PARA + OH+ -> D + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1946) = small + (5.500e-10)
    end if

    !D2_ORTHO + OH+ -> D + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1947) = small + (5.500e-10)
    end if

    !D2_PARA + OD+ -> D + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1948) = small + (1.100e-09)
    end if

    !D2_ORTHO + OD+ -> D + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1949) = small + (1.100e-09)
    end if

    !HD + OH+ -> H + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1950) = small + (5.500e-10)
    end if

    !HD + OH+ -> D + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1951) = small + (5.500e-10)
    end if

    !HD + OD+ -> H + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1952) = small + (5.500e-10)
    end if

    !HD + OD+ -> D + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1953) = small + (5.500e-10)
    end if

    !N2 + OH+ -> O + N2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1954) = small + (3.600e-10)
    end if

    !N2 + OD+ -> O + N2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1955) = small + (3.600e-10)
    end if

    !NH + OH+ -> O + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1956) = small + (3.600e-10)
    end if

    !NH + OD+ -> O + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1957) = small + (3.600e-10)
    end if

    !ND + OH+ -> O + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1958) = small + (3.600e-10)
    end if

    !ND + OD+ -> O + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1959) = small + (3.600e-10)
    end if

    !NO + OH+ -> O + HNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1960) = small + (6.100e-10)
    end if

    !NO + OD+ -> O + DNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1961) = small + (6.100e-10)
    end if

    !OH + OH+ -> O + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1962) = small + (7.000e-10)
    end if

    !OH + OD+ -> O + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1963) = small + (7.000e-10)
    end if

    !OD + OH+ -> O + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1964) = small + (7.000e-10)
    end if

    !OD + OD+ -> O + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1965) = small + (7.000e-10)
    end if

    !HCO + OH+ -> CO + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1966) = small + (2.800e-10)
    end if

    !HCO + OD+ -> CO + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1967) = small + (2.800e-10)
    end if

    !DCO + OH+ -> CO + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1968) = small + (2.800e-10)
    end if

    !DCO + OD+ -> CO + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1969) = small + (2.800e-10)
    end if

    !N + NH2+ -> H + N2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1970) = small + (9.100e-11)
    end if

    !N + ND2+ -> D + N2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1971) = small + (9.100e-11)
    end if

    !N + NHD+ -> H + N2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1972) = small + (4.550e-11)
    end if

    !N + NHD+ -> D + N2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1973) = small + (4.550e-11)
    end if

    !O + NH2+ -> H + HNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1974) = small + (7.200e-11)
    end if

    !O + ND2+ -> D + DNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1975) = small + (7.200e-11)
    end if

    !O + NHD+ -> H + DNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1976) = small + (3.600e-11)
    end if

    !O + NHD+ -> D + HNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1977) = small + (3.600e-11)
    end if

    !C2 + NH2+ -> NH + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1978) = small + (9.700e-10)
    end if

    !C2 + ND2+ -> ND + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1979) = small + (9.700e-10)
    end if

    !C2 + NHD+ -> NH + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1980) = small + (4.850e-10)
    end if

    !C2 + NHD+ -> ND + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1981) = small + (4.850e-10)
    end if

    !CH + NH2+ -> NH + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1982) = small + (3.500e-10)
    end if

    !CH + ND2+ -> NH + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1983) = small + (3.500e-10)
    end if

    !CH + ND2+ -> ND + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1984) = small + (3.500e-10)
    end if

    !CH + NHD+ -> NH + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1985) = small + (1.750e-10)
    end if

    !CH + NHD+ -> ND + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1986) = small + (1.750e-10)
    end if

    !CD + NH2+ -> NH + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1987) = small + (3.500e-10)
    end if

    !CD + NH2+ -> ND + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1988) = small + (3.500e-10)
    end if

    !CD + ND2+ -> ND + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1989) = small + (3.500e-10)
    end if

    !CD + NHD+ -> NH + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1990) = small + (1.750e-10)
    end if

    !CD + NHD+ -> ND + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1991) = small + (1.750e-10)
    end if

    !O2 + NH2+ -> OH + HNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1992) = small + (2.100e-11)
    end if

    !O2 + ND2+ -> OD + DNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1993) = small + (2.100e-11)
    end if

    !O2 + NHD+ -> OH + DNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1994) = small + (1.050e-11)
    end if

    !O2 + NHD+ -> OD + HNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1995) = small + (1.050e-11)
    end if

    !H + NO2+ -> OH + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1996) = small + (1.900e-10)
    end if

    !D + NO2+ -> OD + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1997) = small + (1.900e-10)
    end if

    !H2_PARA + NO2+ -> H2O + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1998) = small + (1.500e-10)
    end if

    !H2_ORTHO + NO2+ -> H2O + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(1999) = small + (1.500e-10)
    end if

    !D2_PARA + NO2+ -> D2O + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2000) = small + (1.500e-10)
    end if

    !D2_ORTHO + NO2+ -> D2O + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2001) = small + (1.500e-10)
    end if

    !HD + NO2+ -> HDO + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2002) = small + (1.500e-10)
    end if

    !C + O2H+ -> O2 + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2003) = small + (1.000e-09)
    end if

    !C + O2D+ -> O2 + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2004) = small + (1.000e-09)
    end if

    !N + O2H+ -> H + NO2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2005) = small + (1.000e-12)
    end if

    !N + O2D+ -> D + NO2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2006) = small + (1.000e-12)
    end if

    !O + O2H+ -> O2 + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2007) = small + (6.200e-10)
    end if

    !O + O2D+ -> O2 + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2008) = small + (6.200e-10)
    end if

    !C2 + O2H+ -> O2 + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2009) = small + (8.100e-10)
    end if

    !C2 + O2D+ -> O2 + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2010) = small + (8.100e-10)
    end if

    !CH + O2H+ -> O2 + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2011) = small + (6.200e-10)
    end if

    !CH + O2D+ -> O2 + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2012) = small + (6.200e-10)
    end if

    !CD + O2H+ -> O2 + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2013) = small + (6.200e-10)
    end if

    !CD + O2D+ -> O2 + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2014) = small + (6.200e-10)
    end if

    !CN + O2H+ -> O2 + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2015) = small + (8.600e-10)
    end if

    !CN + O2D+ -> O2 + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2016) = small + (8.600e-10)
    end if

    !CO + O2H+ -> O2 + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2017) = small + (8.400e-10)
    end if

    !CO + O2D+ -> O2 + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2018) = small + (8.400e-10)
    end if

    !H2_ORTHO + O2H+ -> O2 + H3+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2019) = small + (1.600e-10)
    end if

    !H2_PARA + O2D+ -> O2 + H2D+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2020) = small + (3.200e-10)
    end if

    !H2_ORTHO + O2D+ -> O2 + H2D+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2021) = small + (1.600e-10)
    end if

    !H2_ORTHO + O2D+ -> O2 + H2D+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2022) = small + (1.600e-10)
    end if

    !D2_PARA + O2H+ -> O2 + D2H+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2023) = small + (3.200e-10)
    end if

    !D2_ORTHO + O2H+ -> O2 + D2H+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2024) = small + (1.600e-10)
    end if

    !D2_ORTHO + O2H+ -> O2 + D2H+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2025) = small + (1.600e-10)
    end if

    !D2_PARA + O2D+ -> O2 + D3+_META
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2026) = small + (1.600e-10)
    end if

    !D2_ORTHO + O2D+ -> O2 + D3+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2027) = small + (1.067e-10)
    end if

    !D2_ORTHO + O2D+ -> O2 + D3+_META
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2028) = small + (1.067e-10)
    end if

    !HD + O2H+ -> O2 + H2D+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2029) = small + (1.600e-10)
    end if

    !HD + O2H+ -> O2 + H2D+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2030) = small + (1.600e-10)
    end if

    !HD + O2D+ -> O2 + D2H+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2031) = small + (1.600e-10)
    end if

    !HD + O2D+ -> O2 + D2H+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2032) = small + (1.600e-10)
    end if

    !N2 + O2H+ -> O2 + N2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2033) = small + (7.900e-10)
    end if

    !N2 + O2D+ -> O2 + N2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2034) = small + (7.900e-10)
    end if

    !NH + O2H+ -> O2 + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2035) = small + (6.300e-10)
    end if

    !NH + O2D+ -> O2 + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2036) = small + (6.300e-10)
    end if

    !ND + O2H+ -> O2 + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2037) = small + (6.300e-10)
    end if

    !ND + O2D+ -> O2 + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2038) = small + (6.300e-10)
    end if

    !NO + O2H+ -> O2 + HNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2039) = small + (7.700e-10)
    end if

    !NO + O2D+ -> O2 + DNO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2040) = small + (7.700e-10)
    end if

    !OH + O2H+ -> O2 + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2041) = small + (6.100e-10)
    end if

    !OH + O2D+ -> O2 + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2042) = small + (6.100e-10)
    end if

    !OD + O2H+ -> O2 + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2043) = small + (6.100e-10)
    end if

    !OD + O2D+ -> O2 + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2044) = small + (6.100e-10)
    end if

    !O + CH -> HCO+ + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2045) = small + (2.000e-11&
          *(T32)**(+4.400e-01))
    end if

    !O + CD -> DCO+ + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2046) = small + (2.000e-11&
          *(T32)**(+4.400e-01))
    end if

    !C + CH -> H + C2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2047) = small + (6.590e-11)
    end if

    !C + CD -> D + C2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2048) = small + (6.590e-11)
    end if

    !C + NH -> H + CN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2049) = small + (1.200e-10)
    end if

    !C + ND -> D + CN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2050) = small + (1.200e-10)
    end if

    !C + NO -> O + CN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2051) = small + (6.000e-11&
          *(T32)**(-1.600e-01))
    end if

    !C + NO -> N + CO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2052) = small + (9.000e-11&
          *(T32)**(-1.600e-01))
    end if

    !C + O2 -> O + CO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2053) = small + (4.700e-11&
          *(T32)**(-3.400e-01))
    end if

    !C + OH -> H + CO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2054) = small + (1.000e-10)
    end if

    !C + OD -> D + CO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2055) = small + (1.000e-10)
    end if

    !C + HCO -> CH + CO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2056) = small + (1.000e-10)
    end if

    !C + DCO -> CD + CO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2057) = small + (1.000e-10)
    end if

    !C + HCO -> H + CCO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2058) = small + (1.000e-10)
    end if

    !C + DCO -> D + CCO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2059) = small + (1.000e-10)
    end if

    !C + C2N -> C2 + CN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2060) = small + (1.000e-10)
    end if

    !C + C2H -> H + C3
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2061) = small + (1.000e-10)
    end if

    !C + C2D -> D + C3
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2062) = small + (1.000e-10)
    end if

    !C + CCO -> C2 + CO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2063) = small + (2.000e-10)
    end if

    !C + OCN -> CN + CO
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(2064) = small + (1.000e-10)
    end if

    !CO + N+ -> C + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2065) = small + (6.000e-11)
    end if

    !NO + C+ -> C + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2066) = small + (4.800e-10&
          *(T32)**(-5.000e-01))
    end if

    !CN + O- -> O + CN-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2067) = small + (1.000e-09)
    end if

    !CN + OH- -> OH + CN-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2068) = small + (1.000e-09)
    end if

    !CN + OD- -> OD + CN-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2069) = small + (1.000e-09)
    end if

    !NH + CN+ -> CN + NH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2070) = small + (6.500e-10)
    end if

    !ND + CN+ -> CN + ND+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2071) = small + (6.500e-10)
    end if

    !OH + CN+ -> CN + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2072) = small + (6.400e-10)
    end if

    !OD + CN+ -> CN + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2073) = small + (6.400e-10)
    end if

    !C2H + CN+ -> CN + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2074) = small + (8.000e-10)
    end if

    !C2D + CN+ -> CN + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2075) = small + (8.000e-10)
    end if

    !CH2 + CN+ -> CN + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2076) = small + (8.800e-10)
    end if

    !CD2 + CN+ -> CN + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2077) = small + (8.800e-10)
    end if

    !CHD + CN+ -> CN + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2078) = small + (8.800e-10)
    end if

    !NH2 + CN+ -> CN + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2079) = small + (9.100e-10)
    end if

    !ND2 + CN+ -> CN + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2080) = small + (9.100e-10)
    end if

    !NHD + CN+ -> CN + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2081) = small + (9.100e-10)
    end if

    !C + CO+ -> CO + C+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2082) = small + (1.100e-10)
    end if

    !H + CO+ -> CO + H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2083) = small + (4.000e-10)
    end if

    !D + CO+ -> CO + D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2084) = small + (4.000e-10)
    end if

    !O + CO+ -> CO + O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2085) = small + (1.400e-10)
    end if

    !C2 + CO+ -> CO + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2086) = small + (8.400e-10)
    end if

    !NO + CO+ -> CO + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2087) = small + (3.300e-10)
    end if

    !O2 + CO+ -> CO + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2088) = small + (1.200e-10)
    end if

    !CO2 + CO+ -> CO + CO2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2089) = small + (1.100e-09)
    end if

    !HCN + CO+ -> CO + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2090) = small + (3.400e-10)
    end if

    !DCN + CO+ -> CO + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2091) = small + (3.400e-10)
    end if

    !HCO + CO+ -> CO + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2092) = small + (7.400e-10)
    end if

    !DCO + CO+ -> CO + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2093) = small + (7.400e-10)
    end if

    !H + H2+_PARA -> H2_PARA + H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2094) = small + (6.400e-10)
    end if

    !H + H2+_ORTHO -> H2_ORTHO + H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2095) = small + (6.400e-10)
    end if

    !H + D2+_PARA -> D2_PARA + H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2096) = small + (6.400e-10)
    end if

    !H + D2+_ORTHO -> D2_ORTHO + H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2097) = small + (6.400e-10)
    end if

    !H + HD+ -> HD + H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2098) = small + (6.400e-10)
    end if

    !D + H2+_PARA -> H2_PARA + D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2099) = small + (6.400e-10)
    end if

    !D + H2+_ORTHO -> H2_ORTHO + D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2100) = small + (6.400e-10)
    end if

    !D + D2+_PARA -> D2_PARA + D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2101) = small + (6.400e-10)
    end if

    !D + D2+_ORTHO -> D2_ORTHO + D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2102) = small + (6.400e-10)
    end if

    !D + HD+ -> HD + D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2103) = small + (6.400e-10)
    end if

    !HCN + H2+_PARA -> H2_PARA + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2104) = small + (2.700e-09)
    end if

    !HCN + H2+_ORTHO -> H2_ORTHO + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2105) = small + (2.700e-09)
    end if

    !HCN + D2+_PARA -> D2_PARA + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2106) = small + (2.700e-09)
    end if

    !HCN + D2+_ORTHO -> D2_ORTHO + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2107) = small + (2.700e-09)
    end if

    !HCN + HD+ -> HD + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2108) = small + (2.700e-09)
    end if

    !DCN + H2+_PARA -> H2_PARA + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2109) = small + (2.700e-09)
    end if

    !DCN + H2+_ORTHO -> H2_ORTHO + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2110) = small + (2.700e-09)
    end if

    !DCN + D2+_PARA -> D2_PARA + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2111) = small + (2.700e-09)
    end if

    !DCN + D2+_ORTHO -> D2_ORTHO + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2112) = small + (2.700e-09)
    end if

    !DCN + HD+ -> HD + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2113) = small + (2.700e-09)
    end if

    !NH2 + H2+_PARA -> H2_PARA + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2114) = small + (2.100e-09)
    end if

    !NH2 + H2+_ORTHO -> H2_ORTHO + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2115) = small + (2.100e-09)
    end if

    !NH2 + D2+_PARA -> D2_PARA + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2116) = small + (2.100e-09)
    end if

    !NH2 + D2+_ORTHO -> D2_ORTHO + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2117) = small + (2.100e-09)
    end if

    !NH2 + HD+ -> HD + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2118) = small + (2.100e-09)
    end if

    !ND2 + H2+_PARA -> H2_PARA + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2119) = small + (2.100e-09)
    end if

    !ND2 + H2+_ORTHO -> H2_ORTHO + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2120) = small + (2.100e-09)
    end if

    !ND2 + D2+_PARA -> D2_PARA + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2121) = small + (2.100e-09)
    end if

    !ND2 + D2+_ORTHO -> D2_ORTHO + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2122) = small + (2.100e-09)
    end if

    !ND2 + HD+ -> HD + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2123) = small + (2.100e-09)
    end if

    !NHD + H2+_PARA -> H2_PARA + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2124) = small + (2.100e-09)
    end if

    !NHD + H2+_ORTHO -> H2_ORTHO + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2125) = small + (2.100e-09)
    end if

    !NHD + D2+_PARA -> D2_PARA + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2126) = small + (2.100e-09)
    end if

    !NHD + D2+_ORTHO -> D2_ORTHO + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2127) = small + (2.100e-09)
    end if

    !NHD + HD+ -> HD + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2128) = small + (2.100e-09)
    end if

    !C + N2+ -> N2 + C+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2129) = small + (1.100e-10)
    end if

    !H + N2+ -> N2 + H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2130) = small + (1.200e-10)
    end if

    !D + N2+ -> N2 + D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2131) = small + (1.200e-10)
    end if

    !N + N2+ -> N2 + N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2132) = small + (1.000e-11)
    end if

    !C+ + C- -> C + C
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2133) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !C+ + H- -> C + H
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2134) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !C+ + D- -> C + D
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2135) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !H+ + C- -> C + H
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2136) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !D+ + C- -> C + D
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2137) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !H+ + H- -> H + H
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2138) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !H+ + D- -> H + D
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2139) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !D+ + H- -> H + D
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2140) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !D+ + D- -> D + D
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2141) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !HE+ + C- -> C + HE
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2142) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !HE+ + H- -> H + HE
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2143) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !HE+ + D- -> D + HE
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2144) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !N+ + C- -> C + N
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2145) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !N+ + H- -> H + N
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2146) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !N+ + D- -> D + N
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2147) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !O+ + C- -> C + O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2148) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !O+ + H- -> H + O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2149) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !O+ + D- -> D + O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2150) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !H2+_PARA + H- -> H + H2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2151) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !H2+_ORTHO + H- -> H + H2_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2152) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !H2+_PARA + D- -> D + H2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2153) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !H2+_ORTHO + D- -> D + H2_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2154) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !D2+_PARA + H- -> H + D2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2155) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !D2+_ORTHO + H- -> H + D2_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2156) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !D2+_PARA + D- -> D + D2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2157) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !D2+_ORTHO + D- -> D + D2_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2158) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !HD+ + H- -> H + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2159) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !HD+ + D- -> D + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2160) = small + (2.300e-07&
          *(T32)**(-5.000e-01))
    end if

    !H + HE+ -> HE + H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2161) = small + (1.900e-15)
    end if

    !D + HE+ -> HE + D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2162) = small + (1.900e-15)
    end if

    !NO + C2H+ -> C2H + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2163) = small + (1.200e-10)
    end if

    !NO + C2D+ -> C2D + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2164) = small + (1.200e-10)
    end if

    !NO + CH2+ -> CH2 + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2165) = small + (4.200e-10)
    end if

    !NO + CD2+ -> CD2 + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2166) = small + (4.200e-10)
    end if

    !NO + CHD+ -> CHD + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2167) = small + (4.200e-10)
    end if

    !NO + CO2+ -> CO2 + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2168) = small + (1.200e-10)
    end if

    !O2 + CO2+ -> CO2 + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2169) = small + (5.000e-11)
    end if

    !H + HCN+ -> HCN + H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2170) = small + (3.700e-11)
    end if

    !H + DCN+ -> DCN + H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2171) = small + (3.700e-11)
    end if

    !D + HCN+ -> HCN + D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2172) = small + (3.700e-11)
    end if

    !D + DCN+ -> DCN + D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2173) = small + (3.700e-11)
    end if

    !O + HCN+ -> HCN + O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2174) = small + (6.500e-11)
    end if

    !O + DCN+ -> DCN + O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2175) = small + (6.500e-11)
    end if

    !NO + HCN+ -> HCN + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2176) = small + (8.100e-10)
    end if

    !NO + DCN+ -> DCN + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2177) = small + (8.100e-10)
    end if

    !O2 + HCN+ -> HCN + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2178) = small + (3.200e-10)
    end if

    !O2 + DCN+ -> DCN + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2179) = small + (3.200e-10)
    end if

    !NO + H2O+ -> H2O + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2180) = small + (1.200e-09)
    end if

    !NO + D2O+ -> D2O + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2181) = small + (1.200e-09)
    end if

    !NO + HDO+ -> HDO + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2182) = small + (1.200e-09)
    end if

    !O2 + H2O+ -> H2O + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2183) = small + (4.300e-10)
    end if

    !O2 + D2O+ -> D2O + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2184) = small + (4.300e-10)
    end if

    !O2 + HDO+ -> HDO + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2185) = small + (4.300e-10)
    end if

    !O + H+ -> H + O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2186) = small + (7.000e-10&
          *exp(-2.320e+02*invT))
    end if

    !O + D+ -> D + O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2187) = small + (7.000e-10&
          *exp(-2.320e+02*invT))
    end if

    !C2 + H+ -> H + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2188) = small + (3.100e-09)
    end if

    !C2 + D+ -> D + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2189) = small + (3.100e-09)
    end if

    !CH + H+ -> H + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2190) = small + (1.400e-08&
          *(T32)**(-5.000e-01))
    end if

    !CH + D+ -> D + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2191) = small + (1.400e-08&
          *(T32)**(-5.000e-01))
    end if

    !CD + H+ -> H + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2192) = small + (1.400e-08&
          *(T32)**(-5.000e-01))
    end if

    !CD + D+ -> D + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2193) = small + (1.400e-08&
          *(T32)**(-5.000e-01))
    end if

    !NH + H+ -> H + NH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2194) = small + (1.200e-08&
          *(T32)**(-5.000e-01))
    end if

    !NH + D+ -> D + NH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2195) = small + (1.200e-08&
          *(T32)**(-5.000e-01))
    end if

    !ND + H+ -> H + ND+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2196) = small + (1.200e-08&
          *(T32)**(-5.000e-01))
    end if

    !ND + D+ -> D + ND+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2197) = small + (1.200e-08&
          *(T32)**(-5.000e-01))
    end if

    !NO + H+ -> H + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2198) = small + (1.400e-09&
          *(T32)**(-5.000e-01))
    end if

    !NO + D+ -> D + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2199) = small + (1.400e-09&
          *(T32)**(-5.000e-01))
    end if

    !O2 + H+ -> H + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2200) = small + (1.200e-09)
    end if

    !O2 + D+ -> D + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2201) = small + (1.200e-09)
    end if

    !OH + H+ -> H + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2202) = small + (1.600e-08&
          *(T32)**(-5.000e-01))
    end if

    !OH + D+ -> D + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2203) = small + (1.600e-08&
          *(T32)**(-5.000e-01))
    end if

    !OD + H+ -> H + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2204) = small + (1.600e-08&
          *(T32)**(-5.000e-01))
    end if

    !OD + D+ -> D + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2205) = small + (1.600e-08&
          *(T32)**(-5.000e-01))
    end if

    !C2N + H+ -> H + C2N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2206) = small + (5.560e-09&
          *(T32)**(-5.000e-01))
    end if

    !C2N + D+ -> D + C2N+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2207) = small + (5.560e-09&
          *(T32)**(-5.000e-01))
    end if

    !C3 + H+ -> H + C3+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2208) = small + (4.000e-09)
    end if

    !C3 + D+ -> D + C3+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2209) = small + (4.000e-09)
    end if

    !H2O + H+ -> H + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2210) = small + (7.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !H2O + D+ -> D + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2211) = small + (7.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !D2O + H+ -> H + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2212) = small + (7.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !D2O + D+ -> D + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2213) = small + (7.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !HDO + H+ -> H + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2214) = small + (7.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !HDO + D+ -> D + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2215) = small + (7.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !HCN + H+ -> H + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2216) = small + (2.780e-08&
          *(T32)**(-5.000e-01))
    end if

    !HCN + D+ -> D + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2217) = small + (2.780e-08&
          *(T32)**(-5.000e-01))
    end if

    !DCN + H+ -> H + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2218) = small + (2.780e-08&
          *(T32)**(-5.000e-01))
    end if

    !DCN + D+ -> D + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2219) = small + (2.780e-08&
          *(T32)**(-5.000e-01))
    end if

    !NO + HNC+ -> HNC + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2220) = small + (8.100e-10)
    end if

    !NO + DNC+ -> DNC + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2221) = small + (8.100e-10)
    end if

    !NO + HNO+ -> HNO + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2222) = small + (3.600e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO + DNO+ -> DNO + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2223) = small + (3.600e-10&
          *(T32)**(-5.000e-01))
    end if

    !C2 + N+ -> N + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2224) = small + (1.000e-09)
    end if

    !CN + N+ -> N + CN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2225) = small + (1.100e-09)
    end if

    !CO + N+ -> N + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2226) = small + (4.900e-10)
    end if

    !C2H + N+ -> N + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2227) = small + (9.500e-10)
    end if

    !C2D + N+ -> N + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2228) = small + (9.500e-10)
    end if

    !CH2 + N+ -> N + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2229) = small + (1.000e-09)
    end if

    !CD2 + N+ -> N + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2230) = small + (1.000e-09)
    end if

    !CHD + N+ -> N + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2231) = small + (1.000e-09)
    end if

    !H2O + N+ -> N + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2232) = small + (2.600e-09)
    end if

    !D2O + N+ -> N + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2233) = small + (2.600e-09)
    end if

    !HDO + N+ -> N + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2234) = small + (2.600e-09)
    end if

    !HCN + N+ -> N + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2235) = small + (1.200e-09)
    end if

    !DCN + N+ -> N + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2236) = small + (1.200e-09)
    end if

    !NH2 + N+ -> N + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2237) = small + (1.000e-09)
    end if

    !ND2 + N+ -> N + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2238) = small + (1.000e-09)
    end if

    !NHD + N+ -> N + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2239) = small + (1.000e-09)
    end if

    !H + O+ -> O + H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2240) = small + (7.000e-10)
    end if

    !D + O+ -> O + D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2241) = small + (7.000e-10)
    end if

    !NO + O+ -> O + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2242) = small + (1.700e-12)
    end if

    !O2 + O+ -> O + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2243) = small + (3.000e-11)
    end if

    !CH2 + O+ -> O + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2244) = small + (9.700e-10)
    end if

    !CD2 + O+ -> O + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2245) = small + (9.700e-10)
    end if

    !CHD + O+ -> O + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2246) = small + (9.700e-10)
    end if

    !H2O + O+ -> O + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2247) = small + (3.200e-09)
    end if

    !D2O + O+ -> O + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2248) = small + (3.200e-09)
    end if

    !HDO + O+ -> O + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2249) = small + (3.200e-09)
    end if

    !NH2 + O+ -> O + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2250) = small + (1.000e-09)
    end if

    !ND2 + O+ -> O + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2251) = small + (1.000e-09)
    end if

    !NHD + O+ -> O + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2252) = small + (1.000e-09)
    end if

    !NO2 + O+ -> O + NO2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2253) = small + (1.600e-09)
    end if

    !NH2 + H+ -> H + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2254) = small + (7.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !NH2 + D+ -> D + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2255) = small + (7.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND2 + H+ -> H + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2256) = small + (7.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND2 + D+ -> D + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2257) = small + (7.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !NHD + H+ -> H + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2258) = small + (7.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !NHD + D+ -> D + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2259) = small + (7.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !C + C2+ -> C2 + C+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2260) = small + (1.100e-10)
    end if

    !NO + C2+ -> C2 + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2261) = small + (3.400e-10)
    end if

    !NO + CH+ -> CH + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2262) = small + (7.600e-10)
    end if

    !NO + CD+ -> CD + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2263) = small + (7.600e-10)
    end if

    !C + CN+ -> CN + C+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2264) = small + (1.100e-10)
    end if

    !H + CN+ -> CN + H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2265) = small + (6.400e-10)
    end if

    !D + CN+ -> CN + D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2266) = small + (6.400e-10)
    end if

    !O + CN+ -> CN + O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2267) = small + (6.500e-11)
    end if

    !C2 + CN+ -> CN + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2268) = small + (8.500e-10)
    end if

    !CH + CN+ -> CN + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2269) = small + (6.400e-10)
    end if

    !CD + CN+ -> CN + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2270) = small + (6.400e-10)
    end if

    !CO + CN+ -> CN + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2271) = small + (6.300e-10)
    end if

    !C2 + N2+ -> N2 + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2272) = small + (8.400e-10)
    end if

    !CH + N2+ -> N2 + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2273) = small + (6.300e-10)
    end if

    !CD + N2+ -> N2 + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2274) = small + (6.300e-10)
    end if

    !CN + N2+ -> N2 + CN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2275) = small + (1.000e-10)
    end if

    !CO + N2+ -> N2 + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2276) = small + (7.000e-11)
    end if

    !NH + N2+ -> N2 + NH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2277) = small + (6.500e-10)
    end if

    !ND + N2+ -> N2 + ND+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2278) = small + (6.500e-10)
    end if

    !NO + N2+ -> N2 + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2279) = small + (4.400e-10)
    end if

    !O2 + N2+ -> N2 + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2280) = small + (5.000e-11)
    end if

    !OH + N2+ -> N2 + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2281) = small + (6.300e-10)
    end if

    !OD + N2+ -> N2 + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2282) = small + (6.300e-10)
    end if

    !C2H + N2+ -> N2 + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2283) = small + (7.900e-10)
    end if

    !C2D + N2+ -> N2 + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2284) = small + (7.900e-10)
    end if

    !CH2 + N2+ -> N2 + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2285) = small + (8.700e-10)
    end if

    !CD2 + N2+ -> N2 + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2286) = small + (8.700e-10)
    end if

    !CHD + N2+ -> N2 + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2287) = small + (8.700e-10)
    end if

    !CO2 + N2+ -> N2 + CO2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2288) = small + (9.000e-10)
    end if

    !HCN + N2+ -> N2 + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2289) = small + (1.000e-09)
    end if

    !DCN + N2+ -> N2 + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2290) = small + (1.000e-09)
    end if

    !NH2 + N2+ -> N2 + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2291) = small + (8.900e-10)
    end if

    !ND2 + N2+ -> N2 + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2292) = small + (8.900e-10)
    end if

    !NHD + N2+ -> N2 + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2293) = small + (8.900e-10)
    end if

    !NO + O2+ -> O2 + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2294) = small + (4.500e-10)
    end if

    !NH2 + O2+ -> O2 + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2295) = small + (8.700e-10)
    end if

    !ND2 + O2+ -> O2 + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2296) = small + (8.700e-10)
    end if

    !NHD + O2+ -> O2 + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2297) = small + (8.700e-10)
    end if

    !NO2 + O2+ -> O2 + NO2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2298) = small + (6.600e-10)
    end if

    !O2 + OH+ -> OH + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2299) = small + (5.900e-10)
    end if

    !O2 + OD+ -> OD + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2300) = small + (5.900e-10)
    end if

    !NO + NH2+ -> NH2 + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2301) = small + (9.400e-10)
    end if

    !NO + ND2+ -> ND2 + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2302) = small + (9.400e-10)
    end if

    !NO + NHD+ -> NHD + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2303) = small + (9.400e-10)
    end if

    !HCO + NH2+ -> NH2 + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2304) = small + (4.300e-10)
    end if

    !HCO + ND2+ -> ND2 + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2305) = small + (4.300e-10)
    end if

    !HCO + NHD+ -> NHD + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2306) = small + (4.300e-10)
    end if

    !DCO + NH2+ -> NH2 + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2307) = small + (4.300e-10)
    end if

    !DCO + ND2+ -> ND2 + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2308) = small + (4.300e-10)
    end if

    !DCO + NHD+ -> NHD + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2309) = small + (4.300e-10)
    end if

    !CH + C+ -> C + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2310) = small + (2.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD + C+ -> C + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2311) = small + (2.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !CH2 + C+ -> C + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2312) = small + (4.340e-10&
          *(T32)**(-5.000e-01))
    end if

    !CD2 + C+ -> C + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2313) = small + (4.340e-10&
          *(T32)**(-5.000e-01))
    end if

    !CHD + C+ -> C + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2314) = small + (4.340e-10&
          *(T32)**(-5.000e-01))
    end if

    !HCO + C+ -> C + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2315) = small + (6.700e-10&
          *(T32)**(-5.000e-01))
    end if

    !DCO + C+ -> C + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2316) = small + (6.700e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO + CN+ -> CN + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2317) = small + (8.100e-10)
    end if

    !O2 + CN+ -> CN + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2318) = small + (7.800e-10)
    end if

    !CO2 + CN+ -> CN + CO2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2319) = small + (4.500e-10)
    end if

    !HCN + CN+ -> CN + HCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2320) = small + (2.400e-09)
    end if

    !DCN + CN+ -> CN + DCN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2321) = small + (2.400e-09)
    end if

    !HCO + CN+ -> CN + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2322) = small + (3.700e-10)
    end if

    !DCO + CN+ -> CN + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2323) = small + (3.700e-10)
    end if

    !CH + CO+ -> CO + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2324) = small + (3.200e-10)
    end if

    !CD + CO+ -> CO + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2325) = small + (3.200e-10)
    end if

    !NH + CO+ -> CO + NH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2326) = small + (3.200e-10)
    end if

    !ND + CO+ -> CO + ND+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2327) = small + (3.200e-10)
    end if

    !OH + CO+ -> CO + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2328) = small + (3.100e-10)
    end if

    !OD + CO+ -> CO + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2329) = small + (3.100e-10)
    end if

    !C2H + CO+ -> CO + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2330) = small + (3.900e-10)
    end if

    !C2D + CO+ -> CO + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2331) = small + (3.900e-10)
    end if

    !CH2 + CO+ -> CO + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2332) = small + (4.300e-10)
    end if

    !CD2 + CO+ -> CO + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2333) = small + (4.300e-10)
    end if

    !CHD + CO+ -> CO + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2334) = small + (4.300e-10)
    end if

    !H2O + CO+ -> CO + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2335) = small + (1.700e-09)
    end if

    !D2O + CO+ -> CO + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2336) = small + (1.700e-09)
    end if

    !HDO + CO+ -> CO + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2337) = small + (1.700e-09)
    end if

    !NH2 + CO+ -> CO + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2338) = small + (4.500e-10)
    end if

    !ND2 + CO+ -> CO + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2339) = small + (4.500e-10)
    end if

    !NHD + CO+ -> CO + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2340) = small + (4.500e-10)
    end if

    !C2 + H2+_PARA -> H2_PARA + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2341) = small + (1.100e-09)
    end if

    !C2 + H2+_ORTHO -> H2_ORTHO + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2342) = small + (1.100e-09)
    end if

    !C2 + D2+_PARA -> D2_PARA + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2343) = small + (1.100e-09)
    end if

    !C2 + D2+_ORTHO -> D2_ORTHO + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2344) = small + (1.100e-09)
    end if

    !C2 + HD+ -> HD + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2345) = small + (1.100e-09)
    end if

    !CH + H2+_PARA -> H2_PARA + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2346) = small + (7.100e-10)
    end if

    !CH + H2+_ORTHO -> H2_ORTHO + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2347) = small + (7.100e-10)
    end if

    !CH + D2+_PARA -> D2_PARA + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2348) = small + (7.100e-10)
    end if

    !CH + D2+_ORTHO -> D2_ORTHO + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2349) = small + (7.100e-10)
    end if

    !CH + HD+ -> HD + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2350) = small + (7.100e-10)
    end if

    !CD + H2+_PARA -> H2_PARA + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2351) = small + (7.100e-10)
    end if

    !CD + H2+_ORTHO -> H2_ORTHO + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2352) = small + (7.100e-10)
    end if

    !CD + D2+_PARA -> D2_PARA + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2353) = small + (7.100e-10)
    end if

    !CD + D2+_ORTHO -> D2_ORTHO + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2354) = small + (7.100e-10)
    end if

    !CD + HD+ -> HD + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2355) = small + (7.100e-10)
    end if

    !CN + H2+_PARA -> H2_PARA + CN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2356) = small + (1.200e-09)
    end if

    !CN + H2+_ORTHO -> H2_ORTHO + CN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2357) = small + (1.200e-09)
    end if

    !CN + D2+_PARA -> D2_PARA + CN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2358) = small + (1.200e-09)
    end if

    !CN + D2+_ORTHO -> D2_ORTHO + CN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2359) = small + (1.200e-09)
    end if

    !CN + HD+ -> HD + CN+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2360) = small + (1.200e-09)
    end if

    !CO + H2+_PARA -> H2_PARA + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2361) = small + (6.000e-10)
    end if

    !CO + H2+_ORTHO -> H2_ORTHO + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2362) = small + (6.000e-10)
    end if

    !CO + D2+_PARA -> D2_PARA + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2363) = small + (6.000e-10)
    end if

    !CO + D2+_ORTHO -> D2_ORTHO + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2364) = small + (6.000e-10)
    end if

    !CO + HD+ -> HD + CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2365) = small + (6.000e-10)
    end if

    !CH + N+ -> N + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2366) = small + (3.600e-10)
    end if

    !CD + N+ -> N + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2367) = small + (3.600e-10)
    end if

    !OH + N+ -> N + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2368) = small + (3.700e-10)
    end if

    !OD + N+ -> N + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2369) = small + (3.700e-10)
    end if

    !HCO + O+ -> O + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2370) = small + (4.300e-10)
    end if

    !DCO + O+ -> O + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2371) = small + (4.300e-10)
    end if

    !H2O + HE+ -> HE + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2372) = small + (1.320e-09&
          *(T32)**(-5.000e-01))
    end if

    !D2O + HE+ -> HE + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2373) = small + (1.320e-09&
          *(T32)**(-5.000e-01))
    end if

    !HDO + HE+ -> HE + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2374) = small + (1.320e-09&
          *(T32)**(-5.000e-01))
    end if

    !H + CO2+ -> CO2 + H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2375) = small + (1.000e-10)
    end if

    !D + CO2+ -> CO2 + D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2376) = small + (1.000e-10)
    end if

    !O + CO2+ -> CO2 + O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2377) = small + (9.620e-11)
    end if

    !H2O + CO2+ -> CO2 + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2378) = small + (2.040e-09)
    end if

    !D2O + CO2+ -> CO2 + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2379) = small + (2.040e-09)
    end if

    !HDO + CO2+ -> CO2 + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2380) = small + (2.040e-09)
    end if

    !H2O + HCN+ -> HCN + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2381) = small + (1.800e-09)
    end if

    !H2O + DCN+ -> DCN + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2382) = small + (1.800e-09)
    end if

    !D2O + HCN+ -> HCN + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2383) = small + (1.800e-09)
    end if

    !D2O + DCN+ -> DCN + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2384) = small + (1.800e-09)
    end if

    !HDO + HCN+ -> HCN + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2385) = small + (1.800e-09)
    end if

    !HDO + DCN+ -> DCN + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2386) = small + (1.800e-09)
    end if

    !OH + O+ -> O + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2387) = small + (3.600e-10)
    end if

    !OD + O+ -> O + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2388) = small + (3.600e-10)
    end if

    !NH + H2+_PARA -> H2_PARA + NH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2389) = small + (7.600e-10)
    end if

    !NH + H2+_ORTHO -> H2_ORTHO + NH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2390) = small + (7.600e-10)
    end if

    !NH + D2+_PARA -> D2_PARA + NH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2391) = small + (7.600e-10)
    end if

    !NH + D2+_ORTHO -> D2_ORTHO + NH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2392) = small + (7.600e-10)
    end if

    !NH + HD+ -> HD + NH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2393) = small + (7.600e-10)
    end if

    !ND + H2+_PARA -> H2_PARA + ND+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2394) = small + (7.600e-10)
    end if

    !ND + H2+_ORTHO -> H2_ORTHO + ND+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2395) = small + (7.600e-10)
    end if

    !ND + D2+_PARA -> D2_PARA + ND+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2396) = small + (7.600e-10)
    end if

    !ND + D2+_ORTHO -> D2_ORTHO + ND+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2397) = small + (7.600e-10)
    end if

    !ND + HD+ -> HD + ND+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2398) = small + (7.600e-10)
    end if

    !NO + H2+_PARA -> H2_PARA + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2399) = small + (1.100e-09)
    end if

    !NO + H2+_ORTHO -> H2_ORTHO + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2400) = small + (1.100e-09)
    end if

    !NO + D2+_PARA -> D2_PARA + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2401) = small + (1.100e-09)
    end if

    !NO + D2+_ORTHO -> D2_ORTHO + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2402) = small + (1.100e-09)
    end if

    !NO + HD+ -> HD + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2403) = small + (1.100e-09)
    end if

    !O2 + H2+_PARA -> H2_PARA + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2404) = small + (8.000e-10)
    end if

    !O2 + H2+_ORTHO -> H2_ORTHO + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2405) = small + (8.000e-10)
    end if

    !O2 + D2+_PARA -> D2_PARA + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2406) = small + (8.000e-10)
    end if

    !O2 + D2+_ORTHO -> D2_ORTHO + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2407) = small + (8.000e-10)
    end if

    !O2 + HD+ -> HD + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2408) = small + (8.000e-10)
    end if

    !OH + H2+_PARA -> H2_PARA + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2409) = small + (7.600e-10)
    end if

    !OH + H2+_ORTHO -> H2_ORTHO + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2410) = small + (7.600e-10)
    end if

    !OH + D2+_PARA -> D2_PARA + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2411) = small + (7.600e-10)
    end if

    !OH + D2+_ORTHO -> D2_ORTHO + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2412) = small + (7.600e-10)
    end if

    !OH + HD+ -> HD + OH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2413) = small + (7.600e-10)
    end if

    !OD + H2+_PARA -> H2_PARA + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2414) = small + (7.600e-10)
    end if

    !OD + H2+_ORTHO -> H2_ORTHO + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2415) = small + (7.600e-10)
    end if

    !OD + D2+_PARA -> D2_PARA + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2416) = small + (7.600e-10)
    end if

    !OD + D2+_ORTHO -> D2_ORTHO + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2417) = small + (7.600e-10)
    end if

    !OD + HD+ -> HD + OD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2418) = small + (7.600e-10)
    end if

    !C2H + H2+_PARA -> H2_PARA + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2419) = small + (1.000e-09)
    end if

    !C2H + H2+_ORTHO -> H2_ORTHO + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2420) = small + (1.000e-09)
    end if

    !C2H + D2+_PARA -> D2_PARA + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2421) = small + (1.000e-09)
    end if

    !C2H + D2+_ORTHO -> D2_ORTHO + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2422) = small + (1.000e-09)
    end if

    !C2H + HD+ -> HD + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2423) = small + (1.000e-09)
    end if

    !C2D + H2+_PARA -> H2_PARA + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2424) = small + (1.000e-09)
    end if

    !C2D + H2+_ORTHO -> H2_ORTHO + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2425) = small + (1.000e-09)
    end if

    !C2D + D2+_PARA -> D2_PARA + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2426) = small + (1.000e-09)
    end if

    !C2D + D2+_ORTHO -> D2_ORTHO + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2427) = small + (1.000e-09)
    end if

    !C2D + HD+ -> HD + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2428) = small + (1.000e-09)
    end if

    !CH2 + H2+_PARA -> H2_PARA + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2429) = small + (1.000e-09)
    end if

    !CH2 + H2+_ORTHO -> H2_ORTHO + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2430) = small + (1.000e-09)
    end if

    !CH2 + D2+_PARA -> D2_PARA + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2431) = small + (1.000e-09)
    end if

    !CH2 + D2+_ORTHO -> D2_ORTHO + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2432) = small + (1.000e-09)
    end if

    !CH2 + HD+ -> HD + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2433) = small + (1.000e-09)
    end if

    !CD2 + H2+_PARA -> H2_PARA + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2434) = small + (1.000e-09)
    end if

    !CD2 + H2+_ORTHO -> H2_ORTHO + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2435) = small + (1.000e-09)
    end if

    !CD2 + D2+_PARA -> D2_PARA + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2436) = small + (1.000e-09)
    end if

    !CD2 + D2+_ORTHO -> D2_ORTHO + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2437) = small + (1.000e-09)
    end if

    !CD2 + HD+ -> HD + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2438) = small + (1.000e-09)
    end if

    !CHD + H2+_PARA -> H2_PARA + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2439) = small + (1.000e-09)
    end if

    !CHD + H2+_ORTHO -> H2_ORTHO + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2440) = small + (1.000e-09)
    end if

    !CHD + D2+_PARA -> D2_PARA + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2441) = small + (1.000e-09)
    end if

    !CHD + D2+_ORTHO -> D2_ORTHO + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2442) = small + (1.000e-09)
    end if

    !CHD + HD+ -> HD + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2443) = small + (1.000e-09)
    end if

    !H2O + H2+_PARA -> H2_PARA + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2444) = small + (3.900e-09)
    end if

    !H2O + H2+_ORTHO -> H2_ORTHO + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2445) = small + (3.900e-09)
    end if

    !H2O + D2+_PARA -> D2_PARA + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2446) = small + (3.900e-09)
    end if

    !H2O + D2+_ORTHO -> D2_ORTHO + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2447) = small + (3.900e-09)
    end if

    !H2O + HD+ -> HD + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2448) = small + (3.900e-09)
    end if

    !D2O + H2+_PARA -> H2_PARA + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2449) = small + (3.900e-09)
    end if

    !D2O + H2+_ORTHO -> H2_ORTHO + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2450) = small + (3.900e-09)
    end if

    !D2O + D2+_PARA -> D2_PARA + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2451) = small + (3.900e-09)
    end if

    !D2O + D2+_ORTHO -> D2_ORTHO + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2452) = small + (3.900e-09)
    end if

    !D2O + HD+ -> HD + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2453) = small + (3.900e-09)
    end if

    !HDO + H2+_PARA -> H2_PARA + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2454) = small + (3.900e-09)
    end if

    !HDO + H2+_ORTHO -> H2_ORTHO + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2455) = small + (3.900e-09)
    end if

    !HDO + D2+_PARA -> D2_PARA + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2456) = small + (3.900e-09)
    end if

    !HDO + D2+_ORTHO -> D2_ORTHO + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2457) = small + (3.900e-09)
    end if

    !HDO + HD+ -> HD + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2458) = small + (3.900e-09)
    end if

    !HCO + H2+_PARA -> H2_PARA + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2459) = small + (1.000e-09)
    end if

    !HCO + H2+_ORTHO -> H2_ORTHO + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2460) = small + (1.000e-09)
    end if

    !HCO + D2+_PARA -> D2_PARA + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2461) = small + (1.000e-09)
    end if

    !HCO + D2+_ORTHO -> D2_ORTHO + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2462) = small + (1.000e-09)
    end if

    !HCO + HD+ -> HD + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2463) = small + (1.000e-09)
    end if

    !DCO + H2+_PARA -> H2_PARA + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2464) = small + (1.000e-09)
    end if

    !DCO + H2+_ORTHO -> H2_ORTHO + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2465) = small + (1.000e-09)
    end if

    !DCO + D2+_PARA -> D2_PARA + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2466) = small + (1.000e-09)
    end if

    !DCO + D2+_ORTHO -> D2_ORTHO + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2467) = small + (1.000e-09)
    end if

    !DCO + HD+ -> HD + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2468) = small + (1.000e-09)
    end if

    !C2 + HE+ -> HE + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2469) = small + (5.000e-10)
    end if

    !CH + HE+ -> HE + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2470) = small + (3.830e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD + HE+ -> HE + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2471) = small + (3.830e-09&
          *(T32)**(-5.000e-01))
    end if

    !H2_PARA + HE+ -> HE + H2+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2472) = small + (9.600e-15)
    end if

    !H2_ORTHO + HE+ -> HE + H2+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2473) = small + (9.600e-15)
    end if

    !D2_PARA + HE+ -> HE + D2+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2474) = small + (9.600e-15)
    end if

    !D2_ORTHO + HE+ -> HE + D2+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2475) = small + (9.600e-15)
    end if

    !HD + HE+ -> HE + HD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2476) = small + (9.600e-15)
    end if

    !N2 + HE+ -> HE + N2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2477) = small + (4.000e-10)
    end if

    !O2 + HE+ -> HE + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2478) = small + (3.300e-11)
    end if

    !CO2 + HE+ -> HE + CO2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2479) = small + (1.210e-10)
    end if

    !C2 + H2O+ -> H2O + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2480) = small + (4.700e-10)
    end if

    !C2 + D2O+ -> D2O + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2481) = small + (4.700e-10)
    end if

    !C2 + HDO+ -> HDO + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2482) = small + (4.700e-10)
    end if

    !CH + H2O+ -> H2O + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2483) = small + (3.400e-10)
    end if

    !CH + D2O+ -> D2O + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2484) = small + (3.400e-10)
    end if

    !CH + HDO+ -> HDO + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2485) = small + (3.400e-10)
    end if

    !CD + H2O+ -> H2O + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2486) = small + (3.400e-10)
    end if

    !CD + D2O+ -> D2O + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2487) = small + (3.400e-10)
    end if

    !CD + HDO+ -> HDO + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2488) = small + (3.400e-10)
    end if

    !C2H + H2O+ -> H2O + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2489) = small + (4.400e-10)
    end if

    !C2H + D2O+ -> D2O + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2490) = small + (4.400e-10)
    end if

    !C2H + HDO+ -> HDO + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2491) = small + (4.400e-10)
    end if

    !C2D + H2O+ -> H2O + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2492) = small + (4.400e-10)
    end if

    !C2D + D2O+ -> D2O + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2493) = small + (4.400e-10)
    end if

    !C2D + HDO+ -> HDO + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2494) = small + (4.400e-10)
    end if

    !CH2 + H2O+ -> H2O + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2495) = small + (4.700e-10)
    end if

    !CH2 + D2O+ -> D2O + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2496) = small + (4.700e-10)
    end if

    !CH2 + HDO+ -> HDO + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2497) = small + (4.700e-10)
    end if

    !CD2 + H2O+ -> H2O + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2498) = small + (4.700e-10)
    end if

    !CD2 + D2O+ -> D2O + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2499) = small + (4.700e-10)
    end if

    !CD2 + HDO+ -> HDO + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2500) = small + (4.700e-10)
    end if

    !CHD + H2O+ -> H2O + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2501) = small + (4.700e-10)
    end if

    !CHD + D2O+ -> D2O + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2502) = small + (4.700e-10)
    end if

    !CHD + HDO+ -> HDO + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2503) = small + (4.700e-10)
    end if

    !HCO + H2O+ -> H2O + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2504) = small + (2.800e-10)
    end if

    !HCO + D2O+ -> D2O + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2505) = small + (2.800e-10)
    end if

    !HCO + HDO+ -> HDO + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2506) = small + (2.800e-10)
    end if

    !DCO + H2O+ -> H2O + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2507) = small + (2.800e-10)
    end if

    !DCO + D2O+ -> D2O + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2508) = small + (2.800e-10)
    end if

    !DCO + HDO+ -> HDO + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2509) = small + (2.800e-10)
    end if

    !NH + N+ -> N + NH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2510) = small + (3.700e-10)
    end if

    !ND + N+ -> N + ND+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2511) = small + (3.700e-10)
    end if

    !NH2 + H2O+ -> H2O + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2512) = small + (4.900e-10)
    end if

    !NH2 + D2O+ -> D2O + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2513) = small + (4.900e-10)
    end if

    !NH2 + HDO+ -> HDO + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2514) = small + (4.900e-10)
    end if

    !ND2 + H2O+ -> H2O + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2515) = small + (4.900e-10)
    end if

    !ND2 + D2O+ -> D2O + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2516) = small + (4.900e-10)
    end if

    !ND2 + HDO+ -> HDO + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2517) = small + (4.900e-10)
    end if

    !NHD + H2O+ -> H2O + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2518) = small + (4.900e-10)
    end if

    !NHD + D2O+ -> D2O + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2519) = small + (4.900e-10)
    end if

    !NHD + HDO+ -> HDO + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2520) = small + (4.900e-10)
    end if

    !NO + N+ -> N + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2521) = small + (5.100e-10)
    end if

    !CO2 + N+ -> N + CO2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2522) = small + (1.100e-09)
    end if

    !C2 + O+ -> O + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2523) = small + (4.800e-10)
    end if

    !C2H + H+ -> H + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2524) = small + (3.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !C2H + D+ -> D + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2525) = small + (3.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !C2D + H+ -> H + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2526) = small + (3.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !C2D + D+ -> D + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2527) = small + (3.700e-09&
          *(T32)**(-5.000e-01))
    end if

    !CH2 + H+ -> H + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2528) = small + (1.140e-09&
          *(T32)**(-5.000e-01))
    end if

    !CH2 + D+ -> D + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2529) = small + (1.140e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD2 + H+ -> H + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2530) = small + (1.140e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD2 + D+ -> D + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2531) = small + (1.140e-09&
          *(T32)**(-5.000e-01))
    end if

    !CHD + H+ -> H + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2532) = small + (1.140e-09&
          *(T32)**(-5.000e-01))
    end if

    !CHD + D+ -> D + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2533) = small + (1.140e-09&
          *(T32)**(-5.000e-01))
    end if

    !O2 + N+ -> N + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2534) = small + (4.000e-10)
    end if

    !HCO + N+ -> N + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2535) = small + (4.500e-10)
    end if

    !DCO + N+ -> N + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2536) = small + (4.500e-10)
    end if

    !NO + OH+ -> OH + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2537) = small + (3.600e-10)
    end if

    !NO + OD+ -> OD + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2538) = small + (3.600e-10)
    end if

    !CO2 + H2+_PARA -> H2_PARA + CO2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2539) = small + (1.400e-09)
    end if

    !CO2 + H2+_ORTHO -> H2_ORTHO + CO2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2540) = small + (1.400e-09)
    end if

    !CO2 + D2+_PARA -> D2_PARA + CO2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2541) = small + (1.400e-09)
    end if

    !CO2 + D2+_ORTHO -> D2_ORTHO + CO2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2542) = small + (1.400e-09)
    end if

    !CO2 + HD+ -> HD + CO2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2543) = small + (1.400e-09)
    end if

    !NH2 + C2+ -> C2 + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2544) = small + (4.600e-10)
    end if

    !ND2 + C2+ -> C2 + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2545) = small + (4.600e-10)
    end if

    !NHD + C2+ -> C2 + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2546) = small + (4.600e-10)
    end if

    !CH2 + NH2+ -> NH2 + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2547) = small + (4.900e-10)
    end if

    !CH2 + ND2+ -> ND2 + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2548) = small + (4.900e-10)
    end if

    !CH2 + NHD+ -> NHD + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2549) = small + (4.900e-10)
    end if

    !CD2 + NH2+ -> NH2 + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2550) = small + (4.900e-10)
    end if

    !CD2 + ND2+ -> ND2 + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2551) = small + (4.900e-10)
    end if

    !CD2 + NHD+ -> NHD + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2552) = small + (4.900e-10)
    end if

    !CHD + NH2+ -> NH2 + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2553) = small + (4.900e-10)
    end if

    !CHD + ND2+ -> ND2 + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2554) = small + (4.900e-10)
    end if

    !CHD + NHD+ -> NHD + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2555) = small + (4.900e-10)
    end if

    !NO + NH+ -> NH + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2556) = small + (7.100e-10)
    end if

    !NO + ND+ -> ND + NO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2557) = small + (7.100e-10)
    end if

    !C + O2+ -> O2 + C+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2558) = small + (5.200e-11)
    end if

    !O2 + NH+ -> NH + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2559) = small + (4.500e-10)
    end if

    !O2 + ND+ -> ND + O2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2560) = small + (4.500e-10)
    end if

    !HCO + H+ -> H + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2561) = small + (1.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !HCO + D+ -> D + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2562) = small + (1.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !DCO + H+ -> H + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2563) = small + (1.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !DCO + D+ -> D + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2564) = small + (1.300e-09&
          *(T32)**(-5.000e-01))
    end if

    !CH + O+ -> O + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2565) = small + (3.500e-10)
    end if

    !CD + O+ -> O + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2566) = small + (3.500e-10)
    end if

    !NH + O+ -> O + NH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2567) = small + (3.600e-10)
    end if

    !ND + O+ -> O + ND+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2568) = small + (3.600e-10)
    end if

    !C2H + O+ -> O + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2569) = small + (4.600e-10)
    end if

    !C2D + O+ -> O + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2570) = small + (4.600e-10)
    end if

    !HCO + CH+ -> CH + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2571) = small + (4.600e-10)
    end if

    !HCO + CD+ -> CD + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2572) = small + (4.600e-10)
    end if

    !DCO + CH+ -> CH + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2573) = small + (4.600e-10)
    end if

    !DCO + CD+ -> CD + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2574) = small + (4.600e-10)
    end if

    !HCO + N2+ -> N2 + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2575) = small + (3.700e-10)
    end if

    !DCO + N2+ -> N2 + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2576) = small + (3.700e-10)
    end if

    !C2 + OH+ -> OH + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2577) = small + (4.800e-10)
    end if

    !C2 + OD+ -> OD + C2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2578) = small + (4.800e-10)
    end if

    !CH + OH+ -> OH + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2579) = small + (3.500e-10)
    end if

    !CH + OD+ -> OD + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2580) = small + (3.500e-10)
    end if

    !CD + OH+ -> OH + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2581) = small + (3.500e-10)
    end if

    !CD + OD+ -> OD + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2582) = small + (3.500e-10)
    end if

    !C2H + OH+ -> OH + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2583) = small + (4.500e-10)
    end if

    !C2H + OD+ -> OD + C2H+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2584) = small + (4.500e-10)
    end if

    !C2D + OH+ -> OH + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2585) = small + (4.500e-10)
    end if

    !C2D + OD+ -> OD + C2D+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2586) = small + (4.500e-10)
    end if

    !CH2 + OH+ -> OH + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2587) = small + (4.800e-10)
    end if

    !CH2 + OD+ -> OD + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2588) = small + (4.800e-10)
    end if

    !CD2 + OH+ -> OH + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2589) = small + (4.800e-10)
    end if

    !CD2 + OD+ -> OD + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2590) = small + (4.800e-10)
    end if

    !CHD + OH+ -> OH + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2591) = small + (4.800e-10)
    end if

    !CHD + OD+ -> OD + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2592) = small + (4.800e-10)
    end if

    !H2O + OH+ -> OH + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2593) = small + (1.500e-09)
    end if

    !H2O + OD+ -> OD + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2594) = small + (1.500e-09)
    end if

    !D2O + OH+ -> OH + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2595) = small + (1.500e-09)
    end if

    !D2O + OD+ -> OD + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2596) = small + (1.500e-09)
    end if

    !HDO + OH+ -> OH + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2597) = small + (1.500e-09)
    end if

    !HDO + OD+ -> OD + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2598) = small + (1.500e-09)
    end if

    !HCO + OH+ -> OH + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2599) = small + (2.800e-10)
    end if

    !HCO + OD+ -> OD + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2600) = small + (2.800e-10)
    end if

    !DCO + OH+ -> OH + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2601) = small + (2.800e-10)
    end if

    !DCO + OD+ -> OD + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2602) = small + (2.800e-10)
    end if

    !NH2 + OH+ -> OH + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2603) = small + (5.000e-10)
    end if

    !NH2 + OD+ -> OD + NH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2604) = small + (5.000e-10)
    end if

    !ND2 + OH+ -> OH + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2605) = small + (5.000e-10)
    end if

    !ND2 + OD+ -> OD + ND2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2606) = small + (5.000e-10)
    end if

    !NHD + OH+ -> OH + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2607) = small + (5.000e-10)
    end if

    !NHD + OD+ -> OD + NHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2608) = small + (5.000e-10)
    end if

    !CH + NH2+ -> NH2 + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2609) = small + (3.500e-10)
    end if

    !CH + ND2+ -> ND2 + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2610) = small + (3.500e-10)
    end if

    !CH + NHD+ -> NHD + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2611) = small + (3.500e-10)
    end if

    !CD + NH2+ -> NH2 + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2612) = small + (3.500e-10)
    end if

    !CD + ND2+ -> ND2 + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2613) = small + (3.500e-10)
    end if

    !CD + NHD+ -> NHD + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2614) = small + (3.500e-10)
    end if

    !CH + C2+ -> C2 + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2615) = small + (3.200e-10)
    end if

    !CD + C2+ -> C2 + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2616) = small + (3.200e-10)
    end if

    !CH2 + C2+ -> C2 + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2617) = small + (4.500e-10)
    end if

    !CD2 + C2+ -> C2 + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2618) = small + (4.500e-10)
    end if

    !CHD + C2+ -> C2 + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2619) = small + (4.500e-10)
    end if

    !HCO + C2+ -> C2 + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2620) = small + (3.800e-10)
    end if

    !DCO + C2+ -> C2 + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2621) = small + (3.800e-10)
    end if

    !O + N2+ -> N2 + O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2622) = small + (1.000e-11)
    end if

    !H2O + N2+ -> N2 + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2623) = small + (2.200e-09)
    end if

    !D2O + N2+ -> N2 + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2624) = small + (2.200e-09)
    end if

    !HDO + N2+ -> N2 + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2625) = small + (2.200e-09)
    end if

    !H2O + NH+ -> NH + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2626) = small + (1.050e-09)
    end if

    !H2O + ND+ -> ND + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2627) = small + (1.050e-09)
    end if

    !D2O + NH+ -> NH + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2628) = small + (1.050e-09)
    end if

    !D2O + ND+ -> ND + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2629) = small + (1.050e-09)
    end if

    !HDO + NH+ -> NH + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2630) = small + (1.050e-09)
    end if

    !HDO + ND+ -> ND + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2631) = small + (1.050e-09)
    end if

    !CH + O2+ -> O2 + CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2632) = small + (3.100e-10)
    end if

    !CD + O2+ -> O2 + CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2633) = small + (3.100e-10)
    end if

    !CH2 + O2+ -> O2 + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2634) = small + (4.300e-10)
    end if

    !CD2 + O2+ -> O2 + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2635) = small + (4.300e-10)
    end if

    !CHD + O2+ -> O2 + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2636) = small + (4.300e-10)
    end if

    !HCO + O2+ -> O2 + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2637) = small + (3.600e-10)
    end if

    !DCO + O2+ -> O2 + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2638) = small + (3.600e-10)
    end if

    !H + C+ -> CH+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2639) = small + (1.700e-17)
    end if

    !D + C+ -> CD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2640) = small + (1.700e-17)
    end if

    !O + C+ -> CO+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2641) = small + (2.500e-18)
    end if

    !H2_PARA + C+ -> CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2642) = small + (4.000e-16&
          *(T32)**(-2.000e-01))
    end if

    !H2_ORTHO + C+ -> CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2643) = small + (4.000e-16&
          *(T32)**(-2.000e-01))
    end if

    !D2_PARA + C+ -> CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2644) = small + (4.000e-16&
          *(T32)**(-2.000e-01))
    end if

    !D2_ORTHO + C+ -> CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2645) = small + (4.000e-16&
          *(T32)**(-2.000e-01))
    end if

    !HD + C+ -> CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2646) = small + (4.000e-16&
          *(T32)**(-2.000e-01))
    end if

    !H + H+ -> H2+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2647) = small + (1.000e-20&
          *(T32)**(+1.000e+00))
    end if

    !H + H+ -> H2+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2648) = small + (1.000e-20&
          *(T32)**(+1.000e+00))
    end if

    !H + D+ -> HD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2649) = small + (2.000e-20&
          *(T32)**(+1.000e+00))
    end if

    !D + H+ -> HD+
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2650) = small + (2.000e-20&
          *(T32)**(+1.000e+00))
    end if

    !D + D+ -> D2+_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2651) = small + (2.000e-20&
          *(T32)**(+1.000e+00))
    end if

    !D + D+ -> D2+_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2652) = small + (2.000e-20&
          *(T32)**(+1.000e+00))
    end if

    !C + C -> C2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2653) = small + (1.000e-17)
    end if

    !C + H -> CH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2654) = small + (1.000e-17)
    end if

    !C + D -> CD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2655) = small + (1.000e-17)
    end if

    !C + N -> CN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2656) = small + (1.000e-17)
    end if

    !C + O -> CO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2657) = small + (2.100e-19)
    end if

    !C + H2_PARA -> CH2
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(2658) = small + (2.000e-20&
          *(T32)**(-1.000e+00))
    end if

    !C + H2_ORTHO -> CH2
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(2659) = small + (2.000e-20&
          *(T32)**(-1.000e+00))
    end if

    !C + D2_PARA -> CD2
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(2660) = small + (2.000e-20&
          *(T32)**(-1.000e+00))
    end if

    !C + D2_ORTHO -> CD2
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(2661) = small + (2.000e-20&
          *(T32)**(-1.000e+00))
    end if

    !C + HD -> CHD
    if(Tgas.GE.10d0 .and. Tgas.LT.298d0) then
      k(2662) = small + (2.000e-20&
          *(T32)**(-1.000e+00))
    end if

    !H + O -> OH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2663) = small + (9.900e-19&
          *(T32)**(-3.800e-01))
    end if

    !D + O -> OD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2664) = small + (9.900e-19&
          *(T32)**(-3.800e-01))
    end if

    !O + O -> O2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2665) = small + (4.900e-20&
          *(T32)**(+1.580e+00))
    end if

    !C + C2 -> C3
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2666) = small + (3.300e-16&
          *(T32)**(-1.000e+00))
    end if

    !H + OH -> H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2667) = small + (4.000e-18&
          *(T32)**(-2.000e+00))
    end if

    !H + OD -> HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2668) = small + (4.000e-18&
          *(T32)**(-2.000e+00))
    end if

    !D + OH -> HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2669) = small + (4.000e-18&
          *(T32)**(-2.000e+00))
    end if

    !D + OD -> D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2670) = small + (4.000e-18&
          *(T32)**(-2.000e+00))
    end if

    !C + C- -> C2 + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2671) = small + (5.000e-10)
    end if

    !H + C- -> CH + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2672) = small + (5.000e-10)
    end if

    !D + C- -> CD + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2673) = small + (5.000e-10)
    end if

    !N + C- -> CN + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2674) = small + (5.000e-10)
    end if

    !O + C- -> CO + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2675) = small + (5.000e-10)
    end if

    !CH + C- -> C2H + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2676) = small + (5.000e-10)
    end if

    !CD + C- -> C2D + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2677) = small + (5.000e-10)
    end if

    !H2_PARA + C- -> CH2 + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2678) = small + (1.000e-13)
    end if

    !H2_ORTHO + C- -> CH2 + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2679) = small + (1.000e-13)
    end if

    !D2_PARA + C- -> CD2 + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2680) = small + (1.000e-13)
    end if

    !D2_ORTHO + C- -> CD2 + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2681) = small + (1.000e-13)
    end if

    !HD + C- -> CHD + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2682) = small + (1.000e-13)
    end if

    !NH + C- -> HCN + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2683) = small + (5.000e-10)
    end if

    !ND + C- -> DCN + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2684) = small + (5.000e-10)
    end if

    !O2 + C- -> CO2 + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2685) = small + (5.000e-11)
    end if

    !OH + C- -> HCO + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2686) = small + (5.000e-10)
    end if

    !OD + C- -> DCO + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2687) = small + (5.000e-10)
    end if

    !C + H- -> CH + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2688) = small + (1.000e-09)
    end if

    !C + D- -> CD + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2689) = small + (1.000e-09)
    end if

    !H + H- -> H2_PARA + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2690) = small + (1.300e-09)
    end if

    !H + D- -> HD + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2691) = small + (1.300e-09)
    end if

    !D + H- -> HD + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2692) = small + (1.300e-09)
    end if

    !D + D- -> D2_PARA + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2693) = small + (1.300e-09)
    end if

    !N + H- -> NH + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2694) = small + (1.000e-09)
    end if

    !N + D- -> ND + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2695) = small + (1.000e-09)
    end if

    !O + H- -> OH + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2696) = small + (1.000e-09)
    end if

    !O + D- -> OD + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2697) = small + (1.000e-09)
    end if

    !C2 + H- -> C2H + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2698) = small + (1.000e-09)
    end if

    !C2 + D- -> C2D + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2699) = small + (1.000e-09)
    end if

    !CH + H- -> CH2 + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2700) = small + (1.000e-10)
    end if

    !CH + D- -> CHD + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2701) = small + (1.000e-10)
    end if

    !CD + H- -> CHD + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2702) = small + (1.000e-10)
    end if

    !CD + D- -> CD2 + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2703) = small + (1.000e-10)
    end if

    !CN + H- -> HCN + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2704) = small + (1.000e-10)
    end if

    !CN + D- -> DCN + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2705) = small + (1.000e-10)
    end if

    !CO + H- -> HCO + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2706) = small + (5.000e-11)
    end if

    !CO + D- -> DCO + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2707) = small + (5.000e-11)
    end if

    !NH + H- -> NH2 + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2708) = small + (1.000e-10)
    end if

    !NH + D- -> NHD + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2709) = small + (1.000e-10)
    end if

    !ND + H- -> NHD + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2710) = small + (1.000e-10)
    end if

    !ND + D- -> ND2 + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2711) = small + (1.000e-10)
    end if

    !OH + H- -> H2O + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2712) = small + (1.000e-10)
    end if

    !OH + D- -> HDO + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2713) = small + (1.000e-10)
    end if

    !OD + H- -> HDO + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2714) = small + (1.000e-10)
    end if

    !OD + D- -> D2O + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2715) = small + (1.000e-10)
    end if

    !C + O- -> CO + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2716) = small + (5.000e-10)
    end if

    !H + O- -> OH + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2717) = small + (5.000e-10)
    end if

    !D + O- -> OD + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2718) = small + (5.000e-10)
    end if

    !N + O- -> NO + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2719) = small + (2.200e-10)
    end if

    !O + O- -> O2 + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2720) = small + (1.900e-10)
    end if

    !CH + O- -> HCO + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2721) = small + (5.000e-10)
    end if

    !CD + O- -> DCO + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2722) = small + (5.000e-10)
    end if

    !CO + O- -> CO2 + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2723) = small + (6.500e-10)
    end if

    !H2_PARA + O- -> H2O + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2724) = small + (7.000e-10)
    end if

    !H2_ORTHO + O- -> H2O + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2725) = small + (7.000e-10)
    end if

    !D2_PARA + O- -> D2O + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2726) = small + (7.000e-10)
    end if

    !D2_ORTHO + O- -> D2O + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2727) = small + (7.000e-10)
    end if

    !HD + O- -> HDO + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2728) = small + (7.000e-10)
    end if

    !H + CN- -> HCN + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2729) = small + (1.300e-09)
    end if

    !D + CN- -> DCN + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2730) = small + (1.300e-09)
    end if

    !C + OH- -> HCO + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2731) = small + (5.000e-10)
    end if

    !C + OD- -> DCO + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2732) = small + (5.000e-10)
    end if

    !H + OH- -> H2O + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2733) = small + (1.400e-09)
    end if

    !H + OD- -> HDO + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2734) = small + (1.400e-09)
    end if

    !D + OH- -> HDO + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2735) = small + (1.400e-09)
    end if

    !D + OD- -> D2O + E
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2736) = small + (1.400e-09)
    end if

    !C + E -> C-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2737) = small + (3.000e-15)
    end if

    !H + E -> H-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2738) = small + (3.000e-16&
          *(T32)**(+1.000e+00))
    end if

    !D + E -> D-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2739) = small + (3.000e-16&
          *(T32)**(+1.000e+00))
    end if

    !O + E -> O-
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2740) = small + (1.500e-15)
    end if

    !C2+ + E -> C + C
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2741) = small + (8.840e-08&
          *(T32)**(-5.000e-01))
    end if

    !CH+ + E -> C + H
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2742) = small + (7.000e-08&
          *(T32)**(-5.000e-01))
    end if

    !CD+ + E -> C + D
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2743) = small + (7.000e-08&
          *(T32)**(-5.000e-01))
    end if

    !CN+ + E -> C + N
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2744) = small + (3.380e-07&
          *(T32)**(-5.500e-01))
    end if

    !CO+ + E -> C + O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2745) = small + (2.750e-07&
          *(T32)**(-5.500e-01))
    end if

    !H2+_PARA + E -> H + H
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2746) = small + (2.530e-07&
          *(T32)**(-5.000e-01))
    end if

    !H2+_ORTHO + E -> H + H
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2747) = small + (2.530e-07&
          *(T32)**(-5.000e-01))
    end if

    !D2+_PARA + E -> D + D
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2748) = small + (2.530e-07&
          *(T32)**(-5.000e-01))
    end if

    !D2+_ORTHO + E -> D + D
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2749) = small + (2.530e-07&
          *(T32)**(-5.000e-01))
    end if

    !HD+ + E -> H + D
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2750) = small + (2.530e-07&
          *(T32)**(-5.000e-01))
    end if

    !H2+_PARA + E -> H2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2751) = small + (2.250e-07&
          *(T32)**(-4.000e-01))
    end if

    !H2+_ORTHO + E -> H2_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2752) = small + (2.250e-07&
          *(T32)**(-4.000e-01))
    end if

    !D2+_PARA + E -> D2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2753) = small + (2.250e-07&
          *(T32)**(-4.000e-01))
    end if

    !D2+_PARA + E -> D2_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2754) = small + (2.250e-07&
          *(T32)**(-4.000e-01))
    end if

    !D2+_ORTHO + E -> D2_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2755) = small + (2.250e-07&
          *(T32)**(-4.000e-01))
    end if

    !HD+ + E -> HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2756) = small + (2.250e-07&
          *(T32)**(-4.000e-01))
    end if

    !HEH+ + E -> H + HE
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2757) = small + (3.000e-08&
          *(T32)**(-5.000e-01))
    end if

    !HED+ + E -> D + HE
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2758) = small + (3.000e-08&
          *(T32)**(-5.000e-01))
    end if

    !N2+ + E -> N + N
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2759) = small + (1.800e-07&
          *(T32)**(-3.900e-01))
    end if

    !NH+ + E -> H + N
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2760) = small + (1.180e-07&
          *(T32)**(-5.000e-01))
    end if

    !ND+ + E -> D + N
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2761) = small + (1.180e-07&
          *(T32)**(-5.000e-01))
    end if

    !NO+ + E -> N + O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2762) = small + (4.100e-07&
          *(T32)**(-1.000e+00))
    end if

    !O2+ + E -> O + O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2763) = small + (1.950e-07&
          *(T32)**(-7.000e-01))
    end if

    !OH+ + E -> H + O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2764) = small + (6.300e-09&
          *(T32)**(-4.800e-01))
    end if

    !OD+ + E -> D + O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2765) = small + (6.300e-09&
          *(T32)**(-4.800e-01))
    end if

    !C2H+ + E -> H + C2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2766) = small + (1.160e-07&
          *(T32)**(-7.600e-01))
    end if

    !C2D+ + E -> D + C2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2767) = small + (1.160e-07&
          *(T32)**(-7.600e-01))
    end if

    !C2H+ + E -> C + CH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2768) = small + (1.050e-07&
          *(T32)**(-7.600e-01))
    end if

    !C2D+ + E -> C + CD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2769) = small + (1.050e-07&
          *(T32)**(-7.600e-01))
    end if

    !C2H+ + E -> C + C + H
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2770) = small + (4.800e-08&
          *(T32)**(-7.600e-01))
    end if

    !C2D+ + E -> C + C + D
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2771) = small + (4.800e-08&
          *(T32)**(-7.600e-01))
    end if

    !C2N+ + E -> C + CN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2772) = small + (1.500e-07&
          *(T32)**(-5.000e-01))
    end if

    !C2N+ + E -> N + C2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2773) = small + (1.500e-07&
          *(T32)**(-5.000e-01))
    end if

    !C2O+ + E -> C + CO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2774) = small + (3.000e-07&
          *(T32)**(-5.000e-01))
    end if

    !C3+ + E -> C + C2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2775) = small + (3.000e-07&
          *(T32)**(-5.000e-01))
    end if

    !CH2+ + E -> C + H2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2776) = small + (7.700e-08&
          *(T32)**(-6.000e-01))
    end if

    !CD2+ + E -> C + D2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2777) = small + (7.700e-08&
          *(T32)**(-6.000e-01))
    end if

    !CHD+ + E -> C + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2778) = small + (7.700e-08&
          *(T32)**(-6.000e-01))
    end if

    !CH2+ + E -> H + CH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2779) = small + (1.600e-07&
          *(T32)**(-6.000e-01))
    end if

    !CD2+ + E -> D + CD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2780) = small + (1.600e-07&
          *(T32)**(-6.000e-01))
    end if

    !CHD+ + E -> H + CD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2781) = small + (1.600e-07&
          *(T32)**(-6.000e-01))
    end if

    !CHD+ + E -> D + CH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2782) = small + (1.600e-07&
          *(T32)**(-6.000e-01))
    end if

    !CH2+ + E -> C + H + H
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2783) = small + (4.000e-07&
          *(T32)**(-6.000e-01))
    end if

    !CD2+ + E -> C + D + D
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2784) = small + (4.000e-07&
          *(T32)**(-6.000e-01))
    end if

    !CHD+ + E -> C + H + D
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2785) = small + (4.000e-07&
          *(T32)**(-6.000e-01))
    end if

    !CNC+ + E -> C + CN
    if(Tgas.GE.10d0 .and. Tgas.LT.1000d0) then
      k(2786) = small + (3.800e-07&
          *(T32)**(-6.000e-01))
    end if

    !CNC+ + E -> N + C2
    if(Tgas.GE.10d0 .and. Tgas.LT.1000d0) then
      k(2787) = small + (2.000e-08&
          *(T32)**(-6.000e-01))
    end if

    !CO2+ + E -> O + CO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2788) = small + (4.200e-07&
          *(T32)**(-7.500e-01))
    end if

    !H2O+ + E -> O + H2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2789) = small + (3.900e-08&
          *(T32)**(-5.000e-01))
    end if

    !D2O+ + E -> O + D2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2790) = small + (3.900e-08&
          *(T32)**(-5.000e-01))
    end if

    !HDO+ + E -> O + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2791) = small + (3.900e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2O+ + E -> H + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2792) = small + (8.600e-08&
          *(T32)**(-5.000e-01))
    end if

    !D2O+ + E -> D + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2793) = small + (8.600e-08&
          *(T32)**(-5.000e-01))
    end if

    !HDO+ + E -> H + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2794) = small + (8.600e-08&
          *(T32)**(-5.000e-01))
    end if

    !HDO+ + E -> D + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2795) = small + (8.600e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2O+ + E -> H + H + O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2796) = small + (3.050e-07&
          *(T32)**(-5.000e-01))
    end if

    !D2O+ + E -> D + D + O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2797) = small + (3.050e-07&
          *(T32)**(-5.000e-01))
    end if

    !HDO+ + E -> H + D + O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2798) = small + (3.050e-07&
          *(T32)**(-5.000e-01))
    end if

    !H3+_ORTHO + E -> H + H + H
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2799) = small + (4.870E-08&
          *(T32)**(1.600E-01)*exp(+1.010E+00*invT))
    end if

    !H3+_PARA + E -> H + H + H
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2800) = small + (3.560E-08&
          *(T32)**(-7.300E-01)*exp(-9.800E-01*invT))
    end if

    !D3+_META + E -> D + D + D
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2801) = small + (2.160E-08&
          *(T32)**(-5.000E-01))
    end if

    !D3+_ORTHO + E -> D + D + D
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2802) = small + (2.160E-08&
          *(T32)**(-5.000E-01))
    end if

    !H2D+_ORTHO + E -> H + H + D
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2803) = small + (4.380E-08&
          *(T32)**(-5.000E-01))
    end if

    !H2D+_PARA + E -> H + H + D
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2804) = small + (4.380E-08&
          *(T32)**(-5.000E-01))
    end if

    !D2H+_ORTHO + E -> H + D + D
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2805) = small + (4.380E-08&
          *(T32)**(-5.000E-01))
    end if

    !D2H+_PARA + E -> H + D + D
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2806) = small + (4.380E-08&
          *(T32)**(-5.000E-01))
    end if

    !H3+_ORTHO + E -> H2_ORTHO + H
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2807) = small + (2.510E-08&
          *(T32)**(1.600E-01)*exp(+1.010E+00*invT))
    end if

    !H3+_PARA + E -> H2_ORTHO + H
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2808) = small + (9.200E-09&
          *(T32)**(-7.300E-01)*exp(-9.800E-01*invT))
    end if

    !H3+_PARA + E -> H2_PARA + H
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2809) = small + (9.200E-09&
          *(T32)**(-7.300E-01)*exp(-9.800E-01*invT))
    end if

    !D3+_META + E -> D2_ORTHO + D
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2810) = small + (5.400E-09&
          *(T32)**(-5.000E-01))
    end if

    !D3+_ORTHO + E -> D2_PARA + D
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2811) = small + (2.700E-09&
          *(T32)**(-5.000E-01))
    end if

    !D3+_ORTHO + E -> D2_ORTHO + D
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2812) = small + (2.700E-09&
          *(T32)**(-5.000E-01))
    end if

    !H2D+_ORTHO + E -> HD + H
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2813) = small + (1.200E-08&
          *(T32)**(-5.000E-01))
    end if

    !H2D+_PARA + E -> HD + H
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2814) = small + (1.200E-08&
          *(T32)**(-5.000E-01))
    end if

    !H2D+_ORTHO + E -> H2_ORTHO + D
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2815) = small + (4.200E-09&
          *(T32)**(-5.000E-01))
    end if

    !H2D+_PARA + E -> H2_PARA + D
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2816) = small + (4.200E-09&
          *(T32)**(-5.000E-01))
    end if

    !D2H+_ORTHO + E -> D2_ORTHO + H
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2817) = small + (1.200E-08&
          *(T32)**(-5.000E-01))
    end if

    !D2H+_PARA + E -> D2_PARA + H
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2818) = small + (1.200E-08&
          *(T32)**(-5.000E-01))
    end if

    !D2H+_ORTHO + E -> HD + D
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2819) = small + (4.200E-09&
          *(T32)**(-5.000E-01))
    end if

    !D2H+_PARA + E -> HD + D
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2820) = small + (4.200E-09&
          *(T32)**(-5.000E-01))
    end if

    !HCN+ + E -> H + CN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2821) = small + (2.000e-07&
          *(T32)**(-5.000e-01))
    end if

    !DCN+ + E -> D + CN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2822) = small + (2.000e-07&
          *(T32)**(-5.000e-01))
    end if

    !HCO+ + E -> H + CO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2823) = small + (2.800e-07&
          *(T32)**(-6.900e-01))
    end if

    !DCO+ + E -> D + CO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2824) = small + (2.800e-07&
          *(T32)**(-6.900e-01))
    end if

    !HNC+ + E -> H + CN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2825) = small + (2.000e-07&
          *(T32)**(-5.000e-01))
    end if

    !DNC+ + E -> D + CN
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2826) = small + (2.000e-07&
          *(T32)**(-5.000e-01))
    end if

    !HNO+ + E -> H + NO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2827) = small + (3.000e-07&
          *(T32)**(-5.000e-01))
    end if

    !DNO+ + E -> D + NO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2828) = small + (3.000e-07&
          *(T32)**(-5.000e-01))
    end if

    !HOC+ + E -> H + CO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2829) = small + (2.000e-07&
          *(T32)**(-7.500e-01))
    end if

    !DOC+ + E -> D + CO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2830) = small + (2.000e-07&
          *(T32)**(-7.500e-01))
    end if

    !N2H+ + E -> H + N2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2831) = small + (9.000e-08&
          *(T32)**(-5.100e-01))
    end if

    !N2D+ + E -> D + N2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2832) = small + (9.000e-08&
          *(T32)**(-5.100e-01))
    end if

    !N2H+ + E -> N + NH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2833) = small + (1.000e-08&
          *(T32)**(-5.100e-01))
    end if

    !N2D+ + E -> N + ND
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2834) = small + (1.000e-08&
          *(T32)**(-5.100e-01))
    end if

    !NCO+ + E -> N + CO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2835) = small + (3.000e-07&
          *(T32)**(-5.000e-01))
    end if

    !NH2+ + E -> H + H + N
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2836) = small + (2.000e-07&
          *(T32)**(-5.000e-01))
    end if

    !ND2+ + E -> D + D + N
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2837) = small + (2.000e-07&
          *(T32)**(-5.000e-01))
    end if

    !NHD+ + E -> H + D + N
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2838) = small + (2.000e-07&
          *(T32)**(-5.000e-01))
    end if

    !NH2+ + E -> H + NH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2839) = small + (1.000e-07&
          *(T32)**(-5.000e-01))
    end if

    !ND2+ + E -> D + ND
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2840) = small + (1.000e-07&
          *(T32)**(-5.000e-01))
    end if

    !NHD+ + E -> H + ND
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2841) = small + (1.000e-07&
          *(T32)**(-5.000e-01))
    end if

    !NHD+ + E -> D + NH
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2842) = small + (1.000e-07&
          *(T32)**(-5.000e-01))
    end if

    !NO2+ + E -> O + NO
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2843) = small + (3.000e-07&
          *(T32)**(-5.000e-01))
    end if

    !O2H+ + E -> H + O2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2844) = small + (3.000e-07&
          *(T32)**(-5.000e-01))
    end if

    !O2D+ + E -> D + O2
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2845) = small + (3.000e-07&
          *(T32)**(-5.000e-01))
    end if

    !C+ + E -> C
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2846) = small + (4.400e-12&
          *(T32)**(-6.100e-01))
    end if

    !H+ + E -> H
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2847) = small + (3.500e-12&
          *(T32)**(-7.000e-01))
    end if

    !D+ + E -> D
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2848) = small + (3.500e-12&
          *(T32)**(-7.000e-01))
    end if

    !HE+ + E -> HE
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2849) = small + (4.500e-12&
          *(T32)**(-6.700e-01))
    end if

    !N+ + E -> N
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2850) = small + (3.800e-12&
          *(T32)**(-6.200e-01))
    end if

    !O+ + E -> O
    if(Tgas.GE.10d0 .and. Tgas.LT.280d0) then
      k(2851) = small + (3.400e-12&
          *(T32)**(-6.300e-01))
    end if

    !H+ + D2_PARA -> D+ + HD
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2852) = small + (2.100e-09&
          *exp(-4.050e+02*invT))
    end if

    !H+ + D2_ORTHO -> D+ + HD
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2853) = small + (2.100e-09&
          *exp(-4.910e+02*invT))
    end if

    !D+ + HD -> H+ + D2_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2854) = small + (0.500e-09)
    end if

    !D+ + HD -> H+ + D2_ORTHO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2855) = small + (0.500e-09)
    end if

    !H2D+_PARA + D -> D2H+_PARA + H
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2856) = small + (0.500e-09)
    end if

    !H2D+_PARA + D -> D2H+_ORTHO + H
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2857) = small + (0.500e-09)
    end if

    !H2D+_ORTHO + D -> D2H+_PARA + H
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2858) = small + (0.500e-09)
    end if

    !H2D+_ORTHO + D -> D2H+_ORTHO + H
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2859) = small + (0.500e-09)
    end if

    !D2H+_PARA + H -> H2D+_PARA + D
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2860) = small + (1.000e-09)
    end if

    !D2H+_ORTHO + H -> H2D+_PARA + D
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2861) = small + (1.000e-09&
          *exp(-6.000e+02*invT))
    end if

    !D2H+_PARA + H -> H2D+_ORTHO + D
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2862) = small + (1.000e-09&
          *exp(-4.633e+02*invT))
    end if

    !D2H+_ORTHO + H -> H2D+_ORTHO + D
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2863) = small + (1.000e-09&
          *exp(-5.135e+02*invT))
    end if

    !HD+ + D -> D2+_PARA + H
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2864) = small + (0.500e-09)
    end if

    !HD+ + D -> D2+_ORTHO + H
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2865) = small + (0.500e-09)
    end if

    !D2+_PARA + H -> HD+ + D
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2866) = small + (1.000e-09&
          *exp(-4.300e+02*invT))
    end if

    !D2+_ORTHO + H -> HD+ + D
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2867) = small + (1.000e-09&
          *exp(-4.720e+02*invT))
    end if

    !D2H+_ORTHO + D -> D3+_ORTHO + H
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2868) = small + (0.500e-09)
    end if

    !D2H+_ORTHO + D -> D3+_META + H
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2869) = small + (0.500e-09)
    end if

    !D2H+_PARA + D -> D3+_ORTHO + H
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2870) = small + (1.000e-09)
    end if

    !D3+_ORTHO + H -> D2H+_PARA + D
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2871) = small + (1.000e-09&
          *exp(-6.600e+02*invT))
    end if

    !D3+_META + H -> D2H+_ORTHO + D
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2872) = small + (1.000e-09&
          *exp(-6.550e+02*invT))
    end if

    !D+ + D2_ORTHO -> D+ + D2_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2873) = small + (1.980e-09&
          *exp(-8.600e+01*invT))
    end if

    !D+ + D2_PARA -> D+ + D2_ORTHO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2874) = small + (1.320e-09)
    end if

    !H+ + H2_ORTHO -> H+ + H2_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2875) = small + (2.200e-10)
    end if

    !H+ + H2_PARA -> H+ + H2_ORTHO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2876) = small + (1.980e-09&
          *exp(-1.705e+02*invT))
    end if

    !H2+_ORTHO + H2_ORTHO -> H3+_PARA + H
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2877) = small + (7.000e-10)
    end if

    !H2+_PARA + H2_ORTHO -> H3+_PARA + H
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2878) = small + (1.400e-09)
    end if

    !H2+_ORTHO + H2_PARA -> H3+_PARA + H
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2879) = small + (1.400e-09)
    end if

    !H2+_PARA + H2_PARA -> H3+_PARA + H
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2880) = small + (2.100e-09)
    end if

    !D+ + H -> H+ + D
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2881) = small + (1.000e-09)
    end if

    !H+ + D -> D+ + H
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2882) = small + (1.000e-09&
          *exp(-4.100e+01*invT))
    end if

    !D+ + H2_PARA -> H+ + HD
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2883) = small + (2.100e-09)
    end if

    !D+ + H2_ORTHO -> H+ + HD
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2884) = small + (2.100e-09)
    end if

    !H+ + HD -> D+ + H2_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2885) = small + (1.000e-09&
          *exp(-4.640e+02*invT))
    end if

    !H+ + HD -> D+ + H2_ORTHO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2886) = small + (1.000e-09&
          *exp(-6.345e+02*invT))
    end if

    !HD+ + H2_PARA -> H3+_PARA + D
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2887) = small + (1.050e-09)
    end if

    !HD+ + H2_ORTHO -> H3+_PARA + D
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2888) = small + (5.250e-09)
    end if

    !H2+_PARA + HD -> H3+_PARA + D
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2889) = small + (1.050e-09)
    end if

    !H2+_ORTHO + HD -> H3+_PARA + D
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2890) = small + (5.250e-10)
    end if

    !H3+_PARA + D -> H2D+_PARA + H
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2891) = small + (0.660e-09)
    end if

    !H3+_PARA + D -> H2D+_ORTHO + H
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2892) = small + (0.330e-09)
    end if

    !H3+_ORTHO + D -> H2D+_ORTHO + H
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2893) = small + (1.000e-09)
    end if

    !H2D+_PARA + H -> H3+_PARA + D
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2894) = small + (1.000e-09&
          *exp(-6.320e+02*invT))
    end if

    !H2D+_ORTHO + H -> H3+_ORTHO + D
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2895) = small + (0.500e-09&
          *exp(-5.784e+02*invT))
    end if

    !H2D+_ORTHO + H -> H3+_PARA + D
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2896) = small + (0.500e-09&
          *exp(-5.455e+02*invT))
    end if

    !H2+_PARA + D -> H2D+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2897) = small + (7.000e-18&
          *(T32)**(+1.800e+00)*exp(-1.000e-99*invT))
    end if

    !HD+ + H -> H2D+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2898) = small + (1.200e-17&
          *(T32)**(+1.800e+00)*exp(-1.000e-99*invT))
    end if

    !HD+ + H -> H2+_PARA + D
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2899) = small + (1.000e-09&
          *exp(-1.540e+02*invT))
    end if

    !H2+_PARA + D -> HD+ + H
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2900) = small + (1.000e-09)
    end if

    !D2H+_PARA + HD -> D3+_PARA + H2_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2901) = small + (1.450e-11&
          *exp(+6.900e-01*invT))
    end if

    !D2H+_PARA + HD -> D3+_PARA + H2_ORTHO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2902) = small + (4.090e-11&
          *exp(+7.100e-01*invT))
    end if

    !D3+_PARA + H2_ORTHO -> D2H+_PARA + HD
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2903) = small + (1.690e-09&
          *exp(-5.230e+01*invT))
    end if

    !H2D+_ORTHO + D2_PARA -> D3+_PARA + H2_ORTHO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2904) = small + (1.480e-11&
          *exp(+6.200e-01*invT))
    end if

    !H2D+_PARA + D2_PARA -> D3+_PARA + H2_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2905) = small + (1.690e-11&
          *exp(+6.400e-01*invT))
    end if

    !D2H+_ORTHO + D2_PARA -> D3+_PARA + HD
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2906) = small + (6.870e-11&
          *exp(+8.500e-01*invT))
    end if

    !D2H+_PARA + D2_ORTHO -> D3+_PARA + HD
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2907) = small + (7.440e-11&
          *exp(-1.100e-01*invT))
    end if

    !D2H+_PARA + D2_PARA -> D3+_PARA + HD
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2908) = small + (8.940e-11&
          *exp(+1.000e+00*invT))
    end if

    !D3+_META + D2_PARA -> D3+_PARA + D2_ORTHO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2909) = small + (8.370e-11&
          *exp(+6.100e-01*invT))
    end if

    !D3+_ORTHO + D2_PARA -> D3+_PARA + D2_ORTHO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2910) = small + (4.490e-11&
          *exp(+2.300e-01*invT))
    end if

    !D3+_PARA + D2_ORTHO -> D3+_META + D2_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2911) = small + (4.000e-10&
          *exp(-2.170e+01*invT))
    end if

    !D3+_PARA + D2_ORTHO -> D3+_ORTHO + D2_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2912) = small + (5.340e-10&
          *exp(-6.890e+01*invT))
    end if

    !D3+_PARA + CO -> DCO+ + D2_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2913) = small + (6.000e-10)
    end if

    !D3+_PARA + N2 -> N2D+ + D2_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2914) = small + (6.500e-10)
    end if

    !D3+_PARA + E -> D + D + D
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2915) = small + (2.160E-08&
          *(T32)**(-5.000E-01))
    end if

    !D3+_PARA + E -> D2_PARA + D
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2916) = small + (5.400E-09&
          *(T32)**(-5.000E-01))
    end if

    !D3+_PARA + GRAIN- -> D + D + D + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2917) = small + (2.070e-16&
          *user_GtoDN)
    end if

    !D3+_PARA + GRAIN- -> D2_PARA + D + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2918) = small + (1.048e-16&
          *user_GtoDN)
    end if

    !C+ + GRAIN- -> C + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2919) = small + (4.900e-17&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !N+ + GRAIN- -> N + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2920) = small + (4.700e-17&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !O+ + GRAIN- -> O + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2921) = small + (4.400e-17&
          *(T32)**(+5.000e-01)*user_GtoDN)
    end if

    !HCO + H2+_ORTHO -> CO + H3+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2922) = small + (5.000e-10)
    end if

    !HCO + H2+_PARA -> CO + H3+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2923) = small + (1.000e-09)
    end if

    !H2_ORTHO + HEH+ -> HE + H3+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2924) = small + (9.000e-10)
    end if

    !H2_PARA + HEH+ -> HE + H3+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2925) = small + (1.800e-09)
    end if

    !H2_ORTHO + NH+ -> N + H3+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2926) = small + (1.125e-10)
    end if

    !H2_PARA + NH+ -> N + H3+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2927) = small + (2.250e-10)
    end if

    !H2_ORTHO + O2H+ -> O2 + H3+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2928) = small + (1.600e-10)
    end if

    !H2_PARA + O2H+ -> O2 + H3+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2929) = small + (3.200e-10)
    end if

    !D+ + GRAIN- -> D + GRAIN0
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2930) = small + (5.083e-15&
          *(T32)**(+5.000e-01))
    end if

    !D2_ORTHO + HD+ -> H + D3+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2931) = small + (4.200e-10)
    end if

    !D2_ORTHO + D2+_ORTHO -> D + D3+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2932) = small + (7.000e-09)
    end if

    !D2_PARA + HD+ -> H + D3+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2933) = small + (4.200e-10)
    end if

    !D2_PARA + D2+_ORTHO -> D + D3+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2934) = small + (7.000e-10)
    end if

    !D2_ORTHO + D2+_PARA -> D + D3+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2935) = small + (7.000e-10)
    end if

    !D2_PARA + D2+_PARA -> D + D3+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2936) = small + (2.100e-09)
    end if

    !D3+_PARA + H2_PARA -> H2D+_PARA + D2_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2937) = small + (2.210e-10&
          *exp(-3.792e+02*invT))
    end if

    !D3+_PARA + H2_PARA -> D2H+_PARA + HD
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2938) = small + (1.770e-09&
          *exp(-2.252e+02*invT))
    end if

    !D3+_PARA + H2_ORTHO -> H2D+_ORTHO + D2_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2939) = small + (3.000e-10&
          *exp(-2.867e+02*invT))
    end if

    !D3+_PARA + HD -> D2H+_PARA + D2_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2940) = small + (2.770e-10&
          *exp(-2.297e+02*invT))
    end if

    !D3+_PARA + HD -> D2H+_PARA + D2_ORTHO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2941) = small + (2.240e-10&
          *exp(-1.448e+02*invT))
    end if

    !D3+_PARA + HD -> D2H+_ORTHO + D2_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2942) = small + (1.500e-10&
          *exp(-1.820e+02*invT))
    end if

    !D3+_PARA + HD -> D3+_ORTHO + HD
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2943) = small + (7.750e-10)
    end if

    !D3+_ORTHO + HD -> D3+_PARA + HD
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2944) = small + (9.160e-11&
          *exp(-1.550e+01*invT))
    end if

    !D3+_PARA + D2_PARA -> D3+_ORTHO + D2_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2945) = small + (4.610e-10&
          *exp(+2.900e-01*invT))
    end if

    !D3+_PARA + D2_PARA -> D3+_ORTHO + D2_ORTHO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2946) = small + (4.750e-10&
          *exp(-5.400e-01*invT))
    end if

    !D3+_PARA + D2_ORTHO -> D3+_ORTHO + D2_ORTHO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2947) = small + (6.180e-10&
          *exp(+7.700e-01*invT))
    end if

    !D3+_ORTHO + D2_PARA -> D3+_PARA + D2_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2948) = small + (5.370e-11&
          *exp(-1.520e+01*invT))
    end if

    !D3+_ORTHO + D2_ORTHO -> D3+_PARA + D2_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2949) = small + (2.510e-11&
          *exp(-9.950e+01*invT))
    end if

    !D3+_ORTHO + D2_ORTHO -> D3+_PARA + D2_ORTHO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2950) = small + (7.740e-11&
          *exp(-1.530e+01*invT))
    end if

    !DCO + D2+_ORTHO -> CO + D3+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2951) = small + (3.333e-10)
    end if

    !DCO + D2+_PARA -> CO + D3+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2952) = small + (5.000e-10)
    end if

    !D2_ORTHO + HED+ -> HE + D3+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2953) = small + (6.000e-10)
    end if

    !D2_PARA + HED+ -> HE + D3+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2954) = small + (9.000e-10)
    end if

    !D2_ORTHO + ND+ -> N + D3+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2955) = small + (7.500e-11)
    end if

    !D2_PARA + ND+ -> N + D3+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2956) = small + (1.125e-10)
    end if

    !C + D3+_PARA -> D2_ORTHO + CD+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2957) = small + (1.000e-09)
    end if

    !C + D3+_PARA -> D2_PARA + CD+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2958) = small + (1.000e-09)
    end if

    !N + D3+_PARA -> D + ND2+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2959) = small + (1.000e-17)
    end if

    !O + D3+_PARA -> D2_ORTHO + OD+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2960) = small + (4.270e-10&
          *(T32)**(-2.100e-01))
    end if

    !O + D3+_PARA -> D2_PARA + OD+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2961) = small + (4.270e-10&
          *(T32)**(-2.100e-01))
    end if

    !C2 + D3+_PARA -> D2_ORTHO + C2D+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2962) = small + (9.000e-10)
    end if

    !C2 + D3+_PARA -> D2_PARA + C2D+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2963) = small + (9.000e-10)
    end if

    !CD + D3+_PARA -> D2_ORTHO + CD2+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2964) = small + (4.250e-09&
          *(T32)**(-5.000e-01))
    end if

    !CD + D3+_PARA -> D2_PARA + CD2+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2965) = small + (4.250e-09&
          *(T32)**(-5.000e-01))
    end if

    !CN + D3+_PARA -> D2_ORTHO + DCN+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2966) = small + (4.050e-09&
          *(T32)**(-5.000e-01))
    end if

    !CN + D3+_PARA -> D2_PARA + DCN+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2967) = small + (4.050e-09&
          *(T32)**(-5.000e-01))
    end if

    !CO + D3+_PARA -> D2_ORTHO + DCO+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2968) = small + (6.000e-10)
    end if

    !CO + D3+_PARA -> D2_ORTHO + DOC+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2969) = small + (8.050e-10)
    end if

    !CO + D3+_PARA -> D2_PARA + DOC+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2970) = small + (8.050e-10)
    end if

    !N2 + D3+_PARA -> D2_ORTHO + N2D+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2971) = small + (6.500e-10)
    end if

    !ND + D3+_PARA -> D2_ORTHO + ND2+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2972) = small + (3.750e-09&
          *(T32)**(-5.000e-01))
    end if

    !ND + D3+_PARA -> D2_PARA + ND2+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2973) = small + (3.750e-09&
          *(T32)**(-5.000e-01))
    end if

    !NO + D3+_PARA -> D2_ORTHO + DNO+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2974) = small + (4.250e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO + D3+_PARA -> D2_PARA + DNO+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2975) = small + (4.250e-10&
          *(T32)**(-5.000e-01))
    end if

    !O2 + D3+_PARA -> D2_ORTHO + O2D+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2976) = small + (3.200e-10)
    end if

    !O2 + D3+_PARA -> D2_PARA + O2D+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2977) = small + (3.200e-10)
    end if

    !OD + D3+_PARA -> D2_ORTHO + D2O+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2978) = small + (4.750e-09&
          *(T32)**(-5.000e-01))
    end if

    !OD + D3+_PARA -> D2_PARA + D2O+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2979) = small + (4.750e-09&
          *(T32)**(-5.000e-01))
    end if

    !NO2 + D3+_PARA -> D2_ORTHO + OD + NO+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2980) = small + (3.640e-10&
          *(T32)**(-5.000e-01))
    end if

    !NO2 + D3+_PARA -> D2_PARA + OD + NO+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2981) = small + (3.640e-10&
          *(T32)**(-5.000e-01))
    end if

    !O2D+ + D2_ORTHO -> D3+_PARA + O2
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2982) = small + (1.067e-10)
    end if

    !O2D+ + D2_PARA -> D3+_PARA + O2
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2983) = small + (1.600e-10)
    end if

    !D3+_PARA + D- -> D2_ORTHO + D2_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2984) = small + (1.150e-07&
          *(T32)**(-5.000e-01))
    end if

    !D3+_PARA + D- -> D2_PARA + D2_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2985) = small + (1.150e-07&
          *(T32)**(-5.000e-01))
    end if

    !D3+_PARA -> D2_ORTHO + D+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2986) = small + (1.000e-08&
          *exp(-1.800e+00*user_Av))
    end if

    !D3+_PARA -> D2_PARA + D+
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2987) = small + (1.000e-08&
          *exp(-1.800e+00*user_Av))
    end if

    !D3+_PARA -> D + D2+_ORTHO
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2988) = small + (3.950e-09&
          *exp(-2.300e+00*user_Av))
    end if

    !D3+_PARA -> D + D2+_PARA
    if(Tgas.GE.-9999d0 .and. Tgas.LT.9999d0) then
      k(2989) = small + (3.950e-09&
          *exp(-2.300e+00*user_Av))
    end if

    !H2O + H2+_ORTHO -> H + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(2990) = small + (4.660e-01&
          *2.070e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + H2+_PARA -> H + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(2991) = small + (4.660e-01&
          *2.070e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H3O+ + H- -> H + H2_ORTHO + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2992) = small + (1.720e-07&
          *(T32)**(-5.000e-01)*exp(-1.700e+02*invT))
    end if

    !H3O+ + H- -> H + H2_PARA + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2993) = small + (5.750e-08&
          *(T32)**(-5.000e-01))
    end if

    !H3O+ + H- -> H2_PARA + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2994) = small + (5.750e-08&
          *(T32)**(-5.000e-01))
    end if

    !H3O+ + H- -> H2_ORTHO + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2995) = small + (1.720e-07&
          *(T32)**(-5.000e-01)*exp(-1.700e+02*invT))
    end if

    !H2O + HCN+ -> CN + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(2996) = small + (3.210e-01&
          *8.450e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + HCO+ -> CO + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(2997) = small + (1.000e+00&
          *8.340e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2_PARA + H2O+ -> H + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2998) = small + (6.100e-10)
    end if

    !H2_ORTHO + H2O+ -> H + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(2999) = small + (6.100e-10)
    end if

    !NH + H2O+ -> N + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3000) = small + (1.000e+00&
          *9.850e-10*(0.62d0 + 0.4767d0*4.400e+00*sqrt(3d2&
          *invT)))
    end if

    !OH + H2O+ -> O + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3001) = small + (1.000e+00&
          *8.210e-10*(0.62d0 + 0.4767d0*5.500e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + H2O+ -> OH + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3002) = small + (1.000e+00&
          *9.260e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HCO + H2O+ -> CO + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3003) = small + (3.330e-01&
          *1.120e-09*(0.62d0 + 0.4767d0*3.580e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + H3+_ORTHO -> H2_ORTHO + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3004) = small + (8.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + H3+_ORTHO -> H2_PARA + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3005) = small + (2.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + H3+_PARA -> H2_ORTHO + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3006) = small + (6.500e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + H3+_PARA -> H2_PARA + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3007) = small + (3.500e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + HNC+ -> CN + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3008) = small + (1.000e+00&
          *8.450e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + HNO+ -> NO + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3009) = small + (1.000e+00&
          *8.220e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + N2H+ -> N2 + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3010) = small + (1.000e+00&
          *8.340e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !C + H3O+ -> H2_PARA + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3011) = small + (2.500e-12)
    end if

    !C + H3O+ -> H2_ORTHO + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3012) = small + (7.500e-12&
          *exp(-1.700e+02*invT))
    end if

    !CH + H3O+ -> H2O + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3013) = small + (1.000e+00&
          *1.230e-09*(0.62d0 + 0.4767d0*3.330e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + CH+ -> C + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3014) = small + (1.430e-01&
          *1.010e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + NH+ -> N + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3015) = small + (4.620e-01&
          *9.710e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + OH+ -> O + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3016) = small + (4.640e-01&
          *9.390e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + NH2+ -> NH + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3017) = small + (9.250e-01&
          *9.540e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H3O+ + C- -> C + H + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3018) = small + (3.760e-08&
          *(T32)**(-5.000e-01))
    end if

    !H3O+ + O- -> H + O + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3019) = small + (3.760e-08&
          *(T32)**(-5.000e-01))
    end if

    !H3O+ + CN- -> H + CN + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3020) = small + (3.760e-08&
          *(T32)**(-5.000e-01))
    end if

    !H3O+ + OH- -> H + OH + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3021) = small + (3.760e-08&
          *(T32)**(-5.000e-01))
    end if

    !H + H3O+ -> H2_PARA + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3022) = small + (1.520e-10&
          *exp(-2.050e+04*invT))
    end if

    !H + H3O+ -> H2_ORTHO + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3023) = small + (4.570e-10&
          *exp(-2.070e+04*invT))
    end if

    !H2_ORTHO + H3O+ -> H + H + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3024) = small + (3.000e-11&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !H2_PARA + H3O+ -> H + H + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3025) = small + (3.000e-11&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !H3O+ + E -> H + H + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3026) = small + (2.600e-07&
          *(T32)**(-5.000e-01))
    end if

    !H3O+ + E -> H + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3027) = small + (1.100e-07&
          *(T32)**(-5.000e-01))
    end if

    !H3O+ + E -> H2_PARA + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3028) = small + (1.500e-08&
          *(T32)**(-5.000e-01))
    end if

    !H3O+ + E -> H2_ORTHO + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3029) = small + (4.500e-08&
          *(T32)**(-5.000e-01)*exp(-1.700e+02*invT))
    end if

    !H3O+ + E -> H + O + H2_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3030) = small + (4.200e-09&
          *(T32)**(-5.000e-01)*exp(-1.700e+02*invT))
    end if

    !H3O+ + E -> H + O + H2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3031) = small + (1.400e-09&
          *(T32)**(-5.000e-01))
    end if

    !H2O + HD+ -> H + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3032) = small + (3.500e-01&
          *2.070e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + HD+ -> D + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3033) = small + (1.170e-01&
          *2.070e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + D2+_ORTHO -> H + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3034) = small + (2.330e-01&
          *2.070e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + D2+_PARA -> H + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3035) = small + (2.330e-01&
          *2.070e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + D2+_ORTHO -> D + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3036) = small + (2.330e-01&
          *2.070e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + D2+_PARA -> D + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3037) = small + (2.330e-01&
          *2.070e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + H2+_PARA -> H + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3038) = small + (3.500e-01&
          *2.070e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + H2+_ORTHO -> H + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3039) = small + (3.500e-01&
          *2.070e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + H2+_PARA -> D + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3040) = small + (1.170e-01&
          *2.070e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + H2+_ORTHO -> D + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3041) = small + (1.170e-01&
          *2.070e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + HD+ -> H + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3042) = small + (2.330e-01&
          *2.070e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + HD+ -> D + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3043) = small + (2.330e-01&
          *2.070e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + D2+_PARA -> H + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3044) = small + (1.170e-01&
          *2.070e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + D2+_ORTHO -> H + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3045) = small + (1.170e-01&
          *2.070e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + D2+_PARA -> D + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3046) = small + (3.500e-01&
          *2.070e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + D2+_ORTHO -> D + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3047) = small + (3.500e-01&
          *2.070e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + H2+_PARA -> H + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3048) = small + (2.330e-01&
          *2.070e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + H2+_ORTHO -> H + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3049) = small + (2.330e-01&
          *2.070e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + H2+_PARA -> D + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3050) = small + (2.330e-01&
          *2.070e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + H2+_ORTHO -> D + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3051) = small + (2.330e-01&
          *2.070e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + HD+ -> H + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3052) = small + (1.170e-01&
          *2.070e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + HD+ -> D + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3053) = small + (3.500e-01&
          *2.070e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + D2+_ORTHO -> D + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3054) = small + (4.660e-01&
          *2.070e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + D2+_PARA -> D + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3055) = small + (4.660e-01&
          *2.070e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H3O+ + D- -> H + H2_ORTHO + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3056) = small + (4.310e-08&
          *(T32)**(-5.000e-01)*exp(-1.700e+02*invT))
    end if

    !H3O+ + D- -> H + H2_PARA + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3057) = small + (1.440e-08&
          *(T32)**(-5.000e-01))
    end if

    !H3O+ + D- -> H + HD + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3058) = small + (1.150e-07&
          *(T32)**(-5.000e-01))
    end if

    !H3O+ + D- -> D + H2_ORTHO + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3059) = small + (4.310e-08&
          *(T32)**(-5.000e-01)*exp(-1.700e+02*invT))
    end if

    !H3O+ + D- -> D + H2_PARA + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3060) = small + (1.440e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + H- -> H + H2_ORTHO + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3061) = small + (3.830e-08&
          *(T32)**(-5.000e-01)*exp(-1.700e+02*invT))
    end if

    !H2DO+ + H- -> H + H2_PARA + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3062) = small + (1.920e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + H- -> H + HD + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3063) = small + (1.150e-07&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + H- -> D + H2_ORTHO + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3064) = small + (3.830e-08&
          *(T32)**(-5.000e-01)*exp(-1.700e+02*invT))
    end if

    !H2DO+ + H- -> D + H2_PARA + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3065) = small + (1.920e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + D- -> H + HD + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3066) = small + (7.670e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + D- -> H + D2_PARA + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3067) = small + (1.280e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + D- -> H + D2_ORTHO + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3068) = small + (2.550e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + D- -> D + H2_PARA + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3069) = small + (1.910e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + D- -> D + H2_ORTHO + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3070) = small + (1.910e-08&
          *(T32)**(-5.000e-01)*exp(-1.700e+02*invT))
    end if

    !H2DO+ + D- -> D + HD + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3071) = small + (7.670e-08&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + H- -> H + HD + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3072) = small + (7.670e-08&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + H- -> H + D2_PARA + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3073) = small + (1.910e-08&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + H- -> H + D2_ORTHO + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3074) = small + (1.910e-08&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + H- -> D + H2_ORTHO + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3075) = small + (2.870e-08&
          *(T32)**(-5.000e-01)*exp(-1.700e+02*invT))
    end if

    !HD2O+ + H- -> D + H2_PARA + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3076) = small + (9.570e-09&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + H- -> D + HD + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3077) = small + (7.670e-08&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + D- -> H + D2_PARA + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3078) = small + (2.240e-08&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + D- -> H + D2_ORTHO + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3079) = small + (3.510e-08&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + D- -> D + HD + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3080) = small + (1.150e-07&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + D- -> D + D2_PARA + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3081) = small + (2.240e-08&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + D- -> D + D2_ORTHO + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3082) = small + (3.510e-08&
          *(T32)**(-5.000e-01))
    end if

    !D3O+ + H- -> H + D2_PARA + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3083) = small + (2.870e-08&
          *(T32)**(-5.000e-01))
    end if

    !D3O+ + H- -> H + D2_ORTHO + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3084) = small + (2.870e-08&
          *(T32)**(-5.000e-01))
    end if

    !D3O+ + H- -> D + HD + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3085) = small + (1.150e-07&
          *(T32)**(-5.000e-01))
    end if

    !D3O+ + H- -> D + D2_PARA + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3086) = small + (2.870e-08&
          *(T32)**(-5.000e-01))
    end if

    !D3O+ + H- -> D + D2_ORTHO + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3087) = small + (2.870e-08&
          *(T32)**(-5.000e-01))
    end if

    !D3O+ + D- -> D + D2_PARA + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3088) = small + (9.580e-08&
          *(T32)**(-5.000e-01))
    end if

    !D3O+ + D- -> D + D2_ORTHO + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3089) = small + (1.340e-07&
          *(T32)**(-5.000e-01))
    end if

    !H3O+ + D- -> H2_PARA + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3090) = small + (2.870e-08&
          *(T32)**(-5.000e-01))
    end if

    !H3O+ + D- -> H2_ORTHO + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3091) = small + (8.620e-08&
          *(T32)**(-5.000e-01)*exp(-1.700e+02*invT))
    end if

    !H3O+ + D- -> HD + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3092) = small + (1.150e-07&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + H- -> H2_PARA + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3093) = small + (3.830e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + H- -> H2_ORTHO + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3094) = small + (7.670e-08&
          *(T32)**(-5.000e-01)*exp(-1.700e+02*invT))
    end if

    !H2DO+ + H- -> HD + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3095) = small + (1.150e-07&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + D- -> H2_PARA + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3096) = small + (1.910e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + D- -> H2_ORTHO + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3097) = small + (1.910e-08&
          *(T32)**(-5.000e-01)*exp(-1.700e+02*invT))
    end if

    !H2DO+ + D- -> HD + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3098) = small + (1.530e-07&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + D- -> D2_ORTHO + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3099) = small + (2.550e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + D- -> D2_PARA + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3100) = small + (1.280e-08&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + H- -> H2_PARA + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3101) = small + (9.570e-09&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + H- -> H2_ORTHO + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3102) = small + (2.870e-08&
          *(T32)**(-5.000e-01)*exp(-1.700e+02*invT))
    end if

    !HD2O+ + H- -> HD + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3103) = small + (1.530e-07&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + H- -> D2_PARA + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3104) = small + (1.910e-08&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + H- -> D2_ORTHO + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3105) = small + (1.910e-08&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + D- -> HD + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3106) = small + (1.150e-07&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + D- -> D2_PARA + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3107) = small + (4.470e-08&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + D- -> D2_ORTHO + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3108) = small + (7.030e-08&
          *(T32)**(-5.000e-01))
    end if

    !D3O+ + H- -> HD + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3109) = small + (1.150e-07&
          *(T32)**(-5.000e-01))
    end if

    !D3O+ + H- -> D2_PARA + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3110) = small + (5.750e-08&
          *(T32)**(-5.000e-01))
    end if

    !D3O+ + H- -> D2_ORTHO + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3111) = small + (5.750e-08&
          *(T32)**(-5.000e-01))
    end if

    !D3O+ + D- -> D2_ORTHO + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3112) = small + (1.340e-07&
          *(T32)**(-5.000e-01))
    end if

    !D3O+ + D- -> D2_PARA + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3113) = small + (9.580e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2O + DCN+ -> CN + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3114) = small + (3.210e-01&
          *(T32)**(8.450e-10)*exp(-5.410e+00*invT))
    end if

    !HDO + HCN+ -> CN + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3115) = small + (3.210e-01&
          *8.450e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + DCN+ -> CN + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3116) = small + (3.210e-01&
          *8.450e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + HCN+ -> CN + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3117) = small + (3.210e-01&
          *8.450e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + DCN+ -> CN + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3118) = small + (3.210e-01&
          *8.450e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + DCO+ -> CO + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3119) = small + (1.000e+00&
          *8.340e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + HCO+ -> CO + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3120) = small + (1.000e+00&
          *8.340e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + DCO+ -> CO + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3121) = small + (1.000e+00&
          *8.340e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + HCO+ -> CO + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3122) = small + (1.000e+00&
          *8.340e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + DCO+ -> CO + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3123) = small + (1.000e+00&
          *8.340e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2_PARA + HDO+ -> H + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3124) = small + (4.570e-10)
    end if

    !H2_ORTHO + HDO+ -> H + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3125) = small + (4.570e-10)
    end if

    !H2_PARA + HDO+ -> D + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3126) = small + (1.520e-10)
    end if

    !H2_ORTHO + HDO+ -> D + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3127) = small + (1.520e-10)
    end if

    !H2_PARA + D2O+ -> H + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3128) = small + (3.050e-10)
    end if

    !H2_ORTHO + D2O+ -> H + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3129) = small + (3.050e-10)
    end if

    !H2_PARA + D2O+ -> D + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3130) = small + (3.050e-10)
    end if

    !H2_ORTHO + D2O+ -> D + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3131) = small + (3.050e-10)
    end if

    !HD + H2O+ -> H + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3132) = small + (4.570e-10)
    end if

    !HD + H2O+ -> D + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3133) = small + (1.520e-10)
    end if

    !HD + HDO+ -> H + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3134) = small + (3.050e-10)
    end if

    !HD + HDO+ -> D + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3135) = small + (3.050e-10)
    end if

    !HD + D2O+ -> H + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3136) = small + (1.520e-10)
    end if

    !HD + D2O+ -> D + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3137) = small + (4.570e-10)
    end if

    !D2_ORTHO + H2O+ -> H + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3138) = small + (3.050e-10)
    end if

    !D2_PARA + H2O+ -> H + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3139) = small + (3.050e-10)
    end if

    !D2_ORTHO + H2O+ -> D + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3140) = small + (3.050e-10)
    end if

    !D2_PARA + H2O+ -> D + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3141) = small + (3.050e-10)
    end if

    !D2_PARA + HDO+ -> H + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3142) = small + (1.520e-10)
    end if

    !D2_ORTHO + HDO+ -> H + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3143) = small + (1.520e-10)
    end if

    !D2_PARA + HDO+ -> D + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3144) = small + (4.570e-10)
    end if

    !D2_ORTHO + HDO+ -> D + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3145) = small + (4.570e-10)
    end if

    !D2_ORTHO + D2O+ -> D + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3146) = small + (6.100e-10)
    end if

    !D2_PARA + D2O+ -> D + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3147) = small + (6.100e-10)
    end if

    !NH + HDO+ -> N + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3148) = small + (1.000e+00&
          *9.850e-10*(0.62d0 + 0.4767d0*4.400e+00*sqrt(3d2&
          *invT)))
    end if

    !NH + D2O+ -> N + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3149) = small + (1.000e+00&
          *9.850e-10*(0.62d0 + 0.4767d0*4.400e+00*sqrt(3d2&
          *invT)))
    end if

    !ND + H2O+ -> N + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3150) = small + (1.000e+00&
          *9.850e-10*(0.62d0 + 0.4767d0*4.400e+00*sqrt(3d2&
          *invT)))
    end if

    !ND + HDO+ -> N + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3151) = small + (1.000e+00&
          *9.850e-10*(0.62d0 + 0.4767d0*4.400e+00*sqrt(3d2&
          *invT)))
    end if

    !ND + D2O+ -> N + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3152) = small + (1.000e+00&
          *9.850e-10*(0.62d0 + 0.4767d0*4.400e+00*sqrt(3d2&
          *invT)))
    end if

    !OH + HDO+ -> O + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3153) = small + (1.000e+00&
          *8.210e-10*(0.62d0 + 0.4767d0*5.500e+00*sqrt(3d2&
          *invT)))
    end if

    !OH + D2O+ -> O + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3154) = small + (1.000e+00&
          *8.210e-10*(0.62d0 + 0.4767d0*5.500e+00*sqrt(3d2&
          *invT)))
    end if

    !OD + H2O+ -> O + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3155) = small + (1.000e+00&
          *8.210e-10*(0.62d0 + 0.4767d0*5.500e+00*sqrt(3d2&
          *invT)))
    end if

    !OD + HDO+ -> O + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3156) = small + (1.000e+00&
          *8.210e-10*(0.62d0 + 0.4767d0*5.500e+00*sqrt(3d2&
          *invT)))
    end if

    !OD + D2O+ -> O + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3157) = small + (1.000e+00&
          *8.210e-10*(0.62d0 + 0.4767d0*5.500e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + HDO+ -> OH + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3158) = small + (7.500e-01&
          *9.260e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + HDO+ -> OD + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3159) = small + (2.500e-01&
          *9.260e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + D2O+ -> OH + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3160) = small + (5.000e-01&
          *9.260e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + D2O+ -> OD + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3161) = small + (5.000e-01&
          *9.260e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + H2O+ -> OH + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3162) = small + (7.500e-01&
          *9.260e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + H2O+ -> OD + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3163) = small + (2.500e-01&
          *9.260e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + HDO+ -> OH + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3164) = small + (5.000e-01&
          *9.260e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + HDO+ -> OD + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3165) = small + (5.000e-01&
          *9.260e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + D2O+ -> OH + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3166) = small + (2.500e-01&
          *9.260e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + D2O+ -> OD + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3167) = small + (7.500e-01&
          *9.260e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + H2O+ -> OH + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3168) = small + (5.000e-01&
          *9.260e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + H2O+ -> OD + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3169) = small + (5.000e-01&
          *9.260e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + HDO+ -> OH + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3170) = small + (2.500e-01&
          *9.260e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + HDO+ -> OD + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3171) = small + (7.500e-01&
          *9.260e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + D2O+ -> OD + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3172) = small + (1.000e+00&
          *9.260e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HCO + HDO+ -> CO + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3173) = small + (3.330e-01&
          *1.120e-09*(0.62d0 + 0.4767d0*3.580e+00*sqrt(3d2&
          *invT)))
    end if

    !HCO + D2O+ -> CO + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3174) = small + (3.330e-01&
          *1.120e-09*(0.62d0 + 0.4767d0*3.580e+00*sqrt(3d2&
          *invT)))
    end if

    !DCO + H2O+ -> CO + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3175) = small + (3.330e-01&
          *1.120e-09*(0.62d0 + 0.4767d0*3.580e+00*sqrt(3d2&
          *invT)))
    end if

    !DCO + HDO+ -> CO + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3176) = small + (3.330e-01&
          *1.120e-09*(0.62d0 + 0.4767d0*3.580e+00*sqrt(3d2&
          *invT)))
    end if

    !DCO + D2O+ -> CO + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3177) = small + (3.330e-01&
          *1.120e-09*(0.62d0 + 0.4767d0*3.580e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + H2D+_ORTHO -> H2_PARA + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3178) = small + (1.500e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + H2D+_ORTHO -> H2_ORTHO + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3179) = small + (4.500e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + H2D+_PARA -> H2_PARA + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3180) = small + (2.500e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + H2D+_PARA -> H2_ORTHO + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3181) = small + (3.500e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + H2D+_ORTHO -> HD + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3182) = small + (4.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + H2D+_PARA -> HD + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3183) = small + (4.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + D2H+_ORTHO -> H2_PARA + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3184) = small + (1.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + D2H+_ORTHO -> H2_ORTHO + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3185) = small + (2.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + D2H+_PARA -> H2_PARA + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3186) = small + (1.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + D2H+_PARA -> H2_ORTHO + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3187) = small + (2.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + D2H+_ORTHO -> HD + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3188) = small + (6.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + D2H+_PARA -> HD + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3189) = small + (6.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + D2H+_ORTHO -> D2_ORTHO + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3190) = small + (1.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + D2H+_PARA -> D2_PARA + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3191) = small + (1.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + D3+_PARA -> H2_ORTHO + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3192) = small + (1.670e-02&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + D3+_META -> H2_ORTHO + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3193) = small + (1.670e-02&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + D3+_ORTHO -> H2_PARA + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3194) = small + (1.670e-02&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + D3+_PARA -> H2_PARA + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3195) = small + (1.670e-02&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + D3+_ORTHO -> H2_ORTHO + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3196) = small + (1.670e-02&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + D3+_META -> H2_PARA + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3197) = small + (1.670e-02&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + D3+_PARA -> HD + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3198) = small + (2.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + D3+_META -> HD + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3199) = small + (2.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + D3+_ORTHO -> HD + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3200) = small + (2.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + D3+_PARA -> D2_PARA + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3201) = small + (1.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + D3+_META -> D2_ORTHO + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3202) = small + (1.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + D3+_ORTHO -> D2_ORTHO + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3203) = small + (5.000e-02&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + D3+_ORTHO -> D2_PARA + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3204) = small + (5.000e-02&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + H3+_ORTHO -> H2_PARA + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3205) = small + (7.500e-02&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + H3+_ORTHO -> H2_ORTHO + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3206) = small + (5.250e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + H3+_PARA -> H2_PARA + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3207) = small + (2.250e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + H3+_PARA -> H2_ORTHO + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3208) = small + (3.750e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + H3+_ORTHO -> HD + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3209) = small + (4.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + H3+_PARA -> HD + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3210) = small + (4.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + H2D+_PARA -> H2_PARA + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3211) = small + (1.500e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + H2D+_PARA -> H2_ORTHO + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3212) = small + (1.500e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + H2D+_ORTHO -> H2_PARA + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3213) = small + (5.000e-02&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + H2D+_ORTHO -> H2_ORTHO + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3214) = small + (2.500e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + H2D+_PARA -> HD + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3215) = small + (6.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + H2D+_ORTHO -> HD + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3216) = small + (6.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + H2D+_PARA -> D2_PARA + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3217) = small + (3.330e-02&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + H2D+_PARA -> D2_ORTHO + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3218) = small + (6.670e-02&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + H2D+_ORTHO -> D2_PARA + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3219) = small + (3.330e-02&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + H2D+_ORTHO -> D2_ORTHO + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3220) = small + (6.670e-02&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + D2H+_PARA -> H2_PARA + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3221) = small + (2.500e-02&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + D2H+_PARA -> H2_ORTHO + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3222) = small + (7.500e-02&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + D2H+_ORTHO -> H2_PARA + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3223) = small + (2.500e-02&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + D2H+_ORTHO -> H2_ORTHO + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3224) = small + (7.500e-02&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + D2H+_PARA -> HD + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3225) = small + (6.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + D2H+_ORTHO -> HD + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3226) = small + (6.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + D2H+_PARA -> D2_ORTHO + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3227) = small + (1.330e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + D2H+_PARA -> D2_PARA + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3228) = small + (1.670e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + D2H+_ORTHO -> D2_ORTHO + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3229) = small + (2.330e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + D2H+_ORTHO -> D2_PARA + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3230) = small + (6.670e-02&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + D3+_META -> HD + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3231) = small + (4.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + D3+_PARA -> HD + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3232) = small + (4.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + D3+_ORTHO -> HD + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3233) = small + (4.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + D3+_META -> D2_ORTHO + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3234) = small + (5.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + D3+_META -> D2_PARA + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3235) = small + (1.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + D3+_PARA -> D2_ORTHO + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3236) = small + (2.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + D3+_PARA -> D2_PARA + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3237) = small + (4.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + D3+_ORTHO -> D2_ORTHO + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3238) = small + (3.500e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + D3+_ORTHO -> D2_PARA + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3239) = small + (2.500e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + H3+_PARA -> H2_PARA + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3240) = small + (1.500e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + H3+_PARA -> H2_ORTHO + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3241) = small + (1.500e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + H3+_ORTHO -> H2_ORTHO + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3242) = small + (3.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + H3+_PARA -> HD + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3243) = small + (6.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + H3+_ORTHO -> HD + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3244) = small + (6.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + H3+_PARA -> D2_PARA + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3245) = small + (5.000e-02&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + H3+_ORTHO -> D2_ORTHO + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3246) = small + (5.000e-02&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + H3+_PARA -> D2_ORTHO + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3247) = small + (5.000e-02&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + H3+_ORTHO -> D2_PARA + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3248) = small + (5.000e-02&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + H2D+_PARA -> H2_PARA + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3249) = small + (1.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + H2D+_ORTHO -> H2_ORTHO + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3250) = small + (1.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + H2D+_PARA -> HD + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3251) = small + (6.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + H2D+_ORTHO -> HD + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3252) = small + (6.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + H2D+_PARA -> D2_ORTHO + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3253) = small + (1.830e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + H2D+_PARA -> D2_PARA + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3254) = small + (1.170e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + H2D+_ORTHO -> D2_PARA + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3255) = small + (1.170e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + H2D+_ORTHO -> D2_ORTHO + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3256) = small + (1.830e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + D2H+_PARA -> HD + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3257) = small + (4.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + D2H+_ORTHO -> HD + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3258) = small + (4.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + D2H+_PARA -> D2_ORTHO + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3259) = small + (3.170e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + D2H+_PARA -> D2_PARA + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3260) = small + (2.830e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + D2H+_ORTHO -> D2_ORTHO + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3261) = small + (4.170e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + D2H+_ORTHO -> D2_PARA + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3262) = small + (1.830e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + D3+_PARA -> D2_PARA + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3263) = small + (5.500e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + D3+_PARA -> D2_ORTHO + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3264) = small + (4.500e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + D3+_META -> D2_PARA + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3265) = small + (2.500e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + D3+_META -> D2_ORTHO + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3266) = small + (7.500e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + D3+_ORTHO -> D2_PARA + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3267) = small + (4.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + D3+_ORTHO -> D2_ORTHO + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3268) = small + (6.000e-01&
          *1.730e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + DNC+ -> CN + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3269) = small + (1.000e+00&
          *8.450e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + HNC+ -> CN + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3270) = small + (1.000e+00&
          *8.450e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + DNC+ -> CN + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3271) = small + (1.000e+00&
          *8.450e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + HNC+ -> CN + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3272) = small + (1.000e+00&
          *8.450e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + DNC+ -> CN + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3273) = small + (1.000e+00&
          *8.450e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + DNO+ -> NO + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3274) = small + (1.000e+00&
          *8.220e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + HNO+ -> NO + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3275) = small + (1.000e+00&
          *8.220e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + DNO+ -> NO + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3276) = small + (1.000e+00&
          *8.220e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + HNO+ -> NO + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3277) = small + (1.000e+00&
          *8.220e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + DNO+ -> NO + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3278) = small + (1.000e+00&
          *8.220e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + N2D+ -> N2 + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3279) = small + (1.000e+00&
          *8.340e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + N2H+ -> N2 + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3280) = small + (1.000e+00&
          *8.340e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + N2D+ -> N2 + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3281) = small + (1.000e+00&
          *8.340e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + N2H+ -> N2 + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3282) = small + (1.000e+00&
          *8.340e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + N2D+ -> N2 + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3283) = small + (1.000e+00&
          *8.340e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !C + H2DO+ -> H2_PARA + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3284) = small + (1.670e-12)
    end if

    !C + H2DO+ -> H2_ORTHO + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3285) = small + (1.670e-12&
          *exp(-1.700e+02*invT))
    end if

    !C + H2DO+ -> HD + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3286) = small + (6.670e-12)
    end if

    !C + HD2O+ -> HD + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3287) = small + (6.670e-12)
    end if

    !C + HD2O+ -> D2_PARA + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3288) = small + (1.670e-12)
    end if

    !C + HD2O+ -> D2_ORTHO + HCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3289) = small + (1.670e-12)
    end if

    !C + D3O+ -> D2_PARA + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3290) = small + (5.000e-12)
    end if

    !C + D3O+ -> D2_ORTHO + DCO+
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3291) = small + (5.000e-12)
    end if

    !CH + H2DO+ -> H2O + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3292) = small + (5.000e-01&
          *1.230e-09*(0.62d0 + 0.4767d0*3.330e+00*sqrt(3d2&
          *invT)))
    end if

    !CH + H2DO+ -> HDO + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3293) = small + (5.000e-01&
          *1.230e-09*(0.62d0 + 0.4767d0*3.330e+00*sqrt(3d2&
          *invT)))
    end if

    !CH + HD2O+ -> H2O + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3294) = small + (1.670e-01&
          *1.230e-09*(0.62d0 + 0.4767d0*3.330e+00*sqrt(3d2&
          *invT)))
    end if

    !CH + HD2O+ -> HDO + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3295) = small + (6.670e-01&
          *1.230e-09*(0.62d0 + 0.4767d0*3.330e+00*sqrt(3d2&
          *invT)))
    end if

    !CH + HD2O+ -> D2O + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3296) = small + (1.670e-01&
          *1.230e-09*(0.62d0 + 0.4767d0*3.330e+00*sqrt(3d2&
          *invT)))
    end if

    !CH + D3O+ -> HDO + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3297) = small + (5.000e-01&
          *1.230e-09*(0.62d0 + 0.4767d0*3.330e+00*sqrt(3d2&
          *invT)))
    end if

    !CH + D3O+ -> D2O + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3298) = small + (5.000e-01&
          *1.230e-09*(0.62d0 + 0.4767d0*3.330e+00*sqrt(3d2&
          *invT)))
    end if

    !CD + H3O+ -> H2O + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3299) = small + (5.000e-01&
          *1.230e-09*(0.62d0 + 0.4767d0*3.330e+00*sqrt(3d2&
          *invT)))
    end if

    !CD + H3O+ -> HDO + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3300) = small + (5.000e-01&
          *1.230e-09*(0.62d0 + 0.4767d0*3.330e+00*sqrt(3d2&
          *invT)))
    end if

    !CD + H2DO+ -> H2O + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3301) = small + (1.670e-01&
          *1.230e-09*(0.62d0 + 0.4767d0*3.330e+00*sqrt(3d2&
          *invT)))
    end if

    !CD + H2DO+ -> HDO + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3302) = small + (6.670e-01&
          *1.230e-09*(0.62d0 + 0.4767d0*3.330e+00*sqrt(3d2&
          *invT)))
    end if

    !CD + H2DO+ -> D2O + CH2+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3303) = small + (1.670e-01&
          *1.230e-09*(0.62d0 + 0.4767d0*3.330e+00*sqrt(3d2&
          *invT)))
    end if

    !CD + HD2O+ -> HDO + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3304) = small + (5.000e-01&
          *1.230e-09*(0.62d0 + 0.4767d0*3.330e+00*sqrt(3d2&
          *invT)))
    end if

    !CD + HD2O+ -> D2O + CHD+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3305) = small + (5.000e-01&
          *1.230e-09*(0.62d0 + 0.4767d0*3.330e+00*sqrt(3d2&
          *invT)))
    end if

    !CD + D3O+ -> D2O + CD2+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3306) = small + (1.000e+00&
          *1.230e-09*(0.62d0 + 0.4767d0*3.330e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + CD+ -> C + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3307) = small + (1.430e-01&
          *1.010e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + CH+ -> C + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3308) = small + (1.430e-01&
          *1.010e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + CD+ -> C + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3309) = small + (1.430e-01&
          *1.010e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + CH+ -> C + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3310) = small + (1.430e-01&
          *1.010e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + CD+ -> C + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3311) = small + (1.430e-01&
          *1.010e-09*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + ND+ -> N + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3312) = small + (4.620e-01&
          *9.710e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + NH+ -> N + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3313) = small + (4.620e-01&
          *9.710e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + ND+ -> N + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3314) = small + (4.620e-01&
          *9.710e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + NH+ -> N + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3315) = small + (4.620e-01&
          *9.710e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + ND+ -> N + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3316) = small + (4.620e-01&
          *9.710e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + OD+ -> O + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3317) = small + (4.640e-01&
          *9.390e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + OH+ -> O + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3318) = small + (4.640e-01&
          *9.390e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + OD+ -> O + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3319) = small + (4.640e-01&
          *9.390e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + OH+ -> O + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3320) = small + (4.640e-01&
          *9.390e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + OD+ -> O + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3321) = small + (4.640e-01&
          *9.390e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + NHD+ -> NH + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3322) = small + (6.940e-01&
          *9.540e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + NHD+ -> ND + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3323) = small + (2.310e-01&
          *9.540e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + ND2+ -> NH + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3324) = small + (4.630e-01&
          *9.540e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2O + ND2+ -> ND + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3325) = small + (4.630e-01&
          *9.540e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + NH2+ -> NH + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3326) = small + (6.940e-01&
          *9.540e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + NH2+ -> ND + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3327) = small + (2.310e-01&
          *9.540e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + NHD+ -> NH + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3328) = small + (4.630e-01&
          *9.540e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + NHD+ -> ND + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3329) = small + (4.630e-01&
          *9.540e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + ND2+ -> NH + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3330) = small + (2.310e-01&
          *9.540e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !HDO + ND2+ -> ND + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3331) = small + (6.940e-01&
          *9.540e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + NH2+ -> NH + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3332) = small + (4.630e-01&
          *9.540e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + NH2+ -> ND + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3333) = small + (4.630e-01&
          *9.540e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + NHD+ -> NH + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3334) = small + (2.310e-01&
          *9.540e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + NHD+ -> ND + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3335) = small + (6.940e-01&
          *9.540e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !D2O + ND2+ -> ND + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3336) = small + (9.250e-01&
          *9.540e-10*(0.62d0 + 0.4767d0*5.410e+00*sqrt(3d2&
          *invT)))
    end if

    !H2DO+ + C- -> C + H + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3337) = small + (2.510e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + C- -> C + D + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3338) = small + (1.250e-08&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + C- -> C + H + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3339) = small + (1.250e-08&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + C- -> C + D + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3340) = small + (2.510e-08&
          *(T32)**(-5.000e-01))
    end if

    !D3O+ + C- -> C + D + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3341) = small + (3.760e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + O- -> H + O + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3342) = small + (2.510e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + O- -> D + O + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3343) = small + (1.250e-08&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + O- -> H + O + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3344) = small + (1.250e-08&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + O- -> D + O + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3345) = small + (2.510e-08&
          *(T32)**(-5.000e-01))
    end if

    !D3O+ + O- -> D + O + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3346) = small + (3.760e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + CN- -> H + CN + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3347) = small + (2.510e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + CN- -> D + CN + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3348) = small + (1.250e-08&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + CN- -> H + CN + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3349) = small + (1.250e-08&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + CN- -> D + CN + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3350) = small + (2.510e-08&
          *(T32)**(-5.000e-01))
    end if

    !D3O+ + CN- -> D + CN + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3351) = small + (3.760e-08&
          *(T32)**(-5.000e-01))
    end if

    !H3O+ + OD- -> H + OH + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3352) = small + (1.880e-08&
          *(T32)**(-5.000e-01))
    end if

    !H3O+ + OD- -> H + OD + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3353) = small + (9.400e-09&
          *(T32)**(-5.000e-01))
    end if

    !H3O+ + OD- -> D + OH + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3354) = small + (9.400e-09&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + OH- -> H + OH + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3355) = small + (1.880e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + OH- -> H + OD + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3356) = small + (9.400e-09&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + OH- -> D + OH + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3357) = small + (9.400e-09&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + OD- -> H + OH + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3358) = small + (6.270e-09&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + OD- -> H + OD + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3359) = small + (1.250e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + OD- -> D + OH + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3360) = small + (1.250e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + OD- -> D + OD + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3361) = small + (6.270e-09&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + OH- -> H + OH + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3362) = small + (6.270e-09&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + OH- -> H + OD + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3363) = small + (1.250e-08&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + OH- -> D + OH + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3364) = small + (1.250e-08&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + OH- -> D + OD + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3365) = small + (6.270e-09&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + OD- -> H + OD + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3366) = small + (9.400e-09&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + OD- -> D + OH + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3367) = small + (9.400e-09&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + OD- -> D + OD + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3368) = small + (1.880e-08&
          *(T32)**(-5.000e-01))
    end if

    !D3O+ + OH- -> H + OD + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3369) = small + (9.400e-09&
          *(T32)**(-5.000e-01))
    end if

    !D3O+ + OH- -> D + OH + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3370) = small + (9.400e-09&
          *(T32)**(-5.000e-01))
    end if

    !D3O+ + OH- -> D + OD + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3371) = small + (1.880e-08&
          *(T32)**(-5.000e-01))
    end if

    !D3O+ + OD- -> D + OD + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3372) = small + (3.760e-08&
          *(T32)**(-5.000e-01))
    end if

    !H + H2DO+ -> H2_PARA + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3373) = small + (1.020e-10&
          *exp(-2.050e+04*invT))
    end if

    !H + H2DO+ -> H2_ORTHO + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3374) = small + (2.030e-10&
          *exp(-2.070e+04*invT))
    end if

    !H + H2DO+ -> HD + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3375) = small + (3.050e-10&
          *exp(-2.050e+04*invT))
    end if

    !H + HD2O+ -> H2_PARA + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3376) = small + (2.550e-11&
          *exp(-2.050e+04*invT))
    end if

    !H + HD2O+ -> H2_ORTHO + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3377) = small + (7.650e-11&
          *exp(-2.070e+04*invT))
    end if

    !H + HD2O+ -> HD + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3378) = small + (4.070e-10&
          *exp(-2.050e+04*invT))
    end if

    !H + HD2O+ -> D2_PARA + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3379) = small + (5.100e-11&
          *exp(-2.050e+04*invT))
    end if

    !H + HD2O+ -> D2_ORTHO + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3380) = small + (5.100e-11&
          *exp(-2.050e+04*invT))
    end if

    !H + D3O+ -> HD + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3381) = small + (3.050e-10&
          *exp(-2.050e+04*invT))
    end if

    !H + D3O+ -> D2_PARA + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3382) = small + (1.520e-10&
          *exp(-2.050e+04*invT))
    end if

    !H + D3O+ -> D2_ORTHO + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3383) = small + (1.520e-10&
          *exp(-2.050e+04*invT))
    end if

    !D + H3O+ -> H2_PARA + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3384) = small + (7.620e-11&
          *exp(-2.050e+04*invT))
    end if

    !D + H3O+ -> H2_ORTHO + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3385) = small + (2.290e-10&
          *exp(-2.070e+04*invT))
    end if

    !D + H3O+ -> HD + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3386) = small + (3.050e-10&
          *exp(-2.050e+04*invT))
    end if

    !D + H2DO+ -> H2_PARA + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3387) = small + (5.100e-11&
          *exp(-2.050e+04*invT))
    end if

    !D + H2DO+ -> H2_ORTHO + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3388) = small + (5.100e-11&
          *exp(-2.070e+04*invT))
    end if

    !D + H2DO+ -> HD + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3389) = small + (4.070e-10&
          *exp(-2.050e+04*invT))
    end if

    !D + H2DO+ -> D2_ORTHO + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3390) = small + (6.800e-11&
          *exp(-2.050e+04*invT))
    end if

    !D + H2DO+ -> D2_PARA + H2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3391) = small + (3.400e-11&
          *exp(-2.050e+04*invT))
    end if

    !D + HD2O+ -> HD + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3392) = small + (3.050e-10&
          *exp(-2.050e+04*invT))
    end if

    !D + HD2O+ -> D2_PARA + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3393) = small + (1.190e-10&
          *exp(-2.050e+04*invT))
    end if

    !D + HD2O+ -> D2_ORTHO + HDO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3394) = small + (1.860e-10&
          *exp(-2.050e+04*invT))
    end if

    !D + D3O+ -> D2_ORTHO + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3395) = small + (3.560e-10&
          *exp(-2.050e+04*invT))
    end if

    !D + D3O+ -> D2_PARA + D2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3396) = small + (2.540e-10&
          *exp(-2.050e+04*invT))
    end if

    !H2_PARA + H2DO+ -> H + H + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3397) = small + (1.800e-11&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !H2_ORTHO + H2DO+ -> H + H + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3398) = small + (1.800e-11&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !H2_PARA + H2DO+ -> H + D + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3399) = small + (1.200e-11&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !H2_ORTHO + H2DO+ -> H + D + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3400) = small + (1.200e-11&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !H2_PARA + HD2O+ -> H + H + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3401) = small + (9.000e-12&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !H2_ORTHO + HD2O+ -> H + H + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3402) = small + (9.000e-12&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !H2_PARA + HD2O+ -> H + D + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3403) = small + (1.800e-11&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !H2_ORTHO + HD2O+ -> H + D + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3404) = small + (1.800e-11&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !H2_PARA + HD2O+ -> D + D + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3405) = small + (3.000e-12&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !H2_ORTHO + HD2O+ -> D + D + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3406) = small + (3.000e-12&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !H2_ORTHO + D3O+ -> H + H + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3407) = small + (3.000e-12&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !H2_PARA + D3O+ -> H + H + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3408) = small + (3.000e-12&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !H2_ORTHO + D3O+ -> H + D + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3409) = small + (1.800e-11&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !H2_PARA + D3O+ -> H + D + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3410) = small + (1.800e-11&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !H2_ORTHO + D3O+ -> D + D + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3411) = small + (9.000e-12&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !H2_PARA + D3O+ -> D + D + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3412) = small + (9.000e-12&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !HD + H3O+ -> H + H + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3413) = small + (1.800e-11&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !HD + H3O+ -> H + D + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3414) = small + (1.200e-11&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !HD + H2DO+ -> H + H + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3415) = small + (9.000e-12&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !HD + H2DO+ -> H + D + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3416) = small + (1.800e-11&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !HD + H2DO+ -> D + D + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3417) = small + (3.000e-12&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !HD + HD2O+ -> H + H + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3418) = small + (3.000e-12&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !HD + HD2O+ -> H + D + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3419) = small + (1.800e-11&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !HD + HD2O+ -> D + D + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3420) = small + (9.000e-12&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !HD + D3O+ -> H + D + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3421) = small + (1.200e-11&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !HD + D3O+ -> D + D + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3422) = small + (1.800e-11&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !D2_PARA + H3O+ -> H + H + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3423) = small + (9.000e-12&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !D2_ORTHO + H3O+ -> H + H + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3424) = small + (9.000e-12&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !D2_PARA + H3O+ -> H + D + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3425) = small + (1.800e-11&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !D2_ORTHO + H3O+ -> H + D + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3426) = small + (1.800e-11&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !D2_PARA + H3O+ -> D + D + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3427) = small + (3.000e-12&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !D2_ORTHO + H3O+ -> D + D + H3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3428) = small + (3.000e-12&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !D2_ORTHO + H2DO+ -> H + H + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3429) = small + (3.000e-12&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !D2_PARA + H2DO+ -> H + H + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3430) = small + (3.000e-12&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !D2_ORTHO + H2DO+ -> H + D + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3431) = small + (1.800e-11&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !D2_PARA + H2DO+ -> H + D + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3432) = small + (1.800e-11&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !D2_ORTHO + H2DO+ -> D + D + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3433) = small + (9.000e-12&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !D2_PARA + H2DO+ -> D + D + H2DO+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3434) = small + (9.000e-12&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !D2_ORTHO + HD2O+ -> H + D + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3435) = small + (1.200e-11&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !D2_PARA + HD2O+ -> H + D + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3436) = small + (1.200e-11&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !D2_ORTHO + HD2O+ -> D + D + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3437) = small + (1.800e-11&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !D2_PARA + HD2O+ -> D + D + HD2O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3438) = small + (1.800e-11&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !D2_ORTHO + D3O+ -> D + D + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3439) = small + (3.000e-11&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !D2_PARA + D3O+ -> D + D + D3O+
    if(Tgas.GE.10d0 .and. Tgas.LT.800d0) then
      k(3440) = small + (3.000e-11&
          *(T32)**(5.000e-01)*exp(-5.200e+04*invT))
    end if

    !H2DO+ + E -> H + H + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3441) = small + (8.670e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + E -> H + D + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3442) = small + (1.730e-07&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + E -> H + D + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3443) = small + (1.730e-07&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + E -> D + D + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3444) = small + (8.670e-08&
          *(T32)**(-5.000e-01))
    end if

    !D3O+ + E -> D + D + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3445) = small + (2.600e-07&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + E -> H + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3446) = small + (7.330e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + E -> D + H2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3447) = small + (3.670e-08&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + E -> H + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3448) = small + (3.670e-08&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + E -> D + HDO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3449) = small + (7.330e-08&
          *(T32)**(-5.000e-01))
    end if

    !D3O+ + E -> D + D2O
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3450) = small + (1.100e-07&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + E -> H2_PARA + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3451) = small + (1.000e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + E -> H2_ORTHO + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3452) = small + (1.000e-08&
          *(T32)**(-5.000e-01)*exp(-1.700e+02*invT))
    end if

    !H2DO+ + E -> HD + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3453) = small + (4.000e-08&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + E -> HD + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3454) = small + (4.000e-08&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + E -> D2_PARA + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3455) = small + (1.000e-08&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + E -> D2_ORTHO + OH
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3456) = small + (1.000e-08&
          *(T32)**(-5.000e-01))
    end if

    !D3O+ + E -> D2_PARA + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3457) = small + (3.000e-08&
          *(T32)**(-5.000e-01))
    end if

    !D3O+ + E -> D2_ORTHO + OD
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3458) = small + (3.000e-08&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + E -> H + O + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3459) = small + (3.730e-09&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + E -> D + O + H2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3460) = small + (9.350e-10&
          *(T32)**(-5.000e-01))
    end if

    !H2DO+ + E -> D + O + H2_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3461) = small + (9.350e-10&
          *(T32)**(-5.000e-01)*exp(-1.700e+02*invT))
    end if

    !HD2O+ + E -> H + O + D2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3462) = small + (9.350e-10&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + E -> H + O + D2_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3463) = small + (9.350e-10&
          *(T32)**(-5.000e-01))
    end if

    !HD2O+ + E -> D + O + HD
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3464) = small + (3.730e-09&
          *(T32)**(-5.000e-01))
    end if

    !D3O+ + E -> D + O + D2_PARA
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3465) = small + (2.800e-09&
          *(T32)**(-5.000e-01))
    end if

    !D3O+ + E -> D + O + D2_ORTHO
    if(Tgas.GE.10d0 .and. Tgas.LT.300d0) then
      k(3466) = small + (2.800e-09&
          *(T32)**(-5.000e-01))
    end if

    coe(:) = k(:) !set coefficients to return variable

    !!uncomment below to check coefficient values
    !kmax = 1d0
    !if(maxval(k)>kmax.or.minval(k)<0d0) then
    !   print *,"***************"
    !   do i=1,size(k)
    !      if(k(i)<0d0.or.k(i)>kmax) print *,i,k(i)
    !   end do
    !end if
  end function coe

  !*************************
  subroutine loadReactionsVerbatim()
    use krome_commons
    implicit none
    character*50::fname,line
    integer::ios,i,nunit

    fname = "reactions_verbatim.dat"

    !verbatim reactions are loaded from file
    ! to increase compilation speed
    open(newunit=nunit,file=trim(fname),status="old",iostat=ios)
    if(ios/=0) then
      print *,"ERROR: "//trim(fname)//" file not present!"
      stop
    end if

    !load reactions from file
    do i=1,nrea
      read(nunit,'(a)',iostat=ios) line
      if(ios/=0) then
        print *,"ERROR: problem reading "//trim(fname)
        stop
      end if
      reactionNames(i) = trim(line)
    end do
    close(nunit)

  end subroutine loadReactionsVerbatim

  !*******************
  !The following functions compute the recombination rate
  ! on dust for H+, He+, C+, Si+, and O+. See Weingartner&Draine 2001
  ! dust2gas_ratio, D/D_sol, default is assumed equal to Z/Z_sol
  function H_recombination_on_dust(n,Tgas)
    use krome_commons
    implicit none
    real*8::n(nspec),Tgas,psi
    real*8::H_recombination_on_dust

    H_recombination_on_dust = 0d0

    if(n(idx_E)<1d-20.or.GHabing<=0.d0) return

    psi = GHabing*sqrt(Tgas)/n(idx_E)

    if(psi<=0) return

    H_recombination_on_dust =  1.225d-13*dust2gas_ratio &
        /(1.d0+8.074d-6*psi**(1.378)*(1.d0+5.087d2 &
        *Tgas**(0.01586)*psi**(-0.4723-1.102d-5*log(Tgas))))

  end function H_recombination_on_dust

  !******************
  function He_recombination_on_dust(n,Tgas)
    use krome_commons
    implicit none
    real*8::n(nspec),Tgas,psi
    real*8::He_recombination_on_dust

    He_recombination_on_dust = 0d0
    if(n(idx_E)<1d-20.or.GHabing<=0.d0) return

    psi = GHabing*sqrt(Tgas)/n(idx_E)

    if(psi<=0) return

    He_recombination_on_dust = 5.572d-14*dust2gas_ratio&
        /(1.d0+3.185d-7*psi**(1.512)*(1.d0+5.115d3&
        *Tgas**(3.903d-7)*psi**(-0.4956-5.494d-7*log(Tgas))))

  end function He_recombination_on_dust

  !*******************
  function C_recombination_on_dust(n,Tgas)
    use krome_commons
    implicit none
    real*8::n(nspec),Tgas,psi
    real*8::C_recombination_on_dust

    C_recombination_on_dust = 0d0
    if(n(idx_E)<1d-20.or.GHabing<=0.d0) return

    psi = GHabing*sqrt(Tgas)/n(idx_E)

    if(psi<=0) return

    C_recombination_on_dust = 4.558d-13*dust2gas_ratio&
        /(1.d0+6.089d-3*psi**(1.128)*(1.d0+4.331d2&
        *Tgas**(0.04845)*psi**(-0.8120-1.333d-4*log(Tgas))))

  end function C_recombination_on_dust

  !******************
  function Si_recombination_on_dust(n,Tgas)
    use krome_commons
    implicit none
    real*8::n(nspec),Tgas,psi
    real*8::Si_recombination_on_dust

    Si_recombination_on_dust = 0d0
    if(n(idx_E)<1d-20.or.GHabing<=0.d0) return

    psi = GHabing*sqrt(Tgas)/n(idx_E)

    if(psi<=0) return

    Si_recombination_on_dust = 2.166d-14*dust2gas_ratio&
        /(1.d0+5.678d-8*psi**(1.874)*(1.d0+4.375d4&
        *Tgas**(1.635d-6)*psi**(-0.8964-7.538d-5*log(Tgas))))

  end function Si_recombination_on_dust

  !********************
  function O_recombination_on_dust(n,Tgas)
    use krome_commons
    implicit none
    real*8::n(nspec),Tgas,k_H
    real*8::O_recombination_on_dust

    k_H = H_recombination_on_dust(n(:),Tgas)
    O_recombination_on_dust = 0.25d0*k_H

  end function O_recombination_on_dust

  !*********************
  !This function returns the
  ! photorate of H2 occurring in the
  ! Lyman-Werner bands following the approximation
  ! provided by Glover&Jappsen 2007. Rate in 1/s.
  !Approximation valid at low-density, it assumes H2(nu = 0).
  !It also stores the rate as a common, needed for the photoheating
  function H2_solomonLW(myflux)
    use krome_commons
    use krome_constants
    implicit none
    real*8::H2_solomonLW,myflux

    !myflux is the radiation background at E = 12.87 eV
    !should be converted to erg
    H2_solomonLW = 1.38d9*myflux*eV_to_erg

  end function H2_solomonLW

  !****************************
  !tanh smoothing function that
  ! increses when xarg increases.
  ! xpos is the position of the transition point.
  ! slope is the steepness of the curve.
  function smooth_increase(xarg,xpos,slope)
    implicit none
    real*8::smooth_increase,xarg,xpos,slope

    smooth_increase = .5d0 * (tanh(slope * (xarg - xpos)) &
        + 1d0)

  end function smooth_increase

  !****************************
  !tanh smoothing function that
  ! decreses when xarg increases.
  ! xpos is the position of the transition point.
  ! slope is the steepness of the curve.
  function smooth_decrease(xarg,xpos,slope)
    implicit none
    real*8::smooth_decrease,xarg,xpos,slope

    smooth_decrease = .5d0 * (tanh(-slope * (xarg - xpos)) &
        + 1d0)

  end function smooth_decrease

  !*********************
  !sign: return 1d0 if x>=0d0,
  ! else return -1d0
  function get_sgn(x)
    implicit none
    real*8::x,get_sgn

    get_sgn = 1d0
    if(x==0d0) return
    get_sgn = x/abs(x)

  end function get_sgn

  !*********************
  function conserve(n,ni)
    use krome_commons
    implicit none
    real*8::conserve(nspec),n(nspec),ni(nspec),no(nspec)
    real*8::ntot,nitot,factor

    no(:) = n(:)

    conserve(:) = 0d0
    conserve(:) = no(:)

  end function conserve

  !*************************
  !this subroutine changes the x(:) mass fractions of the species
  ! to force conservation according to the reference ref(:)
  subroutine conserveLin_x(x,ref)
    use krome_commons
    use krome_getphys
    implicit none
    real*8::x(nmols),ref(natoms)
    real*8::A(natoms,natoms),B(natoms),m(nspec)

    m(:) = get_mass()
    A(:,:) = 0d0
    B(:) = ref(:)

    !charge conservation
    x(idx_E) = m(idx_E)*(- 1d0*x(idx_GRAINk) / m(idx_GRAINk) &
        - 1d0*x(idx_Hk) / m(idx_Hk) &
        - 1d0*x(idx_Dk) / m(idx_Dk) &
        - 1d0*x(idx_Ck) / m(idx_Ck) &
        - 1d0*x(idx_Ok) / m(idx_Ok) &
        - 1d0*x(idx_CNk) / m(idx_CNk) &
        - 1d0*x(idx_OHk) / m(idx_OHk) &
        - 1d0*x(idx_ODk) / m(idx_ODk) &
        + 1d0*x(idx_H3j_PARA) / m(idx_H3j_PARA) &
        + 1d0*x(idx_H3j_ORTHO) / m(idx_H3j_ORTHO) &
        + 1d0*x(idx_H2Dj_PARA) / m(idx_H2Dj_PARA) &
        + 1d0*x(idx_H2Dj_ORTHO) / m(idx_H2Dj_ORTHO) &
        + 1d0*x(idx_D2Hj_PARA) / m(idx_D2Hj_PARA) &
        + 1d0*x(idx_D2Hj_ORTHO) / m(idx_D2Hj_ORTHO) &
        + 1d0*x(idx_D3j_ORTHO) / m(idx_D3j_ORTHO) &
        + 1d0*x(idx_D3j_META) / m(idx_D3j_META) &
        + 1d0*x(idx_Hj) / m(idx_Hj) &
        + 1d0*x(idx_HDj) / m(idx_HDj) &
        + 1d0*x(idx_D2j_ORTHO) / m(idx_D2j_ORTHO) &
        + 1d0*x(idx_D2j_PARA) / m(idx_D2j_PARA) &
        + 1d0*x(idx_HEj) / m(idx_HEj) &
        + 1d0*x(idx_HCOj) / m(idx_HCOj) &
        + 1d0*x(idx_DCOj) / m(idx_DCOj) &
        + 1d0*x(idx_Cj) / m(idx_Cj) &
        + 1d0*x(idx_Dj) / m(idx_Dj) &
        + 1d0*x(idx_Nj) / m(idx_Nj) &
        + 1d0*x(idx_Oj) / m(idx_Oj) &
        + 1d0*x(idx_H2j_PARA) / m(idx_H2j_PARA) &
        + 1d0*x(idx_H2j_ORTHO) / m(idx_H2j_ORTHO) &
        + 1d0*x(idx_CHj) / m(idx_CHj) &
        + 1d0*x(idx_CDj) / m(idx_CDj) &
        + 1d0*x(idx_NOj) / m(idx_NOj) &
        + 1d0*x(idx_O2j) / m(idx_O2j) &
        + 1d0*x(idx_CH2j) / m(idx_CH2j) &
        + 1d0*x(idx_CD2j) / m(idx_CD2j) &
        + 1d0*x(idx_CHDj) / m(idx_CHDj) &
        + 1d0*x(idx_HNOj) / m(idx_HNOj) &
        + 1d0*x(idx_DNOj) / m(idx_DNOj) &
        + 1d0*x(idx_NH2j) / m(idx_NH2j) &
        + 1d0*x(idx_ND2j) / m(idx_ND2j) &
        + 1d0*x(idx_NHDj) / m(idx_NHDj) &
        + 1d0*x(idx_COj) / m(idx_COj) &
        + 1d0*x(idx_C2j) / m(idx_C2j) &
        + 1d0*x(idx_OHj) / m(idx_OHj) &
        + 1d0*x(idx_ODj) / m(idx_ODj) &
        + 1d0*x(idx_C2Hj) / m(idx_C2Hj) &
        + 1d0*x(idx_C2Dj) / m(idx_C2Dj) &
        + 1d0*x(idx_NHj) / m(idx_NHj) &
        + 1d0*x(idx_NDj) / m(idx_NDj) &
        + 1d0*x(idx_H2Oj) / m(idx_H2Oj) &
        + 1d0*x(idx_D2Oj) / m(idx_D2Oj) &
        + 1d0*x(idx_HDOj) / m(idx_HDOj) &
        + 1d0*x(idx_CNj) / m(idx_CNj) &
        + 1d0*x(idx_C3j) / m(idx_C3j) &
        + 1d0*x(idx_C2Oj) / m(idx_C2Oj) &
        + 1d0*x(idx_HOCj) / m(idx_HOCj) &
        + 1d0*x(idx_DOCj) / m(idx_DOCj) &
        + 1d0*x(idx_C2Nj) / m(idx_C2Nj) &
        + 1d0*x(idx_CNCj) / m(idx_CNCj) &
        + 1d0*x(idx_HCNj) / m(idx_HCNj) &
        + 1d0*x(idx_DCNj) / m(idx_DCNj) &
        + 1d0*x(idx_HNCj) / m(idx_HNCj) &
        + 1d0*x(idx_DNCj) / m(idx_DNCj) &
        + 1d0*x(idx_NCOj) / m(idx_NCOj) &
        + 1d0*x(idx_N2Hj) / m(idx_N2Hj) &
        + 1d0*x(idx_N2Dj) / m(idx_N2Dj) &
        + 1d0*x(idx_O2Hj) / m(idx_O2Hj) &
        + 1d0*x(idx_O2Dj) / m(idx_O2Dj) &
        + 1d0*x(idx_HEHj) / m(idx_HEHj) &
        + 1d0*x(idx_HEDj) / m(idx_HEDj) &
        + 1d0*x(idx_N2j) / m(idx_N2j) &
        + 1d0*x(idx_CO2j) / m(idx_CO2j) &
        + 1d0*x(idx_NO2j) / m(idx_NO2j) &
        + 1d0*x(idx_D3j_PARA) / m(idx_D3j_PARA) &
        + 1d0*x(idx_H3Oj) / m(idx_H3Oj) &
        + 1d0*x(idx_H2DOj) / m(idx_H2DOj) &
        + 1d0*x(idx_HD2Oj) / m(idx_HD2Oj) &
        + 1d0*x(idx_D3Oj) / m(idx_D3Oj))
    !check if charge conservation goes wrong
    if(x(idx_E)<0d0) then
      print *,"ERROR in conserveLin, electrons < 0"
      stop
    end if

  end subroutine conserveLin_x

  !***************************
  !compute the total reference mass atom type by atom type
  function conserveLinGetRef_x(x)
    use krome_commons
    use krome_getphys
    implicit none
    real*8::conserveLinGetRef_x(natoms),x(nmols)
    real*8::m(nspec)

    m(:) = get_mass()
    conserveLinGetRef_x(:) = 0d0

  end function conserveLinGetRef_x

  !***************************
  !Ref: Sasaki & Takahara (1993)
  !This function evaluate the recombination rate
  ! for H+ + e --> H + gamma and the same
  ! for D+ + e --> D + gamma
  function elec_recomb_ST93(nabund,nelec,ntot,nucleiH,Trad)
    use krome_commons
    use krome_constants
    implicit none
    real*8::nabund,nelec,Trad
    real*8::nucleiH,elec_recomb_ST93
    real*8::al,ak,rc2,r2c
    real*8::a0,b0,c0,d0,e0
    real*8::a1,b1,c1,d1,e1,f1,g1,h1
    real*8::ntot,ratio

    al = 8.227d0
    ak = 22.06d0 / (hubble  *(1d0 + phys_zredshift) &
        * sqrt(1d0 + Omega0 * phys_zredshift))
    !Rc2 evaluation
    rc2 = 8.76d-11 * (1d0 + phys_zredshift)**(-0.58)
    !R2c evaluation
    r2c = (1.80d10 * Trad)**(1.5) &
        * exp(-3.9472d4 / Trad) * rc2

    !coefficients
    a0 = nucleiH * rc2
    b0 = ak * al * nucleiH
    c0 = ak * rc2 * nucleiH * nucleiH
    d0 = r2c * exp(-1.18416d5/Trad)
    e0 = ak * r2c * nucleiH

    !polynomial terms
    a1 = -d0 * (1d0 + b0)
    b1 = d0 * (1d0 + 2d0 * b0)
    c1 = a0 + b0 * (a0 - d0)
    d1 = -a0 * b0
    e1 = a0 * c0
    f1 = 1d0 + b0 + e0
    g1 = -(b0 + e0)
    h1 = c0

    ratio = nabund / ntot

    elec_recomb_ST93 = ntot*(a1 + b1*ratio + c1*ratio**2 + d1*ratio**3 &
        + e1*ratio**4) / (f1 + g1*ratio + h1*ratio**2)

    elec_recomb_ST93 = elec_recomb_ST93 / (nabund * nelec)

  end function elec_recomb_ST93

  !********************
  subroutine load_parts()
    use krome_commons
    implicit none

  end subroutine load_parts

  !*************************
  subroutine load_part(fname,array_part,min_part,dT_part)
    character(len=*)::fname
    integer::ios,icount,i,cv
    real*8,allocatable::array_part(:),emed(:)
    real*8::min_part,dT_part,Told,array_tmp(int(1e5)),rout(2)

    open(33,file=trim(fname),status="old",iostat=ios)
    if(ios.ne.0) then
      print *,"ERROR: partition function not found"
      print *," in file "//fname
      stop
    end if

    print *,"loading partition function from "//fname
    icount = 0
    min_part = 1d99
    Told = 0d0
    do
      read(33,*,iostat=ios) rout(:)
      if(ios<0) exit
      if(ios.ne.0) cycle
      icount = icount + 1
      min_part = min(min_part,rout(1))
      array_tmp(icount) = rout(2)
      dT_part = rout(1) - Told
      Told = rout(1)
    end do
    close(33)

    allocate(array_part(icount),emed(icount))
    array_part(:) = array_tmp(1:icount)

  end subroutine load_part

  !**********************
  function troe_falloff(k0,kinf,Fc,m)
    implicit none
    real*8::troe_falloff,k0,kinf,Fc,m,rm,xexp
    rm = k0*m/kinf
    xexp = 1d0/(1d0+log10(rm)**2)
    troe_falloff = k0*m/(1d0+rm)*Fc**xexp
  end function troe_falloff

  !*************************
  function k3body(k0,kinf,Fc,nM)
    implicit none
    real*8::k3body,k0,kinf,Fc,nM
    real*8::c,n,d,Pr,xexp,F

    c = -0.4d0-0.67d0*log10(Fc)
    n = 0.75d0-1.27d0*log10(Fc)
    d = 0.14d0
    Pr = k0*nM/kinf
    xexp = (log10(Pr)+c)/(n-d*(log10(Pr)+c))
    F = 1d1**(log10(Fc)/(1d0+xexp**2))
    k3body = kinf*(Pr/(1d0+Pr)) * F

  end function k3body

  !***********************
  !see http://kida.obs.u-bordeaux1.fr/help
  function KIDA3body(ka0,kb0,kc0,kaInf,kbInf,kcInf,kaFc,kbFc,&
        kcFc,kdFc,npart,Tgas,pmin,pmax)
    implicit none
    real*8::ka0,kb0,kc0,kaInf,kbInf,kcInf,kaFc,kbFc,kcFc,kdFc
    real*8::KIDA3body,kinf,p,f,npart,Tgas,fc,fexp,invT
    real*8::k0,cc,dd,nn,pmin,pmax

    KIDA3body = 0d0

    invT = 1d0/Tgas
    k0 = ka0*(Tgas/3d2)**kb0*exp(-kc0*invT)
    kinf = kainf*(Tgas/3d2)**kbinf*exp(-kcinf*invT)

    p = k0*npart/kinf
    if(p<pmin) return
    if(p>pmax) return

    fc = (1d0-kaFc)*exp(-Tgas/kbFc) + kaFc*exp(-Tgas/kbFc) &
        + exp(-kdFc*invT)

    cc = -0.4d0 - 0.67d0 *log10(fc)
    dd = 0.14d0
    nn = 0.75d0 - 1.27d0*log10(fc)
    fexp = 1d0 + ((log10(p)+cc)/(nn-dd*(log10(p)+cc)))**2

    f = fc**(1d0/fexp)

    KIDA3body = kinf*(p/(1d0+p))*f

  end function KIDA3body

  !******************************
  !collisional ionization rate from Verner+96
  ! unit: cm3/s
  function colion_v96(Tgas,dE,P,A,X,K)
    implicit none
    real*8::colion_v96,Tgas,dE,A,X,K,U,Te,P

    Te = Tgas * 8.621738d-5 !K to eV
    U = dE / Te
    colion_v96 = A * (1d0 + P*sqrt(U)) * U**K * exp(-U) / (X+U)

  end function colion_v96

  !****************************
  !radiative recombination rates from
  ! Verner routine, standard fit, cm3/s
  function recV96(Tgas,a,b)
    implicit none
    real*8::recV96,Tgas,a,b

    recV96 = a*(1d4/Tgas)**b

  end function recV96

  !****************************
  !radiative recombination rates from
  ! Verner routine, new fit, cm3/s
  function recNewV96(Tgas,r1,r2,r3,r4)
    implicit none
    real*8::recNewV96,Tgas,r1,r2,r3,r4,tt

    tt = sqrt(Tgas/r3)
    recNewV96 = r1/(tt*(tt + 1d0)**(1.-r2) &
        * (1d0 + sqrt(Tgas/r4))**(1.+r2))

  end function recNewV96

  !****************************
  !radiative recombination rates from
  ! Verner routine, iron only, cm3/s
  function recFeV96(Tgas,r1,r2,r3)
    implicit none
    real*8::recFeV96,Tgas,r1,r2,r3,tt

    tt = sqrt(Tgas*1d-4)
    recFeV96 = r1/tt**(r2 + r3 + log10(tt))

  end function recFeV96

  !******************************
  !radiative recombination rates from Verner+96
  ! unit: cm3/s
  function radrec_v96(Tgas,a,b,T0,T1)
    implicit none
    real*8::Tgas,a,b,T0,T1,radrec_v96,iT0

    iT0 = 1d0/T0
    radrec_v96 = a/(sqrt(Tgas*iT0) + (1d0*sqrt(Tgas*iT0))**(1.-b) &
        * (1d0+sqrt(Tgas/T1))**(1+b))

  end function radrec_v96

  !*******************************
  !radiative recombination rates low-temp fit, Verner+96
  ! unit: cm3/s
  function radrec_low_v96(Tgas,a,b,c,d,f)
    implicit none
    real*8::Tgas,a,b,c,d,f,radrec_low_v96,t,invt

    t = Tgas*1d-4
    invt = 1d0/t

    radrec_low_v96 = 1d-12 * (a*invt + b + c*t + d*t**2) &
        * t**(-1.5) * exp(-f*invt)

    radrec_low_v96 = max(0d0,radrec_low_v96)

  end function radrec_low_v96

  !***************************
  !Collisional dissociation rate (cm-3/s) by Martin et al. 1996
  ! H2+H->H+H+H
  !NOTE: the use of this rate is suggested
  ! for high-density regime and in the presence of UV backgrounds.
  ! if necessary it must be included in the reaction file as
  ! H2,H,,H,H,H,,NONE,NONE,dissH2_Martin96(n,Tgas)
  function dissH2_Martin96(n,Tgas)
    use krome_commons
    use krome_getphys
    integer::i
    real*8::n(nspec),Tgas,dissH2_Martin96
    real*8::CDrates,logTv(4),k_CIDm(21,2),k_CID,invT,logT,n_c1,n_c2,n_H
    real*8::logk_h1,logk_h2,logk_l1,logk_l2,logn_c1,logn_c2,p,logk_CID
    real*8::logT2,logT3

    !k_CID = collision-induced dissociation + dissociative tunneling

    !Collisional dissociation of H2
    k_CIDm(:,1) = (/-178.4239d0, -68.42243d0, 43.20243d0, -4.633167d0, &
        69.70086d0, 40870.38d0, -23705.70d0, 128.8953d0, -53.91334d0, &
        5.315517d0, -19.73427d0, 16780.95d0, -25786.11d0, 14.82123d0, &
        -4.890915d0, 0.4749030d0, -133.8283d0, -1.164408d0, 0.8227443d0,&
        0.5864073d0, -2.056313d0/)

    !Dissociative tunneling of H2
    k_CIDm(:,2) = (/-142.7664d0, 42.70741d0, -2.027365d0, -0.2582097d0, &
        21.36094d0, 27535.31d0, -21467.79d0, 60.34928d0, -27.43096d0, &
        2.676150d0, -11.28215d0, 14254.55d0, -23125.20d0, 9.305564d0, &
        -2.464009d0, 0.1985955d0, 743.0600d0, -1.174242d0, 0.7502286d0, &
        0.2358848d0, 2.937507d0/)

    n_H  = get_Hnuclei(n(:))
    logT = log10(Tgas)
    invT = 1.0d0/Tgas
    logT2 = logT*logT
    logT3 = logT2*logT
    logTv = (/1.d0, logT, logT2, logT3/)
    k_CID = 0.d0
    do i=1,2
      logk_h1 = k_CIDm(1,i)*logTv(1) + k_CIDm(2,i)*logTv(2) + &
          k_CIDm(3,i)*logTv(3) + k_CIDm(4,i)*logTv(4) + &
          k_CIDm(5,i)*log10(1.d0+k_CIDm(6,i)*invT)
      logk_h2 = k_CIDm(7,i)*invT
      logk_l1 = k_CIDm(8,i)*logTv(1) + k_CIDm(9,i)*logTv(2) + &
          k_CIDm(10,i)*logTv(3) + k_CIDm(11,i)*log10(1.d0+k_CIDm(12,i)*invT)
      logk_l2 = k_CIDm(13,i)*invT
      logn_c1 = k_CIDm(14,i)*logTv(1) + k_CIDm(15,i)*logTv(2) &
          + k_CIDm(16,i)*logTv(3) + k_CIDm(17,i)*invT
      logn_c2 = k_CIDm(18,i) + logn_c1
      p = k_CIDm(19,i) + k_CIDm(20,i)*exp(-Tgas/1.850d3) &
          + k_CIDm(21,i)*exp(-Tgas/4.40d2)
      n_c1 = 1d1**(logn_c1)
      n_c2 = 1d1**(logn_c2)
      logk_CID = logk_h1 - (logk_h1 - logk_l1) / (1.d0 + (n_H/n_c1)**p) &
          + logk_h2 - (logk_h2 - logk_l2) / (1.d0 + (n_H/n_c2)**p)
      k_CID = k_CID + 1.d1**logk_CID
    enddo

    dissH2_Martin96 = k_CID

  end function dissH2_Martin96

  !***********************************
  subroutine init_exp_table()
    use krome_commons
    implicit none
    integer::i
    real*8::a

    do i=1,exp_table_na
      a = (i-1)*(exp_table_aMax-exp_table_aMin)/(exp_table_na-1) + exp_table_aMin
      exp_table(i) = exp(-a)
    end do

  end subroutine init_exp_table

  !***************************
  !get the index of the specie name
  function get_index(name)
    use krome_commons
    use krome_getphys
    integer::get_index,i
    character*16::names(nspec)
    character*(*)::name
    names(:) = get_names()
    get_index = -1 !default index
    !loop on species to found the specie named name
    do i=1,nspec
      !when found store and break loop
      if(trim(names(i))== trim(name)) then
        get_index = i !store index
        exit
      end if
    end do

    !error if species not found
    if(get_index<0) then
      print *,"ERROR: can't find the index of ",name
      stop
    end if

  end function get_index

  !*****************************
  !computes revers kinetics from reaction and
  ! product indexes
  ! k_rev = k_for * revKc
  ! Note that reaction constant revKc is calculated with
  ! reactants and products from reverse reaction
  function revKc(Tgas,ridx,pidx)
    use krome_constants
    use krome_commons
    implicit none
    real*8::revKc,Tgas,dgibss,stoichiometricChange
    integer::ridx(:),pidx(:),i

    ! when considering forward reaction:
    ! Kc = (P°)**(p+p-r-r) * exp(-dGibss_forward°)
    ! where ° means at standard conditions of
    ! P° = 1 bar = (kb*T/1e6) dyn/cm^2 (cgs)
    ! when considering reverse:
    ! 1/Kc = revKc = (kb*T/1e6)**(p+p-r-r) * exp(-dGibss_reverse°)
    ! kb*T/1e6 is to go from 1 atm pressure to number density cm^-3
    ! When not at standard pressure this does not change:
    ! revKc = P**(p+p-r-r) *exp(-dGibss_reverse° - (p+p-r-r)*ln(P/P°))
    !       = (P°)**(p+p-r-r) * exp(-dGibss_reverse°)

    dgibss = 0.d0 ! Gibbs free energy/(R*T)
    stoichiometricChange = 0d0

    do i=1,size(pidx)
      dgibss = dgibss + revHS(Tgas,pidx(i))
      stoichiometricChange = stoichiometricChange + 1
    end do

    do i=1,size(ridx)
      dgibss = dgibss - revHS(Tgas,ridx(i))
      stoichiometricChange = stoichiometricChange - 1
    end do

    revKc = (boltzmann_erg * Tgas * 1e-6)**(-stoichiometricChange)&
        * exp(-dgibss)

  end function revKc

  !*****************************
  !compute H-S for species with index idx
  ! when temperature is Tgas
  function revHS(Tgas,idx)
    use krome_commons
    use krome_constants
    use krome_fit
    real*8::revHS,Tgas,Tgas2,Tgas3,Tgas4,invT,lnT,H,S
    real*8::Tnist,Tnist2,Tnist3,Tnist4,invTnist,invTnist2,lnTnist
    real*8::p1_nasa(135,7), p2_nasa(135,7), Tlim_nasa(135,3), p(7)
    real*8::p1_nist(135,7), p2_nist(135,7), Tlim_nist(135,3)
    integer::idx

    p(:) = 0.d0
    p1_nasa(:,:) = 0.d0
    p2_nasa(:,:) = 0.d0
    Tlim_nasa(:,:) = 0.d0
    p1_nist(:,:) = 0.d0
    p2_nist(:,:) = 0.d0
    Tlim_nist(:,:) = 0.d0
    Tgas2 = Tgas * Tgas
    Tgas3 = Tgas2 * Tgas
    Tgas4 = Tgas3 * Tgas
    invT = 1d0/Tgas
    lnT = log(Tgas)
    ! NIST polynomials are quite differernt
    ! it doesn't like easy stuff...
    Tnist = Tgas * 1.d-3
    Tnist2 = Tnist * Tnist
    Tnist3 = Tnist2 * Tnist
    Tnist4 = Tnist3 * Tnist2
    invTnist = 1d0/Tnist
    invTnist2 = invTnist * invTnist
    lnTnist = log(Tnist)

    p1_nasa(idx_Hk,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        15976.167d0,&
        -1.1390139d0/)
    p1_nasa(idx_Dk,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        17907.5582d0,&
        -0.101024344d0/)
    p1_nasa(idx_Ck,:)  = (/2.50025151d0,&
        -1.19774349d-06,&
        2.28919443d-09,&
        -1.98276803d-12,&
        6.44398056d-16,&
        70064.893d0,&
        4.87847086d0/)
    p1_nasa(idx_Ok,:)  = (/2.90805921d0,&
        -0.00169804907d0,&
        2.98069955d-06,&
        -2.43835127d-09,&
        7.61229311d-13,&
        11435.7717d0,&
        2.80339097d0/)
    p1_nasa(idx_OHk,:)  = (/3.43126659d0,&
        0.000631146866d0,&
        -1.92914359d-06,&
        2.40618712d-09,&
        -8.66679361d-13,&
        -18508.5918d0,&
        1.07990541d0/)
    p1_nasa(idx_HD,:)  = (/3.43752369d0,&
        0.000617471555d0,&
        -1.85267846d-06,&
        2.32581486d-09,&
        -8.35140695d-13,&
        -17.7564616d0,&
        -2.41112115d0/)
    p1_nasa(idx_H,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        25473.66d0,&
        -0.44668285d0/)
    p1_nasa(idx_D,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        25921.2596d0,&
        0.591714338d0/)
    p1_nasa(idx_HE,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        -745.375d0,&
        0.928723974d0/)
    p1_nasa(idx_CO,:)  = (/3.5795335d0,&
        -0.00061035369d0,&
        1.0168143d-06,&
        9.0700586d-10,&
        -9.0442449d-13,&
        -14344.086d0,&
        3.5084093d0/)
    p1_nasa(idx_C,:)  = (/2.5542395d0,&
        -0.00032153772d0,&
        7.3379223d-07,&
        -7.3223487d-10,&
        2.6652144d-13,&
        85442.681d0,&
        4.5313085d0/)
    p1_nasa(idx_N,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        56104.638d0,&
        4.1939088d0/)
    p1_nasa(idx_O,:)  = (/3.1682671d0,&
        -0.00327931884d0,&
        6.64306396d-06,&
        -6.12806624d-09,&
        2.11265971d-12,&
        29122.2592d0,&
        2.05193346d0/)
    p1_nasa(idx_N2,:)  = (/3.53100528d0,&
        -0.000123660988d0,&
        -5.02999433d-07,&
        2.43530612d-09,&
        -1.40881235d-12,&
        -1046.97628d0,&
        2.96747038d0/)
    p1_nasa(idx_NO,:)  = (/4.21859896d0,&
        -0.00463988124d0,&
        1.10443049d-05,&
        -9.34055507d-09,&
        2.80554874d-12,&
        9845.09964d0,&
        2.28061001d0/)
    p1_nasa(idx_O2,:)  = (/3.78245636d0,&
        -0.00299673416d0,&
        9.84730201d-06,&
        -9.68129509d-09,&
        3.24372837d-12,&
        -1063.94356d0,&
        3.65767573d0/)
    p1_nasa(idx_OH,:)  = (/3.99198424d0,&
        -0.00240106655d0,&
        4.61664033d-06,&
        -3.87916306d-09,&
        1.36319502d-12,&
        3368.89836d0,&
        -0.103998477d0/)
    p1_nasa(idx_CH2,:)  = (/3.84261832d0,&
        -7.36676871d-06,&
        6.16970693d-06,&
        -6.96689962d-09,&
        2.64620979d-12,&
        45863.1528d0,&
        1.2758447d0/)
    p1_nasa(idx_CO2,:)  = (/2.356813d0,&
        0.0089841299d0,&
        -7.1220632d-06,&
        2.4573008d-09,&
        -1.4288548d-13,&
        -48371.971d0,&
        9.9009035d0/)
    p1_nasa(idx_H2O,:)  = (/4.1986352d0,&
        -0.0020364017d0,&
        6.5203416d-06,&
        -5.4879269d-09,&
        1.771968d-12,&
        -30293.726d0,&
        -0.84900901d0/)
    p1_nasa(idx_HCO,:)  = (/4.36380907d0,&
        -0.00535204137d0,&
        2.31954508d-05,&
        -2.6610904d-08,&
        1.02711962d-11,&
        25010.8717d0,&
        2.98106307d0/)
    p1_nasa(idx_N2O,:)  = (/2.2571502d0,&
        0.011304728d0,&
        -1.3671319d-05,&
        9.6819803d-09,&
        -2.9307182d-12,&
        8741.7746d0,&
        10.757992d0/)
    p1_nasa(idx_NO2,:)  = (/3.9440312d0,&
        -0.001585429d0,&
        1.6657812d-05,&
        -2.0475426d-08,&
        7.8350564d-12,&
        2896.618d0,&
        6.3119919d0/)
    p1_nasa(idx_CH,:)  = (/3.4897583d0,&
        0.0003243216d0,&
        -1.6899751d-06,&
        3.162842d-09,&
        -1.4061803d-12,&
        70660.755d0,&
        2.0842841d0/)
    p1_nasa(idx_Hj,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        184021.488d0,&
        -1.14064664d0/)
    p1_nasa(idx_HDj,:)  = (/3.8800679d0,&
        -0.0030653429d0,&
        8.17334271d-06,&
        -6.80432062d-09,&
        1.98627839d-12,&
        178941.448d0,&
        -2.79172055d0/)
    p1_nasa(idx_HEj,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        285323.374d0,&
        1.62166556d0/)
    p1_nasa(idx_Cj,:)  = (/2.61332254d0,&
        -0.000540148065d0,&
        1.03037233d-06,&
        -8.90092552d-10,&
        2.88500586d-13,&
        216862.274d0,&
        3.8345479d0/)
    p1_nasa(idx_Dj,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        184512.004d0,&
        -0.101841452d0/)
    p1_nasa(idx_Oj,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        187935.284d0,&
        4.39337676d0/)
    p1_nasa(idx_O2j,:)  = (/4.61017167d0,&
        -0.00635951952d0,&
        1.42425624d-05,&
        -1.20997923d-08,&
        3.70956878d-12,&
        139742.229d0,&
        -0.201326941d0/)
    p1_nasa(idx_COj,:)  = (/3.77061642d0,&
        -0.00201773246d0,&
        4.61081738d-06,&
        -2.99175463d-09,&
        6.06065045d-13,&
        149006.795d0,&
        3.38129783d0/)
    p1_nasa(idx_OHj,:)  = (/3.50502572d0,&
        0.000241313747d0,&
        -1.42200948d-06,&
        2.64780232d-09,&
        -1.17038711d-12,&
        155210.676d0,&
        1.97907627d0/)
    p1_nasa(idx_H2Oj,:)  = (/4.02465912d0,&
        -0.00108851414d0,&
        5.13576558d-06,&
        -4.40027838d-09,&
        1.40726746d-12,&
        116895.616d0,&
        0.699968812d0/)
    p1_nasa(idx_H3Oj,:)  = (/3.79295251d0,&
        -0.000910852723d0,&
        1.16363521d-05,&
        -1.21364865d-08,&
        4.26159624d-12,&
        71402.7518d0,&
        1.47156927d0/)
    p2_nasa(idx_Hk,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        15976.167d0,&
        -1.1390139d0/)
    p2_nasa(idx_Dk,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        17907.5582d0,&
        -0.101024344d0/)
    p2_nasa(idx_Ck,:)  = (/2.50001597d0,&
        -1.71721376d-08,&
        6.9283294d-12,&
        -1.20607892d-15,&
        7.60308635d-20,&
        70064.9324d0,&
        4.87955907d0/)
    p2_nasa(idx_Ok,:)  = (/2.54474869d0,&
        -4.66695513d-05,&
        1.84912357d-08,&
        -3.18159223d-12,&
        1.98962956d-16,&
        11504.2089d0,&
        4.52131015d0/)
    p2_nasa(idx_OHk,:)  = (/2.80023747d0,&
        0.00113380509d0,&
        -2.99666184d-07,&
        4.01911483d-11,&
        -1.78988913d-15,&
        -18253.5298d0,&
        4.6939462d0/)
    p2_nasa(idx_HD,:)  = (/2.80029834d0,&
        0.0011562336d0,&
        -3.06064442d-07,&
        4.51518392d-11,&
        -2.62838877d-15,&
        238.213151d0,&
        1.23069947d0/)
    p2_nasa(idx_H,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        25473.66d0,&
        -0.44668285d0/)
    p2_nasa(idx_D,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        25921.2596d0,&
        0.591714338d0/)
    p2_nasa(idx_HE,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        -745.375d0,&
        0.928723974d0/)
    p2_nasa(idx_CO,:)  = (/3.0484859d0,&
        0.0013517281d0,&
        -4.8579405d-07,&
        7.8853644d-11,&
        -4.6980746d-15,&
        -14266.117d0,&
        6.0170977d0/)
    p2_nasa(idx_C,:)  = (/2.605583d0,&
        -0.00019593434d0,&
        1.0673722d-07,&
        -1.642394d-11,&
        8.187058d-16,&
        85411.742d0,&
        4.1923868d0/)
    p2_nasa(idx_N,:)  = (/2.4159429d0,&
        0.00017489065d0,&
        -1.1902369d-07,&
        3.0226244d-11,&
        -2.0360983d-15,&
        56133.775d0,&
        4.6496095d0/)
    p2_nasa(idx_O,:)  = (/2.54363697d0,&
        -2.73162486d-05,&
        -4.1902952d-09,&
        4.95481845d-12,&
        -4.79553694d-16,&
        29226.012d0,&
        4.92229457d0/)
    p2_nasa(idx_N2,:)  = (/2.95257637d0,&
        0.0013969004d0,&
        -4.92631603d-07,&
        7.86010195d-11,&
        -4.60755204d-15,&
        -923.948688d0,&
        5.87188762d0/)
    p2_nasa(idx_NO,:)  = (/3.26071234d0,&
        0.00119101135d0,&
        -4.29122646d-07,&
        6.94481463d-11,&
        -4.03295681d-15,&
        9921.43132d0,&
        6.36900518d0/)
    p2_nasa(idx_O2,:)  = (/3.66096065d0,&
        0.000656365811d0,&
        -1.41149627d-07,&
        2.05797935d-11,&
        -1.29913436d-15,&
        -1215.97718d0,&
        3.41536279d0/)
    p2_nasa(idx_OH,:)  = (/2.83853033d0,&
        0.00110741289d0,&
        -2.94000209d-07,&
        4.20698729d-11,&
        -2.4228989d-15,&
        3697.80808d0,&
        5.84494652d0/)
    p2_nasa(idx_CH2,:)  = (/3.11049513d0,&
        0.00373779517d0,&
        -1.37371977d-06,&
        2.23054839d-10,&
        -1.33567178d-14,&
        45971.5953d0,&
        4.62796405d0/)
    p2_nasa(idx_CO2,:)  = (/4.6365111d0,&
        0.0027414569d0,&
        -9.9589759d-07,&
        1.6038666d-10,&
        -9.1619857d-15,&
        -49024.904d0,&
        -1.9348955d0/)
    p2_nasa(idx_H2O,:)  = (/2.6770389d0,&
        0.0029731816d0,&
        -7.7376889d-07,&
        9.4433514d-11,&
        -4.2689991d-15,&
        -29885.894d0,&
        6.88255d0/)
    p2_nasa(idx_HCO,:)  = (/4.23892214d0,&
        0.0019657617d0,&
        -3.82075171d-07,&
        4.80137647d-11,&
        -3.11176347d-15,&
        24726.1645d0,&
        1.99698242d0/)
    p2_nasa(idx_N2O,:)  = (/4.8230729d0,&
        0.0026270251d0,&
        -9.5850872d-07,&
        1.6000712d-10,&
        -9.7752302d-15,&
        8073.4047d0,&
        -2.2017208d0/)
    p2_nasa(idx_NO2,:)  = (/4.884754d0,&
        0.0021723955d0,&
        -8.2806909d-07,&
        1.574751d-10,&
        -1.0510895d-14,&
        2316.4982d0,&
        -0.11741695d0/)
    p2_nasa(idx_CH,:)  = (/2.5209369d0,&
        0.0017653639d0,&
        -4.614766d-07,&
        5.9289675d-11,&
        -3.3474501d-15,&
        70994.878d0,&
        7.4051829d0/)
    p2_nasa(idx_Hj,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        184021.488d0,&
        -1.14064664d0/)
    p2_nasa(idx_HDj,:)  = (/3.63782858d0,&
        0.000458875734d0,&
        1.13136423d-07,&
        -4.23103495d-11,&
        2.49509008d-15,&
        178814.567d0,&
        -2.37056371d0/)
    p2_nasa(idx_HEj,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        285323.374d0,&
        1.62166556d0/)
    p2_nasa(idx_Cj,:)  = (/2.50827618d0,&
        -1.04354146d-05,&
        5.16160809d-09,&
        -1.14187475d-12,&
        9.43539946d-17,&
        216879.645d0,&
        4.3188599d0/)
    p2_nasa(idx_Dj,:)  = (/2.5d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        0.0d0,&
        184512.004d0,&
        -0.101841452d0/)
    p2_nasa(idx_Oj,:)  = (/2.48542028d0,&
        2.56978695d-05,&
        -1.28833378d-08,&
        1.65525487d-12,&
        1.09933344d-16,&
        187940.874d0,&
        4.47425446d0/)
    p2_nasa(idx_O2j,:)  = (/3.31675922d0,&
        0.00111522244d0,&
        -3.83492556d-07,&
        5.72784687d-11,&
        -2.77648381d-15,&
        139876.823d0,&
        5.44726469d0/)
    p2_nasa(idx_COj,:)  = (/2.93062935d0,&
        0.00156033262d0,&
        -6.16246355d-07,&
        1.09957336d-10,&
        -6.66119284d-15,&
        149147.222d0,&
        7.3384673d0/)
    p2_nasa(idx_OHj,:)  = (/2.68358996d0,&
        0.00157006435d0,&
        -5.39972815d-07,&
        9.37643877d-11,&
        -5.70068067d-15,&
        155479.296d0,&
        6.44375894d0/)
    p2_nasa(idx_H2Oj,:)  = (/3.31570445d0,&
        0.00210648746d0,&
        -3.76341515d-07,&
        3.47525972d-11,&
        -1.70335643d-15,&
        117017.475d0,&
        4.03220514d0/)
    p2_nasa(idx_H3Oj,:)  = (/2.49647765d0,&
        0.0057284484d0,&
        -1.83953239d-06,&
        2.73577348d-10,&
        -1.54093917d-14,&
        71624.4227d0,&
        7.45850493d0/)
    Tlim_nasa(idx_Hk,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_Dk,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_Ck,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_Ok,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_OHk,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_HD,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_H,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_D,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_HE,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_CO,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_C,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_N,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_O,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_N2,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_NO,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_O2,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_OH,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_CH2,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_CO2,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_H2O,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_HCO,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_N2O,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_NO2,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_CH,:)  = (/200.0d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_Hj,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_HDj,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_HEj,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_Cj,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_Dj,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_Oj,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_O2j,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_COj,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_OHj,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_H2Oj,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)
    Tlim_nasa(idx_H3Oj,:)  = (/298.15d0,&
        1000.0d0,&
        6000.0d0/)

    ! pick NASA data if present for species
    if (Tlim_nasa(idx,2) /= 0.d0) then
      !select set of NASA polynomials using temperature
      if(Tlim_nasa(idx,1).le.Tgas .and. Tgas.le.Tlim_nasa(idx,2)) then
        p(:) = p1_nasa(idx,:)

      else if(Tlim_nasa(idx,2)<Tgas .and. Tgas.le.Tlim_nasa(idx,3)) then
        p(:) = p2_nasa(idx,:)

        ! currently no option when Tgas not in Tlim range p(:) = 0
      end if

      !compute NASA polynomials for enthalpy and enthropy (unitless)
      H = p(1) + p(2)*0.5d0*Tgas + p(3)*Tgas2/3.d0 + p(4)*Tgas3*0.25d0 + &
          p(5)*Tgas4*0.2d0 + p(6)*invT
      S = p(1)*lnT + p(2)*Tgas + p(3)*Tgas2*0.5d0 + p(4)*Tgas3/3.d0 + &
          p(5)*Tgas4*0.25d0 + p(7)

      revHS = H - S

      ! else pick NIST data (if present)
    else if (Tlim_nist(idx,2) /= 0.d0) then
      if (Tlim_nist(idx,1) < Tgas .and. Tgas < Tlim_nist(idx,2)) then
        p(:) = p1_nist(idx,:)

      else if (Tlim_nist(idx,2) < Tgas .and. Tgas < Tlim_nist(idx,3)) then
        p(:) = p2_nist(idx,:)

        ! currently no option when Tgas not in Tlim range p(:) = 0
      end if

      !compute NIST polynomials for enthalpy and enthropy
      ! H in (kJ/mol)
      H = p(1)*Tnist + p(2)*0.5d0*Tnist2 + p(3)*Tnist3/3.d0 + p(4)*Tnist4*0.25d0&
          - p(5)*invTnist + p(6)
      !  Unitsless
      H = H / (Rgas_kJ * Tgas)

      ! S in (J/mol*K)
      S = p(1)*lnTnist + p(2)*Tnist + p(3)*Tnist2*0.5d0 + p(4)*Tnist3/3.d0&
          - p(5)*invTnist2*0.5d0 + p(7)
      !  Unitless. Note: do not use Tnist
      S = S / Rgas_J

      revHS = H - S

      ! return zero is no data exists
    else
      print *, "No thermochemical data of species index", idx
      revHS = 0.d0

    end if

  end function revHS

  !******************************
  subroutine print_best_flux(n,Tgas,nbestin)
    !print the first nbestin fluxes
    use krome_commons
    use krome_getphys
    implicit none
    real*8::n(nspec),Tgas,flux(nrea)
    integer::nbest,idx(nrea),i,nbestin
    character*50::name(nrea)

    nbest = min(nbestin,nrea) !cannot exceed the number of reactions

    flux(:) = get_flux(n(:),Tgas) !get fluxes
    name(:) = get_rnames() !get reaction names

    !call the sorting algorithm (bubblesort)
    idx(:) = idx_sort(flux(:))

    !print to screen
    print *,"***************"
    do i=1,nbest
      print '(I8,a1,a50,E17.8)',idx(i)," ",name(idx(i)),flux(idx(i))
    end do

  end subroutine print_best_flux

  !******************************
  subroutine print_best_flux_frac(n,Tgas,frac)
    !print the first nbestin fluxes
    use krome_commons
    use krome_getphys
    implicit none
    real*8::n(nspec),Tgas,flux(nrea),frac
    integer::idx(nrea),i
    character*50::name(nrea)

    if(frac>1d0) then
      print *,"ERROR: fraction in krome_print_best_flux should be <=1!"
      stop
    end if

    flux(:) = get_flux(n(:),Tgas) !get fluxes
    name(:) = get_rnames() !get reaction names

    !call the sorting algorithm (bubblesort)
    idx(:) = idx_sort(flux(:))

    !print to screen
    print *,"***************"
    do i=1,nrea
      if(flux(idx(i))<flux(idx(1))*frac) exit
      print '(I8,a1,a50,E17.8)',idx(i)," ",name(idx(i)),flux(idx(i))
    end do

  end subroutine print_best_flux_frac

  !******************************
  subroutine print_best_flux_spec(n,Tgas,nbestin,idx_found)
    !print the first nbestin fluxes for the reactions
    ! that contains the species with index idx_found
    use krome_commons
    use krome_getphys
    implicit none
    real*8::n(nspec),Tgas,flux(nrea),maxflux
    integer::nbest,idx(nrea),i,nbestin,idx_found
    character*50::name(nrea)
    logical::found

    nbest = min(nbestin,nrea) !cannot exceed the number of reactions
    maxflux = 0d0
    flux(:) = get_flux(n(:),Tgas) !get fluxes
    name(:) = get_rnames() !get reaction names
    do i=1,nrea
      found = .false.
      if(arr_r1(i) == idx_found) found = .true.
      if(arr_r2(i) == idx_found) found = .true.
      if(arr_p1(i) == idx_found) found = .true.
      if(arr_p2(i) == idx_found) found = .true.
      if(arr_p3(i) == idx_found) found = .true.
      if(arr_p4(i) == idx_found) found = .true.
      maxflux = max(maxflux,flux(i))
      if(.not.found) flux(i) = 0d0
    end do

    !call the sorting algorithm (bubblesort)
    idx(:) = idx_sort(flux(:))

    !print to screen
    print *,"***************"
    do i=1,nbest
      print '(I8,a1,a50,2E17.8)',idx(i)," ",name(idx(i)),flux(idx(i)),&
          flux(idx(i))/maxflux
    end do

  end subroutine print_best_flux_spec

  !*****************************
  function idx_sort(fin)
    !sorting algorithm: requires an array of real values fin
    ! and returns the sorted index list. descending.
    ! bubblesort: not very efficient, replace with what you prefer
    implicit none
    real*8::fin(:),f(size(fin)),ftmp
    integer::idx_sort(size(fin)),n,itmp,i
    logical::found

    f(:) = fin(:) !copy to local

    n = size(f)
    !init indexes
    do i=1,n
      idx_sort(i) = i
    end do

    !loop to sort
    do
      found = .false. !swapped something flag
      do i=2,n
        !> for descending, < for ascending
        if(f(i)>f(i-1)) then
          found = .true.
          !swap real value
          ftmp = f(i)
          f(i) = f(i-1)
          f(i-1) = ftmp
          !swap index
          itmp = idx_sort(i)
          idx_sort(i) = idx_sort(i-1)
          idx_sort(i-1) = itmp
        end if
      end do
      !if nothing swapped exit
      if(.not.found) exit
    end do

  end function idx_sort

  !******************************
  function get_flux(n,Tgas)
    !get the flux k*n*n*... of the rates
    use krome_commons
    implicit none
    integer::i
    integer::r1,r2
    real*8::get_flux(nrea),n(nspec),k(nrea),rrmax,Tgas

    k(:) = coe(n(:))
    rrmax = 0.d0
    n(idx_dummy) = 1.d0
    n(idx_g) = 1.d0
    n(idx_CR) = 1.d0
    do i=1,nrea
      r1 = arr_r1(i)
      r2 = arr_r2(i)
      arr_flux(i) = k(i)*n(r1)*n(r2)
    end do
    get_flux(:) = arr_flux(:)

  end function get_flux

  !*****************************
  subroutine load_arrays()
    !load the array containing reactants
    ! and product index
    use krome_commons

    arr_r1(1:3466) = (/62,62,62,62,62,63,63,63,63,63,62,62,62,62&
        ,62,63,63,63,63,64,64,64,64,64,64,65,65,65,65,65,65,65,65,62&
        ,62,62,62,62,62,62,62,63,63,63,63,64,64,64,64,64,64,64,65,65&
        ,65,65,65,65,65,65,65,66,66,66,66,66,66,66,66,67,67,67,67,67&
        ,67,67,67,67,64,64,64,64,64,64,64,64,64,65,65,65,65,65,65,65&
        ,65,65,66,66,66,66,66,66,66,67,67,67,67,67,67,67,67,67,69,69&
        ,69,69,68,68,68,68,68,68,68,68,66,66,66,66,66,66,66,66,66,67&
        ,67,67,67,67,67,67,67,67,67,69,69,69,69,68,68,68,68,68,69,69&
        ,69,69,69,69,68,68,68,68,68,68,15,15,15,16,16,17,70,71,71,72&
        ,72,73,73,74,62,62,62,63,63,64,64,64,65,65,65,67,67,67,66,66&
        ,66,69,69,68,68,68,75,76,20,15,16,18,21,22,10,11,13,14,12,10&
        ,11,13,14,12,12,10,11,13,14,12,12,10,11,13,14,12,83,84,83,84&
        ,23,24,25,26,26,27,27,28,29,30,32,33,35,36,36,37,38,39,40,41&
        ,42,43,43,44,45,46,47,46,47,48,49,50,51,52,53,54,55,55,53,54&
        ,55,56,57,58,57,58,59,31,60,61,34,19,19,96,83,84,81,82,73,72&
        ,71,71,97,98,99,100,87,88,89,89,63,63,62,62,68,68,69,69,65,65&
        ,64,64,65,64,67,67,66,66,67,66,63,63,62,62,68,68,69,69,65,64&
        ,65,65,64,64,67,67,66,66,67,66,5,3,4,6,7,8,9,20,31,31,60,61&
        ,60,61,34,19,10,11,13,14,12,23,24,25,24,25,26,26,27,27,28,29&
        ,28,29,30,32,30,32,33,33,35,37,38,39,39,37,38,39,40,41,42,43&
        ,43,41,42,43,44,45,46,47,46,47,48,49,50,51,53,54,55,55,53,54&
        ,55,56,59,60,61,24,25,27,27,28,29,30,32,36,37,38,39,39,40,41&
        ,42,43,43,41,42,43,43,44,45,44,45,46,47,48,49,53,54,55,55,59&
        ,10,11,13,14,12,12,10,11,13,14,12,12,10,11,13,14,12,12,10,11&
        ,13,14,12,12,26,27,40,41,41,41,42,42,42,43,43,43,43,44,44,45&
        ,45,10,11,13,14,12,12,44,45,44,44,45,45,22,22,22,22,22,22,22&
        ,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22&
        ,22,60,61,60,61,60,60,61,61,31,31,34,34,34,34,34,34,34,34,34&
        ,34,19,19,19,19,24,24,25,24,25,26,26,26,28,28,29,28,28,29,29&
        ,28,28,29,29,28,28,28,28,28,29,29,29,29,29,28,28,29,29,26,27&
        ,27,40,40,41,42,43,43,41,42,43,43,46,47,21,60,61,24,25,28,29&
        ,30,32,37,38,39,39,41,42,43,43,53,54,55,55,20,20,20,20,20,20&
        ,21,21,21,21,21,21,22,22,22,22,22,22,31,31,31,31,31,31,60,60&
        ,60,60,60,60,60,60,61,61,61,61,61,61,61,61,34,34,34,34,34,34&
        ,19,19,19,19,19,19,10,11,11,10,10,11,11,10,10,11,11,10,10,11&
        ,11,10,11,13,13,13,14,14,14,14,13,13,14,14,13,13,13,13,14,14&
        ,14,14,13,13,14,14,13,13,14,14,12,12,12,12,12,12,12,12,12,12&
        ,12,12,12,12,23,23,23,23,23,23,24,24,24,24,24,24,24,24,25,25&
        ,25,25,25,25,25,25,26,26,26,26,26,26,27,27,27,27,27,27,28,28&
        ,28,28,28,28,28,28,29,29,29,29,29,29,29,29,40,40,40,40,40,46&
        ,46,46,46,46,46,47,47,47,47,47,47,47,47,15,15,16,16,11,10,11&
        ,11,13,14,14,13,14,14,12,12,12,12,63,62,62,63,63,62,62,68,69&
        ,65,65,64,64,65,65,64,64,64,65,64,67,67,66,66,66,67,66,67,67&
        ,66,66,75,75,76,76,31,60,61,34,34,19,10,11,13,14,12,12,23,24&
        ,25,26,26,27,28,29,30,32,30,32,30,32,33,35,35,36,37,38,39,37&
        ,38,39,39,40,40,40,40,41,42,43,43,41,42,43,43,44,45,44,45,44&
        ,45,44,45,46,47,46,47,46,47,48,49,48,49,48,49,50,51,50,51,52&
        ,52,52,52,53,54,55,53,54,55,55,59,59,20,20,21,21,21,21,22,22&
        ,60,60,61,61,41,42,43,43,20,20,20,20,21,21,21,21,22,22,22,22&
        ,27,27,27,27,41,42,43,43,15,16,22,20,20,20,20,21,21,21,21,22&
        ,22,22,20,20,31,31,60,60,61,61,19,19,24,24,25,25,28,28,29,29&
        ,20,20,31,31,60,60,61,61,24,24,25,25,28,28,29,29,27,27,20,20&
        ,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20&
        ,15,15,16,16,15,15,16,16,15,15,16,16,15,15,15,15,15,16,16,16&
        ,16,16,15,15,16,16,15,15,15,15,15,16,16,16,16,16,15,15,16,16&
        ,15,16,15,16,15,15,16,16,15,15,15,16,16,16,15,16,15,15,16,16&
        ,15,15,16,16,15,15,16,16,15,16,15,16,15,16,20,20,20,20,20,20&
        ,60,60,60,60,60,60,60,60,61,61,61,61,61,61,61,61,22,22,22,22&
        ,22,22,10,11,10,11,10,11,13,14,13,14,13,14,12,12,12,12,21,21&
        ,21,21,21,21,10,11,10,11,10,11,13,14,13,14,13,14,12,12,12,12&
        ,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21&
        ,21,21,21,21,21,21,21,21,21,21,22,22,22,22,22,22,22,22,22,22&
        ,22,22,31,31,31,31,60,60,60,60,60,61,61,61,61,61,19,19,19,19&
        ,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,21,21,21,21,21&
        ,21,21,21,21,21,21,21,22,22,22,22,22,22,22,22,22,22,22,22,22&
        ,22,22,22,22,22,22,22,22,22,22,22,22,22,22,31,31,31,31,31,31&
        ,31,31,31,31,31,31,31,31,31,60,60,60,60,60,60,60,60,60,60,60&
        ,60,60,60,60,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61&
        ,61,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,19,19,19,19&
        ,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19&
        ,19,19,19,19,19,19,23,23,23,23,23,23,23,23,23,23,23,23,23,23&
        ,23,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,25,25,25,25&
        ,25,25,25,25,25,25,25,25,25,25,25,25,25,26,26,26,26,26,26,26&
        ,26,26,26,26,26,26,26,26,27,27,27,27,27,27,27,27,27,27,27,27&
        ,27,27,27,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,29,29&
        ,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,56,56,56,56,56&
        ,56,56,56,56,56,56,56,56,56,56,30,30,32,32,36,36,37,37,37,38&
        ,38,38,39,39,39,39,40,40,46,46,47,47,46,46,47,47,20,20,31,31&
        ,60,60,61,61,24,24,25,25,27,27,28,28,29,29,20,20,22,22,31,31&
        ,60,60,61,61,19,19,24,24,25,25,28,28,29,29,19,19,10,11,11,10&
        ,11,11,10,11,13,14,14,13,14,13,14,14,12,12,12,12,12,12,23,23&
        ,20,20,31,31,60,60,61,61,19,19,24,24,25,25,28,28,29,29,60,61&
        ,10,11,13,14,12,12,24,25,26,27,27,28,29,40,46,47,31,60,61,34&
        ,10,11,13,14,12,12,23,24,25,28,29,30,32,40,44,45,44,45,44,45&
        ,46,47,52,48,48,48,49,49,49,50,50,51,51,21,22,31,60,61,10,11&
        ,13,14,12,12,24,25,24,25,27,41,42,43,43,46,47,20,20,15,15,16&
        ,16,21,21,22,22,31,31,60,60,61,61,34,34,34,34,10,11,10,11,10&
        ,11,13,14,13,14,13,14,12,12,12,12,24,24,25,25,27,27,27,27,27&
        ,27,28,28,29,29,30,30,32,32,37,37,37,38,38,38,39,39,39,39,40&
        ,40,41,41,41,42,42,42,43,43,43,43,44,44,45,45,46,46,47,47,53&
        ,53,53,54,54,54,55,55,55,55,22,10,11,13,14,12,12,41,42,43,43&
        ,46,47,20,20,21,21,22,22,31,31,31,31,31,31,60,60,61,61,34,34&
        ,19,19,19,19,11,10,11,11,13,14,14,13,14,14,12,12,12,12,10,11&
        ,10,11,10,11,13,14,13,14,13,14,12,12,12,12,23,23,24,24,25,25&
        ,26,26,27,27,27,27,28,28,29,29,40,40,40,40,41,41,41,42,42,42&
        ,43,43,43,43,41,41,41,42,42,42,43,43,43,43,20,21,31,60,61,24&
        ,25,24,25,46,47,20,20,21,21,22,22,31,31,60,60,61,61,34,34,19&
        ,19,10,11,10,11,10,11,13,14,13,14,13,14,12,12,12,12,23,23,24&
        ,24,25,25,26,26,28,28,29,29,46,46,47,47,21,21,21,21,22,22,22&
        ,22,31,31,31,31,60,60,60,60,60,61,61,61,61,61,27,27,27,27,15&
        ,16,10,11,13,14,12,20,20,21,21,22,22,31,31,60,60,61,61,34,34&
        ,19,19,11,10,11,11,13,14,14,13,14,14,12,12,12,12,23,23,24,24&
        ,25,25,26,26,28,28,29,29,22,22,20,20,20,20,20,20,20,20,20,20&
        ,20,20,20,20,20,20,20,20,19,26,34,34,34,24,25,28,29,30,32,37&
        ,38,39,53,54,55,20,15,16,22,31,26,27,40,44,45,46,47,15,15,15&
        ,15,15,16,16,16,16,16,44,44,44,44,44,45,45,45,45,45,53,53,53&
        ,53,53,54,54,54,54,54,55,55,55,55,55,20,15,16,21,77,77,77,70&
        ,78,70,70,78,78,74,74,74,79,79,79,80,80,80,81,82,81,82,73,72&
        ,73,72,71,71,15,16,26,26,26,26,26,26,27,15,15,16,16,22,22,26&
        ,26,27,27,26,26,26,27,27,27,22,22,31,31,60,60,61,61,24,24,25&
        ,25,26,26,27,27,28,28,29,29,33,33,35,35,41,41,42,42,43,43,44&
        ,44,45,45,26,26,26,26,31,34,19,30,32,37,38,39,41,42,43,44,45&
        ,53,54,55,15,16,26,27,37,38,39,41,42,43,53,54,55,56,53,53,54&
        ,54,55,55,20,26,26,26,20,15,16,22,31,60,61,19,31,60,61,34,19&
        ,24,25,26,27,28,29,30,32,37,38,39,40,44,45,53,54,55,26,53,54&
        ,55,56,27,27,26,26,26,46,46,46,47,47,47,60,61,37,38,39,46,47&
        ,26,27,40,44,45,46,47,60,61,24,25,28,29,30,32,37,38,39,41,42&
        ,43,53,54,55,31,31,31,31,31,60,60,60,60,60,61,61,61,61,61,34&
        ,34,34,34,34,19,19,19,19,19,60,61,28,29,46,47,41,42,43,15,16&
        ,22,41,42,43,41,41,42,42,43,43,28,29,24,24,24,24,24,25,25,25&
        ,25,25,26,26,26,26,26,27,27,27,27,27,28,28,28,28,28,29,29,29&
        ,29,29,30,30,30,30,30,32,32,32,32,32,37,37,37,37,37,38,38,38&
        ,38,38,39,39,39,39,39,41,41,41,41,41,42,42,42,42,42,43,43,43&
        ,43,43,46,46,46,46,46,47,47,47,47,47,31,60,61,10,11,13,14,12&
        ,23,27,40,31,31,31,60,60,60,61,61,61,30,30,30,32,32,32,37,37&
        ,37,38,38,38,39,39,39,46,46,46,47,47,47,24,25,53,53,53,54,54&
        ,54,55,55,55,26,40,31,30,30,32,32,37,37,38,38,39,39,27,46,47&
        ,26,26,40,40,40,40,40,53,54,55,37,37,37,38,38,38,39,39,39,26&
        ,26,20,27,27,46,46,47,47,60,61,24,25,30,32,46,46,47,47,46,47&
        ,31,31,60,60,61,61,30,30,32,32,37,37,38,38,39,39,41,41,42,42&
        ,43,43,46,46,47,47,53,53,54,54,55,55,60,60,60,61,61,61,60,61&
        ,37,38,39,46,47,22,41,42,43,41,41,42,42,43,43,60,61,37,38,39&
        ,46,47,15,16,22,10,11,13,14,12,15,15,15,16,16,16,20,20,20,20&
        ,20,20,20,20,20,20,15,16,22,20,15,15,16,16,20,15,16,21,22,60&
        ,61,10,11,13,14,12,24,25,27,28,29,20,20,15,15,16,16,21,21,22&
        ,22,31,31,60,60,61,61,34,34,19,19,24,24,25,25,28,28,29,29,20&
        ,15,16,21,22,60,61,19,10,11,13,14,12,15,16,20,20,15,15,16,16&
        ,20,15,16,22,96,83,84,106,95,81,82,73,72,71,81,82,73,73,72,71&
        ,122,123,124,101,102,85,86,97,98,99,100,99,100,99,100,111,111&
        ,108,107,87,88,89,87,88,89,89,87,88,89,112,112,125,103,104&
        ,105,103,104,105,105,103,104,105,63,62,69,68,65,64,67,66,63&
        ,62,62,69,68,68,65,64,65,64,67,66,67,66,113,114,75,76,115,116&
        ,90,91,109,110,118,119,118,119,117,92,93,94,92,93,94,94,126&
        ,120,121,77,70,78,74,79,80,70,70,78,78,64,64,65,65,66,67,66&
        ,67,71,71,73,72,67,67,66,68,69,78,78,70,70,82,81,82,81,78,70&
        ,78,78,70,70,71,71,81,82,62,62,63,64,65,65,81,71,71,81,66,66&
        ,127,65,64,67,66,66,69,68,127,127,127,127,127,127,127,127,77&
        ,79,80,46,46,11,10,11,10,11,10,78,14,14,13,13,14,13,127,127&
        ,127,127,127,127,127,68,127,127,127,68,68,68,47,47,14,13,14&
        ,13,20,20,21,22,22,31,31,61,61,34,34,19,19,19,23,25,25,26,26&
        ,27,27,29,29,56,56,121,121,127,127,127,127,127,127,41,41,128&
        ,128,128,128,41,41,10,11,24,28,41,46,41,41,41,41,41,41,41,20&
        ,20,60,41,41,41,41,128,128,128,128,15,15,11,10,128,128,128&
        ,128,128,128,41,41,41,41,41,41,43,43,43,43,43,43,43,43,43,43&
        ,42,42,42,42,42,42,42,42,128,128,128,128,128,129,129,129,129&
        ,129,129,129,129,129,129,129,130,130,130,130,130,130,130,130&
        ,130,130,130,131,131,131,131,131,131,131,128,128,128,129,129&
        ,129,129,129,129,129,129,130,130,130,130,130,130,130,130,131&
        ,131,131,131,131,41,43,43,42,42,41,43,43,42,42,10,11,10,11,10&
        ,11,10,11,12,12,12,12,12,12,14,13,14,13,13,14,13,14,14,13,24&
        ,24,25,25,25,28,28,29,29,29,41,41,41,41,43,43,43,43,43,43,42&
        ,42,42,42,42,46,46,47,47,47,41,41,41,41,41,41,41,41,41,41,41&
        ,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,43,43,43,43&
        ,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43&
        ,43,43,43,43,43,43,43,43,43,43,43,42,42,42,42,42,42,42,42,42&
        ,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42&
        ,41,43,43,42,42,41,43,43,42,42,41,43,43,42,42,20,20,20,20,20&
        ,20,20,20,60,60,60,60,60,60,60,61,61,61,61,61,61,61,61,41,43&
        ,43,42,42,41,43,43,42,42,41,43,43,42,42,41,41,41,41,43,43,43&
        ,43,43,43,42,42,42,42,42,129,129,130,130,131,129,129,130,130&
        ,131,129,129,130,130,131,128,128,128,129,129,129,129,129,129&
        ,129,130,130,130,130,130,130,130,131,131,131,131,15,15,15,15&
        ,15,15,15,15,15,15,15,16,16,16,16,16,16,16,16,16,16,16,16,16&
        ,10,11,10,11,10,11,10,11,10,11,11,10,11,10,11,10,12,12,12,12&
        ,12,12,12,12,12,12,13,14,13,14,13,14,14,13,14,13,14,13,14,13&
        ,14,13,14,13,129,129,130,130,131,129,129,130,130,131,129,129&
        ,129,130,130,130,131,131,129,129,129,130,130,130,131&
        ,131/)
    arr_r2(1:3466) = (/10,10,11,11,11,10,10,11,11,11,12&
        ,12,12,12,12,12,12,12,12,10,10,11,11,11,11,10,10,10,11,11,11&
        ,11,11,13,13,13,13,14,14,14,14,13,13,14,14,12,12,12,12,12,12&
        ,12,12,12,12,12,12,12,12,12,12,10,10,10,10,11,11,11,11,10,10&
        ,10,10,11,11,11,11,11,13,13,13,13,14,14,14,14,14,13,13,13,13&
        ,14,14,14,14,14,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12&
        ,12,10,10,11,11,10,10,10,10,11,11,11,11,13,13,13,13,14,14,14&
        ,14,14,13,13,13,13,13,14,14,14,14,14,12,12,12,12,12,12,12,12&
        ,12,13,13,13,14,14,14,13,13,13,14,14,14,15,15,16,16,16,1,2,2&
        ,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,77,77,77,77,77,77,77,77,77,77,77&
        ,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77&
        ,77,77,77,77,77,77,95,95,95,95,95,95,95,95,95,95,95,95,106&
        ,106,106,106,106,106,106,106,106,106,106,106,5,5,5,3,4,4,3,3&
        ,4,3,3,4,4,3,4,3,4,6,6,6,6,6,6,6,6,8,9,8,9,37,38,39,37,38,39&
        ,46,47,46,47,44,45,50,51,53,54,55,55,53,54,55,55,56,57,58,59&
        ,59,35,26,26,27,27,50,51,50,51,60,61,26,27,46,47,28,29,28,29&
        ,50,51,28,29,50,51,24,25,25,26,26,53,54,55,28,29,29,46,47,46&
        ,47,50,51,50,51,53,54,54,55,55,53,53,54,55,55,57,58,57,58,106&
        ,106,106,106,106,106,106,106,106,106,106,106,106,106,106,95&
        ,95,95,95,95,95,95,95,95,95,95,95,95,95,95,95,95,95,95,95,95&
        ,81,82,73,72,71,71,81,82,73,72,71,71,81,82,73,72,71,71,81,82&
        ,73,72,71,71,81,82,73,72,73,72,71,71,81,82,81,82,73,72,71,71&
        ,81,82,73,72,71,71,81,82,73,72,71,71,82,81,82,73,72,73,72,73&
        ,72,73,72,71,71,71,71,71,71,81,82,82,81,81,82,82,81,82,81,82&
        ,73,73,72,72,73,73,72,72,71,71,71,71,71,71,71,71,81,81,82,82&
        ,82,73,72,72,73,72,71,71,71,71,81,82,73,72,71,71,81,82,73,72&
        ,73,72,71,71,81,82,81,82,73,72,71,71,81,82,73,72,71,71,81,82&
        ,73,72,71,71,81,82,73,72,73,72,71,71,81,82,81,82,73,72,71,71&
        ,81,82,73,72,71,82,73,72,72,71,71,81,82,82,73,72,72,71,71,122&
        ,123,122,123,122,123,123,123,122,122,122,123,123,123,122,122&
        ,123,123,3,3,3,4,4,4,4,4,4,3,3,3,3,4,4,4,4,4,4,4,3,3,3,3,3,3&
        ,3,4,4,4,4,3,4,3,4,74,74,74,74,74,74,74,74,74,74,74,74,74,74&
        ,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74&
        ,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74&
        ,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74&
        ,74,74,74,74,74,74,74,74,74,74,74,74,99,100,99,100,99,100,99&
        ,100,99,100,99,100,111,111,111,111,87,88,89,89,87,88,89,89,87&
        ,88,89,89,87,88,89,89,112,112,112,112,125,125,125,103,104,105&
        ,105,103,104,105,105,103,104,105,113,114,113,114,113,114,113&
        ,114,113,114,113,114,113,114,113,114,113,114,75,76,75,76,75&
        ,76,75,76,75,76,75,76,75,76,75,76,30,32,37,38,39,37,38,39,39&
        ,53,54,55,55,53,54,55,55,53,54,55,55,34,23,19,46,47,46,47,46&
        ,47,46,47,60,61,60,61,37,38,38,39,39,37,37,38,39,39,28,29,28&
        ,29,41,42,42,43,43,41,41,42,43,43,44,45,44,45,26,26,26,26,50&
        ,51,50,51,50,51,51,50,50,51,27,27,57,58,57,58,57,58,57,58,57&
        ,58,57,58,40,40,52,52,56,56,10,11,13,14,12,12,10,11,13,14,13&
        ,14,12,12,10,11,10,11,13,14,12,12,10,11,13,14,12,12,28,28,29&
        ,29,29,29,28,28,28,28,29,29,28,28,29,29,10,11,13,14,12,12,24&
        ,24,25,25,25,25,24,24,24,24,25,25,24,24,25,25,31,60,61,34,24&
        ,25,26,28,29,27,37,38,39,39,37,38,39,39,46,47,56,56,56,57,58&
        ,30,32,33,36,35,31,60,61,34,24,25,28,29,30,32,33,36,103,104&
        ,105,105,103,104,104,105,105,103,103,104,105,105,103,104,105&
        ,105,63,62,62,68,69,65,64,64,65,64,67,66,66,67,66,63,62,68,69&
        ,65,64,65,64,67,66,67,66,63,62,62,68,69,65,64,64,65,64,67,66&
        ,66,67,66,63,62,68,69,65,64,65,64,67,66,67,66,63,62,62,68,69&
        ,65,64,64,65,64,67,66,66,67,66,63,62,62,68,69,65,64,64,65,64&
        ,67,66,66,67,66,63,62,62,63,62,68,69,65,64,64,65,64,67,66,66&
        ,67,66,63,62,62,68,69,65,64,64,65,64,67,66,66,67,66,63,62,62&
        ,68,69,65,64,64,65,64,67,66,66,67,66,63,62,62,68,69,65,64,64&
        ,65,64,67,66,66,67,66,63,62,62,68,69,65,64,64,65,64,67,66,66&
        ,67,66,63,62,62,68,69,65,64,64,65,64,67,66,66,67,66,63,62,62&
        ,63,62,68,69,65,64,64,65,64,67,66,66,67,66,63,62,62,68,69,65&
        ,64,64,65,64,67,66,66,67,66,63,62,62,68,69,65,64,64,65,64,67&
        ,66,66,67,66,63,62,62,68,69,65,64,64,65,64,67,66,66,67,66,63&
        ,62,62,63,62,68,69,65,64,64,65,64,67,66,66,67,66,63,62,62,68&
        ,69,65,64,64,65,64,67,66,66,67,66,70,78,70,78,70,78,70,78,78&
        ,70,70,78,70,70,78,78,70,78,70,78,70,78,70,78,70,78,115,116&
        ,115,116,115,116,115,116,115,116,115,116,115,116,115,116,115&
        ,116,90,91,90,91,90,91,90,91,90,91,90,91,90,91,90,91,90,91,90&
        ,91,109,110,109,109,109,110,110,110,110,110,109,109,109,109&
        ,109,110,110,110,109,109,109,110,110,110,109,110,118,119,118&
        ,119,118,119,118,119,118,119,118,119,118,119,118,119,118,119&
        ,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,80,80&
        ,80,80,80,80,80,80,80,80,80,80,80,80,80,80,80,80,80,80,80,80&
        ,80,80,80,80,80,70,78,78,70,70,78,70,78,70,78,96,96,96,96,96&
        ,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,83,84,83&
        ,84,83,84,83,84,83,84,83,84,83,84,83,84,83,84,83,84,83,83,84&
        ,84,84,84,83,83,83,83,84,84,83,83,84,84,83,84,83,84,83,84,83&
        ,84,83,84,83,84,83,84,83,84,83,84,83,84,84,83,83,84,83,83,84&
        ,84,83,84,83,84,84,83,83,84,83,83,84,84,83,84,83,84,83,84,83&
        ,84,83,84,84,83,83,84,83,83,84,84,124,124,124,124,124,124,124&
        ,124,124,124,124,124,124,101,102,101,102,101,102,101,102,101&
        ,102,101,102,101,102,101,102,101,102,101,102,101,102,101,102&
        ,102,102,101,101,101,102,102,102,101,101,102,102,101,101,102&
        ,102,102,102,101,101,101,101,102,102,101,101,102,102,101,102&
        ,101,102,101,102,101,102,101,102,101,102,101,102,101,102,101&
        ,102,101,102,101,102,102,101,101,102,101,101,102,102,101,102&
        ,102,101,101,102,101,101,102,102,86,86,86,86,86,86,86,86,86&
        ,86,86,97,98,97,98,97,98,97,98,97,98,97,98,97,98,97,98,97,97&
        ,98,98,98,98,97,97,97,97,98,98,97,97,98,98,97,98,97,98,97,98&
        ,97,98,97,98,97,98,97,98,97,98,92,93,94,94,92,93,94,94,92,93&
        ,94,94,92,93,93,94,94,92,92,93,94,94,92,93,94,94,126,126,126&
        ,126,126,126,126,120,121,120,121,120,121,120,121,120,121,120&
        ,121,120,121,120,121,120,121,121,121,120,120,120,121,121,121&
        ,120,120,121,121,120,121,120,121,120,121,120,121,120,121,120&
        ,121,60,61,60,61,24,25,26,26,27,28,29,46,47,46,47,33,30,32,36&
        ,59,79,77,6,8,9,106,106,106,106,106,106,106,106,106,106,106&
        ,106,95,95,95,95,95,95,95,95,95,95,95,95,81,82,73,72,71,81,82&
        ,73,72,71,81,82,73,72,71,81,82,73,72,71,81,82,73,72,71,81,82&
        ,73,72,71,81,82,73,72,71,124,124,124,124,5,3,4,5,5,3,4,3,4,5&
        ,3,4,5,3,4,5,3,4,3,3,4,4,3,3,4,4,3,4,74,74,99,100,87,88,89&
        ,125,125,113,114,113,114,113,114,113,114,113,114,103,104,105&
        ,103,104,105,70,78,70,78,70,78,70,78,70,78,70,78,70,78,70,78&
        ,70,78,70,78,70,78,70,78,70,78,70,78,70,78,70,78,70,78,115&
        ,116,90,91,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,80&
        ,80,80,80,80,80,80,80,80,80,80,80,80,80,70,78,70,78,70,78,96&
        ,96,83,84,106,106,106,106,106,106,106,106,124,124,124,124,124&
        ,124,124,124,124,124,124,124,124,124,124,124,124,124,124,124&
        ,124,124,86,86,86,86,86,97,98,92,93,94,92,93,94,92,93,94,77&
        ,77,77,77,77,77,77,106,106,106,106,106,106,106,95,95,95,95,95&
        ,95,95,95,95,95,95,95,95,95,95,95,95,81,82,73,72,71,81,82,73&
        ,72,71,81,82,73,72,71,81,82,73,72,71,81,82,73,72,71,79,79,79&
        ,79,80,80,74,74,74,125,125,125,125,125,125,113,114,113,114&
        ,113,114,80,80,81,82,73,72,71,81,82,73,72,71,81,82,73,72,71&
        ,81,82,73,72,71,81,82,73,72,71,81,82,73,72,71,81,82,73,72,71&
        ,81,82,73,72,71,81,82,73,72,71,81,82,73,72,71,81,82,73,72,71&
        ,81,82,73,72,71,81,82,73,72,71,81,82,73,72,71,81,82,73,72,71&
        ,81,82,73,72,71,74,74,74,74,74,74,74,74,74,74,74,103,104,105&
        ,103,104,105,103,104,105,103,104,105,103,104,105,103,104,105&
        ,103,104,105,103,104,105,103,104,105,103,104,105,79,79,103&
        ,104,105,103,104,105,103,104,105,79,79,80,70,78,70,78,70,78&
        ,70,78,70,78,79,79,79,97,98,81,82,73,72,71,96,96,96,92,93,94&
        ,92,93,94,92,93,94,101,102,86,101,102,70,78,70,78,80,80,80,80&
        ,80,80,83,84,83,84,124,124,97,98,97,98,97,98,97,98,97,98,97&
        ,98,97,98,97,98,97,98,97,98,97,98,97,98,97,98,97,98,97,98,97&
        ,98,92,93,94,92,93,94,96,96,96,96,96,96,96,124,124,124,124&
        ,101,102,101,102,101,102,86,86,86,86,86,86,86,77,77,77,77,77&
        ,77,77,77,70,70,78,70,78,78,20,15,16,21,22,10,11,13,14,12,22&
        ,22,22,31,28,29,28,29,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,3,4,3&
        ,4,3,4,3,4,3,4,3,4,3,4,3,4,3,4,3,4,3,4,3,4,3,4,3,4,6,6,6,6,6&
        ,6,6,6,6,6,6,6,6,7,7,8,9,8,9,8,9,1,1,1,1,1,1,1,1,1,1,1,1,1,1&
        ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1&
        ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1&
        ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1&
        ,1,1,1,1,1,1,1,1,1,1,1,13,14,12,12,16,16,16,16,15,15,15,15,16&
        ,16,15,15,16,16,16,15,15,14,13,11,10,11,11,10,10,15,16,10,11&
        ,12,12,10,11,12,12,16,16,16,15,15,15,16,15,15,16,12,12,11,13&
        ,13,13,14,13,13,13,14,14,19,23,1,1,2,2,2,2,2,82,81,122,122&
        ,101,101,120,120,2,71,72,71,72,73,73,10,10,11,12,12,12,12,12&
        ,13,13,14,13,14,14,72,73,123,123,102,102,127,127,127,127,127&
        ,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127&
        ,127,127,127,127,127,14,13,4,4,135,135,135,135,82,81,3,3,3,3&
        ,113,75,103,103,103,103,103,103,63,63,62,62,115,90,118,128&
        ,128,128,83,101,97,92,5,6,7,8,128,128,128,128,1,1,1,1,1,1,71&
        ,71,72,73,72,73,81,82,81,82,71,71,73,72,73,72,81,82,81,82,71&
        ,71,72,73,4,4,4,4,4,3,3,3,3,3,4,4,4,4,4,4,3,3,3,3,3,3,4,4,4,4&
        ,4,3,3,3,3,3,4,4,4,4,4,3,3,3,4,4,4,4,4,3,3,3,3,3,4,4,4,3,3,3&
        ,4,4,114,113,114,113,114,76,75,76,75,76,105,105,105,105,104&
        ,104,104,104,103,103,105,105,104,104,103,103,103,103,105,105&
        ,105,105,104,104,105,104,103,105,104,105,104,103,105,104,105&
        ,105,104,104,103,103,105,105,104,104,103,103,105,105,104,105&
        ,104,103,105,104,65,65,64,64,65,64,67,67,66,66,67,66,67,66&
        ,127,69,68,127,68,69,127,69,68,127,69,68,68,63,63,62,62,63,62&
        ,64,64,65,65,64,65,64,64,65,65,66,66,67,67,66,67,66,66,67,67&
        ,69,127,68,69,69,127,127,68,68,62,62,63,62,63,62,63,62,63,64&
        ,65,64,65,64,64,65,65,66,67,66,66,67,67,127,127,69,69,68,68&
        ,116,115,116,115,116,91,90,91,90,91,119,118,119,118,119,129&
        ,129,129,130,130,130,131,131,129,129,130,130,130,131,131,128&
        ,128,129,129,129,130,130,131,84,83,84,83,84,102,101,102,101&
        ,102,98,97,98,97,98,94,94,93,93,92,92,94,94,93,93,92,92,94,94&
        ,93,5,5,5,5,5,6,6,6,6,6,7,7,7,7,7,9,9,9,8,8,8,9,9,9,9,8,8,8,8&
        ,9,9,9,8,8,8,9,129,129,129,130,130,130,130,130,131,131,131&
        ,128,128,128,129,129,129,129,129,130,130,130,131,131,129,129&
        ,129,129,130,130,130,130,130,130,131,131,131,131,131,131,128&
        ,128,129,129,129,130,130,130,131,131,128,128,128,128,128,128&
        ,129,129,129,129,129,129,130,130,130,130,131,131,1,1,1,1,1,1&
        ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1&
        ,1/)
    arr_p1(1:3466) = (/62,63,62,63,63,62,63,62,62,63,63&
        ,64,64,65,65,62,64,65,65,62,65,62,63,65,65,62,64,65,62,63,64&
        ,64,65,64,65,66,66,64,65,67,67,65,66,65,67,62,62,65,66,66,67&
        ,67,62,62,63,63,64,66,66,67,67,62,64,65,66,63,64,65,66,62,64&
        ,65,67,62,63,64,65,67,64,66,67,68,64,66,67,69,68,65,66,67,68&
        ,65,66,67,69,68,64,64,65,65,67,68,68,64,64,65,65,66,69,69,68&
        ,68,64,67,65,67,64,64,66,67,65,65,66,67,66,67,67,68,66,67,67&
        ,69,68,66,66,67,69,68,66,66,67,69,68,66,67,67,68,66,66,67,67&
        ,69,69,68,68,69,68,68,69,69,68,69,69,68,10,11,12,13,14,2,15&
        ,15,12,16,14,16,13,18,10,11,15,11,15,10,12,15,15,11,12,16,14&
        ,12,13,12,16,16,14,16,14,13,15,16,77,70,78,74,79,80,15,15,16&
        ,16,15,15,15,16,16,15,16,70,70,78,78,70,78,81,82,73,72,71,20&
        ,20,15,16,21,15,16,21,85,22,86,15,16,15,16,20,20,22,20,87,88&
        ,89,22,15,16,15,16,15,16,15,16,75,76,15,16,90,91,21,15,16,15&
        ,16,92,93,94,22,22,22,15,16,22,20,20,20,20,20,95,20,15,16,15&
        ,15,16,16,15,16,22,22,15,16,15,16,15,16,10,11,10,11,13,14,13&
        ,14,10,11,10,11,12,12,13,14,13,14,12,12,15,15,15,15,16,16,16&
        ,16,15,15,16,16,16,16,15,15,15,15,16,16,20,15,16,22,34,28,29&
        ,77,20,96,20,20,83,84,20,20,15,15,16,16,15,21,15,16,101,102&
        ,21,85,22,86,15,16,97,98,15,16,99,100,21,20,20,15,16,15,16,87&
        ,88,89,22,15,16,15,16,103,104,105,15,16,15,16,75,76,15,16,15&
        ,16,15,16,15,16,92,93,94,22,22,15,16,15,16,19,22,15,16,15,16&
        ,20,15,16,15,16,19,15,16,15,16,15,16,15,16,15,16,15,16,19,19&
        ,15,16,15,16,15,16,34,15,15,16,16,15,16,15,15,16,16,15,16,15&
        ,15,16,16,15,16,15,15,16,16,15,16,22,19,19,10,10,12,13,12,13&
        ,10,12,13,12,10,12,12,13,15,15,16,16,15,16,28,29,41,43,43,42&
        ,15,16,15,19,19,19,15,16,19,19,15,16,26,26,24,25,24,25,15,16&
        ,15,16,26,27,27,19,34,31,22,22,19,19,26,26,26,26,15,16,19,22&
        ,19,19,15,16,22,22,26,26,15,16,24,25,15,15,16,15,16,23,23,23&
        ,22,22,22,19,19,19,19,26,26,26,26,24,24,25,24,25,24,25,25,24&
        ,25,27,27,27,27,21,19,22,26,19,28,29,28,29,24,25,24,25,19,19&
        ,20,20,20,21,21,22,22,31,31,60,61,60,61,28,29,28,29,24,25,24&
        ,25,15,15,16,16,15,16,15,15,16,16,15,16,15,15,16,16,15,16,15&
        ,15,16,16,15,16,15,15,15,15,16,16,15,16,15,15,16,16,16,16,15&
        ,16,15,15,16,16,15,16,15,15,16,16,15,16,15,15,15,15,15,15,15&
        ,16,16,16,16,15,15,15,15,16,16,15,15,15,15,15,15,15,16,16,16&
        ,16,16,16,16,16,16,16,16,16,15,15,15,15,16,16,16,16,15,15,15&
        ,15,16,15,15,15,16,16,15,15,16,16,15,15,16,16,15,16,15,15,15&
        ,15,16,16,15,16,15,15,16,16,16,16,15,16,15,15,16,16,15,16,15&
        ,15,16,16,15,16,15,15,15,15,16,16,15,16,15,15,16,16,16,16,15&
        ,16,41,41,42,42,43,19,19,19,19,19,19,19,19,19,19,19,19,19,19&
        ,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,10,10&
        ,10,10,11,10,11,13,13,10,11,10,11,10,11,10,10,11,12,12,10,11&
        ,10,10,11,12,12,13,14,13,14,19,19,19,19,20,15,16,18,20,18,15&
        ,15,16,16,15,16,18,15,16,18,18,18,15,16,18,18,15,16,20,20,18&
        ,18,20,18,18,18,18,15,16,15,16,18,18,18,20,18,18,18,18,15,16&
        ,15,16,15,16,18,18,18,18,15,16,18,18,15,16,19,19,15,16,15,16&
        ,20,20,18,18,15,16,18,18,18,18,18,18,18,15,16,15,16,18,18,15&
        ,16,34,34,15,16,20,20,31,31,31,31,44,45,44,45,15,16,15,16,15&
        ,16,15,16,15,16,15,16,28,29,28,29,44,45,44,45,22,22,19,28,29&
        ,28,29,15,16,15,16,10,13,12,34,34,34,34,34,34,34,34,34,34,34&
        ,34,34,34,34,34,34,34,19,19,19,19,19,19,19,19,19,19,19,19,19&
        ,19,19,19,19,19,60,61,60,15,16,15,16,60,61,60,61,15,16,15,16&
        ,15,16,15,16,21,21,22,19,19,19,19,22,22,22,22,20,20,20,20,60&
        ,60,61,60,61,60,61,61,60,61,22,22,22,22,10,13,12,10,12,10,12&
        ,13,13,12,34,34,34,34,22,22,21,21,22,22,22,22,24,24,25,24,25&
        ,25,22,22,10,12,12,13,28,28,28,29,22,22,22,22,19,19,23,23,26&
        ,26,15,15,16,16,15,16,15,15,15,15,16,16,15,16,15,15,16,16,16&
        ,16,15,16,15,15,16,16,15,16,15,15,15,15,16,16,15,15,16,16,16&
        ,16,15,16,15,16,15,15,16,16,15,16,15,15,15,15,16,16,15,15,16&
        ,16,16,16,15,16,15,16,20,15,16,20,15,16,22,15,16,22,15,16,15&
        ,16,15,16,15,16,15,16,23,26,22,24,25,15,16,34,34,31,20,15,16&
        ,21,15,16,15,16,60,61,34,19,28,29,28,29,28,28,29,28,29,28,29&
        ,29,28,29,28,29,28,29,11,10,11,14,13,11,10,11,12,12,14,13,14&
        ,12,12,15,15,16,16,15,15,16,16,15,15,16,16,11,10,11,14,13,11&
        ,10,11,12,12,14,13,14,12,12,15,15,16,16,15,15,16,16,15,15,16&
        ,16,11,10,11,14,13,11,10,11,12,12,14,13,14,12,12,11,10,11,12&
        ,12,11,10,11,12,12,14,13,14,12,12,11,10,11,12,12,14,13,11,10&
        ,11,12,12,14,13,14,12,12,11,10,11,14,13,11,10,11,12,12,14,13&
        ,14,12,12,11,10,11,14,13,11,10,11,12,12,14,13,14,12,12,11,10&
        ,11,14,13,11,10,11,12,12,14,13,14,12,12,11,10,11,14,13,11,10&
        ,11,12,12,14,13,14,12,12,11,10,11,12,12,11,10,11,12,12,14,13&
        ,14,12,12,11,10,11,12,12,14,13,11,10,11,12,12,14,13,14,12,12&
        ,11,10,11,14,13,11,10,11,12,12,14,13,14,12,12,11,10,11,14,13&
        ,11,10,11,12,12,14,13,14,12,12,11,10,11,12,12,11,10,11,12,12&
        ,14,13,14,12,12,11,10,11,12,12,14,13,11,10,11,12,12,14,13,14&
        ,12,12,11,10,11,14,13,11,10,11,12,12,14,13,14,12,12,10,12,12&
        ,13,15,16,10,10,12,13,12,13,10,12,13,12,22,22,10,12,12,13,19&
        ,19,19,19,34,34,34,34,34,34,34,34,34,34,34,34,46,47,34,34,34&
        ,34,26,26,15,16,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26&
        ,26,19,19,10,10,11,10,10,11,12,12,13,13,14,12,12,13,13,14,10&
        ,11,12,13,14,12,19,19,23,23,23,23,23,23,23,23,23,23,23,23,23&
        ,23,23,23,23,23,15,16,15,15,16,16,15,16,15,16,22,26,22,15,16&
        ,26,19,19,20,15,16,20,15,15,16,16,15,16,21,15,16,15,16,60,61&
        ,19,24,25,60,61,21,21,19,19,26,44,44,45,44,45,45,10,12,12,13&
        ,34,20,20,15,16,15,15,16,16,15,16,21,21,15,16,19,28,29,28,29&
        ,19,19,15,16,10,12,12,13,15,16,15,16,15,16,10,12,12,13,15,16&
        ,15,16,15,15,15,15,16,16,15,15,16,16,16,16,15,16,15,16,10,12&
        ,12,13,46,47,28,29,22,22,10,12,12,13,10,12,12,13,10,10,12,13&
        ,12,13,10,12,13,12,19,19,10,10,12,13,12,13,10,12,13,12,10,12&
        ,12,13,19,19,19,19,10,10,12,13,12,13,10,12,13,12,21,15,15,16&
        ,16,15,16,28,29,28,29,19,19,21,21,15,16,21,21,21,21,15,16,20&
        ,20,21,21,21,21,21,21,21,21,15,16,21,21,21,21,21,21,21,21,21&
        ,21,21,21,21,21,15,15,15,15,16,16,15,15,16,16,16,16,15,16,15&
        ,16,21,21,21,21,21,21,22,22,28,29,21,21,21,21,21,21,46,47,19&
        ,19,10,10,12,13,12,13,10,12,13,12,28,28,29,28,29,29,28,29,28&
        ,29,22,22,19,22,22,22,22,15,16,19,19,22,22,15,16,15,16,22,22&
        ,22,22,22,22,22,22,22,22,15,15,15,15,16,16,15,15,16,16,16,16&
        ,15,16,15,16,22,22,22,22,22,22,22,22,22,22,22,22,19,19,19,19&
        ,15,16,15,16,15,16,15,16,24,25,24,25,24,24,25,24,25,24,25,25&
        ,24,25,28,29,28,29,28,29,41,41,42,42,43,27,27,15,16,27,27,27&
        ,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27&
        ,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,75,76,15,16,15&
        ,16,22,21,22,15,16,60,61,15,16,31,15,16,31,34,20,20,22,28,29&
        ,34,34,34,34,34,34,34,34,34,34,34,34,19,19,19,19,19,19,19,19&
        ,19,19,19,19,10,11,13,14,12,10,11,13,14,12,10,11,13,14,12,10&
        ,11,13,14,12,10,11,13,14,12,10,11,13,14,12,10,11,13,14,12,23&
        ,23,23,23,20,20,20,20,20,15,15,15,16,20,15,16,20,15,16,20,15&
        ,16,15,15,16,16,15,15,16,16,15,16,18,18,30,32,37,38,39,40,40&
        ,44,45,44,45,44,45,44,45,44,45,41,42,43,41,42,43,15,16,15,16&
        ,15,16,15,16,15,16,15,16,15,16,15,16,15,16,15,16,15,16,15,16&
        ,15,16,15,16,15,16,15,16,15,16,48,49,50,51,21,21,21,21,21,21&
        ,21,21,21,21,21,21,21,21,21,21,22,22,22,22,22,22,22,22,22,22&
        ,22,22,22,22,15,16,15,16,15,16,31,31,60,61,34,34,34,34,34,34&
        ,34,34,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23&
        ,23,23,23,23,27,27,27,27,27,28,29,53,54,55,53,54,55,53,54,55&
        ,20,20,20,20,20,20,20,34,34,34,34,34,34,34,19,19,19,19,19,19&
        ,19,19,19,19,19,19,19,19,19,19,19,10,11,13,14,12,10,11,13,14&
        ,12,10,11,13,14,12,10,11,13,14,12,10,11,13,14,12,21,21,21,21&
        ,22,22,18,18,18,40,40,40,40,40,40,44,45,44,45,44,45,22,22,10&
        ,11,13,14,12,10,11,13,14,12,10,11,13,14,12,10,11,13,14,12,10&
        ,11,13,14,12,10,11,13,14,12,10,11,13,14,12,10,11,13,14,12,10&
        ,11,13,14,12,10,11,13,14,12,10,11,13,14,12,10,11,13,14,12,10&
        ,11,13,14,12,10,11,13,14,12,10,11,13,14,12,10,11,13,14,12,18&
        ,18,18,18,18,18,18,18,18,18,18,41,42,43,41,42,43,41,42,43,41&
        ,42,43,41,42,43,41,42,43,41,42,43,41,42,43,41,42,43,41,42,43&
        ,21,21,41,42,43,41,42,43,41,42,43,21,21,22,15,16,15,16,15,16&
        ,15,16,15,16,21,21,21,28,29,10,11,13,14,12,31,31,31,53,54,55&
        ,53,54,55,53,54,55,24,25,27,24,25,15,16,15,16,22,22,22,22,22&
        ,22,60,61,60,61,23,23,28,29,28,29,28,29,28,29,28,29,28,29,28&
        ,29,28,29,28,29,28,29,28,29,28,29,28,29,28,29,28,29,28,29,53&
        ,54,55,53,54,55,31,31,31,31,31,31,31,23,23,23,23,24,25,24,25&
        ,24,25,27,27,27,27,27,27,27,83,84,95,87,87,88,88,89,81,82,71&
        ,71,73,72,31,60,61,34,19,37,37,38,38,39,28,29,27,35,41,43,43&
        ,42,31,60,61,34,19,30,32,37,37,38,38,39,44,45,40,46,47,60,61&
        ,10,12,12,13,24,25,28,29,30,32,37,39,39,38,44,45,46,47,53,55&
        ,55,54,41,43,43,42,19,28,29,26,27,46,47,40,41,41,42,42,43,44&
        ,45,46,47,41,43,43,42,5,3,4,6,20,20,20,20,20,15,15,16,16,15&
        ,10,11,13,14,14,12,15,16,21,15,16,21,22,15,16,15,16,20,20,20&
        ,20,20,21,20,20,20,20,20,15,16,15,16,20,20,20,20,21,22,22,22&
        ,22,15,16,15,16,15,16,15,15,15,16,16,15,15,15,15,11,11,10,14&
        ,13,14,12,12,11,10,14,13,12,12,15,16,15,16,15,16,15,16,15,16&
        ,15,16,21,21,21,15,16,15,15,16,15,16,22,15,16,20,15,16,18,21&
        ,22,78,78,70,70,66,67,66,67,64,64,65,65,73,72,71,71,68,69,68&
        ,66,67,78,78,70,70,62,62,62,62,70,78,70,70,78,78,62,62,62,62&
        ,64,65,65,62,63,62,64,64,81,71,127,127,66,127,127,127,127,127&
        ,127,127,69,68,76,119,16,13,16,13,20,21,22,19,19,18,18,21,21&
        ,27,27,16,15,16,15,16,16,16,64,66,65,66,66,67,68,127,68,68,68&
        ,127,127,127,19,19,18,18,21,21,14,13,16,14,13,14,13,14,13,14&
        ,13,14,14,13,14,14,13,14,13,14,13,14,13,14,13,127,127,14,13&
        ,14,13,16,16,15,15,15,15,10,11,34,19,15,15,21,22,28,19,11,10&
        ,11,10,34,26,23,10,11,41,20,21,22,24,20,15,15,15,10,11,15,15&
        ,15,15,10,11,15,15,15,16,15,15,16,16,15,15,16,16,15,16,15,15&
        ,16,16,15,15,16,16,15,16,16,16,15,15,15,16,16,15,15,15,16,16&
        ,15,15,15,16,16,16,15,15,15,16,16,16,15,15,16,16,16,15,15,16&
        ,16,16,16,16,10,11,12,10,11,12,10,11,12,14,13,10,11,12,13,14&
        ,12,13,14,12,13,14,14,13,34,34,34,34,34,19,19,19,19,19,15,15&
        ,16,16,15,15,16,16,15,16,15,16,15,16,15,15,16,16,15,15,16,16&
        ,16,16,21,21,21,21,21,22,22,22,22,22,28,29,28,29,28,29,28,29&
        ,28,29,28,29,28,29,29,19,19,19,19,19,10,11,10,11,12,12,10,11&
        ,10,11,12,12,14,13,11,11,10,10,11,10,12,12,12,13,14,14,13,10&
        ,11,10,11,12,12,10,11,10,11,12,12,13,14,13,14,10,11,10,11,12&
        ,12,14,13,14,13,12,12,12,14,13,14,13,14,13,10,11,11,12,12,13&
        ,14,14,13,10,11,12,12,14,13,13,14,12,12,14,13,14,13,13,14,13&
        ,14,13,14,34,34,34,34,34,26,26,26,26,26,23,23,23,23,23,10,11&
        ,12,12,13,14,13,14,41,43,41,43,42,43,42,41,43,41,43,42,43,42&
        ,42,20,20,20,20,20,21,21,21,21,21,22,22,22,22,22,24,25,24,25&
        ,24,25,24,25,24,25,24,25,24,25,25,20,20,20,20,20,15,16,15,16&
        ,16,15,16,15,16,16,15,15,16,15,15,16,15,15,16,16,15,15,16,16&
        ,15,16,16,15,16,16,16,10,11,12,10,11,12,13,14,12,13,14,10,11&
        ,12,10,11,12,14,13,12,13,14,14,13,15,15,15,15,15,15,15,15,16&
        ,16,15,15,15,15,16,16,15,15,15,15,16,15,15,16,15,16,15,15,15&
        ,15,16,16,15,15,15,15,16,16,15,15,16,16,16,16,15,15,15,16,16&
        ,15,16,15,16,16,10,11,12,12,13,14,13,14,15,16,16,15,15,16,16&
        ,16/)
    arr_p2(1:3466) = (/11,11,10,10,11,11,11,10,11,10,12&
        ,10,11,10,11,12,11,10,11,12,11,12,12,10,11,12,11,11,12,12,10&
        ,11,10,12,12,10,11,12,12,10,11,12,11,12,11,13,14,12,10,11,10&
        ,11,13,14,13,14,12,10,11,10,11,13,12,12,11,13,12,12,10,14,12&
        ,12,11,14,14,12,12,10,14,12,12,10,13,12,12,10,10,14,12,12,11&
        ,13,12,12,11,11,13,14,13,14,12,10,11,13,14,13,14,12,10,11,10&
        ,11,14,12,14,12,13,14,12,12,13,14,12,12,14,13,14,12,13,13,14&
        ,12,12,13,14,14,12,12,13,14,13,12,12,14,13,14,12,13,14,13,14&
        ,12,14,13,14,13,13,14,13,14,14,13,14,13,135,135,135,135,135&
        ,135,17,16,17,16,17,16,17,17,15,15,15,15,15,16,15,16,16,16,15&
        ,15,15,16,15,16,15,16,16,16,16,16,19,19,1,1,1,1,1,1,15,15,16&
        ,16,16,70,70,78,78,78,70,3,3,4,4,4,3,1,1,1,1,1,70,78,77,77,21&
        ,21,21,22,1,22,1,22,22,31,31,34,31,31,19,1,1,1,19,28,29,29,28&
        ,34,34,19,19,1,1,34,34,1,1,26,24,25,25,24,1,1,1,26,28,29,27&
        ,27,34,20,15,16,21,22,1,77,77,77,70,70,78,78,78,70,70,78,96&
        ,96,83,84,84,83,70,70,70,70,78,78,78,78,78,78,78,78,70,70,70&
        ,70,70,70,78,78,81,82,81,82,73,72,73,72,71,71,81,82,81,82,73&
        ,72,73,72,71,71,1,1,1,1,1,1,1,1,20,1,15,16,1,1,21,22,15,15,16&
        ,16,16,21,21,21,1,1,22,1,22,1,22,22,1,1,31,31,1,1,31,34,31,60&
        ,61,61,60,1,1,1,19,28,29,29,28,1,1,1,34,34,19,19,1,1,34,34,26&
        ,26,24,25,25,24,1,1,1,26,34,96,96,106,106,80,95,95,95,107,107&
        ,108,99,100,100,99,95,75,76,76,75,109,110,110,109,111,111,112&
        ,112,83,84,111,111,113,114,114,113,95,109,109,110,110,110,109&
        ,75,75,76,76,76,75,115,115,116,116,116,115,113,113,114,114&
        ,114,113,7,6,19,8,9,8,8,9,9,9,8,8,9,7,7,7,7,8,8,9,9,9,8,7,7,7&
        ,7,7,7,15,16,16,10,13,12,40,40,28,29,59,59,28,29,28,29,29,28&
        ,50,51,51,50,27,28,29,26,27,19,44,45,28,29,37,39,39,38,35,35&
        ,23,59,44,45,59,59,44,45,44,45,40,40,40,40,15,16,16,52,52,41&
        ,42,43,41,43,42,41,43,43,42,41,43,43,42,41,42,43,43,41,43,41&
        ,42,42,43,41,43,43,42,117,85,117,108,117,113,114,114,113,75&
        ,76,76,75,113,114,85,75,76,75,76,75,76,75,76,75,76,76,75,75&
        ,76,76,75,75,76,76,75,83,83,84,84,84,83,101,101,102,102,102&
        ,101,97,97,98,98,98,97,99,99,100,100,100,99,87,87,88,88,89,89&
        ,89,87,89,89,87,87,88,88,88,89,113,113,114,114,114,113,75,75&
        ,76,76,76,75,63,63,63,66,67,66,67,64,64,65,65,65,64,65,64,63&
        ,63,66,67,66,67,66,67,66,64,65,64,65,68,69,68,69,68,69,68,69&
        ,68,69,68,69,67,66,67,66,65,64,65,64,63,68,68,69,66,67,67,66&
        ,65,64,118,118,119,119,119,118,92,92,93,93,94,94,94,92,94,94&
        ,92,92,93,93,93,94,90,90,91,91,91,90,120,120,121,121,121,120&
        ,103,103,104,104,105,105,105,103,105,105,103,103,104,104,104&
        ,105,95,95,95,95,95,63,66,67,66,65,64,64,65,64,69,68,69,67,66&
        ,81,71,71,73,63,64,65,64,66,67,66,69,68,69,65,64,67,66,11,10&
        ,11,12,12,12,12,14,13,12,12,12,12,14,13,13,14,13,12,12,14,13&
        ,13,14,13,12,12,12,12,12,12,10,12,12,13,18,18,18,21,18,22,18&
        ,18,18,18,18,18,21,18,18,22,21,22,18,18,60,61,18,18,18,18,34&
        ,31,18,19,10,13,12,18,18,18,18,27,19,22,18,28,29,28,29,18,18&
        ,18,18,18,18,60,61,21,21,18,18,22,22,18,18,122,123,18,18,18&
        ,18,18,18,26,26,18,18,26,23,22,21,10,13,12,18,18,18,18,34,22&
        ,107,107,83,84,111,111,75,76,87,89,89,88,75,76,76,75,99,100&
        ,100,99,113,114,114,113,75,76,76,75,75,76,76,75,75,76,76,75&
        ,75,76,86,83,84,84,83,90,91,91,90,86,86,86,83,84,99,100,87,89&
        ,89,88,75,76,92,94,94,93,103,105,105,104,83,84,99,100,87,89&
        ,89,88,92,94,94,93,103,105,105,104,46,47,60,61,61,30,32,32,30&
        ,24,25,25,24,48,49,49,48,44,45,45,44,31,34,31,10,12,12,13,37&
        ,39,39,38,10,12,12,13,10,13,12,12,10,12,10,13,13,12,10,12,12&
        ,13,28,28,29,29,28,29,28,29,28,29,10,12,12,13,24,25,28,29,53&
        ,55,55,54,28,29,28,29,28,29,28,29,27,27,27,27,28,29,29,29,41&
        ,43,43,42,28,29,28,29,28,29,60,60,61,61,61,60,37,37,38,38,39&
        ,39,39,37,39,39,37,37,38,38,38,39,28,28,29,29,29,28,41,41,43&
        ,43,41,41,42,42,43,43,42,42,43,41,42,43,24,24,25,25,25,24,53&
        ,53,55,55,53,53,54,54,55,55,54,54,55,53,54,55,34,34,34,23,23&
        ,23,23,26,26,26,44,45,45,44,48,49,49,48,59,59,27,26,52,27,27&
        ,33,33,34,19,34,19,19,19,19,26,26,27,27,19,19,19,19,99,100&
        ,100,99,87,88,89,89,87,89,87,88,88,89,75,76,76,75,83,83,83,84&
        ,84,84,84,84,83,83,83,83,83,84,84,92,92,93,93,94,94,92,92,93&
        ,93,94,94,97,97,97,98,98,98,98,98,97,97,97,97,97,98,98,103&
        ,103,104,104,105,105,103,103,104,104,105,105,99,99,99,100,100&
        ,100,100,100,99,99,99,99,99,100,100,87,87,87,88,88,89,89,89&
        ,87,87,87,87,87,89,89,89,89,89,87,87,88,88,88,88,88,89,89,89&
        ,89,89,88,88,113,113,113,114,114,114,114,114,113,113,113,113&
        ,113,114,114,75,75,75,76,76,76,76,76,75,75,75,75,75,76,76,109&
        ,109,109,110,110,110,110,110,109,109,109,109,109,110,110,118&
        ,118,118,119,119,119,119,119,118,118,118,118,118,119,119,92&
        ,92,92,93,93,94,94,94,92,92,92,92,92,94,94,94,94,94,92,92,93&
        ,93,93,93,93,94,94,94,94,94,93,93,90,90,90,91,91,91,91,91,90&
        ,90,90,90,90,91,91,120,120,120,121,121,121,121,121,120,120&
        ,120,120,120,121,121,103,103,103,104,104,105,105,105,103,103&
        ,103,103,103,105,105,105,105,105,103,103,104,104,104,104,104&
        ,105,105,105,105,105,104,104,28,28,28,29,29,29,29,29,28,28,28&
        ,28,28,29,29,96,96,96,96,108,108,83,84,83,83,84,84,84,83,83&
        ,84,75,76,95,95,95,95,81,71,71,73,83,84,99,100,87,89,89,88,92&
        ,94,94,93,85,85,103,105,105,104,83,84,126,126,99,100,87,89,89&
        ,88,75,76,92,94,94,93,103,105,105,104,75,76,75,75,75,76,76,76&
        ,75,75,75,75,75,76,76,76,76,76,76,76,75,75,75,76,118,119,83&
        ,84,99,100,87,89,89,88,75,76,92,94,94,93,103,105,105,104,106&
        ,106,101,101,102,102,102,101,124,124,124,80,85,85,85,95,101&
        ,102,95,95,95,85,97,97,98,98,98,97,85,85,85,86,86,95,95,86,95&
        ,95,85,85,75,76,97,98,85,70,78,70,78,70,78,85,85,85,85,77,95&
        ,107,107,107,99,99,100,100,100,99,99,100,111,111,95,99,100&
        ,100,99,99,100,96,96,77,77,77,77,106,106,95,95,107,107,96,96&
        ,96,96,111,111,112,112,87,87,89,89,87,87,88,88,89,89,88,88,89&
        ,87,88,89,106,106,106,106,80,80,95,95,75,76,95,95,95,95,107&
        ,107,107,107,99,100,99,99,100,100,100,99,99,100,75,76,75,76&
        ,75,75,76,76,76,75,75,76,111,111,111,111,87,89,89,88,113,114&
        ,113,113,114,114,114,113,113,114,85,118,118,119,119,119,118&
        ,118,119,119,118,118,119,83,84,124,124,97,98,99,100,111,111&
        ,113,114,87,89,89,88,113,114,75,76,117,117,63,64,65,64,66,67&
        ,66,69,68,69,65,64,67,66,92,92,94,94,92,92,93,93,94,94,93,93&
        ,94,92,93,94,118,119,92,94,94,93,118,119,85,85,120,121,103&
        ,105,105,104,85,85,90,91,90,91,90,90,91,91,91,90,90,91,92,94&
        ,92,93,94,93,94,92,93,94,95,85,95,75,76,90,91,126,126,120,121&
        ,83,84,85,85,86,86,99,100,87,89,89,88,113,114,75,76,103,103&
        ,105,105,103,103,104,104,105,105,104,104,105,103,104,105,118&
        ,119,92,94,94,93,90,91,103,105,105,104,103,105,105,104,118&
        ,119,119,118,90,91,91,90,99,100,100,99,87,88,89,89,87,89,87&
        ,88,88,89,90,91,91,90,85,85,85,85,85,85,85,83,84,126,126,97&
        ,98,99,100,87,89,89,88,113,114,75,76,63,64,65,64,66,67,66,69&
        ,68,69,65,64,67,66,118,119,92,94,94,93,90,91,103,105,105,104&
        ,1,1,31,31,34,34,34,19,19,19,19,19,19,36,36,34,35,35,19,19,85&
        ,85,7,7,7,101,102,97,98,99,100,87,88,89,92,93,94,77,70,78,80&
        ,96,85,86,125,113,114,75,76,70,70,70,70,70,78,78,78,78,78,113&
        ,113,113,113,113,114,114,114,114,114,92,92,92,92,92,93,93,93&
        ,93,93,94,94,94,94,94,77,70,78,79,20,15,16,15,16,15,16,16,16&
        ,18,18,18,21,21,21,22,22,22,10,11,10,11,13,14,13,14,12,12,70&
        ,78,85,85,85,85,85,85,86,70,70,78,78,80,80,85,85,86,86,85,85&
        ,85,86,86,86,80,80,96,96,83,83,84,84,101,101,102,102,85,85,86&
        ,86,97,97,98,98,111,111,107,107,103,103,104,104,105,105,113&
        ,113,114,114,85,85,85,85,96,106,95,99,100,87,88,89,103,104&
        ,105,113,114,92,93,94,70,78,85,86,87,88,89,103,104,105,92,93&
        ,94,126,92,92,93,93,94,94,77,85,85,85,77,70,78,80,96,83,84,95&
        ,96,83,84,106,95,101,102,85,86,97,98,99,100,87,88,89,125,113&
        ,114,92,93,94,85,92,93,94,126,86,86,85,85,85,75,75,75,76,76&
        ,76,83,84,87,88,89,75,76,85,86,125,113,114,75,76,83,84,101&
        ,102,97,98,99,100,87,88,89,103,104,105,92,93,94,96,96,96,96&
        ,96,83,83,83,83,83,84,84,84,84,84,106,106,106,106,106,95,95&
        ,95,95,95,83,84,97,98,75,76,103,104,105,70,78,80,103,104,105&
        ,103,103,104,104,105,105,97,98,101,101,101,101,101,102,102&
        ,102,102,102,85,85,85,85,85,86,86,86,86,86,97,97,97,97,97,98&
        ,98,98,98,98,99,99,99,99,99,100,100,100,100,100,87,87,87,87&
        ,87,88,88,88,88,88,89,89,89,89,89,103,103,103,103,103,104,104&
        ,104,104,104,105,105,105,105,105,75,75,75,75,75,76,76,76,76&
        ,76,96,83,84,81,82,73,72,71,124,86,125,96,96,96,83,83,83,84&
        ,84,84,99,99,99,100,100,100,87,87,87,88,88,88,89,89,89,75,75&
        ,75,76,76,76,101,102,92,92,92,93,93,93,94,94,94,85,125,96,99&
        ,99,100,100,87,87,88,88,89,89,86,75,76,85,85,125,125,125,125&
        ,125,92,93,94,87,87,87,88,88,88,89,89,89,85,85,77,86,86,75,75&
        ,76,76,83,84,101,102,99,100,75,75,76,76,75,76,96,96,83,83,84&
        ,84,99,99,100,100,87,87,88,88,89,89,103,103,104,104,105,105&
        ,75,75,76,76,92,92,93,93,94,94,83,83,83,84,84,84,83,84,87,88&
        ,89,75,76,80,103,104,105,103,103,104,104,105,105,83,84,87,88&
        ,89,75,76,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1&
        ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1&
        ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,135,135,135,135,20,15,16,21,22&
        ,15,15,16,16,16,135,135,135,135,135,135,18,18,21,21,21,22,22&
        ,22,22,31,31,60,61,20,20,34,31,19,31,10,13,12,60,61,61,60,15&
        ,16,15,34,31,19,10,13,12,28,29,29,28,15,16,16,15,15,16,16,15&
        ,15,16,16,15,15,15,16,16,16,15,15,16,16,15,15,16,16,34,34,19&
        ,19,34,34,26,26,19,19,23,23,24,25,19,15,16,16,24,25,25,24,26&
        ,27,27,135,135,135,135,135,135,12,12,13,14,15,15,15,15,16,16&
        ,16,16,15,15,16,16,15,15,15,16,16,13,14,10,11,15,15,15,15,16&
        ,15,12,12,10,11,16,16,16,16,15,15,15,16,16,16,135,135,16,15&
        ,10,11,12,11,10,12,12,12,14,14,13,13,13,13,16,16,16,16,17,17&
        ,17,62,62,62,62,62,62,62,62,17,127,127,127,127,127,127,13,12&
        ,13,13,14,13,12,12,13,14,14,13,13,14,127,127,127,127,127,127&
        ,84,84,93,98,98,100,100,88,88,114,114,76,110,110,119,93,93,91&
        ,91,121,121,104,104,29,29,27,27,13,13,78,78,72,73,128,128,11&
        ,10,41,41,128,128,128,128,128,128,128,128,128,128,128,128,128&
        ,128,128,75,75,87,128,128,128,128,15,22,34,28,103,103,15,15&
        ,15,41,28,28,22,22,129,128,130,130,129,129,129,129,128,128&
        ,130,129,131,131,130,130,130,130,129,129,131,130,131,131,11&
        ,10,12,11,10,11,10,12,11,10,12,13,14,10,11,12,12,13,14,11,10&
        ,12,13,14,12,13,14,13,14,12,13,14,13,14,43,43,41,43,43,41,42&
        ,42,43,41,41,42,42,43,41,41,42,43,43,42,43,43,42,42,129,129&
        ,130,130,131,129,129,130,130,131,129,129,128,128,130,130,129&
        ,129,129,128,130,129,131,130,130,130,129,129,131,131,130,130&
        ,131,131,129,130,129,130,131,129,130,129,130,131,129,128,130&
        ,129,129,128,130,129,131,130,130,129,131,130,131,129,130,129&
        ,130,131,129,129,129,129,128,128,130,130,130,130,129,129,128&
        ,128,131,131,131,131,131,131,130,130,130,129,129,129,129,129&
        ,129,129,129,128,128,130,130,130,130,129,129,128,128,128,128&
        ,131,131,131,131,130,130,129,129,129,129,131,131,131,130,130&
        ,130,130,130,130,130,130,130,129,129,128,128,128,128,131,131&
        ,130,130,129,129,129,129,131,131,130,130,130,130,131,131,131&
        ,131,131,131,129,129,130,130,131,129,129,130,130,131,129,129&
        ,130,130,131,76,76,75,76,75,75,76,76,89,87,88,89,87,88,89,89&
        ,87,88,89,87,88,89,88,129,129,130,130,131,129,129,130,130,131&
        ,129,129,130,130,131,129,128,130,129,129,128,130,129,131,130&
        ,130,129,131,130,131,15,16,15,16,16,22,22,22,22,22,34,34,34&
        ,34,34,28,29,28,28,29,28,28,29,28,29,28,29,28,29,29,28,29,29&
        ,28,29,29,105,105,103,104,104,105,103,103,104,105,105,105,105&
        ,103,104,104,105,103,103,104,105,105,104,104,15,15,16,16,15&
        ,15,16,16,16,16,15,15,16,16,16,16,15,16,15,16,16,15,16,16,16&
        ,16,15,15,16,16,16,16,15,15,16,16,16,16,16,16,16,16,16,16,15&
        ,16,16,16,16,43,41,42,43,42,29,29,28,29,28,28,29,29,22,22,22&
        ,22,22,22,22,22/)
    arr_p3(1:3466) = (/135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,17,135,17,135,17,135,135,17,17,15,17,15,17,17,15,15,17&
        ,17,16,17,17,17,17,16,16,17,16,17,17,17,17,135,135,135,135&
        ,135,135,135,135,135,135,135,1,1,1,1,1,1,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,1,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,19,19,19,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,23,23,23,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,77,77,77,77,79,77,70,70,78,78,78,70&
        ,79,79,79,79,80,80,80,80,77,77,96,96,83,84,77,77,96,77,77,77&
        ,77,83,84,84,83,77,80,95,86,70,78,78,70,97,98,98,97,21,21,79&
        ,79,83,84,106,106,83,84,95,95,135,135,21,21,106,106,101,102&
        ,70,78,85,85,79,80,124,85,79,79,79,101,102,102,101,80,106,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,85,85,85,85,85,85,85,85,85,85,85&
        ,85,85,85,85,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,15,16,135,135,135,135,135,135,135,135,135,135,135,15,16&
        ,16,135,135,135,135,135,135,135,135,135,135,22,22,22,15,15,16&
        ,16,16,16,16,16,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,21,21,21,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,16,135,16,17,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,85,85,135,135,135&
        ,135,135,135,135,135,135,135,28,28,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,41,41,41,41,135,135,128,128,28,135,135,135,11,10&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,29,29,28,28,28,29,29,28&
        ,28,28,29,28,28,29,29,28,29,28,28,29,29,28,29,29,29,28,28,29&
        ,29,29,28,28,29,29,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,43,41,42,43&
        ,42,43,41,42,43,42,43,41,42,43,42,43,41,41,43,41,41,42,43,43&
        ,41,42,43,43,41,42,42,43,42,42,43,42,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,129,129,128,128,130,130,129,129,128,128,131,131&
        ,130,130,129,129,129,128,130,129,128,131,130,129,131,130,130&
        ,130,129,129,128,128,131,131,130,130,129,129,131,131,130,130&
        ,131,131,29,28,29,28,29,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,12,10,11,13,14,12,13&
        ,14/)
    arr_p4(1:3466) = (/135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,17,135,17,135,135,17,17,135,135&
        ,17,135,135,135,135,17,17,135,17,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,77,77,135,135,135,135,135,135,135,135,135,135,135,135,77&
        ,77,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,17,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135,135,135,135,135,135,135,135,135,135,135&
        ,135,135,135,135,135/)

  end subroutine load_arrays

  ! ************************************
  ! solves linear least squares
  subroutine llsq(n, x, y, a, b)

    !****************************************************
    !
    !! LLSQ solves a linear least squares problem matching a line to data.
    !
    !  Discussion:
    !
    !    A formula for a line of the form Y = A * X + B is sought, which
    !    will minimize the root-mean-square error to N data points
    !    ( X(I), Y(I) );
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    07 March 2012
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    In: N, the number of data values.
    !
    !    In: X(N), Y(N), the coordinates of the data points.
    !
    !    Out: A, B, the slope and Y-intercept of the
    !    least-squares approximant to the data.
    !
    implicit none
    integer,intent(in)::n
    real*8,intent(out)::a, b
    real*8,intent(in)::x(n), y(n)
    real*8::bot, top, xbar, ybar

    ! special case
    if(n == 1) then
      a = 0d0
      b = y(1)
      return
    end if

    ! average X and Y
    xbar = sum(x) / n
    ybar = sum(y) / n

    ! compute beta
    top = dot_product(x(:) - xbar, y(:) - ybar)
    bot = dot_product(x(:) - xbar, x(:) - xbar)

    ! if top is zero a is zero
    if(top==0d0) then
      a = 0d0
    else
      a = top / bot
    end if

    b = ybar - a * xbar

  end subroutine llsq

end module krome_subs
