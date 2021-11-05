
!############### MODULE ##############
module krome_commons
  implicit none

  ! *************************************************************
  !  This file has been generated with:
  !  KROME 14.08.dev on 2021-04-07 19:37:24
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
  integer,parameter::idx_E=1
  integer,parameter::idx_GRAINk=2
  integer,parameter::idx_Hk=3
  integer,parameter::idx_Dk=4
  integer,parameter::idx_Ck=5
  integer,parameter::idx_Ok=6
  integer,parameter::idx_CNk=7
  integer,parameter::idx_OHk=8
  integer,parameter::idx_ODk=9
  integer,parameter::idx_H2_PARA=10
  integer,parameter::idx_H2_ORTHO=11
  integer,parameter::idx_HD=12
  integer,parameter::idx_D2_PARA=13
  integer,parameter::idx_D2_ORTHO=14
  integer,parameter::idx_H=15
  integer,parameter::idx_D=16
  integer,parameter::idx_GRAIN0=17
  integer,parameter::idx_HE=18
  integer,parameter::idx_CO=19
  integer,parameter::idx_C=20
  integer,parameter::idx_N=21
  integer,parameter::idx_O=22
  integer,parameter::idx_N2=23
  integer,parameter::idx_NH=24
  integer,parameter::idx_ND=25
  integer,parameter::idx_NO=26
  integer,parameter::idx_O2=27
  integer,parameter::idx_OH=28
  integer,parameter::idx_OD=29
  integer,parameter::idx_C2H=30
  integer,parameter::idx_C2=31
  integer,parameter::idx_C2D=32
  integer,parameter::idx_C2N=33
  integer,parameter::idx_CN=34
  integer,parameter::idx_C3=35
  integer,parameter::idx_CCO=36
  integer,parameter::idx_CH2=37
  integer,parameter::idx_CD2=38
  integer,parameter::idx_CHD=39
  integer,parameter::idx_CO2=40
  integer,parameter::idx_H2O=41
  integer,parameter::idx_D2O=42
  integer,parameter::idx_HDO=43
  integer,parameter::idx_HCN=44
  integer,parameter::idx_DCN=45
  integer,parameter::idx_HCO=46
  integer,parameter::idx_DCO=47
  integer,parameter::idx_HNC=48
  integer,parameter::idx_DNC=49
  integer,parameter::idx_HNO=50
  integer,parameter::idx_DNO=51
  integer,parameter::idx_N2O=52
  integer,parameter::idx_NH2=53
  integer,parameter::idx_ND2=54
  integer,parameter::idx_NHD=55
  integer,parameter::idx_NO2=56
  integer,parameter::idx_O2H=57
  integer,parameter::idx_O2D=58
  integer,parameter::idx_OCN=59
  integer,parameter::idx_CH=60
  integer,parameter::idx_CD=61
  integer,parameter::idx_H3j_PARA=62
  integer,parameter::idx_H3j_ORTHO=63
  integer,parameter::idx_H2Dj_PARA=64
  integer,parameter::idx_H2Dj_ORTHO=65
  integer,parameter::idx_D2Hj_PARA=66
  integer,parameter::idx_D2Hj_ORTHO=67
  integer,parameter::idx_D3j_ORTHO=68
  integer,parameter::idx_D3j_META=69
  integer,parameter::idx_Hj=70
  integer,parameter::idx_HDj=71
  integer,parameter::idx_D2j_ORTHO=72
  integer,parameter::idx_D2j_PARA=73
  integer,parameter::idx_HEj=74
  integer,parameter::idx_HCOj=75
  integer,parameter::idx_DCOj=76
  integer,parameter::idx_Cj=77
  integer,parameter::idx_Dj=78
  integer,parameter::idx_Nj=79
  integer,parameter::idx_Oj=80
  integer,parameter::idx_H2j_PARA=81
  integer,parameter::idx_H2j_ORTHO=82
  integer,parameter::idx_CHj=83
  integer,parameter::idx_CDj=84
  integer,parameter::idx_NOj=85
  integer,parameter::idx_O2j=86
  integer,parameter::idx_CH2j=87
  integer,parameter::idx_CD2j=88
  integer,parameter::idx_CHDj=89
  integer,parameter::idx_HNOj=90
  integer,parameter::idx_DNOj=91
  integer,parameter::idx_NH2j=92
  integer,parameter::idx_ND2j=93
  integer,parameter::idx_NHDj=94
  integer,parameter::idx_COj=95
  integer,parameter::idx_C2j=96
  integer,parameter::idx_OHj=97
  integer,parameter::idx_ODj=98
  integer,parameter::idx_C2Hj=99
  integer,parameter::idx_C2Dj=100
  integer,parameter::idx_NHj=101
  integer,parameter::idx_NDj=102
  integer,parameter::idx_H2Oj=103
  integer,parameter::idx_D2Oj=104
  integer,parameter::idx_HDOj=105
  integer,parameter::idx_CNj=106
  integer,parameter::idx_C3j=107
  integer,parameter::idx_C2Oj=108
  integer,parameter::idx_HOCj=109
  integer,parameter::idx_DOCj=110
  integer,parameter::idx_C2Nj=111
  integer,parameter::idx_CNCj=112
  integer,parameter::idx_HCNj=113
  integer,parameter::idx_DCNj=114
  integer,parameter::idx_HNCj=115
  integer,parameter::idx_DNCj=116
  integer,parameter::idx_NCOj=117
  integer,parameter::idx_N2Hj=118
  integer,parameter::idx_N2Dj=119
  integer,parameter::idx_O2Hj=120
  integer,parameter::idx_O2Dj=121
  integer,parameter::idx_HEHj=122
  integer,parameter::idx_HEDj=123
  integer,parameter::idx_N2j=124
  integer,parameter::idx_CO2j=125
  integer,parameter::idx_NO2j=126
  integer,parameter::idx_D3j_PARA=127
  integer,parameter::idx_H3Oj=128
  integer,parameter::idx_H2DOj=129
  integer,parameter::idx_HD2Oj=130
  integer,parameter::idx_D3Oj=131
  integer,parameter::idx_CR=132
  integer,parameter::idx_g=133
  integer,parameter::idx_Tgas=134
  integer,parameter::idx_dummy=135
  integer,parameter::nrea=3466
  integer,parameter::nmols=131
  integer,parameter::nspec=135
  integer,parameter::natoms=8
  integer,parameter::ndust=0
  integer,parameter::ndustTypes=0
  integer,parameter::nPhotoBins=0
  integer,parameter::nPhotoRea=0

  !cooling index
  integer,parameter::idx_cool_h2 = 1
  integer,parameter::idx_cool_h2gp = 2
  integer,parameter::idx_cool_atomic = 3
  integer,parameter::idx_cool_cen = 3
  integer,parameter::idx_cool_hd = 4
  integer,parameter::idx_cool_metal = 5
  integer,parameter::idx_cool_z = 5
  integer,parameter::idx_cool_dh = 6
  integer,parameter::idx_cool_enthalpic = 6
  integer,parameter::idx_cool_dust = 7
  integer,parameter::idx_cool_compton = 8
  integer,parameter::idx_cool_cie = 9
  integer,parameter::idx_cool_cont = 10
  integer,parameter::idx_cool_continuum = 10
  integer,parameter::idx_cool_expansion = 11
  integer,parameter::idx_cool_exp = 11
  integer,parameter::idx_cool_ff = 12
  integer,parameter::idx_cool_bss = 12
  integer,parameter::idx_cool_custom = 13
  integer,parameter::idx_cool_co = 14
  integer,parameter::idx_cool_zcie = 15
  integer,parameter::idx_cool_zcienouv = 16
  integer,parameter::idx_cool_zextend = 17
  integer,parameter::idx_cool_gh = 18
  integer,parameter::ncools = 18

  !heating index
  integer,parameter::idx_heat_chem = 1
  integer,parameter::idx_heat_compress = 2
  integer,parameter::idx_heat_compr = 2
  integer,parameter::idx_heat_photo = 3
  integer,parameter::idx_heat_dh = 4
  integer,parameter::idx_heat_enthalpic = 4
  integer,parameter::idx_heat_av = 5
  integer,parameter::idx_heat_photoav = 5
  integer,parameter::idx_heat_cr = 6
  integer,parameter::idx_heat_dust = 7
  integer,parameter::idx_heat_xray = 8
  integer,parameter::idx_heat_viscous = 9
  integer,parameter::idx_heat_visc = 9
  integer,parameter::idx_heat_custom = 10
  integer,parameter::idx_heat_zcie = 11
  integer,parameter::nheats = 11

  real*8::arr_k(nrea)

  !commons for rate tables
  !modify ktab_n according to the required precision
  integer,parameter::ktab_n=int(1e3)
  real*8::ktab(nrea,ktab_n),ktab_logTlow, ktab_logTup, ktab_T(ktab_n)
  real*8::inv_ktab_T(ktab_n-1), inv_ktab_idx

  !thermo toggle (when >0 do cooling/heating)
  integer::krome_thermo_toggle
  !$omp threadprivate(krome_thermo_toggle)

  !debug bit flag, print and array with fallback values for extreme environments
  integer:: red_flag
  real*8::n_global(nspec)
  integer, save :: nprint_negative=10
  !$omp threadprivate(n_global,nprint_negative,red_flag)

  !commons for implicit RHS
  integer::arr_r1(nrea)
  integer::arr_r2(nrea)
  integer::arr_p1(nrea)
  integer::arr_p2(nrea)
  integer::arr_p3(nrea)
  integer::arr_p4(nrea)

  !commons for reduction
  integer::arr_u(nrea)
  real*8::arr_flux(nrea)

  !synchronize the gamma and mu from hydro code
  real*8::hydro_gamma, hydro_mu
  !$omp threadprivate(hydro_gamma,hydro_mu)

  !commons for frequency bins

  ! Draine dust absorption data loaded from file, via load_kabs
  ! in krome_photo module
  real*8::find_Av_draine_kabs(nPhotoBins)

  !commons for H2 photodissociation (Solomon)
  ! note: paramters here are set depending on the data
  ! but if you have a different file you should modify them
  integer,parameter::H2pdData_nvibX=15
  integer,parameter::H2pdData_nvibB=37
  real*8::H2pdData_dE(H2pdData_nvibX,H2pdData_nvibB)
  real*8::H2pdData_pre(H2pdData_nvibX,H2pdData_nvibB)
  real*8::H2pdData_EX(H2pdData_nvibX)
  integer::H2pdData_binMap(H2pdData_nvibX,H2pdData_nvibB)

  !commons for dust optical properties

  !square of turbulence velocity for broadening
  real*8::broadeningVturb2

  !mpi rank of process. If 0, ignored
  integer::krome_mpi_rank=0, krome_omp_thread
  !$omp threadprivate(krome_omp_thread)

  !user-defined commons variables from the reaction file
  real*8::user_crflux,user_Av,user_GtoDN
  !$omp threadprivate(user_crflux,user_Av,user_GtoDN)

  !commons for anytab

  !physical commons
  real*8::phys_Tcmb
  real*8::phys_zredshift
  real*8::phys_orthoParaRatio
  real*8::phys_metallicity
  real*8::phys_Tfloor
  !$omp threadprivate(phys_Tcmb)
  !$omp threadprivate(phys_zredshift)
  !$omp threadprivate(phys_orthoParaRatio)
  !$omp threadprivate(phys_metallicity)
  !$omp threadprivate(phys_Tfloor)

  !machine precision
  real*8::krome_epsilon

  !xrayJ21 for tabulated heating and rate
  real*8::J21xray

  !total metallicity relative to solar Z/Z_solar
  real*8::total_Z
  real*8::dust2gas_ratio

  !commons for dust tabs (cool,H2,Tdust)
  integer,parameter::dust_tab_imax=50, dust_tab_jmax=50
  real*8::dust_tab_ngas(dust_tab_imax)
  real*8::dust_tab_Tgas(dust_tab_jmax)
  real*8::dust_mult_Tgas,dust_mult_ngas
  real*8::dust_table_AvVariable_log

  real*8::dust_tab_cool(dust_tab_imax, dust_tab_jmax)
  real*8::dust_tab_heat(dust_tab_imax, dust_tab_jmax)
  real*8::dust_tab_Tdust(dust_tab_imax, dust_tab_jmax)
  real*8::dust_tab_H2(dust_tab_imax, dust_tab_jmax)

  !commons for exp(-a) table
  integer,parameter::exp_table_na=int(1d5)
  real*8,parameter::exp_table_aMax=1d4,exp_table_aMin=0d0
  real*8,parameter::exp_table_multa=(exp_table_na-1) &
      / (exp_table_aMax-exp_table_aMin)
  real*8,parameter::exp_table_da=1d0/exp_table_multa
  real*8::exp_table(exp_table_na)

  !stores the last evaluation of the rates in the fex
  real*8::last_coe(nrea)
  !$omp threadprivate(last_coe)

  !xsecs from file variables

  ! Gibbs free energy data from file variables

  !partition function from file
  integer,parameter::zpart_nCO=641
  integer,parameter::zpart_nH2even=2000
  integer,parameter::zpart_nH2odd=2000
  real*8::zpart_CO(zpart_nCO),minpart_CO,partdT_CO
  real*8::zpart_H2even(zpart_nH2even),minpart_H2even,partdT_H2even
  real*8::zpart_H2odd(zpart_nH2odd),minpart_H2odd,partdT_H2odd

  !Habing flux for the photoelectric heating by dust
  ! and clumping factor for H2 formation
  ! on dust by Jura/Gnedin
  real*8::GHabing,Ghabing_thin,clump_factor
  !$omp threadprivate(GHabing,GHabing_thin)

  !partition functions common vars

  !verbatim reactions
  character*50::reactionNames(nrea)

end module krome_commons
