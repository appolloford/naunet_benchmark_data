!This module contains useful routines to get physical
! quantities, like mean molecular weight, mass density,
! mass, jeans length, etc. etc.

!############### MODULE ##############
module krome_getphys
contains

  !*****************************
  !get the mean molecular weight
  function get_mu(n)
    use krome_commons
    use krome_constants
    implicit none
    real*8::n(:),get_mu,m(nspec)
    m(:) = get_mass()

    !ip_mass is 1/proton_mass_in_g
    get_mu = max(sum(n(1:nmols)*m(1:nmols)),1d-40) &
        / max(sum(n(1:nmols)),1d-40) * ip_mass

  end function get_mu

  !***************************
  !get mean molecular weight
  function get_mu_rho(n,rhogas)
    use krome_commons
    use krome_constants
    implicit none
    real*8::get_mu_rho,rhogas,n(:)

    !ip_mass is 1/proton_mass_in_g
    get_mu_rho = rhogas / max(sum(n(1:nmols)),1d-40) * ip_mass

  end function get_mu_rho

  !************************
  !get species masses (g)
  function get_mass()
    use krome_commons
    implicit none
    real*8::get_mass(nspec)

    get_mass(1) = 9.10938188d-28	!E
    get_mass(2) = 2.00762042185d-21	!GRAIN-
    get_mass(3) = 1.67444345638d-24	!H-
    get_mass(4) = 3.34694345638d-24	!D-
    get_mass(5) = 2.00771060473d-23	!C-
    get_mass(6) = 2.67691710837d-23	!O-
    get_mass(7) = 4.34993336746d-23	!CN-
    get_mass(8) = 2.84427036019d-23	!OH-
    get_mass(9) = 3.01152036019d-23	!OD-
    get_mass(10) = 3.34706503638d-24	!H2_PARA
    get_mass(11) = 3.34706503638d-24	!H2_ORTHO
    get_mass(12) = 5.01956503638d-24	!HD
    get_mass(13) = 6.69206503638d-24	!D2_PARA
    get_mass(14) = 6.69206503638d-24	!D2_ORTHO
    get_mass(15) = 1.67353251819d-24	!H
    get_mass(16) = 3.34603251819d-24	!D
    get_mass(17) = 2.00761951091d-21	!GRAIN0
    get_mass(18) = 6.69206503638d-24	!HE
    get_mass(19) = 4.68444552546d-23	!CO
    get_mass(20) = 2.00761951091d-23	!C
    get_mass(21) = 2.34222276273d-23	!N
    get_mass(22) = 2.67682601455d-23	!O
    get_mass(23) = 4.68444552546d-23	!N2
    get_mass(24) = 2.50957601455d-23	!NH
    get_mass(25) = 2.67682601455d-23	!ND
    get_mass(26) = 5.01904877728d-23	!NO
    get_mass(27) = 5.3536520291d-23	!O2
    get_mass(28) = 2.84417926637d-23	!OH
    get_mass(29) = 3.01142926637d-23	!OD
    get_mass(30) = 4.18259227364d-23	!C2H
    get_mass(31) = 4.01523902183d-23	!C2
    get_mass(32) = 4.34984227364d-23	!C2D
    get_mass(33) = 6.35746178456d-23	!C2N
    get_mass(34) = 4.34984227364d-23	!CN
    get_mass(35) = 6.02285853274d-23	!C3
    get_mass(36) = 6.69206503638d-23	!CCO
    get_mass(37) = 2.34232601455d-23	!CH2
    get_mass(38) = 2.67682601455d-23	!CD2
    get_mass(39) = 2.50957601455d-23	!CHD
    get_mass(40) = 7.36127154001d-23	!CO2
    get_mass(41) = 3.01153251819d-23	!H2O
    get_mass(42) = 3.34603251819d-23	!D2O
    get_mass(43) = 3.17878251819d-23	!HDO
    get_mass(44) = 4.51719552546d-23	!HCN
    get_mass(45) = 4.68444552546d-23	!DCN
    get_mass(46) = 4.85179877728d-23	!HCO
    get_mass(47) = 5.01904877728d-23	!DCO
    get_mass(48) = 4.51719552546d-23	!HNC
    get_mass(49) = 4.68444552546d-23	!DNC
    get_mass(50) = 5.1864020291d-23	!HNO
    get_mass(51) = 5.3536520291d-23	!DNO
    get_mass(52) = 7.36127154001d-23	!N2O
    get_mass(53) = 2.67692926637d-23	!NH2
    get_mass(54) = 3.01142926637d-23	!ND2
    get_mass(55) = 2.84417926637d-23	!NHD
    get_mass(56) = 7.69587479183d-23	!NO2
    get_mass(57) = 5.52100528092d-23	!O2H
    get_mass(58) = 5.68825528092d-23	!O2D
    get_mass(59) = 7.02666828819d-23	!OCN
    get_mass(60) = 2.17497276273d-23	!CH
    get_mass(61) = 2.34222276273d-23	!CD
    get_mass(62) = 5.01968661638d-24	!H3+_PARA
    get_mass(63) = 5.01968661638d-24	!H3+_ORTHO
    get_mass(64) = 6.69218661638d-24	!H2D+_PARA
    get_mass(65) = 6.69218661638d-24	!H2D+_ORTHO
    get_mass(66) = 8.36468661638d-24	!D2H+_PARA
    get_mass(67) = 8.36468661638d-24	!D2H+_ORTHO
    get_mass(68) = 1.00371866164d-23	!D3+_ORTHO
    get_mass(69) = 1.00371866164d-23	!D3+_META
    get_mass(70) = 1.67262158d-24	!H+
    get_mass(71) = 5.01865409819d-24	!HD+
    get_mass(72) = 6.69115409819d-24	!D2+_ORTHO
    get_mass(73) = 6.69115409819d-24	!D2+_PARA
    get_mass(74) = 6.69115409819d-24	!HE+
    get_mass(75) = 4.85170768346d-23	!HCO+
    get_mass(76) = 5.01895768346d-23	!DCO+
    get_mass(77) = 2.00752841709d-23	!C+
    get_mass(78) = 3.34512158d-24	!D+
    get_mass(79) = 2.34213166891d-23	!N+
    get_mass(80) = 2.67673492073d-23	!O+
    get_mass(81) = 3.34615409819d-24	!H2+_PARA
    get_mass(82) = 3.34615409819d-24	!H2+_ORTHO
    get_mass(83) = 2.17488166891d-23	!CH+
    get_mass(84) = 2.34213166891d-23	!CD+
    get_mass(85) = 5.01895768346d-23	!NO+
    get_mass(86) = 5.35356093528d-23	!O2+
    get_mass(87) = 2.34223492073d-23	!CH2+
    get_mass(88) = 2.67673492073d-23	!CD2+
    get_mass(89) = 2.50948492073d-23	!CHD+
    get_mass(90) = 5.18631093528d-23	!HNO+
    get_mass(91) = 5.35356093528d-23	!DNO+
    get_mass(92) = 2.67683817255d-23	!NH2+
    get_mass(93) = 3.01133817255d-23	!ND2+
    get_mass(94) = 2.84408817255d-23	!NHD+
    get_mass(95) = 4.68435443164d-23	!CO+
    get_mass(96) = 4.01514792801d-23	!C2+
    get_mass(97) = 2.84408817255d-23	!OH+
    get_mass(98) = 3.01133817255d-23	!OD+
    get_mass(99) = 4.18250117983d-23	!C2H+
    get_mass(100) = 4.34975117983d-23	!C2D+
    get_mass(101) = 2.50948492073d-23	!NH+
    get_mass(102) = 2.67673492073d-23	!ND+
    get_mass(103) = 3.01144142437d-23	!H2O+
    get_mass(104) = 3.34594142437d-23	!D2O+
    get_mass(105) = 3.17869142437d-23	!HDO+
    get_mass(106) = 4.34975117983d-23	!CN+
    get_mass(107) = 6.02276743892d-23	!C3+
    get_mass(108) = 6.69197394256d-23	!C2O+
    get_mass(109) = 4.85170768346d-23	!HOC+
    get_mass(110) = 5.01895768346d-23	!DOC+
    get_mass(111) = 6.35737069074d-23	!C2N+
    get_mass(112) = 6.35737069074d-23	!CNC+
    get_mass(113) = 4.51710443164d-23	!HCN+
    get_mass(114) = 4.68435443164d-23	!DCN+
    get_mass(115) = 4.51710443164d-23	!HNC+
    get_mass(116) = 4.68435443164d-23	!DNC+
    get_mass(117) = 7.02657719438d-23	!NCO+
    get_mass(118) = 4.85170768346d-23	!N2H+
    get_mass(119) = 5.01895768346d-23	!N2D+
    get_mass(120) = 5.5209141871d-23	!O2H+
    get_mass(121) = 5.6881641871d-23	!O2D+
    get_mass(122) = 8.36468661638d-24	!HEH+
    get_mass(123) = 1.00371866164d-23	!HED+
    get_mass(124) = 4.68435443164d-23	!N2+
    get_mass(125) = 7.36118044619d-23	!CO2+
    get_mass(126) = 7.69578369801d-23	!NO2+
    get_mass(127) = 1.00371866164d-23	!D3+_PARA
    get_mass(128) = 3.17879467619d-23	!H3O+
    get_mass(129) = 3.34604467619d-23	!H2DO+
    get_mass(130) = 3.51329467619d-23	!HD2O+
    get_mass(131) = 3.68054467619d-23	!D3O+
    get_mass(132) = 0.d0	!CR
    get_mass(133) = 0.d0	!g
    get_mass(134) = 0.d0	!Tgas
    get_mass(135) = 0.d0	!dummy

  end function get_mass

  !************************
  !get sqrt of the inverse of the masses (1/sqrt(g))
  function get_imass_sqrt()
    use krome_commons
    implicit none
    real*8::get_imass_sqrt(nspec)

    get_imass_sqrt(1) = 3.31326021505d+13	!E
    get_imass_sqrt(2) = 22318201671.2	!GRAIN-
    get_imass_sqrt(3) = 7.72795806394d+11	!H-
    get_imass_sqrt(4) = 5.46607784381d+11	!D-
    get_imass_sqrt(5) = 2.23177004181d+11	!C-
    get_imass_sqrt(6) = 1.93278051341d+11	!O-
    get_imass_sqrt(7) = 1.5162076997d+11	!CN-
    get_imass_sqrt(8) = 1.87505737904d+11	!OH-
    get_imass_sqrt(9) = 1.82224638843d+11	!OD-
    get_imass_sqrt(10) = 5.46597856701d+11	!H2_PARA
    get_imass_sqrt(11) = 5.46597856701d+11	!H2_ORTHO
    get_imass_sqrt(12) = 4.46341179967d+11	!HD
    get_imass_sqrt(13) = 3.86562679981d+11	!D2_PARA
    get_imass_sqrt(14) = 3.86562679981d+11	!D2_ORTHO
    get_imass_sqrt(15) = 7.73006102111d+11	!H
    get_imass_sqrt(16) = 5.46682184737d+11	!D
    get_imass_sqrt(17) = 22318206734.6	!GRAIN0
    get_imass_sqrt(18) = 3.86562679981d+11	!HE
    get_imass_sqrt(19) = 1.46106959624d+11	!CO
    get_imass_sqrt(20) = 2.23182067346d+11	!C
    get_imass_sqrt(21) = 2.06626443857d+11	!N
    get_imass_sqrt(22) = 1.93281339991d+11	!O
    get_imass_sqrt(23) = 1.46106959624d+11	!N2
    get_imass_sqrt(24) = 1.99618056318d+11	!NH
    get_imass_sqrt(25) = 1.93281339991d+11	!ND
    get_imass_sqrt(26) = 1.41152733144d+11	!NO
    get_imass_sqrt(27) = 1.36670546184d+11	!O2
    get_imass_sqrt(28) = 1.87508740611d+11	!OH
    get_imass_sqrt(29) = 1.82227394912d+11	!OD
    get_imass_sqrt(30) = 1.54624117718d+11	!C2H
    get_imass_sqrt(31) = 1.57813553259d+11	!C2
    get_imass_sqrt(32) = 1.51622357573d+11	!C2D
    get_imass_sqrt(33) = 1.25417494605d+11	!C2N
    get_imass_sqrt(34) = 1.51622357573d+11	!CN
    get_imass_sqrt(35) = 1.2885422666d+11	!C3
    get_imass_sqrt(36) = 1.22241852716d+11	!CCO
    get_imass_sqrt(37) = 2.06621889668d+11	!CH2
    get_imass_sqrt(38) = 1.93281339991d+11	!CD2
    get_imass_sqrt(39) = 1.99618056318d+11	!CHD
    get_imass_sqrt(40) = 1.16553033405d+11	!CO2
    get_imass_sqrt(41) = 1.82224271009d+11	!H2O
    get_imass_sqrt(42) = 1.72876086d+11	!D2O
    get_imass_sqrt(43) = 1.77365681534d+11	!HDO
    get_imass_sqrt(44) = 1.48787194664d+11	!HCN
    get_imass_sqrt(45) = 1.46106959624d+11	!DCN
    get_imass_sqrt(46) = 1.43565011358d+11	!HCO
    get_imass_sqrt(47) = 1.41152733144d+11	!DCO
    get_imass_sqrt(48) = 1.48787194664d+11	!HNC
    get_imass_sqrt(49) = 1.46106959624d+11	!DNC
    get_imass_sqrt(50) = 1.38856722679d+11	!HNO
    get_imass_sqrt(51) = 1.36670546184d+11	!DNO
    get_imass_sqrt(52) = 1.16553033405d+11	!N2O
    get_imass_sqrt(53) = 1.93277612428d+11	!NH2
    get_imass_sqrt(54) = 1.82227394912d+11	!ND2
    get_imass_sqrt(55) = 1.87508740611d+11	!NHD
    get_imass_sqrt(56) = 1.13991115426d+11	!NO2
    get_imass_sqrt(57) = 1.34583221186d+11	!O2H
    get_imass_sqrt(58) = 1.32589905371d+11	!O2D
    get_imass_sqrt(59) = 1.19295832983d+11	!OCN
    get_imass_sqrt(60) = 2.14423849574d+11	!CH
    get_imass_sqrt(61) = 2.06626443857d+11	!CD
    get_imass_sqrt(62) = 4.463357746d+11	!H3+_PARA
    get_imass_sqrt(63) = 4.463357746d+11	!H3+_ORTHO
    get_imass_sqrt(64) = 3.86559168535d+11	!H2D+_PARA
    get_imass_sqrt(65) = 3.86559168535d+11	!H2D+_ORTHO
    get_imass_sqrt(66) = 3.45760328884d+11	!D2H+_PARA
    get_imass_sqrt(67) = 3.45760328884d+11	!D2H+_ORTHO
    get_imass_sqrt(68) = 3.15641428772d+11	!D3+_ORTHO
    get_imass_sqrt(69) = 3.15641428772d+11	!D3+_META
    get_imass_sqrt(70) = 7.732165696d+11	!H+
    get_imass_sqrt(71) = 4.46381685924d+11	!HD+
    get_imass_sqrt(72) = 3.86588992536d+11	!D2+_ORTHO
    get_imass_sqrt(73) = 3.86588992536d+11	!D2+_PARA
    get_imass_sqrt(74) = 3.86588992536d+11	!HE+
    get_imass_sqrt(75) = 1.43566359113d+11	!HCO+
    get_imass_sqrt(76) = 1.41154014095d+11	!DCO+
    get_imass_sqrt(77) = 2.23187130855d+11	!C+
    get_imass_sqrt(78) = 5.46756615481d+11	!D+
    get_imass_sqrt(79) = 2.06630462037d+11	!N+
    get_imass_sqrt(80) = 1.93284628808d+11	!O+
    get_imass_sqrt(81) = 5.46672253003d+11	!H2+_PARA
    get_imass_sqrt(82) = 5.46672253003d+11	!H2+_ORTHO
    get_imass_sqrt(83) = 2.14428340044d+11	!CH+
    get_imass_sqrt(84) = 2.06630462037d+11	!CD+
    get_imass_sqrt(85) = 1.41154014095d+11	!NO+
    get_imass_sqrt(86) = 1.36671708942d+11	!O2+
    get_imass_sqrt(87) = 2.06625907582d+11	!CH2+
    get_imass_sqrt(88) = 1.93284628808d+11	!CD2+
    get_imass_sqrt(89) = 1.99621679333d+11	!CHD+
    get_imass_sqrt(90) = 1.38857942133d+11	!HNO+
    get_imass_sqrt(91) = 1.36671708942d+11	!DNO+
    get_imass_sqrt(92) = 1.93280901055d+11	!NH2+
    get_imass_sqrt(93) = 1.82230151106d+11	!ND2+
    get_imass_sqrt(94) = 1.87511743463d+11	!NHD+
    get_imass_sqrt(95) = 1.46108380244d+11	!CO+
    get_imass_sqrt(96) = 1.5781534345d+11	!C2+
    get_imass_sqrt(97) = 1.87511743463d+11	!OH+
    get_imass_sqrt(98) = 1.82230151106d+11	!OD+
    get_imass_sqrt(99) = 1.54625801546d+11	!C2H+
    get_imass_sqrt(100) = 1.51623945226d+11	!C2D+
    get_imass_sqrt(101) = 1.99621679333d+11	!NH+
    get_imass_sqrt(102) = 1.93284628808d+11	!ND+
    get_imass_sqrt(103) = 1.82227027061d+11	!H2O+
    get_imass_sqrt(104) = 1.72878439275d+11	!D2O+
    get_imass_sqrt(105) = 1.77368222957d+11	!HDO+
    get_imass_sqrt(106) = 1.51623945226d+11	!CN+
    get_imass_sqrt(107) = 1.28855201111d+11	!C3+
    get_imass_sqrt(108) = 1.22242684716d+11	!C2O+
    get_imass_sqrt(109) = 1.43566359113d+11	!HOC+
    get_imass_sqrt(110) = 1.41154014095d+11	!DOC+
    get_imass_sqrt(111) = 1.25418393146d+11	!C2N+
    get_imass_sqrt(112) = 1.25418393146d+11	!CNC+
    get_imass_sqrt(113) = 1.48788694909d+11	!HCN+
    get_imass_sqrt(114) = 1.46108380244d+11	!DCN+
    get_imass_sqrt(115) = 1.48788694909d+11	!HNC+
    get_imass_sqrt(116) = 1.46108380244d+11	!DNC+
    get_imass_sqrt(117) = 1.19296606267d+11	!NCO+
    get_imass_sqrt(118) = 1.43566359113d+11	!N2H+
    get_imass_sqrt(119) = 1.41154014095d+11	!N2D+
    get_imass_sqrt(120) = 1.34584331478d+11	!O2H+
    get_imass_sqrt(121) = 1.32590967056d+11	!O2D+
    get_imass_sqrt(122) = 3.45760328884d+11	!HEH+
    get_imass_sqrt(123) = 3.15641428772d+11	!HED+
    get_imass_sqrt(124) = 1.46108380244d+11	!N2+
    get_imass_sqrt(125) = 1.16553754568d+11	!CO2+
    get_imass_sqrt(126) = 1.13991790072d+11	!NO2+
    get_imass_sqrt(127) = 3.15641428772d+11	!D3+_PARA
    get_imass_sqrt(128) = 1.77365342346d+11	!H3O+
    get_imass_sqrt(129) = 1.72875771924d+11	!H2DO+
    get_imass_sqrt(130) = 1.68710732568d+11	!HD2O+
    get_imass_sqrt(131) = 1.64832920354d+11	!D3O+
    get_imass_sqrt(132) = 0.d0	!CR
    get_imass_sqrt(133) = 0.d0	!g
    get_imass_sqrt(134) = 0.d0	!Tgas
    get_imass_sqrt(135) = 0.d0	!dummy

  end function get_imass_sqrt

  !************************
  !get inverse of the species masses (1/g)
  function get_imass()
    use krome_commons
    implicit none
    real*8::get_imass(nspec)

    get_imass(1) = 1.09776932527d+27	!E
    get_imass(2) = 4.98102125838d+20	!GRAIN-
    get_imass(3) = 5.9721335838d+23	!H-
    get_imass(4) = 2.98780069946d+23	!D-
    get_imass(5) = 4.98079751954d+22	!C-
    get_imass(6) = 3.73564051301d+22	!O-
    get_imass(7) = 2.29888578864d+22	!CN-
    get_imass(8) = 3.51584017468d+22	!OH-
    get_imass(9) = 3.32058190016d+22	!OD-
    get_imass(10) = 2.9876921695d+23	!H2_PARA
    get_imass(11) = 2.9876921695d+23	!H2_ORTHO
    get_imass(12) = 1.99220448934d+23	!HD
    get_imass(13) = 1.49430705554d+23	!D2_PARA
    get_imass(14) = 1.49430705554d+23	!D2_ORTHO
    get_imass(15) = 5.97538433901d+23	!H
    get_imass(16) = 2.98861411108d+23	!D
    get_imass(17) = 4.98102351847d+20	!GRAIN0
    get_imass(18) = 1.49430705554d+23	!HE
    get_imass(19) = 2.13472436506d+22	!CO
    get_imass(20) = 4.98102351847d+22	!C
    get_imass(21) = 4.26944873012d+22	!N
    get_imass(22) = 3.73576763885d+22	!O
    get_imass(23) = 2.13472436506d+22	!N2
    get_imass(24) = 3.98473684081d+22	!NH
    get_imass(25) = 3.73576763885d+22	!ND
    get_imass(26) = 1.99240940739d+22	!NO
    get_imass(27) = 1.86788381943d+22	!O2
    get_imass(28) = 3.51595278056d+22	!OH
    get_imass(29) = 3.32068234565d+22	!OD
    get_imass(30) = 2.390861778d+22	!C2H
    get_imass(31) = 2.49051175924d+22	!C2
    get_imass(32) = 2.2989339316d+22	!C2D
    get_imass(33) = 1.57295479531d+22	!C2N
    get_imass(34) = 2.2989339316d+22	!CN
    get_imass(35) = 1.66034117282d+22	!C3
    get_imass(36) = 1.49430705554d+22	!CCO
    get_imass(37) = 4.26926052901d+22	!CH2
    get_imass(38) = 3.73576763885d+22	!CD2
    get_imass(39) = 3.98473684081d+22	!CHD
    get_imass(40) = 1.35846095958d+22	!CO2
    get_imass(41) = 3.32056849448d+22	!H2O
    get_imass(42) = 2.98861411108d+22	!D2O
    get_imass(43) = 3.14585849859d+22	!HDO
    get_imass(44) = 2.21376292959d+22	!HCN
    get_imass(45) = 2.13472436506d+22	!DCN
    get_imass(46) = 2.06109124864d+22	!HCO
    get_imass(47) = 1.99240940739d+22	!DCO
    get_imass(48) = 2.21376292959d+22	!HNC
    get_imass(49) = 2.13472436506d+22	!DNC
    get_imass(50) = 1.92811894332d+22	!HNO
    get_imass(51) = 1.86788381943d+22	!DNO
    get_imass(52) = 1.35846095958d+22	!N2O
    get_imass(53) = 3.73562354659d+22	!NH2
    get_imass(54) = 3.32068234565d+22	!ND2
    get_imass(55) = 3.51595278056d+22	!NHD
    get_imass(56) = 1.2993974396d+22	!NO2
    get_imass(57) = 1.81126434248d+22	!O2H
    get_imass(58) = 1.75800830064d+22	!O2D
    get_imass(59) = 1.42314957671d+22	!OCN
    get_imass(60) = 4.59775872662d+22	!CH
    get_imass(61) = 4.26944873012d+22	!CD
    get_imass(62) = 1.99215623688d+23	!H3+_PARA
    get_imass(63) = 1.99215623688d+23	!H3+_ORTHO
    get_imass(64) = 1.49427990779d+23	!H2D+_PARA
    get_imass(65) = 1.49427990779d+23	!H2D+_ORTHO
    get_imass(66) = 1.1955020503d+23	!D2H+_PARA
    get_imass(67) = 1.1955020503d+23	!D2H+_ORTHO
    get_imass(68) = 9.96295115574d+22	!D3+_ORTHO
    get_imass(69) = 9.96295115574d+22	!D3+_META
    get_imass(70) = 5.97863863505d+23	!H+
    get_imass(71) = 1.99256609528d+23	!HD+
    get_imass(72) = 1.4945104915d+23	!D2+_ORTHO
    get_imass(73) = 1.4945104915d+23	!D2+_PARA
    get_imass(74) = 1.4945104915d+23	!HE+
    get_imass(75) = 2.0611299469d+22	!HCO+
    get_imass(76) = 1.99244556952d+22	!DCO+
    get_imass(77) = 4.98124953791d+22	!C+
    get_imass(78) = 2.98942796572d+23	!D+
    get_imass(79) = 4.26961478414d+22	!N+
    get_imass(80) = 3.73589477335d+22	!O+
    get_imass(81) = 2.98850552203d+23	!H2+_PARA
    get_imass(82) = 2.98850552203d+23	!H2+_ORTHO
    get_imass(83) = 4.59795130141d+22	!CH+
    get_imass(84) = 4.26961478414d+22	!CD+
    get_imass(85) = 1.99244556952d+22	!NO+
    get_imass(86) = 1.86791560251d+22	!O2+
    get_imass(87) = 4.2694265684d+22	!CH2+
    get_imass(88) = 3.73589477335d+22	!CD2+
    get_imass(89) = 3.98488148599d+22	!CHD+
    get_imass(90) = 1.92815280934d+22	!HNO+
    get_imass(91) = 1.86791560251d+22	!DNO+
    get_imass(92) = 3.73575067128d+22	!NH2+
    get_imass(93) = 3.32078279721d+22	!ND2+
    get_imass(94) = 3.51606539365d+22	!NHD+
    get_imass(95) = 2.13476587776d+22	!CO+
    get_imass(96) = 2.49056826281d+22	!C2+
    get_imass(97) = 3.51606539365d+22	!OH+
    get_imass(98) = 3.32078279721d+22	!OD+
    get_imass(99) = 2.39091385036d+22	!C2H+
    get_imass(100) = 2.29898207658d+22	!C2D+
    get_imass(101) = 3.98488148599d+22	!NH+
    get_imass(102) = 3.73589477335d+22	!ND+
    get_imass(103) = 3.32066893916d+22	!H2O+
    get_imass(104) = 2.98869547661d+22	!D2O+
    get_imass(105) = 3.14594865149d+22	!HDO+
    get_imass(106) = 2.29898207658d+22	!CN+
    get_imass(107) = 1.66036628534d+22	!C3+
    get_imass(108) = 1.49432739665d+22	!C2O+
    get_imass(109) = 2.0611299469d+22	!HOC+
    get_imass(110) = 1.99244556952d+22	!DOC+
    get_imass(111) = 1.57297733394d+22	!C2N+
    get_imass(112) = 1.57297733394d+22	!CNC+
    get_imass(113) = 2.21380757326d+22	!HCN+
    get_imass(114) = 2.13476587776d+22	!DCN+
    get_imass(115) = 2.21380757326d+22	!HNC+
    get_imass(116) = 2.13476587776d+22	!DNC+
    get_imass(117) = 1.42316802668d+22	!NCO+
    get_imass(118) = 2.0611299469d+22	!N2H+
    get_imass(119) = 1.99244556952d+22	!N2D+
    get_imass(120) = 1.81129422793d+22	!O2H+
    get_imass(121) = 1.75803645448d+22	!O2D+
    get_imass(122) = 1.1955020503d+23	!HEH+
    get_imass(123) = 9.96295115574d+22	!HED+
    get_imass(124) = 2.13476587776d+22	!N2+
    get_imass(125) = 1.35847777039d+22	!CO2+
    get_imass(126) = 1.29941282037d+22	!NO2+
    get_imass(127) = 9.96295115574d+22	!D3+_PARA
    get_imass(128) = 3.14584646656d+22	!H3O+
    get_imass(129) = 2.98860325182d+22	!H2DO+
    get_imass(130) = 2.84633112838d+22	!HD2O+
    get_imass(131) = 2.71698916323d+22	!D3O+
    get_imass(132) = 0.d0	!CR
    get_imass(133) = 0.d0	!g
    get_imass(134) = 0.d0	!Tgas
    get_imass(135) = 0.d0	!dummy

  end function get_imass

  !************************
  !species binding energies (surface=BARE), K
  function get_EbindBare()
    use krome_commons
    implicit none
    real*8::get_EbindBare(nspec)

    get_EbindBare(:) = 1d99

    get_EbindBare(idx_H) = 500.0d0
    get_EbindBare(idx_CO) = 1100.0d0
    get_EbindBare(idx_O) = 1700.0d0
    get_EbindBare(idx_O2) = 1250.0d0
    get_EbindBare(idx_OH) = 1360.0d0
    get_EbindBare(idx_CO2) = 2300.0d0
    get_EbindBare(idx_H2O) = 4800.0d0
    get_EbindBare(idx_HCO) = 1100.0d0

  end function get_EbindBare

  !************************
  !species binding energies (surface=ICE), K
  function get_EbindIce()
    use krome_commons
    implicit none
    real*8::get_EbindIce(nspec)

    get_EbindIce(:) = 1d99

    get_EbindIce(idx_H) = 650.0d0
    get_EbindIce(idx_CO) = 1300.0d0
    get_EbindIce(idx_O) = 1700.0d0
    get_EbindIce(idx_O2) = 900.0d0
    get_EbindIce(idx_OH) = 3500.0d0
    get_EbindIce(idx_CO2) = 2300.0d0
    get_EbindIce(idx_H2O) = 4800.0d0
    get_EbindIce(idx_HCO) = 3100.0d0

  end function get_EbindIce

  !************************
  function get_kevap70()
    use krome_commons
    implicit none
    real*8::get_kevap70(nspec)

    get_kevap70(idx_E) = 0d0
    get_kevap70(idx_GRAINk) = 0d0
    get_kevap70(idx_Hk) = 0d0
    get_kevap70(idx_Dk) = 0d0
    get_kevap70(idx_Ck) = 0d0
    get_kevap70(idx_Ok) = 0d0
    get_kevap70(idx_CNk) = 0d0
    get_kevap70(idx_OHk) = 0d0
    get_kevap70(idx_ODk) = 0d0
    get_kevap70(idx_H2_PARA) = 0d0
    get_kevap70(idx_H2_ORTHO) = 0d0
    get_kevap70(idx_HD) = 0d0
    get_kevap70(idx_D2_PARA) = 0d0
    get_kevap70(idx_D2_ORTHO) = 0d0
    get_kevap70(idx_H) = 790490323.12
    get_kevap70(idx_D) = 0d0
    get_kevap70(idx_GRAIN0) = 0d0
    get_kevap70(idx_HE) = 0d0
    get_kevap70(idx_CO) = 149751.929641
    get_kevap70(idx_C) = 0d0
    get_kevap70(idx_N) = 0d0
    get_kevap70(idx_O) = 28.3692788833
    get_kevap70(idx_N2) = 0d0
    get_kevap70(idx_NH) = 0d0
    get_kevap70(idx_ND) = 0d0
    get_kevap70(idx_NO) = 0d0
    get_kevap70(idx_O2) = 17568.7715065
    get_kevap70(idx_OH) = 3649.88043081
    get_kevap70(idx_OD) = 0d0
    get_kevap70(idx_C2H) = 0d0
    get_kevap70(idx_C2) = 0d0
    get_kevap70(idx_C2D) = 0d0
    get_kevap70(idx_C2N) = 0d0
    get_kevap70(idx_CN) = 0d0
    get_kevap70(idx_C3) = 0d0
    get_kevap70(idx_CCO) = 0d0
    get_kevap70(idx_CH2) = 0d0
    get_kevap70(idx_CD2) = 0d0
    get_kevap70(idx_CHD) = 0d0
    get_kevap70(idx_CO2) = 0.00537432797219
    get_kevap70(idx_H2O) = 1.65884938156e-18
    get_kevap70(idx_D2O) = 0d0
    get_kevap70(idx_HDO) = 0d0
    get_kevap70(idx_HCN) = 0d0
    get_kevap70(idx_DCN) = 0d0
    get_kevap70(idx_HCO) = 149751.929641
    get_kevap70(idx_DCO) = 0d0
    get_kevap70(idx_HNC) = 0d0
    get_kevap70(idx_DNC) = 0d0
    get_kevap70(idx_HNO) = 0d0
    get_kevap70(idx_DNO) = 0d0
    get_kevap70(idx_N2O) = 0d0
    get_kevap70(idx_NH2) = 0d0
    get_kevap70(idx_ND2) = 0d0
    get_kevap70(idx_NHD) = 0d0
    get_kevap70(idx_NO2) = 0d0
    get_kevap70(idx_O2H) = 0d0
    get_kevap70(idx_O2D) = 0d0
    get_kevap70(idx_OCN) = 0d0
    get_kevap70(idx_CH) = 0d0
    get_kevap70(idx_CD) = 0d0
    get_kevap70(idx_H3j_PARA) = 0d0
    get_kevap70(idx_H3j_ORTHO) = 0d0
    get_kevap70(idx_H2Dj_PARA) = 0d0
    get_kevap70(idx_H2Dj_ORTHO) = 0d0
    get_kevap70(idx_D2Hj_PARA) = 0d0
    get_kevap70(idx_D2Hj_ORTHO) = 0d0
    get_kevap70(idx_D3j_ORTHO) = 0d0
    get_kevap70(idx_D3j_META) = 0d0
    get_kevap70(idx_Hj) = 0d0
    get_kevap70(idx_HDj) = 0d0
    get_kevap70(idx_D2j_ORTHO) = 0d0
    get_kevap70(idx_D2j_PARA) = 0d0
    get_kevap70(idx_HEj) = 0d0
    get_kevap70(idx_HCOj) = 0d0
    get_kevap70(idx_DCOj) = 0d0
    get_kevap70(idx_Cj) = 0d0
    get_kevap70(idx_Dj) = 0d0
    get_kevap70(idx_Nj) = 0d0
    get_kevap70(idx_Oj) = 0d0
    get_kevap70(idx_H2j_PARA) = 0d0
    get_kevap70(idx_H2j_ORTHO) = 0d0
    get_kevap70(idx_CHj) = 0d0
    get_kevap70(idx_CDj) = 0d0
    get_kevap70(idx_NOj) = 0d0
    get_kevap70(idx_O2j) = 0d0
    get_kevap70(idx_CH2j) = 0d0
    get_kevap70(idx_CD2j) = 0d0
    get_kevap70(idx_CHDj) = 0d0
    get_kevap70(idx_HNOj) = 0d0
    get_kevap70(idx_DNOj) = 0d0
    get_kevap70(idx_NH2j) = 0d0
    get_kevap70(idx_ND2j) = 0d0
    get_kevap70(idx_NHDj) = 0d0
    get_kevap70(idx_COj) = 0d0
    get_kevap70(idx_C2j) = 0d0
    get_kevap70(idx_OHj) = 0d0
    get_kevap70(idx_ODj) = 0d0
    get_kevap70(idx_C2Hj) = 0d0
    get_kevap70(idx_C2Dj) = 0d0
    get_kevap70(idx_NHj) = 0d0
    get_kevap70(idx_NDj) = 0d0
    get_kevap70(idx_H2Oj) = 0d0
    get_kevap70(idx_D2Oj) = 0d0
    get_kevap70(idx_HDOj) = 0d0
    get_kevap70(idx_CNj) = 0d0
    get_kevap70(idx_C3j) = 0d0
    get_kevap70(idx_C2Oj) = 0d0
    get_kevap70(idx_HOCj) = 0d0
    get_kevap70(idx_DOCj) = 0d0
    get_kevap70(idx_C2Nj) = 0d0
    get_kevap70(idx_CNCj) = 0d0
    get_kevap70(idx_HCNj) = 0d0
    get_kevap70(idx_DCNj) = 0d0
    get_kevap70(idx_HNCj) = 0d0
    get_kevap70(idx_DNCj) = 0d0
    get_kevap70(idx_NCOj) = 0d0
    get_kevap70(idx_N2Hj) = 0d0
    get_kevap70(idx_N2Dj) = 0d0
    get_kevap70(idx_O2Hj) = 0d0
    get_kevap70(idx_O2Dj) = 0d0
    get_kevap70(idx_HEHj) = 0d0
    get_kevap70(idx_HEDj) = 0d0
    get_kevap70(idx_N2j) = 0d0
    get_kevap70(idx_CO2j) = 0d0
    get_kevap70(idx_NO2j) = 0d0
    get_kevap70(idx_D3j_PARA) = 0d0
    get_kevap70(idx_H3Oj) = 0d0
    get_kevap70(idx_H2DOj) = 0d0
    get_kevap70(idx_HD2Oj) = 0d0
    get_kevap70(idx_D3Oj) = 0d0
    get_kevap70(idx_CR) = 0d0
    get_kevap70(idx_g) = 0d0
    get_kevap70(idx_Tgas) = 0d0
    get_kevap70(idx_dummy) = 0d0

  end function get_kevap70

  !************************
  !get verbatim reaction names
  function get_rnames()
    use krome_commons
    implicit none
    character*50::get_rnames(nrea)

    !reaction names are loaded from file
    get_rnames(:) = reactionNames(:)

  end function get_rnames

  !************************
  !get species names
  function get_names()
    use krome_commons
    implicit none
    character*16::get_names(nspec)

    get_names(1) = "E"
    get_names(2) = "GRAIN-"
    get_names(3) = "H-"
    get_names(4) = "D-"
    get_names(5) = "C-"
    get_names(6) = "O-"
    get_names(7) = "CN-"
    get_names(8) = "OH-"
    get_names(9) = "OD-"
    get_names(10) = "H2_PARA"
    get_names(11) = "H2_ORTHO"
    get_names(12) = "HD"
    get_names(13) = "D2_PARA"
    get_names(14) = "D2_ORTHO"
    get_names(15) = "H"
    get_names(16) = "D"
    get_names(17) = "GRAIN0"
    get_names(18) = "HE"
    get_names(19) = "CO"
    get_names(20) = "C"
    get_names(21) = "N"
    get_names(22) = "O"
    get_names(23) = "N2"
    get_names(24) = "NH"
    get_names(25) = "ND"
    get_names(26) = "NO"
    get_names(27) = "O2"
    get_names(28) = "OH"
    get_names(29) = "OD"
    get_names(30) = "C2H"
    get_names(31) = "C2"
    get_names(32) = "C2D"
    get_names(33) = "C2N"
    get_names(34) = "CN"
    get_names(35) = "C3"
    get_names(36) = "CCO"
    get_names(37) = "CH2"
    get_names(38) = "CD2"
    get_names(39) = "CHD"
    get_names(40) = "CO2"
    get_names(41) = "H2O"
    get_names(42) = "D2O"
    get_names(43) = "HDO"
    get_names(44) = "HCN"
    get_names(45) = "DCN"
    get_names(46) = "HCO"
    get_names(47) = "DCO"
    get_names(48) = "HNC"
    get_names(49) = "DNC"
    get_names(50) = "HNO"
    get_names(51) = "DNO"
    get_names(52) = "N2O"
    get_names(53) = "NH2"
    get_names(54) = "ND2"
    get_names(55) = "NHD"
    get_names(56) = "NO2"
    get_names(57) = "O2H"
    get_names(58) = "O2D"
    get_names(59) = "OCN"
    get_names(60) = "CH"
    get_names(61) = "CD"
    get_names(62) = "H3+_PARA"
    get_names(63) = "H3+_ORTHO"
    get_names(64) = "H2D+_PARA"
    get_names(65) = "H2D+_ORTHO"
    get_names(66) = "D2H+_PARA"
    get_names(67) = "D2H+_ORTHO"
    get_names(68) = "D3+_ORTHO"
    get_names(69) = "D3+_META"
    get_names(70) = "H+"
    get_names(71) = "HD+"
    get_names(72) = "D2+_ORTHO"
    get_names(73) = "D2+_PARA"
    get_names(74) = "HE+"
    get_names(75) = "HCO+"
    get_names(76) = "DCO+"
    get_names(77) = "C+"
    get_names(78) = "D+"
    get_names(79) = "N+"
    get_names(80) = "O+"
    get_names(81) = "H2+_PARA"
    get_names(82) = "H2+_ORTHO"
    get_names(83) = "CH+"
    get_names(84) = "CD+"
    get_names(85) = "NO+"
    get_names(86) = "O2+"
    get_names(87) = "CH2+"
    get_names(88) = "CD2+"
    get_names(89) = "CHD+"
    get_names(90) = "HNO+"
    get_names(91) = "DNO+"
    get_names(92) = "NH2+"
    get_names(93) = "ND2+"
    get_names(94) = "NHD+"
    get_names(95) = "CO+"
    get_names(96) = "C2+"
    get_names(97) = "OH+"
    get_names(98) = "OD+"
    get_names(99) = "C2H+"
    get_names(100) = "C2D+"
    get_names(101) = "NH+"
    get_names(102) = "ND+"
    get_names(103) = "H2O+"
    get_names(104) = "D2O+"
    get_names(105) = "HDO+"
    get_names(106) = "CN+"
    get_names(107) = "C3+"
    get_names(108) = "C2O+"
    get_names(109) = "HOC+"
    get_names(110) = "DOC+"
    get_names(111) = "C2N+"
    get_names(112) = "CNC+"
    get_names(113) = "HCN+"
    get_names(114) = "DCN+"
    get_names(115) = "HNC+"
    get_names(116) = "DNC+"
    get_names(117) = "NCO+"
    get_names(118) = "N2H+"
    get_names(119) = "N2D+"
    get_names(120) = "O2H+"
    get_names(121) = "O2D+"
    get_names(122) = "HEH+"
    get_names(123) = "HED+"
    get_names(124) = "N2+"
    get_names(125) = "CO2+"
    get_names(126) = "NO2+"
    get_names(127) = "D3+_PARA"
    get_names(128) = "H3O+"
    get_names(129) = "H2DO+"
    get_names(130) = "HD2O+"
    get_names(131) = "D3O+"
    get_names(132) = "CR"
    get_names(133) = "g"
    get_names(134) = "Tgas"
    get_names(135) = "dummy"

  end function get_names

  !************************
  !get cooling names list (empty element if cooling not present)
  function get_cooling_names()
    use krome_commons
    implicit none
    character*16::get_cooling_names(ncools)

    get_cooling_names(:) = ""

    get_cooling_names(idx_cool_h2) = "H2"
    get_cooling_names(idx_cool_h2gp) = "H2GP"
    get_cooling_names(idx_cool_atomic) = "ATOMIC"
    get_cooling_names(idx_cool_cen) = "CEN"
    get_cooling_names(idx_cool_hd) = "HD"
    get_cooling_names(idx_cool_metal) = "METAL"
    get_cooling_names(idx_cool_z) = "Z"
    get_cooling_names(idx_cool_dh) = "DH"
    get_cooling_names(idx_cool_enthalpic) = "ENTHALPIC"
    get_cooling_names(idx_cool_dust) = "DUST"
    get_cooling_names(idx_cool_compton) = "COMPTON"
    get_cooling_names(idx_cool_cie) = "CIE"
    get_cooling_names(idx_cool_cont) = "CONT"
    get_cooling_names(idx_cool_continuum) = "CONTINUUM"
    get_cooling_names(idx_cool_expansion) = "EXPANSION"
    get_cooling_names(idx_cool_exp) = "EXP"
    get_cooling_names(idx_cool_ff) = "FF"
    get_cooling_names(idx_cool_bss) = "BSS"
    get_cooling_names(idx_cool_custom) = "CUSTOM"
    get_cooling_names(idx_cool_co) = "CO"
    get_cooling_names(idx_cool_zcie) = "ZCIE"
    get_cooling_names(idx_cool_zcienouv) = "ZCIENOUV"
    get_cooling_names(idx_cool_zextend) = "ZEXTEND"
    get_cooling_names(idx_cool_gh) = "GH"

  end function get_cooling_names

  !************************
  !get heating names list (empty element if heating not present)
  function get_heating_names()
    use krome_commons
    implicit none
    character*16::get_heating_names(nheats)

    get_heating_names(:) = ""

    get_heating_names(idx_heat_chem) = "CHEM"
    get_heating_names(idx_heat_compress) = "COMPRESS"
    get_heating_names(idx_heat_compr) = "COMPR"
    get_heating_names(idx_heat_photo) = "PHOTO"
    get_heating_names(idx_heat_dh) = "DH"
    get_heating_names(idx_heat_enthalpic) = "ENTHALPIC"
    get_heating_names(idx_heat_av) = "AV"
    get_heating_names(idx_heat_photoav) = "PHOTOAV"
    get_heating_names(idx_heat_cr) = "CR"
    get_heating_names(idx_heat_dust) = "DUST"
    get_heating_names(idx_heat_xray) = "XRAY"
    get_heating_names(idx_heat_viscous) = "VISCOUS"
    get_heating_names(idx_heat_visc) = "VISC"
    get_heating_names(idx_heat_custom) = "CUSTOM"
    get_heating_names(idx_heat_zcie) = "ZCIE"

  end function get_heating_names

  !******************************
  !get the total number of H nuclei
  function get_Hnuclei(n)
    use krome_commons
    real*8::n(:),get_Hnuclei,nH

    nH = n(idx_Hk) + &
        n(idx_OHk) + &
        n(idx_H2_PARA)*2d0 + &
        n(idx_H2_ORTHO)*2d0 + &
        n(idx_HD) + &
        n(idx_H) + &
        n(idx_NH) + &
        n(idx_OH) + &
        n(idx_C2H) + &
        n(idx_CH2)*2d0 + &
        n(idx_CHD) + &
        n(idx_H2O)*2d0 + &
        n(idx_HDO) + &
        n(idx_HCN) + &
        n(idx_HCO) + &
        n(idx_HNC) + &
        n(idx_HNO) + &
        n(idx_NH2)*2d0 + &
        n(idx_NHD) + &
        n(idx_O2H) + &
        n(idx_CH) + &
        n(idx_H3j_PARA)*3d0 + &
        n(idx_H3j_ORTHO)*3d0 + &
        n(idx_H2Dj_PARA)*2d0 + &
        n(idx_H2Dj_ORTHO)*2d0 + &
        n(idx_D2Hj_PARA) + &
        n(idx_D2Hj_ORTHO) + &
        n(idx_Hj) + &
        n(idx_HDj) + &
        n(idx_HCOj) + &
        n(idx_H2j_PARA)*2d0 + &
        n(idx_H2j_ORTHO)*2d0 + &
        n(idx_CHj) + &
        n(idx_CH2j)*2d0 + &
        n(idx_CHDj) + &
        n(idx_HNOj) + &
        n(idx_NH2j)*2d0 + &
        n(idx_NHDj) + &
        n(idx_OHj) + &
        n(idx_C2Hj) + &
        n(idx_NHj) + &
        n(idx_H2Oj)*2d0 + &
        n(idx_HDOj) + &
        n(idx_HOCj) + &
        n(idx_HCNj) + &
        n(idx_HNCj) + &
        n(idx_N2Hj) + &
        n(idx_O2Hj) + &
        n(idx_HEHj) + &
        n(idx_H3Oj)*3d0 + &
        n(idx_H2DOj)*2d0 + &
        n(idx_HD2Oj)
    get_Hnuclei = nH

  end function get_Hnuclei

  !******************************
  !get the total number of mantles
  function get_mantle(n)
    use krome_commons
    real*8::n(:),get_mantle,mantle

    mantle = 0.d0
    get_mantle = mantle

  end function get_mantle

  !***************************
  function get_zatoms()
    use krome_commons
    implicit none
    integer::get_zatoms(nspec)

    get_zatoms(1) = 0	!E
    get_zatoms(2) = 0	!GRAIN-
    get_zatoms(3) = 1	!H-
    get_zatoms(4) = 1	!D-
    get_zatoms(5) = 6	!C-
    get_zatoms(6) = 8	!O-
    get_zatoms(7) = 13	!CN-
    get_zatoms(8) = 9	!OH-
    get_zatoms(9) = 9	!OD-
    get_zatoms(10) = 2	!H2_PARA
    get_zatoms(11) = 2	!H2_ORTHO
    get_zatoms(12) = 2	!HD
    get_zatoms(13) = 2	!D2_PARA
    get_zatoms(14) = 2	!D2_ORTHO
    get_zatoms(15) = 1	!H
    get_zatoms(16) = 1	!D
    get_zatoms(17) = 0	!GRAIN0
    get_zatoms(18) = 2	!HE
    get_zatoms(19) = 14	!CO
    get_zatoms(20) = 6	!C
    get_zatoms(21) = 7	!N
    get_zatoms(22) = 8	!O
    get_zatoms(23) = 14	!N2
    get_zatoms(24) = 8	!NH
    get_zatoms(25) = 8	!ND
    get_zatoms(26) = 15	!NO
    get_zatoms(27) = 16	!O2
    get_zatoms(28) = 9	!OH
    get_zatoms(29) = 9	!OD
    get_zatoms(30) = 13	!C2H
    get_zatoms(31) = 12	!C2
    get_zatoms(32) = 13	!C2D
    get_zatoms(33) = 19	!C2N
    get_zatoms(34) = 13	!CN
    get_zatoms(35) = 18	!C3
    get_zatoms(36) = 20	!CCO
    get_zatoms(37) = 8	!CH2
    get_zatoms(38) = 8	!CD2
    get_zatoms(39) = 8	!CHD
    get_zatoms(40) = 22	!CO2
    get_zatoms(41) = 10	!H2O
    get_zatoms(42) = 10	!D2O
    get_zatoms(43) = 10	!HDO
    get_zatoms(44) = 14	!HCN
    get_zatoms(45) = 14	!DCN
    get_zatoms(46) = 15	!HCO
    get_zatoms(47) = 15	!DCO
    get_zatoms(48) = 14	!HNC
    get_zatoms(49) = 14	!DNC
    get_zatoms(50) = 16	!HNO
    get_zatoms(51) = 16	!DNO
    get_zatoms(52) = 22	!N2O
    get_zatoms(53) = 9	!NH2
    get_zatoms(54) = 9	!ND2
    get_zatoms(55) = 9	!NHD
    get_zatoms(56) = 23	!NO2
    get_zatoms(57) = 17	!O2H
    get_zatoms(58) = 17	!O2D
    get_zatoms(59) = 21	!OCN
    get_zatoms(60) = 7	!CH
    get_zatoms(61) = 7	!CD
    get_zatoms(62) = 3	!H3+_PARA
    get_zatoms(63) = 3	!H3+_ORTHO
    get_zatoms(64) = 3	!H2D+_PARA
    get_zatoms(65) = 3	!H2D+_ORTHO
    get_zatoms(66) = 3	!D2H+_PARA
    get_zatoms(67) = 3	!D2H+_ORTHO
    get_zatoms(68) = 3	!D3+_ORTHO
    get_zatoms(69) = 3	!D3+_META
    get_zatoms(70) = 1	!H+
    get_zatoms(71) = 2	!HD+
    get_zatoms(72) = 2	!D2+_ORTHO
    get_zatoms(73) = 2	!D2+_PARA
    get_zatoms(74) = 2	!HE+
    get_zatoms(75) = 15	!HCO+
    get_zatoms(76) = 15	!DCO+
    get_zatoms(77) = 6	!C+
    get_zatoms(78) = 1	!D+
    get_zatoms(79) = 7	!N+
    get_zatoms(80) = 8	!O+
    get_zatoms(81) = 2	!H2+_PARA
    get_zatoms(82) = 2	!H2+_ORTHO
    get_zatoms(83) = 7	!CH+
    get_zatoms(84) = 7	!CD+
    get_zatoms(85) = 15	!NO+
    get_zatoms(86) = 16	!O2+
    get_zatoms(87) = 8	!CH2+
    get_zatoms(88) = 8	!CD2+
    get_zatoms(89) = 8	!CHD+
    get_zatoms(90) = 16	!HNO+
    get_zatoms(91) = 16	!DNO+
    get_zatoms(92) = 9	!NH2+
    get_zatoms(93) = 9	!ND2+
    get_zatoms(94) = 9	!NHD+
    get_zatoms(95) = 14	!CO+
    get_zatoms(96) = 12	!C2+
    get_zatoms(97) = 9	!OH+
    get_zatoms(98) = 9	!OD+
    get_zatoms(99) = 13	!C2H+
    get_zatoms(100) = 13	!C2D+
    get_zatoms(101) = 8	!NH+
    get_zatoms(102) = 8	!ND+
    get_zatoms(103) = 10	!H2O+
    get_zatoms(104) = 10	!D2O+
    get_zatoms(105) = 10	!HDO+
    get_zatoms(106) = 13	!CN+
    get_zatoms(107) = 18	!C3+
    get_zatoms(108) = 20	!C2O+
    get_zatoms(109) = 15	!HOC+
    get_zatoms(110) = 15	!DOC+
    get_zatoms(111) = 19	!C2N+
    get_zatoms(112) = 19	!CNC+
    get_zatoms(113) = 14	!HCN+
    get_zatoms(114) = 14	!DCN+
    get_zatoms(115) = 14	!HNC+
    get_zatoms(116) = 14	!DNC+
    get_zatoms(117) = 21	!NCO+
    get_zatoms(118) = 15	!N2H+
    get_zatoms(119) = 15	!N2D+
    get_zatoms(120) = 17	!O2H+
    get_zatoms(121) = 17	!O2D+
    get_zatoms(122) = 3	!HEH+
    get_zatoms(123) = 3	!HED+
    get_zatoms(124) = 14	!N2+
    get_zatoms(125) = 22	!CO2+
    get_zatoms(126) = 23	!NO2+
    get_zatoms(127) = 3	!D3+_PARA
    get_zatoms(128) = 11	!H3O+
    get_zatoms(129) = 11	!H2DO+
    get_zatoms(130) = 11	!HD2O+
    get_zatoms(131) = 11	!D3O+
    get_zatoms(132) = 0	!CR
    get_zatoms(133) = 0	!g
    get_zatoms(134) = 0	!Tgas
    get_zatoms(135) = 0	!dummy

  end function get_zatoms

  !******************************
  function get_qeff()
    use krome_commons
    implicit none
    real*8::get_qeff(nrea)

    get_qeff(:) = 0e0

  end function get_qeff

  !**************************
  function get_free_fall_time(n)
    use krome_constants
    use krome_commons
    implicit none
    real*8::n(:),m(nspec)
    real*8::rhogas,get_free_fall_time

    m(:) = get_mass()
    rhogas = sum(n(1:nmols)*m(1:nmols))
    get_free_fall_time = sqrt(3d0*pi/32d0/gravity/rhogas)

  end function get_free_fall_time

  !**************************
  function get_free_fall_time_rho(rhogas)
    use krome_constants
    implicit none
    real*8::rhogas,get_free_fall_time_rho

    get_free_fall_time_rho = sqrt(3d0*pi/32d0/gravity/rhogas)

  end function get_free_fall_time_rho

  !********************************
  function get_jeans_length(n,Tgas)
    !get jeans length in cm
    use krome_constants
    use krome_commons
    implicit none
    real*8::n(:),Tgas,mu,rhogas
    real*8::m(nspec),get_jeans_length
    m(:) = get_mass()
    rhogas = max(sum(n(1:nmols)*m(1:nmols)),1d-40)
    mu = get_mu_rho(n(:),rhogas)
    get_jeans_length = sqrt(pi*boltzmann_erg*Tgas/rhogas&
        /p_mass/gravity/mu)

  end function get_jeans_length

  !********************************
  function get_jeans_length_rho(n,Tgas,rhogas)
    !get jeans length in cm
    use krome_constants
    use krome_commons
    implicit none
    real*8::n(:),Tgas,mu,rhogas
    real*8::get_jeans_length_rho

    mu = get_mu_rho(n(:),rhogas)
    get_jeans_length_rho = sqrt(pi*boltzmann_erg*Tgas/rhogas&
        /p_mass/gravity/mu)

  end function get_jeans_length_rho

  !***************************
  !number density to column density conversion
  function num2col(ncalc,n)
    use krome_commons
    implicit none
    real*8::num2col,ncalc,n(:),Tgas
    Tgas = max(n(idx_Tgas),phys_Tcmb)

    num2col = 1.87d21*(max(ncalc,1d-40)*1d-3)**(2./3.)

  end function num2col

  !***********************
  !column density to number density conversion
  function col2num(ncalc,n)
    use krome_commons
    implicit none
    real*8::col2num,ncalc,n(:),Tgas
    Tgas = max(n(idx_Tgas),phys_Tcmb)

    col2num = 1d3 * (max(ncalc,1d-40)/1.87d21)**1.5

  end function col2num

  !************************
  !get electrons by balancing charges
  function get_electrons(n)
    use krome_commons
    implicit none
    real*8::get_electrons,n(nspec)

    get_electrons =  - n(idx_GRAINk) &
        - n(idx_Hk) &
        - n(idx_Dk) &
        - n(idx_Ck) &
        - n(idx_Ok) &
        - n(idx_CNk) &
        - n(idx_OHk) &
        - n(idx_ODk) &
        + n(idx_H3j_PARA) &
        + n(idx_H3j_ORTHO) &
        + n(idx_H2Dj_PARA) &
        + n(idx_H2Dj_ORTHO) &
        + n(idx_D2Hj_PARA) &
        + n(idx_D2Hj_ORTHO) &
        + n(idx_D3j_ORTHO) &
        + n(idx_D3j_META) &
        + n(idx_Hj) &
        + n(idx_HDj) &
        + n(idx_D2j_ORTHO) &
        + n(idx_D2j_PARA) &
        + n(idx_HEj) &
        + n(idx_HCOj) &
        + n(idx_DCOj) &
        + n(idx_Cj) &
        + n(idx_Dj) &
        + n(idx_Nj) &
        + n(idx_Oj) &
        + n(idx_H2j_PARA) &
        + n(idx_H2j_ORTHO) &
        + n(idx_CHj) &
        + n(idx_CDj) &
        + n(idx_NOj) &
        + n(idx_O2j) &
        + n(idx_CH2j) &
        + n(idx_CD2j) &
        + n(idx_CHDj) &
        + n(idx_HNOj) &
        + n(idx_DNOj) &
        + n(idx_NH2j) &
        + n(idx_ND2j) &
        + n(idx_NHDj) &
        + n(idx_COj) &
        + n(idx_C2j) &
        + n(idx_OHj) &
        + n(idx_ODj) &
        + n(idx_C2Hj) &
        + n(idx_C2Dj) &
        + n(idx_NHj) &
        + n(idx_NDj) &
        + n(idx_H2Oj) &
        + n(idx_D2Oj) &
        + n(idx_HDOj) &
        + n(idx_CNj) &
        + n(idx_C3j) &
        + n(idx_C2Oj) &
        + n(idx_HOCj) &
        + n(idx_DOCj) &
        + n(idx_C2Nj) &
        + n(idx_CNCj) &
        + n(idx_HCNj) &
        + n(idx_DCNj) &
        + n(idx_HNCj) &
        + n(idx_DNCj) &
        + n(idx_NCOj) &
        + n(idx_N2Hj) &
        + n(idx_N2Dj) &
        + n(idx_O2Hj) &
        + n(idx_O2Dj) &
        + n(idx_HEHj) &
        + n(idx_HEDj) &
        + n(idx_N2j) &
        + n(idx_CO2j) &
        + n(idx_NO2j) &
        + n(idx_D3j_PARA) &
        + n(idx_H3Oj) &
        + n(idx_H2DOj) &
        + n(idx_HD2Oj) &
        + n(idx_D3Oj)
    get_electrons = max(get_electrons,0d0)

  end function get_electrons

  !************************
  !get species charges
  function get_charges()
    use krome_commons
    implicit none
    integer::get_charges(nspec)

    get_charges(1) = -1.d0 	!E
    get_charges(2) = -1.d0 	!GRAIN-
    get_charges(3) = -1.d0 	!H-
    get_charges(4) = -1.d0 	!D-
    get_charges(5) = -1.d0 	!C-
    get_charges(6) = -1.d0 	!O-
    get_charges(7) = -1.d0 	!CN-
    get_charges(8) = -1.d0 	!OH-
    get_charges(9) = -1.d0 	!OD-
    get_charges(10) = 0.d0 	!H2_PARA
    get_charges(11) = 0.d0 	!H2_ORTHO
    get_charges(12) = 0.d0 	!HD
    get_charges(13) = 0.d0 	!D2_PARA
    get_charges(14) = 0.d0 	!D2_ORTHO
    get_charges(15) = 0.d0 	!H
    get_charges(16) = 0.d0 	!D
    get_charges(17) = 0.d0 	!GRAIN0
    get_charges(18) = 0.d0 	!HE
    get_charges(19) = 0.d0 	!CO
    get_charges(20) = 0.d0 	!C
    get_charges(21) = 0.d0 	!N
    get_charges(22) = 0.d0 	!O
    get_charges(23) = 0.d0 	!N2
    get_charges(24) = 0.d0 	!NH
    get_charges(25) = 0.d0 	!ND
    get_charges(26) = 0.d0 	!NO
    get_charges(27) = 0.d0 	!O2
    get_charges(28) = 0.d0 	!OH
    get_charges(29) = 0.d0 	!OD
    get_charges(30) = 0.d0 	!C2H
    get_charges(31) = 0.d0 	!C2
    get_charges(32) = 0.d0 	!C2D
    get_charges(33) = 0.d0 	!C2N
    get_charges(34) = 0.d0 	!CN
    get_charges(35) = 0.d0 	!C3
    get_charges(36) = 0.d0 	!CCO
    get_charges(37) = 0.d0 	!CH2
    get_charges(38) = 0.d0 	!CD2
    get_charges(39) = 0.d0 	!CHD
    get_charges(40) = 0.d0 	!CO2
    get_charges(41) = 0.d0 	!H2O
    get_charges(42) = 0.d0 	!D2O
    get_charges(43) = 0.d0 	!HDO
    get_charges(44) = 0.d0 	!HCN
    get_charges(45) = 0.d0 	!DCN
    get_charges(46) = 0.d0 	!HCO
    get_charges(47) = 0.d0 	!DCO
    get_charges(48) = 0.d0 	!HNC
    get_charges(49) = 0.d0 	!DNC
    get_charges(50) = 0.d0 	!HNO
    get_charges(51) = 0.d0 	!DNO
    get_charges(52) = 0.d0 	!N2O
    get_charges(53) = 0.d0 	!NH2
    get_charges(54) = 0.d0 	!ND2
    get_charges(55) = 0.d0 	!NHD
    get_charges(56) = 0.d0 	!NO2
    get_charges(57) = 0.d0 	!O2H
    get_charges(58) = 0.d0 	!O2D
    get_charges(59) = 0.d0 	!OCN
    get_charges(60) = 0.d0 	!CH
    get_charges(61) = 0.d0 	!CD
    get_charges(62) = 1.d0 	!H3+_PARA
    get_charges(63) = 1.d0 	!H3+_ORTHO
    get_charges(64) = 1.d0 	!H2D+_PARA
    get_charges(65) = 1.d0 	!H2D+_ORTHO
    get_charges(66) = 1.d0 	!D2H+_PARA
    get_charges(67) = 1.d0 	!D2H+_ORTHO
    get_charges(68) = 1.d0 	!D3+_ORTHO
    get_charges(69) = 1.d0 	!D3+_META
    get_charges(70) = 1.d0 	!H+
    get_charges(71) = 1.d0 	!HD+
    get_charges(72) = 1.d0 	!D2+_ORTHO
    get_charges(73) = 1.d0 	!D2+_PARA
    get_charges(74) = 1.d0 	!HE+
    get_charges(75) = 1.d0 	!HCO+
    get_charges(76) = 1.d0 	!DCO+
    get_charges(77) = 1.d0 	!C+
    get_charges(78) = 1.d0 	!D+
    get_charges(79) = 1.d0 	!N+
    get_charges(80) = 1.d0 	!O+
    get_charges(81) = 1.d0 	!H2+_PARA
    get_charges(82) = 1.d0 	!H2+_ORTHO
    get_charges(83) = 1.d0 	!CH+
    get_charges(84) = 1.d0 	!CD+
    get_charges(85) = 1.d0 	!NO+
    get_charges(86) = 1.d0 	!O2+
    get_charges(87) = 1.d0 	!CH2+
    get_charges(88) = 1.d0 	!CD2+
    get_charges(89) = 1.d0 	!CHD+
    get_charges(90) = 1.d0 	!HNO+
    get_charges(91) = 1.d0 	!DNO+
    get_charges(92) = 1.d0 	!NH2+
    get_charges(93) = 1.d0 	!ND2+
    get_charges(94) = 1.d0 	!NHD+
    get_charges(95) = 1.d0 	!CO+
    get_charges(96) = 1.d0 	!C2+
    get_charges(97) = 1.d0 	!OH+
    get_charges(98) = 1.d0 	!OD+
    get_charges(99) = 1.d0 	!C2H+
    get_charges(100) = 1.d0 	!C2D+
    get_charges(101) = 1.d0 	!NH+
    get_charges(102) = 1.d0 	!ND+
    get_charges(103) = 1.d0 	!H2O+
    get_charges(104) = 1.d0 	!D2O+
    get_charges(105) = 1.d0 	!HDO+
    get_charges(106) = 1.d0 	!CN+
    get_charges(107) = 1.d0 	!C3+
    get_charges(108) = 1.d0 	!C2O+
    get_charges(109) = 1.d0 	!HOC+
    get_charges(110) = 1.d0 	!DOC+
    get_charges(111) = 1.d0 	!C2N+
    get_charges(112) = 1.d0 	!CNC+
    get_charges(113) = 1.d0 	!HCN+
    get_charges(114) = 1.d0 	!DCN+
    get_charges(115) = 1.d0 	!HNC+
    get_charges(116) = 1.d0 	!DNC+
    get_charges(117) = 1.d0 	!NCO+
    get_charges(118) = 1.d0 	!N2H+
    get_charges(119) = 1.d0 	!N2D+
    get_charges(120) = 1.d0 	!O2H+
    get_charges(121) = 1.d0 	!O2D+
    get_charges(122) = 1.d0 	!HEH+
    get_charges(123) = 1.d0 	!HED+
    get_charges(124) = 1.d0 	!N2+
    get_charges(125) = 1.d0 	!CO2+
    get_charges(126) = 1.d0 	!NO2+
    get_charges(127) = 1.d0 	!D3+_PARA
    get_charges(128) = 1.d0 	!H3O+
    get_charges(129) = 1.d0 	!H2DO+
    get_charges(130) = 1.d0 	!HD2O+
    get_charges(131) = 1.d0 	!D3O+
    get_charges(132) = 0.d0 	!CR
    get_charges(133) = 0.d0 	!g
    get_charges(134) = 0.d0 	!Tgas
    get_charges(135) = 0.d0 	!dummy

  end function get_charges

  !*****************************
  ! get metallicity using C as reference
  function get_metallicityC(n)
    use krome_commons
    implicit none
    real*8::n(:),get_metallicityC,zC,nH

    nH = get_Hnuclei(n(:))

    zC = n(idx_Ck) &
        + n(idx_CNk) &
        + n(idx_CO) &
        + n(idx_C) &
        + 2d0*n(idx_C2H) &
        + 2d0*n(idx_C2) &
        + 2d0*n(idx_C2D) &
        + 2d0*n(idx_C2N) &
        + n(idx_CN) &
        + 3d0*n(idx_C3) &
        + 2d0*n(idx_CCO) &
        + n(idx_CH2) &
        + n(idx_CD2) &
        + n(idx_CHD) &
        + n(idx_CO2) &
        + n(idx_HCN) &
        + n(idx_DCN) &
        + n(idx_HCO) &
        + n(idx_DCO) &
        + n(idx_HNC) &
        + n(idx_DNC) &
        + n(idx_OCN) &
        + n(idx_CH) &
        + n(idx_CD) &
        + n(idx_HCOj) &
        + n(idx_DCOj) &
        + n(idx_Cj) &
        + n(idx_CHj) &
        + n(idx_CDj) &
        + n(idx_CH2j) &
        + n(idx_CD2j) &
        + n(idx_CHDj) &
        + n(idx_COj) &
        + 2d0*n(idx_C2j) &
        + 2d0*n(idx_C2Hj) &
        + 2d0*n(idx_C2Dj) &
        + n(idx_CNj) &
        + 3d0*n(idx_C3j) &
        + 2d0*n(idx_C2Oj) &
        + n(idx_HOCj) &
        + n(idx_DOCj) &
        + 2d0*n(idx_C2Nj) &
        + 2d0*n(idx_CNCj) &
        + n(idx_HCNj) &
        + n(idx_DCNj) &
        + n(idx_HNCj) &
        + n(idx_DNCj) &
        + n(idx_NCOj) &
        + n(idx_CO2j)

    zC = max(zC, 0d0)

    get_metallicityC = log10(zC/nH+1d-40) - (-3.57)

    phys_metallicity = get_metallicityC

  end function get_metallicityC

  !*****************************
  ! get metallicity using O as reference
  function get_metallicityO(n)
    use krome_commons
    implicit none
    real*8::n(:),get_metallicityO,zO,nH

    nH = get_Hnuclei(n(:))

    zO = n(idx_Ok) &
        + n(idx_OHk) &
        + n(idx_ODk) &
        + n(idx_CO) &
        + n(idx_O) &
        + n(idx_NO) &
        + 2d0*n(idx_O2) &
        + n(idx_OH) &
        + n(idx_OD) &
        + n(idx_CCO) &
        + 2d0*n(idx_CO2) &
        + n(idx_H2O) &
        + n(idx_D2O) &
        + n(idx_HDO) &
        + n(idx_HCO) &
        + n(idx_DCO) &
        + n(idx_HNO) &
        + n(idx_DNO) &
        + n(idx_N2O) &
        + 2d0*n(idx_NO2) &
        + 2d0*n(idx_O2H) &
        + 2d0*n(idx_O2D) &
        + n(idx_OCN) &
        + n(idx_HCOj) &
        + n(idx_DCOj) &
        + n(idx_Oj) &
        + n(idx_NOj) &
        + 2d0*n(idx_O2j) &
        + n(idx_HNOj) &
        + n(idx_DNOj) &
        + n(idx_COj) &
        + n(idx_OHj) &
        + n(idx_ODj) &
        + n(idx_H2Oj) &
        + n(idx_D2Oj) &
        + n(idx_HDOj) &
        + n(idx_C2Oj) &
        + n(idx_HOCj) &
        + n(idx_DOCj) &
        + n(idx_NCOj) &
        + 2d0*n(idx_O2Hj) &
        + 2d0*n(idx_O2Dj) &
        + 2d0*n(idx_CO2j) &
        + 2d0*n(idx_NO2j) &
        + n(idx_H3Oj) &
        + n(idx_H2DOj) &
        + n(idx_HD2Oj) &
        + n(idx_D3Oj)

    zO = max(zO, 0d0)

    get_metallicityO = log10(zO/nH+1d-40) - (-3.31)

    phys_metallicity = get_metallicityO

  end function get_metallicityO

  !*****************************
  ! get metallicity using N as reference
  function get_metallicityN(n)
    use krome_commons
    implicit none
    real*8::n(:),get_metallicityN,zN,nH

    nH = get_Hnuclei(n(:))

    zN = n(idx_CNk) &
        + n(idx_N) &
        + 2d0*n(idx_N2) &
        + n(idx_NH) &
        + n(idx_ND) &
        + n(idx_NO) &
        + n(idx_C2N) &
        + n(idx_CN) &
        + n(idx_HCN) &
        + n(idx_DCN) &
        + n(idx_HNC) &
        + n(idx_DNC) &
        + n(idx_HNO) &
        + n(idx_DNO) &
        + 2d0*n(idx_N2O) &
        + n(idx_NH2) &
        + n(idx_ND2) &
        + n(idx_NHD) &
        + n(idx_NO2) &
        + n(idx_OCN) &
        + n(idx_Nj) &
        + n(idx_NOj) &
        + n(idx_HNOj) &
        + n(idx_DNOj) &
        + n(idx_NH2j) &
        + n(idx_ND2j) &
        + n(idx_NHDj) &
        + n(idx_NHj) &
        + n(idx_NDj) &
        + n(idx_CNj) &
        + n(idx_C2Nj) &
        + n(idx_CNCj) &
        + n(idx_HCNj) &
        + n(idx_DCNj) &
        + n(idx_HNCj) &
        + n(idx_DNCj) &
        + n(idx_NCOj) &
        + 2d0*n(idx_N2Hj) &
        + 2d0*n(idx_N2Dj) &
        + 2d0*n(idx_N2j) &
        + n(idx_NO2j)

    zN = max(zN, 0d0)

    get_metallicityN = log10(zN/nH+1d-40) - (-4.17)

    phys_metallicity = get_metallicityN

  end function get_metallicityN

end module krome_getphys
