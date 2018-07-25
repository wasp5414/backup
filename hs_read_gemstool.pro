;===================================================================================================
; This program plots OMI Weighting Functions
;
; PROGRAM NAME:
;  hs_read_gemstool
;
; PURPOSE:
;  reading gemstool rtm weighting function
;
; PROCESS:
;
; REFERENCE:
;
; INPUT:
;
; OUTPUT:
;
; DEVELOPER:
;  Kanghyun Baek (BK)
;  Daegeun Shin (geun)
;  Satellite Remote Sensing Laboratory
;  Division of Earth Environmental System
;  College of Natural Science
;  Pusan National University
;                  Tel: +82-51-510-2172
;                  E-mail: daegeun@pusan.ac.kr
;
; REVISION HISTORY:
;  bk version 0.
;  Updated by geun
;
;  Copyright (C) 2017 Satellite Remote Sensing Laboratory, PNU
;  All right reserved.
;
;===================================================================================================

pro hs_read_gemstool, exp_name, res, iwav=lambda, refwav=refwav, fwhm=fwhm, exp_mult=ext_mult, $
                                        out_path=out_path, config=config, iprof=iprof, sprsjaco_on=sprsjaco_on, $
                                        conv=conv

  out_path=(KEYWORD_SET(out_path)) ? out_path : '/home/geun/2_O3PR/gemstool/GEMSTOOL/GEMSTOOL_UVN_wrapper/GEMSTOOL_UVN_Results/'
  exp_mult=(KEYWORD_SET(ext_mult)) ? ext_mult : 0
  IF exp_mult EQ 1 THEN BEGIN
    exp_num='1'  
    out_dir=out_path+exp_name+'/exp_'+exp_num+'/'
  ENDIF ELSE BEGIN
    out_dir=out_path+exp_name+'/'
  ENDELSE
  conf_path=out_dir+'configs/'
  line=''

; Read RTControl config
  OPENR,1,conf_path+'GEMSTOOL_RTControl.cfg'
    READF,1,line
    READF,1,line
    nstoke=FIX(line)
    READF,1,line
    IF STRMID(line,0,1) EQ 'T' THEN UpDn_idx=0 ELSE UpDn_idx=1
  CLOSE,1

; Read Geometries
  OPENR,2,conf_path+'GEMSTOOL_Geometries.cfg'
    READF,2,line
    ngeo=FIX(line)
    READF,2,line
    tmp=STRSPLIT(line,/EXTRACT)
    sza=FLOAT(tmp[0])  &  vza=FLOAT(tmp[1])  & azm=FLOAT(tmp[2])
  CLOSE,2


; Read Wavelength config
  OPENR,3,conf_path+'GEMSTOOL_AlbedoClosure.cfg'
    READF,3,line
    nclos_bands=FLOAT(line)
    alb_list=LIST()
    FOR g = 0, nclos_bands-1 DO BEGIN
      READF,3,line
      tmp=STRSPLIT(line,/EXTRACT)
      clos_start=FLOAT(tmp[0])
      clos_finish=FLOAT(tmp[1])
      clos_ncoef=FIX(tmp[2])
      clos_coeff=FLTARR(clos_ncoef)
      FOR k = 0, clos_ncoef-1 DO BEGIN
         READF,3,line
         tmp=STRSPLIT(line,/EXTRACT)
         clos_coeff[k]=FLOAT(tmp[1])
      ENDFOR
      alb_list.add, clos_coeff
    ENDFOR
  CLOSE,3

; Read Wavelength config
  OPENR,4,conf_path+'GEMSTOOL_Lambdas.cfg'
    READF,4,line
    lambda_s=FLOAT(line)
    READF,4,line
    lambda_f=FLOAT(line)
    READF,4,line
    lambda_w=FLOAT(line)
  CLOSE,4

  ;======================================
  ;         READ input file (PTH, GAS)
  ;======================================

  ;pth_gas_file='/home/geun/2_O3PR/gemstool/GEMSTOOL/GEMSTOOL_physicsdata/PTH_PROFILES/geun_profile_1.dat'
  ;READCOL, pth_gas_file,idx,pprof,tprof,h2oprof,ozprof,skipline=11
  ;pprof=REVERSE(pprof)
  ;tprof=REVERSE(tprof)
  ;h2oprof=REVERSE(h2oprof)
  ;ozprof=REVERSE(ozprof)
  ;zfile='/home/geun/2_O3PR/gemstool/GEMSTOOL/GEMSTOOL_UVN_wrapper/geun_heightgrid_1.dat'
  ;READCOL, zfile,idx,zprof

  ;======================================
  ;         READ results file
  ;======================================
  if (nstoke eq 1.0) then BEGIN
    Stokes_str = '_I_Scalar_'
    WFS_str = '_I_'
    vn = 3
  ENDif else BEGIN
    Stokes_str = '_IQU_DoLP_'
    WFS_str = '_I_'
    vn = 6
  ENDelse

  ;----------- READ radiance file -------------
  clevel=['TOAUP','BOADN']
  rad_file = out_dir+'Stokes' + stokes_str + clevel[UpDn_idx]+'.out'

  ;--------radiacen read-------------------------------------------- 
  n = file_lines(rad_file)
  data = dblarr(vn,n)

  OPENR, 40, rad_file
    READf, 40, data
  CLOSE, 40

  radiance = reFORm(data[2,*]) 

  IF KEYWORD_SET(lambda) THEN BEGIN
    lambda = lambda
    nwav = N_ELEMENTS(lambda)
  ENDIF ELSE BEGIN
    nwav = fix((lambda_f - lambda_s)/lambda_w) +1
    lambda = lambda_s + findgen(nwav)*lambda_w
  ENDELSE
 
  rad = radiance
    
  IF keyword_set(conv) then begin 
    IF ~keyword_set(refwav) THEN BEGIN
      conv_rad = convolve_gaussian(lambda,radiance,lambda,fwhm) 
    ENDIF ELSE BEGIN
      ; Read refwav 
      OPENR,5,conf_path+'GEMSTOOL_AerosolLoading.cfg'
      FOR itmp = 0,6 DO BEGIN
        READF,5,line
      ENDFOR
      READF,5,line
      refwav = FIX(line)
      CLOSE,5
      tmp =   convolve_gaussian(lambda,radiance,lambda,fwhm) 
      conv_rad = interpol(tmp,lambda,refwav)
    ENDELSE
  ENDIF

  ;-----read AMF file--------------------
  ;  AMF_file = 'Stokes' + stokes_str + 'AMFTotal_TOAUP.outO3'
  ;  openr, 26, AMF_file 
  ;  readf, 26, temp, temp, AMF_NO2
  ;  close, 26

  ;------ read AMFscat file -------------
  ;SW_file = out_dir + 'Stokes' + WFS_str + 'AMFScatWts_'+clevel[UpDn_idx]+'.outO3'
  ;n = file_lines(SW_file)
  ;height_SW = dblarr(n)
  ;Scat_wts  = dblarr(n)
  ;openr, 23, SW_file
  ;for i = 0d, n-1 do begin
    ;readf, 23, temp, hgt, swt
    ;height_SW[i] = hgt
    ;Scat_wts [i] = swt
  ;endfor
  ;close, 23

  ;------ read GasJacobian file -------------
  jaco_file = out_dir + 'Stokes' + WFS_str + 'GasWFs_'+clevel[UpDn_idx]+'.outO3'
  n = file_lines(jaco_file)
  data_gaswfs = fltarr(nwav+2,n)
  jacobian = dblarr(nwav,n)

  openr, 24, jaco_file
    readf, 24, data_gaswfs 
    jacobian[*,*] = data_gaswfs[2:2+nwav-1,*]
  close, 24

  ;---------------------------------------------------- 


  ;------ read AlbJacobian file -------------
  albjaco_file = out_dir + 'Stokes' + WFS_str + 'AlbWFs_'+clevel[UpDn_idx]+'.out'
  n = file_lines(albjaco_file)
  data_albwfs = dblarr(vn,n)

  OPENR, 25, albjaco_file
    READf, 25, data_albwfs
  CLOSE, 25

  albjaco = reFORm(data_albwfs[2,*])
  ;---------------------------------------------------- 


  IF KEYWORD_SET(sprsjaco_on) THEN BEGIN
    ;------ read SpressJcobian file -------------
    sprsjaco_file = out_dir + 'Stokes' + WFS_str + 'SURFPWFs_'+clevel[UpDn_idx]+'.out'
    n = file_lines(sprsjaco_file)
    data_sprswfs = dblarr(vn,n)

    OPENR, 25, sprsjaco_file
      READf, 25, data_sprswfs
    CLOSE, 25

    sprsjaco = reFORm(data_sprswfs[2,*])
    ;---------------------------------------------------- 
  ENDIF

  ;-----------read dlnI/d(DU)---------------------
  dlndu_file = out_dir+'Stokes' + WFS_str + 'dlnI_du_'+clevel[UpDn_idx]+'.outO3'
  n = file_lines(dlndu_file)
  data_dlndu = fltarr(nwav+2,n)
  dln_du = dblarr(nwav,n)

  openr, 26, dlndu_file
    readf, 26, data_dlndu
    dln_du[*,*] = data_dlndu[2:2+nwav-1,*]
  close, 26


  ;IF KEYWORD_SET(iprof) THEN BEGIN

    ;-----------read Inpprofs---------------------
    inprof_file = out_dir+'Stokes' + WFS_str + 'Inprof_'+clevel[UpDn_idx]+'.out'
    ;READCOL,inprof_file,tmp,izprof,itprof,ipprof,h2o,o3vmr
    READCOL,inprof_file,tmp,izprof,itprof,ipprof,o3vmr
    ;iprof = {zprof:izprof,tprof:itprof,pprof:ipprof,h2o:h2o,o3vmr:o3vmr}
    iprof = {zprof:izprof,tprof:itprof,pprof:ipprof,o3vmr:o3vmr}
  ;ENDIF

  jaco=jacobian
  wav=lambda
  ;res = {rad:rad, wav:lambda, jaco:jacobian,dlndu:dln_du, albjaco:albjaco,pprof:pprof, $
         ;tprof:tprof, zprof:zprof, h2oprof:h2oprof, ozprof:ozprof  }
  res = {rad:rad, wav:lambda, jaco:jacobian,dlndu:dln_du, albjaco:albjaco}
  IF KEYWORD_SET(sprsjaco_on) THEN BEGIN
    res = {rad:rad, wav:lambda, jaco:jacobian,dlndu:dln_du, albjaco:albjaco, sprsjaco:sprsjaco}
  ENDIF


  config  = {sza:sza, vza:vza, azm:azm, nwav:nwav, alb_list:alb_list, $
             lambda_f:lambda_f, lambda_s:lambda_s, lambda_w:lambda_w}

END
