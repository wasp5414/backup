;===================================================================================================
; PROGRAM NAME:
;  search_track 
;
; PURPOSE:
;  search the omi-track
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
;  Daegeun Shin (geun)
;  Satellite Remote Sensing Laboratory
;  Division of Earth Environmental System
;  College of Natural Science
;  Pusan National University
;                  Tel: +82-51-510-2172
;                  E-mail: daegeun@pusan.ac.kr
;
; REVISION HISTORY:
;  Updated by geun
;
;  Copyright (C) 2017 Satellite Remote Sensing Laboratory, PNU
;  All right reserved.
;
;===================================================================================================

PRO search_track 
  
  lon_min=115.0  &   lon_max=150.0
  lat_min=20.0   &   lat_max=60.0

  year='2008'
  omi_date='0607'

  pixs=[]  
  fns=[]
  orbs=[]
  
  omi_dir='/DB/Data/OMI/1_OML1BRUG/'+year+'/'
  omi_files=FILE_SEARCH(omi_dir+'OMI-Aura_L1-OML1BRUG_'+year+'m'+omi_date+'*.he4',COUNT=nfiles)
  FOR ifile=0,nfiles-1 DO BEGIN
    filename=omi_files[ifile]
    swathname = 'Earth UV-1 Swath'
    stat1 = OMI__LOAD_HDF_DATA(filename, swathname,'Time', omi_time)
    stat2 = OMI__LOAD_HDF_DATA(filename, swathname,'Latitude' , omi_lat)
    stat3 = OMI__LOAD_HDF_DATA(filename, swathname,'Longitude', omi_lon)
    stat4 = OMI__LOAD_HDF_DATA(filename, swathname,'SolarZenithAngle', omi_sza)
    ;print, STRING(ifile, F='(i5)')+ ' / '+ STRING(nfiles, F='(i5)') + '   y'+year
    ; all parameters are exist
    IF stat1 EQ 0 AND stat2 EQ 0 AND stat3 EQ 0 AND stat4 EQ 0 THEN BEGIN
      tmp=WHERE(omi_lon GE lon_min AND omi_lon LE lon_max AND $
                omi_lat GE lat_min AND omi_lat LE lat_max AND omi_sza LE 88., ntmp)
      ;IF ntmp NE 0 THEN BEGIN
        ;tmp_pos=ARRAY_INDICES(omi_lon, tmp)
        ;pix=REFORM(tmp_pos[0,*]+1)
        ;line=REFORM(tmp_pos[1,*]+1)
        ;val=WHERE(pix GE 14 AND pix LE 16, nval)
        ;val=WHERE(pix GE 0 AND pix LE 30 AND line GE 500 AND line LE 1200, nval)
        ;val=WHERE(pix GE 0, nval)
        ;IF nval NE 0 THEN BEGIN
          fns=[fns,filename]
          orb=STRMID(filename, 66,5)
          ;print, F='(A71,A10)',STRMID(filename,70,71,/rev), ' o'+orb 
          ;print, filename, tmp_pos[0]+1, tmp_pos[1]+1, stn_arr_abb[istn], time_utc+time_son[prof_pos]
          orbs=[orbs,orb]
        ;ENDIF ;  nval
      ;ENDIF; ntmp
      DELVARX, omi_time, omi_lat, omi_lon, omi_sza 
    ENDIF ; stat cond
  ENDFOR ; ifile
  norb=N_ELEMENTS(orbs)
  FOR io=0,norb-1 DO BEGIN
    filename=fns[io]
    swathname = 'Earth UV-1 Swath'
    stat1 = OMI__LOAD_HDF_DATA(filename, swathname,'Time', omi_time)
    stat2 = OMI__LOAD_HDF_DATA(filename, swathname,'Latitude' , omi_lat)
    stat3 = OMI__LOAD_HDF_DATA(filename, swathname,'Longitude', omi_lon)
    stat4 = OMI__LOAD_HDF_DATA(filename, swathname,'SolarZenithAngle', omi_sza)
    ;limit=[-90,-180,90,180]
    limit=[-5,75,60,160] 
    plot_omitrack, omi_lon[13:15,*], omi_lat[13:15,*], limit=limit, /hires
    print, F='(A71,A10)',STRMID(fns[io],70,71,/rev), ' o'+orbs[io] 
    
stop

  ENDFOR
stop

stop




END



