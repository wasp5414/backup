;===================================================================================================
; PROGRAM NAME:
;  omi_lonlat2pixline
;
; PURPOSE:
;  lonlat to pixline in omi-track
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
;  Copyright (C) 2017 Satellite Remote Sensing Laboratory, PNU
;  All right reserved.
;
;===================================================================================================

PRO omi_lonlat2pixline 
  
; dom1
  ;lon_min=-10  &   lon_max=10
  ;lat_min=-20    &   lat_max=0

; dom2
  ;lon_min=-90  &   lon_max=-70
  ;lat_min=-25    &   lat_max=0

; dom2
  lon_min=100  &   lon_max=140
  lat_min=5    &   lat_max=45

  year='2005'
  omi_date='0'

  fns=[]
  orbs=[]
  minpixs=[]
  maxpixs=[]
  minlines=[]
  maxlines=[]
  
  xdr_call=0
  IF xdr_call EQ 0 THEN BEGIN
    omi_dir='/DB/Data/OMI/1_OML1BRUG/'+year+'/'
    omi_files=FILE_SEARCH(omi_dir+'OMI-Aura_L1-OML1BRUG_'+year+'m'+omi_date+'*.he4',COUNT=nfiles)
    omimon=strmid(omi_files,44,2,/rev)
    ;valfile=WHERE(omimon EQ '03' OR omimon EQ '04',nfiles)
    valfile=WHERE(omimon EQ '05',nfiles)
    omi_files=omi_files[valfile]

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
                  omi_lat GE lat_min AND omi_lat LE lat_max, ntmp)
        IF ntmp NE 0 THEN BEGIN
          tmp_pos=ARRAY_INDICES(omi_lon, tmp)
          pix=REFORM(tmp_pos[0,*]+1)
          line=REFORM(tmp_pos[1,*]+1)
          ;val=WHERE(pix GE 14 AND pix LE 16, nval)
          ;IF nval NE 0 THEN BEGIN
            fns=[fns,filename]
            orb=STRMID(filename, 66,5)
            print, F='(A71,A10)',STRMID(filename,70,71,/rev), ' o'+orb 
            orbs=[orbs,orb]
            minpixs=[minpixs,min(pix)]
            maxpixs=[maxpixs,max(pix)]
            minlines=[minlines,min(line)]
            maxlines=[maxlines,max(line)]
          ;ENDIF ;  nval
        ;plot_omitrack, omi_lon[tmp], omi_lat[tmp], limit=[lat_min-10,lon_min-10,lat_max+10,lon_max+10], /hires
        ;plot_omitrack, omi_lon, omi_lat, limit=[lat_min-10,lon_min-10,lat_max+10,lon_max+10], /hires
;stop
        ENDIF; ntmp
        DELVARX, omi_time, omi_lat, omi_lon, omi_sza
      ENDIF ; stat cond
    ENDFOR ; ifile
    norb=N_ELEMENTS(orbs)
    ;SAVE,file='bk_runlist.xdr',fns,orbs,minpixs,maxpixs,minlines,maxlines,/XDR
  ENDIF ELSE BEGIN
    RESTORE, 'bk_runlist.xdr'
  ENDELSE

  write_on=1
  IF write_on EQ 1 THEN BEGIN
    nfns=N_ELEMENTS(fns)
    OPENW,lun,'bk_runlist.list',/GET_LUN
    FOR ifile=0,nfns-1 DO BEGIN
      printf,lun, F='(A12,4I5,A7)',STRMID(fns[ifile],49,12,/rev), minlines[ifile],maxlines[ifile], $
                           minpixs[ifile], maxpixs[ifile], orbs[ifile]

    ENDFOR
    FREE_LUN,lun
  ENDIF

stop




END



