;===================================================================================================
; This program plots OMI passing track
;
; PROGRAM NAME:
;  plot_omitrack
;
; PURPOSE:
;  plotting OMI passing track
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
;  Jbak version 0.
;  Updated by geun
;
;  Copyright (C) 2017 Satellite Remote Sensing Laboratory, PNU
;  All right reserved.
;
;===================================================================================================

PRO plot_omitrack, inlon, inlat, limit=limit, hiresolution=hiresolution

  dimsize=SIZE(inlon)
  IF dimsize[0] EQ 2 THEN BEGIN
    xx=reform(inlon, (size(inlon,/dim))[0]*(size(inlon,/dim))[1] )
    yy=reform(inlat, (size(inlat,/dim))[0]*(size(inlat,/dim))[1] )
  ENDIF ELSE BEGIN
    xx=inlon
    yy=inlat
  ENDELSE

  out_ps='omi_track.ps'
  out_png='omi_track.png'
  CGPS_OPEN, out_ps ,XSIZE=10,YSIZE=8,/NOMATCH,/INCH

  LOADCT,0
  TVLCT, CGColor('WHITE', /Triple), 254 ; Background color.
  TVLCT, CGColor('BLACK', /Triple), 255 ; Drawing color.
  !P.COLOR=255
  !P.BACKGROUND=254
  !P.CHARSIZE=2
  !P.CHARTHICK=2
  !P.POSITION=[0.12,0.12,0.95,0.9]

  ;limit=[30,115,50,142]

  IF ~KEYWORD_SET(limit) THEN limit=[-90,-180,90,180]
  ;limit=[10,40,60,180]
  MAP_SET,  LIMIT=limit, XMARGIN=0, YMARGIN=0,/CYLIN,/NOERA,/NOBOR, POSITION=multi_pos1
  IF KEYWORD_SET(hiresolution) THEN BEGIN
    MAP_CONTINENTS,/CONTINENT,/COUNT,/COUNTRY,/HIRES;,COLOR=CGCOLOR('black');,/river
  ENDIF ELSE BEGIN
    MAP_CONTINENTS,/CONTINENT,/COUNT;,COLOR=CGCOLOR('black');,/river
  ENDELSE

  tmp=fltarr(2,2)

  OPLOT,xx, yy, PSYM=cgSymCat(15),symsize=0.7,color=CGCOLOR('RED')
  CONTOUR, tmp, tmp, tmp, /NODAT,/NOERA,CHARSIZE=2,CHARTHICK=2, $
           XSTYLE=1, YSTYLE=1,YTICKFORMAT='(I3)',XTICKFORMAT='(i4)', $
           XRANGE=[limit(1), limit(3)], YRANGE=[limit(0), limit(2)], $
           XTITLE = 'Longitude',YTITLE='Latitude'

  cgPs_Close,density=800, /png
  ;file_trans_geun,out_png,/DEL

END

