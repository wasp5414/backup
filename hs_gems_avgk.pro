;===================================================================================================
; This program plots OMI Averaging Kernal
;
; PROGRAM NAME:
;  hs_gems_avgk
;
; PURPOSE:
;  plotting OMI Averaging Kernel
;
; DEVELOPER:
;  Jbak
;  Daegeun Shin (geun)
;  Hyeonsic Nam (hs)
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
;  forked by hs
;  Copyright (C) 2017 Satellite Remote Sensing Laboratory, PNU
;  All right reserved.
;
;===================================================================================================

PRO hs_gems_avgk,ymd,type=type,xdrmod=xdrmod
;+---------------------------------------------------------------------------+
; RUN : hs_gems_avgk, 20080608,type=0,xdrmod=1
; INPUT : 8 length integer (yyyymmdd), 1 integer (0,1) type (0 : original avgk, 1 : normalized avgk)
; OUTPUT:  None, plot image and save as png + transfer
;
;+---------------------------------------------------------------------------+

;++++++++++++++++++++++++++++++++++++++++++++++++++++++
; DATA process
;++++++++++++++++++++++++++++++++++++++++++++++++++++++

if not keyword_set(type) then type=0 
if not keyword_set(xdrmod) then xdrmod=0 ;  0: read xdr , 1: read original output (*.out)

ymd=strtrim(ymd,2)
respath='/home/o3p_hs/results/'
;respath='./'
xdrpath='/home/o3p_hs/GEMS/o3p/dat/out/xdr/'
outpath='/home/o3p_hs/GEMS/o3p/dat/out/'
;gems_files = file_search(outpath+'*'+strmid(ymd,0,4)+'m'+strmid(ymd,4,4)+'*') 
gems_files = file_search(outpath+'*'+strmid(ymd,0,4)+strmid(ymd,4,4)+'*origin*')  ; hs for ctp control test
xdr_files  = file_search(xdrpath+'*'+strmid(ymd,0,4)+strmid(ymd,4,4)+'*')

name1=['Original averaging kernel','Nomalized averaging kernel']
name2=['oavgk','navgk']
limit=[20,100,45,140]

; cross section profile
sel_lat=0
sel_lon=120

fnum=n_elements(gems_files)

;for f=2,2 do begin ; for good & bad signal check, 20080613
for f=0,fnum-1 do begin
  hs_read_gems,gems_files[f],gems,xdrmod=xdrmod
  orbit=strmid((strsplit((strsplit(gems_files[f],'/',/ext))[-1],'_',/ext))[5],1,5) ; Orbitnumber extract 
  latdat=bad2nan(gems.lat)
  londat=bad2nan(gems.lon)
  cfracdat=bad2nan(gems.cfrac)
  avgkdat=bad2nan(gems.avgk)
  o3dat=bad2nan(gems.o3)
  ao3dat=bad2nan(gems.ao3)
  ao3edat=bad2nan(gems.ao3e)
  co3dat=bad2nan(gems.co3)
  presdat=bad2nan(gems.pres)
  ctpdat=bad2nan(gems.ctp)
  mondat=bad2nan(gems.mon)
  daydat=bad2nan(gems.day)
  utcdat=bad2nan(gems.utc)
  szadat=bad2nan(gems.sza)
  altdat=bad2nan(gems.alt)
  
  nline=n_elements(gems)
  nlayer=(size(o3dat,/dim))[0]
  ao3edat_norm = ao3edat / (altdat[0:nlayer-1,*] - altdat[1:nlayer,*]) ; Normalized a priori error of ozone
  avgk0 = avgkdat

  ;;; filtering offset
  maxavgres   = 2    & maxrms     = 3   ;  & maxai = 1.0     & maxglint = 20.0  
  mincfrac    = 0.4  & maxcfrac   = 1.0   & minalb = 0.0    & maxalb  =0.2
  minsza      = 0    & maxsza     = 30
  minvza      = 0    & maxvza     = 20
  maxyear     = 2014 & minmon     = 1     & maxmon  = 12

  ;total target pix
  ;minlat      = 18   & maxlat     = 48
  ;minlon      = 110  & maxlon     = 125

  ;clear signal pix
  ;minlat      = 32   & maxlat     = 35
  ;minlon      = 110  & maxlon     = 115
  
  ;cloudy no signal pix
  minlat      = 20   & maxlat     = 28
  minlon      = 115  & maxlon     = 120


  sel = where( $
              ;omiavgres[*,0] le maxavgres and omirms[*,0] le maxrms and  $
              ;omiexval gt 0 and omiexval lt 100 and $  
              ;omiday ne 0 and omiyear le maxyear and omimon le maxmon and omimon ge minmon and $              
              ;omisza ge minsza and omisza le maxsza and $
              ;omivza ge minvza and omivza le maxvza and $
              cfracdat le maxcfrac and cfracdat ge mincfrac and $
              ;omialb(*,0) ge minalb and omialb(*,0) le maxalb and  $
              ;omiline EQ 1040   $
              ;ctp le 600 and  $
              abs(londat) ge minlon and abs(londat) le maxlon and $
              abs(latdat) ge minlat and abs(latdat) le maxlat     $
              , nsel)
  IF nsel eq 0 then begin
    print , 'No pixel selected'
    sel = 0 & nsel = 1
    continue
  ENDIF

  for s=0,nsel-1 do begin ;first pixel selected 
      p = sel[s]  
      print,'index of pixel  :: ',p
      print, mondat[p], daydat[p],  cfracdat[p], szadat[p], ctpdat[p],latdat[p],londat[p]
      avgk      = reform(avgkdat[*,*,p])
      sza       = reform(szadat[p])        & cfrac = reform(cfracdat[p]);  &  alb = reform(omialb[p,0])
      ctp       = reform(ctpdat[p])
      ao3       = reform(ao3dat[*,p])
      ao3e      = reform(ao3edat[*,p])
      ao3e_norm = reform(ao3edat_norm[*,p])
      alt       = reform(altdat[*,p])
  case type of

    0 : begin
      print,'Original AVGK drawing'
      title = 'Origianl AVGK'
      xrange = [min(avgk), max(avgk)]
      yrange = [1000,0.3]
    end

    1 : begin
      print, 'Normalized AVGK drawing'
      for  j = 0, nlayer-1 do begin
        avgk[j,*] = avgk[j,*] / ao3e_norm[j] * ao3e_norm 
        avgk[j,*] = avgk[j,*] / ( alt[j] - alt[j+1] )*2.5

      endfor
      title  = 'AVGK normalized to a priori error'
      xrange = [-0.1,0.6]
      yrange = [1000,0.3]
    end

    ;2 : begin
      ;print, '# of averaging = ',nsel, nprof
      ;avgk0 = avgk ; backup original avgk for normalize
      ;avgk   = fltarr(nlayer, nlayer) 
      ;sza    = mean(reform(sza[p]))
      ;cfrac  =  mean(reform(cfrac[p])) ;& alb = mean(reform(alb(p,0)))
      ;ctp    =  mean(reform(ctp[p]))

      ;FOR i = 0, nline -1 do begin
        ;FOR j = 0 , nline -1 do begin
          ;avgk(i,j) = mean(avgk[i,j,p])
          ;;print  , i, j, avgk(i,j), stddev(omiavgk(sel,i,j))
        ;ENDFOR
      ;ENDFOR
      ;yrange = [1000,0.3]
    ;end


    ;3 : begin
      ;print, 'Not yet'
      ;xrange = [-0.1,0.6]
      ;yrange = [1000,0.3]
      ;title='Averaged Normalized AVGK'
    ;end

  endcase


  pres_mid  = 0.5*(presdat[0:nlayer-1,p]+presdat[1:nlayer,p])  
  alt_mid   = 0.5*(altdat[0:nlayer-1,p]+altdat[1:nlayer,p])
  navgk     = nlayer
  layer0    = 0
  layern    = nlayer ;numer of layer to plot (max:nlayer=24)
  
  ;++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ; PLOT process
  ;++++++++++++++++++++++++++++++++++++++++++++++++++++++
  IF size(avgk,/n_di) ne 2 then begin
     print , 'check data dimension'
  endif

  prefix=respath+'test_'+name2[type]+'_o'+strtrim(orbit,2)+'_p'+strtrim(p,2)
  ps=prefix+'.ps'
  png=prefix+'.png'

  CGPS_OPEN, ps ,xsize=10, ysize=10,/nomatch, /inch, xoffset=0.5, yoffset=0.5
  ;window,0,xs=500,ys=500

  aksym  = 1; 0 no sym // 1;markpeak //2;ak(i,i) ; default = 1
  cap= '!6SZA=' + string(round(sza),format='(I2)') + $
       '!9%!X, !8f!X!Dc!N=' +  string(cfrac, format='(f4.2)'); + $
  ;     ', !4a!X!Ds!N=' +  string(alb, format='(f4.2)')

  cols = fix(findgen(nlayer) * (252. / (nlayer-1)) + 2)
  Position = [0.1,0.08,0.9,0.85]
  !p.position = position
  loadct_wb, 33

  plot, avgk[0, *], pres_mid[*], xrange=xrange,/nodata,yrange=yrange,/ylog,$
        xtitle='!6Rows of averaging Kernel matrix',ytitle='Pressure (hPa)', title=cap, $
        yminor=10,xminor=5, ytickname='!610'+'!U'+['3','2','1','0']+'!N', ystyle=9
  plots, [0, 0], yrange, linestyle=1, color=1, thick=3

  ;if cfrac ge 0.5 then plots, xrange, [1,1]*ptoz(ctp), thick=2, linestyle=1
  ;plots,xrange, [trp,trp], thick=2, linestyle=1

  For j = layer0, layern-1 do begin
    oplot, avgk[j,layer0:nlayer-1], pres_mid[layer0:nlayer-1], color=cols[j], thick=2.
    if pres_mid[j] lt min(yrange) or pres_mid[j] gt max(yrange) then continue
    if aksym eq 1 then begin
      tmp = avgk[j,*]
      q = where( tmp eq max(tmp))
      tmp=tmp[q]
      plots,tmp, pres_mid[q], psym=plotsym_fn(/box,scale=1.5), color=cols[j]
    endif else if aksym eq 2 then begin
      plots,avgk[j,j], pres_mid[j], psym=sym(2,symsize=1.5), color=cols[j]
    endif
      ;plots, total(ak0(j,*)), alt(j), psym=1
  endfor

  legend2, string(alt_mid[layer0:nlayer-1], format='(f4.1)'),textcolor=cols[layer0:nlayer-1], spacing=0.01,/right, box=0
  ;  legend2, dfs_col, box=0, /right
  axis, yaxis=1, ytitle='Altitude (km)', yrange= [ptoz(1000), ptoz(0.3)], ystyle=1,ylog=0
  xyouts, !P.position(0), !p.position(3)+0.1, title, /normal

  cgPs_Close,density=800, /png
  hs_filetrans,png



  ;------------------------------------------------
  ;(2) map of avgk

  ;out_name='test_avgk2'
  ;out_ps=out_name+'.ps'
  ;out_png=out_name+'.png'
  ;CGPs_OPEN, ps ,xsize=10, ysize=10,/nomatch, /inch, xoffset=0.5, yoffset=0.5

  ;; parameter
  ;ak     = reform(avgk)  & ak0 = ak
  ;zs     = atmosz        & ps  = atmosp
  ;p_mid  = zmid(ps)
  ;nl     = n_elements(zs)-1
  ;;------------------------------------------------
  ;xaperr = xae
  ;xaperrn= xaperr / (zs(0:nl-1) - zs(1:nl))

  ;for  j = 0, nl - 1 do begin
    ;ak(*, j) = ak(*, j) / xaperrn(j) * xaperrn
    ;ak(*, j) = ak(*, j) / (zs(j)-zs(j+1))
  ;endfor

  ;data = ak
  ;loadct_wb, 33
  ;xrange=[1200, 10]
  ;nlev = 13 & ncol=nlev-1
  ;levels = findgen(nlev)*0.02-0.1-0.02
  ;levels(0) = min(avgk) & levels(nlev-1) = max(avgk)
  ;del = fix(255/(ncol))
  ;colors = indgen(ncol)*del

  ;plot, findgen(10), findgen(10), /nodata,/ylog,/xlog, xrange=xrange, yrange=xrange,$
        ;xtitle = '!6Pressure(hPa)', ytitle='!6Pressure(hPa)', xminor=9,yminor=9, $
        ;title  = 'OMI Averaging Kernel matrix',position=[0.12,0.1,0.9,0.92]
  ;FOR j = 0 , nl-1 do begin        
     ;FOR i = 0, nl-1 do begin
        ;xcoord = [ps(i), ps(i+1), ps(i+1), ps(i)]
        ;ycoord = [ps(j), ps(j), ps(j+1), ps(j+1)]
        ;tmp = abs( levels-data(i,j))
        ;da  = where( tmp eq min(tmp))
        ;if data(i,j) lt levels(0) then      da = 0
        ;if data(i,j) gt levels(ncol) then   da = ncol-1
        ;if levels(da) gt data(i,j) then begin
           ;da = da -1
        ;endif
        
        ;mycol = colors(da)

        ;if  ps(i) ge 10 and ps(j) ge 10 then begin 
          ;;print, levels(da), data(i,j), levels(da+1), format='(3f7.3)        
          ;if da ne 0 and levels(da)     gt data(i,j) then stop
          ;if da ne ncol-1 and data(i,j) ge levels(da+1)  then stop
          ;polyfill, xcoord, ycoord, color = mycol(0)
        ;endif
    ;ENDFOR
  ;ENDFOR
  ;plots,[ptrp, ptrp], [max(ps), ptrp], thick=5
  ;plots, [max(ps), ptrp], [ptrp, ptrp], thick=5
  ;colorbar_2, levels, colors,format='(f5.2)', $
               ;xs = 0.02, ys=0.8, unit='',/col, /nofirst, /ive
      
  ;cgPs_Close,density=800, /png
  ;file_trans_geun,out_png,/DEL

  endfor
endfor
stop
END
