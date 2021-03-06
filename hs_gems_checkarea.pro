pro hs_gems_checkarea,xdrmod=xdrmod,which_avgk=which_avgk

;+---------------------------------------------------------------------------+
; RUN : hs_gems_checkarea,xdrmod=1
; in first run, xdrmod must be 0
;
; INPUT : 
; OUTPUT: 
; 
; check ozone profile and column over seafog area and clear area
;
;+---------------------------------------------------------------------------+
;xdrmod=1
if not keyword_set(xdrmod) then xdrmod=0
if not keyword_set(which_avgk) then which_avgk=0 ; 0: original 1: normalized

;inputday = strtrim(inputday,2)

unit_of_o3 = '(DU)' ; '(ppb)'
respath='/home/o3p_hs/results/'
outpath='/home/o3p_hs/GEMS/o3p/dat/out/'
pixpath='/home/Data/OMI/2_OML2PIXCOR/2005/'
types = []

gems_files = file_search(outpath+'*'+'07291'+'*')
;gems_files = file_search(outpath+'*'+'20197'+'*origin*')
fnum=n_elements(gems_files)

for i=0,fnum-1 do begin   
  type = (strsplit((strsplit(gems_files[i],'_',/ext))[-1],'.',/ext))[0]
  void = execute('fn_'+type+ '=' +'"'+gems_files[i]+'"')

  ; 1 orbit mode
  orbit = '20197'
  pixcor_file   = file_search(pixpath+'*'+'o'+orbit+'*')
  hs_omi_pixcor,pixcor_file,pixcor,xdrmod=xdrmod
  
  hs_read_gems,gems_files[i],gems,xdrmod=xdrmod
  o3   =bad2nan(gems.o3)
  co3  =bad2nan(gems.co3)
  pres =bad2nan(gems.pres)
  lat  =bad2nan(gems.lat)
  lon  =bad2nan(gems.lon)
  cfrac=bad2nan(gems.cfrac)
  avgk =bad2nan(gems.avgk)
  o3   =bad2nan(gems.o3)
  ao3  =bad2nan(gems.ao3)
  ao3e =bad2nan(gems.ao3e)
  co3  =bad2nan(gems.co3)
  pres =bad2nan(gems.pres)
  ctp  =bad2nan(gems.ctp)
  mon  =bad2nan(gems.mon)
  day  =bad2nan(gems.day)
  utc  =bad2nan(gems.utc)
  sza  =bad2nan(gems.sza)
  alt  =bad2nan(gems.alt)
  nline = n_elements(gems)
  nlayer=(size(o3,/dim))[0]
  ao3e_norm = ao3e / (alt[0:nlayer-1,*] - alt[1:nlayer,*]) ; Normalized a priori error of ozone 
  avgk0 =avgk

  if ( type eq 'origin' ) then begin
    ;void = execute('f_'  +type+ '=' + 'gems')
    void = execute('o3_' +type+ '=' + 'gems.o3')
    void = execute('co3_'+type+ '=' + 'gems.co3')
    ;void = execute('p_'+type+ '=' + 'gems.pres')
    void = execute('ctp_'  +type+ '=' + 'ctp')
    void = execute('cfrac_'+type+ '=' + 'cfrac')
    void = execute('lat_'  +type+ '=' + 'lat')
    void = execute('lon_'  +type+ '=' + 'lon')
    void = execute('avgk_' +type+ '=' + 'avgk')
  endif
  ; multi orbit mode
  ;orbit=strmid((strsplit(gems_files[i],'_',/ext))[-4],1,5)
  ;pixcor_files = file_search(pixpath+'*'+'o'+orbit+'*')
  ;pixcor_files  = [pixcor_files,pixcor_file]
  ;orbits        = [orbits,orbit]
;stop 
  clrindx = where(cfrac_origin lt 0.1)
  cldindx = where(cfrac_origin gt 0.6)
  ;clrindx = where(cfrac_origin lt 0.1 and $
                  ;lat_origin lt 37 and lat_origin gt 30 and $
                  ;lon_origin gt 120 and lon_origin lt 125 and $
                  ;co3_origin[2,*] gt 50) ;clear index
  ;cldindx = where(ctp_origin gt 900 and cfrac_origin gt 0.4 and $
                 ;lat_origin lt 37 and lat_origin gt 30)
  nclrindx = n_elements(clrindx)
  ncldindx = n_elements(cldindx)
  npix  = n_elements(lon)
  avgctp = mean(ctp[cldindx])
  avgcfrac = mean(cfrac[cldindx])
  stdctp = stddev(ctp[cldindx])
  stdcfrac = stddev(cfrac[cldindx])
  ;stop

;+---------------------------------------------------------------------------+
;
; #1.Plot on map 
;
;+---------------------------------------------------------------------------+

  ;title='Seafog area select'
  ;prefix=respath+'seafogcheck3'
  ;ps=prefix+'.ps'
  ;png=prefix+'.png'
  ;cgps_open,ps,xsize=7,ysize=7,/nomatch,/inch,xoffset=0.5,yoffset=0.5
  ;pos=[0.05,0.05,0.82,0.83]
  ;loadct_wb,72
  ;limit=[20,100,45,140]
  ;MAP_SET,0,120,limit=limit,/conic,latdel=10,londel=10,position=pos,standard_parallels=[20,60]
  ;nlev=51
  ;;barcol  =fix(findgen(nlev-1)*250/(nlev-2))
  ;barcol  =reverse(fix(findgen(nlev-1)*250/(nlev-2)))
  ;;totallev=findgen(nlev)*1.5  + 270
  ;;strlev  =findgen(nlev)      + 240
  ;;troplev =findgen(nlev)      + 30
  ;;lev=findgen(nlev)*1.5  + 250
  ;;lev  = findgen(nlev) - 25.
  ;cfraclev  = findgen(nlev)/(nlev-1) 
  ;;ctplev    = reverse(findgen(nlev)/(nlev-1)*900 + 100)
  ;lev = cfraclev
  ;;lev = ctplev
  ;;data = co3_300[0,*] - co3_origin[0,*]
  ;;data = co3_spres[0,*] - co3_origin[0,*]
  ;;data = cfrac_origin
  ;data = cfrac_origin
  ;data2 = ctp_origin

  ;;layer=0 ; 0: total, 1: strat, 2: trop

  ;for j=0,npix-1 do begin
    ;corind=where(abs(pixcor.lat - gems[j].lat) eq min(abs(pixcor.lat-gems[j].lat)))
    ;corind=array_indices(pixcor.lat,corind)
    ;clon=reform(pixcor.clon[corind[0],corind[1],*])
    ;clat=reform(pixcor.clat[corind[0],corind[1],*])

    ;;for k=0,nlev-2 do begin ; reverse case, eg) ctp
      ;;if (data[j] le lev[k] and data[j] gt lev[k+1]) then col=barcol[k]
    ;;endfor
    ;TVLCT, CGColor('Sky Blue',/Triple), 252 ;not fog
    ;TVLCT, CGColor('Black',/Triple), 251 ;bad pixel
    ;if (data2[j] gt 900 and data[j] gt 0.15) then begin
      ;for k=0,nlev-2 do begin
        ;if (data[j] ge lev[k] and data[j] lt lev[k+1]) then col=barcol[k]
      ;endfor
    ;endif else col=252
    ;if (data[j] eq -999.) then col=251 
    ;polyfill,[clon,clon[0]],[clat,clat[0]],color=col,noclip=0
  ;endfor
  ;;colorbar_2,lev,barcol,/col,ys=0.77,xs=0.015,charsize=1.9,charthick=2,levelind=findgen(100)*5,$
            ;;format='(f4.2)',lowleft=[0.87,0.05],/nofirst,/right

  ;map_continents,/continent,/count,MLINETHICK=1.8,/country,/hires
  ;map_grid,latdel=10,londel=10,thick=0.7,/box_axes
  ;xyouts,mean([pos[0],pos[2]]),pos[3]+0.1,align=0.5,title,charsize=1.5,/normal
  ;cgps_close,density=800,/png
  ;hs_filetrans,png
  ;stop
  ;end

;+---------------------------------------------------------------------------+
; 
; #2.Weighting function check
;
;+---------------------------------------------------------------------------+


  ;for s=0,1 do begin ;first pixel selected
  ;;for s=0,nclrindx-1 do begin 
      ;P = clrindx[s]
      ;print,'index of pixel  :: ',P
      ;print, mon[P], day[P],  cfrac[P], sza[P], ctp[P],lat[P],lon[P]
      ;Pavgk      = reform(avgk[*,*,P])
      ;Psza       = reform(sza[P])        & Pcfrac = reform(cfrac[P]);  &  alb = reform(omialb[P,0])
      ;Pctp       = reform(ctp[P])
      ;Pao3       = reform(ao3[*,P])
      ;Pao3e      = reform(ao3e[*,P])
      ;Pao3e_norm = reform(ao3e_norm[*,P])
      ;Palt       = reform(alt[*,P])

    ;case which_avgk of
      ;0 : begin
        ;print,'Original AVGK drawing'
        ;title = 'Origianl AVGK'
        ;xrange = [min(Pavgk), max(Pavgk)]
        ;yrange = [1000,200]
      ;end

      ;1 : begin
        ;print, 'Normalized AVGK drawing'
        ;for  j = 0, nlayer-1 do begin
          ;Pavgk[j,*] = Pavgk[j,*] / Pao3e_norm[j] * Pao3e_norm
          ;Pavgk[j,*] = Pavgk[j,*] / ( Palt[j] - Palt[j+1] )*2.5
        ;endfor
        ;title  = 'AVGK normalized to a priori error'
        ;xrange = [-0.1,0.5]
        ;;xrange = [-0.1,0.6]
        ;yrange = [200,0.2]
        ;;yrange = [1000,200]
      ;end
    ;endcase
    ;pres_mid  = 0.5*(pres[0:nlayer-1,P]+pres[1:nlayer,P]) & alt_mid = 0.5*(alt[0:nlayer-1,P]+alt[1:nlayer,P])
    ;navgk     = nlayer
    ;layer0    = 0
    ;layern    = nlayer ;numer of layer to plot (max:nlayer=24)

    ;IF size(Pavgk,/n_di) ne 2 then begin
       ;print , 'check data dimension'
    ;endif

    ;;prefix=respath+'avgk_'+'seafog'+'_o'+strtrim(orbit,2)+'_'+strtrim(s,2)
    ;prefix=respath+'avgk_'+'clear2_'+strtrim(s,2)
    ;;prefix=respath+'avgk_'+'seafog2_'+strtrim(s,2)
    ;ps=prefix+'.ps'
    ;png=prefix+'.png'

    ;CGPS_OPEN, ps ,xsize=10, ysize=10,/nomatch, /inch, xoffset=0.5, yoffset=0.5
    ;;window,0,xs=500,ys=500

    ;aksym  = 1; 0 no sym // 1;makpeak //2;ak(i,i) ; default = 1
    ;cap= '!6SZA=' + string(round(Psza),format='(I2)') + $
         ;'!9%!X, !8f!X!Dc!N=' +  string(Pcfrac, format='(f4.2)') + $
         ;', Lat=' + string(lat[P], format='(f5.2)') + ', Lon=' + string(lon[P], format='(f6.2)') ; + $
    ;;     ', !4a!X!Ds!N=' +  string(alb, format='(f4.2)')

    ;cols = fix(findgen(nlayer) * (252. / (nlayer-1)) + 2)
    ;Position = [0.1,0.08,0.9,0.85]
    ;!p.position = position
    ;loadct_wb, 33

    ;plot, Pavgk[0, *], pres_mid[*], xrange=xrange,/nodata,yrange=yrange,/ylog,$
          ;xtitle='!6Rows of averaging Kernel matrix',ytitle='Pressure (hPa)', title=cap, $
          ;yminor=10,xminor=5, ytickname='!610'+'!U'+['3','2','1','0']+'!N', ystyle=9
    ;plots, [0, 0], yrange, linestyle=1, color=1, thick=3

    ;;if cfrac ge 0.5 then plots, xrange, [1,1]*ptoz(ctp), thick=2, linestyle=1
    ;;plots,xrange, [trp,trp], thick=2, linestyle=1

    ;For j = layer0, layern-1 do begin
      ;oplot, Pavgk[j,layer0:nlayer-1], pres_mid[layer0:nlayer-1], color=cols[j], thick=2.
      ;if pres_mid[j] lt min(yrange) or pres_mid[j] gt max(yrange) then continue
      ;if aksym eq 1 then begin
        ;tmp = Pavgk[j,*]
        ;q = where( tmp eq max(tmp))
        ;tmp=tmp[q]
        ;plots,tmp, pres_mid[q], psym=plotsym_fn(/box,scale=1.5), color=cols[j]
      ;endif else if aksym eq 2 then begin
        ;plots,Pavgk[j,j], pres_mid[j], psym=sym(2,symsize=1.5), color=cols[j]
      ;endif
        ;;plots, total(ak0(j,*)), alt(j), psym=1
    ;endfor

    ;legend2, string(alt_mid[layer0:nlayer-1], format='(f4.1)'),textcolor=cols[layer0:nlayer-1], spacing=0.01,/right, box=0
    ;;  legend2, dfs_col, box=0, /right
    ;axis, yaxis=1, ytitle='Altitude (km)', yrange= [ptoz(yrange[0]), ptoz(yrange[1])], ystyle=1,ylog=0
    ;xyouts, !P.position(0), !p.position(3)+0.1, title, /normal

    ;cgPs_Close,density=800, /png
    ;hs_filetrans,png
  ;endfor

;+---------------------------------------------------------------------------+
;
;  #3. O3 profile check
;
;+---------------------------------------------------------------------------+
  prefix=respath+'o3p_'+type+'_'+strtrim(2,2)
  ps=prefix+'.ps'
  png=prefix+'.png'
  title = 'GEMS ozone profile ('+type+'), seafog vs clear'
  ;window,0,xs=800,ys=800
  CGPS_OPEN, ps ,xsize=10, ysize=10,/nomatch, /inch, xoffset=0.5, yoffset=0.5
  loadct_wb, 72

  aksym  = 2; 0 no sym // 1;makpeak //2;ak(i,i) ; default = 1
  layer0    = 0
  layern    = nlayer ;numer of layer to plot (max:nlayer=24)
  xrange = [-10,10] & yrange = [1000,10]
  ;nlev = 51
  ;cols = findgen(nlev)/(nlev-1) * 100 + 150
  ;cfraclev  = findgen(nlev)/(nlev-1)
  ;if (clrcld eq 0) then col=240
  ;if (clrcld eq 1) then col=10
  clrcol=240 & cldcol=10 & difcol=30 & col=[cldcol,clrcol,difcol]
  ;cap= '!6SZA=' + string(round(Psza),format='(I2)') + $
       ;'!9%!X, !8f!X!Dc!N=' +  string(Pcfrac, format='(f4.2)') + $
       ;', !6Lat=' + string(lat[P], format='(f5.2)') + ', !6Lon=' + string(lon[P], format='(f6.2)') ; + $
  ;;     ', !4a!X!Ds!N=' +  string(alb, format='(f4.2)')
  cap= '!6SZA=' + string(round(mean(sza[cldindx])),format='(I2)') + $
       '!9%!X !8f!X!Dc!N=' + string(avgcfrac, format='(f4.2)') + $
       '!6(std ' + strtrim(string(stdcfrac,format='(f3.1)'),2)+')' + $
       '!6 CTP=' + strtrim(string(avgctp,format='(f6.1)'),2) + $
       '!6(std ' + strtrim(string(stdctp,format='(f4.1)'),2)+')' + $
       '!6 Lat=' + string(mean(lat[cldindx]), format='(f5.2)') + $
       '!6 Lon=' + string(mean(lon[cldindx]), format='(f6.2)') ; + $
  ;     ', !4a!X!Ds!N=' +  string(alb, format='(f4.2)')
  Position = [0.1,0.08,0.9,0.85]
  !p.position = position
  plot, o3[*,0], pres[0:nlayer-1,0], xrange=xrange,/nodata,yrange=yrange, $
        /ylog,xtitle='!6O3 at each layer (DU)',ytitle='Pressure (hPa)', $
        title=cap,yminor=10,xminor=5, $
        ytickname='!610'+'!U'+['3','2','1','0']+'!N', ystyle=9

;difference check
  for s=0,19 do begin ; twenty pixels selected
    for clrcld=0,1 do begin
      if (clrcld eq 0) then begin
        P1=cldindx[s] 
        print, mon[P1], day[P1], cfrac[P1], sza[P1], ctp[P1], lat[P1], lon[P1]
        Psza      = reform(sza[P1]) & Pcfrac = reform(cfrac[P1])
;alb = reform(alb[P,0])
        Pctp      = reform(ctp[P1])
        Po3       = reform(o3[*,P1])
        pres_mid  = 0.5*(pres[0:nlayer-1,P1]+pres[1:nlayer,P1])
        alt_mid = 0.5*(alt[0:nlayer-1,P1]+alt[1:nlayer,P1])
        ;oplot, reform(o3[*,P1]), pres_mid[layer0:nlayer-2], thick=2., $
        ;color=col[clrcld]
      endif
 
      if (clrcld eq 1) then begin
        P2=clrindx[s]    
        print, mon[P1], day[P1], cfrac[P1], sza[P1], ctp[P1], lat[P1], lon[P1]
        Psza      = reform(sza[P1])        & Pcfrac = reform(cfrac[P1]);  &  alb = reform(alb[P,0])
        Pctp      = reform(ctp[P1])
        Po3       = reform(o3[*,P1])
        pres_mid  = 0.5*(pres[0:nlayer-1,P1]+pres[1:nlayer,P1]) & alt_mid = 0.5*(alt[0:nlayer-1,P1]+alt[1:nlayer,P1])
        ;oplot, reform(o3[*,P2]), pres_mid[layer0:nlayer-2], thick=2., color=col[clrcld]
      endif
      ;for k = 0,nlev-2 do begin
        ;if (Pcfrac ge cfraclev[k] and Pcfrac lt cfraclev[k+1]) then col=cols[k]
      ;endfor
    endfor
    oplot, reform(o3[*,P1]-o3[*,P2]), pres_mid[layer0:nlayer-2], thick=3., color=col[clrcld]
    plots, [0,0], yrange, thick=2, linestyle=1
  endfor
  ;for s=0,19 do begin ; twenty pixels selected
    ;for clrcld=0,1 do begin
      ;if (clrcld eq 0) then P=cldindx[s] 
      ;if (clrcld eq 1) then P=clrindx[s]    
      ;print, mon[P], day[P], cfrac[P], sza[P], ctp[P], lat[P], lon[P]
      ;Psza      = reform(sza[P])        & Pcfrac = reform(cfrac[P]);  &  alb = reform(alb[P,0])
      ;Pctp      = reform(ctp[P])
      ;Po3       = reform(o3[*,P])
      ;pres_mid  = 0.5*(pres[0:nlayer-1,P]+pres[1:nlayer,P]) & alt_mid = 0.5*(alt[0:nlayer-1,P]+alt[1:nlayer,P])
      ;Psza      = reform(sza[P])        & Pcfrac = reform(cfrac[P]);  &  alb = reform(alb[P,0])
      ;Pctp      = reform(ctp[P])
      ;Po3       = reform(o3[*,P])
      ;pres_mid  = 0.5*(pres[0:nlayer-1,P]+pres[1:nlayer,P]) & alt_mid = 0.5*(alt[0:nlayer-1,P]+alt[1:nlayer,P])
      ;;for k = 0,nlev-2 do begin
        ;;if (Pcfrac ge cfraclev[k] and Pcfrac lt cfraclev[k+1]) then col=cols[k]
      ;;endfor
      ;oplot, reform(o3[*,P]), pres_mid[layer0:nlayer-2], thick=2., color=col[clrcld]
    ;endfor
  ;endfor
  axis, yaxis=1, ytitle='Altitude (km)', yrange= [ptoz(yrange[0]), $
        ptoz(yrange[1])], ystyle=1,ylog=0
  legend2,['Seafog pixel','Clear pixel'],textcolor=col,spacing=0.01, $
          /right,box=0
  xyouts, mean([position[0],position[2]]), !p.position(3)+0.07,align=0.5, $
          title,charsize=3, /normal
  ;plots, [0, 0], yrange, linestyle=1, color=1, thick=3
  ;plots, [0,50],[avgctp,avgctp],linestyle=1,color=250,thick=3
  cgPs_Close,density=800, /png
  hs_filetrans,png


;+---------------------------------------------------------------------------+
; 
; Track plot cloud fraction & troposphere column O3 & O3 profile
; ! NOT USE FOR NOW.
;
;+---------------------------------------------------------------------------+


;posi1=[0.15,0.7,0.85,0.90]

;win = window(dimensions=[900,950])
;title = text(0.5,0.95,'O3 profile vs cloud property analysis',font_size=30,alignment=0.5)
;fn = respath+'o3p_vs_cloud.png'
;subtitle1 = text(0.25,posi1[3]+0.01,'Trop column vs cfrac',font_size=20,alignment=0.5)
;opl = plot(data.co3[orbitind2,2],xstyle=1,ystyle=0,thick=2,axis_style=4,posi=posi1,color='black',/current)
;cpl = plot(data.cfrac[orbitind2],xstyle=1,ystyle=0,thick=2,axis_style=4,posi=posi1,color='deep_sky_blue',/current)

;v2_yaxis = axis('y',location='right',color='black',target=cpl,title='Cloud Fraction')

;ax1 = plot(data.co3[orbitind2,2],xstyle=1,ystyle=0,posi=posi1, $
          ;ytitle='DU',thick=2,axis_style=1,/current,/nodata)
;cpl.font_size=20
;ax1.font_size=20
;;ax1.title.font_size=30
;ax1.axes[0].show=0


;posi2=[0.15,0.45,0.85,0.65]
;subtitle2 = text(0.25,posi2[3]+0.01,'O3 profile vs CTP',font_size=20,alignment=0.5)
;ncolumn=24
;ct = colortable(72,/reverse)

;xx = indgen(size(data.o3[orbitind2,*],/dim))
;for i=0,ncolumn-1 do xx[*,i]=indgen(size(data.o3[orbitind2,0],/dim))

;copypres = exp((alog(data.pres[*,1:*]) + alog(data.pres[*,0:-1]))*0.5)
;yrange=[1000,0]
;opl2 = contour(data.o3[orbitind2,*],xx,copypres[orbitind2,*],max_value=50,min_value=0,pos=posi2 $
               ;,xstyle=1,yrange=[1000,0],axis_style=4,/fill,rgb_table=ct,n_levels=21,/current)
;cpl2 = plot(data.ctp[orbitind2],yrange=yrange,xstyle=1,ystyle=0,thick=2,axis_style=4,posi=posi2,color='royal_blue' $
            ;,background_transparency=100,/overplot)

;v2_yaxis2 = axis('y',location='right',color='black',target=cpl2,title='Cloud top pressure')
;ax2 = plot(data.co3[orbitind2,2],yrange=yrange,xstyle=1,xtitle='pixel along track',posi=posi2, $
          ;ytitle='Pressure',axis_style=1,/current,/nodata)
;cpl2.font_size=20
;ax2.font_size=20
;ax2.axes[0].show=0

;posi3=[0.15,0.2,0.85,0.4]
;subtitle3 = text(0.25,posi3[3]+0.01,'A priori O3 profile vs CTP',font_size=20,alignment=0.5)
;ncolumn=24
;level = [-999,findgen(21)*2.5,1000]

;xx = indgen(size(data.o3[orbitind2,*],/dim))
;for i=0,ncolumn-1 do xx[*,i]=indgen(size(data.o3[orbitind2,0],/dim))

;copypres = exp((alog(data.pres[*,1:*]) + alog(data.pres[*,0:-1]))*0.5)
;yrange=[1000,1]
;opl3 = contour(data.ao3[orbitind2,*],xx,copypres[orbitind2,*],max_value=50,min_value=0,pos=posi3 $
               ;,xstyle=1,yrange=[1000,0],axis_style=4,/fill,rgb_table=ct,n_levels=21,/current)
;cpl3 = plot(data.ctp[orbitind2],yrange=yrange,xstyle=1,ystyle=0,thick=2,axis_style=4,posi=posi3,color='royal_blue' $
            ;,background_transparency=100,/overplot)

;v2_yaxis3 = axis('y',location='right',color='black',target=cpl3,title='Cloud top pressure')
;ax3 = plot(data.co3[orbitind2,2],yrange=yrange,xstyle=1,xtitle='pixel along track',posi=posi3 $
           ;,ytitle='Pressure',axis_style=1,/current,/nodata)
;c = colorbar(target=opl3,title='DU',position=[0.2,0.06,0.8,0.09])

;cpl3.font_size=20
;ax3.font_size=20
;c.font_size=15
;c.font_size=15

;win.save,fn
;hs_filetrans,fn

;stop

endfor
stop
end
