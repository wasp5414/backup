pro hs_omi_cldo2,xdrmod=xdrmod,which_avgk=which_avgk,which_plot=which_plot

;+---------------------------------------------------------------------------+
; RUN : hs_omi_cldo2,xdrmod=1
; in first1 run, xdrmod must be 0
;
; INPUT : 
; OUTPUT: 
;
; - read OMICLDO2 file and draw contour on map
;
;+---------------------------------------------------------------------------+

;xdrmod=1
if not keyword_set(xdrmod) then xdrmod=0
if not keyword_set(which_avgk) then which_avgk=0 ; 0: original 1: normalized
if not keyword_set(which_plot) then which_plot=0 ; 0: omicldo2 cfrac 1: omicldo2 ctp



; #1. Date setting
respath ='/home/o3p_hs/results/'
cldo2path ='/home/Data/OMI/2_OML2CLDO2_new/2005/'
pixpath ='/home/Data/OMI/2_OML2PIXCOR/2005/'
to3path ='/home/Data/OMI/2_OML2TO/2005/'
intymd = 20050601 + indgen(14)
ymdlist=strtrim(intymd,2)
;ymdlist=['20050620','20050625']
nymd=n_elements(ymdlist)




; #2. Loop for each date
for iymd=0,nymd-1 do begin
  ymd     = ymdlist[iymd]
  ymdpath = cldo2path+'*'+strmid(ymd,0,4)+'m'+strmid(ymd,4,4)
  filelist =  [file_search(ymdpath+'t21*') $
              ,file_search(ymdpath+'t22*') $
              ;,file_search(ymdpath+'t20*') $
              ,file_search(ymdpath+'t23*')]
  filelist = filelist[where((filelist eq '') eq 0)]

  fnum=n_elements(filelist)

  wanna_stop  = 1
  ;which_plot  = 0
  titles= ['OMICLDO2 orbit cfrac check','OMICLDO2 orbit ctp check']  ;14
  title=titles[which_plot]
  figtags=['omio2cfrac','omio2ctp']   ;14

 

; #3. Ready for plot pixel
  ;prefix=respath+orbit+'+'+'+'+figtags[which_plot]
  prefix=respath+ymdlist[iymd]+'+'+figtags[which_plot]
  ps=prefix+'.ps'
  png=prefix+'.png'
  cgps_open,ps,xsize=10,ysize=7,/nomatch,/inch,xoffset=0.5,yoffset=0.5
  ;window,0,xs=1000,ys=1000
  pos=[0.05,0.05,0.81,0.85]
  
  coltables = [22,22]
  loadct_wb, coltables[which_plot]
  TVLCT, CGColor('BLACK', /Triple), 255 ; Drawing color.
  TVLCT, CGColor('WHITE', /Triple), 254 ; Background color.
  !p.color=255

  wide  = [-25,170,25,-140]
  north = [-5,-165,25,-130]
  south = [-25,-160,-5,-125]
  which_limit=['wide','wide']
  void = execute('limit='+which_limit[which_plot])
  
  ;MAP_SET,fix((limit[0]+limit[2])/2),fix((limit[1]+limit[3])/2), $
          ;limit=limit,/ortho,latdel=10,londel=10,position=pos,standard_parallels=[20,60]
  ;MAP_SET,0,-150, $
          ;scale=5e07,/satellite,latdel=10,londel=10,position=pos,standard_parallels=[20,60]

  MAP_SET,/mercator,0,0,/isotropic,/grid,/continents
  nlev=51
  ;barcol  =fix(findgen(nlev-1)*250/(nlev-2))
  barcol  = fix(findgen(nlev-1)*250/(nlev-2))

  totallev=fix(findgen(nlev)/(nlev-1)*50  + 230)
  totallev2=fix(findgen(nlev)/(nlev-1)*30  + 230)
  totallev3=fix(findgen(nlev)/(nlev-1)*40  + 240)

  strlev  =fix(findgen(nlev)/(nlev-1)*40 + 210)
  strlev2 =fix(findgen(nlev)/(nlev-1)*40 + 210)

  troplev   = fix(findgen(nlev)/(nlev-1)*40 + 10)
  cfraclev  = findgen(nlev)/(nlev-1) 
  ctplev    = reverse(findgen(nlev)/(nlev-1)*800 + 200)
  tplev     = findgen(nlev)/(nlev-1)*300 + 100
  reverse_flag=[0,1]

  badpixcols = ['Black','Black']
  TVLCT, CGColor(badpixcols[which_plot],/Triple), 251 ;bad pixel


; #4. Loop for reading the file
  for ifile=0,fnum-1 do begin   
    orbit=strmid(filelist[ifile],strpos(filelist[ifile],'o',/reverse_search)+1,5)
    hs_read_omicldo2,filelist[ifile],cldo2,xdrmod=xdrmod

    pixcor_file   = file_search(pixpath+'*'+'o'+orbit+'*')
    hs_omi_pixcor,pixcor_file,pixcor,xdrmod=xdrmod,type=1

    to3_file      = file_search(to3path+'*'+'o'+orbit+'*')
    hs_read_omto3,to3_file,to3,xdrmod=xdrmod

    lat  =bad2nan(cldo2.lat)  & lon  =bad2nan(cldo2.lon)
    cfrac=bad2nan(cldo2.cfrac) & ctp = bad2nan(cldo2.ctp)
    clons=pixcor.clon & clats=pixcor.clat
    lons=pixcor.lon   & lats=pixcor.lat
    o2flg=cldo2.flg     & to3flg=to3.flg 

    nline = (size(cfrac))[1]
    npix  = n_elements(cfrac)
    levs = ['cfraclev','ctplev']
    void = execute('lev=' + levs[which_plot])
    datalist = ['cfrac','ctp']
    void = execute('data=' + datalist[which_plot])
    
; #5. Loop for drawing pixel on map
    for j=0,npix-1 do begin
      pix=(array_indices(cfrac,j))[0]+1 & line=(array_indices(cfrac,j))[1]+1
      clon=reform(clons[pix-1,line-1,*])
      clat=reform(clats[pix-1,line-1,*])

      for k=0,nlev-2 do begin
        if reverse_flag[which_plot] then begin
          if (data[j] lt lev[k] and data[j] ge lev[k+1]) then col=barcol[k]
          if (data[j] ge lev[0]) then col=barcol[0]
          if (data[j] lt lev[-1]) then col=barcol[-1]
        endif else begin
          if (data[j] ge lev[k] and data[j] lt lev[k+1]) then col=barcol[k]
          if (data[j] lt lev[0]) then col=barcol[0]
          if (data[j] ge lev[-1]) then col=barcol[-1]
        endelse
      endfor
      ;if (min(lat[*,line-1]) gt 15 or max(lat[*,line-1]) lt -30) then col=251
      print,to3flg[pix-1,line-1,-1]
      if ( not to3flg[pix-1,line-1,-1] eq 0 ) then print,'hello';col=251
      if ( not finite(data[j]) ) then col=251 
      polyfill,[clon,clon[0]],[clat,clat[0]],color=col,noclip=0

    endfor
    
    formats = ['(f4.2)','(i3)']

  endfor



; #6. Plot map background
  colorbar_2,lev,barcol,/col,ys=0.77,xs=0.015,charsize=1.9,charthick=2,levelind=findgen(100)*5,$
             format=formats[which_plot],lowleft=[0.87,0.05],/nofirst,/right

  map_continents,/continent,/count,MLINETHICK=1.8,/country,/hires
  map_grid,latdel=10,londel=10,thick=0.7,charsize=1
  xyouts,mean([pos[0],pos[2]]),pos[3]+0.07,align=0.5,title,charsize=2,/normal
  cgps_close,density=800,/png
  hs_filetrans,png
  if wanna_stop then stop  
endfor
end
