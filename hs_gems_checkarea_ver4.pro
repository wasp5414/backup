pro hs_gems_checkarea_ver4,loop=loop,xdrmod=xdrmod,which_plot=which_plot,which_layer=which_layer

;+---------------------------------------------------------------------------+
; RUN : hs_gems_checkarea_ver4,xdrmod=1
; in first1 run, xdrmod must be 0
;
; INPUT : options
; OUTPUT: GEMS CTP & CO3 field 
;
; READ GEMS AND MODIS COLLOCATION DATA, THEN PLOT ON MAP DEPENDING ON 
; "WHICH_PLOT" OPTION
;
;+---------------------------------------------------------------------------+

if not keyword_set(xdrmod) then xdrmod=0
if not keyword_set(loop) then loop=0 ; stop at first loop
if not keyword_set(which_plot) then which_plot=0 

; 0 : Comparing ORIGINAL with MODIS

if not keyword_set(which_layer) then which_layer=0
;+---------------------------------------------------------------------------+
; PLOT SETTING 
;+---------------------------------------------------------------------------+

unit_of_o3 = '(DU)' ; '(ppb)'
respath   ='/home/o3p_hs/results/'
;outpath   ='/home/o3p_hs/GEMS/o3p/dat/out/backup/2005/' ; for test
outpath   ='/home/o3p_hs/GEMS/o3p/dat/out/'
pixpath   ='/home/Data/OMI/2_OML2PIXCOR/2005/'
myd06path = '/home/Data2/5_MODIS/MYD06/'
myd03path = '/home/Data2/5_MODIS/MYD06/'
xdrpath   = '/home/o3p_hs/data/xdr/'


which_day=0 & which_orbit=0
ftypes   = []
fdates   = [] & forbits=[]
gems_files  = file_search(outpath+'GEM_*')
fnum=n_elements(gems_files)

nlev=51

limits = { $
 wide2:[-35,-155,25,-115],north:[5,-155,20,-125],south:[-25,-160,-5,-125] $
,wide :[-35,-175,25,-135]  $
         }

levs = { $
total1 :fix(findgen(nlev)/(nlev-1)*70 +240), $
total2 :fix(findgen(nlev)/(nlev-1)*30 +240), $
strat  :fix(findgen(nlev)/(nlev-1)*30 +220), $
trop   :fix(findgen(nlev)/(nlev-1)*40 +10 ), $
cfrac  :findgen(nlev)/(nlev-1)             , $
ctp    :fix(findgen(nlev)/(nlev-1)*900+100) }


coltables = [72,49,72,49,72,72,26]
formats=['(i3)','(f4.1)','(i4)','(f4.1)','(i4)']
titles=['Total column O3','Cloud faction','Cloud top pressure',$
        'GEMS cloud fraction','GEMS cloud top pressure''Total column O3']
figtags=['to3_wide','myd06cf_wide','myd06ctp_wide','cfrac_wide',$
         'ctp_wide','to3_wide','to3_north']
badpixcols=['Gray','Red','Gray','Red','Gray','Black','Gray']
layertags=['tc','0','0','0','0','st','tp']
limitlist=['wide','wide','wide','wide','wide','north','south']
levlist=['total2','cfrac','ctp','cfrac','ctp','total2','total2']
datalist=['gems_origin.co3[0,*]','res.cf','res.ctp','res.gems.cfrac', $
          'res.gems.ctp','res.gems.co3[0,*]','res.gems.co3[0,*]']
revcol=[0,0,0,0,0,0,0,0]

void=execute('lev=levs.'+levlist[which_plot])
void=execute('limit=limits.'+limitlist[which_plot])
layertag=[which_plot]
title=titles[which_plot]
figtag=figtags[which_plot]
;title=titles[0]
;figtag=figtags[0]
badpixcol=badpixcols[which_plot]
format=formats[which_plot]
if revcol[which_plot] then $
barcol=fix(findgen(nlev-1)*250/(nlev-2)) else $
barcol=reverse(fix(findgen(nlev-1)*250/(nlev-2)))


;+---------------------------------------------------------------------------+
;  LOOP OF READING OMI PIXCOR FILE
;+---------------------------------------------------------------------------+

for i=0,fnum-1 do begin

  fdates  = [fdates,strmid((strsplit(gems_files,'_',/ext))[i,6],0,8)]
  forbits = [forbits,(strsplit(gems_files,'_',/ext))[i,8]]
  type  = (strsplit(gems_files[i],'/',/ext))[-1]
  type  = (strsplit(type,'.',/ext))[0]
  type  = (strsplit(type,'_',/ext))[1]
  ftypes = [ftypes,type]
  orbits= forbits[uniq(forbits,sort(forbits))]

endfor

dates   = fdates[uniq(fdates,sort(fdates))]
ndates  = n_elements(dates)
ntypes  = n_elements(ftypes[uniq(ftypes)])

;which_file=where(forbits eq orbits[which_orbit])

;files=gems_files[which_file]
;ftypes=types
;types=ftypes[which_file]

first=1 & last=0
for i=0,n_elements(orbits)-1 do begin

  if i eq n_elements(orbits)-1 then last=1

  orbit=orbits[i]
  files=gems_files[where(forbits eq orbit)]
  types=ftypes[where(forbits eq orbit)]

  pixcor_file=file_search(pixpath+'*'+'o'+orbit+'*')
  hs_omi_pixcor,pixcor_file,pixcor,xdrmod=xdrmod

;+---------------------------------------------------------------------------+
; READ GEMS CONTROL RUN DATA 
;+---------------------------------------------------------------------------+

  for j=0,n_elements(files)-1 do begin

    type=types[j]
    str='gems_'+type
    void=execute('hs_read_gems,files[j],'+str+',xdrmod=xdrmod')
    void=execute(str+'.o3=bad2nan('+str+'.o3)')
    void=execute(str+'.co3=bad2nan('+str+'.co3)')
    void=execute(str+'.pres=bad2nan('+str+'.pres)')
    void=execute(str+'.ao3=bad2nan('+str+'.ao3)')
    void=execute(str+'.ao3e=bad2nan('+str+'.ao3e)')
    void=execute(str+'.ctp=bad2nan('+str+'.ctp)')
    void=execute(str+'.cfrac=bad2nan('+str+'.cfrac)')
    void=execute(str+'[where('+str+'.cfrac lt 0)].cfrac=0')
    void=execute(str+'.day=bad2nan('+str+'.day)')
    void=execute(str+'.sza=bad2nan('+str+'.sza)')
    void=execute(str+'.alt=bad2nan('+str+'.alt)')

  endfor

  pix=gems_origin.pix & line=gems_origin.line


;+---------------------------------------------------------------------------+
; NAN VALUE FILTERING & COLLOCATION FILE RESTORE 
;+---------------------------------------------------------------------------+

  colfn='final_col_modis_omi_'+orbits[i]+'.xdr'
  restore,xdrpath+colfn
  notbad=where(res.gems.co3[0] ge 0,npix)
  bad   =where(res.gems.co3[0] lt 0,bpix)

;+---------------------------------------------------------------------------+
; PLOT DATA ON MAP 
;+---------------------------------------------------------------------------+

  void=execute('data = '+datalist[which_plot] )

  print,'NUMBER OF COLLOCATED PIXEL : ', n_elements(where(res.cf ge 0))

  if first then begin
    prefix=respath+orbit+'+'+figtags[which_plot]
    ;prefix=respath+orbit+'+'+type+'+'+figtags[which_plot]
    ps=prefix+'.ps'
    png=prefix+'.png'
    cgps_open,ps,xsize=10,ysize=9,/nomatch,/inch,xoffset=0.5,yoffset=0.5
    ;window,0,xs=1000,ys=1000
    pos=[0.05,0.05,0.81,0.90]

    loadct_wb, coltables[which_plot]
    TVLCT, CGColor('BLACK', /Triple), 255 ; Drawing color.
    TVLCT, CGColor('WHITE', /Triple), 254 ; Background color.
    TVLCT, CGColor(badpixcol,/Triple),251 ; Badpix color
    !p.color=255

    ;MAP_SET, fix((limit[0]+limit[2])/2), fix((limit[1]+limit[3])/2), $
    ;limit=limit, /mercator, latdel=10, londel=10, position=pos
    MAP_SET, fix((limit[0]+limit[2])/2), fix((limit[1]+limit[3])/2), $
    /satellite,scale=4e07, latdel=10, londel=10, position=pos
  endif

;Data polyfill 
  for p=0,npix-1 do begin

    ipix=notbad[p]
    clon=reform(pixcor.clon[pix[ipix]-1,line[ipix]-1,*])
    clat=reform(pixcor.clat[pix[ipix]-1,line[ipix]-1,*])

    for k=0,nlev-2 do begin

      if (data[ipix] lt lev[0]) then col=barcol[0]
      if (data[ipix] ge lev[k] and data[ipix] lt lev[k+1]) then col=barcol[k]
      if (data[ipix] ge lev[-1]) then col=barcol[-1]

    endfor

    if ( (data[ipix] eq -999.) or (finite(data[ipix]) eq 0) ) then col=251
    polyfill,[clon,clon[0]],[clat,clat[0]],color=col,noclip=0

  endfor

;badpixel polyfill
  ;for p=0,bpix-1 do begin
    
    ;ipix=bad[p]
    ;clon=reform(pixcor.clon[pix[ipix]-1,line[ipix]-1,*])
    ;clat=reform(pixcor.clat[pix[ipix]-1,line[ipix]-1,*])

    ;;print, data[ipix], col
    ;polyfill,[clon,clon[0]],[clat,clat[0]],color=251,noclip=0

  ;endfor

  if last then begin
    colorbar_2,lev,barcol,/col,ys=0.77,xs=0.015,charsize=1.9, $
    charthick=2,levelind=findgen(100)*5,$
    format=formats[which_plot],lowleft=[0.87,0.05],/nofirst,/right

    map_continents,/continent,/count,MLINETHICK=1.8,/country,/hires
    map_grid,latdel=10,londel=10,thick=0.7,charsize=1
    xyouts,mean([pos[0],pos[2]]),pos[3]+0.04,align=0.5,title,$
    charsize=2,/normal

  ; SAVE FIGURE
    cgps_close,density=800,/png
    hs_filetrans,png
  endif

  if not loop then stop
  first=0
endfor
PRINT,''
PRINT,'PROCEDURE END. VARIABLES SAVED'
PRINT,''
stop
end
