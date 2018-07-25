function adjust2control,gems,modis,which_column=which_column
  
  common share,gems_200,gems_300,gems_400,gems_500,gems_700 $
              ,gems_900,gems_spres

  group200=where(modis LT 250)
  group300=where(modis LT 450 and modis GT 250)
  group400=where(modis LT 550 and modis GT 450)
  group500=where(modis LT 600 and modis GT 550)
  group700=where(modis LT 800 and modis GT 600)
  group900=where(modis LT 950 and modis GT 800)
  groupSFC=where(modis GT 950)
  
  gems=reform(gems[which_column,*,*])

  gems[group200]=gems_200[group200].co3[which_column]
  gems[group300]=gems_300[group300].co3[which_column]
  gems[group400]=gems_400[group400].co3[which_column]
  gems[group500]=gems_500[group500].co3[which_column]
  gems[group700]=gems_700[group700].co3[which_column]
  gems[group900]=gems_900[group900].co3[which_column]
  gems[groupSFC]=gems_spres[groupSFC].co3[which_column]

  return,gems

end


pro hs_gems_checkarea_ver9,loop=loop,xdrmod=xdrmod,which_plot=which_plot $
                          ,which_column=which_column

;+---------------------------------------------------------------------------+
; RUN : hs_gems_checkarea_ver4,xdrmod=1
; in first1 run, xdrmod must be 0
;
; INPUT : options
; OUTPUT: GEMS CTP & CO3 field 
;
; - ANALYSIS TO COMPARE COLUMN OZONE TO CLOUD TOP PRESSURE
; - GROUP HIGH CLOUD FRACTION AREA AND CLEAR AREA 
; - CHECK WHICH AREA IS HIGHER COLUMN O3 AREA.
; - NOT MAP PLOT ! 
;+---------------------------------------------------------------------------+

if not keyword_set(xdrmod) then xdrmod=0
if not keyword_set(loop) then loop=0 ; stop at first loop
if not keyword_set(which_plot) then which_plot=0 

; 0 : Comparing ORIGINAL with MODIS

if not keyword_set(which_column) then which_column=0
;+---------------------------------------------------------------------------+
; PLOT SETTING 
;+---------------------------------------------------------------------------+

unit_of_o3 = '(DU)' ; '(ppb)'
respath   ='/home/o3p_hs/results/'
;outpath   ='/home/o3p_hs/GEMS/o3p/dat/out/backup/2005/' ; for test
outpath   ='/home/o3p_hs/GEMS/o3p/dat/out/20180525/'
pixpath   ='/home/Data/OMI/2_OML2PIXCOR/2005/'
myd06path = '/home/Data2/5_MODIS/MYD06/'
myd03path = '/home/Data2/5_MODIS/MYD06/'
xdrpath   = '/home/o3p_hs/data/xdr/'


which_day=0 & which_orbit=0
ftypes   = []
fdates   = [] & forbits=[]
group1 = [] & group2 = [] & group3 = [] & group4 = [] 
cfrac1 = [] & cfrac2 = [] & cfrac3 = [] & cfrac4 = []
ntarget1=0
ntarget2=0

gems_files  = file_search(outpath+'GEM_*20050601*')
fnum=n_elements(gems_files)

formats=['(i3)','(i3)','(i3)','(i4)','(i3)','(i3)','(i3)','(i7)']
titles=['test title1' $
       ,'test title2' $
       ,'test title3']

figtags     = ['to3diff_wide','strdiff_wide','trodiff_wide'  , $
               'ctpdiff_wide']
columns     = [0,1,2,0]

coltables   = [72,72,72,72]


which_column=columns[which_plot]
title=titles[which_plot]
figtag=figtags[which_plot]
format=formats[which_plot]

;barcol=fix(findgen(nlev-1)*250/(nlev-2)) 
;barcol=reverse(fix(findgen(nlev-1)*250/(nlev-2)))

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


first=1 & last=0
for i=0,n_elements(orbits)-1 do begin

  if i eq n_elements(orbits)-1 then last=1

  orbit=orbits[i]
  files=gems_files[where(forbits eq orbit)]
  types=ftypes[where(forbits eq orbit)]

;+---------------------------------------------------------------------------+
; READ GEMS CONTROL RUN DATA 
;+---------------------------------------------------------------------------+

  common share,gems_200,gems_300,gems_400,gems_500,gems_700 $
              ,gems_900,gems_spres

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
    ;void=execute(str+'.day=bad2nan('+str+'.day)')
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
  bad   =where(res.cf lt 0,bpix)

;+---------------------------------------------------------------------------+
; SELECT CTP GROUP
;+---------------------------------------------------------------------------+

  adj_gems  = adjust2control(bad2nan(res.gems.co3), $
                             bad2nan(res.ctp)     ,which_column=which_column)

  diff_gems = adj_gems - gems_origin.co3[which_column]

  Diff_ctp  = bad2nan(res.ctp)  - gems_origin.ctp

  cfrac_mask = where(res.gems.cfrac gt 0.4)


  
  print,'NUMBER OF COLLOCATED PIXEL : ', n_elements(where(res.cf ge 0))

  target1 = [-180,-140,-20,20]
  target2 = [-180,-140,-20,20]
  
  lat = res.gems.lat & lon=res.gems.lon
  stop

  ;convective cloudy-GEMS
  ind1=where(lat gt target1[0] and lon lt target1[1] and $
             lat gt target1[2] and lat gt target1[3] $
             ;and res.gems.cfrac gt 0.4,nwhere)
             and res.gems.ctp lt 300 and res.gems.cfrac gt 0.4,nwhere)
  ntarget1=ntarget1+nwhere

  ;clear-GEMS
  ind2=where(lat gt target1[0] and lon lt target1[1] and $
             lat gt target1[2] and lat gt target1[3] $
             and bad2nan(res.gems.cfrac) lt 0.3,nwhere)
  ntarget1=ntarget1+nwhere

  ;convective cloudy-MODIS
  ind3=where(lat gt target1[0] and lon lt target1[1] and $
             lat gt target1[2] and lat gt target1[3] $
             and res.ctp lt 300 and res.cf gt 0.4,nwhere)
  ntarget1=ntarget1+nwhere

  ;clear-MODIS
  ind4=where(lat gt target1[0] and lon lt target1[1] and $
             lat gt target1[2] and lat gt target1[3] $
             and bad2nan(res.cf) lt 0.3,nwhere)
  ntarget1=ntarget1+nwhere

  print,'NUMBER OF TARGET AREA PIXEL : ',ntarget1

  if ntarget1 gt 0 then begin

  group1 = [group1,res.gems[ind3].co3[which_column]]
  group2 = [group2,res.gems[ind4].co3[which_column]]
  group3 = [group3,adj_gems[ind3] ]
  group4 = [group4,adj_gems[ind4] ]
  endif

endfor

print, 'AVG OF GROUP1 :: ',mean(group1)
print, 'AVG OF GROUP2 :: ',mean(group2)
print, 'AVG OF GROUP3 :: ',mean(group3)
print, 'AVG OF GROUP4 :: ',mean(group4)

pres_mid  = 0.5*(presdat[0:nlayer-1,p]+presdat[1:nlayer,p]) 
alt_mid = 0.5*(altdat[0:nlayer-1,p]+altdat[1:nlayer,p])

if first then begin
  prefix=respath+orbit+'+'+figtags[which_plot]
  ;prefix=respath+orbit+'+'+type+'+'+figtags[which_plot]
  ps=prefix+'.ps'
  png=prefix+'.png'
  ;cgps_open,ps,xsize=10,ysize=9,/nomatch,/inch,xoffset=0.5,yoffset=0.5
  window,0,xs=1000,ys=1000
  pos=[0.05,0.05,0.95,0.90]

  loadct_wb, coltables[which_plot]

endif


if last then begin
  ;colorbar_2,lev,barcol,/col,ys=0.77,xs=0.015,charsize=1.9, $
  ;charthick=2,levelind=findgen(100)*5,$
  ;format=formats[which_plot],lowleft=[0.87,0.05],/nofirst,/right
  xax=findgen(0,50)
  yax=findgen(0,25)
  plot,xax,yax ,/nodata

  xyouts,mean([pos[0],pos[2]]),pos[3]+0.04,align=0.5,title,$
  charsize=1.5,/normal

;SAVE FIGURE
  ;cgps_close,density=800,/png
  ;hs_filetrans,png
endif

first=0

PRINT,''
PRINT,'PROCEDURE END. VARIABLES SAVED  :: '
PRINT,''

  if not loop then stop
end
