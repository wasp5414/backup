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


pro hs_gems_checkarea_ver8,loop=loop,xdrmod=xdrmod,which_plot=which_plot $
                          ,which_column=which_column

;+---------------------------------------------------------------------------+
; RUN : hs_gems_checkarea_ver8,xdrmod=1
; in first1 run, xdrmod must be 0
;
; INPUT : options
; OUTPUT: GEMS CTP & CO3 field 
;
; READ GEMS AND MODIS COLLOCATION DATA, THEN PLOT ON MAP DEPENDING ON 
; "WHICH_PLOT" OPTION
; DIFFERENCE OF DATA, GEMS vs MODIS 
; 
; *** LINE PLOT, WITH LATITUDE. NEED TO REGRID DATA. NOT FINISHED ***
;
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
ftypes=[]
fdates=[] & forbits=[]
gems_files=file_search(outpath+'GEM_*20050601*')
fnum=n_elements(gems_files)
ALLeqind=[] & ALLeqco3=[] & ALLadjco3=[]

nlev=51

limits = { $
 wide2:[-35,-155,25,-115],north:[5,-155,20,-125],south:[-25,-160,-5,-125] $
,wide :[-35,-175,25,-135]  $
         }

levs = { $
total1 :fix(findgen(nlev)/(nlev-1)*70 +240), $
total2 :fix(findgen(nlev)/(nlev-1)*30 +240), $
total3 :fix(findgen(nlev)/(nlev-1)*20 +240), $
strat  :fix(findgen(nlev)/(nlev-1)*20 +220), $
trop   :fix(findgen(nlev)/(nlev-1)*10 +20 ), $
cfrac  :findgen(nlev)/(nlev-1)             , $
ctp    :fix(findgen(nlev)/(nlev-1)*900+100), $
to3diff:fix(findgen(nlev)/(nlev-1)*20 -10 ), $
trodiff:fix(findgen(nlev)/(nlev-1)*10 -5  ), $
strdiff:fix(findgen(nlev)/(nlev-1)*10 -5 ) , $
ctpdiff:fix(findgen(nlev)/(nlev-1)*600-300)  }


formats=['(i3)','(i3)','(i3)','(i4)','(i3)','(i3)','(i3)','(i7)']
titles=['Difference of Column O3 between adj and original' $
       ,'Difference of Stratosphere O3 between adj and original' $
       ,'Difference of Troposphere O3 between adj and original' $
       ,'Difference of Cloud top pressure between MYD06 and CLDO2' $
       ,'Stratosphere column O3 Control to 200' $
       ,'Troposphere column O3 Control to 200']

figtags     = ['to3diff_wide','strdiff_wide','trodiff_wide'  , $
               'ctpdiff_wide']
layertags   = ['tc','stc','ttc','ctp']

levlist     = ['to3diff','strdiff','trodiff' $
              ,'ctpdiff']
datalist    = ['diff_gems', 'diff_gems','diff_gems', $
               'diff_ctp']
columns     = [0,1,2,0]

coltables   = [72,72,72,72]
revcol      = [0,0,0,0]
limitlist   = ['wide','wide','wide','wide']
badpixcols  = ['Gray','Gray','Gray','Gray']


void=execute('lev=levs.'+levlist[which_plot])
void=execute('limit=limits.'+limitlist[which_plot])
which_column=columns[which_plot]
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
  bad   =where(res.cf lt 0,bpix)

;+---------------------------------------------------------------------------+
; SELECT CTP GROUP
;+---------------------------------------------------------------------------+

  adj_gems  = adjust2control(bad2nan(res.gems.co3), $
                             bad2nan(res.ctp)     ,which_column=which_column)

  diff_gems = adj_gems - gems_origin.co3[which_column]

  diff_ctp  = bad2nan(res.ctp)  - gems_origin.ctp

  
  ;cfrac_mask = where(abs(bad2nan(res.gems.cfrac) - bad2nan(res.cf)) lt 0.3 $
                     ;and res.gems.cfrac gt 0.3)

  cfrac_mask = where(res.gems.cfrac gt 0.4)

  eqind=where( res.gems.lat lt -3 and res.gems.lat gt -5, nind)
  adjco3=adj_gems[eqind]
  eqco3=res.gems[eqind].co3[0]

  ALLeqind=[ALLeqind,eqind]
  ALLadjco3=[ALLadjco3,adjco3]
  ALLeqco3=[ALLeqco3,eqco3]
  first=0

endfor
;+---------------------------------------------------------------------------+
; PLOT DATA ON MAP 
;+---------------------------------------------------------------------------+

void=execute('data = '+datalist[which_plot] )

print,'NUMBER OF COLLOCATED PIXEL : ', n_elements(where(res.cf ge 0))

prefix=respath+orbit+'+'+figtags[which_plot]
;prefix=respath+orbit+'+'+type+'+'+figtags[which_plot]
ps=prefix+'.ps'
png=prefix+'.png'
cgps_open,ps,xsize=10,ysize=9,/nomatch,/inch,xoffset=0.5,yoffset=0.5
;window,0,xs=1000,ys=1000
pos=[0.05,0.05,0.81,0.90]

xrange=[0,nind] & yrange=[230,270]

loadct_wb, coltables[which_plot]
TVLCT, CGColor('BLACK', /Triple), 255 ; Drawing color.
TVLCT, CGColor('WHITE', /Triple), 254 ; Background color.
TVLCT, CGColor(badpixcol,/Triple),251 ; Badpix color
plot,ALLeqco3,yrange=yrange,xrange=xrange,posi=pos,/nodata

oplot,ALLeqco3
oplot,ALLadjco3,col=251

if last then begin

; SAVE FIGURE
  cgps_close,density=800,/png
  hs_filetrans,png
endif

PRINT,''
PRINT,'PROCEDURE END. VARIABLES SAVED  :: ',datalist[which_plot]
PRINT,''
  if not loop then stop
end
