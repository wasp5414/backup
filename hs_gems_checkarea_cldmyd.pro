pro hs_gems_checkarea_cldmyd,loop=loop,xdrmod=xdrmod,which_plot=which_plot,control=control

;+---------------------------------------------------------------------------+
; RUN : hs_gems_checkarea_ver10,xdrmod=0,which_plot=2,loop=1
;
; INPUT : options
; OUTPUT: GEMS CTP & CO3 field 
;
; read "CLDMYD" type GEMS result and plot on map
; 
;+---------------------------------------------------------------------------+

if not keyword_set(xdrmod) then xdrmod=0
if not keyword_set(loop) then loop=0 ; stop at first loop
if not keyword_set(which_plot) then which_plot=0 
if not keyword_set(control) then control='200'
; 0 : Comparing ORIGINAL with MODIS

;+---------------------------------------------------------------------------+
; PLOT SETTING 
;+---------------------------------------------------------------------------+

unit_of_o3 = '(DU)' ; '(ppb)'
respath   ='/home/o3p_hs/results/'
outpath   ='/home/o3p_hs/GEMS/o3p/dat/out/20180717/'
pixpath   ='/home/Data/OMI/2_OML2PIXCOR/2005/'
myd06path = '/home/Data2/5_MODIS/MYD06/'
myd03path = '/home/Data2/5_MODIS/MYD06/'
xdrpath   = '/home/o3p_hs/data/xdr/'


ftypes=[] & ftimes=[]
fdates=[] & forbits=[]
cldmyd_to3=[] & origin_to3=[]
cldmyd_tto3=[] & origin_tto3=[]
cldmyd_sto3=[] & origin_sto3=[]
cldmyd_cfrac=[] & origin_cfrac=[]
cldmyd_ctp=[] & origin_ctp=[]
cldmyd_lon=[] & origin_lon=[]
cldmyd_lat=[] & origin_lat=[]
cldmyd_rms=[] & origin_rms=[]
cmodis_cot=[]
tmp=[]

gems_files  = file_search(outpath+'GEM*')


fnum=n_elements(gems_files)
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
ctp    :fix(findgen(nlev)/(nlev-1)*900+100) }


formats=['(i3)','(i3)','(i3)','(i4)','(f4.1)']

titles=['Total column O3 '$
       ,'Stratosphere column O3 ' $
       ,'Troposphere column O3 ' $
       ,'Cloud top pressure ' $
       ,'Cloud fraction ']

figtags=['to3_wide_mydinput_',  $
         'sto3_wide_mydinput_', $
         'tto3_wide_mydinput_', $
         'ctp_wide_', $
         'cfrac_wide_']

levlist =['total3','strat' ,'trop'  ,'ctp','cfrac']
datalist=['co3[0]','co3[1]','co3[2]','ctp','cfrac']

coltables = [72,72,72,72,49]
revcol=[0,0,0,0,0,0]
limitlist=['wide','wide','wide','wide','wide']
badpixcols=['Gray','Gray','Gray','Gray','Red']


void=execute('lev=levs.'    +levlist[which_plot])
void=execute('limit=limits.'+limitlist[which_plot])
title=titles[which_plot]
figtag=figtags[which_plot]
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
  time  = strmid((strsplit(gems_files[i],'_',/ext))[6],9,2)
  ftypes = [ftypes,type]
  ftimes = [ftimes,time]

endfor

orbits  = forbits[uniq(forbits,sort(forbits))]
dates   = fdates[uniq(fdates,sort(fdates))]
ndates  = n_elements(dates)
ntypes  = n_elements(ftypes[uniq(ftypes)])
types=['ORIGIN','CLDMYD']


for i=0,n_elements(orbits)-1 do begin

  orbit=orbits[i]

  for t=0,n_elements(types)-1 do begin

    type=types[t]
    
    files=gems_files[where(ftypes eq type)]
    pixcor_file=file_search(pixpath+'*'+'o'+orbit+'*')
    hs_omi_pixcor,pixcor_file,pixcor,xdrmod=xdrmod
    void=execute('hs_read_gems,files[i],gems_'+type+',xdrmod=xdrmod')

    void=execute('pix=gems_'+type+'.pix & line=gems_'+type+'.line')
    void=execute('notbad=where(gems_'+type+'.co3[0] gt 0 and gems_'+type+'.exval gt 0,npix)')
    ;bad   =where(gems_cldmyd.co3[0] lt 0,bpix)

    ;sample target area, only use in option 0,1
    lon_max=-170 & lon_min=-175 & lat_min=5 & lat_max=10

    if type eq 'ORIGIN' then begin
      O2indx=where(gems_origin.co3[0] gt -995)
      cotfn = xdrpath+'COT_o'+strtrim(orbits[i])+'.xdr'
      restore,cotfn
      cmodis_cot  =[cmodis_cot  ,mod2omi_cot[O2indx]]
      origin_to3  =[origin_to3  ,gems_origin[O2indx].co3[0]] 
      origin_tto3 =[origin_tto3 ,gems_origin[O2indx].co3[1]] 
      origin_sto3 =[origin_sto3 ,gems_origin[O2indx].co3[2]] 
      origin_cfrac=[origin_cfrac,gems_origin[O2indx].cfrac]
      origin_ctp  =[origin_ctp  ,gems_origin[O2indx].ctp]
      origin_lon  =[origin_lon  ,gems_origin[O2indx].lon]
      origin_lat  =[origin_lat  ,gems_origin[O2indx].lat]
      origin_rms  =[origin_rms  ,gems_origin[O2indx].rms]

    ;sample target area, only use in option 0,1
      theO2_idx=where(origin_lat gt lat_min and origin_lat lt lat_max and origin_lon gt lon_min and $
                      origin_lon lt lon_max)

      theO2_to3=origin_to3[theO2_idx]
      theO2_cfrac=origin_cfrac[theO2_idx]
      theO2_ctp=origin_ctp[theO2_idx]

    endif else begin 

      cldmyd_to3  =[cldmyd_to3  ,gems_cldmyd[O2indx].co3[0]] 
      cldmyd_tto3 =[cldmyd_tto3 ,gems_cldmyd[O2indx].co3[1]] 
      cldmyd_sto3 =[cldmyd_sto3 ,gems_cldmyd[O2indx].co3[2]] 
      cldmyd_cfrac=[cldmyd_cfrac,gems_cldmyd[O2indx].cfrac]
      cldmyd_ctp  =[cldmyd_ctp  ,gems_cldmyd[O2indx].ctp]
      cldmyd_lon  =[cldmyd_lon  ,gems_cldmyd[O2indx].lon]
      cldmyd_lat  =[cldmyd_lat  ,gems_cldmyd[O2indx].lat]
      cldmyd_rms  =[cldmyd_rms  ,gems_cldmyd[O2indx].rms]

    ;sample target area, only use in option 0,1
      themyd_idx=theO2_idx
      ;themyd_idx=where(cldmyd_lat gt lat_min and cldmyd_lat lt lat_max and cldmyd_lon gt lon_min and $ 
                       ;cldmyd_lon lt lon_max)
      themyd_to3=cldmyd_to3[themyd_idx]
      themyd_cfrac=cldmyd_cfrac[themyd_idx]
      themyd_ctp=cldmyd_ctp[themyd_idx]
    endelse
  endfor
  stop
endfor

for i=0,n_elements(orbits)-1 do begin
endfor 


;+---------------------------------------------------------------------------+
; SCATTERPLOT and correlation check
;+---------------------------------------------------------------------------+

option=3

; cfrac vs total ozone
;+---------------------------------------------------------------------------+
; cfrac vs total ozone
if option eq 0 then begin
cor_O2=correlate(theO2_cfrac,theo2_to3,/double)
cor_myd=correlate(themyd_cfrac,themyd_to3,/double)
ct=colortable(33)
ct[0,*]=[255,255,255]
margin=[0.15,0.15,0.15,0.1]

win=window(window_title='Dens_plots',dimensions=[1000,600])
props={current:1,rgb:ct,axis_style:2,margin:margin,font_size:16,$
       xtickdir:1,ytickdir:1,xshowtext:1,yshowtext:1}

;ymax=max(themyd_to3)+20
;ymin=min(themyd_to3)-10
ymax=310 & ymin=200

xrange1=1.-0 & yrange1=ymax-ymin
bin1=xrange1/100 & bin2=yrange1/100
xticks=string(findgen(11)*0.1,format='(f3.1)') & yticks=string(findgen(10)*10+220,format='(i3)')

h2d1=hist_2d(theO2_cfrac,theO2_to3,bin1=bin1,bin2=bin2,min2=ymin,min1=0,max2=ymax,max1=1)
pl1=image(h2d1,layout=[2,1,1],title='OMCLDO2 CTP input cloud fraction vs total ozone',_extra=props,max=100)
pl1.axes[2].major=0 & pl1.axes[2].minor=0
pl1.axes[3].major=0 & pl1.axes[3].minor=0
pl1.axes[0].tickname=xticks & pl1.axes[1].tickname=yticks

h2d2=hist_2d(themyd_cfrac,themyd_to3,bin1=bin1,bin2=bin2,min2=ymin,min1=0,max2=ymax,max1=1)
pl2=image(h2d2,layout=[2,1,2],title='MODIS CTP input cloud fraction vs total ozone',_extra=props,max=100)
pl2.axes[2].major=0 & pl2.axes[2].minor=0
pl2.axes[3].major=0 & pl2.axes[3].minor=0
pl2.axes[0].tickname=xticks & pl2.axes[1].tickname=yticks

cb=colorbar(target=pl,title='Number of pixel',position=[0.1,0.08,0.9,0.11],border_on=1,orientation=0,textpos=0,font_size=16)
txpos1=(pl1.position[0]+pl1.position[2])*0.5 & txpos2=(pl2.position[0]+pl2.position[2])*0.5
typos1=(pl1.position[3])*0.9                 & typos2=(pl2.position[3])*0.9
show_cor1=text(txpos1,typos1,'Correlation = '+strtrim(cor_O2,2),font_size=16,alignment=0.5)
show_cor2=text(txpos2,typos2,'Correlation = '+strtrim(cor_myd,2),font_size=16,alignment=0.5)

axpo1=txpos1-0.1 & aypo=(pl1.position[3])*0.4
show_lon=text(axpo1,aypo,'Longitude range : '+strtrim(lon_min,2)+', '+strtrim(lon_max,2),font_size=16,alignment=0 )
show_lat=text(axpo1,aypo-0.05,'Latitude range : '+strtrim(lat_min,2)+', '+strtrim(lat_max,2),font_size=16,alignment=0)

fn=respath+strtrim(abs(lon_max)+abs(lat_max))+'option0.png'
win.save,fn,resolution=100
win.close
hs_filetrans,fn
endif

;+---------------------------------------------------------------------------+
; cfrac1 vs cfrac2

if option eq 1 then begin
cor=correlate(theO2_cfrac,theo2_cfrac,/double)
ct=colortable(33)
ct[0,*]=[255,255,255]
margin=[0.15,0.15,0.15,0.1]

win=window(window_title='scat_plots',dimensions=[600,700])
props={current:1,margin:margin,font_size:16,xrange:[0,1],yrange:[0,1],$
       xshowtext:1,yshowtext:1,sym_color:'black',sym_filled:1,symbol:'dot'}
 
pl1=scatterplot(theO2_cfrac,theO2_cfrac,title='OMCLDO2 cloud fraction vs MODIS cloud fraction',_extra=props)
txpos1=(pl1.position[0]+pl1.position[2])*0.5 
typos1=(pl1.position[3])*0.9                 
show_cor1=text(txpos1,typos1,'Correlation = '+strtrim(cor,2),font_size=16,alignment=0.5)

fn=respath+'option1.png'
win.save,fn,resolution=100
win.close
hs_filetrans,fn
endif


;+---------------------------------------------------------------------------+
; with latitude & longitude
;
; #####  ADD REGRESS LINE ON PLOT!  ######
;
if option eq 2 then begin

lon_max1=-155 & lon_min1=-160 & lat_min1=0  & lat_max1=5
lon_max2=-160 & lon_min2=-165 & lat_min2=5  & lat_max2=10
lon_max3=-165 & lon_min3=-170 & lat_min3=10 & lat_max3=15
lon_max4=-170 & lon_min4=-175 & lat_min4=15 & lat_max4=20

lon_maxs=[lon_max4,lon_max3,lon_max2,lon_max1]
lon_mins=[lon_min4,lon_min3,lon_min2,lon_min1]
lat_maxs=[lat_max4,lat_max3,lat_max2,lat_max1]
lat_mins=[lat_min4,lat_min3,lat_min2,lat_min1]

win=window(window_title='Dens_plots',dimensions=[1200,1000],/buffer)

for j=0,3 do begin
for i=0,3 do begin
;i=0 & j=0
  theO2_idx=where(origin_lat gt lat_mins[j] and origin_lat lt lat_maxs[j] and $
                  origin_lon gt lon_mins[i] and origin_lon lt lon_maxs[i])

  theO2_to3=origin_to3[theO2_idx]
  theO2_cfrac=origin_cfrac[theO2_idx]
  theO2_ctp=origin_ctp[theO2_idx]

  cor_O2=correlate(theO2_cfrac,theo2_to3,/double)
  ct=colortable(33)
  ct[0,*]=[255,255,255]
  margin=[0.05,0.3,0.05,0.15]

  props={current:1,rgb:ct,axis_style:2,margin:margin,font_size:10,$
         xtickdir:1,ytickdir:1,xshowtext:0,yshowtext:0,buffer:1}

  ;ymax=max(themyd_to3)+20
  ;ymin=min(themyd_to3)-10
  ymax=310 & ymin=200

  xrange1=1.-0 & yrange1=ymax-ymin
  bin1=xrange1/50 & bin2=yrange1/50
  xticks=string(findgen(11)*0.1,format='(f3.1)') & yticks=string(findgen(10)*10+220,format='(i3)')

  title='Lon : '+strtrim(lon_mins[i],2)+','+strtrim(lon_maxs[i],2)+$
     '   Lat : '+strtrim(lat_mins[j],2)+','+strtrim(lat_maxs[j],2)

  h2d1=hist_2d(theO2_cfrac,theO2_to3,bin1=bin1,bin2=bin2,min2=ymin,min1=0,max2=ymax,max1=1)
  pl1=image(h2d1,layout=[4,4,i+j*4+1],title=title,_extra=props,max=100)
  pl1.axes[2].major=0 & pl1.axes[2].minor=0
  pl1.axes[3].major=0 & pl1.axes[3].minor=0
  pl1.axes[0].tickname=xticks & pl1.axes[1].tickname=yticks
  pl1.position=pl1.position+[-0.02, -0.01, 0.02, 0]
  pl1.position=pl1.position+[+(3-i)*0.01, j*0.030, +(3-i)*0.01, j*0.030]

  pl1.title.font_size=15
  if j eq 3 then pl1.axes[0].showtext=1
  if i eq 0 then pl1.axes[1].showtext=1


  txpos1=(pl1.position[0]+pl1.position[2])*0.5 & typos1=(pl1.position[3]-pl1.position[1])*0.17+pl1.position[1]
  show_cor1=text(txpos1,typos1,'Corr = '+strtrim(cor_O2,2),font_size=16,alignment=0.5)
  ;axpo1=txpos1-0.1 & aypo=(pl1.position[3])*0.4
  ;show_lon=text(axpo1,aypo,'Longitude range : '+strtrim(lon_min,2)+', '+strtrim(lon_max,2),font_size=16,alignment=0 )
  ;show_lat=text(axpo1,aypo-0.05,'Latitude range : '+strtrim(lat_min,2)+', '+strtrim(lat_max,2),font_size=16,alignment=0)

endfor
endfor
cb=colorbar(target=pl,title='Number of pixel',position=[0.1,0.08,0.9,0.11],$
            border_on=1,orientation=0,textpos=0,font_size=16)

fn=respath+strtrim(abs(lon_max)+abs(lat_max))+'option2.png'
win.save,fn,resolution=100
;win.close
hs_filetrans,fn
endif

;+---------------------------------------------------------------------------+
; for MYD input data
;
; #####  ADD REGRESS LINE ON PLOT!  ######
;
if option eq 3 then begin

lon_max1=-155 & lon_min1=-160 & lat_min1=0  & lat_max1=5
lon_max2=-160 & lon_min2=-165 & lat_min2=5  & lat_max2=10
lon_max3=-165 & lon_min3=-170 & lat_min3=10 & lat_max3=15
lon_max4=-170 & lon_min4=-175 & lat_min4=15 & lat_max4=20

lon_maxs=[lon_max4,lon_max3,lon_max2,lon_max1]
lon_mins=[lon_min4,lon_min3,lon_min2,lon_min1]
lat_maxs=[lat_max4,lat_max3,lat_max2,lat_max1]
lat_mins=[lat_min4,lat_min3,lat_min2,lat_min1]

win=window(window_title='Dens_plots',dimensions=[1200,1000],/buffer)

for j=0,3 do begin
for i=0,3 do begin
;i=0 & j=0
  themyd_idx=where(cldmyd_lat gt lat_mins[j] and cldmyd_lat lt lat_maxs[j] and $
                   cldmyd_lon gt lon_mins[i] and cldmyd_lon lt lon_maxs[i])

  themyd_to3=cldmyd_to3[themyd_idx]
  themyd_cfrac=cldmyd_cfrac[themyd_idx]
  themyd_ctp=cldmyd_ctp[themyd_idx]

  cor_myd=correlate(themyd_cfrac,themyd_to3,/double)
  ct=colortable(33)
  ct[0,*]=[255,255,255]
  margin=[0.05,0.3,0.05,0.15]

  props={current:1,rgb:ct,axis_style:2,margin:margin,font_size:10,$
         xtickdir:1,ytickdir:1,xshowtext:0,yshowtext:0,buffer:1}

  ;ymax=max(themyd_to3)+20
  ;ymin=min(themyd_to3)-10
  ymax=310 & ymin=200

  xrange1=1.-0 & yrange1=ymax-ymin
  bin1=xrange1/50 & bin2=yrange1/50
  xticks=string(findgen(11)*0.1,format='(f3.1)') & yticks=string(findgen(10)*10+220,format='(i3)')

  title='Lon : '+strtrim(lon_mins[i],2)+','+strtrim(lon_maxs[i],2)+$
     '   Lat : '+strtrim(lat_mins[j],2)+','+strtrim(lat_maxs[j],2)

  h2d1=hist_2d(themyd_cfrac,themyd_to3,bin1=bin1,bin2=bin2,min2=ymin,min1=0,max2=ymax,max1=1)
  pl1=image(h2d1,layout=[4,4,i+j*4+1],title=title,_extra=props,max=100)
  pl1.axes[2].major=0 & pl1.axes[2].minor=0
  pl1.axes[3].major=0 & pl1.axes[3].minor=0
  pl1.axes[0].tickname=xticks & pl1.axes[1].tickname=yticks
  pl1.position=pl1.position+[-0.02, -0.01, 0.02, 0]
  pl1.position=pl1.position+[+(3-i)*0.01, j*0.030, +(3-i)*0.01, j*0.030]

  pl1.title.font_size=15
  if j eq 3 then pl1.axes[0].showtext=1
  if i eq 0 then pl1.axes[1].showtext=1


  txpos1=(pl1.position[0]+pl1.position[2])*0.5 & typos1=(pl1.position[3]-pl1.position[1])*0.17+pl1.position[1]
  show_cor1=text(txpos1,typos1,'Corr = '+strtrim(cor_myd,2),font_size=16,alignment=0.5)

endfor
endfor
cb=colorbar(target=pl1,title='Number of pixel',position=[0.1,0.08,0.9,0.11],$
            border_on=1,orientation=0,textpos=0,font_size=16)

fn=respath+strtrim(abs(lon_max)+abs(lat_max))+'mydco3vscfrac.png'
win.save,fn,resolution=100
;win.close
hs_filetrans,fn
endif

;+---------------------------------------------------------------------------+
; 
;  RMS vs cloud fraction
;
; #####  ADD REGRESS LINE ON PLOT!  ######
;
if option eq 4 then begin
stop

lon_max1=-155 & lon_min1=-160 & lat_min1=0  & lat_max1=5
lon_max2=-160 & lon_min2=-165 & lat_min2=5  & lat_max2=10
lon_max3=-165 & lon_min3=-170 & lat_min3=10 & lat_max3=15
lon_max4=-170 & lon_min4=-175 & lat_min4=15 & lat_max4=20

lon_maxs=[lon_max4,lon_max3,lon_max2,lon_max1]
lon_mins=[lon_min4,lon_min3,lon_min2,lon_min1]
lat_maxs=[lat_max4,lat_max3,lat_max2,lat_max1]
lat_mins=[lat_min4,lat_min3,lat_min2,lat_min1]

win=window(window_title='O2_plots',dimensions=[1200,1000])

for j=0,3 do begin 
for i=0,3 do begin 
;i=0 & j=0
  theO2_idx=where(origin_lat gt lat_mins[j] and origin_lat lt lat_maxs[j] and $
                  origin_lon gt lon_mins[i] and origin_lon lt lon_maxs[i])

  theo2_to3=origin_to3[theO2_idx]
  theO2_cfrac=origin_cfrac[theO2_idx]
  theO2_ctp=origin_ctp[theO2_idx]
  theO2_rms=origin_rms[theO2_idx]

  cor_O2=correlate(theO2_cfrac,theO2_rms,/double,/nan)
  ct=colortable(33)
  ct[0,*]=[255,255,255]
  margin=[0.05,0.3,0.05,0.15]

  props={current:1,rgb:ct,axis_style:2,margin:margin,font_size:10,$
         xtickdir:1,ytickdir:1,xshowtext:0,yshowtext:0}

  ;ymax=max(themyd_to3)+20
  ;ymin=min(themyd_to3)-10
  ymax=310 & ymin=200

  xrange1=1.-0 & yrange1=ymax-ymin
  bin1=xrange1/50 & bin2=yrange1/50
  xticks=string(findgen(11)*0.1,format='(f3.1)') & yticks=string(findgen(10)*10+220,format='(i3)')

  title='Lon : '+strtrim(lon_mins[i],2)+','+strtrim(lon_maxs[i],2)+$
     '   Lat : '+strtrim(lat_mins[j],2)+','+strtrim(lat_maxs[j],2)

  h2d1=hist_2d(theO2_cfrac,theO2_rms,bin1=bin1,bin2=bin2,min2=ymin,min1=0,max2=ymax,max1=1)
  pl1=image(h2d1,layout=[4,4,i+j*4+1],title=title,_extra=props,max=100)
  pl1.axes[2].major=0 & pl1.axes[2].minor=0
  pl1.axes[3].major=0 & pl1.axes[3].minor=0
  pl1.axes[0].tickname=xticks & pl1.axes[1].tickname=yticks
  pl1.position=pl1.position+[-0.02, -0.01, 0.02, 0]
  pl1.position=pl1.position+[+(3-i)*0.01, j*0.030, +(3-i)*0.01, j*0.030]

  pl1.title.font_size=15
  if j eq 3 then pl1.axes[0].showtext=1
  if i eq 0 then pl1.axes[1].showtext=1

  txpos1=(pl1.position[0]+pl1.position[2])*0.5 & typos1=(pl1.position[3]-pl1.position[1])*0.17+pl1.position[1]
  show_cor1=text(txpos1,typos1,'Corr = '+strtrim(cor_o2,2),font_size=16,alignment=0.5)

endfor
endfor

cb=colorbar(target=pl1,title='Number of pixel',position=[0.1,0.08,0.9,0.11],$
            border_on=1,orientation=0,textpos=0,font_size=16)

fn=respath+strtrim(abs(lon_max)+abs(lat_max))+'O2RMSvscfrac.png'
win.save,fn,resolution=100
;win.close
hs_filetrans,fn


win2=window(window_title='MYD_plots',dimensions=[1200,1000],/buffer)

for j=0,3 do begin
for i=0,3 do begin
;i=0 & j=0
  themyd_idx=where(cldmyd_lat gt lat_mins[j] and cldmyd_lat lt lat_maxs[j] and $
                   cldmyd_lon gt lon_mins[i] and cldmyd_lon lt lon_maxs[i])

  themyd_to3=cldmyd_to3[themyd_idx]
  themyd_cfrac=cldmyd_cfrac[themyd_idx]
  themyd_ctp=cldmyd_ctp[themyd_idx]
  themyd_rms=cldmyd_rms[themyd_idx]
  cor_myd=correlate(themyd_cfrac,themyd_rms,/double)
  ct=colortable(33)
  ct[0,*]=[255,255,255]
  margin=[0.05,0.3,0.05,0.15]

  props={current:1,rgb:ct,axis_style:2,margin:margin,font_size:10,$
         xtickdir:1,ytickdir:1,xshowtext:0,yshowtext:0,buffer:1}

  ;ymax=max(themyd_to3)+20
  ;ymin=min(themyd_to3)-10
  ymax=310 & ymin=200

  xrange1=1.-0 & yrange1=ymax-ymin
  bin1=xrange1/50 & bin2=yrange1/50
  xticks=string(findgen(11)*0.1,format='(f3.1)') & yticks=string(findgen(10)*10+220,format='(i3)')

  title='Lon : '+strtrim(lon_mins[i],2)+','+strtrim(lon_maxs[i],2)+$
     '   Lat : '+strtrim(lat_mins[j],2)+','+strtrim(lat_maxs[j],2)

  h2d1=hist_2d(themyd_cfrac,themyd_to3,bin1=bin1,bin2=bin2,min2=ymin,min1=0,max2=ymax,max1=1)
  pl1=image(h2d1,layout=[4,4,i+j*4+1],title=title,_extra=props,max=100)
  pl1.axes[2].major=0 & pl1.axes[2].minor=0
  pl1.axes[3].major=0 & pl1.axes[3].minor=0
  pl1.axes[0].tickname=xticks & pl1.axes[1].tickname=yticks
  pl1.position=pl1.position+[-0.02, -0.01, 0.02, 0]
  pl1.position=pl1.position+[+(3-i)*0.01, j*0.030, +(3-i)*0.01, j*0.030]

  pl1.title.font_size=15
  if j eq 3 then pl1.axes[0].showtext=1
  if i eq 0 then pl1.axes[1].showtext=1


  txpos1=(pl1.position[0]+pl1.position[2])*0.5 & typos1=(pl1.position[3]-pl1.position[1])*0.17+pl1.position[1]
  show_cor1=text(txpos1,typos1,'Corr = '+strtrim(cor_myd,2),font_size=16,alignment=0.5)

endfor
endfor
cb=colorbar(target=pl1,title='Number of pixel',position=[0.1,0.08,0.9,0.11],$
            border_on=1,orientation=0,textpos=0,font_size=16)

fn=respath+strtrim(abs(lon_max)+abs(lat_max))+'MYDrmsvscfrac.png'
win2.save,fn,resolution=100
;win.close
hs_filetrans,fn
endif

;+---------------------------------------------------------------------------+
; 
;  Total column ozone vs cloud top pressure
;
; #####  ADD REGRESS LINE ON PLOT!  ######
;
if option eq 5 then begin

lon_max1=-155 & lon_min1=-160 & lat_min1=0  & lat_max1=5
lon_max2=-160 & lon_min2=-165 & lat_min2=5  & lat_max2=10
lon_max3=-165 & lon_min3=-170 & lat_min3=10 & lat_max3=15
lon_max4=-170 & lon_min4=-175 & lat_min4=15 & lat_max4=20

lon_maxs=[lon_max4,lon_max3,lon_max2,lon_max1]
lon_mins=[lon_min4,lon_min3,lon_min2,lon_min1]
lat_maxs=[lat_max4,lat_max3,lat_max2,lat_max1]
lat_mins=[lat_min4,lat_min3,lat_min2,lat_min1]

;+---------------------------------------------------------------------------+
; for O2 ctp

win=window(window_title='O2_plots',dimensions=[1200,1000])

for j=0,3 do begin 
for i=0,3 do begin 
;i=0 & j=3
  theO2_idx=where(origin_lat gt lat_mins[j] and origin_lat lt lat_maxs[j] and $
                  origin_lon gt lon_mins[i] and origin_lon lt lon_maxs[i])

  theo2_to3=origin_to3[theO2_idx]
  theO2_cfrac=origin_cfrac[theO2_idx]
  theO2_ctp=origin_ctp[theO2_idx]
  theO2_rms=origin_rms[theO2_idx]

  cor_O2=correlate(theO2_ctp,theO2_to3,/double)
  ct=colortable(33)
  ct[0,*]=[255,255,255]
  margin=[0.05,0.3,0.05,0.15]

  props={current:1,rgb:ct,axis_style:2,margin:margin,font_size:10,$
         xtickdir:1,ytickdir:1,xshowtext:0,yshowtext:0}

  ;ymax=max(themyd_to3)+20
  ;ymin=min(themyd_to3)-10
  ymax=310 & ymin=200
  xmax=1000. & xmin=100.
  xrange1=xmax-xmin & yrange1=ymax-ymin
  bin1=xrange1/50 & bin2=yrange1/50
  xticks=string(indgen(6)*200,format='(i5)') & yticks=string(findgen(10)*10+220,format='(i3)')

  title='Lon : '+strtrim(lon_mins[i],2)+','+strtrim(lon_maxs[i],2)+$
     '   Lat : '+strtrim(lat_mins[j],2)+','+strtrim(lat_maxs[j],2)

  h2d1=hist_2d(theO2_ctp,theO2_to3,bin1=bin1,bin2=bin2,min2=ymin,min1=xmin,max2=ymax,max1=xmax)
  pl1=image(h2d1,layout=[4,4,i+j*4+1],title=title,_extra=props,max=100)
  pl1.axes[2].major=0 & pl1.axes[2].minor=0
  pl1.axes[3].major=0 & pl1.axes[3].minor=0
  pl1.axes[0].tickname=xticks
  pl1.axes[1].tickname=yticks
  pl1.position=pl1.position+[-0.02, -0.01, 0.02, 0]
  pl1.position=pl1.position+[+(3-i)*0.01, j*0.030, +(3-i)*0.01, j*0.030]

  pl1.title.font_size=15
  if j eq 3 then pl1.axes[0].showtext=1
  if i eq 0 then pl1.axes[1].showtext=1

  txpos1=(pl1.position[0]+pl1.position[2])*0.5 & typos1=(pl1.position[3]-pl1.position[1])*0.17+pl1.position[1]
  show_cor1=text(txpos1,typos1,'Corr = '+strtrim(cor_o2,2),font_size=16,alignment=0.5)

endfor
endfor

cb=colorbar(target=pl1,title='Number of pixel',position=[0.1,0.08,0.9,0.11],$
            border_on=1,orientation=0,textpos=0,font_size=16)

fn=respath+strtrim(abs(lon_max)+abs(lat_max))+'O2CTPvsTO3.png'
win.save,fn,resolution=100
;win.close
hs_filetrans,fn


;+---------------------------------------------------------------------------+
; for MYDCTP

win2=window(window_title='MYD_plots',dimensions=[1200,1000],/buffer)

for j=0,3 do begin
for i=0,3 do begin
;i=0 & j=0
  themyd_idx=where(cldmyd_lat gt lat_mins[j] and cldmyd_lat lt lat_maxs[j] and $
                   cldmyd_lon gt lon_mins[i] and cldmyd_lon lt lon_maxs[i])

  themyd_to3=cldmyd_to3[themyd_idx]
  themyd_cfrac=cldmyd_cfrac[themyd_idx]
  themyd_ctp=cldmyd_ctp[themyd_idx]
  themyd_rms=cldmyd_rms[themyd_idx]
  cor_myd=correlate(themyd_ctp,themyd_to3,/double)
  ct=colortable(33)
  ct[0,*]=[255,255,255]
  margin=[0.05,0.3,0.05,0.15]

  props={current:1,rgb:ct,axis_style:2,margin:margin,font_size:10,$
         xtickdir:1,ytickdir:1,xshowtext:0,yshowtext:0,buffer:1}

  ymax=310 & ymin=200
  xmax=1000. & xmin=100.
  xrange1=xmax-xmin & yrange1=ymax-ymin
  bin1=xrange1/50 & bin2=yrange1/50
  xticks=string(indgen(6)*200,format='(i5)') & yticks=string(findgen(10)*10+220,format='(i3)')

  title='Lon : '+strtrim(lon_mins[i],2)+','+strtrim(lon_maxs[i],2)+$
     '   Lat : '+strtrim(lat_mins[j],2)+','+strtrim(lat_maxs[j],2)

  h2d1=hist_2d(themyd_ctp,themyd_to3,bin1=bin1,bin2=bin2,min2=ymin,min1=0,max2=ymax,max1=1)
  pl1=image(h2d1,layout=[4,4,i+j*4+1],title=title,_extra=props,max=100)
  pl1.axes[2].major=0 & pl1.axes[2].minor=0
  pl1.axes[3].major=0 & pl1.axes[3].minor=0
  pl1.axes[0].tickname=xticks & pl1.axes[1].tickname=yticks
  pl1.position=pl1.position+[-0.02, -0.01, 0.02, 0]
  pl1.position=pl1.position+[+(3-i)*0.01, j*0.030, +(3-i)*0.01, j*0.030]

  pl1.title.font_size=15
  if j eq 3 then pl1.axes[0].showtext=1
  if i eq 0 then pl1.axes[1].showtext=1


  txpos1=(pl1.position[0]+pl1.position[2])*0.5 & typos1=(pl1.position[3]-pl1.position[1])*0.17+pl1.position[1]
  show_cor1=text(txpos1,typos1,'Corr = '+strtrim(cor_myd,2),font_size=16,alignment=0.5)

endfor
endfor
cb=colorbar(target=pl1,title='Number of pixel',position=[0.1,0.08,0.9,0.11],$
            border_on=1,orientation=0,textpos=0,font_size=16)

fn=respath+strtrim(abs(lon_max)+abs(lat_max))+'MYDCTPvsTO3.png'
win2.save,fn,resolution=100
;win.close
hs_filetrans,fn

endif

;+---------------------------------------------------------------------------+
; 
;  Total column ozone vs Cloud optical depth
;
; #####  ADD REGRESS LINE ON PLOT!  ######
;
if option eq 6 then begin
stop

lon_max1=-155 & lon_min1=-160 & lat_min1=0  & lat_max1=5
lon_max2=-160 & lon_min2=-165 & lat_min2=5  & lat_max2=10
lon_max3=-165 & lon_min3=-170 & lat_min3=10 & lat_max3=15
lon_max4=-170 & lon_min4=-175 & lat_min4=15 & lat_max4=20

lon_maxs=[lon_max4,lon_max3,lon_max2,lon_max1]
lon_mins=[lon_min4,lon_min3,lon_min2,lon_min1]
lat_maxs=[lat_max4,lat_max3,lat_max2,lat_max1]
lat_mins=[lat_min4,lat_min3,lat_min2,lat_min1]

win=window(window_title='O2_plots',dimensions=[1200,1000])

for j=0,3 do begin 
for i=0,3 do begin 
;i=0 & j=3
  theO2_idx=where(origin_lat gt lat_mins[j] and origin_lat lt lat_maxs[j] and $
                  origin_lon gt lon_mins[i] and origin_lon lt lon_maxs[i])

  theo2_to3=origin_to3[theO2_idx]
  theO2_cfrac=origin_cfrac[theO2_idx]
  theO2_ctp=origin_ctp[theO2_idx]
  theO2_rms=origin_rms[theO2_idx]

  cor_O2=correlate(theO2_ctp,theO2_to3,/double)
  ct=colortable(33)
  ct[0,*]=[255,255,255]
  margin=[0.05,0.3,0.05,0.15]

  props={current:1,rgb:ct,axis_style:2,margin:margin,font_size:10,$
         xtickdir:1,ytickdir:1,xshowtext:0,yshowtext:0}

  ;ymax=max(themyd_to3)+20
  ;ymin=min(themyd_to3)-10
  ymax=310 & ymin=200
  xmax=1000. & xmin=100.
  xrange1=xmax-xmin & yrange1=ymax-ymin
  bin1=xrange1/50 & bin2=yrange1/50
  xticks=string(indgen(6)*200,format='(i4)') & yticks=string(findgen(10)*10+220,format='(i3)')

  title='Lon : '+strtrim(lon_mins[i],2)+','+strtrim(lon_maxs[i],2)+$
     '   Lat : '+strtrim(lat_mins[j],2)+','+strtrim(lat_maxs[j],2)

  h2d1=hist_2d(theO2_ctp,theO2_to3,bin1=bin1,bin2=bin2,min2=ymin,min1=xmin,max2=ymax,max1=xmax)
  pl1=image(h2d1,layout=[4,4,i+j*4+1],title=title,_extra=props,max=100)
  pl1.axes[2].major=0 & pl1.axes[2].minor=0
  pl1.axes[3].major=0 & pl1.axes[3].minor=0
  pl1.axes[0].tickname=xticks & pl1.axes[1].tickname=yticks
  pl1.position=pl1.position+[-0.02, -0.01, 0.02, 0]
  pl1.position=pl1.position+[+(3-i)*0.01, j*0.030, +(3-i)*0.01, j*0.030]

  pl1.title.font_size=15
  if j eq 3 then pl1.axes[0].showtext=1
  if i eq 0 then pl1.axes[1].showtext=1

  txpos1=(pl1.position[0]+pl1.position[2])*0.5 & typos1=(pl1.position[3]-pl1.position[1])*0.17+pl1.position[1]
  show_cor1=text(txpos1,typos1,'Corr = '+strtrim(cor_o2,2),font_size=16,alignment=0.5)

endfor
endfor

cb=colorbar(target=pl1,title='Number of pixel',position=[0.1,0.08,0.9,0.11],$
            border_on=1,orientation=0,textpos=0,font_size=16)

fn=respath+strtrim(abs(lon_max)+abs(lat_max))+'O2CTPvsTO3.png'
win.save,fn,resolution=100
;win.close
hs_filetrans,fn

endif


;+---------------------------------------------------------------------------+
; PLOT DATA ON MAP 
;+---------------------------------------------------------------------------+

    ;void=execute('data = gems_'+type+'.'+datalist[which_plot] )

    ;data=reform(data)

    ;;print,'NUMBER OF COLLOCATED PIXEL : ', n_elements(where(res.cf ge 0))

    ;if first then begin
      ;prefix=respath+orbit+'+'+figtag+type
      ;ps=prefix+'.ps'
      ;png=prefix+'.png'
      ;cgps_open,ps,xsize=10,ysize=9,/nomatch,/inch,xoffset=0.5,yoffset=0.5
      ;;window,0,xs=1000,ys=1000
      ;pos=[0.05,0.05,0.81,0.90]

      ;loadct_wb, coltables[which_plot]
      ;TVLCT, CGColor('BLACK', /Triple), 255 ; Drawing color.
      ;TVLCT, CGColor('WHITE', /Triple), 254 ; Background color.
      ;TVLCT, CGColor(badpixcol,/Triple),251 ; Badpix color
      ;!p.color=255

      ;MAP_SET, fix((limit[0]+limit[2])/2), fix((limit[1]+limit[3])/2), $
      ;/satellite,scale=4e07, latdel=10, londel=10, position=pos
    ;endif


  ;;Data polyfill 
    ;for p=0,npix-1 do begin

      ;ipix=notbad[p]
      ;clon=reform(pixcor.clon[pix[ipix]-1,line[ipix]-1,*])
      ;clat=reform(pixcor.clat[pix[ipix]-1,line[ipix]-1,*])

      ;for k=0,nlev-2 do begin

        ;if (data[ipix] lt lev[0]) then col=barcol[0]
        ;if (data[ipix] ge lev[k] and data[p] lt lev[k+1]) then col=barcol[k]
        ;if (data[ipix] ge lev[-1]) then col=barcol[-1]

        ;if data[ipix] lt 0 then stop ; check data[ipix]!!!

      ;endfor

      ;if data[ipix] lt 0 then begin
        ;col=251
        ;tmp=[tmp,tmpdata[ipix]]
      ;endif

      ;;if where(bad eq ipix) ne -1 then col=251
      ;;if where(cfrac_mask eq ipix) eq -1 then col=254

      ;polyfill,[clon,clon[0]],[clat,clat[0]],color=col,noclip=0

    ;endfor
    ;if last then begin
      ;;plots,[[-150,0],[-170,0],[-170,10],[-150,10],[-150,0]]
      ;colorbar_2,lev,barcol,/col,ys=0.77,xs=0.015,charsize=1.9, $
      ;charthick=2,levelind=findgen(100)*5,$
      ;format=formats[which_plot],lowleft=[0.87,0.05],/nofirst,/right

      ;map_continents,/continent,/count,MLINETHICK=1.8,/country,/hires
      ;map_grid,latdel=10,londel=10,thick=0.7,charsize=1
      ;xyouts,mean([pos[0],pos[2]]),pos[3]+0.04,align=0.5,title,$
      ;charsize=2,/normal

    ;; SAVE FIGURE
      ;cgps_close,density=800,/png
      ;hs_filetrans,png
    ;endif

    ;first=0
  ;endfor  ;end orbit
;endfor  ;end type
;PRINT,''
;PRINT,'PROCEDURE END. VARIABLES SAVED  :: ',type+'.'+datalist[which_plot]
;PRINT,''

if not loop then stop

end
