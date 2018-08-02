pro hs_cld_analysis,loop=loop,xdrmod=xdrmod,which_plot=which_plot,control=control

;+---------------------------------------------------------------------------+
; RUN : hs_gems_checkarea_clrvscld,xdrmod=0,which_plot=2,loop=1
;
; INPUT : options
;
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
to3path   = '/home/Data/OMI/2_OML2TO/2005/'


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
cmodis_cot=[] & nhcot=[] & nlcot=[]
toms_to3  =[] & toms_ctp=[]
tmp=[]
cldmyd_day=[] & origin_day=[]
ncld=[] & nclr=[]
diff=[] & toms_diff=[]
cloud_cot=[]
clear_min=[]
clear_lowqt=[]
clear_med=[]
clear_max=[]
clear_upqt=[]

cloud_min=[]
cloud_lowqt=[]
cloud_med=[]
cloud_max=[]
cloud_upqt=[]

TOMS_cloud_cot=[]
TOMS_clear_min=[]
TOMS_clear_lowqt=[]
TOMS_clear_med=[]
TOMS_clear_max=[]
TOMS_clear_upqt=[]

TOMS_cloud_min=[]
TOMS_cloud_lowqt=[]
TOMS_cloud_med=[]
TOMS_cloud_max=[]
TOMS_cloud_upqt=[]
gems_files  = file_search(outpath+'GEM*')
;gems_files  = file_search(outpath+'GEM*_20050601T003900*')


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
;  LOOP OF READING FILE
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
types=['CLDMYD','ORIGIN']

xdrmod2=1   ; if not changed GEMS file results, restore it.
fn=xdrpath+'cld_analysis.xdr'

if xdrmod2 then begin
  restore,fn
endif else begin

for i=0,n_elements(orbits)-1 do begin

  orbit=orbits[i]
  pixcor_file=file_search(pixpath+'*'+'o'+orbit+'*')
  omito3_file=file_search(to3path+'*'+'o'+orbit+'*')

  hs_omi_pixcor,pixcor_file,pixcor,xdrmod=xdrmod
  hs_read_omto3,omito3_file,omto3,xdrmod=xdrmod

  for t=0,n_elements(types)-1 do begin
    type=types[t]
    day =fdates[where(forbits eq orbit and ftypes eq type)]
    files=gems_files[where(ftypes eq type)]
    void=execute('hs_read_gems,files[i],gems_'+type+',xdrmod=xdrmod')
    void=execute('pix=gems_'+type+'.pix & line=gems_'+type+'.line')
    void=execute('notbad=where(gems_'+type+'.co3[0] gt 0 and gems_'+type+'.exval gt 0,npix)')
    ;bad   =where(gems_cldmyd.co3[0] lt 0,bpix)

    if type eq 'CLDMYD' then begin
      ;indx=where(gems_cldmyd.co3[0] gt 0 and gems_cldmyd.co3[0] gt 0,nindx) ; ONLY GATHERING COLLOCATED PIXEL
      cldmyd_notbad=gems_cldmyd.co3[0] gt 0                                        ; ONLY GATHERING COLLOCATED PIXEL
    endif else begin 
      origin_notbad=gems_origin.co3[0] gt 0

      indx=where(cldmyd_notbad and origin_notbad,nindx) 

      days=strarr(nindx) & days[*]=day[0]
      cotfn = xdrpath+'COT_o'+strtrim(orbits[i])+'.xdr'
      restore,cotfn
      coltoms=findgen(nindx)
      coltomsctp=findgen(nindx)
      colcot=findgen(nindx)
      for n=0,nindx-1 do colcot[n]=mod2omi_cot[gems_origin[indx[n]].pix-1,gems_origin[indx[n]].line-1]
      for n=0,nindx-1 do coltoms[n]=omto3.to3[gems_origin[indx[n]].pix-1,gems_origin[indx[n]].line-1]
      for n=0,nindx-1 do coltomsctp[n]=omto3.ctp[gems_origin[indx[n]].pix-1,gems_origin[indx[n]].line-1]

      cmodis_cot  =[cmodis_cot  ,colcot]
      toms_to3    =[toms_to3    ,coltoms]
      toms_ctp    =[toms_ctp    ,coltomsctp]
      cldmyd_to3  =[cldmyd_to3  ,gems_cldmyd[indx].co3[0]] 
      cldmyd_tto3 =[cldmyd_tto3 ,gems_cldmyd[indx].co3[1]] 
      cldmyd_sto3 =[cldmyd_sto3 ,gems_cldmyd[indx].co3[2]] 
      cldmyd_cfrac=[cldmyd_cfrac,gems_cldmyd[indx].cfrac]
      cldmyd_ctp  =[cldmyd_ctp  ,gems_cldmyd[indx].ctp]
      cldmyd_lon  =[cldmyd_lon  ,gems_cldmyd[indx].lon]
      cldmyd_lat  =[cldmyd_lat  ,gems_cldmyd[indx].lat]
      cldmyd_rms  =[cldmyd_rms  ,gems_cldmyd[indx].rms]
      cldmyd_day  =[cldmyd_day  ,days]

      origin_to3  =[origin_to3  ,gems_origin[indx].co3[0]] 
      origin_tto3 =[origin_tto3 ,gems_origin[indx].co3[1]] 
      origin_sto3 =[origin_sto3 ,gems_origin[indx].co3[2]] 
      origin_cfrac=[origin_cfrac,gems_origin[indx].cfrac]
      origin_ctp  =[origin_ctp  ,gems_origin[indx].ctp]
      origin_lon  =[origin_lon  ,gems_origin[indx].lon]
      origin_lat  =[origin_lat  ,gems_origin[indx].lat]
      origin_rms  =[origin_rms  ,gems_origin[indx].rms]
      origin_day  =[origin_day  ,days]
    endelse
  endfor ; types
endfor ; orbit
save, file=fn, $
      toms_to3,$
      toms_ctp,$
      cmodis_cot,$      
      cldmyd_to3 ,$
      cldmyd_tto3 ,$
      cldmyd_sto3 ,$
      cldmyd_cfrac,$
      cldmyd_ctp,$
      cldmyd_lon ,$
      cldmyd_lat ,$
      cldmyd_rms ,$
      cldmyd_day,$
      origin_to3 ,$
      origin_tto3,$
      origin_sto3,$
      origin_cfrac,$
      origin_ctp ,$
      origin_lon,$
      origin_lat  ,$
      origin_rms ,$
      origin_day 
endelse  ; xdrmod2

;stop   ; data reading check

;+---------------------------------------------------------------------------+
; SCATTERPLOT and correlation check
; OPTION SET
;+---------------------------------------------------------------------------+

option=0
toms_option=1
cfrac_bnd=0.8
clear_bnd=0.2

; OPTION=0 : Total ozone statistics (2 weeks), clear vs cloudy for each day 180W~131W lon x 0~5N lat scene 
; OPTION=1 : Total ozone statistics (2 weeks), clear vs cloudy for each day 175W~170W lon x 0~5N lat scene 
; OPTION=2 : Total ozone statistics (2 weeks), clear vs high & low COT of MODIS for each day 
;            180W~131W lon x 0~5N lat scene (only cot)
;
; OPTION=3 : Total ozone statistics (2 weeks), clear vs cloudy for high & low COT of MODIS for each day 
;            180W~131W lon x 0~5N lat scene (cot & high cfrac)
;
; OPTION=4 : WHOLE pixles in one boxplot!!! 
; OPTION=5 : SORT WITH COT AND CHECK TOTAL OZONE

;+---------------------------------------------------------------------------+
;
; FANCY AREA SELECTION 
;
;+---------------------------------------------------------------------------+

LATBND=[0,5]
BINSIZE=5
MINLON=-180
MAXLON=-130

NBIN=(MAXLON-MINLON)/BINSIZE
LONBIN=intarr(nbin,2)
good_clr_idx=[]
good_cld_idx=[]
good_days   =[]
good_lons   =[]
OEMDIFF = []
TOMSDIFF = []
AVGCOT  = []
FOR d=0,ndates-1 DO BEGIN
  FOR i=0,nbin-1 DO BEGIN
    lonbin[i,*]=[minlon+i*binsize,minlon+i*binsize+binsize]
    area_in = origin_lat gt latbnd[0] and origin_lat lt latbnd[1] and $
              origin_lon gt lonbin[i,0] and origin_lon lt lonbin[i,1]
    day_in = origin_day eq dates[d]

    cloud_in = origin_cfrac gt 0.8
    cloud_out = origin_cfrac lt 0.2

    high_cot = cmodis_cot gt 20
    low_cot = cmodis_cot lt 10 
    cotnotbad = cmodis_cot gt 0

    bin_idx     = where(day_in and area_in)
    clear_idx   = where(day_in and area_in and cloud_out,nclridx)
    cloudy_idx  = where(day_in and area_in and cloud_in ,ncldidx)
    diff        = mean(origin_to3[clear_idx]) - mean(origin_to3[cloudy_idx]) 
    toms_diff   = mean(toms_to3[clear_idx]) - mean(toms_to3[cloudy_idx])
    cot_mean    = mean(cmodis_cot[where(day_in and area_in and cloud_in and cotnotbad)])


    if nclridx gt 30 and ncldidx gt 30 then begin
      good_clr_idx = [good_clr_idx,clear_idx]
      good_cld_idx = [good_cld_idx,cloudy_idx]

      clear_boxarr        = CREATEBOXPLOTDATA(origin_to3[clear_idx])
      toms_clear_boxarr   = CREATEBOXPLOTDATA(toms_to3[clear_idx])
      cloud_boxarr        = CREATEBOXPLOTDATA(origin_to3[cloudy_idx])
      toms_cloud_boxarr  = CREATEBOXPLOTDATA(toms_to3[cloudy_idx])

      clear_min   = [clear_min,clear_boxarr[0]]
      clear_lowqt = [clear_lowqt,clear_boxarr[1]]
      clear_med   = [clear_med,clear_boxarr[2]]
      clear_upqt  = [clear_upqt,clear_boxarr[3]]
      clear_max   = [clear_max,clear_boxarr[4]]

      cloud_min   = [cloud_min,cloud_boxarr[0]]
      cloud_lowqt = [cloud_lowqt,cloud_boxarr[1]]
      cloud_med   = [cloud_med,cloud_boxarr[2]]
      cloud_upqt  = [cloud_upqt,cloud_boxarr[3]]
      cloud_max   = [cloud_max,cloud_boxarr[4]]

      toms_clear_min    = [toms_clear_min,toms_clear_boxarr[0]]
      toms_clear_lowqt  = [toms_clear_lowqt,toms_clear_boxarr[1]]
      toms_clear_med    = [toms_clear_med,toms_clear_boxarr[2]]
      toms_clear_upqt   = [toms_clear_upqt,toms_clear_boxarr[3]]
      toms_clear_max    = [toms_clear_max,toms_clear_boxarr[4]]

      toms_cloud_min    = [toms_cloud_min,toms_cloud_boxarr[0]]
      toms_cloud_lowqt  = [toms_cloud_lowqt,toms_cloud_boxarr[1]]
      toms_cloud_med    = [toms_cloud_med,toms_cloud_boxarr[2]]
      toms_cloud_upqt   = [toms_cloud_upqt,toms_cloud_boxarr[3]]
      toms_cloud_max    = [toms_cloud_max,toms_cloud_boxarr[4]]
      good_days         = [good_days,dates[d]]
      good_lons         = [good_lons,strtrim(lonbin[i,0],2)]
      OEMDIFF           = [OEMDIFF,diff]
      TOMSDIFF          = [TOMSDIFF,toms_diff]
      AVGCOT            = [AVGCOT,cot_mean]
    endif

    nclr=[nclr,nclridx]
    ncld=[ncld,ncldidx]

  ENDFOR ; for lonbin
ENDFOR ; for date

good_clr_day  = origin_day[good_clr_idx]
good_clr_oem  = origin_to3[good_clr_idx]
good_clr_toms = toms_to3[good_clr_idx]
good_clr_myd  = cldmyd_to3[good_clr_idx]
good_clr_cot  = cmodis_cot[good_clr_idx]

good_cld_day  = origin_day[good_cld_idx]
good_cld_oem  = origin_to3[good_cld_idx]
good_cld_toms = toms_to3[good_cld_idx]
good_cld_myd  = cldmyd_to3[good_cld_idx]
good_cld_cot  = cmodis_cot[good_cld_idx]


if option eq 0 then begin
;+---------------------------------------------------------------------------+
; 
; Total ozone statistics (2 weeks) 
;
; clear vs cloudy for one day 180W~131W longitude x  0~5N latitude scene
;
;


;+---------------------------------------------------------------------------+
;
; BOXPLOT START POINT
;

nplot=n_elements(clear_min)

win1=window(window_title='Clear vs Cloud',dimensions=[1000,600])

boxinput1=[[clear_min],[clear_lowqt],[clear_med],[clear_upqt],[clear_max]]
boxinput2=[[cloud_min],[cloud_lowqt],[cloud_med],[cloud_upqt],[cloud_max]]
position=[0.10,0.10,0.90,0.85]

; Generate the Date/Time data
;daygen = TIMEGEN(ndates,START=JULDAY(6,1,2005))
;caldat,daygen,mon,day,year

;xlabel=strtrim(mon,2)+'-'+strtrim(day,2)+', '+good_lons[l]
;xlabel=xlabel[thedays]

xlabel=strmid(good_days,5,1)+'-'+strmid(good_days,6,2)
xlabel2=strmid(good_lons,1,3)+'W'

xrange=[-1,nplot] & yrange=[220,260]
yaxrange=[-5,5]


boxes1=boxplot(boxinput1,fill_color='linen',background_color='white',/current,thick=2,font_size=16,$ 
              xrange=xrange,yrange=yrange,width=0.08,pos=position,xstyle=1)  ;,layout=[2,1,1])
boxes1.xtransparency=100
boxes1.ytransparency=100
boxes1.position= boxes1.position+[-0.008,0,-0.008,0]

boxes2=boxplot(boxinput2,fill_color='gray',background_color='white',/current,thick=2,font_size=16,$
              xrange=xrange,yrange=yrange,width=0.08,pos=position,xstyle=1)  ;,layout=[2,1,1])
boxes2.xtransparency=100
boxes2.ytransparency=100
boxes2.position= boxes2.position+[+0.008,0,+0.008,0]

nodata=boxplot(boxinput1,xtitle="Days",ytitle='Total Ozone (DU)',title='Total ozone statistics, clear vs cloudy',$
              fill_color='linen',background_color='white',/current,thick=2,font_size=16,                    $ 
              xrange=xrange,yrange=yrange,width=0.1,pos=position,xstyle=1,/nodata)  ;,layout=[2,1,1])

nodata.axes[2].hide=1
ax0=nodata.axes[0]
ax0.tickvalues=indgen(nplot)
ax0.tickname=xlabel
ax0.minor=0
ax0.ticklen=0.02
nodata.axes[3].hide=1

title=nodata.title
title.font_size=25
plt = plot(oemdiff,/current,pos=position+[+0,+0.2,+0,-0.2],xtransparency=100,ytransparency=100,yrange=yaxrange,xrange=xrange,$
           thick=2,symbol=6,sym_filled=1,color='brown')
zeroline=plot(intarr(nplot),/overplot,xtransparency=100,ytransparency=100,thick=2,color='sandy brown')

plt2= plot(avgcot,/current,pos=position+[+0,+0.2,+0,-0.2],xtransparency=100,ytransparency=100,yrange=[0,60],xrange=xrange,$
           thick=2,symbol=6,sym_filled=1,color='LIGHT SEA GREEN')

yax=axis('y',location='right',title='Difference of means, clear pixel - cloudy pixel (DU)',$
          axis_range=yaxrange,target=plt,tickfont_size=16);$
;xax=axis('x',location=260,title='longitude',axis_range=xrange,target=nodata,tickfont_size=16)

text0 = Text(0.65, 0.2, 'Gray : cloudy pixel (cfrac > 0.8)', VERTICAL_ALIGNMENT=0.00000,font_size=16)
text1 = Text(0.65, 0.15, 'Linen : clear pixel (cfrac < 0.2)', VERTICAL_ALIGNMENT=0.00000,font_size=16)

fn=respath+'boxplot_2+weeks_bnd08.png'
win1.save,fn,resolution=100
;win1.close
hs_filetrans,fn
;stop
if toms_option then begin
  ;+---------------------------------------------------------------------------+
  ;
  ; TOMS plot
  ;


  win2=window(window_title='Clear vs Cloud,TOMS',dimensions=[1000,600])

  boxinput1=[[toms_clear_min],[toms_clear_lowqt],[toms_clear_med],[toms_clear_upqt],[toms_clear_max]]
  boxinput2=[[toms_cloud_min],[toms_cloud_lowqt],[toms_cloud_med],[toms_cloud_upqt],[toms_cloud_max]]

  ; Generate the Date/Time data
  ;time = TIMEGEN(ndates,START=JULDAY(6,1,2005))
  ;caldat,time,mon,day,year

  ;xlabel=strtrim(mon,2)+'.'+strtrim(day,2)
  ;xlabel=xlabel[thedays]

  xrange=[-1,nplot] & yrange=[220,260]


  boxes1=boxplot(boxinput1,fill_color='linen',background_color='white',/current,thick=2,font_size=16,$ 
                xrange=xrange,yrange=yrange,width=0.08,pos=position,xstyle=1)  ;,layout=[2,1,1])
  boxes1.xtransparency=100
  boxes1.ytransparency=100
  boxes1.position= boxes1.position+[-0.008,0,-0.008,0]

  boxes2=boxplot(boxinput2,fill_color='gray',background_color='white',/current,thick=2,font_size=16,$
                xrange=xrange,yrange=yrange,width=0.08,pos=position,xstyle=1)  ;,layout=[2,1,1])
  boxes2.xtransparency=100
  boxes2.ytransparency=100
  boxes2.position= boxes2.position+[+0.008,0,+0.008,0]

  nodata=boxplot(boxinput1,xtitle="Days",ytitle='TOMS Total Ozone (DU)',title='TOMS Total ozone statistics, clear vs cloudy',$
                fill_color='linen',background_color='white',/current,thick=2,font_size=16,                    $ 
                xrange=xrange,yrange=yrange,width=0.08,pos=position,xstyle=1,/nodata)  ;,layout=[2,1,1])

  nodata.axes[2].hide=1
  ax0=nodata.axes[0]
  ax0.tickvalues=indgen(nplot)
  ax0.tickname=xlabel
  ax0.minor=0
  ax0.ticklen=0.02
  nodata.axes[3].hide=1

  title=nodata.title
  title.font_size=25
  plt = plot(tomsdiff,/current,pos=position+[+0,+0.2,+0,-0.2],$
             xtransparency=100,ytransparency=100,yrange=yaxrange,xrange=xrange,$
             thick=2,symbol=6,sym_filled=1,color='brown')
  zeroline=plot(intarr(nplot),/overplot,xtransparency=100,ytransparency=100,thick=2,color='sandy brown')

  plt2= plot(avgcot,/current,pos=position+[+0,+0.2,+0,-0.2],xtransparency=100,ytransparency=100,yrange=[0,60],xrange=xrange,$
             thick=2,symbol=6,sym_filled=1,color='LIGHT SEA GREEN')

  ;nodata2=plot(diff,/current,pos=position+[+0,+0.2,+0,-0.2],xtransparency=100,yrange=[-5,5],xrange=xrange,/nodata)
  yax=axis('y',location='right',title='Difference of means, clear pixel - cloudy pixel (DU)',$
           axis_range=yaxrange,target=plt,tickfont_size=16);$
            

  text0 = Text(0.65, 0.2, 'Gray : cloudy pixel (cfrac > 0.8)', VERTICAL_ALIGNMENT=0.00000,font_size=16)
  text1 = Text(0.65, 0.15, 'Linen : clear pixel (cfrac < 0.2)', VERTICAL_ALIGNMENT=0.00000,font_size=16)

  fn=respath+'boxplot_2+weeks_bnd08_TOMS.png'
  win2.save,fn,resolution=100
  ;win1.close
  hs_filetrans,fn
  endif ; TOMS option

endif ; end option

IF option EQ 1 THEN BEGIN
;+---------------------------------------------------------------------------+
; 
; Total ozone statistics, clear vs cloudy (2 weeks)
; 
; compare high cot (over 30) case pixel vs low cot (under 10) case pixel
;
; IR 에서는 수적에 의한 흡수가 심함. 수적이 많은 구름은 어떤거지? 
; 걔네랑 아닐때랑 놓고 MODIS cloud top pressure 넣어보면 될거 같은데.
;
; 변인 : cloud optical thickness, cloud fraction, clout top pressure
;
; 일단 cloud fraction은 clear(lt 0.2) vs cloudy(gt 0.8) 로만 나누고
; COT vs TO3
; CTP vs TO3
; COT and CTP vs TO3 이렇게 세개를 그려볼까... 음 일단 COT vs TO3 부터.
;
;+---------------------------------------------------------------------------+
;
; select area
;

na=[] & a_min=[] & a_lowqt=[] & a_med=[] & a_upqt=[] & a_max=[]

nb=[] & b_min=[] & b_lowqt=[] & b_med=[] & b_upqt=[] & b_max=[]

nc=[] & c_min=[] & c_lowqt=[] & c_med=[] & c_upqt=[] & c_max=[]

nd=[] & d_min=[] & d_lowqt=[] & d_med=[] & d_upqt=[] & d_max=[]

nplot=n_elements(clear_min)

win1=window(window_title='Clear vs Cloud',dimensions=[1000,600])

boxinput1=[[clear_min],[clear_lowqt],[clear_med],[clear_upqt],[clear_max]]
boxinput2=[[cloud_min],[cloud_lowqt],[cloud_med],[cloud_upqt],[cloud_max]]
position=[0.10,0.10,0.90,0.85]

xlabel=strmid(good_days,5,1)+'-'+strmid(good_days,6,2)
xlabel2=strmid(good_lons,1,3)+'W'

xrange=[-1,nplot] & yrange=[220,260]
yaxrange=[-5,5]


boxes1=boxplot(boxinput1,fill_color='linen',background_color='white',/current,thick=2,font_size=16,$ 
              xrange=xrange,yrange=yrange,width=0.08,pos=position,xstyle=1)  ;,layout=[2,1,1])
boxes1.xtransparency=100
boxes1.ytransparency=100
boxes1.position= boxes1.position+[-0.008,0,-0.008,0]

boxes2=boxplot(boxinput2,fill_color='gray',background_color='white',/current,thick=2,font_size=16,$
              xrange=xrange,yrange=yrange,width=0.08,pos=position,xstyle=1)  ;,layout=[2,1,1])
boxes2.xtransparency=100
boxes2.ytransparency=100
boxes2.position= boxes2.position+[+0.008,0,+0.008,0]

nodata=boxplot(boxinput1,xtitle="Days",ytitle='Total Ozone (DU)',title='Total ozone statistics, clear vs cloudy',$
              fill_color='linen',background_color='white',/current,thick=2,font_size=16,                    $ 
              xrange=xrange,yrange=yrange,width=0.1,pos=position,xstyle=1,/nodata)  ;,layout=[2,1,1])

nodata.axes[2].hide=1
ax0=nodata.axes[0]
ax0.tickvalues=indgen(nplot)
ax0.tickname=xlabel
ax0.minor=0
ax0.ticklen=0.02
nodata.axes[3].hide=1

title=nodata.title
title.font_size=25
plt = plot(oemdiff,/current,pos=position+[+0,+0.2,+0,-0.2],xtransparency=100,ytransparency=100,yrange=yaxrange,xrange=xrange,$
           thick=2,symbol=6,sym_filled=1,color='brown')
zeroline=plot(intarr(nplot),/overplot,xtransparency=100,ytransparency=100,thick=2,color='sandy brown')

yax=axis('y',location='right',title='Difference of means, clear pixel - cloudy pixel (DU)',$
          axis_range=yaxrange,target=plt,tickfont_size=16);$
;xax=axis('x',location=260,title='longitude',axis_range=xrange,target=nodata,tickfont_size=16)

text0 = Text(0.65, 0.2, 'Gray : cloudy pixel (cfrac > 0.8)', VERTICAL_ALIGNMENT=0.00000,font_size=16)
text1 = Text(0.65, 0.15, 'Linen : clear pixel (cfrac < 0.2)', VERTICAL_ALIGNMENT=0.00000,font_size=16)

fn=respath+'boxplot_2+weeks_bnd08.png'
win1.save,fn,resolution=100
;win1.close
hs_filetrans,fn
for d=0,ndates-1 do begin

lon_max=-170 & lon_min=-175 & lat_min=0 & lat_max=5

;area_in     = origin_lat gt lat_min and origin_lat lt lat_max and $
              ;origin_lon gt lon_min and origin_lon lt lon_max
area_in     = origin_lat gt lat_min and origin_lat lt lat_max
        
cloud_in    = origin_cfrac gt cfrac_bnd
cloud_out   = origin_cfrac gt clear_bnd
cotnotbad   = cmodis_cot gt 0

highcot     = cmodis_cot gt 20
lowcot      = cmodis_cot lt 10

day_in      = origin_day eq dates[d]

clear_idx   = where(area_in and cloud_out and day_in,nclridx)
clear_to3   = origin_to3[clear_idx]
clear_cfrac = origin_cfrac[clear_idx]
clear_ctp   = origin_ctp[clear_idx]
clear_day   = origin_day[clear_idx]


; cloudy
;a_idx    = where(area_in and day_in and highcot and cloud_in,thena)
a_idx    = where(area_in and day_in and highcot ,thena)
a_to3   = origin_to3[a_idx]
a_cfrac = origin_cfrac[a_idx]
a_ctp   = origin_ctp[a_idx]
a_day   = origin_day[a_idx]

;b_idx    = where(area_in and day_in and highcot and cloud_in,thenb)
b_idx    = where(area_in and day_in and highcot ,thenb)
b_to3   = origin_to3[b_idx]
b_cfrac = origin_cfrac[b_idx]
b_ctp   = origin_ctp[b_idx]
b_day   = origin_day[b_idx]

;c_idx    = where(area_in and day_in and lowcot and cloud_in,thenc)
c_idx    = where(area_in and day_in and lowcot ,thenc)
c_to3   = origin_to3[c_idx]
c_cfrac = origin_cfrac[c_idx]
c_ctp   = origin_ctp[c_idx]
c_day   = origin_day[c_idx]

;d_idx    = where(area_in and day_in and highcot and cloud_in,thend)
;d_to3   = origin_to3[d_idx]
;d_cfrac = origin_cfrac[d_idx]
;d_ctp   = origin_ctp[d_idx]
;d_day   = origin_day[d_idx]

b_mto3   = cldmyd_to3[b_idx]
b_mcfrac = cldmyd_cfrac[b_idx]
b_mctp   = cldmyd_ctp[b_idx]
b_mday   = cldmyd_day[b_idx]

c_mto3   = cldmyd_to3[c_idx]
c_mcfrac = cldmyd_cfrac[c_idx]
c_mctp   = cldmyd_ctp[c_idx]
c_mday   = cldmyd_day[c_idx]

nclr=[nclr,nclridx]

na=[na,thena]
nb=[nb,thenb]
nc=[nc,thenc]
;nd=[nd,thend]

if (thenb gt 10 and thenc gt 10) then begin

  clear_boxarr= CREATEBOXPLOTDATA(clear_to3)
  ;a_boxarr= CREATEBOXPLOTDATA(a_to3)
  b_boxarr= CREATEBOXPLOTDATA(b_to3)
  c_boxarr= CREATEBOXPLOTDATA(c_to3)
  
  clear_min=[clear_min,clear_boxarr[0]]
  clear_lowqt=[clear_lowqt,clear_boxarr[1]]
  clear_med=[clear_med,clear_boxarr[2]]
  clear_upqt=[clear_upqt,clear_boxarr[3]]
  clear_max=[clear_max,clear_boxarr[4]]

  b_min=[b_min,b_boxarr[0]]
  b_lowqt=[b_lowqt,b_boxarr[1]]
  b_med=[b_med,b_boxarr[2]]
  b_upqt=[b_upqt,b_boxarr[3]]
  b_max=[b_max,b_boxarr[4]]

  c_min=[c_min,c_boxarr[0]]
  c_lowqt=[c_lowqt,c_boxarr[1]]
  c_med=[c_med,c_boxarr[2]]
  c_upqt=[c_upqt,c_boxarr[3]]
  c_max=[c_max,c_boxarr[4]]

endif

endfor

;stop    ; pixel selection check
thedays=where(nb gt 10 and nc gt 10,nplot)

win1=window(window_title='Clear vs hcot, lcot',dimensions=[1000,600])

boxinput1=[[clear_min],[clear_lowqt],[clear_med],[clear_upqt],[clear_max]]
boxinput2=[[b_min],[b_lowqt],[b_med],[b_upqt],[b_max]]
boxinput3=[[c_min],[c_lowqt],[c_med],[c_upqt],[c_max]]
position=[0.079,0.1,0.93,0.85]

; Generate the Date/Time data
time = TIMEGEN(ndates,START=JULDAY(6,1,2005))
caldat,time,mon,day,year

xlabel=strtrim(mon,2)+'.'+strtrim(day,2)
xlabel=xlabel[thedays]

xrange=[-1,nplot] & yrange=[220,260]

boxes1=boxplot(boxinput1,fill_color='linen',background_color='white',/current,thick=2,font_size=16,$ 
              xrange=xrange,yrange=yrange,width=0.05,pos=position,xstyle=1)  ;,layout=[2,1,1])
boxes1.xtransparency=100
boxes1.ytransparency=100
boxes1.position= boxes1.position+[-0.015,0,-0.015,0]

boxes2=boxplot(boxinput2,fill_color='royal blue',background_color='white',/current,thick=2,font_size=16,$
              xrange=xrange,yrange=yrange,width=0.05,pos=position,xstyle=1)  ;,layout=[2,1,1])
boxes2.xtransparency=100
boxes2.ytransparency=100
boxes2.position= boxes2.position+[+0.01,0,+0.01,0]

boxes3=boxplot(boxinput3,fill_color='light sky blue',background_color='white',/current,thick=2,font_size=16,$
              xrange=xrange,yrange=yrange,width=0.05,pos=position,xstyle=1)  ;,layout=[2,1,1])
boxes3.xtransparency=100
boxes3.ytransparency=100
;boxes3.position= boxes3.position+[+0.01,0,+0.01,0]

nodata=boxplot(boxinput1,xtitle="Days",ytitle='Total Ozone (DU)',title='Total ozone statistics, clear vs cloudy',$
              fill_color='linen',background_color='white',/current,thick=2,font_size=16,                    $ 
              xrange=xrange,yrange=yrange,width=0.1,pos=position,xstyle=1,/nodata)  ;,layout=[2,1,1])

ax0=nodata.axes[0]
ax0.tickvalues=indgen(nplot)
ax0.tickname=xlabel
ax0.minor=0
ax0.ticklen=0.02
nodata.axes[2].hide=1
title=nodata.title
title.font_size=25
nodata.axes[3].hide=1

title=nodata.title
title.font_size=25
;plt = plot(diff,/current,pos=position+[+0,+0.2,+0,-0.2],xtransparency=100,ytransparency=100,yrange=[0,5],xrange=xrange,$
           ;thick=2,symbol=6,sym_filled=1,color='brown')
;yax=axis('y',location='right',title='Difference of means, clear pixel - cloudy pixel (DU)',axis_range=[0,5],$
          ;target=plt,tickfont_size=16);$
          

text0 = Text(0.65, 0.25, 'Linen : clear (cfrac < 0.2)', VERTICAL_ALIGNMENT=0.00000,font_size=16)
text1 = Text(0.65, 0.2, 'Dark blue : high COT (COT > 20)', VERTICAL_ALIGNMENT=0.00000,font_size=16)
text2 = Text(0.65, 0.15, 'Light blue : low COT (COT < 10)', VERTICAL_ALIGNMENT=0.00000,font_size=16)


fn=respath+'boxplot_onlycot.png'
win1.save,fn,resolution=100
;win1.close
hs_filetrans,fn
endif ;end option

if option eq 2 then begin

; ---- TEST PLOT option ----

;+---------------------------------------------------------------------------+
; histogram check 
;+---------------------------------------------------------------------------+

;hist=histogram(origin_cfrac,locations=xbin,binsize=0.05)
;phist=plot(xbin,hist,max_value=4e5,/current)
;phist.save,respath+'histogram+of+cfrac.png',resolution=100
;hs_filetrans,respath+'histogram+of+cfrac.png'
;    min:-0.05, low quartile:0.02, median:0.7, high quartile:0.2, max:1

;
;hist=histogram(cmodis_cot[where(cotnotbad)],locations=xbin)
;phist=plot(xbin,hist,max_value=3e4)
;phist.save,respath+'histogram+of+cot.png',resolution=100
;hs_filetrans,respath+'histogram+of+cot.png'


xrange=[220,260]
binsize=1
;+---------------------------------------------------------------------------+
; TOMS VE OEM, for clear
;
histwin1=window(window_title='hist',dim=[1000,600])
;hist1=histogram(origin_to3[where(origin_cfrac lt 0.2)],locations=xbin1,binsize=2)
;hist2=histogram(toms_to3[where(origin_cfrac lt 0.2)],locations=xbin2,binsize=2)
hist1=histogram(good_clr_oem,locations=xbin1,binsize=binsize)
hist2=histogram(good_clr_toms,locations=xbin2,binsize=binsize)

phist1=plot(xbin1,hist1,max_value=200000,/current,thick=2,xrange=xrange)
phist2=plot(xbin2,hist2,max_value=200000,/overplot,color='red',thick=2)
;phist2=plot(xbin2,hist2,/overplot,color='red',thick=2)

text0 = Text(0.65, 0.65, 'RED: TOMS clear', VERTICAL_ALIGNMENT=0.00000,font_size=16)
text1 = Text(0.65, 0.70, 'BLACK: OEM clear', VERTICAL_ALIGNMENT=0.00000,font_size=16)

fn=respath+'histogram+of+oem+to3_clr.png'
histwin1.save,fn,resolution=100
hs_filetrans,fn

;+---------------------------------------------------------------------------+
; TOMS VE OEM, for cloudy
;
histwin2=window(window_title='hist',dim=[1000,600])

;hist1=histogram(origin_to3[where(origin_cfrac gt 0.5)],locations=xbin1,binsize=2)
;hist2=histogram(toms_to3[where(origin_cfrac gt 0.5)],locations=xbin2,binsize=2)
hist1=histogram(good_cld_oem,locations=xbin1,binsize=binsize)
hist2=histogram(good_cld_toms,locations=xbin2,binsize=binsize)

phist1=plot(xbin1,hist1,max_value=200000,/current,thick=2,xrange=xrange)
phist2=plot(xbin2,hist2,/overplot,color='red',thick=2)
text0 = Text(0.65, 0.65, 'RED: TOMS cloudy', VERTICAL_ALIGNMENT=0.00000,font_size=16)
text1 = Text(0.65, 0.70, 'BLACK: OEM cloudy', VERTICAL_ALIGNMENT=0.00000,font_size=16)

fn=respath+'histogram+of+oem+to3_cld.png'
histwin2.save,fn,resolution=100
hs_filetrans,fn


;+---------------------------------------------------------------------------+
; TOMS clear vs cloudy
;
histwin3=window(window_title='hist',dim=[1000,600])

;hist1=histogram(toms_to3[where(origin_cfrac lt 0.2)],locations=xbin1,binsize=2)
;hist2=histogram(toms_to3[where(origin_cfrac gt 0.5)],locations=xbin2,binsize=2)
hist1=histogram(good_clr_toms,locations=xbin1,binsize=binsize)
hist2=histogram(good_cld_toms,locations=xbin2,binsize=binsize)

phist1=plot(xbin1,hist1,max_value=200000,/current,thick=2,xrange=xrange)
phist2=plot(xbin2,hist2,/overplot,color='gray',thick=2)

text0 = Text(0.65, 0.65, 'GRAY: TOMS cloudy', VERTICAL_ALIGNMENT=0.00000,font_size=16)
text1 = Text(0.65, 0.70, 'BLACK: TOMS clear', VERTICAL_ALIGNMENT=0.00000,font_size=16)

fn=respath+'histogram+of+toms.png'
histwin3.save,fn,resolution=100
hs_filetrans,fn

;+---------------------------------------------------------------------------+
; OEM cloudy vs clear
;
histwin4=window(window_title='hist',dim=[1000,600])

;hist1=histogram(origin_to3[where(origin_cfrac lt 0.2)],locations=xbin1,binsize=2)
;hist2=histogram(origin_to3[where(origin_cfrac gt 0.5)],locations=xbin2,binsize=2)
hist1=histogram(good_clr_oem,locations=xbin1,binsize=binsize)
hist2=histogram(good_cld_oem,locations=xbin2,binsize=binsize)

phist1=plot(xbin1,hist1,max_value=200000,/current,thick=2,xrange=xrange)
phist2=plot(xbin2,hist2,/overplot,color='gray',thick=2)

text0 = Text(0.65, 0.65, 'GRAY: OEM cloudy', VERTICAL_ALIGNMENT=0.00000,font_size=16)
text1 = Text(0.65, 0.70, 'BLACK: OEM clear', VERTICAL_ALIGNMENT=0.00000,font_size=16)

fn=respath+'histogram+of+oem.png'
histwin4.save,fn,resolution=100
hs_filetrans,fn




;+---------------------------------------------------------------------------+
; scatterplot, TOMS ctp vs OEM ctp check 
;+---------------------------------------------------------------------------+

;scat=scatterplot(toms_ctp,origin_ctp,symbol='dot',xrange=[100,1100],yrange=[100,1100])
;scat.axes[0].title='TOMS CTP'
;scat.axes[1].title='OEM CTP'
;fn = respath+'TOMS+vs+OEM+CTP+check.png'
;scat.save, fn,resolution=100
;hs_filetrans,fn

;+---------------------------------------------------------------------------+
; Fail plot,,,, 
;+---------------------------------------------------------------------------+

;win1=window(window_title='Scatter plot',dim=[1000,600])

;;plt = plot(x,y,thick=2,title='title',/current)
;nz=n_elements(uniqz)
;nts=[]
;;barcol=reverse(fix(findgen(nz-1)*200/(nz-2)))

;for i=0,n_elements(nz)-1 do begin
;target=where(z eq uniqz[i],nt)
;scatx=x[target]
;scaty=y[target]
;plt1=scatterplot(scatx,scaty,symbol='plus',sym_thick=2,sym_color='red',/current,xtransparency=100,ytransparency=100)
;plt1.axes[0].title='cloud optical thickness'
;plt1.axes[1].title='total column ozone (DU)'
;nts=[nts,nt]
;endfor

;plt2=scatterplot(x,y,symbol='plus',sym_color='red',/current,/nodata,xrange=[min(x),max(x)],yrange=[min(y),max(y)])
;plt2.axes[0].title='cloud optical thickness'
;plt2.axes[1].title='total column ozone (DU)'

;;text0 = Text(0.65, 0.2, 'Gray : cloudy pixel (cfrac > 0.8)', VERTICAL_ALIGNMENT=0.00000,font_size=16)
;;text1 = Text(0.65, 0.15, 'Linen : clear pixel (cfra < 0.2)', VERTICAL_ALIGNMENT=0.00000,font_size=16)

;fn=respath+'scat_cot+ctp+diff.png'
;win1.save,fn,resolution=100
;;win1.close
;hs_filetrans,fn

endif ;end option


if not loop then stop

end
