pro hs_scatterplot_cfrac_tco3,inputday,xdrmod=xdrmod
;+---------------------------------------------------------------------------+
; RUN : hs_scatterplot_cfrac_tco3,xdrmod=1
; in first run, xdrmod must be 0
;
; INPUT : date, 8 integers(yyyymmdd)
; OUTPUT: 
;
;+---------------------------------------------------------------------------+
if not keyword_set(xdrmod) then xdrmod=0

inputday = strtrim(inputday,2)

unit_of_o3 = '(DU)' ; '(ppb)'
respath='/home/o3p_hs/results/'
outpath='/home/o3p_hs/GEMS/o3p/dat/out/'
pixpath='/home/Data/OMI/2_OML2PIXCOR/2008/'
orbits=[] & pixcor_files=[]
O3 =[] & alt =[] & pres =[] & avgk =[] & ao3  =[] & ao3e =[] & co3 =[]  & ctp  =[] 
lat=[] & lon =[] & sza  =[] & cfrac=[] & exval=[] & rms  =[] & year=[]  & mon  =[]
day=[] & utc =[] & orbs =[] & pixel=[] & tline=[] & line =[] & vza =[]

gems_files    = file_search(outpath+'*'+strmid(inputday,0,4)+'m'+strmid(inputday,4,4)+'*') ;day
;gems_files    = file_search(outpath+'*'+strmid(inputday,0,4)+'m'+strmid(inputday,4,2)+'*') ;month
;gems_files    = file_search(outpath+'GEM_*') ;all files
fnum=n_elements(gems_files)

for i=0,fnum-1 do begin   ;read pixcor files, but not used for now.
  orbit=strmid((strsplit(gems_files[i],'_',/ext))[-4],1,5)
  ;pixcor_files = file_search(pixpath+'*'+'o'+orbit+'*')
  pixcor_file   = file_search(pixpath+'*'+'o'+orbit+'*')
  pixcor_files  = [pixcor_files,pixcor_file]
  orbits        = [orbits,orbit]
  strpix     = strsplit((strsplit(gems_files[i],'_',/ext))[-3],'-',/ext)
  strline    = strsplit((strsplit(gems_files[i],'_',/ext))[-2],'-',/ext)
  strpix[0]  = strmid(strpix[0],1)  ;start pixel
  strline[0] = strmid(strline[0],1) ;start line
  fpix   = [ fix(strpix[0]),fix(strpix[1]) ] & fline = [ fix(strline[0]),fix(strline[1]) ]
  orbitline = indgen(fline[1] - fline[0] + 1) + fline[0]
  totalline = [ tline  , indgen(fline[1] - fline[0] + 1) + fline[0] ]

  for j=0,n_elements(orbitline)-1 do begin
    pixel  = [ pixel , indgen(fpix[1]  - fpix[0]  + 1) + fpix[0]  ]
    ints   = intarr(fpix[1]-fpix[0]+1) & ints[*] = orbitline[j]
    line   = [ line   , ints ]
  endfor

endfor

if not n_elements(pixcor_files) eq fnum then $
message,'   Numbers of corner files & data files do not match! download more pixcor files!'


for i=0,fnum-1 do begin
  hs_read_gems,gems_files[i],gems,xdrmod=xdrmod
  o3    = [o3   ,bad2nan(transpose(gems.o3))]    & alt   = [alt  ,bad2nan(transpose(gems.alt))]
  pres  = [pres ,bad2nan(transpose(gems.pres))]  & avgk  = [avgk ,bad2nan(transpose(gems.avgk))]
  ao3   = [ao3  ,bad2nan(transpose(gems.ao3))]   & ao3e  = [ao3e ,bad2nan(transpose(gems.ao3e))]
  co3   = [co3  ,bad2nan(transpose(gems.co3))]   & ctp   = [ctp  ,bad2nan(gems.ctp)]
  sza   = [sza  ,bad2nan(gems.sza)]              & cfrac = [cfrac,bad2nan(gems.cfrac)]
  vza   = [vza  ,bad2nan(gems.vza)]
  lat   = [lat  ,gems.lat]   & lon  =[lon  ,gems.lon]
  exval = [exval,gems.exval] & rms  =[rms  ,bad2nan(gems.rms)]
  year  = [year ,gems.year]  & mon  =[mon  ,gems.mon]
  day   = [day  ,gems.day]   & utc  =[utc  ,gems.utc]
  orb   = intarr(n_elements(gems.day)) & orb[*] = orbits[i] & orbs = [orbs,orb]
  
endfor

cfrac[where(cfrac lt 0.)]=0.

del_lat = lat[1:*]   - lat[0:-1]
del_lon = lon[1:*]   - lon[0:-1]
del_co3 = co3[1:*,2] - co3[0:-1,2]
del_cfrac = cfrac[1:*] - cfrac[0:-1]

orbit_end=where(del_lat gt 10. or del_lat lt -10.)
del_co3[orbit_end]=!values.f_nan
del_cfrac[orbit_end]=!values.f_nan

data={ O3 :o3 ,alt:alt,pres :pres ,avgk :avgk ,ao3:ao3,ao3e:ao3e,co3:co3,ctp:ctp,lat:lat,$
       lon:lon,sza:sza,vza:vza,cfrac:cfrac,exval:exval,rms:rms,year:year,mon:mon,day:day,utc:utc,orbit:orbs,pixel:pixel,line:line}

;+---------------------------------------------------------------------------+
;
; Track plot cloud fraction & troposphere column O3 & O3 profile
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
;+---------------------------------------------------------------------------+
;
; Scatterplot cloud fraction vs troposphere column O3
;
;+---------------------------------------------------------------------------+

fn = respath +strtrim(inputday,2) +'exp7.png'
win3 = window(dimensions=[1200,400])
scat_posi1=[0.075,0.15,0.325,0.90]
scat_posi2=[0.400,0.15,0.650,0.90]
scat_posi3=[0.725,0.15,0.975,0.90]

;group1 = data.co3[*,2] eq data.co3[*,2] ;exp1,5
;group1 = data.co3[*,2] gt 60 ;20080613exp3
;group1 = data.co3[*,2] gt 50 ;20080613exp3-2
;group1 = abs(del_co3) gt 5   ;20080613exp4
;group1 = data.co3[*,2] gt 50 ;20080614exp6
;group1 = data.co3[*,2] gt 60 ;20080614exp6-2
group1 = abs(del_co3) gt 5   ;20080614exp7
group2 = group1
;group3 = data.orbit eq orbits[1] and data.pixel eq 15 and group1 ;20080613exp1~4
group3 = data.orbit eq orbits[2] and data.pixel eq 15 and group1  ;20080614exp5~7

scatx1 = data.cfrac[where(group1)]
;scatx2 = abs(del_cfrac[where(group2)]) ;exp2
scatx2 = del_cfrac[where(group2)]
scatx3 = data.cfrac[where(group3)]

scaty1 = data.co3[where(group1),2]
;scaty2 = abs(del_co3[where(group2)]) ; exp2
scaty2 = del_co3[where(group2)]
scaty3 = data.co3[where(group3),2]

plot1  = scatterplot(scatx1,scaty1,     $
xstyle=2,ystyle=2,$;,xticklayout=1,yticklayout=1,$;xtickdir=2,ytickdir=2,$
symbol='dot',/sym_filled,sym_color='black',posi=scat_posi1,$
title='troposphere coloumn O3 vs cloud fraction',/current)

plot2  = scatterplot(scatx2,scaty2,     $
xstyle=2,ystyle=2,$;,xticklayout=1,yticklayout=1,$;xtickdir=2,ytickdir=2,$
symbol='dot',/sym_filled,sym_color='black',posi=scat_posi2,$
title='TCO3 diff vs cfrac diff',/current)

plot3  = scatterplot(scatx3,scaty3,     $
xstyle=2,ystyle=2,$;,xticklayout=1,yticklayout=1,$;xtickdir=2,ytickdir=2,$
symbol=1,sym_color='black',posi=scat_posi3,$
title='TCO3 vs cfrac on 1 track',/current)

plot1.xtitle='cloud fraction' & plot2.xtitle='cloud fraction difference' & plot3.xtitle='cloud fraction'
plot1.ytitle='Troposphere column O3(DU)' & plot2.ytitle='Troposphere column O3 difference(DU)' & plot3.ytitle='Troposphere column O3(DU)'
plot1.font_size=15 & plot2.font_size=15 & plot3.font_size=15

cor1 = correlate(scatx1[where(finite(scatx1))],scaty1[where(finite(scaty1))],/double)
cor2 = correlate(scatx2[where(finite(scatx2))],scaty2[where(finite(scaty2))],/double)
cor3 = correlate(scatx3[where(finite(scatx3))],scaty3[where(finite(scaty3))],/double)

text_xposi1 = (scat_posi1[0]+scat_posi1[2])*0.5
text_yposi1 = (scat_posi1[3])*0.9

text_xposi2 = (scat_posi2[0]+scat_posi2[2])*0.5
text_yposi2 = (scat_posi2[3])*0.9

text_xposi3 = (scat_posi3[0]+scat_posi3[2])*0.5
text_yposi3 = (scat_posi3[3])*0.9

show_cor1 = text(text_xposi1,text_yposi1,'correlation = '+strtrim(cor1,2),font_size=15,alignment=0.5) 
show_cor2 = text(text_xposi2,text_yposi2,'correlation = '+strtrim(cor2,2),font_size=15,alignment=0.5) 
show_cor3 = text(text_xposi3,text_yposi3,'correlation = '+strtrim(cor3,2),font_size=15,alignment=0.5) 

win3.save,fn
hs_filetrans,fn
print,fn
stop
end
