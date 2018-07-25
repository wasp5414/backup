pro hs_plot_base
;+---------------------------------------------------------------------------+
;
; INPUT : None
; OUTPUT: plot image and transfer to ftp server
;
;+---------------------------------------------------------------------------+

respath='/home/o3p_hs/results/'
one_array = indgen(100)
two_array = indgen(100,10)

;+---------------------------------------------------------------------------+
;
; 3 X 1 plot, profile along track 
; 
;+---------------------------------------------------------------------------+

; one-dimension array overplot on other scale one-dimension arry

posi1=[0.15,0.7,0.85,0.90]

win = window(dimensions=[900,950])
title = text(0.5,0.95,'Title',font_size=30,alignment=0.5)
fn = respath+'test_figure.png'
subtitle1 = text(0.25,posi1[3]+0.01,'Subtitle for plot 1',font_size=20,alignment=0.5)
layer1 = plot(one_array,xstyle=1,ystyle=0,thick=2,axis_style=4,posi=posi1,color='black',/current)
layer2 = plot(one_array*0.01+1,xstyle=1,ystyle=0,thick=2,axis_style=4,posi=posi1,color='deep_sky_blue',/current)

sub_yaxis = axis('y',location='right',color='black',target=layer2,title='Unit2')

frame = plot(one_array,xstyle=1,ystyle=0,posi=posi1, $
             ytitle='Unit1',thick=2,axis_style=1,/current,/nodata)

layer2.font_size=20
frame.font_size=20
frame.axes[0].show=0

posi2=[0.15,0.45,0.85,0.65]
posi3=[0.15,0.2,0.85,0.4]

win.save,fn
hs_filetrans,fn

;stop

;+---------------------------------------------------------------------------+
;
; Plot on map 
;
;+---------------------------------------------------------------------------+

;group   = data.orbit eq orbits[0]
;mapdata = data.co3[where(group),2]
;xx      = data.lon[where(group)]
;yy      = data.lat[where(group)]

;limit = [20,100,45,140]
;mapwin  = window(dimensions=[800,800])
;m       = map('equidistant conic',limit=limit)
;mapcont = scatterplot(mapdata,xx,yy,/fill,overplot=m)
;mc      = MAPCONTINENTS()

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
