pro temp

if option eq 2 then begin

lon_max1=-155 & lon_min1=-160 & lat_min1=0  & lat_max1=5
lon_max2=-160 & lon_min2=-165 & lat_min2=5  & lat_max2=10
lon_max3=-165 & lon_min3=-170 & lat_min3=10 & lat_max3=15
lon_max4=-170 & lon_min4=-175 & lat_min4=15 & lat_max4=20

lon_maxs=[lon_max1,lon_max2,lon_max3,lon_max4]
lon_mins=[lon_min1,lon_min2,lon_min3,lon_min4]
lat_maxs=[lat_max1,lat_max2,lat_max3,lat_max4]
lat_mins=[lat_min1,lat_min2,lat_min3,lat_min4]


for j=0,3 do begin
for i=0,3 do begin
;i=0 & j=3
  theO2_idx=where(origin_lat gt lat_mins[j] and origin_lat lt lat_maxs[j] and $
                  origin_lon gt lon_mins[i] and origin_lon lt lon_maxs[i])

  theO2_to3=origin_to3[theO2_idx]
  theO2_cfrac=origin_cfrac[theO2_idx]
  theO2_cfrac=origin_cfrac[theO2_idx]
  theO2_cfrac=origin_cfrac[theO2_idx]
  theO2_cfrac=origin_cfrac[theO2_idx]
  theO2_ctp=origin_ctp[theO2_idx]

  cor_O2=correlate(theO2_cfrac,theo2_to3,/double)
  cor_myd=correlate(themyd_cfrac,themyd_to3,/double)
  ct=colortable(33)
  ct[0,*]=[255,255,255]
  margin=[0.2,0.2,0.2,0.1]

  props={current:1,rgb:ct,axis_style:2,margin:margin,font_size:16,$
         xtickdir:1,ytickdir:1,xshowtext:0,yshowtext:0}

  ;ymax=max(themyd_to3)+20
  ;ymin=min(themyd_to3)-10
  ymax=310 & ymin=200

  xrange1=1.-0 & yrange1=ymax-ymin
  bin1=xrange1/100 & bin2=yrange1/100
  xticks=string(findgen(11)*0.1,format='(f3.1)') & yticks=string(findgen(10)*10+220,format='(i3)')

  title='Lon : '+strtrim(lon_mins[i],2)+', '+strtrim(lon_maxs[i],2)+$
       ' Lat : '+strtrim(lat_mins[j],2)+', '+strtrim(lat_maxs[j],2)

  h2d1=hist_2d(theO2_cfrac,theO2_to3,bin1=bin1,bin2=bin2,min2=ymin,min1=0,max2=ymax,max1=1)
  pl1=image(h2d1,layout=[4,4,i+j*4+1],title=title,_extra=props,max=100)
  pl1.axes[2].major=0 & pl1.axes[2].minor=0
  pl1.axes[3].major=0 & pl1.axes[3].minor=0
  pl1.axes[0].tickname=xticks & pl1.axes[1].tickname=yticks
  if not j then  pl1.position=pl1.position+[0,0.15,0,0.15]

  cb=colorbar(target=pl,title='Number of pixel',position=[0.1,0.08,0.9,0.11],border_on=1,orientation=0,textpos=0,font_size=16)
  txpos1=(pl1.position[0]+pl1.position[2])*0.5 & typos1=(pl1.position[3]-pl1.position[1])*0.2+pl1.position[1]
  show_cor1=text(txpos1,typos1,'Corr = '+strtrim(cor_O2,2),font_size=16,alignment=0.5)
  ;axpo1=txpos1-0.1 & aypo=(pl1.position[3])*0.4
  ;show_lon=text(axpo1,aypo,'Longitude range : '+strtrim(lon_min,2)+', '+strtrim(lon_max,2),font_size=16,alignment=0 )
  ;show_lat=text(axpo1,aypo-0.05,'Latitude range : '+strtrim(lat_min,2)+', '+strtrim(lat_max,2),font_size=16,alignment=0)

endfor
endfor

;fn=respath+strtrim(abs(lon_max)+abs(lat_max))+'O2_vs_MYD_with cfrac.png'
;win.save,fn,resolution=100
;win.close
;hs_filetrans,fn
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
