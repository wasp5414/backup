pro hs_gems_findctp,loop=loop,xdrmod=xdrmod,which_plot=which_plot,which_layer=which_layer

;+---------------------------------------------------------------------------+
; RUN : hs_gems_findctp,xdrmod=1
; in first1 run, xdrmod must be 0
;
; INPUT : ORBIT NUMBER
; OUTPUT: ADJUST CLOUD TOP PRESSURE FIELD
;
; with ctp control run, find effective cloud top pressure
; *****NOT FINISHED*****
;+---------------------------------------------------------------------------+

if not keyword_set(xdrmod) then xdrmod=0
if not keyword_set(loop) then loop=0 ; stop at first loop
if not keyword_set(which_plot) then which_plot=0 

; 0 : ORIGINAl scatterplot
; 1 : ORIGINAL densityplot
; 2 : CTP control run compare

if not keyword_set(which_layer) then which_layer=0

unit_of_o3 = '(DU)' ; '(ppb)'
respath   ='/home/o3p_hs/results/'
;outpath   ='/home/o3p_hs/GEMS/o3p/dat/out/backup/2005/' ; for test
outpath   ='/home/o3p_hs/GEMS/o3p/dat/out/'
pixpath   ='/home/Data/OMI/2_OML2PIXCOR/2005/'
myd06path = '/home/Data2/5_MODIS/MYD06/'
myd03path = '/home/Data2/5_MODIS/MYD06/'
xdrpath   = '/home/o3p_hs/data/xdr/'

types   = []
fdates   = [] & forbits=[]
gems_files  = file_search(outpath+'GEM_*')
fnum=n_elements(gems_files)

;+---------------------------------------------------------------------------+
;  GEMS O3P DATA & OMI PIXCOR FILE READ LOOP
;+---------------------------------------------------------------------------+

for i=0,fnum-1 do begin
  fdates = [fdates,strmid((strsplit(gems_files,'_',/ext))[i,6],0,8)]
  forbits = [forbits,(strsplit(gems_files,'_',/ext))[i,8]]
  type  = (strsplit(gems_files[i],'/',/ext))[-1]
  type  = (strsplit(type,'.',/ext))[0]
  type  = (strsplit(type,'_',/ext))[1]
  types = [types,type]
  orbits= forbits[uniq(forbits,sort(forbits))]
endfor

dates   = fdates[uniq(fdates,sort(fdates))]
ndates  = n_elements(dates)
ntypes  = n_elements(types[uniq(types)])

which_day=0 & which_orbit=0
layertags=['tc','st','tp']
which_file=where(forbits eq orbits[which_orbit])
pixcor_file=file_search(pixpath+'*'+'o'+orbits[which_orbit]+'*')
hs_omi_pixcor,pixcor_file,pixcor,xdrmod=xdrmod

files=gems_files[which_file]
ftypes=types
types=ftypes[which_file]

for i=0,n_elements(types)-1 do begin
  type=types[i]
  str='gems_'+type
  void=execute('hs_read_gems,files[i],'+str+',xdrmod=xdrmod')
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
  void=execute(str+'.utc=bad2nan('+str+'.utc)')
  void=execute(str+'.mon=bad2nan('+str+'.mon)')
  void=execute(str+'.lat=bad2nan('+str+'.lat)')
  void=execute(str+'.lon=bad2nan('+str+'.lon)')
endfor

;+------------------------------------------------------------------------+
; ### FILTERING GROUP ###
;+------------------------------------------------------------------------+
data=gems_origin
titles=['Total','spres~800','800~600','600~450','450~350','350~']
group_total = where(data.ctp gt 0.)
group_spres = where(data.ctp le max(data.ctp) and data.ctp gt 800.)
group_700 = where(data.ctp le 800. and data.ctp gt 600.)
group_500 = where(data.ctp le 600. and data.ctp gt 450.)
group_400 = where(data.ctp le 450. and data.ctp gt 350.)
group_top = where(data.ctp le 350. )
groups=['group_total','group_spres','group_700','group_500', $
        'group_400'  ,'group_top']
ymin=min(gems_origin[*].co3[which_layer])-10
ymax=max(gems_origin[*].co3[which_layer])+20


case which_plot of
;+---------------------------------------------------------------------------+
  0 : begin ;ORIGIN scatter plot

  data=gems_origin

;+---------------------------------------------------------------------------+
; ### SCATTER PLOT ### 
;+---------------------------------------------------------------------------+
  margin=[0.15,0.15,0.15,0.15]
  props={current:1,axis_style:2,margin:margin,font_size:16, $
  symbol:'dot',sym_filled:1,sym_size:2,sym_thick:2, $
  sym_color:'black',yrange:[ymin,ymax],buffer:1}
  scat_win=window($
  window_title='Scatter plots',dimensions=[1000,600],/buffer)
  for j=0,5 do begin
    void=execute('which_point = '+groups[j])

    scatx1 = data[which_point].cfrac
    scaty1 = data[which_point].co3[which_layer]

    scatp=scatterplot(scatx1,scaty1, $
    title=titles[j],layout=[3,2,j+1],_extra=props)
  endfor

  scatpfign=respath+layertags[which_layer]+'_scatp_origin.png'
  scat_win.save,scatpfign,resolution=100
  hs_filetrans,scatpfign
  if not loop then stop
  scat_win.close
end
;+---------------------------------------------------------------------------+
  1 : begin ;ORIGIN density plot

  data=gems_origin

;+------------------------------------------------------------------------+
;### DENSITY PLOT ###
;+------------------------------------------------------------------------+
  ct=colortable(33)
  ct[0,*]=[255,255,255]
  margin=[0.15,0.15,0.15,0.1]

  dens_win=window($
  window_title='Dense plots',dimensions=[1000,600],/buffer)
  props={current:1,rgb:ct,axis_style:2,margin:margin,font_size:16, $
  xtickdir:1,ytickdir:1,xshowtext:0,yshowtext:0,buffer:1}

  for j=0,5 do begin
    void=execute('which_point='+groups[j])
    scatx1=data[which_point].cfrac
    scaty1=data[which_point].co3[which_layer]
    xrange1=1. - 0 & yrange1=ymax - ymin
    bin1=xrange1/50 & bin2=yrange1/50

    h2d=hist_2d( scatx1,scaty1, $
    bin1=bin1,bin2=bin2, $
    min2=ymin,min1=0,max2=ymax,max1=1 )

    if j eq 0 then maxh2d=max(h2d)
    pl=image(h2d,layout=[3,2,j+1],title=titles[j],_extra=props,max=100)
    if (j ge 3) then begin
      pl.position=[pl.position[0],pl.position[1]+0.06 $
      ,pl.position[2],pl.position[3]+0.06]
    endif
  endfor


  cb=colorbar(target=pl,title='test cb',POSITION = [0.1,0.08,0.9,0.11] $
  ,border_on=1,orientation=0,textpos=0,font_size=16)

  denspfign=respath+layertags[which_layer]+'_densp_origin.png'
  dens_win.save,denspfign,resolution=100
  hs_filetrans,denspfign
  if not loop then stop
  dens_win.close
end

;+---------------------------------------------------------------------------+
  2 : begin ; CTP control run vs ORIGIN scatter & density plot

  types=types[0:-2] & groups=groups[-1:1:-1]
  ;ORIGIN type & TOTAL group excluded
  
  ymin=min(gems_origin[*].co3[which_layer])-10
  ymax=max(gems_origin[*].co3[which_layer])+20

  for i=0,n_elements(types)-1 do begin
    void=execute('data=gems_'+types[i]) ;200,300,400,500,700,ORIGIN
    titles=['Origin(total)','CTP control '+types[i],'Origin(filtered)', $
            'Origin(total)','CTP control '+types[i],'Origin(filtered)']

;+------------------------------------------------------------------------+
;### DENSITY & SCATTER PLOT ###
;+------------------------------------------------------------------------+

    ct=colortable(33)
    ct[0,*]=[255,255,255]
    margin=[0.15,0.15,0.15,0.1]

    comp_win=window($
    window_title='Compare data',dimensions=[1000,600],/buffer)
    dprops={current:1,rgb:ct,axis_style:2,margin:margin,font_size:16, $
    xtickdir:1,ytickdir:1,xshowtext:0,yshowtext:0,buffer:1}

    sprops={current:1,axis_style:2,margin:margin,font_size:16, $
    symbol:'dot',sym_filled:1,sym_thick:2,sym_size:2,sym_color:'black', $
    xtickname:['0','','','','','1'],yrange:[ymin,ymax],buffer:1 }

    datalist=['gems_origin[group_total]','gems_'+types[i]+'[group_total]', $
              'gems_origin[ '+groups[i]+' ]']

    for j=0,2 do begin
      void=execute('data='+datalist[j])
      scatx1=data.cfrac
      scaty1=data[*].co3[which_layer]
      ;void=execute('which_point=group'+strtrim(j,2))
      ;xrange1=max(scatx1)-min(scatx1) & yrange1=max(scaty1)-min(scaty1)
      xrange1=1.-0 & yrange1=ymax-ymin
      bin1=xrange1/50 & bin2=yrange1/50

      h2d=hist_2d( scatx1,scaty1, $
      bin1=bin1,bin2=bin2,min2=ymin,min1=0,max2=ymax,max1=1 )

      if j eq 0 then maxh2d=max(h2d) ; set maximum,but not use for weak signal

      pl=image(h2d,layout=[3,2,j+4],title=titles[j+3],_extra=dprops,max=100)
      pl.position=[pl.position[0],pl.position[1]+0.06 $
      ,pl.position[2],pl.position[3]+0.06]

      scatp=scatterplot(scatx1,scaty1, $
      title=titles[j],layout=[3,2,j+1],_extra=sprops)
    endfor

    cb=colorbar(target=pl,title='test cb',POSITION = [0.1,0.08,0.9,0.11] $
    ,border_on=1,orientation=0,textpos=0,font_size=16)

    compfign=respath+'compare_'+layertags[which_layer]+types[i]+'.png'
    comp_win.save,compfign,resolution=100
    hs_filetrans,compfign
    if not loop then stop
    comp_win.close
  endfor
end

endcase

end
