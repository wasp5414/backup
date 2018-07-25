pro hs_gems_track,day,xdrmod=xdrmod,
;+---------------------------------------------------------------------------+
; INPUT   : 8 integers(yyyymmdd), 1 integer (1:restore xdr,0:read h5)
; OUTPUT  : None, plot track on map and save image
;+---------------------------------------------------------------------------+

if not keyword_set(xdrmod) then xdrmod=0

ymd=strtrim(day,2)
outpath='/home/o3p_hs/GEMS/o3p/dat/out/'
xdrpath='/home/o3p_hs/GEMS/o3p/dat/out/xdr'

gems_files=file_search(datdir+strmid(day,0,4)+'m'+strmid(day,4,4)+'*',count=nfiles)
respath='/home/o3p_hs/results/'
prefix=respath+'test_plottrack_'+ymd
ps=prefix+'.ps'
png=prefix+'.png'

prefix=respath+'test_map_l2_'+name2[type]+'_'+day

ps=prefix+'.ps'
png=prefix+'.png'
cgps_open,ps,xsize=10,ysize=10,/nomatch,/inch

loadct_wb,0
limit=[20,100,45,140]
pos=[0.05,0.05,0.82,0.83]

map_set,limit=limit,xmargin=0,ymargin=0,/cylin,/noera,/nobor,position=multi_pos1

level=[-999,findgen(21)*2.5+250,1000]
ncol=n_elements(level)-1
color=fix(findgen(ncol)*250/ncol)
color[0]=254

loadct,33
for i =0,n-1 do begin

  orbit=strmid((strsplit(files[-1],'/',/ext))[-1],26,5)
  read_gems_l2o3,files[i],gems,ngood
  op = lonarr(size(gems.co3[0],/dim))

  for k=0,ncol-1 do begin
    op[where(gems.co3[0] gt level[k] and gems.co3[0] lt level[k+1])]=color[k]
  endfor
    
  for p=0,(size(gems.co3[0],/dim))[0]-1 do begin
    if gems[p].co3[0] eq -999. then continue
    ;oplot,[gems[p].lon],[gems[p].lat],psym=cgsymcat(15),symsize=0.7,color=op[p]
    plots,gems[p].lon,gems[p].lat,psym=cgsymcat(15),symsize=0.7,color=op[p]
  endfor


endfor

loadct,0
tvlct,cgcolor('white',/triple),254
tvlct,cgcolor('black',/triple),255
map_continents,/continent,/count,/country,/hires
tmp=fltarr(2,2)
contour,tmp,tmp,tmp,/nodat,/noera,charsize=2,charthick=2,xstyle=1,ystyle=1,ytickformat='(i3)',xtickformat='(i4)',$
        xrange=[limit(1),limit(3)],yrange=[limit(0),limit(2)],xtitle='longitude',ytitle='latitude'

cgps_close,density=800,/png

hs_transfile,figname
end
