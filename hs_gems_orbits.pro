pro hs_gems_orbits,inputday,xdrmod=xdrmod
;+---------------------------------------------------------------------------+
;
; INPUT : date, 8 integers(yyyymmdd)
; OUTPUT: 
;
;+---------------------------------------------------------------------------+
if not keyword_set(xdrmod) then xdrmod=0
if not keyword_set(inputday) then inputday=20080613

inputday = strtrim(inputday,2)

unit_of_o3 = '(DU)' ; '(ppb)'
respath='/home/o3p_hs/results/'
outpath='/home/o3p_hs/GEMS/o3p/dat/out/'
pixpath='/home/Data/OMI/2_OML2PIXCOR/2008/'
orbits=[] & pixcor_files=[]
lat=[] & lon =[] & year=[]  & mon  =[]
day=[] & utc =[] & orbs =[] & pixel=[] & tline=[] & line =[]

gems_files    = file_search(outpath+'*'+strmid(inputday,0,4)+'m'+strmid(inputday,4,4)+'*') ;day
fnum=n_elements(gems_files)

for i=0,fnum-1 do begin   ;read pixcor files, but not used for now.
  orbit=strmid((strsplit(gems_files[i],'_',/ext))[-4],1,5)
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
 

for i=0,fnum-1 do begin
  hs_read_gems,gems_files[i],gems,xdrmod=xdrmod
  lat   = [lat  ,gems.lat]   & lon  =[lon  ,gems.lon]
  year  = [year ,gems.year]  & mon  =[mon  ,gems.mon]
  day   = [day  ,gems.day]   & utc  =[utc  ,gems.utc]
  orb   = intarr(n_elements(gems.day)) & orb[*] = orbits[i] & orbs = [orbs,orb]  

endfor

data={ lat:lat,lon:lon,year:year,mon:mon,day:day,utc:utc,orbit:orbs,pixel:pixel,line:line}

;+---------------------------------------------------------------------------+
;
; Plot on map 
;
;+---------------------------------------------------------------------------+

mapwin  = window(dimensions=[800,800])
limit = [20,100,45,140]
m       = map('equidistant conic',limit=limit,/current)
norb = size(orbits,/n_elements)

for i=0,norb-1 do begin
  orbitind  = data.orbit eq orbits[i]
  symcol    = intarr(n_elements(orbitind))
  symcol[*] = i
  lon       = data.lon[where(orbitind)]
  lat       = data.lat[where(orbitind)]
  orbit     = data.orbit[where(orbitind)]
  mapcont   = scatterplot(lon,lat,/sym_filled,rgb_table=7,symbol='circle',sym_size=0.7, $
                          magnitude=bytscl(symcol,min=0,max=norb-1),overplot=m)
  mc        = MAPCONTINENTS()
endfor
stop
end
