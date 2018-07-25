PRO hs_h4getline, datelist

;+---------------------------------------------------------------------------+
; INPUT  : date list, 1 date= 8 size strings, 'yyyymmdd'
; OUTPUT : 100-140 lon, 15-45 lat area crossing orbit information, struct
;          (info : filename, orbit number, pixel range, line range)
;+---------------------------------------------------------------------------+

res=[]
orbs=[]
maxpixs=[]
minpixs=[]
toplines=[]
botlines=[]
lfns=[]
lon_min=-180 & lon_max=-140 ; won't be applied for TEST_OPTION=1
lat_min=-20  & lat_max=20
datelen=size(datelist,/dim)
test_option=0

for i=0,datelen[0]-1 DO BEGIN
  print,'Current date :   ',datelist[i]
  year=STRMID(datelist[i],0,4)
  month=STRMID(datelist[i],4,2)
  day=STRMID(datelist[i],6,2)
  dir='/DB/Data/OMI/1_OML1BRUG/'+year+'/'
  dfiles=FILE_SEARCH(dir+'OMI-Aura_L1-OML1BRUG_'+year+'m'+month+day+'*.he4',COUNT=dn)
  mfiles=FILE_SEARCH(dir+'OMI-Aura_L1-OML1BRUG_'+year+'m'+month+'*.he4',COUNT=mn)
  yfiles=FILE_SEARCH(dir+'OMI-Aura_L1-OML1BRUG_'+year+'m'+'*.he4',COUNT=yn)
  
  ;run for days
  FOR j=0,dn-1 DO BEGIN
    IF test_option THEN BEGIN
      fn    = dfiles[j]
      tt=fix(strmid(fn,60,2))

      ; full swath extract, select with orbit time
      if (tt gt 20 and tt lt 24) then begin
        swath = 'Earth UV-1 Swath'
        stat1 = omi__load_hdf_data(fn,swath,'Time',datelist[i])
        stat2 = omi__load_hdf_data(fn,swath,'Latitude',lat)
        stat3 = omi__load_hdf_data(fn,swath,'Longitude',lon)
        orb = strmid((strsplit(fn,'/',/ext))[-1],37,5)
        nline=n_elements(lat)/30
        for k=0,nline-1 do begin
          if fix(max(lat[*,k])) eq lat_min then botline=k
        endfor
        for k=nline-1,0,-1 do begin
          if fix(min(lat[*,k])) eq lat_max then topline=k
        endfor
        tmp_res = {filename:fn,orb:orb,pixrange:[1,30],linerange:[botline,topline]}
        res = [res,tmp_res]
      endif

    ENDIF ELSE BEGIN
      fn    = dfiles[j]
      swath = 'Earth UV-1 Swath'
      stat1 = omi__load_hdf_data(fn,swath,'Time',datelist[i])
      stat2 = omi__load_hdf_data(fn,swath,'Latitude',lat)
      stat3 = omi__load_hdf_data(fn,swath,'Longitude',lon)
      stat4 = omi__load_hdf_data(fn,swath,'SolarZenithAngle',sza)
      idx = WHERE(lon GE lon_min AND lon LE lon_max AND $
                  lat GE lat_min AND lat LE lat_max, nidx)
      if nidx ne 0 then begin
        idx_pos = array_indices(lon,idx)
        pix = reform(idx_pos[0,*]+1)
        line = reform(idx_pos[1,*]+1)
        orb = strmid((strsplit(fn,'/',/ext))[-1],37,5)
        lfns = [lfns,fn]
        orbs = [orbs,orb]
        minpixs = [minpixs,min(pix)]
        maxpixs = [maxpixs,max(pix)]
        botlines = [botlines,min(line)]
        toplines = [toplines,max(line)]
        tmp_res = {filename:fn,orb:orb,pixrange:[min(pix),max(pix)],$
                   linerange:[min(line),max(line)]}
        res = [res,tmp_res]
        print, F='(i3,A71,A10)',j,STRMID(fn,70,71,/rev), ' o'+orb
      endif

    ENDELSE
  ENDFOR
ENDFOR


;stop 
make_list=1
if make_list eq 1 then begin
nres=n_elements(res)
openw,lun,'runlist_week2.list',/get_lun
for i=0,nres-1 do begin
  if test_option then begin
  printf,lun,strmid((strsplit(res[i].filename,'/',/extr))[-1],21,14), $
                     res[i].linerange[0],res[i].linerange[1],        $
                     1,30,res[i].orb,format='(A12,4I5,A7)'
  endif else begin
  printf,lun,strmid((strsplit(res[i].filename,'/',/extr))[-1],21,14), $
                     res[i].linerange[0],res[i].linerange[1],         $
                     res[i].pixrange[0],res[i].pixrange[1],res[i].orb,format='(A12,4I5,A7)'
  endelse
endfor
free_lun,lun
endif
  

END

