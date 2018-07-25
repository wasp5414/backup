PRO hs_getline
  time='20080715'
  lon_min=100 & lon_max=140
  lat_min=5   & lat_max=45
  
  year=STRMID(time,0,4)
  month=STRMID(time,4,2)
  day=STRMID(time,6,2)
  
  dir='/DB/Data/OMI/1_OML1BRUG/'+year+'/'
  mfiles=FILE_SEARCH(dir+'OMI-Aura_L1-OML1BRUG_'+year+'m'+month+'*.he4',COUNT=mn)
  yfiles=FILE_SEARCH(dir+'OMI-Aura_L1-OML1BRUG_'+year+'m'+'*.he4',COUNT=yn)

  for i=0,mn-1 do begin
    lon = h5_getdata(mfiles[i],'Earth UV-2 Swath/Geolocation Fields/Longitude')
    lat = h5_getdata(mfiles[i],'Earth UV-2 Swath/Geolocation Fields/Latitude')
    idx =WHERE(lon GE lon_min AND lon LE lon_max AND $
               lat GE lat_min AND lat LE lat_max, nidx)
  print,nidx
  
  endfor

stop

END
