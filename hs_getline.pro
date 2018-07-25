PRO hs_getline, time

  lon_min=115 & lon_max=135
  lat_min=35  & lat_max=40
  
  year=STRMID(time,0,4)
  month=STRMID(time,4,2)
  day=STRMID(time,6,2)

  omi_dir='/DB/Data/OMI/1_OML1BRUG/'+year+'/'
  mfiles=FILE_SEARCH(omi_dir+'OMI-Aura_L1-OML1BRUG_'+year+'m'+month+'*.he4',COUNT=nfiles)
  yfiles=FILE_SEARCH(omi_dir+'OMI-Aura_L1-OML1BRUG_'+year+'m'+'*.he4',COUNT=nfiles)  

  for ifile=0,nfiles-1 do begin
    lon = h5_getdata(fn,'Earth UV-2 Swath/Geolocation Fields/Longitude')
    lat = h5_getdata(fn,'Earth UV-2 Swath/Geolocation Fields/Latitude')
    idx =WHERE(lon GE lon_min AND lon LE lon_max AND $
               lat GE lat_min AND lat LE lat_max, nidx)

  endfor
  
  
  
END
