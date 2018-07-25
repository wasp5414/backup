pro read_l2_pixcor, file, result
               
 ;dg = '/HDFEOS/SWATHS/OMI Ground Pixel Corners UV-2/Data Fields'
 ;gg = '/HDFEOS/SWATHS/OMI Ground Pixel Corners UV-2/Geolocation Fields'
 dg = 'HDFEOS/SWATHS/OMI Ground Pixel Corners UV-2/Data Fields'
 gg = 'HDFEOS/SWATHS/OMI Ground Pixel Corners UV-2/Geolocation Fields'

  get_h5_dataset,lat,file=file,name=gg+'/Latitude',/group
  ;lat = h5read(file,gg+'/Latitude')
  get_h5_dataset,lon,file=file,name=gg+'/Longitude',/group
  ;lon = h5read(file,gg+'/Longitude')

  get_h5_dataset,corlat,file=file,name=dg+'/FoV75CornerLatitude',/group
  ;corlat = h5read(file,dg+'/FoV75CornerLatitude')  
  get_h5_dataset,corlon,file=file,name=dg+'/FoV75CornerLongitude',/group
  ;corlon = h5read(file,dg+'/FoV75CornerLongitude')  
  
  sz = SIZE(lon,/dim)

  ;tmp = h5read(file,gg+'/Time')
  get_h5_dataset,tmp,file=file,name=gg+'/Time',/group
  time = dblarr(60,n_elements(tmp))
  for i=0,59 do time[i,*] = tmp
  caldat,(julday(1,1,1993,0,0,0)*86400.0d0+time)/86400.0d0, $
  mon,day,year,hour,min,sec

  date = strarr(sz[0],sz[1])
  FOR iy=0, sz[1]-1 DO BEGIN
  FOR ix=0, sz[0]-1 DO BEGIN
    date[ix,iy]=string(year[ix,iy],mon[ix,iy],day[ix,iy],$
    format='(I4,I02,I02)')+'T'+$
    string(hour[ix,iy],min[ix,iy],sec[ix,iy],format='(I02,I02,I02)')+'Z'
  ENDFOR & ENDFOR
   
 result ={date:date, lat:lat, lon:lon, corlat:corlat, $
 corlon:corlon, jtime:time}
 
  end
