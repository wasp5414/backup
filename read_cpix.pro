FUNCTION read_cpix, file, uv1=uv1

 dg = '/HDFEOS/SWATHS/OMI Ground Pixel Corners UV-2/Data Fields'
 gg = '/HDFEOS/SWATHS/OMI Ground Pixel Corners UV-2/Geolocation Fields'

 IF keyword_set(uv1) then begin
   dg = '/HDFEOS/SWATHS/OMI Ground Pixel Corners UV-1/Data Fields'
   gg = '/HDFEOS/SWATHS/OMI Ground Pixel Corners UV-1/Geolocation Fields'
 ENDIF

 cLat =  h5read(file,dg+'/FoV75CornerLatitude')
 cLon =  h5read(file,dg+'/FoV75CornerLongitude')
  lat =  h5read(file,gg+'/Latitude')
  lon =  h5read(file,gg+'/Longitude')

 return,{clat:clat, clon:clon, lat:lat, lon:lon}
ENd
