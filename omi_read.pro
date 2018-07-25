
file='/home/o3p_hs/GEMS/o3p/dat/in/OMI-Aura_L1-OML1BRUG_2005m0310t0318-o03464_v003-2011m0120t055725-p1.h5'

uv2geo_path='/Earth UV-2 Swath/Geolocation Fields/'
uv2dat_path='/Earth UV-2 Swath/Data Fields/'

uv1geo_path='/Earth UV-1 Swath/Geolocation Fields/'
uv1dat_path='/Earth UV-1 Swath/Data Fields/'

strings =['Latitude','Longitude']

omi_fid = H5F_open(file)
geo_gid = h5g_open(omi_fid,uv2geo_path)
dat_gid = h5g_open(omi_fid,uv2dat_path)

lat_did = h5d_open(geo_gid,strings[0])
lon_did = h5d_open(geo_gid,strings[1])

lat     = h5d_read(lat_did)
lon     = h5d_read(lon_did)

h5d_close,lat_did
h5d_close,lon_did

h5g_close,geo_gid
h5g_close,dat_gid

h5f_close,omi_fid

xtrack = fltarr(60,1644)
line   = fltarr(60,1644)

print,size(xtrack,/dimensions)

;stop

for i = 0,1643 do begin xtrack(*,i) = indgen(60) 
for j = 0,59 do begin line(j,*) = indgen(1644)

limit = [100,140,20,40]
loc   = where(lon GT 110 and lon LE 140 and lat GT 20 and lat LT 40)
res = {ix:0.,iy:0.}
;res.ix = xtrack(loc)
;res.iy = line(loc)


end
