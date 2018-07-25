pro hs_mapplot_test
loadct_wb,72
; Make a 10 degree latitude/longitude grid covering the Earth:
lat = REPLICATE(10., 37) # FINDGEN(19) - 90.
lon = FINDGEN(37) # REPLICATE(10, 19)
; Convert lat and lon to Cartesian coordinates:
X = COS(!DTOR * lon) * COS(!DTOR * lat)
Y = SIN(!DTOR * lon) * COS(!DTOR * lat)
Z = SIN(!DTOR * lat)
; Create a plotting window
WINDOW, 0, xs=1000,ys=1000,TITLE='TEST'
limit=[20,100,45,140]
MAP_SET,30,120, /satellite, $
        latlab=40,lonlab=120 ,sat_p=[2,38,120],/GRID, /CONTINENTS,londel=1,latdel=1, $
        TITLE='Area check',label=1

; Create another plotting window
;WINDOW, 1, xs=1000,ys=1000,TITLE='Stereographic Contour'
;; Fill the contours over the northern hemisphere and
;; display in a polar sterographic projection:
;MAP_SET, /STEREO, 90, 0, $
   ;/ISOTROPIC, /HORIZON, E_HORIZON={FILL:0}, $
   ;TITLE='Stereographic Contour'
;; Display points in the northern hemisphere only:
;CONTOUR, F(*,10:*), lon(*,10:*), lat(*,10:*), $
   ;/OVERPLOT, /FILL, NLEVELS=5
;MAP_GRID, /LABEL, COLOR=255
;MAP_CONTINENTS, COLOR=255

end
