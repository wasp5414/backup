pro loadct_wb,index

;device -  와 position 포함. 

;background color = white
;drawing color = black
;land = goldenrod
;see = skyblue 로 define

device, decomposed = 0  

loadct, index, /si 
!x.style=1
!y.style=1
!x.minor=1
!y.minor=1
!p.position=[0.15, 0.1, 0.95,0.9]

IF index eq 20 then begin
tvlct,r,g,b,/get
r_=congrid(r[100:255],255)
g_=congrid(g[100:255],255)
b_=congrid(b[100:255],255)
tvlct,r_,g_,b_
ENDIF



;@@ define black and white color
TVLCT, CGColor('BLACK', /Triple), 255 ; Drawing color.
TVLCT, CGColor('WHITE', /Triple), 254 ; Background color.
TVLCT, CGColor('goldenrod', /Triple), 253 ; land.
TVLCT, CGColor('sky blue', /Triple), 252 ; sea.
TVLCT, CGColor('GRAY',/Triple), 251 ; bad pixel
!p.color = 255 & !p.background = 254
!p.position=[0.1, 0.09, 0.88, 0.92]
END
