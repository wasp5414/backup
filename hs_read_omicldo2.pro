PRO hs_read_omicldo2,omi_file,cldo2,XDRMOD=xdrmod,TYPE=type

IF NOT KEYWORD_SET(XDRMOD) THEN xdrmod=0
IF NOT KEYWORD_SET(TYPE) THEN type=0  ; uv-2 read option, not use.


;omi_file='/home/Data/OMI/2_OML2CLDO2_new/2005/OMI-Aura_L2-OMCLDO2_2005m1119t0644-o07165_v003-2017m0619t222753.he5'
dirpos=strpos(omi_file,'/',/reverse_search) ; *** don't use '/' in file name ***
fn=strmid(omi_file,dirpos+1)
prefix=strmid(fn,0,strlen(fn)-4)
xdrpath='/home/o3p_hs/data/xdr/'
xdrfn=xdrpath+prefix+'.xdr'


IF xdrmod EQ 0 THEN BEGIN
  cfrac=read_omi_level2(omi_file,'CloudFractionAndPressure','CloudFraction')
  ctp=read_omi_level2(omi_file,'CloudFractionAndPressure','CloudPressure')
  lat=read_omi_level2(omi_file,'CloudFractionAndPressure','Latitude')
  lon=read_omi_level2(omi_file,'CloudFractionAndPressure','Longitude')
  flg=read_omi_level2(omi_file,'CloudFractionAndPressure','GroundPixelQualityFlags')
  flg=hs_omi_flag(flg)
  PRINT,'  :: OPENING ',omi_file
  cldo2={cfrac:cfrac,ctp:ctp,lat:lat,lon:lon,flg:flg}
  SAVE,file=xdrfn,cldo2,/xdr
  PRINT,'  :: SAVED ',xdrfn

ENDIF ELSE BEGIN
  RESTORE,xdrfn
  PRINT,'  :: RESTORED ',xdrfn

ENDELSE

END
