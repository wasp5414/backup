PRO hs_read_omto3,omi_file,to3,XDRMOD=xdrmod,TYPE=type

IF NOT KEYWORD_SET(XDRMOD) THEN xdrmod=0


;omi_file='/home/Data/OMI/2_OML2TO/2005/OMI-Aura_L2-OMTO3_2005m1231t1534-o07782_v003-2012m0330t211857.he5'
dirpos=strpos(omi_file,'/',/reverse_search) ; *** don't use '/' in file name ***
fn=strmid(omi_file,dirpos+1)
prefix=strmid(fn,0,strlen(fn)-4)
xdrpath='/home/o3p_hs/data/xdr/'
xdrfn=xdrpath+prefix+'.xdr'


IF xdrmod EQ 0 THEN BEGIN
  lat=read_omi_level2(omi_file,'OMI Column Amount O3','Latitude')
  lon=read_omi_level2(omi_file,'OMI Column Amount O3','Longitude')
  flg=read_omi_level2(omi_file,'OMI Column Amount O3','QualityFlags')
  flg=hs_omi_flag(flg,type=1)

  PRINT,'  :: OPENING ',omi_file
  to3={lat:lat,lon:lon,flg:flg}
  SAVE,file=xdrfn,to3,/xdr
  PRINT,'  :: SAVED ',xdrfn

ENDIF ELSE BEGIN
  RESTORE,xdrfn
  PRINT,'  :: RESTORED ',xdrfn

ENDELSE

END
