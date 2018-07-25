PRO hs_omi_pixcor,omi_file,pixcor,XDRMOD=xdrmod,TYPE=type

IF NOT KEYWORD_SET(XDRMOD) THEN xdrmod=0
IF NOT KEYWORD_SET(TYPE) THEN type=0  ; uv-2 read option


;omi_file  = '/home/Data/OMI/2_OML2PIXCOR/2008/OMI-Aura_L2-OMPIXCOR_2008m0613t0150-o20807_v003-2017m0812t195833.he5'

xdrpath='/home/o3p_hs/data/xdr/'
fn=(strsplit((strsplit(omi_file,'/',/ext))[-1],'.',/ext))[0] ;remove extension & directory path
xdrfn=xdrpath+fn+'.xdr'


IF xdrmod EQ 0 THEN BEGIN
  case type of

  0: begin
    clat=read_omi_level2(omi_file,'OMI Ground Pixel Corners UV-1','FoV75CornerLatitude')
    clon=read_omi_level2(omi_file,'OMI Ground Pixel Corners UV-1','FoV75CornerLongitude')
    lat=read_omi_level2(omi_file,'OMI Ground Pixel Corners UV-1','Latitude')
    lon=read_omi_level2(omi_file,'OMI Ground Pixel Corners UV-1','Longitude')
  end

  1: begin
    clat=read_omi_level2(omi_file,'OMI Ground Pixel Corners UV-2','FoV75CornerLatitude')
    clon=read_omi_level2(omi_file,'OMI Ground Pixel Corners UV-2','FoV75CornerLongitude')
    lat=read_omi_level2(omi_file,'OMI Ground Pixel Corners UV-2','Latitude')
    lon=read_omi_level2(omi_file,'OMI Ground Pixel Corners UV-2','Longitude')
  end

  2: begin
    clat=read_omi_level2(omi_file,'OMI Ground Pixel Corners UV-2','TiltedCornerLatitude')
    clon=read_omi_level2(omi_file,'OMI Ground Pixel Corners UV-2','TiltedCornerLongitude')
    lat=read_omi_level2(omi_file,'OMI Ground Pixel Corners UV-2','Latitude')
    lon=read_omi_level2(omi_file,'OMI Ground Pixel Corners UV-2','Longitude')
  end
  endcase 
  PRINT,'  :: OPENING ',omi_file
  pixcor={clat:clat,clon:clon,lat:lat,lon:lon}
  SAVE,file=xdrfn,pixcor,/xdr
  PRINT,'  :: SAVED ',xdrfn

ENDIF ELSE BEGIN
  RESTORE,xdrfn
  PRINT,'  :: RESTORED ',xdrfn


ENDELSE

END
