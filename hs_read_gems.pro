pro hs_read_gems, gems_file, gems, xdrmod=xdrmod, tai93mod=tai93mod
;+---------------------------------------------------------------------------+
; INPUT  : h5 format gems output file path
; OUTPUT : gems data structure
; Need to set - read_xdr option, if you read it first time, set it 0 else :  1
;+---------------------------------------------------------------------------+
if not keyword_set(xdrmod) then xdrmod=0
if not keyword_set(tai93mod) then tai93mod=0

xdrpath='/home/o3p_hs/GEMS/o3p/dat/out/xdr/'
outpath='/home/o3p_hs/GEMS/o3p/dat/out/'

;remove extension
fn=(strsplit((strsplit(gems_file,'/',/ext))[-1],'.',/ext))[0] 

xdrfn=xdrpath+fn+'.xdr'
if not tai93mod then xdrfn=xdrpath+fn+'_ymd.xdr' 

if xdrmod eq 0 then begin
  read_gems_l2o3,gems_file,gems,tai93mod=tai93mod
  PRINT,'  :: OPENING ',gems_file
  save,file=xdrfn,gems,/XDR
  PRINT,'  :: SAVED '+xdrfn
endif else begin
  restore,xdrfn
  PRINT,'  :: RESTORED '+xdrfn
ENDELSE


if tai93mod then print,gems[0].time else $
print, gems[0].year,gems[0].mon,gems[0].day,gems[0].utc

end
