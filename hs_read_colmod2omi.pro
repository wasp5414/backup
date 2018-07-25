pro hs_read_colmod2omi,orbit,res,xdrmod=xdrmod
  
if not keyword_set(xdrmod) then xdrmod=0

year ='2005'
pixfile=file_search('/home/Data/OMI/2_OML2PIXCOR/'+year+'/*'+orbit+'*.he5' $
                    ,count=npixfile)

hs_omi_pixcor,pixfile[0],pixcor
dim=size(pixcor.lon,/dim)
nx=dim[0] & ny=dim[1]
res=intarr(nx,ny)
tmp=''

colpath='/home/o3p_hs/GEMS/o3p/v0.4.3/src/tmp_modctp/'
col_files=file_search(colpath + '*' +orbit+ '*')
xdrpath='/home/o3p_hs/data/xdr/'

;remove extension
fn=(strsplit((strsplit(col_files[0],'/',/ext))[-1],'.',/ext))[0]
xdrfn=xdrpath+fn+'.xdr'

if xdrmod eq 0 then begin
  openr,lun,col_files[0],/get_lun
  form='('+string(nx,f='(I2)')+'I04)'

  for j=0,ny-1 do begin
    readf,lun,tmp
    for i=0,nx-1 do begin
      res[i,j]=strmid(tmp,i*4,4)
    endfor
  endfor

  free_lun,lun 
  save,file=xdrfn,res,/xdr
  print,'   :: saved '+xdrfn
endif else begin
  restore,xdrfn
  print,'   :: RESTORED '+xdrfn
endelse

end
