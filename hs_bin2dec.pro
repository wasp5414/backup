function hs_bin2dec,bin,res,type=type

;+---------------------------------------------------------------------------+
; input   : binary array
; output  : decimal integer 
; <type>
; 0 : 16 bit flag
; 1 :  8 bit flag
;
; **** FLAG AXIS MUST BE LAST AXIS OF THE ARRAY ****
;
;+---------------------------------------------------------------------------+

if not keyword_set(type) then type=0

case type of
0: bin = fix(bin)
1: bin = byte(bin)
endcase

ndim=size(bin,/dim)
nbin=ndim[-1]

case n_elements(ndim) of
1 : begin
  res = 0
  ;for i=0,nbin-1 do begin
  for i=0,nbin-1 do begin
    res=res+bin[i]*2^(i)
  endfor
end

2 : begin
  res=make_array(ndim[0:-2])
  for i=0,nbin-1 do begin
    res[*]=res[*] + bin[*,i]*2^(i)
  endfor
end

3 : begin
  res=make_array(ndim[0:-2])
  for i=0,nbin-1 do begin
    res[*,*]=res[*,*] + bin[*,*,i]*2^(i)
  endfor
end
endcase

return, res

end
