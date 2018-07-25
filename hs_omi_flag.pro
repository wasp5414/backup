function hs_omi_flag, data, type=type

if not keyword_set(type) then type=0

data = long(data) ;& data=bad2nan(data,filled_value=filled_value)
;flgsize = size(binary(data[0]),/dim)
flgsize = 16 
ntrack=(size(data,/dim))[0] & nline=(size(data,/dim))[1]

case type of 
  0: begin ; OMIL2 GroundPixelQualityFlags
    res = make_array(ntrack,nline,flgsize)
    for i=0,n_elements(data)-1 do begin
      idim2=array_indices(data,i)
      res[idim2[0],idim2[1],*] = (binary(data[i]))[16:*]
    endfor
  end

  1: begin ; OMTO3 QualityFlags
;+---------------------------------------------------------------------------+
; Units: NoUnits
; Data Source: PGE
; Title: Quality Flags
; Unique Field Definition: TOMS-OMI-Shared
; Description: >
; Bits 0 to 3 together contain several output error flags:
; 0 - good sample
; 1 - glint contamination (corrected)
; 2 - sza > 84 (degree)
; 3 - 360 residual > threshold
; 4 - residual at unused ozone wavelength > 4 sigma
; 5 - SOI > 4 sigma (SO2 present)
; 6 - non-convergence
; 7 - abs(residual) > 16.0 (fatal)
; 8 - row anomaly error (same as bit 6 in this field)
; Add 10 for descending data.
; Bits 4 to 5 are reserved for future use (currently set to 0).
; Bit 6 - set to 0 when row anomlay error has not been detected,
; set to 1 when row anomaly error has been found.
; Bit 7 - set to 0 when OMI cloud (OMCLDRR or OMCLDO2) pressure is used,
; set to 1 when climatolgical cloud pressure is used.
; Bits 8 to 15 are flags that are set to 0 for FALSE (good value), or
; 1 for TRUE (bad value):
; Bit 8 - geolocation error (anomalous FOV Earth location)
; Bit 9 - sza > 88 (degree)
; Bit 10 - missing input radiance
; Bit 11 - error input radiance
; Bit 12 - warning input radiance
; Bit 13 - missing input irradiance
; Bit 14 - error input irradiance
; Bit 15 - warning input irradiance 
;
;+---------------------------------------------------------------------------+

    flg = intarr(ntrack,nline,flgsize)
    res = intarr(ntrack,nline,flgsize-3)
    for i=0,n_elements(data)-1 do begin
      idim2=array_indices(data,i)
      flg[idim2[0],idim2[1],*]=(binary(data[idim2[0],idim2[1]]) )[-1:16:-1]
      res[idim2[0],idim2[1],0]=hs_bin2dec(flg[idim2[0],idim2[1],0:3])
      res[idim2[0],idim2[1],1:*]=flg[idim2[0],idim2[1],4:*]
    endfor
  end
endcase

return,res

end
