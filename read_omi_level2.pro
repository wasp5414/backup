; $Id: read_omi_level2.pro,v 1.2 2007/11/05 14:02:53 sneepm Exp $
;
; Copyright (c) 2007, KNMI (Maarten Sneep).
; 
; Permission is hereby granted, free of charge, to any person obtaining a 
; copy of this software and associated documentation files (the "Software"), 
; to deal in the Software without restriction, including without limitation 
; the rights to use, copy, modify, merge, publish, distribute, sublicense, 
; and/or sell copies of the Software, and to permit persons to whom the 
; Software is furnished to do so, subject to the following conditions:
; 
; - The above copyright notice and this permission notice shall be included 
;   in all copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
; OTHER DEALINGS IN THE SOFTWARE.
;
;+
; NAME:
; READ_OMI_LEVEL2
;
; PURPOSE:
; Read data from an OMI level 2 file (HDFEOS 5 format). 
;
; CATEGORY:
; Data retrieval
;
; CALLING SEQUENCE:
; data = READ_OMI_LEVEL2(FILENAME, SWATHNAME, FIELDNAME [, /NO_FILL_FILTER] [, /FLAGS]
;
; RETRUN VALUE:
;   An array containing the data from the specified field.
; 
; PARAMETERS:
;   FILENAME:  The full or relative path to the file.
;   SWATHNAME: The name of the swath, note that this is case sensitive. You can find 
;              this in the file specification or interactively with hdfview.
;   FIELDNAME: The name of the field you are interested in. Note that this is again 
;              case sensitive. You can find this in the file specification or 
;              interactively with hdfview.
;
; FLAGS:
; FLAGS:     Assume the field is a flags field. This prevents trantlation to float or 
;              double, scaling and filtering for fill-values.
; 
; USAGE EXAMPLE:
;   filename  = 'OMI-Aura_L2-OMCLDO2_2006m0823t2027-o11207_v002-2006m0824t192131.he5'
;   swathname = 'CloudFractionAndPressure'
;   fieldname = 'Latitude'
;   ; read the latitudes
;   latitudes = read_omi_level2(filename, swathname, fieldname)
;   help, latitudes
;   ; this prints: LATITUDES       FLOAT     = Array[60, 1644]
;   ; read the ground pixel quality flags:
;   fieldname = 'GroundPixelQualityFlags'
;   gpqf      = read_omi_level2(filename, swathname, fieldname, /flags)
;   help, gpqf
;   ; this prints: GPQF            UINT      = Array[60, 1644]
;
; UPDATES:
;   Updated version of this function will be placed on the KNMI OMI website.
;   http://www.knmi.nl/omi
;-

function read_omi_level2, filename, swathname, fieldname, flags=flags
  
  ; the usual compile options
  compile_opt defint32, strictarr, strictarrsubs
    
  ; Check that the required parameters are available and sensible
  if n_params() lt 3 then message, "Required parameter missing"
    if not file_test(filename, /regular, /read) then message, "The file could not be read"
    if strlen(swathname) eq 0 then message, "The SWATHNAME must be not be empty"
    if strlen(fieldname) eq 0 then message, "The FIELDNAME must be not be empty"
    
    ; open the hdf file
    fid = -1
  fid = h5f_open(filename)
  if fid lt 0 then message, "File could not be opened: "+ filename
    
  ; build the path name and get the data. 
    ; We start by assuming that one of the data-fields is wanted
  pathname = '/HDFEOS/SWATHS/' + swathname + '/Data Fields/'
  
  catch, err
  
    ; If the field is not in the data fields, then we get an error.
    ; Here we catch that error, and change the HDF-5 path to search in the geolocation fields
  if err ne 0 then begin
    pathname = "/HDFEOS/SWATHS/" + swathname + "/Geolocation Fields/"
    catch, /cancel
  endif
  
  ; We need to open a field identifier within the already open fid. 
  ; The internal id is opened with the h5d_open() function. 
  ; The arguments of this function are the fid and a concatenation of 
  ; the pathname and the fieldname.
  id = -1
  id = h5d_open(fid, pathname + fieldname)
  
  ; If the field wasn't found (in either the data or geolocation fields), we return an error.
  if(id le 0) then message, "Field " + fieldname + " could not be opened in swath " + swathname
    
    ; get the data
  data = h5d_read(id)
    
    ; find the type of data we're dealing with
    datatype=size(data, /type)
    
    ; if we have a regular data-field, we have to find the scale factor, offset and fill value.
    if not keyword_set(flags) then begin
        ; remove the current error handler
        catch, /cancel 
        skip = 0
        Catch, err
        
        ; if the 'ScaleFactor' attribute cannot be found use the default value if 1.0.
      if err ne 0 then begin
        scale_factor = 1.0D0
        skip = 1
        catch, /cancel
      endif
        
        ; try to reat the 'ScaleFactor' attribute. 
      if skip eq 0 then begin
        Attribute_id = H5A_OPEN_NAME(id, "ScaleFactor") 
        scale_factor = (H5A_READ(Attribute_id))[0]
        h5a_close, Attribute_id
      endif
        
        ; if the scalefacto is not finite, assume 1.
      if not finite(scale_factor) then scale_factor = 1.0D0
        
        ; repeat for the offset
      catch, /cancel
        skip = 0
      Catch, err

      if err ne 0 then begin
        offset = 0.0D0
        skip = 1
        catch, /cancel
      endif

      if skip eq 0 then begin
        Attribute_id = H5A_OPEN_NAME(id, "Offset") 
        offset = (H5A_READ(Attribute_id))[0]
        h5a_close, Attribute_id
      endif

      if not finite(offset) then offset = 0.0D0
        
        ; repeat for the fill value. Slightly different as there are two 
        ; possible attributes for this in OMI files (historical artefact).
      catch, /cancel 
        skip = 0
      Catch, err

      if err ne 0 then begin
            if skip eq 0 then begin
            ;; apparently the "MissingValue" attribute wasn't found, use '_FillValue' instead.
            Attribute_id = H5A_OPEN_NAME(id, "_FillValue") 
            skip = 1
            endif else begin
                skip = 2
                catch, /cancel 
            endelse
      endif
        
      if skip eq 0 then Attribute_id = H5A_OPEN_NAME(id, "MissingValue") 
      fillvalue = (H5A_READ(Attribute_id))[0]
        if skip lt 2 then h5a_close, Attribute_id else fillvalue = !values.f_nan
        
        ; apply fill-value filter
        if finite(fillvalue) then begin
            idx = where(data eq fillvalue, cnt)
            if cnt gt 0 then data[idx] = !values.f_nan
        endif
        
        ; apply scaling and offset
        data *= scale_factor
        data += offset
        
        ; reset the dataype to match the current data, unless the scaling is such that the 
        ; data can only be represented by double.
        if datatype eq 4 then begin
            if max(data,/nan) ge 1.0D30 or min(data,/nan) le -1.0D30 or $
                max(1.0D0/data,/nan) ge 1.0D30 or min(1.0D0/data,/nan) le -1.0D30 $
            then $
                datatype = 5
        endif
        
        if datatype eq 1 or datatype eq 2 or datatype eq 12 then begin
            if abs(scale_factor) ge 1.0D30 or abs(1/scale_factor) ge 1.0D30 then $
                data = fix(data,type=5) else data = fix(data,type=4)
        endif
        if datatype eq 3 or datatype ge 13 then data = fix(data, type=5)
        if datatype eq 4 or datatype eq 5 then data = fix(data, type=datatype)
    endif
    
    ; close the data and file references
  h5d_close, id
    h5f_close, fid
    
    return, data
end

; Change log:
; $Log: read_omi_level2.pro,v $
; Revision 1.2  2007/11/05 14:02:53  sneepm
;
; CV and clarifications.: ----------------------------------------------------------------------
;
; Revision 1.1  2007/10/29 13:10:22  sneepm
; Initial import.
;
;end
