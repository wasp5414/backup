
function OMI__LOAD_HDF_DATA, filename , swathname , fieldname , data, $
	                     edge=edge, start=start, stride=stride

    compile_opt defint32, strictarr, strictarrsubs

    HDF_FAIL = -1
    fileid   = -1
    swathid  = -1
    status   = -1

    ;
    ; sanity check on all parameters
    ;
       
    if (n_params() NE 4) then begin
        goto,FAILURE
    endif

    if ((n_elements(filename) ne 1) or (size(filename,/type) ne 7)) then begin
        goto,FAILURE
    endif

    if ((n_elements(swathname) ne 1) or (size(swathname,/type) ne 7)) then begin
        goto,FAILURE
    endif

    if ((n_elements(fieldname) ne 1) or (size(fieldname,/type) ne 7)) then begin
        goto,FAILURE
    endif

    if not arg_present(data) then begin
        goto, FAILURE
    endif

    ;
    ; try to open the file
    ;
     
    fileid = eos_sw_open(filename,/read) 
    if (fileid EQ HDF_FAIL) then begin
        goto,FAILURE
    endif
   
    ;
    ; retrieve all available swaths in the file
    ;

    swathcount = eos_sw_inqswath(filename, swathlist)

    if (swathcount eq 0) then begin
        goto,FAILURE
    endif else begin
        swathlist=strsplit(swathlist,',',/extract)
    endelse


    ;
    ; check if the requested swath is present in the file
    ;

    flags = strmatch(swathlist,swathname)
    index = where(flags eq 1,count) 

    if (count eq 0)  then begin
        goto,FAILURE
    endif 

    ;
    ; try to attach the swath
    ;

    swathid = eos_sw_attach(fileid,swathname)
    if (swathid eq HDF_FAIL) then begin
        goto,FAILURE
    endif

    ;
    ; read the field info, we need the number type of the field
    ;

    exitcode = eos_sw_fieldinfo(swathid, fieldname, rank, dimensions, numbertype, dimlist)
    if (exitcode eq HDF_FAIL) then begin
        goto,FAILURE
    endif
 
    ;
    ; Check to see that requested subset is within bounds 
    ; (see eos_sw_readfield docs for meaning of start, edge and stride)
    ;
    if n_elements(edge) eq rank and n_elements(start) eq rank then begin
        if n_elements(stride) ne rank then stride = replicate(1, rank)

        for dimcnt=0,rank-1 do begin
    	    if start[dimcnt] lt 0 then goto,FAILURE
    	    if stride[dimcnt] eq 0 then stride[dimcnt] = 1
    	    if edge[dimcnt] lt 0 then edge[dimcnt] = (dimensions[dimcnt] - start[dimcnt])/stride[dimcnt]
    	    if start[dimcnt]+stride[dimcnt]*edge[dimcnt] gt dimensions[dimcnt] then goto,FAILURE
        endfor
    endif else begin
	    edge = dimensions
	    start = replicate(0, rank)
	    stride = replicate(1, rank)
    endelse

    ;
    ; find the fill values as defined in the TMCF for the HDF field number type 
    ;

    case numbertype of
        5    : fillvalue = float(-1267650600228229401496703205376.0)       ; float32
        6    : fillvalue = double(-1267650600228229401496703205376.0D0)    ; float64
        20   : fillvalue = byte(-127)                                      ; int8 // be very careful with these fields in IDL.
        21   : fillvalue = byte(255)                                       ; uint8
        22   : fillvalue = (-32767)                                        ; int16
        23   : fillvalue = uint(65535)                                     ; uint16
        24   : fillvalue = long(-2147483647)                               ; int32
        25   : fillvalue = ulong(4294967295)                               ; uint32  
        else : goto, FAILURE
    endcase

    ;
    ; read the field data
    ;

    exitcode = eos_sw_readfield(swathid,fieldname,data, EDGE=edge,START=start,STRIDE=stride)
    if (exitcode eq HDF_FAIL) then begin
        goto,FAILURE
    endif 

    ;
    ; where possible replace the TMCF fill values with IDL fill values 
    ;

    index = where(data eq fillvalue,count)
    if (count gt 0) then begin
        case size(data,/type) of
            4    : data[index] = !values.f_nan
            5    : data[index] = !values.d_nan
            7    : data[index] = ''
            else : 
      endcase
    endif

    status = 0

FAILURE:
    exitcode = eos_sw_detach(swathid)
    exitcode = eos_sw_close(fileid)
    return,status

end

;=====
