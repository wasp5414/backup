;*******************************************************************************
; NAME:
; GET_SD_ATTRIBUTES
;
;*******************************************************************************

FUNCTION GET_SD_ATTRIBUTES, s_id, natts

;attr = REPLICATE({name:'', count:0L, type:''}, natts)

FOR i = 0, natts - 1 DO BEGIN
  HDF_SD_ATTRINFO, s_id, i, NAME = aname, DATA = adata, $
                            COUNT = acount, TYPE = atype

  IF (atype EQ 'STRING') THEN BEGIN
    a = { name:aname, count:acount, type:atype, value:STRING(adata) }
  ENDIF ELSE BEGIN
    stype = SIZE(adata, /TYPE)
    a = { name:aname, count:acount, type:atype, value:MAKE_ARRAY(acount, TYPE=stype) }
    a.value = adata
  ENDELSE

  if (i eq 0) then attr = create_struct('a'+strtrim(string(i+1),2), a) $
  else attr = create_struct('a'+strtrim(string(i+1),2), a, attr)

ENDFOR

attr = create_struct('count', natts, attr)

RETURN, attr

END


PRO GET_HDF_SDS, data, attr, type, rank, dim, desc, FILE = file, $
                 INDEX = index, NAME = name, REF = ref, LIST = list

;*******************************************************************************
; NAME:
; GET_HDF_SDS
;
; PURPOSE:
; This IDL program will read multi-dimensional arrays from a file stored
; in the Hierarchical Data Format (HDF).  Multi-dimensional array data
; are stored in the HDF model known as a Scientific Data Set, or SDS.
; Users may select SDS data along with accompanying attribute information
; by either specifying the name of the SDS, the HDF reference number,
; or the relative index number within the file (default is to read the
; first SDS in the file). The list option is for showing all SDSes in
;       the file.
;
; CALLING SEQUENCE:
;   GET_HDF_SDS, data, type, rank, dim, attr, desc 
;
; INPUTS:
;       See Keywords below
;
; OUTPUTS:
; Data: The array containing the data.
;
; OPTIONAL OUTPUT PARAMETERS:
; Type: The number type of the data array (i.e. integer, float, etc.)
; Rank: The number of dimensions of the data array.
; Dim:  The size of each dimension.
; Attr: A structure containing the attributes describing the data array.
; Desc: Any data annotation descriptions for the data array.
;
; KEYWORDS Parameters:
; File: The name of the file.
; Index:  The relative index number of the SDS.
; Name:   The name of the SDS.
; Ref:    The HDF reference number for the SDS.
; List:   List SDSes
;
; RESTRICTIONS:
; Trying to retrieve a data description from an SDS which has no data
; descriptions will fail when using an IDL version earlier than 4.0.1b.
; Data descriptions appear as HDF tag 105, and are retrieved with the
; annotation calls.
;
; AUTHOR:
; James Johnson, Hughes STX
;
; MODIFICATION HISTORY:
; Sept 11, 1997 - Version 1.0. 
;
;*******************************************************************************

DFTAG_NDG = 720
DFNT_NONE = 0
DFNT_UCHAR = 3
DFNT_CHAR = 4
DFNT_FLOAT32 = 5
DFNT_FLOAT64 = 6
DFNT_INT8 = 20
DFNT_UINT8 = 21
DFNT_INT16 = 22
DFNT_UINT16 = 23
DFNT_INT32 = 24
DFNT_UINT32 = 25

ON_ERROR, 1

IF NOT KEYWORD_SET(file) THEN BEGIN
  file = ''
  PRINT, FORMAT='("Please enter file name", $)'
  READ, file
  file = STRTRIM(file)
ENDIF

IF HDF_ISHDF(file) THEN BEGIN   ; Check if file is really an HDF file

 sd_id = HDF_SD_START(file, /READ)
 HDF_SD_FILEINFO, sd_id, num_dsets, num_attrs

 IF KEYWORD_SET(list) THEN BEGIN
    
  IF (num_dsets GT 0) THEN BEGIN

    PRINT, ' Ref # Index '+string(39b)+'Name'+string(39b)
    PRINT, ' ----- ----- ---------------------------'

    FOR i=0, num_dsets-1 DO BEGIN
      sds_id = HDF_SD_SELECT(sd_id, i)
      ref = HDF_SD_IDTOREF(sds_id)
      HDF_SD_GETINFO, sds_id, NAME = name
;     IF (HDF_SD_ISCOORDVAR(sds_id) EQ 1) THEN $
;          hdf(i+inc).type = "SDS_COORD" $
;     ELSE hdf(i+inc).type = "SDS"
      PRINT, FORMAT='(x,I5,x,I5,x,A)', ref, i, string(39b)+name+string(39b)
      HDF_SD_ENDACCESS, sds_id
    ENDFOR

  ENDIF

 ENDIF ELSE BEGIN

  IF NOT KEYWORD_SET(index) THEN BEGIN
    IF NOT KEYWORD_SET(name) THEN BEGIN
      IF NOT KEYWORD_SET(ref) THEN index = 0 $
      ELSE index = HDF_SD_REFTOINDEX(sd_id, ref)
    ENDIF ELSE BEGIN
      index = HDF_SD_NAMETOINDEX(sd_id, name)
      IF (index EQ -1) THEN BEGIN
        PRINT, "Error: there is no SDS named ", name, " in ", file
        GOTO, done
      ENDIF
    ENDELSE
  ENDIF ELSE BEGIN
    IF (index LT 0) THEN BEGIN
      PRINT, "Error: index must be >= 0"
      GOTO, done
    ENDIF
    IF (index GE num_dsets) THEN BEGIN
      PRINT, "Error: there are only ", STRTRIM(STRING(num_dsets), 2), $
             " SDS in ", file
      GOTO, done
    ENDIF
  ENDELSE

  IF (index GE 0) THEN BEGIN

    sds_id = HDF_SD_SELECT(sd_id, index)

    IF NOT KEYWORD_SET(ref) THEN ref = HDF_SD_IDTOREF(sds_id)

    iscoord = HDF_SD_ISCOORDVAR(sds_id)
    IF (iscoord EQ 0) THEN BEGIN  ; Regular SDS multi-dimensional array

      HDF_SD_GETINFO, sds_id, $
             NDIMS = rank, DIMS = dimsizes, NATTS = natts, TYPE = type
      HDF_SD_GETDATA, sds_id, data

      dim = REPLICATE({name:'', size:0L}, rank)
      FOR i = 0, rank - 1 DO BEGIN
        dim_id = HDF_SD_DIMGETID(sds_id, i)
        HDF_SD_DIMGET, dim_id, NAME = name
        dim(i).name = name
        dim(i).size = dimsizes(i)
      ENDFOR

    ENDIF ELSE BEGIN      ; Coordinate SDS

      dim_id = HDF_SD_DIMGETID(sds_id, 0)
      HDF_SD_DIMGET, dim_id, $
             COUNT = dimsize, $
             FORMAT = format, LABEL = label, NAME = name, NATTR = natts, $
             SCALE = data, TYPE = type, UNIT = unit
      rank = 1

      dim = {name:'', size:0L}
      dim.name = name
      dim.size = dimsize

    ENDELSE

    IF (natts GT 0) THEN BEGIN
      attr = GET_SD_ATTRIBUTES(sds_id, natts)
    ENDIF

; This will fail if you are using IDL versions earlier than 4.0.1b!
; Uncomment below if you are getting errors retrieving data descriptions.
    ver = STR_SEP(!VERSION.RELEASE, '.')
    IF (FIX(ver(0)) GE 4 AND FIX(ver(1)) GE 0) THEN BEGIN
      IF (!VERSION.RELEASE NE '4.0.1') THEN BEGIN
        !QUIET = 1  ; Suppress warning messages in case desc is NULL
        HDF_DFAN_GETDESC, file, DFTAG_NDG, ref, descstr, /STRING
        IF (STRLEN(descstr) GT 0) THEN desc = descstr
        !QUIET = 0  ; Turn warning message printing back on
      ENDIF ELSE BEGIN
        CATCH, Error_status
        IF (Error_status NE 0) THEN GOTO, skip
        HDF_DFAN_GETDESC, file, DFTAG_NDG, ref, descstr
        IF (STRLEN(STRING(desc)) GT 0) THEN desc = STRING(descstr)
  skip: ; IDL 4.0.1 errors when there is no data description.
      ENDELSE
    ENDIF

    HDF_SD_ENDACCESS, sds_id
  
  ENDIF ELSE PRINT, "Reference #", STRCOMPRESS(STRING(ref)), " is not an SDS"

 ENDELSE
 HDF_SD_END, sd_id 

ENDIF ELSE PRINT, "Error: ", file, " is not a valid HDF file"

done: ; end program.
END