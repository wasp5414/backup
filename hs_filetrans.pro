;===============================================================================
;
; This program transfers file to ftp_tmp folder of geun's PC.
;
; PROGRAM NAME:
;  hs_filetrans
;
; PURPOSE:
;  Transfer the out files to hs's PC
;
; PROCESS:
;  1. 
;  2. 
;
; REFERENCE:
;
; INPUT:
;  TRANS_FILE : file for transfer
;  VERBOSE : shows specific transfer processes 
;  DELETE : delete trans file after transter
;
; OUTPUT:
;  
;
; DEVELOPER:
;  Daegeun Shin
;  Satellite Remote Sensing Laboratory
;  Division of Earth Environmental System
;  College of Natural Science
;  Pusan National University
;                  Tel: +82-51-510-2172
;                  E-mail: daegeun@pusan.ac.kr
;
;===============================================================================


FUNCTION url_callback, status, progress, data

   ; Print the info msgs from the url object
   PRINT, status

   ; Return 1 to continue, return 0 to cancel
   RETURN, 1
END


PRO hs_filetrans,trans_file,VERBOSE=VERBOSE, DELETE=DELETE, OBJNAME=OBJNAME

  verbose=(KEYWORD_SET(verbose)) ? verboase : 0
  urlpath=(KEYWORD_SET(objname)) ? objname : (strsplit(trans_file,'/',/ext))[-1]
  ;urlpath=(KEYWORD_SET(objname)) ? objname : trans_file
  
  oUrl = OBJ_NEW('IDLnetUrl')
  oUrl->SetProperty, VERBOSE = verbose
  oUrl->SetProperty, FTP_CONNECTION_MODE = 0
  oUrl->SetProperty, CALLBACK_FUNCTION ='Url_Callback'
  oUrl->SetProperty, URL_SCHEME = 'ftp'
  oUrl->SetProperty, URL_HOST = '164.125.38.183:23474'
  oUrl->SetProperty, URL_PATH = urlpath 
  oUrl->SetProperty, URL_USERNAME = 'hs'
  oUrl->SetProperty, URL_PASSWORD = 'WkaQhd1!'
  
  result = oUrl->Put(trans_file)
  ;result = oUrl->Put(objname)
  OBJ_DESTROY, oUrl
  ;oUrl->Cleanup

  IF KEYWORD_SET(DELETE) THEN BEGIN
    FILE_DELETE, trans_file 
  ENDIF

END
