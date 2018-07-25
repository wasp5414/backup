; NAME:
;   GET_POSITIONS
;
; PURPOSE:
;   This procedure calculates position values of each sector when multiple plots
;     are to be shown in a graphic window
;
; AUTHOR:
;   Sangwoo Lee, Ph.D.
;   SELab, Inc.
;   Seoul, Korea
;   Phone: 82-10-3727-1172
;   E-mail: lee@spweather.com
;
; USAGE:
;   result = GET_POSITIONS(xn, yn, XMARGIN=xmg, YMARGIN=ymg, GAP=gap)
;
; RETURN VALUE:
;   A 2-dimensional (xn*yn by 4) array consisting of position values for each sector
;   first index corresponds to the index of each sector in portrait order starting from 0
;     second index corresponds to position values of [xp1, yp1, xp2, yp2] for later use
;     for POSITION keyword of each plot
;
; ARGUMENTS:
;   xn, yn: number of sectors in each direction
;
; Keywords:
;   XMARGIN: Page margin in X direction.
;            If it's a single value, left and right margins are regarded to be the same.
;            If it's a two-value array, each element is regarded to be
;              left and right margin values.
;            If omitted, the defaut value is 0.1
;   YMARGIN: Page margin in Y direction.
;            If it's a single value, lower and upper margins are regarded to be the same.
;            If it's a two-value array, each element is regarded to be
;              lower and upper margin values.
;            If omitted, the defaut value is 0.1
;   GAP: Amount of gaps between each plot in X and Y direction.
;            If it's a single value, gaps in X and Y directions are regarded to be the same.
;            If it's a two-value array, each element is regarded to be
;              X and Y direction gaps.
;            If omitted, the defaut value is 0
;
;
; MODIFICATION HISTORY:
;   First written on Feb 17, 2016.
;
;******************************************************************************************
;  Copyright (c) 2016, by Sangwoo Lee and SELab, Inc.               ;
;  All rights reserved.
;******************************************************************************************

FUNCTION GET_POSITIONS, xn, yn, XMARGIN=xmg, YMARGIN=ymg, GAP=gap

IF xmg EQ !null THEN xmg = 0.1
  CASE N_ELEMENTS(xmg) OF
    1 : BEGIN
      xmgl = xmg
      xmgr = xmg
    END
    2 : BEGIN
      xmgl = xmg[0]
      xmgr = xmg[1]
    END
    ELSE : BEGIN
      PRINT, 'XMARGIN keyword only accepts 2 value array or a single value!'
      RETURN, -1
    END
  ENDCASE
IF ymg EQ !null THEN ymg = 0.1
  CASE N_ELEMENTS(ymg) OF
    1 : BEGIN
      ymgl = ymg
      ymgu = ymg
    END
    2 : BEGIN
      ymgl = ymg[0]
      ymgu = ymg[1]
    END
    ELSE : BEGIN
      PRINT, 'YMARGIN keyword only accepts 2 value array or a single value!'
      RETURN, -1
    END
  ENDCASE
IF gap EQ !null THEN gap = 0
  CASE N_ELEMENTS(gap) OF
    1 : BEGIN
      xgap = gap
      ygap = gap
    END
    2 : BEGIN
      xgap = gap[0]
      ygap = gap[1]
    END
    ELSE : BEGIN
      PRINT, 'GAP keyword only accepts 2 value array or a single value!'
      RETURN, -1
    END
  ENDCASE

xdim = 1.0 - (xmgl+xmgr)
ydim = 1.0 - (ymgl+ymgu)
xps = FINDGEN(xn+1)/xn*xdim + xmgl
;PRINT, xps
yps = FINDGEN(yn+1)/yn*ydim + ymgl
;PRINT, yps
positions = FLTARR(xn*yn, 4)
tmp = INDGEN(xn, yn)
FOR j = 0, yn-1 DO BEGIN
  FOR i = 0, xn-1 DO BEGIN
    ind = tmp[i, j]
    IF i GT 0 AND i LT (xn-1) THEN xoff = xgap/2. ELSE xoff = 0
    IF (yn-j-1) GT 0 AND (yn-j) LT yn THEN yoff = ygap/2. ELSE yoff = 0
    IF yn EQ 2 AND j EQ 0 THEN yoff = xgap/2.
    IF yn EQ 2 AND j EQ 1 THEN yoff = 0
    IF xn EQ 2 AND i EQ 0 THEN xoff = 0
    IF xn EQ 2 AND i EQ 1 THEN xoff = xgap/2.
    xp1 = xps[i] + xoff
    yp1 = yps[yn-j-1] + yoff
    IF xn EQ 2 AND i EQ 0 THEN xoff = xgap/2.
    IF xn EQ 2 AND i EQ 1 THEN xoff = 0
    IF yn EQ 2 AND j EQ 0 THEN yoff = 0
    IF yn EQ 2 AND j EQ 1 THEN yoff = xgap/2.
    xp2 = xps[i+1] - xoff
    yp2 = yps[yn-j] - yoff
    positions[ind, *] = [xp1, yp1, xp2, yp2]
  ENDFOR
ENDFOR

RETURN, positions

END