function plotsym_fn, circle=circle, triangle=triangle, diamond=diamond, $
                  angle=angle, box=box, line=line, scale=scale, $
                  _extra=extra
;+
; NAME:
;  plotsym
; PURPOSE:
;  function to make plotting symbols much easier.
; Usage:
;   plot,x,y,psym=plotsym(/circle,scale=2,/fill)
;
; CATEGORY:
;   Graphics.
;
; Keywords:
;  circle =  circle symbol
;  triangle = triangle symbol
;  diamond = diamond symbold
;  box = box symbol
;  line = line symbol
;  scale = scales the symbol
;  angle = angle the symbol should be rotated
;  _extra = extra keywords for usersym.  These are thick, color and fill
;
; Written by:
; Ronn Kling
; Ronn Kling Consulting
; 7038 Westmoreland Dr.
; Warrenton, VA 20187
; ronn@rlkling.com
;
;-

if not keyword_set(scale) then scale=1.0
if not keyword_set(angle) then angle=0.0

if keyword_set(circle) then begin
  theta = findgen(30)/29.*360.
endif else if keyword_set(triangle) then begin
  theta = [-30.,90.,210., -30.]
endif else if keyword_set(diamond) then begin
  theta = [0.,90.,180.,270.,0.]
endif else if keyword_set(box) then begin
  theta = [315.,45.,135.,225.,315.]
endif else if keyword_set(line) then begin
  theta = [-180.,0.]
endif

theta = theta + angle
x = cos(theta * !dtor) * scale
y = sin(theta * !dtor) * scale

usersym, x,y, _extra=extra
return,8
end
