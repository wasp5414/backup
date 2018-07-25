time=20080608+indgen(7)
for j=0,1 do begin
  for i=0,n_elements(time)-1 do begin
    print,time[i]
    ;hs_gems_wind,time[i],layer=j,xdrmod=1
    hs_gems_cloud,time[i],type=j,xdrmod=1
  endfor
endfor

end
