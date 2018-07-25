function bad2nan,data,filled_value=fv

if not keyword_set(filled_value) then fv=-999.

buffmin=fv-1.
buffmax=fv+1.

bad=where(data gt buffmin and data lt buffmax)
data[bad]=!values.f_nan
return,data

end
