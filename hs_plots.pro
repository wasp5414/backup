

pro hs_plots,data,max,min,lev
n=n_elements(data)
nlev=51
barlev=reverse(fix(findgen(nlev-1)*253/nlev))
term=fix((max-min)/nlev)
lev=(findgen(nlev))*term+min

end

