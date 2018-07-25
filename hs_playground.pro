pro hs_playground

true = findgen(100,100)/(100*100-1) * 80

true_obj  = fltarr(100,100)
model_obj   = true_obj


for j=0,4 do begin
  true_obj[(j*2-1)*10-1:(j*2-1)*10+10-1,(j*2-1)*10-1:(j*2-1)*10+10-1]=j*10
  model_obj=true_obj+(j-3)*5
endfor

obs = true  + true_obj
res1 = obs   - true_obj

obs = true  + true_obj
res2 = obs   - model_obj

ct=0

;p1=[.05,.55,.40,.95]
;p2=[.25,.05,.65,.45]
;p3=[.50,.55,.90,.95]

;p4=[.05,.55,.40,.95]
;p5=[.50,.55,.90,.95]
;p6=[.05,.05,.40,.45]
;p7=[.50,.05,.90,.45]

;pl1 = contour(true,n_levels=50,/fill,rgb_table=ct,position=p1,title='true',1)
;pl2 = contour(true_obj ,n_levels=50,/fill,rgb_table=ct,position=p2,/current,title='object')
;pl3 = contour(obs ,n_levels=50,/fill,rgb_table=ct,position=p3,/current,title='observation')

;pl4 = contour(true,n_levels=50,/fill,rgb_table=ct,position=p4,title='true')
;pl5 = contour(obs ,n_levels=50,/fill,rgb_table=ct,position=p5,/current,title='observation')
;pl6 = contour(true_obj ,n_levels=50,/fill,rgb_table=ct,position=p6,/current,title='true obj')
;pl7 = contour(model_obj ,n_levels=50,/fill,rgb_table=ct,position=p7,/current,title='model obj')

;pl8 = contour(true,n_levels=21,/fill,rgb_table=ct,position=p4,title='true')
;pl9 = contour(obs ,n_levels=21,/fill,rgb_table=ct,position=p5,/current,title='observation')

pl1 = contour(true,n_levels=50,/fill,rgb_table=ct,layout=[4,2,1],title='true')
pl2 = contour(true_obj ,n_levels=50,/fill,rgb_table=ct,layout=[4,2,2],/current,title='true object')
pl3 = contour(obs ,n_levels=50,/fill,rgb_table=ct,layout=[4,2,3],/current,title='observation')
pl4 = contour(res1,n_levels=50,/fill,rgb_table=ct,layout=[4,2,4],title='true result')

pl5 = contour(true ,n_levels=50,/fill,rgb_table=ct,layout=[4,2,5],/current,title='true')
pl6 = contour(model_obj ,n_levels=50,/fill,rgb_table=ct,layout=[4,2,6],/current,title='true object')
pl7 = contour(obs ,n_levels=50,/fill,rgb_table=ct,layout=[4,2,7],/current,title='observation')
pl8 = contour(res2 ,n_levels=50,/fill,rgb_table=ct,layout=[4,2,8],/current,title='model result')

stop

end
