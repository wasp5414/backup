pro script
;+---------------------------------------------------------------------------+
; use for IDL script
;+---------------------------------------------------------------------------+


hs_gems_checkarea_cldmyd,which_plot=0
hs_gems_checkarea_cldmyd,which_plot=1
hs_gems_checkarea_cldmyd,which_plot=2
hs_gems_checkarea_cldmyd,which_plot=3


;hs_gems_checkarea_ver10,xdrmod=0

;for i=0,2 do begin
  ;for j=0,n_elements(types)-1 do begin

    ;hs_gems_checkarea_ver5,xdrmod=1,loop=1,which_plot=
    ;hs_gems_checkarea_ver6,which_date=which_date,xdrmod=0,control=types[j],loop=1
    ;hs_gems_checkarea_ver7,xdrmod=1,loop=1,which_plot=j 

  ;endfor
;endfor

;hs_gems_checkarea_ver6,which_date=which_date,xdrmod=1,control='MYD',loop=1
;hs_collocation,xdrmod=1


  ;datelist=strtrim(indgen(2)+20050601,2)
  ;hs_gems_checkarea_ver5,xdrmod=1,loop=1,which_plot=0
  ;hs_gems_checkarea_ver5,xdrmod=1,loop=1,which_plot=1
  ;hs_gems_checkarea_ver5,xdrmod=1,loop=1,which_plot=2
  ;hs_gems_checkarea_ver10,xdrmod=1,which_plot=0,loop=0
  ;hs_gems_checkarea_ver10,xdrmod=0,which_plot=1,loop=1
  ;hs_gems_checkarea_ver10,xdrmod=0,which_plot=2,loop=1
  ;hs_gems_checkarea_colmyd,xdrmod=0


;+---------------------------------------------------------------------------+
; data collocation check 
;+---------------------------------------------------------------------------+

;orbit1='04684'
;orbit2='04685'
;xdrpath='/home/o3p_hs/data/xdr/'
;omipath='/home/Data/OMI/2_OML2TO/'
;;respath=
;colfn1='final_col_modis_omi_'+orbit1+'.xdr'
;colfn2='final_col_modis_omi_'+orbit2+'.xdr'



;data = []
;print,res.gems[*,300].cfrac
;print,res.cf[*,300]

;xdrpath   = '/home/o3p_hs/data/xdr/'

;colfn1='final_col_modis_omi_'+orbit1+'.xdr'
;colfn2='final_col_modis_omi_'+orbit2+'.xdr'

;restore,xdrpath+colfn1
;res1=res
;restore,xdrpath+colfn2
;res2=res

;col_test,xdrmod=1

;+---------------------------------------------------------------------------+
; DATA SCATTERPLOT TEST
;+---------------------------------------------------------------------------+

;cf =[] & ctp =[] & co3=[]
;cf2=[] & ctp2=[] 
;restore,xdrpath+colfn1

;cf1 = [cf1 ,res.cf[*]         ]
;cf2 = [cf2 ,res.gems[*].cfrac ]
;ctp1= [ctp1,res.ctp[*]        ]
;ctp2= [ctp2,res.gems[*].ctp   ]
;co3 = [co3 ,res.gems[*].co3[0]]

;restore,xdrpath+colfn2

;cf1 = [cf1 ,res.cf[*]         ]
;cf2 = [cf2 ,res.gems[*].cfrac ]
;ctp1= [ctp1,res.ctp[*]        ]
;ctp2= [ctp2,res.gems[*].ctp   ]
;co3 = [co3 ,res.gems[*].co3[0]]

;diff = cf2-cf

;group = where( abs(diff) lt 0.1 and cf2 gt 0.4, ntmp)


;;fn=respath+'test_dist.png'
;win=window(dimensions=[1200,900])
;scat_pos1=[0.075,0.15,0.325,0.90]
;scat_pos2=[0.400,0.15,0.650,0.90]
;scat_pos3=[0.725,0.15,0.975,0.90]
;scat_pos4=[                     ]


;scatx=cf1[0:1000] & scaty=co3[0:1000]
;scatplot=scatterplot(scatx,scaty,xstyle=2,ystyle=2,symbol='dot',/sym_filled,sym_color='black',posi=scat_posi1,title='title',/current)

end
