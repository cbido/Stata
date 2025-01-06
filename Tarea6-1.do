use "/Users/vivianasalinas/Dropbox/Cursos UC/Ev Hist/DATA/Tareas2022/EncNacJuv7_t4.dta", clear
rename *, lower

***Preparacion de variables para el analisis de sobrevivencia***
gen event=.
replace event=0 if z3_n_hijos==0
replace event=1 if z3_n_hijos>=1 & z3_n_hijos <=5
drop if z3_n_hijos==99

gen censor =! event


ren z4_anos_hijo_mayor edadhmayor
gen agefb=edad_exacta - edadhmayor
gen time=.
replace time=agefb if event==1
replace time=edad_exacta if event==0

drop if time==.

***Preparacion covariables***
*sexo*
gen mujer=.
replace mujer=1 if sexo==2
replace mujer=0 if sexo==1

*ses*
gen ses=.
replace ses=1 if w8 >=1 & w8 <=4
replace ses=2 if w8==5
replace ses=3 if w8 >=6 & w8 <=10

gen ses2=ses
recode ses2 (2=1) (3=0)

*religion*
gen rel= z21
recode rel (2=0) 

********************************************************************************
****************************MODELOS*********************************************

prsnperd correlativo time censor 

drop if  _period<=12
drop  _d1-_d12

/* modelo cuadratico */
gen tcenter=_period-26
generate p2 = tcenter^2
logit _Y tcenter p2 i.ses mujer rel

/*Graficos exploratorios*/
sort _period mujer ses2
by _period mujer ses2: egen pevent = mean(event)
gen logitp = log(pevent/(1-pevent))

twoway (line logitp _period if mujer==0 & ses2==1) ///
(line logitp _period if mujer==0 & ses2==0), ///
title("Hombres") xtitle("Age") ytitle("Sample logit(hazard)") ///
legend(pos(5) ring(0) col(1) ///
label(1 "SES bajo medio") label(2 "SES alto")) 

twoway (line logitp _period if mujer==1 & ses2==1) ///
(line logitp _period if mujer==1 & ses2==0), ///
title("Mujeres") xtitle("Age") ytitle("Sample logit(hazard)") ///
legend(pos(5) ring(0) col(1) ///
label(1 "SES bajo medio") label(2 "SES alto")) 


***

sort _period mujer rel
by _period mujer rel: egen pevent2 = mean(event)
gen logitp2 = log(pevent2/(1-pevent2))

twoway (line logitp2 _period if mujer==0 & rel==1) ///
(line logitp2 _period if mujer==0 & rel==0), ///
title("Hombres") xtitle("Age") ytitle("Sample logit(hazard)") ///
legend(pos(5) ring(0) col(1) ///
label(1 "Religioso") label(2 "No religioso")) 

twoway (line logitp2 _period if mujer==1 & rel==1) ///
(line logitp2 _period if mujer==1 & rel==0), ///
title("Mujeres") xtitle("Age") ytitle("Sample logit(hazard)") ///
legend(pos(5) ring(0) col(1) ///
label(1 "Religioso") label(2 "No religioso")) 






/*Modelo interaccion ses con sexo*/
logit _Y tcenter p2 ses2##mujer rel

logit _Y tcenter p2 ses2 mujer##rel

logistic _Y tcenter p2 ses2##mujer rel
*odds ref, hombre no rel:1
*hombre rel : 1.23
*mujer no rel: 2.5
*mujer rel: 2.503601 + 1.26277 + 1.232692=4.99
