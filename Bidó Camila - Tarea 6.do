********************************************************************************
* SOL3073 - Análisis de Historia de Eventos
* Tarea 6, Camila Bidó 
* Versión: 10/05/2022
********************************************************************************

*-------------------------------------------------------------------------------
* 1. Definir directorio
*-------------------------------------------------------------------------------

clear all								// 	Limpieza de la memoria
version 15								// 	Versión a utilizar
*Definición directorio de trabajo
cd "/Users/camilabido/Downloads/Analisis de historia de eventos tareas"
*Creación de respaldo
capture log close 						//Cerramos ventanas de resultados anteriores
log using respaldo, replace  
* Otras especificaciones:
set more off 							//Continuidad al resultado del comando
dir										//Ver el directorio de trabajo 

*-------------------------------------------------------------------------------
* 2. Cargar y explorar la BBDD
*-------------------------------------------------------------------------------

* cargar la BBDD
use "EncNacJuv7_t4.dta", clear 

* Explorar la BBDD
describe				//Describe las variables de la base de datos
codebook				//Libro de códigos de las variables

*-------------------------------------------------------------------------------
* 3. Desarrollo de ejercicios
*-------------------------------------------------------------------------------

* El objetivo de esta tarea es familiarizarse cuestionar el supuesto de aditividad en los modelos de tiempo discreto. 
* El evento en estudio es el nacimiento del primer hijo (i.e., convertirse en padre o madre). 
* Utilizará un extracto de la VII Encuesta Nacional de la Juventud.
* La base de datos que utilizará es “EncNacJuv7_t4.dta.

*** PREPARACIÓN DE LOS DATOS ***

// renombramos el identificador 

gen id= CORRELATIVO

* A. Recodificar Sexo como 0 y 1. 

recode SEXO (1=0)(2=1) // recodificar como 0 y 1
label drop SEXO
label define SEXO 0 "Hombre" /// etiquetas de la variable
				  1 "Mujer" 
label value SEXO SEXO // asociar las etiquetas a los atributos de la variable
tab SEXO
* B. A partir de la variable nivel socioeconómico de origen, aproximado por el nivel educacional del jefe de hogar (w8), use nivel socioeconómico de origen como un predictor dicotómico, diferenciando a los jóvenes en hogares con un jefe de educación baja o media de los jóvenes en hogares con un jefe de educación alta.

tab W8
gen W8m = W8
tab W8m
replace W8m= 0 if W8m < 6
replace W8m= 1 if W8m > 0 & W8m < 99
replace W8m= . if W8m == 99
tab W8m	 
label define W8m 0 "Bajo y medio"  /// 
				 1 "Alto" ///			 
label value W8m W8m 
tab W8m	 

* C. Crear las variables necesarias para definir el evento ser madre o padre por primera vez, así como también la censura y el tiempo en que ocurre el evento (a partir de la diferencia entre la edad del hijo mayor y la edad actual del entrevistado/a).

/// Ocurrencia del evento. /// 

// Evento=1 "sucedió el evento" si las personas respondieron con su cantidad de hijos (Z3_N_HIJOS>=1)
// Evento=0 "No sucedió el evento" si las personas respondieron que no tienen hijos (Z3_N_HIJOS=0)

tab Z3_N_HIJOS
replace Z3_N_HIJOS =. if Z3_N_HIJOS == 99
gen evento=.
replace evento= 1 if Z3_N_HIJOS > 0
replace evento= 0 if Z3_N_HIJOS < 1 

/// Censura ///

gen censor =~ evento // Ponemos censura como lo contrario del evento

/// Tiempo en que ocurre el evento. /// 

// edad del hijo mayor

tab Z4_ANOS_HIJO_MAYOR
replace Z4_ANOS_HIJO_MAYOR=. if Z4_ANOS_HIJO_MAYOR > 15

// edad actual del entrevistado/a

tab EDAD_EXACTA

// diferencia entre la edad del hijo mayor y la edad actual del entrevistado/a

gen time=EDAD_EXACTA
replace time= EDAD_EXACTA - Z4_ANOS_HIJO_MAYOR if evento==1
tab time

* D. Elimine los casos en que haya valores perdidos para el evento, la censura o cualquiera de los dos predictores (Sexo y w8)

tab1 evento censor time SEXO W8m, m

replace W8m =. if W8m == 99
drop if time ==.

// organización de los datos en formato persona-período

prsnperd id time censor

// Sacamos los períodos donde no hubo ocurrencia del evento 

 drop if _period <13

*** PREGUNTAS ***

** 1. Utilice como punto de partida el modelo cuadrático de la tarea 5 (con los predictores sexo y nivel socioeconómico).  Esta vez, sin embargo, use nivel socioeconómico de origen como un predictor dicotómico, diferenciando a los jóvenes en hogares con un jefe de educación baja o media de los jóvenes en hogares con un jefe de educación alta. Además, utilizará un nuevo predictor, que identifique a las personas que se identifican con alguna religión (Z21).  

// centramos el tiempo en 26
drop time
gen time= _period-26


/// Modelo lineal /// 

asdoc logit _Y time
estimates store m1
predict h1
gen logith1 = logit(h1)

/// Modelo cuadrático ///

gen time2 = time^2 
asdoc logit _Y time time2 i.SEXO i.W8m i.Z21
estimates store m2
predict h2
gen logith2 = logit(h2)

* a. Explore la posibilidad de que el efecto del sexo varie según nivel socioeconómico, graficando los logit hazard de hombres y mujeres, de los dos niveles socioeconómicos.

sort _period SEXO W8m
by _period SEXO W8m: egen pevent = mean(_Y)
gen logitp = log(pevent/(1-pevent))

twoway (line logitp _period if W8m==0 & SEXO==0) ///
(line logitp _period if W8m==0 & SEXO==1), ///
title("ES Bajo y medio") xtitle("Age") ytitle("Sample logit(hazard)") ///
legend(pos(5) ring(0) col(1) ///
label(1 "Hombre") label(2 "Mujer")) 

twoway (line logitp _period if W8m==1 & SEXO==0) ///
(line logitp _period if W8m==1 & SEXO==1), ///
title("ES Alto") xtitle("Age") ytitle("Sample logit(hazard)") ///
legend(pos(5) ring(0) col(1) ///
label(1 "Hombre") label(2 "Mujer")) 

* b. Haga lo mismo, pero explorando ahora si el efecto de identificarse con una religión es diferente para hombres y mujeres

sort _period Z21 SEXO
by _period Z21 SEXO: egen peventb=mean(_Y)
gen logitpb = log(peventb/(1-peventb))

twoway (line logitpb _period if SEXO==0 & Z21==1) ///
(line logitpb _period if SEXO==0 & Z21==2), ///
title("Hombres") xtitle("Age") ytitle("Sample logit(hazard)") ///
legend(pos(5) ring(0) col(1) ///
label(1 "Sí") label(2 "No")) 

twoway (line logitpb _period if SEXO==1 & Z21==1) ///
(line logitpb _period if SEXO==1 & Z21==2), ///
title("Mujeres") xtitle("Age") ytitle("Sample logit(hazard)") ///
legend(pos(5) ring(0) col(1) ///
label(1 "Sí") label(2 "No")) 

** 2. Estime un modelo cuadrático en que incorpore la interacción sexo-nivel socioeconómico. 

asdoc logit _Y time time2 i.SEXO i.W8m i.Z21 i.SEXO#i.W8m
estimates store m3

** 3. Estime un modelo cuadrático en que incorpore la interacción sexo-identificación religiosa.

asdoc logit _Y time time2 i.SEXO i.W8m i.Z21 i.SEXO#i.Z21 
estimates store m4

** 4. Considere la bondad de ajuste de los modelos que estimó.

asdoc lrtest m2 m3
lrtest m3 m4
lrtest m2 m4
