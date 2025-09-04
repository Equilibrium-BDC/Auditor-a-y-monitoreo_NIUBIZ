* import_encuesta_niubiz_ms_2025.do
*
* 	Imports and aggregates "Estudio de uso y experiencia de soluciones de pago 2025" (ID: encuesta_niubiz_ms_2025) data.
*
*	Inputs:  "Estudio de uso y experiencia de soluciones de pago 2025_WIDE.csv"
*	Outputs: "Estudio de uso y experiencia de soluciones de pago 2025.dta"
*
*	Output by SurveyCTO September 2, 2025 4:50 PM.

* initialize Stata
clear all
set more off
set mem 100m

* initialize workflow-specific parameters
*	Set overwrite_old_data to 1 if you use the review and correction
*	workflow and allow un-approving of submissions. If you do this,
*	incoming data will overwrite old data, so you won't want to make
*	changes to data in your local .dta file (such changes can be
*	overwritten with each new import).
local overwrite_old_data 0

* initialize form-specific parameters
local csvfile "Estudio de uso y experiencia de soluciones de pago 2025_WIDE.csv"
local dtafile "Estudio de uso y experiencia de soluciones de pago 2025.dta"
local corrfile "Estudio de uso y experiencia de soluciones de pago 2025_corrections.csv"
local note_fields1 ""
local text_fields1 "deviceid devicephonenum username device_info duration date_onlyday date_time_today audio_audit caseid raz_social ruc tamanio_empresa giro mp_uso pago_limit100 tom_sp tom_sp_o tm_medios_niubiz"
local text_fields2 "tm_medios_niubiz_o tm_medios_izipay tm_medios_izipay_o tm_medios_culqi tm_medios_culqi_o tm_medios_yape tm_medios_yape_o tm_medios_plin tm_medios_plin_o tm_medios_otro tm_medios_otro_o tm_sp_a tm_uso"
local text_fields3 "uso_1 uso_2 uso_3 uso_4 uso_5 uso_6 uso_7 uso_8 uso_66 tm_uso_o tm_uso_a uso_a_1 uso_a_2 uso_a_3 uso_a_4 uso_a_5 uso_a_6 uso_a_7 uso_a_8 uso_a_66 tm_uso_a_o sow_limit100 mot_pref mot_pref_o"
local text_fields4 "no_uso_niubiz no_uso_niubiz_o mot_rechazo mot_rechazo_o dif_a_niubiz ima_niubiz dif_a_izipay ima_izipay niubiz_frec_prob_uso_o niubiz_tip_prob_exp niubiz_tip_prob_exp_o izipay_frec_prob_uso_o"
local text_fields5 "izipay_tip_prob_exp izipay_tip_prob_exp_o culqi_frec_prob_uso_o culqi_tip_prob_exp culqi_tip_prob_exp_o niubiz_frec_abo_o niubiz_inco_abo niubiz_inco_abo_o izipay_frec_abo_o izipay_inco_abo"
local text_fields6 "izipay_inco_abo_o culqi_frec_abo_o culqi_inco_abo culqi_inco_abo_o temp_abo temp_abo_o rep_abo rep_abo_o notf_abo_o def_segu atrib_cambio_mp uso_bancos banco_1 banco_2 banco_3 banco_4 banco_5 banco_6"
local text_fields7 "banco_7 banco_8 banco_9 banco_10 banco_11 banco_12 banco_66 uso_bancos_o princ_banco_o mot_nouso mot_nouso_o tipo_mp med_com med_com_o obs veri_1 veri_2 instanceid"
local date_fields1 ""
local datetime_fields1 "submissiondate starttime endtime"

disp
disp "Starting import of: `csvfile'"
disp

* import data from primary .csv file
insheet using "`csvfile'", names clear

* drop extra table-list columns
cap drop reserved_name_for_field_*
cap drop generated_table_list_lab*

* continue only if there's at least one row of data to import
if _N>0 {
	* drop note fields (since they don't contain any real data)
	forvalues i = 1/100 {
		if "`note_fields`i''" ~= "" {
			drop `note_fields`i''
		}
	}
	
	* format date and date/time fields
	forvalues i = 1/100 {
		if "`datetime_fields`i''" ~= "" {
			foreach dtvarlist in `datetime_fields`i'' {
				cap unab dtvarlist : `dtvarlist'
				if _rc==0 {
					foreach dtvar in `dtvarlist' {
						tempvar tempdtvar
						rename `dtvar' `tempdtvar'
						gen double `dtvar'=.
						cap replace `dtvar'=clock(`tempdtvar',"DMYhms",2025)
						* automatically try without seconds, just in case
						cap replace `dtvar'=clock(`tempdtvar',"DMYhm",2025) if `dtvar'==. & `tempdtvar'~=""
						format %tc `dtvar'
						drop `tempdtvar'
					}
				}
			}
		}
		if "`date_fields`i''" ~= "" {
			foreach dtvarlist in `date_fields`i'' {
				cap unab dtvarlist : `dtvarlist'
				if _rc==0 {
					foreach dtvar in `dtvarlist' {
						tempvar tempdtvar
						rename `dtvar' `tempdtvar'
						gen double `dtvar'=.
						cap replace `dtvar'=date(`tempdtvar',"DMY",2025)
						format %td `dtvar'
						drop `tempdtvar'
					}
				}
			}
		}
	}

	* ensure that text fields are always imported as strings (with "" for missing values)
	* (note that we treat "calculate" fields as text; you can destring later if you wish)
	tempvar ismissingvar
	quietly: gen `ismissingvar'=.
	forvalues i = 1/100 {
		if "`text_fields`i''" ~= "" {
			foreach svarlist in `text_fields`i'' {
				cap unab svarlist : `svarlist'
				if _rc==0 {
					foreach stringvar in `svarlist' {
						quietly: replace `ismissingvar'=.
						quietly: cap replace `ismissingvar'=1 if `stringvar'==.
						cap tostring `stringvar', format(%100.0g) replace
						cap replace `stringvar'="" if `ismissingvar'==1
					}
				}
			}
		}
	}
	quietly: drop `ismissingvar'


	* consolidate unique ID into "key" variable
	replace key=instanceid if key==""
	drop instanceid


	* label variables
	label variable key "Unique submission ID"
	cap label variable submissiondate "Date/time submitted"
	cap label variable formdef_version "Form version used on device"
	cap label variable review_status "Review status"
	cap label variable review_comments "Comments made during review"
	cap label variable review_corrections "Corrections made during review"


	label variable coordslatitude "Encuestador: Por favor registre su ubicación (latitude)"
	note coordslatitude: "Encuestador: Por favor registre su ubicación (latitude)"

	label variable coordslongitude "Encuestador: Por favor registre su ubicación (longitude)"
	note coordslongitude: "Encuestador: Por favor registre su ubicación (longitude)"

	label variable coordsaltitude "Encuestador: Por favor registre su ubicación (altitude)"
	note coordsaltitude: "Encuestador: Por favor registre su ubicación (altitude)"

	label variable coordsaccuracy "Encuestador: Por favor registre su ubicación (accuracy)"
	note coordsaccuracy: "Encuestador: Por favor registre su ubicación (accuracy)"

	label variable f1 "¿Usted es el dueño o encargado de este negocio?"
	note f1: "¿Usted es el dueño o encargado de este negocio?"
	label define f1 1 "Dueño" 2 "Encargado" 3 "Ninguno"
	label values f1 f1

	label variable f2 "¿Es usted la persona encargada de tomar la decisión de contratación de qué prove"
	note f2: "¿Es usted la persona encargada de tomar la decisión de contratación de qué proveedor de medios de pagos usar en su negocio?"
	label define f2 1 "Sí" 2 "No"
	label values f2 f2

	label variable f3 "¿Utiliza algún medio de pago diferente a efectivo en su negocio?"
	note f3: "¿Utiliza algún medio de pago diferente a efectivo en su negocio?"
	label define f3 1 "Sí" 2 "No"
	label values f3 f3

	label variable f4 "Antes de comenzar la encuesta y para fines de supervisión de nuestro trabajo, qu"
	note f4: "Antes de comenzar la encuesta y para fines de supervisión de nuestro trabajo, queremos preguntarle si ¿su negocio tiene RUC y razón social?"
	label define f4 1 "Sí" 2 "No"
	label values f4 f4

	label variable raz_social "Razón Social:"
	note raz_social: "Razón Social:"

	label variable ruc "RUC:"
	note ruc: "RUC:"

	label variable dep "¿En qué departamento se encuentra el negocio encuestado?"
	note dep: "¿En qué departamento se encuentra el negocio encuestado?"
	label define dep 1 "Lima Metropolitana" 2 "Callao" 3 "Cusco" 4 "Arequipa" 5 "Trujillo" 6 "Piura"
	label values dep dep

	label variable vol_men "¿Cuánto es el total de ingresos mensuales de su negocio/empresa?"
	note vol_men: "¿Cuánto es el total de ingresos mensuales de su negocio/empresa?"
	label define vol_men 1 "Menos de S/. 5,000" 2 "S/. 5,001 - S/.10,000" 3 "S/. 10,001 - S/.12,500" 4 "S/. 12,501 - S/. 20,833" 5 "S/. 20,834 - S/. 33,333" 6 "S/. 33,334 - S/. 58,333" 7 "S/. 58,334 - S/. 83,333" 8 "S/. 83,334 - S/. 250,000" 9 "S/. 250,001 - S/. 666,667" 10 "S/. 666,668 - S/. 2,000,000" 11 "Más de S/. 2,000,000"
	label values vol_men vol_men

	label variable gen "¿Género? [Encuestador]: Marcar según observación"
	note gen: "¿Género? [Encuestador]: Marcar según observación"
	label define gen 1 "Mujer" 2 "Hombre"
	label values gen gen

	label variable edad "¿Podría indicarme su edad?"
	note edad: "¿Podría indicarme su edad?"
	label define edad 1 "De 19 a 30 años" 2 "De 31 a 45 años" 3 "De 46 a 65 años" 4 "Más de 65 años"
	label values edad edad

	label variable giro "¿En cuál de los siguientes rubros ejerce su actividad de negocio principal? [Sel"
	note giro: "¿En cuál de los siguientes rubros ejerce su actividad de negocio principal? [Selección múltiple]"

	label variable mp_uso "¿Qué medios de pago utilizas actualmente en tu negocio? [Selección múltiple]"
	note mp_uso: "¿Qué medios de pago utilizas actualmente en tu negocio? [Selección múltiple]"

	label variable mp_sw_efectivo "Efectivo"
	note mp_sw_efectivo: "Efectivo"

	label variable mp_sw_tarjeta "Tarjetas de Débito, Crédito"
	note mp_sw_tarjeta: "Tarjetas de Débito, Crédito"

	label variable mp_sw_transferencia "Transferencias bancarias (Desde Web, Agencias físicas o Aplicativos de Bancos)"
	note mp_sw_transferencia: "Transferencias bancarias (Desde Web, Agencias físicas o Aplicativos de Bancos)"

	label variable mp_sw_billetera "Billeteras Digitales (Yape, Plin, Tunki, Etc.)"
	note mp_sw_billetera: "Billeteras Digitales (Yape, Plin, Tunki, Etc.)"

	label variable mp_sw_pago "Pagos directos desde el celular en los que el cliente acerca su equipo al termin"
	note mp_sw_pago: "Pagos directos desde el celular en los que el cliente acerca su equipo al terminal de pago (Apple Pay)"

	label variable tom_sp "¿Qué empresas que brindan soluciones/servicios para que sus clientes paguen con "
	note tom_sp: "¿Qué empresas que brindan soluciones/servicios para que sus clientes paguen con tarjetas de crédito, débito o billeteras digitales conoce? (Espontánea) [Selección múltiple]"

	label variable tom_sp_o "Especifique la empresa"
	note tom_sp_o: "Especifique la empresa"

	label variable tm_medios_niubiz "¿Cómo conociste a Niubiz? [Selección múltiple]"
	note tm_medios_niubiz: "¿Cómo conociste a Niubiz? [Selección múltiple]"

	label variable tm_medios_niubiz_o "Especifique el otro medio por el cual conoció Niubiz:"
	note tm_medios_niubiz_o: "Especifique el otro medio por el cual conoció Niubiz:"

	label variable tm_medios_izipay "¿Cómo conociste a Izipay? [Selección múltiple]"
	note tm_medios_izipay: "¿Cómo conociste a Izipay? [Selección múltiple]"

	label variable tm_medios_izipay_o "Especifique el otro medio por el cual conoció Izipay:"
	note tm_medios_izipay_o: "Especifique el otro medio por el cual conoció Izipay:"

	label variable tm_medios_culqi "¿Cómo conociste a Culqi? [Selección múltiple]"
	note tm_medios_culqi: "¿Cómo conociste a Culqi? [Selección múltiple]"

	label variable tm_medios_culqi_o "Especifique el otro medio por el cual conoció Culqi:"
	note tm_medios_culqi_o: "Especifique el otro medio por el cual conoció Culqi:"

	label variable tm_medios_yape "¿Cómo conociste a Yape? [Selección múltiple]"
	note tm_medios_yape: "¿Cómo conociste a Yape? [Selección múltiple]"

	label variable tm_medios_yape_o "Especifique el otro medio por el cual conoció Yape:"
	note tm_medios_yape_o: "Especifique el otro medio por el cual conoció Yape:"

	label variable tm_medios_plin "¿Cómo conociste a Plin? [Selección múltiple]"
	note tm_medios_plin: "¿Cómo conociste a Plin? [Selección múltiple]"

	label variable tm_medios_plin_o "Especifique el otro medio por el cual conoció Plin:"
	note tm_medios_plin_o: "Especifique el otro medio por el cual conoció Plin:"

	label variable tm_medios_otro "¿Cómo conociste a la otra empresa que mencionaste? [Selección múltiple]"
	note tm_medios_otro: "¿Cómo conociste a la otra empresa que mencionaste? [Selección múltiple]"

	label variable tm_medios_otro_o "Especifique el otro medio por el cual conoció {TOM_SP_o}:"
	note tm_medios_otro_o: "Especifique el otro medio por el cual conoció {TOM_SP_o}:"

	label variable tm_sp_a "De las siguientes empresas de soluciones/servicios para que sus clientes paguen "
	note tm_sp_a: "De las siguientes empresas de soluciones/servicios para que sus clientes paguen con tarjetas de crédito, débito ¿cuáles conoce? [Selección múltiple]"

	label variable tm_uso "¿Cuál o cuáles de estas compañías de soluciones de pago ha usado en su negocio? "
	note tm_uso: "¿Cuál o cuáles de estas compañías de soluciones de pago ha usado en su negocio? [Selección múltiple]"

	label variable tm_uso_o "Especifique la compañía que ha usado"
	note tm_uso_o: "Especifique la compañía que ha usado"

	label variable tm_uso_a "¿Cuál o cuáles de estas compañías de soluciones de pago usa actualmente? [Selecc"
	note tm_uso_a: "¿Cuál o cuáles de estas compañías de soluciones de pago usa actualmente? [Selección múltiple]"

	label variable tm_uso_a_o "Especifique la compañía que usa actualmente"
	note tm_uso_a_o: "Especifique la compañía que usa actualmente"

	label variable sw_sp_a_01 "Niubiz"
	note sw_sp_a_01: "Niubiz"

	label variable sw_sp_a_02 "Izipay"
	note sw_sp_a_02: "Izipay"

	label variable sw_sp_a_03 "Culqi"
	note sw_sp_a_03: "Culqi"

	label variable sw_sp_a_04 "OpenPay"
	note sw_sp_a_04: "OpenPay"

	label variable sw_sp_a_05 "MercadoPago"
	note sw_sp_a_05: "MercadoPago"

	label variable sw_sp_a_06 "YAPE"
	note sw_sp_a_06: "YAPE"

	label variable sw_sp_a_07 "Plin"
	note sw_sp_a_07: "Plin"

	label variable sw_sp_a_08 "Vendemas"
	note sw_sp_a_08: "Vendemas"

	label variable sw_sp_a_66 "Otros (juntar a todos los otros)"
	note sw_sp_a_66: "Otros (juntar a todos los otros)"

	label variable tm_pref "De las compañías de soluciones de pago que señaló aceptar ¿cuál es la marca que "
	note tm_pref: "De las compañías de soluciones de pago que señaló aceptar ¿cuál es la marca que usted prefiere usar como medio de cobro principal para sus clientes?"
	label define tm_pref 1 "Niubiz" 2 "Izipay" 3 "Culqi" 4 "OpenPay" 5 "MercadoPago" 6 "Yape" 7 "Plin" 8 "Vendemas"
	label values tm_pref tm_pref

	label variable mot_pref "¿Por qué es tu marca preferida? [Selección múltiple]"
	note mot_pref: "¿Por qué es tu marca preferida? [Selección múltiple]"

	label variable mot_pref_o "Especifique la razón"
	note mot_pref_o: "Especifique la razón"

	label variable nps_nb "Si un amigo/familiar o colega del sector le pidiese consejo sobre con qué compañ"
	note nps_nb: "Si un amigo/familiar o colega del sector le pidiese consejo sobre con qué compañía de medios de pago trabajar, ¿Con qué probabilidad recomendaría NIUBIZ, en una escala del 0 al 10? donde 0 es No la recomendaría en absoluto, y 10 La recomendaría Totalmente"
	label define nps_nb 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values nps_nb nps_nb

	label variable nps_izi "Si un amigo/familiar o colega del sector le pidiese consejo sobre con qué compañ"
	note nps_izi: "Si un amigo/familiar o colega del sector le pidiese consejo sobre con qué compañía de medios de pago trabajar, ¿Con qué probabilidad recomendaría IZIPAY, en una escala del 0 al 10? donde 0 es No la recomendaría en absoluto, y 10 La recomendaría Totalmente"
	label define nps_izi 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values nps_izi nps_izi

	label variable nps_cq "Si un amigo/familiar o colega del sector le pidiese consejo sobre con qué compañ"
	note nps_cq: "Si un amigo/familiar o colega del sector le pidiese consejo sobre con qué compañía de medios de pago trabajar, ¿Con qué probabilidad recomendaría CULQI, en una escala del 0 al 10? donde 0 es No la recomendaría en absoluto, y 10 La recomendaría Totalmente"
	label define nps_cq 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values nps_cq nps_cq

	label variable nps_op "Si un amigo/familiar o colega del sector le pidiese consejo sobre con qué compañ"
	note nps_op: "Si un amigo/familiar o colega del sector le pidiese consejo sobre con qué compañía de medios de pago trabajar, ¿Con qué probabilidad recomendaría OPEN PAY, en una escala del 0 al 10? donde 0 es No la recomendaría en absoluto, y 10 La recomendaría Totalmente"
	label define nps_op 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values nps_op nps_op

	label variable nps_mpago "Si un amigo/familiar o colega del sector le pidiese consejo sobre con qué compañ"
	note nps_mpago: "Si un amigo/familiar o colega del sector le pidiese consejo sobre con qué compañía de medios de pago trabajar, ¿Con qué probabilidad recomendaría VENDEMAS, en una escala del 0 al 10? donde 0 es No la recomendaría en absoluto, y 10 La recomendaría Totalmente"
	label define nps_mpago 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values nps_mpago nps_mpago

	label variable nps_yape "Si un amigo/familiar o colega del sector le pidiese consejo sobre con qué compañ"
	note nps_yape: "Si un amigo/familiar o colega del sector le pidiese consejo sobre con qué compañía de medios de pago trabajar, ¿Con qué probabilidad recomendaría YAPE, en una escala del 0 al 10? donde 0 es No la recomendaría en absoluto, y 10 La recomendaría Totalmente"
	label define nps_yape 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values nps_yape nps_yape

	label variable nps_plin "Si un amigo/familiar o colega del sector le pidiese consejo sobre con qué compañ"
	note nps_plin: "Si un amigo/familiar o colega del sector le pidiese consejo sobre con qué compañía de medios de pago trabajar, ¿Con qué probabilidad recomendaría PLIN, en una escala del 0 al 10? donde 0 es No la recomendaría en absoluto, y 10 La recomendaría Totalmente"
	label define nps_plin 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values nps_plin nps_plin

	label variable no_uso_niubiz "¿Por qué motivo dejaste de usar a Niubiz en tu negocio? [Selección múltiple]"
	note no_uso_niubiz: "¿Por qué motivo dejaste de usar a Niubiz en tu negocio? [Selección múltiple]"

	label variable no_uso_niubiz_o "Especifique el motivo"
	note no_uso_niubiz_o: "Especifique el motivo"

	label variable tm_rec "Dígame por favor ¿Cuál de las siguientes compañías de soluciones de pago preferi"
	note tm_rec: "Dígame por favor ¿Cuál de las siguientes compañías de soluciones de pago preferiría no contratar o no tener funcionando en su negocio?"
	label define tm_rec 1 "Niubiz" 2 "Izipay" 3 "Culqi" 4 "OpenPay" 5 "Vendemas" 6 "YAPE" 7 "Plin" 999 "Ninguno"
	label values tm_rec tm_rec

	label variable mot_rechazo "¿Por qué definitivamente no trabajarías con esta marca? (Espontánea) [Selección "
	note mot_rechazo: "¿Por qué definitivamente no trabajarías con esta marca? (Espontánea) [Selección múltiple]"

	label variable mot_rechazo_o "Especifique el motivo"
	note mot_rechazo_o: "Especifique el motivo"

	label variable dif_niubiz "¿Qué tan única considera que es NIUBIZ en comparación con otras marcas en el mer"
	note dif_niubiz: "¿Qué tan única considera que es NIUBIZ en comparación con otras marcas en el mercado?"
	label define dif_niubiz 0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values dif_niubiz dif_niubiz

	label variable dif_a_niubiz "¿Cuál es la principal característica que tiene NIUBIZ que la diferencia de otras"
	note dif_a_niubiz: "¿Cuál es la principal característica que tiene NIUBIZ que la diferencia de otras marcas?"

	label variable rel_niubiz "¿Qué tan relevante consideras a NIUBIZ al momento de evaluar opciones de solucio"
	note rel_niubiz: "¿Qué tan relevante consideras a NIUBIZ al momento de evaluar opciones de soluciones de pago para tu negocio?"
	label define rel_niubiz 0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values rel_niubiz rel_niubiz

	label variable fam_niubiz "¿Cuánto tiempo usaste o llevas usando NIUBIZ en tu negocio?"
	note fam_niubiz: "¿Cuánto tiempo usaste o llevas usando NIUBIZ en tu negocio?"
	label define fam_niubiz 1 "Menos de 6 meses" 2 "6 meses a 1 año" 3 "1 a 2 años" 4 "De 2 a 3 años" 5 "Mas de 3 años"
	label values fam_niubiz fam_niubiz

	label variable ima_niubiz "¿Cuál es la primera palabra que le viene a la mente cuando piensa en NIUBIZ?"
	note ima_niubiz: "¿Cuál es la primera palabra que le viene a la mente cuando piensa en NIUBIZ?"

	label variable dif_izipay "¿Qué tan única considera que es IZIPAY en comparación con otras marcas en el mer"
	note dif_izipay: "¿Qué tan única considera que es IZIPAY en comparación con otras marcas en el mercado?"
	label define dif_izipay 0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values dif_izipay dif_izipay

	label variable dif_a_izipay "¿Cuál es la principal característica que tiene IZIPAY que la diferencia de otras"
	note dif_a_izipay: "¿Cuál es la principal característica que tiene IZIPAY que la diferencia de otras marcas?"

	label variable rel_izipay "¿Qué tan relevante consideras a IZIPAY al momento de evaluar opciones de solucio"
	note rel_izipay: "¿Qué tan relevante consideras a IZIPAY al momento de evaluar opciones de soluciones de pago para tu negocio?"
	label define rel_izipay 0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values rel_izipay rel_izipay

	label variable fam_izipay "¿Cuánto tiempo usaste o Llevas usando IZIPAY en tu negocio?"
	note fam_izipay: "¿Cuánto tiempo usaste o Llevas usando IZIPAY en tu negocio?"
	label define fam_izipay 1 "Menos de 6 meses" 2 "6 meses a 1 año" 3 "1 a 2 años" 4 "De 2 a 3 años" 5 "Mas de 3 años"
	label values fam_izipay fam_izipay

	label variable ima_izipay "¿Cuál es la primera palabra que le viene a la mente cuando piensa en IZIPAY?"
	note ima_izipay: "¿Cuál es la primera palabra que le viene a la mente cuando piensa en IZIPAY?"

	label variable at01 "Propuesta de valor (tiene un portafolio de productos y servicios que se adecúa a"
	note at01: "Propuesta de valor (tiene un portafolio de productos y servicios que se adecúa a sus necesidades y acepta todos los medios de pago)"
	label define at01 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values at01 at01

	label variable at02 "Préstamos (Ofrece prestamos que ayudan al negocio a crecer)"
	note at02: "Préstamos (Ofrece prestamos que ayudan al negocio a crecer)"
	label define at02 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values at02 at02

	label variable at03 "Tasas y comisiones (Ofrece buenas tasas / comisiones por transacción)"
	note at03: "Tasas y comisiones (Ofrece buenas tasas / comisiones por transacción)"
	label define at03 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values at03 at03

	label variable at04 "Simplicidad (Cuenta con productos y servicios fáciles de usar y entender)"
	note at04: "Simplicidad (Cuenta con productos y servicios fáciles de usar y entender)"
	label define at04 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values at04 at04

	label variable at05 "Seguridad (que los pagos sean seguros, sin fraudes)"
	note at05: "Seguridad (que los pagos sean seguros, sin fraudes)"
	label define at05 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values at05 at05

	label variable at06 "Funcionamiento de Soluciones (Buen funcionamiento de las soluciones de cobro, se"
	note at06: "Funcionamiento de Soluciones (Buen funcionamiento de las soluciones de cobro, se procesan los pagos de manera rápida y sin errores)"
	label define at06 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values at06 at06

	label variable at07 "Calidad en la atención (atención rápida y efectiva a sus consultas/solicitudes/p"
	note at07: "Calidad en la atención (atención rápida y efectiva a sus consultas/solicitudes/problemas)"
	label define at07 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values at07 at07

	label variable at08 "Cumplimiento de promesa de servicio (funcionamiento de equipos y soluciones digi"
	note at08: "Cumplimiento de promesa de servicio (funcionamiento de equipos y soluciones digitales)"
	label define at08 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values at08 at08

	label variable at09 "Rapidez en los depósitos (que los depósitos se realicen lo más cerca posible a l"
	note at09: "Rapidez en los depósitos (que los depósitos se realicen lo más cerca posible a la fecha de la transacción)"
	label define at09 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values at09 at09

	label variable at10 "Relación costo/beneficio (Que brinde alto valor por el costo que se paga)"
	note at10: "Relación costo/beneficio (Que brinde alto valor por el costo que se paga)"
	label define at10 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values at10 at10

	label variable at11 "Herramientas de autogestión (Cuenta con aplicaciones, plataformas o webs que me "
	note at11: "Herramientas de autogestión (Cuenta con aplicaciones, plataformas o webs que me ayudan a gestionar mejor mis ventas) (Calidad de reportes, herramientas, etc)"
	label define at11 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values at11 at11

	label variable at12 "Innovación (que la empresa constantemente reinvente sus productos y servicios pe"
	note at12: "Innovación (que la empresa constantemente reinvente sus productos y servicios pensado en las necesidades del cliente)"
	label define at12 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values at12 at12

	label variable at13 "Liderazgo y solidez (que sea una empresa reconocida y líder en el mercado)"
	note at13: "Liderazgo y solidez (que sea una empresa reconocida y líder en el mercado)"
	label define at13 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values at13 at13

	label variable at14 "Socio estratégico (la empresa se interesa en que mi negocio crezca, es mi aliado"
	note at14: "Socio estratégico (la empresa se interesa en que mi negocio crezca, es mi aliado)"
	label define at14 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values at14 at14

	label variable at15 "Responsabilidad social (que sea una empresa que cuide su impacto con la sociedad"
	note at15: "Responsabilidad social (que sea una empresa que cuide su impacto con la sociedad y el ambiente)"
	label define at15 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values at15 at15

	label variable at16 "Cercanía (Hace sentir a todos sus clientes valorados)"
	note at16: "Cercanía (Hace sentir a todos sus clientes valorados)"
	label define at16 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values at16 at16

	label variable at17 "Libertad de elección (puedo elegir el banco donde me abonan mis ventas, sin que "
	note at17: "Libertad de elección (puedo elegir el banco donde me abonan mis ventas, sin que eso afecte mis condiciones comerciales: tiempo de abono, tasas, etc.)"
	label define at17 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values at17 at17

	label variable six_1 "¿Cuáles son los 6 atributos que más valora al elegir una solución de pago para t"
	note six_1: "¿Cuáles son los 6 atributos que más valora al elegir una solución de pago para tu negocio? Seleccione el PRIMER atributo que más valora al elegir una solución de pago para su negocio"
	label define six_1 1 "Tasas y Comisiones" 2 "Seguridad" 3 "Calidad en la atención" 4 "Rapidez en los depósitos" 5 "Herramientas de Autogestión" 6 "Liderazgo y solidez" 7 "Responsabilidad social" 8 "Libertad de elección" 9 "Préstamos" 10 "Simplicidad" 11 "Funcionamiento de Soluciones" 12 "Cumplimiento de Promesa" 13 "Relación Costo /beneficio" 14 "Innovación" 15 "Socio Estratégico" 16 "Cercanía"
	label values six_1 six_1

	label variable six_2 "Seleccione el SEGUNDO atributo que más valora al elegir una solución de pago par"
	note six_2: "Seleccione el SEGUNDO atributo que más valora al elegir una solución de pago para su negocio"
	label define six_2 1 "Tasas y Comisiones" 2 "Seguridad" 3 "Calidad en la atención" 4 "Rapidez en los depósitos" 5 "Herramientas de Autogestión" 6 "Liderazgo y solidez" 7 "Responsabilidad social" 8 "Libertad de elección" 9 "Préstamos" 10 "Simplicidad" 11 "Funcionamiento de Soluciones" 12 "Cumplimiento de Promesa" 13 "Relación Costo /beneficio" 14 "Innovación" 15 "Socio Estratégico" 16 "Cercanía"
	label values six_2 six_2

	label variable six_3 "Seleccione el TERCER atributo que más valora al elegir una solución de pago para"
	note six_3: "Seleccione el TERCER atributo que más valora al elegir una solución de pago para su negocio"
	label define six_3 1 "Tasas y Comisiones" 2 "Seguridad" 3 "Calidad en la atención" 4 "Rapidez en los depósitos" 5 "Herramientas de Autogestión" 6 "Liderazgo y solidez" 7 "Responsabilidad social" 8 "Libertad de elección" 9 "Préstamos" 10 "Simplicidad" 11 "Funcionamiento de Soluciones" 12 "Cumplimiento de Promesa" 13 "Relación Costo /beneficio" 14 "Innovación" 15 "Socio Estratégico" 16 "Cercanía"
	label values six_3 six_3

	label variable six_4 "Seleccione el CUARTO atributo que más valora al elegir una solución de pago para"
	note six_4: "Seleccione el CUARTO atributo que más valora al elegir una solución de pago para su negocio"
	label define six_4 1 "Tasas y Comisiones" 2 "Seguridad" 3 "Calidad en la atención" 4 "Rapidez en los depósitos" 5 "Herramientas de Autogestión" 6 "Liderazgo y solidez" 7 "Responsabilidad social" 8 "Libertad de elección" 9 "Préstamos" 10 "Simplicidad" 11 "Funcionamiento de Soluciones" 12 "Cumplimiento de Promesa" 13 "Relación Costo /beneficio" 14 "Innovación" 15 "Socio Estratégico" 16 "Cercanía"
	label values six_4 six_4

	label variable six_5 "Seleccione el QUINTO atributo que más valora al elegir una solución de pago para"
	note six_5: "Seleccione el QUINTO atributo que más valora al elegir una solución de pago para su negocio"
	label define six_5 1 "Tasas y Comisiones" 2 "Seguridad" 3 "Calidad en la atención" 4 "Rapidez en los depósitos" 5 "Herramientas de Autogestión" 6 "Liderazgo y solidez" 7 "Responsabilidad social" 8 "Libertad de elección" 9 "Préstamos" 10 "Simplicidad" 11 "Funcionamiento de Soluciones" 12 "Cumplimiento de Promesa" 13 "Relación Costo /beneficio" 14 "Innovación" 15 "Socio Estratégico" 16 "Cercanía"
	label values six_5 six_5

	label variable six_6 "Seleccione el SEXTO atributo que más valora al elegir una solución de pago para "
	note six_6: "Seleccione el SEXTO atributo que más valora al elegir una solución de pago para su negocio"
	label define six_6 1 "Tasas y Comisiones" 2 "Seguridad" 3 "Calidad en la atención" 4 "Rapidez en los depósitos" 5 "Herramientas de Autogestión" 6 "Liderazgo y solidez" 7 "Responsabilidad social" 8 "Libertad de elección" 9 "Préstamos" 10 "Simplicidad" 11 "Funcionamiento de Soluciones" 12 "Cumplimiento de Promesa" 13 "Relación Costo /beneficio" 14 "Innovación" 15 "Socio Estratégico" 16 "Cercanía"
	label values six_6 six_6

	label variable niubiz_at_01 "NIUBIZ tiene el portafolio de productos y servicios que más se adecúa a mis nece"
	note niubiz_at_01: "NIUBIZ tiene el portafolio de productos y servicios que más se adecúa a mis necesidades y acepta todos los medios de pago"
	label define niubiz_at_01 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values niubiz_at_01 niubiz_at_01

	label variable niubiz_at_02 "NIUBIZ tiene ofrecimiento de préstamos para capital de trabajo"
	note niubiz_at_02: "NIUBIZ tiene ofrecimiento de préstamos para capital de trabajo"
	label define niubiz_at_02 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values niubiz_at_02 niubiz_at_02

	label variable niubiz_at_03 "NIUBIZ brinda buenas tasas / comisiones por transacción"
	note niubiz_at_03: "NIUBIZ brinda buenas tasas / comisiones por transacción"
	label define niubiz_at_03 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values niubiz_at_03 niubiz_at_03

	label variable niubiz_at_04 "NIUBIZ brinda los productos y servicios más fáciles de usar y entender en el mer"
	note niubiz_at_04: "NIUBIZ brinda los productos y servicios más fáciles de usar y entender en el mercado"
	label define niubiz_at_04 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values niubiz_at_04 niubiz_at_04

	label variable niubiz_at_05 "NIUBIZ es la empresa con mayor seguridad en los pagos (pagos seguros, sin fraude"
	note niubiz_at_05: "NIUBIZ es la empresa con mayor seguridad en los pagos (pagos seguros, sin fraudes)"
	label define niubiz_at_05 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values niubiz_at_05 niubiz_at_05

	label variable niubiz_at_06 "NIUBIZ brinda un buen servicio para el cobro de mis ventas, sin incidencias y si"
	note niubiz_at_06: "NIUBIZ brinda un buen servicio para el cobro de mis ventas, sin incidencias y sin demoras en el cobro."
	label define niubiz_at_06 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values niubiz_at_06 niubiz_at_06

	label variable niubiz_at_07 "NIUBIZ es la empresa con atención más rápida y efectiva a mis consultas/solicitu"
	note niubiz_at_07: "NIUBIZ es la empresa con atención más rápida y efectiva a mis consultas/solicitudes/problemas"
	label define niubiz_at_07 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values niubiz_at_07 niubiz_at_07

	label variable niubiz_at_08 "NIUBIZ tiene los mejores equipos y soluciones digitales"
	note niubiz_at_08: "NIUBIZ tiene los mejores equipos y soluciones digitales"
	label define niubiz_at_08 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values niubiz_at_08 niubiz_at_08

	label variable niubiz_at_09 "NIUBIZ deposita lo más rápido luego de realizadas las ventas"
	note niubiz_at_09: "NIUBIZ deposita lo más rápido luego de realizadas las ventas"
	label define niubiz_at_09 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values niubiz_at_09 niubiz_at_09

	label variable niubiz_at_10 "NIUBIZ es la empresa que más valor me da por el costo que pago"
	note niubiz_at_10: "NIUBIZ es la empresa que más valor me da por el costo que pago"
	label define niubiz_at_10 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values niubiz_at_10 niubiz_at_10

	label variable niubiz_at_11 "NIUBIZ ofrece buenas herramientas para gestionar mis ventas (reportes, dashbords"
	note niubiz_at_11: "NIUBIZ ofrece buenas herramientas para gestionar mis ventas (reportes, dashbords)"
	label define niubiz_at_11 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values niubiz_at_11 niubiz_at_11

	label variable niubiz_at_12 "NIUBIZ es la empresa más innovadora (constantemente reinventa sus productos y se"
	note niubiz_at_12: "NIUBIZ es la empresa más innovadora (constantemente reinventa sus productos y servicios pensando en lo que necesitan sus clientes)"
	label define niubiz_at_12 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values niubiz_at_12 niubiz_at_12

	label variable niubiz_at_13 "NIUBIZ es la empresa más reconocida que lidera el mercado"
	note niubiz_at_13: "NIUBIZ es la empresa más reconocida que lidera el mercado"
	label define niubiz_at_13 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values niubiz_at_13 niubiz_at_13

	label variable niubiz_at_14 "NIUBIZ es un socio estratégico para mi negocio, ya que se interesa por que mi ne"
	note niubiz_at_14: "NIUBIZ es un socio estratégico para mi negocio, ya que se interesa por que mi negocio crezca"
	label define niubiz_at_14 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values niubiz_at_14 niubiz_at_14

	label variable niubiz_at_15 "NIUBIZ es la empresa que más cuida su impacto en la sociedad y el medio ambiente"
	note niubiz_at_15: "NIUBIZ es la empresa que más cuida su impacto en la sociedad y el medio ambiente."
	label define niubiz_at_15 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values niubiz_at_15 niubiz_at_15

	label variable niubiz_at_16 "NIUBIZ es la empresa que hace sentir a todos sus clientes valorados"
	note niubiz_at_16: "NIUBIZ es la empresa que hace sentir a todos sus clientes valorados"
	label define niubiz_at_16 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values niubiz_at_16 niubiz_at_16

	label variable niubiz_at_17 "NIUBIZ es la empresa que me da la libertad de elegir el banco donde me abonan mi"
	note niubiz_at_17: "NIUBIZ es la empresa que me da la libertad de elegir el banco donde me abonan mis ventas, sin que eso afecte mis condiciones comerciales: tiempo de abono, tasas, etc.)"
	label define niubiz_at_17 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values niubiz_at_17 niubiz_at_17

	label variable izipay_at_01 "IZIPAY tiene el portafolio de productos y servicios que más se adecúa a mis nece"
	note izipay_at_01: "IZIPAY tiene el portafolio de productos y servicios que más se adecúa a mis necesidades y acepta todos los medios de pago"
	label define izipay_at_01 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values izipay_at_01 izipay_at_01

	label variable izipay_at_02 "IZIPAY tiene ofrecimiento de préstamos para capital de trabajo"
	note izipay_at_02: "IZIPAY tiene ofrecimiento de préstamos para capital de trabajo"
	label define izipay_at_02 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values izipay_at_02 izipay_at_02

	label variable izipay_at_03 "IZIPAY brinda buenas tasas / comisiones por transacción"
	note izipay_at_03: "IZIPAY brinda buenas tasas / comisiones por transacción"
	label define izipay_at_03 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values izipay_at_03 izipay_at_03

	label variable izipay_at_04 "IZIPAY brinda los productos y servicios más fáciles de usar y entender en el mer"
	note izipay_at_04: "IZIPAY brinda los productos y servicios más fáciles de usar y entender en el mercado"
	label define izipay_at_04 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values izipay_at_04 izipay_at_04

	label variable izipay_at_05 "IZIPAY es la empresa con mayor seguridad en los pagos (pagos seguros, sin fraude"
	note izipay_at_05: "IZIPAY es la empresa con mayor seguridad en los pagos (pagos seguros, sin fraudes)"
	label define izipay_at_05 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values izipay_at_05 izipay_at_05

	label variable izipay_at_06 "IZIPAY brinda un buen servicio para el cobro de mis ventas, sin incidencias y si"
	note izipay_at_06: "IZIPAY brinda un buen servicio para el cobro de mis ventas, sin incidencias y sin demoras en el cobro."
	label define izipay_at_06 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values izipay_at_06 izipay_at_06

	label variable izipay_at_07 "IZIPAY es la empresa con atención más rápida y efectiva a mis consultas/solicitu"
	note izipay_at_07: "IZIPAY es la empresa con atención más rápida y efectiva a mis consultas/solicitudes/problemas"
	label define izipay_at_07 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values izipay_at_07 izipay_at_07

	label variable izipay_at_08 "IZIPAY tiene los mejores equipos y soluciones digitales"
	note izipay_at_08: "IZIPAY tiene los mejores equipos y soluciones digitales"
	label define izipay_at_08 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values izipay_at_08 izipay_at_08

	label variable izipay_at_09 "IZIPAY deposita lo más rápido luego de realizadas las ventas"
	note izipay_at_09: "IZIPAY deposita lo más rápido luego de realizadas las ventas"
	label define izipay_at_09 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values izipay_at_09 izipay_at_09

	label variable izipay_at_10 "IZIPAY es la empresa que más valor me da por el costo que pago"
	note izipay_at_10: "IZIPAY es la empresa que más valor me da por el costo que pago"
	label define izipay_at_10 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values izipay_at_10 izipay_at_10

	label variable izipay_at_11 "IZIPAY ofrece buenas herramientas para gestionar mis ventas (reportes, dashbords"
	note izipay_at_11: "IZIPAY ofrece buenas herramientas para gestionar mis ventas (reportes, dashbords)"
	label define izipay_at_11 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values izipay_at_11 izipay_at_11

	label variable izipay_at_12 "IZIPAY es la empresa más innovadora (constantemente reinventa sus productos y se"
	note izipay_at_12: "IZIPAY es la empresa más innovadora (constantemente reinventa sus productos y servicios pensando en lo que necesitan sus clientes)"
	label define izipay_at_12 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values izipay_at_12 izipay_at_12

	label variable izipay_at_13 "IZIPAY es la empresa más reconocida que lidera el mercado"
	note izipay_at_13: "IZIPAY es la empresa más reconocida que lidera el mercado"
	label define izipay_at_13 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values izipay_at_13 izipay_at_13

	label variable izipay_at_14 "IZIPAY es un socio estratégico para mi negocio, ya que se interesa por que mi ne"
	note izipay_at_14: "IZIPAY es un socio estratégico para mi negocio, ya que se interesa por que mi negocio crezca"
	label define izipay_at_14 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values izipay_at_14 izipay_at_14

	label variable izipay_at_15 "IZIPAY es la empresa que más cuida su impacto en la sociedad y el medio ambiente"
	note izipay_at_15: "IZIPAY es la empresa que más cuida su impacto en la sociedad y el medio ambiente."
	label define izipay_at_15 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values izipay_at_15 izipay_at_15

	label variable izipay_at_16 "IZIPAY es la empresa que hace sentir a todos sus clientes valorados"
	note izipay_at_16: "IZIPAY es la empresa que hace sentir a todos sus clientes valorados"
	label define izipay_at_16 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values izipay_at_16 izipay_at_16

	label variable izipay_at_17 "IZIPAY es la empresa que me da la libertad de elegir el banco donde me abonan mi"
	note izipay_at_17: "IZIPAY es la empresa que me da la libertad de elegir el banco donde me abonan mis ventas, sin que eso afecte mis condiciones comerciales: tiempo de abono, tasas, etc."
	label define izipay_at_17 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values izipay_at_17 izipay_at_17

	label variable culqi_at_01 "CULQI tiene el portafolio de productos y servicios que más se adecúa a mis neces"
	note culqi_at_01: "CULQI tiene el portafolio de productos y servicios que más se adecúa a mis necesidades y acepta todos los medios de pago"
	label define culqi_at_01 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values culqi_at_01 culqi_at_01

	label variable culqi_at_02 "CULQI tiene ofrecimiento de préstamos para capital de trabajo"
	note culqi_at_02: "CULQI tiene ofrecimiento de préstamos para capital de trabajo"
	label define culqi_at_02 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values culqi_at_02 culqi_at_02

	label variable culqi_at_03 "CULQI brinda buenas tasas / comisiones por transacción"
	note culqi_at_03: "CULQI brinda buenas tasas / comisiones por transacción"
	label define culqi_at_03 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values culqi_at_03 culqi_at_03

	label variable culqi_at_04 "CULQI brinda los productos y servicios más fáciles de usar y entender en el merc"
	note culqi_at_04: "CULQI brinda los productos y servicios más fáciles de usar y entender en el mercado"
	label define culqi_at_04 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values culqi_at_04 culqi_at_04

	label variable culqi_at_05 "CULQI es la empresa con mayor seguridad en los pagos (pagos seguros, sin fraudes"
	note culqi_at_05: "CULQI es la empresa con mayor seguridad en los pagos (pagos seguros, sin fraudes)"
	label define culqi_at_05 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values culqi_at_05 culqi_at_05

	label variable culqi_at_06 "CULQI brinda un buen servicio para el cobro de mis ventas, sin incidencias y sin"
	note culqi_at_06: "CULQI brinda un buen servicio para el cobro de mis ventas, sin incidencias y sin demoras en el cobro."
	label define culqi_at_06 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values culqi_at_06 culqi_at_06

	label variable culqi_at_07 "CULQI es la empresa con atención más rápida y efectiva a mis consultas/solicitud"
	note culqi_at_07: "CULQI es la empresa con atención más rápida y efectiva a mis consultas/solicitudes/problemas"
	label define culqi_at_07 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values culqi_at_07 culqi_at_07

	label variable culqi_at_08 "CULQI tiene los mejores equipos y soluciones digitales"
	note culqi_at_08: "CULQI tiene los mejores equipos y soluciones digitales"
	label define culqi_at_08 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values culqi_at_08 culqi_at_08

	label variable culqi_at_09 "CULQI deposita lo más rápido luego de realizadas las ventas"
	note culqi_at_09: "CULQI deposita lo más rápido luego de realizadas las ventas"
	label define culqi_at_09 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values culqi_at_09 culqi_at_09

	label variable culqi_at_10 "CULQI es la empresa que más valor me da por el costo que pago"
	note culqi_at_10: "CULQI es la empresa que más valor me da por el costo que pago"
	label define culqi_at_10 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values culqi_at_10 culqi_at_10

	label variable culqi_at_11 "CULQI ofrece buenas herramientas para gestionar mis ventas (reportes, dashbords)"
	note culqi_at_11: "CULQI ofrece buenas herramientas para gestionar mis ventas (reportes, dashbords)"
	label define culqi_at_11 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values culqi_at_11 culqi_at_11

	label variable culqi_at_12 "CULQI es la empresa más innovadora (constantemente reinventa sus productos y ser"
	note culqi_at_12: "CULQI es la empresa más innovadora (constantemente reinventa sus productos y servicios pensando en lo que necesitan sus clientes)"
	label define culqi_at_12 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values culqi_at_12 culqi_at_12

	label variable culqi_at_13 "CULQI es la empresa más reconocida que lidera el mercado"
	note culqi_at_13: "CULQI es la empresa más reconocida que lidera el mercado"
	label define culqi_at_13 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values culqi_at_13 culqi_at_13

	label variable culqi_at_14 "CULQI es un socio estratégico para mi negocio, ya que se interesa por que mi neg"
	note culqi_at_14: "CULQI es un socio estratégico para mi negocio, ya que se interesa por que mi negocio crezca"
	label define culqi_at_14 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values culqi_at_14 culqi_at_14

	label variable culqi_at_15 "CULQI es la empresa que más cuida su impacto en la sociedad y el medio ambiente."
	note culqi_at_15: "CULQI es la empresa que más cuida su impacto en la sociedad y el medio ambiente."
	label define culqi_at_15 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values culqi_at_15 culqi_at_15

	label variable culqi_at_16 "CULQI es la empresa que hace sentir a todos sus clientes valorados"
	note culqi_at_16: "CULQI es la empresa que hace sentir a todos sus clientes valorados"
	label define culqi_at_16 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values culqi_at_16 culqi_at_16

	label variable culqi_at_17 "CULQI es la empresa que me da la libertad de elegir el banco donde me abonan mis"
	note culqi_at_17: "CULQI es la empresa que me da la libertad de elegir el banco donde me abonan mis ventas, sin que eso afecte mis condiciones comerciales: tiempo de abono, tasas, etc."
	label define culqi_at_17 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values culqi_at_17 culqi_at_17

	label variable openpay_at_01 "OPENPAY tiene el portafolio de productos y servicios que más se adecúa a mis nec"
	note openpay_at_01: "OPENPAY tiene el portafolio de productos y servicios que más se adecúa a mis necesidades y acepta todos los medios de pago"
	label define openpay_at_01 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values openpay_at_01 openpay_at_01

	label variable openpay_at_02 "OPENPAY tiene ofrecimiento de préstamos para capital de trabajo"
	note openpay_at_02: "OPENPAY tiene ofrecimiento de préstamos para capital de trabajo"
	label define openpay_at_02 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values openpay_at_02 openpay_at_02

	label variable openpay_at_03 "OPENPAY brinda buenas tasas / comisiones por transacción"
	note openpay_at_03: "OPENPAY brinda buenas tasas / comisiones por transacción"
	label define openpay_at_03 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values openpay_at_03 openpay_at_03

	label variable openpay_at_04 "OPENPAY brinda los productos y servicios más fáciles de usar y entender en el me"
	note openpay_at_04: "OPENPAY brinda los productos y servicios más fáciles de usar y entender en el mercado"
	label define openpay_at_04 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values openpay_at_04 openpay_at_04

	label variable openpay_at_05 "OPENPAY es la empresa con mayor seguridad en los pagos (pagos seguros, sin fraud"
	note openpay_at_05: "OPENPAY es la empresa con mayor seguridad en los pagos (pagos seguros, sin fraudes)"
	label define openpay_at_05 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values openpay_at_05 openpay_at_05

	label variable openpay_at_06 "OPENPAY brinda un buen servicio para el cobro de mis ventas, sin incidencias y s"
	note openpay_at_06: "OPENPAY brinda un buen servicio para el cobro de mis ventas, sin incidencias y sin demoras en el cobro."
	label define openpay_at_06 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values openpay_at_06 openpay_at_06

	label variable openpay_at_07 "OPENPAY es la empresa con atención más rápida y efectiva a mis consultas/solicit"
	note openpay_at_07: "OPENPAY es la empresa con atención más rápida y efectiva a mis consultas/solicitudes/problemas"
	label define openpay_at_07 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values openpay_at_07 openpay_at_07

	label variable openpay_at_08 "OPENPAY tiene los mejores equipos y soluciones digitales"
	note openpay_at_08: "OPENPAY tiene los mejores equipos y soluciones digitales"
	label define openpay_at_08 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values openpay_at_08 openpay_at_08

	label variable openpay_at_09 "OPENPAY deposita lo más rápido luego de realizadas las ventas"
	note openpay_at_09: "OPENPAY deposita lo más rápido luego de realizadas las ventas"
	label define openpay_at_09 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values openpay_at_09 openpay_at_09

	label variable openpay_at_10 "OPENPAY es la empresa que más valor me da por el costo que pago"
	note openpay_at_10: "OPENPAY es la empresa que más valor me da por el costo que pago"
	label define openpay_at_10 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values openpay_at_10 openpay_at_10

	label variable openpay_at_11 "OPENPAY ofrece buenas herramientas para gestionar mis ventas (reportes, dashbord"
	note openpay_at_11: "OPENPAY ofrece buenas herramientas para gestionar mis ventas (reportes, dashbords)"
	label define openpay_at_11 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values openpay_at_11 openpay_at_11

	label variable openpay_at_12 "OPENPAY es la empresa más innovadora (constantemente reinventa sus productos y s"
	note openpay_at_12: "OPENPAY es la empresa más innovadora (constantemente reinventa sus productos y servicios pensando en lo que necesitan sus clientes)"
	label define openpay_at_12 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values openpay_at_12 openpay_at_12

	label variable openpay_at_13 "OPENPAY es la empresa más reconocida que lidera el mercado"
	note openpay_at_13: "OPENPAY es la empresa más reconocida que lidera el mercado"
	label define openpay_at_13 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values openpay_at_13 openpay_at_13

	label variable openpay_at_14 "OPENPAY es un socio estratégico para mi negocio, ya que se interesa por que mi n"
	note openpay_at_14: "OPENPAY es un socio estratégico para mi negocio, ya que se interesa por que mi negocio crezca"
	label define openpay_at_14 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values openpay_at_14 openpay_at_14

	label variable openpay_at_15 "OPENPAY es la empresa que más cuida su impacto en la sociedad y el medio ambient"
	note openpay_at_15: "OPENPAY es la empresa que más cuida su impacto en la sociedad y el medio ambiente."
	label define openpay_at_15 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values openpay_at_15 openpay_at_15

	label variable openpay_at_16 "OPENPAY es la empresa que hace sentir a todos sus clientes valorados"
	note openpay_at_16: "OPENPAY es la empresa que hace sentir a todos sus clientes valorados"
	label define openpay_at_16 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values openpay_at_16 openpay_at_16

	label variable openpay_at_17 "OPENPAY es la empresa que me da la libertad de elegir el banco donde me abonan m"
	note openpay_at_17: "OPENPAY es la empresa que me da la libertad de elegir el banco donde me abonan mis ventas sin que eso afecte mis condiciones comerciales: tiempo de abono, tasas, etc."
	label define openpay_at_17 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values openpay_at_17 openpay_at_17

	label variable versus_producto_niubiz "Niubiz"
	note versus_producto_niubiz: "Niubiz"
	label define versus_producto_niubiz 1 "Peor que otras marcas" 2 "En línea con otras marcas" 3 "Mejor que otras marcas" 4 "No sabe/ no aplica (solo si no ha usado la marca)"
	label values versus_producto_niubiz versus_producto_niubiz

	label variable versus_producto_izipay "Izipay"
	note versus_producto_izipay: "Izipay"
	label define versus_producto_izipay 1 "Peor que otras marcas" 2 "En línea con otras marcas" 3 "Mejor que otras marcas" 4 "No sabe/ no aplica (solo si no ha usado la marca)"
	label values versus_producto_izipay versus_producto_izipay

	label variable versus_producto_culqi "Culqi"
	note versus_producto_culqi: "Culqi"
	label define versus_producto_culqi 1 "Peor que otras marcas" 2 "En línea con otras marcas" 3 "Mejor que otras marcas" 4 "No sabe/ no aplica (solo si no ha usado la marca)"
	label values versus_producto_culqi versus_producto_culqi

	label variable versus_producto_openpay "OpenPay"
	note versus_producto_openpay: "OpenPay"
	label define versus_producto_openpay 1 "Peor que otras marcas" 2 "En línea con otras marcas" 3 "Mejor que otras marcas" 4 "No sabe/ no aplica (solo si no ha usado la marca)"
	label values versus_producto_openpay versus_producto_openpay

	label variable versus_precio_niubiz "Niubiz"
	note versus_precio_niubiz: "Niubiz"
	label define versus_precio_niubiz 1 "Peor que otras marcas" 2 "En línea con otras marcas" 3 "Mejor que otras marcas" 4 "No sabe/ no aplica (solo si no ha usado la marca)"
	label values versus_precio_niubiz versus_precio_niubiz

	label variable versus_precio_izipay "Izipay"
	note versus_precio_izipay: "Izipay"
	label define versus_precio_izipay 1 "Peor que otras marcas" 2 "En línea con otras marcas" 3 "Mejor que otras marcas" 4 "No sabe/ no aplica (solo si no ha usado la marca)"
	label values versus_precio_izipay versus_precio_izipay

	label variable versus_precio_culqi "Culqi"
	note versus_precio_culqi: "Culqi"
	label define versus_precio_culqi 1 "Peor que otras marcas" 2 "En línea con otras marcas" 3 "Mejor que otras marcas" 4 "No sabe/ no aplica (solo si no ha usado la marca)"
	label values versus_precio_culqi versus_precio_culqi

	label variable versus_precio_openpay "OpenPay"
	note versus_precio_openpay: "OpenPay"
	label define versus_precio_openpay 1 "Peor que otras marcas" 2 "En línea con otras marcas" 3 "Mejor que otras marcas" 4 "No sabe/ no aplica (solo si no ha usado la marca)"
	label values versus_precio_openpay versus_precio_openpay

	label variable niubiz_prob_exp "¿Tuvo problemas con el uso de sus soluciones Niubiz?"
	note niubiz_prob_exp: "¿Tuvo problemas con el uso de sus soluciones Niubiz?"
	label define niubiz_prob_exp 1 "Sí" 2 "No" 99 "No sabe / No aplica"
	label values niubiz_prob_exp niubiz_prob_exp

	label variable niubiz_frec_prob_uso "¿Con qué frecuencia presentas problemas con tu solución Niubiz?"
	note niubiz_frec_prob_uso: "¿Con qué frecuencia presentas problemas con tu solución Niubiz?"
	label define niubiz_frec_prob_uso 1 "Diario (1 vez al día)" 2 "Interdiario (3 veces a la semana)" 3 "Semanal (1 vez a la semana)" 4 "Mensual (1 vez al mes)" 66 "Otros (especifique)"
	label values niubiz_frec_prob_uso niubiz_frec_prob_uso

	label variable niubiz_frec_prob_uso_o "Especifique la frecuencia:"
	note niubiz_frec_prob_uso_o: "Especifique la frecuencia:"

	label variable niubiz_tip_prob_exp "¿Qué inconvenientes experimentaste con tu solución Niubiz? [Selección múltiple]"
	note niubiz_tip_prob_exp: "¿Qué inconvenientes experimentaste con tu solución Niubiz? [Selección múltiple]"

	label variable niubiz_tip_prob_exp_o "Especifique el inconveniente:"
	note niubiz_tip_prob_exp_o: "Especifique el inconveniente:"

	label variable niubiz_prom_uso "¿La(s) solución(es) Niubiz que tienes contratada satisface las expectativas que "
	note niubiz_prom_uso: "¿La(s) solución(es) Niubiz que tienes contratada satisface las expectativas que tenías al momento de la contratación?"
	label define niubiz_prom_uso 1 "Sí" 2 "No"
	label values niubiz_prom_uso niubiz_prom_uso

	label variable izipay_prob_exp "¿Tuvo problemas con el uso de sus soluciones Izipay?"
	note izipay_prob_exp: "¿Tuvo problemas con el uso de sus soluciones Izipay?"
	label define izipay_prob_exp 1 "Sí" 2 "No" 99 "No sabe / No aplica"
	label values izipay_prob_exp izipay_prob_exp

	label variable izipay_frec_prob_uso "¿Con qué frecuencia presentas problemas con tu solución Izipay?"
	note izipay_frec_prob_uso: "¿Con qué frecuencia presentas problemas con tu solución Izipay?"
	label define izipay_frec_prob_uso 1 "Diario (1 vez al día)" 2 "Interdiario (3 veces a la semana)" 3 "Semanal (1 vez a la semana)" 4 "Mensual (1 vez al mes)" 66 "Otros (especifique)"
	label values izipay_frec_prob_uso izipay_frec_prob_uso

	label variable izipay_frec_prob_uso_o "Especifique la frecuencia:"
	note izipay_frec_prob_uso_o: "Especifique la frecuencia:"

	label variable izipay_tip_prob_exp "¿Qué inconvenientes experimentaste con tu solución Izipay?"
	note izipay_tip_prob_exp: "¿Qué inconvenientes experimentaste con tu solución Izipay?"

	label variable izipay_tip_prob_exp_o "Especifique el inconveniente:"
	note izipay_tip_prob_exp_o: "Especifique el inconveniente:"

	label variable izipay_prom_uso "¿La(s) solución (es) Izipay que tienes contratada satisface las expectativas que"
	note izipay_prom_uso: "¿La(s) solución (es) Izipay que tienes contratada satisface las expectativas que tenías al momento de la contratación?"
	label define izipay_prom_uso 1 "Sí" 2 "No"
	label values izipay_prom_uso izipay_prom_uso

	label variable culqi_prob_exp "¿Tuvo problemas con el uso de sus soluciones Culqi?"
	note culqi_prob_exp: "¿Tuvo problemas con el uso de sus soluciones Culqi?"
	label define culqi_prob_exp 1 "Sí" 2 "No" 99 "No sabe / No aplica"
	label values culqi_prob_exp culqi_prob_exp

	label variable culqi_frec_prob_uso "¿Con qué frecuencia presentas problemas con tu solución Culqi?"
	note culqi_frec_prob_uso: "¿Con qué frecuencia presentas problemas con tu solución Culqi?"
	label define culqi_frec_prob_uso 1 "Diario (1 vez al día)" 2 "Interdiario (3 veces a la semana)" 3 "Semanal (1 vez a la semana)" 4 "Mensual (1 vez al mes)" 66 "Otros (especifique)"
	label values culqi_frec_prob_uso culqi_frec_prob_uso

	label variable culqi_frec_prob_uso_o "Especifique la frecuencia:"
	note culqi_frec_prob_uso_o: "Especifique la frecuencia:"

	label variable culqi_tip_prob_exp "¿Qué inconvenientes experimentaste con la solución Culqi?"
	note culqi_tip_prob_exp: "¿Qué inconvenientes experimentaste con la solución Culqi?"

	label variable culqi_tip_prob_exp_o "Especifique el inconveniente:"
	note culqi_tip_prob_exp_o: "Especifique el inconveniente:"

	label variable culqi_prom_uso "¿La(s) solución (es) Culqi satisface las expectativas que tenías al momento de l"
	note culqi_prom_uso: "¿La(s) solución (es) Culqi satisface las expectativas que tenías al momento de la contratación?"
	label define culqi_prom_uso 1 "Sí" 2 "No"
	label values culqi_prom_uso culqi_prom_uso

	label variable versus_exp_niubiz "Niubiz"
	note versus_exp_niubiz: "Niubiz"
	label define versus_exp_niubiz 1 "Peor que otras marcas" 2 "En línea con otras marcas" 3 "Mejor que otras marcas" 4 "No sabe/ no aplica (solo si no ha usado la marca)"
	label values versus_exp_niubiz versus_exp_niubiz

	label variable versus_exp_izipay "Izipay"
	note versus_exp_izipay: "Izipay"
	label define versus_exp_izipay 1 "Peor que otras marcas" 2 "En línea con otras marcas" 3 "Mejor que otras marcas" 4 "No sabe/ no aplica (solo si no ha usado la marca)"
	label values versus_exp_izipay versus_exp_izipay

	label variable versus_exp_culqi "Culqi"
	note versus_exp_culqi: "Culqi"
	label define versus_exp_culqi 1 "Peor que otras marcas" 2 "En línea con otras marcas" 3 "Mejor que otras marcas" 4 "No sabe/ no aplica (solo si no ha usado la marca)"
	label values versus_exp_culqi versus_exp_culqi

	label variable versus_exp_openpay "OpenPay"
	note versus_exp_openpay: "OpenPay"
	label define versus_exp_openpay 1 "Peor que otras marcas" 2 "En línea con otras marcas" 3 "Mejor que otras marcas" 4 "No sabe/ no aplica (solo si no ha usado la marca)"
	label values versus_exp_openpay versus_exp_openpay

	label variable niubiz_prob_abo "¿Has tenido problemas con el abono de sus ventas de parte de Niubiz?"
	note niubiz_prob_abo: "¿Has tenido problemas con el abono de sus ventas de parte de Niubiz?"
	label define niubiz_prob_abo 1 "Sí" 2 "No" 99 "No sabe / No aplica"
	label values niubiz_prob_abo niubiz_prob_abo

	label variable niubiz_frec_abo "¿Con qué frecuencia presentas problemas con tus abonos de parte de Niubiz?"
	note niubiz_frec_abo: "¿Con qué frecuencia presentas problemas con tus abonos de parte de Niubiz?"
	label define niubiz_frec_abo 1 "Diario (1 vez al día)" 2 "Interdiario (3 veces a la semana)" 3 "Semanal (1 vez a la semana)" 4 "Mensual (1 vez al mes)" 66 "Otros (especifique)"
	label values niubiz_frec_abo niubiz_frec_abo

	label variable niubiz_frec_abo_o "Especifique la frecuencia:"
	note niubiz_frec_abo_o: "Especifique la frecuencia:"

	label variable niubiz_inco_abo "¿Qué problema tuviste con tus abonos de parte de Niubiz? [Selección múltiple]"
	note niubiz_inco_abo: "¿Qué problema tuviste con tus abonos de parte de Niubiz? [Selección múltiple]"

	label variable niubiz_inco_abo_o "Especifique el problema:"
	note niubiz_inco_abo_o: "Especifique el problema:"

	label variable izipay_prob_abo "¿Has tenido problemas con el abono de sus ventas de parte de Izipay?"
	note izipay_prob_abo: "¿Has tenido problemas con el abono de sus ventas de parte de Izipay?"
	label define izipay_prob_abo 1 "Sí" 2 "No" 99 "No sabe / No aplica"
	label values izipay_prob_abo izipay_prob_abo

	label variable izipay_frec_abo "¿Con qué frecuencia presentas problemas con tus abonos de parte de Izipay?"
	note izipay_frec_abo: "¿Con qué frecuencia presentas problemas con tus abonos de parte de Izipay?"
	label define izipay_frec_abo 1 "Diario (1 vez al día)" 2 "Interdiario (3 veces a la semana)" 3 "Semanal (1 vez a la semana)" 4 "Mensual (1 vez al mes)" 66 "Otros (especifique)"
	label values izipay_frec_abo izipay_frec_abo

	label variable izipay_frec_abo_o "Especifique la frecuencia:"
	note izipay_frec_abo_o: "Especifique la frecuencia:"

	label variable izipay_inco_abo "¿Qué problema tuviste con tus abonos de parte de Izipay? [Selección múltiple]"
	note izipay_inco_abo: "¿Qué problema tuviste con tus abonos de parte de Izipay? [Selección múltiple]"

	label variable izipay_inco_abo_o "Especifique el problema:"
	note izipay_inco_abo_o: "Especifique el problema:"

	label variable culqi_prob_abo "¿Has tenido problemas con el abono de sus ventas de parte de Culqi?"
	note culqi_prob_abo: "¿Has tenido problemas con el abono de sus ventas de parte de Culqi?"
	label define culqi_prob_abo 1 "Sí" 2 "No" 99 "No sabe / No aplica"
	label values culqi_prob_abo culqi_prob_abo

	label variable culqi_frec_abo "¿Con qué frecuencia presentas problemas con tus abonos de parte de Culqi?"
	note culqi_frec_abo: "¿Con qué frecuencia presentas problemas con tus abonos de parte de Culqi?"
	label define culqi_frec_abo 1 "Diario (1 vez al día)" 2 "Interdiario (3 veces a la semana)" 3 "Semanal (1 vez a la semana)" 4 "Mensual (1 vez al mes)" 66 "Otros (especifique)"
	label values culqi_frec_abo culqi_frec_abo

	label variable culqi_frec_abo_o "Especifique la frecuencia:"
	note culqi_frec_abo_o: "Especifique la frecuencia:"

	label variable culqi_inco_abo "¿Qué problema tuviste con tus abonos de parte de Culqi?"
	note culqi_inco_abo: "¿Qué problema tuviste con tus abonos de parte de Culqi?"

	label variable culqi_inco_abo_o "Especifique el problema:"
	note culqi_inco_abo_o: "Especifique el problema:"

	label variable temp_abo "En términos de tiempos de abonos:"
	note temp_abo: "En términos de tiempos de abonos:"

	label variable temp_abo_o "Especifique el tiempo de abono:"
	note temp_abo_o: "Especifique el tiempo de abono:"

	label variable rep_abo "¿Qué información te gustaría que se detalle en tus reportes de abonos? [Selecció"
	note rep_abo: "¿Qué información te gustaría que se detalle en tus reportes de abonos? [Selección múltiple]"

	label variable rep_abo_o "Especifique el tipo de información:"
	note rep_abo_o: "Especifique el tipo de información:"

	label variable notf_abo "¿Te gustaría recibir notificaciones cuando se realicen los abonos?"
	note notf_abo: "¿Te gustaría recibir notificaciones cuando se realicen los abonos?"
	label define notf_abo 1 "Sí, a través de correo electrónico" 2 "Sí, a través de WhatsApp" 3 "Sí, a través de SMS" 4 "Sí, a través de otro medio (especificar):" 5 "No"
	label values notf_abo notf_abo

	label variable notf_abo_o "Especifique el medio:"
	note notf_abo_o: "Especifique el medio:"

	label variable versus_abono_niubiz "Niubiz"
	note versus_abono_niubiz: "Niubiz"
	label define versus_abono_niubiz 1 "Peor que otras marcas" 2 "En línea con otras marcas" 3 "Mejor que otras marcas" 4 "No sabe/ no aplica (solo si no ha usado la marca)"
	label values versus_abono_niubiz versus_abono_niubiz

	label variable versus_abono_izipay "Izipay"
	note versus_abono_izipay: "Izipay"
	label define versus_abono_izipay 1 "Peor que otras marcas" 2 "En línea con otras marcas" 3 "Mejor que otras marcas" 4 "No sabe/ no aplica (solo si no ha usado la marca)"
	label values versus_abono_izipay versus_abono_izipay

	label variable versus_abono_culqi "Culqi"
	note versus_abono_culqi: "Culqi"
	label define versus_abono_culqi 1 "Peor que otras marcas" 2 "En línea con otras marcas" 3 "Mejor que otras marcas" 4 "No sabe/ no aplica (solo si no ha usado la marca)"
	label values versus_abono_culqi versus_abono_culqi

	label variable versus_abono_openpay "OpenPay"
	note versus_abono_openpay: "OpenPay"
	label define versus_abono_openpay 1 "Peor que otras marcas" 2 "En línea con otras marcas" 3 "Mejor que otras marcas" 4 "No sabe/ no aplica (solo si no ha usado la marca)"
	label values versus_abono_openpay versus_abono_openpay

	label variable def_segu "¿Puedes contarnos en base a tu experiencia qué es para ti que una empresa de sol"
	note def_segu: "¿Puedes contarnos en base a tu experiencia qué es para ti que una empresa de soluciones de pago te brinde seguridad?"

	label variable versus_seguridad_niubiz "Niubiz"
	note versus_seguridad_niubiz: "Niubiz"
	label define versus_seguridad_niubiz 1 "Peor que otras marcas" 2 "En línea con otras marcas" 3 "Mejor que otras marcas" 4 "No sabe/ no aplica (solo si no ha usado la marca)"
	label values versus_seguridad_niubiz versus_seguridad_niubiz

	label variable versus_seguridad_izipay "Izipay"
	note versus_seguridad_izipay: "Izipay"
	label define versus_seguridad_izipay 1 "Peor que otras marcas" 2 "En línea con otras marcas" 3 "Mejor que otras marcas" 4 "No sabe/ no aplica (solo si no ha usado la marca)"
	label values versus_seguridad_izipay versus_seguridad_izipay

	label variable versus_seguridad_culqi "Culqi"
	note versus_seguridad_culqi: "Culqi"
	label define versus_seguridad_culqi 1 "Peor que otras marcas" 2 "En línea con otras marcas" 3 "Mejor que otras marcas" 4 "No sabe/ no aplica (solo si no ha usado la marca)"
	label values versus_seguridad_culqi versus_seguridad_culqi

	label variable versus_seguridad_openpay "OpenPay"
	note versus_seguridad_openpay: "OpenPay"
	label define versus_seguridad_openpay 1 "Peor que otras marcas" 2 "En línea con otras marcas" 3 "Mejor que otras marcas" 4 "No sabe/ no aplica (solo si no ha usado la marca)"
	label values versus_seguridad_openpay versus_seguridad_openpay

	label variable prob_cambio_mp "¿Cuál es la probabilidad de que usted utilice una nueva marca de medios de pago "
	note prob_cambio_mp: "¿Cuál es la probabilidad de que usted utilice una nueva marca de medios de pago (diferente a la que tiene actualmente) como su principal marca en los próximos 12 meses? Del 0 al 10, donde 0 es nada probable y 10 es muy probable."
	label define prob_cambio_mp 0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
	label values prob_cambio_mp prob_cambio_mp

	label variable atrib_cambio_mp "¿Qué mejora de atributos te harían decidir un cambio de proveedor de medios de p"
	note atrib_cambio_mp: "¿Qué mejora de atributos te harían decidir un cambio de proveedor de medios de pago? Elige máximo 3"

	label variable tp_oper "¿Hace cuánto tiempo existe su negocio?"
	note tp_oper: "¿Hace cuánto tiempo existe su negocio?"
	label define tp_oper 1 "Menos de 6 meses" 2 "Entre 6 meses y 1 año" 3 "Entre 1 y 2 años" 4 "Entre 2 y 3 años" 5 "Entre 3 y 5 años" 6 "Entre 5 y 10 años" 7 "Mas de 10 años"
	label values tp_oper tp_oper

	label variable nro_emp "¿Cuántas personas trabajan en el negocio?"
	note nro_emp: "¿Cuántas personas trabajan en el negocio?"
	label define nro_emp 1 "1" 2 "2 - 4" 3 "5 - 9" 4 "10 - 19" 5 "20 - 49" 6 "50 - 99" 7 "100 - 299" 8 "300 - 499" 9 "500 - 999" 10 "Más de 1000"
	label values nro_emp nro_emp

	label variable pres_banca "¿Usted usa alguna cuenta bancaria para las operaciones de su negocio?"
	note pres_banca: "¿Usted usa alguna cuenta bancaria para las operaciones de su negocio?"
	label define pres_banca 1 "Sí" 2 "No"
	label values pres_banca pres_banca

	label variable uso_bancos "¿En cuáles de los siguientes bancos tiene una cuenta bancaria para su negocio? ["
	note uso_bancos: "¿En cuáles de los siguientes bancos tiene una cuenta bancaria para su negocio? [Selección múltiple]"

	label variable uso_bancos_o "Especifique el banco:"
	note uso_bancos_o: "Especifique el banco:"

	label variable princ_banco "¿Cuál es el principal banco actualmente para su negocio?"
	note princ_banco: "¿Cuál es el principal banco actualmente para su negocio?"
	label define princ_banco 1 "Banco de Credito - BCP" 2 "Scotiabank" 3 "Interbank" 4 "BBVA" 5 "Banbif" 6 "Banco Pichincha" 7 "Citibank" 8 "Banco GNB" 9 "Banco Falabella" 10 "Banco Ripley" 11 "Banco de Comercio" 12 "MiBanco" 66 "Otro banco (especifique)"
	label values princ_banco princ_banco

	label variable princ_banco_o "Especifique el banco:"
	note princ_banco_o: "Especifique el banco:"

	label variable uso_pos "¿Usted utiliza POS (máquina para recibir pagos con tarjeta de sus clientes) en s"
	note uso_pos: "¿Usted utiliza POS (máquina para recibir pagos con tarjeta de sus clientes) en su negocio / empresa?"
	label define uso_pos 1 "Sí" 2 "No"
	label values uso_pos uso_pos

	label variable mot_nouso "¿Por qué no lo utiliza? [Selección múltiple]"
	note mot_nouso: "¿Por qué no lo utiliza? [Selección múltiple]"

	label variable mot_nouso_o "Especifique el motivo:"
	note mot_nouso_o: "Especifique el motivo:"

	label variable tipo_mp "¿Cuáles de las siguientes modalidades de ventas ejerce en su negocio / empresa? "
	note tipo_mp: "¿Cuáles de las siguientes modalidades de ventas ejerce en su negocio / empresa? [Selección múltiple]"

	label variable tipo_ms "¿Qué porcentaje de sus ventas corresponden a los medios digitales?"
	note tipo_ms: "¿Qué porcentaje de sus ventas corresponden a los medios digitales?"

	label variable tipo_agente "¿Su negocio ofrece el servicio de agente?"
	note tipo_agente: "¿Su negocio ofrece el servicio de agente?"
	label define tipo_agente 1 "Sí" 2 "No"
	label values tipo_agente tipo_agente

	label variable med_com "¿A través de qué canal preferiría recibir campañas, promociones o comunicados so"
	note med_com: "¿A través de qué canal preferiría recibir campañas, promociones o comunicados sobre la solución contratada?"

	label variable med_com_o "Especifique el canal:"
	note med_com_o: "Especifique el canal:"

	label variable coords_finallatitude "Encuestador: Por favor registre su ubicación (latitude)"
	note coords_finallatitude: "Encuestador: Por favor registre su ubicación (latitude)"

	label variable coords_finallongitude "Encuestador: Por favor registre su ubicación (longitude)"
	note coords_finallongitude: "Encuestador: Por favor registre su ubicación (longitude)"

	label variable coords_finalaltitude "Encuestador: Por favor registre su ubicación (altitude)"
	note coords_finalaltitude: "Encuestador: Por favor registre su ubicación (altitude)"

	label variable coords_finalaccuracy "Encuestador: Por favor registre su ubicación (accuracy)"
	note coords_finalaccuracy: "Encuestador: Por favor registre su ubicación (accuracy)"

	label variable obs "Observaciones finales:"
	note obs: "Observaciones finales:"

	label variable veri_1 "Escriba la dirección del negocio"
	note veri_1: "Escriba la dirección del negocio"

	label variable veri_2 "Escriba el nombre completo de la persona encuestada"
	note veri_2: "Escriba el nombre completo de la persona encuestada"






	* append old, previously-imported data (if any)
	cap confirm file "`dtafile'"
	if _rc == 0 {
		* mark all new data before merging with old data
		gen new_data_row=1
		
		* pull in old data
		append using "`dtafile'"
		
		* drop duplicates in favor of old, previously-imported data if overwrite_old_data is 0
		* (alternatively drop in favor of new data if overwrite_old_data is 1)
		sort key
		by key: gen num_for_key = _N
		drop if num_for_key > 1 & ((`overwrite_old_data' == 0 & new_data_row == 1) | (`overwrite_old_data' == 1 & new_data_row ~= 1))
		drop num_for_key

		* drop new-data flag
		drop new_data_row
	}
	
	* save data to Stata format
	save "`dtafile'", replace

	* show codebook and notes
	codebook
	notes list
}

disp
disp "Finished import of: `csvfile'"
disp

* OPTIONAL: LOCALLY-APPLIED STATA CORRECTIONS
*
* Rather than using SurveyCTO's review and correction workflow, the code below can apply a list of corrections
* listed in a local .csv file. Feel free to use, ignore, or delete this code.
*
*   Corrections file path and filename:  Estudio de uso y experiencia de soluciones de pago 2025_corrections.csv
*
*   Corrections file columns (in order): key, fieldname, value, notes

capture confirm file "`corrfile'"
if _rc==0 {
	disp
	disp "Starting application of corrections in: `corrfile'"
	disp

	* save primary data in memory
	preserve

	* load corrections
	insheet using "`corrfile'", names clear
	
	if _N>0 {
		* number all rows (with +1 offset so that it matches row numbers in Excel)
		gen rownum=_n+1
		
		* drop notes field (for information only)
		drop notes
		
		* make sure that all values are in string format to start
		gen origvalue=value
		tostring value, format(%100.0g) replace
		cap replace value="" if origvalue==.
		drop origvalue
		replace value=trim(value)
		
		* correct field names to match Stata field names (lowercase, drop -'s and .'s)
		replace fieldname=lower(subinstr(subinstr(fieldname,"-","",.),".","",.))
		
		* format date and date/time fields (taking account of possible wildcards for repeat groups)
		forvalues i = 1/100 {
			if "`datetime_fields`i''" ~= "" {
				foreach dtvar in `datetime_fields`i'' {
					* skip fields that aren't yet in the data
					cap unab dtvarignore : `dtvar'
					if _rc==0 {
						gen origvalue=value
						replace value=string(clock(value,"DMYhms",2025),"%25.0g") if strmatch(fieldname,"`dtvar'")
						* allow for cases where seconds haven't been specified
						replace value=string(clock(origvalue,"DMYhm",2025),"%25.0g") if strmatch(fieldname,"`dtvar'") & value=="." & origvalue~="."
						drop origvalue
					}
				}
			}
			if "`date_fields`i''" ~= "" {
				foreach dtvar in `date_fields`i'' {
					* skip fields that aren't yet in the data
					cap unab dtvarignore : `dtvar'
					if _rc==0 {
						replace value=string(clock(value,"DMY",2025),"%25.0g") if strmatch(fieldname,"`dtvar'")
					}
				}
			}
		}

		* write out a temp file with the commands necessary to apply each correction
		tempfile tempdo
		file open dofile using "`tempdo'", write replace
		local N = _N
		forvalues i = 1/`N' {
			local fieldnameval=fieldname[`i']
			local valueval=value[`i']
			local keyval=key[`i']
			local rownumval=rownum[`i']
			file write dofile `"cap replace `fieldnameval'="`valueval'" if key=="`keyval'""' _n
			file write dofile `"if _rc ~= 0 {"' _n
			if "`valueval'" == "" {
				file write dofile _tab `"cap replace `fieldnameval'=. if key=="`keyval'""' _n
			}
			else {
				file write dofile _tab `"cap replace `fieldnameval'=`valueval' if key=="`keyval'""' _n
			}
			file write dofile _tab `"if _rc ~= 0 {"' _n
			file write dofile _tab _tab `"disp"' _n
			file write dofile _tab _tab `"disp "CAN'T APPLY CORRECTION IN ROW #`rownumval'""' _n
			file write dofile _tab _tab `"disp"' _n
			file write dofile _tab `"}"' _n
			file write dofile `"}"' _n
		}
		file close dofile
	
		* restore primary data
		restore
		
		* execute the .do file to actually apply all corrections
		do "`tempdo'"

		* re-save data
		save "`dtafile'", replace
	}
	else {
		* restore primary data		
		restore
	}

	disp
	disp "Finished applying corrections in: `corrfile'"
	disp
}
