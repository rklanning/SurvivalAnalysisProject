proc import datafile='/home/u63575405/BS852/FINAL/framdat4(3) (1).csv'
out=fram dbms=csv replace; getnames=yes; datarow=2;
run;
proc print data=fram (obs=20);run;

* part 1, assembling demographics table ;

			* categorical / binary data ;
proc freq data=fram;
	tables dth*(sex smoke htn4 chd t2d);
run;

			* continuous data ;
proc means data = fram mean clm;
	var chol4 bmi4 wgt4 fvc4 spf4 dpf4 cigs4;
run;

proc means data = fram mean clm;
	class dth;
	var chol4 bmi4 wgt4 fvc4 spf4 dpf4 cigs4;
run;

proc ttest data=fram;
	class dth;
	var chol4 bmi4 wgt4 fvc4 spf4 dpf4 cigs4;
run;

proc logistic data=fram descending;
	class dth(ref='0') sex(ref='1') smoke(ref='0') htn4(ref='0')/param=ref;
	model dth=sex htn4 smoke chd t2d;
run;


* univariate analysis ;

*Run Univariate, check time-varying coefficient to see if PH assumption met;

proc phreg data=fram zph;
	model SURV*DTH(0) = spf4 / rl ties = efron;
run;

* passes assumption ;
* investigating multivariate model ;
proc phreg data=fram;
   model SURV*DTH(0) = spf4 SEX age4 chol4 cigs4 smoke wgt4 fvc4 bmi4 htn4 t2d /
   selection=stepwise slentry=0.15 slstay=0.15;
run; * age, cigs, sex, spf4 ;

* after variable selection of significant covariates,
  remove missing values from final model ;

data fram2; set fram; 
	keep age4 spf4 cigs4 sex spf4 fvc4 surv dth;
	if cmiss(of age4 spf4 cigs4 sex spf4 fvc4 surv dth) then delete;
run;

* multivariate model with significant covariates ;
proc phreg data=fram2 zph;
	class sex(ref=first) / order=internal;
	model surv*dth(0) = spf4 sex age4 cigs4 fvc4  /rl ties=efron;
run;

* see distribution of age to make appropriate groups ;
proc univariate data=fram; var age4; run;

proc format;
  value agef 1='34-43' 2='44-53' 3='54-63' 4='64-73';
run;

data fram2;
    set fram2;
	agegrp = floor((age4-34)/10 + 1); * create age categories ;
	format agegrp agef.;
run;

*age status violates, test with strata; 
proc phreg data=fram2;
	class sex(ref=first) / order=internal; 
	model SURV*DTH(0) = spf4 sex cigs4 fvc4 / rl ties=efron ;
	strata agegrp; 
run;

* does sex modify effect? Testing interaction ;
proc phreg data=fram2;
	class sex(ref=first) / order=internal; 
	model SURV*DTH(0) = spf4 sex spf4*sex cigs4 fvc4 / rl ties=efron ;
	strata agegrp; 
run;

* stratify data by gender to get gender-specific estimates ;
data female;
	set fram; 
	if SEX = 2 ;
run;

* female model selection ;
proc phreg data=female;
   model SURV*DTH(0) = spf4 SEX age4 chol4 cigs4 meno4 smoke wgt4 fvc4 bmi4 htn4 /
   selection=stepwise slentry=0.15 slstay=0.15;
run;

* testing PH assumption ;
proc phreg data=female zph;
	model surv*dth(0) = age4 spf4 cigs4 fvc4 wgt4 bmi4 meno4 htn4 /rl ties=efron; 
run;

* cig smoking violates, see distribution of cigs/day to build groups ;
proc univariate data=female; var cigs4; run;

proc format;
  value cigsf 1='0-9' 2='10-19' 3='20-29' 4='30-39' 5='40-49';
run;

data female2;
	set female;
	cigsgrp = floor((cigs4)/10 + 1);
	format cigsgrp cigsf.;
run;

* stratify by cigsgrp, check ph and estimates ;
proc phreg data=female2 zph;
	model surv*dth(0) = age4 spf4 cigs4 fvc4 wgt4 bmi4 meno4 htn4 /rl ties=efron; 
	strata cigsgrp;
run;

* data stratification by males ;
data male;
	set fram; 
	if SEX = 1 ;
run;
* automatic variable selection ;
proc phreg data=male;
   model SURV*DTH(0) = spf4 SEX age4 chol4 cigs4 smoke wgt4 fvc4 bmi4 htn4 /
   selection=stepwise slentry=0.15 slstay=0.15;
run;
* model, check PH assumptions ;
proc phreg data=male zph;
	model surv*dth(0) = spf4 age4 cigs4 fvc4 htn4 / rl ties = efron;
run; * all variables pass ;

* is CHD associated with mortality? exploratory ;

data fram_nna; set fram; drop meno4; run;
data fram_nna; set fram_nna; if cmiss(of _all_) then delete; run;

data fram_cr; 
	set fram_nna;
	if (chd=1) then disease=1;
	else if (dth=1 & chd=0) then disease=2;
	else disease=0;
run;
	
proc phreg data=fram_cr zph;
    class CHD(ref=first)/order=internal;
    model SURV*disease(0) = CHD SPF4 spf4 sex age4 cigs4 fvc4 / rl ties=efron;
	strata CHD;
run;

proc phreg data=fram_cr zph plots(overlay=stratum)=cif;
    class chd(order=internal ref=first);
    model SURV*dth(0) = chd/ eventcode =1 rl ties=efron;
run;


