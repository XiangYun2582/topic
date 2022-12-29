PROC IMPORT OUT= WORK.train80 
            DATAFILE= "C:\Users\Huang\Desktop\資料探勘\報告\train80.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
PROC IMPORT OUT= WORK.test20
            DATAFILE= "C:\Users\Huang\Desktop\資料探勘\報告\test20.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
proc print data=test20;
run;
/* 迴歸分析配適模型 */
proc sort data=train80 out=out;
by materials ;
proc print;
run;
data train;
set out;
by materials ;
if materials=1 then do
ma1=1;ma2=0 ; ma3=0 ;end;
if materials=2  then  do
ma1=0;ma2=1 ; ma3=0 ;end;
if materials=3 then  do
ma1=0;ma2=0 ; ma3=1 ;end;
if materials=4  then  do
ma1=0;ma2=0 ; ma3=0 ;end;
proc print;run;
proc reg data=train;
model total_price_NTD_1=day busfar convifar all_post_office all_police_station_far schoolfar hospital_far guard ma1 ma2 ma3/partial VIF;
plot rstudent. *(predicted. day busfar convifar all_post_office all_police_station_far schoolfar hospital_far guard ma1 ma2 ma3 obs.);
plot NPP. *RSTUDENT.;
output out=set1 residual=residual rstudent=rstudent student=student h=h press=press;
proc print;
var residual student h press rstudent;
proc univariate plot normal;
var residual;
run;
/*Brown-Forsythe test*/
DATA SET2;/*Brown-Forsythe test*/
SET SET1;
IF _N_ /2=int(_N_/2) THEN GROUP=1; 
ELSE GROUP=2;
PROC GLM;
CLASS GROUP;
MODEL RESIDUAL=GROUP;
MEANS GROUP/HOVTEST=BF;
RUN;

*forward;
proc reg data=train;
model total_price_NTD_1=day busfar convifar all_post_office all_police_station_far schoolfar hospital_far guard ma1-ma3 /CLB selection=forward; 
run;
*stepwise;
proc reg data=train;
model total_price_NTD_1=day busfar convifar all_post_office all_police_station_far schoolfar hospital_far  guard ma1-ma3 /CLB selection=stepwise; 
run;
proc transreg data=train test;
   model BoxCox(total_price_NTD_1) = identity(hospital_far day busfar materials all_police_station_far convifar all_post_office);
run;
*backward;
proc reg data=train;
model total_price_NTD_1=day busfar convifar all_post_office all_police_station_far  schoolfar hospital_far guard ma1-ma3/CLB selection=backward; 
run;
proc transreg data=train test SS2;
   model BoxCox(total_price_NTD_1) = identity(day busfar convifar all_post_office all_police_station_far hospital_far ma3);
run;

proc transreg data=train80 OUTTEST=m1 TEST SS2;
model BoxCox(total_price_NTD_1) = identity(hospital_far busfar all_police_station_far convifar all_post_office  schoolfar);
run;
*https://www.youtube.com/watch?v=vzyW-JhfJ4g&ab_channel=PhilChan;
