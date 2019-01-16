options pageno=1;


data mammals;
input gestat longev @@;
cards;
219 18 225 25 122 5 278 15 31 6 284 15
201 8 250 15 151 8 330 20 240 12 15 1
31 5 350 12
;
*----------------------------------------------------------*;
*  The regression analysis procedure (PROC REG) is run.    *;
*----------------------------------------------------------*;
proc reg data=mammals;
  model gestat=longev;
run;
quit;
