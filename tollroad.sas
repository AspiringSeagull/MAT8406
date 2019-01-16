options pageno=1;


data tollroad;
input toll dist @@;
cards;
1000 93 1500 212 1200 155 900 57 1100 114
900 35 1000 88 900 47 1400 170 1200 114
;
*----------------------------------------------------------*;
*  The regression analysis procedure (PROC REG) is run.    *;
*----------------------------------------------------------*;
proc reg data=tollroad;
  model toll=dist;
run;
quit;


