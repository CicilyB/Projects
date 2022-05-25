* awareness-knowledge (1);
data ak1;
input aware1 know count;
datalines;
1 1 2
2 1 4
3 1 145
4 1 50
1 2 0
2 2 2
3 2 107
4 2 43
1 3 0
2 3 0
3 3 0
4 3 148
;
Run;
PROC FORMAT;
VALUE aware1fm 1='Not worried at all'
2='A little worried'
3='Somewhat worried'
4='Very worried';
VALUE knowfm 1='No knowledge'
2='Some knowledge'
3='A lot of knowledge';
proc corr data=ak1 pearson spearman; 
var aware1 know; 
freq count;
run; 

*awareness-knowledge (2);
data ak2;
input aware2 know count;
datalines;
1 1 2
2 1 3
3 1 146
4 1 50
1 2 3
2 2 4
3 2 145
4 2 0
1 3 0
2 3 2
3 3 41
4 3 105
;
run;

PROC FORMAT;
VALUE aware2fm 1='Not at all'
2='its possible'
3='I probably will'
4='I definitely will';
VALUE knowfm 1='No knowledge'
2='Some knowledge'
3='A lot of knowledge';
proc corr data=ak2 pearson spearman; 
var aware2 know; 
freq count;
run; 
* awareness-preparedness (1);
data ap1;
input aware1 prep count;
datalines;
1 1 1
2 1 2
3 1 91
4 1 93
1 2 1
2 2 3
3 2 98
4 2 61
1 3 0
2 3 1
3 3 63
4 3 87
1 4 0
2 4 0
3 4 0
4 4 0
;
run;
PROC FORMAT;
VALUE aware1fm 1='Not worried at all'
2='A little worried'
3='Somewhat worried'
4='Very worried';
VALUE prepfm 1='Not prepared'
2='A little prepared'
3='Somewhat prepared'
4='Very prepared';
proc corr data=ap1 pearson spearman; 
var aware1 prep; 
freq count;
run; 
* awareness-preparedness (2);
data ap2;
input aware2 prep count;
datalines;
1 1 5
2 1 5
3 1 127
4 1 50
1 2 0
2 2 1
3 2 101
4 2 61
1 3 0
2 3 3
3 3 104
4 3 44
1 4 0
2 4 0
3 4 0
4 4 0
;
run;
PROC FORMAT;
VALUE aware2fm 1='Not at all'
2='its possible'
3='I probably will'
4='I definitely will';
VALUE prepfm 1='Not prepared'
2='A little prepared'
3='Somewhat prepared'
4='Very prepared';
proc corr data=ap2 pearson spearman; 
var aware2 prep; 
freq count;
run; 
* awareness-related behaviors (1);
data ar1;
input aware1 behav count;
datalines;
1 1 0
2 1 0
3 1 0
4 1 0
1 2 0
2 2 0
3 2 0
4 2 43
1 3 2
2 3 6
3 3 252
4 3 50
1 4 0
2 4 0
3 4 0
4 4 148
;
run;
PROC FORMAT;
VALUE aware1fm 1='Not worried at all'
2='A little worried'
3='Somewhat worried'
4='Very worried';
VALUE behavfm 1='Not at all'
2='A little'
3='Some'
4='A lot';
proc corr data=ar1 pearson spearman; 
var aware1 behav; 
freq count;
run; 
* awareness-related behaviors (2);
data ar2;
input aware2 behav count;
datalines;
1 1 0
2 1 0
3 1 0
4 1 0
1 2 2
2 2 1
3 2 40
4 2 0
1 3 3
2 3 6
3 3 251
4 3 50
1 4 0
2 4 2
3 4 41
4 4 105
;
run;
PROC FORMAT;
VALUE aware2fm 1='Not at all'
2='its possible'
3='I probably will'
4='I definitely will';
VALUE behavfm 1='Not at all'
2='A little'
3='Some'
4='A lot';
proc corr data=ar2 pearson spearman; 
var aware2 behav; 
freq count;
run; 
* knowledge-preparedness;
data kp;
input know prep count;
datalines;
1 1 99
2 1 88
3 1 0
1 2 103
2 2 0
3 2 61
1 3 0
2 3 64
3 3 87
1 4 0
2 4 0
3 4 0
;
run;
PROC FORMAT;
VALUE knowfm 1='No knowledge'
2='Some knowledge'
3='A lot of knowledge';
VALUE prepfm 1='Not prepared'
2='A little prepared'
3='Somewhat prepared'
4='Very prepared';
proc corr data=kp pearson spearman; 
var know prep; 
freq count;
run; 
* knowledge-related behaviors;
data kr;
input know behav count;
datalines;
1 1 0
2 1 0
3 1 0
1 2 0
2 2 43
3 2 0
1 3 201
2 3 109
3 3 0
1 4 0
2 4 0
3 4 148
;
run;
PROC FORMAT;
VALUE knowfm 1='No knowledge'
2='Some knowledge'
3='A lot of knowledge';
VALUE behavfm 1='Not at all'
2='A little'
3='Some'
4='A lot';
proc corr data=kr pearson spearman; 
var know behav; 
freq count;
run; 
* preparedness-related behaviors;
data pr;
input prep behav count;
datalines;
1 1 0
2 1 0
3 1 0
4 1 0
1 2 43
2 2 0
3 2 0
4 2 0
1 3 144
2 3 102
3 3 64
4 3 0
1 4 0
2 4 61
3 4 87
4 4 0
;
run;
PROC FORMAT;
VALUE prepfm 1='Not prepared'
2='A little prepared'
3='Somewhat prepared'
4='Very prepared';
VALUE behavfm 1='Not at all'
2='A little'
3='Some'
4='A lot';
proc corr data=pr pearson spearman; 
var prep behav; 
freq count;
run; 
