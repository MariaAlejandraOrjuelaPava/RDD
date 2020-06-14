*************************CAUSAL INFERENCE AND RESEARCH DESIGN*******************
*****************************ASSIGNMENT 4***************************************

/*First of all, I define directory. */
clear all
cd "C:\Users\MaríaAlejandra\Desktop\RDD\Data"

/*Secondly, I upload the required dataset. */
use "hansen_dwi"

/*After doing so, I perform a preliminary examination of the data. */
br
describe
count
/*The sample consists of 214,558 observations. */
sort year

******POINT 3********
/*In this point, I am required to create a variable which is equivalent to 1 in case bac1>=0.08, and 0 if the contrary situation occurs. 
In this case, I entitled it as 'DUI'. */
gen DUI=1 
replace DUI=0 if bac1<0.08
label var DUI "Indicates whether the unit of observation can be arrested based on its bac1 results"
tab DUI
/*When examining the descriptive statistics of this variable, I encounter that 89.28% of the sample registers a bac1 higher or equal to 0.08. This percertage is equivalent to 191,548 people. On the opposite side, 10.72% exhibits a value of 0 in this variable, implying that its bac1 is inferior to 0.08. */

******POINT 4*********
/*It should be stated that one of the main requirements for implementing the RD method, refers to proving that the running variable does not exhibit any kind of manipulation. In this case, this variable consists on the blood alcohol content (bac1). Therefore, I proceed to evaluate its behavior within the data I count on. */

/*I installed the package of McCrary test. */
net install rddensity, from(https://sites.google.com/site/rdpackages/rddensity/stata) replace
net install lpdensity, from(https://sites.google.com/site/nppackages/lpdensity/stata) replace

/*Significant*/
rddensity bac1, c(0.08) plot 
/*Not significant */
rddensity bac1, c(0.08) plot all

/*Histogram*/
hist bac1, xline(0.08) graphregion(color(white)) bcolor(gray*0.7) title("BAC Histogram") ytitle("Frequency", margin(medium)) xtitle("Blood Alcohol Content", margin (medium)) name(histogram, replace) width(0.001) frequency 
graph export "Histogram.png", as(png) replace

*****POINT 5**********
/*The objective of this point is to verify if there is covariate balance for the 4 controls defined, following equation 1. */
*I first chose to standardize the running variable. 
gen bac1centered=bac1-0.08
*Equation 1 exhibits the interaction between DUI and the running variable. Therefore, I create it. 
gen interac=DUI*bac1centered

reg male DUI bac1centered interac, r
outreg2 using "balancenolimit.doc", replace cti("Male")
reg white DUI bac1centered interac, r
outreg2 using "balancenolimit.doc", cti("White") append
reg aged DUI bac1centered interac, r
outreg2 using "balancenolimit.doc", cti("Age") append
reg acc DUI bac1centered interac, r
outreg2 using "balancenolimit.doc", cti("Accident") append

/*As the bandwidth is 0.05, I limit the sample.*/
reg male DUI bac1centered interac if inrange(bac1centered,-0.05, 0.05) [aweight=10], r
outreg2 using "balance.doc", replace cti("Male")
reg white DUI bac1centered interac if inrange(bac1centered,-0.05, 0.05) [aweight=10], r
outreg2 using "balance.doc", cti("White") append
reg aged DUI bac1centered interac if inrange(bac1centered,-0.05, 0.05) [aweight=10], r
outreg2 using "balance.doc", cti("Age") append
reg acc DUI bac1centered interac if inrange(bac1centered,-0.05, 0.05) [aweight=10], r
outreg2 using "balance.doc", cti("Accident") append


*****POINT 6**********
/*Throughout this point, we must recreate Figure 2 panel A-D; fitting both linear and quadratic. */
*LINEAR
preserve
drop if bac1>0.2

rdplot acc bac1, c(0.08) graph_options(graphregion(color(white)) legend(off) ti("Panel A. Accident at scene") xti("BAC", margin(medium)) name(g1, replace)) p(1)
graph export "1.png", as(png) replace 

rdplot male bac1, c(0.08) graph_options(graphregion(color(white)) legend(off) ti("Panel B. Male") xti("BAC", margin(medium)) name(g2, replace)) p(1) 
graph export "2.png", as(png) replace 
rdplot aged bac1, c(0.08) graph_options(graphregion(color(white)) legend(off) ti("Panel C. Age") xti("BAC", margin(medium)) name(g3, replace)) p(1) 
graph export "3.png", as(png) replace 
rdplot white bac1, c(0.08) graph_options(graphregion(color(white)) legend(off) ti("Panel D. White") xti("BAC", margin(medium))name(g4, replace)) p(1) 
graph export "4.png", as(png) replace 

graph combine g1 g2 g3 g4, col(2) row(2) title("Figure 3-B. BAC and Charcteristics") graphregion(color(white))
graph export "linear.png", as(png) replace

restore

*QUADRATIC
preserve
drop if bac1>0.2

rdplot acc bac1 , c(0.08) graph_options(graphregion(color(white)) legend(off) ti("Panel A. Accident at scene") xti("BAC", margin(medium)) name(g5, replace)) p(2) 
rdplot male bac1, c(0.08) graph_options(graphregion(color(white)) legend(off) ti("Panel B. Male") xti("BAC", margin(medium)) name(g6, replace)) p(2)
rdplot aged bac1, c(0.08) graph_options(graphregion(color(white)) legend(off) ti("Panel C. Age") xti("BAC", margin(medium)) name(g7, replace)) p(2)
rdplot white bac1, c(0.08) graph_options(graphregion(color(white)) legend(off) ti("Panel D. White") xti("BAC", margin(medium)) name(g8, replace)) p(2)

graph combine g5 g6 g7 g8, col(2) row(2) title("Figure 3-B. BAC and Charcteristics") graphregion(color(white))
graph export "cuadratica.png", as(png) replace

restore

****POINT 7***********
/* PANEL A*/
reg recidivism DUI bac1 male white aged acc if inrange(bac1,0.03, 0.13) [aweight=10], robust
outreg2 using "PanelA.doc", replace 
reg recidivism DUI bac1 interac male white aged acc if inrange(bac1,0.03, 0.13) [aweight=10], robust
outreg2 using "PanelA.doc", append  


gen interaction2=bac1*bac1
gen interaction3=interaction2*DUI 
/*Option 1 */
reg recidivism DUI##c.(bac1 interaction2) male white aged acc if inrange(bac1,0.03, 0.13) [aweight=10], robust
outreg2 using "hola.doc", replace
/*Option 2 */
reg recidivism DUI bac1 interac interaction2 interaction3 male white aged acc if inrange(bac1,0.03, 0.13) [aweight=10], robust
outreg2 using "PanelA.doc", append 

/*Note that both options are equivalent*/

/* PANEL B */
reg recidivism DUI bac1 male white aged acc if inrange(bac1,0.055, 0.105) [aweight=20], robust
outreg2 using "PanelB.doc", replace 
reg recidivism DUI bac1 interac male white aged acc if inrange(bac1,0.055, 0.105) [aweight=20], robust
outreg2 using "PanelB.doc", append 

reg recidivism DUI bac1 interac interaction2 interaction3 male white aged acc if inrange(bac1,0.055, 0.105) [aweight=20], robust
outreg2 using "PanelB.doc", append 

*****POINT 8*****
preserve
drop if bac1>=0.15
rdplot recidivism bac1, c(0.08) graph_options(graphregion(color(white)) legend(off) ti("Linear fit") xti("BAC", margin(medium)) name(g9, replace)) p(1)
rdplot recidivism bac1, c(0.08) graph_options(graphregion(color(white)) legend(off) ti("Quadratic fit") xti("BAC", margin(medium)) name(g10, replace)) p(2)

graph combine g9 g10, col(2) row(1) graphregion(color(white)) title("BAC on Recidivism")
graph export "recidivism.png", as(png) replace
restore
