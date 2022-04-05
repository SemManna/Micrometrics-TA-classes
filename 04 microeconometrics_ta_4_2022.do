** Microeconometrics 20295, TA 4, Tips for Problem Set 3
** Prof: Thomas Le Barbanchon
** TA: Jaime Marques Pereira
** Partial credits to Francesca Garbin and Alexandros Cavgias

* Objectives for the class: 
* 0 - Installing RD packages / Setting RD data; 
* 1 - Implementing RD design validations;
			//testing if the assumption for the
			//estimates retrived to be 
			//interpreted as causal ar valid
* 2 - Exploring RD graphics;
* 3 - Introduction to *rdrobust* (to estimate RD effects);
* 4 - Further introduction to *rdrobust*;
* 5 - Implementing RD robustness checks.

********************************************************************************


**#*** 0 - Installing RD packages / Setting RD data *** 
quietly {
    
** Installing RD packages **
*
* You can find all Stata packages written for RDDs at:
* https://github.com/rdpackages?language=stata
*
* In PS3 we will use 2 RD packages: 
* - *rdrobust*, to compute RD estimates;
* - *rddensity*, to implement manipulation tests 
* necessary to attest the validaty of your RD design.
*rdmulti also useful for RDD with multiple cutoffs and multiple scores (multiple running variables, you can read more in the packages-related articles in Stata journals)
*
* Install *rdrobust*
net install rdrobust, ///
from("https://raw.githubusercontent.com/rdpackages/rdrobust/master/stata") replace
*
* Install *rddensity*
net install rddensity, ///
from("https://raw.githubusercontent.com/rdpackages/rddensity/master/stata") replace
*
* Install *lpdensity*
net install lpdensity, /// 
from("https://raw.githubusercontent.com/nppackages/lpdensity/master/stata") replace
*
* LPDENSITY: local polynomial smoothing approach;
*
* First, approximate the discontinuous empirical CDF using local polynomial methods, then, employ that smoothed approximation to construct estimators of the distribution function, density function, and higher-order derivatives.
*
* In this class: used for RDDENSITY plot.


** Setting RD data **
*
* Import dataset from *rdrobust*'s repository
use rdrobust_senate.dta, clear
*
* Describe dataset
describe
//for each state vote share of dems in the next election and the margin of victory
*
* Summarize variables of interest
sum vote margin class termshouse termssenate population, sep(2)
*
* CONTEXT! We will estimate incumbency effects through close US Senate races.
//to understand incumbency effect we want to compare individuals very similar to one another --> margin of victory to compare individuals extremely close in their senate race to see if incumbents then get an advantage
*
* NOTE! Within a standard RD design we have:
* Y -> outcome variable;
* X -> running variable;
* T -> treatment variable.
*
* NOTE! In our example:
* Y -> Democratic vote share in t+1;
* X -> Democratic margin of victory in t-1;
* T -> Democrat Incumbent in t.
*
* Generate variables following RD notation
gen Y = vote
gen X = margin
gen T = (margin>0) //treatment, returns 1 or 0
//for convenience and ease of interpretation, set up the dataset in this way
}


**#*** 1 -  Implementing RD design validations *** 1.C AND 1.D 
quietly {

** C1 Discontinuity in treatment at threshold **
quietly {
	
rdplot T X
//plot treatment as a fct of the running variable, will visualize drastic discontinuity, here is between winning or losing the election 
//T is in fact discontinuous around the thrashold and then outcome also exibits a discontinuity!
* NOTE! Before proceeding, understand if outcome also exhibits a discontinuity
rdplot Y X
//plot outcome as a function of the running variable, it is assuming a polynomial of order 4 and it seems like there is an incumbency effect at the treshold!
*** TIP! Exhibit both discontinuity graphics.
*
* T-X
rdplot T X, graph_options(title(T-X Discontinuity) legend(off)) 
graph rename T_X, replace
* Y-X
rdplot Y X, graph_options(title(Y-X Discontinuity) legend(off))
graph rename Y_X, replace
* Combine and save
graph combine T_X Y_X
graph export combined_discontinuity_graphs.pdf, replace
*
* TIP! COMBINING GRAPHICS WILL BE USEFUL FOR ITEMS 1.C AND 1.D IN PS3.
*
* rdplot OPTIONS allows you to choose the bins to implement e.g.: option binselect(es) (evenly spaced) or binselect(qsmv) (quantile-spaced)

}

** C2 No discontinuity in baseline covariates at threshold ** //individuals below and above cut-off are equal and can be compared
quietly {
//we show this visually and numerically computing rd estimates
* rdrobust: treatment-effects estimation and inference (Robust RD Estimation using MSE bandwidth selection procedure)
*
* First step:  
rdrobust termshouse X
rdplot termshouse X 
*
rdrobust termssenate X
rdplot termssenate X
*
rdrobust population X
rdplot population X 
*
//rdrobust default cutoff is at 0!
//rdrobust output is a table, look at the coefficient, protocol is usually to provide coeffs using the conventional methods but showing std errors showing the robustness - in PS3 we are asked just to show the conventional ones
//the variables display no statistically significant coefficients at the threshold, which also holds visually
//depending on polynomial fit we may see some sort of jump, which is why computing the coefs with rdrobust is important
*
*
* TIP! Create a RD BALANCE CHECK TABLE to summarize covariate-discontinuities tests, exhibiting betas and p-values stored in *rdrobust* post-estimates.
*
* Understand which post-estimates are available from *rdrobust*
help rdrobust
*
rdrobust Y X
ereturn list //return to coefs and stats produced, then acessed to create a table!
*
local covs "termshouse termssenate population"
local num: list sizeof covs
mat balance = J(`num',2,.)
mat list balance 
local row = 1
foreach z in `covs' {
    qui rdrobust `z' margin
	mat balance[`row',1] = round(e(tau_cl),.001)
	mat balance[`row',2] = round(e(pv_rb),.001)
	local ++row
}
//tau_cl is the coeff estimate, while pv_rb is the p-value for the coefficient
mat rownames balance = `covs'
mat colnames balance = "RD Effect" "Robust p-val"
mat list balance 
*
* Stata does not save matrices with the dataset!! 
* You can save a matrix if you first convert it to a dataset using svmat. SVMAT
*
* TIP! *svmat* converts matrices into dataframes and vice-versa and stores its columns (svmat takes a matrix as new variables).
*
* It is a useful tool if you wish to construct a table: (1) store results in a matrix, (2) convert matrix onto dataframe, (3) export dataframe as Excel or Tex table.
//EG. frame command to then output that information into a separate dataframe and then output that dataframe into a Tex table!
*
svmat balance

}

** C3 Density of running variable "continuous" at cutoff **
quietly {
//RDDENSITY command does exactly this! Produces a statistic with associated p-value, testing the null hp. that density of the running variable is continuous at the cutoff, for a valid rd we should not reject this null!
rddensity X
*
* NOTE! The null hypothesis of "manipulation" tests is that the density of the running variable is "continuous" at the cutoff. For a valid RD design, you should not have any discontinuity - as it hints for a manipulation of the RD  design - hence, you should not reject the null.

}

** C4 No discontinuity in outcome variable away from threshold **
quietly { //very nice rcap - graph box-like gaph!
//manual computation, note: default cutoff for rdrobust is 0, which is also how running variable is constructed, if margin is positive you are an incumbent becasue you won the elections
//you can change this, c(cutoff) option tests other cutoffs!

rdrobust Y X, c(5)
rdrobust Y X, c(-5) 
*
rdrobust Y X, c(-10)
rdrobust Y X, c(10) 
*
rdrobust Y X, c(-15) 
rdrobust Y X, c(15) 
*
* NOTE! To avoid a contamination of the test from treatment, do not use alternative treatment cutoffs. Only placebo cutoffs within your control or treatment sample!
*
* TIP! A standard validation graphic is, to plot betas and CIs of RD estimates in placebo cutoffs - to argue for no discontinuities in your outcome variable away from yout treatment cutoff.
*
matrix define R = J(10,7,.)
local k = 1
forvalues x = -20(5)20 {

	if `x' > 0 {
		local condition = "if X >= 0"
	}
	else if `x' < 0 {
		local condition = "if X < 0"
	}
	else {
		local condition = ""
	}
	
	rdrobust Y X `condition', c(`x')

	matrix R[`k', 1] = `x'
	matrix R[`k', 2] = e(h_l)  		
	matrix R[`k', 3] = e(tau_cl)	
	matrix R[`k', 4] = e(tau_bc)	
	matrix R[`k', 5] = e(se_tau_rb)
	matrix R[`k', 6] = R[`k', 4] - invnormal(0.975) * R[`k', 5]
	matrix R[`k', 7] = R[`k', 4] + invnormal(0.975) * R[`k', 5]
	
	local k = `k' + 1
}
* e(h_l): bandwidth used for estimation of the regression function below the cutoff (left) - ow far away from cut-off are comparisons carried out
* e(tau_cl): conventional local-polynomial RD estimate (coefficient, beta of interest)
* e(tau_bc):  bias-corrected local-polynomial RD estimate - robust form
* e(se_tau_rb): robust standard error of the local-polynomial RD estimator
*
matrix list R
*
* NOTE! If your RD design is to be valid, you should not have any RD estimate (aside from that associated with your treatment cutoff) different than zero - i.e., zeros should be inside all plotted CIs, except for the treatment cutoff.
*
// plot RD coefficients and CI for all cutoffs to see if we have some estimate far away from our cutoff where an effect is visible, then restoring the original data
preserve
	clear
	svmat R
	* *rcap* allows you to plot the lower and upper bound of CIs.
	* graph twoway rcap: range plot with capped spikes, *twoway rcap y1var y2var xvar*
twoway (rcap R6 R7 R1, lcolor(navy)) /**scatter* allows you to plot yoru RD point estimates.*/ (scatter R3 R1, mcolor(cranberry) yline(0, lcolor(black) lpattern(dash)) xline(0, lcolor(black) lpattern(dash)) xticks()), graphregion(color(white)) xlabel(-20(5)20) xtitle("Cutoff (vertical dashed line = true cutoff)") xscale(titlegap(*10)) ytitle("RD Treatment Effect") legend(off)
restore

}

}


**#*** 2 - Exploring RD graphs ***
quietly {
//thanks to the graph_options option of RDPLOT you can personalize the RD plot as any other plot in stata
//note, rdplot fits a polynomial in the binned variable, you can also plot it with CI with binselect(es) es for eavenly spaced or qsmv for quantile spaced and choosing the CI (see below)
*
** Discontinuity of outcome, binned scatter plot **
rdplot Y X, ///
       graph_options(title("Incumbency Effects in U.S. Senate Election") ///
                     ytitle(Vote Share in Election at time t+1) ///
                     xtitle(Vote Share in Election at time t) ///
                     graphregion(color(white)))

					 
** Discontinuity of outcome, binned CI plot **				 		 
rdplot Y X, binselect(es) ci(95) ///
       graph_options(title("Incumbency Effects in U.S. Senate Election") ///
                     ytitle(Vote Share in Election at time t+1) ///
                     xtitle(Vote Share in Election at time t) ///
                     graphregion(color(white)))
		//changing binselect to qsmv the graph is very different			 
					 
					 
** Histogram of running variable **
//use rdrobust to plot running var as an hist
rdrobust Y X
scalar h_left = -e(h_l) //lower bandwidth
scalar h_right = e(h_r) //upper bandwidth
twoway (histogram X if X >=h_left & X < 0, freq width(1) color(blue)) ///
	(histogram X if X >= 0 & X <= h_right, freq width(1) color(red)), xlabel(-30(10)30) ///
	graphregion(color(white)) xtitle(Score) ytitle(Number of Observations) legend(off)
//even if it may seem that there is a density discontinuity at the cutoff this is actually due to the bins, if we look at the density within/continuously, this is not the case - as also computed in point 1 with the test statistics

** Density of  running variable **
//nonparametric estimation dor the density using left and right bandwidth
local h_l = h_left
local h_r = h_right
rddensity X, plot plot_range(`h_l' `h_r')
*
* NOTE! rddensity plot is implemented using *lpdensity* for local-polynomial–based density estimation

}			 

	
**#*** 3 - Introduction to *rdrobust*  ***
quietly {
//estimate different RD	effects using the global regressions method, the RD robust command or the local regressions method - through rdrobust
		
** Global regression using polynomials ** 
*
* Generate variables to fit a polynomial of order 4 (or any other of choice)
gen X_2 = margin^2
gen X_3 = margin^3
gen X_4 = margin^4
*
gen X_T = T*X
gen X_T_2 = T*X_2
gen X_T_3 = T*X_3
gen X_T_4 = T*X_4
*
* Estimate global regression, fitting a polynomial of order 4 on our outcome
reg Y T ///
X X_2 X_3 X_4 ///
X_T X_T_2 X_T_3 X_T_4
//T is global RD estimate - note, we are using the full support, while an RD estimate is one that focuses only on a restricted sample of observations massed around the cutoff

** Local regressions **
*
* Through *rdrobust*
rdrobust Y X
*
* Exhibit conventional, bias-corrected and robust estimates 
rdrobust Y X, all

**# TAKE-HOME! Read Cattaneo, Idrobo and Titiunik (2019)'s section 4.3 to understand which are the differences between conventional, bias-corrected and robust estimates.
*
* LINK! You can find Cattaneo, Idrobo and Titiunik (2019) at:
* https://cattaneo.princeton.edu/books/Cattaneo-Idrobo-Titiunik_2019_CUP-Vol1.pdf
* 
* NOTE! Recommended practice for RD studies is to estimate *betas* using the *Conventional* procedure and *standard errors* using the *Robust* procedure.
*
* Estimate RD effects through a local approach, using a uniform kernel
rdrobust Y X, kernel(uniform)
*
* Understand which post-estimates are available from *rdrobust*
//replicate conventional coef manually of rdrobust kernel(uniform)!
ereturn list
*
* Store optimal bandwidth, selected via *rdrobust*'s selection algorithm
scalar h_l=-e(h_l)
scalar h_r=e(h_r)

** Local RD estimates through Stata's *reg* **
*
* TIP! USEFUL FOR POINT 1.J OF PS3. 
*
* Estimate RD effects through a local approach, using a uniform kernel
//uniform kernel weights all the observations within the support equally - it's an OLS in the end
rdrobust Y X, kernel(uniform)
*
* Store optimal bandwidth, selected via *rdrobust*'s selection algorithm
ereturn list 
scalar h_l=-e(h_l)
scalar h_r=e(h_r)
*
* Estimate a linear regression at the left of the cut-off
reg Y X if X < 0 & X >= h_l
matrix coef_left = e(b)
matrix var_left = e(V)
scalar intercept_left = coef_left[1, 2] //storing the intercept
*
* Estimate a linear regression on the right of the cut-off
reg Y X if X >= 0 & X <= h_r
matrix coef_right = e(b)
matrix var_right = e(V)
scalar intercept_right = coef_right[1, 2]
*
* Compute the RD effect as rd = right - left
scalar difference = intercept_right - intercept_left
matrix var_conventional = var_left + var_right
scalar se_difference = sqrt(var_conventional[2,2])
*
scalar list difference
scalar list se_difference
rdrobust Y X, kernel(uniform) all
*
* NOTE! Results are the same. 
*
* Estimate RD effects through a local approach, using a triangular kernel
//a triangular kernel weights observations closer to the cutoff more 
rdrobust Y X, kernel(triangular)
//now, steps from  manual computation produce different estimates, to produce the same ones we need to estimate a weighted least squares were observations are weighted based on the function of the kernel used - here the triangular one: 1-|X/bandwidth|
* Store optimal bandwidth, selected via *rdrobust*'s selection algorithm
ereturn list 
scalar h_l=-e(h_l)
scalar h_r=e(h_r)
*
* Estimate a linear regression at the left of the cut-off
reg Y X if X < 0 & X >= h_l
matrix coef_left = e(b)
scalar intercept_left = coef_left[1, 2]
*
* Estimate a linear regression on the right of the cut-off
reg Y X if X >= 0 & X <= h_r
matrix coef_right = e(b)
scalar intercept_right = coef_right[1, 2]
*
scalar difference = intercept_right - intercept_left
*
scalar list difference
rdrobust Y X, kernel(triangular)
*
* NOTE! Results are NOT the same. 
*
* NOTE! To replicate *rdrobust*'s estimates under a triangular kernel,
* you are required to estimate a WLS with weights defined according to the
* triangular kernel formula.
*
* Re-define scalar for unexplained inversion of sign
scalar h_l = -h_l
*
* Generate weights
*
gen weights = .
*
replace weights = (1 - abs(X/h_l)) if X < 0 & X >= h_l
replace weights = (1 - abs(X/h_r)) if X >= 0 & X <= h_r
*
* NOTE! You can find how to construct these weights for different types of kernels at https://en.wikipedia.org/wiki/Kernel_(statistics).
*
* Repeat previous procedure
*
reg Y X [aw = weights] if X >= h_l & X < 0
matrix coef_left = e(b)
scalar intercept_left = coef_left[1, 2]
*
reg Y X [aw = weights] if X >= 0 & X <= h_r
matrix coef_right = e(b)
*
scalar intercept_right = coef_right[1, 2]
scalar difference = intercept_right - intercept_left
*
scalar list difference
rdrobust Y X, kernel(triangular)
*
* NOTE! Now Results are the same.

}


**#*** 4 - Further introduction to *rdrobust* ***
quietly {
//rdrobust chooses optimal bandwidth - yet it should not matter, varying the bandwidth we should still see the effect, robustness check!
** Specifying the main bandwidth (h) used to construct the RD point estimator
* (two values: one below, one above cutoff)
rdrobust Y X, h(20 20)
//look at BW est() !!
* NOTE! Robust RD analyses do not depend on the bandwidths chosen.


** Including covariates **
//we can control for unit-specific charactheristics!
rdrobust Y X, covs(population)
* NOTE! Including covariates does not always lead to improved precision. 
* For example, if the covariates are irrelevant, they can increase your CIs.


** Selecting bandwidth ** 
rdbwselect Y X, all		
//regression discontinuity bandwidth select command! 	
//shows all optimal bandwidths according to different criteria		 
 
** Defining polynomial order a priori ** 
rdrobust Y X, p(1)
rdrobust Y X, p(2)
rdrobust Y X, p(3)
rdrobust Y X, p(4)

rdplot Y X,  p(1) 

** Involved plot with a polynomial of order 1, 
* focused around the cutoff (within the bandwidth):
quietly rdrobust Y X
rdplot Y X if -e(h_l)<= X & X <= e(h_r), binselect(esmv) kernel(triangular) h(`e(h_l)' `e(h_r)') p(1) graph_options(title("RD Plot: U.S. Senate Election Data") ytitle(Vote Share in Election at time t+2) xtitle(Vote Share in Election at time t) graphregion(color(white)))
//issues with overfitting, but the larger the ploynomial order the more conservative our estimate will be! 
//usually, show RD effect assuming polynomial of order 4, change it one above or below and estimate should not vary
}


**#*** 5 - Implementing regression discontinuity rob. checks ***
quietly {
//irrespective of model choices, estimates should not vary
*
* TIP! USEFUL FOR POINT 1.K OF PS3.
*
* NOTE! Robust RD estimates are those that are independent of most RD methodological choices that a researcher can choose.
*
* Common robustness checks are: 
* (1) robustness to exclusion of observations around the cutoff; 
	//not related to modelling choice but to if estimates are driven by 
	//outliers within the support, which shouldn't, estimates should be
	//robust to exclusion of certain observations around the cutoff
* (2) robustness to different bandwidths; 
	//effect relatively constant irrespective of bandwidths
* (3) robustness to different RD populations.
*
* To illustrate how one argues for the (2) robustness to different bandwidths:
*
* Compute alternative bandwidths
rdrobust Y X
di .5*abs(h_l)
di .75*abs(h_l)
di 1.25*abs(h_l)
di 1.5*abs(h_l)
//computing a series of alternative bandwidts based on the optimal one computed by rdrobust
*
//imputting in a matrix rd effects when variating the bandwidths based on the computed ones
matrix define R = J(4, 6, .)
global bandwidths "8.8772 13.3158 22.193 26.6316"
local r = 1
foreach k of global bandwidths {
	rdrobust Y X, h(`k')
	matrix R[`r', 1] = `k'
	matrix R[`r', 2] = e(tau_cl)
	matrix R[`r', 3] = e(tau_bc)
	matrix R[`r', 4] = e(se_tau_rb)
	matrix R[`r', 5] = R[`r', 2] - invnormal(0.975) * R[`r', 4]
	matrix R[`r', 6] = R[`r', 2] + invnormal(0.975) * R[`r', 4]
	local r = `r' + 1
}
//outputting the matrix into the dataframe and plotting for each bandwidth, the respective coefficient estimate and CI
preserve
	clear
	svmat R
	twoway (rcap R5 R6 R1, lcolor(navy)) /*
	*/ (scatter R2 R1, mcolor(cranberry) yline(0, lcolor(black) lpattern(dash))), /*
	*/ graphregion(color(white)) xlabel(8.8540144 13.281022 22.135036 26.562043) ytitle("RD Treatment Effect") /*
	*/ legend(off) xtitle("Bandwidth") yscale(range(-5 10)) 
restore
*
* If *rdrobust*'s option to set the bandwidth a priori - *h* option - is not functioning, a possible substitute is to:
* (1) manually estimate your RD coefficientes through local regressions;
* (2) compute an **approximation** of a *conventional* standard error as follows:
* (3) replicate loop above to plot point estimates with **approximated** *conventional* CI.
*
* See below (assuming a uniform kernel):
*
rdrobust Y X, kernel(uniform)
*
* Store optimal bandwidth, selected via *rdrobust*'s selection algorithm
ereturn list 
scalar h_l=-e(h_l)
scalar h_r=e(h_r)
*
* Compute alternative bandwidths
di abs(h_l)
di .5*abs(h_l)
di .75*abs(h_l)
di 1.25*abs(h_l)
di 1.5*abs(h_l)
*
matrix define R = J(5, 5, .)
global bandwidths "5.798435 8.6976525 11.59687 14.496087 17.395305"
local r = 1
foreach k of global bandwidths {
	*
	* Left of the cut-off
	reg Y X if X < 0 & X >= -`k'
	matrix coef_left = e(b)
	matrix var_left = e(V)
	scalar intercept_left = coef_left[1, 2]
	*
	* Right of the cut-off
	reg Y X if X >= 0 & X <= `k'
	matrix coef_right = e(b)
	matrix var_right = e(V)
	scalar intercept_right = coef_right[1, 2]
	*
	* RD estimate and standard error
	scalar b_difference = intercept_right - intercept_left
	matrix var_conventional = var_left + var_right
	scalar se_difference = sqrt(var_conventional[2,2])
	*
	matrix R[`r', 1] = `k'
	matrix R[`r', 2] = b_difference
	matrix R[`r', 3] = se_difference
	matrix R[`r', 4] = R[`r', 2] - invnormal(0.975) * R[`r', 3]
	matrix R[`r', 5] = R[`r', 2] + invnormal(0.975) * R[`r', 3]
	local r = `r' + 1
}
*
preserve
	clear
	svmat R
	twoway (rcap R4 R5 R1, lcolor(navy)) /*
	*/ (scatter R2 R1, mcolor(cranberry) yline(0, lcolor(black) lpattern(dash))), /*
	*/ graphregion(color(white)) xlabel(5.798435 8.6976525 11.59687 14.496087 17.395305) ytitle("RD Treatment Effect", height(5)) /*
	*/ legend(off) xtitle("Bandwidth (11.6 == Original h)", height(6.25)) yscale(range(-5 10)) 
restore  
*
* DISCLAIMER: NOT TO USE ON YOUR OWN RESEARCH; CIs CORRESPONDE ONLY TO AN
* **APPROXIMATION** OF "CONVENTIONAL" CIs; HENCE, CIs ARE UNDERESTIMATED,
* EXISTING A OVER-REJECTION OF THE STANDARD NULL HYPOTHESIS THAT B=0.
*
* USED ONLY DUE TO AN APPARENT FAILURE OF *RDROBUST*'S OPTION TO IMPOSE A
* BANDWIDTH A PRIORI OF ESTIMATING ANY RD SPECIFICATION.

}

********************************************************************************

*** LAST BUT NOT LEAST... Can I use *outreg2* in this RDD context?
//yes, they wrot rdrobust so that outreg2 is able to retrive coefs and all other relevant stuff as if it was working with reg!
* If you miss printing tables (tex/Excel), take a look at 
* Calonico Cattaneo Titinik, The Stata Journal (2014), page 35!
*
* ... which you can find here:
*
* https://journals.sagepub.com/doi/pdf/10.1177/1536867X1401400413
