** Microeconometrics 20295, TA 5, Tips for Problem Set 4
** Prof: Thomas Le Barbanchon
** TA: Jaime Marques Pereira
** Partial credits to Francesca Garbin and Alexandros Cavgias

* Objectives for the class: 
* 1 - Introduction to panel data; 
* 2 - Introduction to time-series operators;
* 3 - Regression analyses on Stata - compact coding;
* 4 - Regression analyses on Stata - high-dimensional FEs //eg. controlling for individual-level characteristics with huge datasets;
* 5 - Motivating parallel trends;
* 6 - Estimating DiD specifications on Stata;
* 7 - Implementing DiD robustness checks.
** Extra Material on Panel data analysis in Stata 
** Extra code on binary dependent variables

********************************************************************************


*** 1 - Introduction to panel data *** 
quietly {

** Setting panel data **
* Import dataset from Stata's repository
webuse nlswork, clear
*National Longitudinal Survey of Young Women, 14-24 years old in 1968

* COMMENT! If dealing with panel data, rapidly identify your 
*(a) time identifier, commonly a time variable taking unique values within each time unit, and your 
*(b) cross-sectional identifier, commonly a unique identifier for each cross-sectional unit within each time unit in a panel.
*
*** Describe dataset
describe
*
* *year* is our time identifier - 68 to 88
tab year
* *idcode* is our cross-section identifier
codebook idcode
bysort year: distinct idcode //shows the number of total and distinct observations in each year
*
* Specify time and cross-section identifiers to Stata 
//important to set the dataset as a panel data in stata, specifying the corss-sectional and time dimensions
help xtset
xtset idcode year
*
* NOTE! *XTSET* requires identifiers to be numeric variables.
//non-numeric identifiers are common in amdministrative data..
tostring idcode, gen(str_idcode)
xtset str_idcode year
*
* To proceed, either DESTRING a numeric variable...
destring str_idcode, gen(dstr_idcode)
xtset dstr_idcode year
*
* ... or, if any of your identifiers is not convertable to a numeric variable, use Stata's *GROUP* function instead as follows:
egen cross_id = group(str_idcode)
xtset cross_id year
*
* NOTE! Apart from specifying your time and cross-sectional identifiers, you are also to specify the *temporal unit* that will be used on your panel data analysis - e.g., if yearly, quarterly, bi-annually or monthly averages, constructed from daily panel data.
*
* Choosing your *temporal unit* can be relevant for your identification (e.g. to increase power) and for inference (to argue for robustness to false positivities).
*
* Bertrand et al. (2004) influential paper "How much should we trust differences in differences estimates?" suggests that researchers ought to collapse outcomes in pre-treatment and post-treatment averages to tackle autocorrelation problems that are common in panel data analysis. 
*
* If such a solution underpowers your regression analyses, you ought to cluster standard errors at the level of your cross-sectional unit, or use a more aggregate cluster unit that can be estimated by your data (e.g., state when town is your cross sectional unit). 

* TIP! Option delta() from *XTSET* allows you to change your temporal window.
//EG PS4 our data is not yearly buy bi-annual, this has to be specified

}

*** 2 -  Introduction to time-series operators ***
quietly {

** Setting panel data **
*
* Import dataset from Stata's repository
webuse nlswork, clear
*
* Setting time and cross-sectional identifiers
xtset idcode year, delta(1)
xtdes


** Working with lag operators **
*
* //understanding persistance of wage. l operator for working with lagged variables (without having to create them)
//without this compact notation, we would have to generate lagged variables first, eg, l_lnLwage = l.ln_wage

* One (1) lag
reg ln_wage l.ln_wage //relationship bentwee W in year t and t-1
*
* Two or more (>=2) lags *
reg ln_wage l2.ln_wage //l two is more concise, 2 lags
reg ln_wage l.l.ln_wage //same, t lags


** Working with lead operators **
*
* One (1) lead  //same thing but going forwards, should produce the same result in this case
reg f.ln_wage ln_wage 
*
* Two or more (>=2) leads
reg f.f.ln_wage f.ln_wage ln_wage 


** Working with lead and lag operators succintly ** 
*
reg ln_wage l.ln_wage l.l.ln_wage
*
reg ln_wage l(1/2).ln_wage //to include simultaeously the first and second lag. E.g. first to third would be l(1/3)

}

*** 3 - Regression analyses on Stata - compact coding ***
quietly {

* COMMENT! Compact syntax is particularly useful for regression models with a considerable number of dummies and/or large datasets.

** Dummies without generating Dummies (Stata's *i.*) **
*
* Tabulate your covariate of interest
tab race
*
* Regress Y controlling for different categories of your categorical X 
reg ln_wage i.race
*
* NOTE! While using *i.*, Stata will drop one group to avoid multicollinearity issues - with *i.* Stata will commonly drop your X's first category. //first category showing up in sorted dataset will be used as benchmark! - sort the data for the discrete variable first so to know which benchmark is used
*
* NOTE! *i.* is equivalent to creating dummies to distinguish between alternative categories of your categorical covariate X
//the non-compact way to do this would be to geenrate the dummies first with dummies race, then regress ln_wage on all dummies but one, also, using i. we keep the labels
*
tab race
dummies race 
reg ln_wage race2 race3 
*
* NOTE! Excluding a constant, we are able to recover group averages:
reg ln_wage race1 race2 race3, noc 
//with noconstant we can include all dummies, and we compute the avg wage of any race compared to average wage! Hence produces category-specific averages, identical to sum command
sum ln_wage if race1==1
sum ln_wage if race2==1
sum ln_wage if race3==1


** INTERACTING covariates without generating interactions ( Stata's *i.x#i.y*) **
*
* Tabulate your pair of covariates of interest
tab race south
tab race south, cell
//how wage varies across race, but accountring for whether one is working in the south of the US or not
reg ln_wage i.race#i.south //all coefs are wrt benchmark of the first category
*
* NOTE! We have 6 (x,y) combinations but 5 coefficients - white#0 has been dropped to avoid multicollinearity issues, it should be taken as our reference combination.


** Dummies + Interactions ( Stata's *i.x##i.y*) **
*
* Tabulate again your covariates of interest
tab south union
*
reg ln_wage i.south##i.union //double hastag includes DUMMIES AND INTERACTIONS --> this is also how we will implement DiD
* same as
reg ln_wage i.south i.union i.south#i.union
*
* NOTE! Stata drops first categories (from dummies and intersections) for multi-collinearity issues - here our references are 0.south, 0.union and 0#0.


** Interacting a CONTINUOUS variable with dummies (Stata's *x#c.y*) **
*
* Tabulate your binary covariate of interest
tab south 
* Summarize your continuous covariate of interest
sum hours
//to understand extra wage per extra hour worked regress SPECIFYING THAT HOURS IS CONTINUOUS with c.
reg ln_wage south#c.hours
*
* NOTE! Two coefficients, one for hours if south=0, another for hours if 
* south=1. Note that in contrast with i.x##i.y, Stata does not drop variables. 


** Variables + Interactions ( Stata's *i.x##c.y) **
//including interactions and dummies!
reg ln_wage i.south##c.hours
*
* NOTE! Stata drops first categories (for dummies only)!

}


*** 4 - Regression analyses on Stata - high-dimension FEs ***
quietly {
					
* NOTE! Estimating a DiD specification with thousands of FEs is either 
*(a) too  costly time-wise on Stata, using regular methods, or 
*(b) not possible as you exhaust your degrees of freedom - need to restrict to a more aggregate level, eg. towns for individual-level data
*
* SOLUTION (1)! 
*Estimate your FE model in (a) first-differences or (b) by demeaning individual-level cross-sectional units - so-called within-group estimators.
// reg including all the fixed effects is not feasible. The command is inverting a matrix, with million of observations this would take years..
*
* (a) first-differences 
//controlling for individual FE means subtracting from individual-level observations, individual-specific averages - hence we look at variaitons across time relative to an average. Instead of taking the individual-specific first differences, we can use the d. to first difference (wage and weeks worked)
reg d.ln_wage d.wks_work
*
* (b) demeaning, within estimator - implemented through AREG - more sphisticated regression comand for panel data *areg, absorb(.)* //absorb to control for cross-sectional- or time- FE
areg ln_wage wks_work, absorb(idcode)
*
* (b)demeaning, within-estimator - implemented through *xtreg*, fe i(.)* //regression within an xt context, so a panel context - specifing FE model with individual FEs
xtreg ln_wage wks_work, fe i(idcode)
* Option i() makes xtreg equivalent to areg. * 
*
* NOTE! First-difference and demeaning transformations are NOT equivalent if we are working with a panel with more than 2 periods.
*
*
* SOLUTION (2)! Estimate your FE model using "reghdfe" REGHDFE: REGRESSSION WITH HIGH DIMENSIONAL FE (by Sérgio Correia).
* This is a Stata package that allows you to control for high-dimensional FEs in as an efficient way as possible (through numerical optimization algorithms) - lerge time gains with huge datasets!
*
ssc install reghdfe
ssc install ftools
help reghdfe
*
reghdfe ln_wage wks_work, absorb(idcode)
*fixest libary does the same, but with it Stata can better use the full extent of your processor, and reduces the time required to run the multiple regressions in the model, but it it an R library (can be called through stata anyway) - Laurant Bergè
*				
}
 
*** 5 - Motivating parallel trends  ***
quietly {

* NOTE! To validate your DiD design you are to *motive* the parallel trends assumption with a pre-trend analysis. 
//main identification assumption in DiD - comparing how T group evolved over time pre-and post treatment and how it compares with the evolution of C group - for the evolution to be interpretedd as an ATT we need that the two groups behave in a similar way pre-treatement. 
//With more than two periods, argue graphically and test if the trends of outcomes in the two groups are statistically different in an event-study type of setting. Several pre-and-post treatment coefficients and within that design, arguing in favour of the parrallel trend assumption we should have pre-treatment coefficients NOT different from zero --> avergae behaviour of T group relative to control is not different from zero, i.e. they are 'the same', related
//we first argue for parallel trand assumption graphically before runing event study
*
*
* EXAMPLE! Assume a particular policy change took place in 1980, differentially affecting employment opportunities of African-americans (e.g., an anti-discrimination bill). 
*
* Then we ought to believe that non-African-americans were treated significantly differently before the enactment of our hypothetical anti-discrimination bill.
*
* How are we to understand if non-African-americans are a relevant control group for African-american workers in a DiD analysis? 
*
* COMMON PRACTICE: Graph trends of Y for our control and treatment groups in the periods before a significant policy change.
*
tab race
codebook race
*
gen TREATED = (race==2)
*
gen Y_T=wks_ue if TREATED==1 
gen Y_C=wks_ue if TREATED==0
*
preserve 
*
	collapse Y_C Y_T, by(year) //collapsing the mean for each year, within the two groups
*
	twoway  (line Y_T year) ///
	(line Y_C year, lpattern(dash)),  ///
	xline(80) title(Outcome trends) ///
	ytitle(Number of Unemployed Weeks)   /// 
	legend(order(1 "Treatment" 2 "Control"))   
restore 
//x line plots the vertical line where the hypotetical policy took place.
// here we are NOT looking at statistical significance - visual argument in favour of parallel trends - 
//to understand if the difference btw weeks worked on average in the two groups before the policy, instead of collapsing the info in pre treatment and post treatment period, we can not collapse anything and just runa a regression with coefficients for pre-and post-treatemnt periods. If these pre-treatment coefficients are significantly different from zero, then the difference from the two groups is different from zero prior to the treatment - here we are just visualizing the behaviour of the two lines, just hypothetical, visual argument 
*
* NOTE! We can also perform a placebo test to check if control and treatment 
* trends  were significantly different before our policy-change (LATER!). 

}
			 				 
*** 6 - Estimating DiD specifications on Stata  ***
quietly {

** Setting data **
*
* Downloading dataset
use "http://fmwww.bc.edu/repec/bocode/c/CardKrueger1994.dta", clear 
*
* CONTEXT! Card and Krueger (1994) - effect of minimum wage on unemployment.
*
* Describe dataset
describe
*
* Outcome Y -> full time of employment
gen Y = fte 
* Post-treatment POST -> period affect the enactment of a minimum wage in New Jersey 
gen POST = t
* Treatment group TREATED -> workers from New Jersey
gen TREATED = (treated==1)
*
* Post-treatment treated units - interaction post and T!
* -> workers from New Jersey after a minimum wage being established in NJ
gen POST_TREAT = POST*TREATED


** DiD "by hand" **
*
* NOTE! A DiD specification with a discrete treatment is equivalent to a difference of means. Hence, we can estimate these "by hand".
*
* CONVENTION! 
* Y^g_t -> outcome of group g at time t
* g = {0,1}, where 0 means control and 1 treatment
* t = {0,1}, where 0 means pre-treatment and 1 post-treatment
*
* g=1, t=1
sum Y if TREATED==1 & POST==1
scalar AVG_Y_1_1 = r(mean)
* g=1, t=0
sum Y if TREATED==1 & POST==0
scalar AVG_Y_1_0 = r(mean)
* g=0, t=1
sum Y if TREATED==0 & POST==1
scalar AVG_Y_0_1 = r(mean)
* g=0, t=1
sum Y if TREATED==0 & POST==0
scalar AVG_Y_0_0 = r(mean)
*
scalar DiD = (AVG_Y_1_1 - AVG_Y_1_0) - (AVG_Y_0_1 - AVG_Y_0_0)
scalar list DiD


** DiD by regression  **
*
* #1
reg Y POST_TREAT POST TREATED
*
* #2 same as #1 but using Stata's compact coding 
reg Y i.POST##i.TREATED
*
* #3
* NOTE! As with other inference methods, a Stata *ado* file exists:
ssc install diff
diff Y, t(TREATED) p(POST)

}

*** 7 - Implementing DiD robustness checks ***
quietly {

** Controlling for baseline covariates **
//note, recent (2022) literature casts some doubt on this control
*
* NOTE! A standard robustness check, in particular if levels of baseline covariates are unbalanced across time, is to control for interactions of time (or our post-treatment indicator) and baseline covariates.
*
* #1
* Original "by reg" DiD
reg Y POST_TREAT POST TREATED
* 
* Controlling "by reg" for time*covariate interactions
//allow for over time trends for a series of covariates of interest in explaining empolyment in each region
reg Y POST_TREAT POST TREATED i1.POST#(i1.bk i1.kfc i1.roys i1.wendys)
*
* #3
* Original "by *diff*" DiD
diff Y, t(TREATED) p(POST)
*
* Controlling "by *diff*" for time*covariate interactions
diff Y, t(TREATED) p(POST) cov(i1.POST#(i1.bk i1.kfc i1.roys i1.wendys))
//problem, compact coding is not allowed when using option in a third-party library! --> manually compute interactions to include them in the command
gen POST_BK = bk*POST
gen POST_KFC = kfc*POST 
gen POST_ROYS = roys*POST 
gen POST_WENFYS = wendys*POST
diff Y, t(TREATED) p(POST) cov(POST_BK POST_KFC POST_ROYS POST_WENFYS)
*
//qualitative, the sign is maintained when adding the covariates.
* NOTE! Recent literature on DiDs has argued that controlling for baseline covariates that vary in time and/or are affected by treatment requires you to make additional identification assumptions that normally are not plausible in standard settings (Caetano et al., 2022; which you can find at https://arxiv.org/abs/2202.02903).
*
//that identification assumption is that each of the time-varying covariates including were, pre-treatment, also having parallel trends (individually!) - if that does not happen, estimates will not be robust, and we cannot interpret the estimate as causal if we do not provide the visual arguemtn of parallel trends for every included covariate! Otherwise, the estimate is not causal! Is a weighted average of the unitis according to covariates which will not allow for causal interpretation.
* These additional assumptions boil down to your covariates having to present no pre-treatment trends before treatment, besides your outcome. In most empirical applications this is too stringent of an assumption. Luckily, Caetano and co- authors propose alternative DiD estimators robust to this issue.
//e.g. if we do not have parallel trends in covariates, if we want to add them because parallel trends assumption in the outcome hold only conditional on our covariates, then those doubly-robust DiD estimatos should be used.
*
* To implement these though you will have to temporarily migrate to R and use Callaway & Sant'Anna's "did" library that allows you to compute these types of estimators (so-called doubly-robust DiD estimators).
*
* If you are curious about this I would recommend you to read through Scott Cunningham's blog post on the issue. There he explains the intuition behind the problem and how it can be solved. He also implements the solution with the "did" library in R:
*
* https://causalinf.substack.com/p/a-tale-of-time-varying-covariates?s=r


** Placebo testing **
*
* NOTE! Another common robustness check in a differences in differences analysis is to perform a placebo test, regressing pre-treatment outcomes on treatment status. //using a random date for T and showing that outcome in treatment group does not diverge at that date.
* 
* Unfortunately, we are not able to perform a placebo test in Card and Krueger's dataset - FOR A PLACEBO TEST WE REQUIRE AT LEAST 2 PRE-TREATMENT PERIODS.
*
* Returning to our original dataset, assume a placebo policy change in 1972 and test if our outcome trends of control and treatment groups are different around our place post variable.
*
webuse nlswork, clear
*
destring year, replace	
*
gen Y = wks_ue
*
gen TREATED = (race==2)
gen PLACEBO_POST=(year>=72) 
gen PLACEBO_POST_TREAT = PLACEBO_POST*TREATED 
*
reg Y PLACEBO_POST_TREAT PLACEBO_POST TREATED if year<78
//random data and date, do not interpret coefs, it's just an example
}

********************************************************************************

*** Extra Material on Panel data analysis in Stata ***
quietly {

* Once you xtset your dataset, you can see its characteristics
* Import dataset from Stata's repository
webuse nlswork, clear
*
* Describe dataset
describe
*
* Specify time and cross-section identifiers to Stata
xtset idcode year 

* Balanced or unbalanced panel?
*
* Describe patterns:
xtdescribe

* Summary statistics: overall, and over time (between/within variation)
summ ln_wage
xtsum ln_wage
* how can I identify time-invariant variables from the xtsum table?
xtsum birth_yr

* Additional information on between and within variation
* Panel tabulation for a variable
xttab south

* Transition matrices
xttrans south, freq

* How does occupation change for these individuals over the seven years?
xttab occ
xttrans occ //very useful!!

* Simple time-series plot for experience of 10 individuals
quietly xtline ttl_exp if idcode<=10, overlay 

* Panel data regressions
* Pay attention to the relationships among your variables!
xtsum ln_wage age birth_yr year 

* Capturing variation across individuals or within individuals?
* Pay attention: is there a relationship between age, birth cohort, and current year?
xtreg ln_wage age birth_yr year, fe //only within variation
xtreg ln_wage age birth_yr year, be //only between variation

xtreg ln_wage age birth_yr, fe
xtreg ln_wage age birth_yr, be
* Can you say why fe and be consider these covariates differently? 
* Which variation are these picking up?

}

*** Extra code on binary dependent variables ***
quietly {

clear 

webuse union
de
xtdes
xtsum union age grade south
* SMSA: standard metropolitan statistical area 

* Logit
logit union age grade not_smsa south , vce (cluster id)
estimates store LT //how does Pr of being a union member changes with relevant covariates? Errors clusterd at the individual level

* RE
xtlogit union age grade not_smsa south  , i(id) re nolog
estimates store randomeff

* FE
xtlogit union age grade not_smsa south  , i(id) fe nolog
estimates store fixedeff

* Compare the estimates by compiling a table
estimates table LT randomeff fixedeff, b(%10.4f) se stats(N)

* Hausman test //understand if we should work with random effects or fixed effects model!
*
* equations() specifies by number the pairs of equations that are to be compared e.g. eq(1:2) means that eq.1 of the always-consistent estimator is to be tested against eq.2 of the efficient estimator:
hausman fixedeff randomeff, eq(1:1) 
* Prob>0.0032 reject random effect, even if there is a tiny significant difference.

}
