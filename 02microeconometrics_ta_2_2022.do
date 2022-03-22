** Microeconometrics 20295, TA 2, Tips for Problem Set 1
** Prof: Thomas Le Barbanchon
** TA: Jaime Marques Pereira
** Partial credits to Francesca Garbin and Alexandros Cavgias

* Objectives for the class: 
* 0 - Global and local variables;
* 1 - Loops in Stata;
* 2 - Data visualization;
* 3 - Regressions;
* 4 - Output tables/plots;
* 5 - Balance check table;
* 6 - Inference in randomized experiments.

********************************************************************************


**#0 - Global/local variables ***

quietly {

* Import example dataset installed with Stata
sysuse auto, clear
//GLOBAL VARIABLES, variables that are saved in Stata's memory throughout your work, until you close the window
*
* Define global variable *baseline* as "price mpg lenght"
global baseline "price mpg length"
//this allows us to call all three variables by just calling the global variable
* Summarize variables included in *baseline*
sum ${baseline}
*
* NOTE! Use $ to call a global!!
* NOTE! Globals can be called at any point of the script. 

* Why using parentheses? 
* The ${} are used to make the name of a global macro explicit to Stata when the context would otherwise make it unclear -- primarily when you want to embed a global macro name in a text string:
*
display "abc${baseline}def"
* It allows Stata to figure out that the global macro is called baseline.
*

** Defining a *local* variable **
* Define local variable *listlocal* as "price mpg lenght"
local listlocal "price mpg length"
*
* Summarize variables included in *listlocal* 
sum `listlocal'
*
* NOTE! Use ` ' to call a local.
* NOTE! Locals must be defined in the same block of code where they are used
//local veriables are erased when different blocks of code ar rune, while global ones can be called whenever in the same Stata session!
//if you have limited memory (eg. using a server) to minimize space you should use local vars more  
}

**# 1 - Loops in Stata ***

quietly {

* NOTE! Majority of loops made with Stata can be done with *for*.
//You can look over different objects, line numbers, numerical variables or stings
*
* Two for loops: foreach / forvalues
*for___ allows to loop in stata, different according to the object you are looping over
*
//NOTE the variables we are looping over are called within the loop as local variables, so in between ` '
*
* Looping over numbers /FORVALUES
forvalues i = 10(10)50 {
	di .01*`i'
}

* Looping over strings in a list /FOREACH X IN
foreach x in "Dr. Nick" "Dr. Hibbert" {
	display "`x'"
	display length( "`x'" )
}

* Looping over variables in a list
sysuse auto, clear
foreach x in mpg weight {
	sum `x'
}
* or
foreach x of varlist mpg weight {
	sum `x'	
}
* or
foreach x in $baseline {
	sum `x'
}

}

**#2 - Data visualization ***
 
quietly {

** Histograms and Kernels **

quietly {

* Import example dataset installed with Stata
*
clear all
*
sysuse lifeexp, clear
*
summ lexp, detail
*
* Plot an histogram
hist lexp 
hist lexp, bin(16)
*
* Plotting a non-parametric density distribution
*
* Non-parametric: 
//trying to fit the data/distribution in a function with a weight function approximating it
*
* KERNEL: weighting function. Divide data into intervals and weight observations.
//with kernel you are trying to fit the distribution of a bariable as if you were dealing with bins with a particluar weighting function attached
*
kdensity lexp
* No options: default width of bins is 1.8250.
*
* Same bandwidth as histogram with default bins:
kdensity lexp, width(3.125)
*
* Same bandwidth as histogram with 16 bins:
kdensity lexp, width(1.5625)
*
* NOTE!
* Bandwidth: "smoothing parameter"; weight given to distance between an observation and the remaining observations within a given window (a lower bandwidth means a larger weight on these distances).
//badnwidth is the weight of the bins as if you were looking at it from an hist perspective
//with hist you specify a series of nonoverlapping intervals of the same lenght and see the number of obs falling in each of those, with the density, the number of intervals can be overlapping and we are not trying to look at the density within each interval but smoothing it by weighting the distance between the observation of interest and all others within that window. The higher the smoothing parameter the smoother and interpretable the kernel density but the smaller the detail of the approximation of the distribution. Smaller smoothing parameter, the more detail about the distibution but harder to inetrpret
* 
* Histograms are a kind of kernel density estimate:
* - data are divided into nonoverlapping intervals;
* - counts are made of number of data points within each interval.
*
* Histograms are bar graphs that depict these frequency counts: 
* - the bar is centered at the midpoint of each interval;
* - its height reflects average number of data points in interval.
*
* Kernel density estimates:
* - range is divided into intervals;
* - estimates of density at center of intervals are produced;
* - independent of choice of origin of interval (i.e. location of bins in a histogram).
*
* Difference with histograms #1: intervals are allowed to overlap. 
* Difference with histograms #2: NOT merely counting number of observations in a window!
*
* Thus, a kernel density estimator assigns a weight between 0 and 1. This weight is
* based on distance from center of window, and sums those weighted values to arrive at a density. 
*
**# CONTINUING... OVERLAPPING PLOTS & other graphics
* Plotting a density distribution both discretely and continuously in same graph
twoway (hist lexp) (kdensity lexp)
twoway (hist lexp, bin(16)) (kdensity lexp, width(1.5625))
//default smoothing parameters
*
twoway (hist lexp, bin(32)) (kdensity lexp, width(.78125))
//more details, the smaller the smoothing parameter and higer the bins, thebetter we can understand if we have systematic missing values in our distribution
*
* Overlaying non-parametric and parametric densities
kdensity lexp, normal
hist lexp, normal
//comparing the distribution with how it would be if it were normally distribution

}

** Other types of graphics **

quietly {
	
* Plotting a box-and-whisker plot
graph box lexp
//95th, 75th, 50th 25th and 5th quantiles, + outliers
*
* Plotting a box-and-whisker plot of a single variable over several categories
graph box lexp, over(region)
*
* Plotting a scatterplot 
scatter lexp gnppc
*one var in fucntion of the other
*
* Plotting a scatterplot and fitting a linear regression in same graph 
graph twoway (scatter lexp gnppc) (lfit lexp gnppc)
//overlapping scatter of life exp and gnp per capita together with the linear fir of the two!!
*
* Plotting a scatterplot and fitting a quadratic regression in same graphic 
graph twoway (scatter lexp gnppc) (qfit lexp gnppc)
*
* Plotting a scatterplot and a local linear nonparametric regression
graph twoway (scatter lexp gnppc) (lpoly lexp gnppc, degree(1))
*
* Plotting a binned scatterplot! IMP
*
ssc install binscatter
binscatter lexp gnppc
//binning the variables and running a regression over the bins
* used to observe relationship between two variables, or visualizing OLS regressions;
* useful when working with large datasets (NOTE! Sensitive to number of bins!).
binscatter lexp gnppc, nquantiles(100)
//the smaller the number of bins the more you are forcing a relationship between the variables, important to test if there is a relationship using small number of quantiles to impose a linear relatinoship and see if it's there
//in research, whenever you have binscatter plots you will be asked if the relationship is robust in the number of quantiles
*
* Plotting a binned scatterplot with a quadratic fitted line
binscatter lexp gnppc, line(qfit)
*
* Want to know more?
* https://michaelstepner.com/binscatter/
* 
* NOTE! *scatter* and *binscatter* are important to probe the linearity assumption of your regression specification. Use them to choose across different linear specifications!
*
* Plotting different graphics in same figure, not overlaying them (side by side)
//plot individually and saves them, then combine them
hist lexp, saving(histogram)
kdensity lexp, saving(density)
graph combine histogram.gph density.gph
*
* Exporting created graph into a (non-Stata) readable format
graph export combined_histkdens.png, replace
//must not close the previous graph before running this, or save it, other wise it will not find it 
graph export combined_histkdens.pdf
//better to use png because pdf are heavy

}

**#Stata graphics formatting ** 
***colour schemes and fonts!

quietly {

* NOTE! In the previous graphs we have been using Stata's default parameters for things such as the y-axis label, the font of the graph, so on and so forth.
*
* Still, you have control over these parameters and I would suggest you to personalize these for your thesis and/or future work, to eliminate unecessary clutter and homogenize your graphs with your Latex and/or Markdown files.
*
* To start, there are different default Stata plotting schemes from which you
* can choose from (on how you would like to have your plot shown):
*
* Here is how you set up a plotting scheme in Stata:
set scheme s1color
*this one removes gridlines and creates white sourrounding around the graph
*
* Here you can find other plotting schemes from Stata:
* http://people.umass.edu/biostat690c/pdf/stata%20schemes%20and%20palettes.pdf
*
* And if you want to use a scheme different from these provided by Stata,
* Asjad Naqvi went ahead and coded a package with a series of different schemes
* done by himself:
*
* https://github.com/asjadnaqvi/Stata-schemes
*
* Now, besides these schemes, there are different things that you can alter by
* yourself! For instance, you can increase the gap between the title of 
* the y and x-axis and its respective tick labels. See below!
*
hist lexp /* Default */
hist lexp, yscale(titlegap(*15)) xscale(titlegap(*15)) /* Personalized - now the titles of the axis are no longer annoyingly close to the numbers and axis*/
*
*LATEX FONT
* You can also print your graphs with the default Latex font:
*
* 1. Download and install the Latin Modern Roman font on your computer 
* ... I have left instructions on how to do this on Blackboard.
*
* 2. Change the default font used by Stata for its graphs:
*
graph set window fontface "LM Roman 10"
*
hist lexp, ///
yscale(titlegap(*15)) ///
xscale(titlegap(*15)) ///
subtitle("With Latex Font", margin(b=2))
*
graph set window fontface "@Malgun Gothic"
//restoring original stata font
*
* For more on fonts and other formatting tips, see:
*
* Michael Stepner's guide on how to create effective figures on Stata:
* https://twitter.com/michaelstepner/status/1201621569736445957
*
* Asjad Naqvi's guide on how to personalize the font of your Stata plots:
* https://medium.com/the-stata-guide/stata-graphs-get-those-fonts-right-c38d35625142
*
*
* TASK! Create a single graphic with the *density plot* of the variable *lexp* for countries below and above the median of per capita income [two separate lines]. 
* While doing so, choose titles that clearly convey what the graphic is about and legends that make the content clear for the reader. 
*
sum gnppc, d
return list
*
scalar median_gnppc=r(p50)
gen dum_above_median = (gnppc >= median_gnppc)
*
twoway ///
(kdensity lexp if dum_above_median==0) ///
(kdensity lexp if dum_above_median==1), ///
legend(label(1 "Below median p.c. GNP") label(2 "Above median p.c. GNP")) ///
xtitle("Life expectancy at birth") ytitle("Density of life expectancy at birth") ///
xscale(titlegap(*15)) yscale(titlegap(*15)) ///
title("Life Expectanty above and below Median Income", margin(b=4))
*
* TASK (AT HOME)!
*
* I leave you below a series of graphics using an example that I assembled about the Democratic primaries for the U.S. presidential election of 2020.
* Read the *help* files of each plot and experiment with the different options available, to gather experience on customizing your own graphs!
*
use calidems_2020_example, replace
des
*
twoway connect biden days, /// GRAPH 1
lw(thick) lp(dash) yscale(titlegap(*15)) xscale(titlegap(*15)) /// makes line thick, dashed, titles far from axis
msize(medsmall) ///
mlabel(biden) mlabposition(12) mlabsize(3) mlabcolor(black)
*
*
twoway connect sanders days, /// GRAPH 2
lw(thick) yscale(titlegap(*15)) xscale(titlegap(*15)) ///
msize(medsmall) ///
mlabel(sanders) mlabposition(12) mlabsize(3) mlabcolor(black)
*
label variable biden "Voting intentions for Biden"
label variable sanders "Voting intentions for Sanders"
*
twoway ///
///
(connect biden days, /// GRAPH 1
lw(thick) lp(dash) /// 
mlabel(biden) mlabsize(3) msize(medsmall) mlabcolor(black) mlabp(12)) ///
///
(connect sanders days, /// GRAPH 2
lw(thick) ///
mlabel(sanders) mlabsize(3) msize(medsmall) mlabcolor(black) mlabp(6)), ///
/// COMMON FORMATTING OPTIONS
xlab(1 "Aug 05" 41 "Sep 15" 72 "Oct 16" 109 "Nov 22" 164 "Jan 16" 195 "Fev 16", ///
tlength(0)) ylab(none, tlength(0)) ///
xtitle("") ytitle("Voting Intentions in p.p.") ///
yscale(titlegap(*15) r(15 35)) xscale(titlegap(*15)) ///
title("California's Democratic Primary Polls (Biden vs. Sanders)", margin(b=4)) ///
xline(164, lcolor(black) lw(medium) lp(dot)) /// 
xline(195, lcolor(black) lw(medium) lp(dot)) ///
note("Data: SurveyUSA California polls concerning the Democratic primaries. Source: FiveFortyEight.") 

}

}

**# 3 - Regressions ***
*
** TIP! USEFUL FOR EVERY QUESTION IN PS1;
** INCLUDES SPECIFIC TIP FOR QUESTION 1.D.

quietly {

** OLS regressions **

quietly {

* Import an example dataframe downloaded from Stata's repository
webuse womenwk.dta, clear
*
* Describe dataframe
describe
summarize
*
* Regress *wage* on *age* while computing standard errors robust to outliers
help reg
*
reg wage age, vce(rob)
//estimate beta of wage as a linear function of age allowing for heteroskedasticity
*
scatter wage age //can't really see much
binscatter wage age //fit linear reg
binscatter wage age, nquantiles (100) //robust in n of bins
*
* NOTE! *binscatter* provides an elegant form of probing your linear fit.

}

** Post-estimation commands **

quietly {
	
* Examine post-estimate content
reg wage age, vce(rob)
ereturn list
return list
//keeps in memory all these relevant estimates in the r(table)
*
* Save estimates in a matrix
matrix table = r(table)
matrix list table
*
* Predict residuals 
help regress postestimation //many post estimation commands available on Stata
*
reg wage age, vce(rob)
predict e, res //predict can either predict regression residuals or the linear fit (predicted values for the model), res specifies you want the residuals
summ e
*
* NOTE! Predict creates a new variable containing predictions such
* as linear predictions, residuals, standardized residuals, etc.
*
* Predict fitted values (linear predictions)
predict y_hat, xb //xb for fitted values
summ wage y_hat
*
* Test simple hypothesis about significance
reg wage education, vce(rob)
test education //test significance!
*
* test: Wald tests of simple and composite linear hypotheses about parameters of most recently fit model. Can also specify precise hypotheses:
test education==0
test education==1
test education==1.1
*
* Wald test of equality of coefficients 
//test if some coeffs are equal to one another
reg wage education married children county, vce(rob)
test education==county
*
* Joint test of statistical significance
test education county children
*
* Run same regression in two subgroups: those married and those not married
//regression conditioning between subgroups
bysort married: reg wage education, vce(rob)
* ...same as these two regressions:
reg wage education if married==1, vce(rob)
reg wage education if married==0, vce(rob)
* Note... We could have a third category, i.e. those for which married==.

}

** Standardized coefficients **

quietly {

* You can have access a regression's standardized coefficient as follows:
reg wage education, beta
//standardize means coefficients expressed in terms of number of standard deviations. 
// eg normal reg. tells me how an extra yr of education increases horly wage by one dollar
//with beta option: one std dev in edu translates into half increase in std dev in hourly wage
*
* Problem, beta coeff is NOT it stored!
//not stored, see
ereturn list
return list
matrix coeffs_std= r(table)
matrix list coeffs_std
* Not among post-estimate contents...
*
* Remember how we did it last session?
*
reg wage education
matrix coeffs= r(table)
matrix list coeffs
//To have the stds coefficient we must compute it it
//first get the coeff of the regression and save it in a scalar
scalar beta_edu=coeffs[1,1]
scalar list beta_edu
*
//then we save the standard deviations for the dependent variable and covariate from the descriptive statistics of the summary
sum wage
scalar sigma_wage = r(sd)
sum education
scalar sigma_edu = r(sd)
*
//no compute the standardized beta!
scalar beta_sd = (beta_edu*sigma_edu)/sigma_wage
scalar list beta_sd
* check....
reg wage education, beta
//some discrepancy from approximations
*
* Another alternative:
reg wage education, beta
* NOTE! *reg, beta* displays *standardized* coefficients but does not provide these as a post-estimate content. 
*
* TIP! Google "how to standardize variables in Stata" and read the forum post
* https://stats.idre.ucla.edu/stata/faq/how-do-i-standardize-variables-in-stata/
*
egen std_wage = std(wage)
egen std_educ = std(education)
//egen whenever you want to generate a variable using a Stata-specific function!
*
//then, regressing standardized variables directly produced the standardized coefficient and it can be stored immediatly from the table of the regression
reg std_wage std_educ
matrix table = r(table) 
matrix list table
*
scalar beta_sd_educ = table[1,1]
scalar list beta_sd_educ
*
reg wage education, beta
*
drop std_wage std_educ

}

**#OUTLIERS
** Identifying influential observations **
*
*(TIP! USE FOR QUESTION 1.D.)

quietly {
	
reg wage education
dfbeta, stub(dfbeta1)
*
* dfbeta: Focuses on one coefficient and it measures difference between the regression coefficient when i-th observation is included and excluded, with the difference being scaled by estimated standard error of coefficient.
//answers to how influential is one observation for the coeff we are estimating? And does so for each observation
hist dfbeta1
//mass of observations by their influence on the coefficient
*
* Rule of thumb: Belsley, Kuh, and Welsch (1980) suggest to look closely at observations where dfbeta is greater than 2 divided by square root of N (total number of observations).
//all obs with influence parametner higher than that value should be discussed and pointed out
*
reg wage education
*
ereturn list
return list
*
scalar N = e(N)
scalar list N
*
display 2/sqrt(N)
*
scalar threshold=2/sqrt(N)
scalar list threshold
*
list wage education dfbeta1 if abs(dfbeta1) > threshold & wage!= . & education!=.
//listing and looking at observations that are particluatly influencial in dictating the value of the coefficient
*
//visualising it:
local t_max = threshold
local t_min = -threshold
hist dfbeta1, xline(`t_max') xline(`t_min')
*
* More info on dfbeta here: https://www.techtips.surveydesign.com.au/post/the-dfbeta-command
*
* "predict" also calculates DFBETAs, but can do this for only one variable 
* at a time. Type "help regress postestimation" to see which are your
* postestimation tools after regress

}

**#Logit regressions **

quietly {
	
logit married education, vce(rob)
return list
ereturn list
* used in binary outcome models
*
* NOTE! *logit* has a similar syntax to *reg* and offers more post-estimate content and options
*
* TASK (AT HOME)! 
*
* - Install ado *dummies*. 
* - Generate a dummy for each education category in a line of code.
* - Use a loop to rename the variables. 
* - Regress wage on the list of education categories you created. 
* - Use a loop to compute average of wage for each education category.	
*
* - Install ado *dummies*.
ssc install dummies
*
* - Generate a dummy for each education category in a line of code.
dummies education
summ education* //sum all vars starting with "education"
*
* - Use a loop to rename the variables. 
forvalues i=1/4 {
	rename education`i' educ_cat_`i'
}
*
* - Regress wage on the list of education categories you created. 
reg wage educ_cat_1-educ_cat_4, noc rob
*
* - Use a loop to compute average of wage for each education category.	
forvalues i=1/4 {
	sum wage if educ_cat_`i'==1
}

}

}

**#OUTPUT TABLES AND PLOTS
*** 4 - Output tables/plots ***
*
** TIP! USEFUL FOR QUESTIONS 1.c, 2.e and 3.a.

quietly {

* Install *outreg2*, a command to output regression/summary/tabulation tables
ssc install outreg2
help outreg2
//outreg2 function to output results in a table in diffrent formats
//helpoutreg2 very extensive support
* NOTE! Through *outreg2* one can output tables into text, Excel and Latex.
*
* For the problem set, as it will be indicated in the PDF, 
* you should output tables into Excel files.
*
* Import dataset installed with Stata
sysuse auto, replace
*
* Regress using local objects 
local x_1 "foreign"
local x_2 "weight headroom trunk" 
local x_3 "length turn displacement" 
reg mpg `x_1'
reg mpg `x_1' `x_2' 
reg mpg `x_1' `x_2' `x_3' 
*
* Export using local objects 
local x_1 "foreign"
reg mpg `x_1'
outreg2 using table.xls, excel replace
* remember that file will be saved in your current directory
* to check directory, type "pwd"
* to change directory, type "cd "C:path\to\folder""
*
local x_1 "foreign"
local x_2 "weight headroom trunk" 
reg mpg `x_1' `x_2'
outreg2 using table.xls, excel append
//add new result output to exsiting file!
*
local x_1 "foreign"
local x_2 "weight headroom trunk" 
local x_3 "length turn displacement" 
reg mpg `x_1' `x_2' `x_3' 
outreg2 using table.xls, excel append
*
* NOTE! *replace* starts a new table with the same name;
* 		*append* adds a new column to a existing table with that name.
*
* Export with global objects 
global x_1 "foreign"
global x_2 "weight headroom trunk" 
global x_3 "length turn displacement" 

* NOTE! You can use *estimates store* to avoid repeating *outreg2* after each
* regression and keep track of which specification is in each line.
*
reg mpg $x_1
estimates store reg1

reg mpg $x_1 $x_2
estimates store reg2

reg mpg $x_1 $x_2 $x_3
estimates store reg3

outreg2 [reg1 reg2 reg3] using table2.xls, excel replace
*
*
*
reg mpg $x_1 $x_2 $x_3
*
* Lastly, you can also easily plot your coefficients in a single graph:
*
ssc install coefplot
*
coefplot, drop(_cons) saving(coeff_plot)
//post estimation command
//drop _cons to not plot it, then saving the file
graph export coeff_plot, as(pdf)
graph export coeff_plot.png
*
* NOTE! "coefplot" is highly customizable!

}

**#BALANCE CHECK TABLES
*** 5 - Balance check table ***
*
** TIP! USEFUL FOR QUESTIONS 1.a, 2.a and 2.d.

quietly {

* NOTE! Here we opt for a flexible solution, but easier and more 
* efficient solutions are likely available to Stata users.
*
* Import a dataset installed with Stata...
sysuse auto, replace
*

**** METHOD 1: Create a matrix
*
* IMPORTANT: need to create a matrix with right number of columns.
*
* NOTE! 
* In a matrix you will input numbers (in Method #2 you might have strings also).

matrix balcheck=(.,.,.,.) //dimension 4
*
* Fill in first row and then append following ones (i.e. other variables)
* 
local i=1 //initiate var for looping
*
foreach var of varlist price mpg rep78 headroom {

	qui sum `var', d
	
	matrix balcheck[`i',1]=r(N)
	matrix balcheck[`i',2]=r(mean)
	matrix balcheck[`i',3]=r(sd) 
	matrix balcheck[`i',4]=r(p50)
	
	local i=`i'+1 
	
	if `i'<=4 matrix balcheck=(balcheck \ .,.,.,.) 
	
}
* You can also round values to one or two decimals (we'll see this in a minute).
*
matrix rownames balcheck=price mpg rep78 headroom
matrix colnames balcheck=N Mean StDev Median
*
matrix list balcheck
*
* Export to Excel:
*
putexcel set "my_balance_matrix.xlsx", sheet("Balance_Matrix") replace
putexcel A2="price" A3="mpg" A4="rep78" A5="headroom"
putexcel B1="N" C1="Mean" D1="StdDev" E1="Median"
putexcel B2=matrix(balcheck)
*
*
**** METHOD 2: Create variables in .dta
*
* WARNING: Beware that you are modifying your dataset!
* Make sure you don't have conflicts in terms of names of variables, etc.
*
* NOTE! We will generate a variable in Stata's dataframe for  each column of
* our table, compute the statistics we want in that table, and fill each line
* with the desired quantity.
*
* Generate upper left corner of the balance check table...
gen variable=""
*
* For each descriptive statistic that we want to include in our balance check:
* Generate respective column in our table...
foreach var of newlist N mean sd median {
	gen `var'=.	
}
*
*Then, complete each row for each column with the desired scalar:
*
local i=1
*
foreach var of varlist price mpg rep78 headroom {

	numlist "1/4"
	local myvar: word `i' of `r(numlist)'
	replace var="`var'" if _n==`myvar'
	
	qui sum `var', d
	
	replace N=r(N) if _n==`myvar' 
	replace mean=r(mean) if _n==`myvar' 
	replace sd=r(sd) if _n==`myvar' 
	replace median=r(p50) if _n==`myvar'

	foreach var of varlist N mean sd median { 

		replace `var'=round(`var', 0.01)

	}
	
	local i=`i'+1 
}

outsheet variable N mean sd median using TABLE3.xls, replace
//specify variable you want to output with outsheet! Previous code created new elements in the datasets as variables containing the descriptive stats of interest, now we output those cells
}

**#Inference in randomized experiments and Neyman inference with simulated superpopulation
*** 6 - Inference in randomized experiments ***
*
** TIP! USEFUL FOR QUESTION 5

quietly {
	
* In question 5 you are asked to implement a randomization-based inference over
* LaLonde's experimental dataset. This type of inference is conceptually diffe-
* rent from Neyman's inference, still, to implement it you can find motivation
* on Professor Le Barbanchon's in-class example of Neyman's inference.
*
* You can go through that example in 2 different ways in Stata: (1) through
* matrices or (2) through a new Stata 17 command titled "frame":
*
**** METHOD 1: Through matrices (Professor Le Barbanchon's in-class example)
*
* See do-file uploaded on Blackboard by Professor Le Barbanchon.
*
**** METHOD 2: Through a new Stata 17 command titled "frame"
*
quietly {
*
clear all
set more off
set seed 1123
*
* NOTE! "frame" allows you to use more than 1 dataframe in simultaneous!
*
* Starting then by our super population dataframe:
//frame allows two save results into a separate dataframe! To work with different dataframes symultaneously
frame rename default super_population
*
frame super_population {
	*
	set obs 500000
	*
	gen Y_0=rnormal(0)
	label var Y_0 "Potential outcome when control"
	*
	gen Y_1=rnormal(1)
	label var Y_1 "Potential outcome when treated"
	*
	gen T=rbinomial(1,0.5)
	label var T "Treatment status"
	*
	gen Y=Y_0*(1-T)+Y_1*T
	label var Y "Realized outcome"
	*
	gen tau_i=Y_1-Y_0
	sum tau_i, d
	*
	reg Y T
	*
}
*
* Going then for our random sampling routine
*
frame create fs_ates
*
frame fs_ates {
	*
	set obs 500
	*
	gen coef = 0
	*
}
*
frame super_population {
	*
	* NOTE! For a loop to display a progress bar:
	* https://acarril.github.io/posts/progess-bar
	*
	_dots 0, title(Loop running) reps(75)
	*
	forvalue i=1(1)500 {
		*
		quietly {
			*
			cap drop select sample
			*
			gen select = runiform()
			*
			gen sample = inrange(select,0,1000/500000)
			*
			reg Y T if sample
			*
			frame fs_ates: replace coef = _b[T] if _n==`i'
			*
		}
		*
		_dots `i' 0
		*
	}
	*
}
*
frame fs_ates {
	*
	sum coef, d
	*
	hist coef
	*
}
*
frame super_population {
	*
	cap drop sample
	*
	gen sample = inrange(_n,1,1000)
	*
	reg Y T if sample
	*
	scalar se = _se[T]
	*
	scalar lb = 1 - 1.96*se
	scalar ub = 1 + 1.96*se
	*
}
*
frame fs_ates {
	*
	local lb = lb
	local ub = ub
	*
	hist coef, xline(`lb' `ub') start(.8) width(.02)
}
*
}