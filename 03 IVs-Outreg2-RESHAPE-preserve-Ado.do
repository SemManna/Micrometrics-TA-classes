** Microeconometrics 20295, TA 3, Tips for Problem Set 2
** Prof: Thomas Le Barbanchon
** TA: Jaime Marques Pereira
** Partial credits to Francesca Garbin and Alexandros Cavgias

* Objectives for the class:
* 0 - Revisiting *outreg2* - output information into excel or latek;
* 1 - Introduction to *ivregress2* - estimate 2SLS on Stata for IV;
* 2 - Exporting useful output to a single table;
* 3 - Introduction to *reshape* - wide vs long dataframes;
* 4 - Computing averages from microdata;
* 5 - Introduction to *preserve* and *restore*;
* 6 - *program*, .ado files, *mata* and coding efficiently in Stata.


********************************************************************************

**# 0 - Revisiting *outreg2* ***
*
** TIP! USEFUL FOR QUESTIONS 2.D, 2.F, 3.A, 3.B and 3.D OF EXERCISE 1.
quietly {

* Install *outreg2*, a command to output regression/summary/tabulation tables
ssc install outreg2, replace
help outreg2 //very exaustive help vignette
*
* Import dataset installed with Stata
sysuse auto, replace
des
*
* Generate global variables with alternative sets of independent variables
global x_1 "foreign"
global x_2 "weight headroom trunk" 
global x_3 "length turn displacement" 
*
** Showing only main coefficient **
* 
* NOTE! Use option *keep* from *outreg2*.
*KEEP 
//if you only want to output the coefficents of a particluar set of covariates, use the keep option! The others are not reported in the excel
reg mpg $x_1
outreg2 using table.xls, excel replace keep(foreign) nocons 
reg mpg $x_1 $x_2
outreg2 using table.xls, excel append keep(foreign) nocons
reg mpg $x_1 $x_2 $x_3
outreg2 using table.xls, excel append keep(foreign) nocons
*
* NOTE! Useful but one cannot understand which model is in each column.
** Adding COMMENTS ** 
*
* NOTE! Use option *addtext* from *outreg2*.
*ADDTEXT
//we want to point out in our table, which set of controls is used
* Controls 1 ($x_2): weight headroom trunk
* Controls 2 ($x_3): length turn displacement
reg mpg $x_1
outreg2 using table.xls, excel replace keep(foreign) nocons addtext(Controls 1, NO, Controls 2, NO) //replace option here to overwrite the existing table
reg mpg $x_1 $x_2
outreg2 using table.xls, excel append keep(foreign) nocons addtext(Controls 1, YES, Controls 2, NO) //no need to specify the text as a string " "
reg mpg $x_1 $x_2 $x_3 
outreg2 using table.xls, excel append keep(foreign) nocons addtext(Controls 1, YES, Controls 2, YES)
*
** Adding STATISTICS **
* NOTE! Use option *addstat* from *outreg2*. 
*ADDSTAT
//here we add a simple statistics which does not change, what we could do is also to add a different statistic for each regression. To add a variable or number the command requires a string for the title " " and then the name of the statistics as stored
sum mpg
scalar mean_y = r(mean) 
*
reg mpg $x_1
outreg2 using table.xls, excel replace keep(foreign) nocons addtext(Controls 1, NO, Controls 2, NO) addstat("Outcome mean", mean_y)
reg mpg $x_1 $x_2
outreg2 using table.xls, excel append keep(foreign) nocons addtext(Controls 1, YES, Controls 2, NO) addstat("Outcome mean", mean_y) 
reg mpg $x_1 $x_2 $x_3 
outreg2 using table.xls, excel append keep(foreign) nocons addtext(Controls 1, YES, Controls 2, YES) addstat("Outcome mean", mean_y) 

}


**#1 - Introduction to *ivreg2* *** Estimate 2SLS regressions
*
** TIP! USEFUL FOR QUESTIONS 2 AND (IN PARTICULAR) 3 OF EXERCISE 1.
quietly {
	
* NOTE! While using 2SLS, you will recurrently have to use *ivreg2*. .
*
** Using *ivreg2* **
*
* Install *ivreg2*
ssc install ivreg2, replace
*
* Import dataset installed with Stata
webuse union3, clear 
*
* Describe dataframe
describe
*
* TASK! We want to estimate the effect of being an union member on wages.
*
* Reasons for union membership being endogeneous to wages? One can think for example of simultaneity - workers with larger wages are expected to not be enrolled in a labor union; still, union membership is expected to have a positive effect on wages.
*
//then we may want to estimate the impact of union membership on wages through an instrumental variable
*
* Assume that *black south age* are valid instruments (//hence in this setting they are 1) relevant - actually explain the endogenous covariate, union memebrship - 2) exclusion restriction - they affact the dependent variable only through union memebrship and not through other channels which we are not controlling).
* Estimate the effect of *union* on *wages* using *black south age* as instruments for *union* and *wks_work ttl_exp not_smsa grade* as controls. 
*
ivreg2 wage (union= black south age) wks_work ttl_exp not_smsa grade, robust
//syntax similar to a normal regression, dependent variable first but then the variable you is being instrumented must be inserted between round brakets with the instruments after the equal sign
// with help ivreg2 you can see all the options available
*
* NOTE! Outcome -> *wages*; Endogenous regressor -> *union*; Instruments -> *black south age*; Controls -> *wks_work ttl_exp not_smsa grade*.
*
* Output post-estimate statistics
ssc install ranktest
return list
ereturn list
*
* NOTE! More post-estimation statistics than *regress*. To learn more, read *ivreg2*'s Stata Journal article for details. Have special attention considering difference between *underidentification* and *weak identification* tests.
*
** Useful options with ivreg2**
*FIRST
* Use *first* to visualize the first-stage output.
ivreg2 wage (union= black south age) wks_work ttl_exp not_smsa grade, robust 
ivreg2 wage (union= black south age) wks_work ttl_exp not_smsa grade, robust first 
//first option allows to see, other than the coefficents for the second stage, everything we need from the first stage such as the F stat, yet we do not have access to those statistics in post-estimation commands, only second stage post-estimate commands are available > use SAVEFIRST
*
* Use *savefirst* to save statistics related with first-stage.
*SAVEFIRST
ivreg2 wage (union= black south age) wks_work ttl_exp not_smsa grade, robust first savefirst
return list
ereturn list
//savefirst tells stata to store the first stage statistics other than just saving them, but to use them we also need to use EST RESTORE
*
* NOTE! Exporting first-stage results can be hard. To do so, use *savefirst* before *estimates restore*. To restore first-stage, type "_ivreg2_"+"end. var.".
*RESTORE
//running 2SLS routine but outputting the first stage
ivreg2 wage (union= black south age) wks_work ttl_exp not_smsa grade, robust first savefirst 
est restore _ivreg2_union
outreg2 using table.xls, excel replace keep(black south age) nocons addtext(Controls, YES) 
*
* Use *partial()* to visualize contained tables while using a large number of FEs.
//eg when controlling for fixed-effects information we add year-dummis to our regression, but may not be interested in their coefficients
*PARTIAL
ssc install dummies
tab age
dummies age
ivreg2 wage (union= black south age) wks_work ttl_exp not_smsa grade age1-age13, robust //hard to read, use partial
*
ivreg2 wage (union= black south age) wks_work ttl_exp not_smsa grade age1-age13, robust partial(age1-age13) //better!
*
*
* NOTE! On a reduced form model. To compute a reduced form model, regress the outcome directly on instruments using OLS and the same controls and variance specification of the IV model.
*
reg wage black south age wks_work ttl_exp not_smsa grade, robust
*
* NOTE! Restrict the reduced form model to the same set of observations than that used with *ivreg2*.  --> make sure to exclude missing values!
reg wage black south age wks_work ttl_exp not_smsa grade if union!=., robust
*
*
*
** IV regression "by hand":
*
* First stage:
reg union black south age wks_work ttl_exp not_smsa grade, robust
//estimate the first stage
* Predict the endogenous variable:
predict double unionhat 
//predict endogenous variable (union memebrship) from the first stage
*
* Next regression does not work because there are some missing values in union but these missing get predicted through unionhat!
reg wage unionhat wks_work ttl_exp not_smsa grade, robust
*
* This one replicates results (coefficients!) from ivreg2:
reg wage unionhat wks_work ttl_exp not_smsa grade if union!=., robust
//restrict yourself to the sample where the endogenous covariate does not have missing values
}


**# 2 - Exporting useful output to a single table *** 
*
** TIP! USEFUL FOR QUESTIONS 2 AND (IN PARTICULAR) 3 OF EXERCISE 1.
quietly {
	
* NOTE! Imagine that you want to have a table where you go through your whole IV routine and you add important information about EACH STEP as you go along estimating regressions.
*
* Estimate the outcome mean.
sum wage
scalar mean_y = r(mean) 
*
* Start a table with OLS so that the reader understands the OLS bias.
reg wage union wks_work ttl_exp not_smsa grade, robust
outreg2 using table.xls, excel replace keep(union) nocons addtext(Controls, YES, Reg, OLS) addstat("Outcome mean", mean_y) //adding also the outcome mean statistic to the table with the OLS
*
* Estimate the FIRST STAGE to argue for relevance.
//Using FIRST and SAVEFIRST
ivreg2 wage (union= black south age) wks_work ttl_exp not_smsa grade, robust first savefirst
scalar F_weak = e(widstat) //creating scalar with the F stat from the IVreg
est restore _ivreg2_union //restore first stage
outreg2 using table.xls, excel append keep(black south age) nocons addtext(Controls, YES, Reg, First Stage) addstat("F-statistic instruments", F_weak) //output the first stage!
*
* Estimate a REDUCED FORM model, to argue for validity and hint about the sign and magnitude of IV estimates.
//show reduced form regression for sake of completness, remember to exclude missing values!
reg wage black south age wks_work ttl_exp not_smsa grade if union!=.
outreg2 using table.xls, excel append keep(black south age) nocons addtext(Controls, YES, Reg, Reduced Form) 
*
* Estimate the SECOND STAGE. 
//No est restore here, we want to get the second stage and finally look at the IV regression
ivreg2 wage (union= black south age) wks_work ttl_exp not_smsa grade age1-age13, robust
outreg2 using table.xls, excel append keep(union) nocons addtext(Controls, YES, Reg, IV) 

//note, the table has four rows, one for union and 3 for the 3 instruments, first column the biased OLS, then the relevance of the instrument where the dependent variable is the endogenous covariate (union memebrship) with an F-stat above the rule of thumb of 10 - relevant IVs. Then we try to show that the excluion restriction holds (the effects in the reduced form should be aligned with the reduced form should be aligned with the secnd stage, if they are not we may have heterogeneity and other issues impacting the interpretation of the IV estimate) and lastly we estimate the IV regression
}


**#3 - Introduction to *reshape* ***
*long to wide dataframes and vice-versa
** TIP! IMPORTANT TO UNDERSTAND DO-FILE ASSOCIATED TO EXERCISE 2.
quietly {

** Reshaping dataframes  **

* NOTE! Dataframes can be formatted as *long* or *wide* datasets.
*
* An example for a *long* dataframe is a panel data set with cross sectional and temporal units.
*
* An example for a *wide* dataframe is a cross sectional data set with different lagged variables.

//two main forms of dataset in stata, 
*LONG - corss-sectional and temporal unit are both organized vertically (eg. on the columns we have first obs for unit 1, second obs for unit 1... T^th obs for unit 1, first obs for unit 2, second obs for unit 2... - OR could also be the opposite, first the temporal then the cross sectional such as first obs unit 1, first obs unit 2 ..first obs unit N, second obs unit 1....) - and WIDE - you have individuals on the vertical dimension and on the horizontal one a variable per each dimension, indexed by the year (eg. Italy / GDP2000 GDP2001 GDP2002 ecc)
*WIDE format is particulary useful for matrix formulas (eg. averages over units or years) but normally dataset are grouped over LONG datasets
*
* Import an example dataset
* CONTEXT! Here we will use electoral outcomes in the U.S. presidential elections, focused on popular vote by state, from 2000 to 2016. 
use example_reshape.dta, clear
*
* Describe dataframe
describe
* Examine dataframe format
*
* Press the results window and type CTRL+8 (See Window > Data Editor) or type the commands "browse" or "edit" (depending on the Data Editor mode you want)
br
//we have years in the first column and state (i) in the second one
* Examine temporal dimension
tab year
list if state=="AL" 
*
* Read the *help* file for *reshape*
help reshape
*
* QUESTION! What is the format of the current dataframe?
* ANSWER! The current dataframe is organized as a panel, with 51 cross-sectional units and 5 temporal units.
*
*
* Converting data from a *long* to a *wide* format
reshape wide rep_popular_vote dem_popular_vote others_popular_vote total_popular_vote, i(state) j(year)
//reshaping EVERY variable OTHER THAN the cross sectional and temporal variables and then specifying what do we want out cross sectional variabl to be and out temporal one to be (bear in mind, they can be switched!)
list if state=="AL"
*
* NOTE! We now have popular vote variables for each different election. In a *wide* format we are able to easily compute AVERAGES for different sets of elections. Hence, through *reshape* we can alter our data frames to ease average computation and other tasks.
*
*easily computing average republican popular vote for the first decade of this century and then 2012 and 2016, for each State
gen rep_popvote_avg_dec_00 = .333*(rep_popular_vote2000 + rep_popular_vote2004 + rep_popular_vote2008)
gen rep_popvote_avg_dec_10 = .5*(rep_popular_vote2012 + rep_popular_vote2016)
*
summ rep_popvote_avg_dec_00 rep_popvote_avg_dec_10
*
*
* Re-converting data from a *wide* to a *long* format
reshape long rep_popular_vote dem_popular_vote others_popular_vote total_popular_vote, i(state) j(year)
//restoring long format, computed averages are 'taken out' and replicated, within each state for each year! 
}


**# 4 - Computing averages from microdata ***
*
** TIP! IMPORTANT TO UNDERSTAND DO-FILE ASSOCIATED TO EXERCISE 2.
quietly {

* NOTE! It is not common to have publicly available descriptive statistics for your *unit* of analysis. 
*Changing *unit* is instead common in micro-econometric studies. It is thus important to understand how can a researcher rapidly compute descriptive statistics for multiple units of analysis.
*
** Switching *unit of analysis **
*
* Import an example dataset
* CONTEXT! Here we will use data from a 1991 survey on literacy and educational outcomes, held in different cities and covering close to 1.3M individuals. Best example of microdata
use example_averages, replace
*
* Describe dataframe
describe
*
* EXAMPLE! While using panel data, with city-level data variation (according to the 1991 census definition), you are required to compare statistics with recent city-level census definitions. These are different and are not comparable. If you are to accurately compare statistics across different definitions, then, you are required to change your *unit* of analysis while computing statistics of interest. 
//we have informatino about individuals which depends on a geographical unit changing over time. We want to compute averages within geographical units
*
*
* Install *ado* which allows you to generate weighted averages!
ssc inst _gwtmean, replace 
help gwtmean
*
* NOTE! Census observations have *sample weights*. These must be accounted for when computing city-level averages. Hence installing *wtmean*.
*
* TASK! Construct a city-level data set with the percentage of illiterate people and average years of education using the 1991 city census definition. 
*Construct an identical data set while using the 2010 city census definition.
*
* 1991's census definition
use example_averages, replace
egen perc_illiterate_def_1 = wtmean(illiterate), by(city_id_1991) weight(weight_person) 
//gen a var using a the weighted mean function for the mean of illiterate by city (according to 1991 id) and using our weights. Since the var is a dummy, the avg is a proportion
egen avg_educ_def_1 = wtmean(school_years), by(city_id_1991) weight(weight_person)
duplicates drop city_id_1991, force 
//now we have city-level averages duplicated for each individual in that city. We only want to keep city-level data, so we drop individual obs via dropping al duplicates of city id to have only city level averages.
sum perc_illiterate_def_1 avg_educ_def_1
save data_city_id_1991, replace
//saving this modified sataframe
*
* 2010's census definition
//same but using the 2010 census definition
use example_averages, replace
egen perc_illiterate_def_2 = wtmean(illiterate), by(city_id_2010) weight(weight_person)
egen avg_educ_def_2 = wtmean(school_years), by(city_id_2010) weight(weight_person)
duplicates drop city_id_2010, force
sum perc_illiterate_def_2 avg_educ_def_2
save data_city_id_2010, replace
*
* QUESTION! Are you able to perform these tasks without opening the data frame twice?
* ANSWER! Yes, through *preserve* and *restore*. We will see in a few minutes. (//collpase seems much more concise for simple operations)
*
//we could compute these means, alterig our dataframe, restoring the original dataframe with individual observatinos at God's speed. Reloading large datasets could take a lot of time when using vast administrative dataframes
* NOTE! We can perform the same task easily through one Stata command - *collapse*.
*
* 1991's census definition
use example_averages, replace
help collapse
collapse (mean) illiterate school_years [pw=weight_person], by(city_id_1991)
//collpase dataframe to the 68 observations of the different cities with the mean for the variables of interest weighted according to the weight variable within the city-unit specified with the by option
*
//note, here we did not create a new variable for the mean function in the dataframe! The means are under the variables themselves in this modified dataframe, we need to rename them!
*
rename illiterate perc_illiterate_def_1
rename school_years avg_educ_def_1
sum perc_illiterate_def_1 avg_educ_def_1
save data_city_id_1991, replace
//again, adding preserve (at the beginning) and restore (here) we could run all this, quickly getting back to the original dataframe. In this case we would not even need to rename the variables.

* 2010's census definition
use example_averages, replace
collapse (mean) illiterate school_years [pw=weight_person], by(city_id_2010)
*
rename illiterate perc_illiterate_def_2
rename school_years avg_educ_def_2
sum perc_illiterate_def_2 avg_educ_def_2
save data_city_id_2010, replace
*
** Computing averages from microdata **
*
* TASK! Compute descriptive statistics for a subpopulation of interest. In particular, compute which proportion of children between 6 and 18 that attends school.
*
use example_averages, replace
gen dum_school_age=(age>=6 & age<18)
* In this way, dum_school_age=1 if child is in that age range; =0 if younger/older !!
*
gen aux_1=dum_school_age*attend_school
gen aux_2=dum_school_age*weight_person
* These two aux are multiplied by a dummy: 
* they are =0 if they refer to a person outside the age range of interest
*
egen perc_attend_school_6_to_18  = wtmean(aux_1), by(city_id_1991) weight(aux_2) //weighted mean, wieght is aux 3
drop aux_1 aux_2
*
* NOTE! We can also perform the same task easily through *collapse*. More elegantly perhaps.
*
use example_averages, replace
keep if age>=6 & age<18
*
collapse (mean) attend_school [pw=weight_person], by(city_id_1991) 
rename attend_school perc_attend_school_6_to_18 
*
duplicates drop city_id_1991, force
save perc_attend_school_6_to_18 , replace

}


**# 5 - Introduction to *preserve* and *restore*  ***
** TIP! IMPORTANT TO UNDERSTAND DO-FILE ASSOCIATED TO EXERCISE 2.
quietly {
* NOTE! While handling large data sets, having the ability to perform previous tasks
*  without having to re-open a data frame multiple times saves time. We can do so 
* with *preserve* and *restore*.
*
* Previously...
*
* We had to construct a city-level data set with the percentage of illiterate people 
* and average years of education using both the 1991 and 2010 city census definitions.
*
use example_averages, replace
*
* 1991's census definition
preserve //freezes dataframe in the memory before perfomring operations
	egen perc_illiterate_def_1 = wtmean(illiterate), by(city_id_1991) weight(weight_person)
	egen avg_educ_def_1 = wtmean(school_years), by(city_id_1991) weight(weight_person)
	duplicates drop city_id_1991, force
	sum perc_illiterate_def_1 avg_educ_def_1
	save data_city_id_1991, replace
restore //restoring when frozen
*
//we still have out original dataframe, and we saved the modified one!
** 2010's census definition
preserve 
	egen perc_illiterate_def_2 = wtmean(illiterate), by(city_id_2010) weight(weight_person)
	egen avg_educ_def_2 = wtmean(school_years), by(city_id_2010) weight(weight_person)
	duplicates drop city_id_2010, force
	sum perc_illiterate_def_2 avg_educ_def_2
	save data_city_id_2010, replace
restore 
*
* and...
*
* same while computing averages for a subpopulation of interest!
*
use example_averages, replace
*
preserve 
	keep if age>=6 & age<18
	collapse (mean) attend_school [pw=weight_person], by(city_id_1991) 
	rename attend_school perc_attend_school_6_to_18 
	duplicates drop city_id_1991, force
	save perc_attend_school_6_to_18 , replace
restore 

}


**# 6 - *program*, .ado files, *mata* and coding in Stata ***
*
** TIP! IMPORTANT TO UNDERSTAND DO-
** FILE ASSOCIATED TO EXERCISE 2.
quietly {

* NOTE! In Goldsmith-Pinkham et al.'s do-file "make_rotemberg_summary_ADH.do" - impact of China shock on labour outcomes you will notice different types of Stata objects that we have not discussed until now.
*
* For instance, *bartik_weights* and *ch_weak* are two commands that are used in that dof-file, for which you can find two separate .ado files in the folder we provided you with. In these .ado files you will then find references to *program* and *mata*.
// ch_weak to understand what it is look at the notes on the table we are asked to replicate (in the appendix of the Goldsmith-Pinkham paper). There we have the definition of ch_weak to compute confidence intervals with weak instruments
*
* What is an .ado file?
*
* An .ado file is a file that defines a Stata command. 
// as a general notion, it is a file which contains a program for a particular command or function that we want to call in our do-files, especially if needed repeatedly
*You can find more information about what is an .ado file, which Stata commands are ran from .ado files and how can you write and/or install an .ado file here:
* https://www.stata.com/manuals13/u17.pdf
*
*
* What is a *program*?
*
* A *program* is nothing else than a function, a routine that you repeatedly follow with different data, or in a particular project, that you wish to implement without having to have multiple lines of code cluttering your do-file.
//function that you call many times to repeat some algorithm that you need at different points
*
* TIP! Write *program*s in a general manner, so that you can use your functions
* in different contexts, with no need to re-write them at every project that you
* work on.
*
* You can find an extensive guide on programming in Stata here:
* https://www.stata.com/manuals/u18.pdf
*
* What is *mata*?
* 
* Putting it succintly, citing a blog post from Stata employees:
* "Mata is a full-blown programming language that compiles what you type into bytecode, optimizes it, and executes it fast."
//provide mathematical operations to your computer in ways that can be computed very fast
*
* more useful info on "https://www.stata.com/features/overview/introduction-to-mata/"
*
* We will not dive into *mata* during this TA neither during the course, still, we wanted to leave you with some references that might come in handy if you are asked in a near future to implement a new econometric method/algorithm  in Stata:
*
* Asjad Naqvi's Medium post introducing *mata*:
*https://medium.com/the-stata-guide/mata-statas-end-game-5983c0ee11bd
*
* Christopher Baum's *mata* tutorial:
*https://www.stata.com/meeting/germany09/baum.pdf
*
* Stata's *mata* manual:
* https://www.stata.com/manuals/m.pdf

}
