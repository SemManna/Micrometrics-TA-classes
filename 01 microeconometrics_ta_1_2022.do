** Microeconometrics 20295, TA 1, Introduction to Stata
** Prof: Thomas Le Barbanchon
** TA: Jaime Marques Pereira
** Partial credits to Francesca Garbin and Alexandros Cavgias

* Objectives for the class:
* 0 - Keyboard shortcuts;
* 1 - Basic algebra, vectors and matrices, and logic operations;
* 2 - Stata workflow and version control;
* 3 - Data frame management (& version control);
* 4 - Basic data examination.

********************************************************************************
//quitely allows to run the code without showing in the main window all the commands and to organize the script of the do file in sections

*** 0 - Keyboard shortcuts ***

quietly {

** Use an example dataset installed with Stata **

quietly {
*
* Select the line below and press Ctrl+d
sysuse lifeexp, clear
* CTRL+d -> execute selected part of do-file
* "Sheet" symbol with blue triangle ("play"): execute selected part

// stata allows to work with one stataframe at the time. If we don't specify ", clear" we cannot open the new dataframe. the option , clear drops the previous one.

** Describe variables present in the imported dataset **
*
* Select the *result* window and press F2
* F2 -> describe
* Or type following command (it appears in the result window after pressing F2):
des

* "Help" gives you all the information about a command and its options
help describe
help description
//no action description, looks for it in other commands
}

** Browse values taken in each variable **

quietly {

* Select the *result* window selected and press CTRL+8
* CTRL+8 -> data editor
*
* Alternative 1: from Window > Data Editor (Edit)
*
* Altervative 2: Data > Data Editor > Data Editor (Edit)
*
* Alternative 3: 
edit
*
* Otherwise, to avoid unwillingly to modify the data:
*
* Altervative 1: Data > Data Editor > Data Editor (Browse)
*
* Aternative 2:  "browse" (abbr. "br") command
bro

}


** Close *data editor/do-file/result* window **

quietly {
	
* To close for example the *data editor* window (both Edit and Browse), press Alt+F4
* Alt+F4 -> close window

}


** List a variable of choice **

quietly {

* E.g. variable "popgrowth"
*
* Select the *command* window, type "list" and press SPACE, 
* press TAB and a dropdown menu appears
*
* Alternative 1: Press "p" to select "popgrowth" and press ENTER
* TAB+"letter" -> complete variable
*
* Altervative 2: in the dropwdown menu, go up/down with the arrows and select your variable by pressing TAB
*
* Can be done with many commands, e.g. summ, tab, etc.

}


** Confirm working directory **

quietly {
	
* In the Command window type "pwd" and press ENTER
* pwd -> recall work directory path
* This is where your graphs, datasets, etc are saved
* 
* if you want to change the current working directory, then type
* cd "C:\Users\path\to\your\folder"

}

** Open new do-file **

quietly {

* Select the *do-file* window and press CTRL+n
* Select the *result* window and press CTRL+9

* CTRL+n -> new do-file (while working on the do-file window)
* Ctrl+9 -> new do file (while working on the result window)

}


** Clear *result* window **

quietly {
	
* In the Command window type cls 
* cls -> removes text from result window

* BUT you are still using the same dataset
* If you want to remove data from memory, type "clear"

}

}

********************************************************************************

*** 1 - Basic algebra, vectors and matrices, and logic operations *** 

quietly {

** Basic algebra **

quietly {
	
* To sum 2 plus 2
display 2 + 2
*
* Or in a shorter version
di 2 + 2

}


** Vectors and matrices **

quietly {
	
* Define a scalar x equal to 1
scalar x = 1
*
* Define a matrix with a first row [1, 2, 3] and a second [5, 6, 7]
matrix A = (1,2,3\5,6,7)
*
* List both objects
list x
list A
*
* ERROR! You must specify the class of the object before any operation.
* NOTE! Only for objects that are not a variable in your data frame.
*
scalar list x
matrix list A
*
* QUESTION! Why should one care about vectors and matrices in Stata?
*
* Output tables usually demand manipulation of objects that are not in your data
* frame. E.g presenting alternative types of clustered SEs or standardized coef.
*
* TASK! Save the element 2,2 of matrix A in a scalar named y.
*
scalar y = A[2,2]
scalar list y 
*
* NOTE! Stata, unlike Phyton, R or Matlab, does not allow you to easily 
* visualize objects nor present your data frame. To avoid mistakes, PRINT
* post-estimation objects each time you write an operation with these.
*
* Repeat the previous task while printing each object used
*
matrix list A
scalar y = A[2,2]
scalar list y 

}

** Logic operations ** 

quietly {
	
* Import example dataset installed with Stata
sysuse auto, clear
des
*
* (0 conditions) Tabulate variable rep78
tab rep78
tab rep78, missing
* (1 condition) Tabulate rep78 for values greater or equal than 4 
// when using tab, if you not specify ", missing" after it you won't see the missing values
tab rep78 if rep78 >= 4
* (2 conditions) Tabulate rep78 for values >= 4 and different than missing
tab rep78 if rep78 >= 4 & rep78 != . //redoundant operator &, try with |
*
* (0 conditions) Summarize variable price
sum price
* (1 condition) Summarize price for observations where rep78 is lower to 2
sum price if rep78 < 2
* (1 condition) Summarize price for observations where car type is domestic
tab foreign
codebook foreign
sum price if foreign==0
* (2 conditions) Summarize price for obs. where rep78 is equal to 1 OR 2
sum price if rep78 == 1 | rep78 == 2
* (3 conditions) Summarize price for obs. where (rep78 is equal to 1 OR 2) AND car type is domestic
sum price if (rep78 == 1 | rep78 == 2) & foreign==0
* (by condition) Summarize price by the values of variable foreign
by foreign: sum price
*
* NOTE: dataset needs to be sorted by that variable
* type "des" and see that it is sorted by foreign
*
sort price
describe
//sort price sorted the data, at the bottom of the table we can see it says 'sorted by: price'
//hit browse and you can also see this yourself
by foreign: sum price
*
* ERROR! 
*
* Solution 1
sort foreign
by foreign: sum price
*
* Solution 2
bysort foreign: sum price

* In summary:
* sum -> summarize
* tab -> tabulate
*
* Logical connectors:
* == 		-> EQUAL
* != 		-> NON EQUAL
* <, <= 	-> SMALLER THAN, SMALLER OR EQUAL THAN
* >, >= 	-> GREATER THAN, GREATER OR EQUAL THAN
* & 		-> AND
* | 		-> OR 
* With multiple conditions, use parentheses

** More ways of introducing conditions:
*
* - does a certain argument belong to a certain list?
* - does a certain argument belong to a certain range?
*
* The next commands are all equivalent:
*
tab make rep78 if rep78 == 3 | rep78 == 4 | rep78 == 5
tab make rep78 if rep78 >= 3 & rep78 <= 5
tab make rep78 if rep78> 2 & rep78 < 6
tab make rep78 if inlist(rep78, 3, 4, 5)
tab make rep78 if inrange(rep78, 3, 5)

* So long as none of the arguments z, a, b is missing, inrange(z,a,b) is true whenever 
* z>=a and z<=b.Thus inrange(60, 50, 70) is true (numerically 1) because 60>=50 and 60<=70
* However, inrange(60, 70, 50) is false (0) because 60 is not >=70 and 60 is not <=50
*
* REMARK! Not sure in advance about the ordering of arguments? 
*
* inrange(z, min(a,b), max(a,b))
*
* REMARK! inrange() can have issues with missing values, inlist() does not.

}

}

********************************************************************************

*** 2 - Stata workflow and version control *** 

quietly {
	
** Stata workflow **

quietly {

* NOTE! No need to memorize.
*
* 1) StataCheatSheet's uploaded on Blackboard have the majority of commands used.
* 2) Leverage on Stata's community by googling your questions and focusing on
* discussion forums such as Statalist and Stackoverflow.
*
* Import example dataset installed with Stata
sysuse lifeexp, clear
*
* TASK! Count the number of countries in the example dataset.
*
* Google "compute number of distinct observations stata". 
*
* Then click:
* "https://www.stata.com/support/faqs/data-management/..."
* Or: 
* "https://www.statalist.org/forums/forum/general-stata-discussion/general/..."
* 
search distinct
ssc install distinct
// distinct, to download the new command e.g. distinct, ssc install "distinct"

* Use distinct
distinct country
return list
gen dist_country=r(ndistinct)
summ dist_country
*
//to store the value of the distinct values (or in general of values generated by a command) use the command "return list", and then you can use the values generated. Then define as scalar the values returned, i.e. "scalar x = r(ndistinct)". now 'scalar list x' list the value of r(distinct)

* Use codebook
codebook country

}

** Version control **

quietly {
	

* NOTE! Have a history of changes for each of your scripts. Of every change in the code! This is crucial as it allows you to experiment with your coding freely.
//VERSION CONTROL, evry time you save there will be a version of the file saved online that you can retrive!
//set up githumb account, install it
//create new repository and publish it (privately)
//commands in the ignore github file for it to not track the changes on files other than the ones we care about (usually the .do)
*
* It is also important in collaborations as you will want to keep up with what your co-authors did or are doing, script-wise. And again, you want to be able to write code on a shared file without fully commiting to it. Solution: a version control software. More specifically? Git + Github!
*
* TASK! Set up your own Github account, install Github Desktop and start your
* own repository for the microeconometrics course.
* Go to: "https://github.com/" and sign up.
* Install Github Desktop from here: https://desktop.github.com/
*
* Create a new repository through Github Desktop titled "micrometrics_stata". //folder online where you store the file and the different versions of it
* Place this folder wherever you want (on your desktop or in a specific folder 
* related to your courses or the course itself). Describe the repository as:
*
* "Repository to store do-files related to the microeconometrics Masters course (cd. 20295) at Bocconi University."
*
* Add a .gitignore file through Github Desktop where you choose a random set 
* of files to be ignored (we will alter this file later on, important).
*
* FOLLOW IN-CLASS INSTRUCTIONS
*
* Create a folder through Stata titled "ta_1" on your newly created folder. 
*
mkdir "C:\Users\semma\OneDrive - Università Commerciale Luigi Bocconi\Bocconi\ESS\Microeconometrics\Micrometrics-TA-classes\ta_1"
*
* Add the current do-file to the folder at stake and commit this change + push 
* it to your Github repository (commit and push through Github Desktop).
*
* FOLLOW IN-CLASS INSTRUCTIONS
*
* TIP! When using Git + Github, track only text-like files.
* IMPORTANT
* Re-write .gitignore file so that Git ignores all files other than do-files.
* In particular, type the following commands on your terminal window (if using 
* Windows):
*
* "echo /* > C:\Users\jsdmp\Desktop\micrometrics_stata\.gitignore"
* "echo /*.gitignore >> C:\Users\jsdmp\Desktop\micrometrics_stata\.gitignore"
* "echo !/*.do >> C:\Users\jsdmp\Desktop\micrometrics_stata\.gitignore"
* "echo !/*.md >> C:\Users\jsdmp\Desktop\micrometrics_stata\.gitignore"

*
* Commit this change and push it to your Github repository.
*
* Create a text file on that same folder titled "micrometrics_notes".
*
* Create a copy of the do-file initially added to the folder.
*
* Check changes that were recorded by Git! As you can see, adding a text file
* to your repository was not recorded by Git. Instead, adding a new do-file
* was correctly recorded. This is important for space management reasons: you
* do not want to track changes from relatively large files (e.g., PDFs or CSVs)
* as this will exhaust your disk space (through Git) and your Github storage 
* limit (100GB per repository; files of no more than 2GB).
*
* REFERENCE! This section was vastly inspired by Asjad Naqvi 
* (https://twitter.com/AsjadNaqvi) exhaustive Medium post on 
* how you can interchange between Git, Github and Stata as
* efficiently as possible (from a researcher's standpoint).
*
* You can find the Medium post here:
*
* https://medium.com/the-stata-guide/stata-and-github-integration-8c87ddf9784a
*
* I would also suggest that you read through Asjad's other Medium posts!

}

}


********************************************************************************

*** 3 - Data frame management ***

quietly {

** Pre-setting data management information  **

quietly {
	
* Setting working directory
cd "C:\Users\semma\OneDrive - Università Commerciale Luigi Bocconi\Bocconi\ESS\Microeconometrics\Micrometrics-TA-classes"
*
* Import example dataset installed with Stata
sysuse lifeexp, clear

}

** First contact with a data frame **

quietly {
	
* List and describe variables present in the data frame
describe   

}                   


** Create, replace, and drop variables **              

quietly {

* Generate new variable
gen gnppc_eu = 1.1*gnppc
*
* Replace values from existent variable
replace gnppc_eu = gnppc_eu/1000
*
* Drop existent variable
drop gnppc  

} 			  

			  
** Labels and notes ** 

quietly {

* NOTE! Labels should contain *short* and *self-contained* explanations 
* to understand in second takes what are the variables present in the data.
*
* Check already existing labels
*
label list
*
* TASK! Use the *variable manager* to change
* - the label of *popgrowth* to "average annual % growth";  
* - the name of *popgrowth* to *pop_growth*. 
*
label variable popgrowth "average annual % growth"
rename popgrowth pop_growth
*
* TASK! Use the *do file* to change:
* - the label of *gnppc* to "per capita GNP"; 
* - the name of *gnppc* to *pc_gnp*. 
*
label variable gnppc "per capita GNP"
rename gnppc pc_gnp
*
* TIP! For any task, do it once clicking and as many times you want coding.
*
* NOTE! Notes should contain larger explanations of each variable (not possible
* to include as a label due to the limit of characters imposed on labels).
*
* List notes from variables present in the data frame
notes
*
* TASK! Use the *variables manager* to add a note in the variable *region* 
* "Region Eur & C.Asia were separated before 1994."
* AND use the *do file* to modify the content of the same note.
* "Region Eur & C.Asia were merged on 1994."
*
notes region: Region Eur & C.Asia were separated before 1994.
notes
notes region: Region Eur & C.Asia were merged on 1994.
notes

}


** Appending datasets ** 

quietly {
	
* Import example dataset installed with Stata
sysuse auto, clear
*
* TIP! When combining datasets generate a **unique identifier**.
*
* Generate identifier for observations of file 1
gen file=1
* gen -> generate
*
* Save file 1
save 01auto_copy.dta, replace
*
* Import again example dataset installed with Stata
sysuse auto, clear
*
* Generate identifier forobservations of file 2
gen file=2
*
* Append file 2 onto file 1
append using "01auto_copy.dta"
*
* Save appended file
save 01auto_copy_double.dta, replace

}

** Merging datasets **  

quietly {
	
* Import appended file
use 01auto_copy_double.dta, replace
use "C:\Users\semma\OneDrive - Università Commerciale Luigi Bocconi\Bocconi\ESS\Microeconometrics\Micrometrics-TA-classes\01auto_copy_double.dta", clear
*
* Proceeding with the identifier variable "make"
*
* Merge appended file with file 1
* NOTE! Pre-specifying that in each file *make* is a unique identifier (1-to-1).
merge 1:1 make using "01auto_copy.dta"
* ERROR! In the master (appended) file the variable make does not uniquely
* identify observations. In fact:
distinct make
*
* Merge appended file with file 1
* NOTE! Pre-specifying that in the master file *make* is not a unique identifier (m-to-1).
merge m:1 make using "01auto_copy.dta"
*
* TIP! Browse observations that were *not merged* and take note.
br if _merge!=3
br if _merge==1
br if _merge==2
* br -> browse
*
* NOTE! A safe merge must have at least one dataset with a *unique identifier*.
*
* What do you do if neither has an unique identifier? 
* - Drop useless duplicates (through *duplicates drop*);
* - Collapse the data at the level of a unique identifier.  
*
* Import appended file
use 01auto_copy_double.dta, replace
*
* Count number of unique values taken by variable *make*
distinct make
*
* Drop duplicate observations
duplicates drop make, force
*
* Merge appended file with file 1
* NOTE! Pre-specifying that in each file *make* is a unique identifier (1-to-1).
merge 1:1 make using "01auto_copy.dta"
*
br

}

}

********************************************************************************


*** 4 - Basic data examination ***

quietly {

** Browsing missing and extreme values **

quietly {
	
* Import an example dataset installed with Stata
sysuse lifeexp, clear
*
* Examine densities
tab region
*
* Browse observations with missing values for particular variables
br if missing(safewater) 
list if missing(safewater)
*
* Browse extreme values of relevant variables
sort popgrowth
list in 1/10 
list in -10/-1 
*
* Retrieve summary statistics for particular variables 
sum safewater 
sum safewater, detail 
* Pair-wise correlations
pwcorr
pwcorr, star(.05)
*
* TASK! Retrieve number of observations in the sample at stake (with safewater!=.)
* 
sum safewater, d 
return list
scalar sample_size = r(N)
scalar list sample_size
*
* TASK! Compute the *standardized effect* of safewater on lexp and save as a
* *scalar* named beta_sd.
*
* INFO! The OLS estimate of the effect of safewater on lexp is .184967:
reg lexp popgrowth gnppc safewater
* Also, * the *standardized effect* is defined as the effect of one s.d. change in x (safewater) measured in terms of a s.d. of y (lexp)
*
reg lexp popgrowth gnppc safewater
return list
matrix coeffs= r(table)
matrix list coeffs
gen beta_water=coeffs[1,3]
summ beta_water
*
sum lexp
scalar sigma_y = r(sd)
sum safewater 
scalar sigma_x = r(sd)
*scalar beta_sd = (.184967*sigma_x)/sigma_y
scalar beta_sd = (beta_water*sigma_x)/sigma_y
scalar list beta_sd

}

**  Defining a *global* variable **

quietly {
	
* Import example dataset installed with Stata
sysuse auto, clear
*
* Define global variable *baseline* as "price mpg lenght"
global baseline "price mpg length"
*
* Summarize variables included in *baseline*
sum $baseline
*
* NOTE! Use $ to call a global.
* NOTE! Globals can be called at any point of the script. 

}

** Defining a *local* variable **

quietly {
	
* Define local variable *reg_local* as "price mpg lenght"
local reg_local "price mpg length"
*
* Summarize variables included in *reg_local* 
sum `reg_local'
*
* NOTE! Locals must be defined in the same block of code where they are used
* NOTE! Use `  to call a local.
*
* TIP! Input dataframe content to vectors and matrices through local variables.
* 
levelsof mpg, local(mileage_levels)
matrix input mileage_levels = (`mileage_levels')
matrix list mileage_levels 

}

** Doing loops ** 

quietly {
	
* NOTE! Majority of loops made with Stata can be done with *for*.
* Two for loops: foreach / forvalues
*
* looping over numbers * 
forvalues i = 10(10)50 {
	di .01*`i'
}

*
* looping over strings in a list * 
foreach x in "Dr. Nick" "Dr. Hibbert" {
	display length( "`x'" )
}

*
* loop over files in a list *
foreach x in auto.dta auto2.dta {
	sysuse "`x'", clear
	tab rep78, missing
}

*
* loop over variables in a list * 
sysuse auto, clear
foreach x in mpg weight {

	sum `x'

}
* or
foreach x of varlist mpg weight {

	sum `x'
	
}
*
* TASK! Use a local variable and the command *levelsof* to compute the average 
* of variable *price* within each level of *mpg*.
*
levelsof mpg, local(consump_levels)
foreach x in `consump_levels' {

	sum price if mpg==`x' 
	
}
*
* Useful when lists are too long to type (e.g. loop over the universe of candidates).

}

}
