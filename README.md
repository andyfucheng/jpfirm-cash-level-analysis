# jpfirm-cash-level-analysis
The purpose of this project is to apply statistical learning to make a decision about the level of corporate 
cash holdings and to decide whether the company should pay a dividend (payout decision). To construct the analysis, 
here we use a dataset of listed Japanese corporations between 2011 and 2019.

### Data Summary
Before starting the analysis, here shows the summary of the dataset we are going to use. The dataset applied in this project
is organized in a long data format, which contains 28089 rows of financials filed by Japanese public companies from 2011 to 2019. In 
this project, we are going to inspect through these figures and try to answer the following questions about corporate decisions:
1. Decide the level of corporate holdings based on the dataset.
2. Decide whether the company pays a dividend (Payout decision) based on the dataset.
This dataset of listed Japanese corporations between 2011 and 2019 include finance-related variables like EBITDA and net income. First
step we clean the dataset to exclude rows with any missing value and summarize the dataset.
```
library(readxl)
jpcorp = read_excel("japan_public_corporations_1119.xlsx")
dfjpcorp = data.frame(jpcorp)

#omit NA variables
dfjpcorp = na.omit(dfjpcorp)

output = summary(dfjpcorp)
print(output)
```
Also, to increase the prediction accuracy and reduce the impact of data outliers, we winsorize continuous variables at 1% and 99% 
by using library DescTools.
```
library(DescTools)
Winsorize(dfjpcorp$at,probs=c(0.01, 0.99), na.rm = TRUE)
Winsorize(dfjpcorp$lt,probs=c(0.01, 0.99), na.rm = TRUE)
Winsorize(dfjpcorp$ebitda,probs=c(0.01, 0.99), na.rm = TRUE)
Winsorize(dfjpcorp$sales,probs=c(0.01, 0.99), na.rm = TRUE)
Winsorize(dfjpcorp$netincome,probs=c(0.01, 0.99), na.rm = TRUE)
Winsorize(dfjpcorp$capexp,probs=c(0.01, 0.99), na.rm = TRUE)
Winsorize(dfjpcorp$totaldividend,probs=c(0.01, 0.99), na.rm = TRUE)
Winsorize(dfjpcorp$ltermdebt,probs=c(0.01, 0.99), na.rm = TRUE)
Winsorize(dfjpcorp$cash,probs=c(0.01, 0.99), na.rm = TRUE)
Winsorize(dfjpcorp$totalsale,probs=c(0.01, 0.99), na.rm = TRUE)
Winsorize(dfjpcorp$totalcurrentliabilities,probs=c(0.01, 0.99), na.rm = TRUE)
Winsorize(dfjpcorp$opincome,probs=c(0.01, 0.99), na.rm = TRUE)
```
Next, to quantify the corporates decision problems we are going to solve, here two new, dependent variables are defined: *cashat* 
(cash holding ratio) and *dividend* (binaru dividend payment decision). And our main independent factors used to predict target variables 
are: total asset size (*lnat*), capital expenditure ratio (*capat*), leverage ratio (*leverage*), company age (*comage*), sales (*lnsales*)
, and EBITDA (*ebitdaat*).
```
#Main Dependent Variables
dfjpcorp$cashat = dfjpcorp$cash/dfjpcorp$at
dfjpcorp$dividend = dfjpcorp$totaldividend >0
attach(dfjpcorp)

#Main Predictors (Independent Variables)
dfjpcorp$lnat = log(dfjpcorp$at)
dfjpcorp$capat = dfjpcorp$capexp/dfjpcorp$at
dfjpcorp$leverage = dfjpcorp$ltermdebt/dfjpcorp$at
dfjpcorp$comage = log(dfjpcorp$year-dfjpcorp$yearfound+1)
dfjpcorp$lnsales = log(dfjpcorp$sales)
dfjpcorp$ebitdaat = dfjpcorp$ebitda/dfjpcorp$at

#remove infinite values
library(IDPmisc)
dfjpcorp = NaRV.omit(dfjpcorp)
dfjpcorp = na.omit(dfjpcorp)
```
### Research Background
Since there are costs and implications when corporations choose to hold excess cash, we aim to use statistical learning to predict the
level of cash holdings and dividend payout decision for Japanese companies. Cash holdings are strongly correlated with internal cahs 
flows and dividend payout (Nguyen 3).
