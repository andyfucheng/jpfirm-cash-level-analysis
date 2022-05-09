# jpfirm-cash-level-analysis 
## Co-workers: Leslie Lee and Nicholas Narmada
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
flows and dividend payout (Nguyen 3). The pecking order model predicts that cash holdings are positively related to cash flows and dividend payouts, and negatively related to debt ratios. Dividend payments are used to recognize financial constraints, which have critical implications to firm strategy and perfomance.

Firms may hold excess cash due to risk aversion. Holding excess cash allows management to be more independent from capital markets, but the bidder would have greater access to liquid access, which would mean more gains to take over the firm. "Excess cash holdings can also exacerbate inefficiencies through the selection of unprofitable investments. On the other hand, firms hold liquid assets to reduce the costs of raising external recources when investment opportunities unexpectedly arise." (Nguyen)
Statistical learning can be used to predict levels of excess cash to determine if the firm has to cut back on investment, cut back dividends, or raise funds. Information asymmetry and agency costs play a significant role in cash holdings. Information asymmetry between managers and outside investors leads to greater costs in external financing compared to internal cash flows.

In japanese, corporations, the Keiretsu system can help mitigate the information asymmetry by reducing risk by sharing it across large and small corporations. Agency costs increase when interests of shareholders are different from interest of the debtholders, which may cause serious financial strain. With active participation of the monopolistic Japanese banks and cross owernship within corporations, agency costs can be lowered so Japanses firms do not need to have as much cash as US firms. Data in previous models such as the free cash flow model and tradeoff mdoel can be used to determine how firms should utilize their excess cash and minimize financial distress.
