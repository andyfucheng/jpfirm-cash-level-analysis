# jpfirm-cash-level-analysis 
*Co-workers: Leslie Lee and Nicholas Narmada*

The purpose of this project is to apply statistical learning to make a decision about the level of corporate 
cash holdings and to decide whether the company should pay a dividend (payout decision). To construct the analysis, 
here we use a dataset of listed Japanese corporations between 2011 and 2019.

## Data Summary
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
## Research Background
Since there are costs and implications when corporations choose to hold excess cash, we aim to use statistical learning to predict the
level of cash holdings and dividend payout decision for Japanese companies. Cash holdings are strongly correlated with internal cahs 
flows and dividend payout (Nguyen 3). The pecking order model predicts that cash holdings are positively related to cash flows and dividend payouts, and negatively related to debt ratios. Dividend payments are used to recognize financial constraints, which have critical implications to firm strategy and perfomance.

Firms may hold excess cash due to risk aversion. Holding excess cash allows management to be more independent from capital markets, but the bidder would have greater access to liquid access, which would mean more gains to take over the firm. "Excess cash holdings can also exacerbate inefficiencies through the selection of unprofitable investments. On the other hand, firms hold liquid assets to reduce the costs of raising external recources when investment opportunities unexpectedly arise." (Nguyen)
Statistical learning can be used to predict levels of excess cash to determine if the firm has to cut back on investment, cut back dividends, or raise funds. Information asymmetry and agency costs play a significant role in cash holdings. Information asymmetry between managers and outside investors leads to greater costs in external financing compared to internal cash flows.

In japanese, corporations, the Keiretsu system can help mitigate the information asymmetry by reducing risk by sharing it across large and small corporations. Agency costs increase when interests of shareholders are different from interest of the debtholders, which may cause serious financial strain. With active participation of the monopolistic Japanese banks and cross owernship within corporations, agency costs can be lowered so Japanses firms do not need to have as much cash as US firms. Data in previous models such as the free cash flow model and tradeoff mdoel can be used to determine how firms should utilize their excess cash and minimize financial distress.
## Cash Holdings Decision
This project uses both Generalized Linear Model (GLM) and Linear Model (LM) to test the significance of each previous predictors in affecting corporation's cash holding level.
### Gnerealized Linear Model (GLM) 
To calculate for the model fit using GLM, we use *glm()* function from the *stats* library.
```
#Generalized linear model
glm.fits = glm(cashat~lnat+capat+leverage+comage+lnsales+ebitdaat, data=dfjpcorp)
summary(glm.fits)
```
Based on the result, the best generalized linear model suited for this case is the Guassian model.
### Linear Model (LM)
We use the *lm()* function which is also from *stats* library to calculate for the model fit of LM.
```
lm.fit = lm(cashat~lnat+capat+leverage+comage+lnsales+ebitdaat, data=dfjpcorp)
summary(lm.fit)
```
Based on the result, we can tell that all six predictors are statistically significant at the 0.0001% confidence level in affecting the cash holding level. Each of the independent variables' t-value is large enough which results in a small p-value. Thus, we can reject the null hypothesis of all predictors that there is no relationship between the predictors and the level of cash holding.The nature of the LM model allows for a low residual standard error (0.1253), and the R-squared statistic which equals to 0.4132 is definitely high enough. Thus, the accuracy of the model would be good enough to suggest the significance of predictors.
Among the independent variables, we can observe that capital expenditure *capat* has the greatest negative effect on cash holdings, where one unit increase in capat results in 0.634134 decrease in cash holding level. While in the other case, EBITDA *ebitdaat* has the greatest positive effect on cash holding where one unit increase in *ebitdaat* results in a 0.467623 increase in cash holding level.
## Dividend Payout Decision
### Logistic Model
First of all, to estimate the relationship between dividend payout decisions based on the predictors, we implement the same *glm()* function from *stats*, but set the *family* attribute as "binomial. According to the result, we can state that the correlation of all predictors are statistically significant at 0.0001%. Furthermore, among all the variables, we can observe a negative correlation between *leverage* and dividend payout decision while the rest of five obtain positive correlation. As for the economic siginificance from this logistic model, we can say that capital expenditure *capat* has the greatest positive impact as one unit increase lead to 3.75338 units rise in TRUE in *dividend*. 
```
#logistic model
glm.fits_bi= glm(dividend~lnat+capat+leverage+comage+lnsales+ebitdaat, data=dfjpcorp, family=binomial)
summary(glm.fits_bi)
glm.prob = predict(glm.fits_bi,  type ="response")
glm.prob[1:5]

contrasts(dfjpcorp$dividend)

# convert predicted probabilty into class labels
# note that we have 24851 rows
glm.pred=rep("FALSE",24851)
glm.pred[glm.prob>.5]="TRUE"
```
### Latent Dirichlet Allocation (LDA)
In the LDA model, we define firms whose *totaldividend>0* belong to TRUE in *dividend* while the negative side belongs to FALSE in *dividend*. In the whole dataset, there are 84.85% of firms did not pay the dividend while only 15.15% firms pay. Under this condition, except *leverage* which has a negative impact of -3.1508 change in TRUE in *dividend*, each unit increase of the rest of predictors change *dividend* positively. In the case that firms' *divident* is TRUE, the accuracy rate of logistic model is 87.88% which is slightly higher than 87.64% is LDA model. Here you can replace the threshold of *dividend* as you want to check if the accuracy rate changes responsively.
```
#LDA regression
library(MASS)

lda.fit=lda(dividend~lnat+capat+leverage+comage+lnsales+ebitdaat, data=dfjpcorp)
lda.fit
lda.prob = predict(lda.fit, type="response")
lda.prob.class=lda.prob$class

contrasts(dfjpcorp$dividend)
```
We also divide the whole dataset into training and test dataset based on year 2016 and create the confusion tables to compare the accuracy rate. According to the results, we learnt that the logistic model has a slightly higer accuracy rate of 88.52% in comparison of 88.50% in LDA model. Besides, in both models, a higher prediciton accuracy rate in TRUE than in FALSE is observed.
```
training = (dfjpcorp$year < 2016)
dfjpcorp.2016 = dfjpcorp[!training,]
Dividend.2016 = dfjpcorp$dividend[!training]

#logistic model.2016
glm.fits_bi= glm(dividend~lnat+capat+leverage+comage+lnsales+ebitdaat, data=dfjpcorp, family=binomial, subset = training)
glm.prob = predict(glm.fits_bi, dfjpcorp.2016, type ="response")
glm.pred = rep("FALSE",11716 )
glm.pred[glm.prob>.5]="TRUE"
table(glm.pred, Dividend.2016)
mean(glm.pred == Dividend.2016)
mean(glm.pred != Dividend.2016)

#LDA.2016 regression
lda.fits=lda(dividend~lnat+capat+leverage+comage+lnsales+ebitdaat,data=dfjpcorp,subset=training)
lda.fits
lda.prob = predict(lda.fits, dfjpcorp.2016, type ="response")
lda.pred = lda.prob$class
#lda.pred = rep("FALSE",11716 )
#lda.pred[lda.posterior>.5]="TRUE"
table(lda.pred, Dividend.2016)
mean(lda.pred == Dividend.2016)
mean(lda.pred != Dividend.2016)
```
## Non-linear Association of Company Age
When running the cross validation, we use 10 different samples to make sure we have a better accuracy in assessing which order of *age* gives the lowest MSE. However, since the Leave One Out Cross Validation (LOOCV) is very time consuming, we only run it for the cash holding model. You can view the detailed code for the cross validation starts from line 193 to 476 at the attached R file.
