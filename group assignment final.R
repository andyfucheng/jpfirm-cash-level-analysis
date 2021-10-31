#Group Assignment
#Part1 
#data cleaning

library(readxl)
jpcorp = read_excel("japan_public_corporations_1119.xlsx")
dfjpcorp = data.frame(jpcorp)

#omit NA variables
dfjpcorp = na.omit(dfjpcorp)

#winsorize continuous variables at 1% and 99% by using library DescTools 
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

#Main Predictors (Independent Variables)
dfjpcorp$lnat = log(dfjpcorp$at)
dfjpcorp$capat = dfjpcorp$capexp/dfjpcorp$at
dfjpcorp$leverage = dfjpcorp$ltermdebt/dfjpcorp$at
dfjpcorp$comage = log(dfjpcorp$year-dfjpcorp$yearfound+1)
dfjpcorp$lnsales = log(dfjpcorp$sales)
dfjpcorp$ebitdaat = dfjpcorp$ebitda/dfjpcorp$at

#Main Dependent Variables
dfjpcorp$cashat = dfjpcorp$cash/dfjpcorp$at
dfjpcorp$dividend = dfjpcorp$totaldividend >0
attach(dfjpcorp)

#remove infinite values
library(IDPmisc)
dfjpcorp = NaRV.omit(dfjpcorp)
dfjpcorp = na.omit(dfjpcorp)
#variables after lnat is newly created predictors
summary(dfjpcorp)

#standard deviation for each variable
sd(dfjpcorp[,c(6)])
sd(dfjpcorp[,c(7)])
sd(dfjpcorp[,c(8)])
sd(dfjpcorp[,c(9)])
sd(dfjpcorp[,c(10)])
sd(dfjpcorp[,c(11)])
sd(dfjpcorp[,c(12)])
sd(dfjpcorp[,c(13)])
sd(dfjpcorp[,c(15)])
sd(dfjpcorp[,c(17)])
sd(dfjpcorp[,c(18)])
sd(dfjpcorp[,c(19)])
sd(dfjpcorp[,c(20)])
sd(dfjpcorp[,c(21)])
sd(dfjpcorp[,c(22)])
sd(dfjpcorp[,c(23)])
sd(dfjpcorp[,c(24)])
sd(dfjpcorp[,c(25)])
#output dataframe to csv file
write.csv(dfjpcorp,'jpcorps_cleaned.csv', row.names = TRUE)



#Part3
#part3.a
#Cash holding is the ratio between cash and total assets
#dfjpcorp$cashat = dfjpcorp$cash/dfjpcorp$at

#Generalized linear model
glm.fits = glm(cashat~lnat+capat+leverage+comage+lnsales+ebitdaat, data=dfjpcorp)
summary(glm.fits)

#According to the result, Gaussian model is the best.
lm.fit = lm(cashat~lnat+capat+leverage+comage+lnsales+ebitdaat, data=dfjpcorp)
summary(lm.fit)

#compare glm and lm
#AIC

#part3.b
#adding binary indicator for dividend payment decision
#dfjpcorp$dividend = dfjpcorp$totaldividend >0

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

# create a confusion table
table(glm.pred,dfjpcorp$dividend)
mean(glm.pred == dfjpcorp$dividend)

#LDA regression
library(MASS)

lda.fit=lda(dividend~lnat+capat+leverage+comage+lnsales+ebitdaat, data=dfjpcorp)
lda.fit
lda.prob = predict(lda.fit, type="response")
lda.prob.class=lda.prob$class

contrasts(dfjpcorp$dividend)

table(dfjpcorp$dividend,lda.prob$class)
mean(dfjpcorp$dividend == lda.prob.class)

# convert predicted probabilty into class labels
# note that we have 24851 rows

summary(totaldividend)

#Discuss different threshold 
glm.mean=rep(0,50)
lda.mean=rep(0,50)
for (i in 1:50){
  dfjpcorp$dividend_1 = dfjpcorp$totaldividend >i

  #logistic model
  #summary(dfjpcorp)
  glm.fits_bi= glm(dividend_1~lnat+capat+leverage+comage+lnsales+ebitdaat, data=dfjpcorp, family=binomial)
  summary(glm.fits_bi)
  glm.prob = predict(glm.fits_bi,  type ="response")
  glm.prob[1:5]

  contrasts(dfjpcorp$dividend_1)
  
  # convert predicted probabilty into class labels
  # note that we have 1250 rows
  glm.pred=rep("FALSE",24851)
  glm.pred[glm.prob>.5]="TRUE"
  table(glm.pred,dfjpcorp$dividend_1)
  glm.mean[i]=mean(glm.pred == dfjpcorp$dividend_1)

  #lda
  lda.fit=lda(dividend_1~lnat+capat+leverage+comage+lnsales+ebitdaat, data=dfjpcorp)
  lda.fit
  lda.prob = predict(lda.fit, type="response")
  lda.prob.class=lda.prob$class

  contrasts(dfjpcorp$dividend_1)

  table(dfjpcorp$dividend_1,lda.prob$class)
  lda.mean[i]=mean(dfjpcorp$dividend_1 == lda.prob.class)
}
glm.mean
lda.mean

#compare error rate with different threshold

#Part3_c
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


#Part4
#consider polynomial condition
library(boot)

#cash holding model through glm regression
glm.fits = glm(cashat~lnat+capat+leverage+comage+lnsales+ebitdaat, data=dfjpcorp)

#dividend decision (logistic regression)
glm.fits_bi= glm(dividend~lnat+capat+leverage+comage+lnsales+ebitdaat, data=dfjpcorp, family=binomial)

#dividend decisin (lda regression)
lda.fit=lda(dividend~lnat+capat+leverage+comage+lnsales+ebitdaat, data=dfjpcorp)

#logistic model with polynomial function 

#Validatosn set approach
vsa.glm = data.frame(poly1 = double(),
                     poly2 = double(),
                     poly3 = double(),
                     poly4 = double(),
                     poly5 = double(),
                     poly6 = double(),
                     poly7 = double(),
                     poly8 = double(),
                     poly9 = double(),
                     poly10 = double(),
                     stringsAsFactors=FALSE)

vsa.logi = data.frame(poly1 = double(),
                      poly2 = double(),
                      poly3 = double(),
                      poly4 = double(),
                      poly5 = double(),
                      poly6 = double(),
                      poly7 = double(),
                      poly8 = double(),
                      poly9 = double(),
                      poly10 = double(),
                      stringsAsFactors=FALSE)

vsa.lda = data.frame(poly1 = double(),
                     poly2 = double(),
                     poly3 = double(),
                     poly4 = double(),
                     poly5 = double(),
                     poly6 = double(),
                     poly7 = double(),
                     poly8 = double(),
                     poly9 = double(),
                     poly10 = double(),
                     stringsAsFactors=FALSE)
  
for (j in 1:10){
  set.seed(j)
  train.cv=sample(24851,12425)
  test.cv = dfjpcorp[-train.cv,]
  Dividend.test = dfjpcorp$dividend[-train.cv]

#cashat glm with polynomial (Validation set approach)
  cv.error=rep(0,10) 
  for (i in 1:10){
    glm.fit = glm(cashat~poly(comage,i)+lnat+capat+leverage+lnsales+ebitdaat,data=dfjpcorp, subset=train.cv)
    attach(dfjpcorp)
    cv.error[i]=mean((cashat -predict (glm.fit ,dfjpcorp))[-train.cv ]^2)
    
  }
  vsa.glm[j,] = cv.error
  
  #dividend logistical model
  cv.error.logi=rep(0,10)
  for (i in 1:10){
    glm.fits_bi= glm(dividend~poly(comage,i)+lnat+capat+leverage+lnsales+ebitdaat, data=dfjpcorp, subset=train.cv,family=binomial)
    glm.prob = predict(glm.fits_bi, test.cv, type ="response")
    glm.pred = rep("FALSE",12426 )
    glm.pred[glm.prob>.5]="TRUE"
    cv.error.logi[i]=mean(glm.pred != Dividend.test)
  }
  vsa.logi[j,]=cv.error.logi
  
  #LDA model with polynomial (Validation set approach)
  cv.error.lda=rep(0,10)
  for (i in 1:10){
    lda.fit=lda(dividend~poly(comage,i)+lnat+capat+leverage+lnsales+ebitdaat,data=dfjpcorp,subset=train.cv)
    lda.prob = predict(lda.fit, test.cv, type ="response")
    lda.pred = lda.prob$class
    cv.error.lda[i]=mean(lda.pred != Dividend.test)
  }
  vsa.lda[j,] = cv.error.lda
}

vsa.glm
vsa.logi
vsa.lda

write.csv(vsa.glm,'vsa.glm.csv', row.names = TRUE)
write.csv(vsa.logi,'vsa.logi.csv', row.names = TRUE)
write.csv(vsa.lda,'vsa.lda.csv', row.names = TRUE)


#LOOCV 
loocv.glm = data.frame(poly1 = double(),
                     poly2 = double(),
                     poly3 = double(),
                     poly4 = double(),
                     poly5 = double(),
                     poly6 = double(),
                     poly7 = double(),
                     poly8 = double(),
                     poly9 = double(),
                     poly10 = double(),
                     stringsAsFactors=FALSE)

loocv.logi = data.frame(poly1 = double(),
                      poly2 = double(),
                      poly3 = double(),
                      poly4 = double(),
                      poly5 = double(),
                      poly6 = double(),
                      poly7 = double(),
                      poly8 = double(),
                      poly9 = double(),
                      poly10 = double(),
                      stringsAsFactors=FALSE)

loocv.lda = data.frame(poly1 = double(),
                     poly2 = double(),
                     poly3 = double(),
                     poly4 = double(),
                     poly5 = double(),
                     poly6 = double(),
                     poly7 = double(),
                     poly8 = double(),
                     poly9 = double(),
                     poly10 = double(),
                     stringsAsFactors=FALSE)


set.seed(1)
#cash holdings GLM with polynomial (LOOCV)
cv.error=rep(0,10) 
for (i in 1:10){
  glm.fits = glm(cashat~poly(comage,i)+lnat+capat+leverage+lnsales+ebitdaat, data=dfjpcorp)
  cv.error[i]=cv.glm(dfjpcorp,glm.fits)$delta[1]
}

loocv.glm[1,]=cv.error
  
#dividend logistic model
cv.error.logi=rep(0,10)
for (i in 1:10){
  glm.fits_bi= glm(dividend~poly(comage,i)+lnat+capat+leverage+lnsales+ebitdaat, data=dfjpcorp,family=binomial)
  #cv.error.logi[i]=mean((dividend -predict (glm.fits_bi ,dfjpcorp))[-train.cv ]^2)
  cv.error.logi[i]=cv.glm (dfjpcorp,glm.fits_bi)$delta [1]
}
loocv.logi[1,]=cv.error.logi
  
#LDA with polynomial function(LOOCV)
cv.error.lda=rep(0,10) 
for (i in 1:10){
  lda.fit=lda(dividend~poly(comage,i)+lnat+capat+leverage+lnsales+ebitdaat,data=dfjpcorp,CV=TRUE)
  table(dfjpcorp$dividend, lda.fit$class)
  cv.error.lda[i]=mean(lda.fit$class != dfjpcorp$dividend)
}
loocv.lda[1,]=cv.error.lda

#Not sufficient since the number obs is too many


#K-Fold CV
kfold.glm = data.frame(poly1 = double(),
                       poly2 = double(),
                       poly3 = double(),
                       poly4 = double(),
                       poly5 = double(),
                       poly6 = double(),
                       poly7 = double(),
                       poly8 = double(),
                       poly9 = double(),
                       poly10 = double(),
                       stringsAsFactors=FALSE)

kfold.logi = data.frame(poly1 = double(),
                      poly2 = double(),
                      poly3 = double(),
                      poly4 = double(),
                      poly5 = double(),
                      poly6 = double(),
                      poly7 = double(),
                      poly8 = double(),
                      poly9 = double(),
                      poly10 = double(),
                      stringsAsFactors=FALSE)

kfold.lda = data.frame(poly1 = double(),
                       poly2 = double(),
                       poly3 = double(),
                       poly4 = double(),
                       poly5 = double(),
                       poly6 = double(),
                       poly7 = double(),
                       poly8 = double(),
                       poly9 = double(),
                       poly10 = double(),
                       stringsAsFactors=FALSE)

df = dfjpcorp[,c("lnat","capat","leverage","comage","lnsales","ebitdaat","dividend")]

for (j in 1:10){#set seed
  #GLM with polynomial (K-Fold Cross Validation)
  set.seed(j)
  cv.errork10=rep(0,10)
  for (i in 1:10){
    glm.fit=glm(cashat~poly(comage,i)+lnat+capat+leverage+lnsales+ebitdaat, data=dfjpcorp)
    # Specify K=10
    cv.errork10[i]=cv.glm(dfjpcorp,glm.fit,K=10)$delta[1]
  }
  cv.errork10
  kfold.glm[j,]=cv.errork10
  
  #base on the result of K-fold cross validation, comage with the power of 6 has the smallest MSE value.
  
  #dividend logistic model
  cv.error.logi=rep(0,10)
  for (i in 1:10){
    glm.fit.bi=glm(dividend~poly(comage,i)+lnat+capat+leverage+lnsales+ebitdaat, data=dfjpcorp,family=binomial)
    # Specify K=10
    cv.error.logi[i]=cv.glm(dfjpcorp,glm.fit.bi,K=10)$delta[1]
  }
  kfold.logi[j,]=cv.error.logi
}


#LDA with K-Fold Cross Validation
cv.lda <-
  function (data=df, model=dividend~., yname="dividend", K=10, seed=j) {
    n <- nrow(data)
    set.seed(seed)
    datay=data[,yname] #response variable
    library(MASS)
    #partition the data into K subsets
    f <- ceiling(n/K)
    s <- sample(rep(1:K, f), n)  
    #generate indices 1:10 and sample n of them  
    # K fold cross-validated error
    
    CV=NULL
    
    lda_error_rate=rep(0,10)
    for (i in 1:10) { #polynomials
      for (m in 1:K) { #n=1
        test.index <- seq_len(n)[(s == m)] #test data
        train.index <- seq_len(n)[(s != m)] #training data
        #model with training data
        lda.fit=lda(dividend~poly(comage,i)+lnat+capat+leverage+comage+lnsales+ebitdaat, data=data[train.index,])
        #observed test set y
        lda.y <- data[test.index, yname]
        #predicted test set y
        lda.predy=predict(lda.fit, data[test.index,])$class
        
        #observed - predicted on test data
        error= mean(lda.y!=lda.predy)
        #error rates 
        CV=c(CV,error)
      }
      lda_error_rate[i] = mean(CV)
      
    }
    #Output
    list(call = model, K = K, 
         lda_error_rate=lda_error_rate, seed = seed)  
  }

for (j in 1:10){
  er_lda=cv.lda(data=df,model=dividend~., yname="dividend", K=10, seed=j)
  kfold.lda[j,] = er_lda$lda_error_rate
}

kfold.glm
kfold.logi
kfold.lda

write.csv(kfold.glm,'kfold.glm.csv', row.names = TRUE)
write.csv(kfold.logi,'kfold.logi.csv', row.names = TRUE)
write.csv(kfold.lda,'kfold.lda.csv', row.names = TRUE)
