install.packages('lars')
library(lars)
###Diabetes is a dataset contain variables related to diabetes
attach(diabetes)
data(diabetes)
df <- diabetes
df
library("glmnet")
###GLMNET is used for lasso and ridge regeression 
library("mvtnorm") 

summary(x)
par(mfrow=c(2,5))
for(i in 1:10){
  plot(x[,i], y)
  abline(lm(y~x[,i]))
}
##Ploting all the graph between X and y.(As X contains 10 variables so loop goes for 1 to 10 for all the variables)

mod <- lm(y~x)
## Linear model by lm function.Y as a funtion of x

summary(mod)
### this will show all residuals and Coefficients.
### * values in Coefficients shows that they are highly correlated with the Y
### More the number of stars more significant the values is.
### . shows the significant values but lesser than * values
### Value here of R squared is near to 0.5 that means they are not highly correlated.As predictor jointly explain 51% of observed varience. 
model_las <- glmnet(x, y,alpha = 1)
### glmnet is used for LASSO and Ridge.Alpha 1 means LASSO and alpha 0 means Ridge.
plot.glmnet(model_las, xvar = "norm", label = TRUE)
### Ploting of model

fit <- cv.glmnet(x, y,alpha = 1)
### Cross validation of LASSO with the help of cv.glmnet function.
### USsed to get better value of lamdda for better fitting of equation

plot.cv.glmnet(fit)
### will show all the values of mean squred error as lambda increases.
fit$lambda.min
### Lambda min 

fit <- glmnet(x=x, y=y, alpha = 1, lambda=fit$lambda.min)
### LASSO regression using lambda value as the minimun value of lambda
fit$beta
### We can observe that with this lambda value few beta values has been changed and few are compresed to zero
plot(fit$beta)

x <- fit$beta
y <- coefficients(mod)
y
###Comparision with the linear model found that few values Coefficients has been compressed and few tends to zero
fit$lambda.1se
### Now try with new lambda with in one standard error
fit$lambda
fit2 <- glmnet(x=x, y=y, alpha = 1, lambda=fit$lambda.1se)
fit2$beta
### We can observe that with this lambda value few beta values has been changed and few are compresed to zero.But we can observe the differnce
### Now only few most significant values are left.this will reduce complexity but meanwhile increases mean sqaure error if we use 
### this model on train data and predict on test data



model <- lm(y~x2)
## Linear model by lm function.Y as a funtion of X2
summary(model)
### this will show all residuals and Coefficients.
### * values in Coefficients shows that they are highly correlated with the Y
### More the number of stars more significant the values is.
### . shows the significant values but lesser than * values
### Multipe R squared and Adjusted R squared shows that how much they are correlated.
### Value here is near to 0.5 that means they are not highly correlated
model_lasso <- glmnet(x2, y)
### glmnet is used for LASSO and Ridge.Alpha 1 means LASSO and alpha 0 means Ridge.
plot.glmnet(model_lasso, xvar = "norm", label = TRUE)
### Ploting of model
cv_fit <- cv.glmnet(x2, y,alpha = 1, nlambda = 1000)

plot.cv.glmnet(cv_fit)
cv_fit$lambda.min
### Cross validation of LASSO with the help of cv.glmnet function.
### USsed to get better value of lamdda for better fitting of equation
fit <- glmnet(x=x2, y=y, alpha = 1, lambda=cv_fit$lambda.min)
fit$beta
### We can observe that with this lambda value few beta values has been changed and few are compresed to zero
cv_fit$lambda.1se
### Now try with new lambda with in one standard error
fit <- glmnet(x=x, y=y, alpha = 1, lambda=cv_fit$lambda.1se)
fit$beta
### We can observe that with this lambda value few beta values has been changed and few are compresed to zero.But we can observe the differnce
### Now only few most significant values are left.this will reduce complexity but meanwhile increases mean sqaure error if we use 
### this model on train data and predict on test data






###############################PCA##################################################
###PCA

data <- mtcars
head(data)
count(data)
##mtcars is the data set contain list of cars with their specifications.

class(mtcars$mpg)
##feature scaling is the process to standarize the range of independent variable or feature of data

##In this case feature scaling is being done on every variables except cyl
##[-2] implies that all the elements expect the element which is present at the 2nd postion
data[-2]=scale(data[-2])

###
### Princomp is the function perform a principle component analysis on the given numeric data and return the result as an object
### In this this funcion is applied on all the elements except cyl variable
### Score TRUE means the score of the supplied data on the pricipal component.

pc <- princomp(x=data[-2],cor = TRUE,score=TRUE)
### Cor =TRUE :- When cor=TRUE the cv (covariance matrix) is divided by the outer product of the matrix that is the cv/(sds %o% sds)
pc1 <- princomp(x=data[-2],cor = FALSE,score=TRUE)
### Cor =FALSE : outer product division does not take place sp values of components are different 
summary(pc)
summary(pc1)
### Principal component values are different because of cv/(sds %o% sds)
### When run with FLASE it computes eigenvectors of the covariance matrix, centers the data, and projects the data onto the eigenvectors. 
### When run with  cor=TRUE, the function computes eigenvectors of the correlation matrix, z-scores the data using variances computed with 1/n factor, 
### and projects the data onto the eigenvectors.
### ploting of principal component when cor=TRUE.
plot(pc,type='l')
### ploting of principal component when cor=FALSE.
plot(pc1,type='l')

library(devtools)
install_github("ggbiplot", "vqv")

library(ggbiplot)
g <- ggbiplot(pc, obs.scale = 1, var.scale = 1, 
              groups = as.factor(mtcars$cyl), ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)
### Graph is being plotted between PC1 and PC2 Where PC1 is 57% and PC2 is PC2 is 26%.
### Green color ecliplse is for 6 cyl where most corelatted elements/most influnsing factors are gear,am,drat,mpg,vs,qsec
### Blue color ecliplse is for 4 cyl where most corelatted elements/most influnsing factors are wt,disp,hp,carb
### Red color eclipse is for 4 cyl where most corelatted elements/most influnsing factors are vs,mpg

### Without PCA it would have been difficult to analyze this dataset on a whole.But due to PCA algorithm 11 varables 
###can be analyzed in 2 D.We can interpret most of our reslut









