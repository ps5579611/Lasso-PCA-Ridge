library(ISLR)
Hitters
x=model.matrix(Salary ~.,Hitters)[,-1]
y=na.omit(Hitters$Salary)

library(glmnet)
grid = 10^seq(10,-2,length=100)
ridge.mod = glmnet(x,y,alpha = 0,lambda = grid)
predict(ridge.mod,s=50,type = "coefficients")[1:20,]

set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test = (-train)
y.test=y[test]

set.seed(1)
cv.out <- cv.glmnet(x[train,],y[train],alpha=0)
bestlam =cv.out$lambda.min
bestlam
ridge.pred <- predict(ridge.mod,s=bestlam,newx = x[test,])
mean((ridge.pred-y.test)^2)
out <- glmnet(x,y,alpha = 0)
predict(out,type="coefficients",s=bestlam)[1:20,]


lasso.mod = glmnet(x[train,],y[train],alpha=1,lambda = grid)
set.seed(1)
cv.out <- cv.glmnet(x[train,],y[train],alpha=1)
bestlam =cv.out$lambda.min
bestlam
lasso.pred <- predict(lasso.mod,s=bestlam,newx = x[test,])
mean((lasso.pred-y.test)^2)

out <- glmnet(x,y,alpha = 1,lambda = grid)
lasso.coef <- predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]

install.packages('pls')
library(pls)
set.seed(2)
pcr.fit= pcr(Salary~.,data=Hitters,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type = "MSEP")

set.seed(1)
pcr.fit <- pcr(Salary~.,data=Hitters,subset=train,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type = "MSEP")
pcr.pred <- predict(pcr.fit,x[test,],ncomp = 10)
##Number of comp will change meas squared error
mean((pcr.pred-y.test)^2)

#######################PLSR
set.seed(1)
plsr.fit <- plsr(Salary~.,data=Hitters,subset=train,scale=TRUE,validation="CV")

summary(plsr.fit)
validationplot(plsr.fit,val.type = "MSEP")
plsr.pred <- predict(plsr.fit,x[test,],ncomp = 4)
##Number of comp will change meas squared error
mean((plsr.pred-y.test)^2)








