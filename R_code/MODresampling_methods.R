
attach(data)
n = nrow(data)

######### Validation Set Approach #########

set.seed(1)
train=sample(1:n,n/2)
lm.fit=lm(fit.linear, data = data, subset = train)

mean(((final_vote-predict(lm.fit,data))[-train])^2)

# use the poly() function to estimate the test error for the polynomials-2 transformation.
lm.fit2=lm(fit.poly2.complete, data = data, subset=train)

mean(((final_vote-predict(lm.fit2,data))[-train])^2)

# use the poly() function to estimate the test error for the polynomials-3 transformation.
lm.fit3=lm(fit.poly3, data = data, subset=train) 

mean(((final_vote-predict(lm.fit3,data))[-train])^2)

# use the poly() function to estimate the test error for the polynomials-4 transformation.
lm.fit4=lm(fit.poly4, data = data, subset=train) 

mean(((final_vote-predict(lm.fit4,data))[-train])^2)

# use the poly() function to estimate the test error for the log transformation.
lm.fit5=lm(fit.log, data = data, subset=train) 

mean(((final_vote-predict(lm.fit5,data))[-train])^2)

## Changing the seed, the results remain consistent with our previous findings


########## K-Fold Cross Validation ##########
library(boot)

glm.fit=glm(fit.poly4 ,data=data)

cv.err=cv.glm(data,glm.fit, K = 10)
cv.err$delta 

# K-Fold Cross validation for polynomial regressions with orders i=1,2,...,4.

cv.error=rep(0,4)
for (i in 1:4){
  glm.fit=glm(final_vote~approved+poly(comfort_score,i)+poly(service_score,i)
              +poly(food_score,i)+poly(enjoyment_score,i)+poly(station_score,i)
              +poly(internet_connection_score,i)+poly(expendiency_score,i), data = data)
  cv.error[i]=cv.glm(data,glm.fit, K=10)$delta[1]
}
cv.error
# We still see little evidence that using cubic or higher-order polynomial terms leads to lower test error than simply


########## Bootstrap ##########

# The boot.fn() function can also be used in order to create bootstrap estimates 
# for the intercept and slope terms by randomly sampling from among the observations with replacement
# We will compare the estimates obtained using the bootstrap to those obtained using the previous models
library(stringr)

# No-transformation
set.seed (2)
boot.fn=function(data,index){
  return(coef(lm(final_vote~approved+comfort_score+service_score+food_score+enjoyment_score+station_score
                 +internet_connection_score+expendiency_score, data = data,subset=index)))
}
boot.fn(data, 1:n)

# Boot estimate is not deterministic
boot.fn(data,sample(1:n, 79576,replace=T))
boot.fn(data,sample(1:n, 79576,replace=T))
# We use the boot() function to compute the standard errors 
# of 1,000 bootstrap estimates for the intercept and slope terms.
b = boot(data ,boot.fn ,1000)

s = summary(lm(fit.linear, data = data))

# Take all std. errors of the bootstrap estimate 
x <- capture.output(b)
x <- str_extract(x, "^t[0-9.]+.*$")
x <- x[!is.na(x)]
se <- as.numeric(unlist(str_extract_all(x, '[0-9.]+$')))

# Take all std. errors of the linear model
c = s$coefficients[ ,2]
c = as.numeric(c)

cat("\nDifference between no-Transformation Std.errors:\n",c - se,"\n")


# Polinomials-2 no-linear transformation
set.seed (2)

temp_val = 0
boot.fn=function(data,index){
  temp_val = index
  return(coef(lm(final_vote~approved+poly(comfort_score,2)+poly(service_score,2)
                 +poly(food_score,2)+poly(enjoyment_score,2)+poly(station_score,2)
                 +poly(internet_connection_score,2)+poly(expendiency_score,2),data = data,subset=temp_val)))
}
boot.fn(data, 1:n)

boot.fn(data,sample(1:n, 79576,replace=T))

# We use the boot() function to compute the standard errors 
# of 1,000 bootstrap estimates for the intercept and slope terms.

b = boot(data ,boot.fn ,1000)

s = summary(lm(fit.poly2.complete,data = data))

# Take all std. errors of the bootstrap estimate 
x <- capture.output(b)
x <- str_extract(x, "^t[0-9.]+.*$")
x <- x[!is.na(x)]
se <- as.numeric(unlist(str_extract_all(x, '[0-9.]+$')))

# Take all std. errors of the poly-2 transformation
c = s$coefficients[ ,2]
c = as.numeric(c)

cat("\nDifference between poly-2 transformation Std.errors:\n",c - se,"\n")

# Polinomials-3 no-linear transformation
set.seed (2)

temp_val = 0
boot.fn=function(data,index){
  temp_val = index
  return(coef(lm(final_vote~approved+poly(comfort_score,3)+poly(service_score,3)
                  +poly(food_score,3)+poly(enjoyment_score,3)+poly(station_score,3)
                  +poly(internet_connection_score,3)+poly(expendiency_score,3), data = data, subset=temp_val)))
}

boot.fn(data, 1:n)

boot.fn(data,sample(1:n, 79576,replace=T))

b = boot(data ,boot.fn ,1000)

s = summary(lm(fit.poly3, data = data))

# Take all std. errors of the bootstrap estimate 
x <- capture.output(b)
x <- str_extract(x, "^t[0-9.]+.*$")
x <- x[!is.na(x)]
se <- as.numeric(unlist(str_extract_all(x, '[0-9.]+$')))

# Take all std. errors of the poly-3 transformation
c = s$coefficients[ ,2]
c = as.numeric(c)

cat("\nDifference between poly-3 transformation Std.errors:\n",c - se,"\n")

# Polinomials-4 no-linear transformation
set.seed (2)

boot.fn=function(data,index){
  temp_val = index
  return(coef(lm(final_vote~approved+poly(comfort_score,4)+poly(service_score,4)
                 +poly(food_score,4)+poly(enjoyment_score,4)+poly(station_score,4)
                 +poly(internet_connection_score,4)+poly(expendiency_score,4), data = data,subset=temp_val)))
}

boot.fn(data, 1:n)


boot.fn(data,sample(1:n, 79576,replace=T))

boot(data ,boot.fn ,1000)

summary(lm(fit.poly4, data = data))

# Take all std. errors of the bootstrap estimate 
x <- capture.output(b)
x <- str_extract(x, "^t[0-9.]+.*$")
x <- x[!is.na(x)]
se <- as.numeric(unlist(str_extract_all(x, '[0-9.]+$')))

# Take all std. errors of the poly-4 transformation
c = s$coefficients[ ,2]
c = as.numeric(c)

cat("\nDifference between poly-4 transformation Std.errors:\n",c - se,"\n")


######## Plot ########

## Plot linear model
dev.new()
plot(as.factor(comfort_score),final_vote,main='Linear Model', xlab="seat comfort", ylab="final_vote")
xx=seq(min(comfort_score),max(comfort_score),along.with = comfort_score)
ci_lin <- predict(lm(final_vote~comfort_score,data=data),newdata=data.frame(comfort_score=xx),se.fit = T,interval = "confidence")
matplot(xx,ci_lin$fit[,1],lty=1, ltw=2, col="red", type="l", add=T)

## Plot linear model with polinomials-2 transformation
dev.new()
plot(as.factor(comfort_score),final_vote,main='Polinomial-2 Model', xlab="seat comfort", ylab="final_vote")
xx=seq(min(comfort_score),max(comfort_score),along.with = comfort_score)
ci_lin <- predict(lm(final_vote~I(comfort_score^2),data=data),newdata=data.frame(comfort_score=xx),se.fit = T,interval = "confidence")
matplot(xx,ci_lin$fit[,1],lty=1, ltw=2, col="red", type="l", add=T)

## Plot linear model with polinomials-3 transformation
dev.new()
plot(as.factor(comfort_score),final_vote,main='Polinomial-3 Model', xlab="seat comfort", ylab="final_vote")
xx=seq(min(comfort_score),max(comfort_score),along.with = comfort_score)
ci_lin <- predict(lm(final_vote~I(comfort_score^3),data=data),newdata=data.frame(comfort_score=xx),se.fit = T,interval = "confidence")
matplot(xx,ci_lin$fit[,1],lty=1, ltw=2, col="red", type="l", add=T)

## Plot linear model with polinomials-4 transformation
dev.new()
plot(as.factor(comfort_score),final_vote,main='Polinomial-4 Model', xlab="seat comfort", ylab="final_vote")
xx=seq(min(comfort_score),max(comfort_score),along.with = comfort_score)
ci_lin <- predict(lm(final_vote~I(comfort_score^4),data=data),newdata=data.frame(comfort_score=xx),se.fit = T,interval = "confidence")
matplot(xx,ci_lin$fit[,1],lty=1, ltw=2, col="red", type="l", add=T)

