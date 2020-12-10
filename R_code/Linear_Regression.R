# Linear regression - multiple regressor, without regularization 
library(tidyverse)
library(caret)
attach(data)

# Open a new windows
dev.new()

# Check Trend between final_vote and regressors

par(mfrow=c(3,2))
plot(comfort_score, final_vote, main="final_vote-regressors trend"); plot(as.factor(comfort_score), final_vote,main="final_vote-regressors trend",xlab="seat comfort", ylab="final_vote")
plot(service_score, final_vote); plot(as.factor(service_score), final_vote, xlab="service_score", ylab="final_vote")
plot(food_score, final_vote); plot(as.factor(food_score), final_vote, xlab="food_score", ylab="final_vote")

dev.new()
par(mfrow=c(4,2))
plot(enjoyment_score, final_vote,main="final_vote-regressors trend"); plot(as.factor(enjoyment_score), final_vote,main="final_vote-regressors trend", xlab="enjoyment_score", ylab="final_vote")
plot(station_score, final_vote); plot(as.factor(station_score), final_vote, xlab="station_score", ylab="final_vote")
plot(internet_connection_score, final_vote); plot(as.factor(internet_connection_score), final_vote, xlab="internet_connection_score", ylab="final_vote")
plot(expendiency_score, final_vote); plot(as.factor(expendiency_score), final_vote, xlab="expendiency_score", ylab="final_vote")
# The trend is not linear between final_vote and regressors

# Linear model with final_vote response and others parameters are regressors

fit <- lm(fit.linear, data = data)

summary(fit)
car::vif(fit)

# It shows confidence interval of variables
confint(fit)
dev.new()
par(mfrow=c(2,2))
plot(fit)

# The trend is not linear, so we fit polinomials-2 no-linear transformation.

fit2 <- lm(fit.poly2.complete ,data = data)

summary(fit2)
car::vif(fit2)

confint(fit2)
dev.new()
par(mfrow=c(2,2))
plot(fit2)
# Compare the previous model "fit" with model "fit2"
anova(fit,fit2)

# Fit polinomials-3, no-linear transformation

fit3 <- lm(fit.poly3, data = data)

summary(fit3)
confint(fit3)
car::vif(fit3)
dev.new()
par(mfrow=c(2,2))
plot(fit3)
anova(fit2,fit3)

# Fit log transformation

fit5 <- lm(fit.log , data = data)

summary(fit5)
confint(fit5)
car::vif(fit5)
dev.new()
par(mfrow=c(2,2))
plot(fit5)
anova(fit3,fit5)

# Polinomials-4 no-linear transformation

fit4 <- lm(fit.poly4, data = data)

summary(fit4)
car::vif(fit4)
anova(fit3,fit4)
dev.new()
par(mfrow=c(2,2))
plot(fit4)


# The best transformation is polinomials-4, non-linear transformation