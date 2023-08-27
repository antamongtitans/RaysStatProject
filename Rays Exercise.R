path <- "data_for_exercise.csv"

content <- read.csv(path)
content2 = subset(content, select = -c(X))

cor(content2$home_win_pct, content2$attendance)
cor(content2$away_win_pct, content2$attendance)
cor(content2$opposing_team, content2$attendance)
cor(content2$day_of_week_effect, content2$attendance)
cor(content2$temp, content2$attendance)
cor(content2$pct_season_completed, content2$attendance)
cor(content2$is_bobblehead, content2$attendance)
cor_matrix <-cor(content2)
cor_matrix

scatter.smooth(x=content2$home_win_pct, y=content2$attendance, main="Home Winning Percentage ~ Attendance")

par(mfrow=c(1, 2))
boxplot(content2$home_win_pct, main="Home Winning Percentage", sub=paste("Outlier rows: ", boxplot.stats(content2$home_win_pct)$out))
boxplot(content2$attendance, main="Attendance", sub=paste("Outlier rows: ", boxplot.stats(content2$attendance)$out))
print(boxplot.stats(content2$attendance)$out)
print(boxplot.stats(content2$home_win_pct)$out)

outliers1 <- boxplot.stats(content2$home_win_pct)$out
outliers2 <- boxplot.stats(content2$attendance)$out

content3 <- content2[ !content2$home_win_pct %in% outliers1, ]
content4 <- content3[ !content3$attendance %in% outliers2, ]

cor_matrix2 <-cor(content4)
cor_matrix2

linearMod1 <- lm(attendance ~ home_win_pct, data=content2)
linearMod2 <- lm(attendance ~ home_win_pct, data=content4)
print(linearMod1)
print(linearMod2)

summary(linearMod1)
summary(linearMod2)

AIC(linearMod1)
AIC(linearMod2)
BIC(linearMod1)
BIC(linearMod2)

trainingRowIndex <- sample(1:nrow(content2), 0.8*nrow(content2))
trainingContent <- content2[trainingRowIndex, ]
testContent  <- content2[-trainingRowIndex, ]

linearMod3 <- lm(attendance ~ home_win_pct, data=trainingContent)
attendancePred3 <- predict(linearMod3, testContent)
print(linearMod3)
summary(linearMod3)
AIC(linearMod3)
BIC(linearMod3)

actuals_preds3 <- data.frame(cbind(actuals=testContent$attendance, predicteds=attendancePred3))
correlation_accuracy3 <- cor(actuals_preds3)
correlation_accuracy3
head(actuals_preds3)

attendancePred1 <- predict(linearMod1, testContent)
attendancePred2 <- predict(linearMod2, testContent)
actuals_preds1 <- data.frame(cbind(actuals=testContent$attendance, predicteds=attendancePred1))
actuals_preds2 <- data.frame(cbind(actuals=testContent$attendance, predicteds=attendancePred2))
head(actuals_preds1)
head(actuals_preds2) 
correlation_accuracy1 <- cor(actuals_preds1)
correlation_accuracy1
correlation_accuracy2 <- cor(actuals_preds2)
correlation_accuracy2


y <- content2$attendance
x1 <- data.matrix(content2[,c('home_win_pct','away_win_pct','opposing_team','day_of_week_effect','temp','pct_season_completed','is_bobblehead')])

install.packages("glmnet", repos = "https://cran.us.r-project.org")
library(glmnet)
model1 <- glmnet(x1, y, alpha = 0)

summary(model1)

cv_model1 <- cv.glmnet(x1, y, alpha = 0)
best_lambda1 <- cv_model1$lambda.min
best_lambda1

plot(cv_model1)

best_model1 <- glmnet(x1, y, alpha = 0, lambda = best_lambda1)
coef(best_model1)

plot(model1, xvar = "lambda")

y_predicted1 <- predict(model1, s = best_lambda1, newx = x1)
sst1 <- sum((y - mean(y))^2)
sse1 <- sum((y_predicted1 - y)^2)
rsq1 <- 1 - sse1/sst1
rsq1

x2 <- data.matrix(content2[,c('home_win_pct','away_win_pct','day_of_week_effect','temp','pct_season_completed','is_bobblehead')])
model2 <- glmnet(x2, y, alpha = 0)

summary(model2)

cv_model2 <- cv.glmnet(x2, y, alpha = 0)
best_lambda2 <- cv_model2$lambda.min
best_lambda2

plot(cv_model2)

best_model2 <- glmnet(x2, y, alpha = 0, lambda = best_lambda2)
coef(best_model2)

plot(model2, xvar = "lambda")

y_predicted2 <- predict(model2, s = best_lambda2, newx = x2)
sst2 <- sum((y - mean(y))^2)
sse2 <- sum((y_predicted2 - y)^2)
rsq2 <- 1 - sse2/sst2
rsq2

x3 <- data.matrix(content2[,c('home_win_pct','away_win_pct','temp','pct_season_completed','is_bobblehead')])
model3 <- glmnet(x3, y, alpha = 0)

summary(model3)

cv_model3 <- cv.glmnet(x3, y, alpha = 0)
best_lambda3 <- cv_model3$lambda.min
best_lambda3

plot(cv_model3)

best_model3 <- glmnet(x3, y, alpha = 0, lambda = best_lambda3)
coef(best_model3)

plot(model3, xvar = "lambda")

y_predicted3 <- predict(model3, s = best_lambda3, newx = x3)
sst3 <- sum((y - mean(y))^2)
sse3 <- sum((y_predicted3 - y)^2)
rsq3 <- 1 - sse3/sst3
rsq3

cv_model4 <- cv.glmnet(x1, y, alpha = 1)
best_lambda4 <- cv_model4$lambda.min
best_lambda4

plot(cv_model4) 

best_model4 <- glmnet(x1, y, alpha = 1, lambda = best_lambda4)
coef(best_model4)

y_predicted4 <- predict(best_model4, s = best_lambda4, newx = x1)
sst4 <- sum((y - mean(y))^2)
sse4 <- sum((y_predicted4 - y)^2)
rsq4 <- 1 - sse4/sst4
rsq4

