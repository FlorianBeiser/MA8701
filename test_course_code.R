library(nortest)
ds = read.table("sniffer.dat", header = T)
# data2 = read.table("https://raw.githubusercontent.com/mettelang/MA8701V2021/main/Part1/sniffer.dat")
x = apply(ds[, -5], 2, scale)
y = ds[, 5] - mean(ds[, 5])
print(dim(x))
dss = data.frame(y, x)
ggpairs(dss)
print(cov(dss))
print(cor(dss))

full = lm(y ~ ., dss)
print(summary(full))
print(confint(full))
ggplot(full, aes(.fitted, .stdresid)) + geom_point(pch = 21) + geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_smooth(se = FALSE, col = "red", size = 0.5, method = "loess") + labs(x = "Fitted values", y = "Standardized residuals", 
                                                                              title = "Fitted values vs standardized residuals", subtitle = deparse(full$call))
ggplot(full, aes(sample = .stdresid)) + stat_qq(pch = 19) + geom_abline(intercept = 0, slope = 1, linetype = "dotted") + 
    labs(x = "Theoretical quantiles", y = "Standardized residuals", title = "Normal Q-Q", subtitle = deparse(full$call))

print(ad.test(rstudent(full)))

# Perform best subset selection using Cp
bests = regsubsets(x, y)
sumbests = summary(bests)
print(sumbests)
print(which.min(sumbests$cp))

# since it has found the model
red = lm(y ~ GasTemp + TankPres + GasPres, data = dss)
print(summary(red))

start = glmnet(x = x, y = y, alpha = 0)
autolambda = start$lambda
newlambda = c(autolambda, 0.5, 0.3, 0.2, 0.1)
fit.ridge = glmnet(x, y, alpha = 0, lambda = newlambda)

plot(fit.ridge, xvar = "lambda", label = TRUE)
plot(fit.ridge, xvar = "norm", label = TRUE)

cv.ridge = cv.glmnet(x, y, alpha = 0, lambda = newlambda)
print(cv.ridge)
print(paste("The lambda giving the smallest CV error", cv.ridge$lambda.min))
print(paste("The 1st err method lambda", cv.ridge$lambda.1se))
plot(cv.ridge)

# use 1sd error rule default
plot(fit.ridge, xvar = "lambda", label = TRUE)
abline(v = log(cv.ridge$lambda.1se))
coef(cv.ridge)
full$coeff
red$coeff

x = c(-10:10)
y = (x -2 ) ^ 2 - 200 ^ 2
plot(x, y)

# test of south african heart disease
ds = read.table("https://raw.githubusercontent.com/mettelang/MA8701V2021/main/Part1/SAheart.data", sep = ",", header = T)[, -1]
ds$chd = as.factor(ds$chd)
ds$famhist = as.factor(ds$famhist)
dim(ds)
colnames(ds)
head(ds)

# standardize xs
xs = model.matrix(chd~., data = ds)[, -1]
xss = scale(xs)
ys = as.numeric(ds[, 10]) - 1
head(xss)
head(ys)
head(xs)
print(ys)
print(as.numeric(ds[, 10]) - 1)
table(ys)

dss = data.frame(ys, xss)
colnames(dss)[1] = "chd"
apply(dss, 2, sd)
apply(dss, 2, mean)
ggpairs(dss)


glm_heart = glm(chd~., data = dss, family = "binomial")
summary(glm_heart)
exp(coef(glm_heart))

ridgefit = glmnet(x = xss, y = ys, alpha = 0, standardize = F, family = "binomial")
plot(ridgefit, xvar = "lambda", label = T)

# cross validation
cv.ridge = cv.glmnet(x = xss, y = ys, alpha = 0, standardize = F, family = "binomial")
print(paste("The lambda giving the smallest CV error", cv.ridge$lambda.min))
print(paste("The 1st err method lambda", cv.ridge$lambda.1se))
plot(cv.ridge)
plot(ridgefit, xvar = "lambda", label = T)
abline(v = log(cv.ridge$lambda.1se))
print(cbind(coef(ridgefit, s = cv.ridge$lambda.min), coef(glm_heart)))
print(cbind(coef(ridgefit, s = cv.ridge$lambda.1se), coef(glm_heart)))

cbind(1:9, colnames(xss))
lassofit = glmnet(x = xss, y = ys, alpha = 1, standardize = F, family = "binomial")
plot(lassofit, xvar = "lambda", label = T)

cv.lasso = cv.glmnet(x = xss, y = ys, alpha = 1, standardize = F, family = "binomial")
print(paste("The lambda giving the smallest CV error", cv.lasso$lambda.min))
print(paste("The 1sd err method lambda", cv.lasso$lambda.1se))
plot(cv.lasso)
plot(lassofit, xvar = "lambda", label = T)
abline(v = log(cv.lasso$lambda.1se))
resmat = cbind(coef(lassofit, s = cv.lasso$lambda.1se), coef(ridgefit, s = cv.ridge$lambda.1se), coef(glm_heart))
colnames(resmat) = c("lasso logistic", "ridge logistic", "logistic")
print(resmat)

diabetes = read.table("https://web.stanford.edu/~hastie/Papers/LARS/diabetes.data", header = T)
attach(diabetes)
library(regress)
reg.ols = regress(x, y)



library(GGally)
library(ggplot2)
data(diamonds, package = "ggplot2")
diamonds.samp <- diamonds[sample(1:dim(diamonds)[1], 200), ]
ggpairs(
    diamonds.samp[, 1:2], 
    mapping = ggplot2::aes(color = cut), 
    upper = list(continuous = wrap("density", alpha = 0.5), combo = "box"), 
    lower = list(continuous = wrap("points", alpha = 0.3), combo = wrap("dot", alpha = 0.4)), 
    diag = list(continuous = wrap("densityDiag")), 
    title = "Diamonds"
)




y <- c(1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 
       0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 
       1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0)
weight <- c(2.1, 2.5, 1.2, 1, 3, 2.1, 1.5, 2.2, 1.9, 2.7, 1.1, 2.9, 1.2, 2.1, 
            2.2, 2.5, 1.9, 1.2, 2, 2.9, 2.2, 1.5, 3, 2.4, 1.2, 1.6, 2.3, 2.1, 
            2.6, 2.4, 2.5, 2, 1, 1.4, 2.9, 1.5, 3, 2.9, 2.9, 2.1, 2.8, 2.7, 1, 
            2.9, 1.1, 2.2, 1.3, 1.7, 1.5, 1.7)

plot(x = weight, y,, yaxt = "n", pch = 20)
axis(side = 2, at = 0:1, labels = 0:1, las = 1)

mod_sat = glm(y ~ as.factor(1:length(y)), family = binomial)
mod_nul = glm(y ~ 1, family = binomial)

mod = glm(y ~ weight, family = binomial)

print(2 * (logLik(mod_sat) - logLik(mod)))
print(2 * (logLik(mod_sat) - logLik(mod_nul)))

print(summary(mod))
print(summary(mod) $ null.deviance)
print(summary(mod) $ deviance)
deviance(mod)


x1 = x[,1]
x2 = x[,2]
cov_x = sum((x1 - mean(x1)) * (x2 - mean(x2))) / (length(x1) - 1) / sd(x1) / sd(x2)
print(cov_x)
print(sd(x1))
print(sd(x2))



a = c(1:12)
mean_a = mean(a)
re_a = a - mean_a
res = re_a / sd(a)
r_t = sqrt(sum(re_a ^ 2) / (length(a) - 1))
rr_t = re_a / r_t
print(res)
print(scale(a))



