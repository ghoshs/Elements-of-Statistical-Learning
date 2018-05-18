##### PROBLEM 4 #####
#####################

library(ISLR)
str(Auto)

# a.
library(dplyr)
Autocor <- select(Auto, mpg:origin)
auto_cor <- cor(Autocor) 
# There is a high correlation between (>|0.8|) the following pairs:
# mpg - displacement
# mpg - weight
# cylinders - displacement
# cylinders - horsepower
# cylinders - weight
# displacement - horsepower
# displacement - weight
# There may be some dependency in the input variables.

#b. 
#jpeg("scatter_plots.jpg")
pairs(~mpg+displacement+horsepower+weight+cylinders, data=Auto)
#dev.off()
#c.
model1 <- lm(mpg~cylinders, data = Auto)
model2 <- lm(mpg~displacement, data=Auto)
model3 <- lm(mpg~horsepower, data=Auto)
model4 <- lm(mpg~year, data=Auto)
summary(model1)
#            Estimate Std. Error t value p value
#(Intercept)  42.9155     0.8349   51.40   <2e-16 ***
#cylinders    -3.5581     0.1457  -24.43   <2e-16 ***
#Multiple R-squared:  0.6047,	Adjusted R-squared:  0.6037 
summary(model2)
#              Estimate Std. Error t value p value    
# (Intercept)  35.12064    0.49443   71.03   <2e-16 ***
# displacement -0.06005    0.00224  -26.81   <2e-16 ***
# Multiple R-squared:  0.6482,	Adjusted R-squared:  0.6473 
summary(model3)
#               Estimate Std. Error t value p value    
#  (Intercept) 39.935861   0.717499   55.66   <2e-16 ***
#  horsepower  -0.157845   0.006446  -24.49   <2e-16 ***
#Multiple R-squared:  0.6059,	Adjusted R-squared:  0.6049
summary(model4)
#               Estimate Std. Error t value p value    
#  (Intercept) -70.01167    6.64516  -10.54   <2e-16 ***
#  year          1.23004    0.08736   14.08   <2e-16 ***
#Multiple R-squared:  0.337,	Adjusted R-squared:  0.3353 

# Since higher R-squared implies an accurate model, cylinders, 
#displacement and horsepower may have sigmificant relation to mpg.

#d.
model <- lm(mpg~cylinders+displacement+horsepower+year+weight+acceleration+origin, data=Auto)
summary(model)
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -17.218435   4.644294  -3.707  0.00024 ***
# cylinders     -0.493376   0.323282  -1.526  0.12780    
# displacement   0.019896   0.007515   2.647  0.00844 ** 
# horsepower    -0.016951   0.013787  -1.230  0.21963    
# year           0.750773   0.050973  14.729  < 2e-16 ***
# weight        -0.006474   0.000652  -9.929  < 2e-16 ***
# acceleration   0.080576   0.098845   0.815  0.41548    
# origin         1.426141   0.278136   5.127 4.67e-07 ***
# Residual standard error: 3.328 on 384 degrees of freedom
# Multiple R-squared:  0.8215,	Adjusted R-squared:  0.8182 
# F-statistic: 252.4 on 7 and 384 DF,  p-value: < 2.2e-16
# Not only the R squale value increases, the significance in relationship
# of predictors to output also changes.
# Predictors weight, origin and year are most significant, then displacement.
# Cylinders, acceleration and horsepowers have the least significant relationship.

#e.
jpeg("model_diagnostic.jpg")
par(mfrow=c(2, 2))
plot(model)
dev.off()
# Data points 323 and 330 can be the outliers, though they do not have
# unusually large values. There is one leverage point in the dataset.

#f.
lm1 <- lm(mpg~cylinders+year+weight+cylinders:year+
            cylinders:weight+year:weight+log(displacement), data=Auto)
lm2 <- lm(mpg~cylinders+year+weight+cylinders:year+
            cylinders:weight+year:weight+I(displacement^0.5), data=Auto)
lm3 <- lm(mpg~cylinders+year+weight+cylinders:year+
            cylinders:weight+year:weight+I(displacement^2), data=Auto)

summary(lm1)
# Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -5.223e+01  1.460e+01  -3.578  0.00039 ***
# cylinders          1.571e+00  4.386e+00   0.358  0.72030    
# year               1.618e+00  1.757e-01   9.207  < 2e-16 ***
# weight             2.383e-03  1.006e-02   0.237  0.81282    
# log(displacement) -2.801e+00  1.297e+00  -2.159  0.03147 *  
# cylinders:year    -5.805e-02  5.782e-02  -1.004  0.31605    
# cylinders:weight   1.000e-03  2.007e-04   4.985  9.4e-07 ***
# year:weight       -1.873e-04  1.269e-04  -1.476  0.14082    
# Residual standard error: 3.005 on 384 degrees of freedom
# Multiple R-squared:  0.8545,	Adjusted R-squared:  0.8518 
# F-statistic: 322.1 on 7 and 384 DF,  p-value: < 2.2e-16

summary(lm2)
# Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         -5.840e+01  1.471e+01  -3.969 8.62e-05 ***
# cylinders            9.789e-01  4.391e+00   0.223    0.824    
# year                 1.613e+00  1.763e-01   9.145  < 2e-16 ***
# weight               1.398e-03  1.008e-02   0.139    0.890    
# I(displacement^0.5) -3.267e-01  1.910e-01  -1.711    0.088 .  
# cylinders:year      -5.689e-02  5.831e-02  -0.976    0.330    
# cylinders:weight     1.138e-03  1.811e-04   6.284 8.98e-10 ***
# year:weight         -1.878e-04  1.274e-04  -1.474    0.141    
# Residual standard error: 3.011 on 384 degrees of freedom
# Multiple R-squared:  0.8538,	Adjusted R-squared:  0.8511 
# F-statistic: 320.4 on 7 and 384 DF,  p-value: < 2.2e-16

summary(lm3)
# Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -5.527e+01  1.473e+01  -3.752 0.000203 ***
# cylinders         -1.012e+00  4.309e+00  -0.235 0.814508    
# year               1.582e+00  1.777e-01   8.898  < 2e-16 ***
# weight             1.776e-03  1.017e-02   0.175 0.861494    
# I(displacement^2) -1.851e-06  1.002e-05  -0.185 0.853635    
# cylinders:year    -3.889e-02  5.864e-02  -0.663 0.507580    
# cylinders:weight   1.222e-03  1.926e-04   6.346 6.22e-10 ***
# year:weight       -2.065e-04  1.277e-04  -1.617 0.106708    
# Residual standard error: 3.023 on 384 degrees of freedom
# Multiple R-squared:  0.8527,	Adjusted R-squared:   0.85 
# F-statistic: 317.6 on 7 and 384 DF,  p-value: < 2.2e-16