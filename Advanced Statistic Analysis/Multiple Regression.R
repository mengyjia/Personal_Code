# Let's see what factors will influence the Number of weeks of severance pay
#data dictionary:
#WeekSP :Number of weeks of severance pay 
#Age : Age of employee 
#Years : Number of years with the company 
#Pay : Annual pay (in thousands of dollars)



### Step 1: Develop a model on theoretical basis
### Identify the dependent (response) and independent (predictor/explanatory) variables.

### Weeks of SP = f(Age, Years, AnnualPay)
### Weeks_SP = beta0 + beta1*Age + beta2*Years + beta3*AnnualPay + error

### Read the data
Pay <- read.csv("Pay.csv")

### Step 2: Observe whether there is a linear relationship between each explanatory (independent)
### variable and the response (dependent) variable. Look for bending patterns.
### Bending patterns between explanatory variables and the response variable would suggest transformations.
### Plot a scatterplot matrix
plot(Pay) 
### OR
pairs(Pay[1:4], panel = panel.smooth)

### Correlation matrix
### Using correlation matrix, we can assess the strength and direction of correlation.
### The package Hmisc includes rcorr() function that displays r and the p-values.
cor(Pay)
### OR
### Use the rcorr function in the Hmisc package. This displays the r and p-values

library(Hmisc)
rcorr(as.matrix(Pay))

### Observations from the Corrlation Matrix:
### Strong Positive correlation between WeeksSP and Age, WeeksSP and Years
### Weak Positive correlation between WeeksSP and Pay
### p-values matrix shows that the correlation between WeeksSP and Age is statistically significant
### WeeksSP and Years is statistically significant.
### WeeksSP and Pay is not statistically significant.

### Multicollinearity
### A general rule is if the correlation between two independent variables is 
### between ???0.70 and 0.70, there likely is not a problem using both of the 
### independent variables.

### Observations: The r value between Age and Years is 0.81 which is higher than 0.70.
### From informal observation, problem of multicollinearity may exist.
### To assess existence of multicollinearity formally, we compute the VIF values.
### VIF values > 10 (or 30) indicate signs of multicollinearity.
Pay_Model <- lm(WeeksSP ~ Age+Years+Pay, data = Pay)
install.packages("car")

vif(Pay_Model)
### Observation: From the VIF values, we see that all VIF values are well under 10.
### So multicollinearity may not be a problem.

### Step 3: Develop the multiple regression model.
Pay_Model <- lm(WeeksSP ~ Age+Years+Pay, data = Pay)
summary(Pay_Model)

### Step 4: Perform Regression Diagnostics
### Conditions that need to be satisfied: normality, independence, and homoskedasticity

# Normality Assessment
# Look at the p-value from Anderson-Darling normality test (available in nortest package)
# H0: Residuals are normally distributed
# H1: Residuals are not normally distributed

ad.test(residuals(Pay_Model)) # p-value = 0.7063. Fail to Reject H0. Normality assumption satisfied.

### Combine multiple plots into one overall graph - create a multipanel plot
### Can use par() or layout() functions.
### Here, I have used the par() function.
### mfrow =c(nrows, ncols) creates a matrix of nrows x ncols

par(mfrow=c(2,2)) # Change panel layout to 2 x 2
plot(Pay_Model, which=1:4)
par(mfrow=c(1,1)) # Change it back to 1 x 1

# Independence Assessment
## Durbin-Watson test will help us assess the condition of independence.
## If the data are not a time-series, we don't have to worry too much about this condition.

dwtest(Pay_Model) ## Command available in lmtest package

### No signs of autocorrelation.

### You can check all assumptions at once using the GVLMA package.
gvlma(Pay_Model)

### All assumptions are satisfied.

### Step 5: Testing the overall model validity
### H0: beta1 = beta2 = beta3 = 0 (There is no linear relationship between the dependent variable and the independent variables.)
### H1: At least one beta(i) is not equal to zero. (There is a linear relationship between the dependent variable and at least one of the independent variables.)

### p-value for the F-test: 3.758e-12. The overall model is valid.

### This test measures the collective effect of all explanatory variables on the response variable.
### Since the p-value is approx. 0, we infer that the Age, Years, and Pay are jointly significant
### in explaining the variation in Weeks of Severance Pay.

### Adj. R-sq = 0.6825. 68.25% of variation in Weeks SP is explained by variation in
### Age, Years, and Pay. The remaining 31.75% of variation is unexplained.

### Step 6: Interpreting the coefficients.
### Intercept. The intercept is 6.06. This is the average Weeks of SP when Age, Years, and Pay are zero.
### This is meaningless.

### Age. The relationship between Weeks SP and Age is described by b1 = -0.0078. 
### For each additional year of age, weeks of SP decreases on average by 0.0078 weeks
### assuming other independent variables in the model are held constant.

### Years. The relationship between Weeks SP and No. of Years with the Company is described by b2 = 0.603
### For each additional year with the company, the no. of weeks of severance pay increases by 0.603
### assuming other independent variables in the model are held constant.

### Pay. The relationship between Weeks SP and Annual Pay is described by b3 = -0.0702.
### For every one thousand dollars increase in annual pay, the no. of weeks of severance package decreases
### by 0.07 weeks assuming other independent variables in the model are held constant.

### Step 7: Testing the coefficients (t-test for individual coefficients).
### Null Hypothesis: beta(i) = 0
### Alternative hyppothesis: beta(i) =/= 0
### t = (b(i) - beta(i))/se(b(i))

### Test of Coefficient of Age
### p-value = 0.9069. Not statistically significant.
### Not enough evidence to conclude that Age is linearly related to the No. of Weeks of SP.

### Test of Coefficient of Years
### p-value approx. 0. Highly statistically significant.
### Overwhelming evidence that No. of Years with the Company is linearly related to the No. of Weeks of SP.

### Test of Coefficient of Annual Pay
### p-value = 0.1864. Not statistically significant.
### Not enough evidence to conclude that Annual Pay is linearly related to the No. of Weeks of SP.

### Only No. of Years with the company is linearly related to the severance pay.

### Prediction Interval for Bill Smith. Age = 32 years, No. of years with the company = 10, Pay = $32,000
predict(Pay_Model, data.frame(Age = 36, Years = 10, Pay = 32), interval = "prediction", level = 0.95)
### Predict with 95% confidence 9.57 weeks of pay (point estimate)
### LCL = 5.63 weeks of pay 
### UCL = 13.50 weeks of pay.
### The offer of 5 weeks severance pay falls below the prediction interval and thus Bill is correct.







