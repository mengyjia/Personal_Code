#################################################
#### SOLVED EXAMPLE 6 - MODEL BUILDING ##########
#################################################

#### In the previous part, we ran stepwise regression to select the variables.
#### In this part, we create a model using the proposed variables.
#### Run diagnostics, assess statistical significance, etc.
#### Assess multicollinearity
#### And finally check using Partial F-test if the variable TIME has a statistically significance influence 
#### on SALES.

VarSelection <- read.csv("VariableSelection.csv")

#### Stepwise Regression: Build regression model from a set of candidate predictor variables 
#### by entering and removing predictors based on p values, in a stepwise manner until 
#### there is no variable left to enter or remove any more. The model should include all 
#### the candidate predictor variables. If details is set to TRUE, each step is displayed.
model <- lm(SALES ~ ., data = VarSelection)
k <- ols_stepwise(model, prem = 0.05, pent = 0.05, details = FALSE)

#### Choose ACCTS, ADV, POTENT, SHARE as independent variables.
#### Create a multiple regression model

#### First create a new dataset containing only 5 variables.
#### Dependent Variable - SALES
#### Independent Variable - ACCTS, ADV, POTENT, SHARE
finalvars <- c("SALES", "ACCTS", "ADV", "POTENT", "SHARE")
SalesForcePerf <- VarSelection[finalvars]

### Step 2: Observe whether there is a linear relationship between each explanatory (independent)
### variable and the response (dependent) variable. Look for bending patterns.
### Bending patterns between explanatory variables and the response variable would suggest transformations.
### Plot a scatterplot matrix
plot(SalesForcePerf) 
### OR
pairs(SalesForcePerf[1:4], panel = panel.smooth)

### Observations from the scatterplot matrix. 
### The scatterplots show roughly linear relationship between the dependent and independent variables.
### For some relationships, there may be some illusion of curvatures, but nothing drastically curved.
### Let us wait until we plot residual plots and see if there appear to be any violations. 

### Correlation matrix
### Using correlation matrix, we can assess the strength and direction of correlation.
### The package Hmisc includes rcorr() function that displays r and the p-values.
cor(SalesForcePerf)
### OR
### Use the rcorr function in the Hmisc package. This displays the r and p-values
rcorr(as.matrix(SalesForcePerf))

### Observations from the correlation matrix.
### None of the correlation coefficients exceed 0.7 absolute value. So, no informal signs of multicollinearity.
### We will assess multicollinearity formally using vif.

### Develop the multiple regression model.
Sales_Model <- lm(SALES ~ ., data = SalesForcePerf)
summary(Sales_Model)

### Formal Test for multicollinearity
vif(Sales_Model) ### No signs of multicollinearity.

### Perform Regression Diagnostics
### Conditions that need to be satisfied: normality, independence, and homoskedasticity

# Normality Assessment
# Look at the p-value from Anderson-Darling normality test (available in nortest package)
# H0: Residuals are normally distributed
# H1: Residuals are not normally distributed

ad.test(residuals(Sales_Model)) # p-value = 0.9862. Fail to Reject H0. Normality assumption satisfied.

hist(residuals(Sales_Model))

### Combine multiple plots into one overall graph - create a multipanel plot
### Can use par() or layout() functions.
### Here, I have used the par() function.
### mfrow =c(nrows, ncols) creates a matrix of nrows x ncols

par(mfrow=c(2,2)) # Change panel layout to 2 x 2
plot(Sales_Model, which=1:4)
par(mfrow=c(1,1)) # Change it back to 1 x 1

# Independence Assessment
## Durbin-Watson test will help us assess the condition of independence.
## If the data are not a time-series, we don't have to worry too much about this condition.

dwtest(Sales_Model) ## Command available in lmtest package

### No signs of autocorrelation.

### You can check all assumptions at once using the GVLMA package.
gvlma(Sales_Model)

### All assumptions are satisfied.

### Testing the overall model validity
### H0: beta1 = beta2 = beta3 = 0 (There is no linear relationship between the dependent variable and the independent variables.)
### H1: At least one beta(i) is not equal to zero. (There is a linear relationship between the dependent variable and at least one of the independent variables.)

### p-value for the F-test: 9.563e-10. The overall model is valid.

### This test measures the collective effect of all explanatory variables on the response variable.
### Since the p-value is approx. 0, we infer that the ACCTS, ADV, POTENT, and SHARE are jointly significant
### in explaining the variation in SALES for the territory.

### Adj. R-sq = 88.05%. 88.05% of variation in SALES is explained by variation in
### ACCTS, ADV, POTENT, and SHARE. The remaining 11.95% of variation is unexplained.
### A very good model.

### Interpreting the coefficients.
### Intercept. The intercept is -1,442. This is the average Sales in units when ACCTS, ADV, POTENT, and SHARE are zero.
### This intercept is meaningless since the sales cannot be negative. 

### ACCTS. The relationship between SALES and ACCTS is described by b1 = 9.21. 
### For each additional account assigned to a salesperson, average Sales increases by 9.21 units 
### assuming other independent variables in the model are held constant.

### ADV. The relationship between dollar expenditures on ADV and SALES is described by b2 = -0.175. 
### For each additional dollar spent on ADV, Sales decreases on average by 0.175 units
### assuming other independent variables in the model are held constant.

### POTENT. The relationship between Industry Sales for the territory and SALES for the territory is described by b3 = -0.0382. 
### For each additional unit of increase in industry sales, Sales decreases on average by 0.03822 units
### assuming other independent variables in the model are held constant.

### SHARE. The relationship between past market share for past 4 years and SALES is described by b4 = 190.1. 
### For each additional percentage increase in market share for 4 previous years, Sales increases on average by 190 units
### assuming other independent variables in the model are held constant.

### Testing the coefficients (t-test for individual coefficients).
### Null Hypothesis: beta(i) = 0
### Alternative hyppothesis: beta(i) =/= 0
### t = (b(i) - beta(i))/se(b(i))

### Test of Coefficient of ACCTS
### p-value = 0.0043. Statistically significant.
### ACCTS is linearly related to the SALES.

### Test of Coefficient of ADV
### p-value = 0.000125. Statistically significant.
### ADV is linearly related to the SALES.

### Test of Coefficient of POTENT
### p-value = 0.000111. Statistically significant.
### POTENT is linearly related to the SALES.

### Test of Coefficient of SHARE
### p-value = 0.001065. Statistically significant.
### SHARE is linearly related to the SALES.

#### RUN A PARTIAL F-test TO ASSESS IF THE VARIABLE TIME HAS
#### STATISTICALLY SIGNIFICANT INFLUENCE ON SALES.
fullmodel <- lm(SALES ~ ACCTS + ADV + POTENT + SHARE + TIME, data = VarSelection)
reducedmodel <- lm(SALES ~ ACCTS + ADV + POTENT + SHARE, data = VarSelection)

summary(fullmodel)
summary(reducedmodel)

anova(reducedmodel, fullmodel)

### Conclusion and Interpretation
### The p-value from the ANOVA output leads to the non-rejection of H0
### H0: The reduced model and the full model do not differ significantly, so choose the reduced model.
### H1: The full model is significantly better.
### We conclude at any reasonable significance level that the reduced model and the full model do not differ significantly.
### The TIME variable does not have 
### a statistically significant influence on sales and should not be included in the analysis.