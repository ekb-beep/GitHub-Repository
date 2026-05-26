
newdata=read.csv(file="UpdatedFreq.csv", header = T)
head(newdata)
data1=data_frame()
# Create a new dataset excluding the last three columns
data.train = newdata[, 1:(ncol(newdata) - 3)]


library(tidyr)
library(dplyr)




#######FREQUENCY#######
#######FREQUENCY#######
# Reshape the data to a long format 
data_long = data.train %>%
  gather(key = "Period", value = "Frequency", -State, -Industry)
head(data_long)

# Replace NA values with 0 in the Frequency column
data_long$Frequency[is.na(data_long$Frequency)] = 0

# Fit a GLM with Poisson distribution
model = glm(Frequency ~ State + Industry + Period, family = poisson(), data = data_long)
summary(model)


install.packages('xtable')
library(xtable)
latex_table <- xtable(summary(model))
print(latex_table, comment = FALSE)
# Fit a GLM with Negative Binomial distribution
library(MASS)
modely1 = glm.nb(Frequency ~ State + Industry + Period, data = data_long)
summary(modely1)

#Fit a Zero Inflated Model
# Fit a Zero-Inflated Poisson model
library(pscl)

# Replace NA values with 0 in the Frequency column
data_long$Frequency[is.na(data_long$Frequency)] = 0

#####Zero-Inflated Model
zip_model = zeroinfl(Frequency ~ State + Industry + Period | 1, data = data_long, dist = "poisson")
summary(zip_model)
AIC(zip_model)
install.packages('texreg')
library(texreg)
latex_table <- texreg(zip_model)
print(latex_table)

install.packages('stargazer')
library(stargazer)

stargazer(zip_model, type = "latex", out = "model_output.tex")


#Hurdle model
library(pscl)
model_hurdle = hurdle(Frequency ~ Industry+Period, data = data_long, 
                      dist = "poisson", 
                      zero.dist = "binomial", 
                      link = "log")
summary(model_hurdle)
AIC(model_hurdle)

model_hurdle2 = hurdle(Frequency ~ State , data = data_long, 
                       dist = "negbin", 
                       zero.dist = "binomial", 
                       link = "log")
summary(model_hurdle2)
AIC(model_hurdle2)

########AMOUNT###########
newdata2=read.csv(file = "updatedamount.csv", header = T)
head(newdata2)
#######FIT THE DISTRIBUTION#######
#####FIT DISTRIBUTIONS
library(fitdistrplus)
# Check for missing values and remove any outliers
data_clean <- na.omit(newdata2)
str(data_clean)

####LOGNORMAL DISTRIBUTION
logofdata= log(data_clean$Ransom.Amount)
data_fit = fitdist(logofdata, distr = "lnorm")
plot(data_fit)
summary(data_fit)

####WEIBULL DISTRIBUTION
data_fit = fitdist(logofdata, distr = "weibull")
plot(data_fit)
summary(data_fit)

####GAMMA DISTRIBUTION
data_fit1 = fitdist(logofdata, distr = "gamma")
plot(data_fit)
summary(data_fit1)


# Create a new dataset excluding the last three columns
data2.train = newdata2[, 1:(ncol(newdata2) - 3)]

# Create a test dataset including only the last three columns
data2.test <- newdata2[, (ncol(newdata2) - 2):ncol(newdata2)]


# Reshape the data to a long format
data_long2 = data2.train %>%
  gather(key = "Period", value = "Amount", -State , -Industry)

head(data_long2)

# Fit a GLM with Gaussian family 
model29 <- glm(Amount ~ State + Industry + Period, family = gaussian(), data = data_long2)
summary(model29)
plot(model29)
library(xtable)
latex_table <- xtable(summary(model29))
print(latex_table, comment = FALSE)

# Fit a GLM with Gaussian family with Log-normal link function
model2 <- glm(Amount ~ State + Industry + Period, family = gaussian(link = log), data = data_long2)
summary(model2)
plot(model2)
latex_table <- xtable(summary(model2))
print(latex_table, comment = FALSE)

# Fit a GLM with GAMMA
model21 <- glm(Amount ~ State + Industry + Period, family = Gamma(link = inverse), data = data_long2)
summary(model21)
plot(model21)

# Fit a GLM with Inverse Gaussian
model25 <- glm(Amount ~ State + Industry + Period, family = inverse.gaussian(link=log), data = data_long2)
summary(model25)
plot(model25)

library(survival)
# Create a survival object
surv_obj <- Surv(log(data_long2$Amount))

# Fit the Weibull model
weibull_model <- survreg(surv_obj ~ State + Industry + Period , data = data_long2, dist="weibull")

# Print the model summary
summary(weibull_model)
AIC(weibull_model)
latex_table <- xtable(summary(weibull_model))
print(latex_table, comment = FALSE)
# Assuming 'weibull_model' is your model
weibull_summary <- summary(weibull_model)

# Extracting coefficients table
coef_table <- weibull_summary$coefficients

# Convert to data frame (if it's not already)
coef_df <- as.data.frame(coef_table)

# Create the xtable
library(xtable)
latex_table <- xtable(coef_df)

# Print the LaTeX table
print(latex_table, comment = FALSE)


# 1. Residual Plots
residuals_obj <- residuals(weibull_model, type = "deviance")
plot(residuals_obj)



#####CONVERTING THE PERIOD TO NUMERIC#######
############################################
# Extract the year
year <- as.numeric(substr(data_long2$Period, 2, 5))

# Determine the numeric value for the half
half <- ifelse(grepl("1st.Half", data_long2$Period), 0.5, 1)

# Compute the numeric representation
data_long2$PeriodNumeric <- (year - 2018) + half



# Fit a GLM with Gaussian family (with numeric Periods)
model279 <- glm(Amount ~ State + Industry + PeriodNumeric, family = gaussian(), data = data_long2)
summary(model279)
plot(model279)
AIC(model279)


# Fit a GLM with Gaussian family with Log-normal link function(With Numeric Periods)
model276 <- glm(Amount ~ State + Industry + Period, family = gaussian(link = log), data = data_long2)
summary(model276)
plot(model276)
AIC(model276)


# Fit a Weibull Model(with numeric Periods)
# Create a survival object
surv_obj <- Surv(log(data_long2$Amount))

# Fit the Weibull model
weibull_model2 <- survreg(surv_obj ~ State + Industry + Period , data = data_long2, dist="weibull")

# Print the model summary
summary(weibull_model2)
AIC(weibull_model2)


# 1. Residual Plots
residuals_obj <- residuals(weibull_model2, type = "deviance")
plot(residuals_obj)

###H20 MODEL#########
library(h2o)
h2o.init()

data_h2o <- as.h2o(data_long2)
splits <- h2o.splitFrame(data_h2o, ratios = c(0.7), seed = 123)
train <- splits[[1]]
test <- splits[[2]]

automl_models <- h2o.automl(
  x = c("State", "Industry", "Period"), 
  y = "Amount", 
  training_frame = train, 
  max_models = 10, 
  seed = 123, 
  max_runtime_secs = 3600 # Set to 1 hour
)

performance <- h2o.performance(glm_model, test)




