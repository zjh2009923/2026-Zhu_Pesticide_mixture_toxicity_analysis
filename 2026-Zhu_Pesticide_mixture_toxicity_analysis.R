library(fitdistrplus)
library(R330)
library(car)
library(s20x)
library(mgcv)
library(ggplot2)
library(corrplot)
library(e1071)

# Read in data
n <- read.csv("C:/Users/Chris/OneDrive/Desktop/RDATA/data2A.csv")

# Correlation matrix
C <- cor(n)

corrplot(
  C,
  method = "number",
  number.cex = 0.3,
  tl.cex = 0.4
)

# Read in cleared data
m <- read.csv("C:/Users/Chris/OneDrive/Desktop/RDATA/finaldatafinal.csv")



# Method 1 - All Possible Regressions for Variable Selection
###########################################################################################

# All remaining variables for Method 1, no soils
full_variable <- with(
  m,
  lm(
    Pesticide ~
      Rainfall +
      Bare_ground_cover.. +
      Green_veg.. +
      banana +
      Channel.aqueduct +
      Dairy.sheds.and.yards +
      Grazing.irrigated.modified.pastures +
      Grazing.native.vegetation +
      Horse.studs +
      Intensive.horticulture +
      Irrigated.perennial.horticulture +
      Irrigated.seasonal.horticulture +
      Irrigated.sugar +
      Lake +
      Land.in.transition +
      macadamia +
      Mining +
      olive +
      Perennial.horticulture +
      pineapple +
      Plantation.forests +
      Production.native.forests +
      River +
      Urban.residential
  )
)

# Apply all possible regression
allpossregs(full_variable, data=m)

#fit 1, use variables suggested from the model 
allpossregs.fit1 <- with(
  m,
  lm(
    Pesticide ~
      Rainfall +
      Green_veg.. +
      banana + Horse.studs + Grazing.native.vegetation + 
      Intensive.horticulture + Irrigated.perennial.horticulture + 
      macadamia
  )
)

summary(allpossregs.fit1) #R2 = 0.5424, Grazing.native.vegetation not significant

# Plot fit 1
par(mfrow=c(3, 2))
plot(allpossregs.fit1, main="allpossregs.fit1") 
plot(allpossregs.fit1, main="allpossregs.fit1", which = 4) 

#calculate skewness
e1071::skewness(m$Pesticide, na.rm = TRUE)

#fit 2 sqrt y
allpossregs.fit2 <- with(
  m,
  lm(
    sqrt(Pesticide) ~
      Rainfall +
      Green_veg.. +
      banana + Horse.studs + Grazing.native.vegetation + 
      Intensive.horticulture + Irrigated.perennial.horticulture + 
      macadamia
  )
)

summary(allpossregs.fit2) # R2=0.5642, Grazing.native.vegetation not significant

# Plot fit 2
par(mfrow=c(3, 2))
plot(allpossregs.fit2, main="allpossregs.fit2") 
plot(allpossregs.fit2, main="allpossregs.fit2", which = 4) 

#Fit 3 remove Grazing.native.vegetation
allpossregs.fit3 <- with(
  m,
  lm(
    sqrt(Pesticide) ~
      Rainfall +
      Green_veg.. +
      banana + Horse.studs + 
      Intensive.horticulture + Irrigated.perennial.horticulture + 
      macadamia
  )
)

summary(allpossregs.fit3) #R2=0.565

#plot fit 3
par(mfrow=c(3, 2))
plot(allpossregs.fit3, main="allpossregs.fit3") 
plot(allpossregs.fit3, main="allpossregs.fit3", which = 4)

#Fit 4 remove point 300 as outlier
allpossregs.fit4 <- with(
  m[-c(300), ],
  lm(
    sqrt(Pesticide) ~
      Rainfall +
      Green_veg.. +
      banana + Horse.studs + 
      Intensive.horticulture + Irrigated.perennial.horticulture + 
      macadamia
  )
)

summary(allpossregs.fit4) #r= 0.5713


#plot fit 4
par(mfrow=c(3, 2))
plot(allpossregs.fit4, main="allpossregs.fit4") 
plot(allpossregs.fit4, main="allpossregs.fit4", which = 4) 

# test VIF
vif(allpossregs.fit4)

#Method 2 - Stepwise Regressions for Variable Selection
###################################################################################
# Proceed with Stepwise Regression and rule variables out
step(full_variable, direction = "backward") 

# Use variables suggested by stepwise regression to fit all possible regression 
full_variable_2 <- with(
  m,
  lm(
    Pesticide ~
      Rainfall +
      Green_veg.. +
      banana +
      Channel.aqueduct +
      Grazing.native.vegetation +
      Irrigated.perennial.horticulture +
      Irrigated.seasonal.horticulture +
      Irrigated.sugar
  )
)

allpossregs(full_variable_2, data=m)

# Investigate model with all variables to start with, then reassess
Step.fit1 <- with(
  m,
  lm(
    Pesticide ~
      Rainfall +
      Green_veg.. +
      banana +
      Channel.aqueduct +
      Grazing.native.vegetation +
      Irrigated.perennial.horticulture +
      Irrigated.seasonal.horticulture +
      Irrigated.sugar
  )
)

summary(Step.fit1) #R=0.5416 
#plot fit 1
par(mfrow=c(3, 2))
plot(Step.fit1, main="Step.fit1") 
plot(Step.fit1, main="Step.fit1", which = 4)

#fit 2 Try square root Y
Step.fit2 <- with(
  m,
  lm(
    sqrt(Pesticide) ~
      Rainfall +
      Green_veg.. +
      banana +
      Channel.aqueduct +
      Grazing.native.vegetation +
      Irrigated.perennial.horticulture +
      Irrigated.seasonal.horticulture +
      Irrigated.sugar
  )
)

summary(Step.fit2) #R2= 0.5651 Grazing.native.vegetation insignificant

par(mfrow=c(3, 2))
plot(Step.fit2, main="Step.fit2") 
plot(Step.fit2, main="Step.fit2", which = 4) 

#Remove green veg
Step.fit3 <- with(
  m,
  lm(
    sqrt(Pesticide) ~
      Rainfall +
      Green_veg.. +
      banana +
      Channel.aqueduct +
      Irrigated.perennial.horticulture +
      Irrigated.seasonal.horticulture +
      Irrigated.sugar
  )
)

summary(Step.fit3) #R2= 0.5655   
par(mfrow=c(3, 2))
plot(Step.fit3, main="Step.fit3") 
plot(Step.fit3, main="Step.fit3", which = 4) 

#Fit 4 keep green veg remove 300
Step.fit4 <- with(
  m[-c(300),],
  lm(
    sqrt(Pesticide) ~
      Rainfall +
      Green_veg.. +
      banana +
      Channel.aqueduct +
      Irrigated.perennial.horticulture +
      Irrigated.seasonal.horticulture +
      Irrigated.sugar
  )
)
summary(Step.fit4) #R=0.571  
par(mfrow=c(3, 2))
plot(Step.fit4, main="Step.fit4") 
plot(Step.fit4, main="Step.fit4", which = 4) 

#test vif
vif(Step.fit4)

##############################
#method 1 R= 0.5713 method 2 R= 0.571
##############################
#MLR
#with method 1 variables
mlr.fit1 <- with(
  m,
  lm(
    sqrt(Pesticide) ~
      poly(Rainfall, 2) +
      poly(Green_veg.., 3) +
      banana +
      Horse.studs +
      Intensive.horticulture +
      Irrigated.perennial.horticulture +
      macadamia
  )
)

summary(mlr.fit1) #R2= 0.5957
par(mfrow=c(3, 2))
plot(mlr.fit1, main="mlr.fit1") 
plot(mlr.fit1, main="mlr.fit1", which = 4)

# FIt 2 remove point 184 
mlr.fit2 <- with(
  m[-c(184),],
  lm(
    sqrt(Pesticide) ~
      poly(Rainfall, 2) +
      poly(Green_veg.., 3) +
      banana +
      Horse.studs +
      Intensive.horticulture +
      Irrigated.perennial.horticulture +
      macadamia
  )
)

summary(mlr.fit2) #R2= 0.6021  
par(mfrow=c(3, 2))
plot(mlr.fit2, main="mlr.fit2") 
plot(mlr.fit2, main="mlr.fit2", which = 4)


# FIt 3 also remove point 120
mlr.fit3 <- with(
  m[-c(184, 120),],
  lm(
    sqrt(Pesticide) ~
      poly(Rainfall, 2) +
      poly(Green_veg.., 3) +
      banana +
      Horse.studs +
      Intensive.horticulture +
      Irrigated.perennial.horticulture +
      macadamia
  )
)

summary(mlr.fit3) #R2= 0.6133   
par(mfrow=c(3, 2))
plot(mlr.fit3, main="mlr.fit3") 
plot(mlr.fit3, main="mlr.fit3", which = 4)

#############################
#fit 4 try method 2 variables
mlr.fit4 <- with(
  m,
  lm(
    sqrt(Pesticide) ~
      poly(Rainfall, 2) +
      poly(Green_veg.., 3) +
      banana +
      Channel.aqueduct +
      Irrigated.perennial.horticulture +
      Irrigated.seasonal.horticulture +
      Irrigated.sugar
  )
)

summary(mlr.fit4) #R2= 0.599  
par(mfrow=c(3, 2))
plot(mlr.fit4, main="mlr.fit4") 
plot(mlr.fit4, main="mlr.fit4", which = 4)

#fit 5 remove point 184(outlier)
mlr.fit5 <- with(
  m[-c(184),],
  lm(
    sqrt(Pesticide) ~
      poly(Rainfall, 2) +
      poly(Green_veg.., 3) +
      banana +
      Channel.aqueduct +
      Irrigated.perennial.horticulture +
      Irrigated.seasonal.horticulture +
      Irrigated.sugar
  )
)

summary(mlr.fit5) #R2= 0.6048  
par(mfrow=c(3, 2))
plot(mlr.fit5, main="mlr.fit5") 
plot(mlr.fit5, main="mlr.fit5", which = 4)

#fit 6 remove 120
mlr.fit6 <- with(
  m[-c(184, 120),],
  lm(
    sqrt(Pesticide) ~
      poly(Rainfall, 2) +
      poly(Green_veg.., 3) +
      banana +
      Channel.aqueduct +
      Irrigated.perennial.horticulture +
      Irrigated.seasonal.horticulture +
      Irrigated.sugar
  )
)

summary(mlr.fit6) #R2= 0.6165   
par(mfrow=c(3, 2))
plot(mlr.fit6, main="mlr.fit6") 
plot(mlr.fit6, main="mlr.fit6", which = 4)

#fit 7 remove 298
mlr.fit7 <- with(
  m[-c(184, 120, 298),],
  lm(
    sqrt(Pesticide) ~
      poly(Rainfall, 2) +
      poly(Green_veg.., 3) +
      banana +
      Channel.aqueduct +
      Irrigated.perennial.horticulture +
      Irrigated.seasonal.horticulture +
      Irrigated.sugar
  )
)

summary(mlr.fit7) #R2= 0.617, minor improvement, keep 298

######################
#GAM with method 1 variables
gam.fit1 <- mgcv::gam(
  sqrt(Pesticide) ~
    s(Rainfall) +
    s(Green_veg..) +
    s(banana, k = 4) +
    s(Horse.studs, k = 5) +
    s(Intensive.horticulture) +
    s(Irrigated.perennial.horticulture) +
    s(macadamia),
  data = m
)

summary(gam.fit1) #R2= 0.662 banana, Horse.studs insignificant

#Fit 2 remove banana, Horse.studs
gam.fit2 <- mgcv::gam(
  sqrt(Pesticide) ~
    s(Rainfall) +
    s(Green_veg..) +
    s(Intensive.horticulture) +
    s(Irrigated.perennial.horticulture) +
    s(macadamia),
  data = m
)

summary(gam.fit2) #R2 =0.661 Deviance explained = 68.4%

#GAM for method 2
gam.fit3 <- mgcv::gam(
  sqrt(Pesticide) ~
    s(Rainfall) +
    s(Green_veg..) +
    s(banana, k = 4) +
    s(Channel.aqueduct) +
    s(Irrigated.perennial.horticulture) +
    s(Irrigated.seasonal.horticulture) +
    s(Irrigated.sugar),
  data = m
)

summary(gam.fit3) #R2 = 0.664 Deviance explained = 68.7%


### PLOT result
par(mfrow = c(1, 1))

# Rainfall
plot(gam.fit2, select = 1, ylim = c(-4, 4),
     main = "sqrt Pesticide")

# Green vegetation
plot(gam.fit2, select = 2, ylim = c(-3, 3),
     main = "sqrt Pesticide")

# Intensive horticulture
plot(gam.fit2, select = 3, ylim = c(-5, 5),
     main = "sqrt Pesticide")

# Irrigated perennial horticulture
plot(gam.fit2, select = 4, ylim = c(-5, 8),
     main = "sqrt Pesticide")

# Macadamia
plot(gam.fit2, select = 5, ylim = c(-5, 5),
     main = "sqrt Pesticide")

# Validation
###################################
# Initialize empty vectors to store predictions
predicted_values <- numeric(nrow(m))
absolute_percentage_errors <- numeric(nrow(m))

# Loop over each observation
for (i in 1:nrow(m)) {
  
  # Create training and testing datasets
  train_data <- m[-i, ]
  test_data <- m[i, , drop = FALSE]
  
  # Fit GAM model to training data
  model <- mgcv::gam(
    sqrt(Pesticide) ~
      s(Rainfall) +
      s(Green_veg..) +
      s(Intensive.horticulture) +
      s(Irrigated.perennial.horticulture) +
      s(macadamia),
    data = train_data
  )
  
  # Predict sqrt(Pesticide) for the left-out observation
  predicted_values[i] <- predict(
    model,
    newdata = test_data,
    type = "response"
  )
}

# Actual values on sqrt(Pesticide) scale
actual_values <- sqrt(m$Pesticide)

# Calculate MAPE
absolute_percentage_errors <-
  abs((actual_values - predicted_values) / actual_values) * 100

mape <- mean(absolute_percentage_errors)

# Calculate MAE
mae <- mean(abs(actual_values - predicted_values))

# Calculate RMSE
rmse <- sqrt(mean((actual_values - predicted_values)^2))

cat("MAPE:", mape, "%\n")
cat("MAE:", mae, "\n")
cat("RMSE:", rmse, "\n")