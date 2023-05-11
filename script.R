# Insurance dataset 
# Load the dataset into a data frame first
# refer to notes on Blackboard for discusisons on 
# dummy variables and how they are generated

insurance_data <- read.csv("insurance.csv", na = "")
str(insurance_data)

# several variables need to be converted
# sex - male = 0, female = 1
# Smoker - yes = 1, no = 0
# Region contains 4 categories
# N = 4, so we need n-1 indicator variables
# = 3 indicator variables
# Code variables in alphabetical order
head(insurance_data$region, 15)

# Convert variables as described above
names(insurance_data)
attach(insurance_data)

insurance_data$sex <- factor(sex,
                             levels = c("male", "female"), 
                             ordered = FALSE)

insurance_data$smoker <- factor(smoker, 
                                levels = c("yes", "no"), 
                                ordered = FALSE)

insurance_data$region <- factor(region,  
                                levels = c("northeast", "northwest", "southeast", "southwest"), 
                                ordered = FALSE)

str(insurance_data)

# View the split of categorical variables within the data frame
# to examine balance
table(insurance_data$sex)
table(insurance_data$smoker)
table(insurance_data$region)


# Initial investigation of data variables
# and their correlations
# Be careful of your interpretation of this chart
pairs(insurance_data)
install.packages("psych")
library(psych)

# Seems there could be a positive correlation between 
# smoker and charges, perhaps charges and age
# and BMI and charges
pairs.panels(insurance_data,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "pearson",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# if we build the model now, R will automatically split the
# factor variables
# Alternatively we will control this process

# in linear regression, model represented by:
# y = b0 + B1x1 + B2x2 + B3x3..... + e
# where y = insurance charges
# x1 = age of the person
# x2  sex of the person
# x3 = bmi of the person
# x4 = children
# x5 = smoker
# x6 = region
# It is clear that x1, and x3 are continuous and x2, x4, x5, x6 are categorical
# therefore we need to create dummy variables for the categorical
# variables
# Eg for the smoker variable x5
# x5 = 1 if person is smoker
# x5 = 0 if person is non-smoker




# Initial build of th MLR model
# Dummy varaibles created automatically by R
set.seed(1)
model <- lm(formula = charges ~ 
              age + 
              sex + 
              bmi + 
              children + 
              smoker + 
              region, 
            data = insurance_data)

model
summary(model)


# BMI, childern, smokerno, have an influence
# over the depencdent variable
# Drop sex variablle
# keep region varable because 
# it is part of my research questuon

names(insurance_data)

#droppeing column sex

insurance_data <- insurance_data[c(1, 3:7)]

names(insurance_data)


#round the variabls
insurance_data$bmi <- round(bmi, 1)
insurance_data$charges <- round(charges, 2)

# create the model again

model2 <- lm(formula = charges ~ 
              age + 
              bmi + 
              children + 
              smoker + 
              region, 
            data = insurance_data)

model2
summary(model2)

#model assumptions
# linearity
# we can check the linear co re;ation
#if it exists between dependent and each 
# independemnt variablkke
#only works for continues data


scatter.smooth(x = age,
               y = charges,
               main = "insurance charges for the age",
               xlab = "age(years)",
                ylab = "insurance charges" )

#bmi and childern

scatter.smooth(x = bmi,
               y = charges,
               main = "insurance charges for the bmi",
               xlab = "age(years)",
               ylab = "insurance charges" )


scatter.smooth(x = children,
               y = charges,
               main = "insurance charges for the chlidren",
               xlab = "age(years)",
               ylab = "insurance charges" )

# scatter.smoot for categorical data.
attach(insurance_data)

plot(smoker,
    charges,
    main = "Charges by smoker sattus",
    xlab = "smoker",
    ylab = "insurance charges")



plot(region,
     charges,
     main = "Charges by smoker sattus",
     xlab = "smoker",
     ylab = "insurance charges")

cor(charges, bmi)




#normality




qqnorm(age)
qqline(age)

normality_age_test <- shapiro.test(insurance_data$age)
normality_age_test$p.value

#p value is less than 0.05. so it is not normally distributed
# cant check the factor variables with this approch

with(insurance_data,
     tapply(charges, smoker, shapiro.test))


with(insurance_data,
     tapply(charges, region, shapiro.test))

#colinearity
install.packages("car")
library(car)
# variables should be close to 1 but under 5
# 10 indicaes that the vars are not needed
#





