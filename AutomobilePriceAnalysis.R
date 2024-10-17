## Automobile Price Analysis Model

# Input -------------------------------------------------------------------
library(readxl)
library(ggplot2)
input <- read.csv('dataset.csv', sep = ',',na.strings = "?", header = FALSE)

names(input) <- c('symboling', 'normalizedLosses', 'make', 'fuelType', 'aspiration', 'numOfDoors', 'bodyStyle',
                  'driveWheels' , 'engineLocation', 'wheelBase' , 'length' , 'width', 'height' , 'curbWeight' ,
                  'engineType', 'Cylinders', 'engineSize' , 'fuelSystem', 'bore' , 'stroke' , 
                  'compressionRatio' ,'horsepower' , 'peakRpm', 'cityMpg' , 'highwayMpg', 'price' )

input$symboling <- as.factor(input$symboling)
input$make <- as.factor(input$make)
input$fuelType <- as.factor(input$fuelType)
input$aspiration <- as.factor(input$aspiration)
input$numOfDoors <- as.factor(input$numOfDoors)
input$bodyStyle <- as.factor(input$bodyStyle)
input$driveWheels <- as.factor(input$driveWheels)
input$engineLocation <- as.factor(input$engineLocation)
input$engineType <- as.factor(input$engineType)
input$Cylinders <- as.factor(input$Cylinders)
input$fuelSystem <- as.factor(input$fuelSystem)

summary(input$price)


# Missingness and Imputation -------------------------------------------------------------
library(VIM)
sapply(input, function(x)(sum(is.na(x))))
missingness <- aggr(input, col=c('grey','mediumseagreen'), numbers=TRUE, sortVars=TRUE,labels=names(input), cex.axis=.7,gap=3)

input$normalizedLosses <- NULL                
input <- input[!(is.na(input$price)), ]       

library(mice)
imputation <- mice(input, m=1, method=c('','','','','logreg','','','','','','','','','','','','',
                                        'pmm','pmm','','pmm','pmm','','',''), maxit = 20)
data <- complete(imputation,1) 

ggplot(data, aes(sample = input$price)) +
  stat_qq() +
  stat_qq_line(color='red')

#bore 0.019512195           pmm-predictive mean matching                
#stroke 0.019512195         pmm
#numOfDoors 0.009756098     logreg-logistic regression (binary)
#horsepower 0.009756098     pmm
#peakRpm 0.009756098        pmm

sapply(data, function(x)(sum(is.na(x))))
missingness <- aggr(data, col=c('grey','mediumseagreen'), numbers=TRUE, sortVars=TRUE,labels=names(data), cex.axis=.7,gap=3)


# Starting model ----------------------------------------------------------
plot(data$price)
summary(data$price)

table(data$fuelSystem) 

mod0 <- lm(price ~ .,data= data)
summary(mod0)
par(mfrow=c(2,2))
plot(mod0)

#anova(mod0)
drop1(mod0, test = 'F')

library(forestmodel)
forest_model(mod0)

# New variables and optimal grouping √ --------------------------------------

# factorMerger library has been deprecated recently, a manual approach is employed 
# in the "make" variable just as an example (using the once obtained optimal groups).

#library(factorMerger) 
library(ggplot2)

#New variables 
data$volume <- data$length*data$width*data$height
data$engineSize <- (data$engineSize)*16.39 #is in CID unit, convert to CC

#optimal grouping 
optimalGroups <- data 
modPreOG <- lm(price ~ .,data= data)
summary(modPreOG)  

#symboling 
plot(optimalGroups$symboling, optimalGroups$price)
table(optimalGroups$symboling)
levels(optimalGroups$symboling) <- c('safe','safe','normal','normal','risky','risky')

#make (mergeFactors deprecated, manual approach instead)
plot(data$make,data$price)
#reduce_levels <- mergeFactors(response = data$price , factor = data$make)
#plot(reduce_levels, panel= "GIC" , title = "m", panelGrid = FALSE)
#reduced <- cutTree(reduce_levels)
#optimalGroups$make<-reduced
#levels(optimalGroups$make) <- c('LE','CH','CO','CO','EX','HE') #unisco due lvl
#plot(optimalGroups$make,optimalGroups$price)
#low-end, cheap, competitive, expensive, high-end

optimalGroups$make <- as.character(optimalGroups$make)
merge_mapping <- list(
  'LE' = c('chevrolet', 'dodge',"honda","plymouth","subaru"),  # Those were found automatically by mergeFactors based on price
  'CH' = c('isuzu', 'mitsubishi',"renault","toyota","volkswagen","nissan","mazda"),
  'CO' = c('saab', 'peugot', 'alfa-romero',"mercury"),  
  'EX' = c('audi',"volvo","bmw"),
  'HE' = c('porsche',"mercedes-benz","jaguar")
)
for (new_level in names(merge_mapping)) {
  old_levels <- merge_mapping[[new_level]]
  optimalGroups$make[optimalGroups$make %in% old_levels] <- new_level
}
optimalGroups$make <- factor(optimalGroups$make)
plot(optimalGroups$make, optimalGroups$price)

# This process should be done for every factor variable 

#bodystyle 
plot(data$bodyStyle,data$price)
reduce_levels <- mergeFactors(response = data$price , factor = data$bodyStyle)
plot(reduce_levels, panel= "GIC" , title = "b", panelGrid = FALSE)
reduced <- cutTree(reduce_levels)
optimalGroups$bodyStyle<-reduced
levels(optimalGroups$bodyStyle) <- c('citycar', 'familycar' , 'sportcar') 
plot(optimalGroups$bodyStyle,optimalGroups$price)

#cylinders
plot(data$Cylinders,data$price)
reduce_levels <- mergeFactors(response = data$price , factor = data$Cylinders)
plot(reduce_levels, panel= "GIC" , title = "c", panelGrid = FALSE)
reduced <- cutTree(reduce_levels)
optimalGroups$Cylinders<-reduced
levels(optimalGroups$Cylinders) <- c('2-4', '5-6' , '8-12') 
plot(optimalGroups$Cylinders,optimalGroups$price)

#engineType       
plot(data$engineType,data$price) 
optimalGroups$engineType = droplevels(optimalGroups$engineType)
reduce_levels <- mergeFactors(response = data$price , factor = data$engineType)
plot(reduce_levels, panel= "GIC" , title = "e", panelGrid = FALSE)
reduced <- cutTree(reduce_levels)
optimalGroups$engineType<-reduced
levels(optimalGroups$engineType) <- c('overheadcam','other','dual-ohc','ohc+valve') 
plot(optimalGroups$engineType,optimalGroups$price)

#fuelsystem 
plot(data$fuelSystem,data$price)
reduce_levels <- mergeFactors(response = data$price , factor = data$fuelSystem)
plot(reduce_levels, panel= "GIC" , title = "f", panelGrid = FALSE)
reduced <- cutTree(reduce_levels)
optimalGroups$fuelSystem<-reduced
levels(optimalGroups$fuelSystem) <- c('1-2 barrel','other','indirect/multip inj') 
plot(optimalGroups$fuelSystem,optimalGroups$price)

modPostOG <- lm(price ~ .,data= optimalGroups)
summary(modPostOG) 
par(mfrow=c(2,2))
plot(modPostOG)
data <- optimalGroups

# Collinearity ------------------------------------------------------------
library(mctest)
library(dplyr)
library(corrgram)
library(plyr)

mod1 <- lm(price ~ .,data= data)
cov=attr(terms(mod1), "term.labels") 
data_factor <- data[,cov] %>% dplyr::select_if(is.factor)
data_factor_preCol <- data_factor
data_num <- data[,cov] %>% dplyr::select_if(is.numeric)

#factor part
combos <- combn(ncol(data_factor),2)
adply(combos, 2, function(x) {
  test <- chisq.test(data_factor[, x[1]], data_factor[, x[2]], simulate.p.value = TRUE)
  tab  <- table(data_factor[, x[1]], data_factor[, x[2]])
  out <- data.frame("Row" = colnames(data_factor)[x[1]]
                    , "Column" = colnames(data_factor[x[2]])
                    , "Chi.Square" = round(test$statistic,3)
                    , "df"= test$parameter
                    , "p.value" = round(test$p.value, 5)
                    , "n" = sum(table(data_factor[,x[1]], data_factor[,x[2]]))
                    , "u1" =length(unique(data_factor[,x[1]]))-1
                    , "u2" =length(unique(data_factor[,x[2]]))-1
                    , "nMinu1u2" =sum(table(data_factor[,x[1]], data_factor[,x[2]]))* min(length(unique(data_factor[,x[1]]))-1 , length(unique(data_factor[,x[2]]))-1) 
                    , "Chi.Square norm"  =(test$statistic/(sum(table(data_factor[,x[1]], data_factor[,x[2]]))* min(length(unique(data_factor[,x[1]]))-1 , length(unique(data_factor[,x[2]]))-1))) 
  )
  
  
  return(out)
}) 
table(data_factor$engineLocation)

data_factor$engineLocation <- NULL #

data_factor$make <- NULL #
data_factor$driveWheels <- NULL
data_factor$fuelType <- NULL#
data_factor$symboling <- NULL #
data_factor$fuelSystem <- NULL
data_factor$bodyStyle <- NULL #
data_factor$engineType <- NULL

table(data_factor$Cylinders)
table(data_factor$numOfDoors)
table(data_factor$aspiration)

combos <- combn(ncol(data_factor),2)
adply(combos, 2, function(x) {
  test <- chisq.test(data_factor[, x[1]], data_factor[, x[2]], simulate.p.value = TRUE)
  tab  <- table(data_factor[, x[1]], data_factor[, x[2]])
  out <- data.frame("Row" = colnames(data_factor)[x[1]]
                    , "Column" = colnames(data_factor[x[2]])
                    , "Chi.Square" = round(test$statistic,3)
                    , "df"= test$parameter
                    , "p.value" = round(test$p.value, 5)
                    , "n" = sum(table(data_factor[,x[1]], data_factor[,x[2]]))
                    , "u1" =length(unique(data_factor[,x[1]]))-1
                    , "u2" =length(unique(data_factor[,x[2]]))-1
                    , "nMinu1u2" =sum(table(data_factor[,x[1]], data_factor[,x[2]]))* min(length(unique(data_factor[,x[1]]))-1 , length(unique(data_factor[,x[2]]))-1) 
                    , "Chi.Square norm"  =(test$statistic/(sum(table(data_factor[,x[1]], data_factor[,x[2]]))* min(length(unique(data_factor[,x[1]]))-1 , length(unique(data_factor[,x[2]]))-1))) 
  )
  
  return(out)
}) 

# numeric part 
mod <- lm(data$price~., data = data_num)
imcdiag(mod)
corrgram(data_num, use = "complete.obs", lower.panel = panel.cor, cex=1, cex.labels = 1)

data_num$height <- NULL
data_num$length <- NULL          #riassunte da volume    
data_num$width <- NULL 
data_num$wheelBase <- NULL 
#data_num$curbWeight <- NULL 

data_num$cityMpg <- NULL       #prova a riassumere
data_num$highwayMpg <- NULL      #molto connesse a cavalli
data_num$engineSize <- NULL     #meglio bore e stroke ma resta vif alto

mod <- lm(data$price~., data = data_num)
imcdiag(mod)

#post collinearity model
data$height <- NULL
data$length <- NULL             
data$width <- NULL 
data$wheelBase <- NULL 
#data$curbWeight <- NULL 
data$cityMpg <- NULL       
data$highwayMpg <- NULL   
data$engineSize <- NULL

data$engineLocation <- NULL
data$make <- NULL
data$driveWheels <- NULL
data$fuelType <- NULL
data$symboling <- NULL
data$fuelSystem <- NULL
data$bodyStyle <- NULL
data$engineType <- NULL


mod2 <- lm(price~., data = data)
summary(mod2)
drop1(mod2, test = "F")
par(mfrow=c(2,2))
plot(mod2)

# Trasformation  ----------------------------------------------------------
library(MASS)
library(gam)

data_trasf <- data

#y trasformation
br <-boxcox(mod2)
lambda <- br$x[which.max(br$y)]
lambda #-0.22
modln <- lm(log(price) ~., data = data)  
summary(modln)
par(mfrow=c(2,2))
plot(modln) #
data_trasf$price <- log(data_trasf$price)

#x trasformation
spline <- gam(price ~ aspiration+numOfDoors+Cylinders+s(curbWeight)+s(bore)+s(stroke)+s(compressionRatio)+
              s(horsepower)+s(peakRpm)+s(volume),
              data = data_trasf)
par(mfrow=c(1,2))
summary(spline)  # if significative, there exists a transformation that makes it better
plot(spline)

ggplot(data = data_trasf, aes(x = log(curbWeight), y = price)) + geom_point(color = "gray") +
  geom_smooth(method = "loess", span = 0.5, col = "red") +
  geom_smooth(method = "loess", span = 0.7, col = "purple3") +
  theme_bw()

ggplot(data = data_trasf, aes(x = (stroke)^2, y = price)) + geom_point(color = "gray") +   ####
  geom_smooth(method = "loess", span = 0.5, col = "red") + 
  geom_smooth(method = "loess", span = 0.7, col = "purple3") +
  theme_bw()

ggplot(data = data_trasf, aes(x = (compressionRatio), y = price)) + geom_point(color = "gray") +
  geom_smooth(method = "loess", span = 0.4, col = "red") +
  geom_smooth(method = "loess", span = 0.6, col = "purple3") +
  theme_bw()

plot(data_trasf$compressionRatio, data_trasf$price) 

ggplot(data = data_trasf, aes(x = log(horsepower), y = price)) + geom_point(color = "gray") +
  geom_smooth(method = "loess", span = 0.5, col = "red") +
  geom_smooth(method = "loess", span = 0.7, col = "purple3") +
  theme_bw()

ggplot(data = data_trasf, aes(x = log(peakRpm), y = price)) + geom_point(color = "gray") +
  geom_smooth(method = "loess", span = 0.5, col = "red") +
  geom_smooth(method = "loess", span = 0.7, col = "purple3") +
  theme_bw()


modS <- lm(price ~ aspiration+numOfDoors+Cylinders+log(curbWeight)+bore
           +log(stroke)+log(compressionRatio)+log(horsepower)+log(peakRpm)+volume,
              data = data_trasf)
anova(modS)
drop1(modS, test = 'F')
summary(modS)
par(mfrow=c(2,2))
plot(modS)

# Model Selection ---------------------------------------------------------
library(MASS)

step <- stepAIC(modS,direction='both')                       #aic
step <- stepAIC(modS,direction='both', k=log(nrow(data)))    #sbc

modF <- lm(price ~ Cylinders + log(curbWeight) + log(stroke) + log(compressionRatio) + 
             log(horsepower) + log(peakRpm),
           data = data_trasf)
anova(modS,modF,test= 'Chisq') 

summary(modF)
par(mfrow=c(2,2))
plot(modF)

#p = 0.149 the models are different
#Sum of Sq = -0.21941 less covariates are okay

# Cook-Dfits --------------------------------------------------------------

soglia_cook = 4/(nrow(data_trasf)-5)
cook <- cooks.distance(modF)
plot(cook, col = 'black')
abline(h = soglia_cook, col = 'mediumseagreen',lwd=2)

outliers <- which(cook > soglia_cook) # 17 
(17/201)*100 # percentage, =8.46% under 10, we remove
data_F <- data_trasf[-c(outliers),]

modF1 <- lm(price ~ Cylinders + log(curbWeight) + log(stroke) + log(compressionRatio) + 
              log(horsepower) + log(peakRpm),
           data = data_F)

anova(modF1)
drop1(modF1, test = 'F')
summary(modF1)
par(mfrow=c(2,2))
plot(modF1) 

# Heteroskedasticity ------------------------------------------------------
library(lmtest)
library(car)
library(labeling)
library(sandwich)

plot(modF1$fitted.values, modF1$residuals)

bptest(modF1) 
ncvTest(modF1)

coeftest(modF1, vcov = vcovHC(modF1))

check_heteroskedasticity(modF1)

# Bootstraps --------------------------------------------------------------
library(leaps)

# after bootstrap, we decided to remove stroke
modF2 <- lm(price ~ Cylinders + log(curbWeight) + log(compressionRatio) + 
              log(horsepower) + log(peakRpm),
            data = data_F)
anova(modF2)
drop1(modF2, test = 'F')
summary(modF2)
par(mfrow=c(2,2))
plot(modF2) 

BS =Boot(modF2, R=1999)
Confint(BS, level=c(.95), type="perc")
hist(BS, legend="separate")
drop1(modF2,test='F')
summary(modF2)
summary(BS)

BS =Boot(modF1, R=1999)
Confint(BS, level=c(.95), type="perc")
hist(BS, legend="separate")
drop1(modF2,test='F')
summary(modF2)
summary(BS)


# Confronto con modello iniziale ------------------------------------------
library(gvlma)
par(mfrow = c(2,2))
plot(modPostOG)
plot(modF1)
par(mfrow = c(1,1))

forest_model(modPostOG)
forest_model(modF1)

summary(modPostOG)
summary(modF1)

gvlma(modPostOG)
gvlma(modF1)

# Logistic Model ----------------------------------------------------------

data_Logistic<-data_F
data_Logistic$price <- exp(data_Logistic$price)
summary(data_Logistic$price)

# split between affordable and expensive cars at third quartile
# so, split at 15700

data_Logistic$price <- cut(data_Logistic$price, c(5150, 15700, Inf), labels=c('0','1'))
summary(data_Logistic$price)

mod_lm = glm(formula = price ~ Cylinders + log(curbWeight) + log(stroke) + log(compressionRatio) + 
               log(horsepower) + log(peakRpm), data = data_Logistic, family = "binomial")

summary(mod_lm)              
drop1(mod_lm, test="LRT")

#model selection
step <- stepAIC(mod_lm,direction='both')                       
step <- stepAIC(mod_lm,direction='both', k=log(nrow(data)))   

###
mod_lm5 = glm(formula = price ~ log(curbWeight) + log(compressionRatio) + log(horsepower) + 
                log(peakRpm), data = data_Logistic, family = "binomial")
summary(mod_lm5)
drop1(mod_lm5, test="LRT")

###
data_Logistic$predicted = predict(mod_lm5, data_Logistic, type="response")
data_Logistic$predicted_y <- ifelse(data_Logistic$predicted > 0.5,1,0)

table(observed=data_Logistic$price, predicted=data_Logistic$predicted_y)
prob = table(observed=data_Logistic$price, predicted=data_Logistic$predicted_y)/nrow(data_Logistic)
prob

accuracy = sum(diag(prob))
accuracy                   # good accuracy

exp(mod_lm5$coefficients)

##
library(forestmodel)
exp(cbind(OR=coef(mod_lm5),confint(mod_lm5)))
forest_model(mod_lm5)

