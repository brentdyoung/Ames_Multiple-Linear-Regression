#Brent Young
#AMES - Multiple Linear Regression

setwd("F:/Northwestern - M.S. Predictive Analytics/Past Courses/Spring 2017_Course 410_Regression & Multivariate Analysis/Assignments/Ames/R/Ames")

##################

mydata <- read.csv(file="ames_housing_data.csv",head=TRUE,sep=",")

str(mydata)
head(mydata)
names(mydata)
mydata$TotalFloorSF <- mydata$FirstFlrSF + mydata$SecondFlrSF
mydata$HouseAge <- mydata$YrSold - mydata$YearBuilt
mydata$QualityIndex <- mydata$OverallQual * mydata$OverallCond

subdat <- subset(mydata, select=c("TotalFloorSF","HouseAge","QualityIndex",
                                  "SalePrice","LotArea","BsmtFinSF1",
                                   "HouseStyle","LotShape"))

str(subdat)
subdatnum <- subset(mydata, select=c("TotalFloorSF","HouseAge","QualityIndex",
                                  "SalePrice","LotArea"))
#####################################################################
######################### Assignment 1 ##############################
#####################################################################

#################################################################
################## univariate EDA ##############################
###############################################################
require(ggplot2)
ggplot(subdat) +
  geom_bar( aes(LotShape) ) +
  ggtitle("Number of houses per Lotshape") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=SalePrice)) + 
  geom_histogram(color="black", binwidth= 10000) +
  labs(title="Distribution of Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=TotalFloorSF)) + 
  geom_histogram(color="black", binwidth= 100) +
  labs(title="Distribution of TotalFloorSF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=QualityIndex)) + 
  geom_histogram(color="black", binwidth= 10) +
  labs(title="Distribution of QualityIndex") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
#######################################################################
########### bivariate EDA ########################################
###################################################################
ggplot(subdat, aes(x=TotalFloorSF, y=QualityIndex)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Total Floor SF vs QualityIndex") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=TotalFloorSF, y=HouseAge)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Total Floor SF vs HouseAge") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat, aes(x=LotShape, y=HouseAge)) + 
  geom_boxplot(fill="blue") +
  labs(title="Distribution of HouseAge") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

############################################################
################ model focussed EDA #######################
###########################################################

ggplot(subdat, aes(x=TotalFloorSF, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs Total Floor SF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) 
  ## geom_smooth(method=lm)  ## method=lm, se=FALSE ###

ggplot(subdat, aes(x=QualityIndex, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs QualityIndex") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) 

ggplot(subdat, aes(x=LotShape, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Distribution of Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#####################################################################
############# EDA for multiple variables ###########################
##################################################################
require(GGally)
ggpairs(subdat)

require(lattice)
pairs(subdat, pch = 21)

require(corrplot)
mcor <- cor(subdatnum)
corrplot(mcor, method="shade", shade.col=NA, tl.col="black",tl.cex=0.5)

#####################################################################
############# Define the sample data ###########################
##################################################################

subdat2 <- subdat[which(subdat$TotalFloorSF < 4000),]

###################################################################
##################  Assignment 2  ################################
#################################################################

attach(subdat2)

ggplot(subdat2, aes(x=TotalFloorSF, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs Total Floor SF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) + 
  geom_smooth(method=lm)  ## method=lm, se=FALSE ###

ggplot(subdat2, aes(x=QualityIndex, y=SalePrice)) + 
  geom_point(color="blue", shape=1) +
  ggtitle("Scatter Plot of Sale Price vs QualityIndex") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm)  ## method=lm, se=FALSE ###



# 3D Scatterplot with Coloring and Vertical Lines
# and Regression Plane 
library(scatterplot3d)
attach(subdat2)
s3d <-scatterplot3d(TotalFloorSF,QualityIndex,SalePrice,pch=16, 
                    highlight.3d=TRUE,type="h", main="3D Scatterplot")
fit <- lm(SalePrice ~ TotalFloorSF + QualityIndex) 
s3d$plane3d(fit)

library(Rcmdr)
attach(subdat2)
scatter3d(SalePrice,TotalFloorSF,QualityIndex)

############## fitting a SLR ###################################

SLRresult = lm(SalePrice ~ TotalFloorSF, data=subdat2)
anova(SLRresult)
summary(SLRresult)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(SLRresult)
pred <- as.data.frame(predict(SLRresult,subdat2,interval="prediction"))
str(pred)
head(pred)
subdat2 <- cbind(subdat2,pred)
str(subdat2)
head(subdat2)
subdat2 <- subset( subdat2, select = -lwr)
subdat2 <- subset( subdat2, select = -upr)
library(reshape)
subdat2 <- rename(subdat2, c(fit="fitSLR"))


############## fitting a MLR ###################################

MLRresult = lm(SalePrice ~ TotalFloorSF+QualityIndex, data=subdat2)
anova(MLRresult)
summary(MLRresult)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRresult)

pred <- as.data.frame(predict(MLRresult,subdat2,interval="prediction"))
str(pred)
head(pred)
subdat2 <- cbind(subdat2,pred)
str(subdat2)
head(subdat2)
subdat2 <- subset( subdat2, select = -lwr)
subdat2 <- subset( subdat2, select = -upr)
subdat2 <- rename(subdat2, c(fit="fitMLR"))
head(subdat2)

###################################################################
##################### Assignment 3  ################################
#################################################################
dev.off()
par(mfrow=c(1,1))
library(car)
vif(MLRresult)
influencePlot(MLRresult,	id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance" )

summary(inflm.MLR <- influence.measures(MLRresult))
dffits <- dffits(MLRresult)
subdat2 <- cbind(subdat2,dffits)
str(subdat2)
######################## Compare Models #########################
anova(SLRresult, MLRresult)

############categorical variables #################
ggplot(subdat2) +
  geom_bar( aes(HouseStyle) ) +
  ggtitle("Number of houses per HouseStyle") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(subdat2, aes(x=HouseStyle, y=SalePrice)) + 
  geom_boxplot(fill="blue") +
  labs(title="Distribution of Sale Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

MLRresult2 = lm(SalePrice ~ TotalFloorSF+QualityIndex+HouseStyle, data=subdat2)
anova(MLRresult2)
summary(MLRresult2)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRresult2)
par(mfrow=c(1,1))
pred <- as.data.frame(predict(MLRresult2,subdat2,interval="prediction"))
str(pred)
head(pred)
subdat2 <- cbind(subdat2,pred)
str(subdat2)
head(subdat2)
subdat2 <- subset( subdat2, select = -lwr)
subdat2 <- subset( subdat2, select = -upr)
subdat2 <- rename(subdat2, c(fit="fitMLR2"))
################ Define dummy variable #######################
subdat2$Style1 <- ifelse(subdat2$HouseStyle == "1Story", 1, 0)
subdat2$Style2 <- ifelse(subdat2$HouseStyle == "2Story", 1, 0)
MLRresult3 = lm(SalePrice ~ TotalFloorSF+QualityIndex+Style1+Style2, data=subdat2)
anova(MLRresult3)
summary(MLRresult3)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRresult3)
par(mfrow=c(1,1))
pred <- as.data.frame(predict(MLRresult3,subdat2,interval="prediction"))
str(pred)
head(pred)
subdat2 <- cbind(subdat2,pred)
str(subdat2)
head(subdat2)
subdat2 <- subset( subdat2, select = -lwr)
subdat2 <- subset( subdat2, select = -upr)
subdat2 <- rename(subdat2, c(fit="fitMLR3"))
str(subdat2)

subdat2$logSalePrice <- log(subdat2$SalePrice)
MLRresult4 = lm(logSalePrice ~ TotalFloorSF+QualityIndex+Style1+Style2, data=subdat2)
anova(MLRresult4)
summary(MLRresult4)
par(mfrow=c(2,2))  # visualize four graphs at once
plot(MLRresult4)
############################################################
############## assignment 5 #############################
#########################################################

# Set the seed on the random number generator so you get the same split every time that
# you run the code.
my.data <- subdat2
set.seed(123)
my.data$u <- runif(n=dim(my.data)[1],min=0,max=1)

# Create train/test split;
train.df <- subset(my.data, u<0.70);
test.df  <- subset(my.data, u>=0.70);
names(train.df)

train.clean <- subset(train.df, select=c("TotalFloorSF","HouseAge","QualityIndex",
                                     "SalePrice","LotArea","Style1","Style2"))

test.clean <- subset(test.df, select=c("TotalFloorSF","HouseAge","QualityIndex",
                                       "SalePrice","LotArea","Style1","Style2"))



# Check your data split. The sum of the parts should equal the whole.
# Do your totals add up?
dim(my.data)[1]
dim(train.df)[1]
dim(test.df)[1]
dim(train.df)[1]+dim(test.df)[1]

train.clean <- na.omit(train.clean)
test.clean <- na.omit(test.clean)


# Define the upper model as the FULL model
upper.lm <- lm(SalePrice ~ .,data=train.clean);
summary(upper.lm)

# Define the lower model as the Intercept model
lower.lm <- lm(SalePrice ~ -1,data=train.clean);
summary(lower.lm)
# Need a SLR to initialize stepwise selection
sqft.lm <- lm(SalePrice ~ TotalFloorSF,data=train.clean);
summary(sqft.lm)

# Note: There is only one function for classical model selection in R - stepAIC();
# stepAIC() is part of the MASS library.
# The MASS library comes with the BASE R distribution, but you still need to load it;
library(MASS)

# Call stepAIC() for variable selection

forward.lm <- stepAIC(object=lower.lm,scope=list(upper=upper.lm,lower=lower.lm),
                      direction=c('forward'));
summary(forward.lm)

backward.lm <- stepAIC(object=upper.lm,direction=c('backward'));
summary(backward.lm)

stepwise.lm <- stepAIC(object=sqft.lm,scope=list(upper=formula(upper.lm),lower=~1),
                       direction=c('both'));
summary(stepwise.lm)

junk.lm <- lm(SalePrice ~ QualityIndex + LotArea, data=train.clean)
summary(junk.lm)

library(car)
sort(vif(forward.lm),decreasing=TRUE)
sort(vif(backward.lm),decreasing=TRUE)
sort(vif(stepwise.lm),decreasing=TRUE)
sort(vif(junk.lm),decreasing=TRUE)

forward.test <- predict(forward.lm,newdata=test.clean);
backward.test <- predict(backward.lm,newdata=test.clean);
stepwise.test <- predict(stepwise.lm,newdata=test.clean);
junk.test <- predict(junk.lm,newdata=test.clean);

# Training Data
# Abs Pct Error
forward.pct <- abs(forward.lm$residuals)/train.clean$SalePrice;
MAPE <- mean(forward.pct)
MAPE
backward.pct <- abs(backward.lm$residuals)/train.clean$SalePrice;
MAPE <- mean(backward.pct)
MAPE
stepwise.pct <- abs(stepwise.lm$residuals)/train.clean$SalePrice;
MAPE <- mean(stepwise.pct)
MAPE
junk.pct <- abs(junk.lm$residuals)/train.clean$SalePrice;
MAPE <- mean(junk.pct)
MAPE

# Test Data
# Abs Pct Error
forward.testPCT <- abs(test.df$SalePrice-forward.test)/test.df$SalePrice;
MAPE <- mean(forward.testPCT)
MAPE
backward.testPCT <- abs(test.df$SalePrice-backward.test)/test.df$SalePrice;
MAPE <- mean(backward.testPCT)
MAPE
stepwise.testPCT <- abs(test.df$SalePrice-stepwise.test)/test.df$SalePrice;
MAPE <- mean(stepwise.testPCT)
MAPE
junk.testPCT <- abs(test.df$SalePrice-junk.test)/test.df$SalePrice;
MAPE <- mean(junk.testPCT)
MAPE


# Assign Prediction Grades training data;
forward.PredictionGrade <- ifelse(forward.pct<=0.10,'Grade 1: [0.0.10]',
                                  ifelse(forward.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(forward.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]')
                                  )					
)

forward.trainTable <- table(forward.PredictionGrade)
forward.trainTable/sum(forward.trainTable)


# Assign Prediction Grades test data;
forward.testPredictionGrade <- ifelse(forward.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(forward.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(forward.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )					
)

forward.testTable <-table(forward.testPredictionGrade)
forward.testTable/sum(forward.testTable)




