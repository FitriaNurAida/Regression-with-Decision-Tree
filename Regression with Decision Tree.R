# Import Data #
movie <- read.csv("E:/My Dictionary/Using R/Data/Movie_regression.csv")
View(movie)

# Data Preprocessing #
summary(movie) #there are missing values in variable Time_taken
movie$Time_taken[is.na(movie$Time_taken)] <- mean(movie$Time_taken,na.rm = TRUE)

# Test-Train Split
install.packages('caTools')
library(caTools)
set.seed(0)
split <- sample.split(movie,SplitRatio = 0.8)
train <- subset(movie,split == TRUE)
test <- subset(movie,split == FALSE)

############################### MODELING #################################
install.packages('rpart')
install.packages('rpart.plot')
library(rpart)
library(rpart.plot)

regtree <- rpart(formula = Collection~., data = train, control = rpart.control(maxdepth = 3))
rpart.plot(regtree, box.palette="RdBu", digits = -3) 
test$pred <- predict(regtree, test, type = "vector")


MSE <- mean((test$pred - test$Collection)^2); MSE
RMSE <- sqrt(MSE); RMSE

# Tree Pruning
fulltree <- rpart(formula = Collection~., data = train, control = rpart.control( cp = 0)) #cp=0 means full tree wil grow
rpart.plot(fulltree, box.palette="RdBu", digits = -3)
printcp(fulltree)
plotcp(regtree)

mincp <- regtree$cptable[which.min(regtree$cptable[,"xerror"]),"CP"]
prunedtree <- prune(fulltree, cp = mincp)
rpart.plot(prunedtree, box.palette="RdBu", digits = -3)

test$fulltree <- predict(fulltree, test, type = "vector")
MSEfull <- mean((test$fulltree - test$Collection)^2); MSEfull
RMSEfull <-sqrt(MSEfull); RMSEfull

test$pruned <- predict(prunedtree, test, type = "vector")
MSEpruned <- mean((test$pruned - test$Collection)^2);MSEpruned
RMSEpruned <- sqrt(MSEpruned); RMSEpruned


