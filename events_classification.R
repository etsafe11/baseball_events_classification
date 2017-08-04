# Load required packages
library(plyr)
library(caret)
library(party)
library(mboost)
library(plotly)

# Set working directory
setwd("C://Users/ethompson/Desktop/Projects/Statcast")

# Tidying data
filenames <- list.files(path = "statcast_data", full.names=TRUE)
data_raw <- do.call("rbind", lapply(filenames, read.csv, header = TRUE))
data <- data_raw[,c("launch_angle","launch_speed", "events")]
data$launch_angle <- as.numeric(as.character(data$launch_angle))
data$launch_speed <- as.numeric(as.character(data$launch_speed))
rownames(data) <- NULL
data <- na.omit(data)
data <- (data[data$events != "null", ])
data <- subset(data, events == "home_run" |
                 events == "single" |
                 events == "double" |
                 events == "triple")
data$events <- factor(data$events)

# Partition data into training and test sets
gp <- runif(nrow(data))
data <- data[order(gp), ]
data <- data[1:10000, ]
inTrain <- createDataPartition(data$events, p=0.7, list=FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]
method <- 'rf' # random forest
ctrl <- trainControl(method = 'repeatedcv', number = 5, repeats = 5)
modelFit <- train(events ~ launch_angle + launch_speed, 
        method = method, data = training, trControl = ctrl)
modelFit
# Run the model on the test set
predicted <- predict(modelFit, newdata = testing)
# Confusion matrix
confusionMatrix(testing$events, predicted)

# Plot
# Launch speeds from 40 to 120
x <- seq(40,120, by = 1)
# Launch angles from 10 to 50
y <- seq(10,50, by = 1)
# Make a data frame of the relevant x and y values
plotDF <- data.frame(expand.grid(x,y))
# Add the correct column names
colnames(plotDF) <- c('launch_speed','launch_angle')
# Add the classification
plotPredictions <- predict(modelFit, newdata = plotDF)
plotDF$pred <- plotPredictions

p <- plot_ly(data = plotDF, x = ~ launch_speed, y = ~ launch_angle, 
             color = ~ pred, type = "scatter", mode = "markers") %>%
  layout(title = "Launch Speed + Launch Angle")
p
