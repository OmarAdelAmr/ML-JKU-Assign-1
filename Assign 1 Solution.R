setwd('~/Desktop/1- JKU Material/02- Machine Learning - Supervised Techniques UE/ML-JKU-Assign-1')
origData <- read.csv2('DataSet1.csv', sep = ',', header = FALSE, stringsAsFactors = FALSE)
set.seed(123)

origData$V1 <- as.numeric(as.character(origData$V1))
origData$V2 <- as.numeric(as.character(origData$V2))

library(class)

plotting_function <- function(x_values, y_values)
{
  plot(x_values, y_values,type = "l", col="red", xlab = "K Value", ylab = "Error Value",
       main = "KNN Visualization", cex.lab=1.2, cex.axis = 1.2, cex=1.2, xaxt="n")
  axis(side = 1, at = x_values)
}

error_values <- vector()

for (kCounter in seq(1, 51, 2)) 
  {
  error_counter <- 0

  for(counter in seq(1,10))
  {
  testingStartIndex <- (20 * (counter - 1)) + 1
  testingEndIndex <- testingStartIndex + 19
  
  testingSet <- origData[testingStartIndex: testingEndIndex, ]
  testingSetTarget <- testingSet[, 3]
  testingSet <- testingSet[, -3]
  
  trainingSet <- origData[-(testingStartIndex: testingEndIndex), ]
  trainingSetTarget <- trainingSet[ ,3]
  trainingSet <- trainingSet[, -3]
  
  knn_result <- knn(train=trainingSet, test=testingSet,cl=trainingSetTarget, k = kCounter)
  error_counter <- error_counter + table(testingSetTarget, knn_result)[1,2] + table(testingSetTarget, knn_result)[1,2]
  }
  error_values[kCounter] <- error_counter
}
error_values <- error_values[!is.na(error_values)]
plotting_function(seq(1,51,2), error_values)
