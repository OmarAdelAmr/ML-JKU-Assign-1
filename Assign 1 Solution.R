# Name: Omar Amr
# Matrikel-Nr: k11776960

library(class)

directory <- '~/Desktop/1- JKU Material/02- Machine Learning - Supervised Techniques UE/ML-JKU-Assign-1'

setwd(directory)
original_data_set <- read.csv('DataSet1.csv', sep = ',', header = FALSE, stringsAsFactors = FALSE)

flipped_data_set <- original_data_set
data_set_with_noise <- original_data_set

set.seed(5000)

plotting_function <- function(x_values, y_values, description)
{
  plot(x_values, y_values,type = "l", col="red", xlab = "K Value", ylab = "Error Average Value",
       main = description, cex.lab=1.2, cex.axis = 1.2, cex=1.2, xaxt="n")
  axis(side = 1, at = x_values)
}

flipping_function <- function()
{
  for (x in (1:nrow(original_data_set)))
    {
      flipping_probability <- floor(runif(1, 1, 100))
      if(flipping_probability <= 20)
      {
        flipped_data_set[x, 3] <<-  flipped_data_set[x, 3] * -1
      }
    }
}

add_noise_function <- function()
{
  random_vector1 <- runif(200, 0.0, 1.0)
  random_vector2 <- runif(200, 0.0, 1.0)
  random_vector3 <- runif(200, 0.0, 1.0)
  random_vector4 <- runif(200, 0.0, 1.0)
  data_set_with_noise[4] <<- random_vector1
  data_set_with_noise[5] <<- random_vector2
  data_set_with_noise[6] <<- random_vector1
  data_set_with_noise[7] <<- random_vector2
}

error_calculation_function <- function(current_data_set, description)
{
  error_values <- vector()
  for (kCounter in seq(1, 51, 2)) 
  {
    error_counter <- 0
    
    for(counter in seq(1, 10))
    {
      testingStartIndex <- (20 * (counter - 1)) + 1
      testingEndIndex <- testingStartIndex + 19
      
      testingSet <- current_data_set[testingStartIndex: testingEndIndex, ]
      testingSetTarget <- testingSet[, 3]
      testingSet <- testingSet[, -3]
      
      trainingSet <- current_data_set[-(testingStartIndex: testingEndIndex), ]
      trainingSetTarget <- trainingSet[ ,3]
      trainingSet <- trainingSet[, -3]
      
      knn_result <- knn(train=trainingSet, test=testingSet,cl=trainingSetTarget, k = kCounter)
      error_counter <- error_counter + table(testingSetTarget, knn_result)[1,2] + table(testingSetTarget, knn_result)[2,1]
    }
    error_values[kCounter] <- error_counter
  }
  error_values <- error_values[!is.na(error_values)] / 200
  plotting_function(seq(1, 51, 2), error_values, description)
}

error_calculation_function(original_data_set, "KNN Visualization (Original Data Set)")

flipping_function()
error_calculation_function(flipped_data_set, "KNN Visualization After Flipping Lables With 20% Probability")

add_noise_function()
error_calculation_function(data_set_with_noise, "KNN Visualization After Adding 4 Noise Features")