setwd('~/Desktop/1- JKU Material/02- Machine Learning - Supervised Techniques UE/Assignment 1')
origData <- read.csv2('DataSet1.csv', sep = ',', header = FALSE, stringsAsFactors = FALSE)

set.seed(123)

training_sample_size <- floor(0.9 * nrow(origData))
training_index <- sample(seq_len(nrow(origData)), size = training_sample_size)

trainingSet <- origData[training_index, ]
testingSet <- origData[-training_index, ]

for(k in seq(1, 51, 2))
{
  print(paste("K is: ", k))
  
}

