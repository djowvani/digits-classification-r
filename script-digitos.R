# Pixmap for pnm image reading
library(pixmap)

# Matix for number image samples
matrix <- matrix(NA, nrow = length(files), ncol = 4096)

# Read all files from directory
path <- './digitos'
files <- list.files(path)

# Populating matrix with number image samples
for (i in length(files)) {
  # Read a sample file
  numberFileSample <- read.pnm(paste(path, files[i], sep='/'))
  
  # Read attribute 'gray' () as vector
  numberFileSampleVector <- as.vector(numberFileSample@gray)
  
  # Populate it inside the matrix
  matrix[i,] <- numberFileSampleVector
}

# Create dataframe on top of the matrix used
dataframe_digits <- as.data.frame(matrix)

# Populate dataframe with image's class, using file name
for (i in lenght(files)){
  dataframe_digits$class[i] <- substr(files[i], 1, 1)
}

# Create a sample with 80% of the data amount for training
imgSamples <- sample(nrow(dataframe_digits), (0.8 * nrow(dataframe)))

# ...
cl 

# ...
train <- dataframe[imgSamples, -ncol(dataframe_digits)]

# ...
test <- dataframe[-imgSamples, -ncol(dataframe_digits)]

# KNN Algorithm Results, for four passed values (1, 3, 7 & 9)
knnResult_1 <- as.vector(knn(train, test, cl))
knnResult_3 <- as.vector(knn(train, test, cl, k = 3))
knnResult_7 <- as.vector(knn(train, test, cl, k = 7))
knnResult_9 <- as.vector(knn(train, test, cl, k = 9))

# Expected result for ...
expectedResult <- as.vector(dataframe_digits[-samples, ncol(dataframe_digits)])

# Dataset for KNN results
knnResultsDataset <- as.data.frame(knnResult_1)
knnResultsDataset[,2] <- as.data.frame(knnResult_3)
knnResultsDataset[,3] <- as.data.frame(knnResult_7)
knnResultsDataset[,4] <- as.data.frame(knnResult_9)
knnResultsDataset[,5] <- as.data.frame(expectedResult)

knnAccuracy <- vector()

# Accuracy measures for KNN
for (i in 1:ncol(knnResultsDataset) - 1) {
  tab <- table(knnResultsDataset[,i] == knnResultsDataset[,5])
  freq <- tab[names(tab)==TRUE]
  knnAccuracy[i] <- freq/nrow(knnResultsDataset)
}

# TODO
# Aplicar PCA
# Refazer agrupamento ou KNN