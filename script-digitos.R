# === Libraries ===
# Pixmap for pnm image reading
library(pixmap)
library(class)

# Preparing data files & data structure
# Read all files from directory
path <- './digitos'
files <- list.files(path)

# Matrix for number image samples
matrix <- matrix(data = NA, nrow = length(files), ncol = 4096)

# Populating matrix with number image samples (from 1 :to: 1949)
for (i in 1:length(files)) {
  # Read a sample file using the path + filename
  numberFileSample <- read.pnm(paste(path, files[i], sep='/'))
  
  # Read attribute 'grey' as vector ('grey' is the fille attribute that reads the image as a 'boolean matrix')
  numberFileSampleVector <- as.vector(numberFileSample@grey)
  
  # Populate it inside the matrix
  matrix[i,] <- numberFileSampleVector
}

# Create dataframe on top of the matrix used, so it can have the last column with the number's classes
dataframe_digits <- as.data.frame(matrix)

# Populate dataframe with image's class, using file name's first character
for (i in 1:length(files)) {
  dataframe_digits$class[i] <- substr(files[i], 1, 1)
}

# Create a sample, with 80% of the data amount, for training the AI model
# (total, desired %)
imgSamples <- sample(nrow(dataframe_digits), (0.8 * nrow(dataframe_digits)))

# Getting classes (last column) from the 80% train data
number_classes <- dataframe_digits[imgSamples, ncol(dataframe_digits)]

# Getting train data from the 80% train data (the 'minus' represents INVERSE SELECTION, if imgSamples is 80%, then -imgSamples is 20%)
train <- dataframe_digits[imgSamples, -ncol(dataframe_digits)]

# Getting test data from the 20% train data
test <- dataframe_digits[-imgSamples, -ncol(dataframe_digits)]

# === Classification ===
# Uses k-nearest neighbour for classification
# KNN algorithm results, for four passed values (1, 3, 7 and 9)
knnResult_1 <- as.vector(knn(train, test, number_classes))
knnResult_3 <- as.vector(knn(train, test, number_classes, k = 3))
knnResult_7 <- as.vector(knn(train, test, number_classes, k = 7))
knnResult_9 <- as.vector(knn(train, test, number_classes, k = 9))

# Expected results, the actual classes of the numbers
expectedResult <- as.vector(dataframe_digits[-imgSamples, ncol(dataframe_digits)])

# Dataset for storing KNN results and expected results
knnResultsDataset <- as.data.frame(knnResult_1)
knnResultsDataset[,2] <- as.data.frame(knnResult_3)
knnResultsDataset[,3] <- as.data.frame(knnResult_7)
knnResultsDataset[,4] <- as.data.frame(knnResult_9)
knnResultsDataset[,5] <- as.data.frame(expectedResult)

# Vector to store matching results KNN ~ Classes
knnAccuracy <- vector()

# Accuracy measures for KNN
for (i in 1:ncol(knnResultsDataset) - 1) {
  # Compares if the results acquired from knn match to the classes, returns true or false
  true_false_table <- table(knnResultsDataset[,i] == knnResultsDataset[,5])
  
  # Times a 'true' was stored in the table
  true_frequency <- true_false_table[names(true_false_table) == TRUE]
  
  # Amount of 'true' divided by amount of rows = accuracy %
  knnAccuracy[i] <- true_frequency/nrow(knnResultsDataset)
}


# Projeto 2
# Utilizando o dataset de dígitos escritos à mão, realize as seguintes tarefas de machine
# learning:
#  - Classificação
#  - Clusterização.
# Verifique a acurácia destes algoritmos.
# Aplique PCA para realizar a redução de dimensionalidade. Aplique os algoritmos
# novamente, e verifique a acurácia. Houve alguma melhora na acurácia para este caso?
#  Entrega:
#  O projeto deve ser realizado em duplas.
# Cada dupla deverá entregar um relatório contendo:
#  - gráficos de visualização dos dados que sejam de interesse para entendimento do
# problema;
#  - Acurácia dos algoritmos;
#  - Código fonte R.
# Data de entrega: 27/11