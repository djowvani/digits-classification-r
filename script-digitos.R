# ============================= Libraries =============================
# Library for reading pnm images
library(pixmap)

# Library for KNN
library(class)

# Library for SVM
library(e1071)

# Library for Decision Trees
library(rpart)
library(rpart.plot)

# Library for PCA
library(vegan)
library(factoextra)
  
# Library for Confusion Matrix
library(caret)

# ============================= Read Files =============================
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
  dataframe_digits$digit_class[i] <- substr(files[i], 1, 1)
}

# Setting seed for random number generator (for each time we need a new train set)
set.seed(123)

# Create a sample, with 80% of the data amount, for training the AI model (Params: total, desired %)
imgSamples <- sample(nrow(dataframe_digits), (0.8 * nrow(dataframe_digits)))

# Getting classes (last column) from the 80% train data
number_classes <- dataframe_digits[imgSamples, 4097]

# Getting train data from the 80% train data (the 'minus' represents INVERSE SELECTION, if imgSamples is 80%, then -imgSamples is 20%)
train <- dataframe_digits[imgSamples, -4097]

# Getting test data from the 20% train data
test <- dataframe_digits[-imgSamples, -4097]

# ============================= Classification Analysis =============================
# ===== KNN (K-Nearest Neighbors) =====
# Uses k-nearest neighbour for classification
# KNN algorithm results, for four passed values (1, 3, 7 and 9)
knnResult_1 <- as.vector(knn(train, test, number_classes))
knnResult_3 <- as.vector(knn(train, test, number_classes, k = 3))
knnResult_7 <- as.vector(knn(train, test, number_classes, k = 7))
knnResult_9 <- as.vector(knn(train, test, number_classes, k = 9))

# Dataset for storing KNN results
knnResultsDataset <- as.data.frame(knnResult_1)
knnResultsDataset[,2] <- as.data.frame(knnResult_3)
knnResultsDataset[,3] <- as.data.frame(knnResult_7)
knnResultsDataset[,4] <- as.data.frame(knnResult_9)

# Expected results, the actual classes of the numbers
expectedResult <- as.vector(dataframe_digits[-imgSamples, ncol(dataframe_digits)])

# Storing the actual classes on a final column in the dataset
knnResultsDataset[,5] <- as.data.frame(expectedResult)

# Vector to store matching results KNN ~ Classes
knnAccuracy <- vector()

# Accuracy measures for KNN (reading all columns, except for the last one, the actual classes)
knnAccuracy[1] <- length(which(knnResult_1 == expectedResult))/length(expectedResult)
knnAccuracy[2] <- length(which(knnResult_3 == expectedResult))/length(expectedResult)
knnAccuracy[3] <- length(which(knnResult_7 == expectedResult))/length(expectedResult)
knnAccuracy[4] <- length(which(knnResult_9 == expectedResult))/length(expectedResult)

# ===== SVM (Support Vector Machine) =====
# Getting round value for 80% of total row lines from the dataframe
sample_80percent <- floor(0.8 * nrow(dataframe_digits))

# Setting up training data 80%
train_index <- sample(seq_len(nrow(dataframe_digits)), size = sample_80percent)

# "Clone" of the dataframe up to the 80% index
train <- dataframe_digits[train_index, ]
# "Clone" of the dataframe of the remaining 20%
test <- dataframe_digits[-train_index, ]

# Getting test's classes
testClass <- test[,4097]
# Removing test's classes from the test object
test <- test[,-4097]

# SVM execution
classifier = svm(formula = digit_class ~ ., 
                 data = train,
                 type = 'C-classification', 
                 kernel = 'linear'
                 )

# Executes the classifier and generates new test
y_pred = predict(classifier, newdata = test)

# Accuracy measures for SVM
svmAccuracy <- length(which(knnResult_1 == expectedResult))/length(expectedResult)

# ===== Decision Tree =====
# Setting seed for random number generator (for each time we need a new train set)
set.seed(123)

# Getting round value for 80% of total row lines from the dataframe
sample_80percent <- floor(0.8 * nrow(dataframe_digits))

# Setting up training data 80%
train_index <- sample(seq_len(nrow(dataframe_digits)), size = sample_80percent)

# "Clone" of the dataframe up to the 80% index
train <- dataframe_digits[train_index, ]
# "Clone" of the dataframe of the remaining 20%
test <- dataframe_digits[-train_index, ]

# Setting up the decision tree
decisionTree <- rpart(digit_class ~ ., train, method = "class", control = rpart.control(minsplit = 1))

# Plot the tree
plot <- rpart.plot(decisionTree, type = 3)

# Amount of samples that reach the node, amount of samples that doesn't belong to the majority class
classif <- test[,ncol(dataframe_digits)]
test <- test[,-ncol(dataframe_digits)]
pred <- predict(decisionTree, test, type = "class") # Prob or class

# Accuracy measures for Decision Tree
treeAccuracy <- length(which(pred == expectedResult))/length(expectedResult)

# =============================== Clustering Analysis ===============================
# ===== K-Means =====
# Getting classes
data <- dataframe_digits[, -4097]

# K-Means execution with 10 centers
predict <- kmeans(data, 10)

# Reading clusters
table(predict$cluster)

# S3 method for kmeans
fitted(object, method = c("centers", "classes"), ...)

# ===================================== PCA ========================================
# 
digits_pca <- prcomp(dataframe_digits[,c(1:7,10,11)], center = TRUE, scale. = TRUE)

# 
summary(digits_pca)

# 
fviz_eig(digits_pca)
fviz_pca_ind(digits_pca,col.ind = "black", # Color by the quality of representation
             repel = TRUE # Avoid text overlapping
            )

fviz_pca_var(mtcars.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
            )
#90 graus n?o correlacionadas
#180 graus negativamente correlacionadas
#angulo pequeno muito correlacionadas.

fviz_pca_biplot(mtcars.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)



#PCA iris


irs.pca <- prcomp(iris[,1:4], center = TRUE,scale. = TRUE)
summary(irs.pca)

install.packages("factoextra")
library(factoextra)
fviz_eig(irs.pca)
fviz_pca_ind(irs.pca,geom.ind = "point", pointshape = 21,
             pointsize = 2, 
             col.ind = "black", # Color by the quality of representation
             palette = "jco", 
             repel=TRUE,# Avoid text overlapping
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             fill.ind = iris[,5]
            )

fviz_pca_var(irs.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
            )

fviz_pca_biplot(irs.pca,geom.ind = "point", pointshape = 21,
                pointsize = 2, 
                col.ind = "black", # Color by the quality of representation
                palette = "jco", 
                repel=TRUE,# Avoid text overlapping
                addEllipses = TRUE,
                label = "var",
                #                
                fill.ind = iris[,5],
                col.var = "contrib", # Color by contributions to the PC
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
                )

# ===== KNN (Post-PCA) =====
knnResult_1 <- as.vector(knn(train, test, number_classes))
knnResult_3 <- as.vector(knn(train, test, number_classes, k = 3))
knnResult_7 <- as.vector(knn(train, test, number_classes, k = 7))
knnResult_9 <- as.vector(knn(train, test, number_classes, k = 9))

knnResultsDataset <- as.data.frame(knnResult_1)
knnResultsDataset[,2] <- as.data.frame(knnResult_3)
knnResultsDataset[,3] <- as.data.frame(knnResult_7)
knnResultsDataset[,4] <- as.data.frame(knnResult_9)

expectedResult <- as.vector(dataframe_digits[-imgSamples, ncol(dataframe_digits)])

knnResultsDataset[,5] <- as.data.frame(expectedResult)

knnAccuracy <- vector()

for (i in 1:ncol(knnResultsDataset) - 1) {
  true_false_table <- table(knnResultsDataset[,i] == knnResultsDataset[,5])
  true_frequency <- true_false_table[names(true_false_table) == TRUE]
  knnAccuracy[i] <- true_frequency/nrow(knnResultsDataset)
}

# ===== SVM (Post-PCA) =====
sample_80percent <- floor(0.8 * nrow(dataframe_digits))

train_index <- sample(seq_len(nrow(dataframe_digits)), size = sample_80percent)

train <- dataframe_digits[train_index, ]
test <- dataframe_digits[-train_index, ]

testClass <- test[,4097]
test <- test[,-4097]

classifier = svm(formula = digit_class ~ ., 
                 data = train,
                 type = 'C-classification', 
                 kernel = 'linear'
)

y_pred = predict(classifier, newdata = test)

svmAccuracy <- length(which(knnResult_1 == expectedResult))/length(expectedResult)

# ===== Decision Tree (Post-PCA) =====
set.seed(123)

sample_80percent <- floor(0.8 * nrow(dataframe_digits))

train_index <- sample(seq_len(nrow(dataframe_digits)), size = sample_80percent)

train <- dataframe_digits[train_index, ]
test <- dataframe_digits[-train_index, ]

decisionTree <- rpart(digit_class ~ ., train, method = "class", control = rpart.control(minsplit = 1))

plot <- rpart.plot(decisionTree, type = 3)

classif <- test[,ncol(dataframe_digits)]
test <- test[,-ncol(dataframe_digits)]
pred <- predict(decisionTree, test, type = "class")

treeAccuracy <- length(which(pred == expectedResult))/length(expectedResult)

# ===== K-Means (Post-PCA) =====

# ===================================== Plots ========================================


# Projeto 2
# Utilizando o dataset de dígitos escritos à mão, realize as seguintes tarefas de machine
# learning:
#  - Classificação.
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