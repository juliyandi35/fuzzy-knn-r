library(readxl)
data_knn <- read_excel("datasmotefix.xlsx")
View(data_knn)

# Preprocessing
data_knn <- data_knn[,-1]
#data_knn <- data_knn[,1:5]
#data_knn <- data_knn[,-6]
View(data_knn)

# Analisis Deskriptif
summary(data_knn)

#pembagian data train dan data test
set.seed(123)
ind <- sample(nrow(data_knn), nrow(data_knn) * 0.8)

train <- data_knn[ind,]
train.x <- train[,-1]
#train.x <- train.x[,-5]
train.x <- na.omit(train.x)

# Min-max normalization function
min_max_normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
# Normalisasi data train
View(train.x)

test <- data_knn[-ind,]
test.x <- test[,-1]
#test.x <- test.x[,-5]
test.x <- na.omit(test.x)

View(test.x)

Prob_kelas_1 <- matrix(0,nrow = nrow(test),ncol = 1)
Prob_kelas_2 <- matrix(0,nrow = nrow(test),ncol = 1)
Prob_kelas_3 <- matrix(0,nrow = nrow(test),ncol = 1)
Prob_kelas_4 <- matrix(0,nrow = nrow(test),ncol = 1)
Prob_kelas_5 <- matrix(0,nrow = nrow(test),ncol = 1)
Prob_kelas_6 <- matrix(0,nrow = nrow(test),ncol = 1)

train$jarak <- 0
train$d <- 0
for (ii in 1:nrow(test)){
  print(ii)
  train <- data_knn[ind==1,]
  train.x <- train[,-1]
  #train.x <- train.x[,-5]
  train.x <- na.omit(train.x)

  test <- data_knn[ind==2,]
  test.x <- test[,-1]
  #test.x <- test.x[,-5]
  test.x <- na.omit(test.x)


  # Perhitungan jarak Euclidean dan derajat keanggotaan
  distance<-sapply(1 : nrow(train.x), function(i) sum(
    (train.x[i, ] - test.x[ii, ])^2))

  for(i in 1:nrow(train)){
    train$jarak[i]=distance[i]
    train$d[i]=train$jarak[i]^(-2)
  }

  # Perhitungan nilai K dan pemilihan data berdasarkan nilai K terkecil
  train1 <- train[train$Y==1,]
  k1 <- min(train1$jarak)
  train1 <- train1[train1$jarak!=k1,]
  k2 <- min(train1$jarak)
  train1 <- train1[train1$jarak!=k2,]

  train1 <- train[train$Y ==1,]
  T1 <- train1[train1$jarak == k1,]
  T2 <- train1[train1$jarak == k2,]

  train2 <- train[train$Y==2,]
  k3 <- min(train2$jarak)
  train2 <- train2[train2$jarak!=k3,]
  k4 <- min(train2$jarak)
  train2 <- train2[train2$jarak!=k4,]

  train2 <- train[train$Y ==2,]
  T3 <- train2[train2$jarak == k3,]
  T4 <- train2[train2$jarak == k4,]

  train3 <- train[train$Y==3,]
  k5 <- min(train3$jarak)
  train3 <- train3[train3$jarak!=k5,]
  k6 <- min(train3$jarak)
  train3 <- train3[train3$jarak!=k6,]

  train3 <- train[train$Y ==3,]
  T5 <- train3[train3$jarak == k5,]
  T6 <- train3[train3$jarak == k6,]

  train4 <- train[train$Y==4,]
  k7 <- min(train4$jarak)
  train4 <- train4[train4$jarak!=k7,]
  k8 <- min(train4$jarak)
  train4 <- train4[train4$jarak!=k8,]

  train4 <- train[train$Y ==4,]
  T7 <- train4[train4$jarak == k7,]
  T8 <- train4[train4$jarak == k8,]

  train5 <- train[train$Y==5,]
  k9 <- min(train5$jarak)
  train5 <- train5[train5$jarak!=k9,]
  k10 <- min(train5$jarak)
  train5 <- train5[train5$jarak!=k10,]

  train5 <- train[train$Y ==5,]
  T9 <- train5[train5$jarak == k9,]
  T10 <- train5[train5$jarak == k10,]

  train6 <- train[train$Y==6,]
  k11 <- min(train6$jarak)
  train6 <- train6[train6$jarak!=k11,]
  k12 <- min(train6$jarak)
  train6 <- train6[train6$jarak!=k12,]

  train6 <- train[train$Y ==6,]
  T11 <- train6[train6$jarak == k11,]
  T12 <- train6[train6$jarak == k12,]


  # Perhitungan Probabilitas Masing-masing Kelas
  S1=T1$d+T2$d
  S2=T3$d+T4$d
  S3=T5$d+T6$d
  S4=T7$d+T8$d
  S5=T9$d+T10$d
  S6=T11$d+T12$d

  D <- S1+S2+S3+S4+S5+S6

  Prob_kelas_1[ii]<-S1/D
  Prob_kelas_2[ii]<-S2/D
  Prob_kelas_3[ii]<-S3/D
  Prob_kelas_4[ii]<-S4/D
  Prob_kelas_5[ii]<-S5/D
  Prob_kelas_6[ii]<-S6/D
}



# Output hasil prediksi di setiap kelas
class_labels <- c(1,2,3,4,5,6)

predicted_labels <- matrix(0,nrow=nrow(test.x))

for (i in 1:nrow(test.x)) {
  probs <- c(Prob_kelas_1[i], Prob_kelas_2[i], Prob_kelas_3[i], Prob_kelas_4[i], Prob_kelas_5[i], Prob_kelas_6[i])
  max_prob_class <- class_labels[which.max(probs)]

  predicted_labels[i] <- max_prob_class
}

# Masukkan label yang sudah terprediksi ke dalam data frame test
test$predicted_label <- predicted_labels

# Tampilkan hasil
View(test)

# Menghitung akurasi
actual_labels <- test$Y
mape <- mean(abs((actual_labels - predicted_labels) / actual_labels)) * 100
mse <- mean((actual_labels - predicted_labels)^2)
correct_predictions <- sum(actual_labels == predicted_labels)
total_predictions <- nrow(test)
accuracy <- correct_predictions / total_predictions
cat("Nilai MAPE dari prediksi ini adalah:",mape)
cat("Nilai MSE dari prediksi ini adalah:",mse)
cat("Akurasi secara umum dari prediksi ini adalah:",accuracy*100)
