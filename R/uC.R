library(tidyverse)
library(caret)

# Načtení a úprava dat
diabetes_data <- read.csv("data-input/uC/data_C_diabetes_indiani.csv") %>% 
  mutate(test = as.factor(test))

# analýza příznaků

summary(diabetes_data)

dia.pca <- prcomp(diabetes_data[, 1:8], center = TRUE, scale. = TRUE)
summary(dia.pca)

# Rozdělení na trénovací a testovací množinu
set.seed(123456)
train_idx <- createDataPartition(diabetes_data$test, p = 0.8, list = FALSE)
train_data <- diabetes_data[train_idx, ]
test_data <- diabetes_data[-train_idx, ]

# Normalizace (min-max škálování)
preproc <- preProcess(train_data, method = c("range"))
train_scaled <- predict(preproc, train_data)
test_scaled <- predict(preproc, test_data)

# Nastavení kontrol pro trénování modelu
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  sampling = "up"
)

# Trénování SVM
set.seed(123)
svm_model <- train(
  test ~ ., 
  data = train_scaled,
  method = "svmRadial",
  trControl = ctrl,
  metric = "ROC",
  preProcess = c("center", "scale"),
  tuneLength = 10
)

# Trénování k-NN
knn_grid <- expand.grid(k = seq(3, 15, by = 2))
set.seed(123)
knn_model <- train(
  test ~ ., 
  data = train_scaled,
  method = "knn",
  trControl = ctrl,
  metric = "ROC",
  preProcess = c("center", "scale"),
  tuneGrid = knn_grid
)

# Vyhodnocení SVM
svm_pred <- predict(svm_model, newdata = test_scaled)
svm_pred <- factor(svm_pred, levels = levels(train_scaled$test))
test_actual <- factor(test_scaled$test, levels = levels(train_scaled$test))
print(confusionMatrix(svm_pred, test_actual))

# Vyhodnocení k-NN
knn_pred <- predict(knn_model, newdata = test_scaled)
knn_pred <- factor(knn_pred, levels = levels(train_scaled$test))
print(confusionMatrix(knn_pred, test_actual))

# vizualizace ----

library(ggplot2)
library(pROC)
library(reshape2)

# Confusion matrix jako heatmapa
plot_confusion_matrix <- function(cm, title) {
  cm_df <- as.data.frame(cm$table)
  colnames(cm_df) <- c("Actual", "Predicted", "Freq")
  
  ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), size = 5) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(title = title, x = "Predikce", y = "Skutečnost") +
    theme_minimal()
}

# Confusion matrices
cm_svm <- confusionMatrix(svm_pred, test_actual)
cm_knn <- confusionMatrix(knn_pred, test_actual)

# Zobrazit heatmapy
plot_confusion_matrix(cm_svm, "Confusion Matrix - SVM")
plot_confusion_matrix(cm_knn, "Confusion Matrix - k-NN")

# ROC křivky
svm_probs <- predict(svm_model, newdata = test_scaled, type = "prob")
knn_probs <- predict(knn_model, newdata = test_scaled, type = "prob")

roc_svm <- roc(test_actual, svm_probs$negatif)
roc_knn <- roc(test_actual, knn_probs$negatif)

# Plot ROC křivek
plot(roc_svm, col = "blue", legacy.axes = TRUE, main = "ROC křivky SVM vs k-NN")
plot(roc_knn, col = "red", add = TRUE)
legend("bottomright", legend = c("SVM", "k-NN"), col = c("blue", "red"), lwd = 2)
