# Load required libraries
library(rpart)
library(rpart.plot)
library(ggplot2)

# ============================================
# 1. BASIC DECISION TREE
# ============================================

data(iris)

head(iris)
str(iris)
summary(iris)

set.seed(123)
train_indices <- sample(1:nrow(iris), 0.7 * nrow(iris))
train_data <- iris[train_indices, ]
test_data <- iris[-train_indices, ]

tree_model <- rpart(Species ~ ., 
                    data = train_data, 
                    method = "class",
                    control = rpart.control(
                      minsplit = 5,
                      minbucket = 2,
                      cp = 0.01
                    ))

print(tree_model)
summary(tree_model)

predictions <- predict(tree_model, test_data, type = "class")

confusion_matrix <- table(Predicted = predictions, Actual = test_data$Species)
print(confusion_matrix)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

# ============================================
# 2. VISUALIZATION
# ============================================

rpart.plot(tree_model,
           type = 1,
           extra = 1,
           under = TRUE,
           fallen.leaves = TRUE,
           main = "Decision Tree for Iris Species")

rpart.plot(tree_model,
           type = 4,
           extra = 101,
           box.palette = "GnBu",
           branch.lty = 3,
           shadow.col = "gray",
           nn = TRUE,
           main = "Detailed Decision Tree")

# ============================================
# 3. FUNCTIONS
# ============================================

predict_species <- function(sepal_length, sepal_width, petal_length, petal_width) {
  new_data <- data.frame(
    Sepal.Length = sepal_length,
    Sepal.Width = sepal_width,
    Petal.Length = petal_length,
    Petal.Width = petal_width
  )
  
  prediction <- predict(tree_model, new_data, type = "class")
  probabilities <- predict(tree_model, new_data, type = "prob")
  
  list(
    species = as.character(prediction),
    probabilities = setNames(as.vector(probabilities),
                             c("setosa", "versicolor", "virginica"))
  )
}

# ============================================
# 4. EXAMPLES
# ============================================

example1 <- predict_species(5.1, 3.5, 1.4, 0.2)
cat("Example 1 predicted:", example1$species, "\n")

example2 <- predict_species(6.3, 2.8, 4.9, 1.8)
cat("Example 2 predicted:", example2$species, "\n")

example3 <- predict_species(5.9, 3.0, 4.2, 1.3)
cat("Example 3 predicted:", example3$species, "\n")

# ============================================
# 5. DECISION BOUNDARY PLOT
# ============================================

petal_length_grid <- seq(min(iris$Petal.Length), max(iris$Petal.Length), length.out = 100)
petal_width_grid <- seq(min(iris$Petal.Width), max(iris$Petal.Width), length.out = 100)

grid <- expand.grid(
  Petal.Length = petal_length_grid,
  Petal.Width = petal_width_grid,
  Sepal.Length = mean(iris$Sepal.Length),
  Sepal.Width = mean(iris$Sepal.Width)
)

grid$Prediction <- predict(tree_model, grid, type = "class")

ggplot() +
  geom_tile(data = grid, aes(x = Petal.Length, y = Petal.Width, fill = Prediction), alpha = 0.6) +
  geom_point(data = iris, aes(x = Petal.Length, y = Petal.Width, color = Species), size = 2) +
  theme_minimal()

# ============================================
# 6. START INTERACTIVE SESSION (UNCOMMENTED)
# ============================================

cat("\nModel accuracy:", round(accuracy * 100, 2), "%\n")

interactive_prediction_menu()

# ============================================
# SAVE MODEL (UNCOMMENTED)
# ============================================

save_model_for_future <- function() {
  saveRDS(tree_model, file = "iris_decision_tree_model.rds")
  cat("Model saved successfully!\n")
}

save_model_for_future()
