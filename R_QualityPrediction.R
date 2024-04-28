# Import the merged dataset
winequality <- read.csv("/Users/m2air/Downloads/WineData/winequality.csv", header = TRUE)

# Display column names
names(winequality)

# Check the dimensions of the dataset
dim(winequality)

# Display the structure of the dataset
str(winequality)
# Display summary statistics
summary(winequality)

head(winequality)

# Display histograms for numerical variables (excluding 'type')
par(mfrow=c(3, 4))  # Set up a 3x4 grid for plots
hist(winequality$fixed.acidity, main = "Fixed Acidity Distribution", xlab = "Fixed Acidity")
hist(winequality$volatile.acidity, main = "Volatile Acidity Distribution", xlab = "Volatile Acidity")
hist(winequality$citric.acid, main = "Citric Acid Distribution", xlab = "Citric Acid")
hist(winequality$residual.sugar, main = "Residual Sugar Distribution", xlab = "Residual Sugar")
hist(winequality$chlorides, main = "Chlorides Distribution", xlab = "Chlorides")
hist(winequality$free.sulfur.dioxide, main = "Free Sulfur Dioxide Distribution", xlab = "Free Sulfur Dioxide")
hist(winequality$total.sulfur.dioxide, main = "Total Sulfur Dioxide Distribution", xlab = "Total Sulfur Dioxide")
hist(winequality$density, main = "Density Distribution", xlab = "Density")
hist(winequality$pH, main = "pH Distribution", xlab = "pH")
hist(winequality$sulphates, main = "Sulphates Distribution", xlab = "Sulphates")
hist(winequality$alcohol, main = "Alcohol Distribution", xlab = "Alcohol")
hist(winequality$quality, main = "Quality Distribution", xlab = "Quality")

# Set up the layout for plots
par(mfrow = c(3, 4))
# Create boxplots for numerical variables by wine type
boxplot(fixed.acidity ~ type, data = winequality, main = "Fixed Acidity by Wine Type", xlab = "Wine Type", ylab = "Fixed Acidity")
boxplot(volatile.acidity ~ type, data = winequality, main = "Volatile Acidity by Wine Type", xlab = "Wine Type", ylab = "Volatile Acidity")
boxplot(citric.acid ~ type, data = winequality, main = "Citric Acid by Wine Type", xlab = "Wine Type", ylab = "Citric Acid")
boxplot(residual.sugar ~ type, data = winequality, main = "Residual Sugar by Wine Type", xlab = "Wine Type", ylab = "Residual Sugar")
boxplot(chlorides ~ type, data = winequality, main = "Chlorides by Wine Type", xlab = "Wine Type", ylab = "Chlorides")
boxplot(free.sulfur.dioxide ~ type, data = winequality, main = "Free Sulfur Dioxide by Wine Type", xlab = "Wine Type", ylab = "Free Sulfur Dioxide")
boxplot(total.sulfur.dioxide ~ type, data = winequality, main = "Total Sulfur Dioxide by Wine Type", xlab = "Wine Type", ylab = "Total Sulfur Dioxide")
boxplot(density ~ type, data = winequality, main = "Density by Wine Type", xlab = "Wine Type", ylab = "Density")
boxplot(pH ~ type, data = winequality, main = "pH by Wine Type", xlab = "Wine Type", ylab = "pH")
boxplot(sulphates ~ type, data = winequality, main = "Sulphates by Wine Type", xlab = "Wine Type", ylab = "Sulphates")
boxplot(alcohol ~ type, data = winequality, main = "Alcohol by Wine Type", xlab = "Wine Type", ylab = "Alcohol")
boxplot(quality ~ type, data = winequality, main = "Quality by Wine Type", xlab = "Wine Type", ylab = "Quality")

# Function to Winsorize a vector
winsorize <- function(x, p = 0.05) {
  q <- quantile(x, probs = c(p, 1 - p), na.rm = TRUE)
  x[x < q[1]] <- q[1]
  x[x > q[2]] <- q[2]
  return(x)
}

# Apply Winsorization to all numerical variables except 'type'
numerical_vars <- winequality[, sapply(winequality, is.numeric)]
winequality[, names(numerical_vars)] <- sapply(numerical_vars, winsorize)

# Display summary statistics after Winsorization
summary(winequality)

# Extract the relevant quantitative predictor variables
predictor_variables <- winequality[, c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides", "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol")]

# Calculate the correlation matrix
correlation_matrix <- cor(predictor_variables)

# Print the correlation matrix
print("Correlation Matrix:")
print(correlation_matrix)

# Check for multicollinearity issue
max_correlation <- max(abs(correlation_matrix))
if (max_correlation < 0.9) {
  print("There is no multicollinearity issue in this model.")
} else {
  print("Multicollinearity may be a problem in this model.")
}

# Compute Variance Inflation Factors (VIF) manually
vif_values <- sapply(1:ncol(predictor_variables), function(i) {
  lm_out <- lm(predictor_variables[, i] ~ ., data = predictor_variables[, -i])
  1 / (1 - summary(lm_out)$r.squared)
})

# Print Variance Inflation Factors (VIF)
print("Variance Inflation Factors (VIF):")
print(vif_values)

# Calculate the correlation matrix
correlation_matrix <- cor(predictor_variables)

# Find highly correlated variable pairs
correlated_pairs <- which(correlation_matrix > 0.9 & correlation_matrix < 1, arr.ind = TRUE)

# Print highly correlated variable pairs
print("Highly Correlated Variable Pairs:")
print(correlated_pairs)

# Remove one variable from each highly correlated pair
if (length(correlated_pairs) > 0) {
  vars_to_remove <- unique(c(correlated_pairs[, 1], correlated_pairs[, 2]))
  predictor_variables <- predictor_variables[, -vars_to_remove]
  print("Variables removed due to multicollinearity:")
  print(colnames(predictor_variables))
} else {
  print("There are no highly correlated variables.")
}
#
#
#
##
#
#
#
#

# Model Building
# Fit the multiple linear regression model
model <- lm(quality ~ ., data = data)

# Model Evaluation
# Summary of the model
summary(model)

# Assess model assumptions
par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
plot(model)

# Make predictions
predictions <- predict(model, data)

# Assess prediction accuracy
accuracy <- sqrt(mean((data$quality - predictions)^2))
print(paste("Root Mean Squared Error (RMSE):", round(accuracy, 2)))

# Extract coefficients
coefficients <- coef(model)

# Print coefficients
print(coefficients)


