# Select PC1 to PC8
pca_data <- GM_perforation_centered_scaled.PCA$x[, 1:8]
# Ensure that 'Type' is a factor
Type <- as.factor(GM_perforation_centered_scaled.PCA$fac$Type)


# Perform LDA
lda_result <- MASS::lda(pca_data, grouping = Type)


# Load the necessary libraries
library(MASS)
library(ggplot2)

# Assuming pca_data and Type are already available

# Perform LDA
lda_result <- MASS::lda(pca_data, grouping = Type)

plot(lda_result)

# Get the predicted values from LDA
lda_pred <- predict(lda_result)

# Create a data frame for plotting
lda_df <- data.frame(lda_pred$x, Type = Type)

# Plot the LDA results
ggplot(lda_df, aes(x = LD1, y = LD2, color = Type)) +
  geom_point() +
  labs(title = "LDA: Linear Discriminant Analysis",
       x = "LD1", y = "LD2") +
  theme_minimal()


# Load the necessary libraries
library(MASS)
library(ggplot2)
library(dplyr)  # for data manipulation
library(ggalt)  # for convex hulls

# Assuming pca_data and Type are already available

# Perform LDA
lda_result <- lda(pca_data, grouping = Type)

# Get the predicted values from LDA
lda_pred <- predict(lda_result)

# Create a data frame for plotting
lda_df <- data.frame(lda_pred$x, Type = Type)

# Calculate convex hulls for each group
lda_hull <- lda_df %>%
  group_by(Type) %>%
  slice(chull(LD1, LD2))  # This will create the convex hull

# Plot the LDA results with convex hulls
ggplot(lda_df, aes(x = LD1, y = LD2, color = Type)) +
  geom_point() +  # Plot the points
  geom_polygon(data = lda_hull, aes(x = LD1, y = LD2, fill = Type, color = Type), alpha = 0.3, linetype = 1) +
  labs(title = "LDA: Linear Discriminant Analysis with Convex Hulls",
       x = "LD1", y = "LD2") +
  theme_minimal() +
  theme(legend.position = "top")



