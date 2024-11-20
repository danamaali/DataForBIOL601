# Install and load necessary packages
install.packages("FactoMineR")
install.packages("factoextra")
library(FactoMineR)
library(factoextra)
library(ggplot2)

# Load the dataset
data <- read.csv("BreastCancerData.csv")

# Check and prepare data
# Assuming the target variable is in the column "Diagnosis" (B for benign, M for malignant)
# and numeric variables are used for PCA
target_var <- "Diagnosis" # Change this to match the actual column name for groups
numeric_data <- data[sapply(data, is.numeric)] # Select numeric variables only

# Perform PCA
pca_result <- PCA(numeric_data, graph = FALSE)

# Scree Plot
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50), 
         title = "Scree Plot: Explained Variance")

# Circle of Correlations
fviz_pca_var(pca_result,
             repel = TRUE, # Avoid label overlap
             title = "Circle of Correlations")

# Biplot with Groups (PC1 and PC2)
# Add the target variable to PCA results for grouping
data$Group <- as.factor(data[[target_var]]) # Convert the target variable to a factor

# Plot PCA individuals with convex hulls for B and M groups
fviz_pca_ind(pca_result,
             geom.ind = "point", # Show individuals as points
             col.ind = data$Group, # Color points by group
             palette = c("red", "blue"), # Colors for groups
             addEllipses = FALSE, # Do not add ellipses
             title = "PCA Biplot with Convex Hulls (PC1 and PC2)") +
  stat_chull(data = data.frame(pca_result$ind$coord, Group = data$Group),
             aes(x = Dim.1, y = Dim.2, fill = Group, color = Group), 
             alpha = 0.2, geom = "polygon", show.legend = FALSE) +
  theme_minimal()

# Convex hulls are added using ggplot's `stat_chull` function.

# Perform PCA
pca_result <- PCA(numeric_data, graph = FALSE)

# Create the biplot with ellipses
fviz_pca_biplot(pca_result,
                geom.ind = "point", # Show individuals as points
                col.ind = data$Group, # Color individuals by groups
                palette = c("red", "blue"), # Colors for groups
                addEllipses = TRUE, # Add confidence ellipses
                ellipse.level = 0.95, # Confidence level for ellipses
                label = "var", # Show variable labels
                col.var = "black", # Color for variable arrows
                repel = TRUE, # Avoid overlapping text
                title = "PCA Biplot with Ellipses (PC1 and PC2)")
