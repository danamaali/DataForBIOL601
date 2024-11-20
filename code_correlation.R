# Load necessary libraries
library(ggplot2)
library(reshape2)

# Load the dataset
data <- read.csv("BreastCancerData.csv")

# Select only numeric columns (exclude any non-numeric columns like IDs or labels)
numeric_data <- data[sapply(data, is.numeric)]


numeric_data$ID=NULL
# Compute the correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Melt the correlation matrix for ggplot
melted_cor_matrix <- melt(cor_matrix)

# Create the heatmap using ggplot2
heatmap <- ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), 
                       name = "Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Correlation Matrix Heatmap", x = "Variables", y = "Variables")

# Display the heatmap
print(heatmap)

install.packages("corrplot")

# Load the library
library(corrplot)


# Plot the correlation matrix with hierarchical clustering
corrplot(cor_matrix, method = "color", order = "hclust",
         addrect = 3, # Draw rectangles around 3 clusters
         tl.col = "black", tl.srt = 45, # Adjust text color and rotation
         col = colorRampPalette(c("blue", "white", "red"))(200),
         title = "Correlation Matrix with Clustering")

corrplot(cor_matrix, method = "color", order = "hclust",
         addrect = 3, # Draw rectangles around clusters
         tl.col = "black", # Text color
         tl.srt = 45, # Rotate text for better readability
         tl.cex = 0.5, # Adjust text size (smaller value for smaller text)
         col = colorRampPalette(c("blue", "white", "red"))(200),
         title = "Correlation Matrix with Clustering")

corrplot(correlation_matrix, 
         method = "color",          # Use a color gradient
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         tl.col = "black",          # Text label color
         tl.srt = 45,               # Text label rotation
         order = "hclust",          # Apply hierarchical clustering
         hclust.method = "complete", # Clustering method (can be "ward.D", "average", etc.)
         addrect = 3,               # Draw rectangles around clusters (optional)
         title = "Correlation Matrix with Clustering",
         mar = c(0, 0, 1, 0))       # Adjust margins for the plot
