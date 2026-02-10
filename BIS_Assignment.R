# Load required libraries
library(readxl)      # For reading Excel files
library(cluster)     # For clustering algorithms
library(factoextra)  # For cluster validation and visualization
library(dendextend)  # For dendrogram comparison
library(corrplot)    # For correlation matrix visualization
library(fpc)         # For cluster statistics
library(NbClust)     # For determining optimal number of clusters
library(dplyr)       # For data manipulation
library(ggplot2)     # For advanced plotting
library(gridExtra)   # For arranging multiple plots
library(clusterSim)
library(MASS)
library(reshape2)    # For melting matrix
library(ggdendro)

# 1. LOAD AND EXPLORE DATA
wine_data <- read_excel("C:/Users/user/Downloads/4th year 1st sem/data analytics/Assignment - 02-20250813/Whitewine.xlsx")
summary(wine_data)
glimpse(wine_data)
head(wine_data)
dim(wine_data)
sapply(wine_data, class)

# 2. INITIAL VISUALIZATIONS
# Boxplots
oldpar <- par(mfrow = c(2, 6), mar = c(3, 3, 2, 1))
for (i in 1:11) {
  boxplot(wine_data[[i]])
  mtext(names(wine_data)[i], cex = 0.8, side = 1, line = 2)
}
par(oldpar)

# Pairwise plots (subset)
par(mfrow = c(1,1))
pairs(wine_data[, 1:5])

# Histograms
oldpar <- par(mfrow = c(3, 4))
for (i in 1:12) {
  truehist(wine_data[[i]], xlab = names(wine_data)[i], col = 'lightgreen',
           main = paste("Average =", signif(mean(wine_data[[i]]),3)))
}
par(oldpar)

# preprocessing

any(duplicated(wine_data))
wine_data[duplicated(wine_data) | duplicated(wine_data, fromLast = TRUE), ]
no_dup_wine_data <- wine_data %>% distinct()
dim(no_dup_wine_data)

# 4. PREPARE FEATURES AND TARGET
wine_features <- no_dup_wine_data[,-12]  # Remove quality column
wine_quality <- no_dup_wine_data[, 12]   # Quality column
dim(wine_features)



# Feature ranges
feature_ranges <- sapply(wine_features, function(x) max(x) - min(x))
print(feature_ranges)

# 5. CORRELATION ANALYSIS
# Correlations with quality
correlations_with_quality <- cor(wine_features, wine_quality)
print(correlations_with_quality)

# Select numeric variables from cleaned data
numeric_vars <- sapply(wine_features, is.numeric)
wine_features_num <- wine_features[, numeric_vars] 

# Correlation with quality (bar chart)
cor_with_quality <- cor(wine_features_num, wine_quality, use = "complete.obs")
cor_with_quality <- cor_with_quality[,1]
cor_with_quality <- cor_with_quality[names(cor_with_quality) != "quality"]

cor_df <- data.frame(
  variable = names(cor_with_quality),
  correlation = cor_with_quality
)

ggplot(cor_df, aes(x = reorder(variable, correlation), y = correlation, fill = correlation)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  labs(title = "White Wine Feature Correlation with Quality",
       x = "Features",
       y = "Correlation with Quality") +
  theme_minimal()



# Correlation matrix heatmap
cor_matrix <- cor(wine_data, use = "complete.obs")
cor_melt <- melt(cor_matrix)

ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(abs(value) > 0.3, round(value, 2), "")), 
            color = "black", size = 3) +  # Only show if |correlation| > 0.3
  scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  coord_fixed()


correlations <- cor(wine_features_num, wine_quality)[,1]
sorted_correlations <- sort(abs(correlations), decreasing = TRUE)

print("Variables ranked by absolute correlation with quality:")
print(sorted_correlations)


# 6. FEATURE SCALING (ONLY FEATURES, NOT QUALITY)
wine_features_scaled <- scale(wine_features)  # Scale only features
wine_features_scaled_df <- as.data.frame(wine_features_scaled)

numeric_vars_scaled <- sapply(wine_features_scaled, is.numeric)
wine_features_num_scaled <- wine_features_scaled[, numeric_vars_scaled]

print(colMeans(wine_features_num_scaled))
print(apply(wine_features_num_scaled, 2, sd))  # Should all be 1

scaled_wine_data = scale(wine_data) #scale all
scaled_df <- as.data.frame(scaled_wine_data)
print(scaled_wine_data)

# Histograms after scaling
oldpar <- par(mfrow = c(3, 4), mar = c(4, 4, 2, 1))
for (i in 1:11) {
  truehist(wine_features_scaled[,i], 
           xlab = colnames(wine_features_scaled)[i], 
           col = 'lightgreen',
           main = paste("Average =", signif(mean(wine_features_scaled[,i]),3)))
}
par(oldpar)

# Pairwise plots (subset)
par(mfrow = c(1,1))
pairs(wine_features_scaled[, 1:5])


# 7. DETERMINE OPTIMAL CLUSTERS - IMPROVED VERSION
set.seed(123)
wss <- numeric(8)
for (k in 1:8) {
  kmeans_result <- kmeans(wine_features_scaled, centers = k, nstart = 25, iter.max = 200)
  wss[k] <- kmeans_result$tot.withinss
}

# Calculate percentage decrease in WSS for better elbow identification
elbow_plot <- data.frame(k = 1:8, wss = wss)
elbow_plot$wss_decrease <- c(NA, diff(elbow_plot$wss))
elbow_plot$wss_decrease_pct <- c(NA, abs(diff(elbow_plot$wss)) / elbow_plot$wss[-nrow(elbow_plot)] * 100)

# Create improved elbow plot
library(ggplot2)
p1_improved <- ggplot(elbow_plot, aes(x = k, y = wss)) +
  geom_line(color = "steelblue", size = 1.2) + 
  geom_point(color = "red", size = 3) +
  geom_text(aes(label = paste0("k=", k, "\n", round(wss, 0))), 
            vjust = -1.5, hjust = 0.5, size = 3) +
  scale_x_continuous(breaks = 1:8) +
  labs(title = "Elbow Method for Optimal Number of Clusters",
       x = "Number of Clusters (k)", 
       y = "Within-cluster Sum of Squares (WSS)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", color = "grey70")
  )

print(p1_improved)

# Additional analysis: Calculate WSS reduction rates
cat("\nWSS Reduction Analysis:\n")
cat("k\tWSS\t\tDecrease\t% Decrease\n")
for(i in 1:8) {
  if(i == 1) {
    cat(sprintf("%d\t%.1f\t\t-\t\t-\n", i, elbow_plot$wss[i]))
  } else {
    cat(sprintf("%d\t%.1f\t\t%.1f\t\t%.1f%%\n", 
                i, elbow_plot$wss[i], 
                abs(elbow_plot$wss_decrease[i]), 
                elbow_plot$wss_decrease_pct[i]))
  }
}




# 7. DETERMINE OPTIMAL CLUSTERS
set.seed(123)
wss <- numeric(8)
for (k in 1:8) {
  kmeans_result <- kmeans(wine_features_scaled, centers = k, nstart = 25, iter.max = 200)
  wss[k] <- kmeans_result$tot.withinss
}

# K-means with k=1
kmeans_1 <- kmeans(wine_features_scaled, centers = 1, nstart = 25, iter.max = 100)

# K-means with k=2
kmeans_2 <- kmeans(wine_features_scaled, centers = 2, nstart = 25, iter.max = 100)

# K-means with k=3
kmeans_3 <- kmeans(wine_features_scaled, centers = 3, nstart = 25, iter.max = 100)

# K-means with k=4  
kmeans_4 <- kmeans(wine_features_scaled, centers = 4, nstart = 25, iter.max = 100)

# K-means with k=5  
kmeans_5 <- kmeans(wine_features_scaled, centers = 5, nstart = 25, iter.max = 100)

# Compare results
cat("K=1: Total WSS =", kmeans_1$tot.withinss, "\n")
cat("K=2: Total WSS =", kmeans_2$tot.withinss, "\n")
cat("K=3: Total WSS =", kmeans_3$tot.withinss, "\n")
cat("K=4: Total WSS =", kmeans_4$tot.withinss, "\n")
cat("K=5: Total WSS =", kmeans_5$tot.withinss, "\n")



# Silhouette analysis for better comparison
library(cluster)

sil_2 <- silhouette(kmeans_2$cluster, dist(wine_features_scaled))
sil_3 <- silhouette(kmeans_3$cluster, dist(wine_features_scaled))
sil_4 <- silhouette(kmeans_4$cluster, dist(wine_features_scaled))
sil_5 <- silhouette(kmeans_5$cluster, dist(wine_features_scaled))

cat("K=2 Average Silhouette:", round(mean(sil_2[,3]), 3), "\n")
cat("K=3 Average Silhouette:", round(mean(sil_3[,3]), 3), "\n")
cat("K=4 Average Silhouette:", round(mean(sil_4[,3]), 3), "\n")
cat("K=5 Average Silhouette:", round(mean(sil_5[,3]), 3), "\n")

# Plot one at a time instead of side by side
library(factoextra)
p2 <- fviz_silhouette(sil_2)
p3 <- fviz_silhouette(sil_3)
p4 <- fviz_silhouette(sil_4)
p5 <- fviz_silhouette(sil_5)
print(p2)
print(p3)
print(p4)
print(p5)


print(wine_data[,-12])
print(winner_clusters$cluster)
winner_clusters <- kmeans_3
cluster_means_original <- aggregate(no_dup_wine_data[,-12], 
                                    by = list(Cluster = winner_clusters$cluster), 
                                    FUN = mean)
print(cluster_means_original)


# Compare clusters with wine quality
cluster_quality <- data.frame(
  Cluster = winner_clusters$cluster,
  Quality = no_dup_wine_data$quality
)

# Summary by cluster
quality_by_cluster <- aggregate(Quality ~ Cluster, data = cluster_quality, 
                                FUN = function(x) c(mean = mean(x), 
                                                    median = median(x),
                                                    count = length(x)))
print(quality_by_cluster)


# Visualization
library(ggplot2)
ggplot(cluster_quality, aes(x = factor(Cluster), y = Quality)) +
  geom_boxplot() +
  labs(title = "Wine Quality by Cluster", x = "Cluster", y = "Quality")



# Check the actual chemical differences between clusters 1 and 2
cluster_means <- aggregate(no_dup_wine_data[,-12], 
                           by = list(Cluster = winner_clusters$cluster), 
                           FUN = mean)

print("Cluster characteristics:")
print(cluster_means)

# Look at key differentiating features
library(dplyr)
cluster_comparison <- no_dup_wine_data %>%
  mutate(Cluster = winner_clusters$cluster) %>%
  group_by(Cluster) %>%
  summarise(
    avg_alcohol = round(mean(alcohol), 2),
    avg_residual_sugar = round(mean(`residual sugar`), 2),
    avg_acidity = round(mean(`fixed acidity`), 2),
    avg_quality = round(mean(quality), 2),
    count = n()
  )

print(cluster_comparison)

#perform pca and plot elbow




library(ggplot2)

# Perform PCA
pca_result <- prcomp(wine_features_scaled, scale. = FALSE)  # Already scaled

# Get PCA coordinates
pca_data <- data.frame(
  Dim1 = pca_result$x[, 1],
  Dim2 = pca_result$x[, 2], 
  cluster = as.factor(winner_clusters$cluster)
)
print(winner_clusters$cluster)

# Calculate variance explained
var_explained <- summary(pca_result)$importance[2, 1:2] * 100

# Add row numbers as text labels
pca_data$row_num <- 1:nrow(pca_data)

ggplot(pca_data, aes(x = Dim1, y = Dim2, color = cluster)) +
  geom_text(aes(label = row_num), size = 2, alpha = 0.7) +  # Numbers instead of points
  stat_ellipse(type = "convex", alpha = 0.3) +
  scale_color_manual(values = c("1" = "red", "2" = "cyan", "3" = "blue")) +
  labs(
    x = paste0("Dim1 (", round(var_explained[1], 1), "%)"),
    y = paste0("Dim2 (", round(var_explained[2], 1), "%)"),
    title = "Cluster plot",
    color = "cluster"
  ) +
  theme_minimal()



corrplot(cor(wine_features_scaled))

current_config <- kmeans(wine_features_scaled, centers = 3, nstart = 25)
wine_no_duplicates_removed <- wine_data[, !names(wine_data) %in% c("quality")]
wine_not_scaled_no_duplicates_removed <- scale(wine_no_duplicates_removed)
config_2 <- kmeans(wine_not_scaled_no_duplicates_removed, centers = 3, nstart = 25)
wine_not_scaled <- kmeans(no_dup_wine_data, centers = 3, nstart = 25)


library(cluster)

# Compare silhouette scores
sil_current <- silhouette(current_config$cluster, dist(wine_features_scaled))
sil_config_2 <- silhouette(config_2$cluster, dist(wine_data[-12]))
sil_config_3 <- silhouette(wine_not_scaled$cluster, dist(no_dup_wine_data))

cat("Current config silhouette:", round(mean(sil_current[,3]), 3), "\n")
cat("Duplicates not removed:", round(mean(sil_config_2[,3]), 3), "\n")
cat("Not scaled:", round(mean(sil_config_3[,3]), 3), "\n")

#hierachical clustering
# Calculate distance matrix
dist_matrix <- dist(wine_features_scaled, method = "euclidean")

# Perform hierarchical clustering with different linkage methods
hc_single <- hclust(dist_matrix, method = "single")
hc_complete <- hclust(dist_matrix, method = "complete")
hc_average <- hclust(dist_matrix, method = "average")
hc_ward <- hclust(dist_matrix, method = "ward")

cat("\nHierarchical clustering completed with 3 linkage methods:\n")
cat("- Single linkage\n")
cat("- Complete linkage\n")
cat("- Average linkage\n")

# ========================================
# 3. CREATE DENDROGRAMS
# ========================================

# Set up plotting area for dendrograms
par(mfrow = c(1,1))

plot(hc_single)
plot(hc_complete)
plot(hc_average)
plot(hc_ward)



# ========================================
# 3. CREATE DENDROGRAMS
# ========================================

# Set up plotting area for dendrograms
par(mfrow = c(1,1))

# Plot dendrograms for each method
dev.new()
plot(hc_single, main = "Single Linkage", xlab = "", sub = "", cex = 0.7)
rect.hclust(hc_single, k = 3, border = "red")
dev.new()
plot(hc_complete, main = "Complete Linkage", xlab = "", sub = "", cex = 0.7)
rect.hclust(hc_complete, k = 3, border = "red")

plot(hc_average, main = "Average Linkage", xlab = "", sub = "", cex = 0.7)
rect.hclust(hc_average, k = 3, border = "red")

plot(hc_ward, main = "Ward", xlab = "", sub = "", cex = 0.7)
rect.hclust(hc_ward, k = 3, border = "red")

par(mfrow = c(1, 1))

# Create enhanced dendrograms using dendextend
library(dendextend)
library(colorspace)

# Convert to dendrogram objects
dend_single <- as.dendrogram(hc_single)
dend_complete <- as.dendrogram(hc_complete)
dend_average <- as.dendrogram(hc_average)
dend_ward <- as.dendrogram(hc_ward)

# Color dendrograms by clusters (k=3)
dend_single_colored <- color_branches(dend_single, k = 3)
dend_complete_colored <- color_branches(dend_complete, k = 3)
dend_average_colored <- color_branches(dend_average, k = 3)
dend_ward_colored <- color_branches(dend_ward, k = 3)

plot(dend_single_colored, main = "Single Linkage (k=3)", ylab = "Height")
plot(dend_complete_colored, main = "Complete Linkage (k=3)", ylab = "Height")
plot(dend_average_colored, main = "Average Linkage (k=3)", ylab = "Height")
plot(dend_ward_colored, main = "ward (k=3)", ylab = "Height")

par(mfrow = c(1, 1))

cut_height <- max(hc_ward$height) * 0.35
groups_single<-cutree(hc_single,k=3) 
table(groups_single)
groups_complete<-cutree(hc_complete,k=3) 
table(groups_complete)
groups_average<-cutree(hc_average,k=3) 
table(groups_average,no_dup_wine_data$quality)
length(groups_average)
length(wine_quality)
groups_ward<-cutree(hc_ward,k=3) 
table(groups_ward, no_dup_wine_data$quality)


no_dup_wine_d <- no_dup_wine_data
no_dup_wine_d$cluster <- as.factor(groups_ward)


ggplot(no_dup_wine_d, aes(x = `total sulfur dioxide`, y = `free sulfur dioxide`, color = cluster)) +  
  geom_point(alpha = 0.7, size = 2) + 
  scale_color_manual(values = c('black', 'red', 'green')) +
  labs(title = "Wine Clusters: Residual Sugar vs Alcohol")

plot(hc_ward, main = "Ward Dendrogram")
abline(h = cut_height, col = "red", lty = 2)

# ========================================
# 4. COPHENETIC CORRELATION ANALYSIS
# ========================================

# Calculate cophenetic distances
coph_single <- cophenetic(hc_single)
coph_complete <- cophenetic(hc_complete)
coph_average <- cophenetic(hc_average)
coph_ward <- cophenetic(hc_ward)

# Calculate cophenetic correlation coefficients
cor_single <- cor(dist_matrix, coph_single)
cor_complete <- cor(dist_matrix, coph_complete)
cor_average <- cor(dist_matrix, coph_average)
cor_ward <- cor(dist_matrix, coph_ward)

# Display results
cat("\n========================================\n")
cat("COPHENETIC CORRELATION ANALYSIS\n")
cat("========================================\n")
cat("Single Linkage:", round(cor_single, 4), "\n")
cat("Complete Linkage:", round(cor_complete, 4), "\n")
cat("Average Linkage:", round(cor_average, 4), "\n")
cat("Ward:", round(cor_ward, 4), "\n")

# Create a comparison data frame
coph_results <- data.frame(
  Method = c("Single", "Complete", "Average","ward"),
  Cophenetic_Correlation = c(cor_single, cor_complete, cor_average, cor_ward)
)

# Plot cophenetic correlations
ggplot(coph_results, aes(x = reorder(Method, Cophenetic_Correlation), 
                         y = Cophenetic_Correlation, fill = Method)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_text(aes(label = round(Cophenetic_Correlation, 3)), 
            vjust = -0.3, size = 4) +
  labs(title = "Cophenetic Correlation by Linkage Method",
       x = "Linkage Method",
       y = "Cophenetic Correlation",
       subtitle = "Higher values indicate better preservation of original distances") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, max(coph_results$Cophenetic_Correlation) * 1.1)

library(caret)             
#the confusionMatrix function is included in caret package 
predicted <- factor(groups_ward)
actual <- factor(no_dup_wine_data$quality)

confusionMatrix(predicted, actual)

# Quality distribution by cluster
ggplot(cluster_data, aes(x = factor(cluster), y = quality, fill = factor(cluster))) +
  geom_boxplot() +
  labs(title = paste("Wine Quality Distribution by Cluster -", best_method, "Linkage"),
       x = "Cluster",
       y = "Wine Quality",
       fill = "Cluster") +
  theme_minimal()




# Create scatter plots for hierarchical clustering results

# Method 1: PCA-based scatter plot (best for high-dimensional data)
# Perform PCA on scaled features
pca_result <- prcomp(wine_features_scaled, scale. = FALSE)  # Already scaled
pca_data <- data.frame(
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2],
  PC3 = pca_result$x[, 3],
  single = as.factor(clusters_single),
  complete = as.factor(clusters_complete),
  average = as.factor(clusters_average),
  ward = as.factor(clusters_ward),
  quality = no_dup_wine_data$quality
)

# Calculate variance explained
var_explained <- summary(pca_result)$importance[2, 1:3] * 100

# Create PCA scatter plots for each linkage method
p_single_pca <- ggplot(pca_data, aes(x = PC1, y = PC2, color = single)) +
  geom_point(alpha = 0.6, size = 1.5) +
  stat_ellipse(type = "norm", alpha = 0.3) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(title = "Single Linkage Clusters (PCA)",
       x = paste0("PC1 (", round(var_explained[1], 1), "%)"),
       y = paste0("PC2 (", round(var_explained[2], 1), "%)"),
       color = "Cluster") +
  theme_minimal()

p_complete_pca <- ggplot(pca_data, aes(x = PC1, y = PC2, color = complete)) +
  geom_point(alpha = 0.6, size = 1.5) +
  stat_ellipse(type = "norm", alpha = 0.3) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(title = "Complete Linkage Clusters (PCA)",
       x = paste0("PC1 (", round(var_explained[1], 1), "%)"),
       y = paste0("PC2 (", round(var_explained[2], 1), "%)"),
       color = "Cluster") +
  theme_minimal()

p_average_pca <- ggplot(pca_data, aes(x = PC1, y = PC2, color = average)) +
  geom_point(alpha = 0.6, size = 1.5) +
  stat_ellipse(type = "norm", alpha = 0.3) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(title = "Average Linkage Clusters (PCA)",
       x = paste0("PC1 (", round(var_explained[1], 1), "%)"),
       y = paste0("PC2 (", round(var_explained[2], 1), "%)"),
       color = "Cluster") +
  theme_minimal()

p_ward_pca <- ggplot(pca_data, aes(x = PC1, y = PC2, color = ward)) +
  geom_point(alpha = 0.6, size = 1.5) +
  stat_ellipse(type = "norm", alpha = 0.3) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(title = "Average Linkage Clusters (PCA)",
       x = paste0("PC1 (", round(var_explained[1], 1), "%)"),
       y = paste0("PC2 (", round(var_explained[2], 1), "%)"),
       color = "Cluster") +
  theme_minimal()
plot(p_ward_pca)
# Arrange PCA plots
grid.arrange(p_single_pca, p_complete_pca, p_average_pca,p_ward_pca, ncol = 2)
