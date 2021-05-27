library(RcppRidge)
library(tidyverse)

load("./data/Irish_Processed.RData")

set.seed(22)

# This takes a minute to run
# The average demand at each time of day for each customer
df_avg_tod <- cbind(cust, tod = extra$tod) %>%
              pivot_longer(cols = starts_with("I"),
                           names_to = "ID",
                           values_to = "demand") %>%
              group_by(tod, ID) %>%
              summarise(avg_demand = mean(demand)) %>%
              spread(tod, avg_demand) %>%
              select(-1) # Drop ID column

# Function that plots an elbow plot using kmeans from the package
elbow_plot_kmeans <- function(x, kmax = 15) {
  wss_values <- map_dbl(1:kmax, function(k) {
    sum(k_means(x, centers = k)$wcss)
  })

  ggplot() + geom_point(aes(x = 1:kmax, y = wss_values)) + geom_line(aes(x = 1:kmax, y = wss_values)) +
       theme_light() +
       labs(x="Number of clusters K", y="Total within-clusters sum of squares")
}

# Creates an elbow plot from the data
elbow_plot_kmeans(scale(data.matrix(df_avg_tod)), kmax = 15)

plot_clusters <- function(data, clusters) {
  x <- as.matrix(data)
  p <- pca(scale(x))
  x.pca <- p$PC
  x.pca$clusters <- factor(clusters)
  ggplot() + geom_point(data = x.pca, aes(x =- X1, y = X2, color = clusters), size = 1) + theme_light() + labs(x = sprintf("PC1 (%.2f %%)", p$energy[1] * 100), y = sprintf("PC2 (%.2f) %%", p$energy[2] * 100))
}

clusters <- k_means(scale(data.matrix(df_avg_tod)), centers = 5)$clusters

#clusters <- spectralClustering(scale(data.matrix(df_avg_tod)))
plot_clusters(df_avg_tod, clusters)

# Save clusters for later use
names(clusters) <- as.vector(surv$ID)
save(clusters, file = "./data/clusters.Rdata")