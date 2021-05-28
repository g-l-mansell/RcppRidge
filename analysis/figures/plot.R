#for each cluster load the results

source("analysis/0_utilityfunctions.R")
load("data/clusters.Rdata")
MAE <- readRDS("analysis/cluster_data/MAE.RData")

# out <- readRDS(paste0("analysis/cluster_data/clust", clust, ".rds"))
# res <- readRDS(paste0("analysis/cluster_data/plot_cluster", clust, ".rds"))
# betas <- readRDS(paste0("analysis/cluster_data/beta_ints_cluster", clust, ".rds"))

for(clust in 1:5){
  res <- readRDS(paste0("analysis/cluster_data/plot_cluster", clust, ".rds"))
  res <- res[order(res$month),]
  
  png(paste0("analysis/figures/monthly_predictions_cluster", clust, ".png"), width=800, height=500)
  plot_monthly_predictions(res$y_test, cbind(res$y_lower, res$y_pred, res$y_upper)) 
  dev.off()
}

#comparing the profile for jan in each model
png(paste0("analysis/figures/compare_clusters_jan.png"), width=700, height=300)
par(mfrow=c(2, 3))
par(mar=c(4,4,2,1))
times <- seq(0, 23.5, 0.5)

#y_lim=2
for(clust in 1:5){
  n <- sum(clusters==clust)
  res <- readRDS(paste0("analysis/cluster_data/plot_cluster", clust, ".rds"))
  res <- res[order(res$month),]
  
  day_idx <- 1:48
  y_int <- cbind(res$y_lower, res$y_pred, res$y_upper) /n
  y_test <- res$y_test /n
  
  #y_lim = range(y_test, y_int, y_lim)
  
  plot(x=range(times), y=y_lim, type="n", 
         main=paste("Cluster", clust, "Jan"), xlab="Time of Day", ylab="Demand per household (kWh)")
    
    #plot predicted demand in red
    lines(times, y_int[day_idx,2], col=2)
    #plot interval with red dashed lines
    lines(times, y_int[day_idx,1], col=2, lty=2)
    lines(times, y_int[day_idx,3], col=2, lty=2)
    #plot true demand in black last
    lines(times, y_test[day_idx])
}
dev.off()



#looking at betas for a single model
cols <- readRDS("analysis/cluster_data/columnheadings.RData")
t <- 40
clust <- 5

betas <- readRDS(paste0("analysis/cluster_data/beta_ints_cluster", clust, ".rds"))
beta_int <- betas[,,t]
rownames(beta_int) <- cols

png("analysis/figures/example_cluster_betas.png", width=400, height=800)
forestplot(beta_int, col=fpColors(box = "darkblue"), lwd.ci=2,
           txt_gp = fpTxtGp(label = gpar(fontfamily = "Arial"), ticks = gpar(cex=1)))
dev.off()
