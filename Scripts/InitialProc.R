# Importing and Cleaning Count Data ---------------------------------------

#Import data
ordered_raw_count <- read.csv("~/Desktop/eDNA_Analysis/Data/raw_count_18S_ordered.csv", header = TRUE, sep = ",")

#Preserve sample names
sample.Names <- ordered_raw_count[,1]

#Strip sample names from data matrix and make a log10 matrix and a proportional matrix
ordered_count <- ordered_raw_count[,-1]
ordered_count_prop <- ordered_count/rowSums(ordered_count)
ordered_count_log <- log10(ordered_count)
ordered_count_log_fix=do.call(data.frame, lapply
              (ordered_count_log, function(value) replace
                (value, is.infinite(value), 0)))
ordered_count_log_fix <- data.matrix(ordered_count_log_fix)

# Analyzing Sample Data for Irregularities --------------------------------

#Generate two heatmaps of count data, one for proportional and one for log10 normalization
#Use the heatmaps to check if there are any irregularities in data, such as specific samples being widely different etc
col.scale <- colorRampPalette(c("blue", "red"), space = "rgb")(100)
heatmap(as.matrix(ordered_count_prop), Rowv = NA, Colv = NA, col = col.scale, margins = c(10, 2), na.rm = TRUE, main = "Proportional", xlab = "OTUs", ylab = "Samples")
heatmap(as.matrix(ordered_count_log_fix), Rowv = NA, Colv = NA, col = col.scale, margins = c(10, 2), na.rm = TRUE, main = "Log10", xlab = "OTUs", ylab = "Samples")

#Now analyzing the samples in another way, summing to see if there are any with a noticeably different number of OTUs
sample_Sums <- colSums(ordered_count)
sorted_sample_Sums <- sort(sample_Sums)
samples_Number <- c(1:62)
sample_Sums_DF <- data.frame(sorted_sample_Sums, samples_Number)
plot(sort(sorted_sample_Sums), col = "black", pch = 16, xlab = "Samples", ylab="Total Coverage", main="Number of OTUs per Sample")
linreg_Sums <- lm(sample_Sums_DF$sorted_sample_Sums~sample_Sums_DF$samples_Number, sample_Sums_DF)
R_Squared_Value <- summary(linreg_Sums)$r.squared

#Now, analyzing the difference between samples using a Jensen-Shannon Divergence model, commented out lines are difference between OTUs
ordered_count_matrix <- data.matrix(ordered_count_log_fix)
#JSDMatrixOTUs <- JSD(ordered_count_matrix, unit = "log10", est.prob = "empirical")
#image(JSDMatrixOTUs, col = col.scale)

#Plotting the JSD model heatmap
JSDMatrixSamples <- JSD(t(ordered_count_matrix), unit = "log2", est.prob = "empirical")
image(JSDMatrixSamples, col = col.scale, main = "Heatmap of Jensen-Shannon Divergence of Samples")


# Analyzing OTUs for Irregularities ---------------------------------------

OTU_counts_total <- rowSums(ordered_count)
OTU_presence_samples <- rowSums(ordered_count >0)
OTU_counts_total_sorted <- sort(OTU_counts_total,decreasing=TRUE)
OTU_presence_samples_sorted <-sort(OTU_presence_samples, decreasing=TRUE)

plot(log10(OTU_counts_total_sorted),xlab="OTUs",ylab="Total counts",main="Total Occurences for OTUs",axes=FALSE)
axis(1)
axis(2,at=c(0,1,2,3,4),label=c('1','10','100','1000','10000'))
box(bty="l")

plot(OTU_presence_samples_sorted,xlab="OTUs",ylab="Total samples present",main="Number of samples present for OTUs")

#Removing the many OTUs that only appear in one sample, "removing the tail"
single_OTUs_index <- which(OTU_presence_samples == 1)
abridged_count_data <- ordered_count[-c(single_OTUs_index),]
abr_matrix <- data.matrix(abridged_count_data)
abr_count_data_prop <- abridged_count_data/rowSums(abridged_count_data)
abr_count_data_log10 <- log10(abridged_count_data)
abr_count_data_log10_fix=do.call(data.frame, lapply
                                 (abr_count_data_log10, function(value) replace
                                   (value, is.infinite(value), 0)))

heatmap(as.matrix(abr_count_data_prop), Rowv = NA, Colv = NA, col = col.scale, margins = c(10, 2), na.rm = TRUE, main = "Proportional", xlab = "OTUs", ylab = "Samples")
heatmap(as.matrix(abr_count_data_log10_fix), Rowv = NA, Colv = NA, col = col.scale, margins = c(10, 2), na.rm = TRUE, main = "Log10", xlab = "OTUs", ylab = "Samples")

image(as.matrix(abr_count_data_prop))
image(as.matrix(abr_count_data_log10_fix))
abr_count_matrix_norm <- as.matrix(abr_count_data_log10_fix)

JSDMatrixSamplesAbridged <- JSD(t(abr_count_matrix_norm), unit = "log2", est.prob = "empirical")
image(JSDMatrixSamplesAbridged, col = col.scale, main = "Heatmap of Jensen-Shannon Divergence of Abridged Samples")

avg.sil <- c()
avg.silAb <- c()

# Calculating k-medoid clustering -----------------------------------------
 for (k in 2:20) {
   pam.full <- pam(JSDMatrixSamples, k, diss = TRUE)
   clust_ordered <- order(pam.full$clustering)
   image((JSDMatrixSamples[clust_ordered,clust_ordered]),col=col.scale,axes=FALSE,xlab="",ylab="", main = c("Number of Clusters ", k))
   for(r in 1:(k-1))
   {
     f <- sum((pam.full$clustering)<=r)/length((pam.full$clustering))
     lines(x=c(f,f),y=c(0,1),col=rgb(0.1,0.1,0.9))
     lines(y=c(f,f),x=c(0,1),col=rgb(0.1,0.1,0.9))
   }
   text(x=0.2,y=0.7,label=k,cex=3,col=grey(0.9))
   box()
   sil <- silhouette((pam.full$clustering), dmatrix = JSDMatrixSamples)
   avg.sil.width <- mean(sil[,"sil_width"])
   avg.sil <- append(avg.sil, avg.sil.width)
   plot(sil, main = "Silhouette plot of K-Medoids Clustering based on Jansen-Shannon Divergence Values")
}
#6 clusters works best!!!!

for (k in 2:20) {
  pam.fullAb <- pam(JSDMatrixSamplesAbridged, k, diss = TRUE)
  clust_orderedAb <- order(pam.fullAb$clustering)
  image((JSDMatrixSamplesAbridged[clust_orderedAb,clust_orderedAb]),col=col.scale,axes=FALSE,xlab="",ylab="", main = c("Number of Clusters ", k))
  for(r in 1:(k-1))
  {
    f <- sum((pam.fullAb$clustering)<=r)/length((pam.fullAb$clustering))
    lines(x=c(f,f),y=c(0,1),col=rgb(0.1,0.1,0.9))
    lines(y=c(f,f),x=c(0,1),col=rgb(0.1,0.1,0.9))
  }
  text(x=0.2,y=0.7,label=k,cex=3,col=grey(0.9))
  box()
  silAb <- silhouette((pam.fullAb$clustering), dmatrix = JSDMatrixSamplesAbridged)
  avg.sil.widthAb <- mean(silAb[,"sil_width"])
  avg.silAb <- append(avg.silAb, avg.sil.widthAb)
  plot(silAb, main = "Silhouette plot of K-Medoids Clustering based on Jansen-Shannon Divergence Values of Abridged Data")
}
# 6 also works for the abridged data set


# Dirichlet Multinomial Mixture for Full Dataset -------------------------------------------
ordered_count_log_fix_matrix <- data.matrix(ordered_count_log_fix)
rev_matrix <- t(ordered_count_log_fix_matrix)
rev_matrix_abr <- t(abr_matrix)
abr_log_matrix <- data.matrix(abr_count_data_log10_fix)
rev_abr_log <- t(abr_log_matrix)

fit <- mclapply(1:9, dmn, count=rev_matrix, verbose = TRUE, mc.cores = 8)
lplc <- base::sapply(fit, DirichletMultinomial::laplace)
aic  <- base::sapply(fit, DirichletMultinomial::AIC)
bic  <- base::sapply(fit, DirichletMultinomial::BIC)

plot(lplc, type="b", xlab="Number of Dirichlet Components", ylab="Model Fit")
lines(aic, type="b", lty = 2)
lines(bic, type="b", lty = 3)

best <- fit[[which.min(unlist(lplc))]]

mixturewt(best)

ass <- apply(mixture(best), 1, which.max)

for (k in seq(ncol(fitted(best)))) {
  d <- melt(fitted(best))
  colnames(d) <- c("OTU", "cluster", "value")
  d <- subset(d, cluster == k) %>%
    # Arrange OTUs by assignment strength
    arrange(value) %>%
    mutate(OTU = factor(OTU, levels = unique(OTU))) %>%
    # Only show the most important drivers
    filter(abs(value) > quantile(abs(value), 0.8))

  p <- ggplot(d, aes(x = OTU, y = value)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = paste("Top drivers: community type", k))
  print(p)
}



# Dirichlet Multinomial Mixture for Abridged Dataset -------------------------------------------

fitAb <- mclapply(1:20, dmn, count=t(abr_matrix), verbose = TRUE, mc.cores = 8)
lplcAb <- base::sapply(fitAb, DirichletMultinomial::laplace)
aicAb  <- base::sapply(fitAb, DirichletMultinomial::AIC)
bicAb  <- base::sapply(fitAb, DirichletMultinomial::BIC)

plot(lplcAb, type="b", xlab="Number of Dirichlet Components", ylab="Model Fit")
lines(aicAb, type="b", lty = 2)
lines(bicAb, type="b", lty = 3)

bestAb <- fit[Ab[which.min(unlist(lplcAb))]]

mixturewt(bestAb)

assAb <- apply(mixture(bestAb), 1, which.max)

for (k in seq(ncol(fitted(bestAb)))) {
  dAb <- melt(fitted(bestAb))
  colnames(dAb) <- c("OTU", "cluster", "value")
  dAb <- subset(dAb, cluster == k) %>%
    # Arrange OTUs by assignment strength
    arrange(value) %>%
    mutate(OTU = factor(OTU, levels = unique(OTU))) %>%
    # Only show the most important drivers
    filter(abs(value) > quantile(abs(value), 0.8))

  pAb <- ggplot(dAb, aes(x = OTU, y = value)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = paste("Top drivers: community type", k))
  print(pAb)
}









