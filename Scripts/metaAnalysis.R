meta_data <- read.csv("~/Desktop/eDNA_Analysis/Data/meta_data_trimmed.csv", header = FALSE, sep = ",")
clust_data <- t(as.data.frame(pam.full$clustering))
meta_data[nrow(meta_data)+1,] <- clust_data

meta_data <- meta_data[,order(meta_data[6,])]
write.csv(meta_data,"~/Desktop/eDNA_Analysis/Data/meta_data_grouped.csv", row.names = FALSE)

