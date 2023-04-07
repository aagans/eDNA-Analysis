# Data Ingestion ----------------------------------------------------------

import_dataset <- function(data_file, sample_columns = TRUE) {
  raw_count_df <- read.csv(data_file, header = TRUE, sep = ",")
  ordered_count <- raw_count_df[, -1]
  if (sample_columns == TRUE) {
    return(ordered_count)
  }
  else if (sample_columns == FALSE) {
    ordered_count <- t(ordered_count)
    return(ordered_count)
  }
}

# Data Cleaning -----------------------------------------------------------

normalize_data <- function(dataframe, method) {
  if (method == 'proportional') {
    normed_count_df <- ordered_count / rowSums(ordered_count)
  }
  else if (method == 'log10') {
    ordered_count_log <- log10(ordered_count)
    ordered_count_log_fix <- do.call(data.frame, lapply
                                     (ordered_count_log, function(value)
                                       replace(value, is.infinite(value), 0)))
    normed_count_df <- ordered_count_log_fix
  }
  return(normed_count_df)
}

# Sample/OTU Analysis ---------------------------------------------------------

plot_heatmap <- function(dataframe, title_str, colorscale) {
  heatmap(
    as.matrix(dataframe),
    Rowv = NA,
    Colv = NA,
    col = colorscale,
    margins = c(10, 2),
    na.rm = TRUE,
    main = title_str,
    xlab = "OTUs",
    ylab = "Samples"
  )
}

total_coverage_fit <- function(dataframe, plot_graph = TRUE) {
  sample_sums <- colSums(dataframe)
  sorted_sample_sums <- sort(sample_sums)
  samples_number <- c(1:ncol(dataframe))
  sample_sums_df <- data.frame(sorted_sample_sums, samples_number)
  linreg_sums <-
    lm(
      sample_sums_df$sorted_sample_sums ~ sample_sums_df$samples_number,
      sample_sums_df
    )
  r_squared_value <- summary(linreg_sums)$r.squared
  if (plot_graph == TRUE) {
    plot(
      sort(sorted_sample_sums),
      col = "black",
      pch = 16,
      xlab = "Samples",
      ylab = "Total Coverage",
      main = "Number of OTUs per Sample"
    )
    
  }
  return(r_squared_value)
}

OTU_occurence <- function(dataframe, plot_graph = TRUE) {
  OTU_counts_total <- rowSums(dataframe)
  OTU_presence_samples <- rowSums(dataframe > 0)
  OTU_counts_total_sorted <-
    sort(OTU_counts_total, decreasing = TRUE)
  OTU_presence_samples_sorted <-
    sort(OTU_presence_samples, decreasing = TRUE)
  if (plot_graph == TRUE) {
    plot(
      log10(OTU_counts_total_sorted),
      xlab = "OTUs",
      ylab = "Total counts",
      main = "Total Occurences for OTUs",
      axes = FALSE
    )
    axis(1)
    axis(2,
         at = c(0, 1, 2, 3, 4),
         label = c('1', '10', '100', '1000', '10000'))
    box(bty = "l")
    plot(
      OTU_presence_samples_sorted,
      xlab = "OTUs",
      ylab = "Total samples present",
      main = "Number of samples present for OTUs"
    )
  }
  return(OTU_presence_samples)
}

# Trim Data ---------------------------------------------------------------

trim_data <- function(dataset, threshold = 1) {
  OTU_presence_sample <- OTU_occurence(dataset, FALSE)
  threshold_OTUs_index <- which(OTU_presence_samples <= threshold)
  abridged_count_data <- ordered_count[-c(threshold_OTUs_index), ]
  return(abridged_count_data)
}


# Sample Distance Calculations --------------------------------------------

jsd_matrix <- function(dataset, plot_graph = TRUE, colorscale) {
  difference_matrix <-
    JSD(t(dataset), unit = "log2", est.prob = "empirical")
  if (plot_graph == TRUE) {
    image(difference_matrix, col = colorscale,
          main = "Heatmap of Jensen-Shannon Distance Matrix")
  }
  return(difference_matrix)
}

# Clustering --------------------------------------------------------------

kmedoid_clustering <-
  function(difference_matrix,
           max_num_clusters,
           colorscale,
           silhouette_analysis = TRUE) {
    avg_sil <- c()
    pam_analyses <- c()
    
    for (k in 2:max_num_clusters) {
      pam_analysis <- pam(difference_matrix, k, diss = TRUE)
      pam_analyses <- append(pam_analysis)
      clust_ordered <- order(pam_analysis$clustering)
      image((difference_matrix[clust_ordered, clust_ordered]),
            col = colorscale,
            axes = FALSE,
            xlab = "",
            ylab = "",
            main = c("Number of Clusters ", k)
      )
      
      if (silhouette_analysis == TRUE) {
        for (r in 1:(k - 1))
        {
          f <-
            sum((pam_analysis$clustering) <= r) / length((pam_analysis$clustering))
          lines(
            x = c(f, f),
            y = c(0, 1),
            col = rgb(0.1, 0.1, 0.9)
          )
          lines(
            y = c(f, f),
            x = c(0, 1),
            col = rgb(0.1, 0.1, 0.9)
          )
        }
        text(
          x = 0.2,
          y = 0.7,
          label = k,
          cex = 3,
          col = grey(0.9)
        )
        box()
        
        sil <-
          silhouette((pam_analysis$clustering), dmatrix = difference_matrix)
        avg_sil_width <- mean(sil[, "sil_width"])
        avg_sil <- append(avg_sil, avg_sil_width)
        plot(sil, main = "Silhouette plot of K-Medoids Clustering 
             based on Jansen-Shannon Distance Values")
      }
    }
    return(pam_analyses)
  }
