#Data ingestion
csv_file <-
  "~/Documents/GithHub/eDNA_Analysis/Data/raw_count_18S_ordered.csv"



#Import data
raw_count_df <- read.csv(csv_file, header = TRUE, sep = ",")

#Preserve sample names
sample_Names <- raw_count_df[, 1]

#Strip sample names from data matrix and make a log10 matrix and a proportional matrix
ordered_count <- raw_count_df[, -1]
ordered_count_prop <- ordered_count / rowSums(ordered_count)
ordered_count_log <- log10(ordered_count)
ordered_count_log_fix <- do.call(data.frame, lapply
                                (ordered_count_log, function(value)
                                  replace(value, is.infinite(value), 0)))
ordered_matrix <- data.matrix(ordered_count)
samp_OTU_matrix <- t(ordered_matrix)
