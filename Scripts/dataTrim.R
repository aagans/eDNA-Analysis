


index_pres_vec <- c()
OTU_counts_total <- rowSums(ordered_count)
OTU_counts_prop <-
  OTU_counts_total / sum(OTU_counts_total)
len_OTU <- length(OTU_counts_prop)
for (i in 1:len_OTU) {
  index_vec <- OTU_counts_prop[1:i]
  index_vec <- unname(index_vec)
  pres <- (1 - sum(index_vec))
  if (pres <= 0.01) {
    index_pres_vec <- append(index_pres_vec, TRUE)
  } else{
    index_pres_vec <- append(index_pres_vec, FALSE)
  }
}






index_pres_vec_abr <- c()
OTU_counts_total_abr <- rowSums(abridged_count_data)
OTU_counts_abr_prop <-
  OTU_counts_total_abr / sum(OTU_counts_total_abr)
len_OTU_abr <- length(OTU_counts_abr_prop)
for (i in 1:len_OTU_abr) {
  index_vec_abr <- OTU_counts_abr_prop[1:i]
  index_vec_abr <- unname(index_vec_abr)
  pres_abr <- (1 - sum(index_vec_abr))
  if (pres_abr <= 0.01) {
    index_pres_vec_abr <- append(index_pres_vec_abr, TRUE)
  } else{
    index_pres_vec_abr <- append(index_pres_vec_abr, FALSE)
  }
}
