pkgs <- c("BiocManager", "cluster", "ClusterR", "factoextra", "FactoMineR", "fpc", "ggplot2", "gplots", "knitr", "markdown", "NbClust", "parallel", "philentropy", "RColorBrewer","remotes", "rmarkdown", "vegan")
for (pkg in pkgs) {
  install.packages(pkg)
  library(pkg, character.only = TRUE)
}

bio.pkgs <- c("DirichletMultinomial", "miaViz", "microbiome")
for (pkg in bio.pkgs) {
  BiocManager::install()
  library(pkg, character.only = TRUE)
}

remotes::install_github("hallucigenia-sparsa/seqgroup")
library('seqgroup')
