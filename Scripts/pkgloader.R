pkgs <- c("BiocManager", "shiny", "cluster", "ClusterR", "factoextra", "FactoMineR", "fpc", "ggplot2", "gplots", "knitr", "markdown", "NbClust", "parallel", "philentropy", "RColorBrewer","remotes", "rmarkdown", "vegan")
for (pkg in pkgs) {
  install.packages(pkg)
  library(pkg, character.only = TRUE)
}

bio.pkgs <- c("GenomeInfoDbData", "GenomeInfoDb", )
for (pkg in bio.pkgs) {
  BiocManager::install(pkg)
  library(pkg, character.only = TRUE)
}

bio.pkgs <- c("DirichletMultinomial", "miaViz", "microbiome")
for (pkg in bio.pkgs) {
  BiocManager::install(pkg)
  library(pkg, character.only = TRUE)
}

remotes::install_github("hallucigenia-sparsa/seqgroup")
library('seqgroup')
