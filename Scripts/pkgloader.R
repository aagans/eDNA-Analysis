install.load <- readline(prompt="Do you want to INSTALL or just LOAD packages? INPUT: ")


if (install.load == 'INSTALL'){
pkgs <- c("BiocManager", "reshape2", "magrittr", "dplyr", "shiny", "cluster", "ClusterR", "factoextra", "FactoMineR", "fpc", "ggplot2", "gplots", "knitr", "markdown", "NbClust", "parallel", "philentropy", "RColorBrewer","remotes", "rmarkdown", "vegan")
for (pkg in pkgs) {
  install.packages(pkg)
  library(pkg, character.only = TRUE)
}

bio.pkgs <- c("GenomeInfoDbData", "GenomeInfoDb" )
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
} else if (install.load == 'LOAD'){
  pkgs <- c("BiocManager","reshape2", "magrittr", "dplyr", "shiny", "cluster", "ClusterR", "factoextra", "FactoMineR", "fpc", "ggplot2", "gplots", "knitr", "markdown", "NbClust", "parallel", "philentropy", "RColorBrewer","remotes", "rmarkdown", "vegan")
  for (pkg in pkgs) {
    library(pkg, character.only = TRUE)
  }
  
  bio.pkgs <- c("GenomeInfoDbData", "GenomeInfoDb" )
  for (pkg in bio.pkgs) {
    library(pkg, character.only = TRUE)
  }
  
  bio.pkgs <- c("DirichletMultinomial", "miaViz", "microbiome")
  for (pkg in bio.pkgs) {
    library(pkg, character.only = TRUE)
  }
  library('seqgroup')
}else print("Please input INSTALL or LOAD")
