# DGE2Pathview Explorer

An interactive R Shiny app for differential gene expression analysis (DESeq2) 
and KEGG pathway visualisation (pathview).
## 🚀 Quick Start

Clone the repository and install required packages:

```r
# Install dependencies
source("install.R")

# Run the app
shiny::runApp()

## Installation
```r
# ---- Install CRAN packages ----
cran_packages <- c(
  "shiny", "shinydashboard", "shinyWidgets", "shinyjs",
  "DT", "ggplot2", "plotly", "base64enc", "zip",
  "dplyr", "stringr", "tidyr", "readr"
)

install.packages(setdiff(cran_packages, rownames(installed.packages())))

# ---- Install Bioconductor manager ----
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

# ---- Install Bioconductor packages ----
bioc_packages <- c(
  "DESeq2", "pathview", "KEGGREST", "AnnotationDbi",
  "limma", "SummarizedExperiment",
  "org.Hs.eg.db", "org.Mm.eg.db", "org.Rn.eg.db",
  "org.Dr.eg.db", "org.Dm.eg.db", "org.Ce.eg.db",
  "org.Sc.sgd.db",
  "impute", "vsn"
)

BiocManager::install(setdiff(bioc_packages, rownames(installed.packages())))
```

## Usage
```r
shiny::runApp("app.R")
```
## ▶️ Run directly from GitHub

```r
install.packages("shiny")
shiny::runGitHub("DGE2Pathview", "simranven996")

See `docs/DGE2Pathview_UserManual.pdf` for full documentation.
