# DGE2Pathview Explorer

An interactive R Shiny app for differential gene expression analysis (DESeq2) 
and KEGG pathway visualisation (pathview).

## Installation
```r
install.packages(c('shiny','shinydashboard','shinyWidgets',
                   'shinyjs','DT','ggplot2','plotly','base64enc','zip'))

BiocManager::install(c('DESeq2','pathview','KEGGREST','AnnotationDbi',
  'org.Hs.eg.db','org.Mm.eg.db','org.Rn.eg.db','org.Dr.eg.db',
  'org.Dm.eg.db','org.Ce.eg.db','org.Sc.sgd.db'))
```

## Usage
```r
shiny::runApp("app.R")
```

See `docs/DGE2Pathview_UserManual.pdf` for full documentation.