# Mona - Cell Explorer

Mona is an R package and Shiny application for single-cell data visualization, with the goal of allowing anyone to explore their data. It is focused on three central ideas:


Ease - Includes only the most important functions and options, clearly organized, fast loading and processing 

Interactivity - View multiple plots of multiple types at once, change/move/expand them as needed, use tools like zoom, pan, and select

Design - Modern look and feel, dedicate as much space to plots as possible, everything within a single page


## Installation

Assuming you have R and RStudio already on your system, open RStudio and enter:

```
remotes::install_github("ZornLab/Mona")
```
There is also some additional software you may need to install outside of R: the hdf5 library and a C/C++ compiler.

## Getting started

Mona should be thought of as a layer on top of Seurat, the package which handles the actual data prepration. For a consistent and reliable experience, datasets must be processed with the included functions, which try to follow the best practices and use the latest features in Seurat (SCT v2, BPCells, MAST). 

For already processed data, the easiest way to get started would be to 1. save any important metadata 2. reprocess the data 3. re-add metadata once complete. 

For data processed outside Seurat, multiple tools are available for converting between single cell formats. You will then need to use the included functions to create a Seurat object

The general steps for preparing your data will be similar to the following: 

```
counts <- read.table("pbmc_counts.txt",sep="\t",header=T,row.names = 1)
seurat.object <- CreateSeuratObject(counts = counts, project = "pbmc", min.cells = 3, min.features = 200)
seurat.object <- run_mona_qc(seurat.object,"human")
seurat.object <- process_mona(seurat.object)
save_mona_dir(seurat.object,dir="pbmc",name="PBMC",description="A simple PBMC dataset",species="human")
```

## Roadmap

- Slider to select cells by expression
- Density visualization
- Trajectory visualization 
- Automatic cell annotation

