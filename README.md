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
There is additional software you may need to install outside of R: the hdf5 library and a C/C++ compiler. See the instructions for [BPCells](https://github.com/bnprks/BPCells) for more information.

## Getting started

Mona can be accessed with the following code, and a test dataset is available to immediately begin trying out various features:

```
library(Mona)
mona()
```

For exploring your own datasets, some additional steps are needed. Mona should be thought of as a layer on top of Seurat, which handles the actual data prepration. For a consistent and reliable experience, we recommended processing your datasets with the included functions, which try to follow the best practices and use the latest features in Seurat. The steps for a single dataset will be similar to the following: 

```
counts <- read.table("pbmc_counts.txt",sep="\t",header=T,row.names = 1)
seurat.object <- CreateSeuratObject(counts = counts, project = "pbmc", min.cells = 3, min.features = 200)
seurat.object <- run_mona_qc(seurat.object,"human")
seurat.object <- process_mona(seurat.object)
save_mona_dir(seurat.object,dir="pbmc",name="PBMC",description="A simple PBMC dataset",species="human")
```

We also provide a streamlined approach for integrating multiple datasets together. Each dataset must be individually processed prior to integration. 

```
seurat.object <- integrate_mona(list(dataset1,dataset2,dataset3))
save_mona_dir(seurat.object,dir="",name="PBMC",description="A simple PBMC dataset",species="human")
```

Note that if you are working with already processed datasets or choose not to use the included functions, we currently we only support Seurat objects processed with the "SCT v2" approach.

Regardless of how the data is processed, all datasets MUST be converted into a 'Mona directory' with the save_mona_dir() function. This serves as the input for Mona, greatly increasing the speed at which the data can be read. You are encouraged to save a separate "standard" version of the dataset, however Mona directories can also be converted back into a standard Seurat object format, important if you modify the dataset within Mona. 

For data processed outside Seurat, multiple tools are available for converting between single cell formats. See [sceasy](https://github.com/cellgeni/sceasy) for more information.

## Roadmap

- Slider to select cells by expression
- Density visualization
- Trajectory visualization 
- Automatic cell annotation

