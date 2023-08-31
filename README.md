# Mona - Cell Explorer

Mona is an R package/Shiny application for single-cell data visualization, with the goal of allowing anyone to explore their data. It is focused on three central ideas:


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

Opening Mona is done using the code below, which should launch it within your web browser. At the moment Mona has been tested for individual use, but hosting is also possible.

```
library(Mona)
mona()
```

A test dataset is available to immediately begin trying out its features (see 'View datasets'). Users can also click the top left and view the 'Help' section for guidance on using Mona.

## Data preparation

To explore your own datasets, some additional steps are needed. Mona should be thought of as a layer on top of Seurat, which handles the actual data prepration. For a consistent and reliable experience, we recommended processing your datasets with the included functions, which try to follow the best practices in Seurat. The steps for a single dataset will be similar to the following: 

```
counts <- Read10X("raw_data/dataset")
seurat.object <- CreateSeuratObject(counts = counts, min.cells = 3, min.features = 200)
seurat.object <- run_mona_qc(seurat.object)
seurat.object <- process_mona(seurat.object)
save_mona_dir(seurat.object,dir="my_dataset",name="Name",description="Description",species="human")
```

We also provide a streamlined approach for integrating multiple datasets together. Each dataset must be individually processed prior to integration. 

```
seurat.object <- integrate_mona(list(dataset1,dataset2,dataset3))
save_mona_dir(seurat.object,dir="integrated",name="Name",description="Description",species="human")
```

Note that if you choose to use your own scripts for processing, currently only the "SCT v2" approach is supported.

Regardless of how the data is processed, all datasets MUST be converted into a 'Mona directory' with the save_mona_dir() function. These directories serve as the input for Mona, but you should also always save a separate "standard" version of the dataset (such as RDS).

Mona directories are still essentially Seurat objects, meaning their metadata can be copied and added back to the standard dataset, important if you are modifying/annotating the dataset within Mona. 

For data processed outside Seurat, multiple tools are available for converting between single cell formats. [sceasy](https://github.com/cellgeni/sceasy) is recommended.

## Roadmap

- Slider to select cells by expression
- Density visualization
- Trajectory visualization 
- Automatic cell annotation

