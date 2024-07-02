# Mona - Single Cell Data Explorer

Mona is an R package/Shiny application for single-cell data visualization (including RNA, spatial, ATAC, etc.), with the goal of allowing anyone to explore their data. It is built around three central ideas:


Ease - Focus on the most important plots and options, quickly load and view large datasets, provide functions to automate data processing

Interactivity - View multiple plots of multiple types at once, change/move/expand them seamlessly, use tools like zoom, pan, and select 

Design - Dedicate as much space to plots as possible, clearly organized, everything within a single page


![](github/screenshot.png)


Major features include:

- View up to 8 plots
- Expand plots to full screen
- Split plots by metadata
- 3D embeddings
- Differential expression
- Volcano/MA plots
- Gene set scores
- Gene expression density
- Retrieve GO terms
- Reference-based label transfer


## Installation

Assuming you have R (and optionally, RStudio) already on your system, open R/RStudio and enter:

```
if (!require("remotes")) install.packages("remotes")
remotes::install_github("ZornLab/Mona")
```
There is additional software you may need outside of R: the hdf5 library and a C/C++ compiler. See [BPCells](https://github.com/bnprks/BPCells), which handles Mona's data storage, for more information. Binaries for BPCells are also available [here](https://bnprks.r-universe.dev/BPCells).

## Getting started

Open Mona using the code below, which should launch it within your web browser.

```
library(Mona)
mona()
```

A test dataset is available to try out its features (see 'View datasets'). Users can also click the top left and view the 'Help' section for guidance on using Mona.

## Data preparation

To provide a starting point for users unfamiliar with single cell RNA analysis, Mona provides functions to help generate Seurat objects from raw data. Here is an example for processing a single dataset: 

```
counts <- Read10X("raw_data/dataset")
seurat <- process_mona(counts)
```

We also provide a simple way to integrate multiple datasets together:

```
counts_list <- list(WT=counts_1,MUT=counts_2)
seurat <- integrate_mona(counts_list)
```

Mona works equally well with already processed data from Seurat/Signac or other formats. Note that not all technologies have been tested, and Mona currently operates only with count-like data, not fragments. 

## Using Mona

All datasets must be converted into a 'Mona directory' before they can be viewed. For Seurat objects:

```
save_mona_dir(seurat,assay="SCT",dir="Desktop/my_dataset",name="Name",description="Description",species="human")
```

A Mona directory can also be created from three components - the lognorm counts, cell metadata, and a list of coordinates. This is useful if working with anndata, SCE, etc.

```
save_mona_dir(counts=counts,meta=meta,coords=coords,dir="Desktop/my_dataset",name="Name",description="Description",species="human")
```

The final step is to launch Mona, click 'Load dataset', and navigate to the directory. You can also open a dataset automatically by providing the path:

```
mona("Desktop/my_dataset")
```

Once finished, if you edited the metadata/annotations use 'Save dataset' to preserve your changes. Your current settings and gene sets can also be saved separately using 'Save session'. 

## Label transfer

Mona includes a custom method for automated label transfer, where celltypes or other labels can be learned from annotated data. Similar to above, a 'Mona reference' can be created as shown here:

```
create_mona_ref(mona_dir="Desktop/my_dataset",anno="Celltype",file="Desktop/my_ref",species="human",type="RNA",norm="SCT")
```

Then with your query dataset open within Mona, go to 'Transfer labels', load the reference, and press 'Transfer' to begin. 

## Hosting

Like other Shiny apps, Mona can be hosted online and made available to multiple users. You'll likely want an 'app.R' file that launches Mona with the following arguments. This will provide a specific list of datasets to view, disables editing or loading other datasets, and provides a message at startup to help new users.

```
mona(data_dir="/datasets/",load_data=FALSE,save_data=FALSE,show_help=TRUE)
```

