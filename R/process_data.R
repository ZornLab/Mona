#Simplified, efficient versions of FindMarkers and FindAllMarkers
#Necessary because original functions are not compatible with BPCells
#Enforces the following parameters by default:
#test.use="MAST"
#latent.vars="CDR"
#max.cells.per.ident = 500
#logfc.threshold = 0.5
#min.pct = 0.1
#Returns only the top 100 markers per cluster

mona_fc <- function(data.use,cells.1,cells.2,mean.fxn) {
  fc.name <- "avg_log2FC"
  thresh.min <- 0
  pct.1 <- round(
    x = rowSums(x = data.use[, cells.1, drop = FALSE] > thresh.min) /
      length(x = cells.1),
    digits = 3
  )
  pct.2 <- round(
    x = rowSums(x = data.use[, cells.2, drop = FALSE] > thresh.min) /
      length(x = cells.2),
    digits = 3
  )
  data.1 <- mean.fxn(data.use[, cells.1, drop = FALSE])
  data.2 <- mean.fxn(data.use[, cells.2, drop = FALSE])
  fc <- (data.1 - data.2)
  fc.results <- as.data.frame(x = cbind(fc, pct.1, pct.2))
  colnames(fc.results) <- c(fc.name, "pct.1", "pct.2")
  return(fc.results)
}

mona_mast <- function(data.use,cells.1,cells.2,latent.vars) {
  group.info <- data.frame(row.names = c(cells.1, cells.2))
  group.info[cells.1, "group"] <- "Group1"
  group.info[cells.2, "group"] <- "Group2"
  group.info[, "group"] <- factor(x = group.info[, "group"])
  if (is.null(latent.vars)) {
    latent.vars.names <- c("condition")
    latent.vars <- group.info
  } else {
    latent.vars.names <- c("condition", colnames(x = latent.vars))
    latent.vars <- cbind(latent.vars, group.info)
  }
  latent.vars$wellKey <- rownames(x = latent.vars)
  fdat <- data.frame(rownames(x = data.use))
  colnames(x = fdat)[1] <- "primerid"
  rownames(x = fdat) <- fdat[, 1]
  sca <- MAST::FromMatrix(
    exprsArray = as.matrix(x = data.use),
    check_sanity = FALSE,
    cData = latent.vars,
    fData = fdat
  )
  cond <- factor(x = SummarizedExperiment::colData(sca)$group)
  cond <- relevel(x = cond, ref = "Group1")
  SummarizedExperiment::colData(sca)$condition <- cond
  fmla <- as.formula(
    object = paste0(" ~ ", paste(latent.vars.names, collapse = "+"))
  )
  zlmCond <- MAST::zlm(formula = fmla, sca = sca)
  summaryCond <- MAST::summary(object = zlmCond, doLRT = 'conditionGroup2')
  summaryDt <- summaryCond$datatable
  summaryDt <- summaryDt[summaryDt$component == "H",]
  p_val <- summaryDt[["Pr(>Chisq)"]]
  genes.return <- summaryDt[["primerid"]]
  to.return <- data.frame(p_val, row.names = genes.return)
  return(to.return)
}

markers_mona_all <- function(object,metadata) {
  dqset.seed(123)
  genes.de <- list()
  messages <- list()
  Idents(object) <- metadata
  idents.all <- sort(x = unique(x = Idents(object)))
  for (i in 1:length(x = idents.all)) {
    genes.de[[i]] <- tryCatch(
      expr = {
        markers_mona(
          object = object,
          metadata = metadata,
          cluster = idents.all[i]
        )
      },
      error = function(cond) {
        return(cond$message)
      }
    )
    if (is.character(x = genes.de[[i]])) {
      messages[[i]] <- genes.de[[i]]
      genes.de[[i]] <- NULL
    }
  }
  gde.all <- data.frame()
  for (i in 1:length(x = idents.all)) {
    if (is.null(x = unlist(x = genes.de[i]))) {
      next
    }
    gde <- genes.de[[i]]
    if (nrow(x = gde) > 0) {
      gde <- gde[order(gde$p_val, -gde[, 2]), ]
      gde$cluster <- idents.all[i]
      gde$gene <- rownames(x = gde)
      gde$metadata <- metadata
      gde.all <- rbind(gde.all, gde)
    } else {
      gde <- data.frame(p_val=0,avg_log2FC=0,pct.1=0,pct.2=0,p_val_adj=0,cluster=idents.all[i],gene="none",metadata=metadata)
      gde.all <- rbind(gde.all, gde)
    }
  }
  rownames(x = gde.all) <- make.unique(names = as.character(x = gde.all$gene))
  if (length(x = messages) > 0) {
    warning("The following tests were not performed: ", call. = FALSE, immediate. = TRUE)
    for (i in 1:length(x = messages)) {
      if (!is.null(x = messages[[i]])) {
        warning("When testing ", idents.all[i], " versus all:\n\t", messages[[i]], call. = FALSE, immediate. = TRUE)
      }
    }
  }
  if (nrow(x = gde.all) == 0) {
    warning("No DE genes identified", call. = FALSE, immediate. = TRUE)
    return(NULL)
  } else {
    gde.all <- gde.all %>% arrange(p_val_adj) %>% group_by(cluster) %>% slice(1:100)
    return(gde.all)
  }
}

# recorrect_umi should be TRUE if merged/integrated and may not have had PrepSCTFIndMarkers() run yet (for user-generated datasets)
# Set to FALSE if it definitely has been run but was subsetted afterwards
# If it definitely was run and you want to save some time, can be set to FALSE

#' Mona marker calculation
#'
#' Function for calculating markers for a single cluster
#'
#' @rawNamespace import(Seurat, except = "JS")
#' @rawNamespace import(MAST, except = "show")
#' @import dqrng
#' @rawNamespace import(dplyr, except = "vars")
#' @param object A Seurat object
#' @param metadata Metadata column
#' @param cluster Group within the metadata
#' @param cells List of cell names
#' @param downsample Number of cells per group
#' @return DE results
markers_mona <- function(object,metadata=NULL,cluster=NULL,cells=NULL,downsample=500) {
  if (is.null(cells)) {
    Idents(object) <- metadata
    cells.1 <- WhichCells(object, idents = cluster)
  } else {
    cells.1 <- cells
  }
  cells.2 <- setdiff(x = colnames(object), y = cells.1)
  if (length(x = cells.1) > downsample) {
    cells.1 <-  dqsample(cells.1, downsample)
  }
  if (length(x = cells.2) > downsample) {
    cells.2 <-  dqsample(cells.2, downsample)
  }
  data.subset <- object[, c(cells.1, cells.2)]
  data.use <- t(FetchData(data.subset,vars=rownames(data.subset)))
  if ("CDR" %in% names(object@meta.data)) {
    cdr <- data.subset[["CDR"]][c(cells.1, cells.2),]
  } else {
    cdr <- NULL
  }
  mean.fxn <- function(x) {return(log(x = rowMeans(x = expm1(x = x)) + 1, base = 2))}
  fc.results <- mona_fc(
    data.use,
    cells.1 = cells.1,
    cells.2 = cells.2,
    mean.fxn = mean.fxn
  )
  # feature selection (based on percentages)
  alpha.min <- pmax(fc.results$pct.1, fc.results$pct.2)
  names(x = alpha.min) <- rownames(x = fc.results)
  features <- names(x = which(x = alpha.min >= 0.1))
  if (length(x = features) == 0) {
    warning("No features pass min.pct threshold; returning empty data.frame")
    return(fc.results[features, ])
  }
  # feature selection (based on logFC)
  total.diff <- fc.results[, 1] 
  names(total.diff) <- rownames(fc.results)
  features.diff <- names(x = which(x = abs(x = total.diff) >= 0.5))
  features <- intersect(x = features, y = features.diff)
  if (length(x = features) == 0) {
    warning("No features pass logfc.threshold threshold; returning empty data.frame")
    return(fc.results[features, ])
  }
  data.use <- data.use[features,c(cells.1, cells.2),drop=F]
  de.results <- mona_mast(data.use,cells.1,cells.2,cdr)
  de.results <- cbind(de.results, fc.results[rownames(x = de.results), , drop = FALSE])
  de.results <- de.results[order(de.results$p_val, -de.results[, 1]), ]
  de.results$p_val_adj = p.adjust(
    p = de.results$p_val,
    method = "bonferroni",
    n = nrow(x = object)
  )
  de.results <- subset(x = de.results, subset = p_val_adj <= 0.05)
  return(de.results)
}

#' Mona data processing
#'
#' Function for performing standard processing including normalization, PCA, clustering, and UMAPs.
#' 
#' @rawNamespace import(Seurat, except = "JS")
#' @import sctransform
#' @import glmGamPoi
#' @import scuttle
#' @param counts Raw counts in a matrix-like format
#' @param meta A optional dataframe with rownames as cells and colnames as cell metadata
#' @param mode Whether to use SCT v2 or LogNormalize 
#' @param qc Whether to automatically remove low-quality cells
#' @return Seurat object with SCT/LogNormalize processing applied
#' @export
process_mona <- function(counts=NULL,meta=NULL,mode=c("sct","lognorm"),qc=TRUE) {
  mode <- match.arg(mode)
  options(Seurat.object.assay.version = 'v5')
  seurat <- CreateSeuratObject(counts = as.matrix(counts), meta.data = meta, min.cells = 3, min.features = 200)
  if (qc) {
    seurat$percent.mt <- PercentageFeatureSet(seurat, pattern = "Mt-|^mt-|^MT-")
    qc_data <- FetchData(seurat,vars=c("nCount_RNA","nFeature_RNA","percent.mt"))
    filters <- perCellQCFilters(qc_data, sum.field = "nCount_RNA",detected.field = "nFeature_RNA",sub.fields = "percent.mt")
    print(paste0("Cells before QC filtering: ",ncol(seurat)))
    seurat <- subset(seurat, cells = colnames(seurat)[!filters$discard])
    print(paste0("Cells after QC filtering: ",ncol(seurat)))
  }
  if (mode == "sct") {
    print("Processing with SCT v2")
  	seurat <- SCTransform(seurat, vst.flavor = "v2", variable.features.n = 3000)
  	seurat <- RunPCA(seurat,npcs=30,verbose=F)
  	seurat <- FindNeighbors(seurat, dims = 1:30)
  	seurat <- FindClusters(seurat, resolution = c(0.4, 0.8, 1.2))
  	seurat <- RunUMAP(seurat,dims = 1:30,n.components=2L,reduction.name="UMAP_2D",reduction.key="umap2d_")
  	seurat <- RunUMAP(seurat,dims = 1:30,n.components=3L,reduction.name="UMAP_3D",reduction.key="umap3d_")
  	seurat[["CDR"]] <- scale(colSums(seurat[["RNA"]]$counts>0))
  	seurat[["orig.ident"]] <- NULL
  	seurat[["seurat_clusters"]] <- NULL
  	seurat[["clusters_res_0.4"]] <- seurat[["SCT_snn_res.0.4"]]
  	seurat[["SCT_snn_res.0.4"]] <- NULL
  	seurat[["clusters_res_0.8"]] <- seurat[["SCT_snn_res.0.8"]]
  	seurat[["SCT_snn_res.0.8"]] <- NULL
  	seurat[["clusters_res_1.2"]] <- seurat[["SCT_snn_res.1.2"]]
  	seurat[["SCT_snn_res.1.2"]] <- NULL
  } else if (mode == "lognorm") {
    print("Processing with LogNormalize")
    seurat <- NormalizeData(seurat,verbose = FALSE)
    seurat <- FindVariableFeatures(seurat, selection.method = "vst", nfeatures = 3000, verbose = FALSE)
    seurat <- ScaleData(seurat)
    seurat <- RunPCA(seurat,npcs=30,verbose=F)
    seurat <- FindNeighbors(seurat, dims = 1:30)
    seurat <- FindClusters(seurat, resolution = c(0.4, 0.8, 1.2))
    seurat <- RunUMAP(seurat,dims = 1:30,n.components=2L,reduction.name="UMAP_2D",reduction.key="umap2d_")
    seurat <- RunUMAP(seurat,dims = 1:30,n.components=3L,reduction.name="UMAP_3D",reduction.key="umap3d_")
    seurat[["CDR"]] <- scale(colSums(seurat[["RNA"]]$counts>0))
    seurat[["orig.ident"]] <- NULL
    seurat[["seurat_clusters"]] <- NULL
    seurat[["clusters_res_0.4"]] <- seurat[["RNA_snn_res.0.4"]]
    seurat[["RNA_snn_res.0.4"]] <- NULL
    seurat[["clusters_res_0.8"]] <- seurat[["RNA_snn_res.0.8"]]
    seurat[["RNA_snn_res.0.8"]] <- NULL
    seurat[["clusters_res_1.2"]] <- seurat[["RNA_snn_res.1.2"]]
    seurat[["RNA_snn_res.1.2"]] <- NULL
  }
	return(seurat)
}

#' Mona integration
#'
#' Function for integrating multiple Seurat objects into a single batch-corrected Seurat object
#' 'process_mona' must be run individually on each dataset before integration
#'
#' @rawNamespace import(Seurat, except = "JS")
#' @import sctransform
#' @import glmGamPoi
#' @import scuttle
#' @param counts_list A named list of raw counts in matrix-like format
#' @param meta_list An optional list of dataframes with rownames as cells and colnames as cell metadata
#' @param mode Whether to use SCT v2 or LogNormalize 
#' @param qc Whether to automatically remove low-quality cells
#' @return An integrated Seurat object 
#' @export
integrate_mona <- function(counts_list=NULL,meta_list=NULL,mode=c("sct","lognorm"),qc=TRUE) {
  mode <- match.arg(mode)
  options(Seurat.object.assay.version = 'v5')
  datasets <- names(counts_list)
  if (is.null(meta_list)) {
    meta_list <- lapply(1:length(counts_list),function(x) {
      df <- data.frame("Dataset"=rep(datasets[x],ncol(counts_list[[x]])))
      rownames(df) <- colnames(counts_list[[x]])
      return(df)
      })
  } else {
    meta_list <- lapply(1:length(meta_list),function(x) cbind(meta_list[[x]],data.frame("Dataset"=rep(datasets[x],nrow(meta_list[[x]])))))
  }
  meta <- do.call("rbind",meta_list)
  seurat <- CreateSeuratObject(counts = counts_list, meta.data = meta, min.cells = 3, min.features = 200)
  if (qc) {
    seurat$percent.mt <- PercentageFeatureSet(seurat, pattern = "Mt-|^mt-|^MT-")
    qc_data <- FetchData(seurat,vars=c("nCount_RNA","nFeature_RNA","percent.mt"))
    filters <- perCellQCFilters(qc_data, sum.field = "nCount_RNA",detected.field = "nFeature_RNA",sub.fields = "percent.mt")
    print(paste0("Cells before QC filtering: ",ncol(seurat)))
    seurat <- subset(seurat, cells = colnames(seurat)[!filters$discard])
    print(paste0("Cells after QC filtering: ",ncol(seurat)))
  }
  if (mode == "sct") {
    print("Processing with SCT v2")
    seurat <- SCTransform(seurat, vst.flavor = "v2", variable.features.n = 3000)
    seurat <- RunPCA(seurat,npcs=30,verbose=FALSE)
    print("Starting integration...")
    seurat <- IntegrateLayers(object = seurat, method = CCAIntegration, assay = "SCT", normalization.method = "SCT", orig.reduction = "pca", new.reduction = "integrated.cca",verbose = FALSE)
  	seurat <- FindNeighbors(seurat, reduction = "integrated.cca", dims = 1:30)
  	seurat <- FindClusters(seurat, resolution = c(0.4, 0.8, 1.2))
  	seurat <- RunUMAP(seurat,dims = 1:30,n.components=2L,reduction = "integrated.cca",reduction.name="UMAP_2D",reduction.key="umap2d_")
  	seurat <- RunUMAP(seurat,dims = 1:30,n.components=3L,reduction = "integrated.cca",reduction.name="UMAP_3D",reduction.key="umap3d_")
  	seurat <- PrepSCTFindMarkers(seurat)
  	seurat[["CDR"]] <- scale(colSums(seurat[["RNA"]]$counts>0))
  	seurat[["orig.ident"]] <- NULL
  	seurat[["seurat_clusters"]] <- NULL
  	seurat[["clusters_res_0.4"]] <- seurat[["SCT_snn_res.0.4"]]
  	seurat[["SCT_snn_res.0.4"]] <- NULL
  	seurat[["clusters_res_0.8"]] <- seurat[["SCT_snn_res.0.8"]]
  	seurat[["SCT_snn_res.0.8"]] <- NULL
  	seurat[["clusters_res_1.2"]] <- seurat[["SCT_snn_res.1.2"]]
  	seurat[["SCT_snn_res.1.2"]] <- NULL
  } else if (mode == "lognorm") {
    print("Processing with LogNormalize")
    seurat <- JoinLayers(seurat)
    seurat[["RNA"]] <- split(seurat[["RNA"]], f = seurat$Dataset)
    seurat <- NormalizeData(seurat,verbose = FALSE)
    seurat <- FindVariableFeatures(seurat, selection.method = "vst", nfeatures = 3000, verbose = FALSE)
    seurat <- ScaleData(seurat)
    seurat <- RunPCA(seurat,npcs=30,verbose = FALSE)
    print("Starting integration...")
    seurat <- IntegrateLayers(object = seurat, method = CCAIntegration, assay = "RNA", normalization.method = "LogNormalize", group.by = "Dataset", orig.reduction = "pca", new.reduction = "integrated.cca",verbose = FALSE)
    seurat <- FindNeighbors(seurat, reduction = "integrated.cca", dims = 1:30)
    seurat <- FindClusters(seurat, resolution = c(0.4, 0.8, 1.2))
    seurat <- RunUMAP(seurat,dims = 1:30,n.components=2L,reduction = "integrated.cca",reduction.name="UMAP_2D",reduction.key="umap2d_")
    seurat <- RunUMAP(seurat,dims = 1:30,n.components=3L,reduction = "integrated.cca",reduction.name="UMAP_3D",reduction.key="umap3d_")
    seurat <- JoinLayers(seurat)
    seurat[["CDR"]] <- scale(colSums(seurat[["RNA"]]$counts>0))
    seurat[["orig.ident"]] <- NULL
    seurat[["seurat_clusters"]] <- NULL
    seurat[["clusters_res_0.4"]] <- seurat[["RNA_snn_res.0.4"]]
    seurat[["RNA_snn_res.0.4"]] <- NULL
    seurat[["clusters_res_0.8"]] <- seurat[["RNA_snn_res.0.8"]]
    seurat[["RNA_snn_res.0.8"]] <- NULL
    seurat[["clusters_res_1.2"]] <- seurat[["RNA_snn_res.1.2"]]
    seurat[["RNA_snn_res.1.2"]] <- NULL
  }
	return(seurat)
}

#' Mona directory creation
#'
#' Function for preparing Seurat object for use within Mona. Creates a directory containing the actual object as a .qs file, as well as expression data saved using BPCells.
#' Also calculates markers, removes unnecessary data, stores metadata.
#'
#' @rawNamespace import(Seurat, except = "JS")
#' @import BPCells
#' @import qs
#' @rawNamespace import(MAST, except = "show")
#' @import dqrng
#' @rawNamespace import(dplyr, except = "vars")
#' @param seurat A Seurat object, recommend using those generated by 'process_mona' and 'integrate_mona'
#' @param dir The name of the directory where the dataset will be stored on your file system. Make it memorable to differentiate from other datasets.
#' @param name The actual name of the dataset, determines the name that will be displayed within Mona.
#' @param description A brief sentence describing the dataset. Not required, but useful for tracking important information or when sharing with others.
#' @param species The species the dataset originated from. Affects some functions within Mona, so should be specified. Must use one of the following, or an NCBI taxonomy ID: human, mouse, rat, fruitfly, nematode, zebrafish, thale-cress, frog, pig
#' @return A 'Mona directory' that can be loaded into Mona
#' @export
save_mona_dir <- function(seurat=NULL,dir=NULL,name=NULL,description=NULL,species="human",markers=T) {
  options(Seurat.object.assay.version = 'v5')
  assays <- names(seurat@assays)
  if ("SCT" %in% assays && length(x = levels(x = seurat[["SCT"]])) > 1) {
    cell_attributes <- SCTResults(object = seurat, slot = "cell.attributes")
    observed_median_umis <- lapply(
      X = cell_attributes,
      FUN = function(x) median(x[, "umi"])
    )
    model.list <- slot(object = seurat[["SCT"]], "SCTModel.list")
    median_umi.status <- lapply(X = model.list,
                                FUN = function(x) { return(tryCatch(
                                  expr = slot(object = x, name = 'median_umi'),
                                  error = function(...) {return(NULL)})
                                )})
    if (any(is.null(unlist(median_umi.status)))){
      stop("SCT assay does not contain median UMI information. Must run `PrepSCTFindMarkers()` to continue.")
    }
    model_median_umis <- SCTResults(object = seurat, slot = "median_umi")
    min_median_umi <- min(unlist(x = observed_median_umis))
    if (any(unlist(model_median_umis) != min_median_umi)){
      stop("Object contains multiple models with unequal library sizes. Must run `PrepSCTFindMarkers()` to continue.")
    }
  }
  if (markers) {
    print("Calculating markers...")
    meta_names <- colnames(seurat@meta.data)
    filter_1 <- sapply(meta_names, function(x) class(seurat[[x]][,1]) %in% c("integer","numeric"))
    filter_2 <- sapply(meta_names, function(x) length(unique(seurat[[x]][,1])) > 100)
    meta_names <- meta_names[!(filter_1 & filter_2)]
    markers <- lapply(meta_names,function(x) markers_mona_all(seurat,x))
    markers_final <- bind_rows(markers)
    markers_final$avg_log2FC <- signif(markers_final$avg_log2FC,3)
    markers_final$p_val_adj <- formatC(markers_final$p_val_adj, format = "e", digits = 2)
    seurat@misc$markers <- markers_final[,c("gene","cluster","metadata","avg_log2FC","p_val_adj")]
  } else {
    seurat@misc$markers <- data.frame(gene="none",cluster="none",metadata="none",avg_log2FC=0,p_val_adj=0)
  }
  save_assay <- "RNA"
  if ("SCT" %in% assays) {
    save_assay <- "SCT"
  }
  if ("integrated" %in% assays) {
    gene_var <- VariableFeatures(seurat,assay="integrated")
  } else {
    gene_var <- VariableFeatures(seurat,assay=save_assay)
  }
	seurat@misc$var_100 <- gene_var[1:min(100, length(gene_var))]
	seurat@misc$var_500 <- gene_var[1:min(500, length(gene_var))]
	seurat@misc$var_1000 <- gene_var[1:min(1000, length(gene_var))]
	reducs <- names(seurat@reductions)
	reducs <- reducs[!grepl("pca",reducs)]
	reducs <- reducs[!grepl("integrated",reducs)]
	DefaultAssay(seurat) <- save_assay
	seurat <- DietSeurat(seurat,layers=c("data"),assays = c(save_assay),dimreducs = reducs)
	seurat@misc$species <- species
	seurat@misc$name <- name
	seurat@misc$description <- description
	print("Saving expression data")
	write_matrix_dir(mat = as(seurat[[save_assay]]$data,"sparseMatrix"), dir = dir)
	disk.mat <- open_matrix_dir(dir = dir)
	seurat[[save_assay]] <- as(object = seurat[[save_assay]], Class = "Assay5")
	seurat[[save_assay]]$data <- disk.mat
	qsave(seurat,file=paste0(dir,"/seurat.qs"))
}

#' Transfer Mona metadata
#'
#' Function for taking metadata from a Mona directory and applying it to a standard Seurat object, such as when dataset has been annotated
#' 
#' @rawNamespace import(Seurat, except = "JS")
#' @param mona_dir A Mona directory
#' @param seurat A Seurat object
#' @return Seurat object with updated metadata
#' @export
transfer_mona_data <- function(mona_dir=NULL,seurat=NULL) {
  mona_seurat <- qread(paste0(mona_dir,"/seurat.qs"))
  seurat@meta.data <- mona_seurat@meta.data
  return(seurat)
}