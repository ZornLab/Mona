mona_fc <- function(data.use,cells.1,cells.2,mean.fxn) {
  pct.1 <- round(
    x = rowSums(x = data.use[, cells.1, drop = FALSE] > 0) / length(x = cells.1),
    digits = 3
  )
  pct.2 <- round(
    x = rowSums(x = data.use[, cells.2, drop = FALSE] > 0) / length(x = cells.2),
    digits = 3
  )
  avg.1 <- mean.fxn(data.use[, cells.1, drop = FALSE])
  avg.2 <- mean.fxn(data.use[, cells.2, drop = FALSE])
  fc <- (avg.1 - avg.2)
  avg.1 <- round(avg.1,digits = 3)
  avg.2 <- round(avg.2,digits = 3)
  fc.results <- as.data.frame(x = cbind(fc, pct.1, pct.2, avg.1, avg.2))
  colnames(fc.results) <- c("avg_log2FC", "pct.1", "pct.2", "avg.1", "avg.2")
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

markers_mona_all <- function(exp=NULL,meta=NULL,anno=NULL,fc_only=F) {
  dqset.seed(123)
  genes.de <- list()
  messages <- list()
  groups <- sort(x = funique(meta[[anno]]))
  if (length(groups) == 1) {
    return(NULL)
  }
  for (i in 1:length(x = groups)) {
    genes.de[[i]] <- tryCatch(
      expr = {
        markers_mona(exp,meta,anno,groups[i],fc_only=fc_only)
      },
      error = function(cond) {
        return(cond$message)
      },
      warning = function(cond) {
        return(cond$message)
      }
    )
    if (is.character(x = genes.de[[i]])) {
      messages[[i]] <- genes.de[[i]]
      genes.de[[i]] <- NULL
    }
  }
  gde.all <- data.frame()
  for (i in 1:length(x = groups)) {
    if (is.null(x = unlist(x = genes.de[i]))) {
      next
    }
    gde <- genes.de[[i]]
    if (fc_only) {
      if (nrow(gde) > 0) {
        gde$cluster <- groups[i]
        gde$gene <- rownames(x = gde)
        gde.all <- rbind(gde.all, gde)
      } else {
        next
      }
    } else {
      if (nrow(gde) > 0) {
        gde <- gde[order(gde$p_val, -gde[, 2]), ]
        gde$cluster <- groups[i]
        gde$gene <- rownames(x = gde)
        gde$metadata <- anno
        gde.all <- rbind(gde.all, gde)
      } else {
        gde <- data.frame(p_val=0,avg_log2FC=0,pct.1=0,pct.2=0,avg.1=0,avg.2=0,p_val_adj=0,cluster=groups[i],gene="none",metadata=anno)
        gde.all <- rbind(gde.all, gde)
      }
    }
  }
  rownames(x = gde.all) <- make.unique(names = as.character(x = gde.all$gene))
  if (length(x = messages) > 0) {
    warning("The following tests were not performed: ", call. = FALSE, immediate. = TRUE)
    for (i in 1:length(x = messages)) {
      if (!is.null(x = messages[[i]])) {
        warning("When testing ", groups[i], " versus all:\n\t", messages[[i]], call. = FALSE, immediate. = TRUE)
      }
    }
  }
  if (nrow(x = gde.all) == 0) {
    warning("No DE genes identified", call. = FALSE, immediate. = TRUE)
    return(NULL)
  } else {
    if (fc_only) {
      gde.all <- gde.all %>% filter(case_when(avg_log2FC > 0 ~ avg.1 >= 0.5, avg_log2FC < 0 ~ avg.1 >= 0)) %>% group_by(cluster) %>% arrange(desc(abs(avg_log2FC)), .by_group = TRUE) %>% slice(1:25)
    } else {
      gde.all <- gde.all %>% group_by(cluster) %>% arrange(p_val_adj, .by_group = TRUE) %>% slice(1:500)
    }
    if (nrow(x = gde.all) == 0) {
      warning("No DE genes identified", call. = FALSE, immediate. = TRUE)
      return(NULL)
    }
    return(gde.all)
  }
}

#' Mona marker calculation
#'
#' Function for calculating markers
#'
#' @rawNamespace import(Seurat, except = "JS")
#' @rawNamespace import(MAST, except = "show")
#' @import dqrng
#' @rawNamespace import(dplyr, except = "vars")
#' @param exp a cell by gene expression matrix
#' @param meta a cell by annotation matrix
#' @param anno a specific column in meta
#' @param group a specific group/cluster within anno 
#' @param cells.1 list of cells 
#' @param cells.2 second list of cells for comparison
#' @param fc_only Return FC data only
#' @return DE results
markers_mona <- function(exp=NULL,meta=NULL,anno=NULL,group=NULL,cells.1=NULL,cells.2=NULL,fc_only=F) {
  if (!is.null(anno) && !is.null(group)) {
    cells.1 <- rownames(meta[meta[[anno]] == group,])
    cells.2 <- setdiff(x = rownames(meta), y = cells.1)
  }
  if (length(x = cells.1) > 500) {
    cells.1 <- dqrng::dqsample(cells.1, 500)
  }
  if (length(x = cells.2) > 500) {
    cells.2 <- dqrng::dqsample(cells.2, 500)
  }
  data.use <- t(as.matrix(exp[c(cells.1, cells.2),]))
  if ("CDR" %in% colnames(meta)) {
    cdr <- meta[["CDR"]][c(cells.1, cells.2)]
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
    warning("No features pass min.pct threshold")
    return(NULL)
  }
  # feature selection (based on logFC)
  total.diff <- fc.results[, 1] 
  names(total.diff) <- rownames(fc.results)
  fc.cutoff <- if (fc_only) 0.33 else 0.5 
  features.diff <- names(x = which(x = abs(x = total.diff) >= fc.cutoff))
  features <- intersect(x = features, y = features.diff)
  if (length(x = features) == 0) {
    warning("No features pass logfc.threshold threshold")
    return(NULL)
  }
  if (fc_only) {
    return(fc.results[features, , drop = FALSE])
  }
  data.use <- data.use[features,c(cells.1, cells.2),drop=F]
  de.results <- mona_mast(data.use,cells.1,cells.2,cdr)
  if (nrow(de.results) == 0) {
    warning("No DEGs found")
    return(NULL)
  }
  de.results <- cbind(de.results, fc.results[rownames(x = de.results), ,drop = FALSE])
  de.results <- de.results[order(de.results$p_val, -de.results[, 1]), ]
  de.results$p_val_adj = p.adjust(
    p = de.results$p_val,
    method = "bonferroni",
    n = ncol(exp)
  )
  de.results <- subset(x = de.results, subset = p_val_adj <= 0.05)
  if (nrow(de.results) == 0) {
    warning("No DEGs found")
    return(NULL)
  } else {
    return(de.results)
  }
}

#' Mona scRNA data processing
#'
#' Function for creating a standard Seurat object for a single dataset
#' The 'sct' mode and quality control are recommended for most cases
#' 
#' @rawNamespace import(Seurat, except = "JS")
#' @import sctransform
#' @import glmGamPoi
#' @import scuttle
#' @param counts Raw counts in a matrix-like format, cells as columns and genes as rows
#' @param meta An optional dataframe with rownames as cells and colnames as cell metadata
#' @param mode Whether to use SCT or LogNormalize 
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
    print("Processing with SCT")
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

#' Mona scRNA integration
#'
#' Function for combining multiple datasets into a single batch-corrected Seurat object
#' The 'sct' mode and quality control are recommended for most cases
#'
#' @rawNamespace import(Seurat, except = "JS")
#' @import sctransform
#' @import glmGamPoi
#' @import scuttle
#' @param counts_list A named list of raw counts in matrix-like format
#' @param meta_list An optional list of dataframes with rownames as cells and colnames as cell metadata
#' @param mode Whether to use SCT or LogNormalize 
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
  counts_list <- lapply(counts_list, function(x) as.matrix(x))
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
    print("Processing with SCT")
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
#' Function for preparing a dataset for use within Mona
#' If using a Seurat/Signac object, provide 'seurat' and 'assay'. Otherwise, provide 'counts', 'meta', and 'coords'
#' Note that counts, meta, and coords must be organized along rows by cell
#'
#' @rawNamespace import(Seurat, except = "JS")
#' @import BPCells
#' @import qs
#' @rawNamespace import(MAST, except = "show")
#' @import dqrng
#' @rawNamespace import(dplyr, except = "vars")
#' @import UCell
#' @param seurat A Seurat/Signac object, such as those generated by 'process_mona' and 'integrate_mona'
#' @param assay Which assay contains normalized counts, AKA the 'data' slot. Usually 'RNA' or 'SCT'
#' @param counts A matrix of lognorm counts
#' @param meta A matrix of cell annotations
#' @param coords A named list of reductions/image data, each being a matrix of dimensions/coordinates
#' @param dir The file name/path where you want to save the dataset
#' @param name The name of the dataset that will be displayed within Mona
#' @param description A brief sentence describing the dataset. Not required, but useful when sharing with others
#' @param species Species of the dataset. The following are supported: human, mouse, rat, fruitfly, nematode, zebrafish, frog, pig
#' @param markers Whether to pre-calculate markers. Set to true if you plan to use markers often, but processing time can be long for many annotations.
#' @param scores Whether to calculate ranks for gene set scores. Set to false if you don't use this feature, as it adds to processing time and directory size.
#' @param password A password required to open the Mona directory. NOT secure, is stored as plain text in 'mona.qs'. Primarily to control access to datasets when hosting Mona.
#' @return A 'Mona directory' that can be loaded into Mona
#' @export
save_mona_dir <- function(seurat=NULL,assay=NULL,counts=NULL,meta=NULL,coords=NULL,dir=NULL,name=NULL,description=NULL,species="human",scores=T,markers=F,password=NULL) {
  mona <- list()
  BPPARAM <- if (.Platform$OS.type=="windows") BiocParallel::SerialParam() else BiocParallel::MulticoreParam(workers=1)
  if (!is.null(seurat) && !is.null(assay)) {
    print("Saving expression data")
    exp <- seurat[[assay]]$data %>% as("sparseMatrix") %>% t() %>% write_matrix_dir(dir = file.path(dir,"exp"))
    if (scores) {
      print("Calculating and saving gene ranks")
      seurat[[assay]]$data %>% StoreRankings_UCell(BPPARAM = BPPARAM) %>% t() %>% write_matrix_dir(dir = file.path(dir,"ranks"))
    }
    print("Saving remaining data")
    mona[["meta"]] <- seurat@meta.data %>% replace(is.na(.), "Undefined") %>% mutate_if(is.factor, as.character)
    reduct_list <- lapply(seurat@reductions, function(x) {
      embed <- x@cell.embeddings 
      if (ncol(embed) == 3) round(embed[,1:3,drop=F],3) else round(embed[,1:2,drop=F],3)
    })
    image_list <- lapply(seurat@images, function(x) {
      x@coordinates[,c("col","row")] %>% as("matrix")
    })
    reduct_filter <- sapply(seurat@reductions, function(x) ncol(x@cell.embeddings) == 2)
    mona[["reduct"]] <- c(reduct_list[c(which(reduct_filter),which(!reduct_filter))],image_list)
  } else if (!is.null(counts) && !is.null(meta) && !is.null(coords)) {
    if (nrow(counts) != nrow(meta)) {
      stop("Counts and meta must have same number of cells/rows.")
    }
    print("Saving expression data")
    exp <- counts %>% as("sparseMatrix") %>% write_matrix_dir(dir = file.path(dir,"exp"))  
    if (scores) {
      print("Calculating and saving gene ranks")
      seurat[[assay]]$data %>% StoreRankings_UCell(BPPARAM = BPPARAM) %>% t() %>% write_matrix_dir(dir = file.path(dir,"ranks"))
    }
    print("Saving remaining data")
    mona[["meta"]] <- meta %>% replace(is.na(.), "Undefined") %>% mutate_if(is.factor, as.character)
    reduct_list <- lapply(coords, function(x) if (ncol(x) == 3) round(x[,1:3,drop=F],3) else round(x[,1:2,drop=F],3))
    reduct_filter <- sapply(coords, function(x) ncol(x) == 2)
    mona[["reduct"]] <- reduct_list[c(which(reduct_filter),which(!reduct_filter))]
  } else {
    stop("Must provide either a Seurat object and assay or separate counts, metadata, and coordinates.")
  }
  gene_var <- BPCells::matrix_stats(exp,col_stats = "variance")[["col_stats"]]["variance",] %>% sort(decreasing = T) %>% names()
  gene_mean <- BPCells::colMeans(exp) %>% sort(decreasing = T) %>% names()
  mona[["sets"]] <- list(gene_var[1:min(500, length(gene_var))],gene_mean[1:min(500, length(gene_mean))])
  if (markers) {
    print("Calculating and saving markers")
    meta_names <- colnames(mona[["meta"]])
    filter <- sapply(meta_names, function(x) class(mona[["meta"]][,x]) %in% c("integer","numeric"))
    anno_names <- meta_names[!filter]
    markers <- lapply(anno_names,function(anno) markers_mona_all(exp,mona[["meta"]],anno))
    markers_final <- bind_rows(markers) %>% as.data.frame()
    markers_final$avg_log2FC <- signif(markers_final$avg_log2FC,3)
    markers_final$p_val_adj <- formatC(markers_final$p_val_adj, format = "e", digits = 2)
    markers_final$metadata <- as.character(markers_final$metadata)
    markers_final$cluster <- as.character(markers_final$cluster)
    mona[["markers"]] <- markers_final[,c("gene","cluster","metadata","avg_log2FC","p_val_adj","avg.1","avg.2")]
  } else {
    mona[["markers"]] <- data.frame(gene="none",cluster="none",metadata="none",avg_log2FC=0,p_val_adj=0,avg.1=0,avg.2=0)
  }
  info <- list(species=species,name=name,description=description)
  mona[["info"]] <- info
  qsave(info,file=file.path(dir,"info.qs"))
  if (!is.null(password)) {
    mona[["password"]] <- password
  }
  qsave(mona,file=file.path(dir,"mona.qs"))
}

#' Transfer Mona metadata
#'
#' Function for copying metadata from a Mona directory to a Seurat object
#' Useful fo continuing analysis outside of Mona after annotating a dataset
#' 
#' @rawNamespace import(Seurat, except = "JS")
#' @param mona_dir A Mona directory
#' @param seurat A Seurat object
#' @return Seurat object with updated metadata
#' @export
transfer_mona_data <- function(mona_dir=NULL,seurat=NULL) {
  mona <- qread(file.path(mona_dir,"mona.qs"))
  seurat@meta.data <- mona$meta
  return(seurat)
}

#' Mona reference creation
#' 
#' Function to create references for label transfer
#' Supply either a Mona directory, a Seurat object and assay, or a log-norm counts matrix and metadata table
#' @import BPCells
#' @import qs
#' @import glmnet
#' @import irlba
#' @param mona_dir A Mona directory
#' @param seurat A Seurat object
#' @param assay Which assay contains the normalized gene counts, AKA the 'data' slot. Usually 'RNA' or 'SCT'
#' @param counts A matrix of log-norm counts, cells as rows
#' @param meta A table of cell metadata, cells as rows
#' @param anno A vector of one or more annotations in the dataset you wish to train on
#' @param file The file name/path where you want to save the reference
#' @param species Species of the dataset. The following are supported: human, mouse, rat, fruitfly, nematode, zebrafish, frog, pig
#' @param type Whether the data is RNA or ATAC
#' @param norm Type of normalization, most common are SCT, LogNorm, and TFIDF
#' @return A Mona reference object
#' @export
#'
create_mona_ref <- function(mona_dir=NULL,seurat=NULL,assay=NULL,counts=NULL,meta=NULL,anno=NULL,file=NULL,species=NULL,type=c("RNA","ATAC"),norm=c("SCT","LogNorm","TFIDF")) {
  if (is.null(anno)) {
    stop("Please specify one or more annotations")
  }
  if (is.null(file)) {
    stop("Please specify the file to save reference to")
  }
  type <- match.arg(type)
  print("Reading in data")
  if (!is.null(mona_dir)) {
    exp <- open_matrix_dir(file.path(mona_dir,"exp"))
    mona <- qread(file.path(mona_dir,"mona.qs"))
    meta <- mona$meta
  } else if (!is.null(counts) & !is.null(meta)) {
    exp <- as(counts,"sparseMatrix") %>% write_matrix_dir(tempfile("mat"))
  } else if (!is.null(seurat) & !is.null(assay)) {
    exp <- seurat[[assay]]$data %>% as("sparseMatrix") %>% t() %>% write_matrix_dir(tempfile("mat"))
    meta <- seurat@meta.data
  } else {
    return()
  }
  mona_ref <- list()
  for (x in anno) {
    print(paste0("Creating model for ",x))
    ref <- list()
    exp_use <- exp
    meta_use <- meta[,x,drop=F]
    meta_use$cellname <- rownames(meta_use)
    meta_filter <- !is.na(meta_use[[x]])
    if (sum(meta_filter) != nrow(meta_use)) {
      exp_use <- exp_use[meta_filter,]
      meta_use <- meta_use[meta_filter,]
    }
    meta_counts <- as.data.frame(table(meta_use[[x]]))
    meta_filter <- as.character(meta_counts$Var1[meta_counts$Freq < 20])
    if (length(meta_filter) != nrow(meta_counts)) {
      print("The following groups will be excluded:")
      print(meta_filter)
      subset <- which(!(meta_use[[x]] %in% meta_filter))
      exp_use <- exp_use[subset,]
      meta_use <- meta_use[subset,]
    }
    meta_use <- meta_use %>% group_by(across(all_of(x))) %>% slice_sample(n=5000) %>% ungroup()
    meta_use <- as.data.frame(meta_use)
    rownames(meta_use) <- meta_use$cellname
    exp_use <- exp_use[meta_use$cellname,]
    print(paste0("Using ", nrow(meta_use), " cells"))
    meta_var <- meta_use %>% group_by(across(all_of(x))) %>% slice_sample(n=1000) %>% ungroup()
    meta_var <- as.data.frame(meta_var)
    rownames(meta_var) <- meta_var$cellname
    exp_var <- exp_use[meta_var$cellname,]
    markers <- markers_mona_all(exp_var,meta_var,x,fc_only=T)
    marker_genes <- funique(markers$gene)
    stats <- matrix_stats(exp_var, col_stats="variance")
    variable_genes <- stats$col_stats["variance",] %>% sort(decreasing = T) %>% names()
    variable_genes <- variable_genes[1:min(2000, length(variable_genes))]
    variable_genes <- variable_genes[stats$col_stats["variance",variable_genes] != 0]
    variable_genes <- funique(c(variable_genes,marker_genes))
    n_genes <- length(variable_genes)
    print(paste0("Using ", n_genes, " genes"))
    exp_use <- exp_use[,variable_genes]
    exp_use <- transpose_storage_order(exp_use)
    exp_use <- t(exp_use)
    gene_means <- stats$col_stats["mean",variable_genes]
    gene_vars <- stats$col_stats["variance",variable_genes]
    exp_use <- exp_use %>% write_matrix_dir(tempfile("mat"))
    exp_use <- (exp_use - gene_means) / gene_vars
    comps <- 100
    if (n_genes < 100) comps <- round(n_genes/2)
    print("Calculating PCA...")
    svd <- irlba(exp_use, nv=comps, maxit=5000)
    train <- multiply_cols(svd$v, svd$d) %>% as.data.frame()
    print("Training model...")
    model <- glmnet(train,as.factor(meta_use[[x]]),family = "multinomial", alpha = 0, lambda = c(9:1 %o% 10^(1:-4)))
    ref$species <- species
    ref$type <- type
    ref$norm <- norm
    ref$genes <- rownames(exp_use)
    ref$center <- as.vector(gene_means)
    ref$scale <- as.vector(gene_vars)
    ref$rotation <- svd$u
    ref$embed <- train
    ref$model <- model
    mona_ref[[x]] <- ref
  }
  if (length(mona_ref) >= 1) {
    qsave(mona_ref,file=paste0(file,".qs"))
  }
}

#' Mona annotation
#' 
#' Function for transferring cell labels from a reference to the current Mona dataset
#' Users must ensure the reference and query share the same type and normalization
#' @import glmnet
#' @import babelgene
#' @import harmony
#' @import callr
#' @param mona_ref A Mona reference object
#' @param anno Which annotation within the reference to use
#' @param exp The current Mona dataset counts
#' @param species The current Mona dataset species
#' @return A list of labels
#'
mona_annotate <- function(mona_ref,anno,exp,species) {
  library(magrittr)
  library(glmnet)
  ref <- mona_ref[[anno]]
  center <- ref$center
  scale <- ref$scale
  rotation <- ref$rotation
  species_ref <- switch(ref$species,"human"="Homo sapiens","mouse"="Mus musculus","rat"="Rattus norvegicus","fruitfly"="Drosophila melanogaster","zebrafish"="Danio rerio","nematode"="Caenorhabditis elegans","pig"="Sus scrofa","frog"="Xenopus tropicalis")
  species_query <- switch(species,"human"="Homo sapiens","mouse"="Mus musculus","rat"="Rattus norvegicus","fruitfly"="Drosophila melanogaster","zebrafish"="Danio rerio","nematode"="Caenorhabditis elegans","pig"="Sus scrofa","frog"="Xenopus tropicalis")
  if (species_ref != species_query) {
    if (species_ref == "Homo sapiens") {
      orthologs <- babelgene::orthologs(ref$genes,species_query,human=T) %>% dplyr::distinct(symbol,.keep_all = T)
      gene_check <-  sapply(ref$genes, function(x) {
        index <- match(x,orthologs$human_symbol)
        if (is.na(index)) {
          FALSE
        } else {
          orthologs$symbol[index] %in% colnames(exp) 
        }
      })
      if (sum(gene_check)/length(ref$genes) < 0.5) return("Not enough shared genes!")
      order <- match(ref$genes[gene_check],orthologs$human_symbol)
      mat <- exp[,orthologs$symbol[order]]
    } else if (species_query == "Homo sapiens") {
      orthologs <- babelgene::orthologs(ref$genes,species_ref,human=F) %>% dplyr::distinct(human_symbol,.keep_all = T)
      gene_check <-  sapply(ref$genes, function(x) {
        index <- match(x,orthologs$symbol)
        if (is.na(index)) {
          FALSE
        } else {
          orthologs$human_symbol[index] %in% colnames(exp) 
        }
      })
      if (sum(gene_check)/length(ref$genes) < 0.5) return("Not enough shared genes!")
      order <- match(ref$genes[gene_check],orthologs$symbol)
      mat <- exp[,orthologs$human_symbol[order]]
    } else {
      orthologs_1 <- babelgene::orthologs(ref$genes,species_ref,human=F) %>% dplyr::distinct(human_ensembl,.keep_all = T)
      orthologs_2 <- babelgene::orthologs(orthologs_1$human_ensembl,species_query,human=T) %>% dplyr::distinct(symbol,.keep_all = T)
      gene_check <-  sapply(ref$genes, function(x) {
        index_1 <- match(x,orthologs_1$symbol)
        if (is.na(index_1)) {
          FALSE
        } else {
          index_2 <- match(orthologs_1$human_ensembl[index_1],orthologs_2$human_ensembl)
          if (is.na(index_2)) {
            FALSE
          } else{
            orthologs_2$symbol[index_2] %in% colnames(exp) 
          }
        }
      })
      if (sum(gene_check)/length(ref$genes) < 0.5) return("Not enough shared genes!")
      order_1 <- match(ref$genes[gene_check],orthologs_1$symbol)
      order_2 <- match(orthologs_1$human_ensembl[order_1],orthologs_2$human_ensembl)
      mat <- exp[,orthologs_2$symbol[order_2]]
    }
  } else {
    gene_check <- ref$genes %in% colnames(exp)
    if (sum(gene_check)/length(ref$genes) < 0.5) return("Not enough shared genes!")
    mat <- exp[,ref$genes[gene_check]]
  }
  center <- center[gene_check]
  scale <- scale[gene_check]
  rotation <- rotation[gene_check,,drop=F]
  colnames(mat) <- ref$genes[gene_check]
  mat <- t(mat)
  mat <- mat %>% write_matrix_dir(tempfile("mat"))
  mat <- (mat - center) / scale
  mat <- t(mat)
  test_embed <- mat %*% rotation %>% as.data.frame()
  rownames(test_embed) <- paste0("test",1:nrow(test_embed))
  all_embed <- ref$embed
  rownames(all_embed) <- paste0("train",1:nrow(all_embed))
  all_embed <- as.data.frame(rbind(all_embed, test_embed))
  meta <- c(rep("train",nrow(ref$embed)),rep("test",nrow(test_embed)))
  names(meta) <- rownames(all_embed)
  all_embed <- harmony::RunHarmony(all_embed,meta,reference_values="train",verbose=F,lambda=NULL,.options = harmony::harmony_options(block.size = 0.1))
  test_embed <- as.matrix(all_embed[meta == "test",,drop=F])
  predict <- stats::predict(ref$model,newx=test_embed,type="class",s=min(ref$model$lambda))
  predict <- as.character(predict)
  scores <- stats::predict(ref$model,newx=test_embed,type="response",s=min(ref$model$lambda))
  scores <- apply(scores,1,max)
  return(list(predict=predict,scores=scores))
}

