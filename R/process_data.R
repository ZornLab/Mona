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
  latent.vars.names <- c("condition", colnames(x = latent.vars))
  latent.vars <- cbind(latent.vars, group.info)
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

markers_mona_all <- function(object,metadata,recorrect_umi=T) {
    require(Seurat)
    require(MAST)
    require(BPCells)
    require(dqrng)
    require(dplyr)
    dqset.seed(123)
  genes.de <- list()
  messages <- list()
  Idents(object) <- metadata
  idents.all <- sort(x = unique(x = Idents(object)))
  for (i in 1:length(x = idents.all)) {
    message("Calculating cluster ", idents.all[i])
    genes.de[[i]] <- tryCatch(
      expr = {
        markers_mona(
          object = object,
          metadata = metadata,
          cluster = idents.all[i],
          recorrect_umi = recorrect_umi
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
markers_mona <- function(object,metadata=NULL,cluster=NULL,cells=NULL,downsample=500,recorrect_umi=T) {
  if (recorrect_umi && length(x = levels(x = object[["SCT"]])) > 1) {
    cell_attributes <- SCTResults(object = object, slot = "cell.attributes")
    observed_median_umis <- lapply(
      X = cell_attributes,
      FUN = function(x) median(x[, "umi"])
    )
    model.list <- slot(object = object, "SCTModel.list")
    median_umi.status <- lapply(X = model.list,
                                FUN = function(x) { return(tryCatch(
                                  expr = slot(object = x, name = 'median_umi'),
                                  error = function(...) {return(NULL)})
                                )})
    if (any(is.null(unlist(median_umi.status)))){
      stop("SCT assay does not contain median UMI information.",
           "Run `PrepSCTFindMarkers()` before running `FindMarkers()` or invoke `FindMarkers(recorrect_umi=FALSE)`.")
    }
    model_median_umis <- SCTResults(object = object, slot = "median_umi")
    min_median_umi <- min(unlist(x = observed_median_umis))
    if (any(unlist(model_median_umis) != min_median_umi)){
      stop("Object contains multiple models with unequal library sizes. Run `PrepSCTFindMarkers()` before running `FindMarkers()`.")
    }
  }
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
  cdr <- data.subset[["CDR"]][c(cells.1, cells.2),]
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

run_mona_qc <- function(seurat.object=NULL,species=c("human","mouse","other")) {
	require(Seurat)
	require(scuttle)
	seurat.object$percent.mt <- PercentageFeatureSet(seurat.object, pattern = "Mt-|^mt-|^MT-")
	qc_data <- FetchData(seurat.object,vars=c("nCount_RNA","nFeature_RNA","percent.mt"))
	filters <- perCellQCFilters(qc_data, sum.field = "nCount_RNA",detected.field = "nFeature_RNA",sub.fields = "percent.mt")
	print(paste0("Cells before feature cutoffs: ",ncol(seurat.object)))
	seurat.object <- subset(seurat.object, cells = colnames(seurat.object)[!filters$discard])
	print(paste0("Cells after feature cutoffs: ",ncol(seurat.object)))
	seurat.object$CDR <- scale(colSums(seurat.object[["RNA"]]$counts>0))
	if (species == "other") {
		return(seurat.object)
	} else if (species == "human") {
		s.genes <- cc.genes.updated.2019$s.genes
		g2m.genes <- cc.genes.updated.2019$g2m.genes
		seurat.object <- CellCycleScoring(seurat.object,s.features = s.genes,g2m.features = g2m.genes,assay = 'RNA',set.ident = F)
	} else if (species == "mouse") {
	   	s.genes <- c('Mcm5', 'Pcna', 'Tyms', 'Fen1', 'Mcm7', 'Mcm4', 'Rrm1', 'Ung', 'Gins2', 'Mcm6', 'Cdca7', 'Dtl', 'Prim1', 'Uhrf1', 'Cenpu', 'Hells', 'Rfc2', 'Polr1b', 'Nasp', 'Rad51ap1', 'Gmnn', 'Wdr76', 'Slbp', 'Ccne2', 'Ubr7', 'Msh2', 'Rad51', 'Rrm2', 'Cdc45', 'Cdc6', 'Exo1', 'Tipin', 'Dscc1', 'Blm', 'Casp8ap2', 'Usp1', 'Clspn', 'Pola1', 'Chaf1b', 'Mrpl36', 'E2f8')
		g2m.genes <- c('Hmgb2', 'Cdk1', 'Nusap1', 'Ube2c', 'Birc5', 'Tpx2', 'Top2a', 'Ndc80', 'Cks2', 'Nuf2', 'Cks1b', 'Mki67', 'Cenpf', 'Tacc3', 'Pimreg', 'Smc4', 'Ccnb2', 'Ckap2l', 'Ckap2', 'Aurkb', 'Bub1', 'Kif11', 'Anp32e', 'Tubb4b', 'Gtse1', 'Kif20b', 'Hjurp', 'Cdca3', 'Jpt1', 'Cdc20', 'Ttk', 'Cdc25c', 'Kif2c', 'Rangap1', 'Ncapd2', 'Dlgap5', 'Cdca2', 'Cdca8', 'Ect2', 'Kif23', 'Hmmr', 'Aurka', 'Psrc1', 'Anln', 'Lbr', 'Ckap5', 'Cenpe', 'Ctcf', 'Nek2', 'G2e3', 'Gas2l3', 'Cbx5', 'Cenpa')
		seurat.object <- CellCycleScoring(seurat.object,s.features = s.genes,g2m.features = g2m.genes,assay = 'RNA',set.ident = F)
	}
	return(seurat.object)
}

# Takes a single Seurat object
# Returns Seurat object
process_mona <- function(seurat=NULL) {
	require(Seurat)
	require(sctransform)
	require(glmGamPoi)
	seurat.object <- SCTransform(seurat, vst.flavor = "v2", variable.features.n = 5000)
	seurat.object <- RunPCA(seurat.object,npcs=30)
	seurat.object <- FindNeighbors(seurat.object, dims = 1:30)
	seurat.object <- FindClusters(seurat.object, resolution = c(0.4, 0.8, 1.2))
	seurat.object <- RunUMAP(seurat.object,dims = 1:30,n.components=2L,reduction.name="UMAP_2D",reduction.key="umap2d_")
	seurat.object <- RunUMAP(seurat.object,dims = 1:30,n.components=3L,reduction.name="UMAP_3D",reduction.key="umap3d_")
	seurat.object <- PrepSCTFindMarkers(seurat.object)
	seurat.object[["orig.ident"]] <- NULL
	seurat.object[["seurat_clusters"]] <- NULL
	seurat.object[["clusters_res_0.4"]] <- seurat.object[["SCT_snn_res.0.4"]]
	seurat.object[["SCT_snn_res.0.4"]] <- NULL
	seurat.object[["clusters_res_0.8"]] <- seurat.object[["SCT_snn_res.0.8"]]
	seurat.object[["SCT_snn_res.0.8"]] <- NULL
	seurat.object[["clusters_res_1.2"]] <- seurat.object[["SCT_snn_res.1.2"]]
	seurat.object[["SCT_snn_res.1.2"]] <- NULL
	return(seurat.object)
}

integrate_mona <- function(object_list=NULL) {
	features <- SelectIntegrationFeatures(object.list = object_list, nfeatures = 5000)
	object_list <- PrepSCTIntegration(object.list = object_list, anchor.features = features)
	anchors <- FindIntegrationAnchors(object.list = object_list, dims = 1:30, normalization.method = "SCT", anchor.features = features)
	seurat.object <- IntegrateData(anchorset = anchors, dims = 1:30, normalization.method = "SCT")
	DefaultAssay(seurat.object) <- "integrated"
	seurat.object <- RunPCA(seurat.object, verbose = TRUE, npcs=30)
	seurat.object <- FindNeighbors(seurat.object, reduction = "pca", dims = 1:30)
	seurat.object <- FindClusters(seurat.object, resolution = c(0.4, 0.8, 1.2))
	seurat.object <- RunUMAP(seurat.object,dims = 1:30,n.components=2L,reduction.name="UMAP_2D",reduction.key="umap2d_")
	seurat.object <- RunUMAP(seurat.object,dims = 1:30,n.components=3L,reduction.name="UMAP_3D",reduction.key="umap3d_")
	seurat.object <- PrepSCTFindMarkers(seurat.object)
	return(seurat.object)
}

get_mona_markers <- function(seurat.object=NULL) {
	meta_names <- colnames(seurat.object@meta.data)
  filter_1 <- sapply(meta_names, function(x) class(seurat.object[[x]][,1]) %in% c("integer","numeric"))
  filter_2 <- sapply(meta_names, function(x) length(unique(seurat.object[[x]][,1])) > 100)
  meta_names <- meta_names[!(filter_1 & filter_2)]
	markers <- lapply(meta_names,function(x) markers_mona_all(seurat.object,x,F))
	markers_final <- bind_rows(markers)
	markers_final$avg_log2FC <- signif(markers_final$avg_log2FC,3)
	markers_final$p_val_adj <- formatC(markers_final$p_val_adj, format = "e", digits = 2)
	seurat.object@misc$markers <- markers_final[,c("gene","cluster","metadata","avg_log2FC","p_val_adj")]
	return(seurat.object)
}

save_mona_dir <- function(seurat.object=NULL,dir=NULL,species="human",name=NULL,description=NULL) {
	require(qs)
	require(BPCells)
	require(Seurat)
	gene_var <- seurat.object[["SCT"]]@SCTModel.list$model1@feature.attributes$residual_variance
	names(gene_var) <- rownames(seurat.object[["SCT"]]@SCTModel.list$model1@feature.attributes)
	gene_var <- sort(gene_var, decreasing = TRUE)
	seurat.object@misc$var_100 <- names(gene_var)[1:min(100, length(gene_var))]
	seurat.object@misc$var_500 <- names(gene_var)[1:min(500, length(gene_var))]
	seurat.object@misc$var_1000 <- names(gene_var)[1:min(1000, length(gene_var))]
	reducs <- names(seurat.object@reductions)
	reducs <- reducs[!grepl("pca",reducs)]
	seurat.object <- DietSeurat(seurat.object,layers=c("data"),assays = c("SCT"),dimreducs = reducs)
	seurat.object@misc$species <- species
	seurat.object@misc$name <- name
	seurat.object@misc$description <- description
	write_matrix_dir(mat = seurat.object[["SCT"]]$data, dir = dir)
	disk.mat <- open_matrix_dir(dir = dir)
	seurat.object[["SCT"]] <- as(object = seurat.object[["SCT"]], Class = "Assay5")
	seurat.object[["SCT"]]$data <- disk.mat
	qsave(seurat.object,file=paste0(dir,"/seurat.qs"))
}