plotUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("render_plot"))
}

plotServer <- function(id,num_plots,plot_remove,cur_selection,selection_list,sets=NULL,point_size=NULL,point_transparent=NULL,data=NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      gg_color_hue <- function(n) {
        hues = seq(15, 375, length = n + 1)
        hcl(h = hues, c = 100, l = 65)[1:n]
      }
      
      plot_inputs <- "
        function(el, x){
          var id = el.getAttribute('id');
          var gd = document.getElementById(id);
          var d3 = Plotly.d3;
          Shiny.setInputValue('plot_rendered',id,{priority: 'event'})
          Plotly.update(id).then(attach);
          function attach() {
            gd.addEventListener('click', function(evt) {
              Shiny.setInputValue('plot_clicked',id,{priority: 'event'})
            });
          };
        }"
      
      #--------------------------------------
      # bs4Dash modifications
      
      custom_box <- function(..., title = NULL, footer = NULL, status = NULL,
                             solidHeader = FALSE, background = NULL, width = 6, height = NULL,
                             collapsible = TRUE, collapsed = FALSE, closable = FALSE, maximizable = FALSE, icon = NULL,
                             gradient = FALSE, boxToolSize = "sm", elevation = NULL, headerBorder = TRUE, label = NULL, dropdownMenu = NULL,
                             sidebar = NULL, id = NULL) {
        
        if (is.null(status)) solidHeader <- TRUE
        
        dropNulls <- function(x) {
          x[!vapply(x, is.null, FUN.VALUE = logical(1))]
        }
        
        setBoxStyle <- function(height, sidebar) {
          style <- NULL
          if (!is.null(height)) {
            style <- paste0("height: ", shiny::validateCssUnit(height))
          }
          style
        }
        
        setBoxClass <- function(status, solidHeader, collapsible, collapsed,
                                elevation, gradient, background, sidebar) {
          cardCl <- "card bs4Dash"
          if (!is.null(status)) {
            cardCl <- paste0(cardCl, " card-", status)
          }
          if (!solidHeader) cardCl <- paste0(cardCl, " card-outline")
          if (collapsible && collapsed) cardCl <- paste0(cardCl, " collapsed-card")
          if (!is.null(elevation)) cardCl <- paste0(cardCl, " elevation-", elevation)
          if (!is.null(background)) {
            cardCl <- paste0(cardCl, " bg-", if (gradient) "gradient-", background)
          }
          cardCl
        }
        
        props <- dropNulls(
          list(
            title = unlist(
              dropNulls(
                lapply(title, function(e) {
                  if (inherits(e, "shiny.tag.list") ||inherits(e, "shiny.tag")) {
                    as.character(e)
                  }
                })
              )
            ),
            status = status,
            solidHeader = solidHeader,
            background = background,
            width = width,
            height = height,
            collapsible = collapsible,
            closable = closable,
            maximizable = maximizable,
            gradient = gradient
          )
        )
        
        cardCl <- setBoxClass(
          status, 
          solidHeader, 
          collapsible, 
          collapsed,
          elevation, 
          gradient, 
          background, 
          sidebar
        )
        
        style <- setBoxStyle(height, sidebar)
        
        cardToolTag <- shiny::tags$div(class = "card-tools float-right")
        
        downloadTag <- shiny::tags$button(
          type = "button",
          class = "btn action-button btn-tool btn-sm shiny-bound-input",
          id = ns("download_plot"),
          shiny::icon("camera")
        )
        
        maximizableTag <- shiny::tags$button(
          type = "button",
          class = "btn btn-tool btn-sm",
          `data-card-widget` = "maximize",
          shiny::icon("up-right-and-down-left-from-center")
        )
        
        closableTag <- shiny::tags$button(
          class = "btn btn-tool btn-sm", 
          `data-card-widget` = "remove", 
          type = "button",
          shiny::icon("xmark")
        )
        
        cardToolTag <- shiny::tagAppendChildren(
          cardToolTag,
          downloadTag,
          maximizableTag,
          closableTag
        )
        
        headerTag <- shiny::tags$div(
          class = if (headerBorder) "card-header" else "card-header border-0",
          shiny::tags$h3(class = "card-title", icon, title)
        )
        headerTag <- shiny::tagAppendChild(headerTag, cardToolTag)
        
        bodyTag <- shiny::tags$div(
          class = "card-body",
          style = style,
          ...,
          sidebar[[2]]
        )
        
        cardTag <- shiny::tags$div(class = cardCl, id = id)
        cardTag <- shiny::tagAppendChildren(cardTag, headerTag, bodyTag)
        
        shiny::tags$div(
          class = if (!is.null(width)) paste0("col-sm-", width),
          cardTag,
          shiny::tags$script(
            type = "application/json",
            `data-for` = id,
            jsonlite::toJSON(
              x = props,
              auto_unbox = TRUE,
              json_verbatim = TRUE
            )
          )
        )
      }
      
      #--------------------------------------
      
      update_plot_inputs <- function() {
        if (!is.null(data$seurat)) {
          updateSelectizeInput(session, "layout", choices = data$reducs)
          updateSelectizeInput(session, "metadata", choices = c("---"="",data$meta), selected = NULL)
          updateSelectizeInput(session, "meta_violin", choices = c("None",data$meta), selected = NULL)
          updateSelectizeInput(session, "meta_heatmap", choices = c(data$meta,"Cells"), selected = NULL)
          updateSelectizeInput(session, "meta_bubble", choices = c(data$meta), selected = NULL)
          updateSelectizeInput(session, "meta_props_1", choices = c("---"="",data$meta), selected = NULL)
          updateSelectizeInput(session, "meta_props_2", choices = c("All Data",data$meta), selected = NULL)
          updateSelectizeInput(session, "gene_exp", choices = c("---"="",data$genes), selected = NULL, server = T,options=list(maxOptions=30000))
          updateSelectizeInput(session, "gene_violin", choices = c("---"="",data$genes), selected = NULL,server = T,options=list(maxOptions=30000))
        }
      }
      
      meta_change_update <- function() {
        all_meta <- data$meta
        meta_choice <- input$metadata
        updateSelectizeInput(session, "metadata", choices = c("---"="",all_meta), selected = if (meta_choice %in% all_meta) meta_choice else NULL)
        meta_choice <- input$meta_violin
        updateSelectizeInput(session, "meta_violin", choices = c("None",all_meta), selected = if (meta_choice %in% all_meta) meta_choice else NULL)
        meta_choice <- input$meta_heatmap
        updateSelectizeInput(session, "meta_heatmap", choices = c(all_meta,"Cells"), selected = if (meta_choice %in% all_meta) meta_choice else NULL)
        meta_choice <- input$meta_bubble
        updateSelectizeInput(session, "meta_bubble", choices = c(all_meta), selected = if (meta_choice %in% all_meta) meta_choice else NULL)
        meta_choice <- input$meta_props_1
        updateSelectizeInput(session, "meta_props_1", choices = c("---"="",all_meta), selected = if (meta_choice %in% all_meta) meta_choice else NULL)
        meta_choice <- input$meta_props_2
        updateSelectizeInput(session, "meta_props_2", choices = c("All Data",all_meta), selected = if (meta_choice %in% all_meta) meta_choice else NULL)
      }
      
      gene_sets <- reactiveVal()
      genes_bubble <- reactiveVal(NULL)
      genes_heatmap <- reactiveVal(NULL)
      
      update_set_names <- function() {
        names <- unname(set_names())
        if (length(set_names()) == length(gene_sets())) {
          update_set_lists()
        }
        if (class(names) == "character") {
          choice <- input$reduction_gene_set
          if (!(list(choice) %in% c("All genes","Top 100 variable","Top 500 variable","Top 1000 variable","Quality",names))) choice <- NULL 
          updateSelectizeInput(session, "reduction_gene_set",choices = c("All genes","Top 100 variable","Top 500 variable","Top 1000 variable","Quality",names),selected = choice)
          choice <- input$violin_gene_set
          if (!(list(choice) %in% c("All genes","Top 100 variable","Top 500 variable","Top 1000 variable","Quality",names))) choice <- NULL 
          updateSelectizeInput(session, "violin_gene_set",choices = c("All genes","Top 100 variable","Top 500 variable","Top 1000 variable","Quality",names),selected = choice)
          choice <- input$heatmap_gene_set
          if (!(list(choice) %in% c("---","Top 100 variable","Top 500 variable","Top 1000 variable",names))) choice <- NULL 
          updateSelectizeInput(session, "heatmap_gene_set",choices = c("---"="","Top 100 variable","Top 500 variable","Top 1000 variable",names),selected = choice)
          choice <- input$bubble_gene_set
          if (!(list(choice) %in% c("---",names))) choice <- NULL 
          updateSelectizeInput(session, "bubble_gene_set",choices = c("---"="",names),selected = choice)
        } else{
          if (length(names) == 0) {
            updateSelectizeInput(session, "reduction_gene_set",choices = c("All genes","Top 100 variable","Top 500 variable","Top 1000 variable","Quality"),selected = NULL)
            updateSelectizeInput(session, "violin_gene_set",choices = c("All genes","Top 100 variable","Top 500 variable","Top 1000 variable","Quality"),selected = NULL)
            updateSelectizeInput(session, "heatmap_gene_set",choices = c("---"="","Top 100 variable","Top 500 variable","Top 1000 variable"),selected = NULL)
            updateSelectizeInput(session, "bubble_gene_set",choices = c("---"=""),selected = NULL)
          }
        }
      }
      
      update_set_lists <- function() {
        reduction_choice <- input$reduction_gene_set
        violin_choice <- input$violin_gene_set
        bubble_choice <- input$bubble_gene_set
        heatmap_choice <- input$heatmap_gene_set
        names <- unname(set_names())
        genes <- set_lists()
        names(genes) <- names
        update_reduction <- F
        update_violin <- F
        update_bubble <- F
        update_heatmap <- F
        if (!is.null(reduction_choice) && !(reduction_choice %in% c("All genes","Top 100 variable","Top 500 variable","Top 1000 variable","Quality"))) {
          update_reduction <- !(identical(gene_sets()[[reduction_choice]],genes[[reduction_choice]]))
        }
        if (!is.null(violin_choice) && !(violin_choice %in% c("All genes","Top 100 variable","Top 500 variable","Top 1000 variable","Quality"))) {
          update_violin <- !(identical(gene_sets()[[violin_choice]],genes[[violin_choice]]))
        }
        if (!is.null(bubble_choice)) {
          update_bubble <- !(identical(gene_sets()[[bubble_choice]],genes[[bubble_choice]]))
        }
        if (!is.null(heatmap_choice) && !(heatmap_choice %in% c("Top 100 variable","Top 500 variable","Top 1000 variable"))) {
          update_heatmap <- !(identical(gene_sets()[[heatmap_choice]],genes[[heatmap_choice]]))
        }
        gene_sets(genes)
        if (update_reduction) {
          gene_choice <- input$gene_exp
          all_genes <- genes[[reduction_choice]]
          if (gene_choice %in% all_genes) {
            updateSelectizeInput(session, "gene_exp", choices = c("---"="",all_genes), selected = gene_choice, server = T,options=list(maxOptions=30000))
          } else {
            updateSelectizeInput(session, "gene_exp", choices = c("---"="",all_genes), selected = NULL, server = T,options=list(maxOptions=30000))
          }
        }
        if (update_violin) {
          gene_choice <- input$gene_violin
          all_genes <- genes[[violin_choice]]
          if (gene_choice %in% all_genes) {
            updateSelectizeInput(session, "gene_violin", choices = c("---"="",all_genes), selected = gene_choice, server = T,options=list(maxOptions=30000))
          } else {
            updateSelectizeInput(session, "gene_violin", choices = c("---"="",all_genes), selected = NULL, server = T,options=list(maxOptions=30000))
          }
        }
        if (update_bubble) {
          genes_bubble(gene_sets()[[bubble_choice]])
        }
        if (update_heatmap) {
          genes_heatmap(gene_sets()[[heatmap_choice]])
        }
      }
      
      observeEvent(input$heatmap_gene_set, {
        if (input$heatmap_gene_set == "Top 100 variable") {
          genes_heatmap(data$var_genes[[1]])
        } else if (input$heatmap_gene_set == "Top 500 variable") {
          genes_heatmap(data$var_genes[[2]])
        } else if (input$heatmap_gene_set == "Top 1000 variable") {
          genes_heatmap(data$var_genes[[3]])
        } else if (input$heatmap_gene_set != "") {
          genes_heatmap(gene_sets()[[input$heatmap_gene_set]])
        } else {
          genes_heatmap(NULL)
        }
      })
      
      observeEvent(input$bubble_gene_set, {
        if (input$bubble_gene_set != "") {
          genes_bubble(gene_sets()[[input$bubble_gene_set]])
        } else {
          genes_bubble(NULL)
        }
      })
      
      get_genes_reduction <- function() {
        if (input$reduction_gene_set == "All genes") {
          return(data$genes)
        } else if (input$reduction_gene_set == "Quality") {
          return(data$quality)
        } else if (input$reduction_gene_set == "Top 100 variable") {
          return(data$var_genes[[1]])
        } else if (input$reduction_gene_set == "Top 500 variable") {
          return(data$var_genes[[2]])
        } else if (input$reduction_gene_set == "Top 1000 variable") {
          return(data$var_genes[[3]])
        } else {
          return(gene_sets()[[input$reduction_gene_set]])
        }
      }
      
      observeEvent(input$reduction_gene_set, {
        if(input$reduction_gene_set != "") {
          updateSelectizeInput(session, "gene_exp", choices = c("---"="",get_genes_reduction()), selected = NULL, server = T,options=list(maxOptions=30000))
        }
      })
      
      get_genes_violin <- function() {
        if (input$violin_gene_set == "All genes") {
          return(data$genes)
        } else if (input$violin_gene_set == "Quality") {
          return(data$quality)
        } else if (input$violin_gene_set == "Top 100 variable") {
          return(data$var_genes[[1]])
        } else if (input$violin_gene_set == "Top 500 variable") {
          return(data$var_genes[[2]])
        } else if (input$violin_gene_set == "Top 1000 variable") {
          return(data$var_genes[[3]])
        } else {
          return(gene_sets()[[input$violin_gene_set]])
        }
      }
      
      observeEvent(input$violin_gene_set, {
        if (input$violin_gene_set != "") {
          updateSelectizeInput(session, "gene_violin", choices = c("---"="",get_genes_violin()), selected = NULL, server = T,options=list(maxOptions=30000))
        }
      })
      
      observeEvent(input$box$visible, {
        if (!input$box$visible) {
          reset_select()
          removeUI(paste0("#",id,"-render_plot"))
          num_plots(num_plots() - 1)
          plot_remove(id)
        }
      })
      
      output$render_plot <- renderUI({
        ns <- session$ns
        onNextInput({
          num_plots(num_plots() + 1)
          update_plot_inputs()
          update_set_names()
        })
        custom_box(
          width=NULL,
          headerBorder = F,
          height = "80vh",
          background = 'teal',
          collapsible = F,
          maximizable = T,
          closable = T,
          id = ns("box"),
          title = dropdown(
            status = "primary",
            style = "simple",
            icon = icon("gear"),
            size="md",
            inputId = ns("plot_drop"),
            conditionalPanel(
              condition = "output.plot_type == 'reduction'",
              bs4Dash::tabsetPanel(
                id = ns("data_type"),
                type="pills",
                tabPanel(
                  title = "Metadata",
                  selectizeInput(ns("metadata"),
                                 label = "Color cells by",
                                 choices = NULL,
                                 selected = NULL
                  ),
                  strong("Show labels"),
                  materialSwitch(ns("labels"),"",value=F,status="primary")
                ),
                tabPanel(
                  title = "Gene",
                  selectizeInput(ns("reduction_gene_set"),
                                 label = "Set",
                                 choices=NULL,
                                 selected=NULL
                  ), 
                  selectizeInput(ns("gene_exp"), 
                                 label = "View expression/value of",
                                 choices = NULL,
                                 selected = NULL
                  ),
                  strong("Density mode"),
                  materialSwitch(ns("density"),"",value=F,status="primary")
                  #HTML("<i style='display:inline-block' id='density_warning' class='fa fa-triangle-exclamation'></i>")
                )
              ),
              selectizeInput(ns("layout"),
                             label="Layout",
                             choices = NULL,
                             selected = NULL,
                             width= "150px"
              ),
              ns = ns
            ),
            conditionalPanel(
              condition = "output.plot_type == 'heatmap'",
              selectizeInput(ns("heatmap_gene_set"),
                             label = "Set",
                             choices=NULL,
                             selected=NULL
              ), 
              selectizeInput(ns("meta_heatmap"), 
                             label = "Compare across",
                             choices = NULL,
                             selected = NULL
              ),
              ns = ns
            ),
            conditionalPanel(
              condition = "output.plot_type == 'bubble'",
              selectizeInput(ns("bubble_gene_set"),
                             label = "Set",
                             choices=NULL,
                             selected=NULL
              ), 
              selectizeInput(ns("meta_bubble"), 
                             label = "Compare across",
                             choices = NULL,
                             selected = NULL
              ),
              ns = ns
            ),
            conditionalPanel(
              condition = "output.plot_type == 'violin'",
              selectizeInput(ns("violin_gene_set"),
                             label = "Set",
                             choices=NULL,
                             selected=NULL
              ), 
              selectizeInput(ns("gene_violin"), 
                             label = "View expression/value of",
                             choices = NULL,
                             selected = NULL
              ),
              selectizeInput(ns("meta_violin"), 
                             label = "Group by",
                             choices = NULL,
                             selected = NULL
              ),
              ns = ns
            ),
            conditionalPanel(
              condition = "output.plot_type == 'props'",
              selectizeInput(ns("meta_props_1"), 
                             label = "View proportions of",
                             choices = NULL,
                             selected = NULL
              ),
              selectizeInput(ns("meta_props_2"), 
                             label = "Across",
                             choices = NULL,
                             selected = NULL
              ),
              ns = ns
            ),
            fluidRow(
              tags$button(
                id = ns("reduction"),
                type="button", 
                class="btn action-button btn-large",
                img(src = "images/reduction.png", height = 45, width = 45),
                style="padding-left: 5px;"
              ),
              tags$button(
                id = ns("heatmap"),
                type="button", 
                class="btn action-button btn-large",
                img(src = "images/heatmap_off.png", height = 45, width = 45),
                style="padding-left: 4px;"
              ),
              tags$button(
                id = ns("bubble"),
                type="button", 
                class="btn action-button btn-large",
                img(src = "images/bubble_off.png", height = 45, width = 45),
                style="padding-left: 4px;"
              ),
              tags$button(
                id = ns("violin"),
                type="button", 
                class="btn action-button btn-large",
                img(src = "images/violin_off.png", height = 45, width = 45),
                style="padding-left: 4px;"
              ),
              tags$button(
                id = ns("props"),
                type="button", 
                class="btn action-button btn-large",
                img(src = "images/props_off.png", height = 45, width = 45),
                style="padding-left: 4px; padding-right: 5px;"
              )
            )
          ),
          tags$head(tags$style(
            paste0("#",ns('plot_drop')," {background-color: #88abff; color: #FFF;}"),
            paste0("#",ns('box')," .card-header {height: 50px;}"),
            paste0("#",ns('box')," .form-group {margin-top: 8px;}")
          )),
          withSpinner(plotlyOutput(ns("plot"),width = "auto"),type=5)
        )
      })
      
      plot_name <- paste0(id,"-plot")
      plot_type <- reactiveVal('reduction')
      output$plot_type <- renderText({
        plot_type()
      })
      outputOptions(output, "plot_type", suspendWhenHidden=FALSE)
      
      meta_cells <- reactive({
        req(data$seurat)
        req(input$metadata)
        event_data("plotly_selected",source=ns("meta_plot"))
      })
      meta_clear_cells <- reactive({
        req(data$seurat)
        req(input$metadata)
        event_data("plotly_deselect",source=ns("meta_plot"))
      })
      
      observeEvent(meta_clear_cells(), {
        reset_select()
      })
      
      observeEvent(meta_cells(), {
        cells <- meta_cells()$key
        if (length(cells) > 0) {
          selection_list$selects[[plot_name]] <- cells
          cur_selection$plot <- plot_name
          cur_selection$cells <- cells
        }
      },ignoreInit = T)
      
      exp_cells <- reactive({
        req(data$seurat)
        req(input$gene_exp)
        event_data("plotly_selected",source=ns("exp_plot"))
      })
      exp_clear_cells <- reactive({
        req(data$seurat)
        req(input$gene_exp)
        event_data("plotly_deselect",source=ns("exp_plot"))
      })
      
      observeEvent(exp_cells(), {
        cells <- exp_cells()$key
        if (length(cells) > 0) {
          selection_list$selects[[plot_name]] <- cells
          cur_selection$plot <- plot_name
          cur_selection$cells <- cells
        }
      },ignoreInit = T)
      
      observeEvent(exp_clear_cells(), {
        reset_select()
      })
      
      reset_select <- function() {
        select_name <- paste0("plotly_selected","-",id,"-meta_plot")
        session$userData$plotlyInputStore[[select_name]] <- NULL
        select_name <- paste0("plotly_selected","-",id,"-exp_plot")
        session$userData$plotlyInputStore[[select_name]] <- NULL
        selection_list$selects[[plot_name]] <- NULL
        if (cur_selection$plot == plot_name) {
          cur_selection$plot <- "plot0-plot"
          cur_selection$cells <- NULL
        }
      }
      
      session$userData[[paste0("meta_",id,"_obs")]] <- observeEvent(data$meta, {
        meta_change_update()
      },ignoreInit = T)
      
      session$userData[[paste0("reducs_",id,"_obs")]] <- observeEvent(data$reducs, {
        updateSelectizeInput(session, "layout", choices = data$reducs)
      })
      
      session$userData[[paste0("genes_",id,"_obs")]] <- observeEvent(data$genes, {
        updateSelectizeInput(session, "gene_exp", choices = c("---"="",data$genes), selected = NULL, server = T,options=list(maxOptions=30000))
        updateSelectizeInput(session, "gene_violin", choices = c("---"="",data$genes), selected = NULL,server = T,options=list(maxOptions=30000))
      })
      
      set_names <- reactive(sapply(sets$sets,function(x) x$name()))
      set_lists <- reactive(lapply(sets$sets,function(x) x$genes()))
      
      observeEvent(set_names(), {
        update_set_names()
      })
      
      observeEvent(set_lists(), {
        update_set_lists()
      })
      
      #--------------------------------------------------
      #Plotting functions
      
      plot_reduction <- function(seurat,data_type,layout,meta_select,genes_select,labels,point_size,point_transparent,density) {
        validate(
          need(seurat,""),
          need(layout,""),
          need(data_type,"")
        )
        reduc_key <- seurat@reductions[[layout]]@key
        dims <- if(grepl("3",reduc_key)) 3 else 2
        reduct_names <- c(paste0(reduc_key,c(1:dims)))
        if (data_type == "Metadata") {
          validate(
            need(meta_select,"")
          )
          plot_data <- Seurat::FetchData(seurat,vars=c(reduct_names,meta_select))
          colnames(plot_data) <- c(paste0("dim",c(1:dims)),meta_select)
          label_info <- NULL
          if (labels) {
            label_info <- get_cluster_labels(plot_data,dims)
          }
          plot_data$cellname <- rownames(plot_data)
          return(create_meta_plot(plot_data,dims,label_info,point_size,point_transparent))
        } else if (data_type == "Gene") {
          validate(
            need(genes_select,"")
          )
          plot_data <- Seurat::FetchData(seurat,vars=c(reduct_names,genes_select))
          colnames(plot_data) <- c(paste0("dim",c(1:dims)),genes_select)
          plot_data$cellname <- rownames(plot_data)
          legend <- if (input$reduction_gene_set != "Quality") "Expression" else "Value"
          return(create_exp_plot(plot_data,dims,point_size,point_transparent,density,legend))
        }
      }
      
      create_meta_plot <- function(plot_data,dims,label_info,point_size,point_transparent) {
        meta <- plot_data[,dims+1]
        name <- colnames(plot_data)[dims+1]
        groups <- gtools::mixedsort(unique(meta))
        plot_data$color <- factor(as.character(meta),levels=groups)
        color_pal <- gg_color_hue(length(groups))
        names(color_pal) <- groups
        if (dims == 2) {
          meta_plot <- plot_ly(plot_data, x = ~dim1, y = ~dim2, customdata = ~color, color = ~color, colors = color_pal, opacity = point_transparent(),marker=list(size=point_size()), text = rownames(plot_data), hovertemplate="%{customdata}<extra></extra>", type = 'scattergl', mode = 'markers', source = ns('meta_plot'), key = ~cellname) %>% 
            plotly::config(doubleClickDelay = 400,displaylogo = FALSE,modeBarButtonsToAdd = list('drawopenpath','eraseshape'), modeBarButtonsToRemove = list('hoverClosestCartesian','hoverCompareCartesian','toImage')) %>%
            plotly::layout(title = list(text=name,y=0.98,font = list(size = 20)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",hoverdistance=5, margin=list(t=40,b=10,l=20,r=60),legend=list(font = list(size = 14),itemsizing='constant',entrywidth = 0,bgcolor="rgba(0, 0, 0, 0)"),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
            onRender(plot_inputs)
          if (!is.null(label_info)) {
            label_size <- median(nchar(as.vector(label_info$label)))
            font_size <- 18
            if (label_size > 3) {
              font_size <- 14
            }
            meta_plot <- meta_plot %>% add_annotations(x=label_info$x, y=label_info$y, text=label_info$label, xref="x", yref="y", showarrow=F, opacity=0.8, font=list(size=font_size))
          }
          meta_plot
        } else {
          meta_plot <- plot_ly(plot_data, x = ~dim1, y = ~dim2, z = ~dim3, customdata = ~color, color = ~color, colors = color_pal, opacity = point_transparent(), marker=list(size=point_size()), text = rownames(plot_data), hovertemplate="%{customdata}<extra></extra>", type = 'scatter3d', mode = 'markers', key = ~cellname) %>% 
            plotly::config(doubleClickDelay = 400,displaylogo = FALSE,modeBarButtonsToAdd = list('drawopenpath','eraseshape'), modeBarButtonsToRemove = list('hoverClosest3d','toImage')) %>%
            plotly::layout(title = list(text=name,y=0.98,font = list(size = 20)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",hoverdistance=5,margin=list(t=40,b=10,l=20,r=60),legend=list(font = list(size = 14),itemsizing='constant',entrywidth = 0,bgcolor="rgba(0, 0, 0, 0)"),scene=list(xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)"))
          if (!is.null(label_info)) {
            label_size <- median(nchar(as.vector(label_info$label)))
            font_size <- 18
            if (label_size > 3) {
              font_size <- 14
            }
            anno_list <- list()
            for(i in 1:nrow(label_info)){
              tmp <- list(x=label_info$x[i],y=label_info$y[i],z=label_info$z[i],text=label_info$label[i],showarrow=F, opacity=0.8, font=list(size=font_size))
              anno_list[[i]] <- tmp
            }
            meta_plot <- meta_plot %>% plotly::layout(scene=list(annotations=anno_list))
          }
          meta_plot
        } 
      }
      
      create_exp_plot <- function(plot_data,dims,point_size,point_transparent,density,legend) {
        name <- colnames(plot_data)[dims+1]
        plot_data <- plot_data %>% arrange(.data[[name]])
        if (density) {
          embeds <- plot_data[,1:dims]
          weights <- plot_data[,dims+1]
          dens <- ks::kde(embeds,w = weights / sum(weights) * length(weights))
          if (dims == 2) {
            ix <- findInterval(embeds[, 1], dens$eval.points[[1]])
            iy <- findInterval(embeds[, 2], dens$eval.points[[2]])
            plot_data[,dims+1] <- dens$estimate[cbind(ix, iy)]
          } else if (dims == 3) {
            ix <- findInterval(embeds[, 1], dens$eval.points[[1]])
            iy <- findInterval(embeds[, 2], dens$eval.points[[2]])
            iz <- findInterval(embeds[, 3], dens$eval.points[[3]])
            plot_data[,dims+1] <- dens$estimate[cbind(ix, iy, iz)]
          }
        }
        if (legend == "Expression") {
          Expression <- plot_data[,dims+1]
          if (dims == 2) {
            plot_ly(plot_data, x = ~dim1, y = ~dim2, customdata = ~Expression, color = ~Expression, colors="viridis",opacity = point_transparent(), marker=list(size=point_size()), text = rownames(plot_data), hovertemplate="%{customdata:.2f}<extra></extra>", type = 'scattergl', mode = 'markers', source = ns('exp_plot'), key = ~cellname) %>% 
              plotly::config(doubleClickDelay = 400,displaylogo = FALSE,modeBarButtonsToAdd = list('drawopenpath','eraseshape'), modeBarButtonsToRemove = list('hoverClosestCartesian','hoverCompareCartesian','toImage')) %>%
              plotly::layout(title = list(text=name,y=0.98,font = list(size = 20)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",hoverdistance=5,margin=list(t=40,b=10,l=20,r=30),legend=list(font = list(size = 14),entrywidth = 0,bgcolor="rgba(0, 0, 0, 0)"),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
              onRender(plot_inputs)
          } else {
            plot_ly(plot_data, x = ~dim1, y = ~dim2, z = ~dim3, customdata = ~Expression, color = ~Expression, colors="viridis",opacity = point_transparent(), marker=list(size=point_size()), text = rownames(plot_data), hovertemplate="%{customdata:.2f}<extra></extra>", type = 'scatter3d', mode = 'markers', key = ~cellname) %>% 
              plotly::config(doubleClickDelay = 400,displaylogo = FALSE,modeBarButtonsToAdd = list('drawopenpath','eraseshape'), modeBarButtonsToRemove = list('hoverClosest3d','toImage')) %>%
              plotly::layout(title = list(text=name,y=0.98,font = list(size = 20)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",hoverdistance=5,margin=list(t=40,b=10,l=20,r=30),legend=list(font = list(size = 14),entrywidth = 0,bgcolor="rgba(0, 0, 0, 0)"),scene=list(xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)"))
          }
        } else {
          Value <- plot_data[,dims+1]
          if (dims == 2) {
            plot_ly(plot_data, x = ~dim1, y = ~dim2, customdata = ~Value, color = ~Value, colors="viridis",opacity = point_transparent(), marker=list(size=point_size()), text = rownames(plot_data), hovertemplate="%{customdata:.2f}<extra></extra>", type = 'scattergl', mode = 'markers', source = ns('exp_plot'), key = ~cellname) %>% 
              plotly::config(doubleClickDelay = 400,displaylogo = FALSE,modeBarButtonsToAdd = list('drawopenpath','eraseshape'), modeBarButtonsToRemove = list('hoverClosestCartesian','hoverCompareCartesian','toImage')) %>%
              plotly::layout(title = list(text=name,y=0.98,font = list(size = 20)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",hoverdistance=5,margin=list(t=40,b=10,l=20,r=30),legend=list(font = list(size = 14),entrywidth = 0,bgcolor="rgba(0, 0, 0, 0)"),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
              onRender(plot_inputs)
          } else {
            plot_ly(plot_data, x = ~dim1, y = ~dim2, z = ~dim3, customdata = ~Value, color = ~Value, colors="viridis",opacity = point_transparent(), marker=list(size=point_size()), text = rownames(plot_data), hovertemplate="%{customdata:.2f}<extra></extra>", type = 'scatter3d', mode = 'markers', key = ~cellname) %>% 
              plotly::config(doubleClickDelay = 400,displaylogo = FALSE,modeBarButtonsToAdd = list('drawopenpath','eraseshape'), modeBarButtonsToRemove = list('hoverClosest3d','toImage')) %>%
              plotly::layout(title = list(text=name,y=0.98,font = list(size = 20)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",hoverdistance=5,margin=list(t=40,b=10,l=20,r=30),legend=list(font = list(size = 14),entrywidth = 0,bgcolor="rgba(0, 0, 0, 0)"),scene=list(xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)"))
          }
        }
      }
      
      get_cluster_labels <- function(plot_data,dims) {
        header <- colnames(plot_data)
        coords <- plot_data %>% group_by(pick(dims+1)) %>% summarise(x=median(get(header[1])),y=median(get(header[2])))
        coords <- coords[,c(2,3,1)]
        colnames(coords) <- c("x","y","label")
        repels <- repel_text(coords)
        if (dims == 3) {
          dim3 <- plot_data %>% group_by(pick(dims+1)) %>% summarise(z=median(get(header[3])))
          repels$z <- dim3$z
        }
        return(repels)
      }
      
      plot_violin <- function(seurat,genes_select,meta_select,gene_set) {
        validate(
          need(seurat,""),
          need(genes_select,"")
        )
        y_name <- if (gene_set == "Quality") "Value" else "Expression"
        if (meta_select == "None") {
          plot_data <- Seurat::FetchData(seurat,vars=c(genes_select))
          colnames(plot_data) <- c("gene")
          plot_data$cellname <- rownames(plot_data)
          plot_ly(plot_data, type = "violin", y = ~gene, box=list(visible=T),meanline=list(visible=T),source = ns('plot'), key= ~cellname) %>%
            plotly::layout(title=list(text=genes_select,y=0.98,font = list(size = 20)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=40,b=10,l=90,r=20),yaxis=list(title=y_name,zeroline=F),xaxis=list(showticklabels = F),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
            plotly::config(doubleClickDelay = 400,displaylogo = FALSE,modeBarButtonsToAdd = list('drawopenpath','eraseshape'),modeBarButtonsToRemove = list('hoverClosestCartesian','hoverCompareCartesian','toImage'))
        } else {
          plot_data <- Seurat::FetchData(seurat,vars=c(genes_select,meta_select))
          colnames(plot_data) <- c("gene","meta")
          groups <- as.vector(unique(plot_data$meta))
          groups_sorted <- gtools::mixedsort(groups)
          color_pal <- gg_color_hue(length(groups_sorted))
          names(color_pal) <- groups_sorted
          plot_data$cellname <- rownames(plot_data)
          plot_ly(plot_data, type = "violin", x = ~meta, y = ~gene, split = ~meta, color= ~meta, colors=color_pal, box=list(visible=T),meanline=list(visible=T),source = ns('plot'), key= ~cellname) %>%
            plotly::layout(title=list(text=genes_select,y=0.98,font = list(size = 20)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=40,b=10,l=90,r=60),legend=list(font = list(size = 14),entrywidth = 0,bgcolor="rgba(0, 0, 0, 0)"),yaxis=list(title=y_name,zeroline=F),xaxis=list(title=meta_select),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
            plotly::config(doubleClickDelay = 400,displaylogo = FALSE,modeBarButtonsToAdd = list('drawopenpath','eraseshape'),modeBarButtonsToRemove = list('hoverClosestCartesian','hoverCompareCartesian','toImage'))
        }
      }
      
      plot_heatmap <- function(seurat,geneset,meta_select) {
        validate(
          need(seurat,""),
          need(geneset,""),
          need(meta_select,"")
        )
        show_genes <- T
        if (length(geneset) > 50) {
          show_genes <- F
        }
        if (meta_select == "Cells") {
          plot_data <- Seurat::FetchData(seurat,vars=c(geneset))
          plot_data <- as.matrix(plot_data)
          x_order <- hclust(dist(plot_data))$order
          y_order <- hclust(dist(t(plot_data)))$order
          plot_data <- plot_data[x_order,y_order]
          plot_ly(x=colnames(plot_data),y=rownames(plot_data),z=plot_data,colors="viridis",type="heatmap") %>% 
            plotly::layout(title=list(text="",y=0.98,font = list(size = 20)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=30,b=10,l=80,r=60),yaxis=list(title="Cells",showticklabels=F,autotypenumbers = 'strict'),xaxis=list(title="Genes",showticklabels=show_genes),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
            plotly::config(doubleClickDelay = 400,displaylogo = FALSE,modeBarButtonsToAdd = list('drawopenpath','eraseshape'),modeBarButtonsToRemove = list('hoverClosestCartesian','hoverCompareCartesian','toImage'))
        } else {
          plot_data <- Seurat::FetchData(seurat,vars=c(geneset,meta_select))
          plot_data <- plot_data %>% arrange(get(meta_select))
          plot_means <- plot_data %>% group_by(get(meta_select)) %>% summarise(across(all_of(geneset),get_avg_exp)) %>% data.frame()
          rownames(plot_means) <- plot_means[,1]
          plot_means <- as.matrix(plot_means[,2:ncol(plot_means)])
          x_order <- hclust(dist(plot_means))$order
          y_order <- hclust(dist(t(plot_means)))$order
          plot_means <- plot_means[x_order,y_order]
          plot_ly(x=colnames(plot_means),y=rownames(plot_means),z=plot_means,colors="viridis",type="heatmap") %>% 
            plotly::layout(title=list(text="",y=0.98,font = list(size = 20)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=30,b=10,l=80,r=60),yaxis=list(title=meta_select,autotypenumbers = 'strict'),xaxis=list(title="Genes",showticklabels=show_genes),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
            plotly::config(doubleClickDelay = 400,displaylogo = FALSE,modeBarButtonsToAdd = list('drawopenpath','eraseshape'),modeBarButtonsToRemove = list('hoverClosestCartesian','hoverCompareCartesian','toImage'))
        }
      }
      
      get_avg_exp <- function(data) {
        return(mean(expm1(data)))
      }
      
      get_percent_exp <- function(data) {
        return(length(data[data > 0]) / length(data))
      }
      
      plot_bubble <- function(seurat,geneset,meta_select) {
        validate(
          need(seurat,""),
          need(geneset,""),
          need(meta_select,"")
        )
        plot_data_raw <- Seurat::FetchData(seurat,vars=c(geneset,meta_select))
        plot_means <- plot_data_raw %>% group_by(get(meta_select)) %>% summarise(across(all_of(geneset),get_avg_exp)) %>% pivot_longer(cols=all_of(geneset), names_to="Gene")
        plot_percents <- plot_data_raw %>% group_by(get(meta_select)) %>% summarise(across(all_of(geneset),get_percent_exp)) %>% pivot_longer(cols=all_of(geneset), names_to="Gene")
        plot_data <- cbind(plot_means,plot_percents$value)
        colnames(plot_data) <- c("Meta","Gene","Expression","Percent")
        plot_data$Meta <- as.character(plot_data$Meta)
        plot_data$Color <- scales::squish(plot_data$Expression,range = c(0,3))
        plot_data$Percent <- plot_data$Percent*100
        plot_data$Size <- plot_data$Percent
        plot_data$Size[plot_data$Percent < 1.0] <- NA
        plot_ly(plot_data,x=~Gene,y=~Meta,color=~Color,colors="viridis",size=~Size,sizes=c(5,20),type="scatter",mode = "markers",marker = list(sizemode = "diameter"),hoverinfo = 'text', hovertext = paste0(round(plot_data$Expression,2),"\n",round(plot_data$Percent,2),"%")) %>%
          plotly::layout(title=list(text="",y=0.98,font = list(size = 20)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=30,b=10,l=80,r=60),xaxis=list(showgrid=F,zeroline=T,autotypenumbers = 'strict'),yaxis=list(showgrid=F,zeroline=T,autotypenumbers = 'strict',categoryorder="array",categoryarray=gtools::mixedsort(plot_data$Meta)),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
          plotly::config(doubleClickDelay = 400,displaylogo = FALSE,modeBarButtonsToAdd = list('drawopenpath','eraseshape'),modeBarButtonsToRemove = list('hoverClosestCartesian','hoverCompareCartesian','toImage'))
      }
      
      plot_props <- function(seurat,meta_1,meta_2) {
        validate(
          need(seurat,""),
          need(meta_1,""),
          need(meta_2,"")
        )
        if (meta_2 == "All Data") {
          counts <- table(seurat[[meta_1]])
          props <- counts/ncol(seurat)
          props <- data.frame(props)
          counts <- data.frame(counts)
          groups <- as.vector(props[,1])
          groups_sorted <- gtools::mixedsort(groups)
          props$color <- factor(as.character(props[,1]),levels=groups_sorted)
          color_pal <- gg_color_hue(length(groups_sorted))
          names(color_pal) <- groups_sorted
          props$Freq <- props$Freq*100
          props$Count <- counts$Freq
          plot_ly(props, x= ~Freq, color= ~color, colors=color_pal, type= "bar", orientation="h", hoverinfo = 'text', hovertext = paste0(props$color,"\n",round(props$Freq,1),"%\n", props$Count," cells")) %>%
            plotly::layout(title=list(text=meta_1,y=0.98,font=list(size=20)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=40,b=10,l=80,r=20),legend=list(font = list(size = 14)),barmode= "stack",yaxis=list(title="",zeroline=F,visible=F),xaxis=list(title="",zeroline=F,visible=F),showlegend = T,modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
            plotly::config(doubleClickDelay = 400,displaylogo = FALSE,modeBarButtonsToRemove = list('hoverClosestCartesian','hoverCompareCartesian','toImage'))
        } else {
          counts <- table(seurat[[meta_1]][,1],seurat[[meta_2]][,1])
          props <- prop.table(counts,margin=2)
          props <- data.frame(props)
          counts <- data.frame(counts)
          props$Freq <- props$Freq * 100
          props$Count <- counts$Freq
          order <- gtools::mixedsort(levels(as.factor(props$Var1)))
          props$Var1 <- factor(props$Var1,levels=order)
          order <- gtools::mixedsort(levels(as.factor(props$Var2)))
          props$Var2 <- factor(props$Var2,levels=order)
          groups <- as.vector(unique(props$Var1))
          groups_sorted <- gtools::mixedsort(groups)
          color_pal <- gg_color_hue(length(groups_sorted))
          names(color_pal) <- groups_sorted
          plot_ly(props, x= ~Var2, y= ~Freq, color= ~Var1, colors=color_pal, type= "bar", hoverinfo = 'text', hovertext = paste0(props$Var1,"\n",round(props$Freq,1),"%\n", props$Count," cells")) %>%
            plotly::layout(title=list(text="",y=0.98,font=list(size=20)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",barmode= "stack",margin=list(t=20,b=10,l=100,r=50),legend=list(font = list(size = 14),entrywidth = 0,bgcolor="rgba(0, 0, 0, 0)"),yaxis=list(title=list(text=meta_1,font=list(size=18))),xaxis=list(title=list(text=meta_2,font=list(size=18))),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)"),showlegend = T) %>%
            plotly::config(doubleClickDelay = 400,displaylogo = FALSE,modeBarButtonsToRemove = list('hoverClosestCartesian','hoverCompareCartesian','toImage'))
        }
      }
      
      observeEvent(plot_type(), {
        if (plot_type() == 'reduction') {
          output$plot <- renderPlotly({plot_reduction(data$use,input$data_type,input$layout,input$metadata,input$gene_exp,input$labels,point_size,point_transparent,input$density)})
        }
        else if (plot_type() == 'violin') {
          output$plot <- renderPlotly({plot_violin(data$use,input$gene_violin,input$meta_violin,input$violin_gene_set)})
        }
        else if (plot_type() == 'heatmap') {
          output$plot <- renderPlotly({plot_heatmap(data$use,genes_heatmap(),input$meta_heatmap)})
        }
        else if (plot_type() == 'bubble') {
          output$plot <- renderPlotly({plot_bubble(data$use,genes_bubble(),input$meta_bubble)})
        }
        else if (plot_type() == 'props') {
          output$plot <- renderPlotly({plot_props(data$use,input$meta_props_1,input$meta_props_2)})
        }
      })
      
      observeEvent(input$reduction, {
        if (plot_type() != "reduction") {
          shinyjs::runjs(paste0("$('#",ns(plot_type())," > img').attr('src', 'images/",plot_type(),"_off.png');"," $('#",ns('reduction')," > img').attr('src', 'images/reduction.png');"))
          plot_type('reduction')
        }
      })
      observeEvent(input$heatmap, {
        if (plot_type() != "heatmap") {
          shinyjs::runjs(paste0("$('#",ns(plot_type())," > img').attr('src', 'images/",plot_type(),"_off.png');"," $('#",ns('heatmap')," > img').attr('src', 'images/heatmap.png');"))
          plot_type('heatmap')
        }
      })
      observeEvent(input$bubble, {
        if (plot_type() != "bubble") {
          shinyjs::runjs(paste0("$('#",ns(plot_type())," > img').attr('src', 'images/",plot_type(),"_off.png');"," $('#",ns('bubble')," > img').attr('src', 'images/bubble.png');"))
          plot_type('bubble')
        }
      })
      observeEvent(input$violin, {
        if (plot_type() != "violin") {
          shinyjs::runjs(paste0("$('#",ns(plot_type())," > img').attr('src', 'images/",plot_type(),"_off.png');"," $('#",ns('violin')," > img').attr('src', 'images/violin.png');"))
          plot_type('violin')
        }
      })
      observeEvent(input$props, {
        if (plot_type() != "props") {
          shinyjs::runjs(paste0("$('#",ns(plot_type())," > img').attr('src', 'images/",plot_type(),"_off.png');"," $('#",ns('props')," > img').attr('src', 'images/props.png');"))
          plot_type('props')
        }
      })
      observeEvent(input$box$maximized, {
        plot_height <- if (input$box$maximized) {
          "88vh"
        } else {
          "76vh"
        }
        js_call <- paste0("setTimeout(() => {$('#",ns('plot'),"').css('height','",plot_height,"');}, 200);", " $('#",ns('plot'),"').trigger('resize');")
        shinyjs::runjs(js_call)
      }, ignoreInit = T)
      
      observeEvent(input$download_plot, {
        showModal(modalDialog(
          title = "Export plot",
          easyClose = T,
          size="s",
          textInput(ns("export_name"),label="Name",value="Plot"),
          selectizeInput(ns("export_format"),label="Format",choices=c("png","svg","jpeg","webp")),
          textInput(ns("export_width"),label="Width",value = 1200),
          textInput(ns("export_height"),label="Height", value = 900),
          actionButton(ns("download_confirm"), "Export"),
          footer = NULL
        ))
      })
      
      export_settings <- reactiveValues(format="png",name="Plot",width=1200,height=900)
      
      observeEvent(input$export_format, {
        export_settings$format <- input$export_format
      })
      
      observeEvent(input$export_name, {
        export_settings$name <- input$export_name
      })
      
      observeEvent(input$export_width, {
        export_settings$width <- input$export_width
      })
      
      observeEvent(input$export_height, {
        export_settings$height <- input$export_height
      })
      
      observeEvent(input$download_confirm, {
        removeModal(session)
        runjs(paste0("Plotly.downloadImage('",ns('plot'),"', {format: '",export_settings$format,"', width: ",export_settings$width,", height: ",export_settings$height,", filename: '",export_settings$name,"'});"))
      })
    }
  )
}