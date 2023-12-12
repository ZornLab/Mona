plotUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("render_plot"))
}

plotServer <- function(id,num_plots,plot_remove,cur_selection,selection_list,sets=NULL,plot_settings=NULL,data=NULL,markers=NULL,degs=NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      gg_color_hue <- function(n) {
        hues = seq(15, 375, length = n + 1)
        hcl(h = hues, c = 100, l = 65)[1:n]
      }
      
      random_colors <- function(n) {
        pal <- c("#0000FF","#FF0000","#00FF00","#000033","#FF00B6","#005300","#FFD300","#009FFF","#9A4D42","#00FFBE","#783FC1","#1F9698","#FFACFD","#B1CC71","#F1085C","#FE8F42","#DD00FF","#201A01","#720055","#766C95","#02AD24","#C8FF00","#886C00","#FFB79F","#858567","#A10300","#14F9FF","#00479E","#DC5E93","#93D4FF","#004CFF","#F2F318")
        if (n <= 32) {
          return(pal[1:n])
        } else {
          return(pal)
        }
      }
      
      plot_inputs <- paste0("
        function(el, x){
          var id = el.getAttribute('id');
          var gd = document.getElementById(id);
          var d3 = Plotly.d3;
          Shiny.setInputValue('",ns("plot_rendered"),"',id,{priority: 'event'})
          Plotly.update(id).then(attach);
          function attach() {
            if (!gd.hasAttribute('click_listen')) {
              gd.addEventListener('click', function(evt) {
                Shiny.setInputValue('plot_clicked',id + '@0',{priority: 'event'})
              });
              gd.setAttribute('click_listen','true');
            }
          };
        }")
      
      plot_inputs_exp <- paste0("
        function(el, x){
          var id = el.getAttribute('id');
          var gd = document.getElementById(id);
          var d3 = Plotly.d3;
          document.querySelectorAll('#' + id + ' .colorbar')[0].style.display='none';
          Shiny.setInputValue('",ns("plot_rendered"),"',id,{priority: 'event'})
          Shiny.setInputValue('",ns("show_slider"),"',id,{priority: 'event'})
          Plotly.update(id).then(attach);
          function attach() {
            if (!gd.hasAttribute('click_listen')) {
              gd.addEventListener('click', function(evt) {
                Shiny.setInputValue('plot_clicked',id + '@0',{priority: 'event'})
              });
              gd.setAttribute('click_listen','true');
            }
          };
        }")
      
      subplot_inputs <- paste0("
        function(el, x){
          var id = el.getAttribute('id');
          var gd = document.getElementById(id);
          var subplots = $(gd).find('.draglayer').children()
          Shiny.setInputValue('",ns("plot_rendered"),"',id,{priority: 'event'})
          for (var i = 1; i < subplots.length; i++) {
            (function(index) {
              subplots[index].addEventListener('click', function() {
                Shiny.setInputValue('subplot_clicked',id + '@' + index,{priority: 'event'})
              })
            })(i);
          }
        }")
      
      subplot_inputs_exp <- paste0("
        function(el, x){
          var id = el.getAttribute('id');
          var gd = document.getElementById(id);
          var subplots = $(gd).find('.draglayer').children()
          var colorbars = document.querySelectorAll('#' + id + ' .colorbar')
          if (subplots.length > 1) {
            document.querySelectorAll('#' + id + ' .legend')[0].style.display='none';
          }
          Shiny.setInputValue('",ns("show_slider"),"',id,{priority: 'event'})
          Shiny.setInputValue('",ns("plot_rendered"),"',id,{priority: 'event'})
          for (var i = 0; i < subplots.length; i++) {
            (function(index) {
              var subplot = index + 1
              subplots[index].addEventListener('click', function() {
                Shiny.setInputValue('subplot_clicked',id + '@' + subplot,{priority: 'event'})
              })
              colorbars[index].style.display='none';
            })(i);
          }
        }")
      
      plot_inputs_exp_3d <- paste0("
        function(el, x){
          var id = el.getAttribute('id');
          var colorbars = document.querySelectorAll('#' + id + ' .colorbar')
          colorbars[0].style.display='none';
          Shiny.setInputValue('",ns("show_slider_3d"),"',id,{priority: 'event'})
          Shiny.setInputValue('",ns("plot_rendered"),"',id,{priority: 'event'})
        }")
      
      subplot_inputs_exp_3d <- paste0("
        function(el, x){
          var id = el.getAttribute('id');
          var gd = document.getElementById(id);
          var subplots = $(gd).find('.gl-container').children()
          var colorbars = document.querySelectorAll('#' + id + ' .colorbar')
          if (subplots.length > 1) {
            document.querySelectorAll('#' + id + ' .legend')[0].style.display='none';
          }
          Shiny.setInputValue('",ns("show_slider_3d"),"',id,{priority: 'event'})
          Shiny.setInputValue('",ns("plot_rendered"),"',id,{priority: 'event'})
          for (var i = 0; i < subplots.length; i++) {
            (function(index) {
              colorbars[index].style.display='none';
            })(i);
          }
        }")
      
      plot_inputs_3d <- paste0("
        function(el, x){
          var id = el.getAttribute('id');
          Shiny.setInputValue('",ns("plot_rendered"),"',id,{priority: 'event'})
        }")
      
      subplot_inputs_3d <- paste0("
        function(el, x){
          var id = el.getAttribute('id');
          Shiny.setInputValue('",ns("plot_rendered"),"',id,{priority: 'event'})
        }")
      
      #subplot_inputs_3d_exp <- paste0("
      #  function(el, x){
      #    var id = el.getAttribute('id');
      #    document.querySelectorAll('#' + id + ' .legend')[0].style.display='none';
      #    Shiny.setInputValue('",ns("plot_rendered"),"',id,{priority: 'event'})
      #  }")
      
      reduct_clear_select <- list(
        name = "Clear Select",
        icon = list(
          path = "M20.377,16.519l6.567-6.566c0.962-0.963,0.962-2.539,0-3.502l-0.876-0.875c-0.963-0.964-2.539-0.964-3.501,0  L16,12.142L9.433,5.575c-0.962-0.963-2.538-0.963-3.501,0L5.056,6.45c-0.962,0.963-0.962,2.539,0,3.502l6.566,6.566l-6.566,6.567  c-0.962,0.963-0.962,2.538,0,3.501l0.876,0.876c0.963,0.963,2.539,0.963,3.501,0L16,20.896l6.567,6.566  c0.962,0.963,2.538,0.963,3.501,0l0.876-0.876c0.962-0.963,0.962-2.538,0-3.501L20.377,16.519z",
          transform = 'matrix(1 0 0 1 -3 -3) scale(0.65)'
        ),
        click = htmlwidgets::JS(
          paste0("function(gd) {
            Plotly.restyle(gd,{'selectedpoints': null});
            Shiny.setInputValue('",ns("custom_clear_select"),"',",format(Sys.time(), "%H%M%S"),",{priority: 'event'});
          }")
        )
      )
      
      reduct_select_all <- list(
        name = "Select Visible",
        icon = list(
          path = "M9,9H15V15H9M7,17H17V7H7M15,5H17V3H15M15,21H17V19H15M19,17H21V15H19M19,9H21V7H19M19,21A2,2 0 0,0 21,19H19M19,13H21V11H19M11,21H13V19H11M9,3H7V5H9M3,17H5V15H3M5,21V19H3A2,2 0 0,0 5,21M19,3V5H21A2,2 0 0,0 19,3M13,3H11V5H13M3,9H5V7H3M7,21H9V19H7M3,13H5V11H3M3,5H5V3A2,2 0 0,0 3,5Z",
          transform = 'matrix(1 0 0 1 -4 -4)'
        ),
        click = htmlwidgets::JS(
          paste0("function(gd) {
            var visible = gd.data.map(trace => trace.visible != 'legendonly');
            var cells = gd.data.map(trace => trace.x.length);
            var keys = gd.data.map(trace => trace.key);
            var subplots = gd.data.map(trace => trace.customdata);
            var selected = [];
            var select_dict = {};
            for (var i = 0; i < visible.length; i++) {
              if (visible[i]) {
                var trace_sub = subplots[i][0];
                if (trace_sub in select_dict) {
                  select_dict[trace_sub].push(keys[i]);
                } else {
                  select_dict[trace_sub] = [keys[i]];
                }
                var n_cells = cells[i];
                var cell_array = [];
                for (var j = 0; j < n_cells; j++) {
                  cell_array.push(j);
                }
                selected.push(cell_array);
              } else {
                selected.push([]);
              }
            }
            Plotly.restyle(gd,{'selectedpoints': selected});
            Shiny.setInputValue('",ns("meta_custom_select"),"',select_dict,{priority: 'event'});
          }")
        )
      )
      onDropdownOpen <- '
        function(el){
          setTimeout(function(){
            $(el).find(".optgroup .option").hide();
            var selected = $(el).parent().find("div.item")
            if(selected.length !== 0) {
              var meta = selected.attr("data-value").split("@")[1];
              var unselected = $(el).find("div.optgroup[data-group!=\'"+meta+"\']:visible");
              $(unselected).hide()
            }
          }, 0);
        }'
      
      onItemAdd <- '
        function(val,item){
          setTimeout(function(){
            var meta = val.split("@")[1];
            var unselected = $("body").find("div.optgroup[data-group!=\'"+meta+"\']:visible");
            $(unselected).hide()
          }, 0);
        }'
      
      onItemRemove <- '
        function(val,item){
          setTimeout(function(){
            var meta = val.split("@")[1];
            var selectize = val.split("@")[0];
            var items = $("#"+selectize).siblings().find(".items .item");
            if(items.length === 0) {
              $("#"+selectize).siblings().find(".optgroup .option").hide();
            } else {
              var unselected = $("body").find("div.optgroup[data-group!=\'"+meta+"\']:visible");
              $(unselected).hide();
            }
          }, 0);
        }'

      
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
      
      create_split_list <- function(type) {
        meta_names <- data$meta
        split_list <- lapply(meta_names, function(x) {
          sublist_names <- gtools::mixedsort(unique(data$meta_use[[x]]))
          sublist <- as.list(paste0(id,"-",type,"_split","@",x,"@",sublist_names))
          names(sublist) <- sublist_names
          sublist
        })      
        names(split_list) <- meta_names
        return(split_list)
      }
      
      update_plot_inputs <- function() {
        if (!is.null(data$seurat)) {
          updateSelectizeInput(session, "layout_meta", choices = data$reducs)
          updateSelectizeInput(session, "layout_gene", choices = data$reducs)
          updateSelectizeInput(session, "metadata", choices = c(data$meta), selected = character(0))
          updateSelectizeInput(session, "meta_split", choices = create_split_list("meta"), selected = character(0),server=T,options=list(maxOptions=1000))
          updateSelectizeInput(session, "gene_split", choices = create_split_list("gene"), selected = character(0),server=T,options=list(maxOptions=1000))
          updateSelectizeInput(session, "meta_violin", choices = c("All Data",data$meta), selected = NULL)
          updateSelectizeInput(session, "meta_heatmap", choices = c(data$meta,"Cells"), selected = character(0))
          updateSelectizeInput(session, "meta_props_1", choices = c(data$meta), selected = character(0))
          updateSelectizeInput(session, "meta_props_2", choices = c("All Data",data$meta), selected = NULL)
          updateSelectizeInput(session, "gene_exp", choices = c(data$genes), selected = character(0), server = T,options=list(maxOptions=1000))
          updateSelectizeInput(session, "gene_violin", choices = c(data$genes), selected = character(0), server = T,options=list(maxOptions=1000))
        }
      }
      
      check_split <- function(split) {
        choices <- strsplit(split,"@")
        split_anno <- unique(sapply(choices,function(x) x[[2]]))
        split_groups <- sapply(choices,function(x) x[[3]])
        if (!(split_anno %in% data$meta)) {
          return(F)
        }
        all_groups <- unique(data$meta_use[split_anno][,1])
        group_check <- sum(split_groups %in% all_groups) == length(split_groups)
        return(group_check)
      }
      
      meta_change_update <- function() {
        all_meta <- data$meta
        meta_choice <- input$metadata
        updateSelectizeInput(session, "metadata", choices = c(all_meta), selected = if (isTruthy(meta_choice) && meta_choice %in% all_meta) meta_choice else character(0))
        meta_choice <- input$meta_violin
        updateSelectizeInput(session, "meta_violin", choices = c("All Data",all_meta), selected = if (isTruthy(meta_choice) && meta_choice %in% all_meta) meta_choice else NULL)
        meta_choice <- input$meta_heatmap
        updateSelectizeInput(session, "meta_heatmap", choices = c(all_meta,"Cells"), selected = if (isTruthy(meta_choice) && meta_choice %in% all_meta) meta_choice else character(0))
        meta_choice <- input$meta_props_1
        updateSelectizeInput(session, "meta_props_1", choices = c(all_meta), selected = if (isTruthy(meta_choice) && meta_choice %in% all_meta) meta_choice else character(0))
        meta_choice <- input$meta_props_2
        updateSelectizeInput(session, "meta_props_2", choices = c("All Data",all_meta), selected = if (isTruthy(meta_choice) && meta_choice %in% all_meta) meta_choice else NULL)
      }
      
      gene_sets <- reactiveVal()
      genes_heatmap <- reactiveVal(NULL)
      
      update_set_names <- function() {
        names <- unname(set_names())
        if (length(set_names()) == length(gene_sets())) {
          update_set_lists()
        }
        if (class(names) == "character") {
          choice <- input$reduction_gene_set
          if (!(list(choice) %in% c("All genes","Top 100 variable","Top 500 variable","Top 100 average","Top 500 average","Other features",names))) choice <- NULL 
          updateSelectizeInput(session, "reduction_gene_set",choices = c("All genes","Top 100 variable","Top 500 variable","Top 100 average","Top 500 average","Other features",names),selected = choice)
          choice <- input$violin_gene_set
          if (!(list(choice) %in% c("All genes","Top 100 variable","Top 500 variable","Top 100 average","Top 500 average","Other features",names))) choice <- NULL 
          updateSelectizeInput(session, "violin_gene_set",choices = c("All genes","Top 100 variable","Top 500 variable","Top 100 average","Top 500 average","Other features",names),selected = choice)
          choice <- input$heatmap_gene_set
          if (!(list(choice) %in% c("Top 100 variable","Top 500 variable","Top 100 average","Top 500 average",names))) choice <- character(0)
          updateSelectizeInput(session, "heatmap_gene_set",choices = c("Top 100 variable","Top 500 variable","Top 100 average","Top 500 average",names),selected = choice)
        } else{
          if (length(names) == 0) {
            updateSelectizeInput(session, "reduction_gene_set",choices = c("All genes","Top 100 variable","Top 500 variable","Top 100 average","Top 500 average","Other features"),selected = NULL)
            updateSelectizeInput(session, "violin_gene_set",choices = c("All genes","Top 100 variable","Top 500 variable","Top 100 average","Top 500 average","Other features"),selected = NULL)
            updateSelectizeInput(session, "heatmap_gene_set",choices = c("Top 100 variable","Top 500 variable","Top 100 average","Top 500 average"),selected = character(0))
          }
        }
      }
      
      update_set_lists <- function() {
        reduction_choice <- input$reduction_gene_set
        violin_choice <- input$violin_gene_set
        heatmap_choice <- input$heatmap_gene_set
        names <- unname(set_names())
        genes <- set_lists()
        names(genes) <- names
        update_reduction <- F
        update_violin <- F
        update_heatmap <- F
        if (!is.null(reduction_choice) && !(reduction_choice %in% c("All genes","Top 100 variable","Top 500 variable","Top 100 average","Top 500 average","Other features"))) {
          update_reduction <- !(identical(gene_sets()[[reduction_choice]],genes[[reduction_choice]]))
        }
        if (!is.null(violin_choice) && !(violin_choice %in% c("All genes","Top 100 variable","Top 500 variable","Top 100 average","Top 500 average","Other features"))) {
          update_violin <- !(identical(gene_sets()[[violin_choice]],genes[[violin_choice]]))
        }
        if (!is.null(heatmap_choice) && !(heatmap_choice %in% c("Top 100 variable","Top 500 variable","Top 100 average","Top 500 average"))) {
          update_heatmap <- !(identical(gene_sets()[[heatmap_choice]],genes[[heatmap_choice]]))
        }
        gene_sets(genes)
        if (update_reduction) {
          gene_choice <- input$gene_exp
          all_genes <- genes[[reduction_choice]]
          choices <- if(reduction_choice %in% names) c("Gene set score",all_genes) else c(all_genes)
          if (gene_choice %in% all_genes) {
            updateSelectizeInput(session, "gene_exp", choices = choices, selected = gene_choice, server = T,options=list(maxOptions=1000))
          } else {
            updateSelectizeInput(session, "gene_exp", choices = choices, selected = character(0), server = T,options=list(maxOptions=1000))
          }
        }
        if (update_violin) {
          gene_choice <- input$gene_violin
          all_genes <- genes[[violin_choice]]
          choices <- if(reduction_choice %in% names) c("Gene set score",all_genes) else c(all_genes)
          if (gene_choice %in% all_genes) {
            updateSelectizeInput(session, "gene_violin", choices = choices, selected = gene_choice, server = T,options=list(maxOptions=1000))
          } else {
            updateSelectizeInput(session, "gene_violin", choices = choices, selected = character(0), server = T,options=list(maxOptions=1000))
          }
        }
        if (update_heatmap) {
          genes_heatmap(gene_sets()[[heatmap_choice]])
        }
      }
      
      observeEvent(input$heatmap_gene_set, {
        if (input$heatmap_gene_set == "Top 100 variable") {
          genes_heatmap(data$top_sets[[1]])
        } else if (input$heatmap_gene_set == "Top 500 variable") {
          genes_heatmap(data$top_sets[[2]])
        } else if (input$heatmap_gene_set == "Top 100 average") {
          genes_heatmap(data$top_sets[[3]])
        } else if (input$heatmap_gene_set == "Top 500 average") {
          genes_heatmap(data$top_sets[[4]])
        } else if (input$heatmap_gene_set != "") {
          genes_heatmap(gene_sets()[[input$heatmap_gene_set]])
        } else {
          genes_heatmap(NULL)
        }
      })
      
      get_genes_reduction <- function() {
        if (input$reduction_gene_set == "All genes") {
          return(data$genes)
        } else if (input$reduction_gene_set == "Other features") {
          return(data$quality)
        } else if (input$reduction_gene_set == "Top 100 variable") {
          return(data$top_sets[[1]])
        } else if (input$reduction_gene_set == "Top 500 variable") {
          return(data$top_sets[[2]])
        } else if (input$reduction_gene_set == "Top 100 average") {
          return(data$top_sets[[3]])
        } else if (input$reduction_gene_set == "Top 500 average") {
          return(data$top_sets[[4]])
        } else {
          return(c("Gene set score",gene_sets()[[input$reduction_gene_set]]))
        }
      }
      
      observeEvent(input$reduction_gene_set, {
        if(input$reduction_gene_set != "") {
          updateSelectizeInput(session, "gene_exp", choices = c(get_genes_reduction()), selected = character(0), server = T,options=list(maxOptions=1000))
        }
      })
      
      get_genes_violin <- function() {
        if (input$violin_gene_set == "All genes") {
          return(data$genes)
        } else if (input$violin_gene_set == "Other features") {
          return(data$quality)
        } else if (input$violin_gene_set == "Top 100 variable") {
          return(data$top_sets[[1]])
        } else if (input$violin_gene_set == "Top 500 variable") {
          return(data$top_sets[[2]])
        } else if (input$violin_gene_set == "Top 100 average") {
          return(data$top_sets[[3]])
        } else if (input$violin_gene_set == "Top 500 average") {
          return(data$top_sets[[4]])
        } else {
          return(c("Gene set score",gene_sets()[[input$violin_gene_set]]))
        }
      }
      
      observeEvent(input$violin_gene_set, {
        if (input$violin_gene_set != "") {
          updateSelectizeInput(session, "gene_violin", choices = c(get_genes_violin()), selected = character(0), server = T,options=list(maxOptions=1000))
        }
      })
      
      observeEvent(input$box$visible, {
        if (!input$box$visible) {
          reset_select()
          removeUI(paste0("#",id,"-render_plot"),immediate=T)
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
          lapply(plot_types_all, function(x) shinyjs::hide(paste0(x,"_div")))
          shinyjs::show(paste0(plot_type(),"_div"))
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
            div(
              id = ns('reduction_div'),
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
                  selectizeInput(ns("meta_split"), 
                                 label = "Split by group",
                                 choices = NULL,
                                 selected = NULL,
                                 multiple=T,
                                 options=list(maxItems=3,onDropdownOpen = I(onDropdownOpen),onItemAdd = I(onItemAdd),onItemRemove = I(onItemRemove))
                  ),
                  fluidRow(
                    column(
                      width=7,
                      selectizeInput(ns("layout_meta"),
                       label="Layout",
                       choices = NULL,
                       selected = NULL,
                       width= "150px"
                      )
                    ),
                    column(
                      width=5,
                      div(
                        style="margin-top:8px; margin-bottom:12px;",
                        strong("Show labels"),
                        materialSwitch(ns("labels"),"",value=F,status="primary")
                      )
                    )
                  )
                ),
                tabPanel(
                  title = "Features",
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
                  selectizeInput(ns("gene_split"), 
                                 label = "Split by group",
                                 choices = NULL,
                                 selected = NULL,
                                 multiple=T,
                                 options=list(maxItems=3,onDropdownOpen = I(onDropdownOpen),onItemAdd = I(onItemAdd),onItemRemove = I(onItemRemove))
                  ),
                  fluidRow(
                    column(
                      width=7,
                      selectizeInput(ns("layout_gene"),
                         label="Layout",
                         choices = NULL,
                         selected = NULL,
                         width= "150px"
                      )
                    ),
                    column(
                      width=5,
                      div(
                        style="margin-top:8px; margin-bottom:12px;",
                        strong("Density mode"),
                        materialSwitch(ns("density"),"",value=F,status="primary")
                      )
                    )
                  )
                )
              )
            ),
            div(
              id = ns('heatmap_div'),
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
              radioGroupButtons(
                inputId = ns("heatmap_type"),
                label = "Type",
                choices = c("Heatmap","Dot")
              ),
              fluidRow(
                column(
                  width=4,
                  offset = 0,
                  strong("Scaling"),
                  materialSwitch(ns("scale_heatmap"),"",value=T,status="primary")
                ),
                column(
                  width=4,
                  offset=0,
                  strong("Clustering"),
                  materialSwitch(ns("cluster_heatmap"),"",value=T,status="primary")
                ),
                column(
                  width=4,
                  offset=0,
                  strong("Flip axes"),
                  materialSwitch(ns("flip_heatmap"),"",value=F,status="primary")
                )
              )
            ),
            div(
              id = ns('violin_div'),
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
                             label = "Across",
                             choices = NULL,
                             selected = NULL
              )
            ),
            div(
              id = ns('props_div'),
              selectizeInput(ns("meta_props_1"), 
                             label = "View proportions of",
                             choices = NULL,
                             selected = NULL
              ),
              selectizeInput(ns("meta_props_2"), 
                             label = "Across",
                             choices = NULL,
                             selected = NULL
              )
            ),
            div(
              id = ns('volcano_div'),
              radioGroupButtons(
                inputId = ns("volcano_data"),
                label = "Data",
                choices = c("Markers","DEGs")
              ),
              radioGroupButtons(
                inputId = ns("volcano_type"),
                label = "Plot type",
                choices = c("Volcano","MA")
              )
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
              ),
              tags$button(
                id = ns("volcano"),
                type="button", 
                class="btn action-button btn-large",
                img(src = "images/volcano_off.png", height = 45, width = 45),
                style="padding-left: 4px;"
              ),
            )
          ),
          tags$head(tags$style(
            paste0("#",ns('plot_drop')," {background-color: #88abff; color: #FFF;}"),
            paste0("#",ns('box')," .card-header {height: 50px;}"),
            paste0("#",ns('box')," .form-group {margin-top: 8px;}")
          )),
          withSpinner(plotlyOutput(ns("plot"),width = "auto"),type=5,color = "#738bfb"),
          div(id=ns("slider_div"),style="position:absolute; right:1%; bottom:52%; margin-right:82px;",uiOutput(ns("gene_slider"))),
          div(id=ns("color_div"),style="position:absolute; right:1%; bottom:52%; width:87px; margin-bottom:7px;",plotOutput(height="240px",ns("gene_colorbar")))
        )
      })
      
      plot_name <- paste0(id,"-plot")
      plot_type <- reactiveVal('reduction')
      has_cur_select <- reactiveVal(NULL)
      cur_labels <- reactiveValues(plot1=NULL,plot2=NULL,plot3=NULL)
      cur_visible <- reactiveVal(NULL)
      is_3D <- reactiveVal(F)
      plot_types_all <- c("reduction", "heatmap", "violin", "props", "volcano")
      
      toggle_slider <- function(toggle_on,with_slider=T) {
        if (toggle_on) {
          if (with_slider) {
            shinyjs::show("slider_div")
          }
          shinyjs::show("color_div")
        } else {
          shinyjs::hide("slider_div")
          shinyjs::hide("color_div")
        }
      }
      
      reset_cur_labels <- function() {
        cur_labels$plot1 <- NULL
        cur_labels$plot2 <- NULL
        cur_labels$plot3 <- NULL
      }
      
      observeEvent(input$show_slider, {
        toggle_slider(T)
      })
      
      observeEvent(input$show_slider_3d, {
        toggle_slider(T,F)
      })
      
      volcano_lines <- reactive({
        req(data$seurat)
        event_data("plotly_relayout",source=ns("volcano_plot"),priority="event")
      })
      
      volcano_x1 <- reactiveVal(-0.5)
      volcano_x2 <- reactiveVal(0.5)
      volcano_y <- reactiveVal(10)
      
      ma_lines <- reactive({
        req(data$seurat)
        event_data("plotly_relayout",source=ns("ma_plot"),priority="event")
      })
      
      ma_y1 <- reactiveVal(0.5)
      ma_y2 <- reactiveVal(-0.5)
      ma_x <- reactiveVal(0)
      
      create_anno_marker <- function(x, y, text){
        return(list(
          x = x,
          y = y,
          text = as.character(text),
          xref = "x",
          yref = "y",
          showarrow = F,
          yshift=15,
          opacity = 0.8,
          font = list(size = 13)
        ))
      }
      
      update_volcano <- function() {
        plotlyProxy(ns("plot"), session) %>%
          plotlyProxyInvoke("relayout", 
            list(shapes=list(
              list(type = "line", x0 = volcano_x1(), x1 = volcano_x1(), y0 = 0, y1 = 1, yref = "paper",layer="above",opacity=0.3,line=list(dash="dash",color="blue")),
              list(type = "line", x0 = volcano_x2(), x1 = volcano_x2(), y0 = 0, y1 = 1, yref = "paper",layer="above",opacity=0.3,line=list(dash="dash",color="blue")),
              list(type = "line", x0 = 0, x1 = 1, xref="paper", y0 = volcano_y(), y1 = volcano_y(),layer="above",opacity=0.3,line=list(dash="dash",color="blue"))
            ))
          )
        
        if (input$volcano_data == "Markers") {
          data_use <- markers()
        } else {
          data_use <- degs()
        }
        signif <- as.numeric(data_use$`p-val`)
        p_val <- -log10(signif)
        p_val[p_val >= 300] <- 300
        fc <- data_use$log2FC
        colors <- rep(0,nrow(data_use))
        filter_sig <- p_val >= volcano_y()
        if (volcano_x1() != volcano_x2()) {
          fc_lower <- min(volcano_x1(),volcano_x2())
          fc_upper <- max(volcano_x1(),volcano_x2())
        } else {
          fc_lower <- volcano_x1()
          fc_upper <- volcano_x2()
        }
        filter_neg <- fc <= fc_lower
        filter_pos <- fc >= fc_upper
        colors[filter_sig & filter_neg] <- -1
        colors[filter_sig & filter_pos] <- 1
        anno_data <- data.frame(p_val,fc,gene=data_use$gene)
        anno_data <- anno_data[colors != 0,]
        annotation_mat <- mapply(create_anno_marker, anno_data$fc, anno_data$p_val, anno_data$gene)
        annotation_list <- lapply(1:ncol(annotation_mat), function(i) annotation_mat[,i])
        plotlyProxy(ns("plot"), session) %>%
          plotlyProxyInvoke("relayout", 
            list(annotations=annotation_list)
          )
        plotlyProxy(ns("plot"), session) %>%
          plotlyProxyInvoke("restyle", 
            list(marker=list(color=colors,colorscale=list(c(-1, "rgb(224, 40, 40)"),c(0, "rgb(204, 204, 204)"), c(1, "rgb(40, 58, 224)")),size=plot_settings$point_size + 4,cmin=-1,cmax=1))
          )
      }
      
      observeEvent(volcano_lines(), {
        names <- names(volcano_lines())
        if (grepl("shapes[0]",names[1],fixed=T)) {
          volcano_x1(volcano_lines()[[1]])
          update_volcano()
        }
        else if (grepl("shapes[1]",names[1],fixed=T)) {
          volcano_x2(volcano_lines()[[1]])
          update_volcano()
        }
        else if (grepl("shapes[2]",names[1],fixed=T)) {
          volcano_y(volcano_lines()[[4]])
          update_volcano()
        }
      })
      
      update_ma <- function() {
        plotlyProxy(ns("plot"), session) %>%
          plotlyProxyInvoke("relayout", 
            list(shapes = list(
              list(type = "line", x0 = 0, x1 = 1,xref = "paper", y0 = ma_y1(), y1 = ma_y1(),layer="above",opacity=0.3,line=list(dash="dash",color="blue")),
              list(type = "line", x0 = 0, x1 = 1, xref = "paper",y0 = ma_y2(), y1 = ma_y2(),layer="above",opacity=0.3,line=list(dash="dash",color="blue")),
              list(type = "line", x0 = ma_x(), x1 = ma_x(), y0 = 0, y1 = 1,yref = "paper",layer="above",opacity=0.3,line=list(dash="dash",color="blue"))
            ))
          )
        
        if (input$volcano_data == "Markers") {
          data_use <- markers()
        } else {
          data_use <- degs()
        }
        gene <- data_use$gene
        m_val <- data_use$log2FC
        a_val <- log2(data_use$avg.1 * data_use$avg.2) / 2
        p_val <- as.numeric(data_use$`p-val`)
        color <- -log10(p_val)
        color[color > 300] <- 300
        anno_data <- data.frame(m_val,a_val,p_val,gene,color)
        if (ma_y1() != ma_y2()) {
          fc_lower <- min(ma_y1(),ma_y2())
          fc_upper <- max(ma_y1(),ma_y2())
        } else {
          fc_lower <- ma_y1()
          fc_upper <- ma_y2()
        }
        opacity <- rep(0.2,nrow(anno_data))
        filter_avg <- a_val >= ma_x()
        filter_neg <- m_val <= fc_lower
        filter_pos <- m_val >= fc_upper
        opacity[filter_avg & filter_neg] <- 1
        opacity[filter_avg & filter_pos] <- 1
        anno_data <- anno_data[opacity != 0.2,]
        color <- -log10(p_val)
        color[color > 300] <- 300
        annotation_mat <- mapply(create_anno_marker, anno_data$a_val, anno_data$m_val, anno_data$gene)
        annotation_list <- lapply(1:ncol(annotation_mat), function(i) annotation_mat[,i])
        plotlyProxy(ns("plot"), session) %>%
          plotlyProxyInvoke("relayout", 
            list(annotations=annotation_list)
          )
        plotlyProxy(ns("plot"), session) %>%
          plotlyProxyInvoke("restyle", 
            list(marker=list(color = color,colorscale='Viridis',opacity=opacity,size=plot_settings$point_size + 4,reversescale=T))
          )
      }
      
      observeEvent(ma_lines(), {
        names <- names(ma_lines())
        if (grepl("shapes[0]",names[1],fixed=T)) {
          ma_y1(ma_lines()[[4]])
          update_ma()
        }
        else if (grepl("shapes[1]",names[1],fixed=T)) {
          ma_y2(ma_lines()[[4]])
          update_ma()
        }
        else if (grepl("shapes[2]",names[1],fixed=T)) {
          ma_x(ma_lines()[[1]])
          update_ma()
        }
      })
      
      empty_select_update <- function(empty_select) {
        subplot <- unlist(empty_select[["subplot"]])
        cells <- unlist(empty_select[["cells"]])
        results <- data.frame(subplot,cells)
        for (x in unique(subplot)) {
          subset <- results[subplot == x,]
          if (sum(subset$cells) == 0) {
            subplot_name <- paste0(plot_name,"@",x)
            selection_list$selects[[subplot_name]] <- NULL
            if (isTruthy(cur_selection$plot) && cur_selection$plot == subplot_name) {
              cur_selection$plot <- NULL
              cur_selection$cells <- NULL
            }
          }
        }
      }
      
      meta_cells <- reactive({
        req(data$seurat)
        req(input$metadata)
        selected <- event_data("plotly_selected",source=ns("meta_plot"),priority="event")
        subplot <- unique(selected$customdata)
        subplot <- subplot[!is.na(subplot)]
        if (!is.null(subplot)) {
          list(cells=selected$key,subplot=subplot)
        } else {
          shinyjs::runjs(paste0("
            var gd = document.getElementById('",plot_name,"');
            var selects = gd.data.map(trace => trace.selectedpoints);
            var subplots = gd.data.map(trace => trace.customdata);
            var subplot_num = [];
            var cell_num = [];
            for (var i = 0; i < subplots.length; i++) {
              if (selects[i] !== undefined) {
                subplot_num.push(subplots[i][0]);
                cell_num.push(selects[i].length);
              }
            }
            Shiny.setInputValue('",ns("meta_clear_cells_empty"),"',{subplot: subplot_num, cells: cell_num},{priority: 'event'});
          "))
          return(NULL)
        }
      })
      
      meta_clear_cells <- reactive({
        req(data$seurat)
        req(input$metadata)
        event_data("plotly_deselect",source=ns("meta_plot"),priority="event")
      })
      
      meta_legend <- debounce(reactive({
        req(data$seurat)
        req(input$metadata)
        event_data("plotly_legendclick",source=ns("meta_plot"),priority="event")
      }),300)
      
      
      observeEvent(meta_legend(), {
        if(input$labels) {
          shinyjs::delay(100,
            shinyjs::runjs(
              paste0("
                var gd = document.getElementById('",plot_name,"');
                var visible = gd.data.map(trace => trace.visible != 'legendonly');
                var cluster = gd.data.map(trace => trace.legendgroup);
                var subplots = gd.data.map(trace => trace.customdata);
                var results = [];
                for (var i = 0; i < visible.length; i++) {
                  if (subplots[i][0] === 0 && visible[i]) {
                    results.push(cluster[i]);
                  }
                }
                var scenes = Object.keys(gd.layout).filter(key => key.includes('scene'));
                if (scenes.length > 0) {
                  if (scenes.length == 3) {
                    scenes = scenes.slice(0,1);
                  } else if (scenes.length == 5) {
                    scenes = scenes.slice(0,2);
                  } else if (scenes.length == 7) {
                    scenes = scenes.slice(0,3);
                  }
                  cameras = [];
                  for (i in scenes) {
                    var data = gd.layout[scenes[i]]['camera'];
                    if (data === undefined) {
                      data = [scenes[i]];
                    }
                    cameras.push([data]);
                  }
                  Shiny.setInputValue('",ns("meta_3D_camera"),"',cameras,{priority: 'event'});
                }
                Shiny.setInputValue('",ns("meta_legend_visible"),"',results,{priority: 'event'});
              ")
            )
          )
        }
      })
      
      update_labels <- function(visible) {
        if(input$data_type == "Metadata" && input$labels) {
          if (is.null(visible)) {
            anno_list <- list()
          } else {
            if (is.null(split_order_1())) {
              label_info <- cur_labels$plot1 %>% filter(label %in% visible)
              anno_list <- prepare_2D_labels(label_info)
            } else {
              anno_list <- lapply(1:length(split_order_1()),function(x) {
                plot_num <- switch(x,"plot1","plot2","plot3")
                label_info <- cur_labels[[plot_num]] %>% filter(label %in% visible)
                prepare_2D_labels(label_info,x)
              })
              anno_list <- do.call(c,anno_list)
            }
          }
          return(anno_list)
        } else {
          return(list())
        }
      }
      
      update_labels_3D <- function(visible) {
        label_list <- list()
        if(input$data_type == "Metadata" && input$labels) {
          if (is.null(split_order_1())) {
            if(!is.null(visible)) {
              label_info <- cur_labels$plot1 %>% filter(label %in% visible)
              label_list <- prepare_3D_labels(label_info)
            }
            plotlyProxy(ns("plot"), session) %>%
              plotlyProxyInvoke("relayout", 
                list(scene = list(annotations=label_list,domain=list(x=c(0,1),y=c(0,0.9)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)))
              )
          } else {
            label_list <- lapply(1:length(split_order_1()),function(x) {
              if(!is.null(visible)) {
                plot_num <- switch(x,"plot1","plot2","plot3")
                label_info <- cur_labels[[plot_num]] %>% filter(label %in% visible)
                prepare_3D_labels(label_info)
              } else {
                list()
              }
            })
            scene_num <- length(label_list)
            if (scene_num == 1) {
              plotlyProxy(ns("plot"), session) %>%
                plotlyProxyInvoke("relayout", 
                  list(scene = list(annotations=label_list[[1]],domain=list(x=c(0,1),y=c(0,0.9)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)))
                )
            } else if (scene_num == 2) {
              plotlyProxy(ns("plot"), session) %>%
                plotlyProxyInvoke("relayout", 
                  list(scene = list(annotations=label_list[[1]],domain=list(x=c(0,0.5),y=c(0,0.9)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
                       scene2 = list(annotations=label_list[[2]],domain=list(x=c(0.5,1),y=c(0,0.9)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F))
                  )
                )
            } else if (scene_num == 3) {
              plotlyProxy(ns("plot"), session) %>%
                plotlyProxyInvoke("relayout", 
                  list(scene = list(annotations=label_list[[1]],domain=list(x=c(0,0.33),y=c(0,0.9)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
                       scene2 = list(annotations=label_list[[2]],domain=list(x=c(0.33,0.66),y=c(0,0.9)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
                       scene3 = list(annotations=label_list[[3]],domain=list(x=c(0.66,0.99),y=c(0,0.9)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F))
                  )
                )
            }
          }
        }
      }
      
      observeEvent(input$meta_3D_camera, {
        print(input$meta_3D_camera)
      })
      
      observeEvent(input$meta_legend_visible, {
        visible <- input$meta_legend_visible
        cur_visible(visible)
        if (is_3D()) {
          update_labels_3D(visible)
        } else {
          update_all_anno()
        }
      },ignoreInit = T, ignoreNULL = F)
      
      observeEvent(meta_clear_cells(), {
        if (!isTruthy(split_1())) {
          shinyjs::runjs(paste0("
            var gd = document.getElementById('",plot_name,"');
            Plotly.restyle(gd,{'selectedpoints': null});
          "))
          reset_select()
        } else {
          shinyjs::runjs(paste0("
            var gd = document.getElementById('",plot_name,"');
            var selects = gd.data.map(trace => trace.selectedpoints);
            var subplots = gd.data.map(trace => trace.customdata);
            var deselected = [];
            var traces = [];
            for (var i = 0; i < subplots.length; i++) {
              if (subplots[i][0] > 0 & selects[i] === undefined) {
                deselected.push(subplots[i][0]);
              }
            }
            for (var j = 0; j < subplots.length; j++) {
              if (deselected.includes(subplots[j][0])) {
                traces.push(j);
              }
            }
            console.log(deselected);
            console.log(traces);
            Plotly.restyle(gd,{'selectedpoints': null},traces);
            Shiny.setInputValue('",ns("meta_clear_cells_subplot"),"',deselected,{priority: 'event'});
          "))
        }
      })
      
      observeEvent(input$meta_clear_cells_subplot, {
        subplots <- unique(input$meta_clear_cells_subplot)
        subplot_names <- paste0(plot_name,"@",subplots)
        for (name in subplot_names) {
          selection_list$selects[[name]] <- NULL
          if (isTruthy(cur_selection$plot) && cur_selection$plot == name) {
            cur_selection$plot <- NULL
            cur_selection$cells <- NULL
          }
        }
      })
      
      observeEvent(input$meta_clear_cells_empty, {
        empty_select_update(input$meta_clear_cells_empty)
      })
      
      observeEvent(input$custom_clear_select, {
        reset_select()
      })
      
      observeEvent(meta_cells(), {
        cells <- meta_cells()[["cells"]]
        subplot <- meta_cells()[["subplot"]]
        plot_select <- paste0(plot_name,"@",subplot)
        if (length(cells) > 0) {
          selection_list$selects[[plot_select]] <- cells
          cur_selection$plot <- plot_select
          cur_selection$cells <- cells
        } else {
          selection_list$selects[[plot_select]] <- NULL
          if (!is.null(cur_selection$plot) && plot_select == cur_selection$plot) {
            cur_selection$plot <- NULL
            cur_selection$cells <- NULL
          }
        }
      },ignoreInit = T,ignoreNULL = T)
      
      observeEvent(input$meta_custom_select, {
        cells <- lapply(input$meta_custom_select,function(x) unlist(x))
        subplot_num <- names(cells)
        if (length(subplot_num) == 1 && length(cells[[1]]) > 0) {
          cell_list <- cells[[1]]
          plot_select <- paste0(plot_name,"@0")
          selection_list$selects[[plot_select]] <- cell_list
          cur_selection$plot <- plot_select
          cur_selection$cells <- cell_list
        } else {
          subplot_num <- subplot_num[2:length(subplot_num)]
          for (x in subplot_num) {
            cell_list <- cells[[x]]
            if (length(cell_list) > 0) {
              subplot_name <- paste0(plot_name,"@",x)
              selection_list$selects[[subplot_name]] <- cell_list
              cur_selection$plot <- subplot_name
              cur_selection$cells <- cell_list
            }
          }
        }
      })
      
      exp_cells <- reactive({
        req(data$seurat)
        req(input$gene_exp)
        selected <- event_data("plotly_selected",source=ns("exp_plot"),priority="event")
        subplot <- unique(selected$customdata)
        subplot <- subplot[!is.na(subplot)]
        if (!is.null(subplot)) {
          list(cells=selected$key,subplot=subplot)
        } else {
          shinyjs::runjs(paste0("
            var gd = document.getElementById('",plot_name,"');
            var selects = gd.data.map(trace => trace.selectedpoints);
            var subplots = gd.data.map(trace => trace.customdata);
            var subplot_num = [];
            var cell_num = [];
            for (var i = 0; i < subplots.length; i++) {
              if (subplots[i] !== undefined & selects[i] !== undefined) {
                subplot_num.push(subplots[i][0]);
                cell_num.push(selects[i].length);
              }
            }
            Shiny.setInputValue('",ns("exp_clear_cells_empty"),"',{subplot: subplot_num, cells: cell_num},{priority: 'event'});
          "))
          return(NULL)
        }
      })
      
      exp_clear_cells <- reactive({
        req(data$seurat)
        req(input$gene_exp)
        event_data("plotly_deselect",source=ns("exp_plot"),priority="event")
      })
      
      observeEvent(exp_clear_cells(), {
        if (!isTruthy(split_2())) {
          reset_select()
        } else {
          shinyjs::runjs(paste0("
            var gd = document.getElementById('",plot_name,"');
            var selects = gd.data.map(trace => trace.selectedpoints);
            var subplots = gd.data.map(trace => trace.customdata);
            var deselected = [];
            for (var i = 0; i < subplots.length; i++) {
              if (subplots[i] !== undefined & selects[i] === undefined) {
                deselected.push(subplots[i][0]);
              }
            }
            Shiny.setInputValue('",ns("exp_clear_cells_subplot"),"',deselected,{priority: 'event'});
          "))
        }
      })
      
      observeEvent(input$exp_clear_cells_subplot, {
        subplots <- unique(input$exp_clear_cells_subplot)
        subplot_names <- paste0(plot_name,"@",subplots)
        for (name in subplot_names) {
          selection_list$selects[[name]] <- NULL
          if (isTruthy(cur_selection$plot) && cur_selection$plot == name) {
            cur_selection$plot <- NULL
            cur_selection$cells <- NULL
          }
        }
      })
      
      observeEvent(input$exp_clear_cells_empty, {
        empty_select_update(input$exp_clear_cells_empty)
      })
      
      observeEvent(exp_cells(), {
        cells <- exp_cells()[["cells"]]
        subplot <- exp_cells()[["subplot"]]
        plot_select <- paste0(plot_name,"@",subplot)
        if (length(cells) > 0) {
          selection_list$selects[[plot_select]] <- cells
          cur_selection$plot <- plot_select
          cur_selection$cells <- cells
        } else {
          selection_list$selects[[plot_select]] <- NULL
          if (!is.null(cur_selection$plot) && plot_select == cur_selection$plot) {
            cur_selection$plot <- NULL
            cur_selection$cells <- NULL
          }
        }
      },ignoreInit = T, ignoreNULL = T)
      
      observeEvent(input$data_type, {
        if (input$data_type == "Metadata") {
          toggle_slider(F)
        }
        reset_select()
      })
      
      reset_select <- function() {
        select_name <- paste0("plotly_selected","-",id,"-meta_plot")
        session$userData$plotlyInputStore[[select_name]] <- NULL
        select_name <- paste0("plotly_selected","-",id,"-exp_plot")
        session$userData$plotlyInputStore[[select_name]] <- NULL
        all_selects <- names(selection_list$selects)
        plot_selects <- all_selects[grepl(plot_name,all_selects)]
        if (length(plot_selects) > 0) {
          for(plot in plot_selects){
            selection_list$selects[[plot]] <- NULL
            if (isTruthy(cur_selection$plot) && cur_selection$plot == plot) {
              cur_selection$plot <- NULL
              cur_selection$cells <- NULL
            }
          }
        }
      }
      
      observeEvent(input$plot_rendered, {
        reset_select()
      })
      
      get_plot_title <- function(is_bold) {
        if (input$data_type == "Metadata") {
          text <- split_order_1()
          if (is.null(text)) {
            if (is_bold) {
              return(paste0("<b>",input$metadata,"</b>"))
            } else {
              return(input$metadata)
            }
          }
        } else {
          text <- split_order_2()
          if (is.null(text)) {
            if (is_bold) {
              return(paste0("<b>",input$gene_exp,"</b>"))
            } else {
              return(input$gene_exp)
            }
          }
        }
        bold <- rep(F,length(text))
        if (is_bold) bold[has_cur_select()] <- T
        if (length(text) == 1) {
          pos <- c(0.5)
        } else if (length(text) == 2) {
          pos <- c(0.25,0.75)
        } else {
          pos <- c(0.17,0.5,0.83)
        }
        anno_data <- data.frame(pos=pos,text=text,bold=bold)
        return(anno_data)
      }
      
      create_anno_reduct <- function(pos, text, bold){
        return(list(
          x = pos,
          y = 1,
          text = if (bold) paste0("<b>",as.character(text),"</b>") else as.character(text),
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "top",
          showarrow = FALSE,
          font = list(size = 16)
        ))
      }
      
      update_all_anno <- function() {
        title_data <- NULL
        if (isTruthy(cur_selection$plot)) {
          select <- strsplit(cur_selection$plot,"@")[[1]]
          if (plot_name == select[1]) {
            has_cur_select(as.numeric(select[2]))
            title_data <- get_plot_title(T)
          } else {
            has_cur_select(NULL)
            title_data <- get_plot_title(F)
          }
        } else {
          has_cur_select(NULL)
          title_data <- get_plot_title(F)
        }
        label_list <- update_labels(cur_visible())
        if (typeof(title_data) == "list") {
          title_mat <- mapply(create_anno_reduct, title_data$pos, title_data$text, title_data$bold)
          title_list <- lapply(1:ncol(title_mat), function(i) title_mat[,i])
          annotation_list <- c(title_list,label_list)
          plotlyProxy(ns("plot"), session) %>%
            plotlyProxyInvoke("relayout", 
              list(annotations=annotation_list)
            )
        } else {
          plotlyProxy(ns("plot"), session) %>%
            plotlyProxyInvoke("relayout", 
              list(annotations=label_list,title = list(text=title_data,y=0.98,font = list(size = 20)))
            )
        }
      }
      
      observeEvent(cur_selection$cells, {
        if (plot_type() == 'reduction') {
          update_all_anno()
        }
      },ignoreNULL = F,ignoreInit = T)
      
      exp_range <- reactiveValues(min=NULL,max=NULL)
      slider_ready <- reactiveVal(TRUE)

      output$gene_slider <- renderUI({
        if(input$data_type == "Features" && isTruthy(input$gene_exp) && isTruthy(exp_range$max) && exp_range$max != 0) {
          slider_ready(FALSE)
          noUiSliderInput(ns("exp_range_select"),orientation = "vertical",direction="rtl",min=exp_range$min,max=exp_range$max,value=c(exp_range$min,exp_range$max),width="1vw",height="178px",color="#96a8fc")
        } else {
          NULL
        }
      })
      
      output$gene_colorbar <- renderPlot(bg="transparent",{
        if(input$data_type == "Features" && isTruthy(input$gene_exp) && isTruthy(exp_range$max)) {
          data <- data.frame(x=c(1,2),y=c(1,2),color=c(exp_range$min,exp_range$max))
          colnames(data) <- c("x","y","Expression")
          color_scale <- plot_settings$color_cont
          if (is.function(color_scale)) {
            if (exp_range$max == 0) {
              color_scale <- "gray85"
            } else {
              color_scale <- c("gray85","blue4","steelblue1","cyan1")
            }
          } else {
            if (exp_range$max == 0) {
              color_scale <- if (color_scale == "viridis") "#440154FF" else if (color_scale == "plasma") "#0D0887FF"
            } else {
              color_scale <- if (color_scale == "viridis") viridisLite::viridis(10) else if (color_scale == "plasma") viridisLite::plasma(10)
            }
          }     
          plot <- ggplot(data,aes(x = x, y = y, color = Expression)) +
            geom_point() + scale_color_gradientn(colors = color_scale) + 
            theme(legend.key.width = unit(1.0, 'cm'),legend.key.height = unit(1.25,'cm'))
          legend <- cowplot::get_legend(plot)
          legend <- legend$grobs[[1]]
          legend$grobs[[1]] <- zeroGrob()
          legend$grobs[[4]] <- zeroGrob()
          legend$grobs[[3]]$children[[1]]$gp$fontsize <- 12.0
          grid::grid.newpage()
          grid::grid.draw(legend)
        } else {
          return(NULL)
        }
      })
      
      observeEvent(input$exp_range_select, {
        min_check <- abs(input$exp_range_select[1] - exp_range$min)
        max_check <- abs(input$exp_range_select[2] - exp_range$max)
        if ((min_check <= 0.005 && max_check <= 0.005) && !slider_ready()) {
          slider_ready(T)
        } else{
          if (!slider_ready()) {
            slider_ready(T)
          }
          exp_js <- htmlwidgets::JS(
            paste0(
              "var id = '",ns("plot"),"';
              var exp_low = ",input$exp_range_select[1]," - 0.005;
              var exp_high = ",input$exp_range_select[2]," + 0.005;
              var gd = document.getElementById(id);
              for (var plot = 0; plot < gd.data.length; plot+=2) {
                var exp = gd.data[plot].text;
                var keys = gd.data[plot].key;
                var selected = [];
                var selected_keys = [];
                for (var i = 0; i < keys.length; i++) {
                  var exp_val = exp[i];
                  if (exp_val >= exp_low && exp_val <= exp_high) {
                    selected.push(i);
                    selected_keys.push(keys[i]);
                  }
                }
                Plotly.restyle(gd,{'selectedpoints': [selected]},plot);
                var subplot_num = plot/2 + 1
                if (gd.data.length == 2) {
                  subplot_num = 0
                }
                Shiny.setInputValue('",ns("exp_custom_select"),"',{subplot: subplot_num, cells: selected_keys},{priority: 'event'});
              }"
            )
          )
          shinyjs::runjs(exp_js)
        }
      },ignoreInit = T)
      
      observeEvent(input$exp_custom_select, {
        custom_select <- input$exp_custom_select
        subplot <- custom_select[["subplot"]]
        cells <- unlist(custom_select[["cells"]])
        plot_select <- paste0(plot_name,"@",subplot)
        if (length(cells) > 0) {
          selection_list$selects[[plot_select]] <- cells
          cur_selection$plot <- plot_select
          cur_selection$cells <- cells
        } else {
          selection_list$selects[[plot_select]] <- NULL
          if (!is.null(cur_selection$plot) && plot_select == cur_selection$plot) {
            cur_selection$plot <- NULL
            cur_selection$cells <- NULL
          }
        }
      })
      
      session$userData[[paste0("meta_",id,"_obs")]] <- observeEvent(data$meta, {
        meta_change_update()
      },ignoreInit = T)
      
      session$userData[[paste0("reducs_",id,"_obs")]] <- observeEvent(data$reducs, {
        updateSelectizeInput(session, "layout_meta", choices = data$reducs)
        updateSelectizeInput(session, "layout_gene", choices = data$reducs)
      })
      
      session$userData[[paste0("genes_",id,"_obs")]] <- observeEvent(data$genes, {
        updateSelectizeInput(session, "gene_exp", choices = c(data$genes), selected = character(0), server = T,options=list(maxOptions=1000))
        updateSelectizeInput(session, "gene_violin", choices = c(data$genes), selected = character(0), server = T,options=list(maxOptions=1000))
      })
      
      set_names <- reactive(sapply(sets$sets,function(x) x$name()))
      set_lists <- reactive(lapply(sets$sets,function(x) x$genes()))
      
      observeEvent(set_names(), {
        update_set_names()
      })
      
      observeEvent(set_lists(), {
        update_set_lists()
      })
      
      session$userData[[paste0("meta_use_",id,"_obs")]] <- observeEvent(data$meta_use, {
        if (isTruthy(input$metadata) && input$metadata %in% data$meta) {
          subset <- data$meta_use[input$metadata]
          if (!identical(subset,meta_plot_reduct())) {
            meta_plot_reduct(subset)
          }
        } else {
          meta_plot_reduct(NULL)
        }
        if (isTruthy(input$meta_violin) && input$meta_violin != "All Data" && input$meta_violin %in% data$meta) {
          subset <- data$meta_use[input$meta_violin]
          if (!identical(subset,meta_plot_violin())) {
            meta_plot_violin(subset)
          }
        } else {
          meta_plot_violin(NULL)
        }
        if (isTruthy(input$meta_heatmap) && input$meta_heatmap != "Cells" && input$meta_heatmap %in% data$meta) {
          subset <- data$meta_use[input$meta_heatmap]
          if (!identical(subset,meta_plot_heatmap())) {
            meta_plot_heatmap(subset)
          }
        } else {
          meta_plot_heatmap(NULL)
        }
        if (isTruthy(input$meta_props_1) && input$meta_props_1 %in% data$meta) {
          subset <- data$meta_use[input$meta_props_1]
          if (!identical(subset,meta_plot_props_1())) {
            meta_plot_props_1(subset)
          }
        } else {
          meta_plot_props_1(NULL)
        }
        if (isTruthy(input$meta_props_2) && input$meta_props_2 != "All Data" && input$meta_props_2 %in% data$meta) {
          subset <- data$meta_use[input$meta_props_2]
          if (!identical(subset,meta_plot_props_2())) {
            meta_plot_props_2(subset)
          }
        } else {
          meta_plot_props_2(NULL)
        }
        meta_choice <- input$meta_split
        split_selected <- if (isTruthy(meta_choice) && check_split(meta_choice)) meta_choice else character(0)
        updateSelectizeInput(session, "meta_split", choices = create_split_list("meta"), selected = split_selected,options=list(maxItems=3,onDropdownOpen = I(onDropdownOpen),onItemAdd = I(onItemAdd),onItemRemove = I(onItemRemove)))
        if (isTruthy(input$meta_split)) {
          choices <- strsplit(input$meta_split,"@")
          split_anno <- unique(sapply(choices,function(x) x[[2]]))
          split_groups <- sapply(choices,function(x) x[[3]])
          subset <- data$meta_use[split_anno]
          subset[!(subset[,1] %in% split_groups),] <- NA
          if (!identical(subset,split_1())) {
            split_1(subset)
            split_order_1(split_groups)
          }
        } else {
          split_1(NULL)
          split_order_1(NULL)
        }
        meta_choice <- input$gene_split
        split_selected <- if (isTruthy(meta_choice) && check_split(meta_choice)) meta_choice else character(0)
        updateSelectizeInput(session, "gene_split", choices = create_split_list("gene"), selected = split_selected,options=list(maxItems=3,onDropdownOpen = I(onDropdownOpen),onItemAdd = I(onItemAdd),onItemRemove = I(onItemRemove)))
        if (isTruthy(input$gene_split)) {
          choices <- strsplit(input$gene_split,"@")
          split_anno <- unique(sapply(choices,function(x) x[[2]]))
          split_groups <- sapply(choices,function(x) x[[3]])
          subset <- data$meta_use[split_anno]
          subset[!(subset[,1] %in% split_groups),] <- NA
          if (!identical(subset,split_2())) {
            toggle_slider(T)
            split_2(subset)
            split_order_2(split_groups)
          }
        } else {
          toggle_slider(T)
          split_2(NULL)
          split_order_2(NULL)
        }
      })
      
      observeEvent(input$metadata, {
        if (input$metadata != "") {
          subset <- data$meta_use[input$metadata]
          if (!identical(subset,meta_plot_reduct())) {
            meta_plot_reduct(subset)
          }
        }
      },ignoreInit = T)
      
      observeEvent(input$meta_violin, {
        if (input$meta_violin != "All Data" && input$meta_violin != "") {
          subset <- data$meta_use[input$meta_violin]
          if (!identical(subset,meta_plot_violin())) {
            meta_plot_violin(subset)
          }
        }
      },ignoreInit = T)
      
      observeEvent(input$meta_heatmap, {
        if (input$meta_heatmap != "Cells" && input$meta_heatmap != "") {
          subset <- data$meta_use[input$meta_heatmap]
          if (!identical(subset,meta_plot_heatmap())) {
            meta_plot_heatmap(subset)
          }
        }
      },ignoreInit = T)
      
      observeEvent(input$meta_props_1, {
        if (input$meta_props_1 != "") {
          subset <- data$meta_use[input$meta_props_1]
          if (!identical(subset,meta_plot_props_1())) {
            meta_plot_props_1(subset)
          }
        }
      },ignoreInit = T)
      
      observeEvent(input$meta_props_2, {
        if (input$meta_props_2 != "All Data" && input$meta_props_2 != "") {
          subset <- data$meta_use[input$meta_props_2]
          if (!identical(subset,meta_plot_props_2())) {
            meta_plot_props_2(subset)
          }
        }
      },ignoreInit = T)
      
      observeEvent(input$meta_split, {
        if (isTruthy(input$meta_split)) {
          choices <- strsplit(input$meta_split,"@")
          split_anno <- unique(sapply(choices,function(x) x[[2]]))
          split_groups <- sapply(choices,function(x) x[[3]])
          subset <- data$meta_use[split_anno]
          subset[!(subset[,1] %in% split_groups),] <- NA
          if (!identical(subset,split_1())) {
            split_1(subset)
            split_order_1(split_groups)
          }
        } else {
          split_1(NULL)
          split_order_1(NULL)
        }
      },ignoreInit = T,ignoreNULL = F)
      
      observeEvent(input$gene_split, {
        if (isTruthy(input$gene_split)) {
          choices <- strsplit(input$gene_split,"@")
          split_anno <- unique(sapply(choices,function(x) x[[2]]))
          split_groups <- sapply(choices,function(x) x[[3]])
          subset <- data$meta_use[split_anno]
          subset[!(subset[,1] %in% split_groups),] <- NA
          if (!identical(subset,split_2())) {
            toggle_slider(T)
            split_2(subset)
            split_order_2(split_groups)
          }
        } else {
          toggle_slider(T)
          split_2(NULL)
          split_order_2(NULL)
        }
      },ignoreInit = T,ignoreNULL = F)
      
      meta_plot_reduct <- reactiveVal(NULL)
      split_1 <- reactiveVal(NULL)
      split_2 <- reactiveVal(NULL)
      split_order_1 <- reactiveVal(NULL)
      split_order_2 <- reactiveVal(NULL)
      meta_plot_violin <- reactiveVal(NULL)
      meta_plot_heatmap <- reactiveVal(NULL)
      meta_plot_props_1 <- reactiveVal(NULL)
      meta_plot_props_2 <- reactiveVal(NULL)
      
      #--------------------------------------------------
      #Plotting functions
      
      plot_reduction <- function(seurat,data_type,layout_meta,layout_gene,plot_meta,genes_select,labels,plot_settings,density,meta_split,gene_split,meta_order,gene_order) {
        validate(
          need(seurat,""),
          need(data_type,"")
        )
        if (data_type == "Metadata") {
          validate(
            need(plot_meta(),""),
            need(layout_meta,""),
            need(nrow(plot_meta()) == ncol(seurat),"")
          )
          reduct <- seurat@reductions[[layout_meta]]
          dims <- ncol(reduct)
          is_3D(dims == 3)
          reduct_names <- c(paste0(reduct@key,c(1:dims)))
          if (is.null(meta_split())) {
            plot_data <- cbind(Seurat::FetchData(seurat,vars=c(reduct_names)),plot_meta())
            colnames(plot_data) <- c(paste0("dim",c(1:dims)),colnames(plot_meta()))
            selection_list$counts[[paste0(plot_name,"@0")]] <- NULL
          } else {
            plot_data <- cbind(Seurat::FetchData(seurat,vars=c(reduct_names)),plot_meta(),meta_split())
            colnames(plot_data) <- c(paste0("dim",c(1:dims)),colnames(plot_meta()),"split")
            counts <- table(plot_data$split)
            counts_final <- as.vector(counts[match(names(counts),split_order_1())])
            for (x in 1:length(counts_final)) {
              selection_list$counts[[paste0(plot_name,"@",x)]] <- counts_final[x]
            }
          }
          plot_data$cellname <- rownames(plot_data)
          return(create_meta_plot(plot_data,dims,meta_order(),labels,plot_settings))
        } else if (data_type == "Features") {
          validate(
            need(genes_select,""),
            need(layout_gene,"")
          )
          reduct <- seurat@reductions[[layout_gene]]
          dims <- ncol(reduct)
          reduct_names <- c(paste0(reduct@key,c(1:dims)))
          if (is.null(gene_split())) {
            plot_data <- Seurat::FetchData(seurat,vars=c(reduct_names,genes_select))
            colnames(plot_data) <- c(paste0("dim",c(1:dims)),genes_select)
            selection_list$counts[[paste0(plot_name,"@0")]] <- NULL
          } else {
            plot_data <- cbind(Seurat::FetchData(seurat,vars=c(reduct_names,genes_select)),gene_split())
            colnames(plot_data) <- c(paste0("dim",c(1:dims)),genes_select,"split")
            counts <- table(plot_data$split)
            counts_final <- as.vector(counts[match(names(counts),split_order_2())])
            for (x in 1:length(counts_final)) {
              selection_list$counts[[paste0(plot_name,"@",x)]] <- counts_final[x]
            }
          }
          plot_data$cellname <- rownames(plot_data)
          return(create_exp_plot(plot_data,dims,gene_order(),plot_settings,density))
        }
      }
      
      prepare_2D_labels <- function(label_info,subplot=0) {
        refs <- c("x","y")
        if (subplot == 1) {
          refs <- c("x2","y2")
        } else if (subplot == 2) {
          refs <- c("x3","y3")
        } else if (subplot == 3) {
          refs <- c("x4","y4")
        }
        anno_list <- list()
        for(i in 1:nrow(label_info)){
          tmp <- list(x=label_info$x[i], y=label_info$y[i], text=label_info$label[i], xref=refs[1], yref=refs[2], showarrow=F, opacity=0.8, bgcolor="#fcfcff",font=list(size=15))
          anno_list[[i]] <- tmp
        }
        return(anno_list)
      }
      
      meta_plot_2D <- function(plot_data,label_info,plot_settings,color_pal,name,subplot="no_sub",subplot_num=0,showlegend=T,visible=T){
        meta_plot <- plot_ly(plot_data, x = ~dim1, y = ~dim2, customdata = rep(subplot_num,nrow(plot_data)), color = ~color, colors = color_pal, legendgroup= ~color, showlegend = showlegend, opacity = plot_settings$point_transparent,marker=list(size=plot_settings$point_size), unselected=list(marker=list(opacity=0.05)), text = plot_data$color, hoverinfo=if(visible) "text" else "none", type = 'scattergl', mode = 'markers', source = ns('meta_plot'), key = ~cellname) %>% 
          plotly::config(doubleClickDelay = 400,displaylogo = FALSE,scrollZoom = TRUE, modeBarButtons= list(list('drawopenpath','eraseshape'),list('select2d','lasso2d',reduct_select_all,reduct_clear_select),list('zoom2d','pan2d','resetScale2d'))) %>%
          plotly::layout(title = list(text=name,y=0.98,font = list(size = 20)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",hoverdistance=5, margin=list(t=40,b=10,l=20,r=60),legend=list(font = list(size = 14),itemsizing='constant',entrywidth = 0,bgcolor="rgba(0, 0, 0, 0)"),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F, range=if(visible) NULL else c(100,101)),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F,range=if(visible) NULL else c(100,101)),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
          event_register("plotly_legendclick")
        anno_list <- list()
        if (!is.null(label_info)) {
          anno_list <- c(anno_list,prepare_2D_labels(label_info))
        }
        if (subplot != "no_sub") {
          anno_list <- c(anno_list,list(create_anno_reduct(0.5,subplot,F)))
        } else {
          meta_plot <- meta_plot %>% onRender(plot_inputs)
        }
        if (length(anno_list) > 0) {
          meta_plot <- meta_plot %>% layout(annotations=anno_list)
        }
        return(meta_plot)
      }
      
      prepare_3D_labels <- function(label_info) {
        anno_list <- list()
        for(i in 1:nrow(label_info)){
          tmp <- list(x=label_info$x[i],y=label_info$y[i],z=label_info$z[i],text=label_info$label[i],showarrow=F, opacity=0.8, bgcolor="#fcfcff", font=list(size=15))
          anno_list[[i]] <- tmp
        }
        return(anno_list)
      }
      
      meta_plot_3D <- function(plot_data,label_info,plot_settings,color_pal,name,subplot="no_sub",subplot_num=0,scene_num=1,showlegend=T,visible=T){
        meta_plot <- plot_ly(plot_data, x = ~dim1, y = ~dim2, z = ~dim3, customdata = rep(subplot_num,nrow(plot_data)), color = ~color, colors = color_pal, legendgroup= ~color, opacity = plot_settings$point_transparent, marker=list(size=plot_settings$point_size), showlegend = showlegend, text = plot_data$color, hoverinfo=if(visible) "text" else "none", type = 'scatter3d', mode = 'markers', source=ns("meta_plot"),scene = paste0("scene",scene_num), key = ~cellname) %>% 
          plotly::config(doubleClickDelay = 400,displaylogo = FALSE,scrollZoom = TRUE, modeBarButtonsToRemove = list('hoverClosest3d','toImage')) %>%
          plotly::layout(title = list(text=name,y=0.98,font = list(size = 20)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",hoverdistance=5,margin=list(t=40,b=10,l=20,r=60),showlegend = T, legend=list(font = list(size = 14),itemsizing='constant',entrywidth = 0,bgcolor="rgba(0, 0, 0, 0)"),scene=list(xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
          event_register("plotly_legendclick")
        if (!is.null(label_info)) {
          meta_plot <- meta_plot %>% plotly::layout(scene=list(annotations=prepare_3D_labels(label_info)))
        }
        if (subplot == "no_sub") {
          meta_plot <- meta_plot %>% onRender(plot_inputs_3d)
        }
        return(meta_plot)
      }
      
      create_meta_plot <- function(plot_data,dims,split_groups,labels,plot_settings) {
        meta <- plot_data[,dims+1]
        name <- colnames(plot_data)[dims+1]
        groups <- gtools::mixedsort(unique(meta))
        plot_data$color <- factor(as.character(meta),levels=groups)
        if (plot_settings$color_discrete == "rainbow") {
          color_pal <- gg_color_hue(length(groups))
        } else {
          color_pal <- random_colors(length(groups))
        }
        if (is.null(split_groups)) {
          label_info <- NULL
          if (labels) {
            label_info <- get_cluster_labels(plot_data,dims)
            cur_labels[["plot1"]] <- label_info
          }
          cur_visible(unique(plot_data$color))
          if (dims == 2) {
            meta_plot_2D(plot_data,label_info,plot_settings,color_pal,name)
          } else {
            meta_plot_3D(plot_data,label_info,plot_settings,color_pal,name)
          } 
        } else {
          data_list <- lapply(split_groups,function(group) plot_data %>% filter(split == group))
          names(data_list) <- split_groups
          if (dims == 2) {
            plot_list <- lapply(1:length(split_groups), function(group_num) {
              group_name <- split_groups[group_num]
              group_data <- data_list[[as.character(group_name)]]
              label_info <- NULL
              if (labels) {
                label_info <- get_cluster_labels(group_data,dims)
                cur_labels[[paste0("plot",group_num)]] <- label_info
              }
              meta_plot_2D(group_data,label_info,plot_settings,color_pal,name,group_name,group_num,F)
            })
            legend_colors <- plot_data %>% filter(!is.na(split))
            legend_colors <- unique(legend_colors$color)
            cur_visible(legend_colors)
            legend_data <- data.frame(dim1=rep(0,length(legend_colors)),dim2=rep(0,length(legend_colors)),color=legend_colors,cellname=1:length(legend_colors))
            legend_plot <- meta_plot_2D(legend_data,NULL,plot_settings,color_pal,name,"no_sub",0,T,F)
            plot_list <- c(list(legend_plot),plot_list)
            subplot_widths = c(0,1.0)
            if (length(plot_list) == 3) {
              subplot_widths <- c(0,0.5,0.5)
            } else if (length(plot_list) == 4) {
              subplot_widths <- c(0,0.33,0.33,0.33)
            }
            plotly::subplot(plot_list,nrows=1,widths=subplot_widths) %>% onRender(subplot_inputs) %>% event_register("plotly_legendclick")
          } else {
            scene_num <- 0
            if (labels) {
              label_list <- lapply(1:length(split_groups), function(group_num) {
                group_name <- split_groups[group_num]
                group_data <- data_list[[as.character(group_name)]]
                label_info <- get_cluster_labels(group_data,dims)
                cur_labels[[paste0("plot",group_num)]] <- label_info
                prepare_3D_labels(label_info)
              })
            }
            plot_list <- lapply(1:length(split_groups), function(group_num) {
              group_name <- split_groups[group_num]
              group_data <- data_list[[as.character(group_name)]]
              scene_num <<- scene_num + 1
              meta_plot_3D(group_data,NULL,plot_settings,color_pal,name,group_name,group_num,scene_num,F,T)
            })
            legend_colors <- plot_data %>% filter(!is.na(split))
            legend_colors <- unique(legend_colors$color)
            cur_visible(legend_colors)
            legend_data <- data.frame(dim1=rep(0,length(legend_colors)),dim2=rep(0,length(legend_colors)),dim3=rep(0,length(legend_colors)),color=legend_colors,cellname=1:length(legend_colors))
            legend_plot <- meta_plot_3D(legend_data,NULL,plot_settings,color_pal,name,"no_sub",0,length(split_groups)*2+1,T,F)
            plot_list <- c(list(legend_plot),plot_list)
            title_pos <- c(0.5)
            if (length(split_groups) == 2) {
              title_pos <- c(0.2,0.8)
            } else if (length(split_groups) == 3) {
              title_pos <- c(0.12,0.5,0.88)
            }
            title_list <- lapply(1:length(split_groups), function(group_num) {
              group_data <- data_list[[group_num]]
              plot_ly(type="scatter3d",customdata=-1,mode="markers",scene=paste0("scene",length(split_groups)+group_num),visible=T) %>% add_annotations(text=split_groups[group_num],x=title_pos[group_num],y=0.98,z=0.5,showarrow=F, opacity=1,font=list(size=16))              
            })
            meta_plot <- plotly::subplot(c(title_list,plot_list),nrows = 2) %>% onRender(subplot_inputs_3d) %>% event_register("plotly_legendclick")
            if (scene_num == 1) {
              meta_plot <- meta_plot %>% plotly::layout(title = list(text=name,y=0.98,font = list(size = 20)),
                scene = list(annotations=if(labels) label_list[[1]] else NULL,domain=list(x=c(0,1),y=c(0,0.9)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
                scene2 = list(domain=list(x=c(0,1),y=c(0.9,1)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
                scene3 = list(domain=list(x=c(0,0.01),y=c(0,0.01)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F,range=c(100,101)),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F))
              )
            } else if (scene_num == 2) {
              meta_plot <- meta_plot %>%  plotly::layout(title = list(text=name,y=0.98,font = list(size = 20)),
               scene = list(annotations=if(labels) label_list[[1]] else NULL,domain=list(x=c(0,0.5),y=c(0,0.9)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene2 = list(annotations=if(labels) label_list[[2]] else NULL,domain=list(x=c(0.5,1.0),y=c(0,0.9)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene3 = list(domain=list(x=c(0,0.5),y=c(0.9,1.0)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene4 = list(domain=list(x=c(0.5,1),y=c(0.9,1.0)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene5 = list(domain=list(x=c(0,0.01),y=c(0,0.01)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F,range=c(100,101)),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F))
              )  
            } else if (scene_num == 3) {
              meta_plot <- meta_plot %>%  plotly::layout(title = list(text=name,y=0.98,font = list(size = 20)),
               scene = list(annotations=if(labels) label_list[[1]] else NULL,domain=list(x=c(0,0.33),y=c(0,0.9)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene2 = list(annotations=if(labels) label_list[[2]] else NULL,domain=list(x=c(0.33,0.66),y=c(0,0.9)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene3 = list(annotations=if(labels) label_list[[3]] else NULL,domain=list(x=c(0.66,0.99),y=c(0,0.9)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene4 = list(domain=list(x=c(0,0.33),y=c(0.9,1.0)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene5 = list(domain=list(x=c(0.33,0.66),y=c(0.9,1.0)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene6 = list(domain=list(x=c(0.66,0.99),y=c(0.9,1.0)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene7 = list(domain=list(x=c(0,0.01),y=c(0,0.01)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F,range=c(100,101)),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F))
              )
            }
            meta_plot
          }
        }
      }
      
      exp_plot_2D <- function(plot_data,values,plot_settings,color_scale,color_min,color_max,name,subplot="no_sub",subplot_num=0) {
        exp_plot <- plot_ly(plot_data, x = ~dim1, y = ~dim2, customdata = rep(subplot_num,nrow(plot_data)), color = ~values, colors=color_scale,opacity = plot_settings$point_transparent, marker=list(size=plot_settings$point_size,cmin=color_min,cmax=color_max), unselected=list(marker=list(opacity=0.05)), text = values, hovertemplate="%{text:.2f}<extra></extra>", type = 'scattergl', mode = 'markers', source = ns('exp_plot'), key = ~cellname) %>% 
          plotly::config(doubleClickDelay = 400,displaylogo = FALSE,scrollZoom = TRUE,modeBarButtons= list(list('drawopenpath','eraseshape'),list('select2d','lasso2d',reduct_clear_select),list('zoom2d','pan2d','resetScale2d'))) %>%
          plotly::layout(title = list(text=name,y=0.98,font = list(size = 20)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",hoverdistance=5,margin=list(t=40,b=10,l=20,r=0),legend=list(font = list(size = 14),entrywidth = 0,bgcolor="rgba(0, 0, 0, 0)"),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)"))
        if (subplot != "no_sub") {
          exp_plot <- exp_plot  %>% add_annotations(
            text = subplot,
            x = 0.5,
            y = 1,
            yref = "paper",
            xref = "paper",
            xanchor = "center",
            yanchor = "top",
            showarrow = FALSE,
            font = list(size = 16)
          )
        } else {
          exp_plot <- exp_plot %>% onRender(plot_inputs_exp)
        }
        return(exp_plot)
      }
      
      exp_plot_3D <- function(plot_data,values,plot_settings,color_scale,color_min,color_max,name,subplot="no_sub",scene_num=1,subplot_num=0) {
        exp_plot <- plot_ly(plot_data, x = ~dim1, y = ~dim2, z = ~dim3, customdata = rep(subplot_num,nrow(plot_data)), color = ~values, colors=color_scale,opacity = plot_settings$point_transparent, marker=list(size=plot_settings$point_size,cmin=color_min,cmax=color_max), text = values, hovertemplate="%{text:.2f}<extra></extra>", type = 'scatter3d', mode = 'markers', scene = paste0("scene",scene_num), key = ~cellname) %>% 
          plotly::config(doubleClickDelay = 400,displaylogo = FALSE,scrollZoom = TRUE, modeBarButtonsToRemove = list('hoverClosest3d','toImage')) %>%
          plotly::layout(title = list(text=name,y=0.98,font = list(size = 20)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",hoverdistance=5,margin=list(t=40,b=10,l=20,r=30),legend=list(font = list(size = 14),entrywidth = 0,bgcolor="rgba(0, 0, 0, 0)"),scene=list(xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)"))
        if (subplot == "no_sub") {
          exp_plot <- exp_plot %>% onRender(plot_inputs_exp_3d)
        }
        return(exp_plot)
      }
      
      get_density <- function(plot_data,dims) {
        embeds <- plot_data[,1:dims]
        weights <- plot_data[,dims+1]
        if (sum(weights) != 0) {
          dens <- ks::kde(embeds,w = weights / sum(weights) * length(weights))
          if (dims == 2) {
            ix <- findInterval(embeds[, 1], dens$eval.points[[1]])
            iy <- findInterval(embeds[, 2], dens$eval.points[[2]])
            plot_data[,dims+1] <- dens$estimate[cbind(ix, iy)] * 100
          } else if (dims == 3) {
            ix <- findInterval(embeds[, 1], dens$eval.points[[1]])
            iy <- findInterval(embeds[, 2], dens$eval.points[[2]])
            iz <- findInterval(embeds[, 3], dens$eval.points[[3]])
            plot_data[,dims+1] <- dens$estimate[cbind(ix, iy, iz)] * 100
          }
        }
        return(plot_data)
      }
      
      create_exp_plot <- function(plot_data,dims,split_groups,plot_settings,density) {
        name <- colnames(plot_data)[dims+1]
        plot_data <- plot_data %>% arrange(.data[[name]])
        if (is.null(split_groups)) {
          if (density) {
            plot_data <- get_density(plot_data,dims)
          }
          values <- plot_data[,dims+1]
          color_scale <- plot_settings$color_cont
          color_min <- max(0,min(values))
          color_max <- max(values)
          exp_range$min <- color_min
          exp_range$max <- color_max
          if (sum(values) == 0) {
            if (is.function(color_scale)) {
              color_scale <- "gray85"
            } else {
              color_scale <- if (color_scale == "viridis") "#440154FF" else if (color_scale == "plasma") "#0D0887FF"
            }          }
          if (dims == 2) {
            exp_plot_2D(plot_data,values,plot_settings,color_scale,color_min,color_max,name)
          } else {
            exp_plot_3D(plot_data,values,plot_settings,color_scale,color_min,color_max,name)
          }
        } else {
          data_list <- lapply(split_groups,function(group) plot_data %>% filter(split == group))
          if (density) {
            data_list <- lapply(data_list, function(x) get_density(x,dims))
          }
          names(data_list) <- split_groups
          data_min <- sapply(data_list, function(data) min(data[,dims+1]))
          data_max <- sapply(data_list, function(data) max(data[,dims+1]))
          color_min <- max(0,min(data_min))
          color_max <- max(data_max)
          exp_range$min <- color_min
          exp_range$max <- color_max
          if (dims == 2) {
            plot_list <- lapply(1:length(split_groups), function(group_num) {
              group_name <- split_groups[group_num]
              group_data <- data_list[[as.character(group_name)]]
              values <- group_data[,dims+1]
              color_scale <- plot_settings$color_cont
              if (sum(values) == 0) {
                if (is.function(color_scale)) {
                  color_scale <- "gray85"
                } else {
                  color_scale <- if (color_scale == "viridis") "#440154FF" else if (color_scale == "plasma") "#0D0887FF"
                }          
              }
              exp_plot_2D(group_data,values,plot_settings,color_scale,color_min,color_max,name,group_name,group_num)
            })
            plotly::subplot(plot_list,nrows=1) %>% onRender(subplot_inputs_exp)
          } else {
            scene_num <- 0
            plot_list <- lapply(1:length(split_groups), function(group_num) {
              group_name <- split_groups[group_num]
              group_data <- data_list[[as.character(group_name)]]
              values <- group_data[,dims+1]
              color_scale <- plot_settings$color_cont
              if (sum(values) == 0) {
                if (is.function(color_scale)) {
                  color_scale <- "gray85"
                } else {
                  color_scale <- if (color_scale == "viridis") "#440154FF" else if (color_scale == "plasma") "#0D0887FF"
                }          
              }
              scene_num <<- scene_num + 1
              exp_plot_3D(group_data,values,plot_settings,color_scale,color_min,color_max,name,group_name,scene_num,group_num)
            })
            title_pos <- c(0.5)
            if (length(split_groups) == 2) {
              title_pos <- c(0.2,0.8)
            } else if (length(split_groups) == 3) {
              title_pos <- c(0.12,0.5,0.88)
            }
            title_list <- lapply(1:length(split_groups), function(group_num) {
              group_data <- data_list[[group_num]]
              plot_ly(type="scatter3d",mode="markers",scene=paste0("scene",length(split_groups)+group_num),visible=T) %>% add_annotations(text=split_groups[group_num],x=title_pos[group_num],y=0.98,z=0.5,showarrow=F, opacity=1,font=list(size=16))              
            })
            exp_plot <- plotly::subplot(c(title_list,plot_list),nrows = 2) %>% onRender(subplot_inputs_exp_3d)
            if (scene_num == 1) {
              exp_plot <- exp_plot %>% plotly::layout(title = list(text=name,y=0.98,font = list(size = 20)),
                scene = list(domain=list(x=c(0,1),y=c(0,0.9)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
                scene2 = list(domain=list(x=c(0,1),y=c(0.9,1)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F))
              )
            } else if (scene_num == 2) {
              exp_plot <- exp_plot %>%  plotly::layout(title = list(text=name,y=0.98,font = list(size = 20)),
                scene = list(domain=list(x=c(0,0.5),y=c(0,0.9)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
                scene2 = list(domain=list(x=c(0.5,1.0),y=c(0,0.9)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
                scene3 = list(domain=list(x=c(0,0.5),y=c(0.9,1.0)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
                scene4 = list(domain=list(x=c(0.5,1),y=c(0.9,1.0)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F))
                                                                       
              )  
            } else if (scene_num == 3) {
              exp_plot <- exp_plot %>%  plotly::layout(title = list(text=name,y=0.98,font = list(size = 20)),
               scene = list(domain=list(x=c(0,0.33),y=c(0,0.9)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene2 = list(domain=list(x=c(0.33,0.66),y=c(0,0.9)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene3 = list(domain=list(x=c(0.66,0.99),y=c(0,0.9)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene4 = list(domain=list(x=c(0,0.33),y=c(0.9,1.0)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene5 = list(domain=list(x=c(0.33,0.66),y=c(0.9,1.0)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene6 = list(domain=list(x=c(0.66,0.99),y=c(0.9,1.0)), aspectmode='cube',xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F))
              )
            }
            exp_plot
          }
        }
      }
      
      get_cluster_labels <- function(plot_data,dims) {
        header <- colnames(plot_data)
        coords <- plot_data %>% group_by(pick(dims+1)) %>% summarise(x=median(get(header[1])),y=median(get(header[2])))
        coords <- coords[,c(2,3,1)]
        colnames(coords) <- c("x","y","label")
        repels <- repel::repel_text(coords)
        if (dims == 3) {
          dim3 <- plot_data %>% group_by(pick(dims+1)) %>% summarise(z=median(get(header[3])))
          repels$z <- dim3$z
        }
        return(repels)
      }
      
      plot_violin <- function(seurat,genes_select,meta_select,plot_meta,gene_set,plot_settings) {
        validate(
          need(seurat,""),
          need(genes_select,"")
        )
        y_name <- if (gene_set == "Other features") "Value" else "Expression"
        if (meta_select == "All Data") {
          plot_data <- Seurat::FetchData(seurat,vars=c(genes_select))
          colnames(plot_data) <- c("gene")
          plot_data$cellname <- rownames(plot_data)
          plot_ly(plot_data, type = "violin", y = ~gene, spanmode="hard", box=list(visible=T),meanline=list(visible=T),source = ns('plot'), key= ~cellname) %>%
            plotly::layout(title=list(text=genes_select,y=0.98,font = list(size = 20)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=40,b=10,l=90,r=20),yaxis=list(title=y_name,zeroline=F),xaxis=list(showticklabels = F),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
            plotly::config(doubleClickDelay = 400,displaylogo = FALSE,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d')))
        } else {
          validate(
            need(plot_meta(),""),
            need(nrow(plot_meta()) == ncol(seurat),"")
          )
          plot_data <- cbind(Seurat::FetchData(seurat,vars=c(genes_select)),plot_meta())
          colnames(plot_data) <- c("gene","meta")
          order <- gtools::mixedsort(unique(plot_data$meta))
          plot_data$meta <- factor(plot_data$meta,levels=order)          
          groups <- as.vector(unique(plot_data$meta))
          groups_sorted <- gtools::mixedsort(groups)
          if (plot_settings$color_discrete == "rainbow") {
            color_pal <- gg_color_hue(length(groups))
          } else {
            color_pal <- random_colors(length(groups))
          }          
          plot_data$cellname <- rownames(plot_data)
          plot_ly(plot_data, type = "violin", x = ~meta, y = ~gene, split = ~meta, color= ~meta, colors=color_pal, spanmode="hard", box=list(visible=T),meanline=list(visible=T),source = ns('plot'), key= ~cellname) %>%
            plotly::layout(title=list(text=genes_select,y=0.98,font = list(size = 20)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=40,b=10,l=90,r=60),legend=list(font = list(size = 14),entrywidth = 0,bgcolor="rgba(0, 0, 0, 0)"),yaxis=list(title=y_name,zeroline=F),xaxis=list(title=meta_select),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
            plotly::config(doubleClickDelay = 400,displaylogo = FALSE,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d')))
        }
      }
      
      plot_heatmap <- function(seurat,plot_type,geneset,set_name,meta_select,plot_meta,flip,scaling,clustering,plot_settings) {
        if (plot_type == "Heatmap") {
          validate(
            need(seurat,""),
            need(geneset,""),
            need(meta_select,"")
          )
          if (meta_select == "Cells") {
            plot_data <- Seurat::FetchData(seurat,vars=c(geneset))
            color_type <- plot_settings$color_cont
            if (scaling) {
              plot_data <- scale(plot_data) %>% as.matrix(plot_data) 
              plot_data[is.nan(plot_data)] <- -3
              plot_data <- MinMax(plot_data, min = -3,max = 3)
              color_type <- plot_settings$color_scaled
            }
            if (nrow(plot_data) >= 2 && clustering) {
              x_order <- hclust(dist(plot_data))$order
              plot_data <- plot_data[x_order,,drop=F]
            }
            if (ncol(plot_data) >= 2 && clustering) {
              y_order <- hclust(dist(t(plot_data)))$order
              plot_data <- plot_data[,y_order,drop=F]
            }
            if (!flip) {
              plot_data <- t(plot_data)
              plot_ly(x=colnames(plot_data),y=rownames(plot_data),z=plot_data,colors=color_type,type="heatmap",colorbar=list(len=200,lenmode="pixels",thickness=28,title=list(text="Avg Exp"))) %>% 
                plotly::layout(title=list(text=set_name,font = list(size = 20)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=30,b=25,l=80,r=45),yaxis=list(title=list(text="Genes",standoff=8)),xaxis=list(title="Cells",showticklabels=F,autotypenumbers = 'strict'),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
                plotly::config(doubleClickDelay = 400,displaylogo = FALSE,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d')))
            } else {
              plot_ly(x=colnames(plot_data),y=rownames(plot_data),z=plot_data,colors=plot_settings$color_scaled,type="heatmap",colorbar=list(len=200,lenmode="pixels",thickness=28,title=list(text="Avg Exp"))) %>% 
                plotly::layout(title=list(text=set_name,font = list(size = 20)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=30,b=25,l=80,r=45),yaxis=list(title="Cells",showticklabels=F,autotypenumbers = 'strict'),xaxis=list(title=list(text="Genes",standoff=8)),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
                plotly::config(doubleClickDelay = 400,displaylogo = FALSE,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d')))
            }
          } else {
            validate(
              need(plot_meta(),""),
              need(nrow(plot_meta()) == ncol(seurat),"")
            )
            plot_data <- cbind(Seurat::FetchData(seurat,vars=c(geneset)),plot_meta())
            fix_values <- F
            color_type <- plot_settings$color_cont
            if (length(unique(plot_meta()[,1])) > 1) {
              if (scaling) {
                plot_means <- plot_data %>% group_by(get(meta_select)) %>% summarise(across(all_of(geneset),get_avg_exp)) %>% mutate(across(all_of(geneset), ~ as.numeric(scale(log1p(.x))))) %>% mutate(across(all_of(geneset), ~ replace(.x, is.nan(.x), -3))) %>% data.frame()
                fix_values <- T
                color_type <- plot_settings$color_scaled
              } else {
                plot_means <- plot_data %>% group_by(get(meta_select)) %>% summarise(across(all_of(geneset),get_avg_exp)) %>% data.frame()
              }
            } else {
              plot_means <- plot_data %>% group_by(get(meta_select)) %>% summarise(across(all_of(geneset),get_avg_exp)) %>% data.frame()
            }
            rownames(plot_means) <- plot_means[,1]
            plot_means <- as.matrix(plot_means[,2:ncol(plot_means)])
            if (fix_values) {
              plot_means <- Seurat::MinMax(plot_means,-3,3)
            }
            if (clustering) {
              if (nrow(plot_means) >= 2) {
                x_order <- hclust(dist(plot_means))$order
                plot_means <- plot_means[x_order,,drop=F]
              }
              if (ncol(plot_means) >= 2) {
                y_order <- hclust(dist(t(plot_means)))$order
                plot_means <- plot_means[,y_order,drop=F]
                geneset <- geneset[y_order]
              }
            }
            if (!flip) {
              plot_means <- t(plot_means)
              plot_ly(x=colnames(plot_means),y=geneset,z=plot_means,colors=color_type,type="heatmap",colorbar=list(len=200,lenmode="pixels",thickness=28,title=list(text="Avg Exp"))) %>% 
                plotly::layout(title=list(text=set_name,font = list(size = 20)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=30,b=25,l=100,r=45),yaxis=list(title=list(text="Genes",standoff=8)),xaxis=list(title=list(text=meta_select,standoff=8),autotypenumbers = 'strict'),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
                plotly::config(doubleClickDelay = 400,displaylogo = FALSE,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d')))
              
            } else {
              plot_ly(x=geneset,y=rownames(plot_means),z=plot_means,colors=color_type,type="heatmap",colorbar=list(len=200,lenmode="pixels",thickness=28,title=list(text="Avg Exp"))) %>% 
                plotly::layout(title=list(text=set_name,font = list(size = 20)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=30,b=25,l=100,r=45),yaxis=list(title=list(text=meta_select,standoff=8),autotypenumbers = 'strict'),xaxis=list(title=list(text="Genes",standoff=8)),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
                plotly::config(doubleClickDelay = 400,displaylogo = FALSE,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d')))
            }
          }
        } else {
          validate(
            need(seurat,""),
            need(geneset,""),
            need(meta_select,""),
            need(meta_select != "Cells",""),
            need(plot_meta(),""),
            need(nrow(plot_meta()) == ncol(seurat),"")
          )
          plot_data_raw <- cbind(Seurat::FetchData(seurat,vars=c(geneset)),plot_meta())
          color_type <- plot_settings$color_cont
          fix_values <- F
          if (length(unique(plot_meta()[,1])) > 1) {
            if (scaling) {
              plot_means <- plot_data_raw %>% group_by(get(meta_select)) %>% summarise(across(all_of(geneset),get_avg_exp)) %>% mutate(across(all_of(geneset), ~ as.numeric(scale(log1p(.x))))) %>% mutate(across(all_of(geneset), ~ replace(.x, is.nan(.x), -3)))
              color_type <- plot_settings$color_scaled
              fix_values <- T
            } else {
              plot_means <- plot_data_raw %>% group_by(get(meta_select)) %>% summarise(across(all_of(geneset),get_avg_exp))
            }
          } else {
            plot_means <- plot_data_raw %>% group_by(get(meta_select)) %>% summarise(across(all_of(geneset),get_avg_exp))
          }
          plot_percents <- plot_data_raw %>% group_by(get(meta_select)) %>% summarise(across(all_of(geneset),get_percent_exp)) %>% pivot_longer(cols=all_of(geneset), names_to="Gene")
          plot_means_final <- plot_means %>% pivot_longer(cols=all_of(geneset), names_to="Gene")
          plot_data <- cbind(plot_means_final,plot_percents$value)
          colnames(plot_data) <- c("Meta","Gene","Expression","Percent")
          if (clustering) {
            plot_means <- as.data.frame(plot_means)
            rownames(plot_means) <- plot_means[,1]
            plot_means <- plot_means[,2:ncol(plot_means),drop=F]
            meta_list <- rownames(plot_means)
            gene_list <- colnames(plot_means)
            if (nrow(plot_means) >= 2) {
              order <- hclust(dist(plot_means))$order
              meta_list <- meta_list[order]
            }
            if (ncol(plot_means) >= 2) {
              order <- hclust(dist(t(plot_means)))$order
              gene_list <- gene_list[order]
            }
            plot_data$Meta <- factor(plot_data$Meta, levels = meta_list) 
            plot_data$Gene <- factor(plot_data$Gene, levels = gene_list) 
            plot_data <- plot_data[order(plot_data$Meta, plot_data$Gene), ]
          }
          plot_data$Meta <- as.character(plot_data$Meta)
          plot_data$Gene <- as.character(plot_data$Gene)
          plot_data$Color <- if (fix_values) scales::squish(plot_data$Expression,range = c(-3,3)) else plot_data$Expression
          plot_data$Percent <- plot_data$Percent*100
          plot_data$Size <- plot_data$Percent
          plot_data$Size[plot_data$Percent < 1.0] <- NA
          meta_order <- plot_data$Meta
          if (!scaling) {
            meta_order <- gtools::mixedsort(meta_order)
          }
          if (!flip) {
            plot_ly(plot_data,x=~Meta,y=~Gene,color=~Color,colors=color_type,size=~Size,sizes=c(5,20),type="scatter",mode = "markers",marker = list(sizemode = "diameter"),hoverinfo = 'text', hovertext = paste0("x: ",plot_data$Meta,"\n","y: ",plot_data$Gene,"\n","z: ",round(plot_data$Color,2),", ",round(plot_data$Percent,2),"%")) %>%
              plotly::layout(title=list(text=set_name,font = list(size = 20)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=30,b=25,l=100,r=45),xaxis=list(title=list(text=meta_select,standoff=8),showgrid=F,zeroline=T,autotypenumbers = 'strict',categoryorder="array",categoryarray=meta_order),yaxis=list(title=list(text="Genes",standoff=8),showgrid=F,zeroline=T,categoryorder="array",categoryarray=plot_data$Gene),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
              plotly::config(doubleClickDelay = 400,displaylogo = FALSE,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d'))) %>% 
              plotly::colorbar(len=200,lenmode="pixels",thickness=28,title=list(text="Avg Exp"))
          } else {
            plot_ly(plot_data,x=~Gene,y=~Meta,color=~Color,colors=color_type,size=~Size,sizes=c(5,20),type="scatter",mode = "markers",marker = list(sizemode = "diameter"),hoverinfo = 'text', hovertext = paste0("x: ",plot_data$Gene,"\n","y: ",plot_data$Meta,"\n","z: ",round(plot_data$Color,2),", ",round(plot_data$Percent,2),"%")) %>%
              plotly::layout(title=list(text=set_name,font = list(size = 20)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=30,b=25,l=100,r=45),xaxis=list(title=list(text="Genes",standoff=8),showgrid=F,zeroline=T,categoryorder="array",categoryarray=plot_data$Gene),yaxis=list(title=list(text=meta_select,standoff=8),showgrid=F,zeroline=T,autotypenumbers = 'strict',categoryorder="array",categoryarray=meta_order),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
              plotly::config(doubleClickDelay = 400,displaylogo = FALSE,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d'))) %>% 
              plotly::colorbar(len=200,lenmode="pixels",thickness=28,title=list(text="Avg Exp"))
          }
        }
      }
      
      get_avg_exp <- function(data) {
        return(mean(expm1(data)))
      }
      
      get_percent_exp <- function(data) {
        return(length(data[data > 0]) / length(data))
      }
      
      plot_props <- function(meta_1,meta_2,plot_meta_1,plot_meta_2,plot_settings) {
        validate(
          need(meta_1,""),
          need(meta_2,"")
        )
        if (meta_2 == "All Data") {
          validate(
            need(plot_meta_1(),"")
          )
          counts <- table(plot_meta_1()[,1])
          props <- counts/nrow(plot_meta_1())
          props <- data.frame(props) %>% filter(Freq != 0)
          counts <- data.frame(counts) %>% filter(Freq != 0)
          groups <- as.vector(props[,1])
          groups_sorted <- gtools::mixedsort(groups)
          props$color <- factor(groups,levels=groups_sorted)
          if (plot_settings$color_discrete == "rainbow") {
            color_pal <- gg_color_hue(length(groups))
          } else {
            color_pal <- random_colors(length(groups))
          }          
          props$Freq <- props$Freq*100
          props$Count <- counts$Freq
          props <- props %>% arrange(color)
          plot_ly(props, labels = ~color, values = ~Freq, marker=list(colors=color_pal), type = 'pie', title=list(position="top_center"),sort=T,pull=0.0,textposition="inside",insidetextfont=list(color="white"),hoverinfo = 'text', hovertext = paste0(props$color,"\n",round(props$Freq,1),"%\n", props$Count," cells")) %>%           
            plotly::layout(title=list(text=meta_1,y=0.98,font=list(size=20)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=55,b=20,l=80,r=60),legend=list(font = list(size = 14),bgcolor="rgba(0, 0, 0, 0)",traceorder="normal"),yaxis=list(title="",zeroline=F,visible=F),showlegend = T,modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
            plotly::config(doubleClickDelay = 400,displaylogo = FALSE,modeBarButtons= list(list()))
          #plot_ly(props, x= ~Freq, color= ~color, colors=color_pal, type= "bar", orientation="h", hoverinfo = 'text', hovertext = paste0(props$color,"\n",round(props$Freq,1),"%\n", props$Count," cells")) %>%
          #  plotly::layout(title=list(text=meta_1,y=0.98,font=list(size=20)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=40,b=10,l=80,r=20),legend=list(font = list(size = 14),traceorder="normal"),barmode= "stack",yaxis=list(title="",zeroline=F,visible=F),xaxis=list(title="",zeroline=F,visible=F),showlegend = T,modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
          #  plotly::config(doubleClickDelay = 400,displaylogo = FALSE,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d')))
        } else {
          validate(
            need(plot_meta_1(),""),
            need(plot_meta_2(),"")
          )
          counts <- table(plot_meta_1()[,1],plot_meta_2()[,1])
          props <- prop.table(counts,margin=2)
          props <- data.frame(props) %>% filter(Freq != 0)
          counts <- data.frame(counts) %>% filter(Freq != 0)
          props$Freq <- props$Freq * 100
          props$Count <- counts$Freq
          order <- gtools::mixedsort(unique(as.vector(props$Var2)))
          props$Var2 <- factor(props$Var2,levels=order)
          order <- gtools::mixedsort(unique(as.vector(props$Var1)))
          props$Var1 <- factor(props$Var1,levels=order)
          color_pal <- gg_color_hue(length(order))
          plot_ly(props, x= ~Var2, y= ~Freq, color= ~Var1, colors=color_pal, type= "bar", hoverinfo = 'text', hovertext = paste0(props$Var1,"\n",round(props$Freq,1),"%\n", props$Count," cells")) %>%
            plotly::layout(title=list(text="",y=0.98,font=list(size=20)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",barmode= "stack",margin=list(t=20,b=10,l=100,r=50),legend=list(font = list(size = 14),entrywidth = 0,bgcolor="rgba(0, 0, 0, 0)",traceorder="normal"),yaxis=list(title=list(text=meta_1,font=list(size=18))),xaxis=list(title=list(text=meta_2,font=list(size=18))),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)"),showlegend = T) %>%
            plotly::config(doubleClickDelay = 400,displaylogo = FALSE,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d')))
        }
      }
      
      plot_volcano <- function(data_type,plot_type,plot_settings) {
        if (data_type == "Markers") {
          validate(
            need(markers(),"")
          )
          data_use <- markers()
        } else {
          validate(
            need(degs(),"")
          )
          data_use <- degs()
        }
        if (plot_type == "Volcano") {
          signif <- as.numeric(data_use$`p-val`)
          p_val <- -log10(signif)
          p_val[p_val > 300] <- 300
          fc <- data_use$log2FC
          fc_range <- max(abs(fc)) + 0.5
          sig_range <- max(p_val) + 5
          color <- rep(0,nrow(data_use))
          volcano_x1(-0.5)
          volcano_x2(0.5)
          volcano_y(10)
          filter_sig <- p_val >= 10
          filter_neg <- fc <= -0.5
          filter_pos <- fc >= 0.5
          color[filter_sig & filter_neg] <- -1
          color[filter_sig & filter_pos] <- 1
          gene <- data_use$gene
          plot_data <- data.frame(p_val,fc,signif,gene)
          anno_data <- plot_data[color != 0,]
          plot_ly(plot_data, x = ~fc, y = ~p_val, text = ~gene, opacity = plot_settings$point_transparent,marker=list(color=color,colorscale=list(c(-1, "rgb(224, 40, 40)"),c(0, "rgb(204, 204, 204)"), c(1, "rgb(40, 58, 224)")),size=plot_settings$point_size + 4,cmin=-1,cmax=1), hovertemplate=paste('<b>%{text}</b><br>','FC: %{x:.2f}<br>','Sig: %{y:.2f}','<extra></extra>'),type = 'scattergl', mode = 'markers',source = ns('volcano_plot')) %>% 
            plotly::config(edits = list(shapePosition = TRUE),doubleClickDelay = 400,displaylogo = FALSE,scrollZoom = TRUE,scrollZoom = TRUE,modeBarButtons= list(list('zoom2d','pan2d','resetScale2d'))) %>%
            plotly::layout(plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",hoverdistance=10, margin=list(t=30,b=20,l=50,r=20),xaxis=list(title="log2(Fold Change)",range=c(-fc_range,fc_range)),yaxis=list(title="-10log(Adjusted p-value)",range=c(-1,sig_range)),legend = list(),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)"),
              shapes = list(
              list(type = "line", x0 = -0.5, x1 = -0.5, y0 = 0, y1 = 1, yref = "paper",layer="above",opacity=0.3,line=list(dash="dash",color="blue")),
              list(type = "line", x0 = 0.5, x1 = 0.5, y0 = 0, y1 = 1, yref = "paper",layer="above",opacity=0.3,line=list(dash="dash",color="blue")),
              list(type = "line", x0 = 0, x1 = 1, xref="paper", y0 = 10, y1 = 10,layer="above",opacity=0.3,line=list(dash="dash",color="blue"))
              )
            ) %>% add_annotations(x=anno_data$fc, y=anno_data$p_val, text=anno_data$gene, xref="x", yref="y", showarrow=F, yshift=15, opacity=0.8,font=list(size=13))
        } else {
          gene <- data_use$gene
          m_val <- data_use$log2FC
          a_val <- log2(data_use$avg.1 * data_use$avg.2) / 2
          a_range <- max(abs(a_val)) + 0.5
          p_val <- as.numeric(data_use$`p-val`)
          color <- -log10(p_val)
          color[color > 300] <- 300
          plot_data <- data.frame(m_val,a_val,p_val,gene,color)
          opacity <- rep(0.2,nrow(data_use))
          ma_y1(0.5)
          ma_y2(-0.5)
          ma_x(0.5)
          filter_avg <- a_val >= 0.5
          filter_neg <- m_val <= -0.5
          filter_pos <- m_val >= 0.5
          opacity[filter_avg & filter_neg] <- 1
          opacity[filter_avg & filter_pos] <- 1
          anno_data <- plot_data[opacity != 0.2,]
          colorscale <- "Viridis"
          plot_ly(plot_data, x = ~a_val, y = ~m_val, customdata = ~p_val, text = ~gene,marker=list(color = color,colorscale=colorscale,opacity=opacity,size=plot_settings$point_size + 4,reversescale=T), hovertemplate=paste('<b>%{text}</b><br>','Exp: %{x:.2f}<br>','FC: %{y:.2f}<br>','Sig: %{customdata}','<extra></extra>'),type = 'scattergl', mode = 'markers', source = ns('ma_plot')) %>% 
            plotly::config(edits = list(shapePosition = TRUE),doubleClickDelay = 400,displaylogo = FALSE,scrollZoom = TRUE,scrollZoom = TRUE,modeBarButtons= list(list('zoom2d','pan2d','resetScale2d'))) %>%
            plotly::layout(plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",hoverdistance=10, margin=list(t=30,b=20,l=50,r=20),xaxis=list(title="log2(Mean Expression)",range=c(-a_range,a_range)),yaxis=list(title="log2(Fold Change)"),legend = list(),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)"),
             shapes = list(
               list(type = "line", x0 = 0, x1 = 1,xref = "paper", y0 = 0.5, y1 = 0.5,layer="above",opacity=0.3,line=list(dash="dash",color="blue")),
               list(type = "line", x0 = 0, x1 = 1, xref = "paper",y0 = -0.5, y1 = -0.5,layer="above",opacity=0.3,line=list(dash="dash",color="blue")),
               list(type = "line", x0 = 0.5, x1 = 0.5, y0 = 0, y1 = 1,yref = "paper",layer="above",opacity=0.3,line=list(dash="dash",color="blue"))
             )
            ) %>% add_annotations(x=anno_data$a_val, y=anno_data$m_val, text=anno_data$gene, xref="x", yref="y", showarrow=F, yshift=15, opacity=0.8,font=list(size=13))
        }
      }
        
      
      observeEvent(plot_type(), {
        lapply(plot_types_all, function(x) shinyjs::hide(paste0(x,"_div")))
        shinyjs::show(paste0(plot_type(),"_div"))
        if (plot_type() == 'reduction') {
          output$plot <- renderPlotly({plot_reduction(data$use,input$data_type,input$layout_meta,input$layout_gene,meta_plot_reduct,input$gene_exp,input$labels,plot_settings,input$density,split_1,split_2,split_order_1,split_order_2)})
        }
        else if (plot_type() == 'violin') {
          reset_select()
          toggle_slider(F)
          output$plot <- renderPlotly({plot_violin(data$use,input$gene_violin,input$meta_violin,meta_plot_violin,input$violin_gene_set,plot_settings)})
        }
        else if (plot_type() == 'heatmap') {
          reset_select()
          toggle_slider(F)
          output$plot <- renderPlotly({plot_heatmap(data$use,input$heatmap_type,genes_heatmap(),input$heatmap_gene_set,input$meta_heatmap,meta_plot_heatmap,input$flip_heatmap,input$scale_heatmap,input$cluster_heatmap,plot_settings)})
        }
        else if (plot_type() == 'props') {
          reset_select()
          toggle_slider(F)
          output$plot <- renderPlotly({plot_props(input$meta_props_1,input$meta_props_2,meta_plot_props_1,meta_plot_props_2,plot_settings)})
        }
        else if (plot_type() == 'volcano') {
          reset_select()
          toggle_slider(F)
          output$plot <- renderPlotly({plot_volcano(input$volcano_data,input$volcano_type,plot_settings)})
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
      observeEvent(input$volcano, {
        if (plot_type() != "volcano") {
          shinyjs::runjs(paste0("$('#",ns(plot_type())," > img').attr('src', 'images/",plot_type(),"_off.png');"," $('#",ns('volcano')," > img').attr('src', 'images/volcano.png');"))
          plot_type('volcano')
        }
      })
      observeEvent(input$box$maximized, {
        if (input$box$maximized) {
          js_call <- paste0("setTimeout(() => {$('#",ns('plot'),"').css('height','88vh');}, 100);", " $('#",ns('plot'),"').trigger('resize');")
          shinyjs::runjs(js_call)
        } else {
          js_call <- paste0("setTimeout(() => {$('#",ns('plot'),"').css('height','76vh');}, 100);", " $('#",ns('plot'),"').trigger('resize');")
          shinyjs::runjs(js_call)
        }
      }, ignoreInit = T)
      
      observeEvent(input$download_plot, {
        showModal(modalDialog(
          title = "Export plot",
          easyClose = T,
          size="s",
          textInput(ns("export_name"),label="Name",value=paste0("mona-",substr(as.character(Sys.Date()),6,10))),
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