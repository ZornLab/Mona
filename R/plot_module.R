plotUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("render_plot"))
}

plotServer <- function(id,num_plots,plot_remove,cur_selection,selection_list,sets=NULL,plot_settings=NULL,dataset=NULL,markers=NULL,degs=NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      default_font <- '"Source Sans Pro",-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,"Helvetica Neue",Arial,sans-serif,"Apple Color Emoji","Segoe UI Emoji","Segoe UI Symbol"'
      
      fetch_data <- function(meta=NULL,genes=NULL,reduct=NULL,ranks=NULL) {
        if (isTruthy(meta)) {
          return(dataset$meta[dataset$subset,meta])
        } else if (isTruthy(genes)) {
          return(as.matrix(dataset$exp[dataset$subset,genes,drop=F]))
        } else if (isTruthy(reduct)) {
          return(dataset$reduct[[reduct]][dataset$subset,,drop=F])
        } else if (isTruthy(ranks)) {
          return(as.matrix(dataset$ranks[dataset$subset,ranks,drop=F]))
        }
      }
      
      generate_colors <- function(type,n) {
        if (type == "classic") {
          hcl(h = seq(15, 375, length = n + 1), c = 100, l = 65)[1:n]
        } else if (type == "classic-random") {
          set.seed(123)
          sample(hcl(h = seq(15, 375, length = n + 1), c = 100, l = 65)[1:n])
        } else if (type == "bright") {
          rainbow(n, s = 0.6, v = 0.98)
        } else if (type == "bright-random") {
          set.seed(123)
          sample(rainbow(n, s = 0.6, v = 0.98))
        }
      }
      
      colors_as_list <- function(colors) {
        if (is.function(colors)) {
          if (colors(0)[[1]] == 217) {
            colorscale <- list(c(0, "#D9D9D9"),c(0.33,"#00008B"),c(0.66,"#63B8FF"),c(1,"#00FFFF"))
          } else if (colors(0)[[1]] == 28) {
            colorscale <- list(c(0,"#1C86EE"),c(0.5,"#FCFCFC"),c(1,"#FF3030"))
          } else if (colors(0)[[1]] == 191) {
            colorscale <- list(c(0,"#BF3EFF"),c(0.5,"#333333"),c(1,"#F1F708"))
          }
        } else {
          if (colors == "viridis") {
            colorscale <- "Viridis"
          } else if (colors == "plasma") {
            colorscale <- list(c(0,"#0D0887FF"),c(0.1,"#42049EFF"),c(0.2,"#6A00A8FF"),c(0.3,"#900DA4FF"),c(0.4,"#B12A90FF"),c(0.5,"#CC4678FF"),c(0.6,"#E16462FF"),c(0.7,"#F1844BFF"),c(0.8,"#FCA636FF"),c(0.9,"#FCCE25FF"),c(1.0,"#F0F921FF"))
          } else {
            colorscale <- list(c(0,colors),c(1,colors))
          }
        }
        return(colorscale)
      }
      
      fix_order <- function(groups) {
        groups <- as.character(groups)
        if ("Undefined" %in% groups) {
          groups <- c(groups[groups != "Undefined"],"Undefined")
        }
        return(groups)
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
      
      get_scores <- function(features) {
        validate(
          need(length(features) > 1,"")
        )
        return(suppressWarnings(as.vector(ScoreSignatures_UCell(features=list(features),precalc.ranks = t(fetch_data(ranks=features))))))
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
          var subplots = $(gd).find('.draglayer').children();
          Shiny.setInputValue('",ns("plot_rendered"),"',id,{priority: 'event'})
          for (var i = 1; i < subplots.length; i++) {
            (function(index) {
              subplots[index].addEventListener('click', function() {
                Shiny.setInputValue('subplot_clicked',id + '@' + index,{priority: 'event'})
              })
            })(i);
          }
        }")
      
      subplot_inputs_null <- paste0("
        function(el, x){
          var id = el.getAttribute('id');
          var gd = document.getElementById(id);
          var subplots = $(gd).find('.draglayer').children();
          Shiny.setInputValue('",ns("plot_rendered"),"',id,{priority: 'event'})
          for (var i = 0; i < subplots.length; i++) {
            (function(index) {
              subplots[index].addEventListener('click', function() {
                var new_index = index + 1;
                Shiny.setInputValue('subplot_clicked',id + '@' + new_index,{priority: 'event'})
              })
            })(i);
          }
        }")
      
      subplot_inputs_exp <- paste0("
        function(el, x){
          var id = el.getAttribute('id');
          var gd = document.getElementById(id);
          var subplots = $(gd).find('.draglayer').children()
          var colorbars = document.querySelectorAll('#' + id + ' .colorbar');
          Shiny.setInputValue('",ns("show_slider"),"',id,{priority: 'event'})
          Shiny.setInputValue('",ns("plot_rendered"),"',id,{priority: 'event'})
          for (var i = 0; i < subplots.length; i++) {
            (function(index) {
              var subplot = index + 1
              subplots[index].addEventListener('click', function() {
                Shiny.setInputValue('subplot_clicked',id + '@' + subplot,{priority: 'event'})
              })
              if (colorbars[index] !== undefined) {
                colorbars[index].style.display='none';
              }
            })(i);
          }
        }")
      
      plot_inputs_exp_3d <- paste0("
        function(el, x){
          var id = el.getAttribute('id');
          var colorbars = document.querySelectorAll('#' + id + ' .colorbar')[0].style.display='none';
          Shiny.setInputValue('",ns("show_slider_3d"),"',id,{priority: 'event'})
          Shiny.setInputValue('",ns("plot_rendered"),"',id,{priority: 'event'})
        }")
      
      subplot_inputs_exp_3d <- paste0("
        function(el, x){
          var id = el.getAttribute('id');
          var gd = document.getElementById(id);
          var subplots = $(gd).find('.gl-container').children()
          var colorbars = document.querySelectorAll('#' + id + ' .colorbar')
          Shiny.setInputValue('",ns("show_slider_3d"),"',id,{priority: 'event'})
          Shiny.setInputValue('",ns("plot_rendered"),"',id,{priority: 'event'})
          for (var i = 0; i < subplots.length; i++) {
            (function(index) {
              if (colorbars[index] !== undefined) {
                colorbars[index].style.display='none';
              }
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
      dropdown_open <- function() {
        'function(el){
          setTimeout(function(){
            var selected = $(el).parent().find("div.item")
            if(selected.length !== 0) {
              var meta = selected.attr("data-value").split("@")[1];
              var unselected = el[0].querySelectorAll(".optgroup:not([data-group=\'"+meta+"\'])");
              $(unselected).hide()
            } else {
              var opts = el[0].querySelectorAll(".optgroup .option")
              $(opts).hide()
            }
          }, 0);
        }'
      }
      
      item_add <- function(type) {
        paste0('function(val,item){
          setTimeout(function(){
            var meta = val.split("@")[1];
            var plot = val.split("@")[0];
            var selectize = $("#"+plot+"-',type,'_split");
            var unselected = selectize[0].nextElementSibling.querySelectorAll(".optgroup:not([data-group=\'"+meta+"\'])");
            $(unselected).hide();
          }, 0);
        }')
      }
      
      item_remove <- function(type) {
        paste0('function(val,$item){
          setTimeout(function(){
            var meta = val.split("@")[1];
            var plot = val.split("@")[0];
            var selectize = $("#"+plot+"-',type,'_split");
            if(selectize.children().length === 0) {
              var opts = selectize[0].nextElementSibling.querySelectorAll(".optgroup .option")
              $(opts).hide();
            } else {
              var unselected = selectize[0].nextElementSibling.querySelectorAll(".optgroup:not([data-group=\'"+meta+"\'])");
              $(unselected).hide();
              var opt_group = selectize[0].nextElementSibling.querySelectorAll(".optgroup[data-group=\'"+meta+"\']")
              var opts = opt_group[0].querySelectorAll(".option")
              $(opts).show();
            }
          }, 0);
        }')
      }
      
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
      
      create_split_list <- function() {
        meta_names <- dataset$anno
        name_list <- lapply(meta_names, function(x) {
          gtools::mixedsort(funique(fetch_data(meta=x)))
        })  
        split_list <- lapply(1:length(meta_names), function(x) {
          sublist <- as.list(paste0(id,"@",meta_names[x],"@",name_list[[x]]))
          names(sublist) <- name_list[[x]]
          sublist
        })
        names(split_list) <- meta_names
        return(split_list)
      }
      
      update_plot_inputs <- function() {
        if (!is.null(dataset$exp)) {
          updateSelectizeInput(session, "layout_meta", choices = names(dataset$reduct))
          updateSelectizeInput(session, "layout_gene", choices = names(dataset$reduct))
          updateSelectizeInput(session, "metadata", choices = c(dataset$anno), selected = character(0))
          split_choices <- create_split_list()
          updateSelectizeInput(session, "meta_split", choices = split_choices, selected = character(0))
          updateSelectizeInput(session, "gene_split", choices = split_choices, selected = character(0))
          updateSelectizeInput(session, "violin_split", choices = split_choices, selected = character(0))
          updateSelectizeInput(session, "scatter_split", choices = split_choices, selected = character(0))
          updateSelectizeInput(session, "meta_violin", choices = c("All Data",dataset$anno), selected = NULL)
          updateSelectizeInput(session, "meta_heatmap", choices = c(dataset$anno,"All Cells"), selected = character(0))
          updateSelectizeInput(session, "meta_props_1", choices = c(dataset$anno), selected = character(0))
          updateSelectizeInput(session, "meta_props_2", choices = c("All Data",dataset$anno), selected = NULL)
          updateSelectizeInput(session, "gene_exp", choices = c(dataset$genes), selected = character(0), server = T,options=list(maxOptions=500))
          updateSelectizeInput(session, "gene_violin", choices = c(dataset$genes), selected = character(0), server = T,options=list(maxOptions=500))
          updateSelectizeInput(session, "scatter_x_axis", choices = list(Metadata=dataset$anno,Quality=dataset$quality,Genes=dataset$genes), selected = character(0), server = T,options=list(maxOptions=500))
          updateSelectizeInput(session, "scatter_y_axis", choices = list(Metadata=dataset$anno,Quality=dataset$quality,Genes=dataset$genes), selected = character(0), server = T,options=list(maxOptions=500))
          updateSelectizeInput(session, "scatter_color", choices = list(Metadata=dataset$anno,Quality=dataset$quality,Genes=dataset$genes), selected = character(0), server = T,options=list(maxOptions=500))        
        }
      }
      
      check_split <- function(split) {
        choices <- strsplit(split,"@")
        split_anno <- funique(sapply(choices,function(x) x[[2]]))
        split_groups <- sapply(choices,function(x) x[[3]])
        if (!(split_anno %in% dataset$anno)) {
          return(F)
        }
        all_groups <- funique(fetch_data(meta=split_anno))
        group_check <- sum(split_groups %in% all_groups) == length(split_groups)
        return(group_check)
      }
      
      meta_change_update <- function() {
        all_meta <- dataset$anno
        meta_choice <- input$metadata
        updateSelectizeInput(session, "metadata", choices = c(all_meta), selected = if (isTruthy(meta_choice) && meta_choice %in% all_meta) meta_choice else character(0))
        meta_choice <- input$meta_violin
        updateSelectizeInput(session, "meta_violin", choices = c("All Data",all_meta), selected = if (isTruthy(meta_choice) && meta_choice %in% all_meta) meta_choice else NULL)
        meta_choice <- input$meta_heatmap
        updateSelectizeInput(session, "meta_heatmap", choices = c(all_meta,"All Cells"), selected = if (isTruthy(meta_choice) && meta_choice %in% all_meta) meta_choice else character(0))
        meta_choice <- input$meta_props_1
        updateSelectizeInput(session, "meta_props_1", choices = c(all_meta), selected = if (isTruthy(meta_choice) && meta_choice %in% all_meta) meta_choice else character(0))
        meta_choice <- input$meta_props_2
        updateSelectizeInput(session, "meta_props_2", choices = c("All Data",all_meta), selected = if (isTruthy(meta_choice) && meta_choice %in% all_meta) meta_choice else NULL)
        all_variables <- c(all_meta,dataset$quality,dataset$genes)
        meta_choice <- input$scatter_x_axis
        if (isTruthy(meta_choice) && meta_choice %in% all_variables) {
          x_ignore(T)
          updateSelectizeInput(session, "scatter_x_axis", choices = list(Metadata=all_meta,Quality=dataset$quality,Genes=dataset$genes), selected = meta_choice, server = T,options=list(maxOptions=500))
        } else {
          updateSelectizeInput(session, "scatter_x_axis", choices = list(Metadata=all_meta,Quality=dataset$quality,Genes=dataset$genes), selected = character(0), server = T,options=list(maxOptions=500))
        }
        meta_choice <- input$scatter_y_axis
        if (isTruthy(meta_choice) && meta_choice %in% all_variables) {
          y_ignore(T)
          updateSelectizeInput(session, "scatter_y_axis", choices = list(Metadata=all_meta,Quality=dataset$quality,Genes=dataset$genes), selected = meta_choice, server = T,options=list(maxOptions=500))
        } else {
          updateSelectizeInput(session, "scatter_y_axis", choices = list(Metadata=all_meta,Quality=dataset$quality,Genes=dataset$genes), selected = character(0), server = T,options=list(maxOptions=500))
        }        
        meta_choice <- input$scatter_color
        if (isTruthy(meta_choice) && meta_choice %in% all_variables) {
          col_ignore(T)
          updateSelectizeInput(session, "scatter_color", choices = list(Metadata=all_meta,Quality=dataset$quality,Genes=dataset$genes), selected = meta_choice, server = T,options=list(maxOptions=500))
        } else {
          updateSelectizeInput(session, "scatter_color", choices = list(Metadata=all_meta,Quality=dataset$quality,Genes=dataset$genes), selected = character(0), server = T,options=list(maxOptions=500))
        }      
      }
      
      gene_sets <- reactiveVal()
      genes_heatmap <- reactiveVal(NULL)
      
      update_set_names <- function() {
        names <- as.character(set_names())
        names_old <- names(gene_sets())
        name_change <- NULL
        if (sum(names != "NULL") == sum(names_old != "NULL")) {
          name_change <- names[!(names %in% names_old)]
          genes <- set_lists()
          names(genes) <- names
          gene_sets(genes)
        }
        if (class(names) == "character") {
          options <- c("All genes","Top 500 variable","Top 500 average","Other features",names)
          choice <- input$reduction_gene_set
          if (!(list(choice) %in% options)) {
            if (isTruthy(name_change)) {
              choice <- name_change
            } else {
              choice <- NULL 
            }
          }
          updateSelectizeInput(session, "reduction_gene_set",choices = options,selected = choice)
          choice <- input$violin_gene_set
          if (!(list(choice) %in% options)) {
            if (isTruthy(name_change)) {
              choice <- name_change
            } else {
              choice <- NULL 
            }
          }
          updateSelectizeInput(session, "violin_gene_set",choices = options,selected = choice)
          options <- c("Top 500 variable","Top 500 average",names)
          choice <- input$heatmap_gene_set
          if (!(list(choice) %in% options)) {
            if (isTruthy(name_change)) {
              choice <- name_change
            } else {
              choice <- character(0)
            }
          }
          updateSelectizeInput(session, "heatmap_gene_set",choices = options,selected = choice)
        } else{
          if (length(names) == 0) {
            options <- c("All genes","Top 500 variable","Top 500 average","Other features")
            choice <- input$reduction_gene_set
            if (!(list(choice) %in% options)) choice <- NULL 
            updateSelectizeInput(session, "reduction_gene_set",choices = options,selected = choice)
            choice <- input$violin_gene_set
            if (!(list(choice) %in% options)) choice <- NULL 
            updateSelectizeInput(session, "violin_gene_set",choices = options,selected = choice)
            options <- options[2:5]
            choice <- input$heatmap_gene_set
            if (!(list(choice) %in% options)) choice <- character(0)
            updateSelectizeInput(session, "heatmap_gene_set",choices = options,selected = choice)
          }
        }
        if (isTruthy(input$gene_exp) && input$gene_exp == "Gene set score") set_name_reduct(input$reduction_gene_set) else set_name_reduct(NULL)
        if (isTruthy(input$gene_violin) && input$gene_violin == "Gene set score") set_name_violin(input$violin_gene_set) else set_name_violin(NULL)
      }
      
      update_set_lists <- function() {
        names <- unname(set_names())
        genes <- set_lists()
        names(genes) <- names
        reduction_choice <- input$reduction_gene_set
        violin_choice <- input$violin_gene_set
        heatmap_choice <- input$heatmap_gene_set
        update_reduction <- F
        update_violin <- F
        update_heatmap <- F
        if (!is.null(reduction_choice) && !(reduction_choice %in% c("All genes","Top 500 variable","Top 500 average","Other features"))) {
          update_reduction <- !(identical(gene_sets()[[reduction_choice]],genes[[reduction_choice]]))
        }
        if (!is.null(violin_choice) && !(violin_choice %in% c("All genes","Top 500 variable","Top 500 average","Other features"))) {
          update_violin <- !(identical(gene_sets()[[violin_choice]],genes[[violin_choice]]))
        }
        if (!is.null(heatmap_choice) && !(heatmap_choice %in% c("Top 500 variable","Top 500 average"))) {
          update_heatmap <- !(identical(gene_sets()[[heatmap_choice]],genes[[heatmap_choice]]))
        }
        gene_sets(genes)
        if (update_reduction) {
          gene_choice <- input$gene_exp
          all_genes <- genes[[reduction_choice]]
          choices <- if(reduction_choice %in% names) c("Gene set score",all_genes) else c(all_genes)
          if (!(gene_choice %in% choices)) gene_choice <- character(0)
          updateSelectizeInput(session, "gene_exp", choices = choices, selected = gene_choice, server = T,options=list(maxOptions=500))
        }
        if (update_violin) {
          gene_choice <- input$gene_violin
          all_genes <- genes[[violin_choice]]
          choices <- if(violin_choice %in% names) c("Gene set score",all_genes) else c(all_genes)
          if (!(gene_choice %in% choices)) gene_choice <- character(0)
          updateSelectizeInput(session, "gene_violin", choices = choices, selected = gene_choice, server = T,options=list(maxOptions=500))
        }
        if (update_heatmap) {
          genes_heatmap(gene_sets()[[heatmap_choice]])
        }
      }
      
      observeEvent(input$heatmap_gene_set, {
        if (input$heatmap_gene_set == "Top 500 variable") {
          genes_heatmap(dataset$sets[[1]])
        } else if (input$heatmap_gene_set == "Top 500 average") {
          genes_heatmap(dataset$sets[[2]])
        } else if (input$heatmap_gene_set != "") {
          genes_heatmap(gene_sets()[[input$heatmap_gene_set]])
        } else {
          genes_heatmap(NULL)
        }
      })
      
      get_genes_reduction <- function() {
        if (input$reduction_gene_set == "All genes") {
          return(dataset$genes)
        } else if (input$reduction_gene_set == "Other features") {
          return(dataset$quality)
        } else if (input$reduction_gene_set == "Top 500 variable") {
          return(dataset$sets[[1]])
        } else if (input$reduction_gene_set == "Top 500 average") {
          return(dataset$sets[[2]])
        } else {
          return(c("Gene set score",gene_sets()[[input$reduction_gene_set]]))
        }
      }
      
      observeEvent(input$reduction_gene_set, {
        if(input$reduction_gene_set != "") {
          updateSelectizeInput(session, "gene_exp", choices = c(get_genes_reduction()), selected = character(0), server = T,options=list(maxOptions=500))
        }
      })
      
      get_genes_violin <- function() {
        if (input$violin_gene_set == "All genes") {
          return(dataset$genes)
        } else if (input$violin_gene_set == "Other features") {
          return(dataset$quality)
        } else if (input$violin_gene_set == "Top 500 variable") {
          return(dataset$sets[[1]])
        } else if (input$violin_gene_set == "Top 500 average") {
          return(dataset$sets[[2]])
        } else {
          return(c("Gene set score",gene_sets()[[input$violin_gene_set]]))
        }
      }
      
      observeEvent(input$violin_gene_set, {
        if (input$violin_gene_set != "") {
          updateSelectizeInput(session, "gene_violin", choices = c(get_genes_violin()), selected = character(0), server = T,options=list(maxOptions=500))
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
                                 options=list(maxItems=3,maxOptions=2000,plugins = list('remove_button'),onDropdownOpen = I(dropdown_open()),onItemAdd = I(item_add("meta")),onItemRemove = I(item_remove("meta")))
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
                                 options=list(maxItems=3,maxOptions=2000,plugins = list('remove_button'),onDropdownOpen = I(dropdown_open()),onItemAdd = I(item_add("gene")),onItemRemove = I(item_remove("gene")))
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
                ),
                tabPanel(
                  title = "Other",
                  selectizeInput(ns("scatter_x_axis"),
                     label = "X-axis",
                     choices=NULL,
                     selected=NULL
                  ),
                  selectizeInput(ns("scatter_y_axis"),
                     label = "Y-axis",
                     choices=NULL,
                     selected=NULL
                  ),
                  selectizeInput(ns("scatter_color"),
                     label = "Color by",
                     choices=NULL,
                     selected=NULL
                  ),
                  selectizeInput(ns("scatter_split"), 
                   label = "Split by group",
                   choices = NULL,
                   selected = NULL,
                   multiple=T,
                   options=list(maxItems=3,maxOptions=2000,plugins = list('remove_button'),onDropdownOpen = I(dropdown_open()),onItemAdd = I(item_add("scatter")),onItemRemove = I(item_remove("scatter")))
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
                choices = c("Heatmap","Bubble")
              ),
              fluidRow(
                column(
                  width=5,
                  offset = 1,
                  strong("Scale"),
                  materialSwitch(ns("scale_heatmap"),"",value=T,status="primary")
                ),
                column(
                  width=5,
                  offset=1,
                  strong("Cluster"),
                  materialSwitch(ns("cluster_heatmap"),"",value=T,status="primary")
                )
              ),
              fluidRow(
                column(
                  width=5,
                  offset=1,
                  strong("Average"),
                  materialSwitch(ns("average_heatmap"),"",value=T,status="primary")
                ),
                column(
                  width=5,
                  offset=1,
                  strong("Flip axes"),
                  materialSwitch(ns("flip_heatmap"),"",value=F,status="primary")
                ),
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
              ),
              selectizeInput(ns("violin_split"), 
                             label = "Split by group",
                             choices = NULL,
                             selected = NULL,
                             multiple=T,
                             options=list(maxItems=3,maxOptions=2000,plugins = list('remove_button'),onDropdownOpen = I(dropdown_open()),onItemAdd = I(item_add("violin")),onItemRemove = I(item_remove("violin")))
              )
            ),
            div(
              id = ns('props_div'),
              selectizeInput(ns("meta_props_1"), 
                             label = "View groups of",
                             choices = NULL,
                             selected = NULL
              ),
              selectizeInput(ns("meta_props_2"), 
                             label = "Across",
                             choices = NULL,
                             selected = NULL
              ),
              radioGroupButtons(
                inputId = ns("props_type"),
                label = "Data Type",
                choices = c("Proportions","Counts")
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
              ),
              strong("Show labels"),
              materialSwitch(ns("volcano_labels"),"",value=T,status="primary")
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
          div(id=ns("slider_div"),style="position:absolute; right:1%; bottom:52%; width:96px;",uiOutput(ns("gene_slider"))),
          div(id=ns("color_div"),style="position:absolute; right:1%; bottom:52%; width:87px; margin-bottom:7px;",plotOutput(height="240px",ns("gene_colorbar")))
        )
      })
      
      plot_name <- paste0(id,"-plot")
      plot_type <- reactiveVal('reduction')
      has_cur_select <- reactiveVal(NULL)
      cur_labels <- reactiveValues(plot1=NULL,plot2=NULL,plot3=NULL)
      cur_cameras <- reactiveValues(plot1=NULL,plot2=NULL,plot3=NULL)
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
        req(dataset$exp)
        req(plot_type() == 'volcano')
        req(input$volcano_type == "Volcano")
        req((isTruthy(markers()) && input$volcano_data == "Markers") || (isTruthy(degs()) && input$volcano_data == "DEGs"))
        event_data("plotly_relayout",source=ns("volcano_plot"),priority="event")
      })
      
      volcano_x1 <- reactiveVal(-0.5)
      volcano_x2 <- reactiveVal(0.5)
      volcano_y <- reactiveVal(10)
      
      ma_lines <- reactive({
        req(dataset$exp)
        req(plot_type() == 'volcano')
        req(input$volcano_type == "MA")
        req((isTruthy(markers()) && input$volcano_data == "Markers") || (isTruthy(degs()) && input$volcano_data == "DEGs"))
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
        plotlyProxy(ns("plot"), session) %>%
          plotlyProxyInvoke("restyle", 
                            list(marker=list(color=colors,colorscale=list(c(-1, "rgb(224, 40, 40)"),c(0, "rgb(204, 204, 204)"), c(1, "rgb(40, 58, 224)")),size=plot_settings$point_size + 4,cmin=-1,cmax=1))
          )
        if (input$volcano_labels) {
          anno_data <- data.frame(p_val,fc,gene=data_use$gene)
          anno_data <- anno_data[colors != 0,]
          annotation_mat <- mapply(create_anno_marker, anno_data$fc, anno_data$p_val, anno_data$gene)
          annotation_list <- lapply(1:ncol(annotation_mat), function(i) annotation_mat[,i])
          plotlyProxy(ns("plot"), session) %>%
            plotlyProxyInvoke("relayout", 
              list(annotations=annotation_list)
            )
        }
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
        if (ma_y1() != ma_y2()) {
          fc_lower <- min(ma_y1(),ma_y2())
          fc_upper <- max(ma_y1(),ma_y2())
        } else {
          fc_lower <- ma_y1()
          fc_upper <- ma_y2()
        }
        opacity <- rep(0.2,nrow(data_use))
        filter_avg <- a_val >= ma_x()
        filter_neg <- m_val <= fc_lower
        filter_pos <- m_val >= fc_upper
        opacity[filter_avg & filter_neg] <- 1
        opacity[filter_avg & filter_pos] <- 1
        colorscale <- NULL
        if (is.function(plot_settings$color_cont)) {
          colorscale <- list(c(0, "#D9D9D9"),c(0.33,"#00008B"),c(0.66,"#63B8FF"),c(1,"#00FFFF"))
        } else {
          if (plot_settings$color_cont == "viridis") {
            colorscale <- "Viridis"
          } else {
            colors <- viridisLite::plasma(10)
            colorscale <- list(c(0,"#0D0887FF"),c(0.1,"#42049EFF"),c(0.2,"#6A00A8FF"),c(0.3,"#900DA4FF"),c(0.4,"#B12A90FF"),c(0.5,"#CC4678FF"),c(0.6,"#E16462FF"),c(0.7,"#F1844BFF"),c(0.8,"#FCA636FF"),c(0.9,"#FCCE25FF"),c(1.0,"#F0F921FF"))
          }
        }
        plotlyProxy(ns("plot"), session) %>%
          plotlyProxyInvoke("restyle", 
            list(marker=list(color = color,colorscale=colorscale,opacity=opacity,size=plot_settings$point_size + 4,showscale=T,colorbar=list(len=200,lenmode="pixels",thickness=28,y=0.8,title=list(text="-log10(p-value)"))))
          )
        if (input$volcano_labels) {
          anno_data <- data.frame(m_val,a_val,p_val,gene,color)
          anno_data <- anno_data[opacity != 0.2,]
          annotation_mat <- mapply(create_anno_marker, anno_data$a_val, anno_data$m_val, anno_data$gene)
          annotation_list <- lapply(1:ncol(annotation_mat), function(i) annotation_mat[,i])
          plotlyProxy(ns("plot"), session) %>%
            plotlyProxyInvoke("relayout", 
              list(annotations=annotation_list)
            )
        }
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
        for (x in funique(subplot)) {
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
        req(isTruthy(input$metadata) || scatter_type() %in% c("metadata","none"))
        selected <- event_data("plotly_selected",source=ns("meta_plot"),priority="event")
        subplot <- funique(selected$customdata)
        subplot <- subplot[!is.na(subplot)]
        if (!is.null(subplot)) {
          list(cells=unlist(selected$key),subplot=subplot)
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
        req(dataset$exp)
        req(isTruthy(input$metadata) || scatter_type() %in% c("metadata","none")) 
        event_data("plotly_deselect",source=ns("meta_plot"),priority="event")
      })
      
      meta_legend <- debounce(reactive({
        req(dataset$exp)
        req(isTruthy(input$metadata) || scatter_type() == "metadata") 
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
                  cameras = {};
                  for (i in scenes) {
                    var data = [gd.layout[scenes[i]]['camera']];
                    if (data === undefined) {
                      data = [];
                    }
                    cameras[scenes[i]] = data;
                  }
                  Shiny.setInputValue('",ns("meta_legend_visible"),"',{visible:results, cameras:cameras},{priority: 'event'});
                } else {
                  Shiny.setInputValue('",ns("meta_legend_visible"),"',{visible:results, cameras:undefined},{priority: 'event'});
                }
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
            if(length(visible) > 0) {
              label_info <- cur_labels$plot1 %>% filter(label %in% visible)
              label_list <- prepare_3D_labels(label_info)
            } else {
              label_list <- list()
            }
            plotlyProxy(ns("plot"), session) %>%
              plotlyProxyInvoke("relayout", 
                list(scene = list(annotations=label_list,camera=list(center=cur_cameras$plot1[[1]],eye=cur_cameras$plot1[[2]]),domain=list(x=c(0,1),y=c(0,0.9)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)))
              )
          } else {
            label_list <- lapply(1:length(split_order_1()),function(x) {
              if(length(visible) > 0) {
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
                  list(scene = list(annotations=label_list[[1]],camera=list(center=cur_cameras$plot1[[1]],eye=cur_cameras$plot1[[2]]),domain=list(x=c(0,1),y=c(0,0.9)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)))
                )
            } else if (scene_num == 2) {
              plotlyProxy(ns("plot"), session) %>%
                plotlyProxyInvoke("relayout", 
                  list(scene = list(annotations=label_list[[1]],camera=list(center=cur_cameras$plot1[[1]],eye=cur_cameras$plot1[[2]]),domain=list(x=c(0,0.5),y=c(0,0.9)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
                       scene2 = list(annotations=label_list[[2]],camera=list(center=cur_cameras$plot2[[1]],eye=cur_cameras$plot2[[2]]),domain=list(x=c(0.5,1),y=c(0,0.9)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F))
                  )
                )
            } else if (scene_num == 3) {
              plotlyProxy(ns("plot"), session) %>%
                plotlyProxyInvoke("relayout", 
                  list(scene = list(annotations=label_list[[1]],camera=list(center=cur_cameras$plot1[[1]],eye=cur_cameras$plot1[[2]]),domain=list(x=c(0,0.33),y=c(0,0.9)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
                       scene2 = list(annotations=label_list[[2]],camera=list(center=cur_cameras$plot2[[1]],eye=cur_cameras$plot2[[2]]),domain=list(x=c(0.33,0.66),y=c(0,0.9)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
                       scene3 = list(annotations=label_list[[3]],camera=list(center=cur_cameras$plot3[[1]],eye=cur_cameras$plot3[[2]]),domain=list(x=c(0.66,0.99),y=c(0,0.9)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F))
                  )
                )
            }
          }
        }
      }
      
      observeEvent(input$meta_legend_visible, {
        visible <- input$meta_legend_visible[["visible"]]
        cameras <- input$meta_legend_visible[["cameras"]]
        cur_visible(visible)
        if (!is.null(cameras)) {
          for (x in 1:length(cameras)) {
            scene_data <- unlist(cameras[[x]])
            if (!is.null(scene_data)) {
              center <- as.list(scene_data[c("center.x","center.y","center.z")])
              names(center) <- c("x","y","z")
              eye <- as.list(scene_data[c("eye.x","eye.y","eye.z")])
              names(eye) <- c("x","y","z")
              cur_cameras[[paste0("plot",x)]] <- list(center,eye)
            } else {
              cur_cameras[[paste0("plot",x)]] <- list(NULL,NULL)
            }
          }
        }
        if (is_3D()) {
          update_labels_3D(visible)
        } else {
          update_all_anno()
        }
      },ignoreInit = T, ignoreNULL = F)
      
      observeEvent(meta_clear_cells(), {
        if ((input$data_type == "Metadata" && !isTruthy(split_1())) || (input$data_type == "Other" && !isTruthy(split_3()))) {
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
            Plotly.restyle(gd,{'selectedpoints': null},traces);
            Shiny.setInputValue('",ns("meta_clear_cells_subplot"),"',deselected,{priority: 'event'});
          "))
        }
      })
      
      observeEvent(input$meta_clear_cells_subplot, {
        subplots <- funique(input$meta_clear_cells_subplot)
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
        req(isTruthy(input$gene_exp) || scatter_type() == "feature") 
        selected <- event_data("plotly_selected",source=ns("exp_plot"),priority="event")
        subplot <- funique(selected$customdata)
        subplot <- subplot[!is.na(subplot)]
        if (!is.null(subplot)) {
          list(cells=unlist(selected$key),subplot=subplot)
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
        req(dataset$exp)
        req(isTruthy(input$gene_exp) || scatter_type() == "feature") 
        event_data("plotly_deselect",source=ns("exp_plot"),priority="event")
      })
      
      observeEvent(exp_clear_cells(), {
        if ((input$data_type == "Features" && !isTruthy(split_2())) || (input$data_type == "Other" && !isTruthy(split_3()))) {
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
        subplots <- funique(input$exp_clear_cells_subplot)
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
        if (input$data_type == "Metadata" || (input$data_type == "Other" && scatter_type() %in% c("metadata","none"))) {
          toggle_slider(F)
        }
        reset_select()
      },ignoreInit = T)
      
      observeEvent(scatter_type(), {
        if (scatter_type() %in% c("metadata","none")) {
          toggle_slider(F)
        }
        reset_select()
      },ignoreInit = T)
      
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
        } else if (input$data_type == "Features"){
          text <- split_order_2()
          if (is.null(text)) {
            feature_name <- input$gene_exp
            if (feature_name == "Gene set score") {
              feature_name <- set_name_reduct()
            }
            if (is_bold) {
              return(paste0("<b>",feature_name,"</b>"))
            } else {
              return(feature_name)
            }
          }
        } else if (input$data_type == "Other"){
          text <- split_order_3()
          if (is.null(text)) {
            scatter_name <- paste0(input$scatter_x_axis," vs ",input$scatter_y_axis)
            if (isTruthy(input$scatter_color)) {
              scatter_name <- paste0(scatter_name,"<br><span style='font-size: 17px;'>Colored by ",input$scatter_color,"</span>")
            }
            if (is_bold) {
              return(paste0("<b>",scatter_name,"</b>"))
            } else {
              return(scatter_name)
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
          font = list(size = 18)
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
          y_pos <- if (grepl("span",title_data)) 0.96 else 0.98
          font_size <- if (grepl("span",title_data)) 17 else 24
          plotlyProxy(ns("plot"), session) %>%
            plotlyProxyInvoke("relayout", 
              list(annotations=label_list,title = list(text=title_data,y=y_pos,font = list(size = font_size)))
            )
        }
      }
      
      exp_range <- reactiveValues(min=NULL,max=NULL)
      slider_ready <- reactiveVal(TRUE)
      slider_refresh <- reactiveVal(NULL)

      output$gene_slider <- renderUI({
        refresh <- slider_refresh()
        if((input$data_type == "Features" && isTruthy(input$gene_exp) || (input$data_type == "Other" && isTruthy(col_vals_scatter()))) && isTruthy(exp_range$max) && exp_range$max != 0) {
          slider_ready(FALSE)
          noUiSliderInput(ns("exp_range_select"),orientation = "vertical",direction="rtl",min=exp_range$min,max=exp_range$max,value=c(exp_range$min,exp_range$max),width="1vw",height="178px",color="#96a8fc")
        } else {
          NULL
        }
      })
      
      output$gene_colorbar <- renderPlot(bg="transparent",{
        refresh <- slider_refresh()
        if((input$data_type == "Features" && isTruthy(input$gene_exp) || (input$data_type == "Other" && isTruthy(col_vals_scatter()))) && isTruthy(exp_range$max)) {
          data <- data.frame(x=c(1,2),y=c(1,2),color=c(exp_range$min,exp_range$max))
          colnames(data) <- c("x","y","Expression")
          color_scale <- plot_settings$color_cont
          if (is.function(color_scale)) {
            if (exp_range$max == 0) {
              color_scale <- "#D9D9D9"
            } else {
              color_scale <- c("#D9D9D9","#00008B","#63B8FF","#00FFFF")
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
          legend <- cowplot::get_plot_component(plot,"guide-box",return_all = T)
          if (is.null(legend$layout)) legend <- legend[[1]]
          legend <- legend$grobs[[1]]
          legend$grobs[[grep("title",legend$layout$name)]] <- zeroGrob()
          legend$grobs[[grep("background",legend$layout$name)]] <- zeroGrob()
          legend$grobs[[grep("label",legend$layout$name)]]$children[[1]]$gp$fontsize <- 12.0
          grid::grid.newpage()
          grid::grid.draw(legend)
        } else {
          return(NULL)
        }
      })
      
      observeEvent(input$exp_range_select, {
        min_check <- round(abs(input$exp_range_select[1] - exp_range$min),3)
        max_check <- round(abs(input$exp_range_select[2] - exp_range$max),3)
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
              for (var plot = 0; plot < gd.data.length; plot+=1) {
                var exp = gd.data[plot].marker.color;
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
                var subplot_num = plot + 1
                if (gd.data.length == 1) {
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
        if (isTruthy(split_order_2()) && subplot == 0) subplot <- 1
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
      
      session$userData[[paste0("select_",id,"_obs")]] <- observeEvent(cur_selection$cells, {
        if (plot_type() == 'reduction') {
          update_all_anno()
        }
      },ignoreNULL = F,ignoreInit = T)
      
      session$userData[[paste0("meta_",id,"_obs")]] <- observeEvent(dataset$anno, {
        meta_change_update()
      },ignoreInit = T)
      
      session$userData[[paste0("reducs_",id,"_obs")]] <- observeEvent(dataset$reduct, {
        updateSelectizeInput(session, "layout_meta", choices = names(dataset$reduct))
        updateSelectizeInput(session, "layout_gene", choices = names(dataset$reduct))
      })
      
      session$userData[[paste0("genes_",id,"_obs")]] <- observeEvent(dataset$genes, {
        updateSelectizeInput(session, "gene_exp", choices = c(dataset$genes), selected = character(0), server = T,options=list(maxOptions=500))
        updateSelectizeInput(session, "gene_violin", choices = c(dataset$genes), selected = character(0), server = T,options=list(maxOptions=500))
      })
      
      set_names <- reactive(sapply(sets$sets,function(x) x$name()))
      set_lists <- reactive(lapply(sets$sets,function(x) x$genes()))
      
      observeEvent(set_names(), {
        update_set_names()
      })
      
      observeEvent(set_lists(), {
        update_set_lists()
      })
      
      process_splits <- function(split_data,type) {
        choices <- strsplit(split_data,"@")
        split_anno <- funique(sapply(choices,function(x) x[[2]]))
        split_groups <- sapply(choices,function(x) x[[3]])
        subset <- fetch_data(meta=split_anno)
        subset[!(subset %in% split_groups)] <- NA
        if (type == "meta") {
          if (!identical(subset,split_1())) {
            split_1(subset)
            split_order_1(split_groups)
            update_selection_counts(subset,split_groups)
          }
        } else if (type == "gene") {
          if (!identical(subset,split_2())) {
            split_2(subset)
            split_order_2(split_groups)
            update_selection_counts(subset,split_groups)
          }
        } else if (type == "scatter") {
          if (!identical(subset,split_3())) {
            split_3(subset)
            split_order_3(split_groups)
            update_selection_counts(subset,split_groups)
          }
        } else if (type == "violin") {
          if (!identical(subset,split_4())) {
            split_4(subset)
            split_order_4(split_groups)
          }
        } 
      } 
      
      update_selection_counts <- function(split,order) {
        counts <- table(split[!is.na(split)])
        counts_final <- as.vector(counts[match(names(counts),order)])
        for (x in 1:length(counts_final)) {
          selection_list$counts[[paste0(plot_name,"@",x)]] <- counts_final[x]
        }
      }
      
      process_scatter_input <- function(type,selected) {
        check_meta <- F
        meta_update <- F
        subset <- NULL
        if (isTruthy(selected)) {
          if (selected %in% dataset$anno) {
            subset <- fetch_data(meta=selected)
            check_meta <- T
          } else if (selected %in% dataset$quality) {
            subset <- fetch_data(meta=selected) %>% round(2)
          } else if (selected %in% dataset$genes) {
            subset <- fetch_data(genes=selected) %>% as.vector() %>% round(2)
          }
        }
        if (check_meta) {
          groups <- gtools::mixedsort(funique(subset)) %>% fix_order()
          subset <- factor(as.character(subset),levels=groups)
          cur_vals <- switch(type,"x"=x_vals_scatter(),"y"=y_vals_scatter(),"col"=col_vals_scatter())
          if (!identical(subset,cur_vals)) meta_update <- T
        }
        if ((!check_meta) || (check_meta && meta_update)) {
          if (type == "x") {
            x_vals_scatter(subset)
            scatter_names$x <- selected
          } else if (type == "y") {
            y_vals_scatter(subset)
            scatter_names$y <- selected
          } else {
            col_vals_scatter(subset)
            scatter_names$col <- selected
          }
        }
      }
      
      refresh_meta <- function() {
        if (isTruthy(input$metadata) && input$metadata %in% dataset$anno) {
          subset <- fetch_data(meta=input$metadata)
          if (!identical(subset,meta_plot_reduct())) {
            meta_plot_reduct(subset)
          }
        } else {
          meta_plot_reduct(NULL)
        }
        
        if (isTruthy(input$meta_violin) && input$meta_violin != "All Data" && input$meta_violin %in% dataset$anno) {
          subset <- fetch_data(meta=input$meta_violin)
          if (!identical(subset,meta_plot_violin())) {
            meta_plot_violin(subset)
          }
        } else {
          meta_plot_violin(NULL)
        }
        if (isTruthy(input$meta_heatmap) && input$meta_heatmap != "All Cells" && input$meta_heatmap %in% dataset$anno) {
          subset <- fetch_data(meta=input$meta_heatmap)
          if (!identical(subset,meta_plot_heatmap())) {
            meta_plot_heatmap(subset)
          }
        } else {
          meta_plot_heatmap(NULL)
        }
        if (isTruthy(input$meta_props_1) && input$meta_props_1 %in% dataset$anno) {
          subset <- fetch_data(meta=input$meta_props_1)
          if (!identical(subset,meta_plot_props_1())) {
            meta_plot_props_1(subset)
          }
        } else {
          meta_plot_props_1(NULL)
        }
        if (isTruthy(input$meta_props_2) && input$meta_props_2 != "All Data" && input$meta_props_2 %in% dataset$anno) {
          subset <- fetch_data(meta=input$meta_props_2)
          if (!identical(subset,meta_plot_props_2())) {
            meta_plot_props_2(subset)
          }
        } else {
          meta_plot_props_2(NULL)
        }
        
        process_scatter_input("x",input$scatter_x_axis)
        process_scatter_input("y",input$scatter_y_axis)
        process_scatter_input("col",input$scatter_color)
        
        split_choices <- create_split_list()
        meta_choice <- input$meta_split
        split_selected <- if (isTruthy(meta_choice) && check_split(meta_choice)) meta_choice else character(0)
        updateSelectizeInput(session, "meta_split", choices = split_choices, selected = split_selected)
        if (isTruthy(split_selected)) {
          process_splits(split_selected,"meta")
        } else {
          split_1(NULL)
          split_order_1(NULL)
          selection_list$counts[[paste0(plot_name,"@0")]] <- NULL
        }
        meta_choice <- input$gene_split
        split_selected <- if (isTruthy(meta_choice) && check_split(meta_choice)) meta_choice else character(0)
        updateSelectizeInput(session, "gene_split", choices = split_choices, selected = split_selected)
        if (isTruthy(split_selected)) {
          process_splits(split_selected,"gene")
        } else {
          split_2(NULL)
          split_order_2(NULL)
          selection_list$counts[[paste0(plot_name,"@0")]] <- NULL
        }
        meta_choice <- input$scatter_split
        split_selected <- if (isTruthy(meta_choice) && check_split(meta_choice)) meta_choice else character(0)
        updateSelectizeInput(session, "scatter_split", choices = split_choices, selected = split_selected)
        if (isTruthy(split_selected)) {
          process_splits(split_selected,"scatter")
        } else {
          split_3(NULL)
          split_order_3(NULL)
        }
        meta_choice <- input$violin_split
        split_selected <- if (isTruthy(meta_choice) && check_split(meta_choice)) meta_choice else character(0)
        updateSelectizeInput(session, "violin_split", choices = split_choices, selected = split_selected)
        if (isTruthy(split_selected)) {
          process_splits(split_selected,"violin")
        } else {
          split_4(NULL)
          split_order_4(NULL)
        }
      }
      
      session$userData[[paste0("meta_change_",id,"_obs")]] <- observeEvent(dataset$meta, {
        refresh_meta()
      })
      
      session$userData[[paste0("subset_",id,"_obs")]] <- observeEvent(dataset$subset, {
        refresh_meta()
      },ignoreInit = T)
      
      observeEvent(input$gene_exp, {
        if (input$gene_exp == "Gene set score") {
          set_name_reduct(input$reduction_gene_set)
          set_list_reduct(gene_sets()[[input$reduction_gene_set]])
        } else {
          set_name_reduct(NULL)
          set_list_reduct(NULL)
        }
      })
      
      observeEvent(input$gene_violin, {
        if (input$gene_violin == "Gene set score") {
          set_name_violin(input$violin_gene_set)
          set_list_violin(gene_sets()[[input$violin_gene_set]])
        } else {
          set_name_violin(NULL)
          set_list_violin(NULL)
        }
      })
      
      observeEvent(input$metadata, {
        if (input$metadata != "") {
          subset <- fetch_data(meta=input$metadata)
          if (!identical(subset,meta_plot_reduct())) {
            meta_plot_reduct(subset)
          }
        }
      },ignoreInit = T)
      
      observeEvent(input$scatter_x_axis, {
        if (x_ignore()) {
          x_ignore(F)
        } else {
          process_scatter_input("x",input$scatter_x_axis)
        }
      },ignoreInit = T)
      
      observeEvent(input$scatter_y_axis, {
        if (y_ignore()) {
          y_ignore(F)
        } else {
          process_scatter_input("y",input$scatter_y_axis)
        }
      },ignoreInit = T)
      
      observeEvent(input$scatter_color, {
        if (col_ignore()) {
          col_ignore(F)
        } else {
          process_scatter_input("col",input$scatter_color)
        }
      },ignoreInit = T)
      
      observeEvent(input$meta_violin, {
        if (input$meta_violin != "All Data" && input$meta_violin != "") {
          subset <- fetch_data(meta=input$meta_violin)
          if (!identical(subset,meta_plot_violin())) {
            meta_plot_violin(subset)
          }
        }
      },ignoreInit = T)
      
      observeEvent(input$meta_heatmap, {
        if (input$meta_heatmap != "All Cells" && input$meta_heatmap != "") {
          subset <- fetch_data(meta=input$meta_heatmap)
          if (!identical(subset,meta_plot_heatmap())) {
            meta_plot_heatmap(subset)
          }
        } else {
          meta_plot_heatmap(NULL)
        }
      },ignoreInit = T)
      
      observeEvent(input$meta_props_1, {
        if (input$meta_props_1 != "") {
          subset <- fetch_data(meta=input$meta_props_1)
          if (!identical(subset,meta_plot_props_1())) {
            meta_plot_props_1(subset)
          }
        }
      },ignoreInit = T)
      
      observeEvent(input$meta_props_2, {
        if (input$meta_props_2 != "All Data" && input$meta_props_2 != "") {
          subset <- fetch_data(meta=input$meta_props_2)
          if (!identical(subset,meta_plot_props_2())) {
            meta_plot_props_2(subset)
          }
        }
      },ignoreInit = T)
      
      observeEvent(input$meta_split, {
        if (isTruthy(input$meta_split)) {
          process_splits(input$meta_split,"meta")
        } else {
          split_1(NULL)
          split_order_1(NULL)
          selection_list$counts[[paste0(plot_name,"@0")]] <- NULL
        }
      },ignoreInit = T,ignoreNULL = F)
      
      observeEvent(input$gene_split, {
        if (isTruthy(input$gene_split)) {
          process_splits(input$gene_split,"gene")
        } else {
          split_2(NULL)
          split_order_2(NULL)
          selection_list$counts[[paste0(plot_name,"@0")]] <- NULL
        }
      },ignoreInit = T,ignoreNULL = F)
      
      observeEvent(input$scatter_split, {
        if (isTruthy(input$scatter_split)) {
          process_splits(input$scatter_split,"scatter")
        } else {
          split_3(NULL)
          split_order_3(NULL)
          selection_list$counts[[paste0(plot_name,"@0")]] <- NULL
        }
      },ignoreInit = T,ignoreNULL = F)
      
      observeEvent(input$violin_split, {
        if (isTruthy(input$violin_split)) {
          process_splits(input$violin_split,"violin")
        } else {
          split_4(NULL)
          split_order_4(NULL)
        }
      },ignoreInit = T,ignoreNULL = F)
      
      split_1 <- reactiveVal(NULL)
      split_2 <- reactiveVal(NULL)
      split_3 <- reactiveVal(NULL)
      split_4 <- reactiveVal(NULL)
      split_order_1 <- reactiveVal(NULL)
      split_order_2 <- reactiveVal(NULL)
      split_order_3 <- reactiveVal(NULL)
      split_order_4 <- reactiveVal(NULL)
      meta_plot_reduct <- reactiveVal(NULL)
      meta_plot_violin <- reactiveVal(NULL)
      meta_plot_heatmap <- reactiveVal(NULL)
      meta_plot_props_1 <- reactiveVal(NULL)
      meta_plot_props_2 <- reactiveVal(NULL)
      
      set_name_reduct <- reactiveVal(NULL)
      set_list_reduct <- reactiveVal(NULL)
      set_name_violin <- reactiveVal(NULL)
      set_list_violin <- reactiveVal(NULL)
      
      scatter_type <- reactiveVal("")
      scatter_names <- reactiveValues(x="",y="",col="")
      x_vals_scatter <- reactiveVal(NULL)
      y_vals_scatter <- reactiveVal(NULL)
      col_vals_scatter <- reactiveVal(NULL)
      x_ignore <- reactiveVal(FALSE)
      y_ignore <- reactiveVal(FALSE)
      col_ignore <- reactiveVal(FALSE)
      
      #--------------------------------------------------
      #Plotting functions
      
      plot_reduction <- function(data_type,layout_meta,layout_gene,plot_meta,gene_select,x_scatter,y_scatter,color_scatter,labels,plot_settings,density,meta_split,gene_split,scatter_split,meta_order,gene_order,scatter_order,set_name,set_list) {
        validate(
          need(data_type,"")
        )
        if (data_type == "Metadata") {
          validate(
            need(plot_meta(),""),
            need(layout_meta,""),
            need(length(plot_meta()) == length(dataset$subset),"")
          )
          reduct <- fetch_data(reduct=layout_meta)
          dims <- ncol(reduct)
          is_3D(dims == 3)
          if (is.null(meta_split())) {
            plot_data <- data.frame(reduct,plot_meta(),check.names = F)
            colnames(plot_data) <- c(paste0("dim",c(1:dims)),input$metadata)
          } else {
            plot_data <- data.frame(reduct,plot_meta(),meta_split(),check.names = F)
            colnames(plot_data) <- c(paste0("dim",c(1:dims)),input$metadata,"split")
          }
          plot_data$cellname <- rownames(plot_data)
          return(create_meta_plot(plot_data,dims,meta_order(),labels,plot_settings))
        } else if (data_type == "Features") {
          validate(
            need(gene_select,""),
            need(layout_gene,"")
          )
          reduct <- fetch_data(reduct=layout_gene)
          dims <- ncol(reduct)
          feature_name <- gene_select
          if (feature_name == "Gene set score") {
            features <- get_scores(set_list())
            feature_name <- set_name()
          } else if (feature_name %in% dataset$quality) {
            features <- fetch_data(meta = feature_name)
          } else {
            features <- fetch_data(genes = feature_name)
          }
          if (is.null(gene_split())) {
            plot_data <- data.frame(reduct,features,check.names = F)
            colnames(plot_data) <- c(paste0("dim",c(1:dims)),feature_name)
          } else {
            plot_data <- data.frame(reduct,features,gene_split(),check.names = F)
            colnames(plot_data) <- c(paste0("dim",c(1:dims)),feature_name,"split")
          }
          plot_data$cellname <- rownames(plot_data)
          return(create_exp_plot(plot_data,dims,gene_order(),plot_settings,density))
        } else if (data_type == "Other") {
          validate(
            need(x_scatter(),""),
            need(y_scatter(),"")
          )
          if (is.null(color_scatter())) {
            scatter_type("none")
          } else if (class(color_scatter()) == "factor"){
            scatter_type("metadata")
          } else {
            scatter_type("feature")
          }
          plot_data <- data.frame(x=x_scatter(),y=y_scatter(),color=if (scatter_type() == "none") rep(0,length(x_scatter())) else color_scatter(),check.names = F)
          colnames(plot_data) <- c("x","y","color")
          plot_data$cellname <- rownames(fetch_data(reduct = 1))
          if (!is.null(scatter_split())) {
            plot_data$split <- scatter_split()
          } 
          return(plot_scatter(plot_data,scatter_type(),scatter_names,scatter_order(),plot_settings))
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
          tmp <- list(x=label_info$x[i], y=label_info$y[i], text=label_info$label[i], xref=refs[1], yref=refs[2], showarrow=F, opacity=0.8, bgcolor="#fcfcff",font=list(size=16))
          anno_list[[i]] <- tmp
        }
        return(anno_list)
      }
      
      meta_plot_2D <- function(plot_data,label_info,plot_settings,color_pal,name,subplot="no_sub",subplot_num=0,showlegend=T,visible=T){
        hover <- if (plot_settings$cellname && visible) "%{text}<extra>%{fullData.name}</extra>" else if (visible) "%{fullData.name}<extra></extra>" else ""
        meta_plot <- plot_ly(plot_data, x = ~dim1, y = ~dim2, customdata = rep(subplot_num,nrow(plot_data)), color = ~color, colors = color_pal, legendgroup= ~color, showlegend = showlegend, opacity = plot_settings$point_transparent,marker=list(size=plot_settings$point_size), unselected=list(marker=list(opacity=0.05)), text = rownames(plot_data), hovertemplate= hover, type = 'scattergl', mode = 'markers', source = ns('meta_plot'), key = ~cellname) %>% 
          plotly::config(doubleClickDelay = 400,displaylogo = F,scrollZoom = F, modeBarButtons= list(list('drawopenpath','eraseshape'),list('select2d','lasso2d',reduct_select_all,reduct_clear_select),list('zoom2d','pan2d','resetScale2d'))) %>%
          plotly::layout(font=list(family=default_font),title = list(text=name,y=0.98,font = list(size = 24)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",hoverdistance=5,spikedistance=0,margin=list(t=40,b=10,l=20,r=60),legend=list(font = list(size = 16),itemsizing='constant',entrywidth = 0,bgcolor="rgba(0, 0, 0, 0)"),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F, range=if(visible) NULL else c(100,101)),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F,range=if(visible) NULL else c(100,101)),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
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
          tmp <- list(x=label_info$x[i],y=label_info$y[i],z=label_info$z[i],text=label_info$label[i],showarrow=F, opacity=0.8, bgcolor="#fcfcff", font=list(size=16))
          anno_list[[i]] <- tmp
        }
        return(anno_list)
      }
      
      meta_plot_3D <- function(plot_data,label_info,plot_settings,color_pal,name,subplot="no_sub",subplot_num=0,scene_num=1,showlegend=T,visible=T){
        hover <- if (plot_settings$cellname && visible) "%{text}<extra>%{fullData.name}</extra>" else if (visible) "%{fullData.name}<extra></extra>" else ""
        meta_plot <- plot_ly(plot_data, x = ~dim1, y = ~dim2, z = ~dim3, customdata = rep(subplot_num,nrow(plot_data)), color = ~color, colors = color_pal, legendgroup= ~color, opacity = plot_settings$point_transparent, marker=list(size=plot_settings$point_size), showlegend = showlegend, text = rownames(plot_data), hovertemplate = hover, type = 'scatter3d', mode = 'markers', source=ns("meta_plot"), scene = paste0("scene",scene_num), key = ~cellname) %>% 
          plotly::layout(font=list(family=default_font),title = list(text=name,y=0.98,font = list(size = 24)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",hoverdistance=5,spikedistance=0,margin=list(t=40,b=10,l=20,r=60),showlegend = T, legend=list(font = list(size = 16),itemsizing='constant',entrywidth = 0,bgcolor="rgba(0, 0, 0, 0)"),scene=list(xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
          event_register("plotly_legendclick")
        if (!is.null(label_info)) {
          meta_plot <- meta_plot %>% plotly::layout(scene=list(annotations=prepare_3D_labels(label_info)))
        }
        if (subplot == "no_sub" && visible == T) {
          meta_plot <- meta_plot %>% plotly::config(doubleClickDelay = 400,displaylogo = F,scrollZoom = F, modeBarButtonsToRemove = list('hoverClosest3d','toImage')) %>% onRender(plot_inputs_3d)
        }
        return(meta_plot)
      }
      
      create_meta_plot <- function(plot_data,dims,split_groups,labels,plot_settings) {
        meta <- plot_data[,dims+1]
        name <- colnames(plot_data)[dims+1]
        groups <- gtools::mixedsort(funique(meta)) %>% fix_order()
        plot_data$color <- factor(as.character(meta),levels=groups)
        color_pal <- generate_colors(plot_settings$color_discrete,length(groups))
        color_pal[match("Undefined",groups)] <- "#D6D6D6"
        if (is.null(split_groups)) {
          label_info <- NULL
          if (labels) {
            label_info <- get_cluster_labels(plot_data,dims)
            cur_labels[["plot1"]] <- label_info
          }
          cur_visible(funique(plot_data$color))
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
            legend_colors <- funique(legend_colors$color)
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
            legend_colors <- funique(legend_colors$color)
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
              plot_ly(type="scatter3d",customdata=-1,mode="markers",scene=paste0("scene",length(split_groups)+group_num),visible=T) %>% add_annotations(text=split_groups[group_num],x=title_pos[group_num],y=0.98,z=0.5,showarrow=F, opacity=1,font=list(size=18))              
            })
            meta_plot <- plotly::subplot(c(title_list,plot_list),nrows = 2) %>% plotly::config(doubleClickDelay = 400,displaylogo = F,scrollZoom = F, modeBarButtonsToRemove = list('hoverClosest3d','toImage')) %>% onRender(subplot_inputs_3d) %>% event_register("plotly_legendclick")
            if (scene_num == 1) {
              meta_plot <- meta_plot %>% plotly::layout(font=list(family=default_font),title = list(text=name,y=0.98,font = list(size = 24)),
                scene = list(annotations=if(labels) label_list[[1]] else NULL,domain=list(x=c(0,1),y=c(0,0.9)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
                scene2 = list(domain=list(x=c(0,1),y=c(0.9,1)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
                scene3 = list(domain=list(x=c(0,0.01),y=c(0,0.01)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F,range=c(100,101)),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F))
              )
            } else if (scene_num == 2) {
              meta_plot <- meta_plot %>%  plotly::layout(font=list(family=default_font),title = list(text=name,y=0.98,font = list(size = 24)),
               scene = list(annotations=if(labels) label_list[[1]] else NULL,domain=list(x=c(0,0.5),y=c(0,0.9)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene2 = list(annotations=if(labels) label_list[[2]] else NULL,domain=list(x=c(0.5,1.0),y=c(0,0.9)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene3 = list(domain=list(x=c(0,0.5),y=c(0.9,1.0)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene4 = list(domain=list(x=c(0.5,1),y=c(0.9,1.0)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene5 = list(domain=list(x=c(0,0.01),y=c(0,0.01)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F,range=c(100,101)),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F))
              )  
            } else if (scene_num == 3) {
              meta_plot <- meta_plot %>%  plotly::layout(font=list(family=default_font),title = list(text=name,y=0.98,font = list(size = 24)),
               scene = list(annotations=if(labels) label_list[[1]] else NULL,domain=list(x=c(0,0.33),y=c(0,0.9)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene2 = list(annotations=if(labels) label_list[[2]] else NULL,domain=list(x=c(0.33,0.66),y=c(0,0.9)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene3 = list(annotations=if(labels) label_list[[3]] else NULL,domain=list(x=c(0.66,0.99),y=c(0,0.9)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene4 = list(domain=list(x=c(0,0.33),y=c(0.9,1.0)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene5 = list(domain=list(x=c(0.33,0.66),y=c(0.9,1.0)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene6 = list(domain=list(x=c(0.66,0.99),y=c(0.9,1.0)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene7 = list(domain=list(x=c(0,0.01),y=c(0,0.01)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F,range=c(100,101)),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F))
              )
            }
            meta_plot$x$layout <- meta_plot$x$layout[grep('NA', names(meta_plot$x$layout), invert = TRUE)]
            meta_plot
          }
        }
      }
      
      exp_plot_2D <- function(plot_data,plot_settings,color_scale,color_min,color_max,name,subplot="no_sub",subplot_num=0) {
        hover <- if (plot_settings$cellname) "%{text}<extra>%{marker.color:.2f}</extra>" else "%{marker.color:.2f}<extra></extra>" 
        exp_plot <- plot_ly(plot_data, x = ~dim1, y = ~dim2, customdata = rep(subplot_num,nrow(plot_data)), marker=list(color=plot_data[[name]],colorscale=colors_as_list(color_scale),opacity=plot_settings$point_transparent,size=plot_settings$point_size,cmin=color_min,cmax=color_max,showscale=if (subplot_num < 2) T else F,colorbar=list(len=250,lenmode="pixels",thickness=28,y=0.8)), unselected=list(marker=list(opacity=0.05)),text = rownames(plot_data), hovertemplate=hover,showlegend=F, type = 'scattergl', mode = 'markers', source = ns('exp_plot'), key = ~cellname) %>% 
          plotly::config(doubleClickDelay = 400,displaylogo = F,scrollZoom = F,modeBarButtons= list(list('drawopenpath','eraseshape'),list('select2d','lasso2d',reduct_clear_select),list('zoom2d','pan2d','resetScale2d'))) %>%
          plotly::layout(font=list(family=default_font),title = list(text=name,y=0.98,font = list(size = 24)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",hoverdistance=5,spikedistance=0,margin=list(t=40,b=10,l=20,r=120),legend=list(font = list(size = 16),entrywidth = 0,bgcolor="rgba(0, 0, 0, 0)"),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)"))
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
            font = list(size = 18)
          )
        } else {
          exp_plot <- exp_plot %>% onRender(plot_inputs_exp)
        }
        return(exp_plot)
      }
      
      exp_plot_3D <- function(plot_data,plot_settings,color_scale,color_min,color_max,name,subplot="no_sub",scene_num=1,subplot_num=0) {
        hover <- if (plot_settings$cellname) "%{text}<extra>%{marker.color:.2f}</extra>" else "%{marker.color:.2f}<extra></extra>" 
        exp_plot <- plot_ly(plot_data, x = ~dim1, y = ~dim2, z = ~dim3, customdata = rep(subplot_num,nrow(plot_data)), marker=list(color=plot_data[[name]],colorscale=colors_as_list(color_scale),opacity=plot_settings$point_transparent,size=plot_settings$point_size,cmin=color_min,cmax=color_max,showscale=if (subplot_num < 2) T else F,colorbar=list(len=250,lenmode="pixels",thickness=28,y=0.8)), text = rownames(plot_data), hovertemplate=hover, showlegend=F, type = 'scatter3d', mode = 'markers', source = ns("exp_plot"), scene = paste0("scene",scene_num), key = ~cellname) %>% 
          plotly::layout(font=list(family=default_font),title = list(text=name,y=0.98,font = list(size = 24)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",hoverdistance=5,spikedistance=0,margin=list(t=40,b=10,l=20,r=30),legend=list(font = list(size = 16),entrywidth = 0,bgcolor="rgba(0, 0, 0, 0)"),scene=list(xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)"))
        if (subplot == "no_sub") {
          exp_plot <- exp_plot %>% plotly::config(doubleClickDelay = 400,displaylogo = F,scrollZoom = F, modeBarButtonsToRemove = list('hoverClosest3d','toImage')) %>% onRender(plot_inputs_exp_3d)
        }
        return(exp_plot)
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
          slider_refresh(format(Sys.time(), "%H%M%S"))
          if (sum(values) == 0) {
            if (is.function(color_scale)) {
              color_scale <- "#D9D9D9"
            } else {
              color_scale <- if (color_scale == "viridis") "#440154FF" else if (color_scale == "plasma") "#0D0887FF"
            }          }
          if (dims == 2) {
            exp_plot_2D(plot_data,plot_settings,color_scale,color_min,color_max,name)
          } else {
            exp_plot_3D(plot_data,plot_settings,color_scale,color_min,color_max,name)
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
          slider_refresh(format(Sys.time(), "%H%M%S"))
          if (dims == 2) {
            plot_list <- lapply(1:length(split_groups), function(group_num) {
              group_name <- split_groups[group_num]
              group_data <- data_list[[as.character(group_name)]]
              values <- group_data[,dims+1]
              color_scale <- plot_settings$color_cont
              if (sum(values) == 0) {
                if (is.function(color_scale)) {
                  color_scale <- "#D9D9D9"
                } else {
                  color_scale <- if (color_scale == "viridis") "#440154FF" else if (color_scale == "plasma") "#0D0887FF"
                }          
              }
              exp_plot_2D(group_data,plot_settings,color_scale,color_min,color_max,name,group_name,group_num)
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
                  color_scale <- "#D9D9D9"
                } else {
                  color_scale <- if (color_scale == "viridis") "#440154FF" else if (color_scale == "plasma") "#0D0887FF"
                }          
              }
              scene_num <<- scene_num + 1
              exp_plot_3D(group_data,plot_settings,color_scale,color_min,color_max,name,group_name,scene_num,group_num)
            })
            title_pos <- c(0.5)
            if (length(split_groups) == 2) {
              title_pos <- c(0.2,0.8)
            } else if (length(split_groups) == 3) {
              title_pos <- c(0.12,0.5,0.88)
            }
            title_list <- lapply(1:length(split_groups), function(group_num) {
              group_data <- data_list[[group_num]]
              plot_ly(type="scatter3d",mode="markers",scene=paste0("scene",length(split_groups)+group_num),visible=T) %>% add_annotations(text=split_groups[group_num],x=title_pos[group_num],y=0.98,z=0.5,showarrow=F, opacity=1,font=list(size=18))              
            })
            exp_plot <- plotly::subplot(c(title_list,plot_list),nrows = 2, margin=c(0.02,0.2,0.02,0.02)) %>% plotly::config(doubleClickDelay = 400,displaylogo = F,scrollZoom = F, modeBarButtonsToRemove = list('hoverClosest3d','toImage')) %>% onRender(subplot_inputs_exp_3d)
            if (scene_num == 1) {
              exp_plot <- exp_plot %>% plotly::layout(font=list(family=default_font),title = list(text=name,y=0.98,font = list(size = 24)),
                scene = list(domain=list(x=c(0,1),y=c(0,0.9)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
                scene2 = list(domain=list(x=c(0,1),y=c(0.9,1)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F))
              )
            } else if (scene_num == 2) {
              exp_plot <- exp_plot %>%  plotly::layout(font=list(family=default_font),title = list(text=name,y=0.98,font = list(size = 24)),
                scene = list(domain=list(x=c(0,0.5),y=c(0,0.9)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
                scene2 = list(domain=list(x=c(0.5,1.0),y=c(0,0.9)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
                scene3 = list(domain=list(x=c(0,0.5),y=c(0.9,1.0)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
                scene4 = list(domain=list(x=c(0.5,1),y=c(0.9,1.0)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F))
                                                                       
              )  
            } else if (scene_num == 3) {
              exp_plot <- exp_plot %>%  plotly::layout(font=list(family=default_font),title = list(text=name,y=0.98,font = list(size = 24)),
               scene = list(domain=list(x=c(0,0.33),y=c(0,0.9)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene2 = list(domain=list(x=c(0.33,0.66),y=c(0,0.9)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene3 = list(domain=list(x=c(0.66,0.99),y=c(0,0.9)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene4 = list(domain=list(x=c(0,0.33),y=c(0.9,1.0)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene5 = list(domain=list(x=c(0.33,0.66),y=c(0.9,1.0)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F)),
               scene6 = list(domain=list(x=c(0.66,0.99),y=c(0.9,1.0)),xaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),yaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F),zaxis=list(title="",showgrid=F,zeroline=F,showticklabels=F))
              )
            }
            exp_plot$x$layout <- exp_plot$x$layout[grep('NA', names(exp_plot$x$layout), invert = TRUE)]
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
      
      scatter_plot_null <- function(plot_data,plot_settings,names,subplot="no_sub",subplot_num=0) {
        hover <- if (plot_settings$cellname) paste0(plot_data$cellname,"<br>x: ",plot_data$x,"<br>y: ",plot_data$y) else paste0("x: ",plot_data$x,"<br>y: ",plot_data$y)
        x_meta <- class(plot_data$x) == "factor"
        y_meta <- class(plot_data$y) == "factor"
        scatter_plot <- plot_ly(plot_data, x = if (x_meta) ~jitter(as.numeric(x),0.75) else ~x, y = if (y_meta) ~jitter(as.numeric(y),0.75) else ~y, customdata = rep(subplot_num,nrow(plot_data)), showlegend=F, opacity = plot_settings$point_transparent,marker=list(color="#b9c5fd",size=plot_settings$point_size), unselected=list(marker=list(opacity=0.05)), text = hover, hoverinfo='text', type = 'scattergl', mode = 'markers',source = ns('meta_plot'), key = ~cellname) %>%
        plotly::config(doubleClickDelay = 400,displaylogo = F,scrollZoom = F, modeBarButtons= list(list('drawopenpath','eraseshape'),list('select2d','lasso2d',reduct_clear_select),list('zoom2d','pan2d','resetScale2d'))) %>%
          plotly::layout(font=list(family=default_font),title = list(text=paste0(names$x," vs ",names$y),y=0.98,font = list(size = 22)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",hoverdistance=5,spikedistance=0,margin=list(t=40,b=10,l=if (subplot_num == 0) 80 else 60,r=60),xaxis=list(title=list(text=names$x,font=list(size=15)),tickfont=list(size=if (x_meta) 14 else 13),zeroline=F,tickmode=if (x_meta) "array" else "auto",tickvals=if (x_meta) 1:length(levels(plot_data$x)) else NULL,ticktext=if (x_meta) levels(plot_data$x) else NULL),yaxis=list(title=if (subplot_num <= 1) list(text=names$y,font=list(size=15)) else "",tickfont=list(size=if (y_meta) 14 else 13),zeroline=F,tickmode=if (y_meta) "array" else "auto",tickvals=if (y_meta) 1:length(levels(plot_data$y)) else NULL,ticktext=if (y_meta) levels(plot_data$y) else NULL),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
          event_register("plotly_legendclick")
        anno_list <- list()
        if (subplot != "no_sub") {
          anno_list <- c(anno_list,list(create_anno_reduct(0.5,subplot,F)))
        } else {
          scatter_plot <- scatter_plot %>% onRender(plot_inputs)
        }
        if (length(anno_list) > 0) {
          scatter_plot <- scatter_plot %>% layout(annotations=anno_list)
        }
        return(scatter_plot)
      }
      
      scatter_plot_meta <- function(plot_data,plot_settings,color_pal,names,subplot="no_sub",subplot_num=0,showlegend=T,visible=T) {
        hover <- if (plot_settings$cellname && visible) paste0(plot_data$cellname,"<br>x: ",plot_data$x,"<br>y: ",plot_data$y,"<br>Group: ",plot_data$color) else if (visible) paste0("x: ",plot_data$x,"<br>y: ",plot_data$y,"<br>Group: ",plot_data$color) else ""
        x_meta <- class(plot_data$x) == "factor"
        y_meta <- class(plot_data$y) == "factor"
        scatter_plot <- plot_ly(plot_data, x = if (x_meta) ~jitter(as.numeric(x),0.75) else ~x, y = if (y_meta) ~jitter(as.numeric(y),0.75) else ~y, color = ~color, colors = color_pal, customdata = rep(subplot_num,nrow(plot_data)), legendgroup= ~color, showlegend = showlegend, opacity = plot_settings$point_transparent,marker=list(size=plot_settings$point_size), unselected=list(marker=list(opacity=0.05)), text = hover, hoverinfo='text', type = 'scattergl', mode = 'markers',source = ns('meta_plot'), key = ~cellname) %>%
          plotly::config(doubleClickDelay = 400,displaylogo = F,scrollZoom = F, modeBarButtons= list(list('drawopenpath','eraseshape'),list('select2d','lasso2d',reduct_select_all,reduct_clear_select),list('zoom2d','pan2d','resetScale2d'))) %>%
          plotly::layout(font=list(family=default_font),title = list(text=paste0(names$x," vs ",names$y,"<br><span style='font-size: 17px;'>Colored by ",names$col,"</span>"),y=0.96,font = list(size = 22)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",hoverdistance=5,spikedistance=0,margin=list(t=if (subplot_num==0) 40 else 60,b=10,l=if (subplot_num == 0) 80 else 60,r=60),legend=list(font = list(size = 16),itemsizing='constant',entrywidth = 0,bgcolor="rgba(0, 0, 0, 0)"),xaxis=list(title=if (visible) list(text=names$x,font=list(size=15)) else "",tickfont=list(size=if (x_meta) 14 else 13),showgrid=visible,zeroline=F,tickmode=if (x_meta) "array" else "auto",tickvals=if (x_meta) 1:length(levels(plot_data$x)) else NULL,ticktext=if (x_meta) levels(plot_data$x) else NULL,showticklabels=visible,range=if(visible) NULL else c(100,101)),yaxis=list(title=if (visible && subplot_num <= 1) list(text=names$y,font=list(size=15)) else "",tickfont=list(size=if (y_meta) 14 else 13),showgrid=T,zeroline=F,tickmode=if (y_meta) "array" else "auto",tickvals=if (y_meta) 1:length(levels(plot_data$y)) else NULL,ticktext=if (y_meta) levels(plot_data$y) else NULL,showticklabels=visible,range=NULL),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
          event_register("plotly_legendclick")
        anno_list <- list()
        if (subplot != "no_sub") {
          anno_list <- c(anno_list,list(create_anno_reduct(0.5,subplot,F)))
        } else {
          scatter_plot <- scatter_plot %>% onRender(plot_inputs)
        }
        if (length(anno_list) > 0) {
          scatter_plot <- scatter_plot %>% layout(annotations=anno_list)
        }
        return(scatter_plot)
      }
      
      scatter_plot_exp <- function(plot_data,plot_settings,color_scale,color_min,color_max,names,subplot="no_sub",subplot_num=0) {
        hover <- if (plot_settings$cellname) paste0(plot_data$cellname,"<br>x: ",plot_data$x,"<br>y: ",plot_data$y,"<br>Value: ",plot_data$color) else paste0("x: ",plot_data$x,"<br>y: ",plot_data$y,"<br>Value: ",plot_data$color)
        x_meta <- class(plot_data$x) == "factor"
        y_meta <- class(plot_data$y) == "factor"
        scatter_plot <- plot_ly(plot_data, x = if (x_meta) ~jitter(as.numeric(x),0.75) else ~x, y = if (y_meta) ~jitter(as.numeric(y),0.75) else ~y, customdata = rep(subplot_num,nrow(plot_data)), marker=list(color=plot_data$color,colorscale=colors_as_list(color_scale), opacity=plot_settings$point_transparent,size=plot_settings$point_size,cmin=color_min,cmax=color_max,showscale=if (subplot_num < 2) T else F,colorbar=list(len=250,lenmode="pixels",thickness=28,y=0.8)), showlegend=F, unselected=list(marker=list(opacity=0.05)), text = hover, hoverinfo = 'text', type = 'scattergl', mode = 'markers',source = ns('exp_plot'), key = ~cellname) %>%
          plotly::config(doubleClickDelay = 400,displaylogo = F,scrollZoom = F,modeBarButtons= list(list('drawopenpath','eraseshape'),list('select2d','lasso2d',reduct_clear_select),list('zoom2d','pan2d','resetScale2d'))) %>%
          plotly::layout(font=list(family=default_font),title = list(text=paste0(names$x," vs ",names$y,"<br><span style='font-size: 17px;'>Colored by ",names$col,"</span>"),y=0.96,font = list(size = 22)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",hoverdistance=5,spikedistance=0,margin=list(t=if (subplot_num==0) 40 else 60,b=10,l=if (subplot_num == 0) 80 else 60,r=120),legend=list(font = list(size = 16),entrywidth = 0,bgcolor="rgba(0, 0, 0, 0)"),xaxis=list(title=list(text=names$x,font=list(size=15)),tickfont=list(size=if (x_meta) 14 else 13),zeroline=F,tickmode=if (x_meta) "array" else "auto",tickvals=if (x_meta) 1:length(levels(plot_data$x)) else NULL,ticktext=if (x_meta) levels(plot_data$x) else NULL),yaxis=list(title=if (subplot_num <= 1) list(text=names$y,font=list(size=15)) else "",tickfont=list(size=if (y_meta) 14 else 13),zeroline=F,tickmode=if (y_meta) "array" else "auto",tickvals=if (y_meta) 1:length(levels(plot_data$y)) else NULL,ticktext=if (y_meta) levels(plot_data$y) else NULL),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)"))
        if (subplot != "no_sub") {
          scatter_plot <- scatter_plot  %>% add_annotations(
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
          scatter_plot <- scatter_plot %>% onRender(plot_inputs_exp)
        }
        return(scatter_plot)
      }
      
      plot_scatter <- function(plot_data,color_type,names,split_groups,plot_settings) {
        data_color <- plot_data$color
        if (color_type == "none") {
          if (is.null(split_groups)) {
            scatter_plot_null(plot_data,plot_settings,names)
          } else {
            data_list <- lapply(split_groups,function(group) plot_data %>% filter(split == group))
            names(data_list) <- split_groups
            plot_list <- lapply(1:length(split_groups), function(group_num) {
              group_name <- split_groups[group_num]
              group_data <- data_list[[as.character(group_name)]]
              scatter_plot_null(group_data,plot_settings,names,group_name,group_num)
            })
            subplot_widths = c(1.0)
            if (length(plot_list) == 2) {
              subplot_widths <- c(0.5,0.5)
            } else if (length(plot_list) == 3) {
              subplot_widths <- c(0.33,0.33,0.33)
            }
            plotly::subplot(plot_list,nrows=1,widths=subplot_widths,titleX = T,shareY = F, titleY = T) %>% onRender(subplot_inputs_null) %>% event_register("plotly_legendclick")
          }
        } else if (color_type == "metadata") {
          groups <- gtools::mixedsort(funique(data_color)) %>% fix_order()
          data_color <- factor(as.character(data_color),levels=groups)
          plot_data$color <- data_color
          color_pal <- generate_colors(plot_settings$color_discrete,length(groups))
          color_pal[match("Undefined",groups)] <- "#D6D6D6"
          if (is.null(split_groups)) {
            scatter_plot_meta(plot_data,plot_settings,color_pal,names)
          } else {
            data_list <- lapply(split_groups,function(group) plot_data %>% filter(split == group))
            names(data_list) <- split_groups
            plot_list <- lapply(1:length(split_groups), function(group_num) {
              group_name <- split_groups[group_num]
              group_data <- data_list[[as.character(group_name)]]
              scatter_plot_meta(group_data,plot_settings,color_pal,names,group_name,group_num,F,T)
            })
            legend_colors <- plot_data %>% filter(!is.na(split))
            legend_colors <- funique(legend_colors$color)
            cur_visible(legend_colors)
            legend_data <- data.frame(x=rep(0,length(legend_colors)),y=rep(0,length(legend_colors)),color=legend_colors,cellname=1:length(legend_colors))
            legend_plot <- scatter_plot_meta(legend_data,plot_settings,color_pal,names,"no_sub",0,T,F)
            plot_list <- c(list(legend_plot),plot_list)
            subplot_widths = c(0,1.0)
            if (length(plot_list) == 3) {
              subplot_widths <- c(0,0.5,0.5)
            } else if (length(plot_list) == 4) {
              subplot_widths <- c(0,0.33,0.33,0.33)
            }
            plotly::subplot(plot_list,nrows=1,widths=subplot_widths,titleX = T,shareY = F, titleY = T) %>% onRender(subplot_inputs) %>% event_register("plotly_legendclick")
          }
        } else if (color_type == "feature") {
          if (is.null(split_groups)) {
            color_scale <- plot_settings$color_cont
            color_min <- max(0,min(data_color))
            color_max <- max(data_color)
            exp_range$min <- color_min
            exp_range$max <- color_max
            slider_refresh(format(Sys.time(), "%H%M%S"))
            if (sum(data_color) == 0) {
              if (is.function(color_scale)) {
                color_scale <- "#D9D9D9"
              } else {
                color_scale <- if (color_scale == "viridis") "#440154FF" else if (color_scale == "plasma") "#0D0887FF"
              }
            }
            scatter_plot_exp(plot_data,plot_settings,color_scale,color_min,color_max,names)
          } else {
            data_list <- lapply(split_groups,function(group) plot_data %>% filter(split == group))
            names(data_list) <- split_groups
            data_min <- sapply(data_list, function(data) min(data$color))
            data_max <- sapply(data_list, function(data) max(data$color))
            color_min <- max(0,min(data_min))
            color_max <- max(data_max)
            exp_range$min <- color_min
            exp_range$max <- color_max
            slider_refresh(format(Sys.time(), "%H%M%S"))
            plot_list <- lapply(1:length(split_groups), function(group_num) {
              group_name <- split_groups[group_num]
              group_data <- data_list[[as.character(group_name)]]
              values <- group_data$color
              color_scale <- plot_settings$color_cont
              if (sum(values) == 0) {
                if (is.function(color_scale)) {
                  color_scale <- "#D9D9D9"
                } else {
                  color_scale <- if (color_scale == "viridis") "#440154FF" else if (color_scale == "plasma") "#0D0887FF"
                }          
              }
              scatter_plot_exp(group_data,plot_settings,color_scale,color_min,color_max,names,group_name,group_num)
            })
            plotly::subplot(plot_list,nrows=1,titleX = T,shareY = F, titleY = T) %>% onRender(subplot_inputs_exp)
          }
        }
      }
      
      plot_violin <- function(gene_select,meta_select,plot_meta,gene_set,plot_settings,split,split_order,set_name,set_list) {
        validate(
          need(gene_select,"")
        )
        y_name <- if (gene_set == "Other features") "Value" else "Expression"
        feature_name <- gene_select
        if (feature_name == "Gene set score") {
          features <- get_scores(set_list())
          feature_name <- set_name()
        } else if (feature_name %in% dataset$quality) {
          features <- fetch_data(meta=feature_name)
        } else {
          features <- fetch_data(genes=feature_name)
        }
        plot_data <- data.frame(features,check.names = F)
        if (meta_select == "All Data") {
          if (is.null(split())) {
            colnames(plot_data) <- c("value")
            plot_data$meta <- "All Data"
            plot_ly(plot_data, type = "violin", y = ~value, split = ~meta, color= ~meta, colors=c("#96a8fc"), spanmode="hard", hoveron="violins+kde",box=list(visible=T),meanline=list(visible=T)) %>%
              plotly::layout(font=list(family=default_font),title=list(text=feature_name,y=0.98,font = list(size = 24)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=40,b=10,l=90,r=20),yaxis=list(title=list(text=y_name,font=list(size=15)),zeroline=F,tickfont=list(size=13)),xaxis=list(showticklabels = F),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
              plotly::config(doubleClickDelay = 400,displaylogo = F,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d')))
          } else {
            plot_data <- data.frame(cbind(plot_data,split()))
            colnames(plot_data) <- c("value","split")
            plot_data <- plot_data[!is.na(plot_data$split),]
            order <- split_order()
            plot_data$split <- factor(plot_data$split,levels=order)          
            color_pal <- generate_colors(plot_settings$color_discrete,length(order))
            color_pal[match("Undefined",order)] <- "#D6D6D6"
            plot_ly(plot_data, type = "violin", y = ~value, split = ~split, color= ~split, colors=color_pal, spanmode="hard", hoveron="violins+kde",box=list(visible=T),meanline=list(visible=T)) %>%
              plotly::layout(font=list(family=default_font),title=list(text=feature_name,y=0.98,font = list(size = 24)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=40,b=10,l=90,r=20),legend=list(font = list(size = 16),entrywidth = 0,bgcolor="rgba(0, 0, 0, 0)"),yaxis=list(title=list(text=y_name,font=list(size=15)),zeroline=F,tickfont=list(size=13)),xaxis=list(showticklabels = F),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
              plotly::config(doubleClickDelay = 400,displaylogo = F,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d')))
          }
        } else {
          validate(
            need(plot_meta(),""),
            need(length(plot_meta()) == length(dataset$subset),"")
          )
          if (is.null(split())) {
            plot_data <- data.frame(cbind(plot_data,plot_meta()))
            colnames(plot_data) <- c("value","meta")
            order <- gtools::mixedsort(funique(plot_data$meta)) %>% fix_order()
            plot_data$meta <- factor(plot_data$meta,levels=order)          
            color_pal <- generate_colors(plot_settings$color_discrete,length(order))
            color_pal[match("Undefined",order)] <- "#D6D6D6"
            plot_ly(plot_data, type = "violin", x = ~meta, y = ~value, split = ~meta, color= ~meta, colors=color_pal, spanmode="hard", hoveron="violins+kde",box=list(visible=T),meanline=list(visible=T)) %>%
              plotly::layout(font=list(family=default_font),title=list(text=feature_name,y=0.98,font = list(size = 24)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=40,b=10,l=90,r=60),legend=list(font = list(size = 16),entrywidth = 0,bgcolor="rgba(0, 0, 0, 0)"),yaxis=list(title=list(text=y_name,font=list(size=15)),zeroline=F,tickfont=list(size=13)),xaxis=list(title=list(text=meta_select,font=list(size=15)),tickfont=list(size=14)),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
              plotly::config(doubleClickDelay = 400,displaylogo = F,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d')))
          } else {
            plot_data <- data.frame(cbind(plot_data,plot_meta(),split()))
            colnames(plot_data) <- c("value","meta","split")
            plot_data <- plot_data[!is.na(plot_data$split),]
            meta_order <- gtools::mixedsort(funique(plot_data$meta)) %>% fix_order()
            split_order <- split_order()
            color_pal <- generate_colors(plot_settings$color_discrete,3)
            options(warn=-1)
            plot <- plot_data %>% plot_ly(type = 'violin')
            for (x in 1:length(split_order)) {
              cur_split <- split_order[x]
              cur_color <- color_pal[x]
              plot <- plot %>% add_trace(x = as.character(plot_data$meta[plot_data$split == cur_split]),y = plot_data$value[plot_data$split == cur_split], legendgroup = cur_split,scalegroup = cur_split,name = cur_split,color = I(cur_color),spanmode="hard", hoveron="violins+kde",box=list(visible=T),meanline=list(visible=T)) 
            }
            plot <- plot %>% plotly::layout(font=list(family=default_font),violinmode="group",title=list(text=feature_name,y=0.98,font = list(size = 24)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=40,b=10,l=90,r=60),legend=list(font = list(size = 16),entrywidth = 0,bgcolor="rgba(0, 0, 0, 0)"),yaxis=list(title=list(text=y_name,font=list(size=15)),zeroline=F,tickfont=list(size=13)),xaxis=list(title=list(text=meta_select,font=list(size=15)),tickfont=list(size=14),autotypenumbers="strict",categorymode="array",categoryarray=meta_order),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
              plotly::config(doubleClickDelay = 400,displaylogo = F,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d')))
            shinyjs::delay(100,options(warn=1))
            plot
          }
        }
      }
      
      plot_heatmap <- function(plot_type,geneset,set_name,meta_select,plot_meta,flip,scaling,clustering,averaging,plot_settings) {
        if (plot_type == "Heatmap") {
          validate(
            need(geneset,""),
            need(length(geneset) > 1,""),
            need(meta_select,"")
          )
          if (!averaging) {
            # Per-cell heatmap
            plot_data <- data.frame(fetch_data(genes=geneset),check.names = F)
            if (nrow(plot_data) * ncol(plot_data) > 1000000) {
              showNotification("Dimensions too big! Try downsampling/averaging.", type = "message")
              return()
            }
            fix_values <- F
            color_type <- plot_settings$color_cont
            if (scaling) {
              plot_data <- scale(plot_data) %>% as.matrix(plot_data) 
              plot_data[is.nan(plot_data)] <- -3
              plot_data <- MinMax(plot_data,-3,3)
              color_type <- plot_settings$color_scaled
              fix_values <- T
            }
            if (nrow(plot_data) >= 2 && clustering) {
              x_order <- hclust(dist(plot_data))$order
            } else if (meta_select != "All Cells") {
              meta <- plot_meta()
              meta_order <- gtools::mixedsort(funique(meta))
              x_order <- order(match(meta,meta_order))
            } else {
              x_order <- 1:nrow(plot_data)
            }
            plot_data <- plot_data[x_order,,drop=F] %>% round(3)
            if (ncol(plot_data) >= 2 && clustering) {
              y_order <- hclust(dist(t(plot_data)))$order
              plot_data <- plot_data[,y_order,drop=F]
            }
            if (!flip) {
              plot_data <- t(plot_data)
              hover_1 <- matrix(colnames(plot_data),nrow=nrow(plot_data),ncol=ncol(plot_data),byrow=T)
              hover_2 <- matrix(rownames(plot_data),nrow=nrow(plot_data),ncol=ncol(plot_data),byrow=F)
              if (meta_select == "All Cells") {
                hover_final <- matrix(paste0("x: ",hover_1,"\n","y: ",hover_2,"\n","Exp: ",round(plot_data,2)),nrow(plot_data),ncol(plot_data))
                plot_ly(x=1:ncol(plot_data),y=rownames(plot_data),z=plot_data,colors=color_type,zmid=if(fix_values) 0 else NULL,type="heatmap",colorbar=list(len=200,lenmode="pixels",thickness=28,title=list(text="Avg Exp")),hoverinfo="text",hovertext=hover_final) %>% 
                  plotly::layout(font=list(family=default_font),title=list(text=set_name,y=0.98,font = list(size = 24)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=40,b=25,l=90,r=45),yaxis=list(title=list(text="Genes",standoff=8,font=list(size=15)),tickfont=list(size=13)),xaxis=list(title=list(text="Cells",font=list(size=15)),showticklabels=F,autotypenumbers = 'strict'),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
                  plotly::config(doubleClickDelay = 400,displaylogo = F,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d')))
              } else {
                meta_list <- plot_meta()[x_order]
                meta_length <- length(meta_list)
                groups <- gtools::mixedsort(funique(meta_list)) %>% fix_order()
                color_pal <- generate_colors(plot_settings$color_discrete,length(groups))
                color_pal[match("Undefined",groups)] <- "#D6D6D6"
                meta_colors <- plot_ly(x=1:meta_length,y=rep("X",meta_length),z=as.numeric(as.factor(meta_list))/fnunique(meta_list),colors=color_pal,showscale=F,type="heatmap",hoverinfo = 'text', hovertext = meta_list) %>%
                  plotly::layout(font=list(family=default_font),title=list(text=set_name,y=0.98,font = list(size = 24)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=40,b=25,l=90,r=45),yaxis=list(showticklabels=F,showgrid=F,zeroline=F),xaxis=list(title=list(text="Cells",font=list(size=15)),showticklabels=F,showgrid=F,zeroline=F),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
                  plotly::config(doubleClickDelay = 400,displaylogo = F,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d')))
                hover_3 <- matrix(meta_list,nrow=nrow(plot_data),ncol=ncol(plot_data),byrow=T)
                hover_final <- matrix(paste0("x: ",hover_1,"\n","y: ",hover_2,"\n","Exp: ",round(plot_data,2),"\n","Group: ",hover_3),nrow(plot_data),ncol(plot_data))
                heatmap <- plot_ly(x=1:ncol(plot_data),y=rownames(plot_data),z=plot_data,colors=color_type,zmid=if(fix_values) 0 else NULL,type="heatmap",colorbar=list(len=200,lenmode="pixels",thickness=28,title=list(text="Avg Exp")),hoverinfo="text",hovertext=hover_final) %>% 
                  plotly::layout(font=list(family=default_font),title=list(text=set_name,y=0.98,font = list(size = 24)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=40,b=25,l=90,r=45),yaxis=list(title=list(text="Genes",standoff=6,font=list(size=15)),tickfont=list(size=13)),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
                  plotly::config(doubleClickDelay = 400,displaylogo = F,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d')))
                plotly::subplot(list(heatmap,meta_colors),which_layout = 2, nrows = 2,heights = c(0.94,0.06),shareX=T,titleX = T,titleY = T)
              }
            } else {
              plot_data <- as.matrix(plot_data)
              hover_1 <- matrix(colnames(plot_data),nrow=nrow(plot_data),ncol=ncol(plot_data),byrow=T)
              hover_2 <- matrix(rownames(plot_data),nrow=nrow(plot_data),ncol=ncol(plot_data),byrow=F)
              if (meta_select == "All Cells") {
                hover_final <- matrix(paste0("x: ",hover_1,"\n","y: ",hover_2,"\n","Exp: ",round(plot_data,2)),nrow(plot_data),ncol(plot_data))
                plot_ly(x=colnames(plot_data),y=1:nrow(plot_data),z=plot_data,colors=color_type,zmid=if(fix_values) 0 else NULL,type="heatmap",colorbar=list(len=200,lenmode="pixels",thickness=28,title=list(text="Avg Exp")),hoverinfo="text",hovertext=hover_final) %>% 
                  plotly::layout(font=list(family=default_font),title=list(text=set_name,y=0.98,font = list(size = 24)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=40,b=25,l=60,r=45),yaxis=list(title=list(text="Cells",font=list(size=15)),showticklabels=F,autotypenumbers = 'strict'),xaxis=list(title=list(text="Genes",standoff=8,font=list(size=15)),tickfont=list(size=13)),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
                  plotly::config(doubleClickDelay = 400,displaylogo = F,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d')))
              } else {
                meta_list <- plot_meta()[x_order]
                meta_length <- length(meta_list)
                groups <- gtools::mixedsort(funique(meta_list)) %>% fix_order()
                color_pal <- generate_colors(plot_settings$color_discrete,length(groups))
                color_pal[match("Undefined",groups)] <- "#D6D6D6"
                meta_colors <- plot_ly(x=rep("X",meta_length),y=1:meta_length,z=as.numeric(as.factor(meta_list))/fnunique(meta_list),colors=color_pal,showscale=F,type="heatmap",hoverinfo = 'text', hovertext = meta_list) %>%
                  plotly::layout(font=list(family=default_font),title=list(text=set_name,y=0.98,font = list(size = 24)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=40,b=25,l=60,r=45),yaxis=list(title=list(text="Cells",font=list(size=15)),showticklabels=F,showgrid=F,zeroline=F),xaxis=list(showticklabels=F,showgrid=F,zeroline=F),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
                  plotly::config(doubleClickDelay = 400,displaylogo = F,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d')))
                hover_3 <- matrix(meta_list,nrow=nrow(plot_data),ncol=ncol(plot_data),byrow=F)
                hover_final <- matrix(paste0("x: ",hover_1,"\n","y: ",hover_2,"\n","Exp: ",round(plot_data,2),"\n","Group: ",hover_3),nrow(plot_data),ncol(plot_data))
                heatmap <- plot_ly(x=colnames(plot_data),y=1:nrow(plot_data),z=plot_data,colors=color_type,zmid=if(fix_values) 0 else NULL,type="heatmap",colorbar=list(len=200,lenmode="pixels",thickness=28,title=list(text="Avg Exp")),hoverinfo="text",hovertext=hover_final) %>% 
                  plotly::layout(font=list(family=default_font),title=list(text=set_name,y=0.98,font = list(size = 24)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=40,b=25,l=60,r=45),xaxis=list(title=list(text="Genes",standoff=8,font=list(size=15)),tickfont=list(size=13)),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
                  plotly::config(doubleClickDelay = 400,displaylogo = F,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d')))
                plotly::subplot(list(meta_colors,heatmap),which_layout = 1,nrows = 1,widths = c(0.04,0.96),shareY=T,titleX = T,titleY = T)
              }
            }
          } else {
            # Averaged heatmap
            if (meta_select == "All Cells") {
              plot_data <- data.frame(fetch_data(genes=geneset),check.names = F)
              plot_means <- plot_data %>% fsummarise(across(all_of(geneset),get_avg_exp)) %>% data.frame() %>% round(3)
              rownames(plot_means) <- "All"
              if (clustering && ncol(plot_means) >= 2) {
                y_order <- hclust(dist(t(plot_means)))$order
                plot_means <- plot_means[,y_order,drop=F]
                geneset <- geneset[y_order]
              }
              color_type <- plot_settings$color_cont
              if (!flip) {
                plot_means <- t(plot_means)
                hover_1 <- matrix(colnames(plot_means),nrow=nrow(plot_means),ncol=ncol(plot_means),byrow=T)
                hover_2 <- matrix(rownames(plot_means),nrow=nrow(plot_means),ncol=ncol(plot_means),byrow=F)
                hover_final <- matrix(paste0("x: ",hover_1,"\n","y: ",hover_2,"\n","Exp: ",round(plot_means,2)),nrow(plot_means),ncol(plot_means))
                plot_ly(x=colnames(plot_means),y=geneset,z=plot_means,colors=color_type,type="heatmap",colorbar=list(len=200,lenmode="pixels",thickness=28,title=list(text="Avg Exp")),hoverinfo="text",hovertext=hover_final) %>% 
                  plotly::layout(font=list(family=default_font),title=list(text=set_name,y=0.98,font = list(size = 24)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=40,b=25,l=100,r=45),yaxis=list(title=list(text="Genes",standoff=8,font=list(size=15)),tickfont=list(size=13)),xaxis=list(title=list(text="Cells",standoff=8,font=list(size=15)),tickfont=list(size=14),autotypenumbers = 'strict'),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
                  plotly::config(doubleClickDelay = 400,displaylogo = F,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d')))
              } else {
                plot_means <- as.matrix(plot_means)
                hover_1 <- matrix(colnames(plot_means),nrow=nrow(plot_means),ncol=ncol(plot_means),byrow=T)
                hover_2 <- matrix(rownames(plot_means),nrow=nrow(plot_means),ncol=ncol(plot_means),byrow=F)
                hover_final <- matrix(paste0("x: ",hover_1,"\n","y: ",hover_2,"\n","Exp: ",round(plot_means,2)),nrow(plot_means),ncol(plot_means))
                plot_ly(x=geneset,y=rownames(plot_means),z=plot_means,colors=color_type,type="heatmap",colorbar=list(len=200,lenmode="pixels",thickness=28,title=list(text="Avg Exp")),hoverinfo="text",hovertext=hover_final) %>% 
                  plotly::layout(font=list(family=default_font),title=list(text=set_name,y=0.98,font = list(size = 24)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=40,b=25,l=100,r=45),yaxis=list(title=list(text="Cells",standoff=8,font=list(size=15)),tickfont=list(size=14),autotypenumbers = 'strict'),xaxis=list(title=list(text="Genes",standoff=8,font=list(size=15)),tickfont=list(size=13)),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
                  plotly::config(doubleClickDelay = 400,displaylogo = F,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d')))
              }
            } else {
              validate(
                need(plot_meta(),""),
                need(length(plot_meta()) == length(dataset$subset),"")
              )
              plot_data <- data.frame(fetch_data(genes=geneset),plot_meta(),check.names = F)
              colnames(plot_data)[ncol(plot_data)] <- meta_select
              fix_values <- F
              color_type <- plot_settings$color_cont
              if (fnunique(plot_meta()) > 1 && scaling) {
                plot_means <- plot_data %>% group_by(get(meta_select)) %>% fsummarise(across(all_of(geneset),get_avg_exp)) %>% mutate(across(all_of(geneset), ~ as.numeric(scale(log1p(.x))))) %>% mutate(across(all_of(geneset), ~ replace(.x, is.nan(.x), -3))) %>% data.frame()
                fix_values <- T
                color_type <- plot_settings$color_scaled
              } else {
                plot_means <- plot_data %>% group_by(get(meta_select)) %>% fsummarise(across(all_of(geneset),get_avg_exp)) %>% data.frame()
              }
              rownames(plot_means) <- plot_means[,1]
              plot_means <- as.matrix(plot_means[,2:ncol(plot_means)]) %>% round(3)
              if (fix_values) {
                plot_means <- MinMax(plot_means,-3,3)
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
                hover_1 <- matrix(colnames(plot_means),nrow=nrow(plot_means),ncol=ncol(plot_means),byrow=T)
                hover_2 <- matrix(rownames(plot_means),nrow=nrow(plot_means),ncol=ncol(plot_means),byrow=F)
                hover_final <- matrix(paste0("x: ",hover_1,"\n","y: ",hover_2,"\n","Exp: ",round(plot_means,2)),nrow(plot_means),ncol(plot_means))
                plot_ly(x=colnames(plot_means),y=geneset,z=plot_means,colors=color_type,zmid=if(fix_values) 0 else NULL,type="heatmap",colorbar=list(len=200,lenmode="pixels",thickness=28,title=list(text="Avg Exp")),hoverinfo="text",hovertext=hover_final) %>% 
                  plotly::layout(font=list(family=default_font),title=list(text=set_name,y=0.98,font = list(size = 24)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=40,b=25,l=100,r=45),yaxis=list(title=list(text="Genes",standoff=8,font=list(size=15)),tickfont=list(size=13)),xaxis=list(title=list(text=meta_select,standoff=8,font=list(size=15)),tickfont=list(size=14),autotypenumbers = 'strict'),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
                  plotly::config(doubleClickDelay = 400,displaylogo = F,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d')))
              } else {
                hover_1 <- matrix(colnames(plot_means),nrow=nrow(plot_means),ncol=ncol(plot_means),byrow=T)
                hover_2 <- matrix(rownames(plot_means),nrow=nrow(plot_means),ncol=ncol(plot_means),byrow=F)
                hover_final <- matrix(paste0("x: ",hover_1,"\n","y: ",hover_2,"\n","Exp: ",round(plot_means,2)),nrow(plot_means),ncol(plot_means))
                plot_ly(x=geneset,y=rownames(plot_means),z=plot_means,colors=color_type,zmid=if(fix_values) 0 else NULL,type="heatmap",colorbar=list(len=200,lenmode="pixels",thickness=28,title=list(text="Avg Exp")),hoverinfo="text",hovertext=hover_final) %>% 
                  plotly::layout(font=list(family=default_font),title=list(text=set_name,y=0.98,font = list(size = 24)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=40,b=25,l=100,r=45),yaxis=list(title=list(text=meta_select,standoff=8,font=list(size=15)),tickfont=list(size=14),autotypenumbers = 'strict'),xaxis=list(title=list(text="Genes",standoff=8,font=list(size=15)),tickfont=list(size=13)),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
                  plotly::config(doubleClickDelay = 400,displaylogo = F,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d')))
              }
            }
          }
        } else {
          # Bubble plots
          validate(
            need(geneset,""),
            need(meta_select,"")
          )
          if (meta_select == "All Cells") {
            plot_data_raw <- data.frame(fetch_data(genes=geneset),check.names = F)
            colorscale <- colors_as_list(plot_settings$color_cont)
            plot_means <- plot_data_raw %>% fsummarise(across(all_of(geneset),get_avg_exp))
            plot_percents <- plot_data_raw %>% fsummarise(across(all_of(geneset),get_percent_exp)) %>% pivot_longer(cols=all_of(geneset), names_to="Gene")
            plot_means_final <- plot_means %>% pivot_longer(cols=all_of(geneset), names_to="Gene")
            plot_data <- cbind(plot_means_final,plot_percents$value)
            colnames(plot_data) <- c("Gene","Expression","Percent")
            if (clustering) {
              plot_means <- as.data.frame(plot_means)
              rownames(plot_means) <- "All"
              gene_list <- colnames(plot_means)
              if (ncol(plot_means) >= 2) {
                order <- hclust(dist(t(plot_means)))$order
                gene_list <- gene_list[order]
              }
              plot_data$Gene <- factor(plot_data$Gene, levels = gene_list) 
              plot_data <- plot_data[order(plot_data$Gene), ]
            }
            plot_data$Meta <- "All"
            plot_data$Gene <- as.character(plot_data$Gene)
            plot_data$Color <- round(plot_data$Expression,3)
            plot_data$Percent <- round(plot_data$Percent*100,1)
            plot_data$Size <- plot_data$Percent
            plot_data$Size[plot_data$Percent < 1.0] <- NA
            if (!flip) {
              plot_ly(plot_data,x=~Meta,y=~Gene,type="scatter",mode = "markers",marker = list(sizemode = "area",size=~Size,sizeref=0.18,color=~Color,colorscale=colorscale,colorbar=list(len=200,lenmode="pixels",thickness=28,title=list(text="Avg Exp"),y=0.8),line=list(width=0)),hoverinfo = 'text', hovertext = paste0("x: ",plot_data$Meta,"\n","y: ",plot_data$Gene,"\n","Exp: ",round(plot_data$Color,2),"\n","Percent: ",round(plot_data$Percent,2),"%")) %>%
                plotly::layout(font=list(family=default_font),title=list(text=set_name,y=0.98,font = list(size = 24)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=30,b=25,l=100,r=45),xaxis=list(title=list(text="Cells",standoff=8,font=list(size=15)),tickfont=list(size=14),showgrid=F,zeroline=T),yaxis=list(title=list(text="Genes",standoff=8,font=list(size=15)),tickfont=list(size=13),showgrid=F,zeroline=T,categoryorder="array",categoryarray=plot_data$Gene),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
                plotly::config(doubleClickDelay = 400,displaylogo = F,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d')))            
            } else {
            plot_ly(plot_data,x=~Gene,y=~Meta,type="scatter",mode = "markers",marker = list(sizemode = "area",size=~Size,sizeref=0.18,color=~Color,colorscale=colorscale,colorbar=list(len=200,lenmode="pixels",thickness=28,title=list(text="Avg Exp"),y=0.8),line=list(width=0)),hoverinfo = 'text', hovertext = paste0("x: ",plot_data$Gene,"\n","y: ",plot_data$Meta,"\n","Exp: ",round(plot_data$Color,2),"\n","Percent: ",round(plot_data$Percent,2),"%")) %>%
              plotly::layout(font=list(family=default_font),title=list(text=set_name,y=0.98,font = list(size = 24)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=30,b=25,l=100,r=45),xaxis=list(title=list(text="Genes",standoff=8,font=list(size=15)),tickfont=list(size=13),showgrid=F,zeroline=T,categoryorder="array",categoryarray=plot_data$Gene),yaxis=list(title=list(text="Cells",standoff=8,font=list(size=15)),tickfont=list(size=14),showgrid=F,zeroline=T),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
              plotly::config(doubleClickDelay = 400,displaylogo = F,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d')))
            }
          }
          else {
            validate(
              need(plot_meta(),""),
              need(length(plot_meta()) == length(dataset$subset),"")
            )
            plot_data_raw <- data.frame(fetch_data(genes=geneset),plot_meta(),check.names = F)
            colnames(plot_data_raw)[ncol(plot_data_raw)] <- meta_select
            color_type <- plot_settings$color_cont
            fix_values <- F
            if (fnunique(plot_meta()) > 1 && scaling) {
              plot_means <- plot_data_raw %>% group_by(get(meta_select)) %>% fsummarise(across(all_of(geneset),get_avg_exp)) %>% mutate(across(all_of(geneset), ~ as.numeric(scale(log1p(.x))))) %>% mutate(across(all_of(geneset), ~ replace(.x, is.nan(.x), -3)))
              color_type <- plot_settings$color_scaled
              fix_values <- T
            } else {
              plot_means <- plot_data_raw %>% group_by(get(meta_select)) %>% fsummarise(across(all_of(geneset),get_avg_exp))
            }
            colorscale <- colors_as_list(color_type)
            plot_percents <- plot_data_raw %>% group_by(get(meta_select)) %>% fsummarise(across(all_of(geneset),get_percent_exp)) %>% pivot_longer(cols=all_of(geneset), names_to="Gene")
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
            plot_data$Color <- round(plot_data$Color,3)
            plot_data$Percent <- round(plot_data$Percent*100,1)
            plot_data$Size <- plot_data$Percent
            plot_data$Size[plot_data$Percent < 1.0] <- NA
            meta_order <- plot_data$Meta
            if (!clustering) {
              meta_order <- gtools::mixedsort(meta_order)
            }
            if (!flip) {
              plot_ly(plot_data,x=~Meta,y=~Gene,type="scatter",mode = "markers",marker = list(sizemode = "area",size=~Size,sizeref=0.18,color=~Color,colorscale=colorscale,cmid=if(fix_values) 0 else NULL,colorbar=list(len=200,lenmode="pixels",thickness=28,title=list(text="Avg Exp"),y=0.8),line=list(width=0)),hoverinfo = 'text', hovertext = paste0("x: ",plot_data$Meta,"\n","y: ",plot_data$Gene,"\n","Exp: ",round(plot_data$Color,2),"\n","Percent: ",round(plot_data$Percent,2),"%")) %>%
                plotly::layout(font=list(family=default_font),title=list(text=set_name,y=0.98,font = list(size = 24)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=30,b=25,l=100,r=45),xaxis=list(title=list(text=meta_select,standoff=8,font=list(size=15)),tickfont=list(size=14),showgrid=F,zeroline=T,autotypenumbers = 'strict',categoryorder="array",categoryarray=meta_order),yaxis=list(title=list(text="Genes",standoff=8,font=list(size=15)),tickfont=list(size=13),showgrid=F,zeroline=T,categoryorder="array",categoryarray=plot_data$Gene),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
                plotly::config(doubleClickDelay = 400,displaylogo = F,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d')))            
            } else {
              plot_ly(plot_data,x=~Gene,y=~Meta,type="scatter",mode = "markers",marker = list(sizemode = "area",size=~Size,sizeref=0.18,color=~Color,colorscale=colorscale,cmid=if(fix_values) 0 else NULL,colorbar=list(len=200,lenmode="pixels",thickness=28,title=list(text="Avg Exp"),y=0.8),line=list(width=0)),hoverinfo = 'text', hovertext = paste0("x: ",plot_data$Gene,"\n","y: ",plot_data$Meta,"\n","Exp: ",round(plot_data$Color,2),"\n","Percent: ",round(plot_data$Percent,2),"%")) %>%
                plotly::layout(font=list(family=default_font),title=list(text=set_name,y=0.98,font = list(size = 24)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=30,b=25,l=100,r=45),xaxis=list(title=list(text="Genes",standoff=8,font=list(size=15)),tickfont=list(size=13),showgrid=F,zeroline=T,categoryorder="array",categoryarray=plot_data$Gene),yaxis=list(title=list(text=meta_select,standoff=8,font=list(size=15)),tickfont=list(size=14),showgrid=F,zeroline=T,autotypenumbers = 'strict',categoryorder="array",categoryarray=meta_order),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
                plotly::config(doubleClickDelay = 400,displaylogo = F,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d')))            
            }
          }
        }
      }
      
      get_avg_exp <- function(data) {
        return(fmean(expm1(data)))
      }
      
      get_percent_exp <- function(data) {
        return(length(data[data > 0]) / length(data))
      }
      
      plot_props <- function(meta_1,meta_2,plot_meta_1,plot_meta_2,type,plot_settings) {
        validate(
          need(meta_1,""),
          need(meta_2,"")
        )
        if (meta_2 == "All Data") {
          validate(
            need(plot_meta_1(),"")
          )
          if (type == "Proportions") {
            counts <- table(plot_meta_1())
            props <- counts/length(plot_meta_1())
            props <- data.frame(props) %>% filter(Freq != 0)
            counts <- data.frame(counts) %>% filter(Freq != 0)
            order <- as.character(props$Var1) %>% gtools::mixedsort() %>% fix_order()
            props <- props[match(order,props$Var1),]
            counts <- counts[match(order,counts$Var1),]
            color_pal <- generate_colors(plot_settings$color_discrete,length(order))
            color_pal[match("Undefined",order)] <- "#D6D6D6"
            props$Freq <- props$Freq*100
            props$Count <- counts$Freq
            label_type <- if (nrow(props) > 20) "none" else "inside"
            plot_ly(props, labels = ~Var1, values = ~Freq, marker=list(colors=color_pal), type = 'pie', title=list(position="top_center"),sort=F,pull=0.0,textposition=label_type,insidetextfont=list(color="white",size=14),hoverinfo = 'text', hovertext = paste0(props$Var1,"\n",round(props$Freq,1),"%\n", props$Count," cells")) %>%           
              plotly::layout(font=list(family=default_font),title=list(text=paste0(meta_1, " Proportions"),y=0.98,font=list(size=24)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=55,b=20,l=80,r=60),legend=list(font = list(size = 16),bgcolor="rgba(0, 0, 0, 0)",traceorder="normal"),yaxis=list(title="",zeroline=F,visible=F),showlegend = T,modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)")) %>%
              plotly::config(doubleClickDelay = 400,displaylogo = F,modeBarButtons= list(list()))
          } else {
            counts <- table(plot_meta_1())
            counts <- data.frame(counts) %>% filter(Freq != 0)
            order <- as.character(counts$Var1) %>% gtools::mixedsort() %>% fix_order()
            counts$Var1 <- factor(counts$Var1,levels=order)
            color_pal <- generate_colors(plot_settings$color_discrete,length(order))
            color_pal[match("Undefined",order)] <- "#D6D6D6"
            plot_ly(counts, x= ~Var1, y= ~Freq, color= ~Var1, colors=color_pal,type= "bar", hoverinfo = 'text', hovertext = paste0(counts$Var1,"\n", counts$Freq," cells")) %>%
              plotly::layout(font=list(family=default_font),title=list(text=paste0(meta_1," Counts"),y=0.98,font=list(size=24)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",margin=list(t=20,b=10,l=100,r=50),legend=list(font = list(size = 16),entrywidth = 0,bgcolor="rgba(0, 0, 0, 0)",traceorder="normal"),yaxis=list(title=list(text="Number of cells",font=list(size=15)),tickfont=list(size=13)),xaxis=list(title=list(text=meta_1,font=list(size=15)),tickfont=list(size=14)),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)"),showlegend = T) %>%
              plotly::config(doubleClickDelay = 400,displaylogo = F,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d')))
          }
        } else {
          validate(
            need(plot_meta_1(),""),
            need(plot_meta_2(),"")
          )
          if (type == "Proportions") {
            counts <- table(plot_meta_1(),plot_meta_2())
            props <- prop.table(counts,margin=2)
            props <- data.frame(props) %>% filter(Freq != 0)
            counts <- data.frame(counts) %>% filter(Freq != 0)
            props$Freq <- props$Freq * 100
            props$Count <- counts$Freq
            order <- gtools::mixedsort(funique(as.vector(props$Var2)))
            props$Var2 <- factor(props$Var2,levels=order)
            order <- gtools::mixedsort(funique(as.vector(props$Var1))) %>% fix_order()
            props$Var1 <- factor(props$Var1,levels=order)
            color_pal <- generate_colors(plot_settings$color_discrete,length(order))
            color_pal[match("Undefined",order)] <- "#D6D6D6"
            plot_ly(props, x= ~Var2, y= ~Freq, color= ~Var1, colors=color_pal, type= "bar", hoverinfo = 'text', hovertext = paste0(props$Var1,"\n",props$Count," cells","\n",round(props$Freq,1),"%")) %>%
              plotly::layout(font=list(family=default_font),title=list(text=paste0(meta_1," Proportions"),y=0.98,font=list(size=24)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",barmode= "stack",margin=list(t=20,b=10,l=100,r=50),legend=list(font = list(size = 16),entrywidth = 0,bgcolor="rgba(0, 0, 0, 0)",traceorder="normal"),yaxis=list(title=list(text="Percentage of cells",font=list(size=15)),tickfont=list(size=13)),xaxis=list(title=list(text=meta_2,font=list(size=15)),tickfont=list(size=14)),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)"),showlegend = T) %>%
              plotly::config(doubleClickDelay = 400,displaylogo = F,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d')))
          } else {
            counts <- table(plot_meta_1(),plot_meta_2())
            props <- prop.table(counts,margin=1)
            props <- data.frame(props) %>% filter(Freq != 0)
            counts <- data.frame(counts) %>% filter(Freq != 0)
            counts$Prop <- props$Freq * 100
            order <- gtools::mixedsort(funique(as.vector(counts$Var2)))
            counts$Var2 <- factor(counts$Var2,levels=order)
            order <- gtools::mixedsort(funique(as.vector(counts$Var1))) %>% fix_order()
            counts$Var1 <- factor(counts$Var1,levels=order)
            color_pal <- generate_colors(plot_settings$color_discrete,length(order))
            color_pal[match("Undefined",order)] <- "#D6D6D6"
            plot_ly(counts, x= ~Var2, y= ~Freq, color= ~Var1, colors=color_pal, type= "bar", hoverinfo = 'text', hovertext = paste0(counts$Var1,"\n", counts$Freq," cells","\n",round(counts$Prop,1),"%")) %>%
              plotly::layout(font=list(family=default_font),title=list(text=paste0(meta_1," Counts"),y=0.98,font=list(size=24)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",barmode= "stack",margin=list(t=20,b=10,l=100,r=50),legend=list(font = list(size = 16),entrywidth = 0,bgcolor="rgba(0, 0, 0, 0)",traceorder="normal"),yaxis=list(title=list(text="Number of cells",font=list(size=15),standoff=2),tickfont=list(size=13)),xaxis=list(title=list(text=meta_2,font=list(size=15)),tickfont=list(size=14)),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)"),showlegend = T) %>%
              plotly::config(doubleClickDelay = 400,displaylogo = F,modeBarButtons= list(list('drawopenpath','eraseshape'),list('zoom2d','pan2d','resetScale2d')))          
          }
        }
      }
      
      plot_volcano <- function(data_type,plot_type,show_labels,plot_settings) {
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
        if (nrow(data_use) == 0) {
          return(NULL)
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
          plot <- plot_ly(plot_data, x = ~fc, y = ~p_val, text = ~gene, opacity = plot_settings$point_transparent,marker=list(color=color,colorscale=list(c(-1, "rgb(224, 40, 40)"),c(0, "rgb(204, 204, 204)"), c(1, "rgb(40, 58, 224)")),size=plot_settings$point_size + 4,cmin=-1,cmax=1), hovertemplate=paste('<b>%{text}</b><br>','FC: %{x:.2f}<br>','Sig: %{y:.2f}','<extra></extra>'),type = 'scattergl', mode = 'markers',source = ns('volcano_plot')) %>% 
            plotly::config(edits = list(shapePosition = TRUE),doubleClickDelay = 400,displaylogo = F,scrollZoom = F,modeBarButtons= list(list('zoom2d','pan2d','resetScale2d'))) %>%
            plotly::layout(font=list(family=default_font),title = list(text=data_type,font = list(size = 24)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",hoverdistance=10,spikedistance=0,margin=list(t=30,b=20,l=50,r=20),xaxis=list(title=list(text="log2(Fold Change)",font=list(size=15)),tickfont=list(size=13),range=c(-fc_range,fc_range)),yaxis=list(title=list(text="-10log(p-value)",font=list(size=15)),tickfont=list(size=13),range=c(-1,sig_range)),legend = list(),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)"),
              shapes = list(
              list(type = "line", x0 = -0.5, x1 = -0.5, y0 = 0, y1 = 1, yref = "paper",layer="above",opacity=0.3,line=list(dash="dash",color="blue")),
              list(type = "line", x0 = 0.5, x1 = 0.5, y0 = 0, y1 = 1, yref = "paper",layer="above",opacity=0.3,line=list(dash="dash",color="blue")),
              list(type = "line", x0 = 0, x1 = 1, xref="paper", y0 = 10, y1 = 10,layer="above",opacity=0.3,line=list(dash="dash",color="blue"))
              )
            )
          if (show_labels) {
            plot <- plot %>% add_annotations(x=anno_data$fc, y=anno_data$p_val, text=anno_data$gene, xref="x", yref="y", showarrow=F, yshift=15, opacity=0.8,font=list(size=13))
          }
          plot
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
          colorscale <- colors_as_list(plot_settings$color_cont)
          plot <- plot_ly(plot_data, x = ~a_val, y = ~m_val, customdata = ~p_val, text = ~gene,marker=list(color = color,colorscale=colorscale,opacity=opacity,size=plot_settings$point_size + 4,showscale=T,colorbar=list(len=200,lenmode="pixels",thickness=28,y=0.8,title=list(text="-log10(p-value)"))), hovertemplate=paste('<b>%{text}</b><br>','Exp: %{x:.2f}<br>','FC: %{y:.2f}<br>','Sig: %{customdata}','<extra></extra>'),type = 'scattergl', mode = 'markers', source = ns('ma_plot')) %>% 
            plotly::config(edits = list(shapePosition = TRUE),doubleClickDelay = 400,displaylogo = F,scrollZoom = F,modeBarButtons= list(list('zoom2d','pan2d','resetScale2d'))) %>%
            plotly::layout(font=list(family=default_font),title = list(text=data_type,font = list(size = 24)),plot_bgcolor = "#fcfcff",paper_bgcolor="#fcfcff",hoverdistance=10,spikedistance=0,margin=list(t=30,b=20,l=50,r=20),xaxis=list(title=list(text="log2(Mean Expression)",font=list(size=15)),tickfont=list(size=13),range=c(-a_range,a_range)),yaxis=list(title=list(text="log2(Fold Change)",font=list(size=15)),tickfont=list(size=13)),legend = list(),modebar=list(color="#c7c7c7",activecolor="#96a8fc",orientation="v",bgcolor="rgba(0, 0, 0, 0)"),
             shapes = list(
               list(type = "line", x0 = 0, x1 = 1,xref = "paper", y0 = 0.5, y1 = 0.5,layer="above",opacity=0.3,line=list(dash="dash",color="blue")),
               list(type = "line", x0 = 0, x1 = 1, xref = "paper",y0 = -0.5, y1 = -0.5,layer="above",opacity=0.3,line=list(dash="dash",color="blue")),
               list(type = "line", x0 = 0.5, x1 = 0.5, y0 = 0, y1 = 1,yref = "paper",layer="above",opacity=0.3,line=list(dash="dash",color="blue"))
             )
            )
          if (show_labels) {
            plot <- plot %>% add_annotations(x=anno_data$a_val, y=anno_data$m_val, text=anno_data$gene, xref="x", yref="y", showarrow=F, yshift=15, opacity=0.8,font=list(size=13))
          }
          plot
        }
      }
        
      
      observeEvent(plot_type(), {
        lapply(plot_types_all, function(x) shinyjs::hide(paste0(x,"_div")))
        shinyjs::show(paste0(plot_type(),"_div"))
        if (plot_type() == 'reduction') {
          output$plot <- renderPlotly({plot_reduction(input$data_type,input$layout_meta,input$layout_gene,meta_plot_reduct,input$gene_exp,x_vals_scatter,y_vals_scatter,col_vals_scatter,input$labels,plot_settings,input$density,split_1,split_2,split_3,split_order_1,split_order_2,split_order_3,set_name_reduct,set_list_reduct)})
        }
        else if (plot_type() == 'violin') {
          reset_select()
          toggle_slider(F)
          output$plot <- renderPlotly({plot_violin(input$gene_violin,input$meta_violin,meta_plot_violin,input$violin_gene_set,plot_settings,split_4,split_order_4,set_name_violin,set_list_violin)})
        }
        else if (plot_type() == 'heatmap') {
          reset_select()
          toggle_slider(F)
          output$plot <- renderPlotly({plot_heatmap(input$heatmap_type,genes_heatmap(),input$heatmap_gene_set,input$meta_heatmap,meta_plot_heatmap,input$flip_heatmap,input$scale_heatmap,input$cluster_heatmap,input$average_heatmap,plot_settings)})
        }
        else if (plot_type() == 'props') {
          reset_select()
          toggle_slider(F)
          output$plot <- renderPlotly({plot_props(input$meta_props_1,input$meta_props_2,meta_plot_props_1,meta_plot_props_2,input$props_type,plot_settings)})
        }
        else if (plot_type() == 'volcano') {
          reset_select()
          toggle_slider(F)
          output$plot <- renderPlotly({plot_volcano(input$volcano_data,input$volcano_type,input$volcano_labels,plot_settings)})
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
          textInput(ns("export_height"),label="Height", value = 800),
          actionButton(ns("download_confirm"), "Export"),
          footer = NULL
        ))
      })
      
      export_settings <- reactiveValues(format="png",name="Plot",width=1200,height=800)
      
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