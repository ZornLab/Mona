#' Launch Mona app
#' @import shiny
#' @import bs4Dash
#' @import fresh
#' @import shinycssloaders
#' @import plotly
#' @import dplyr
#' @import tidyr
#' @import htmlwidgets
#' @import DT
#' @import shinyWidgets
#' @import sortable
#' @import shinyjs
#' @import imola
#' @import repel
#' @import gfonts
#' @import spsComps
#' @import shinyFiles
#' @import fs
#' @import qs
#' @import ks
#' @import dqrng
#' @import gprofiler2
#' @import Seurat
#' @import SeuratObject
#' @import BPCells
#' @export

mona <- function() {
  
  options(shiny.maxRequestSize=8000*1024^2)
  set.seed(123)
  dqset.seed(123)
  options(Seurat.object.assay.version = 'v5')
  
  resources <- system.file("www", package = "Mona")
  addResourcePath("www", resources)
  resources <- system.file("images", package = "Mona")
  addResourcePath("images", resources)
  
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
  
  plot_render <- "
    function(el, x){
      var id = el.getAttribute('id');
      Shiny.setInputValue('plot_rendered',id,{priority: 'event'})
    }"
  
  theme <- create_theme(
    bs4dash_layout(
      sidebar_width = "10%",
      control_sidebar_width = "15%"
    ),
    bs4dash_color(
      lightblue = "#b9c5fd",
      blue = "#96a8fc",
      teal="#fcfcff"
    ),
    bs4dash_sidebar_light(
      bg = "#fcfcff"
    )
  )
  
  # Triggered whenever plots are sorted to check their order
  sortable_custom_input <- function() {
    js_text <- "function(evt) {
    if (typeof Shiny !== \"undefined\") {
      var plots = [...this.el.querySelectorAll('.plotly.html-widget')];
      var ids = Array.from(plots, node => node.id);
      Shiny.setInputValue('sort_info',ids,{priority: 'event'})
    }
  }"
    htmlwidgets::JS(js_text)
  }
  
  ui <- dashboardPage(
    
    freshTheme = theme,
    dark = NULL,
    help= NULL,
    
    title = "Mona - Cell Explorer",
    
    controlbar = bs4DashControlbar(
      id = "control_bar",
      collapsed = T,
      pinned = T,
      overlay=T, 
      skin="light",
      shiny::actionButton("close_control",label="",icon=icon("xmark"),width="32px",style="padding: 3px; background-color: #fcfcff; border-width: 0px;"),
      conditionalPanel(
        condition = "output.control_mode == 'settings'",
        br(),
        div(
          style="margin:10px;",
          sliderInput("downsample","Downsample to",min = 10, max = 100,value = 100, step = 10,post = "%",width="95%"),
          sliderTextInput("point_size","Point size",grid=T,choices=c("Small","Medium","Large"),selected="Medium",width = "95%")
        )
      ),
      conditionalPanel(
        condition = "output.control_mode == 'search'",
        textInput("searched_gene",label="",placeholder = "Enter gene symbol", width="80%"),
        uiOutput("gene_search")
      )
    ),
    
    footer = NULL,
    
    header = dashboardHeader(
      img(src = "images/cell.svg", height = 50, width = 50, style="padding-bottom: 5px;"),
      tags$h4("Mona", style="padding-top: 8px; font-family: 'Alegreya Sans SC', sans-serif; font-style: normal;"),
      title = "Menu",
      skin = "light",
      fixed=T,
      actionLink("data_avail",label = "View datasets",style="color: black; padding-left: 25px;"),
      actionLink("data_new",label = "Load new dataset",style="color: black; padding-left: 25px;", class="shinyDirectories", "data-title"="Select a Mona directory"),
      actionLink("data_save",label = "Save dataset",style="color: black; padding-left: 25px;"),
      rightUi=uiOutput("data_link")
    ),
    sidebar = dashboardSidebar(
      skin = "light",
      id = "sidebar",
      collapsed = T,
      expandOnHover = F,
      minified = F,
      sidebarMenu(
        id = "side_menu",
        menuItem(
          "Explorer",
          tabName = "Explorer",
          icon = icon("eye")
        ),
        menuItem(
          "Help",
          tabName = "Help",
          icon = icon("question")
        ),
        menuItem(
          "About",
          tabName = "About",
          icon = icon("user")
        ),
        menuItem(
          "GitHub",
          icon=icon("github"),
          href = "https://github.com/ZornLab/Mona"
        )
      )
    ),
    body = dashboardBody(
      useShinyjs(),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "www/tooltip.min.css"),
        tags$script(src="www/tooltip.min.js"),
        tags$link(rel = "stylesheet", type = "text/css", href = "www/sF/styles.css"),
        tags$script(src="www/sF/shinyFiles.js"),
        tags$link(rel = "stylesheet", type = "text/css", href = "www/css/alegreya-sans-sc.css")
      ),
      tabItems(
        tabItem(
          tabName = "Explorer",
          fluidRow(
            shiny::column(
              width=3,
              div(
                id="controls",
                align="center",
                circleButton("open_search",icon = icon("search"),size = "default",style="margin-right: 5px; margin-bottom: 10px; margin-top: -5px; background-color: #fcfcff;"),
                circleButton("new_plot",icon = icon("chart-column"),size = "default", style="margin-bottom: 10px; margin-top: -5px; background-color: #fcfcff;"),
                #circleButton("annotate_cells",icon = icon("gears"),size = "default", style="margin-bottom: 10px; margin-top: -5px; background-color: #fcfcff;"),
                circleButton("settings",icon = icon("sliders"),size = "default", style="margin-bottom: 10px; margin-top: -5px; margin-left: 5px; background-color: #fcfcff;"),
                box(
                  id="cell_box",
                  width=NULL,
                  height="36vh",
                  collapsible = T,
                  headerBorder = F,
                  background = "teal",
                  fluidRow(
                    shiny::column(
                      width=3,
                      shiny::actionButton("subset_select",icon=icon("scissors"),label="",width="32px",style="margin-right: 3px; padding: 3px; background-color: #fcfcff;")
                    ),
                    shiny::column(
                      width=6,
                      align="center",
                      tags$div(uiOutput("cell_select"),style="padding-top:3px;")
                    ),
                    shiny::column(
                      width=3,
                      shiny::actionButton("subset_undo",icon=icon("rotate-left"),label="",width="32px",style="margin-left: 3px; padding: 3px; background-color: #fcfcff;")
                    )
                  ),
                  fluidRow(
                    shiny::column(
                      width=6,
                      virtualSelectInput(
                        inputId = "anno_select",
                        label = "",
                        choices = c(),
                        search = F,
                        optionsCount = 5,
                        optionHeight = "25px",
                        keepAlwaysOpen = T,
                        placeholder = "",
                        noOptionsText = ""
                      ),
                      shiny::actionButton("new_anno",icon=icon("plus"),label="",width="32px",style="margin-right: 3px; margin-top: 10px; padding: 3px; background-color: #fcfcff;"),
                      shiny::actionButton("remove_anno",icon=icon("minus"),label="",width="32px",style="margin-right: 3px; margin-top: 10px; padding: 3px; background-color: #fcfcff;"),
                      shiny::actionButton("rename_anno",icon=icon("pen"),label="",width="32px", style="margin-top: 10px; padding: 3px; background-color: #fcfcff;")                
                    ),
                    shiny::column(
                      width=6,
                      virtualSelectInput(
                        inputId = "cluster_select",
                        label = "",
                        choices = c(),
                        search = F,
                        optionsCount = 5,
                        optionHeight = "25px",
                        keepAlwaysOpen = T,
                        placeholder = "",
                        noOptionsText = ""
                      ),
                      shiny::actionButton("new_cluster",icon=icon("plus"),label="",width="32px",style="margin-right: 3px; margin-top: 10px; padding: 3px; background-color: #fcfcff;"),
                      shiny::actionButton("remove_cluster",icon=icon("minus"),label="",width="32px",style="margin-right: 3px; margin-top: 10px; padding: 3px; background-color: #fcfcff;"),
                      shiny::actionButton("rename_cluster",icon=icon("pen"),label="",width="32px", style="margin-top: 10px; padding: 3px; background-color: #fcfcff;")
                    )
                  )
                ),
                box(
                  title = "",
                  width=NULL,
                  height="41vh",
                  headerBorder = F,
                  collapsible = F,
                  background = "teal",
                  id="gene_box",
                  shiny::tabsetPanel(
                    id="gene_tabs",
                    type="pills",
                    tabPanel(
                      title="Markers",
                      conditionalPanel(
                        condition = ("output.marker_mode == 'none'"),
                        div(
                          id="markers_none",
                          p("No markers found")
                        )
                      ),
                      conditionalPanel(
                        condition = ("output.marker_mode == 'show'"),
                        withSpinner(DTOutput("marker_table"),type=5),
                        fluidRow(
                          shiny::column(
                            width=2,
                            downloadButton("save_markers",icon=icon("download"),label="",style="width: 32px; margin-top: 8px; padding: 3px; margin-left: 15px; background-color: #fcfcff;"),
                          ),
                          shiny::column(
                            width=8,
                            radioGroupButtons(
                              inputId = "fc_filter",
                              label = "",
                              choices = c("Neg","All","Pos"),
                              selected = "All"
                            )
                          ),
                          shiny::column(
                            width=2,
                            shiny::actionButton("copy_markers",icon=icon("copy"),label="",width="32px",style="margin-top: 8px; padding: 3px; margin-right: 15px; background-color: #fcfcff;")
                          )
                        )
                      )
                    ),
                    tabPanel(
                      title="Function",
                      conditionalPanel(
                        condition = ("output.marker_mode == 'show'"),
                        withSpinner(DTOutput("go_table"),type=5)
                      )
                    ),
                    tabPanel(
                      title="Sets",
                      div(
                        id="sets"
                      ),
                      shiny::actionButton("new_gene_set",icon=icon("plus"),label="",width="50%",style="padding: 1px; position: absolute; top: 88% !important; left:25% !important; background-color: #fcfcff;")
                    ))
                ))
            ),
            shiny::column(
              width=9,
              div(
                id = "flex_div",
                flexPanel(
                  id = "plot_flex",
                  direction = "row",
                  wrap = "wrap",
                  flex = NULL,
                  grow = 1,
                  shrink = 0,
                  basis = "100%",
                  gap = "0% 0.5%",
                  justify_content = "flex-start",
                  align_items = "flex-start",
                  align_content = "flex-start"
                )
              ),
              sortable_js(
                "plot_flex",
                elementId = "sortable",
                options=sortable_options(swap=T,handle=".card-header",onEnd = sortable_custom_input())
              )
            )
          )
        ),
        tabItem(
          tabName = "Help",
          box(
            title="Navigation",
            collapsible = F,
            width = 12,
            div(img(src = "images/layout.png", height = 250,width = 375),style="text-align: center"),
            br(),
            tags$ul(
              tags$li("The navigation bar at the top is where datasets are loaded and saved. You can also access more information about the current dataset."),
              tags$li("The plot section holds any plots you create. It is dynamic and can contain up to 8 at once. Plots can also be rearranged or expanded to take up the full screen."),
              tags$li("The controls area has multiple features including searching for genes, adjusting settings, and most importantly, creating new plots."),
              tags$li("The cell section is used for making selections within embeddings and annotating them. You can also view the cell metadata to either edit them or find markers."),
              tags$li("Finally, the gene selection is where you can view markers, or create custom gene sets to use when generating plots.")
            )
          ),
          box(
            title="Functions",
            collapsible = F,
            width = 12,
            h5("Selection"),
            p("To focus on a smaller subset of cells, select them using the box/lasso tool when vieweing a 2D embedding.  Please note that this is the only plot type where selection is supported. Once selected, the data can be subset to carry that selection through to all plots, calculate markers for that selection, or simply give it a name within the metadata.")
          ),
          box(
            title="Plot Types",
            collapsible = F,
            width = 12,
            h5("Embedding"),
            p("Use to view cell metadata and gene expression at the per-cell level. Typically used with UMAPs but any other reduction added to the dataset can also be plotted."),
            h5("Heatmap"),
            p("Use for understanding broad patterns in gene expression. Genes can be viewed either per-cell or the average per-group. Clustering is automatically performed on both axes."),
            h5("Dot"),
            p("Similar to heatmaps, but allows you to focus on a smaller set of genes. Shows the average gene expression AND the percent of cells expressing the gene in each group."),
            h5("Violin"),
            p("Use to view the distribution of a gene or quality measure, either across the entire dataset or per-group."),
            h5("Bar"),
            p("Use to view the proportion of cells across groups and how different metadata relate to one another.")
          ),
          box(
            title="Data Preparation",
            collapsible = F,
            width = 12,
            p("To get started, we assume you have some familiarity with R and Seurat. If not, visit the GitHub for more information and use the provided R scripts."),
            p("For a fast, uniform experience for all users, Mona has an expected format for datasets:"),
            tags$ul(
              tags$li("Dataset is a 'Mona directory' containing a Seurat v5 object and one or more BPCells matrices (which are sub-directories)."),
              tags$li("The Seurat object is saved using the 'qs' library for better read/save times, meaning it must have a .qs file extension."),
              tags$li("Data is processed with SCT v2 rather than the 'LogNormalize' approach (Seurat team has shown superior performance in finding variable features and DEGs)."),
              tags$li("BPCells matrices keep the expression data on-disk, greatly reducing the amount of memory needed. This is typically done for just raw counts, but for Mona the processed data must be formatted this way."),
              tags$li("The 'misc' slot contains information about the dataset name, species, and any pre-calculated markers.")
            ),
            p("Here are some optional things to consider:"),
            tags$ul(
              tags$li("Users can include additional reductions besides 2D UMAPs, such as 3D UMAPs, TSNEs, or force-directed layouts."),
              tags$li("To save space, users can remove unneeded assays/layers after processing. Only the data layer of the SCT assay is required, but this could impact future processing."),
            ),
            p("Note that the 'Mona directory' is still essentially a Seurat object, and matrices can be converted back to the standard in-memory format. This allows users to easily continue their analysis in Seurat or elsewhere.")
          ),
          box(
            title="Performance",
            collapsible = F,
            width = 12,
            p("Here are some general recommendations for having a smooth experience:"),
            tags$ul(
              tags$li("Mona has been tested on 200000+ cells without issue, but this is machine-dependent. You can expect things to run slower the larger your dataset is."),
              tags$li("While the app is executing something, like rendering a plot or calculating markers, you should allow it to finish before performing another action."),
              tags$li("Consider downsampling your cells if you are encountering slowness. Open the settings to try this feature."),
              tags$li("Embeddings and heatmaps will generally be the most demanding plot types to generate, when dealing with a large number of genes or metadata"),
              tags$li("Viewing 1-2 plots is the easiest way to explore your data. Although up to 8 are possible, more plots will require more resources.")
            )
          )
        ),
        tabItem(
          tabName = "About",
          box(
            title="About",
            collapsible = F,
            width = 12,
            h4("Developed by Konrad Thorner"),
            h5("Konrad.Thorner@cchmc.org"),
            br(),
            h5("Thanks to the following people for feedback during development:"),
            tags$ul(
              tags$li("Aaron Zorn")
            ),
            br(),
            img(src = "images/cchmc.jpg", height = 100, width = 300),
            br(),br(),
            h5("Mona would not be possible without the following excellent R packages:"),
            tags$ul(
              tags$li(tags$a(href="https://satijalab.org/seurat/","Seurat")),
              tags$li(tags$a(href="https://github.com/RGLab/MAST/","MAST")),
              tags$li(tags$a(href="https://plotly.com/r/","plotly")),
              tags$li(tags$a(href="https://rinterface.github.io/bs4Dash/","bs4Dash")),
              tags$li(tags$a(href="https://talgalili.github.io/heatmaply/index.html","heatmaply")),
              tags$li(tags$a(href="https://github.com/dreamRs/shinyWidgets","shinywidgets")),
              tags$li(tags$a(href="https://github.com/thomasp85/shinyFiles","shinyFiles")),
              tags$li(tags$a(href="https://deanattali.com/shinyjs/","shinyjs")),
              tags$li(tags$a(href="https://rstudio.github.io/sortable/","sortable")),
              tags$li(tags$a(href="https://www.anatomyofcode.com/imola/","imola"))
            ),
            br(),
            h5("Plot icons were provided by:"),
            tags$a(href='https://www.flaticon.com/free-icons/business-and-finance',"Icons created by ranksol graphics - Flaticon")
          )
        )
      )
    )
  )
  
  server <- function(input, output, session) { 

    updateTabItems(session,"nav_menu","null")
    updateTabItems(session,"side_menu","Explorer")
    
    addPopover(id="subset_select",options=list(content="Subset data",placement="top",delay=500,trigger="hover"))
    addPopover(id="subset_undo",options=list(content="Undo subset",placement="top",delay=500,trigger="hover"))
    addPopover(id="open_search",options=list(content="Search genes",placement="top",delay=500,trigger="hover"))
    addPopover(id="new_plot",options=list(content="Add new plot",placement="top",delay=500,trigger="hover"))
    addPopover(id="settings",options=list(content="View settings",placement="top",delay=500,trigger="hover"))
    
    addPopover(id="new_anno",options=list(content="Create new metadata",placement="top",delay=500,trigger="hover"))
    addPopover(id="remove_anno",options=list(content="Remove metadata",placement="top",delay=500,trigger="hover"))
    addPopover(id="rename_anno",options=list(content="Rename metadata",placement="top",delay=500,trigger="hover"))
    addPopover(id="new_cluster",options=list(content="Create group from selection",placement="top",delay=500,trigger="hover"))
    addPopover(id="remove_cluster",options=list(content="Remove group",placement="top",delay=500,trigger="hover"))
    addPopover(id="rename_cluster",options=list(content="Rename group",placement="top",delay=500,trigger="hover"))
    
    addPopover(id="save_markers",options=list(content="Export markers",placement="top",delay=500,trigger="hover"))
    addPopover(id="copy_markers",options=list(content="Save to set",placement="top",delay=500,trigger="hover"))
    
    addPopover(id="new_gene_set",options=list(content="Create new gene set",placement="top",delay=500,trigger="hover"))
    
    
    cur_data <- reactiveValues(seurat=NULL,name="",species="",description="",meta=NULL,genes=NULL,var_genes=NULL,quality=NULL,reducs=NULL,subset=NULL)
    output$data_link <- renderUI({
      if (!is.null(cur_data$seurat)){
        tagList(tags$li(class='dropdown', actionLink("data_info",label=cur_data$name,icon=tags$i(class = "fas fa-info-circle", style="font-size: 18px; padding-right: 5px; color: #b9c5fd;"),style="color: black; font-size: 120%;")))
      }
    })
    
    # Called when a dataset is loaded when another data is already loaded
    # Essentially wipes everything: plots, gene sets, selection, metadata, etc.
    reset_data <- function() {
      if (!is.null(cur_data$seurat)) {
        lapply(names(geneset_list$sets), function(x) {
          removeUI(paste0("#",x))
          geneset_list$sets[[x]] <- NULL
        })
        lapply(names(selection_list$selects), function(x) {
          selection_list$selects[[x]] <- NULL
        })
        lapply(names(plots_list$plots), function(x) {
          removeUI(paste0("#",x,"-render_plot"))
          num_plots(num_plots() - 1)
          plot_remove(x)
        })
        cur_markers(NULL)
        marker_mode("off")
        cur_selection$plot <- "plot0-plot"
        cur_selection$cells <- NULL
        updateSliderInput(session,"downsample",value=100)
        shinyjs::click("new_plot")
      }
    }
    
    observeEvent(input$data_info, {
      showModal(modalDialog(
        title = "Data Information",
        easyClose = T,
        size="m",
        h5(tags$b("Name")),
        p(paste0(cur_data$name)),
        h5(tags$b("Size")),
        p(paste0(ncol(cur_data$seurat), " cells, ", nrow(cur_data$seurat), " genes")),
        h5(tags$b("Species")),
        p(cur_data$species),
        h5(tags$b("Description")),
        p(cur_data$description),
        footer = NULL
      ))
    })
    shinyjs::runjs("$('#control_bar').css({'top':'45px','display':'block','height':'auto'});")
    control_mode <- reactiveVal("closed")
    output$control_mode <- renderText({
      control_mode()
    })
    outputOptions(output, "control_mode", suspendWhenHidden=FALSE)
    
    observeEvent(input$open_search, {
      if (control_mode() == "closed") {
        control_mode("search")
        shinyjs::addClass(selector = "body",class="control-sidebar-slide-open")
      } else if (control_mode() == "settings") {
        control_mode("search")
      } else {
        control_mode("closed")
        shinyjs::removeClass(selector = "body",class="control-sidebar-slide-open")
      }
    })
    
    observeEvent(input$close_control, {
      control_mode("closed")
      shinyjs::removeClass(selector = "body",class="control-sidebar-slide-open")
    })
    
    observeEvent(input$sidebar, {
      if(input$sidebar) {
        shinyjs::runjs("$('#controls').css({'width':'21%',transition:'0.2s linear'});")
      } else {
        shinyjs::runjs("$('#controls').css({'width':'23.5%',transition:'0.2s linear'});")
      }
    })
    
    observeEvent(input$settings, {
      if (control_mode() == "closed") {
        control_mode("settings")
        shinyjs::addClass(selector = "body",class="control-sidebar-slide-open")
      } else if (control_mode() == "search") {
        control_mode("settings")
      } else {
        control_mode("closed")
        shinyjs::removeClass(selector = "body",class="control-sidebar-slide-open")
      }
    })
    
    
    # Settings
    #--------------------------------------------
    columns <- reactiveVal(F)
    
    observeEvent(input$settings, {
      
    })
    
    # Data input
    #--------------------------------------------
    
    # Used to sort metadata into either continuous or categorical
    filter_meta <- function(meta) {
      filter_1 <- sapply(meta, function(x) class(cur_data$seurat[[x]][,1]) %in% c("integer","numeric"))
      filter_2 <- sapply(meta, function(x) length(unique(cur_data$seurat[[x]][,1])) > 100)
      return(meta[filter_1 & filter_2])
    }
    
    get_var_genes <- function(seurat) {
      return(list(seurat@misc$var_100,seurat@misc$var_500,seurat@misc$var_1000))
      #return(seurat[["SCT"]]@meta.data$var.features %>% na.exclude())
    }
    
    downsample_data <- function() {
      if (input$downsample == 100) {
        cur_data$use <- cur_data$seurat
      } else {
        cell_count <- ncol(cur_data$seurat)
        subset <- dqsample.int(cell_count, round(cell_count*(input$downsample/100)))
        cur_data$use <- cur_data$seurat[,subset]
      }
    }
    
    refresh_data_use <- function() {
      cells <- colnames(cur_data$use)
      cur_data$use <- cur_data$seurat[,cells]
    }
    
    root <- c(home=fs::path_home())
    save_dir <- reactiveVal("datasets/")
    shinyDirChoose(input, id='data_new', roots=root, session = session,allowDirCreate = F)
    
    # Sets up "cur_data" when a new dataset is loaded
    # Note that depending on where data was processed, path to matrix may need to be updated
    data_setup <- function(mona_dir) {
      showNotification("Loading dataset...", type = "message")
      cur_data$seurat <- qread(paste0(mona_dir,"seurat.qs"))
      mat_dir <- cur_data$seurat[["SCT"]]$data@matrix@matrix@dir
      if (mat_dir != mona_dir) {
        cur_data$seurat[["SCT"]]$data@matrix@matrix@dir <- mona_dir
      }
      save_dir(mona_dir)
      cur_data$name <- cur_data$seurat@misc$name
      cur_data$species <- cur_data$seurat@misc$species
      cur_data$description <- cur_data$seurat@misc$description
      meta <- colnames(cur_data$seurat@meta.data)
      filter <- filter_meta(meta)
      cur_data$quality <- filter
      cur_data$meta <- meta[!(meta %in% filter)]
      updateVirtualSelect(
        inputId = "anno_select",
        choices = c(cur_data$meta),
        selected = NULL
      )
      updateVirtualSelect(
        inputId = "cluster_select",
        choices = c(""),
        selected = NULL
      )
      genes <- rownames(cur_data$seurat)
      cur_data$genes <- sort(genes)
      cur_data$var_genes <- get_var_genes(cur_data$seurat)
      cur_data$reducs <- names(cur_data$seurat@reductions)
      downsample_data()
      shinyjs::show("new_gene_set")
    }
    
    observeEvent(input$load1, {
      reset_data()
      removeModal(session)
      mona_dir <- "datasets/pbmc3k/"
      data_setup(mona_dir)
    })
    
    observeEvent(input$data_avail, {
      showModal(modalDialog(
        title = "Select dataset",
        easyClose = T,
        size="m",
        bs4Accordion(
          id = "data_select",
          accordionItem(
            title = "PBMC 3K",
            status = "lightblue",
            collapsed = T,
            p("The classic PBMC dataset from 10X Genomics",br(),"2700 cells, 12572 genes"),
            shiny::actionButton("load1", "Load data")
          )
        ),
        footer = NULL
      ))
    })
    
    observeEvent(input$data_new, {
      file_info <- input$data_new
      if(is.list(file_info[[1]])) {
        mona_dir <- paste(file_info$path,collapse = "/")
        mona_dir <- paste0(root,mona_dir,"/")
        mona_files <- list.files(mona_dir)
        if ("seurat.qs" %in% mona_files & "index_data" %in% mona_files) {
          data_setup(mona_dir)
        } else {
          showNotification("Not a valid Mona directory...", type = "message")
        }
      }
    })
    
    observeEvent(input$data_save, {
      if (!is.null(cur_data$seurat)) {
        showNotification("Saving dataset!", type = "message")
        qsave(cur_data$seurat, paste0(save_dir(),"seurat_test.qs"))
      }
    })
    
    downsample_amount <- debounce(reactive(input$downsample),300)
    
    observeEvent(downsample_amount(), {
      if (!is.null(cur_data$seurat)) {
        downsample_data()
      }},ignoreInit = T)
    
    
    # gprofiler2 API
    #------------------------------
    
    output$gene_search <- renderUI({
      symbol <- input$searched_gene
      if (nchar(symbol) >= 3) {
        url <- paste0("https://mygene.info/v3/query?q=symbol%3A",symbol,"&fields=symbol%2Cname%2Calias%2Csummary&species=",cur_data$species,"&size=1&from=0&fetch_all=false&facet_size=10&entrezonly=false&ensemblonly=false&dotfield=false")
        results <- jsonlite::fromJSON(url,flatten=T)
        results <- results$hits
        if (length(results) > 0) {
          div(
            h5(results$symbol, style="padding: 3px;"),
            h6(paste(results$alias[[1]],collapse = ", "), style="padding: 3px;"),
            h6(results$name, style="padding: 3px;"),
            p(results$summary,style = "font-size: 12px; padding: 3px;")
          )
        } else {
          h5("Gene not found", style="padding: 3px;")
        }
      } else {
        h5("")
      }
    })
    
    # Plotting 
    #-----------------------------------
    
    num_plots <- reactiveVal(0)
    plots_list <- reactiveValues(plots=list())
    plot_remove <- reactiveVal(NULL)
    plot_id <- reactiveVal(0)
    plot_order <- reactiveVal(NULL)
    point_size <- reactiveVal(6)
    
    # Called when a plot is removed, frees up memory
    remove_shiny_inputs <- function(id, .input) {
      invisible(
        lapply(grep(id, names(.input), value = TRUE), function(i) {
          .subset2(.input, "impl")$.values$remove(i)
        })
      )
    }
    
    # Similar to remove_shiny_inputs
    remove_observers <- function(id, .session) {
      invisible(
        lapply(grep(paste0(id, "_obs"), names(.session$userData), value = TRUE),
         function(i) {
           .subset2(.session$userData, i)$destroy()
         })
      )
    }
    
    shinyjs::click("new_plot")
    
    observeEvent(input$new_plot, {
      if(num_plots() < 8) {
        plot_id(plot_id() + 1)
        id <- paste0("plot",plot_id())
        insertUI(
          selector = '#plot_flex',
          where = "beforeEnd",
          ui = plotUI(id)
        )
        plots_list$plots[[id]] <- plotServer(id,num_plots,plot_remove,cur_selection,selection_list,geneset_list,point_size,cur_data)
      } else {
        showNotification("Max of 8 plots allowed!", type = "message")
      }
    })
    
    observeEvent(num_plots(), {
      if (num_plots() <= 1) {
        shinyjs::runjs("$('.plot_flex > *').css('flex-basis','100%');")
      } else if (num_plots() >= 2) {
        shinyjs::runjs("$('.plot_flex > *').css({'flex-basis':'49.5%','flex-grow':'1'});")
      }
    })
    
    observeEvent(plot_remove(), {
      id <- plot_remove()
      remove_shiny_inputs(id,input)
      remove_observers(id,session)
      plots_list$plots[[id]] <- NULL
    })
    
    observeEvent(input$sort_info, {
      lapply(input$sort_info,function(x) shinyjs::runjs(paste0("$('#",x,"').trigger('resize');")))
    })
    
    observeEvent(input$point_size, {
      point_size(switch(input$point_size, "Small"=4, "Medium"=6, "Large"=8))
    })
    
    #---------------------
    # Annotation
    
    observeEvent(input$anno_select, {
      if (input$anno_select != "") {
        meta <- unique(cur_data$seurat[[input$anno_select]][,1])
        groups <- gtools::mixedsort(meta)
        updateVirtualSelect(
          inputId = "cluster_select",
          choices = groups,
          selected = NULL
        )
      }
    })
    
    # Whenever a cluster is selected, either pulls up pre-calculated markers or calculates new markers
    observeEvent(input$cluster_select, {
      if (input$cluster_select != "") {
        markers <- cur_data$seurat@misc$markers
        markers <- subset(markers,metadata==input$anno_select & cluster==input$cluster_select)
        print(markers)
        if (nrow(markers) > 0) {
          if (nrow(markers) == 1 && markers$gene == "none") {
            marker_mode("none")
          } else {
            marker_mode("show")
            markers <- markers[,c("gene","avg_log2FC","p_val_adj")]
            colnames(markers) <- c("gene","log2FC","p-value")
            cur_markers(markers)
          }
        } else if (nrow(markers) == 0 && length(unique(cur_data$seurat[[input$anno_select]][,1])) > 1){
          cur_markers(NULL)
          cur_markers(get_new_markers(metadata=input$anno_select,cluster=input$cluster_select))
        }
      }
    },ignoreInit = T)
    
    observeEvent(input$new_anno, {
      if (is.null(cur_data$seurat)) {
        return()
      }
      showModal(modalDialog(
        title = "New annotation",
        easyClose = T,
        size="s",
        textInput("new_anno_name",label="Name",value=""),
        selectizeInput("copy_anno",label="Use as template",choices=c("None",cur_data$meta)),
        shiny::actionButton("new_anno_confirm", "Create"),
        footer = NULL
      ))
    })
    
    observeEvent(input$new_anno_confirm, {
      removeModal(session)
      if (!(input$new_anno_name %in% colnames(cur_data$seurat@meta.data))) {
        if (input$copy_anno == "None") {
          cur_data$seurat[[input$new_anno_name]] <- rep("Undefined",ncol(cur_data$seurat))
        } else {
          cur_data$seurat[[input$new_anno_name]] <- cur_data$seurat[[input$copy_anno]][,1]
        }
        refresh_data_use()
        cur_anno <- input$anno_select
        meta <- colnames(cur_data$seurat@meta.data)
        filter <- filter_meta(meta)
        cur_data$quality <- filter
        cur_data$meta <- meta[!(meta %in% filter)]
        updateVirtualSelect(
          inputId = "anno_select",
          choices = c(cur_data$meta),
          selected = cur_anno
        )
      }
    })
    
    observeEvent(input$remove_anno, {
      if (is.null(cur_data$seurat) | input$anno_select == "") {
        return()
      }
      showModal(modalDialog(
        title = "Remove annotation?",
        easyClose = T,
        size="s",
        paste0("You are about to remove '", input$anno_select,"'"),
        br(),br(),
        shiny::actionButton("remove_anno_confirm", "Confirm"),
        footer = NULL
      ))
    })
    
    observeEvent(input$remove_anno_confirm, {
      removeModal(session)
      cur_data$seurat[[input$anno_select]] <- NULL
      markers <- cur_data$seurat@misc$markers
      cur_data$seurat@misc$markers <- markers[markers$metadata != input$anno_select,]
      refresh_data_use()
      meta <- colnames(cur_data$seurat@meta.data)
      filter <- filter_meta(meta)
      cur_data$quality <- filter
      cur_data$meta <- meta[!(meta %in% filter)]
      updateVirtualSelect(
        inputId = "anno_select",
        choices = c(cur_data$meta),
        selected = NULL
      )
      updateVirtualSelect(
        inputId = "cluster_select",
        choices = c(""),
        selected = NULL
      )
    })
    
    observeEvent(input$rename_anno, {
      if (is.null(cur_data$seurat) | input$anno_select == "") {
        return()
      }
      showModal(modalDialog(
        title = "New annotation name",
        easyClose = T,
        size="s",
        HTML(paste0("<b>Old:</b> <br>",input$anno_select)),
        br(),br(),
        textInput("rename_anno_name",label="New:",value=""),
        shiny::actionButton("rename_anno_confirm", "Rename"),
        footer = NULL
      ))
    })
    
    observeEvent(input$rename_anno_confirm, {
      removeModal(session)
      if (!(input$rename_anno_name %in% colnames(cur_data$seurat@meta.data))) {
        cur_data$seurat[[input$rename_anno_name]] <- cur_data$seurat[[input$anno_select]]
        cur_data$seurat[[input$anno_select]] <- NULL
        markers_meta <- cur_data$seurat@misc$markers$metadata
        markers_meta[markers_meta == input$anno_select] <- input$rename_anno_name
        cur_data$seurat@misc$markers$metadata <- markers_meta
        refresh_data_use()
        meta <- colnames(cur_data$seurat@meta.data)
        filter <- filter_meta(meta)
        cur_data$quality <- filter
        cur_data$meta <- meta[!(meta %in% filter)]
        updateVirtualSelect(
          inputId = "anno_select",
          choices = c(cur_data$meta),
          selected = input$rename_anno_name
        )
      }
    })
    
    observeEvent(input$new_cluster, {
      if (is.null(cur_data$seurat) | is.null(cur_selection$cells)) {
        return()
      }
      showModal(modalDialog(
        title = "New group",
        easyClose = T,
        size="s",
        textInput("new_cluster_name",label="Name",value=""),
        shiny::actionButton("new_cluster_confirm", "Create"),
        footer = NULL
      ))
    })
    
    observeEvent(input$new_cluster_confirm, {
      removeModal(session)
      clusters <- cur_data$seurat[[input$anno_select]]
      filter <- rownames(clusters) %in% cur_selection$cells
      selected_clusters <- as.vector(unique(clusters[filter,1]))
      clusters <- as.vector(clusters[,1])
      clusters[filter] <- input$new_cluster_name
      cur_data$seurat[[input$anno_select]] <- clusters
      if (input$new_cluster_name %in% unique(clusters)){
        markers <- cur_data$seurat@misc$markers
        filter <- (markers$metadata == input$anno_select & markers$cluster == input$new_cluster_name)
        cur_data$seurat@misc$markers <- markers[!filter,]
      }
      markers <- cur_data$seurat@misc$markers
      filter <- (markers$metadata == input$anno_select & markers$cluster %in% selected_clusters)
      cur_data$seurat@misc$markers <- markers[!filter,]
      refresh_data_use()
      groups <- gtools::mixedsort(unique(clusters))
      updateVirtualSelect(
        inputId = "cluster_select",
        choices = groups,
        selected = NULL
      )
    })
    
    observeEvent(input$remove_cluster, {
      if (is.null(cur_data$seurat) | input$cluster_select == "") {
        return()
      }
      showModal(modalDialog(
        title = "Remove group?",
        easyClose = T,
        size="s",
        paste0("You are about to remove '", input$cluster_select,"'"),
        br(),br(),
        shiny::actionButton("remove_cluster_confirm", "Confirm"),
        footer = NULL
      ))
    })
    
    observeEvent(input$remove_cluster_confirm, {
      removeModal(session)
      clusters <- cur_data$seurat[[input$anno_select]][,1]
      clusters <- as.vector(clusters)
      filter <- clusters == input$cluster_select
      clusters[filter] <- "Undefined"
      cur_data$seurat[[input$anno_select]] <- clusters
      markers <- cur_data$seurat@misc$markers
      filter <- (markers$metadata == input$anno_select & markers$cluster == input$cluster_select)
      cur_data$seurat@misc$markers <- markers[!filter,]
      refresh_data_use()
      groups <- gtools::mixedsort(unique(clusters))
      updateVirtualSelect(
        inputId = "cluster_select",
        choices = groups,
        selected = NULL
      )
    })
    
    observeEvent(input$rename_cluster, {
      if (is.null(cur_data$seurat) | input$cluster_select == "") {
        return()
      }
      showModal(modalDialog(
        title = "New group name",
        easyClose = T,
        size="s",
        HTML(paste0("<b>Old:</b> <br>",input$cluster_select)),
        br(),br(),
        textInput("rename_cluster_name",label="New:",value=""),
        shiny::actionButton("rename_cluster_confirm", "Rename"),
        footer = NULL
      ))
    })
    
    observeEvent(input$rename_cluster_confirm, {
      removeModal(session)
      clusters <- cur_data$seurat[[input$anno_select]][,1]
      clusters <- as.vector(clusters)
      filter <- clusters == input$cluster_select
      clusters[filter] <- input$rename_cluster_name
      cur_data$seurat[[input$anno_select]] <- clusters
      if (input$rename_cluster_name %in% unique(clusters)){
        markers <- cur_data$seurat@misc$markers
        filter <- (markers$metadata == input$anno_select & markers$cluster == input$rename_cluster_name)
        cur_data$seurat@misc$markers <- markers[!filter,]
      } else {
        markers_cluster <- cur_data$seurat@misc$markers
        filter <- (markers$metadata == input$anno_select & markers$cluster == input$cluster_select)
        markers_cluster[filter] <- input$rename_cluter_name
        cur_data$seurat@misc$markers$cluster <- markers_cluster
      }
      refresh_data_use()
      groups <- gtools::mixedsort(unique(clusters))
      updateVirtualSelect(
        inputId = "cluster_select",
        choices = groups,
        selected = NULL
      )
    })
    
    #-----------------------
    # Genes
    
    geneset_list = reactiveValues(sets=list())
    
    set_id <- reactiveVal(0)
    
    shinyjs::hide("new_gene_set")
    
    observeEvent(input$new_gene_set, {
      set_id(set_id() + 1)
      id <- paste0("geneset",set_id())
      insertUI(
        selector = '#sets',
        where = "beforeEnd",
        ui = genesUI(id)
      )
      geneset_list$sets[[id]] <- genesServer(id,geneset_list,cur_data)
    })
    
    cur_markers <- reactiveVal(NULL)
    marker_subset <- reactiveVal(NULL)
    marker_mode <- reactiveVal("off")
    output$marker_mode <- renderText({
      marker_mode()
    })
    outputOptions(output, "marker_mode", suspendWhenHidden=FALSE)
    
    update_markers <- function() {
      
    }
    
    get_new_markers <- function(metadata=NULL,cluster=NULL,cells=NULL) {
      showNotification("Finding markers...", type = "message")
      if (is.null(cells)) {
        markers <- markers_mona(cur_data$use,metadata=metadata,cluster=cluster,recorrect_umi=F)
      } else {
        markers <- markers_mona(cur_data$use,cells=cells,recorrect_umi=F)
      }
      markers <- markers %>% arrange(p_val_adj) %>% slice(1:100)
      markers$gene <- rownames(markers)
      markers$avg_log2FC <- signif(markers$avg_log2FC,3)
      markers$p_val_adj <- formatC(markers$p_val_adj, format = "e", digits = 2)
      if (is.null(cells) && nrow(markers) > 0) {
        markers$cluster <- cluster
        markers$metadata <- metadata
        markers <- markers[,c("gene","cluster","metadata","avg_log2FC","p_val_adj")]
        markers_all <- cur_data$seurat@misc$markers
        cur_data$seurat@misc$markers <- rbind(markers_all,markers)
      }
      markers <- markers[,c("gene","avg_log2FC","p_val_adj")]
      colnames(markers) <- c("gene","log2FC","p-value")
      if (nrow(markers) > 0) {
        marker_mode("show")
      } else {
        marker_mode("none")
      }
      return(markers)
    }
    #,columnDefs = list(list(className = 'dt-left', targets = "_all"))
    generate_marker_table <- function(markers) {
      validate(
        need(markers,"")
      )
      DT::datatable(
        markers,
        extensions = c("Buttons"),
        options = list(dom="t", pageLength=10,scrollY="175px",scrollCollapse=T,paging=F),
        rownames= FALSE,
        class = "compact"
      ) %>% DT::formatStyle(columns = c("gene","log2FC","p-value"), fontSize = '13px', lineHeight="70%")
      
    }
    
    output$marker_table <- DT::renderDT(generate_marker_table(marker_subset()), server = FALSE)
    
    # Based on the gprofiler2 package, requires internet connection as this is not pre-calculated
    get_go_terms <- function(markers) {
      species <- switch(cur_data$species,"human"="hsapiens","mouse"="mmusculus")
      genes <- markers$gene
      gostres <- gost(query = genes, 
                      organism = species, ordered_query = FALSE, 
                      multi_query = FALSE, significant = TRUE, exclude_iea = FALSE, 
                      measure_underrepresentation = FALSE, evcodes = FALSE, 
                      user_threshold = 0.05, correction_method = "g_SCS", 
                      domain_scope = "annotated", custom_bg = NULL, 
                      numeric_ns = "", sources = NULL, as_short_link = FALSE, highlight = TRUE)
      results <- gostres$result
      results <- results[,c("term_id","term_name","p_value")]
      results$p_value <- formatC(results$p_value, format = "e", digits = 2)
      return(results)
    }
    
    output$go_table <- DT::renderDT({DT::datatable(
      get_go_terms(marker_subset()),
      extensions = c("Buttons"),
      options = list(dom="t", pageLength=10,scrollY="175px",scrollCollapse=T,paging=F),
      rownames= FALSE,
      class = "compact"
    ) %>%
        DT::formatStyle(columns = c("term_id","term_name","p_value"), fontSize = '12px', lineHeight="85%")
    }, server = FALSE)
    
    output$save_markers <-
      downloadHandler(
        filename = function() {
          paste0("markers_",input$anno_select,"_",input$cluster_select,".txt")
        },
        content = function(file) {
          write.table(cur_markers(),file,sep="\t",col.names = T,row.names = T,quote = F)
        }
      )
    
    observe({
      if(!is.null(cur_markers())) {
        if (input$fc_filter == "All") {
          marker_subset(cur_markers())
        } else if (input$fc_filter == "Neg") {
          marker_subset(cur_markers() %>% filter(log2FC < 0))
        } else if (input$fc_filter == "Pos") {
          marker_subset(cur_markers() %>% filter(log2FC > 0))
        }
      }
    })
    
    observeEvent(input$copy_markers, {
      if (!is.null(cur_markers())) {
        set_id(set_id() + 1)
        id <- paste0("geneset",set_id())
        insertUI(
          selector = '#sets',
          where = "beforeEnd",
          ui = genesUI(id)
        )
        geneset_list$sets[[id]] <- genesServer(id,geneset_list,cur_data,markers=cur_markers,markers_name=input$cluster_select)
        showNotification("Markers copied!", type = "message")
      }
    })
    
    # Selection 
    #------------------------------
    
    selection_list = reactiveValues(selects=list())
    cur_selection <- reactiveValues(plot="plot0-plot",cells=NULL)
    
    output$cell_select <- renderUI({
      if(!is.null(cur_selection$cells)) {
        select_text <- paste0(length(cur_selection$cells)," cells", " selected")
        tags$a(href = "#", onclick = "Shiny.setInputValue('selection_clicked','click',{priority:'event'});", select_text)
      } else if (!is.null(cur_data$seurat)){ 
        paste0(ncol(cur_data$use), " cells total")
      } else {
        ""
      }
    })
    
    observeEvent(input$plot_clicked, {
      id <- input$plot_clicked
      cells <- selection_list$selects[[id]]
      if (!is.null(cells) && cur_selection$plot != id) {
        cur_selection$plot <- id
        cur_selection$cells <- cells
      }
    })
    
    observeEvent(input$plot_rendered, {
      id <- input$plot_rendered
      selection_list$selects[[id]] <- NULL
      if (!is.null(cur_selection$plot) && cur_selection$plot == id) {
        cur_selection$plot <- NULL
        cur_selection$cells <- NULL
      }
    })
    
    observeEvent(input$selection_clicked, {
      cur_markers(get_new_markers(cells=cur_selection$cells))
    },ignoreInit = T)
    
    observeEvent(input$subset_select, {
      showNotification("Subsetting data!", type = "message")
      cur_data$use <- cur_data$use[,cur_selection$cells]
    },ignoreInit = T)
    
    observeEvent(input$subset_undo, {
      downsample_data()
    },ignoreInit = T)
    
  }
  
  shinyApp(ui = ui, server = server)
}
