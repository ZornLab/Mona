#' Launch Mona app
#' @rawNamespace import(shiny, except = "tabsetPanel")
#' @importFrom bs4Dash bs4DashControlbar dashboardPage dashboardHeader dashboardSidebar dashboardBody bs4Accordion accordionItem box sidebarMenu menuItem tabsetPanel tabItems tabItem updateTabItems addPopover
#' @import fresh
#' @import shinycssloaders
#' @import plotly
#' @rawNamespace import(dplyr, except = "vars")
#' @import tidyr
#' @import htmlwidgets
#' @importFrom DT DTOutput renderDT datatable formatStyle
#' @import shinyWidgets
#' @import sortable
#' @importFrom shinyjs runjs show hide addClass removeClass useShinyjs
#' @import imola
#' @import repel
#' @import gfonts
#' @import spsComps
#' @import shinyFiles
#' @import fs
#' @import qs
#' @import ks
#' @import dqrng
#' @importFrom collapse fmatch funique fnunique fmean fsummarise
#' @import gprofiler2
#' @rawNamespace import(ggplot2, except = c("vars","last_plot"))
#' @import cowplot
#' @import grid
#' @rawNamespace import(Seurat, except = "JS")
#' @rawNamespace import(SeuratObject, except = c("show","JS"))
#' @import BPCells
#' @import UCell
#' @import msigdbr
#' @import parsnip
#' @import glmnet
#' @import babelgene
#' @import harmony
#' @import irlba
#' @import callr
#' @param mona_dir A Mona directory, will automatically open at startup
#' @param data_dir A directory of Mona directories. Can be browsed and opened under 'View datasets'.
#' @param load_data Can users load their own datasets? If hosted, use FALSE and provide all needed datasets using 'data_dir'.
#' @param save_data Can users save datasets/sessions? If hosted, use FALSE to make app 'read only' or TRUE to allow editing.
#' @param show_help Include a pop-up on start to help new users?
#' @export

mona <- function(mona_dir=NULL,data_dir=NULL,load_data=TRUE,save_data=TRUE,show_help=FALSE) {
  
  options(shiny.maxRequestSize=8000*1024^2)
  set.seed(123)
  dqset.seed(123)
  options(Seurat.object.assay.version = 'v5')

  resources <- system.file("www", package = "Mona")
  addResourcePath("www", resources)
  resources <- system.file("images", package = "Mona")
  addResourcePath("images", resources)

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
  
  custom_ref_input <- function(inputId) {
    restoredValue <- restoreInput(id = inputId, default = NULL)
    
    if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
      warning("Restored value for ", inputId, " has incorrect format.")
      restoredValue <- NULL
    }
    
    if (!is.null(restoredValue)) {
      restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
    }

    div(
      tags$label(
        class = "input-group-btn input-group-prepend",
        span(
          class = "btn btn-default btn-file action-button", 
          style = htmltools::css(background_color="#fcfcff","border_radius!"="0.25rem","margin_bottom"="0px","flex"=1),
          "Load reference",
          tags$input(
            id = inputId,
            name = inputId,
            type = "file",
            style = "position: absolute !important; top: -99999px !important; left: -99999px !important;",
            `data-restore` = restoredValue
          )
        )
      )
    )
  }
  
  ui <- dashboardPage(
    
    freshTheme = theme,
    dark = NULL,
    help= NULL,
    
    title = "Mona - Single Cell Explorer",
    
    controlbar = bs4DashControlbar(
      id = "control_bar",
      collapsed = T,
      pinned = T,
      overlay=T, 
      skin="light",
      shiny::actionButton("close_control",label="",icon=icon("xmark"),width="32px",style="padding: 3px; background-color: #fcfcff; border-width: 0px;"),
      div(
        id = "settings_div",
        style="margin:10px;",
        sliderInput("downsample","Downsample cells",min = 10, max = 100,value = 100, step = 10,post = "%",width="95%"),
        sliderTextInput("point_size","Point size",grid=T,choices=c("Small","Medium","Large"),selected="Medium",width = "95%"),
        p("Point transparency", style = "font-weight: 700;"),
        materialSwitch("point_transparent","",value=F,status="primary"),
        p("Cell name on hover", style = "font-weight: 700;"),
        materialSwitch("cellname","",value=F,status="primary"),
        p("Scroll zoom", style = "font-weight: 700;"),
        materialSwitch("scroll","",value=F,status="primary"),
        selectizeInput("color_scale_1",label="Discrete color",choices=c("classic","bright","classic-random","bright-random")),
        selectizeInput("color_scale_2",label="Continuous color",choices=c("viridis","plasma","mona")),
        selectizeInput("color_scale_3",label="Scaled color",choices=c("blue-red","purple-yellow","viridis","plasma","mona")),
      ),
      div(
        id="search_div",
        div(
          id="search_div_inner",
          selectizeInput("searched_gene", label = "",choices = NULL,selected=character(0),options=list(maxOptions=100,create=T,persist=F)),
        ),
        uiOutput("gene_search")
      ),
      div(
        id="sets_div",
        style="margin:10px;",
        selectizeInput("set_cat",label="Category",choices=c("H - Hallmark"="H","C1 - Positional"="C1","C2 - Curated"="C2","C3 - Regulatory targets"="C3","C4 - Cancer"="C4","C5 - Ontology"="C5","C6 - Oncogenic"="C6","C7 - Immune"="C7","C8 - Cell type"="C8")),
        selectizeInput("set_subcat",label="Subcategory",choices=c()),
        selectizeInput("set_name",label="Set",choices=c()),
        div(
          id="sets_div_desc",
          uiOutput("set_desc")
        ),
        div (
          id="sets_div_totals",
          uiOutput("set_totals")
        ),
        div (
          id="sets_div_copy",
          shiny::actionButton("copy_set",label="Copy",style="width:100%; padding-top:3px; background-color: #fcfcff;")
        )
      ),
      div(
        id="species1_div",
        style="margin:10px;",
        br(),
        p("Gene sets unavailable for current species",style="font-size: 18px;")
      ),
      div(
        id="species2_div",
        style="margin:10px;",
        br(),
        p("Gene search unavailable for current species",style="font-size: 18px;")
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
      actionLink("data_new",label = "Load dataset",style="color: black; padding-left: 25px;", class="shinyDirectories", "data-title"="Select a Mona directory"),
      actionLink("data_save",label = "Save dataset",style="color: black; padding-left: 25px;"),
      actionLink("session_save",label = "Save session",style="color: black; padding-left: 25px;"),
      actionLink("data_export",label = "Export data",style="color: black; padding-left: 25px;"),
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
        ),
        menuItem(
          "Datasets",
          icon=icon("google-drive"),
          href="https://drive.google.com/drive/folders/1MXUyQz4E7SgWcRrvN5JP65dsGWZv9YOV?usp=sharing"
        )
      )
    ),
    body = dashboardBody(
      useShinyjs(),
      tags$head(
        tags$style(HTML("table {table-layout: fixed;}")),
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
                circleButton("open_search",icon = icon("search"),size = "default",style="margin-right: 5px; margin-bottom: 1vh; margin-top: -5px; background-color: #fcfcff;"),
                circleButton("label_transfer",icon = icon("arrow-right-arrow-left"),size = "default", style="margin-right: 5px; margin-bottom: 1vh; margin-top: -5px; background-color: #fcfcff;"),
                circleButton("get_sets",icon = icon("dna"),size = "default", style="margin-right: 5px; margin-bottom: 1vh; margin-top: -5px; background-color: #fcfcff;"),
                circleButton("settings",icon = icon("sliders"),size = "default", style="margin-right: 5px; margin-bottom: 1vh; margin-top: -5px; background-color: #fcfcff;"),
                circleButton("new_plot",icon = icon("chart-column"),size = "default", style="margin-bottom: 1vh; margin-top: -5px; background-color: #fcfcff;"),
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
                      style='padding-left:1px; padding-right:2px;',
                      div(
                        style="display: inline-block;",
                        dropMenu(
                          shiny::actionButton("de_button_1",icon=icon("1"),label="",width="2.0vw",style="margin-right: 2px; padding: 3px; background-color: #fcfcff;"),
                          radioGroupButtons(
                            inputId = "de_opts_1",
                            label = "",
                            choices = c("Group", "Select"),
                            selected = character(0),
                            direction = "vertical"
                          ),
                          padding=0
                        )
                      ),
                      div(
                        style="display: inline-block;",
                        dropMenu(
                          shiny::actionButton("de_button_2",icon=icon("2"),label="",width="2.0vw",style="margin-left: 2px; padding: 3px; background-color: #fcfcff;"),
                          radioGroupButtons(
                            inputId = "de_opts_2",
                            label = "",
                            choices = c("Rest", "Group", "Select"),
                            selected = character(0),
                            direction = "vertical"
                          ),
                          padding=0
                        )
                      )
                    ),
                    shiny::column(
                      width=6,
                      style='padding-left:1px; padding-right:1px;',
                      align="center",
                      tags$div(id="cell_text",uiOutput("cell_select"),style="padding-top:3px;")
                    ),
                    shiny::column(
                      width=3,
                      style='padding-left:2px; padding-right:1px;',
                      shiny::actionButton("subset_select",icon=icon("scissors"),label="",width="2.0vw",style="margin-right: 2px; padding: 3px; background-color: #fcfcff;"),
                      shiny::actionButton("subset_undo",icon=icon("rotate-left"),label="",width="2.0vw",style="margin-left: 2px; padding: 3px; background-color: #fcfcff;")
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
                        optionHeight = "24vh",
                        keepAlwaysOpen = T,
                        placeholder = "",
                        noOptionsText = ""
                      ),
                      shiny::actionButton("new_anno",icon=icon("plus"),label="",width="2.0vw",style="margin-right: 3px; margin-top: 1.5vh; padding: 3px; background-color: #fcfcff;"),
                      shiny::actionButton("remove_anno",icon=icon("minus"),label="",width="2.0vw",style="margin-right: 3px; margin-top: 1.5vh; padding: 3px; background-color: #fcfcff;"),
                      shiny::actionButton("rename_anno",icon=icon("pen"),label="",width="2.0vw", style="margin-top: 1.5vh; padding: 3px; background-color: #fcfcff;")                
                    ),
                    shiny::column(
                      width=6,
                      virtualSelectInput(
                        inputId = "cluster_select",
                        label = "",
                        choices = c(),
                        search = F,
                        optionsCount = 5,
                        optionHeight = "24vh",
                        keepAlwaysOpen = T,
                        placeholder = "",
                        noOptionsText = ""
                      ),
                      shiny::actionButton("new_cluster",icon=icon("plus"),label="",width="2.0vw",style="margin-right: 3px; margin-top: 1.5vh; padding: 3px; background-color: #fcfcff;"),
                      shiny::actionButton("remove_cluster",icon=icon("minus"),label="",width="2.0vw",style="margin-right: 3px; margin-top: 1.5vh; padding: 3px; background-color: #fcfcff;"),
                      shiny::actionButton("rename_cluster",icon=icon("pen"),label="",width="2.0vw", style="margin-top: 1.5vh; padding: 3px; background-color: #fcfcff;")
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
                  bs4Dash::tabsetPanel(
                    id="gene_tabs",
                    type="pills",
                    tabPanel(
                      title="Markers",
                      div(
                        id="markers_none",
                        p("No markers found")
                      ),
                      div(
                        id="markers_new",
                        p("Calculate markers?"),
                        shiny::actionButton("markers_find",icon=icon("arrow-right"),label="",width="2.0vw",style="margin-top: 1.2vh; padding: 3px; background-color: #fcfcff;")
                      ),
                      div(
                        id="markers_show",
                        withSpinner(DTOutput("marker_table"),type=5,color="#738bfb"),
                        div(
                          id="marker_controls",
                          fluidRow(
                            shiny::column(
                              width=2,
                              downloadButton("save_markers",icon=icon("download"),label="",style="width: 2.0vw; margin-top: 1.2vh; padding: 3px; margin-left: 12px; background-color: #fcfcff;"),
                            ),
                            shiny::column(
                              width=8,
                              div(
                                id="gene_div",
                                textInput("gene_filter",label="",value="",width="90%")
                              ),
                              div(
                                id="fc_div",
                                sliderTextInput("fc_filter",NULL,grid=F,choices=seq(-10,10,1),selected=c(-10,10),width = "90%",hide_min_max = T)
                              ),
                              div(
                                id="pval_div",
                                sliderTextInput("pval_filter",NULL,grid=F,choices=c(1e-200,1e-150,1e-100,1e-50,1e-20,1e-10,1e-2,5e-2),selected=5e-2,width = "90%", hide_min_max = T)
                              )
                            ),
                            shiny::column(
                              width=2,
                              shiny::actionButton("copy_markers",icon=icon("copy"),label="",width="2.0vw",style="margin-top: 1.2vh; padding: 3px; margin-right: 12px; background-color: #fcfcff;")
                            )
                          )
                        )
                      )
                    ),
                    tabPanel(
                      title="DEG",
                      div(
                        id="deg_none",
                        p("No DEGs found")
                      ),
                      div(
                        id="deg_new",
                        h5("Group 1"),
                        tags$div(uiOutput("de_cells_1_text"),style="padding-top:3px;"),
                        h5("Group 2",style="padding-top:8px;"),
                        tags$div(uiOutput("de_cells_2_text"),style="padding-top:3px;"),
                        br(),
                        shiny::actionButton("deg_find",icon=icon("arrow-right"),label="",width="2.0vw",style="margin-top: 1.2vh; margin-right: 2px; padding: 3px; background-color: #fcfcff;"),
                        shiny::actionButton("deg_return",icon=icon("arrow-rotate-left"),label="",width="2.0vw",style="margin-top: 1.2vh; margin-left: 2px; padding: 3px; background-color: #fcfcff;"),
                      ),
                      div(
                        id="deg_show",
                        withSpinner(DTOutput("deg_table"),type=5,color="#738bfb"),
                        div(
                          id="deg_controls",
                          fluidRow(
                            shiny::column(
                              width=2,
                              downloadButton("save_deg",icon=icon("download"),label="",style="width: 2.0vw; margin-top: 1.2vh; padding: 3px; margin-left: 12px; background-color: #fcfcff;"),
                            ),
                            shiny::column(
                              width=8,
                              div(
                                id="gene_div_deg",
                                textInput("gene_filter_deg",label="",value="",width="90%")
                              ),
                              div(
                                id="fc_div_deg",
                                sliderTextInput("fc_filter_deg",NULL,grid=F,choices=seq(-10,10,1),selected=c(-10,10),width = "90%",hide_min_max = T)
                              ),
                              div(
                                id="pval_div_deg",
                                sliderTextInput("pval_filter_deg",NULL,grid=F,choices=c(1e-200,1e-150,1e-100,1e-50,1e-20,1e-10,1e-2,5e-2),selected=5e-2,width = "90%", hide_min_max = T)
                              )
                            ),
                            shiny::column(
                              width=2,
                              shiny::actionButton("copy_deg",icon=icon("copy"),label="",width="2.0vw",style="margin-top: 1.2vh; padding: 3px; margin-right: 12px; background-color: #fcfcff;")
                            )
                          )
                        )
                      )
                    ),
                    tabPanel(
                      title="GO",
                      div(
                        id="go_select",
                        br(),
                        p("Choose genes"),
                        selectizeInput("go_genes",
                         label = "",
                         choices=c("Markers","DEGs"),
                         selected=NULL,
                         width = "80%"
                        ),
                        br(),br(),
                        shiny::actionButton("go_confirm",icon=icon("arrow-right"),label="",width="2.0vw",style="margin-top: 1.2vh; margin-right: 2px; padding: 3px; background-color: #fcfcff;"),
                        shiny::actionButton("go_return",icon=icon("arrow-rotate-left"),label="",width="2.0vw",style="margin-top: 1.2vh; margin-left: 2px; padding: 3px; background-color: #fcfcff;"),
                      ),
                      div(
                        id="go_none",
                        p("No GO terms found")
                      ),
                      div(
                        id="go_species",
                        p("Species not supported")
                      ),
                      div(
                        id="go_show",
                        withSpinner(DTOutput("go_table"),type=5,color="#738bfb")
                      ),
                      div(
                        id="go_controls",
                        style="margin-top:27.0vh;",
                        fluidRow(
                          column(
                            width=2,
                            downloadButton("save_go",icon=icon("download"),label="",style="width: 2.0vw; margin-top: 1.2vh; padding: 3px; margin-left: 12px; background-color: #fcfcff;")
                          ),
                          shiny::column(
                            width=8,
                            div(
                              id="id_div",
                              textInput("id_filter",label="",value="",width="75%")
                            ),
                            div(
                              id="name_div",
                              textInput("name_filter",label="",value="",width="90%")
                            ),
                            div(
                              id="pval_div_go",
                              sliderTextInput("pval_filter_go",NULL,grid=F,choices=c(1e-50,1e-20,1e-10,1e-2,5e-2),selected=5e-2,width = "90%", hide_min_max = T)
                            )
                          ),
                          column(
                            width=2,
                            shiny::actionButton("go_choose",icon=icon("arrow-rotate-left"),label="",width="2.0vw",style="margin-top: 1.2vh; padding: 3px; margin-right: 12px; background-color: #fcfcff;")
                          )
                        )
                      )
                    ),
                    tabPanel(
                      title="Sets",
                      div(
                        id="sets"
                      ),
                      shiny::actionButton("new_gene_set",icon=icon("plus"),label="",width="50%",style="padding: 1px; position: absolute; top: 86% !important; left:25% !important; background-color: #fcfcff;")
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
                  align_content = "flex-start",
                  plotUI("plot1")
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
          fluidRow(
            column(
              width=1
            ),
            column(
              width = 10,
              bs4Accordion(
                id = "help_sections",
                accordionItem(
                  title = "Navigation",
                  status = "lightblue",
                  collapsed = T,
                  div(img(src = "images/layout.png", height = 250,width = 375),style="text-align: center"),
                  br(),
                  tags$ul(
                    tags$li("The navigation bar at the top is where datasets are loaded and saved. Additionally, view information about the current dataset or export gene expression/metadata."),
                    tags$li("The plot section holds any plots you create. It is dynamic and can contain up to 8 at once. Plots can also be rearranged or expanded to take up the full screen."),
                    tags$li("The tools area has multiple features including searching for genes/gene sets, adjusting settings, and creating new plots."),
                    tags$li("The cell section is where cell metadata can be edited. Select a particular group to view the associated markers. Selections made within scatter plots also appear here and can be named or subsetted."),
                    tags$li("Finally, the gene section is where you can view markers/DEGs, GO terms associated with the markers/DEGs, and create gene sets for use in plots.")
                  )
                ),
                accordionItem(
                  title = "Functions",
                  status = "lightblue",
                  collapsed = T,
                  h5("Plots"),
                  tags$ul(
                  tags$li("The top left button of each plot box opens the plot settings. Here you control what data is displayed and can switch between the 5 different plot types."),
                  tags$li("When hovering over a plot, a control bar will appear with tools for zooming, panning, drawing, and selecting."),
                  tags$li("Click on the top of the plot boxes to drag and rearrange them. There is also a 'camera' button for saving static images, and an 'expand' button for viewing the plot full-screen."),
                  tags$li("For plots with legends, clicking on a group will hide that group, while double clicking will focus only on that group."),
                  tags$li("All plots show additional information when hovering. For very large plots, zoom into a specific area for a better view before hovering."),
                  ),
                  h5("Selection"),
                  tags$ul(
                  tags$li("2D scatter plots such as UMAPs allow for selecting specific populations of cells. These selections can be named, used in differential expression, or subset to focus only on them."),
                  tags$li("Use the box/lasso tool in the control bar to manually select cells. Hold shift to combine multiple selections."),
                  tags$li("Alternatively, for metadata use the legend to show/hide the groups you need, then click the 'Select visible' button. For genes/features, use the slider to select a specific value range."),
                  ),
                  h5("Differential expression"),
                  tags$ul(
                    tags$li("To view genes associated with a particular group, start by clicking on a group in the cell box."),
                    tags$li("Under the 'Markers' tab of the gene box, the markers may be already available, or it may ask to calculate them. Afterwards, they will be remembered as long as the group doesn't change."),
                    tags$li("For other comparisons, use the '1' and '2' buttons in the cell box to choose particular groups/selections. They will appear under the 'DEG' tab of the gene box, where you can calculate and view the results."),
                    tags$li("Markers and DEGs can also be saved to a file or gene set for further use.")
                  ),
                  h5("Gene sets"),
                  tags$ul(
                  tags$li("Gene sets are useful in many contexts: to avoid repeatedly typing in genes when making plots, to view multiple genes in a heatmap/bubble plot, and to view 'gene set scores' that represent the collective expression of the set."),
                  tags$li("Go to the 'Sets' tab of the gene section. From here, you can manually enter a list of genes, or upload a gene set file. The file should be tab/comma separated and can be organized by row/column. If there is more than one set, names must be along the first row/column."),
                  tags$li("Gene sets can also be generated from markers/DEGs. Use the 'Save to set' button when viewing them."),
                  tags$li("Finally, you can search for and download preexisting gene sets from the MSigDB with the 'Find gene sets' button."),
                  tags$li("Your sets will be available in the scatter/heatmap/violin plot settings. Depending on the plot type, you can view them all at once, select individual genes, or calculate the gene set score.")
                  ),
                  h5("Saving"),
                  tags$ul(
                  tags$li("Within the cell box users can add, rename, or remove cell annotations. But this introduces the possibility of losing or overwriting important data."),
                  tags$li("So users can explore freely,", strong("these changes do not save automatically!"), " Before closing the app or switching datasets, use 'Save dataset' to keep the current changes."),
                  tags$li("To also save your current gene sets and settings, use 'Save session'. They will then be loaded every time you open the dataset.")
                  ),
                  h5("Label transfer"),
                  tags$ul(
                    tags$li("Mona has a custom method for label transfer, which finds the best matching cell type or annotation in a reference and applies them to your data."),
                    tags$li("Users must first create a 'Mona reference' using the 'create_mona_ref()' function. It accepts Mona directories, Seurat objects, or raw data and produces models from the dataset."),
                    tags$li("Back in Mona with the dataset you want to label open, click on the 'Transfer labels' button. Load a reference, select a label, and press 'Transfer'. After a few minutes, the predictions will appear as a new annotation."),
                    tags$li(strong("Your reference and query must be compatible,")," meaning they are the same assay (RNA/ATAC) and have the same normalization. Otherwise, any predictions cannot be relied upon."),
                    tags$li("Note that the species does not need to be the same, as Mona will attempt to find orthologs. However, if too few genes are in common it will be unable to continue.")
                  )
                ),
                accordionItem(
                  title = "Plot Types",
                  status = "lightblue",
                  collapsed = T,
                  h5("Scatter"),
                  p("Use to view cell metadata, gene expression, etc. at the per-cell level. Most commonly used with UMAPs but any 2D/3D embedding can be plotted. Also supports plotting variables on each axis."),
                  h5("Heatmap"),
                  p("Use for understanding broad patterns in gene expression. Genes can be viewed either per-cell or the average per-group. Bubble plot can also show percent expression per-group."),
                  h5("Violin"),
                  p("Use to view the distribution of a gene/feature, either across the entire dataset or per-group."),
                  h5("Bar"),
                  p("Use to view the proportion of cells across groups and how different metadata relate to one another."),
                  h5("Volcano/MA"),
                  p("Use to visualize differentially expressed genes, and find those with high fold change/significance/expression.")
                ),
                accordionItem(
                  title = "Data Preparation",
                  status = "lightblue",
                  collapsed = T,
                  p("If you have not already processed your data, consider using Mona's built-in functions 'process_mona()' and 'integrate_mona()', which are based on Seurat. Visit the GitHub for more information."),
                  p("To view your datasets, you must convert into a custom format called the 'Mona directory' - use 'save_mona_dir()' on a Seurat v5 object to generate one."),
                  p("Alternatively, you can create a Mona directory out of three components: log-norm counts, a table of metadata, and a list of reductions. This can be helpful when working with other formats."),
                  p("Afterwards, any Mona directory can be viewed in Mona by clicking on 'Load dataset' and selecting it, or calling 'mona()' with the path to the directory.")
                ),
                accordionItem(
                  title = "Performance",
                  status = "lightblue",
                  collapsed = T,
                  p("Here are some recommendations for having a smooth experience:"),
                  tags$ul(
                    tags$li("Mona has been tested on 500,000 cells without issue, but this will vary depending on your system. Expect things to run slower for larger datasets."),
                    tags$li("Keep your datasets on the system where Mona is installed. Communicating with a remote directory/server will create a noticeable delay."),
                    tags$li("If it's not important to view every cell, downsample your data under 'Settings'. This can be useful when generating large heatmaps."),
                    tags$li("While the app is executing something, like rendering a plot or calculating markers, allow it to finish before performing another action."),
                    tags$li("Actions like subsetting will cause all plots to refresh simultaneously, which can take time to process if many are open.")
                  )
                ),
                accordionItem(
                  title = "FAQ",
                  status = "lightblue",
                  collapsed = T,
                  h5("Why can't I find a particular gene? It doesn't show up anywhere/I can't add it to a gene set?"),
                  p("It's possible the gene had low expression and was filtered out during processing. More likely, you are using an alternative name and need to find the specific name used in the dataset."),
                  h5("Something's wrong with the dataset, and X is being treated as categorical/continuous when it should be the other way around?"),
                  p("Mona tries its best to determine what is categorical 'metadata' like clusters and what is a continuous 'feature' like mitochondrial percentage, but sometimes it can be ambiguous."),
                  p("If you see something incorrect, convert the column in your metadata with 'as.character' for categorical or 'as.numeric' for continuous, then recreate your Mona directory."),
                  h5("What are the 'supported species'?"),
                  p("Mona works with any single cell data from any species. But some functionality - searching for genes and gene sets - is limited to certain species."),
                  p("When creating a Mona directory, please use the following common names if applicable: human, mouse, rat, fruitfly, nematode, zebrafish, frog, pig. Note that 'nematode' refers to C. elegans and 'frog' refers to Xenopus tropicalis."),
                  h5("How do the markers and DEGs work?"),
                  p("Both are calculated using the MAST method. For improved efficiency, cells are downsampled to 500 per group, and only the top 500 significant genes are kept."),
                  p("Markers are simply the DEGs for a group compared to all the other groups in an annotation, AKA a 'one versus rest' approach. DEGs are more flexible, because they can compare any two populations of cells."),
                  p("Note that markers are meant to reflect the entire dataset, and so downsampling/subsetting are not taken into account. Use DEGs for these cases."),
                  h5("How does the label transfer work?"),
                  p("Mona's label transfer is a supervised machine learning method inspired by multiple approaches."),
                  p("It aims for a balance between speed and accuracy, using a 100 component PCA as the feature space, Harmony for batch effect removal, and logistic regression for classification."),
                  p("When preparing a reference, the ideal dataset should have a balanced representation of labels. If there are too few cells for a given label, that label will be excluded."),
                  p("The reference does not need to be the same species as your dataset, but this is likely to impact performance."),
                  h5("Why do different plots have different gene expression values?"),
                  p("Scatter and violin plots show the actual log-normalized expression. 'Density mode' and gene set scores are exceptions to this and have no specific unit."),
                  p("Heatmaps and bubble plots use scaled values by default to create better contrast and show where expression is above/below the mean. This can be disabled if desired."),
                  p("Be careful when comparing values across plots. Colors can have completely different meanings depending on the expression range within each plot."),
                  h5("Why do the plots sometimes refresh and I lose my changes?"),
                  p("Plots are redrawn whenever the entire dataset changes, like downsampling/subsetting. It also occurs when you edit the metadata or gene set currently in use by the plot. Finally, plots will redraw when changing the plot type."),
                  h5("How do I view my data as a 3D embedding?"),
                  p("Using the built-in functions process_mona() or integrate_mona(), a 3D UMAP will be calculated automatically. If processing on your own in Seurat, call RunUMAP() an additional time with 'n.components=3L'."),
                  p("You can then switch between embeddings using the 'Layout' dropdown."),
                  h5("What are the limitations of Mona?"),
                  p("Mona directories are built from a single matrix. To view multiple samples/assays they must be integrated in some way, or simply create separate Mona directories."),
                  p("Mona is also designed with a focus on RNA data. ATAC peaks can still be converted to gene scores, and future support is possible.")
                )
              )
            ),
            column(
              width=1
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
            h5("Thanks to the following people for their valuable feedback:"),
            tags$ul(
              tags$li("Aaron Zorn"),
              tags$li("Andrea Holderbaum"),
              tags$li("Jacek Biesada")
            ),
            br(),
            h5("Supported by the Center for Stem Cell & Organoid Medicine (CuSTOM) and the Developmental Biology Division, Cincinnati Children's Hospital"),
            img(src = "images/cchmc.jpg", height = 100, width = 300),
            br(),br(),
            h5("Mona would not be possible without the following excellent R packages:"),
            tags$ul(
              tags$li(tags$a(href="https://satijalab.org/seurat/","Seurat")),
              tags$li(tags$a(href="https://bnprks.github.io/BPCells/","BPCells")),
              tags$li(tags$a(href="https://github.com/RGLab/MAST/","MAST")),
              tags$li(tags$a(href="https://plotly.com/r/","plotly")),
              tags$li(tags$a(href="https://github.com/traversc/qs","qs")),
              tags$li(tags$a(href="https://github.com/carmonalab/UCell","UCell")),
              tags$li(tags$a(href="https://rinterface.github.io/bs4Dash/","bs4Dash")),
              tags$li(tags$a(href="https://biit.cs.ut.ee/gprofiler/page/r","gprofiler2")),
              tags$li(tags$a(href="https://portals.broadinstitute.org/harmony/","harmony")),
              tags$li(tags$a(href="https://parsnip.tidymodels.org/","parsnip")),
              tags$li(tags$a(href="https://github.com/igordot/babelgene","babelgene")),
              tags$li(tags$a(href="https://github.com/dreamRs/shinyWidgets","shinywidgets")),
              tags$li(tags$a(href="https://github.com/thomasp85/shinyFiles","shinyFiles")),
              tags$li(tags$a(href="https://deanattali.com/shinyjs/","shinyjs")),
              tags$li(tags$a(href="https://rstudio.github.io/sortable/","sortable"))            
            ),
            br(),
            h5("Gene search provided by:"),
            tags$a(href='https://mygene.info/',"MyGene.info"),
            br(),br(),
            h5("Plot icons provided by:"),
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
    addPopover(id="get_sets",options=list(content="Find gene sets",placement="top",delay=500,trigger="hover"))
    addPopover(id="label_transfer",options=list(content="Transfer labels",placement="top",delay=500,trigger="hover"))

    addPopover(id="new_anno",options=list(content="Create new annotation",placement="top",delay=500,trigger="hover"))
    addPopover(id="remove_anno",options=list(content="Remove annotation",placement="top",delay=500,trigger="hover"))
    addPopover(id="rename_anno",options=list(content="Edit annotation",placement="top",delay=500,trigger="hover"))
    addPopover(id="new_cluster",options=list(content="Create group from selection",placement="top",delay=500,trigger="hover"))
    addPopover(id="remove_cluster",options=list(content="Remove group",placement="top",delay=500,trigger="hover"))
    addPopover(id="rename_cluster",options=list(content="Edit group",placement="top",delay=500,trigger="hover"))
    
    addPopover(id="markers_find",options=list(content="Calculate markers",placement="bottom",delay=500,trigger="hover"))
    addPopover(id="save_markers",options=list(content="Export markers",placement="top",delay=500,trigger="hover"))
    addPopover(id="copy_markers",options=list(content="Save to set",placement="top",delay=500,trigger="hover"))

    addPopover(id="deg_find",options=list(content="Calculate DEGs",placement="bottom",delay=500,trigger="hover"))
    addPopover(id="deg_return",options=list(content="Return to previous results",placement="bottom",delay=500,trigger="hover"))
    addPopover(id="save_deg",options=list(content="Export DEGs",placement="top",delay=500,trigger="hover"))
    addPopover(id="copy_deg",options=list(content="Save to set",placement="top",delay=500,trigger="hover"))

    addPopover(id="go_confirm",options=list(content="Fetch GO terms",placement="top",delay=500,trigger="hover"))
    addPopover(id="go_return",options=list(content="Return to previous results",placement="top",delay=500,trigger="hover"))
    addPopover(id="save_go",options=list(content="Export terms",placement="top",delay=500,trigger="hover"))
    addPopover(id="go_choose",options=list(content="Choose new genes",placement="top",delay=500,trigger="hover"))
    
    addPopover(id="new_gene_set",options=list(content="Create new gene set",placement="top",delay=500,trigger="hover"))
    
    if (show_help) {
      showModal(modalDialog(
        title = "Welcome to Mona!",
        easyClose = T,
        size="m",
        align="center",
        "Click on 'View datasets' and select a dataset to get started.",
        br(),br(),
        img(src = "images/intro1.png", style="padding-right: 10px;"),
        "Click this button to open the plot settings.",
        br(),br(),
        img(src = "images/intro2.png", style="padding-right: 10px;"),
        "Use these menus to edit metadata and view markers.",
        br(),br(),
        img(src = "images/intro3.png", style="padding-right: 10px;"),
        "Click here and go to 'Help' for detailed info on all features.",
        footer = NULL
      ))  
    }
    
    dataset <- reactiveValues(meta=NULL,reduct=NULL,sets=NULL,info=NULL,markers=NULL,exp=NULL,ranks=NULL,genes=NULL,anno=NULL,quality=NULL,subset=NULL)
    dataset_dirs <- reactiveVal(NULL)
    mona_obj <- reactiveVal(NULL)
    load_dir <- reactiveVal(NULL)
    output$data_link <- renderUI({
      if (!is.null(dataset$exp)){
        tagList(tags$li(class='dropdown', actionLink("data_info",label=dataset$info$name,icon=tags$i(class = "fas fa-info-circle", style="font-size: 18px; padding-right: 5px; color: #b9c5fd;"),style="color: black; font-size: 120%;")))
      }
    })
    
    observeEvent(input$data_info, {
      showModal(modalDialog(
        title = "Data Information",
        easyClose = T,
        size="m",
        h5(tags$b("Name")),
        p(paste0(dataset$info$name)),
        h5(tags$b("Description")),
        p(dataset$info$description),
        h5(tags$b("Size")),
        p(paste0(nrow(dataset$exp), " cells, ", ncol(dataset$exp), " genes")),
        h5(tags$b("Species")),
        p(dataset$info$species),
        footer = NULL
      ))
    })
    shinyjs::runjs("$('#control_bar').css({'top':'45px','display':'block','height':'auto'});")
    control_mode <- reactiveVal("closed")
    control_modes_all <- c("settings","search","sets","species1","species2")
    species_all <- c("human","mouse","rat","fruitfly","nematode","zebrafish","frog","pig")

    close_sidebar <- function() {
      control_mode("closed")
      shinyjs::removeClass(selector = "body",class="control-sidebar-slide-open")
    }
    
    observeEvent(control_mode(), {
      lapply(control_modes_all, function(x) shinyjs::hide(paste0(x,"_div")))
      if (control_mode() != "closed") {
        shinyjs::show(paste0(control_mode(),"_div"))
      }
    })
    
    reference <- reactiveVal()
    transfer_process <- reactiveVal(NULL)
    check_transfer <- reactiveVal(F)
    
    observeEvent(input$label_transfer, {
      if (!is.null(dataset$exp)) {
        reference(NULL)
        showModal(modalDialog(
          title = "Transfer labels",
          easyClose = T,
          size="m",
          uiOutput("ref_ui"),
          footer = NULL
        ))
      }
    })
    
    observeEvent(input$import_ref, {
      if (!is.integer(input$import_ref)) {
        ref_file <- input$import_ref[["datapath"]]
        showNotification("Loading reference...", type = "message")
        ref <- qread(ref_file)
        if (sum(c("species","norm","genes","center","scale","rotation","model") %in% names(ref[[1]])) == 7) {
          reference(ref)
        } else {
          showNotification("Not a valid reference file", type = "message")
        }
      }
    })
    
    output$ref_ui <- renderUI({
      ref_info <- reference()[[1]]
      if (is.null(ref_info)) {
        ref_info <- list(species="",type="",norm="")
      }
      fluidRow(
        column(
          width=5,
          div(
            h6(paste0("Species: ",ref_info$species)),
            h6(paste0("Type: ",ref_info$type)),
            h6(paste0("Normalization: ",ref_info$norm))
          ),
          br(),
          custom_ref_input("import_ref")
        ),
        column(
          width=2
        ),
        column(
          width=5,
          selectizeInput("label_anno",label="Choose annotation",choices=c()),
          br(),
          shiny::actionButton("start_transfer","Transfer",style="background-color: #fcfcff;",width="100%")
        )
      )
    })
    
    observeEvent(reference(), {
      updateSelectizeInput(session,"label_anno",choices = names(reference()))
    })
    
    observe({
      req(check_transfer())
      invalidateLater(millis = 500)
      p <- isolate(transfer_process())
      if (p$is_alive() == FALSE) {
        check_transfer(F)
        transfer_process(NULL)
        results <- p$get_result()
        removeNotification(id="transfer_wait")
        if (length(results) == 1) {
          showNotification(results, type = "message")
        } else {
          dataset$meta[[paste0(input$label_anno,".predicted")]] <- results
          cur_anno <- input$anno_select
          update_anno_names()
          updateVirtualSelect(
            inputId = "anno_select",
            choices = c(dataset$anno),
            selected = cur_anno
          )
          showNotification("Label transfer complete!", type = "message")
        }
      }
    })
    
    observeEvent(input$start_transfer, {
      validate(
        need(dataset$exp,""),
        need(reference(),""),
        need(input$label_anno,"")
      )
      removeModal(session)
      showNotification("Transferring... please wait", type = "message",duration=NULL,id="transfer_wait")
      Sys.sleep(0.5)
      arg_list <- list(mona_annotate,reference(),input$label_anno,dataset$exp,dataset$info$species)
      p <- r_bg(function(arg_list) arg_list[[1]](arg_list[[2]],arg_list[[3]],arg_list[[4]],arg_list[[5]]),supervise = T,args=list(arg_list))
      transfer_process(p)
      check_transfer(T)
    })
    
    observeEvent(input$get_sets, {
      if (!is.null(dataset$exp)){
        if (control_mode() == "closed") {
          if (dataset$info$species %in% species_all) {
            control_mode("sets")
          } else {
            control_mode("species1")
          }
          shinyjs::addClass(selector = "body",class="control-sidebar-slide-open")
        } else if (control_mode() != "sets" && control_mode() != "species1") {
          if (dataset$info$species %in% species_all) {
            control_mode("sets")
          } else {
            control_mode("species1")
          }
        } else {
          close_sidebar()
        }
      }
    })
    
    observeEvent(input$open_search, {
      if (!is.null(dataset$exp)){
        if (control_mode() == "closed") {
          if (dataset$info$species %in% species_all) {
            control_mode("search")
          } else {
            control_mode("species2")
          }
          shinyjs::addClass(selector = "body",class="control-sidebar-slide-open")
        } else if (control_mode() != "search" && control_mode() != "species2") {
          if (dataset$info$species %in% species_all) {
            control_mode("search")
          } else {
            control_mode("species2")
          }
        } else {
          close_sidebar()
        }
      }
    })
    
    observeEvent(input$close_control, {
      close_sidebar()
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
      } else if (control_mode() != "settings") {
        control_mode("settings")
      } else {
        close_sidebar()
      }
    })
    
    # Data input
    #--------------------------------------------
    
    update_anno_names <- function() {
      meta <- colnames(dataset$meta)
      filter <- sapply(meta, function(x) class(dataset$meta[[x]]) %in% c("integer","numeric"))
      dataset$anno <- meta[!filter]
    }
    
    downsample_data <- function() {
      if (input$downsample == 100) {
        dataset$subset <- 1:nrow(dataset$exp)
      } else {
        cell_count <- nrow(dataset$exp)
        dataset$subset <- dqsample.int(cell_count, round(cell_count*(input$downsample/100)))
      }
    }
    
    root <- c(home=fs::path_home())
    save_dir <- reactiveVal("examples/")
    shinyDirChoose(input, id='data_new', roots=root, session = session,allowDirCreate = F)

    password_check <- function() {
      if (is.null(mona_obj()[["password"]])) {
        reset_data()
      } else {
        showModal(modalDialog(
          title = "Private dataset",
          easyClose = T,
          size="s",
          "Enter password to open:",
          passwordInput("password_text",label="",value=""),
          shiny::actionButton("password_confirm", "Confirm",style="background-color: #fcfcff;"),
          footer = NULL
        ))   
      }
    }
    
    # Sets up "dataset" when a new dataset is loaded
    # Note that depending on where data was processed, path to matrix may need to be updated
    data_setup <- function() {
      showNotification("Loading dataset...", type = "message")
      dataset$meta <- mona_obj()[["meta"]]
      dataset$reduct <- mona_obj()[["reduct"]]
      dataset$sets <- mona_obj()[["sets"]]
      dataset$info <- mona_obj()[["info"]]
      dataset$markers <- mona_obj()[["markers"]]
      mona_obj(NULL)
      dataset$exp <- open_matrix_dir(file.path(load_dir(),"exp"))
      dataset$exp@dir <- file.path(load_dir(),"exp")
      dataset$ranks <- open_matrix_dir(file.path(load_dir(),"ranks"))
      dataset$ranks@dir <- file.path(load_dir(),"ranks")
      save_dir(load_dir())
      meta <- colnames(dataset$meta)
      filter <- sapply(meta, function(x) class(dataset$meta[[x]]) %in% c("integer","numeric"))
      dataset$quality <- meta[filter]
      dataset$anno <- meta[!filter]
      updateVirtualSelect(inputId = "anno_select",choices = c(dataset$anno),selected = NULL)
      updateVirtualSelect(inputId = "cluster_select",choices = c(""),selected = NULL)
      dataset$genes <- sort(colnames(dataset$exp))
      updateSelectizeInput(session, "searched_gene", choices = c(dataset$genes),selected=character(0),server = T)
      updateSelectizeInput(session,"set_cat",selected=character(0))
      downsample_data()
      shinyjs::show("new_gene_set")
      if ("session.qs" %in% list.files(load_dir())) {
        session_data <- qread(file.path(load_dir(),"session.qs"))
        point_size <- as.character(session_data[[1]])
        choice = switch(point_size, "5"="Small", "7"="Medium", "9"="Large")
        updateSliderTextInput(session,"point_size",selected=choice)
        point_transparent <- as.character(session_data[[2]])
        choice = switch(point_transparent, "1.0"=F, "0.4"=T)
        updateMaterialSwitch(session,"point_transparent",value=choice)
        cellname <- session_data[[3]]
        updateMaterialSwitch(session,"cellname",value=cellname)
        scroll <- session_data[[4]]
        updateMaterialSwitch(session,"scroll",value=scroll)
        updateSelectizeInput(session,"color_scale_1",selected=session_data[[5]])
        updateSelectizeInput(session,"color_scale_2",selected=session_data[[6]])
        updateSelectizeInput(session,"color_scale_3",selected=session_data[[7]])
        set_names <- session_data[[8]]
        set_genes <- session_data[[9]]
        num_sets <- length(set_names)
        if (num_sets > 0) {
          for (x in 1:num_sets) {
            set_id(set_id() + 1)
            id <- paste0("geneset",set_id())
            insertUI(
              selector = '#sets',
              where = "beforeEnd",
              ui = genesUI(id)
            )
            genesets$sets[[id]] <- genesServer(id,genesets,dataset,genes=set_genes[[x]],name=set_names[[x]],upload=geneset_upload)
          }
        }
      }
    }
    
    # Called when a dataset is loaded when another data is already loaded
    # Essentially wipes everything: plots, gene sets, selection, metadata, etc.
    reset_data <- function() {
      if (!is.null(dataset$exp)) {
        lapply(names(genesets$sets), function(x) {
          removeUI(paste0("#",x),immediate = T)
          genesets$sets[[x]] <- NULL
        })
        lapply(names(selection_list$selects), function(x) {
          selection_list$selects[[x]] <- NULL
        })
        lapply(names(selection_list$counts), function(x) {
          selection_list$counts[[x]] <- NULL
        })
        lapply(names(plots_list$plots), function(x) {
          removeUI(paste0("#",x,"-render_plot"),immediate = T)
          remove_shiny_inputs(x,input)
          remove_observers(x,session)
          plots_list$plots[[x]] <- NULL
        })
        num_plots(0)
        cur_markers(NULL)
        cur_degs(NULL)
        cur_terms(NULL)
        close_sidebar()
        de_cells_1$name <- NULL
        de_cells_1$cells <- NULL
        de_cells_2$name <- NULL
        de_cells_2$cells <- NULL
        marker_mode(NULL)
        deg_mode(NULL)
        shinyjs::hide("go_controls")
        go_mode("select")
        cur_selection$plot <- NULL
        cur_selection$cells <- NULL
        updateSliderInput(session,"downsample",value=100)
        shinyjs::delay(500,data_setup())
        shinyjs::delay(500,add_new_plot())
      } else {
        data_setup()
      }
    }
    
    observeEvent(input$password_confirm, {
      removeModal(session)
      if (input$password_text == mona_obj()[["password"]]) {
        reset_data()
      } else {
        showNotification("Password incorrect", type = "message")
      }
    })
    
    observeEvent(input$dataset_click, {
      choice <- input$dataset_select
      removeModal(session)
      load_dir(dataset_dirs()[[choice]])
      mona_obj(qread(file.path(load_dir(),"mona.qs")))
      mona_files <- list.files(load_dir())
      if (sum(c("mona.qs","exp","ranks") %in% mona_files) == 3) {
        password_check()
      } else {
        showNotification("Not a valid Mona directory", type = "message")
      }
    })
    
    observeEvent(input$data_avail, {
      showModal(
        modalDialog(
        title = "Select dataset",
        easyClose = T,
        size="m",
        bs4Accordion(
          id = "dataset_select",
          .list=dataset_choices()
        ),
        footer = NULL
        )
      )
      shinyjs::runjs("
        var elements = document.querySelectorAll('.dataset_load')
        elements.forEach(element => {
          element.addEventListener('click', () => Shiny.onInputChange('dataset_click', Math.random()))
        })
      ")
      shinyjs::runjs("
        $('#dataset_select .collapse').on('shown.bs.collapse', function(e) {
          var $card = $(this).closest('.card');
          var $open = $($(this).data('parent')).find('.collapse.show');
          var additionalOffset = 0;
          if($card.prevAll().filter($open.closest('.card')).length !== 0)
          {
            additionalOffset =  $open.height();
          }
          $('#dataset_select').animate({
            scrollTop: $card[[0]].offsetTop - additionalOffset
          }, 300);
        });
      ")
    })
    
    gene_sets_export <- reactive({
      if (length(genesets$sets) > 0) {
        set_names <- lapply(genesets$sets,function(x) x$name())
        setNames(names(set_names), set_names)
      } else {
        NULL
      }
    })
    
    observeEvent(input$data_export, {
      if (!is.null(dataset$exp)) {
        showModal(modalDialog(
          title = "Export data",
          easyClose = T,
          size="m",
          fluidRow(
            column(
              width=6,
              h5("Expression"),
              br(),
              selectizeInput("exp_export",label="Set",choices=gene_sets_export()),
              textInput("exp_export_name",label="Name",value=paste0("exp_",Sys.Date())),
              shiny::actionButton("exp_export_confirm", "Export",style="background-color: #fcfcff;"),
              downloadButton("exp_download",label = "", style = "visibility: hidden;")
            ),
            column(
              width=6,
              h5("Metadata"),
              br(),
              style='padding-left:8px; border-left: 1px solid;',
              selectizeInput("meta_export",label="Annotation/Value",choices=c("All",dataset$anno,dataset$quality)),
              textInput("meta_export_name",label="Name",value=paste0("meta_",Sys.Date())),
              shiny::actionButton("meta_export_confirm", "Export",style="background-color: #fcfcff;"),
              downloadButton("meta_download",label = "", style = "visibility: hidden;")
            )
          ),
          footer = NULL
        ))
      }
    })
    
    observeEvent(input$exp_export_confirm, {
      if (isTruthy(input$exp_export) && isTruthy(input$exp_export_name)) {
        removeModal(session)
        shinyjs::click("exp_download")
      }
    })
    
    observeEvent(input$meta_export_confirm, {
      if (isTruthy(input$meta_export) && isTruthy(input$meta_export_name)) {
        removeModal(session)
        shinyjs::click("meta_download")
      }
    })
    
    output$meta_download <- downloadHandler(
        filename = function() {
          paste0(input$meta_export_name,".txt")
        },
        content = function(file) {
          if (input$meta_export == "All") {
            write.table(dataset$meta[dataset$subset,,drop=F],file,sep="\t",col.names = NA,row.names = T,quote = F)
          } else {
            write.table(dataset$meta[dataset$subset,input$meta_export,drop=F],file,sep="\t",col.names = NA,row.names = T,quote = F)
          }
        }
    )
    
    output$exp_download <- downloadHandler(
      filename = function() {
        paste0(input$exp_export_name,".txt")
      },
      content = function(file) {
        genes_export <- genesets$sets[[input$exp_export]]$genes()
        write.table(as.matrix(dataset$exp[dataset$subset,genes_export,drop=F]),file,sep="\t",col.names = NA,row.names = T,quote = F)
      }
    )
    
    mona_startup <- reactiveVal(mona_dir)
    data_startup <- reactiveVal(data_dir)
    load_startup <- reactiveVal(load_data)
    save_startup <- reactiveVal(save_data)
    dataset_choices <- reactiveVal(NULL)
    
    observeEvent(load_startup(), {
      plots_list$plots[["plot1"]] <- plotServer("plot1",num_plots,plot_remove,cur_selection,selection_list,genesets,plot_settings,dataset,marker_subset,deg_subset)
      shinyjs::show("data_avail")
      shinyjs::show("data_export")
      if (load_startup()) {
        shinyjs::show("data_new")
      }
    })
    
    observeEvent(save_startup(), {
      if (save_startup()) {
        shinyjs::show("data_save")
        shinyjs::show("session_save")
      }
    })
    
    observeEvent(mona_startup(), {
      load_dir(mona_startup())
      mona_obj(qread(file.path(load_dir(),"mona.qs")))
      mona_files <- list.files(load_dir())
      if (sum(c("mona.qs","exp","ranks") %in% mona_files) == 3) {
        password_check()
      } else {
        showNotification("Not a valid Mona directory", type = "message")
      }
    })
    
    observeEvent(data_startup(), {
      if(is.null(data_startup())) {
        dataset_dirs(list.dirs(system.file("examples",package="Mona"),recursive = F))
      } else {
        dataset_dirs(list.dirs(data_startup(),recursive = F))
      }
    },ignoreNULL = F)
    
    observeEvent(dataset_dirs(), {
      choices <- lapply(1:length(dataset_dirs()), function(x) {
        mona <- qread(file.path(dataset_dirs()[x],"mona.qs"))
        accordionItem(
          title = mona$info$name,
          status = "lightblue",
          collapsed = T,
          p(mona$info$description),
          shiny::actionButton(paste0("load",x), "Load data",style="background-color: #fcfcff;",class="dataset_load"),
        )
      })
      dataset_choices(choices)
    })
    
    observeEvent(input$data_new, {
      file_info <- input$data_new
      if(is.list(file_info[[1]])) {
        data_path <- paste(file_info$path,collapse = "/")
        load_dir(paste0(root,data_path))
        mona_obj(qread(file.path(load_dir(),"mona.qs")))
        mona_files <- list.files(load_dir())
        if (sum(c("mona.qs","exp","ranks") %in% mona_files) == 3) {
          password_check()
        } else {
          showNotification("Not a valid Mona directory", type = "message")
        }
      }
    })
    
    observeEvent(input$data_save, {
      if (!is.null(dataset$exp)) {
        mona <- list(meta=dataset$meta,reduct=dataset$reduct,sets=dataset$sets,info=dataset$info,markers=dataset$markers)
        qsave(mona, file.path(save_dir(),"mona.qs"))
        showNotification("Dataset saved!", type = "message")
      }
    })
    
    observeEvent(input$session_save, {
      if (!is.null(dataset$exp)) {
        session_data <- list(plot_settings$point_size,plot_settings$point_transparent,plot_settings$cellname,plot_settings$scroll,input$color_scale_1,input$color_scale_2,input$color_scale_3,lapply(genesets$sets,function(x) x$name()),lapply(genesets$sets,function(x) x$genes()))
        qsave(session_data,file.path(save_dir(),"session.qs"))
        showNotification("Session saved!", type = "message")
      }
    })
    
    downsample_amount <- debounce(reactive(input$downsample),300)
    
    observeEvent(downsample_amount(), {
      if (!is.null(dataset$exp)) {
        downsample_data()
      }},ignoreInit = T)
    
    
    # gprofiler2 API
    #------------------------------
    
    output$gene_search <- renderUI({
      gene <- input$searched_gene
      if (isTruthy(gene)) {
        if (gene %in% dataset$genes) {
          url <- paste0('https://mygene.info/v3/query?q=symbol%3A',gene,'&fields=symbol%2Cname%2Calias%2Csummary&species=',dataset$info$species,'&size=1&from=0&fetch_all=false&facet_size=10&entrezonly=false&ensemblonly=false&dotfield=false')
        } else {
          url <- paste0('https://mygene.info/v3/query?q="',gsub(" ","+",gene),'"&fields=symbol%2Cname%2Calias%2Csummary&species=',dataset$info$species,'&size=1&from=0&fetch_all=false&facet_size=10&entrezonly=false&ensemblonly=false&dotfield=false')
        }
        results <- jsonlite::fromJSON(url,flatten=T)
        results <- results$hits
        if (length(results) > 0) {
          aliases <- results$alias[[1]]
          gene_name <- results$name
          gene_desc <- results$summary
          div(
            h6(if(isTruthy(aliases)) paste(aliases,collapse = ", ") else "No aliases", style="padding: 4px;"),
            h6(if(isTruthy(gene_name)) gene_name else "No name", style="padding: 4px;"),
            p(if(isTruthy(gene_desc)) gene_desc else "No description",style = "font-size: 12px; padding: 4px;")
          )
        } else {
          h5("Gene not found", style="padding: 4px;")
        }
      } else {
        h5("")
      }
    })
    
    # msigdbr
    #-----------------------------------
    msigdb_search <- reactiveVal()
    msigdb_genes <- reactiveVal()
    
    observeEvent(input$set_cat, {
      validate(
        need(dataset$exp,"")
      )
      cat_key <- msigdbr_collections()
      subcats <- cat_key %>% filter(gs_cat == input$set_cat) %>% pull(gs_subcat)
      if (length(subcats) == 1) subcats <- c("NA"=input$set_cat)
      updateSelectizeInput(session,"set_name",choices = c(),server=T)
      updateSelectizeInput(session,"set_subcat",choices = subcats,selected=if(length(subcats) == 1) NULL else character(0))
    })
    
    observeEvent(input$set_subcat, {
      validate(
        need(dataset$exp,""),
        need(input$set_cat,""),
        need(input$set_subcat != "","")
      )
      species <- switch(dataset$info$species,"human"="Homo sapiens","mouse"="Mus musculus","rat"="Rattus norvegicus","fruitfly"="Drosophila melanogaster","zebrafish"="Danio rerio","nematode"="Caenorhabditis elegans","pig"="Sus scrofa","frog"="Xenopus tropicalis")
      subcat <- if (input$set_subcat == input$set_cat) "" else input$set_subcat
      msigdb_search(msigdbr(species = species, category = input$set_cat, subcategory = subcat) %>% select(gs_name,gs_description,gene_symbol))
      sets <- gtools::mixedsort(funique(msigdb_search()$gs_name))
      updateSelectizeInput(session,"set_name",choices = sets,selected=character(0),server=T)
    })
    
    output$set_desc <- renderUI({
      if (isTruthy(input$set_name)) {
        subset <- msigdb_search() %>% filter(gs_name == input$set_name)
        description <- subset$gs_description[1]
        if (description == "") description <- "No description"
        p(description,style = "font-size: 14px; padding: 4px;")
      } else{
        div()
      }
    })
    
    output$set_totals <- renderUI({
      if (isTruthy(input$set_name)) {
        subset <- msigdb_search() %>% filter(gs_name == input$set_name)
        genes <- funique(subset$gene_symbol)
        div(
          p(paste0(length(genes)," genes total"),style="text-align:center; margin-bottom: 0.5rem;"),
          p(paste0(sum(genes %in% dataset$genes), " found in dataset"),style="text-align:center; margin-bottom: 0.5rem;")
        )
      } else{
        div()
      }
    })
    
    observeEvent(input$copy_set, {
      if (isTruthy(input$set_name)) {
        genes <- msigdb_search() %>% filter(gs_name == input$set_name) %>% distinct(gene_symbol) %>% pull(gene_symbol)
        genes <- genes[genes %in% dataset$genes]
        set_id(set_id() + 1)
        id <- paste0("geneset",set_id())
        insertUI(
          selector = '#sets',
          where = "beforeEnd",
          ui = genesUI(id)
        )
        genesets$sets[[id]] <- genesServer(id,genesets,dataset,genes=genes,name=input$set_name,upload=geneset_upload)
        showNotification("Gene set copied!", type = "message")
      }
    })
    
    
    # Plotting 
    #-----------------------------------
    
    num_plots <- reactiveVal(0)
    plots_list <- reactiveValues(plots=list())
    plot_remove <- reactiveVal(NULL)
    plot_id <- reactiveVal(1)
    plot_order <- reactiveVal(NULL)
    plot_settings <- reactiveValues(point_size=7,point_transparent=1.0,cellname=F,scroll=F,color_discrete="classic",color_cont="viridis",color_scaled="blue-red")
    
    
    plot_split_setup <- '
        $(function() {
          $("body").on("mousedown", ".selectize-dropdown-content", function(e){
            e.preventDefault(); 
            return false;
          }); 
          $("body").on("click", ".optgroup-header", function(){
            $(this).siblings().toggle();
          });
        });'
    
    shinyjs::runjs(plot_split_setup)
    
    plot_calculating <- "
      $(document).on('shiny:outputinvalidated', function(event) {
        var id = event.target.id
        if (id === undefined) {
          return;
        } else {
          var id_string = id.toString();
          if (id_string.includes('-plot')) {
            var ns = id_string.split('-')[0];
            document.getElementById(ns + '-' + 'slider_div').style.display = 'none';
            document.getElementById(ns + '-' + 'color_div').style.display = 'none';
          }
        } 
      });"
    
    shinyjs::runjs(plot_calculating)

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
    
    add_new_plot <- function() {
      plot_id(plot_id() + 1)
      id <- paste0("plot",plot_id())
      insertUI(
        selector = '#plot_flex',
        where = "beforeEnd",
        ui = plotUI(id)
      )
      plots_list$plots[[id]] <- plotServer(id,num_plots,plot_remove,cur_selection,selection_list,genesets,plot_settings,dataset,marker_subset,deg_subset)
    }
    
    observeEvent(input$new_plot, {
      if(num_plots() < 8) {
        showNotification("Adding plot...", type = "message")
        add_new_plot()
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
      plot_settings$point_size <- switch(input$point_size, "Small"=5, "Medium"=7, "Large"=9)
    })
    
    observeEvent(input$point_transparent, {
      if(input$point_transparent) {
        plot_settings$point_transparent <- 0.4
      } else {
        plot_settings$point_transparent <- 1.0
      }
    })
    
    observeEvent(input$cellname, {
        plot_settings$cellname <- input$cellname
    })
    
    observeEvent(input$scroll, {
      plot_settings$scroll <- input$scroll
    })
    
    observeEvent(input$color_scale_1, {
      plot_settings$color_discrete <- input$color_scale_1
    })
    
    observeEvent(input$color_scale_2, {
      plot_settings$color_cont <- switch(input$color_scale_2, "viridis"="viridis", "plasma"="plasma", "mona"=colorRamp(colors=c("#D9D9D9","#00008B","#63B8FF","#00FFFF")))
    })
    
    observeEvent(input$color_scale_3, {
      plot_settings$color_scaled <- switch(input$color_scale_3, "blue-red"=colorRamp(colors=c("#1C86EE","#FCFCFC","#FF3030")),"purple-yellow"=colorRamp(colors=c("#BF3EFF","#333333","#F1F708")),"viridis"="viridis", "plasma"="plasma", "mona"=colorRamp(colors=c("#D9D9D9","#00008B","#63B8FF","#00FFFF")))
    })
    
    #---------------------
    # Annotation
    
    observeEvent(input$anno_select, {
      if (isTruthy(input$anno_select)) {
        meta <- funique(dataset$meta[[input$anno_select]])
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
      if (isTruthy(input$cluster_select)) {
        shinyjs::runjs("$('#marker_table').css('visibility','visible');")
        shinyjs::runjs("$('#marker_controls').css('visibility','visible');")
        markers <- dataset$markers
        markers <- subset(markers,metadata==input$anno_select & cluster==input$cluster_select)
        if (nrow(markers) > 0) {
          if (nrow(markers) == 1 && markers$gene == "none") {
            marker_mode("none")
            cur_markers(NULL)
          } else {
            markers <- markers[,c("gene","avg_log2FC","p_val_adj","avg.1","avg.2")]
            colnames(markers) <- c("gene","log2FC","p-val","avg.1","avg.2")
            shinycssloaders::showSpinner("marker_table")
            marker_mode("show")
            cur_markers(markers)
          }
        } else if (nrow(markers) == 0 && fnunique(dataset$meta[[input$anno_select]]) > 1){
          marker_mode("new")
          cur_markers(NULL)
        }
      } else {
        marker_mode(NULL)
        cur_markers(NULL)
      }
    },ignoreInit = T)
    
    observeEvent(input$new_anno, {
      if (!isTruthy(dataset$exp)) {
        return()
      }
      showModal(modalDialog(
        title = "New annotation",
        easyClose = T,
        size="m",
        fluidRow(
          column(
            width=6,
            selectizeInput("copy_anno",label="Use as template",choices=c("None",dataset$anno)),
            textInput("new_anno_name",label="Name",value=""),
            shiny::actionButton("new_anno_confirm", "Create",style="background-color: #fcfcff;")
          ),
          column(
            width=6,
            style='padding-left:8px; border-left: 1px solid;',
            selectizeInput("merge_anno",
             label = "Merge two annotations",
             choices = dataset$anno,
             selected = NULL,
             multiple=T,
             options=list(maxItems=2,plugins = list('remove_button'))
            ),
            textInput("merge_anno_name",label="Name",value=""),
            shiny::actionButton("merge_anno_confirm", "Merge",style="background-color: #fcfcff;")
          )
        ),
        footer = NULL
      ))
    })
    
    observeEvent(input$new_anno_confirm, {
      removeModal(session)
      validate(
        need(input$new_anno_name,"")
      )
      if (!(input$new_anno_name %in% colnames(dataset$meta))) {
        if (input$copy_anno == "None") {
          dataset$meta[[input$new_anno_name]] <- rep("Undefined",nrow(dataset$meta))
        } else {
          dataset$meta[[input$new_anno_name]] <- dataset$meta[[input$copy_anno]]
          markers_all <- dataset$markers
          markers <- subset(markers_all,metadata==input$copy_anno)
          markers$metadata <- input$new_anno_name
          dataset$markers <- rbind(markers_all,markers)
        }
        cur_anno <- input$anno_select
        update_anno_names()
        updateVirtualSelect(
          inputId = "anno_select",
          choices = c(dataset$anno),
          selected = cur_anno
        )
      }
    })
    
    observeEvent(input$merge_anno_confirm, {
      removeModal(session)
      validate(
        need(input$merge_anno,""),
        need(length(input$merge_anno) >= 2,""),
        need(input$merge_anno_name,"")
      )
      selected <- input$merge_anno
      anno_1 <- as.character(dataset$meta[[selected[[1]]]])
      anno_2 <- as.character(dataset$meta[[selected[[2]]]])
      anno_final <- paste(anno_1,anno_2,sep="-")
      dataset$meta[[input$merge_anno_name]] <- anno_final
      cur_anno <- input$anno_select
      update_anno_names()
      updateVirtualSelect(
        inputId = "anno_select",
        choices = c(dataset$anno),
        selected = cur_anno
      )
    })
    
    observeEvent(input$remove_anno, {
      if (!isTruthy(dataset$exp) | !isTruthy(input$anno_select)) {
        return()
      }
      showModal(modalDialog(
        title = "Remove annotation?",
        easyClose = T,
        size="s",
        paste0("You are about to remove '", input$anno_select,"'"),
        br(),br(),
        shiny::actionButton("remove_anno_confirm", "Confirm",style="background-color: #fcfcff;"),
        footer = NULL
      ))
    })
    
    observeEvent(input$remove_anno_confirm, {
      removeModal(session)
      dataset$meta[[input$anno_select]] <- NULL
      markers <- dataset$markers
      dataset$markers <- markers[markers$metadata != input$anno_select,]
      update_anno_names()
      updateVirtualSelect(
        inputId = "anno_select",
        choices = c(dataset$anno),
        selected = NULL
      )
      updateVirtualSelect(
        inputId = "cluster_select",
        choices = c(""),
        selected = NULL
      )
    })
    
    observeEvent(input$rename_anno, {
      if (!isTruthy(dataset$exp) | !isTruthy(input$anno_select)) {
        return()
      }
      showModal(modalDialog(
        title = "Edit annotation",
        easyClose = T,
        size="m",
        fluidRow(
          column(
            width=6,
            style='padding-right:14px;',
            HTML(paste0("<b>Old name:</b> <br>",input$anno_select)),
            br(),br(),
            textInput("rename_anno_name",label="New name:",value=""),
            shiny::actionButton("rename_anno_confirm", "Rename",style="background-color: #fcfcff;")
          ),
          column(
            width=6,
            style='padding-left:8px; border-left: 1px solid;',
            selectizeInput("fill_anno",
             label = "Fill missing values with",
             choices = dataset$anno,
             selected = NULL
            ),
            br(),
            shiny::actionButton("fill_anno_confirm", "Fill in",style="background-color: #fcfcff;")
          )
        ),
        footer = NULL
      ))
    })
    
    observeEvent(input$rename_anno_confirm, {
      removeModal(session)
      validate(
        need(input$rename_anno_name,"")
      )
      if (!(input$rename_anno_name %in% colnames(dataset$meta))) {
        dataset$meta[[input$rename_anno_name]] <- dataset$meta[[input$anno_select]]
        dataset$meta[[input$anno_select]] <- NULL
        markers_meta <- dataset$markers$metadata
        markers_meta[markers_meta == input$anno_select] <- input$rename_anno_name
        dataset$markers$metadata <- markers_meta
        update_anno_names()
        updateVirtualSelect(
          inputId = "anno_select",
          choices = c(dataset$anno),
          selected = input$rename_anno_name
        )
      }
    })
    
    observeEvent(input$fill_anno_confirm, {
      removeModal(session)
      validate(
        need(input$fill_anno,"")
      )
      anno_1 <- as.character(dataset$meta[[input$anno_select]])
      anno_2 <- as.character(dataset$meta[[input$fill_anno]])
      anno_final <- anno_1
      anno_final[is.na(anno_1)] <- anno_2[is.na(anno_1)]
      anno_final[anno_1 == "Undefined"] <- anno_2[anno_1 == "Undefined"]
      dataset$meta[[input$anno_select]] <- anno_final
      cur_anno <- input$anno_select
      update_anno_names()
      updateVirtualSelect(
        inputId = "anno_select",
        choices = c(dataset$anno),
        selected = cur_anno
      )
      groups <- gtools::mixedsort(funique(anno_final))
      updateVirtualSelect(
        inputId = "cluster_select",
        choices = groups,
        selected = NULL
      )
    })
    
    observeEvent(input$new_cluster, {
      if (!isTruthy(dataset$exp) | !isTruthy(cur_selection$cells) | !isTruthy(input$anno_select)) {
        return()
      }
      showModal(modalDialog(
        title = "New group",
        easyClose = T,
        size="s",
        textInput("new_cluster_name",label="Name",value=""),
        shiny::actionButton("new_cluster_confirm", "Create",style="background-color: #fcfcff;"),
        footer = NULL
      ))
    })
    
    observeEvent(input$new_cluster_confirm, {
      removeModal(session)
      validate(
        need(input$new_cluster_name,"")
      )
      clusters <- dataset$meta[input$anno_select]
      clusters_old <- clusters[,1]
      filter <- rownames(clusters) %in% cur_selection$cells
      selected_clusters <- as.vector(funique(clusters[filter,1]))
      clusters <- as.vector(clusters[,1])
      clusters[filter] <- input$new_cluster_name
      dataset$meta[[input$anno_select]] <- clusters
      if (input$new_cluster_name %in% funique(clusters_old)){
        markers <- dataset$markers
        filter <- (markers$metadata == input$anno_select & markers$cluster == input$new_cluster_name)
        dataset$markers <- markers[!filter,]
      }
      markers <- dataset$markers
      filter <- (markers$metadata == input$anno_select & markers$cluster %in% selected_clusters)
      dataset$markers <- markers[!filter,]
      groups <- gtools::mixedsort(funique(clusters))
      updateVirtualSelect(
        inputId = "cluster_select",
        choices = groups,
        selected = NULL
      )
    })
    
    observeEvent(input$remove_cluster, {
      if (!isTruthy(dataset$exp) | !isTruthy(input$cluster_select)) {
        return()
      }
      showModal(modalDialog(
        title = "Remove group?",
        easyClose = T,
        size="s",
        paste0("You are about to remove '", input$cluster_select,"'"),
        br(),br(),
        shiny::actionButton("remove_cluster_confirm", "Confirm",style="background-color: #fcfcff;"),
        footer = NULL
      ))
    })
    
    observeEvent(input$remove_cluster_confirm, {
      removeModal(session)
      clusters <- dataset$meta[[input$anno_select]]
      clusters <- as.vector(clusters)
      filter <- clusters == input$cluster_select
      clusters[filter] <- "Undefined"
      dataset$meta[[input$anno_select]] <- clusters
      markers <- dataset$markers
      filter <- (markers$metadata == input$anno_select & markers$cluster == input$cluster_select)
      dataset$markers <- markers[!filter,]
      groups <- gtools::mixedsort(funique(clusters))
      updateVirtualSelect(
        inputId = "cluster_select",
        choices = groups,
        selected = NULL
      )
    })
    
    observeEvent(input$rename_cluster, {
      if (!isTruthy(dataset$exp) | !isTruthy(input$cluster_select)) {
        return()
      }
      showModal(modalDialog(
        title = "Edit group",
        easyClose = T,
        size="s",
        HTML(paste0("<b>Old name:</b> <br>",input$cluster_select)),
        br(),br(),
        textInput("rename_cluster_name",label="New name:",value=""),
        shiny::actionButton("rename_cluster_confirm", "Rename",style="background-color: #fcfcff;"),
        footer = NULL
      ))
    })
    
    observeEvent(input$rename_cluster_confirm, {
      removeModal(session)
      validate(
        need(input$rename_cluster_name,"")
      )
      clusters <- dataset$meta[[input$anno_select]]
      clusters_old <- as.vector(clusters)
      clusters_new <- clusters_old
      filter <- clusters_new == input$cluster_select
      clusters_new[filter] <- input$rename_cluster_name
      dataset$meta[[input$anno_select]] <- clusters_new
      if (input$rename_cluster_name %in% funique(clusters_old)){
        markers <- dataset$markers
        filter <- (markers$metadata == input$anno_select & markers$cluster == input$rename_cluster_name)
        dataset$markers <- markers[!filter,]
      } else {
        markers_meta <- dataset$markers$metadata
        markers_cluster <- dataset$markers$cluster
        filter <- (markers_meta == input$anno_select & markers_cluster == input$cluster_select)
        markers_cluster[filter] <- input$rename_cluster_name
        dataset$markers$cluster <- markers_cluster
      }
      groups <- gtools::mixedsort(funique(clusters_new))
      updateVirtualSelect(
        inputId = "cluster_select",
        choices = groups,
        selected = NULL
      )
    })
    
    #-----------------------
    # Genes
    
    genesets = reactiveValues(sets=list())
    geneset_upload = reactiveValues(genes=NULL,names=NULL)
    
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
      genesets$sets[[id]] <- genesServer(id,genesets,dataset,upload=geneset_upload)
    })
    
    observeEvent(geneset_upload$genes, {
      set_genes <- geneset_upload$genes
      set_names <- geneset_upload$names
      num_sets <- length(set_names)
      for (x in 1:num_sets) {
        set_id(set_id() + 1)
        id <- paste0("geneset",set_id())
        insertUI(
          selector = '#sets',
          where = "beforeEnd",
          ui = genesUI(id)
        )
        genes <- set_genes[[x]] %>% unique() %>% stringi::stri_remove_empty_na()
        genes <- genes[genes %in% dataset$genes]
        name <- set_names[[x]]
        genesets$sets[[id]] <- genesServer(id,genesets,dataset,genes=genes,name=name,upload=geneset_upload)
      }
      geneset_upload$names <- NULL
      geneset_upload$genes <- NULL
    },ignoreNULL = T)
    
    cur_markers <- reactiveVal(NULL)
    marker_subset <- reactiveVal(NULL)
    cur_terms <- reactiveVal(NULL)
    terms_subset <- reactiveVal(NULL)
    de_cells_1 <- reactiveValues(name=NULL,cells=NULL)
    de_cells_2 <- reactiveValues(name=NULL,cells=NULL)
    cur_degs <- reactiveVal(NULL)
    deg_subset <- reactiveVal(NULL)
    go_type <- reactiveVal(NULL)
    deg_process <- reactiveVal(NULL)
    
    marker_modes_all <- c("show","none","new")
    
    marker_mode <- function(type) {
      lapply(marker_modes_all, function(x) shinyjs::hide(paste0("markers_",x)))
      if (!is.null(type)) {
        shinyjs::show(paste0("markers_",type))
      }
    }
    
    deg_modes_all <- c("show","none","new")
    
    deg_mode <- function(type) {
      lapply(deg_modes_all, function(x) shinyjs::hide(paste0("deg_",x)))
      if (!is.null(type)) {
        shinyjs::show(paste0("deg_",type))
      }
    }
    
    go_modes_all <- c("show","none","select","species")
    
    go_mode <- function(type) {
      lapply(go_modes_all, function(x) shinyjs::hide(paste0("go_",x)))
      if (!is.null(type)) {
        shinyjs::show(paste0("go_",type))
      }
    }
    
    shinyjs::hide("gene_div")
    shinyjs::hide("fc_div")
    shinyjs::hide("pval_div")
    shinyjs::hide("gene_div_deg")
    shinyjs::hide("fc_div_deg")
    shinyjs::hide("pval_div_deg")
    shinyjs::hide("id_div")
    shinyjs::hide("name_div")
    shinyjs::hide("pval_div_go")
    
    prepare_terms <- function() {
      validate(
        need(dataset$exp,""),
        need(input$go_genes,"")
      )
      cur_terms(NULL)
      terms_subset(NULL)
      if (input$go_genes == "Markers") {
        genes <- marker_subset()$gene
      } else if (input$go_genes == "DEGs") {
        genes <- deg_subset()$gene
      } else {
        geneset <- match(input$go_genes,set_names())
        genes <- genesets$sets[[geneset]]$genes()
      }
      species_check <- dataset$info$species %in% species_all
      if (isTruthy(genes) && length(genes) > 0 && species_check) {
        shinyjs::hide("go_controls")
        shinycssloaders::showSpinner("go_table")
        go_mode("show")
        shinyjs::enable("save_go")
        shinyjs::runjs("$('#go_controls').css('margin-top','0.0vh')")
        cur_terms(get_go_terms(genes))
        shinyjs::show("go_controls")
      } else {
        if (species_check) go_mode(NULL) else go_mode("species")
        shinyjs::disable("save_go")
        shinyjs::runjs("$('#go_controls').css('margin-top','27.0vh')")
        cur_terms(NULL)
        shinyjs::show("go_controls")      }
    }

    de_opts_change <- function() {
      deg_mode("new")
    }
    
    observeEvent(input$de_opts_1,{
      type <- input$de_opts_1
      updateRadioGroupButtons(session,"de_opts_1",selected=character(0))
      if (type == "Group" && isTruthy(input$cluster_select)) {
        meta_subset <- dataset$meta[dataset$subset,input$anno_select,drop=F]
        de_cells_1$cells = rownames(meta_subset[meta_subset == input$cluster_select,,drop=F])
        de_cells_1$name = paste0(input$anno_select," - ",input$cluster_select)
        de_opts_change()
      } else if (type == "Select" && isTruthy(cur_selection$cells)) {
        de_cells_1$cells = cur_selection$cells
        de_cells_1$name = paste0(length(de_cells_1$cells)," cells")
        de_opts_change()
      }
      shinyjs::click("de_button_1")
    },ignoreInit = T,ignoreNULL = T)
    
    observeEvent(input$de_opts_2,{
      type <- input$de_opts_2
      updateRadioGroupButtons(session,"de_opts_2",selected=character(0))
      if (type == "Group" && isTruthy(input$cluster_select)) {
        meta_subset <- dataset$meta[dataset$subset,input$anno_select,drop=F]
        de_cells_2$cells = rownames(meta_subset[meta_subset == input$cluster_select,,drop=F])
        de_cells_2$name = paste0(input$anno_select," - ",input$cluster_select)
        de_opts_change()
      } else if (type == "Select" && isTruthy(cur_selection$cells)) {
        de_cells_2$cells = cur_selection$cells
        de_cells_2$name = paste0(length(de_cells_2$cells)," cells")
        de_opts_change()
      } else if (type == "Rest" && isTruthy(de_cells_1$cells)) {
        meta_subset <- dataset$meta[dataset$subset,1,drop=F]
        de_cells_2$cells = rownames(meta_subset[!(rownames(meta_subset) %in% de_cells_1$cells),,drop=F])
        de_cells_2$name = "Rest"
        de_opts_change()
      }
      shinyjs::click("de_button_2")
    },ignoreInit = T,ignoreNULL = T)
    
    output$de_cells_1_text <- renderUI({
      de_cells_1$name
    })
    
    output$de_cells_2_text <- renderUI({
      de_cells_2$name
    })
    
    get_new_markers <- function(anno=NULL,group=NULL) {
      markers <- markers_mona(dataset$exp,dataset$meta,anno=anno,group=group)
      if (is.null(markers)) {
        marker_mode("none")
        return(NULL)
      }
      markers <- markers %>% arrange(p_val_adj) %>% slice(1:500)
      markers$gene <- rownames(markers)
      markers$avg_log2FC <- signif(markers$avg_log2FC,3)
      markers$p_val_adj <- formatC(markers$p_val_adj, format = "e", digits = 2)
      if (nrow(markers) > 0) {
        markers$cluster <- as.character(group)
        markers$metadata <- as.character(anno)
        markers <- markers[,c("gene","cluster","metadata","avg_log2FC","p_val_adj","avg.1","avg.2")]
        markers_all <- dataset$markers
        dataset$markers <- rbind(markers_all,markers)
        markers <- markers[,c("gene","avg_log2FC","p_val_adj","avg.1","avg.2")]
        colnames(markers) <- c("gene","log2FC","p-val","avg.1","avg.2")
        marker_mode("show")
        return(markers)
      } else {
        marker_mode("none")
        return(NULL)
      }
    }
    
    get_deg <- function() {
      markers <- markers_mona(dataset$exp,dataset$meta,cells.1=de_cells_1$cells,cells.2=de_cells_2$cells)
      if (is.null(markers)) {
        deg_mode("none")
        return(NULL)
      }
      markers <- markers %>% arrange(p_val_adj) %>% slice(1:500)
      markers$gene <- rownames(markers)
      markers$avg_log2FC <- signif(markers$avg_log2FC,3)
      markers$p_val_adj <- formatC(markers$p_val_adj, format = "e", digits = 2)
      markers <- markers[,c("gene","avg_log2FC","p_val_adj","avg.1","avg.2")]
      colnames(markers) <- c("gene","log2FC","p-val","avg.1","avg.2")
      if (nrow(markers) > 0) {
        deg_mode("show")
        shinyjs::show("deg_controls")
        return(markers)
      } else {
        deg_mode("none")
        return(NULL)
      }
    }
    
    fc_bg <- function (data,color1="rgba(185, 197, 253, 0.35)",color2="rgba(255, 0, 0, 0.15)") 
    {
      max_val=max(abs(data))
      JS(sprintf("isNaN(parseFloat(value)) || value < 0 ? 'linear-gradient(90deg, %s ' + -value/%f * 100 + '%%, transparent ' + -value/%f * 100 + '%%)': 'linear-gradient(90deg, %s ' + value/%f * 100 + '%%, transparent ' + value/%f * 100 + '%%)'",
                 color1,max_val,max_val,color2,max_val,max_val))
    } 
    
    generate_marker_table <- function() {
      genes <- marker_subset()
      Sys.sleep(0.25)
      shinycssloaders::hideSpinner("marker_table")
      if (isTruthy(genes)) {
        suppressWarnings(DT::datatable(
          genes,
          extensions = c("Buttons"),
          options = list(dom="t",pageLength=10,scrollY="23.3vh",scrollCollapse=T,paging=F,autoWidth=F,scrollX=T,language = list(emptyTable="No markers match filters"),columnDefs = list(list(targets = c(3,4), visible=F),list(targets = "_all", width = "33%"),list(className = 'dt-left', targets = "_all"))),
          rownames= FALSE,
          selection = list(target = 'column',mode='single'),
          class = "compact"
        ) %>% DT::formatStyle(columns = c("gene","log2FC","p-val"), fontSize = '1.75vh', lineHeight="70%") %>% 
          DT::formatStyle("log2FC",background=fc_bg(genes[,"log2FC"]),backgroundSize = '98% 75%',backgroundRepeat = 'no-repeat',backgroundPosition = 'center')
        )
      }
    }
    
    generate_deg_table <- function() {
      genes <- deg_subset()
      Sys.sleep(0.25)
      shinycssloaders::hideSpinner("deg_table")
      if (isTruthy(genes)) {
        suppressWarnings(DT::datatable(
          genes,
          extensions = c("Buttons"),
          options = list(dom="t", pageLength=10,scrollY="23.3vh",scrollCollapse=T,paging=F,autoWidth=F,scrollX=T,language = list(emptyTable="No DEGs match filters"),columnDefs = list(list(targets = c(3,4), visible=F),list(targets = "_all", width = "33%"),list(className = 'dt-left', targets = "_all"))),
          rownames= FALSE,
          selection = list(target = 'column',mode='single'),
          class = "compact"
        ) %>% DT::formatStyle(columns = c("gene","log2FC","p-val"), fontSize = '1.75vh', lineHeight="70%") %>%
          DT::formatStyle("log2FC",background=fc_bg(genes[,"log2FC"]),backgroundSize = '98% 75%',backgroundRepeat = 'no-repeat',backgroundPosition = 'center')
        )
      }
    }
    
    output$marker_table <- DT::renderDT(generate_marker_table(), server = FALSE)
    
    output$deg_table <- DT::renderDT(generate_deg_table(), server = FALSE)
    
    observeEvent(marker_subset(), {
      if (!is.null(go_type()) && go_type() == "Markers" && isTruthy(marker_subset())) {
        prepare_terms()
      }
    })
    
    observeEvent(deg_subset(), {
      if (!is.null(go_type()) && go_type() == "DEGs" && isTruthy(deg_subset())) {
        prepare_terms()
      }
    })
    
    observeEvent(input$markers_find, {
      shinycssloaders::showSpinner("marker_table")
      marker_mode("show")
      cur_markers(get_new_markers(anno=input$anno_select,group=input$cluster_select))
    })
    
    observeEvent(input$deg_find, {
      if (isTruthy(de_cells_1$cells) && isTruthy(de_cells_2$cells)) {
        if (sum(de_cells_1$cells %in% de_cells_2$cells) > 0) {
          showNotification("DEG groups overlap!", type = "message")
        } else {
          shinycssloaders::showSpinner("deg_table")
          deg_mode("show")
          cur_degs(get_deg())
        }
      }
    })
    
    observeEvent(input$deg_return, {
      if (!is.null(cur_degs())) {
        deg_mode("show")
      }
    })
    
    observeEvent(input$go_choose, {
      go_mode("select")
      shinyjs::hide("go_controls")
    })
    
    observeEvent(input$go_return, {
      if (!is.null(cur_terms())) {
        go_mode("show")
        shinyjs::show("go_controls")
      }
    })
    
    observeEvent(input$go_confirm, {
      if (input$go_genes == "Markers" && is.null(marker_subset())) {
        showNotification("No markers available!", type = "message")
      } else if (input$go_genes == "DEGs" && is.null(deg_subset())) {
        showNotification("No DEGs available!", type = "message")
      } else {
        go_type(input$go_genes)
        prepare_terms()
      }
    })
    
    set_names <- reactive(unlist(sapply(genesets$sets,function(x) x$name())))
    
    observeEvent(set_names(), {
      options <- c("Markers","DEGs",unname(set_names()))
      updateSelectizeInput(session,"go_genes",choices=options,selected = NULL)
    },ignoreNULL = F)
    
    
    # Based on the gprofiler2 package, requires internet connection as this is not pre-calculated
    get_go_terms <- function(genes) {
      species <- switch(dataset$info$species,"human"="hsapiens","mouse"="mmusculus","rat"="rnorvegicus","fruitfly"="dmelanogaster","zebrafish"="drerio","nematode"="celegans","pig"="sscrofa","frog"="xtropicalis")
      gostres <- gost(query = genes, 
                      organism = species, ordered_query = FALSE, 
                      multi_query = FALSE, significant = TRUE, exclude_iea = FALSE, 
                      measure_underrepresentation = FALSE, evcodes = FALSE, 
                      user_threshold = 0.05, correction_method = "g_SCS", 
                      domain_scope = "annotated", custom_bg = NULL, 
                      numeric_ns = "", sources = NULL, as_short_link = FALSE, highlight = TRUE)
      results <- gostres$result
      if (is.null(results)) {
        go_mode("none")
        shinyjs::disable("save_go")
        shinyjs::runjs("$('#go_controls').css('margin-top','8.0vh')")
        return(NULL)
      } else {
        results <- results[,c("term_id","term_name","p_value")]
        colnames(results) <- c("id","name","p-val")
        results <- results[order(results$`p-val`),]
        results$`p-val` <- formatC(results$`p-val`, format = "e", digits = 2)
        shinyjs::runjs("$('#go_controls').css('margin-top','0.0vh')")
        return(results)
      }
    }
    
    generate_go_table <- function() {
      terms <- terms_subset()
      Sys.sleep(0.25)
      shinycssloaders::hideSpinner("go_table")
      if (isTruthy(terms)) {
        suppressWarnings(DT::datatable(
          terms,
          extensions = c("Buttons"),
          options = list(dom="t", pageLength=10,scrollY="23.3vh",scrollCollapse=T,paging=F,autoWidth=F,scrollX=T,language = list(emptyTable="No GO terms match filters"),columnDefs = list(list(targets = c(1), width = "40%"),list(className = 'dt-left', targets = "_all"))),
          rownames= FALSE,
          selection = list(target = 'column',mode='single'),
          class = "compact"
        ) %>% DT::formatStyle(columns = c("id","name","p-val"), fontSize = '1.6vh', lineHeight="85%")
        )
      }
    }
    
    output$go_table <- DT::renderDT(generate_go_table(), server = FALSE)
    
    output$save_markers <-
      downloadHandler(
        filename = function() {
            paste0("markers_",input$anno_select,"_",input$cluster_select,".txt")
        },
        content = function(file) {
          write.table(marker_subset(),file,sep="\t",col.names = T,row.names = T,quote = F)
        }
      )
    
    output$save_deg <-
      downloadHandler(
        filename = function() {
            paste0("DEG.txt")
        },
        content = function(file) {
          write.table(deg_subset(),file,sep="\t",col.names = T,row.names = T,quote = F)
        }
      )
    
    output$save_go <-
      downloadHandler(
        filename = function() {
          if (go_type() == "Markers") {
            paste0("go_",input$anno_select,"_",input$cluster_select,".txt")
          } else if (go_type() == "DEGs") {
            "go_DEG.txt"
          } else {
            paste0("go_",input$go_genes,".txt")
          }
        },
        content = function(file) {
          write.table(terms_subset(),file,sep="\t",col.names = T,row.names = T,quote = F)
        }
      )
    
    gene_filter <- debounce(reactive(paste0("^",input$gene_filter)),300)
    fc_filter_1 <- debounce(reactive(input$fc_filter[1]),300)
    fc_filter_2 <- debounce(reactive(input$fc_filter[2]),300)
    pval_filter <- debounce(reactive(input$pval_filter),300)
    
    observe({
      markers <- cur_markers()
      if(!is.null(markers)) {
          fc_1 <- if (fc_filter_1() == -10) -100 else fc_filter_1()
          fc_2 <- if (fc_filter_2() == 10) 100 else fc_filter_2()
          if (gene_filter() != "^") markers <- filter(markers,grepl(gene_filter(),gene,ignore.case = T))
          markers <- filter(markers, log2FC >= fc_1 & log2FC <= fc_2 & as.numeric(`p-val`) <= pval_filter())
          marker_subset(markers)
      } else {
        marker_subset(NULL)
      }
    })
    
    gene_filter_deg <- debounce(reactive(paste0("^",input$gene_filter_deg)),300)
    fc_filter_1_deg <- debounce(reactive(input$fc_filter_deg[1]),300)
    fc_filter_2_deg <- debounce(reactive(input$fc_filter_deg[2]),300)
    pval_filter_deg <- debounce(reactive(input$pval_filter_deg),300)
    
    observe({
      degs <- cur_degs()
      if(!is.null(degs)) {
        fc_1 <- if (fc_filter_1_deg() == -10) -100 else fc_filter_1_deg()
        fc_2 <- if (fc_filter_2_deg() == 10) 100 else fc_filter_2_deg()
        if (gene_filter_deg() != "^") degs <- filter(degs,grepl(gene_filter_deg(),gene,ignore.case = T))
        degs <- filter(degs, log2FC >= fc_1 & log2FC <= fc_2 & as.numeric(`p-val`) <= pval_filter_deg())
        deg_subset(degs)
      } else {
        deg_subset(NULL)
      }
    })
    
    id_filter <- debounce(reactive(input$id_filter),300)
    name_filter <- debounce(reactive(input$name_filter),300)
    pval_filter_go <- debounce(reactive(input$pval_filter_go),300)
    
    observe({
      terms <- cur_terms()
      if(!is.null(terms)) {
        if (!is.null(id_filter())) terms <- filter(terms,grepl(id_filter(),id,ignore.case = T))
        if (!is.null(name_filter())) terms <- filter(terms,grepl(name_filter(),name,ignore.case = T))
        terms <- filter(terms, as.numeric(`p-val`) <= pval_filter_go())
        terms_subset(terms)
      } else {
        terms_subset(NULL)
      }
    })
    
    observeEvent(input$copy_markers, {
      if (!is.null(marker_subset())) {
        set_id(set_id() + 1)
        id <- paste0("geneset",set_id())
        insertUI(
          selector = '#sets',
          where = "beforeEnd",
          ui = genesUI(id)
        )
        genes <- marker_subset()$gene
        genesets$sets[[id]] <- genesServer(id,genesets,dataset,genes=genes,name=paste0("Markers - ",input$cluster_select),upload=geneset_upload)
        showNotification("Markers copied!", type = "message")
      }
    })
    
    observeEvent(input$copy_deg, {
      if (!is.null(deg_subset())) {
        set_id(set_id() + 1)
        id <- paste0("geneset",set_id())
        insertUI(
          selector = '#sets',
          where = "beforeEnd",
          ui = genesUI(id)
        )
        genes <- deg_subset()$gene
        genesets$sets[[id]] <- genesServer(id,genesets,dataset,genes=genes,name="DEG",upload=geneset_upload)
        showNotification("Markers copied!", type = "message")
      }
    })
    
    observeEvent(input$marker_table_columns_selected, {
      col_select <- input$marker_table_columns_selected
      if (col_select == 0) {
        shinyjs::show("gene_div")
        shinyjs::hide("fc_div")
        shinyjs::hide("pval_div")
      } else if (col_select == 1) {
        shinyjs::hide("gene_div")
        shinyjs::show("fc_div")
        shinyjs::hide("pval_div")
      } else {
        shinyjs::hide("gene_div")
        shinyjs::hide("fc_div")
        shinyjs::show("pval_div")
      }
    })
    
    observeEvent(input$deg_table_columns_selected, {
      col_select <- input$deg_table_columns_selected
      if (col_select == 0) {
        shinyjs::show("gene_div_deg")
        shinyjs::hide("fc_div_deg")
        shinyjs::hide("pval_div_deg")
      } else if (col_select == 1) {
        shinyjs::hide("gene_div_deg")
        shinyjs::show("fc_div_deg")
        shinyjs::hide("pval_div_deg")
      } else {
        shinyjs::hide("gene_div_deg")
        shinyjs::hide("fc_div_deg")
        shinyjs::show("pval_div_deg")
      }
    })
    
    observeEvent(input$go_table_columns_selected, {
      col_select <- input$go_table_columns_selected
      if (col_select == 0) {
        shinyjs::show("id_div")
        shinyjs::hide("name_div")
        shinyjs::hide("pval_div_go")
      } else if (col_select == 1) {
        shinyjs::hide("id_div")
        shinyjs::show("name_div")
        shinyjs::hide("pval_div_go")
      } else {
        shinyjs::hide("id_div")
        shinyjs::hide("name_div")
        shinyjs::show("pval_div_go")
      }
    })
    
    # Selection 
    #------------------------------
    
    selection_list = reactiveValues(selects=list(),counts=list())
    cur_selection <- reactiveValues(plot=NULL,cells=NULL)
    cell_percent <- reactiveVal(100)
    
    observeEvent(cur_selection$cells, {
      selected <- length(cur_selection$cells)
      if (selected > 0) {
        bs4Dash::removePopover(id="cell_text")
        subplot <- strsplit(cur_selection$plot,"@")[[1]][2]
        if (subplot > 0) {
          bs4Dash::addPopover(id="cell_text",options=list(content=HTML(paste0(round(selected/length(dataset$subset),3)*100,"% of total","<br/>",round(selected/selection_list$counts[[cur_selection$plot]],3)*100,"% within plot")),html=T,placement="bottom",delay=500,trigger="hover"))
        } else {
          bs4Dash::addPopover(id="cell_text",options=list(content=HTML(paste0(round(selected/length(dataset$subset),3)*100,"% of total")),html=T,placement="bottom",delay=500,trigger="hover"))
        }
      } else {
        bs4Dash::removePopover(id="cell_text")
      }
    },ignoreNULL = F)

    output$cell_select <- renderUI({
      if(!is.null(cur_selection$cells)) {
        shinyjs::runjs("$('#cell_text').css('color','#96a8fc')")
        paste0(length(cur_selection$cells)," cells selected")
      } else if (!is.null(dataset$exp)){ 
        shinyjs::runjs("$('#cell_text').css('color','#1f2d3d')")
        if (length(dataset$subset) == nrow(dataset$exp)) {
          paste0(length(dataset$subset), " cells total")
        } else {
          paste0(length(dataset$subset), " cells subsetted")
        }
      } else {
        ""
      }
    })
    
    observeEvent(input$plot_clicked, {
      plot_click <- input$plot_clicked
      cells <- selection_list$selects[[plot_click]]
      if (isTruthy(cells) && (is.null(cur_selection$plot) || cur_selection$plot != plot_click)) {
        cur_selection$plot <- plot_click
        cur_selection$cells <- cells
      }
    })
    
    observeEvent(input$subplot_clicked, {
      plot_click <- input$subplot_clicked
      cells <- selection_list$selects[[plot_click]]
      if (isTruthy(cells) && (is.null(cur_selection$plot) || cur_selection$plot != plot_click)) {
        cur_selection$plot <- plot_click
        cur_selection$cells <- cells
      }
    })
    
    observeEvent(input$subset_select, {
      if (length(cur_selection$cells) > 0) {
        showNotification("Subsetting data...", type = "message")
        dataset$subset <- fmatch(cur_selection$cells,rownames(dataset$exp))
        cur_selection$plot <- NULL
        cur_selection$cells <- NULL
      }
    },ignoreInit = T)
    
    observeEvent(input$subset_undo, {
      if (!is.null(dataset$exp)) {
        downsample_data()
      }
    },ignoreInit = T)
    
  }
  
  shinyApp(ui = ui, server = server,options = list(launch.browser=T))
}
