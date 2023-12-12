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
#' @import gprofiler2
#' @rawNamespace import(ggplot2, except = c("vars","last_plot"))
#' @import cowplot
#' @import grid
#' @rawNamespace import(Seurat, except = "JS")
#' @rawNamespace import(SeuratObject, except = c("show","JS"))
#' @import BPCells
#' @param mona_dir A Mona directory, will automatically open at startup
#' @export

mona <- function(mona_dir=NULL) {
  
  options(shiny.maxRequestSize=8000*1024^2)
  set.seed(123)
  dqset.seed(123)
  options(Seurat.object.assay.version = 'v5')
  
  resources <- system.file("www", package = "Mona")
  addResourcePath("www", resources)
  resources <- system.file("images", package = "Mona")
  addResourcePath("images", resources)
  dataset_dirs <- list.dirs(system.file("datasets",package="Mona"),recursive = F)

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
        div(
          style="margin:10px;",
          sliderInput("downsample","Downsample cells",min = 10, max = 100,value = 100, step = 10,post = "%",width="95%"),
          sliderTextInput("point_size","Point size",grid=T,choices=c("Small","Medium","Large"),selected="Medium",width = "95%"),
          p("Transparent points", style = "font-weight: 700;"),
          materialSwitch("point_transparent","",value=F,status="primary"),
          selectizeInput("color_scale_1",label="Discrete color",choices=c("rainbow","random")),
          selectizeInput("color_scale_2",label="Continuous color",choices=c("viridis","plasma","mona")),
          selectizeInput("color_scale_3",label="Scaled color",choices=c("blue-red","purple-yellow","viridis","plasma","mona")),
        )
      ),
      conditionalPanel(
        condition = "output.control_mode == 'search'",
        div(
        id="searched_gene",
        selectizeInput("searched_gene", label = "",choices = NULL,selected=character(0),width="85%",options=list(maxOptions=100,create=T,persist=F)),
        ),
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
                circleButton("new_plot",icon = icon("chart-column"),size = "default", style="margin-bottom: 1vh; margin-top: -5px; background-color: #fcfcff;"),
                #circleButton("annotate_cells",icon = icon("gears"),size = "default", style="margin-bottom: 1vh; margin-top: -5px; background-color: #fcfcff;"),
                circleButton("settings",icon = icon("sliders"),size = "default", style="margin-bottom: 1vh; margin-top: -5px; margin-left: 5px; background-color: #fcfcff;"),
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
                        p("Group is new/modified/not calculated"),
                        shiny::actionButton("markers_find",icon=icon("dna"),label="",width="2.0vw",style="margin-top: 1.2vh; padding: 3px; background-color: #fcfcff;")
                      ),
                      div(
                        id="markers_show",
                        withSpinner(DTOutput("marker_table"),type=5,color="#738bfb"),
                        fluidRow(
                          shiny::column(
                            width=2,
                            downloadButton("save_markers",icon=icon("download"),label="",style="width: 2.0vw; margin-top: 1.2vh; padding: 3px; margin-left: 15px; background-color: #fcfcff;"),
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
                            shiny::actionButton("copy_markers",icon=icon("copy"),label="",width="2.0vw",style="margin-top: 1.2vh; padding: 3px; margin-right: 15px; background-color: #fcfcff;")
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
                        shiny::actionButton("deg_find",icon=icon("dna"),label="",width="2.0vw",style="margin-top: 1.2vh; margin-right: 2px; padding: 3px; background-color: #fcfcff;"),
                        shiny::actionButton("deg_return",icon=icon("arrow-left"),label="",width="2.0vw",style="margin-top: 1.2vh; margin-left: 2px; padding: 3px; background-color: #fcfcff;")
                      ),
                      div(
                        id="deg_show",
                        withSpinner(DTOutput("deg_table"),type=5,color="#738bfb"),
                        fluidRow(
                          shiny::column(
                            width=2,
                            downloadButton("save_deg",icon=icon("download"),label="",style="width: 2.0vw; margin-top: 1.2vh; padding: 3px; margin-left: 15px; background-color: #fcfcff;"),
                          ),
                          shiny::column(
                            width=8,
                            radioGroupButtons(
                              inputId = "fc_filter_deg",
                              label = "",
                              choices = c("Neg","All","Pos"),
                              selected = "All"
                            )
                          ),
                          shiny::column(
                            width=2,
                            shiny::actionButton("copy_deg",icon=icon("copy"),label="",width="2.0vw",style="margin-top: 1.2vh; padding: 3px; margin-right: 15px; background-color: #fcfcff;")
                          )
                        )
                      )
                    ),
                    tabPanel(
                      title="Function",
                      div(
                        id="go_none",
                        p("No GO terms found")
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
                            width=3,
                            offset=1,
                            downloadButton("save_go",icon=icon("download"),label="",style="width: 2.0vw; margin-top: 1.2vh; padding: 3px; margin-left: 15px; background-color: #fcfcff;")
                          ),
                          column(
                            width=7,
                            radioGroupButtons(
                              inputId = "go_toggle",
                              label = "",
                              choices = c("Markers","DEG"),
                              selected = "Markers"
                            )
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
                  collapsed = F,
                  div(img(src = "images/layout.png", height = 250,width = 375),style="text-align: center"),
                  br(),
                  tags$ul(
                    tags$li("The navigation bar at the top is where datasets are loaded and saved. You can also access more information about the current dataset."),
                    tags$li("The plot section holds any plots you create. It is dynamic and can contain up to 8 at once. Plots can also be rearranged or expanded to take up the full screen."),
                    tags$li("The controls area has multiple features including searching for genes, adjusting settings, and most importantly, creating new plots."),
                    tags$li("The cell section is used for viewing cell metadata to either edit them or find markers. Selections made within embeddings also appear here and can be added to the metadata or subsetted."),
                    tags$li("Finally, the gene section is where you can view markers, GO terms associated with those markers, and create custom gene sets to use when generating plots.")
                  )
                ),
                accordionItem(
                  title = "Functions",
                  status = "lightblue",
                  collapsed = T,
                  h5("Plots"),
                  tags$ul(
                  tags$li("The top left button of each plot box opens a dropdown where all settings are located. Along the bottom are 5 different plot types, where you can freely switch between them and see the specific settings for each."),
                  tags$li("When your mouse is over a plot an additional control bar will appear. This contains important tools for zooming, panning, drawing, and selecting."),
                  tags$li("When working with multiple plots, you can click on the top of the box to drag and rearrange them. The 'camera' icon gives you the ability to save static images, while the 'expand' icon lets you view a full-screen version of the plot for a distraction-free experience."),
                  tags$li("Clicking on a group within a legend will hide that group, while double clicking will cause the plot to focus only on that group."),
                  tags$li("All plots have additional information you can see only by hovering. For very large plots, zoom into a specific area to get a better view before hovering."),
                  ),
                  h5("Selection"),
                  tags$ul(
                  tags$li("To focus on a smaller subset of cells, select them using the box/lasso tool (found in the control bar located on the lefthand side) when viewing a 2D embedding.  Please note that this is the only plot type where selection is supported."),
                  tags$li("Once selected, the data can be subset to carry that selection through to all plots, calculate markers for that selection, or simply give it a name within the metadata."),
                  tags$li("It's also possible to combine multiple selections by holding shift while selecting."),
                  ),
                  h5("Gene sets"),
                  tags$ul(
                  tags$li("Interested in a specific list of genes? Instead of constantly retyping them, go to the 'Sets' tab of the gene section."),
                  tags$li("Manually enter the genes you are interested in, or prepare and upload a text file with genes separated by commas or one per line. The genes can now be easily accessed when generating plots."),
                  tags$li("Gene sets can also be generated from marker lists. Use the 'Save to set' button when viewing markers."),
                  ),
                  h5("Saving"),
                  tags$ul(
                  tags$li("Mona gives users the ability to edit the cell metadata, including renaming clusters or creating entirely new annotations. But these changes do not automatically persist after closing the app! Use 'Save dataset' anytime you make changes you wish to save.")
                  )
                ),
                accordionItem(
                  title = "Plot Types",
                  status = "lightblue",
                  collapsed = T,
                  h5("Embedding"),
                  p("Use to view cell metadata and gene expression at the per-cell level. Typically used with UMAPs but any other reduction added to the dataset can also be plotted."),
                  h5("Heatmap"),
                  p("Use for understanding broad patterns in gene expression. Genes can be viewed either per-cell or the average per-group. Clustering is automatically performed on both axes."),
                  h5("Bubble"),
                  p("Similar to heatmaps, but allows you to focus on a smaller set of genes. Shows the average gene expression and the percent of cells expressing the gene in each group."),
                  h5("Violin"),
                  p("Use to view the distribution of a gene or quality measure, either across the entire dataset or per-group."),
                  h5("Bar"),
                  p("Use to view the proportion of cells across groups and how different metadata relate to one another.")
                ),
                accordionItem(
                  title = "Data Preparation",
                  status = "lightblue",
                  collapsed = T,
                  p("If you have some familiarity with R and Seurat, you probably already have a processed dataset. If not, visit the GitHub for more information and use Mona's built-in functions."),
                  p("The most important thing to know is that you cannot use Seurat objects themselves. Mona has an expected format called the 'Mona directory' - use 'save_mona_dir()' on a Seurat v5 object to generate them."),
                  p("Afterwards, any 'Mona directory' can be viewed in Mona by clicking on 'Load new dataset' and selecting it, or calling mona() with the path to the directory.")
                ),
                accordionItem(
                  title = "Performance",
                  status = "lightblue",
                  collapsed = T,
                  p("Here are some recommendations for having a smooth experience:"),
                  tags$ul(
                    tags$li("Mona has been tested on 200,000 cells without issue, but this is machine-dependent. Expect things to run slower the larger your dataset is."),
                    tags$li("Ensure that your datasets are on the system where R/Mona are installed. Communicating back and forth with a remote directory/server will create a noticeable delay."),
                    tags$li("If it's not critical to view every cell, consider downsampling your data if you are encountering slowness. Open the settings to try this feature."),
                    tags$li("While the app is executing something, like rendering a plot or calculating markers, allow it to finish before performing another action."),
                    tags$li("Heatmaps will generally be the most demanding plot type to generate, especially with large number of genes or complex metadata."),
                    tags$li("Be careful with using 'Compare across cells' for heatmaps and 'Density mode' for 3D embeddings, they can have long processing times."),
                    tags$li("Although you can view up to 8 plots at once, keep in mind that more plots will require more resources.")
                  )
                ),
                accordionItem(
                  title = "FAQ",
                  status = "lightblue",
                  collapsed = T,
                  h5("Why can't I find a particular gene? It doesn't show up anywhere, and it can't be added to a gene set?"),
                  p("It's possible the gene was very lowly expressed and was filtered out (see the 'min.cells' argument for CreateSeuratObject()). More likely, you are using an alias and need to find the specific gene symbol used in the dataset."),
                  h5("How does the marker table work?"),
                  p("Every dataset contains different kinds of categorical 'metadata', and each 'metadata' contains multiple 'groups'. Markers are the differentially expressed genes for each group, relative to all the other groups in that metadata."),
                  p("To save time, by default markers are pre-calculated when creating the Mona directory. Then by clicking on a group in the cell box, markers for that group will appear under the 'Markers' tab of the gene box."),
                  p("There's a small problem: the metadata can change or be added to over time. In these cases, Mona will alert you and markers can be calculated on-the-fly. Note that markers are meant to reflect the entire dataset, and so downsampling/subsetting are not taken into account. Users should use DEGs for these more complex cases."),
                  h5("Why do different plots have different expression values?"),
                  p("Embeddings and violin plots show the 'true' values, AKA the normalized expression stored in the 'data' slot. When comparing multiple genes simultaneously, as with heatmaps and bubble plots, scaled values are the default because it creates better contrast and shows where expression is above/below the mean. This can be disabled if desired."),
                  h5("Why do the plots sometimes refresh and I lose my changes?"),
                  p("Plots are redrawn whenever the entire dataset changes, like downsampling/subsetting. It also occurs when you edit the metadata or gene set currently in use by the plot. Finally, plots will redraw when changing the plot type (keep this in mind if you swap between plots often)."),
                  h5("How do I view my data as a 3D embedding?"),
                  p("Using the built-in functions process_mona() or integrate_mona(), this will be calculated automatically. If processing on your own, make sure to call RunUMAP() an additional time with 'n.components=3L'.")
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
            h5("Thanks to the following people for feedback during development:"),
            tags$ul(
              tags$li("Aaron Zorn"),
              tags$li("Andrea Holderbaum"),
              tags$li("Jacek Biesada")
            ),
            br(),
            h5("Made with support from the Center for Stem Cell & Organoid Medicine (CuSTOM) and the Developmental Biology Division, Cincinnati Children's Hospital"),
            img(src = "images/cchmc.jpg", height = 100, width = 300),
            br(),br(),
            h5("Mona would not be possible without the following excellent R packages:"),
            tags$ul(
              tags$li(tags$a(href="https://satijalab.org/seurat/","Seurat")),
              tags$li(tags$a(href="https://bnprks.github.io/BPCells/","BPCells")),
              tags$li(tags$a(href="https://github.com/RGLab/MAST/","MAST")),
              tags$li(tags$a(href="https://plotly.com/r/","plotly")),
              tags$li(tags$a(href="https://rinterface.github.io/bs4Dash/","bs4Dash")),
              tags$li(tags$a(href="https://biit.cs.ut.ee/gprofiler/page/r","gprofiler2")),
              tags$li(tags$a(href="https://talgalili.github.io/heatmaply/index.html","heatmaply")),
              tags$li(tags$a(href="https://github.com/dreamRs/shinyWidgets","shinywidgets")),
              tags$li(tags$a(href="https://github.com/thomasp85/shinyFiles","shinyFiles")),
              tags$li(tags$a(href="https://deanattali.com/shinyjs/","shinyjs")),
              tags$li(tags$a(href="https://rstudio.github.io/sortable/","sortable")),
              tags$li(tags$a(href="https://www.anatomyofcode.com/imola/","imola"))
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
    
    #addPopover(id="de_button_1",options=list(content="DEG group 1",placement="top",delay=500,trigger="hover"))
    #addPopover(id="de_button_2",options=list(content="DEG group 2",placement="top",delay=500,trigger="hover"))
    addPopover(id="new_anno",options=list(content="Create new metadata",placement="top",delay=500,trigger="hover"))
    addPopover(id="remove_anno",options=list(content="Remove metadata",placement="top",delay=500,trigger="hover"))
    addPopover(id="rename_anno",options=list(content="Edit metadata",placement="top",delay=500,trigger="hover"))
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

    addPopover(id="save_go",options=list(content="Export terms",placement="top",delay=500,trigger="hover"))
    
    addPopover(id="new_gene_set",options=list(content="Create new gene set",placement="top",delay=500,trigger="hover"))
    
    
    cur_data <- reactiveValues(seurat=NULL,use=NULL,name="",species="",description="",meta=NULL,meta_table=NULL,meta_use=NULL,genes=NULL,top_sets=NULL,quality=NULL,reducs=NULL)
    output$data_link <- renderUI({
      if (!is.null(cur_data$seurat)){
        tagList(tags$li(class='dropdown', actionLink("data_info",label=cur_data$name,icon=tags$i(class = "fas fa-info-circle", style="font-size: 18px; padding-right: 5px; color: #b9c5fd;"),style="color: black; font-size: 120%;")))
      }
    })
    
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
    
    get_top_sets <- function(seurat) {
      return(list(seurat@misc$var_100,seurat@misc$var_500,seurat@misc$mean_100,seurat@misc$mean_500))
    }
    
    downsample_data <- function() {
      if (input$downsample == 100) {
        cur_data$use <- cur_data$seurat
        cur_data$meta_use <- cur_data$meta_table
      } else {
        cell_count <- ncol(cur_data$seurat)
        subset <- dqsample.int(cell_count, round(cell_count*(input$downsample/100)))
        cur_data$use <- cur_data$seurat[,subset]
        cur_data$meta_use <- cur_data$meta_table[colnames(cur_data$use),]
      }
    }
    
    refresh_data_use <- function() {
      cur_data$meta_use <- cur_data$meta_table[colnames(cur_data$use),]
    }
    
    root <- c(home=fs::path_home())
    save_dir <- reactiveVal("datasets/")
    shinyDirChoose(input, id='data_new', roots=root, session = session,allowDirCreate = F)
    
    # Sets up "cur_data" when a new dataset is loaded
    # Note that depending on where data was processed, path to matrix may need to be updated
    data_setup <- function(data_dir) {
      showNotification("Loading dataset...", type = "message")
      cur_data$seurat <- qread(paste0(data_dir,"/seurat.qs"))
      assay <- DefaultAssay(cur_data$seurat)
      mat_dir <- cur_data$seurat[[assay]]$data@matrix@matrix@dir
      if (mat_dir != data_dir) {
        cur_data$seurat[[assay]]$data@matrix@matrix@dir <- data_dir
      }
      save_dir(data_dir)
      cur_data$name <- cur_data$seurat@misc$name
      cur_data$species <- cur_data$seurat@misc$species
      cur_data$description <- cur_data$seurat@misc$description
      meta_all <- cur_data$seurat@meta.data
      meta <- colnames(meta_all)
      filter_1 <- sapply(meta, function(x) class(meta_all[[x]]) %in% c("integer","numeric"))
      filter_2 <- sapply(meta, function(x) length(unique(meta_all[[x]])) >= 100)
      filter <- meta[filter_1 & filter_2]
      cur_data$quality <- filter
      cur_data$meta <- meta[!(meta %in% filter)]
      cur_data$meta_table <- meta_all[,cur_data$meta,drop=F]
      if (length(cur_data$quality) > 0) {
        cur_data$seurat@meta.data <- meta_all[,cur_data$quality,drop=F]
      } else {
        cur_data$seurat@meta.data <- meta_all[,1,drop=F]
      }
      
      updateVirtualSelect(inputId = "anno_select",choices = c(cur_data$meta),selected = NULL)
      updateVirtualSelect(inputId = "cluster_select",choices = c(""),selected = NULL)
      
      genes <- rownames(cur_data$seurat)
      cur_data$genes <- sort(genes)
      
      updateSelectizeInput(session, "searched_gene", choices = c(cur_data$genes),selected=character(0),server = T)
      
      cur_data$top_sets <- get_top_sets(cur_data$seurat)
      cur_data$reducs <- names(cur_data$seurat@reductions)
      downsample_data()
      shinyjs::show("new_gene_set")
      shinyjs::show("go_controls")
      shinyjs::disable("save_go")
    }
    
    # Called when a dataset is loaded when another data is already loaded
    # Essentially wipes everything: plots, gene sets, selection, metadata, etc.
    reset_data <- function(data_dir) {
      if (!is.null(cur_data$seurat)) {
        lapply(names(geneset_list$sets), function(x) {
          removeUI(paste0("#",x),immediate = T)
          geneset_list$sets[[x]] <- NULL
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
        shinyjs::hide("markers_show")
        shinyjs::hide("markers_none")
        shinyjs::hide("deg_show")
        shinyjs::hide("deg_none")
        cur_selection$plot <- NULL
        cur_selection$cells <- NULL
        updateSliderInput(session,"downsample",value=100)
        shinyjs::delay(500,data_setup(data_dir))
        shinyjs::delay(500,shinyjs::click("new_plot"))
      } else {
        data_setup(data_dir)
      }
    }
    
    observeEvent(input$load1, {
      removeModal(session)
      data_dir <- dataset_dirs[[1]]
      reset_data(data_dir)
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
            p("The classic PBMC dataset from 10X Genomics",br(),"~2700 cells, ~13000 genes"),
            shiny::actionButton("load1", "Load data")
          )
        ),
        footer = NULL
      ))
    })
    
    data_startup <- reactiveVal(mona_dir)
    
    observeEvent(data_startup(), {
      mona_files <- list.files(data_startup())
      if ("seurat.qs" %in% mona_files & "index_data" %in% mona_files) {
        reset_data(data_startup())
      } else {
        showNotification("Not a valid Mona directory...", type = "message")
      }
    })
    
    observeEvent(input$data_new, {
      file_info <- input$data_new
      if(is.list(file_info[[1]])) {
        data_dir <- paste(file_info$path,collapse = "/")
        data_dir <- paste0(root,data_dir)
        mona_files <- list.files(data_dir)
        if ("seurat.qs" %in% mona_files & "index_data" %in% mona_files) {
          reset_data(data_dir)
        } else {
          showNotification("Not a valid Mona directory...", type = "message")
        }
      }
    })
    
    observeEvent(input$data_save, {
      if (!is.null(cur_data$seurat)) {
        meta_quality <- cur_data$seurat@meta.data
        cur_data$seurat@meta.data <- cbind(meta_quality,cur_data$meta_table)
        qsave(cur_data$seurat, paste0(save_dir(),"/seurat.qs"))
        cur_data$seurat@meta.data <- meta_quality
        showNotification("Saving complete!", type = "message")
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
      gene <- input$searched_gene
      if (isTruthy(gene)) {
        if (gene %in% cur_data$genes) {
          url <- paste0('https://mygene.info/v3/query?q=symbol%3A',gene,'&fields=symbol%2Cname%2Calias%2Csummary&species=',cur_data$species,'&size=1&from=0&fetch_all=false&facet_size=10&entrezonly=false&ensemblonly=false&dotfield=false')
        } else {
          url <- paste0('https://mygene.info/v3/query?q="',gsub(" ","+",gene),'"&fields=symbol%2Cname%2Calias%2Csummary&species=',cur_data$species,'&size=1&from=0&fetch_all=false&facet_size=10&entrezonly=false&ensemblonly=false&dotfield=false')
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
    
    # Plotting 
    #-----------------------------------
    
    num_plots <- reactiveVal(0)
    plots_list <- reactiveValues(plots=list())
    plot_remove <- reactiveVal(NULL)
    plot_id <- reactiveVal(0)
    plot_order <- reactiveVal(NULL)
    plot_settings <- reactiveValues(point_size=6,point_transparent=1.0,color_discrete="rainbow",color_cont="viridis",color_scaled="blue-red")
    
    
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
        plots_list$plots[[id]] <- plotServer(id,num_plots,plot_remove,cur_selection,selection_list,geneset_list,plot_settings,cur_data,marker_subset,deg_subset)
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
      plot_settings$point_size <- switch(input$point_size, "Small"=4, "Medium"=6, "Large"=8)
    })
    
    observeEvent(input$point_transparent, {
      if(input$point_transparent) {
        plot_settings$point_transparent <- 0.4
      } else {
        plot_settings$point_transparent <- 1.0
      }
    })
    
    observeEvent(input$color_scale_1, {
      plot_settings$color_discrete <- input$color_scale_1
    })
    
    observeEvent(input$color_scale_2, {
      plot_settings$color_cont <- switch(input$color_scale_2, "viridis"="viridis", "plasma"="plasma", "mona"=colorRamp(colors=c("gray85","blue4","steelblue1","cyan1")))
    })
    
    observeEvent(input$color_scale_3, {
      plot_settings$color_scaled <- switch(input$color_scale_3, "blue-red"=colorRamp(colors=c("dodgerblue2","gray99","firebrick1")),"purple-yellow"=colorRamp(colors=c("darkorchid1","gray20","#f1f708")),"viridis"="viridis", "plasma"="plasma", "mona"=colorRamp(colors=c("gray85","blue4","steelblue1","cyan1")))
    })
    
    #---------------------
    # Annotation
    
    observeEvent(input$anno_select, {
      if (isTruthy(input$anno_select)) {
        meta <- unique(cur_data$meta_table[[input$anno_select]])
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
        markers <- cur_data$seurat@misc$markers
        markers <- subset(markers,metadata==input$anno_select & cluster==input$cluster_select)
        if (nrow(markers) > 0) {
          if (nrow(markers) == 1 && markers$gene == "none") {
            shinyjs::hide("markers_show")
            shinyjs::hide("markers_new")
            shinyjs::show("markers_none")
            cur_markers(NULL)
          } else {
            markers <- markers[,c("gene","avg_log2FC","p_val_adj","avg.1","avg.2")]
            colnames(markers) <- c("gene","log2FC","p-val","avg.1","avg.2")
            shinyjs::hide("markers_none")
            shinyjs::hide("markers_new")
            shinyjs::show("markers_show")
            shinycssloaders::showSpinner("marker_table")
            cur_markers(markers)
          }
        } else if (nrow(markers) == 0 && length(unique(cur_data$meta_table[[input$anno_select]])) > 1){
            shinyjs::hide("markers_none")
            shinyjs::hide("markers_show")
            shinyjs::show("markers_new")
            cur_markers(NULL)
        }
      }
    },ignoreInit = T)
    
    observeEvent(input$new_anno, {
      if (!isTruthy(cur_data$seurat)) {
        return()
      }
      showModal(modalDialog(
        title = "New annotation",
        easyClose = T,
        size="s",
        selectizeInput("copy_anno",label="Use as template",choices=c("None",cur_data$meta)),
        textInput("new_anno_name",label="Name",value=""),
        shiny::actionButton("new_anno_confirm", "Create"),
        footer = NULL
      ))
    })
    
    observeEvent(input$new_anno_confirm, {
      removeModal(session)
      validate(
        need(input$new_anno_name,""),
      )
      if (!(input$new_anno_name %in% colnames(cur_data$meta_table))) {
        if (input$copy_anno == "None") {
          cur_data$meta_table[[input$new_anno_name]] <- rep("Undefined",nrow(cur_data$meta_table))
        } else {
          cur_data$meta_table[[input$new_anno_name]] <- cur_data$meta_table[[input$copy_anno]]
          markers_all <- cur_data$seurat@misc$markers
          markers <- subset(markers_all,metadata==input$copy_anno)
          markers$metadata <- input$new_anno_name
          cur_data$seurat@misc$markers <- rbind(markers_all,markers)
        }
        cur_anno <- input$anno_select
        cur_data$meta <- colnames(cur_data$meta_table)
        refresh_data_use()
        updateVirtualSelect(
          inputId = "anno_select",
          choices = c(cur_data$meta),
          selected = cur_anno
        )
      }
    })
    
    observeEvent(input$remove_anno, {
      if (!isTruthy(cur_data$seurat) | !isTruthy(input$anno_select)) {
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
      cur_data$meta_table[[input$anno_select]] <- NULL
      markers <- cur_data$seurat@misc$markers
      cur_data$seurat@misc$markers <- markers[markers$metadata != input$anno_select,]
      cur_data$meta <- colnames(cur_data$meta_table)
      refresh_data_use()
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
      if (!isTruthy(cur_data$seurat) | !isTruthy(input$anno_select)) {
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
            shiny::actionButton("rename_anno_confirm", "Rename")
          ),
          column(
            width=6,
            style='padding-left:14px; border-left: 1px solid;',
            selectizeInput("merge_anno",
             label = "Fill missing values with",
             choices = cur_data$meta,
             selected = NULL
            ),
            textInput("merge_anno_name",label="Annotation name:",value=""),
            shiny::actionButton("merge_anno_confirm", "Merge")
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
      if (!(input$rename_anno_name %in% colnames(cur_data$meta_table))) {
        cur_data$meta_table[[input$rename_anno_name]] <- cur_data$meta_table[[input$anno_select]]
        cur_data$meta_table[[input$anno_select]] <- NULL
        markers_meta <- cur_data$seurat@misc$markers$metadata
        markers_meta[markers_meta == input$anno_select] <- input$rename_anno_name
        cur_data$seurat@misc$markers$metadata <- markers_meta
        cur_data$meta <- colnames(cur_data$meta_table)
        refresh_data_use()
        updateVirtualSelect(
          inputId = "anno_select",
          choices = c(cur_data$meta),
          selected = input$rename_anno_name
        )
      }
    })
    
    observeEvent(input$merge_anno_confirm, {
      removeModal(session)
      validate(
        need(input$merge_anno_name,""),
        need(input$merge_anno,"")
      )
      if (!(input$merge_anno_name %in% colnames(cur_data$meta_table))) {
        anno_1 <- as.character(cur_data$meta_table[[input$anno_select]])
        anno_2 <- as.character(cur_data$meta_table[[input$merge_anno]])
        anno_final <- anno_1
        anno_final[is.na(anno_1)] <- anno_2[is.na(anno_1)]
        anno_final[anno_1 == "Undefined"] <- anno_2[anno_1 == "Undefined"]
        cur_data$meta_table[[input$merge_anno_name]] <- anno_final
        cur_anno <- input$anno_select
        cur_data$meta <- colnames(cur_data$meta_table)
        refresh_data_use()
        updateVirtualSelect(
          inputId = "anno_select",
          choices = c(cur_data$meta),
          selected = cur_anno
        )
      }
    })
    
    observeEvent(input$new_cluster, {
      if (!isTruthy(cur_data$seurat) | !isTruthy(cur_selection$cells) | !isTruthy(input$anno_select)) {
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
      validate(
        need(input$new_cluster_name,"")
      )
      clusters <- cur_data$meta_table[input$anno_select]
      clusters_old <- clusters[,1]
      filter <- rownames(clusters) %in% cur_selection$cells
      selected_clusters <- as.vector(unique(clusters[filter,1]))
      clusters <- as.vector(clusters[,1])
      clusters[filter] <- input$new_cluster_name
      cur_data$meta_table[[input$anno_select]] <- clusters
      if (input$new_cluster_name %in% unique(clusters_old)){
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
      if (!isTruthy(cur_data$seurat) | !isTruthy(input$cluster_select)) {
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
      clusters <- cur_data$meta_table[[input$anno_select]]
      clusters <- as.vector(clusters)
      filter <- clusters == input$cluster_select
      clusters[filter] <- "Undefined"
      cur_data$meta_table[[input$anno_select]] <- clusters
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
      if (!isTruthy(cur_data$seurat) | !isTruthy(input$cluster_select)) {
        return()
      }
      showModal(modalDialog(
        title = "Edit group",
        easyClose = T,
        size="m",
        fluidRow(
          column(
            width=6,
            style='padding-right:14px;',
            HTML(paste0("<b>Old name:</b> <br>",input$cluster_select)),
            br(),br(),
            textInput("rename_cluster_name",label="New name:",value=""),
            shiny::actionButton("rename_cluster_confirm", "Rename")
          ),
          column(
            width=6,
            style='padding-left:14px; border-left: 1px solid;',
            textInput("recluster_res",label="Resolution (0.1 to 2)",value=""),
            shiny::actionButton("recluster_confirm", "Find subclusters")
          )
        ),
        footer = NULL
      ))
    })
    
    observeEvent(input$rename_cluster_confirm, {
      removeModal(session)
      validate(
        need(input$rename_cluster_name,"")
      )
      clusters <- cur_data$meta_table[[input$anno_select]]
      clusters_old <- as.vector(clusters)
      clusters_new <- clusters_old
      filter <- clusters_new == input$cluster_select
      clusters_new[filter] <- input$rename_cluster_name
      cur_data$meta_table[[input$anno_select]] <- clusters_new
      if (input$rename_cluster_name %in% unique(clusters_old)){
        markers <- cur_data$seurat@misc$markers
        filter <- (markers$metadata == input$anno_select & markers$cluster == input$rename_cluster_name)
        cur_data$seurat@misc$markers <- markers[!filter,]
      } else {
        markers_meta <- cur_data$seurat@misc$markers$metadata
        markers_cluster <- cur_data$seurat@misc$markers$cluster
        filter <- (markers_meta == input$anno_select & markers_cluster == input$cluster_select)
        markers_cluster[filter] <- input$rename_cluster_name
        cur_data$seurat@misc$markers$cluster <- markers_cluster
      }
      refresh_data_use()
      groups <- gtools::mixedsort(unique(clusters_new))
      updateVirtualSelect(
        inputId = "cluster_select",
        choices = groups,
        selected = NULL
      )
    })
    
    observeEvent(input$recluster_confirm, {
      removeModal(session)
      validate(
        need(input$recluster_res,"")
      )
      resolution <- input$recluster_res
      if (typeof(resolution) == "double" && resolution > 0) {
        
      }
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
    cur_terms <- reactiveVal(NULL)
    de_cells_1 <- reactiveValues(name=NULL,cells=NULL)
    de_cells_2 <- reactiveValues(name=NULL,cells=NULL)
    cur_degs <- reactiveVal(NULL)
    deg_subset <- reactiveVal(NULL)
    go_type <- reactiveVal(NULL)
    
    de_opts_change <- function() {
      shinyjs::hide("deg_show")
      shinyjs::hide("deg_none")
      shinyjs::show("deg_new")
    }
    
    observeEvent(input$de_opts_1,{
      type <- input$de_opts_1
      updateRadioGroupButtons(session,"de_opts_1",selected=character(0))
      if (type == "Group" && isTruthy(input$cluster_select)) {
        meta_subset <- cur_data$meta_use[cur_data$meta_use[[input$anno_select]] == input$cluster_select,]
        de_cells_1$cells = rownames(meta_subset)
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
        meta_subset <- cur_data$meta_use[cur_data$meta_use[[input$anno_select]] == input$cluster_select,]
        de_cells_2$cells = rownames(meta_subset)
        de_cells_2$name = paste0(input$anno_select," - ",input$cluster_select)
        de_opts_change()
      } else if (type == "Select" && isTruthy(cur_selection$cells)) {
        de_cells_2$cells = cur_selection$cells
        de_cells_2$name = paste0(length(de_cells_2$cells)," cells")
        de_opts_change()
      } else if (type == "Rest" && isTruthy(de_cells_1$cells)) {
        meta_subset <- cur_data$meta_use[!(rownames(cur_data$meta_use) %in% de_cells_1$cells),]
        de_cells_2$cells = rownames(meta_subset)
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
    
    get_new_markers <- function(metadata=NULL,cluster=NULL) {
      markers <- markers_mona(cur_data$seurat,meta_table=cur_data$meta_table,metadata=metadata,cluster=cluster)
      markers <- markers %>% arrange(p_val_adj) %>% slice(1:100)
      markers$gene <- rownames(markers)
      markers$avg_log2FC <- signif(markers$avg_log2FC,3)
      markers$p_val_adj <- formatC(markers$p_val_adj, format = "e", digits = 2)
      if (nrow(markers) > 0) {
        markers$cluster <- as.character(cluster)
        markers$metadata <- as.character(metadata)
        markers <- markers[,c("gene","cluster","metadata","avg_log2FC","p_val_adj","avg.1","avg.2")]
        markers_all <- cur_data$seurat@misc$markers
        cur_data$seurat@misc$markers <- rbind(markers_all,markers)
      }
      markers <- markers[,c("gene","avg_log2FC","p_val_adj","avg.1","avg.2")]
      colnames(markers) <- c("gene","log2FC","p-val","avg.1","avg.2")
      if (nrow(markers) > 0) {
        shinyjs::hide("markers_none")
        shinyjs::hide("markers_new")
        shinyjs::show("markers_show")
        return(markers)
      } else {
        shinyjs::hide("markers_show")
        shinyjs::hide("markers_new")
        shinyjs::show("markers_none")
        return(NULL)
      }
    }
    
    get_deg <- function() {
      markers <- markers_mona(cur_data$use,group.1=de_cells_1$cells,group.2=de_cells_2$cells)
      markers <- markers %>% arrange(p_val_adj) %>% slice(1:100)
      markers$gene <- rownames(markers)
      markers$avg_log2FC <- signif(markers$avg_log2FC,3)
      markers$p_val_adj <- formatC(markers$p_val_adj, format = "e", digits = 2)
      markers <- markers[,c("gene","avg_log2FC","p_val_adj","avg.1","avg.2")]
      colnames(markers) <- c("gene","log2FC","p-val","avg.1","avg.2")
      if (nrow(markers) > 0) {
        shinyjs::hide("deg_none")
        shinyjs::hide("deg_new")
        shinyjs::show("deg_show")
        return(markers)
      } else {
        shinyjs::hide("deg_show")
        shinyjs::hide("deg_new")
        shinyjs::show("deg_none")
        return(NULL)
      }
    }
    
    generate_marker_table <- function() {
      genes <- marker_subset()
      Sys.sleep(0.25)
      shinycssloaders::hideSpinner("marker_table")
      if (isTruthy(genes)) {
        DT::datatable(
          genes,
          extensions = c("Buttons"),
          options = list(dom="t", pageLength=10,scrollY="23.3vh",scrollCollapse=T,paging=F,autoWidth=F,scrollX=T,columnDefs = list(list(targets = c(3,4), visible=F),list(targets = "_all", width = "33%"),list(className = 'dt-left', targets = "_all"))),
          rownames= FALSE,
          class = "compact"
        ) %>% DT::formatStyle(columns = c("gene","log2FC","p-val"), fontSize = '1.75vh', lineHeight="70%")
      }
    }
    
    generate_deg_table <- function() {
      genes <- deg_subset()
      Sys.sleep(0.25)
      shinycssloaders::hideSpinner("deg_table")
      if (isTruthy(genes)) {
        DT::datatable(
          genes,
          extensions = c("Buttons"),
          options = list(dom="t", pageLength=10,scrollY="23.3vh",scrollCollapse=T,paging=F,autoWidth=F,scrollX=T,columnDefs = list(list(targets = c(3,4), visible=F),list(targets = "_all", width = "33%"),list(className = 'dt-left', targets = "_all"))),
          rownames= FALSE,
          class = "compact"
        ) %>% DT::formatStyle(columns = c("gene","log2FC","p-val"), fontSize = '1.75vh', lineHeight="70%")
      }
    }
    
    output$marker_table <- DT::renderDT(generate_marker_table(), server = FALSE)
    
    output$deg_table <- DT::renderDT(generate_deg_table(), server = FALSE)
    
    observeEvent(marker_subset(), {
      if (isTruthy(marker_subset())) {
        shinyjs::show("go_show")
      } else{
        shinyjs::hide("go_show")
      }
    })
    
    observeEvent(input$markers_find, {
      shinyjs::hide("markers_none")
      shinyjs::hide("markers_new")
      shinyjs::show("markers_show")
      shinycssloaders::showSpinner("marker_table")
      cur_markers(get_new_markers(metadata=input$anno_select,cluster=input$cluster_select))
    })
    
    observeEvent(input$deg_find, {
      if (isTruthy(de_cells_1$cells) && isTruthy(de_cells_2$cells)) {
        if (sum(de_cells_1$cells %in% de_cells_2$cells) > 0) {
          showNotification("DEG groups overlap!", type = "message")
        } else {
          shinyjs::hide("deg_none")
          shinyjs::hide("deg_new")
          shinyjs::show("deg_show")
          shinycssloaders::showSpinner("deg_table")
          cur_degs(get_deg())
        }
      }
    })
    
    observeEvent(input$deg_return, {
      shinyjs::hide("deg_none")
      shinyjs::hide("deg_new")
      if (!is.null(cur_degs())) {
        shinyjs::show("deg_show")
      }
    })
    
    observeEvent(input$go_toggle, {
      shinyjs::show("go_show")
      go_type(input$go_toggle)
    })
    
    # Based on the gprofiler2 package, requires internet connection as this is not pre-calculated
    get_go_terms <- function(genes) {
      species <- switch(cur_data$species,"human"="hsapiens","mouse"="mmusculus","rat"="rnorvegicus","fruitfly"="dmelanogaster","zebrafish"="drerio","nematode"="celegans","pig"="sscrofa","frog"="xtropicalis")
      gene_list <- genes$gene
      gostres <- gost(query = gene_list, 
                      organism = species, ordered_query = FALSE, 
                      multi_query = FALSE, significant = TRUE, exclude_iea = FALSE, 
                      measure_underrepresentation = FALSE, evcodes = FALSE, 
                      user_threshold = 0.05, correction_method = "g_SCS", 
                      domain_scope = "annotated", custom_bg = NULL, 
                      numeric_ns = "", sources = NULL, as_short_link = FALSE, highlight = TRUE)
      results <- gostres$result
      if (is.null(results)) {
        #shinyjs::hide("go_show")
        shinyjs::show("go_none")
        shinyjs::disable("save_go")
        shinyjs::runjs("$('#go_controls').css('margin-top','8.0vh')")
        return(NULL)
      } else {
        results <- results[,c("term_id","term_name","p_value")]
        colnames(results) <- c("id","name","p-val")
        results$`p-val` <- formatC(results$`p-val`, format = "e", digits = 2)
        cur_terms(results)
        shinyjs::show("go_show")
        shinyjs::enable("save_go")
        shinyjs::hide("go_none")
        shinyjs::runjs("$('#go_controls').css('margin-top','0.0vh')")
        return(results)
      }
    }
    
    generate_go_table <- function() {
      if (go_type() == "Markers") {
        genes <- marker_subset()
      } else if (go_type() == "DEG") {
        genes <- deg_subset()
      }
      terms <- NULL
      if (isTruthy(genes) && nrow(genes) > 0) {
        shinyjs::show("go_show")
        shinyjs::hide("go_none")
        shinyjs::enable("save_go")
        shinyjs::runjs("$('#go_controls').css('margin-top','0.0vh')")
        terms <- get_go_terms(genes)
        if (isTruthy(terms)) {
          DT::datatable(
            terms,
            extensions = c("Buttons"),
            options = list(dom="t", pageLength=10,scrollY="23.3vh",scrollCollapse=T,paging=F,autoWidth=F,scrollX=T,columnDefs = list(list(targets = c(1), width = "44%"),list(className = 'dt-left', targets = "_all"))),
            rownames= FALSE,
            class = "compact"
          ) %>% DT::formatStyle(columns = c("id","name","p-val"), fontSize = '1.6vh', lineHeight="85%")
        }
      } else {
        #shinyjs::hide("go_show")
        shinyjs::hide("go_none")
        shinyjs::disable("save_go")
        shinyjs::runjs("$('#go_controls').css('margin-top','24.0vh')")
        DT::datatable(data.frame())
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
          } else {
            "go_DEG.txt"
          }
        },
        content = function(file) {
          write.table(cur_terms(),file,sep="\t",col.names = T,row.names = T,quote = F)
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
      } else {
        marker_subset(NULL)
      }
    })
    
    observe({
      if(!is.null(cur_degs())) {
        if (input$fc_filter_deg == "All") {
          deg_subset(cur_degs())
        } else if (input$fc_filter_deg == "Neg") {
          deg_subset(cur_degs() %>% filter(log2FC < 0))
        } else if (input$fc_filter_deg == "Pos") {
          deg_subset(cur_degs() %>% filter(log2FC > 0))
        }
      } else {
        deg_subset(NULL)
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
        geneset_list$sets[[id]] <- genesServer(id,geneset_list,cur_data,markers=marker_subset,markers_name=input$cluster_select)
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
        geneset_list$sets[[id]] <- genesServer(id,geneset_list,cur_data,markers=deg_subset,markers_name="DEG")
        showNotification("Markers copied!", type = "message")
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
          bs4Dash::addPopover(id="cell_text",options=list(content=HTML(paste0(round(selected/nrow(cur_data$meta_use),3)*100,"% of total","<br/>",round(selected/selection_list$counts[[cur_selection$plot]],3)*100,"% within plot")),html=T,placement="bottom",delay=500,trigger="hover"))
        } else {
          bs4Dash::addPopover(id="cell_text",options=list(content=HTML(paste0(round(selected/nrow(cur_data$meta_use),3)*100,"% of total")),html=T,placement="bottom",delay=500,trigger="hover"))
        }
      } else {
        bs4Dash::removePopover(id="cell_text")
      }
    },ignoreNULL = F)

    output$cell_select <- renderUI({
      if(!is.null(cur_selection$cells)) {
        shinyjs::runjs("$('#cell_text').css('color','#96a8fc')")
        paste0(length(cur_selection$cells)," cells selected")
      } else if (!is.null(cur_data$seurat)){ 
        shinyjs::runjs("$('#cell_text').css('color','#1f2d3d')")
        if (nrow(cur_data$meta_use) == nrow(cur_data$meta_table)) {
          paste0(nrow(cur_data$meta_use), " cells total")
        } else {
          paste0(nrow(cur_data$meta_use), " cells subsetted")
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
      if (length(cur_selection$cells > 0)) {
        showNotification("Subsetting data!", type = "message")
        cur_data$use <- cur_data$use[,cur_selection$cells]
        cur_data$meta_use <- cur_data$meta_table[colnames(cur_data$use),]
        cur_selection$plot <- NULL
        cur_selection$cells <- NULL
      }
    },ignoreInit = T)
    
    observeEvent(input$subset_undo, {
      downsample_data()
    },ignoreInit = T)
    
  }
  
  shinyApp(ui = ui, server = server,options = list(launch.browser=T))
}
