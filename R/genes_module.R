genesUI <- function(id) {
  ns <- NS(id)
  div(
    id=id,
    splitLayout(
      actionButton(ns("import_genes"),icon=icon("file-lines"),label="",width="32px", height="32px", style="padding: 3px; background-color: #fcfcff;", class="shinyFiles", "data-title"="Select a gene set file","data-selecttype"="single"),
      textInput(ns("set_name"),label="",placeholder = "Name",value = paste0("Gene set ",substr(id,8,nchar(id)))),
      actionButton(ns("close_set"),icon=icon("xmark"),label="",width="32px", height="32px", style="padding: 3px; background-color: #fcfcff;"),
      cellWidths = c("15%","50%","15%")
    ),
    fluidRow(
      column(
        width = 1
      ),
      column(
        width=10,
        selectizeInput(ns("gene_set"),label="",choices=NULL,multiple=T,options=list(
          onChange = I(paste0("function () {
            var extra = $('#",ns('genes_extra'),"')
            if(extra) extra.remove()
            const $control = this.$control
            const $items = $control.find('.item')
            const limit = 20
            if ($items.length <= limit)
                return
            $items.toArray().forEach((item, index) => {
                if (index < limit)
                    return
                $(item).hide()
            });
            $control.append(`<span id=",ns('genes_extra'),">+${$items.length - limit} other genes</span>`)}"
          ))
        )
        )
      ),
      column(
        width=1
      )
    ),
    tags$head(tags$style(paste0("#",ns("set_name")," {text-align: center;}")))
  )
}

genesServer <- function(id,sets,data=NULL,markers=NULL,markers_name=NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      onNextInput({
        if (is.null(markers)) {
          updateSelectizeInput(session,"gene_set",choices=data$genes,server = T,options=list(maxOptions=1000))
        } else {
          marker_genes <- markers()[["gene"]]
          updateTextInput(session,"set_name",value=paste0("Markers - ",markers_name))
          updateSelectizeInput(session,"gene_set",choices=data$genes,selected=marker_genes,server = T,options=list(maxOptions=1000))
        }
      })
      root <- c(home=fs::path_home())
      shinyFileChoose(input, id='import_genes', roots=root, filetypes=c('txt'), session = session)
      
      observeEvent(input$import_genes, {
        if (!is.integer(input$import_genes)) {
          gene_file <- parseFilePaths(root, input$import_genes)$datapath
          file_length <- count.fields(gene_file)
          genes <- c()
          if (length(file_length) > 1) {
            gene_table <- read.table(gene_file,sep="\n")
            genes <- gene_table[,1]
          } else {
            gene_table <- t(read.table(gene_file,sep=","))
            genes <- gene_table[,1]
            genes <- unname(genes)
          }
          genes_final <- genes[genes %in% data$genes]
          genes_leftover <- genes[!(genes %in% data$genes)]
          gene_diff <- length(genes) - length(genes_final)
          if (gene_diff > 0) {
            showModal(modalDialog(
              title = "Warning!",
              easyClose = T,
              size="m",
              p(if (gene_diff == 1) paste0(gene_diff, " gene not found in dataset") else paste0(gene_diff, " genes not found in dataset"),style="text-align:center"),
              p(if (gene_diff <= 50) paste(genes_leftover,collapse=", ") else "",style="text-align:center"),
              modalButton("Ok"),
              footer = NULL
            ))
          }
          updateSelectizeInput(session,"gene_set",choices=data$genes,selected=genes_final,server = T,options=list(maxOptions=30000))
        }
      })
      
      observeEvent(data$genes, {
        updateSelectizeInput(session,"gene_set",choices=data$genes,server = T,options=list(maxOptions=30000))
      })
      
      observeEvent(input$set_name, {
        all_names <- lapply(sets$sets,function(x) x$name())
        if (sum(all_names == input$set_name) > 1) {
          updateTextInput(session,"set_name",value=paste0(input$set_name," - ",floor(runif(n=1, min=1, max=99))))
        }
      })
      
      observeEvent(input$close_set, {
        sets$sets[[id]] <- NULL
        removeUI(paste0("#",id))
      })
      
      return(
        list(
          name=reactive(input$set_name),
          genes=reactive(input$gene_set)
        )
      )
    }
  )
}