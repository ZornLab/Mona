custom_file_input <- function(inputId) {
  restoredValue <- restoreInput(id = inputId, default = NULL)
  
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
    warning("Restored value for ", inputId, " has incorrect format.")
    restoredValue <- NULL
  }
  
  if (!is.null(restoredValue)) {
    restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
  }
  
  div(
    class = "form-group shiny-input-container",
    style = htmltools::css(width="2.0vw"),
    div(
      tags$label(
        class = "input-group-btn input-group-prepend",
        span(
          class = "btn btn-default btn-file action-button", 
          style = htmltools::css(width="2.0vw",padding="3px",background_color="#fcfcff","border_radius!"="0.25rem"),
          icon("file-lines"),
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
  )
}

genesUI <- function(id) {
  ns <- NS(id)
  div(
    id=id,
    splitLayout(
      custom_file_input(ns("import_genes")),
      textInput(ns("set_name"),label="",placeholder = "Name",value = paste0("Gene set ",substr(id,8,nchar(id)))),
      actionButton(ns("close_set"),icon=icon("xmark"),label="",width="2.0vw", style="padding: 3px; background-color: #fcfcff;"),
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
                if (index < limit) {
                    $(item).show()
                } else {
                  $(item).hide()
                }
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

genesServer <- function(id,sets,data=NULL,genes=NULL,name=NULL,upload=NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      genes <- genes
      name <- name
      onNextInput({
        if (is.null(name)) {
          updateSelectizeInput(session,"gene_set",choices=data$genes,server = T,options=list(maxOptions=1000))
        } else {
          updateTextInput(session,"set_name",value=name)
          updateSelectizeInput(session,"gene_set",choices=data$genes,selected=genes,server = T,options=list(maxOptions=1000))
        }
      })
      
      observeEvent(input$import_genes, {
        if (!is.integer(input$import_genes)) {
          gene_file <- input$import_genes[["datapath"]]
          genes <- NULL
          name <- NULL
          genes_all <- NULL
          gene_table <- data.table::fread(gene_file,header = F,fill = T)
          if (nrow(gene_table) == 1) {
            genes <- t(gene_table)
            if (!(genes[1] %in% data$genes)) {
              name <- genes[1]
              genes <- genes[2:length(genes)]
              genes_all <- genes
            }
          } else if (ncol(gene_table) == 1) {
            genes <- gene_table$V1
            if (!(genes[1] %in% data$genes)) {
              name <- genes[1]
              genes <- genes[2:length(genes)]
              genes_all <- genes
            }
          } else {
            first_col <- gene_table$V1
            first_row <- t(gene_table[1,])
            if (sum(first_col %in% data$genes) == 0) {
              name <- first_row[1]
              genes <- first_row[2:length(first_row)]
              remainder <- gene_table[2:nrow(gene_table),2:ncol(gene_table),drop=F]
              upload$names <- first_col[2:length(first_col)]
              genes_remainder <- lapply(split(remainder, 1:nrow(remainder)), unlist)
              upload$genes <- genes_remainder
              genes_all <- c(genes,unlist(genes_remainder))
            } else if (sum(first_row %in% data$genes) == 0) {
              name <- first_col[1]
              genes <- first_col[2:length(first_col)]
              remainder <- gene_table[2:nrow(gene_table),2:ncol(gene_table),drop=F]
              upload$names <- first_row[2:length(first_row)]
              genes_remainder <- as.list(remainder)
              upload$genes <- genes_remainder
              genes_all <- c(genes,unlist(genes_remainder))
            }
          }
          if (is.null(genes)) {
            showNotification("Not a valid gene set file...", type = "message")
          } else {
            genes_all <- genes_all %>% unique() %>% stringi::stri_remove_empty_na()
            genes_leftover <- genes_all[!(genes_all %in% data$genes)]
            gene_diff <- length(genes_leftover)
            if (gene_diff > 0) {
              showModal(modalDialog(
                title = "Warning!",
                easyClose = T,
                size="m",
                p(if (gene_diff == 1) paste0(gene_diff, " gene not found in dataset:") else paste0(gene_diff, " genes not found in dataset:"),style="text-align:center"),
                p(if (gene_diff <= 50) paste(genes_leftover,collapse=", ") else "",style="text-align:center"),
                modalButton("Ok"),
                footer = NULL
              ))
            }
            genes <- genes %>% unique() %>% stringi::stri_remove_empty_na()
            genes_final <- genes[genes %in% data$genes]
            updateTextInput(session,"set_name",value=name)
            updateSelectizeInput(session,"gene_set",choices=data$genes,selected=genes_final,server = T,options=list(maxOptions=1000))
          }
        }
      })
      
      observeEvent(data$genes, {
        updateSelectizeInput(session,"gene_set",choices=data$genes,server = T,options=list(maxOptions=1000))
      })
      
      observeEvent(input$close_set, {
        sets$sets[[id]] <- NULL
        removeUI(paste0("#",id))
      })
      
      return(
        list(
          name=debounce(reactive(input$set_name),300),
          genes=debounce(reactive(input$gene_set),300)
        )
      )
    }
  )
}