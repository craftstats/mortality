
# modulo para crear gráfico con opciones y botones ------------------------

plot_con_opciones_UI <- function(id) {
  ns <-NS(id)
  div(
    plotUI(ns("salida")),
    fluidRow(
           column ( width=4, 
                      downloadBttn(outputId = ns("down"), label = "Download", size = "xs")),
           column ( width=4, offset = 4,
                     actionBttn(inputId = ns("zoom"), icon = icon("search-plus", class = "opt"),
                     style = "fill", color = "danger", size = "s"))
    )
  )
}

plot_con_opciones <- function(input, output, session, tipo, objeto, name) {
  req(objeto())  
  ns <- session$ns
  plot <- callModule(tipo,"salida", objeto(), name)
  
  onclick("zoom", {
    showModal(
      modalDialog(
          renderPlot({plo()}), height = 600
          ),
          easyClose = TRUE, size = "l", footer = NULL
      ) }
  )
  
  output$down <- downloadHandler(
    filename = function() { paste(name, '.png', sep='') },
    content = function(file) {
      png(file)
      print(plotInput())
      dev.off()
    }
  )
}



# distintos tipos de funciones para crear los gráficos --------------------

# El modulo UI general para todas las funciones creadoras
plotUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("plot"))
}




