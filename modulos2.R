
deploy3 <- function(lista, fUI) {
  
  if (length(lista) >0) {
    tagList(map(names(lista), ~ fUI(.x, .x)))
    
  }
  
}

run3 <- function(bas, fserver) {
  if (length(bas$memoria) >0) {
    map(names(bas$memoria), ~ callModule(fserver, .x, .x, bas))
    }
}


# Modulo descriptivos ( 3 boxes) ------------------------------------------

descriptivos_UI <- function(id, title) {
  ns <- NS(id)
  fluidRow(
       h2(title),
       box(width = 6, title = "Rates respecto a edad", collapsible = TRUE,
           plot_con_opciones_UI(ns("age"))),
       box(width = 6, title = "Rates respecto a cohorte", collapsible = TRUE, 
           plot_con_opciones_UI(ns("coho")))
       )
}

descriptivos_server <- function(input, output, session, name, bas) {
  ns <- session$ns
  cat(name)
  callModule(plot_con_opciones, "age", name, bas, "functions") 
  callModule(plot_con_opciones, "coho", name, bas, "time")
}


# modulo para crear gráfico con opciones y botones ------------------------

plot_con_opciones_UI <- function(id) {
  ns <-NS(id)
  div(
    uiOutput(ns("salida")),
    fluidRow(
           column ( width=4, 
                      downloadBttn(outputId = ns("down"), label = "Download", size = "xs")),
           column ( width=4, offset = 4,
                     actionBttn(inputId = ns("zoom"), icon = icon("search-plus", class = "opt"),
                     style = "fill", color = "danger", size = "s"))
    )
  )
}

plot_con_opciones <- function(input, output, session, name, bas, plottype) {
  ns <- session$ns
  output$salida <- renderUI({
      lista <- bas$memoria
      cat(names(lista))
      mia <- min(lista[[name]]$year)
      mxa <- max(lista[[name]]$year)
      mie <- min(lista[[name]]$age)
      mxe <- max(lista[[name]]$age)
      cat(mxe)
      cat(lista$series)
      datos <- st2demo(lista[[name]])
      print(datos)
     div(      
       sliderInput(ns("anos"), "Years", min = mia, max = mxa , value = c(mia,mxa), step = 1),
       sliderInput(ns("edad"), "Edad", min = mie, max = mxe , value = c(mie,mxe), step = 1),
       renderPlot({plot(datos,
                        ages=seq(input$edad[1], input$edad[2], 1), 
                        years=seq(input$anos[1], input$anos[2], 1),
                        plot.type = plottype  
       )})
     )
  }) 
  
  plo <- reactive({
        auxi <- input$zoom    
        ages=seq(input$edad[1], input$edad[2], 1)
        plot(st2demo( bas$memoria[[name]]),
         ages = ages, 
         years=seq(input$anos[1], input$anos[2], 1),
         plot.type = plottype) })  
    
  
  onclick("zoom", {
    showModal(
      modalDialog(
          renderPlot({plo()}, height = 600),
          easyClose = TRUE, size = "l", footer = NULL
      ))
    }
  )
  
  output$down <- downloadHandler(
    filename = function() { paste("name", '.png', sep='') },
    content = function(file) {
      png(file)
      print(plo())
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




