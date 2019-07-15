library(shinyWidgets)

deploy3 <- function(lista, fUI) {
  
  if (length(lista) >0) {
    tagList(map(names(lista), ~ fUI(.x, .x)))
    
  }
  
}

run3 <- function(bas, fserver) {
  if (length(bas$selected) >0) {
    map(names(bas$selected), ~ callModule(fserver, .x, .x, bas))
    }
}
# El modulo UI general para todas las funciones creadoras

plotUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("plot"))
}


# distintos tipos de funciones para crear las salidas gráficas --------------------
salida1 <- function(input, output, session, name , bas , typeplot) {
  ns <- session$ns
  lista <- bas$selected
  mia <- min(lista[[name]]$year)
  mxa <- max(lista[[name]]$year)
  mie <- min(lista[[name]]$age)
  mxe <- max(lista[[name]]$age)
  datos <- st2demo(lista[[name]])
  pp<- reactive({
           plot(datos, ages=seq(input$edad[1], input$edad[2], 1), 
              years=seq(input$anos[1], input$anos[2], 1),
              plot.type = typeplot)
  })
  
  output$plot <- renderUI({
    div(
      dropdownButton( 
            sliderInput(ns("anos"), "Cohorte", min = mia, max = mxa , value = c(mia,mxa), step = 1),
           sliderInput(ns("edad"), "Edad", min = mie, max = mxe , value = c(mie,mxe), step = 1), 
           icon = icon("gear", class = "opt"),
           size="sm",
           tooltip = tooltipOptions(title = "Opciones"),
           status = "info"
       ),
     
      renderPlot({pp()})
    )
  })
  
  qq<- reactive({
    plot(datos, ages=seq(input$edad[1], input$edad[2], 1), 
              years=seq(input$anos[1], input$anos[2], 1),
              plot.type = typeplot)
  })
  return(qq)
}  




salida2 <- function(input, output, session, name , bas , typeplot) {
  ns <- session$ns
  lista <- bas$selected
  mia <- min(lista[[name]]$year)
  mxa <- max(lista[[name]]$year)
  mie <- min(lista[[name]]$age)
  mxe <- max(lista[[name]]$age)
  datos <- st2demo(lista[[name]])
  
  pp<- reactive({
  years <- seq(input$anos[1], input$anos[2], 1)
  ages <- seq(input$edad[1], input$edad[2], 1)
      plot_life(datos, ages, years, typeplot)
  })
  
  output$plot <- renderUI({
    div(
      dropdownButton( 
        sliderInput(ns("anos"), "Cohorte", min = mia, max = mxa , value = c(mia,mxa), step = 1),
        sliderInput(ns("edad"), "Edad", min = mie, max = mxe , value = c(mie,mxe), step = 1), 
        icon = icon("gear", class = "opt"),
        size="sm",
        tooltip = tooltipOptions(title = "Opciones"),
        status = "info"
      ),
      
      renderPlot({pp()})
    )
  })

    return(pp)
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
  callModule(plot_con_opciones, "age", salida1, name, bas, typeplot = "functions") 
  callModule(plot_con_opciones, "coho", salida1,  name, bas, typeplot = "time")
}


# Modulo lifetables ( 3 boxes) ------------------------------------------

lifetables_UI <- function(id, title) {
  ns <- NS(id)
  fluidRow(
    h2(title),
    box(width = 6, title = "Esperanza respecto a edad", collapsible = TRUE,
        plot_con_opciones_UI(ns("agelt"))),
    box(width = 6, title = "Esperanza respecto a cohorte", collapsible = TRUE, 
        plot_con_opciones_UI(ns("coholt")))
  )
}

lifetables_server <- function(input, output, session, name, bas) {
  ns <- session$ns
  callModule(plot_con_opciones, "agelt", salida2, name, bas, typeplot = "ages") 
  callModule(plot_con_opciones, "coholt", salida2,  name, bas, typeplot = "cohos")
}


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

plot_con_opciones <- function(input, output, session, tiposalida, name, bas, ...) {
  ns <- session$ns
  plo <- callModule(tiposalida,"salida", name, bas, ...)
 
 
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



  
  
  
 
 
  
  

