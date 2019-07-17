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
        sliderInput(ns("anos"), "Years", min = mia, max = mxa , value = c(mia,mxa), step = 1),
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
        sliderInput(ns("anos"), "Years", min = mia, max = mxa , value = c(mia,mxa), step = 1),
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


salida3 <- function(input, output, session, name , bas , typeplot) {
  ns <- session$ns
  lista <- bas$selected
  pp<- reactive({
    plot(residuals(lista[[name]]), type = typeplot, reslim = c(-3.5, 3.5))
  })
  
  output$plot <- renderUI({
    div(renderPlot({pp()}))
  })
  
  qq<- reactive({
    plot(residuals(lista[[name]]), type = typeplot, reslim = c(-3.5, 3.5))
  })
  return(qq)
}  

salida4 <- function(input, output, session, name , bas) {
  ns <- session$ns
  lista <- bas$selected
  
  output$plot <- renderUI({
    div(renderPlot({plot(lista[[name]], parametricbx = FALSE)}))
  })
  
  qq<- reactive({
    plot(lista[[name]], parametricbx = FALSE)
  })
  return(qq)
}  
