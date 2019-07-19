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
        renderPlot({plo()}, height = 720),
        easyClose = TRUE, size = "l", footer = NULL
      ))
  }
  )
  
  output$down <- downloadHandler(
    filename = function() { paste("name", '.png', sep='') },
    content = function(file) {
      png(file, width = 720, height = 720)
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
         plot.type = typeplot, transform = input$log)
  })
  
  output$plot <- renderUI({
    div(
      dropdown( 
        sliderInput(ns("anos"), "Years", min = mia, max = mxa , value = c(mia,mxa), step = 1),
        sliderInput(ns("edad"), "Edad", min = mie, max = mxe , value = c(mie,mxe), step = 1), 
        prettyToggle(ns("log"), label_on = "Tansformación logarítmica", label_off = "Transformación logarítmica",
                     value = TRUE, status_on = "success", status_off = "warning", icon_on = icon("check"), icon_off = icon("remove")),
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
         plot.type = typeplot, transform = input$log)
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
      dropdown( 
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
  
 lim <- reactive({
   if (is.null(input$reslim)) lim <- 3.5
   else lim <- input$reslim
   lim
 })
   
  output$plot <- renderUI({
    div(
      dropdown(
        sliderInput(ns("reslim"), "Residuals limit", min = 1, max = 15, value = 3.5, step =0.5),
        icon = icon("gear", class = "opt"),
        size="sm",
        tooltip = tooltipOptions(title = "Opciones"),
        status = "info"
      ),
       
       renderPlot({ plot(residuals(lista[[name]]), type = typeplot, reslim = c(-lim(), lim()))})
      )
  })
  
  qq<- reactive({
    plot(residuals(lista[[name]]), type = typeplot, reslim = c(-lim(), lim()))
  })
  return(qq)
}  

salida4 <- function(input, output, session, name , bas) {
  ns <- session$ns
  lista <- bas$selected
  
  output$plot <- renderUI({
    div(
      dropdown(
        prettyToggle(ns("showall"), label_on = "Muetra todos", 
                     label_off = "Muetra todos", value = FALSE, icon_on = icon("check"),
                     status_on = "info", status_off = "warning", icon_off = icon("remove")),
        icon = icon("gear", class = "opt"),
       size="sm",
       tooltip = tooltipOptions(title = "Opciones"),
       status = "info"
  ),
    renderPlot({plot(lista[[name]], parametricbx = input$showall)}))
  })
  
  qq<- reactive({
    plot(lista[[name]], parametricbx = input$showall)
  })
  return(qq)
}  


salida5 <- function(input, output, session, name , bas, bytype) {
  ns <- session$ns
  base <- bas$selected[[name]]
  if (bytype == "ages") {
    mi <- min(base$years)
    ma <- max(base$years)
    val <- 2000
    text <- "Year"
    
  } else {
    mi <- min(base$ages)
    ma <- min(max(base$ages))
    val <- 65
    text <- "Age"
  }   
  
  output$slider <- renderUI({
    sliderInput(ns("filter"), text, min = mi, max = ma , value = val, step = 1)
  })    
  
  pp <- reactive({
    req(input$filter)
    plot_fitted(base, bytype, input$type, input$filter, input$inter)
    
    
  })
  
  output$plot <- renderUI({
    div(
      dropdown(
        uiOutput(ns("slider")),
        awesomeRadio(ns("type"), label = "Medida", choices = c("Rates", "LogRates", "Deaths"), 
                  inline = TRUE, checkbox = TRUE),  
        prettyToggle(ns("inter"), label_on = "Plot interactivo", 
                     label_off = "Plot interactivo", value = FALSE, icon_on = icon("check"),
                     status_on = "info", status_off = "warning", icon_off = icon("remove")),
        icon = icon("gear", class = "opt"),
        size="sm",
        tooltip = tooltipOptions(title = paste0("Choose ", text, " y otras opciones")),
        status = "info"
       ),
       uiOutput(ns("ppp"))
      )
  })
  
  output$ppp <- renderUI({
    #if (is.null(input$inter)) inter <- FALSE
    #else inter <- input$inter
    
    if (input$inter) div(renderPlotly({pp()}))
    else div(renderPlot({pp()}))
    
  })
  
 
  return(pp)
}  




