tabla_opciones_UI <- function(id){
  ns <- NS(id)
  div(
    tablaUI(ns("salida")),
    fluidRow(
      column(width=4),
      column(width=4,
             downloadBttn(outputId = ns("down"), label = "Download", size = "xs")
      )
    )
  )
} 

tabla_opciones_server <- function(input, output, session, tiposalida, name, bas, nombre,  ...) {
  ns <- session$ns
  tab <- callModule(tiposalida,"salida", name, bas, ...)
  
  output$down <- downloadHandler(
    filename = function() {
      paste(nombre, ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(tab(), path = file)
    }
  )
  
} 

tablaUI <- function(id) {
  ns <- NS(id)
     uiOutput(ns("tabla"))
}

tablapar <- function(input, output, session, name , bas , type) {
  ns <- session$ns
  model <- bas$selected[[name]]
  if (type == "age") {datos <- coef_age(model)}
  else { if (type == "year") datos <- coef_year(model) 
          else datos <- coef_coho(model)}
  tabla <- reactive({
    datos          
  })
  output$tabla <- renderUI({
    div(renderDT({
       datatable(datos, extensions = c("Scroller"), class = "display compact",
              options = list(dom = 't', scroller=TRUE, scrollY = 400, scrollX = 100)) %>% 
              formatRound(2:7,3)
      })
    )
    })
  return(tabla)
  
}  

tabladata <- function(input, output, session, name , bas , type) {
  ns <- session$ns
  base <- bas$selected[[name]]
  if (type == "rate") {datos <- base$Dxt/base$Ext}
  else { if (type == "death") datos <- base$Dxt 
  else datos <- base$Ext}
  datos <- tibble::rownames_to_column(as.data.frame(datos), var = "Age")
  tabla <- reactive({
    datos          
  })
  output$tabla <- renderUI({
    div(
      renderDT({
        datatable(datos, extensions = c("Scroller"), class = "display compact",
              options = list(dom = 't', scroller=TRUE, scrollY = 400, scrollX = 100)) %>% 
        formatRound(2:150,3)
       })
    )  
  })
  return(tabla)
}  



tablalt <- function(input, output, session, name , bas , type) {
  ns <- session$ns
  base <- st2demo(bas$selected[[name]])
  
   if (type == "Age") {
        mi <- min(base$year)
        ma <- max(base$year)
        val <- ma
        text <- "Year"
        
    } else {
       mi <- min(base$age)
       ma <- min(max(base$age), 100)
       val <- mi
       text <- "Age"
    }   
  output$filter1 <- renderUI({sliderInput(ns("filter"), text, min = mi, max = ma , value = val, step = 1)})    

  tabla <- reactive({
    
    if (type == "Age") {
      if (is.null(input$filter)) year <- ma
      else year <- input$filter
      auxi <- lifetable(base, years= year)
      tab <-with(auxi, data.frame(age = base$age[1: (min(max(base$age), 100) + 1)],
               mx =mx, qx =qx, lx =lx , dx =dx , Lx =Lx , Tx =Tx , ex =ex ))
      colnames(tab) <- c("Age", "mx", "qx", "lx", "dx", "Lx", "Tx", "ex")
      tab
    } else {  
      auxi <- lifetable(base)
      if (is.null(input$filter)) y <- mi + 1
      else  y <- input$filter + 1
      tab <-with(auxi, data.frame(year = base$year, 
              mx = mx[y,], qx = qx[y, ], lx = lx[y, ], dx = dx[y, ], Lx =  Lx[y, ], Tx =  Tx[y, ], ex = ex[y, ]))
      }  
  })  
  
 output$tabla <- renderUI({
    div(
      dropdown( 
        uiOutput(ns("filter1")),
        icon = icon("gear", class = "opt"),
        size="sm",
        status = "info", tooltip = tooltipOptions(title =paste0("Choose ", text))
       ),
      renderDT({
        datatable(tabla(), extensions = c("Scroller"), class = "display compact",
              options = list(dom = 't', scroller=TRUE, scrollY = 400, scrollX = 100)) %>% 
        formatRound(2:150,3)
       })
    )
   })  
  return(tabla)
}  


tablares <- function(input, output, session, name , bas , type) {
  ns <- session$ns
  model <- bas$selected[[name]]
  datos <- residuals(model)
  datos <- as.data.frame(cbind(Age = datos$ages, datos$residuals))
  tabla <- reactive({
    datos          
  })
  output$tabla <- renderUI({
    div(renderDT({
      datatable(datos, extensions = c("Scroller"), class = "display compact",
                options = list(dom = 't', scroller=TRUE, scrollY = 400, scrollX = 100)) %>% 
        formatRound(2:170,3)
    })
    )
  })
  return(tabla)
  
}  


tablafit <- function(input, output, session, name , bas) {
  ns <- session$ns
  model <- bas$selected[[name]]
  tabla <- reactive({
    if (is.null(input$type)) type <- "rates"
    else type <- input$type
    tabla <-  tibble::rownames_to_column(as.data.frame(fitted(model, type)), var = "Age")         
  })
  output$tabla <- renderUI({
     div(
       dropdown( 
         awesomeRadio(ns("type"), "Tipo:", choices = c(Rates = "rates", LogRates = "link", Deaths = "deaths"), 
                      inline = TRUE, checkbox = TRUE),
         icon = icon("gear", class = "opt"),
         size="sm",
         tooltip = tooltipOptions(title = "Opciones"),
         status = "info"
        ),
         renderDT({
             datatable(tabla(), extensions = c("Scroller"), class = "display compact",
                options = list(dom = 't', scroller=TRUE, scrollY = 400, scrollX = 100)) %>% 
             formatRound(2:170,3)
        })
       
     )   
  })
  return(tabla)
  
} 


tablacofore <- function(input, output, session, name , bas , type) {
  ns <- session$ns
  fore <- bas$selected[[name]]
  datos <- coef_fores(fore)
  tabla <- reactive({
    datos          
  })
  output$tabla <- renderUI({
    div(renderDT({
      datatable(datos, extensions = c("Scroller"), class = "display compact",
                options = list(dom = 't', scroller=TRUE, scrollY = 400, scrollX = 100)) %>% 
        formatRound(2:170,3)
    })
    )
  })
  return(tabla)
  
}  
