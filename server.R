#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

source(file = "utils.R")
library(shiny)
library(shinydashboard)
library(demography)
library(StMoMo)
library(shinyalert)
library(shinycssloaders)
library(shinyWidgets)
library(plotly)
library(DT)


# Define server logic required to draw a histogram

paises <-c("AUS","ESP","ITA")
names(paises) <- c("Australia", "España", "Italia")
links <- list(Poisson = "log", Binomial = "logit")
lll<- readRDS("idemo")


server <-function(input, output, session) {
  
  bases<-list()
  bases[["España"]] <- lll
  modelos <- list()
  observe_helpers(withMathJax = TRUE)
  
  output$ui <- renderUI({
    if (is.null(input$input_type))
    return()
  
  switch(input$input_type,
          "hmd" = div(
            textInput("usuario", "Usuario", "rebeldatalab@gmail.com"),
            textInput("passw", "Contraseña", "1562189576"),
            selectInput("pais", "Pais", choices = paises, selected = "ESP", selectize = TRUE),
            actionButton("carga","Cargar")
          ),
         "archivo" =  fileInput("file1", "Choose CSV File",
                                multiple = FALSE,
                                accept = c("text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv")
          )
        )
       
  })
  
  observe({
    if (input$menu == "menu_life") {
      updateSelectInput(session, inputId = "pais3", choices = names(bases), label ="Pais")
    }
  })
  
  output$ui2 <- renderUI({
      if (is.null(pais()))
        return()
    
    
      selectInput("pais2", "Pais", choices = names(bases), selectize = FALSE)
  })
  
  output$uislider1 <- renderUI({
    req(input$pais2)
    mi <- min(bases[[input$pais2]]$year)
    mx <- max(bases[[input$pais2]]$year)
    
     sliderInput("anos", "Años", min = mi, max = mx , value = c(mi,mx), step = 1)
  })
  
  output$uislider2 <- renderUI({
    req(input$pais2)
    mi <- min(bases[[input$pais2]]$age)
    mx <- max(bases[[input$pais2]]$age)
    
    sliderInput("edad", "Edad", min = mi, max = mx , value = c(mi,mx), step = 1)
  })
  
  
  
  pais <- eventReactive(input$carga, {
    key_pais(paises, input$pais)
  })
  

  flag_lectura <- eventReactive(input$carga, {
    
    if (!(pais() %in% names(bases))) {
  
      tryCatch({
        
        bases[[pais()]] <<-  hmd.mx2(country=input$pais, username=input$usuario, password=input$passw, label= pais())
        return(TRUE)
        },
        error = function(err) {
        shinyalert("¡Fallo de lectura!", "No pudimos conectar con la base de datos.\n Revisa tu usuario y contraseña.", type = "error")   
        return(FALSE)
        }
      )    
    }
  })  
  
 
 # output$cargadas <- renderText({
 #    paste("la longitud de ", pais(), names(bases))
 #  })
 
 
 output$summary  <- renderPrint({
   req(flag_lectura())
       auxi <- StMoMoData(bases[[pais()]])
  auxi
 })
  
  
 output$plot1<- renderPlot({
    req(input$anos)
    auxi<- bases[[input$pais2]]
    p <- plot(auxi, transform = input$transf, 
         years = seq(input$anos[[1]], input$anos[[2]], 1),
         ages = seq(input$edad[[1]], input$edad[[2]], 1),
         plot.type="function")
    p
  })
  

  
  output$boxdoble<- renderUI({
    req(input$pais3)
    mi <- min(bases[[input$pais3]]$year)
    mx <- max(bases[[input$pais3]]$year)
        tabBox(
        id = "box2",
        tabPanel(
          title = "Plot",
          div(
            dropdown(
                sliderInput("ano2", "Años", min = mi, max = mx , value = c(mi,mx), step = 1)
              ,
              size = "xs",
              icon = icon("gear", class="opt"),
              up = TRUE
            )
          ),
           plotOutput("plot_life1")
          ,
          div(
            actionBttn(
                inputId = "zoom",
                icon = icon("search-plus", class = "opt"),
                style = "fill",
                color = "danger",
                size = "xs"
              )
          )
          #)
        ),
        tabPanel(
          title = "Table",
          div(DT::dataTableOutput("table"),style = "font-size: 85%"),
          div(
            #style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              sliderInput("ano3", "Año", min = mi, max = mx , value = mx, step = 1),
              size = "xs",
              icon = icon("gear", class = "opt"),
              up = TRUE
            )
          )
           
          
        )

      
      )  
  })

  
  
plooo<- eventReactive(input$ano2,{
    years = seq(input$ano2[[1]], input$ano2[[2]], 1)
    p<-plot(lifetable(bases[[input$pais3]], years=years))
    p
    })

output$plot_life1 <- renderPlot({
   plooo()
  })
  
  observeEvent((input$zoom), {
    showModal(modalDialog(
      renderPlot({
        years = seq(input$ano2[[1]], input$ano2[[2]], 1)
        p<-plot(lifetable(bases[[input$pais3]], years=years))
        p
      }, height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  
  output$table <- renderDT({
    req(input$ano3)
    auxi <-lifetable(bases[[input$pais3]], years=input$ano3)
    ltable <- with(auxi, data.frame(mx = mx[-1], qx = qx[-1], lx = lx[-1], dx = dx[-1], Lx =  Lx[-1], Tx =  Tx[-1], ex = ex[-1]))
    pp<-datatable(ltable, class = "display compact", extensions = c("Buttons", "Scroller"), options = list(
                                 dom = 'Bt',
                                 scrollY = 400,
                                 scroller=TRUE,
                                 buttons = list(list(extend = "collection",
                                                     text = 'Download',
                                                     action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test', true, {priority: 'event'});}")))))
    formatRound(pp, 1:7,3)          

  })
  
  
  output$create_model <- renderUI({
     mi<-0
    mx <-100
    req(pais())
    
    div( 
      selectInput("pais4", "Pais", choices = names(bases),  selectize = FALSE),
      prettyRadioButtons(inputId = "serie", label = "Choose serie:", choices = c("female", "male", "total"), 
                                icon = icon("check"),bigger = TRUE,status = "info",inline = TRUE),
      helper( 
        radioGroupButtons(inputId = "modelo", label = "Choose class of model", choices = c("LC", "CBD", "APC", "RH", "M6","M7","M8", "PLAT"), 
                          justified = TRUE, checkIcon = list(yes = icon("ok", lib = "glyphicon")))
            ,type = "markdown", content = "models", size="l") ,
      prettyRadioButtons(inputId = "link", label = "Choose type of error", choices = links, 
                             icon = icon("check"),bigger = TRUE,status = "info",inline = TRUE),
      sliderInput("ano4", "Años", min = mi, max = mx , value = c(mi,mx), step = 1),
      sliderInput("edad3", "Edad", min = mi, max = mx , value = c(mi,mx), step = 1),
      uiOutput("extra_param"),
      textInput("mod_name", ""),
      
      
      actionBttn(inputId = "runmodel", label = "Run model", icon = icon("gear"), style = "simple", color = "success")
      
        
      
    )
   
  })
  
  output$extra_param <- renderUI({
    req(input$modelo)
    switch(input$modelo,
           "LC" = radioGroupButtons(inputId = "const", label = "Constraint to impose", choices = c("sum", "last", "first"), 
                                    justified = TRUE, checkIcon = list(yes = icon("ok", lib = "glyphicon"))),
           "M8" = numericInput(inputId = "xc", label = "Cohort age modulating parameter", value = floor((input$ano4[1] + input$ano4[2])/2)),
           "RH" = div(
                    radioGroupButtons(inputId = "cohortAgeFun", label = "Cohort age modulating parameter", choices = c("1", "NP"), 
                               justified = TRUE, checkIcon = list(yes = icon("ok", lib = "glyphicon"))),
                    prettyToggle(inputId = "approxConst", label_on = "Constraint of Hunt and Villegas (2015) applied", icon_on = icon("check"),
                         status_on = "info", status_off = "warning", label_off = "Constraint of Hunt and Villegas (2015) not applied",  
                        icon_off = icon("remove"), value = FALSE)
                     )
          ) 
    
    
  })
  
  
  observe( {
    req(input$pais4)
    val <- input$pais4 
    miy <- min(bases[[val]]$year)
    mxy <- max(bases[[val]]$year)
    mia <- min(bases[[val]]$age)
    mxa <- max(bases[[val]]$age)
    
    updateSliderInput(session, "ano4", min = miy, max = mxy , value = c(miy,mxy), step = 1)
    updateSliderInput(session, "edad3", min = mia, max = mxa , value = c(mia,mxa), step = 1)
    
    updateTextInput(session, "mod_name", value = paste("Mod", input$runmodel, input$pais4, input$serie, input$modelo, input$link))
  })
   
 
  
  sal_mod <- eventReactive((input$runmodel), {
    
    const <- ifelse(is.null(input$const), "sum", input$const)
    cohortAgeFun <-ifelse(is.null(input$cohortAgeFun), "1",input$cohortAgeFun) 
    approxConst <- ifelse(is.null(input$approxConst), TRUE, input$approxConst)
    xc <- ifelse(is.null(input$xc), 1900, input$xc)
    
    
    
    if (input$link == "logit") {type <- "initial"}
    else {type <- "central"}
    
    data <- StMoMoData(bases[[input$pais4]], series = input$serie, type = type) 
    
  anos <- seq(input$ano4[[1]], input$ano4[[2]], 1)
  ages <- seq(input$edad3[[1]], input$edad3[[2]], 1)
      auxi <- create_model(input$modelo, data, input$link, anos, ages, const, cohortAgeFun, approxConst, xc)
        auxi
       
  })
  
  output$show_models <- renderPrint({
    req(sal_mod())
    sal_mod()
  })     

 
  
  
  output$modeloutput <- renderUI({
    req(sal_mod())
    div(
      tabBox(width = 4, title = paste(input$mod_name, "Parameters"),
            tabPanel(title = "Plots",
                renderPlot(plot(sal_mod(), parametricbx = FALSE)),
                div(
                  style = "position:absolute;right:0.5em;bottom: 0.5em;",
                  actionBttn(inputId = "zoom2", icon = icon("search-plus", class = "opt"),
                                   style = "fill", color = "danger", size = "xs")
                )
          
            ),
            tabPanel(title = "Age parameters",
                        DT::dataTableOutput("table2")
            ),
            tabPanel(title = "Cohort parameters",
                     DT::dataTableOutput("table3")
            )
        
      ),
      tabBox(width = 4, title = paste(input$mod_name, "residuals"),
            tabPanel(title = "Plot", 
                renderPlot(plot(residuals(sal_mod()), type="scatter"))
            ),
            tabPanel(title = "Heatmap",
                renderPlot(plot(residuals(sal_mod()), type = "colourmap"))
            ),
            tabPanel(title = "Table",
                actionBttn(inputId = "showtable")     
            )
            
      )
    )
    
  })
  observeEvent(input$showtable,{
    table <- residuals(req(sal_mod()))$residuals
    showModal(modalDialog(
      renderDT(datatable(table), height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
    
    
  })
  
  
  observeEvent((input$zoom2), {
    showModal(modalDialog(
        renderPlot(plot(sal_mod(), parametricbx = FALSE), height = 600),
        easyClose = TRUE,
        size = "l",
        footer = NULL
    ))
  })
  output$table2 <- renderDT({
    req(sal_mod())
    cb_coef_age(sal_mod()) %>% datatable() %>% 
    formatRound(1:7,3)          
    
  })
   
  
}