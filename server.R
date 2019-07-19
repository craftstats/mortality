#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#



# Define server logic required to draw a histogram
options(shiny.sanitize.errors = FALSE)
paises <-c("AUS","ESP","ITA")
names(paises) <- c("Australia", "España", "Italia")
links <- list(Poisson = "log", Binomial = "logit")
lll<- readRDS("idemo")
llll<-StMoMoData(lll)
jjj <- readRDS("modelopru")

server <-function(input, output, session) {
  observe_helpers(withMathJax = TRUE)
  
  
  HMD <- reactiveValues()
  bases <- reactiveValues(memoria = list(), actual = NULL, selected = NULL)
  HMD[["España"]] <- lll
 # observe({bases$memoria[["España_female"]]<-llll})
  modelos <- reactiveValues(memoria = list(), selected = NULL, actual = NULL)
  model2 <- reactiveValues(descri = list())
  
  
  
  
  
  
  ## pantalla inicial --------------------------------------------------------  
  output$inicial <- renderUI({
    if (is.null(input$input_type)) return()
      switch(input$input_type,
          "hmd" = div(
            box(status = "primary", solidHeader = TRUE, width=4,
            textInput("usuario", "Usuario", "rebeldatalab@gmail.com"),
            textInput("passw", "Contraseña", "1562189576"),
            selectInput("pais", "Pais", choices = paises, selected = "ESP", selectize = TRUE),
            prettyRadioButtons(inputId = "serieXXX", label = "Choose serie:", choices = c("female", "male", "total"), 
                               icon = icon("check"),bigger = TRUE,status = "info",inline = TRUE),
            actionButton(inputId = "carga",label = "Cargar")
            ),
            box(status = "primary", solidHeader = TRUE,width = 4,
                #withSpinner(
                  verbatimTextOutput("summary_hmd")
                 # )
              )
          ),
            
         "archivo" =  div(
                   box(status = "primary", solidHeader = TRUE, width=4,  
                      fileInput("file1", "Archivo con la matriz de ratios",
                                multiple = FALSE,
                                accept = c(".xlsx")
                               ),
                          selectInput("hoja1", "Nombre de la hoja", choices = ""),
                          fileInput("file2", "Hoja con la matriz de población",
                                     multiple = FALSE,
                                     accept = c(".xlsx")
                          ),
                          selectInput("hoja2", "Nombre de la hoja", choices = ""),
                          textInput("newtext", label = "Pais", value = "Misdatos"),
                          selectizeInput("newserie", label = "Tipo de serie", choices = c("female", "male", "total"), 
                                         selected = NULL, multiple = FALSE, options= list(create = TRUE)),
                         actionBttn(inputId = "archicarga", label = "Cargar")
                       ),
                   box(status = "primary", solidHeader = TRUE, width=4,
                          #withSpinner(
                            verbatimTextOutput("summary_exc")
                            #)
                          )
                  )
        )
  })
  

# Lectura from archivo excel ----------------------------------------------

observe({
  req(input$file1)
  updateSelectInput(session, "hoja1", choices = sheets_name(input$file1))
})
observe({
req(input$file2)  
  updateSelectInput(session, "hoja2", choices = sheets_name(input$file2))    
  })

 flag_exc <- eventReactive(input$archicarga, {
    if (!is.null(input$file1) & !is.null(input$file2) & (trimws(input$newtext)!="") & (trimws(input$newserie)!="")) {
      mat_ratios <- read_excel(input$file1$datapath, sheet = input$hoja1)
      mat_pop <- read_excel(input$file2$datapath, sheet = input$hoja2)
      tryCatch({
        
        objeto <- demogdata(
          data = mat_ratios[, -1], pop = mat_pop[, -1]*100000, 
          ages = as.numeric(gsub("+","",unlist(mat_ratios[,1]), fixed = T)),
          years = as.numeric(colnames(mat_ratios)[-1]),
          label = trimws(input$newtext), name = trimws(input$newserie), 
          type = "mortality", lambda = 0
        )
        e0(objeto)  # para comprobar que realmente es un objeto demography 
        nombre <- paste(input$newtext, input$newserie, sep ="_")
         bases$memoria[[nombre]] <- StMoMoData(objeto)
         bases$actual  <- bases$memoria[[nombre]]
         return(TRUE)
      },
        error = function(err) {
        shinyalert("¡No se pudo construir un objeto demography!", "Revisa que las hojas de excel sean válidas y tengan formato correcto", type = "error")   
        return(FALSE)
      }) 
    } else {
      shinyalert("¡No puedes cargar todavía!", "Revisa lo que te falta por introducir", type = "error")
      return(FALSE)
    }
    
  })
  
 output$summary_exc <- renderPrint({
    if (req(flag_exc())) isolate(bases$actual)
  })
  
# lectura hmd -------------------------------------------------------------

  flag_hmd <- eventReactive(input$carga, {
    
    pais <- key_pais(paises, input$pais)
    nombre <- paste(pais, input$serieXXX, sep ="_")
    
    if (!(pais %in% names(HMD))) {
      tryCatch({
        withProgress(
          HMD[[pais]] <-hmd.mx2(country=input$pais, username=input$usuario, password=input$passw, label= pais),
          message = "Cargando datos", min = 0, max = 100, val = 30
                      )
         bases$memoria[[nombre]] <- StMoMoData(HMD[[pais]], series = input$serieXXX)
         bases$actual <-  bases$memoria[[nombre]]
        return(TRUE)                                
        
      },
        error = function(err) {
        shinyalert("¡Fallo de lectura!", "No pudimos conectar con la base de datos.\n Revisa tu usuario y contraseña.", type = "error")   
        return(FALSE)
      }) 
    } else
      {
        if (!(nombre %in% names(bases))) {
          bases$memoria[[nombre]] <- StMoMoData(HMD[[pais]], series = input$serieXXX)}
           bases$actual <- bases$memoria[[nombre]]
           return(TRUE)
        
      }  
  })  
  
 output$summary_hmd  <- renderPrint({
   req(flag_hmd())
   if (is.null(bases$actual)) cat("Puedes cargar más datos")
   else 
   bases$actual
 })

# Tabla de datos --------------------------------------------------------

 output$tablebases <- renderDT({
   if (length(bases$memoria)!=0)
   datatable(create_tabla_bases(bases$memoria))
   else NULL
 })
 
 output$lista_exluidos <- renderTable({
   if (length(bases$memoria)!=0)
   create_tabla_bases(bases$memoria)$Nombre[input$tablebases_rows_selected]
   else NULL
 })
 
 observeEvent(input$borrar,{
   if (!is.null(input$tablebases_rows_selected)) {
     bases$memoria <- bases$memoria[-input$tablebases_rows_selected]
     bases$selected <- bases$memoria
     bases$actual <- NULL
   }
 })
 
 observe({
   req(input$tablebases_rows_selected)
   bases$selected <- bases$memoria[-input$tablebases_rows_selected]
 })
 
  
#------------------------------------------------------------------------------
  

# Página descriptivos -----------------------------------------------------

output$descri <- renderUI({
  if (is.null(input$tablebases_rows_selected)) {bases$selected <- bases$memoria}
  else {bases$selected <- bases$memoria[-input$tablebases_rows_selected]
  }
  run3(bases, descriptivos_server)
  deploy3(bases$selected, descriptivos_UI)
  
  })

# Página lifetables -------------------------------------------------------

  
  
  output$lifetables <- renderUI({
    if (is.null(input$tablebases_rows_selected)) {bases$selected <- bases$memoria}
    else {bases$selected <- bases$memoria[-input$tablebases_rows_selected]}
    run3(bases, lifetables_server)
    deploy3(bases$selected, lifetables_UI)
    
  })
  
  

# Definir modelo ----------------------------------------------------------

    output$create_model <- renderUI({
       mi<-0
      mx <-100
      req(bases$memoria)

      div(
        selectInput("basemodelo", "Datos", choices = names(bases$memoria),  selectize = FALSE),
        helper(
          radioGroupButtons(inputId = "modelo", label = "Choose class of model", choices = c("LC", "CBD", "APC", "RH", "M6","M7","M8", "PLAT"),
                            justified = TRUE, status = "primary",
                            checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                             no = icon("remove",
                                                       lib = "glyphicon")))
              ,type = "markdown", content = "models", size="l") ,
        prettyRadioButtons(inputId = "link", label = "Choose type of error", choices = links,
                               icon = icon("check"),bigger = TRUE,status = "info",inline = TRUE),
        sliderInput("anosmodelo", "Años", min = mi, max = mx , value = c(mi,mx), step = 1),
        sliderInput("edadmodelo", "Edad", min = mi, max = mx , value = c(mi,mx), step = 1),
        numericInput("clipmodelo", label = "Nº de cohortes en los extremos con peso 0", min = 0, value = 0, step = 1),
        uiOutput("extra_param"),
        textInput("namemodelo",label= "Nombre del modelo"),
        actionBttn(inputId = "runmodel", label = "Run model", icon = icon("gear"), style = "simple", color = "success")
      )
    })
  
      observe( {
        req(input$basemodelo)
        val <- input$basemodelo
        miy <- min(bases$memoria[[val]]$year)
        mxy <- max(bases$memoria[[val]]$year)
        mia <- min(bases$memoria[[val]]$age)
        mxa <- max(bases$memoria[[val]]$age)
        vxa <- min(mxa, 100)
        updatePrettyRadioButtons(session, "link", selected = 
                                 ifelse(input$modelo %in% c("CBD", "PLAT", "M6", "M7", "M8"), "logit", "log"))
        updateSliderInput(session, "anosmodelo", min = miy, max = mxy , value = c(miy,mxy), step = 1)
        updateSliderInput(session, "edadmodelo", min = mia, max = mxa , value = c(mia,vxa), step = 1)

        updateTextInput(session, "namemodelo", value = paste0("Mod", input$runmodel, input$modelo))
      })
    
  
      output$extra_param <- renderUI({
        req(input$modelo)
        switch(input$modelo,
               "LC" = radioGroupButtons(inputId = "const", label = "Constraint to impose", choices = c("sum", "last", "first"),
                                        justified = TRUE,  status = "warning", checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                                                                no = icon("remove",
                                                                                                          lib = "glyphicon"))),
               "M8" = numericInput(inputId = "xc", label = "Cohort age modulating parameter", value = floor((input$anosmodelo[1] + input$anosmodelo[2])/2)),
               "RH" = div(
                        radioGroupButtons(inputId = "cohortAgeFun", label = "Cohort age modulating parameter", choices = c("NP", "1"),
                                   justified = TRUE, status = "warning", checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                                      no = icon("remove",
                                                                                lib = "glyphicon"))),
                        prettyToggle(inputId = "approxConst", label_on = "Aplicar constraint of Hunt and Villegas (2015)", icon_on = icon("check"),
                             status_on = "info", status_off = "warning", label_off = "Aplicar constraint of Hunt and Villegas (2015)",
                            icon_off = icon("remove"), value = FALSE),
                        prettyToggle(inputId = "LCfirst", label_on = "Utilizar LC para los valores iniciales", icon_on = icon("check"),
                                     status_on = "info", status_off = "warning", label_off = "Utilizar LC para los valores iniciales",
                                     icon_off = icon("remove"), value = FALSE)
                         )
              )


      })
    

# Construir y mostrar modelo ----------------------------------------------

      flag_mod <- eventReactive(input$runmodel, {
          nombre <- trimws(input$namemodelo)
          const <- ifelse(!(input$modelo == "LC"), "", input$const)
           cohortAgeFun <-ifelse(!(input$modelo == "RH"), "",input$cohortAgeFun)
           approxConst <- ifelse(!(input$modelo == "RH"), "", input$approxConst)
           LCfirst <- ifelse(!(input$modelo == "RH"), FALSE, input$LCfirst)
           xc <- ifelse(!(input$modelo == "M8"), "", input$xc)
        
        anos <- seq(input$anosmodelo[1], input$anosmodelo[2], 1)
        ages <- seq(input$edadmodelo[1], input$edadmodelo[2], 1)
        tryCatch({ 
        
          auxi  <- suppressWarnings(create_model(input$modelo, bases$memoria[[input$basemodelo]], 
                                                  input$link,  anos, ages, input$clipmodelo, const, 
                                                cohortAgeFun, approxConst, LCfirst , xc))
          
          if (auxi$fail) {shinyalert("¡Fallo de ejecución!", "No pudimos calcular el modelo. Prueba cambiando los parámetros", type = "error")
            return(FALSE)
            
          }else{
          
            modelos$memoria[[nombre]] <- auxi
          modelos$actual <- modelos$memoria[[nombre]]
          model2$descri[[nombre]] <- list(Nombre = nombre,
                                           Datos = input$basemodelo,
                                           Modelo = input$modelo,
                                           Link = key_pais(links, input$link),
                                           Anos = paste0(input$anosmodelo[1],"-", input$anosmodelo[2]),
                                           Edades = paste0(input$edadmodelo[1], "-", input$edadmodelo[2]),
                                           coho =  paste0(min(modelos$actual$cohorts),"-", max(modelos$actual$cohorts)),
                                           formu =  modelos$actual$model$textFormula, 
                                          const= paste0(const, cohortAgeFun, approxConst, xc),
                                          nparam = modelos$actual$npar,
                                          aic = AIC(modelos$actual),
                                          bic = BIC(modelos$actual)
                                         ) 
          return(TRUE)
          }
         },
         error = function(err) {
           shinyalert("¡Fallo de ejecución!", "No pudimos calcular el modelo. Prueba cambiando los parámetros", type = "error")
          return(FALSE)
         })
    })

        output$show_models <- renderPrint({
          req(flag_mod())
          #nombre <- trimws(input$namemodelo)
          modelos$actual
        })
    

# Tabla modelos -----------------------------------------------------------

        output$tablemodelos <- renderDT({
          if (!is.null(modelos$memoria))
          datatable(create_tabla_modelos(model2$descri)) %>% 
          formatRound(11:12,1) 
        })
        
        output$modelos_exluidos <- renderTable({
          if (!is.null(modelos$memoria))
          create_tabla_modelos(model2$descri)$Nombre[input$tablemodelos_rows_selected]
        })

        observeEvent(input$borrar_mod,{
          if (!is.null(input$tablemodelos_rows_selected)) {
            modelos$memoria <- modelos$memoria[-input$tablemodelos_rows_selected]
            model2$descri <- model2$descri[-input$tablemodelos_rows_selected]
            modelos$selected <- modelos$memoria
            modelos$actual <- NULL
          }
        })
        
        observe({
          req(input$tablemodelos_rows_selected)
          modelos$selected <- modelos$memoria[-input$tablemodelos_rows_selected]
        })
        
       

# Página gráficos modelos -------------------------------------------------

        
        

        output$modelooutput <- renderUI({
          if (is.null(input$tablemodelos_rows_selected)) {modelos$selected <- modelos$memoria}
          else {modelos$selected <- modelos$memoria[-input$tablemodelos_rows_selected]}
          run3(modelos, modelos_server)
          deploy3(modelos$selected, modelos_UI)

        })


 
#         tabPanel(
#           title = "Table",
#           div(DT::dataTableOutput("table"),style = "font-size: 85%"),
#           div(
#             #style = "position: absolute; left: 0.5em; bottom: 0.5em;",
#             dropdown(
#               sliderInput("ano3", "Año", min = mi, max = mx , value = mx, step = 1),
#               size = "xs",
#               icon = icon("gear", class = "opt"),
#               up = TRUE
#             )
#           )
#            
#           
#         )
# 
#       
#       )  
#   })
# 
#   
#   
# plooo<- eventReactive(input$ano2,{
#     years = seq(input$ano2[[1]], input$ano2[[2]], 1)
#     p<-plot(lifetable(bases[[input$pais3]], years=years))
#     p
#     })
# 
# output$plot_life1 <- renderPlot({
#    plooo()
#   })
#   
#   observeEvent((input$zoom), {
#     showModal(modalDialog(
#       renderPlot({
#         years = seq(input$ano2[[1]], input$ano2[[2]], 1)
#         p<-plot(lifetable(bases[[input$pais3]], years=years))
#         p
#       }, height = 600),
#       easyClose = TRUE,
#       size = "l",
#       footer = NULL
#     ))
#   })
#   
#   output$table <- renderDT({
#     req(input$ano3)
#     auxi <-lifetable(bases[[input$pais3]], years=input$ano3)
#     ltable <- with(auxi, data.frame(mx = mx[-1], qx = qx[-1], lx = lx[-1], dx = dx[-1], Lx =  Lx[-1], Tx =  Tx[-1], ex = ex[-1]))
#     pp<-datatable(ltable, class = "display compact", extensions = c("Buttons", "Scroller"), options = list(
#                                  dom = 'Bt',
#                                  scrollY = 400,
#                                  scroller=TRUE,
#                                  buttons = list(list(extend = "collection",
#                                                      text = 'Download',
#                                                      action = DT::JS("function ( e, dt, node, config ) {
#                                     Shiny.setInputValue('test', true, {priority: 'event'});}")))))
#     formatRound(pp, 1:7,3)          
# 
#   })
#   
#   

#   

#   
#   

#    
#  
#   

# 
#  
#   
#   
#   output$modeloutput <- renderUI({
#     req(sal_mod())
#     type_rate <- "g"
#     div(
#       
#       tabBox(width = 4, title = paste(input$mod_name, "Fitted"),
#             tabPanel(title = "Plot(cohort)",
#                      sliderInput("coho1", "Choose cohort:", min = min(sal_mod()$years), max = max(sal_mod()$years),
#                                                             value = max(sal_mod()$years)),
#                      renderPlot(creaplot(sal_mod(), "ages", type_rate, input$coho1))
#             ),
#             tabPanel(title = "Plot(age)",
#                      sliderInput("age11", "Choose age:", min = min(sal_mod()$ages), max = max(sal_mod()$ages),
#                                  value = max(sal_mod()$ages)),
#                      renderPlot(creaplot(sal_mod(), "years", type_rate, input$age11))
#             )
#             
#              
#       ),
#       tabBox(width = 4, title = paste(input$mod_name, "Parameters"),
#             tabPanel(title = "Plots",
#                 renderPlot(plot(sal_mod(), parametricbx = FALSE)),
#                 div(
#                   style = "position:absolute;right:0.5em;bottom: 0.5em;",
#                   actionBttn(inputId = "zoom2", icon = icon("search-plus", class = "opt"),
#                                    style = "fill", color = "danger", size = "xs")
#                 )
#           
#             ),
#             tabPanel(title = "Age parameters",
#                         DT::dataTableOutput("table2")
#             ),
#             tabPanel(title = "Cohort parameters",
#                      DT::dataTableOutput("table3")
#             )
#         
#       ),
#       tabBox(width = 4, title = paste(input$mod_name, "residuals"),
#             tabPanel(title = "Plot", 
#                 renderPlot(plot(residuals(sal_mod()), type="scatter"))
#             ),
#             tabPanel(title = "Heatmap",
#                 renderPlot(plot(residuals(sal_mod()), type = "colourmap"))
#             ),
#             tabPanel(title = "Table",
#                 actionBttn(inputId = "showtable")     
#             )
#             
#       )
#     )
#     
#   })
#   observeEvent(input$showtable,{
#     table <- residuals(req(sal_mod()))$residuals
#     showModal(modalDialog(
#       renderDT(datatable(table), height = 600),
#       easyClose = TRUE,
#       size = "l",
#       footer = NULL
#     ))
#     
#     
#   })
#   
#   
#   observeEvent((input$zoom2), {
#     showModal(modalDialog(
#         renderPlot(plot(sal_mod(), parametricbx = FALSE), height = 600),
#         easyClose = TRUE,
#         size = "l",
#         footer = NULL
#     ))
#   })
#   output$table2 <- renderDT({
#     req(sal_mod())
#     cb_coef_age(sal_mod()) %>% datatable() %>% 
#     formatRound(1:7,3)          
#     
#   })
#  
#   
#   
#   
#   output$create_forecast <- renderUI({
#     h <- 1
#     req(sal_mod())
#     
#     div( 
#       selectInput("modelofore", "Pais", choices = input$mod_name,  selectize = FALSE),
#       prettyRadioButtons(inputId = "serie2", label = "Choose serie:", choices = c("female", "male", "total"), 
#                          icon = icon("check"),bigger = TRUE,status = "info",inline = TRUE),
#       
#       helper( 
#         radioGroupButtons(inputId = "modelo", label = "Choose class of model", choices = c("LC", "CBD", "APC", "RH", "M6","M7","M8", "PLAT"), 
#                           justified = TRUE, checkIcon = list(yes = icon("ok", lib = "glyphicon")))
#         ,type = "markdown", content = "models", size="l") ,
#       prettyRadioButtons(inputId = "link", label = "Choose type of error", choices = links, 
#                          icon = icon("check"),bigger = TRUE,status = "info",inline = TRUE),
#       sliderInput("ano4", "Años", min = mi, max = mx , value = c(mi,mx), step = 1),
#       sliderInput("edad3", "Edad", min = mi, max = mx , value = c(mi,mx), step = 1),
#       uiOutput("extra_param"),
#       textInput("mod_name", ""),
#       
#       
#       actionBttn(inputId = "runmodel", label = "Run model", icon = icon("gear"), style = "simple", color = "success")
#       
#       
#       
#     )
#     
#   })
#   
#   
#   
  
  
    
  
}