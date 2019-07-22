
options(shiny.sanitize.errors = FALSE)
paises <- c("AUS","AUT", "BLR", "BEL", "BGR", "CAN", "CHL", "HRV", "CZE",
"DNK", "EST", "FIN", "FRATNP", "FRACNP", "DEUTNP", "DEUTE", "DEUTW",
"GRC", "HUN", "ISL", "ISR", "ITA", "JPN", "KOR", "LVA", "LTU", "LUX", "NLD",
"NZL_NP", "NZL_MA", "NZL_NM", "NOR", "POL", "PRT", "RUS", "SVK", "SVN",
"ESP", "SWE", "CHE", "TWN", "GBR_NP", "GBRTENW", "GBRCENW", "GBR_SCO",
"GBR_NIR", "USA", "UKR")

names(paises) <- c("Australia", "Austria", "Belarus", "Belgium", "Bulgaria",
                   "Canada", "Chile", "Croatia", "Czechia", "Denmark", 
                   "Estonia", "Finland", "France_total", "France_civilian",
                   "Germany_total", "Germany_east", "Germany_west", 
                   "Greece", "Hungary", "Iceland", "Israel", "Italy", 
                   "Japan", "Korea", "Latvia", "Lithuania", "Luxembourg",
                   "Netherlands", "New Zealand Total", "New Zealand maori",
                   "New Zealand non-maori", "Norway", "Poland", "Portugal",
                   "Russia", "Slovakia", "Slovenia", "Spain", "Sweden",
                   "Switzerland", "Taiwan", "UK total", "England Wales total",
                   "Englan Wales civilian", "Scotland", "Nothern Ireland", 
                   "USA", "Ukraine")
                   
                   
                   
                   
links <- list(Poisson = "log", Binomial = "logit")
spain<- readRDS("idemo")
<<<<<<< HEAD


=======
>>>>>>> 1492abbf3884bc6fdec5ad88b2193c606b8a5f3b

server <-function(input, output, session) {
  observe_helpers(withMathJax = TRUE)

  HMD <- reactiveValues()
  HMD[["Spain"]] <- spain
  bases <- reactiveValues(memoria = list(), actual = NULL, selected = NULL)
 
  modelos <- reactiveValues(memoria = list(), selected = NULL, actual = NULL, descri = list())
 
  fores <- reactiveValues(memoria = list(), actual = NULL, selected = NULL, descri = list(), simu = list())
  
  
 
observeEvent(input$grabar, {
  auxi <- list(bases = bases$memoria, 
               modelos = modelos$memoria,
               modes = modelos$descri,
               fores = fores$memoria,
               foresdes = fores$descri, 
               simu = fores$simu)
  saveRDS(auxi, here("grabados", input$savename))
  shinyjs::removeClass(selector = "body.sidebar-mini", class = "control-sidebar-open")
})   
  
  
  ## pantalla inicial --------------------------------------------------------  
  output$inicial <- renderUI({
    if (is.null(input$input_type)) return()
      switch(input$input_type,
          "HMD" = div(
            box(status = "primary", solidHeader = TRUE, width=4,
            textInput("usuario", "Usuario", "rebeldatalab@gmail.com"),
            textInput("passw", "Contraseña", "1562189576"),
            selectInput("pais", "Pais", choices = paises, selected = "ESP", selectize = TRUE),
            prettyRadioButtons(inputId = "serieXXX", label = "Choose serie:", choices = c("female", "male", "total"), 
                               icon = icon("check"),bigger = TRUE,status = "info",inline = TRUE),
            actionButton(inputId = "carga",label = "Cargar")
            ),
            box(status = "primary", solidHeader = TRUE,width = 4,
               
                  verbatimTextOutput("summary_hmd")
               
              )
          ),
            
         "Archivo" =  div(
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
                         
                            verbatimTextOutput("summary_exc")
                         
                          )
                  ),
         "Guardado" = div(
              box(status = "primary", solidHeader = TRUE, width=4, 
                  selectInput("loadmi", "Elige guardado:", choices = list.files(here("grabados"))),
                  actionBttn(inputId = "loadcarga", label = "Cargar")
                 ),
              box(status = "primary", solidHeader = TRUE, width=4,
                  
                  verbatimTextOutput("summary_load")
                  
              )
        )
      )
  })
 
flag_load <- eventReactive(input$loadcarga, {
  auxi <- readRDS(here("grabados",input$loadmi))
  bases$memoria <- auxi$bases
  modelos$memoria <- auxi$modelos
  modelos$descri <- auxi$modes
  fores$memoria <- auxi$fores
  fores$descri <- auxi$foresdes
  fores$simu <- auxi$simu
  auxi
})
 
output$summary_load <- renderPrint({
  req(flag_load()) 
  cat("Cargando: \n Datos:\n")
  cat(names(flag_load()$bases))
  cat("\nModelos: \n")
  cat(names(flag_load()$modelos))
  cat("\nForecasts: \n")
  cat(names(flag_load()$fores))
  
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
   datatable(create_tabla_bases(bases$memoria), class = "display compact",
             options = list(dom = 't'))
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
    if (length(bases$memoria)>0) {  
       mi<-0
      mx <-100
      req(bases$memoria)

      div(
        fluidRow(column(width=6,
          selectInput("basemodelo", "Datos", choices = names(bases$memoria),  selectize = FALSE)),
          column(width=6, textInput("namemodelo",label= "Nombre del modelo"))
        ),
          
        fluidRow(column(width=9, helper(
          radioGroupButtons(inputId = "modelo", label = "Choose class of model", choices = c("LC", "CBD", "APC", "RH", "M6","M7","M8", "PLAT"),
                            justified = TRUE, status = "primary",
                            checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                             no = icon("remove",
                                                       lib = "glyphicon")))
              ,type = "markdown", content = "models", size="l")) ,
          column(width =3, prettyRadioButtons(inputId = "link", label = "Type of error", choices = links,
                                              icon = icon("check"),bigger = TRUE,status = "info"))
          ),
                
        fluidRow(column(width=6, sliderInput("anosmodelo", "Años", min = mi, max = mx , value = c(mi,mx), step = 1)),
                 column(width=6, sliderInput("edadmodelo", "Edad", min = mi, max = mx , value = c(mi,mx), step = 1))
                 ),
        numericInput("clipmodelo", label = "Nº de cohortes en los extremos con peso 0", min = 0, value = 0, step = 1),
        uiOutput("extra_param"),
        actionBttn(inputId = "runmodel", label = "Run model", icon = icon("gear"), style = "simple", color = "success")
      )
    } else NULL
      
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
          modelos$descri[[nombre]] <- list(Nombre = nombre,
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
          modelos$actual
        })
    

# Tabla modelos -----------------------------------------------------------

        output$tablemodelos <- renderDT({
          if (length(modelos$memoria)!=0)
          datatable(create_tabla_modelos(modelos$descri), class = "display compact",
                    options = list(dom = 't')) %>% 
          formatRound(11:12,1) 
          else NULL
        })
        
        output$modelos_exluidos <- renderTable({
          if (length(modelos$memoria)!=0)
          create_tabla_modelos(modelos$descri)$Nombre[input$tablemodelos_rows_selected]
          else NULL
        })

        observeEvent(input$borrar_mod,{
          if (!is.null(input$tablemodelos_rows_selected)) {
            modelos$memoria <- modelos$memoria[-input$tablemodelos_rows_selected]
            modelos$descri <- modelos$descri[-input$tablemodelos_rows_selected]
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



# Definir forecast --------------------------------------------------------

        output$create_forecast <- renderUI({
          if (length(modelos$memoria)>0) {    
          
          div(
            fluidRow(column(width =6, selectInput("modelofore", "Modelo: ", choices = names(modelos$memoria),  selectize = FALSE)),
                     column(width=6, textInput("namefore",label= "Nombre de la predicción:"))
                     ),
            fluidRow(column(width =6,sliderInput("anosfore", "Años a predecir:", min = 1, max = 200 , value = 50, step = 1)),
                     column(width=6, prettyRadioButtons(inputId = "salto", label = "Método para el salto:", 
                                                        choices = c("Estimación" = "fit", "Observado" = "actual")))
                     ),
            prettyRadioButtons(inputId = "metodoperiodo", label = "Método para el periodo:", 
                               choices = c("Random walk" = "mrwd", "Arima" = "iarima"), 
                               icon = icon("check"),bigger = TRUE,status = "info",inline = TRUE),
            uiOutput("ktarima"),
            uiOutput("gcarima"),
            actionBttn(inputId = "runfore", label = "Run forecast", icon = icon("gear"), style = "simple", color = "success")
          )
            
          } else NULL  
        })
        
        observe( {
          req(input$modelofore)
          updateTextInput(session, "namefore", value = paste0("Fore", input$runfore, input$modelofore, input$anosfore))
        })
        
        
        output$ktarima <- renderUI({
          if (req(input$metodoperiodo) == "iarima") {
               div(
                 fluidRow(
                   column(width=4, numericInput("ktarimap", "P", value = 1, min = 0)),
                   column(width=4, numericInput("ktarimad", "D", value = 1, min = 0)),
                   column(width=4, numericInput("ktarimaq", "Q", value = 0, min = 0))
                 )
               )
             }
        })   
        
        
        output$gcarima <- renderUI({
           req(input$modelofore) 
          if (modelos$descri[[input$modelofore]]$Modelo %in% c("APC", "RH", "M6", "M7", "M8", "PLAT")) {
            if (modelos$descri[[input$modelofore]]$Modelo %in% c("APC", "RH")) {
              val1 <- 1
              val2 <- 1
            } else {
              val1 <- 2
              val2 <- 0
            }
            div(
              fluidRow(
                column(width=4, numericInput("gcarimap", "Arima para la cohorte: \nP", value = val1, min = 0)),
                column(width=4, numericInput("gcarimad", "D", value = val2, min = 0)),
                column(width=4, numericInput("gcarimaq", "Q", value = 0, min = 0))
              )
            )  
           
          }
          
        })   
        
        

# Create forecast ---------------------------------------------------------


        flag_fore <- eventReactive(input$runfore, {
          nombre <- trimws(input$namefore)
      
            
            auxi  <- forecast(modelos$memoria[[input$modelofore]],
                        h = input$anosfore,
                        kt.method = input$metodoperiodo,
                        kt.order = c(input$ktarimap, input$ktarimad, input$ktarimaq),
                        gc.order = c(input$gcarimap, input$gcarimad, input$gcarimaq),
                        jumpchoice = input$salto
                      )
           
                      fores$memoria[[nombre]] <- auxi
                      fores$actual <- auxi
                      fores$descri[[nombre]] <- list(Nombre = nombre,
                                                      Modelo = input$modelofore,
                                                       Avance= input$anosfore,
                                                       Periodo = ifelse(input$metodoperiodo == "mrwd", "Random walk", "Arima"),
                                                       ArimaPeriodo = ifelse(input$metodoperiodo == "mrwd", "",
                                                                             paste(input$ktarimap, input$ktarimad, input$ktarimaq)),
                                                       ArimaCohorte = ifelse(is.null(auxi$gc.f), "", 
                                                                             paste(input$gcarimap, input$gcarimad, input$gcarimaq)),
                                                       Ages = paste0(min(auxi$ages),"-", max(auxi$ages)),
                                                       Years = paste0(min(auxi$years),"-", max(auxi$years)),
                                                       Jump = ifelse(input$salto == "fit", "Estimación", "Observado")
                                                    )
                      fores$simu[[nombre]] <- simulate(modelos$memoria[[input$modelofore]],
                                             h = input$anosfore,
                                             kt.method = input$metodoperiodo,
                                             kt.order = c(input$ktarimap, input$ktarimad, input$ktarimaq),
                                             gc.order = c(input$gcarimap, input$gcarimad, input$gcarimaq),
                                             jumpchoice = input$salto
                      )
                     
            })
        
        output$show_forecast <- renderPrint({
          req(flag_fore())
          fores$actual
        })
        
        

# Tabla de forecast -------------------------------------------------------

        output$tablefores <- renderDT({
          
          if (length(fores$memoria)>0)
            datatable(map_df(fores$descri, `[`), class = "display compact",
                      options = list(dom = 't'))
        })
        
        output$fores_exluidos <- renderTable({
          if (length(fores$memoria)>0)
            map_df(fores$descri, `[`)$Nombre[input$tablefores_rows_selected]
        })
        
        observeEvent(input$borrar_fore,{
          if (!is.null(input$tablefores_rows_selected)) {
            fores$memoria <- fores$memoria[-input$tablefores_rows_selected]
            fores$descri <- fores$descri[-input$tablefores_rows_selected]
            fores$selected <- fores$memoria
            fores$actual <- NULL
          }
        })
        
        observe({
          req(input$tablefores_rows_selected)
          fores$selected <- fores$memoria[-input$tablefores_rows_selected]
        })
        
                

# Show forecast -----------------------------------------------------------

        output$foresoutput <- renderUI({
          if (is.null(input$tablefores_rows_selected)) {fores$selected <- fores$memoria}
          else {fores$selected <- fores$memoria[-input$tablefores_rows_selected]}
          run3(fores, fores_server)
          deploy3(fores$selected, fores_UI)
          
        })
        
    
  
}