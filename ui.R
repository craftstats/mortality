#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram
ui <-dashboardPagePlus(
        
        
        
     dashboardHeaderPlus(title = "Mortalidad",
                         enable_rightsidebar = TRUE,
                         rightSidebarIcon = "save",
                           left_menu =  tagList(dropdownBlock(
                                         id = "save",
                                         title = "Guardar",
                                         icon = icon("ok", lib = "glyphicon"),
                                         badgeStatus = NULL,
                                         sliderInput(
                                                 inputId = "n",
                                                 label = "Number of observations",
                                                 min = 10, max = 100, value = 30)
                                       )
                          )),
                        
     
     dashboardSidebar(
           sidebarMenu( id ="menu",
               menuItem("Datos", tabName = "menu_datos"),
               menuItem("Gráficos descriptivos", tabName = "menu_grafi"),
                          
               menuItem("Lifetables", tabName = "menu_life"),
               menuItem("Construct models", tabName = "menu_models"),
               menuItem("Evaluate models", tabName = "menu_plotmodels"),
               menuItem("Forecast models", tabName = "menu_forecast"),
               menuItem("Evaluate forecasts", tabName = "menu_plotfores")
           )
     ),
     dashboardBody(
             includeCSS("pru.css"),
             useShinyalert(),
             useShinyjs(),
             tabItems(
               tabItem(tabName = "menu_datos",
                    fluidRow(
                      box(status = "primary", solidHeader = TRUE, width = 4,
                        radioButtons("input_type", "Lectura de datos",
                                     c("Human Mortality Database"="hmd", "Archivo"="archivo")
                        )
                      ),
                      uiOutput("inicial")
                    ),
                     hr(),
                      hr(),
                    
                    fluidRow(box(status = "primary", solidHeader = TRUE, width = 8, title = "Lista de datos cargados",  
                                 dataTableOutput("tablebases")
                              ),
                             box(status = "primary", solidHeader = TRUE, width = 4, title = "Datos que no se utilizaran",
                                 tableOutput('lista_exluidos'),
                                 div(actionBttn("borrar", icon =icon("trash")), align = "right")
                             )
                    )
              ),
              tabItem(tabName = "menu_grafi",
                   uiOutput("descri")
              ),
              tabItem(tabName = "menu_life",
                   uiOutput("lifetables")
                   
            ),
           tabItem(tabName = "menu_models",
                   fluidRow(box(status = "primary", solidHeader = TRUE, width = 6, title = "Create Model",
                                uiOutput("create_model")
                             
                           ),
                           box(status = "primary", solidHeader = TRUE, width = 6, title = "Creating Model",
                               withSpinner(verbatimTextOutput("show_models"))
                      )
                   ),
                   
                   fluidRow(box(status = "primary", solidHeader = TRUE, width = 10, title = "Lista de modelos cargados",  
                                dataTableOutput("tablemodelos")
                   ),
                   box(status = "primary", solidHeader = TRUE, width = 2, title = "Modelos que no se utilizaran",
                       tableOutput('modelos_exluidos'),
                       div(actionBttn("borrar_mod", icon =icon("trash")), align = "right")
                   )
                   )
           ),
           tabItem(tabName = "menu_plotmodels",
                      fluidRow(
                             uiOutput("modelooutput")
                           )   
           
           ),
           tabItem(tabName = "menu_forecast",
                   fluidRow(box(status = "primary", solidHeader = TRUE, width = 6, title = "Create forecast",
                                uiOutput("create_forecast")
                                
                        ),
                      box(status = "primary", solidHeader = TRUE, width = 6, title = "Creating forecast",
                         withSpinner(verbatimTextOutput("show_forecast"))
                       )
                   ),
                   
                   fluidRow(box(status = "primary", solidHeader = TRUE, width = 10, title = "Lista de predicciones cargadas",  
                                dataTableOutput("tablefores")
                             ),
                       box(status = "primary", solidHeader = TRUE, width = 2, title = "Predicciones que no se utilizaran",
                          tableOutput('fores_exluidos'),
                         div(actionBttn("borrar_fore", icon =icon("trash")), align = "right")
                      )
                   )
           ),
           tabItem(tabName = "menu_plotfores",
                   fluidRow(
                           uiOutput("foresoutput")
                   )   
                   
           )
           
                              
           
     )
     )
)