#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram
ui <-dashboardPage(
        
        
        
     dashboardHeader(title = "Mortalidad"),
     
     dashboardSidebar(
           sidebarMenu( id ="menu",
               menuItem("Datos", tabName = "menu_datos"),
               menuItem("Gráficos descriptivos", tabName = "menu_grafi",
                         badgeLabel = "Sin datos", badgeColor = "red"), 
               menuItem("Lifetables", tabName = "menu_life"),
               menuItem("Construct models", tabName = "menu_models"),
               menuItem("Evaluate models", tabName = "menu_plotmodels"),
               menuItem("Forecast models", tabName = "menu_forecast")
           )
     ),
     dashboardBody(
             includeCSS("pru.css"),
             useShinyalert(),
             useShinyjs(),
             tabItems(
               tabItem(tabName = "menu_datos",
                    fluidRow(
                      box(width = 4,
                        radioButtons("input_type", "Lectura de datos",
                                     c("Human Mortality Database"="hmd", "Archivo"="archivo")
                        )
                      ),
                      uiOutput("inicial")
                    ),
                     hr(),
                      hr(),
                    
                    fluidRow(box(width = 8, title = "Lista de datos cargados",  
                                 dataTableOutput("tablebases")
                              ),
                             box(width = 4, title = "Datos que no se utilizaran",
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
                   fluidRow(box(width = 6, title = "Create Model",
                                uiOutput("create_model")
                             
                           ),
                           box(width = 6, title = "Creating Model",
                               withSpinner(verbatimTextOutput("show_models"))
                      )
                   ),
                   hr(),
                   hr(),
                   
                   fluidRow(box(width = 10, title = "Lista de modelos cargados",  
                                dataTableOutput("tablemodelos")
                   ),
                   box(width = 2, title = "Modelos que no se utilizaran",
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
                   fluidRow(box(width = 6, title = "Create forecast",
                                uiOutput("create_forecast")
                                
                   ),
                   box(width = 6, title = "Creating forecast",
                       withSpinner(verbatimTextOutput("show_forecast"))
                   )
                   )
           )                   
           
     )
     )
)