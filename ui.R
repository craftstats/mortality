#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyalert)
library(shinyhelper)
library(shinycssloaders)
#library(shinydashboardPlus)


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
             tabItems(
            tabItem(tabName = "menu_datos",
                    fluidRow(
                      box(width = 4,
                        radioButtons("input_type", "Lectura de datos",
                                     c("Human Mortality Database"="hmd", "Archivo"="archivo")
                        )
                      ),
                      box(width=4,
                        uiOutput("ui")
                      ),
                      box(width = 4,
                          withSpinner(textOutput("summary")))
                    )
            ),
            tabItem(tabName = "menu_grafi",
                    fluidRow( 
                          box(width = 6,
                             uiOutput("ui2"),
                             uiOutput("uislider1"),
                             helper(uiOutput("uislider2"), type ="inline", title="un poco de ayuda", content = "lorem ipsum dsfdsfdasfdsf dsfsdfsdfsd"),
                             checkboxInput("transf", "Transformación logarítmica", TRUE),
                             checkboxInput("interac", "Interactivo", FALSE)
                          ),
                          box(width = 6, collapsible = TRUE, 
                            plotOutput("plot1")
                          )
                          
                          
                   )
                   
           ),
           tabItem(tabName = "menu_life",
                   fluidRow(
                     selectInput("pais3", "Pais", choices = "", selected="", selectize = FALSE)
                   ),
                   div(
                     fluidRow(
                           uiOutput("boxdoble")
                       
                     )  
                   )
            ),
           tabItem(tabName = "menu_models",
                   fluidRow(box(width = 6, title = "Create Model",
                                uiOutput("create_model")
                             
                           ),
                           box(width = 6, title = "Creating Model",
                               withSpinner(verbatimTextOutput("show_models"))
                      )
                   )
           ),
           tabItem(tabName = "menu_plotmodels",
                      fluidRow(
                             uiOutput("modeloutput")
                           )   
           
           ),
           tabItem(tabName = "menu_forecast",
                   fluidRow(
                           uiOutput("forecast")
                   )
           )                   
           
     )
     )
)