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



# Modulo descriptivos ( 3 boxes) ------------------------------------------

descriptivos_UI <- function(id, title) {
  ns <- NS(id)
  fluidRow(
    fluidRow(column(width =3), box(width = 6, status = "primary", h4(title))),
       tabBox(width = 4, title = "Rates respecto a edad",
           plot_con_opciones_UI(ns("age"))),
       tabBox(width = 4, title = "Rates respecto a year",  
           plot_con_opciones_UI(ns("year"))),
       tabBox(width=4, title = "Datos",
              tabPanel(title = "No tablas"
                       ),
              tabPanel(title = "Rates",
                       tabla_opciones_UI(ns("rates"))
                       ),
              tabPanel(title = "Deaths",
                       tabla_opciones_UI(ns("deaths"))
                       ),
              tabPanel(title = "Population",
                       tabla_opciones_UI(ns("pop"))
              )
           )
       )
}

descriptivos_server <- function(input, output, session, name, bas) {
  ns <- session$ns
  callModule(plot_con_opciones, "age", salida1, name, bas, typeplot = "functions") 
  callModule(plot_con_opciones, "year", salida1,  name, bas, typeplot = "time")
  callModule(tabla_opciones_server, "rates", tabladata,  name, bas, "Rates",type = "rate")
  callModule(tabla_opciones_server, "deaths", tabladata,  name, bas, "Deaths", type = "death")
  callModule(tabla_opciones_server, "pop", tabladata,  name, bas, "Population", type = "pop")
  
}


# Modulo lifetables ( 3 boxes) ------------------------------------------

lifetables_UI <- function(id, title) {
  ns <- NS(id)
  fluidRow(
    fluidRow(column(width =3), box(width = 6, status = "primary", h4(title))),
    tabBox(width = 6, title = "Esperanza respecto a edad", 
              tabPanel(title = "Plot",
                plot_con_opciones_UI(ns("agelt"))
              ),
              tabPanel(title = "Tabla", 
                       tabla_opciones_UI(ns("tagelt"))
              )
    ),
    tabBox(width = 6, title = "Esperanza respecto a year", 
           tabPanel(title = "Plot",
                    plot_con_opciones_UI(ns("yearlt"))
           ),
           tabPanel(title = "Tabla", 
                    tabla_opciones_UI(ns("tyearlt"))
           )
    )
  )
}

lifetables_server <- function(input, output, session, name, bas) {
  ns <- session$ns
  callModule(plot_con_opciones, "agelt", salida2, name, bas, typeplot = "ages") 
  callModule(plot_con_opciones, "yearlt", salida2,  name, bas, typeplot = "year")
  callModule(tabla_opciones_server, "tagelt", tablalt,  name, bas, "Esperanzaedad", type = "Age")
  callModule(tabla_opciones_server, "tyearlt", tablalt,  name, bas, "Esperanzayear", type = "Year")
}


# Modulo gráficos de modelos ----------------------------------------------

modelos_UI <- function(id, title) {
  ns <- NS(id)
  fluidRow(
    fluidRow(column(width =3), box(width = 6, status = "primary", h4(title))),
    tabBox(width = 4, title = "Estimaciones",
           tabPanel(title = "By Age",
                  plot_con_opciones_UI(ns("eage"))  
                    ),
           tabPanel(title = "By Year",
                    plot_con_opciones_UI(ns("eyear"))  
                   ),
           tabPanel(title = "Tabla",
                    tabla_opciones_UI(ns("etabla")) 
                    )
         ),
    tabBox(width = 4, title = "Parameters",
          tabPanel(title = "Plot", 
            plot_con_opciones_UI(ns("parameters"))
          ),
          tabPanel(title = "Age",
                   tabla_opciones_UI(ns("age"))
          ),
          tabPanel(title = "Year",
                   tabla_opciones_UI(ns("year"))
          ),
          tabPanel(title = "Cohort",
                   tabla_opciones_UI(ns("cohort"))
          )
    ),
    tabBox(width = 4, title = "Residuales",
              tabPanel(title = "Scatterplot",
                    plot_con_opciones_UI(ns("residuals"))
              ),
              tabPanel(title = "Heatmap",
                   plot_con_opciones_UI(ns("heatmap"))
              ),
              tabPanel(title = "Signplot",
                    plot_con_opciones_UI(ns("signplot"))
              ),
             tabPanel(title = "Tabla",
                      tabla_opciones_UI(ns("tresiduales"))
             )
          )   
     )
 }


modelos_server <- function(input, output, session, name, bas) {
  ns <- session$ns
  callModule(plot_con_opciones, "eage", salida5, name , bas, bytype = "ages")
  callModule(plot_con_opciones, "eyear", salida5, name , bas, bytype = "years")
  callModule(tabla_opciones_server, "etabla", tablafit, name, bas, "fitted")
  callModule(plot_con_opciones, "parameters", salida4, name , bas )
  callModule(tabla_opciones_server, "age", tablapar, name, bas, nombre = "ageparameters", type = "age")
  callModule(tabla_opciones_server, "year", tablapar, name, bas, nombre = "yearparameters", type = "year")
  callModule(tabla_opciones_server, "cohort", tablapar, name, bas, nombre = "cohortparameters", type = "cohort")
  callModule(plot_con_opciones, "residuals", salida3, name, bas, typeplot = "scatter") 
  callModule(plot_con_opciones, "heatmap", salida3,  name, bas, typeplot = "colourmap")
  callModule(plot_con_opciones, "signplot", salida3, name, bas, typeplot = "signplot")
  callModule(tabla_opciones_server, "tresiduales", tablares, name , bas, "residuales") 
}  


  

