library(ggplot2)
library(dplyr)
library(demography)
library(tidyr)
library(plotly)

create_plot_i <- function(x, ages, years, type, series=names(x$rate)[1]){
  df <- as.data.frame(x$rate[[series]]) %>% 
        tibble::rownames_to_column(var = "Age") %>% 
        mutate(Age = as.numeric(gsub("+","",Age, fixed = T)))  %>% 
        gather(-Age, key = "Year",value =  "Rate")  %>%
        filter(Age %in% ages, Year %in% years)  %>% 
        mutate(Rate = na_if (Rate, 0))
  
if (type) {df$Rate <- log(df$Rate)}  
        

p <-ggplot(df, aes(x = Age, y = Rate, col = Year, group = Year)) + geom_line() + theme(legend.position = "none")

p <- ggplotly(p, tooltip = c("x","colour", "y"))
 p
}


salida1 <- function(input, output, session, name , bas , typeplot) {
  