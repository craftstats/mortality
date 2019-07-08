library(ggplot2)
library(dplyr)
library(demography)
library(tidyr)
library(plotly)

create_db_demog <- function(x, series=names(x$rate)[1]){
  df <- as.data.frame(x$rate[[series]]) %>% 
        tibble::rownames_to_column(var = "Age") %>% 
        mutate(Age = as.numeric(gsub("+","",Age, fixed = T))) %>% 
        gather(-Age, key = "Year",value =  "Rate") %>% 
      mutate(Rate = na_if (Rate, 0))
  
}
create_db_demog(italyDemo)->ppp

p <-ggplot(ppp, aes(x = Age, y = log(Rate), col = Year, group = Year)) + geom_line() 

 ggplotly(p, tooltip = c("x","colour", "y"))
 