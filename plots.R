plot_life <- function(objeto, ages, years, type) {
  df <- objeto %>% 
    lifetable()  %>% 
    .[["ex"]] %>% 
    as_tibble(rownames = "Age") %>% 
    mutate(Age = as.numeric(gsub("+","",Age, fixed = T))) %>% 
    tidyr::gather(-Age, key = "Year",value =  "ex")  %>%
    mutate( Year = as.numeric(Year))  %>% 
    filter(Age %in% ages, Year %in% years)
  
  
  if (type == "ages") {p <- ggplot(df, aes(x = Age, y = ex, group = as.factor(Year), col = as.factor(Year)))}
  else  {p <- ggplot(df, aes(x = Year, y = ex, group = as.factor(Age), col = as.factor(Age)))}
  
  p <- p + geom_line() + ylab("Life expectancy") + 
       theme_minimal() + theme(legend.position = "none")
   #p<- ggplotly(p, tooltip = c("x","colour", "y"))
  return(p)
  
  }