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




plot_fitted <- function(mod, bywhat, tipo, value, inter = FALSE) {
  if (tipo =="LogRates") {
    type <- "link"
    ylab  <- "Log death rate"
    uxt <- log(mod$Dxt / mod$Ext)
  }  else {
    if (tipo == "Rates") {
      type <- "rates"
      ylab <-  "Death rate"
      uxt <- mod$Dxt / mod$Ext
    } else {
      type <- "deaths"
      ylab <- "Number of deaths"
      uxt <- mod$Dxt
    }
  }
  
  uxthat <- fitted(mod, type)
  xlab = gsub("s", "", bywhat, fixed =TRUE)
  
  valuechar <- as.character(value)
  if(bywhat == "years") {
    uxt = uxt[valuechar, ]
    uxthat <- uxthat[valuechar, ]
  } else {
    uxt = uxt[, valuechar ]
    uxthat <- uxthat[, valuechar]
  }
  
  auxi <- data.frame(x = mod[[bywhat]], y = uxt, yhat = uxthat)
  
  p <- ggplot(auxi, aes(x,y)) + geom_point() + geom_line(aes(x, yhat, group = 1)) + 
    ylab(ylab) + xlab(xlab) + ggtitle(paste0("fitted vs. observed rates at ", value)) +
    theme_minimal()
  
  if (inter) p <- ggplotly(p)
  p
}
