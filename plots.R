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



plot_fan <- function(simu, ages, atras, log = FALSE, fan = TRUE) {
  le <- length(simu$model$years)
  basef <- function(si, age, atra) {
    auxi <- tibble::rownames_to_column(data.frame(rate = si$fitted[age, (le -atra + 1):le,1], Age=age), var="Year") %>% 
            mutate(Year = as.numeric(Year))
   }
  basesim <-  function(si, age, atra) {
      auxi <- tibble::rownames_to_column(data.frame(si$rates[age,,]), var = "Year") %>% 
              tidyr::gather("simu", "rate", -Year) %>% 
              mutate(Year = as.numeric(Year), Age = age)
  }
  ylab = "Death rate"
  
  if (log) {
    ylab = "Log death rate"
  } 
  
  auxif <- map(ages, ~ basef(simu, .x, atras)) %>% data.table::rbindlist()
  auxisim <- map(ages, ~ basesim(simu, .x, atras)) %>% data.table::rbindlist()
    
  ylab = "Death rate"
  
  if (log) {
    ylab = "Log death rate"
    auxif <- mutate(auxif, rate = log(rate))
    auxisim <- mutate(auxisim, rate = log(rate))
  } 
  
  p <- ggplot(auxif, aes(x=Year, y = rate, group = Age, color = Age)) + geom_line() 
    
      if (fan) p <- p + geom_fan(data=auxisim, aes(x=Year, y = rate, group = Age, color = Age)) + scale_fill_distiller(palette="Spectral") 
      else p <- p + geom_interval(data=auxisim, aes(x=Year, y = rate, group = Age, color = Age))
     
   p + theme_minimal() + ylab(ylab)
  
}

plot_fore <- function(fore, ages, atras, log = FALSE, inter = FALSE) {
  le <- length(fore$model$years)
  basef <- function(si, age, atra) {
    auxi <- tibble::rownames_to_column(data.frame(rate = si$fitted[age, (le -atra + 1):le], Age=age), var="Year") %>% 
      mutate(Year = as.numeric(Year))
  }
  basesim <-  function(si, age, atra) {
    auxi <- tibble::rownames_to_column(data.frame(rate = si$rates[age,]), var = "Year") %>% 
      mutate(Year = as.numeric(Year), Age = age)
  }
  
  auxif <- map(ages, ~ basef(fore, .x, atras)) %>% data.table::rbindlist()
  auxisim <- map(ages, ~ basesim(fore, .x, atras)) %>% data.table::rbindlist()
  
  ylab = "Death rate"
  
  if (log) {
    ylab = "Log death rate"
    auxif <- mutate(auxif, rate = log(rate))
    auxisim <- mutate(auxisim, rate = log(rate))
  } 
  
  p <- ggplot(auxif, aes(x=Year, y = rate, group = Age, color = Age)) + geom_line() 
  
  p <- p + geom_line(data=auxisim, aes(x=Year, y = rate, group = Age, color = Age), linetype = 2)
  
  p <- p + theme_minimal() + ylab(ylab)
  
  if (inter) p <- ggplotly(p, tooltip = c("Year","rate"))
  
  p
  
}
  
  


