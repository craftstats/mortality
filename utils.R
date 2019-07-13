

f2 <- function(x, ages) mean(ages) - x
constPlat <- function(ax, bx, kt, b0x, gc, wxt, ages){
       nYears <- dim(wxt)[2]
       x <- ages
       t <- 1:nYears
       c <- (1 - tail(ages, 1)):(nYears - ages[1])
       xbar <- mean(x)
       phiReg <- lm(gc ~ 1 + c + I(c ^ 2), na.action = na.omit)
       phi <- coef(phiReg)
       gc <- gc - phi[1] - phi[2] * c - phi[3] * c ^ 2
       kt[2, ] <- kt[2, ] + 2 * phi[3] * t
       kt[1, ] <- kt[1, ] + phi[2] * t + phi[3] * (t ^ 2 - 2 * xbar * t)
       ax <- ax + phi[1] - phi[2] * x + phi[3] * x ^ 2
       ci <- rowMeans(kt, na.rm = TRUE)
       ax <- ax + ci[1] + ci[2] * (xbar - x)
       kt[1, ] <- kt[1, ] - ci[1]
       kt[2, ] <- kt[2, ] - ci[2]
       list(ax = ax, bx = bx, kt = kt, b0x = b0x, gc = gc)
  }




create_plot_i <- function(x, ages, years, type, series="female"){
  df <- as.data.frame(x$rate[[series]]) %>% 
    tibble::rownames_to_column(var = "Age") %>% 
    mutate(Age = as.numeric(gsub("+","",Age, fixed = T)))  %>% 
    tidyr::gather(-Age, key = "Year",value =  "Rate")  %>%
    filter(Age %in% ages, Year %in% years)  %>% 
    mutate(Rate = na_if (Rate, 0))
  
  if (type) {df$Rate <- log(df$Rate)}  
  
  
  p <-ggplot(df, aes(x = Age, y = Rate, col = Year, group = Year)) + geom_line() + theme(legend.position = "none")
  
  p <- ggplotly(p, tooltip = c("x","colour", "y"))
  return(p)
}



creaplot <- function(mod, x, y, value) {
  if (y =="log") {
    type <- "link"
    ylab  <- "log death rate"
  }  else {
    type <- "rates"
    ylab <-  "death" 
  }
  uxthat <- fitted(mod, type)
  xlab = gsub("s", "", x, fixed =TRUE)
  
  
  uxt <- mod$Dxt / mod$Ext
  
  valuechar <- as.character(value)
  if(x == "years") {
      uxt = uxt[valuechar, ]
     uxthat <- uxthat[valuechar, ]
} else {
  uxt = uxt[, valuechar ]
  uxthat <- uxthat[, valuechar]
  
}
  
 # auxf <- function(m, uxt, yl, main, ) {
    plot(mod[[x]], uxt, xlab = xlab, ylab = ylab, main = paste0("fitted vs. observed rates at ", value))
    lines(mod[[x]], uxthat)
  # }
  # base2grob::base2grob(auxf()) 
}





key_pais <- function(v, val) {
  val
  names(v)[v==val]
}


create_tabla_bases <- function(bas) {
  df <- data_frame(
            nombre = names(bas),
            pais =  map_chr(bas, "label"),
            series = map_chr(bas, "series"),
            edades = map_chr(bas, ~paste0(min(.$ages), " - ", max(.$ages))),
            cohortes = map_chr(bas, ~paste0(min(.$years), " - ", max(.$years))),
  )
}


create_model <- function(type, data, link, years, ages, const="sum", cohortAgeFun="1", approxConst=FALSE, xc = 1960) {
  switch(type,
         LC = fit(lc(link = link, const = const), data = data, ages.fit = ages, years.fit = years), 
         CBD = fit(cbd(link = link), data = data, ages.fit = ages, years.fit = years),
         APC = fit(apc(link = link), data = data, ages.fit = ages, years.fit = years),
         RH = fit(rh(link = link, cohortAgeFun = cohortAgeFun, approxConst = approxConst), data = data, ages.fit = ages, years.fit = years),
         M6 = fit(m6(link = link), data = data, ages.fit = ages, years.fit = years),
         M7 = fit(m7(link = link), data = data, ages.fit = ages, years.fit = years),
         M8 = fit(m8(link = link, xc = xc), data = data, ages.fit = ages, years.fit = years),
         PLAT = fit(StMoMo(link = link, staticAgeFun = TRUE, periodAgeFun = c("1", f2), 
                        cohortAgeFun = "1", constFun = constPlat),
                        data = data, ages.fit = ages, years.fit = years)
         
  )
}

cb_coef_age <- function(model) {
  auxi <- data.frame(ages = model$ages)
  if (!is.null(model$ax)) auxi$ax <- model$ax
  if (!is.null(model$ax)) {
    aux2 <- as.matrix(model$bx)
    colnames(aux2) <- paste0("bx", 1:dim(aux2)[2])
    auxi <- cbind(auxi, aux2)
    row.names(auxi)<- c()
    auxi
  }
  if (!is.null(model$b0x)) auxi$b0x <- model$b0x
  auxi
}


hmd.mx2 <- function(country, username, password, label=country)
{
  path <- paste("https://www.mortality.org/hmd/", country, "/STATS/", "Mx_1x1.txt", sep = "")
  userpwd <- paste(username, ":", password, sep = "")
  txt <- RCurl::getURL(path, userpwd = userpwd)
  con <- textConnection(txt)
  mx <- try(utils::read.table(con, skip = 2, header = TRUE, na.strings = "."),TRUE)
  close(con)
  if(class(mx)=="try-error")
    stop("Connection error at www.mortality.org. Please check username, password and country label.")
  
  path <- paste("https://www.mortality.org/hmd/", country, "/STATS/", "Exposures_1x1.txt", sep = "")
  userpwd <- paste(username, ":", password, sep = "")
  txt <- RCurl::getURL(path, userpwd = userpwd)
  con <- textConnection(txt)
  pop <- try(utils::read.table(con, skip = 2, header = TRUE, na.strings = "."),TRUE)
  close(con)
  if(class(pop)=="try-error")
    stop("Exposures file not found at www.mortality.org")
  obj <- list(type="mortality",label=label,lambda=0)
  obj$year <- sort(unique(mx[, 1]))
  #obj$year <- ts(obj$year, start=min(obj$year))
  n <- length(obj$year)
  m <- length(unique(mx[, 2]))
  obj$age <- mx[1:m, 2]
  mnames <- names(mx)[-c(1, 2)]
  n.mort <- length(mnames)
  obj$rate <- obj$pop <- list()
  for (i in 1:n.mort)
  {
    obj$rate[[i]] <- matrix(mx[, i + 2], nrow = m, ncol = n)
    obj$rate[[i]][obj$rate[[i]] < 0] <- NA
    obj$pop[[i]] <- matrix(pop[, i + 2], nrow = m, ncol = n)
    obj$pop[[i]][obj$pop[[i]] < 0] <- NA
    dimnames(obj$rate[[i]]) <- dimnames(obj$pop[[i]]) <- list(obj$age, obj$year)
  }
  names(obj$pop) = names(obj$rate) <- tolower(mnames)
  obj$age <- as.numeric(as.character(obj$age))
  if (is.na(obj$age[m]))
    obj$age[m] <- 2 * obj$age[m - 1] - obj$age[m - 2]
  return(structure(obj, class = "demogdata"))
}
