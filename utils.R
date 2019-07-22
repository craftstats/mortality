

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


sheets_name <- function(file) {
  if (!is.null(file)) {
    tryCatch({
      return(excel_sheets(path = file$datapath))
    },
    error = function(err) {
      sendSweetAlert(session, html = "¡Fallo de lectura!", "Revisa que sea un archiovo excel válido", type = "error")   
      return(NULL)
    }) 
  } else {
    return(NULL)
  }  
}


st2demo <- function(objeto) {
  demogdata(objeto$Dxt/objeto$Ext, objeto$Dxt, objeto$ages, objeto$years, "mortality", objeto$label, objeto$series, 0)
}





# plot_fitted <- function(mod, bywhat, tipo, value, inter = FALSE) {
#   if (tipo =="log") {
#     type <- "link"
#     ylab  <- "Log death rate"
#     uxt <- log(mod$Dxt / mod$Ext)
#   }  else {
#     if (tipo == "rate") {
#     type <- "rates"
#     ylab <-  "Death rate"
#     uxt <- mod$Dxt / mod$Ext
#     } else {
#       type <- "deaths"
#       ylab <- "Number of deaths"
#       uxt <- mod$Dxt
#     }
#   }
#   
#   uxthat <- fitted(mod, type)
#   xlab = gsub("s", "", bywhat, fixed =TRUE)
#   
#   
#   
#   
#   valuechar <- as.character(value)
#   if(bywhat == "years") {
#       uxt = uxt[valuechar, ]
#      uxthat <- uxthat[valuechar, ]
# } else {
#   uxt = uxt[, valuechar ]
#   uxthat <- uxthat[, valuechar]
#   
# }
# 
#   auxi <- data.frame(x = mod[[bywhat]], y = uxt, yhat = uxthat)
#   
#   p <- ggplot(auxi, aes(x,y)) + geom_point() + geom_line(aes(x, yhat, group = 1)) + 
#            ylab(ylab) + xlab(xlab) + ggtitle(paste0("fitted vs. observed rates at ", value)) +
#        theme_minimal()
#  
#  if (inter) p <- plotly::ggplotly(p)
#   p
# }





key_pais <- function(v, val) {
  val
  names(v)[v==val]
}


create_tabla_bases <- function(bas) {
  df <- data_frame(
            Nombre = names(bas),
            Pais =  map_chr(bas, "label"),
            Series = map_chr(bas, "series"),
            Ages = map_chr(bas, ~paste0(min(.$ages), " - ", max(.$ages))),
            Years = map_chr(bas, ~paste0(min(.$years), " - ", max(.$years))),
  )
}

create_tabla_modelos <- function(bas2) {
 
  df <- data_frame(
      Nombre =  map_chr(bas2, "Nombre"),
     Datos = map_chr(bas2, "Datos"),
     Modelo = map_chr(bas2, "Modelo"),
     Error = map_chr(bas2, "Link"),
     Years = map_chr(bas2, "Anos"),
     Ages = map_chr(bas2, "Edades"),
     Cohorts = map_chr(bas2, "coho"),
     Formula = map_chr(bas2, "formu"),
     Constrains = map_chr(bas2, "const"),
     Parameters  = map_dbl(bas2, "nparam"),
     AIC = map_dbl(bas2, "aic"),
     BIC = map_dbl(bas2, "bic")
  )
}

create_model <- function(type, data, link, years, ages, clip , const,
                         cohortAgeFun, approxConst, LCfirst, xc) {
  
  
  wxt <- genWeightMat(ages, years, clip)
  if (link == "logit") {data <- central2initial(data)}
  if (LCfirst) {
    type <- "RHesp"
    LCfit <- fit(lc(link = link, const = "sum"), data = data, ages.fit = ages, years.fit = years, wxt = wxt)
  }
  
  switch(type,
         LC = fit(lc(link = link, const = const), data = data, ages.fit = ages, years.fit = years, wxt = wxt), 
         CBD = fit(cbd(link = link), data = data, ages.fit = ages, years.fit = years, wxt = wxt),
         APC = fit(apc(link = link), data = data, ages.fit = ages, years.fit = years, wxt = wxt),
         RH = fit(rh(link = link, cohortAgeFun = cohortAgeFun, approxConst = approxConst), data = data, ages.fit = ages, years.fit = years, wxt = wxt),
         RHesp = fit(rh(link = link, cohortAgeFun = cohortAgeFun, approxConst = approxConst), 
                           data = data, ages.fit = ages, years.fit = years, wxt = wxt,
                            start.ax = LCfit$ax, start.bx = LCfit$bx, start.kt = LCfit$kt),
         M6 = fit(m6(link = link), data = data, ages.fit = ages, years.fit = years, wxt = wxt),
         M7 = fit(m7(link = link), data = data, ages.fit = ages, years.fit = years, wxt = wxt),
         M8 = fit(m8(link = link, xc = xc), data = data, ages.fit = ages, years.fit = years, wxt = wxt),
         PLAT = fit(StMoMo(link = link, staticAgeFun = TRUE, periodAgeFun = c("1", f2), 
                        cohortAgeFun = "1", constFun = constPlat),
                        data = data, ages.fit = ages, years.fit = years, wxt= wxt)
         
  )
}

coef_age <- function(model) {
  auxi <- data.frame(ages = model$ages)
  if (!is.null(model$ax)) auxi$ax <- model$ax
  if (!is.null(model$bx)) {
    aux2 <- as.matrix(model$bx)
    colnames(aux2) <- paste0("bx", 1:dim(aux2)[2])
    auxi <- cbind(auxi, aux2)
    row.names(auxi)<- c()
    }
  if (!is.null(model$b0x)) auxi$b0x <- model$b0x
  auxi
}

coef_year <- function(model) {
  auxi <- data.frame(years = model$years)
  if (!is.null(model$kt)) {
    aux2 <- t(as.matrix(model$kt))
    colnames(aux2) <- paste0("kt", 1:dim(aux2)[2])
    auxi <- cbind(auxi, aux2)
    row.names(auxi)<- c()
  }
  auxi
}

coef_coho <- function(model) {
  auxi <- data.frame(years = model$cohorts)
  if (!is.null(model$gc)) {
    auxi$gc <- model$gc
  }
  auxi
}


coef_fores <- function(model) {
  auxi <- data.frame(years = model$years)
 
  if (!is.null(model$kt.f)) {
    aux2 <- t(model$kt.f$mean)
    colnames(aux2) <- paste0("kt", 1:dim(aux2)[2])
    auxi <- cbind(auxi, aux2)
    row.names(auxi)<- c()
  }
  if (!is.null(model$gc.f)) auxi$gc <- model$gc.f$mean
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
