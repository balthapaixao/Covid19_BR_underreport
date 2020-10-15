source('https://raw.githubusercontent.com/balthapaixao/Covid19_BR_underreport/master/Func_/timeseries.R')
source('https://raw.githubusercontent.com/balthapaixao/Covid19_BR_underreport/master/Func_/evtdet.R')
library(readxl)
library(dplyr)
library(reshape)
library(boot)
library(ggplot2)
library(zoo)
library(tidyverse)
library(RColorBrewer)

source("https://raw.githubusercontent.com/balthapaixao/Covid19_BR_underreport/master/Func_/ETL_SRAG.R")

col_set <- brewer.pal(11, 'Spectral')
col_2 <- col_set[c(3,1)]

compute_ma <- function(data) {
  data_ema <- data %>% select(ano, semana)
  for (i in 3:ncol(data)) {
    sw <- data.frame(ts.sw(data[,i], 52*4))
    sw <- sw[,sprintf("t%d",  52*((4:1)-1))]
    ema <- apply(sw, 1, ts.emean)
    data_ema[, colnames(data)[i]] = ema
  }
  return(data_ema)
}
linreg <- function(data) {
  #browser()
  data <- as.data.frame(data)
  colnames(data) <- "x"
  data$t <- 1:nrow(data)
  
  #Adjusting a linear regression to the whole window
  lm(x~t, data)
}
pre_proc_data <- function(data = X, tipo = 'graph', datelim = '2020-06-27'){
  datelim <- as.POSIXct(datelim)
  a_day <- 60*60*24
  data_corte <-  trunc(datelim,"days")+ a_day #as.POSIXct(datelim)
  
  colunas <- colnames(srag)
  colnames(X) <- colunas
  
  X <- X[which(X$Tipo =="Estado" & X$sexo =="Total" & X$escala =="casos"), ]
  colnames(X)[8] <- "ano"
  colnames(X)[9] <- "semana"
  colnames(X)[12] <- "total"
  X$semana <- paste("", formatC(X$semana, width = 2, flag="0"))
  X$datadia <- as.POSIXct(as.Date(paste(X$ano, X$semana, 7, sep = '-'), "%Y-%U-%u"))
  X <- X[X$datadia < data_corte, ]
  
  X_casos <- X[which(X$dado =="srag"), ]
  X_obitos <- X[which(X$dado =="obito"), ]
  
  if(tipo == 'graph'){
    filtro <- c('datadia', "Unidade da Federação", 'total')
    }
  else{
    filtro <- c('ano', 'semana', "Unidade da Federação",
                'total', 'SARS-CoV-2')
  }
  
  dt_casos <- X_casos[filtro]
  dt_obitos <- X_obitos[filtro]
  
  if(tipo == 'graph'){
    lista_serie_casos <- pre_proc_merge(dt_casos, tipo)
    lista_serie_obitos <- pre_proc_merge(dt_obitos, tipo)
    
    serie_total = list("cases" = lista_serie_casos, "deaths" = lista_serie_obitos)
  }
  else{
    lista_serie_casos <- pre_proc_merge(dt_casos, tipo)
    lista_serie_obitos <- pre_proc_merge(dt_obitos, tipo)
    
    serie_total = list("cases" = lista_serie_casos, "deaths" = lista_serie_obitos)
  }
  return (serie_total)
}
pre_proc_merge <- function(data, tipo){
  #ufs <- read_excel("~/Aux_arqs/ufs.xls")
  ufs <- read_delim("https://raw.githubusercontent.com/balthapaixao/Covid19_BR_underreport/master/Aux_arqs/ufs.csv", delim = ';', col_types = cols())
  ufs["X1"] <- NULL
  ufs$Sigla <- as.factor(ufs$Sigla)
  if(tipo == 'graph'){
    XU <- merge(x=data, y=ufs, by.x="Unidade da Federação", by.y="Estado") %>% 
      select(sigla=Sigla, datadia, value=total)
    serie_sem_covid <- cast(XU, datadia ~ sigla)
    serie <- list("no_covid" = serie_sem_covid)
  }
  else{ ##else é subnotif
    XU <- merge(x=data, y=ufs, by.x="Unidade da Federação", by.y="Estado") %>%
      select(sigla=Sigla, ano, semana, value=total)
    serie_sem_covid <- cast(XU, ano+semana ~ sigla, mean)
    
    XU_covid <- merge(x=data, y=ufs, by.x="Unidade da Federação", by.y="Estado") %>%
      select(sigla=Sigla, ano, semana, value='SARS-CoV-2')
    serie_covid <- cast(XU_covid, ano+semana ~ sigla, mean)
    serie_covid <- serie_covid %>% filter(ano == 2020)
    
    serie <- list('no_covid' = serie_sem_covid, 'covid' = serie_covid)
  }
  return(serie)
}

pre_proc_ms <- function(datelim = '2020-06-27'){
  infogripe_data = datelim
  if(infogripe_data == '2020-06-27'){
    print("ARTICLE DATA")
    data_source <- "https://raw.githubusercontent.com/balthapaixao/Covid19_BR_underreport/master/Aux_arqs/HIST_PAINEL_COVIDBR_09jul2020.csv"
    serie_ms <- read_delim(data_source, delim = ';',col_types = cols())
    
  }else{
    print("LAST UPDATE OCTOBER 9th")
    data_source <- "https://raw.githubusercontent.com/balthapaixao/Covid19_BR_underreport/master/Aux_arqs/HIST_PAINEL_COVIDBR_last_version.csv"
    serie_ms <- read_delim(data_source, delim = ',',col_types = cols())
  }
  serie_ms["X1"] <- NULL
  serie_ms$casosAcumulado <- as.numeric(serie_ms$casosAcumulado)
  serie_ms$obitosAcumulado <- as.numeric(serie_ms$obitosAcumulado)
  
  serie <- serie_ms[which(serie_ms$regiao != 'Brasil' & serie_ms$data == infogripe_data), ]
  
  serie_total_casos <- serie[c('estado', 'casosAcumulado')]
  serie_total_obitos <- serie[c('estado', 'obitosAcumulado')]
  
  serie_total_casos <- aggregate(serie_total_casos$casosAcumulado, 
                                 by=list(Category=serie_total_casos$estado), FUN=max)
  serie_total_obitos <- aggregate(serie_total_obitos$obitosAcumulado , 
                                  by=list(Category=serie_total_obitos$estado), FUN=max)
  
  serie_ms_total <- list("hm_acc_cases" = serie_total_casos, "hm_acc_deaths" = serie_total_obitos)
  
  return(serie_ms_total)
}
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
get_2020_cp <- function(data){
  data$YW <- paste(data$ano, trim(data$semana), sep = '-')
  data <- data[4:3]
  state <- names(data)[2]
  names(data) <- c("time", "serie")
  
  #CP_V3 (linear regression)
  events_v3 <- evtdet.changepoints_v3(data, mdl=linreg, m=4)
  pts <- events_v3[events_v3$type=="change point", ]
  
  cps_2020 <- pts[as.POSIXct(as.Date(paste(pts$time, 7, sep='-'), "%Y-%U-%u")) > as.POSIXct('2020-01-01'),]
  
  if(nrow(cps_2020)){
    cp_2020 <- as.character(cps_2020['time'][[1,1]])
  }
  else{
    cp_2020 <- as.character("XXXX-XX")
  }
  return(cp_2020)
}
get_anomaly_srag <- function(serie, estado){
  filtro <- c('datadia', estado)
  serie_plot <- serie[filtro]
  #CP_V3 (linear regression)
  events_v3 <- evtdet.changepoints_v3(serie_plot, mdl=linreg, m=4)
  #Adaptative normalization
  w <-  vector()
  x <- ts.an.outliers.boxplot(serie_plot[[estado]], 104, alpha=3)
  serie_plot$anom <- x
  z <- subset(serie_plot, serie_plot['anom']==TRUE)
  
  #Bindin data to show anomalies(A_N) and change points(CP_V3) together
  if (nrow(z) != 0){
    z$serie <- estado
    z$type <- "anomaly"
    z <- z[c('datadia', 'serie', 'type')]
    colnames(z)[1] <- "time"
    an_v3 <- rbind(z, events_v3[events_v3$type=="change point", ] )
  }
  else {
    an_v3 <- events_v3[events_v3$type=="change point", ]
  }
  return(an_v3)
}

plot_subnotif <- function(serie_ur_cases, serie_ur_deaths, state, week=NULL){
  data_filtered <- data.frame(weeks = as.integer(row.names(serie_ur_cases)),
                              ur_cases = serie_ur_cases[[state]], 
                              ur_deaths = serie_ur_deaths[[state]])
  colnames(data_filtered) <- c("weeks", "cases", "deaths")
  if (!is.null(week))
    data_filtered <- data_filtered[data_filtered$weeks >= week,]
  data_plot <- data_filtered %>%
    select(weeks, cases, deaths) %>%
    gather(key = "type_of_record", value = "value", -weeks)
  
  yy <- (data_plot$value)[!is.na(data_plot$value)]
  yyb <- quantile(yy)
  ymax <- min(3*(yyb[4]-yyb[2])+yyb[4], max(yy))
  p <- ggplot(data=data_plot, aes(x = weeks, y = value, colour=type_of_record, group=type_of_record)) + geom_point(size=1.5) + geom_line(size=1)
  p <- p + scale_color_manual(values = col_2)
  p <- p + theme_minimal()
  p <- p + xlab("Week") + ylab("Under-report rate") #+ ylim(0, ymax)
  p <- p + theme(legend.position = c(0.8, 0.85))
  p <- p + guides(color = guide_legend(state_name(state)))
  return(p)
}
evtplot <- function(data, events, reference=NULL, mark.cp=FALSE, mark.size=1, ylim=NULL,...){
  serie_name <- names(data)[-1]
  names(data) <- c("time","serie")
  
  #Time of detected events
  events_true <- events # as.data.frame(events[events$type=="anomaly",])
  #Time of detected events of type change points
  events_cp <- as.data.frame(events[events$type=="change point",])
  
  #Data observation of detected events
  data_events_true <- as.data.frame(data[data$time %in% events$time,])
  
  #If true events are identified
  if(!is.null(reference)) {
    names(reference) <- c("time","event")
    
    #Time of identified true events
    ref_true <- as.data.frame(reference[reference$event==TRUE,])
    #Time of identified true events that were correctly detected
    ref_events_true <- as.data.frame(ref_true[ref_true$time %in% events$time,])
    
    #Data observation of identified true events
    data_ref_true <- as.data.frame(data[data$time %in% ref_true$time,])
    #Data observation of identified true events that were correctly detected
    data_ref_events_true <- as.data.frame(data[data$time %in% ref_events_true$time,])
  }
  
  min_data <- min(data$serie)
  max_data <- max(data$serie)
  if(!is.null(ylim)){
    min_data <- ifelse(!is.na(ylim[1]), ylim[1], min(data$serie))
    max_data <- ifelse(!is.na(ylim[2]), ylim[2], max(data$serie))
  }
  
  top_1 <- max_data+(max_data-min_data)*0.02
  top_2 <- max_data+(max_data-min_data)*0.05
  bottom_1 <- min_data-(max_data-min_data)*0.02
  bottom_2 <- min_data-(max_data-min_data)*0.05
  
  
  require(ggplot2)
  #Plotting time series
  plot <- ggplot(data, aes(x=time, y=serie)) +
    geom_line()+
    xlab("Time")+
    ylab(serie_name)+
    theme_bw()
  
  #Setting y limits if provided
  if(!is.null(ylim)) plot <- plot + ggplot2::ylim(bottom_2,top_2)
  
  #Plotting change points
  if(mark.cp & nrow(events_cp)>0) 
    tryCatch(plot <- plot + geom_segment(aes(x=time, y=top_1, xend=time, yend=bottom_1),data=events_cp, col="red", size = .5, linetype="dashed"),
             error = function(e) NULL)
  
  #Plotting top bar
  plot <- plot + geom_hline(aes(yintercept = top_1),data=events_cp, col="black", size = 0.5)
  plot <- plot + geom_hline(aes(yintercept = top_2),data=events_cp, col="black", size = 0.5)
  tryCatch(plot <- plot + geom_segment(aes(x=time, y=top_1, xend=time, yend=top_2),data=ref_true, col="blue"),
           error = function(e) NULL)
  tryCatch(plot <- plot + geom_segment(aes(x=time, y=top_1, xend=time, yend=top_2),data=events_true, col="orange"),
           error = function(e) NULL)
  tryCatch(plot <- plot + geom_segment(aes(x=time, y=top_1, xend=time, yend=top_2),data=ref_events_true, col="green"),
           error = function(e) NULL)
  
  #Plotting bottom bar
  plot <- plot + geom_hline(aes(yintercept = bottom_1),data=events_cp, col="black", size = 0.5)
  plot <- plot + geom_hline(aes(yintercept = bottom_2),data=events_cp, col="black", size = 0.5)
  tryCatch(plot <- plot + geom_segment(aes(x=time, y=bottom_1, xend=time, yend=bottom_2),data=ref_true, col="blue"),
           error = function(e) NULL)
  tryCatch(plot <- plot + geom_segment(aes(x=time, y=bottom_1, xend=time, yend=bottom_2),data=events_true, col="orange"),
           error = function(e) NULL)
  tryCatch(plot <- plot + geom_segment(aes(x=time, y=bottom_1, xend=time, yend=bottom_2),data=ref_events_true, col="green"),
           error = function(e) NULL)
  
  #Plotting relevant event points
  tryCatch(plot <- plot + geom_point(aes(x=time, y=serie),data=data_ref_true, size=mark.size, col="blue"),
           error = function(e) NULL)
  tryCatch(plot <- plot + geom_point(aes(x=time, y=serie),data=data_events_true, size=mark.size, col="orange"),
           error = function(e) NULL)
  tryCatch(plot <- plot + geom_point(aes(x=time, y=serie),data=data_ref_events_true, size=mark.size, col="green"),
           error = function(e) NULL)
  
  return(plot)
}
plot_srag <- function(serie, state, type, anomaly.size=0.5){
  
  an_v3 <- get_anomaly_srag(serie, state)
  
  filtro <- c('datadia', state)
  max_casos <- max(serie[[state]])
  lims <- as.POSIXct(strptime(c("2009-02-01","2020-06-01"), format = "%Y-%m-%d")) 
  
  if ( max_casos < 50){
    p <- evtplot(serie[filtro], an_v3, mark.cp=TRUE, mark.size=anomaly.size, ylim = c(0, 50)) +
      scale_x_datetime(date_breaks = "12 months",
                       minor_breaks = "6 months",
                       date_labels = "%m-%Y",
                       limits = lims) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))#, aspect.ratio = 2/5)
  }
  else{
    p <- evtplot(serie[filtro], an_v3, mark.cp=TRUE, mark.size=anomaly.size, ylim = c(0, max_casos + 100)) +
      scale_x_datetime(date_breaks = "12 months",
                       minor_breaks = "6 months", 
                       date_labels = "%m-%Y", 
                       limits = lims) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))#, aspect.ratio = 2/5)
  }
  p <- p + ylab(type) + xlab("date")
  p <- p + ggtitle(state_name(state)) + theme(plot.title = element_text(margin = ggplot2::margin(b = 0), hjust=0.99))
  return(p)
}
calc_error <- function(serie){
  print("------- GENERATING THE ERRORS OF THE MODEL TABLE -------")
  serie_ma_16_19 <- compute_ma(serie) %>% filter(ano == 2019)
  
  serie_ma_15_18 <- compute_ma(serie) %>% filter((ano >= 2015 & ano <= 2018))
  serie_16_19 <- serie %>% filter((ano >= 2016 & ano <= 2019))
  error_baseline <- serie_16_19[, 3:29] - serie_ma_15_18[, 3:29]
  mean_noise_baseline <- apply(error_baseline, 2, mean)
  
  up <- sapply(error_baseline, FUN = upper_boundaries)
  low <- sapply(error_baseline, FUN = lower_boundaries)
  
  interval <- paste('[' , paste(round(low, 3), round(up, 3), sep = ', '), ']' ,sep = "") 
  
  data <- data.frame("epsilon_baseline" = round(mean_noise_baseline, 3), 
                     "interval" = interval) #tabela de intervalos de erros
  return(data)
}
calc_underreport_by_cps <- function(serie, serie_covid, hmdata){
  print("------- GENERATING THE CUMULATIVE UNDERREPORTED TABLE BY CHANGE POINTS-------")
  serie_sema_16_19 <- compute_ma(serie) %>% filter(ano == 2019) #SEMA to test the expected error in 2020 first 8 weeks 
  serie_2020 <- serie %>% filter(ano == 2020) 
  
  serie_sema_15_18 <- compute_ma(serie) %>% filter((ano >= 2015 & ano <= 2018)) #SEMA to calculate the noise of the method
  serie_16_19 <- serie %>% filter((ano >= 2016 & ano <= 2019)) 
  error_baseline <- serie_16_19[, 3:29] - serie_sema_15_18[, 3:29] #testing the noise
  mean_noise_baseline <- apply(error_baseline, 2, mean) #mean of the noise of the method
  
  each_size <- nrow(serie_2020)# - 10
  
  mean_df <- data.frame(lapply(mean_noise_baseline, function(x) t(data.frame(x))))
  mean_df <- mean_df[rep(seq_len(nrow(mean_df)), each =each_size), ]
  row.names(mean_df) <- seq(1, each_size)
  
  uppers <- sapply(error_baseline, FUN = upper_boundaries)
  uppers <- data.frame(lapply(uppers, function(x) t(data.frame(x))))
  uppers <- uppers[rep(seq_len(nrow(uppers)), each = each_size), ]
  row.names(uppers) <- seq(1, each_size)
  lowers <- sapply(error_baseline, FUN = lower_boundaries)
  lowers <- data.frame(lapply(lowers, function(x) t(data.frame(x))))
  lowers <- lowers[rep(seq_len(nrow(lowers)), each = each_size), ]
  row.names(lowers) <- seq(1, each_size)
  
  novelty <- data.frame(row.names = seq(1, each_size))
  novelty_upper <- data.frame(row.names = seq(1, each_size))
  novelty_lower <- data.frame(row.names = seq(1, each_size))
  p_values_novelty <- c()
  p_values_ur <- c()
  cum_covid <- c()
  #change_weeks <- c()
  states <- colnames(mean_df)
  
  for (state in states){
    ftr <- c('ano', 'semana', state)
    cw <- get_2020_cp(serie[ftr])
    cw <- (strsplit(cw,'-')[[1]][2])
    
    EW <- as.integer(cw)
    if(is.na(EW)){ #corrige erro do PR
      EW <- 12
    }
    col_nov <- c(rep(0, (EW-1)))
    col_up <- c(rep(0, (EW-1)))
    col_low <- c(rep(0, (EW-1)))
    col_nov_mean <- c(rep(0, (EW-1)))
    
    col_nov <- c(col_nov, serie_2020[EW:each_size, state] - serie_sema_16_19[EW:each_size, state])
    novelty[state] <- col_nov
    
    wilcoxtest_novelty <- wilcox.test(novelty[[state]], error_baseline[[state]], alternative = 'greater') # verifica se houve novidade
    p_values_novelty <- c(p_values_novelty, wilcoxtest_novelty$p.value)
    
    col_up <- c(col_up, novelty[EW:each_size, state] - lowers[EW:each_size, state])
    novelty_upper[state] <- col_up
    
    col_low <- c(col_low, novelty[EW:each_size, state] - uppers[EW:each_size, state])
    novelty_lower[state] <- col_low
    
    col_nov_mean <- c(col_nov_mean, novelty[EW:each_size, state] - mean_df[EW:each_size, state])
    novelty[state] <- col_nov_mean
    
    wilcoxtest_ur <- wilcox.test(novelty[, state], serie_covid[, state], #EW ou todo o periodo de covid?
                                 alternative = 'greater', paired = TRUE) # verifica se houve novidade, teste pareado 
    p_values_ur <- c(p_values_ur, wilcoxtest_ur$p.value)
    
    cum_covid <- c(cum_covid, tail(cumsum(serie_covid[, state]), 1))
  }
  ur_inf <- novelty_lower
  ur_middle <- novelty
  ur_sup <- novelty_upper
  
  df_p_values <- data.frame("epsilon_2020" = mean_noise_baseline,
                            "random_noise_test" = (p_values_novelty < 0.05),
                            "reported_values_test" = (p_values_ur < 0.05)) #teste de p_values
  df_p_values$epsilon_2020 <- NULL
  print("------- P-VALUES TEST TABLE -------")
  print(df_p_values)
  
  ur_inf_tbl <- t(tail(cumsum(ur_inf), 1))
  ur_middle_tbl <- t(tail(cumsum(ur_middle), 1))
  ur_sup_tbl <- t(tail(cumsum(ur_sup), 1))
  
  acc_table <- data.frame('inferior' = round(ur_inf_tbl, 0),
                          'middle' = round(ur_middle_tbl, 0),
                          'superior' = round(ur_sup_tbl, 0),
                          'covid_cum' = round(cum_covid, 0)) #create the accumulated table
  
  colnames(acc_table)[1] <- "inferior"
  colnames(acc_table)[2] <- "predicted"
  colnames(acc_table)[3] <- "superior"
  colnames(acc_table)[4] <- "cum_covid"
  
  table_to_show <- acc_table
  table_to_show[table_to_show < 0] <- 0
  table_to_show <-table_to_show[c('predicted', "cum_covid")]
  
  txs <- calc_ur_rate(acc_table)
  txs$rate = paste(txs[['Rate observed']],txs[['Confidence interval']],
                   sep = ' +/- ')
  
  UR_table <- table_to_show
  UR_table$rate <- txs$rate
  UR_table$Acc._Covid_HM <- hmdata$x
  
  colnames(UR_table)[1] <- "Acc._Novelty_SARI"
  colnames(UR_table)[2] <- "Acc._Covid_SARI"
  colnames(UR_table)[3] <- "Rate"
  
  for (i in seq(1:27)){
    if (df_p_values$random_noise_test[i] == FALSE){
      UR_table$Rate[i] <- "*"
    }else{
      if(df_p_values$reported_values_test[i] == FALSE){
        UR_table$Rate[i] <- "**"
      }
    }
  }
  return(UR_table) 
}
calc_underreport <- function(serie, serie_covid, hmdata){
  print("------- GENERATING THE CUMULATIVE UNDERREPORTED TABLE -------")
  
  serie_sema_16_19 <- compute_ma(serie) %>% filter(ano == 2019) #SEMA to test the expected error in 2020 first 8 weeks 
  serie_2020 <- serie %>% filter(ano == 2020) 
  
  serie_sema_15_18 <- compute_ma(serie) %>% filter((ano >= 2015 & ano <= 2018)) #SEMA to calculate the noise of the method
  serie_16_19 <- serie %>% filter((ano >= 2016 & ano <= 2019)) 
  error_baseline <- serie_16_19[, 3:29] - serie_sema_15_18[, 3:29] #testing the noise
  mean_noise_baseline <- apply(error_baseline, 2, mean) #mean of the noise of the method
  
  each_size <- nrow(serie_2020) - 10
  
  mean_df <- data.frame(lapply(mean_noise_baseline, function(x) t(data.frame(x))))
  mean_df <- mean_df[rep(seq_len(nrow(mean_df)), each =each_size), ]
  
  novelty <- serie_2020[11:nrow(serie_2020), 3:29] - serie_sema_16_19[11:nrow(serie_2020), 3:29] #removing the noise from 2020
  
  wilcoxtest_novelty <- mapply(wilcox.test, novelty, error_baseline, alternative = 'greater') # verifica se houve novidade
  p_values_novelty <- wilcoxtest_novelty[seq(3, length(wilcoxtest_novelty), 7)]
  
  uppers <- sapply(error_baseline, FUN = upper_boundaries)
  uppers <- data.frame(lapply(uppers, function(x) t(data.frame(x))))
  uppers <- uppers[rep(seq_len(nrow(uppers)), each = each_size), ]
  lowers <- sapply(error_baseline, FUN = lower_boundaries)
  lowers <- data.frame(lapply(lowers, function(x) t(data.frame(x))))
  lowers <- lowers[rep(seq_len(nrow(lowers)), each = each_size), ]
  #TIRO O ERRO POR SEMANA
  novelty_upper <- novelty - lowers #boundaries 
  novelty_lower <- novelty - uppers #boundaries
  novelty <- novelty - mean_df
  
  wilcoxtest_ur <- mapply(wilcox.test, novelty, serie_covid[11:nrow(serie_2020),3:29],
                          alternative = 'greater', paired = TRUE) # verifica se houve novidade, teste pareado 
  p_values_ur <- wilcoxtest_ur[seq(3, length(wilcoxtest_ur), 7)]
  
  cum_covid <- t(tail(cumsum(serie_covid[11:nrow(serie_2020), 3:29]), 1))[,1]
  ur_inf <- novelty_lower
  ur_middle <- novelty
  ur_sup <- novelty_upper
  
  df_p_values <- data.frame("epsilon_2020" = mean_noise_baseline,
                            "random_noise_test" = (unlist(p_values_novelty) < 0.05),
                            "reported_values_test" = (unlist(p_values_ur) < 0.05)) #teste de p_values
  df_p_values$epsilon_2020 <- NULL
  print("------- P-VALUES TEST TABLE -------")
  print(df_p_values)
  
  ur_inf_tbl <- t(tail(cumsum(ur_inf), 1))
  ur_middle_tbl <- t(tail(cumsum(ur_middle), 1))
  ur_sup_tbl <- t(tail(cumsum(ur_sup), 1))
  
  acc_table <- data.frame('inferior' = round(ur_inf_tbl, 0),
                          'middle' = round(ur_middle_tbl, 0),
                          'superior' = round(ur_sup_tbl, 0),
                          'covid_cum' = round(cum_covid, 0)) #create the accumulated table
  
  colnames(acc_table)[1] <- "inferior"
  colnames(acc_table)[2] <- "predicted"
  colnames(acc_table)[3] <- "superior"
  colnames(acc_table)[4] <- "cum_covid"
  
  table_to_show <- acc_table
  table_to_show[table_to_show < 0] <- 0
  table_to_show <-table_to_show[c('predicted', "cum_covid")]
  
  txs <- calc_ur_rate(acc_table)
  txs$rate = paste(txs[['Rate observed']],txs[['Confidence interval']],
                   sep = ' +/- ')
  
  
  UR_table <- table_to_show
  UR_table$rate <- txs$rate
  UR_table$Cum._Covid_HM <- hmdata$x
  
  colnames(UR_table)[1] <- "Cum._Novelty_SARI"
  colnames(UR_table)[2] <- "Cum._Covid_SARI"
  colnames(UR_table)[3] <- "Rate"
  
  for (i in seq(1:27)){
    if (df_p_values$random_noise_test[i] == FALSE){
      UR_table$Rate[i] <- "*"
    }else{
      if(df_p_values$reported_values_test[i] == FALSE){
        UR_table$Rate[i] <- "**"
      }
    }
  }
  return(UR_table)
}
calc_underreport_by_cps_online <- function(serie, serie_covid, hmdata){
  print("------- GENERATING THE CUMULATIVE UNDERREPORTED TABLE BY CHANGE POINTS-------")
  serie_sema_16_19 <- compute_ma(serie) %>% filter(ano == 2019) #SEMA to test the expected error in 2020 first 8 weeks 
  serie_2020 <- serie %>% filter(ano == 2020) 
  
  serie_sema_15_18 <- compute_ma(serie) %>% filter((ano >= 2015 & ano <= 2018)) #SEMA to calculate the noise of the method
  serie_16_19 <- serie %>% filter((ano >= 2016 & ano <= 2019)) 
  error_baseline <- serie_16_19[, 3:29] - serie_sema_15_18[, 3:29] #testing the noise
  mean_noise_baseline <- apply(error_baseline, 2, mean) #mean of the noise of the method
  
  each_size <- nrow(serie_2020)# - 10
  
  mean_df <- data.frame(lapply(mean_noise_baseline, function(x) t(data.frame(x))))
  mean_df <- mean_df[rep(seq_len(nrow(mean_df)), each =each_size), ]
  row.names(mean_df) <- seq(1, each_size)
  
  uppers <- sapply(error_baseline, FUN = upper_boundaries)
  uppers <- data.frame(lapply(uppers, function(x) t(data.frame(x))))
  uppers <- uppers[rep(seq_len(nrow(uppers)), each = each_size), ]
  row.names(uppers) <- seq(1, each_size)
  lowers <- sapply(error_baseline, FUN = lower_boundaries)
  lowers <- data.frame(lapply(lowers, function(x) t(data.frame(x))))
  lowers <- lowers[rep(seq_len(nrow(lowers)), each = each_size), ]
  row.names(lowers) <- seq(1, each_size)
  
  novelty <- data.frame(row.names = seq(1, each_size))
  novelty_upper <- data.frame(row.names = seq(1, each_size))
  novelty_lower <- data.frame(row.names = seq(1, each_size))
  p_values_novelty <- c()
  p_values_ur <- c()
  acc_covid <- data.frame(row.names = seq(1, each_size))
  states <- colnames(mean_df)
  size <- nrow(serie)
  change_weeks <- c()
  for (state in states){
    ftr <- c('ano', 'semana', state)
    serie_ftr <- serie[ftr]
    for (i in seq(each_size-1, 1)){
      weeklim <- size-i
      serie_w <- serie_ftr[1:weeklim, ]
      cp <-get_2020_cp(serie_w)
      if(cp != 'XXXX-XX'){
        #print(weeklim)
        cw <- (strsplit(cp,'-')[[1]][2])
        break
      }else{
        if(i==1){
          cw <- cp #c(first_cp_2020, cp)
        }
      }
    }
    change_weeks <- c(change_weeks, cp)
    EW <- as.integer(cw)
    if(is.na(EW)){ #corrige erro do PR
      EW <- 11 #ponto de mudanca de obitos no PR
    }
    col_nov <- c(rep(0, (EW-1)))
    col_up <- c(rep(0, (EW-1)))
    col_low <- c(rep(0, (EW-1)))
    col_nov_mean <- c(rep(0, (EW-1)))
    col_cov <- c(rep(0, (EW-1)))
    
    col_nov <- c(col_nov, serie_2020[EW:each_size, state] - serie_sema_16_19[EW:each_size, state])
    novelty[state] <- col_nov
    
    wilcoxtest_novelty <- wilcox.test(novelty[[state]], error_baseline[[state]], alternative = 'greater') # verifica se houve novidade
    p_values_novelty <- c(p_values_novelty, wilcoxtest_novelty$p.value)
    
    col_up <- c(col_up, novelty[EW:each_size, state] - lowers[EW:each_size, state])
    novelty_upper[state] <- col_up
    
    col_low <- c(col_low, novelty[EW:each_size, state] - uppers[EW:each_size, state])
    novelty_lower[state] <- col_low
    
    col_nov_mean <- c(col_nov_mean, novelty[EW:each_size, state] - mean_df[EW:each_size, state])
    novelty[state] <- col_nov_mean
    
    wilcoxtest_ur <- wilcox.test(novelty[, state], serie_covid[, state], 
                                 alternative = 'greater', paired = TRUE) # verifica se houve novidade, teste pareado 
    p_values_ur <- c(p_values_ur, wilcoxtest_ur$p.value)
    
    col_cov <- c(col_cov, (cumsum(serie_covid[EW:each_size, state])))
    acc_covid[state] <- col_cov
  }
 
  ur_inf <- novelty_lower
  ur_middle <- novelty
  ur_sup <- novelty_upper
  
  df_p_values <- data.frame("epsilon_2020" = mean_noise_baseline,
                            "random_noise_test" = (p_values_novelty < 0.05),
                            "reported_values_test" = (p_values_ur < 0.05)) #teste de p_values
  df_p_values$epsilon_2020 <- NULL
  print("------- P-VALUES TEST TABLE -------")
  print(df_p_values)
  
  
  ur_inf_tbl <- cumsum(ur_inf)
  ur_middle_tbl <- cumsum(ur_middle)
  ur_sup_tbl <- cumsum(ur_sup)
  
  tx_inf_weekly <- calc_ur_rate_weekly(ur_inf_tbl, acc_covid)
  tx_mid_weekly <- calc_ur_rate_weekly(ur_middle_tbl, acc_covid)
  tx_sup_weekly <- calc_ur_rate_weekly(ur_sup_tbl, acc_covid)
  
  acc_table <- data.frame('inferior' = t(tail(tx_inf_weekly, 1)),
                          'middle' = t(tail(tx_mid_weekly, 1)),
                          'superior' = t(tail(tx_sup_weekly, 1)),
                          'covid_cum' = t(tail(acc_covid, 1))) #create the accumulated table
  
  colnames(acc_table) <- c("inferior", 'predicted', 'superior', 'acc_covid')
  
  table_to_show <- acc_table
  table_to_show[table_to_show < 0] <- 0
  table_to_show <-table_to_show[c('predicted', "acc_covid")]
  
  txs <- data.frame('predicted' = table_to_show['predicted'] )
  sup_minus_mid <- acc_table['superior'] - acc_table['predicted']
  mid_minus_inf <- acc_table['predicted'] - acc_table['inferior'] 
  ci <- pmax(sup_minus_mid, mid_minus_inf) #get the max value between the two differences generated
  txs$ci <- round(ci$superior, 3)
  
  txs$rate = paste(txs[['predicted']], txs[['ci']],
                   sep = ' +/- ')
  
  
  UR_table <- table_to_show
  UR_table$predicted <- t(round(tail(ur_middle_tbl, 1), 0))
  UR_table$rate <- txs$rate
  UR_table$Acc._Covid_HM <- hmdata$x
  UR_table$CP <- change_weeks
  
  
  names(UR_table) <- c("Acc._Novelty_SARI", "Acc._Covid_SARI", "Rate",'Acc._Covid_HM', 'Change_Point')
  
  for (i in seq(1:27)){
    if (df_p_values$random_noise_test[i] == FALSE){
      UR_table$Rate[i] <- "*"
    }else{
      if(df_p_values$reported_values_test[i] == FALSE){
        UR_table$Rate[i] <- "**"
      }
    }
  }
  print(UR_table)
  return(tx_mid_weekly) #TO PLOT
}
calc_ur_rate_weekly <- function(ur_acc, acc){ ## acc table is generated by calc_underreport_weekly()
  ur <- ur_acc - acc        
  ur[ur < 0] <- 0
  ur_rate <- (ur / acc)
  rate <- round(ur_rate, 3)
  return(rate)
}
calc_ur_rate <- function(acc_table){ ## acc table is generated by calc_underreport()
  cov <- t(acc_table[4])
  inf <- t(acc_table[1]) - cov
  mid <- t(acc_table[2]) - cov
  sup <- t(acc_table[3]) - cov        
  inf[inf < 0] <- 0
  mid[mid < 0] <- 0
  sup[sup < 0] <- 0
  
  ur_inf <- (inf / cov)
  ur_mid <- (mid / cov)
  ur_sup <- (sup / cov)
  
  sup_minus_mid <- ur_sup - ur_mid
  mid_minus_inf <- ur_mid - ur_inf 
  ci <- pmax(sup_minus_mid, mid_minus_inf) #get the max value between the two differences generated
  
  rate_table <- data.frame('Baseline' = t(round(ur_mid, 3)),
                           'intervalo de confianca' = t(round(ci, 3)))
  
  colnames(rate_table)[1] <- "Rate observed"
  colnames(rate_table)[2] <- "Confidence interval"
  
  return(rate_table)
}
lower_boundaries <- function(x, replic=1000) {
  set.seed(10)
  # function to obtain the mean
  Bmean <- function(data, i) {
    d <- data[i] # allows boot to select sample
    return(mean(d))
  }
  results <- boot(data=x, statistic=Bmean, R=replic)  
  result <- boot.ci(results, type=c("bca"))
  return(lower=result$bca[4])
}
upper_boundaries <- function(x, replic=1000) {
  set.seed(10)
  # function to obtain the mean
  Bmean <- function(data, i) {
    d <- data[i] # allows boot to select sample
    return(mean(d))
  }
  results <- boot(data=x, statistic=Bmean, R=replic)  
  result <- boot.ci(results, type=c("bca"))
  return(upper = (result$bca[5]))
}

create_states <- function() {
  data <- read.table(
    text = "Name,State
Acre,AC
Alagoas,AL
Amapá,AP
Amazonas,AM
Bahia,BA
Ceará,CE
Espírito Santo,ES
Goiás,GO
Maranhão,MA
Mato Grosso,MT
Mato Grosso do Sul,MS
Minas Gerais,MG
Pará,PA
Paraíba,PB
Paraná,PR
Pernambuco,PE
Piauí,PI
Rio de Janeiro,RJ
Rio Grande do Norte,RN
Rio Grande do Sul,RS
Rondônia,RO
Roraima,RR
Santa Catarina,SC
São Paulo,SP
Sergipe,SE
Tocantins,TO
Distrito Federal,DF",
    header = TRUE,
    sep = ","
  )
  return(data)
}

states <- create_states()

state_name <- function(state)  {
  return(states$Name[states$State==state])
}
