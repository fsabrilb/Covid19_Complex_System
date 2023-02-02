# Libraries ----
library(dplyr)
library(readr)
library(data.table)

library(Cairo)
library(ggplot2)
library(curl)
library(goft)
library(grid)
library(httr)
library(pracma)
library(readxl)
library(scales)
library(stable)
library(ExtDist)
library(forecast)
library(openxlsx)
library(reshape2)
library(gridExtra)
library(lubridate)
library(univariateML)

#### Graph Theil index function ####
Graph_Theil <- function(Theil_Data, Y_Label, DateBegin, DateEnd){
  #### Graph of Theil index Cases and deaths ####
  Theil <- Theil_Data
  Theil$Date_End <- as.Date(Theil$Date_End)
  Graph1 <- ggplot() + 
    cowplot::theme_half_open(20) + 
    theme(legend.title=element_blank(),
          axis.ticks = element_line(colour = "black", size = 1.05),
          panel.border = element_rect(colour = "black", fill=NA, size = 1.15),
          panel.grid.major = element_line(colour='grey93', size = 1.00),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title.x.top = element_blank(),
          axis.text.x.top = element_blank(),
          axis.text.y.right = element_text(size = rel(0.8)),
          axis.text.x.bottom = element_text(size = rel(0.7)),
          axis.text.y.left = element_text(size = rel(0.8)),
          legend.text = element_text(size = rel(0.8)),
          legend.position = 'bottom',
          panel.background = element_rect(fill = "transparent")) +
    ## Theil index Cases
    geom_line(data = Theil,
              aes(x = Date_End, y = Theil0_Inf, 
                  colour = paste0('Cases')), 
              size = 1.2, linetype = 'dotted') +
    geom_point(data = Theil,
               aes(x = Date_End, y = Theil0_Inf, 
                   colour = paste0('Cases')),
               size = 3.9, pch = 15) +
    ## Theil index Deaths
    geom_line(data = Theil,
              aes(x = Date_End, y = Theil0_Dea, 
                  colour = paste0('Deaths')), 
              size = 1.2, linetype = 'dotted') +
    geom_point(data = Theil,
               aes(x = Date_End, y = Theil0_Dea, 
                   colour = paste0('Deaths')),
               size = 3.9, pch = 15) +
    ## Labels
    scale_x_date(date_breaks = '2 weeks', 
                 date_minor_breaks = '2 weeks',
                 sec.axis = dup_axis()) +
    scale_y_continuous(n.breaks = 25) +
    labs(x = 'Date',
         y = 'Theil index',
         subtitle = paste0('Theil index of cases and deaths - ', Y_Label)) +
    scale_color_manual(values = c("Firebrick1","Firebrick4",
                                  "darkgreen","darkolivegreen4",
                                  "darkmagenta",
                                  "darkgoldenrod","lightgoldenrod4")) +
    coord_cartesian(xlim = c(as.Date(DateBegin), as.Date(DateEnd)))
  #### Graph of Modified Theil index Cases and deaths ####
  Graph2 <- ggplot() + 
    cowplot::theme_half_open(20) + 
    theme(legend.title=element_blank(),
          axis.ticks = element_line(colour = "black", size = 1.05),
          panel.border = element_rect(colour = "black", fill=NA, size = 1.15),
          panel.grid.major = element_line(colour='grey93', size = 1.00),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title.x.top = element_blank(),
          axis.text.x.top = element_blank(),
          axis.text.y.right = element_text(size = rel(0.8)),
          axis.text.x.bottom = element_text(size = rel(0.7)),
          axis.text.y.left = element_text(size = rel(0.8)),
          legend.text = element_text(size = rel(0.8)),
          legend.position = 'bottom',
          panel.background = element_rect(fill = "transparent")) +
    ## Theil index Cases
    geom_line(data = Theil,
              aes(x = Date_End, y = Theil1_Inf, 
                  colour = paste0('Cases')), 
              size = 1.2, linetype = 'dotted') +
    geom_point(data = Theil,
               aes(x = Date_End, y = Theil1_Inf, 
                   colour = paste0('Cases')),
               size = 3.9, pch = 15) +
    ## Theil index Deaths
    geom_line(data = Theil,
              aes(x = Date_End, y = Theil1_Dea, 
                  colour = paste0('Deaths')), 
              size = 1.2, linetype = 'dotted') +
    geom_point(data = Theil,
               aes(x = Date_End, y = Theil1_Dea, 
                   colour = paste0('Deaths')),
               size = 3.9, pch = 15) +
    ## Labels
    scale_x_date(date_breaks = '2 weeks', 
                 date_minor_breaks = '2 weeks',
                 sec.axis = dup_axis()) +
    scale_y_continuous(n.breaks = 25) +
    labs(x = 'Date',
         y = 'Theil index',
         subtitle = paste0('Modified Theil index of cases and deaths - ', 
                           Y_Label)) +
    scale_color_manual(values = c("darkorange","darkorange4",
                                  "midnightblue","steelblue",
                                  "Firebrick1","Firebrick4",
                                  "darkgreen","darkolivegreen4",
                                  "darkmagenta",
                                  "darkgoldenrod","lightgoldenrod4")) +
    coord_cartesian(xlim = c(as.Date(DateBegin), as.Date(DateEnd)))
  #### Graph of Normalized Theil index Cases and deaths ####
  Graph3 <- ggplot() + 
    cowplot::theme_half_open(20) + 
    theme(legend.title=element_blank(),
          axis.ticks = element_line(colour = "black", size = 1.05),
          panel.border = element_rect(colour = "black", fill=NA, size = 1.15),
          panel.grid.major = element_line(colour='grey93', size = 1.00),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title.x.top = element_blank(),
          axis.text.x.top = element_blank(),
          axis.text.y.right = element_text(size = rel(0.8)),
          axis.text.x.bottom = element_text(size = rel(0.7)),
          axis.text.y.left = element_text(size = rel(0.8)),
          legend.text = element_text(size = rel(0.8)),
          legend.position = 'bottom',
          panel.background = element_rect(fill = "transparent")) +
    ## Theil index Cases
    geom_line(data = Theil,
              aes(x = Date_End, y = Normalized_Theil0_Inf, 
                  colour = paste0('Cases T0')), 
              size = 1.2, linetype = 'dotted') +
    geom_point(data = Theil,
               aes(x = Date_End, y = Normalized_Theil0_Inf, 
                   colour = paste0('Cases T0')),
               size = 3.9, pch = 15) +
    ## Modified Theil index Cases
    geom_line(data = Theil,
              aes(x = Date_End, y = Normalized_Theil1_Inf, 
                  colour = paste0('Cases T1')), 
              size = 1.2, linetype = 'dotted') +
    geom_point(data = Theil,
               aes(x = Date_End, y = Normalized_Theil1_Inf, 
                   colour = paste0('Cases T1')),
               size = 3.9, pch = 15) +
    ## Theil index Deaths
    geom_line(data = Theil,
              aes(x = Date_End, y = Normalized_Theil0_Dea, 
                  colour = paste0('Deaths T0')), 
              size = 1.2, linetype = 'dotted') +
    geom_point(data = Theil,
               aes(x = Date_End, y = Normalized_Theil0_Dea, 
                   colour = paste0('Deaths T0')),
               size = 3.9, pch = 15) +
    ## Modified Theil index Deaths
    geom_line(data = Theil,
              aes(x = Date_End, y = Normalized_Theil1_Dea, 
                  colour = paste0('Deaths T1')), 
              size = 1.2, linetype = 'dotted') +
    geom_point(data = Theil,
               aes(x = Date_End, y = Normalized_Theil1_Dea, 
                   colour = paste0('Deaths T1')),
               size = 3.9, pch = 15) +
    ## Labels
    scale_x_date(date_breaks = '2 weeks', 
                 date_minor_breaks = '2 weeks',
                 sec.axis = dup_axis()) +
    scale_y_continuous(n.breaks = 25) +
    labs(x = 'Date',
         y = 'Normalized Theil index',
         subtitle = paste0('Normalized Theil index of cases and deaths - ', 
                           Y_Label)) +
    scale_color_manual(values = c("darkorange","darkorange4",
                                  "midnightblue","steelblue",
                                  "Firebrick1","Firebrick4",
                                  "darkgreen","darkolivegreen4",
                                  "darkmagenta",
                                  "darkgoldenrod","lightgoldenrod4")) +
    coord_cartesian(xlim = c(as.Date(DateBegin), as.Date(DateEnd)))
  #### Return of the function ####
  MyList <- list('Theil0' = Graph1, 'Theil1' = Graph2, 'TheilN' = Graph3)
  return(MyList)
}
#### TFS of cumulative cases and deaths + Theil + Hurst function ####
Temporal_Scaling <- function(Hurst_Data, Theil_Data, Init_TFS){
  ## Hurst Raw Data
  H_Data <- Hurst_Data$Hurst_Exponents
  T_Data <- Theil_Data
  
  TFS_Data <- data.frame()
  #### Calculating TFS over cumulative cases and deaths (REAL TFS) ####
  for(i in c(Init_TFS:length(unique(T_Data$Date_End)))){
    Var_Inf <-  T_Data$Var_Cum_Inf[1:i]
    Mea_Inf <- T_Data$Mean_Cum_Inf[1:i]
    Var_Dea <-  T_Data$Var_Cum_Dea[1:i]
    Mea_Dea <- T_Data$Mean_Cum_Dea[1:i]
    
    ## Regression to power Law Cases
    A_Inf <- cov(x = log(Mea_Inf), y = log(Var_Inf),
                 use="complete.obs")/var(x = log(Mea_Inf), na.rm = TRUE)
    K_Inf <- exp(mean(log(Var_Inf), na.rm = T)
                 - A_Inf*mean(log(Mea_Inf), na.rm = T))
    R_Inf <- cor(x = log(Mea_Inf), y = log(Var_Inf),use="complete.obs")
    ## Regression to power Law Deaths
    A_Dea <- cov(x = log(Mea_Dea), y = log(Var_Dea),
                 use="complete.obs")/var(x = log(Mea_Dea), na.rm = TRUE)
    K_Dea <- exp(mean(log(Var_Dea), na.rm = T)
                 - A_Dea*mean(log(Mea_Dea), na.rm = T))
    R_Dea <- cor(x = log(Mea_Dea), y = log(Var_Dea),use="complete.obs")
    
    ## Data
    TFS_Data <- rbind(
      TFS_Data,
      data.frame(
        'Date_End' = unique(T_Data$Date_End)[i],
        'Alpha_Cases'  = A_Inf, 'K_Cases'  = K_Inf, 'R2_Cases'  = R_Inf^2,
        'Alpha_Deaths' = A_Dea, 'K_Deaths' = K_Dea, 'R2_Deaths' = R_Dea^2
      )
    )
    
    cat(paste0('TFS Dates until ', unique(T_Data$Date_End)[i], '\n'))
  }
  #### Merge Data #### 
  Temp_Scaling_Data <- merge(TFS_Data, T_Data, by = 'Date_End')
  Temp_Scaling_Data <- merge(Temp_Scaling_Data, H_Data, by = 'Date_End')
  Temp_Scaling_Data <- Temp_Scaling_Data[,c(8,1:7,9:32,34,35)]
  names(Temp_Scaling_Data)[1] <- 'Date_Start'
  #### Function return ####
  return(Temp_Scaling_Data)
}
#### Graph Temporal Scaling function ####
Graph_Temporal_Scaling <- function(Temporal_Data, Y_Label){
  #### Graph of Alpha TFS vs Theil index Cases and deaths ####
  Graph1 <- ggplot() + 
    cowplot::theme_half_open(20) + 
    theme(legend.title=element_blank(),
          axis.ticks = element_line(colour = "black", size = 1.05),
          panel.border = element_rect(colour = "black", fill=NA, size = 1.15),
          panel.grid.major = element_line(colour='grey93', size = 1.00),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title.x.top = element_blank(),
          axis.text.x.top = element_blank(),
          axis.text.y.right = element_text(size = rel(0.8)),
          axis.text.x.bottom = element_text(size = rel(0.7)),
          axis.text.y.left = element_text(size = rel(0.8)),
          legend.text = element_text(size = rel(0.8)),
          legend.position = 'bottom',
          panel.background = element_rect(fill = "transparent")) +
    ## Alpha vs Theil index Cases
    geom_line(data = Temporal_Data,
              aes(x = Alpha_Cases, y = Theil1_Inf_Cum, 
                  colour = paste0('Cases')), 
              size = 1.2, linetype = 'dotted') +
    geom_point(data = Temporal_Data,
               aes(x = Alpha_Cases, y = Theil1_Inf_Cum, 
                   colour = paste0('Cases')),
               size = 3.9, pch = 15) +
    ## Alpha vs Theil index Deaths
    geom_line(data = Temporal_Data,
              aes(x = Alpha_Deaths, y = Theil1_Dea_Cum, 
                  colour = paste0('Deaths')), 
              size = 1.2, linetype = 'dotted') +
    geom_point(data = Temporal_Data,
               aes(x = Alpha_Deaths, y = Theil1_Dea_Cum, 
                   colour = paste0('Deaths')),
               size = 3.9, pch = 15) +
    ## Labels
    scale_x_continuous(n.breaks = 25) +
    scale_y_continuous(n.breaks = 25) +
    labs(x = 'Alpha TFS',
         y = 'Theil index',
         subtitle = paste0('Theil index vs Alpha TFS of cases and deaths - ', 
                           Y_Label)) +
    scale_color_manual(values = c("Firebrick1","Firebrick4",
                                  "darkgreen","darkolivegreen4",
                                  "darkmagenta",
                                  "darkgoldenrod","lightgoldenrod4"))
  
  #### Graph of Alpha TFS vs Hurst Exponent Cases and deaths ####
  Graph2 <- ggplot() + 
    cowplot::theme_half_open(20) + 
    theme(legend.title=element_blank(),
          axis.ticks = element_line(colour = "black", size = 1.05),
          panel.border = element_rect(colour = "black", fill=NA, size = 1.15),
          panel.grid.major = element_line(colour='grey93', size = 1.00),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title.x.top = element_blank(),
          axis.text.x.top = element_blank(),
          axis.text.y.right = element_text(size = rel(0.8)),
          axis.text.x.bottom = element_text(size = rel(0.7)),
          axis.text.y.left = element_text(size = rel(0.8)),
          legend.text = element_text(size = rel(0.8)),
          legend.position = 'bottom',
          panel.background = element_rect(fill = "transparent")) +
    ## Alpha vs Hurst Exponent Cases
    geom_line(data = Temporal_Data,
              aes(x = Alpha_Cases, y = Hurst_Inf, 
                  colour = paste0('Cases')), 
              size = 1.2, linetype = 'dotted') +
    geom_point(data = Temporal_Data,
               aes(x = Alpha_Cases, y = Hurst_Inf, 
                   colour = paste0('Cases')),
               size = 3.9, pch = 15) +
    ## Alpha vs Hurst Exponent Deaths
    geom_line(data = Temporal_Data,
              aes(x = Alpha_Deaths, y = Hurst_Dea, 
                  colour = paste0('Deaths')), 
              size = 1.2, linetype = 'dotted') +
    geom_point(data = Temporal_Data,
               aes(x = Alpha_Deaths, y = Hurst_Dea, 
                   colour = paste0('Deaths')),
               size = 3.9, pch = 15) +
    ## Labels
    scale_x_continuous(n.breaks = 25) +
    scale_y_continuous(n.breaks = 25) +
    labs(x = 'Alpha TFS',
         y = 'Hurst Exponent',
         subtitle = paste0('Hurst Exponent vs Alpha TFS of cases and deaths - ', 
                           Y_Label)) +
    scale_color_manual(values = c("darkgreen","darkolivegreen4",
                                  "Firebrick1","Firebrick4",
                                  "darkmagenta",
                                  "darkgoldenrod","lightgoldenrod4"))
  #### Graph of Hurst Exponent vs Theil index Cases and deaths ####
  Graph3 <- ggplot() + 
    cowplot::theme_half_open(20) + 
    theme(legend.title=element_blank(),
          axis.ticks = element_line(colour = "black", size = 1.05),
          panel.border = element_rect(colour = "black", fill=NA, size = 1.15),
          panel.grid.major = element_line(colour='grey93', size = 1.00),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title.x.top = element_blank(),
          axis.text.x.top = element_blank(),
          axis.text.y.right = element_text(size = rel(0.8)),
          axis.text.x.bottom = element_text(size = rel(0.7)),
          axis.text.y.left = element_text(size = rel(0.8)),
          legend.text = element_text(size = rel(0.8)),
          legend.position = 'bottom',
          panel.background = element_rect(fill = "transparent")) +
    ## Hurst exponent vs Theil index Cases
    geom_line(data = Temporal_Data,
              aes(x = Hurst_Inf, y = Theil1_Inf_Cum, 
                  colour = paste0('Cases')), 
              size = 1.2, linetype = 'dotted') +
    geom_point(data = Temporal_Data,
               aes(x = Hurst_Inf, y = Theil1_Inf_Cum, 
                   colour = paste0('Cases')),
               size = 3.9, pch = 15) +
    ## Hurst exponent vs Theil index Deaths
    geom_line(data = Temporal_Data,
              aes(x = Hurst_Dea, y = Theil1_Dea_Cum, 
                  colour = paste0('Deaths')), 
              size = 1.2, linetype = 'dotted') +
    geom_point(data = Temporal_Data,
               aes(x = Hurst_Dea, y = Theil1_Dea_Cum, 
                   colour = paste0('Deaths')),
               size = 3.9, pch = 15) +
    ## Labels
    scale_x_continuous(n.breaks = 25) +
    scale_y_continuous(n.breaks = 25) +
    labs(x = 'Hurst exponent',
         y = 'Theil index',
         subtitle = paste0('Theil index vs Hurst Exponent of cases and deaths - ', 
                           Y_Label)) +
    scale_color_manual(values = c("Firebrick1","Firebrick4",
                                  "darkgreen","darkolivegreen4",
                                  "darkmagenta",
                                  "darkgoldenrod","lightgoldenrod4"))
  
  #### Return of the function ####
  MyList <- list('AlphaTheil' = Graph1, 
                 'AlphaHurst' = Graph2, 
                 'HurstTheil' = Graph3)
  return(MyList)
}
#### Filtering Data and save it ####
Raw_Data_Saving <- function(Data, FolderName, RawDataName, 
                            Y_Label_Count, Date_Begin_Count, Date_End_Count,
                            DatesVector_Adjust, FlagLognormal_Adjust,
                            FlagB_Adjust,
                            Y_Label_Adjust, Date_Begin_Adjust, Date_End_Adjust,
                            DatesVector_TFS, FlagParams_TFS,
                            Y_Label_TFS, Cols_TFS,
                            MA_Days, 
                            Y_Label_Corr, Date_Begin_Corr, Date_End_Corr,
                            DatesVector_Hurst, Windows_Number_Vector_Hurst,
                            Y_Label_Hurst, Date_Begin_Hurst, Date_End_Hurst,
                            DatesVector_Theil,
                            Y_Label_Theil, Date_Begin_Theil, Date_End_Theil,
                            Y_Label_Temporal,
                            Order, DateSave, Init_TFS, WindowMoving){
  #### Worbook ####
  ## New Directory with DateSave
  dir.create(paste0('Data_', DateSave))
  dir.create(paste0('Data_', DateSave, '/', FolderName))
  ## Info Data
  INFO <- data.frame(
    "Sheet" = c(2:14),
    "Info" = c(RawDataName,
               'Count by Spatial Division cumulative regions',
               'Akaike Information Criterion for daily cumulative cases and deaths',
               'Parameters for Lognormal and Burr adjusting',
               'TFS with cumulative daily cases and deaths using spatial region',
               'Simple Moving Average Raw Data',
               'Correlation between cases and deaths',
               'Hurst Raw Data of daily Cases and Deaths group by spatial region',
               'Hurst Exponent of daily Cases and Deaths group by spatial region',
               'Theil index of daily Cases and Deaths group by spatial region',
               'Moving Theil index of daily Cases and Deaths group by spatial region',
               'TFS (Cumulative), Hurst and Theil of daily Cases and Deaths group by spatial region',
               'TFS (Cumulative), Hurst and Moving Theil of daily Cases and Deaths group by spatial region')
  )
  ### Saving in a Workbook
  wb <- createWorkbook()
  ## Information
  addWorksheet(wb = wb, sheetName = 'Information')
  writeData(wb, sheet = 1, x = INFO, borders = "all",
            startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
  ## Raw Data
  addWorksheet(wb = wb, sheetName = 'Raw Data')
  writeData(wb, sheet = 2, x = Data, borders = "all", 
            startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
  ## Counts Regions by Spatial Division cumulative regions with cases and deaths
  addWorksheet(wb = wb, sheetName = 'Count Regions')
  Counts_Data <- Counts_Regions(Raw_Data = Data, Order = Order)
  writeData(wb, sheet = 3, x = Counts_Data, borders = "all", 
            startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
  ## Adjust to different distributions using AIC
  addWorksheet(wb = wb, sheetName = 'AIC')
  Adjust_Data <- AdjustVectorDay(Raw_Data = Data, 
                                 DatesVector = DatesVector_Adjust,
                                 FlagLognormal = FlagLognormal_Adjust,
                                 FlagB = FlagB_Adjust)
  writeData(wb, sheet = 4, x = Adjust_Data$AIC, borders = "all", 
            startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
  addWorksheet(wb = wb, sheetName = 'Params Lognormal and Burr')
  writeData(wb, sheet = 5, x = Adjust_Data$Params, borders = "all", 
            startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
  ## TFS with cumulative daily cases and deaths using spatial region (no only one day)
  addWorksheet(wb = wb, sheetName = 'TFS')
  TFS_Data <- TFS(Raw_Data = Data, DatesVector = DatesVector_TFS,
                  FlagParams = FlagParams_TFS)
  writeData(wb, sheet = 6, x = TFS_Data, borders = "all", 
            startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
  ## Temporal Correlation between Cases and Deaths
  addWorksheet(wb = wb, sheetName = 'MA Raw Data')
  Corr_Data <- Correlation(Raw_Data = Data, MA_Days = MA_Days)
  writeData(wb, sheet = 7, x = Corr_Data$DATA, borders = "all", 
            startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
  addWorksheet(wb = wb, sheetName = 'Correlation')
  writeData(wb, sheet = 8, x = Corr_Data$CORR, borders = "all", 
            startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
  ## Hurst Exponent of daily cases and Deaths group by spatial region
  addWorksheet(wb = wb, sheetName = 'Hurst Raw Data')
  Hurst_Data <- Hurst(Raw_Data = Data, DatesVector = DatesVector_Hurst,
                      Windows_Number_Vector = Windows_Number_Vector_Hurst)
  writeData(wb, sheet = 9, x = Hurst_Data$Hurst_Raw_Data, borders = "all", 
            startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
  addWorksheet(wb = wb, sheetName = 'Hurst Exponent')
  writeData(wb, sheet = 10, x = Hurst_Data$Hurst_Exponents, borders = "all", 
            startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
  ## Theil index of daily cases and Deaths group by spatial region
  addWorksheet(wb = wb, sheetName = 'Theil Raw Data')
  Theil_Data <- Theil(Raw_Data = Data, DatesVector = DatesVector_Theil)
  writeData(wb, sheet = 11, x = Theil_Data, borders = "all", 
            startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
  ## Moving Theil index of daily cases and Deaths group by spatial region
  addWorksheet(wb = wb, sheetName = 'Moving Theil Raw Data')
  Theil_Data_M <- TheilMoving(Raw_Data = Data, DatesVector = DatesVector_Theil,
                              WindowsSize = WindowMoving)
  writeData(wb, sheet = 12, x = Theil_Data_M, borders = "all", 
            startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
  ## TFS, Theil and Hurst of daily cases and Deaths group by spatial region
  addWorksheet(wb = wb, sheetName = 'Temporal Scaling Data')
  Temporal_Data <- Temporal_Scaling(Hurst_Data = Hurst_Data,
                                    Theil_Data = Theil_Data, 
                                    Init_TFS   = Init_TFS)
  writeData(wb, sheet = 13, x = Temporal_Data, borders = "all", 
            startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
  ## TFS, Moving Theil and Hurst of daily cases and Deaths group by spatial region
  addWorksheet(wb = wb, sheetName = 'Moving Temporal Scaling Data')
  Temporal_Data_M <- Temporal_Scaling(Hurst_Data = Hurst_Data,
                                      Theil_Data = Theil_Data_M, 
                                      Init_TFS   = Init_TFS)
  writeData(wb, sheet = 14, x = Temporal_Data_M, borders = "all", 
            startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
  
  ## Saving
  saveWorkbook(wb, paste0('Data_', DateSave,'/', FolderName, '/Data.xlsx'),
               overwrite = TRUE)
  print(paste0('Save Workbook'))
  #### Graphs ####
  ## Counts Region Graph
  Graph_Count <- Graph_Counts(
    Count_Data = Counts_Data, Y_Label = Y_Label_Count, 
    DateBegin = Date_Begin_Count, DateEnd = Date_End_Count)
  
  Cairo::Cairo(
    ## conversion de 72 ppi a 400 dpi entonces 400/72=5.55
    width = 640*1.85*5.55, #ancho
    height = 480*1.3375*5.55, #alto
    file = paste0('Data_', DateSave,'/', FolderName, '/Graph_Counts_Region.png'),
    type = "png", #tiff
    bg = "white", #white or transparent depending on your requirement 
    dpi = 400,
    units = "px" #you can change to pixels etc 
  )
  plot(Graph_Count)
  dev.off()
  print(paste0('Save Counts Region Graph'))
  
  ## AIC Graph
  Graph_Adjusts <- Graph_Adjust(
    Adjust_Data = Adjust_Data, Y_Label = Y_Label_Adjust, 
    DateBegin = Date_Begin_Adjust, DateEnd = Date_End_Adjust)
  
  Cairo::Cairo(
    ## conversion de 72 ppi a 400 dpi entonces 400/72=5.55
    width = 690*1.85*5.55, #ancho
    height = 500*1.3375*5.55, #alto
    file = paste0('Data_', DateSave,'/', FolderName, '/Graph_AIC.png'),
    type = "png", #tiff
    bg = "white", #white or transparent depending on your requirement 
    dpi = 900,
    units = "px" #you can change to pixels etc 
  )
  plot(Graph_Adjusts$AIC)
  dev.off()
  print(paste0('Save AIC Graph'))
  
  ## Mu Graph
  Cairo::Cairo(
    ## conversion de 72 ppi a 400 dpi entonces 400/72=5.55
    width = 640*1.85*5.55, #ancho
    height = 540*1.3375*5.55, #alto
    file = paste0('Data_', DateSave,'/', FolderName, '/Graph_Mu.png'),
    type = "png", #tiff
    bg = "white", #white or transparent depending on your requirement 
    dpi = 400,
    units = "px" #you can change to pixels etc 
  )
  plot(Graph_Adjusts$MU)
  dev.off()
  print(paste0('Save Mu Graph'))
  
  ## Sigma Graph
  Cairo::Cairo(
    ## conversion de 72 ppi a 400 dpi entonces 400/72=5.55
    width = 640*1.85*5.55, #ancho
    height = 540*1.3375*5.55, #alto
    file = paste0('Data_', DateSave,'/', FolderName, '/Graph_Sigma.png'),
    type = "png", #tiff
    bg = "white", #white or transparent depending on your requirement 
    dpi = 400,
    units = "px" #you can change to pixels etc 
  )
  plot(Graph_Adjusts$SIGMA)
  dev.off()
  print(paste0('Save Sigma Graph'))
  
  ## Burr G Graph
  Cairo::Cairo(
    ## conversion de 72 ppi a 400 dpi entonces 400/72=5.55
    width = 640*1.85*5.55, #ancho
    height = 540*1.3375*5.55, #alto
    file = paste0('Data_', DateSave,'/', FolderName, '/Graph_Burr_G.png'),
    type = "png", #tiff
    bg = "white", #white or transparent depending on your requirement 
    dpi = 400,
    units = "px" #you can change to pixels etc 
  )
  plot(Graph_Adjusts$G)
  dev.off()
  print(paste0('Save Burr G parameter Graph'))
  
  ## TFS Cases Graph using spatial region
  Graphs_TFS <- Graph_TFS(
    TFS_Data = TFS_Data, Y_Label = Y_Label_TFS, Cols  = Cols_TFS)
  
  Cairo::Cairo(
    ## conversion de 72 ppi a 400 dpi entonces 400/72=5.55
    width = 650*1.85*7.55, #ancho
    height = 1820*1.3375*7.55, #alto
    file = paste0('Data_', DateSave,'/', FolderName, '/Graph_TFS_Cases.png'),
    type = "png", #tiff
    bg = "white", #white or transparent depending on your requirement 
    dpi = 500,
    units = "px" #you can change to pixels etc 
  )
  plot(Graphs_TFS$Cases)
  dev.off()
  print(paste0('Save TFS Cases Graph'))
  
  ## TFS Deaths Graph using spatial region
  Cairo::Cairo(
    ## conversion de 72 ppi a 400 dpi entonces 400/72=5.55
    width = 650*1.85*7.55, #ancho
    height = 1820*1.3375*7.55, #alto
    file = paste0('Data_', DateSave,'/', FolderName, '/Graph_TFS_Deaths.png'),
    type = "png", #tiff
    bg = "white", #white or transparent depending on your requirement 
    dpi = 500,
    units = "px" #you can change to pixels etc 
  )
  plot(Graphs_TFS$Deaths)
  dev.off()
  print(paste0('Save TFS Deaths Graph'))
  
  ## Evolution Alpha Ensemble Fluctuation Scaling Cases and Deaths Graph
  Cairo::Cairo(
    ## conversion de 72 ppi a 400 dpi entonces 400/72=5.55
    width = 640*1.85*5.55, #ancho
    height = 540*1.3375*5.55, #alto
    file = paste0('Data_', DateSave,'/', FolderName, '/Graph_EFS_Alpha.png'),
    type = "png", #tiff
    bg = "white", #white or transparent depending on your requirement 
    dpi = 500,
    units = "px" #you can change to pixels etc 
  )
  plot(Graphs_TFS$Alpha)
  dev.off()
  print(paste0('Save Evolution Alpha of EFS in Cases and Deaths Graph'))
  
  ## Evolution K Ensemble Fluctuation Scaling Cases and Deaths Graph
  Cairo::Cairo(
    ## conversion de 72 ppi a 400 dpi entonces 400/72=5.55
    width = 640*1.85*5.55, #ancho
    height = 540*1.3375*5.55, #alto
    file = paste0('Data_', DateSave,'/', FolderName, '/Graph_EFS_K.png'),
    type = "png", #tiff
    bg = "white", #white or transparent depending on your requirement 
    dpi = 500,
    units = "px" #you can change to pixels etc 
  )
  plot(Graphs_TFS$K_TFS)
  dev.off()
  print(paste0('Save Evolution K of EFS in Cases and Deaths Graph'))
  
  ## Normalized Cases and Deaths Graph
  Graphs_Correlation <- Graph_Correlation(
    Correlation_Data = Corr_Data, Y_Label = Y_Label_Corr, 
    DateBegin = Date_Begin_Corr, DateEnd = Date_End_Corr)
  
  Cairo::Cairo(
    ## conversion de 72 ppi a 400 dpi entonces 400/72=5.55
    width = 640*1.85*5.55, #ancho
    height = 540*1.3375*5.55, #alto
    file = paste0('Data_', DateSave,'/', FolderName, '/Graph_Norm_Cases_Deaths.png'),
    type = "png", #tiff
    bg = "white", #white or transparent depending on your requirement 
    dpi = 500,
    units = "px" #you can change to pixels etc 
  )
  plot(Graphs_Correlation$Epidemic_Curve)
  dev.off()
  print(paste0('Save Normalized Cases and Deaths Graph'))
  
  ## Correlation Cases and Deaths Graph
  Cairo::Cairo(
    ## conversion de 72 ppi a 400 dpi entonces 400/72=5.55
    width = 640*1.85*5.55, #ancho
    height = 540*1.3375*5.55, #alto
    file = paste0('Data_', DateSave,'/', FolderName, '/Graph_Correlation.png'),
    type = "png", #tiff
    bg = "white", #white or transparent depending on your requirement 
    dpi = 500,
    units = "px" #you can change to pixels etc 
  )
  plot(Graphs_Correlation$Correlation)
  dev.off()
  print(paste0('Save Correlation Cases and Deaths Graph'))
  
  ## Hurst exponent Cases and Deaths Graph
  Graphs_Hurst <- Graph_Hurst(
    Hurst_Data = Hurst_Data, Y_Label = Y_Label_Hurst, 
    DateBegin = Date_Begin_Hurst, DateEnd = Date_End_Hurst)
  
  Cairo::Cairo(
    ## conversion de 72 ppi a 400 dpi entonces 400/72=5.55
    width = 640*1.85*5.55, #ancho
    height = 540*1.3375*5.55, #alto
    file = paste0('Data_', DateSave,'/', FolderName, '/Graph_Hurst_Evolution.png'),
    type = "png", #tiff
    bg = "white", #white or transparent depending on your requirement 
    dpi = 500,
    units = "px" #you can change to pixels etc 
  )
  plot(Graphs_Hurst$Hurst)
  dev.off()
  print(paste0('Save Hurst exponent Cases and Deaths Graph'))
  
  ## Theil index Cases and Deaths Graph
  Graphs_Theil <- Graph_Theil(
    Theil_Data = Theil_Data, Y_Label = Y_Label_Theil, 
    DateBegin = Date_Begin_Theil, DateEnd = Date_End_Theil)
  
  Cairo::Cairo(
    ## conversion de 72 ppi a 400 dpi entonces 400/72=5.55
    width = 640*1.85*5.55, #ancho
    height = 540*1.3375*5.55, #alto
    file = paste0('Data_', DateSave,'/', FolderName, '/Graph_Theil0_Evolution.png'),
    type = "png", #tiff
    bg = "white", #white or transparent depending on your requirement 
    dpi = 500,
    units = "px" #you can change to pixels etc 
  )
  plot(Graphs_Theil$Theil0)
  dev.off()
  print(paste0('Save Theil exponent Cases and Deaths Graph'))
  
  ## Modified Theil index Cases and Deaths Graph
  Cairo::Cairo(
    ## conversion de 72 ppi a 400 dpi entonces 400/72=5.55
    width = 640*1.85*5.55, #ancho
    height = 540*1.3375*5.55, #alto
    file = paste0('Data_', DateSave,'/', FolderName, '/Graph_Theil1_Evolution.png'),
    type = "png", #tiff
    bg = "white", #white or transparent depending on your requirement 
    dpi = 500,
    units = "px" #you can change to pixels etc 
  )
  plot(Graphs_Theil$Theil1)
  dev.off()
  print(paste0('Save Modified Theil exponent Cases and Deaths Graph'))
  
  ## Normalized Theil index Cases and Deaths Graph
  Cairo::Cairo(
    ## conversion de 72 ppi a 400 dpi entonces 400/72=5.55
    width = 640*1.85*5.55, #ancho
    height = 540*1.3375*5.55, #alto
    file = paste0('Data_', DateSave,'/', FolderName, '/Graph_TheilN_Evolution.png'),
    type = "png", #tiff
    bg = "white", #white or transparent depending on your requirement 
    dpi = 500,
    units = "px" #you can change to pixels etc 
  )
  plot(Graphs_Theil$TheilN)
  dev.off()
  print(paste0('Save Normalized Theil exponent Cases and Deaths Graph'))
  
  ## Theil index vs Alpha TFS Cases and Deaths Graph
  Graphs_Temporal <- Graph_Temporal_Scaling(
    Temporal_Data = Temporal_Data, Y_Label = Y_Label_Temporal)
  
  Cairo::Cairo(
    ## conversion de 72 ppi a 400 dpi entonces 400/72=5.55
    width = 640*1.85*5.55, #ancho
    height = 540*1.3375*5.55, #alto
    file = paste0('Data_', DateSave,'/', FolderName, '/Graph_Theil_Cum_Alpha.png'),
    type = "png", #tiff
    bg = "white", #white or transparent depending on your requirement 
    dpi = 500,
    units = "px" #you can change to pixels etc 
  )
  plot(Graphs_Temporal$AlphaTheil)
  dev.off()
  print(paste0('Save Theil index vs Alpha TFS Cases and Deaths Graph'))
  
  ## Hurst Exponent vs Alpha TFS Cases and Deaths Graph
  Cairo::Cairo(
    ## conversion de 72 ppi a 400 dpi entonces 400/72=5.55
    width = 640*1.85*5.55, #ancho
    height = 540*1.3375*5.55, #alto
    file = paste0('Data_', DateSave,'/', FolderName, '/Graph_Hurst_Alpha.png'),
    type = "png", #tiff
    bg = "white", #white or transparent depending on your requirement 
    dpi = 500,
    units = "px" #you can change to pixels etc 
  )
  plot(Graphs_Temporal$AlphaHurst)
  dev.off()
  print(paste0('Save Hurst exponent vs Alpha TFS Cases and Deaths Graph'))
  
  ## Theil index vs Hurst Exponent Cases and Deaths Graph
  Cairo::Cairo(
    ## conversion de 72 ppi a 400 dpi entonces 400/72=5.55
    width = 640*1.85*5.55, #ancho
    height = 540*1.3375*5.55, #alto
    file = paste0('Data_', DateSave,'/', FolderName, '/Graph_Theil_Hurst.png'),
    type = "png", #tiff
    bg = "white", #white or transparent depending on your requirement 
    dpi = 500,
    units = "px" #you can change to pixels etc 
  )
  plot(Graphs_Temporal$HurstTheil)
  dev.off()
  print(paste0('Save Theil index vs Hurst exponent Cases and Deaths Graph'))
  
  #### Function return ####
  MyList <- list('RawData' = Data, 'CountData' = Counts_Data,
                 'AIC' = Adjust_Data$AIC, 
                 'ParamsLognormal' = Adjust_Data$Params,
                 'TFS' = TFS_Data, 'Correlation' = Corr_Data,
                 'Hurst' = Hurst_Data, 'Theil' = Theil_Data,
                 'GraphCountDate' = Graph_Count,
                 'GraphAIC' = Graph_Adjusts$AIC,
                 'GraphMU' = Graph_Adjusts$MU,
                 'GraphSIGMA' = Graph_Adjusts$SIGMA,
                 'GraphTFSCases' = Graphs_TFS$Cases,
                 'GraphTFSDeaths' = Graphs_TFS$Deaths,
                 'GraphEFSAlpha' = Graphs_TFS$Alpha,
                 'GraphEFSK' = Graphs_TFS$K_TFS,
                 'GraphNormCasesDeaths' = Graphs_Correlation$Epidemic_Curve,
                 'GraphCorr' = Graphs_Correlation$Correlation,
                 'GraphHurst' = Graphs_Hurst$Hurst,
                 'GraphTheil0' = Graphs_Theil$Theil0,
                 'GraphTheil1' = Graphs_Theil$Theil1,
                 'GraphTheilN' = Graphs_Theil$TheilN,
                 'GraphAlphaTheil' = Graphs_Temporal$AlphaTheil,
                 'GraphAlphaHurst' = Graphs_Temporal$AlphaHurst,
                 'GraphHurstTheil' = Graphs_Temporal$HurstTheil
  )
  return(MyList)
}

################################################################################
# - Optimizar window size del Hurst (Recalcular Hurst)
# - Introducir trayectorias difusivas (TTS)
# - Multifractal Hurst
# - Ajuste distirbucion Burr (Derivada fraccional)
# - Ajustes a Levy distribution