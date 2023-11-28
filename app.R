
library(shiny)
library(FITfileR)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)
library(openxlsx)
library(readxl)
library(writexl)
library(whippr)
library(DT)
library(ggplot2)
library(shinyjs)

sizes <- c("XS","S","M","L","XL","2XL","-")

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "united"),
    
  
  navbarPage(
    title = "Baseline Data Analysis", 
    collapsible = TRUE,
    fluid = TRUE,
    
    tabPanel(  
   title="Data Input",
       # Sidebar  
    sidebarLayout(
        sidebarPanel(
          h4("Garmin Files (.FIT)"),
          fileInput("FITmax", "Garmin Max Data", buttonLabel = "Upload max.FIT",accept = ".FIT"),
          fileInput("FITsub", "Garmin Submax Data", buttonLabel = "Upload submax.FIT",accept = ".FIT"),
          h4("Parvo Files (.csv)"),
          fileInput("PARVOmax", "Parvo Max Data", buttonLabel = "Upload VO2max", accept = ".csv"),
          fileInput("PARVOsubmax", "Parvo Submax Data", buttonLabel = "Upload Submax", accept = ".csv"),
          p(),
          ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            type = "pills",
          tabPanel("Additional Data", 
                   br(),
                   h5("VO2max RPE"),
                   div(id="RPEmax", numericInput("RPEmax","Select RPE:",min = 6,max = 20,value = 18, width = 150), 
                       tags$style(type="text/css", "#RPEmax {color: red}")), 
                   textAreaInput("comments", "Comments:",width = 425, height = 50),
                   h5("Apparel Sizes"),
                   fluidRow(selectInput("top", "Shirt Size:", sizes, width = 150, selected = "-"),selectInput("pant", "Short Size:", sizes, width = 150, selected = "-"), selectInput("bra", "Bra Size:", sizes, width = 150,selected = "-")), 
                   h5("Bike Measurements"),
                   fluidRow(numericInput("seatVert", "Seat Height:",step = 1, min = 1,max = 20, value = 9, width = 150),numericInput("seatHoriz", "Seat Horizontal:",step = 0.1, min = 0, max = 10, value = 9, width = 150), numericInput("handlebars", "Handlebars:",step = 1, value = 3, width = 150))),
          tabPanel("Data Averaging",
                   br(),
                   h5("Submax Data Averaging:"),
                   numericRangeInput("sub1_avg", "Stage 1 Avg Range (min)", value = c(3,5), step = 1, width = 200),
                   numericRangeInput("sub2_avg", "Stage 2 Avg Range (min)", value = c(9,11), step = 1, width = 200),
                   numericRangeInput("sub3_avg", "Stage 3 Avg Range (min)", value = c(15,17), step = 1, width = 200)))))),
   
   tabPanel(  
     title="Data Plots",
     tabsetPanel(
       type = "pills",
       tabPanel("Test Data", fluidRow(column(width = 6, plotOutput("plotFITmax", width = "100%")), 
                                column(width = 6, plotOutput("plotFITsub", width = "100%")),
                                column(width = 6, plotOutput("plotPARVOmax", width = "100%")),
                                column(width = 6, plotOutput("plotPARVOsub", width = "100%")))),
       tabPanel("Thresholds", plotOutput("plotThresholds"), sliderInput("exDuration", "Test Time (min)", min = 0, max = 15, value = c(1,13), step = 0.25, width = "100%"),
                DT::dataTableOutput("VentThresh")),
     tabPanel("Settings",
              sliderInput("y1_max", "HR Y-Axis Max",min = 120, max = 220, value = 200, step = 10),
              sliderInput("y2_max", "Power Y-Axis Max",min = 50, max = 550, value = 450, step = 25),
              sliderInput("y3_max", "VO2 Y-Axis Max",min = 0, max = 100, value = 65, step = 5),
              sliderInput("y4_max", "RER Y-Axis Max",min = 0.5, max = 1.5, value = 1.3, step = 0.1),
     ))),
       
       
   tabPanel(  
          title="Data Tables",
           tabsetPanel(
            type = "pills",
            
           tabPanel(
             "Garmin Max Data",
          div(DT::dataTableOutput("FITmax_table"),
           style = "font-size:60%;white-space: nowrap"),
            ),
            tabPanel(
              "Garmin Submax Data",
              div(DT::dataTableOutput("FITsub_table"),
                  style = "font-size:60%;white-space: nowrap")
              ),
              tabPanel(
                "Parvo Max Data",
                div(DT::dataTableOutput("PARVOmax_table"),
                    style = "font-size:60%;white-space: nowrap")), 
              tabPanel(
                "Parvo Submax Data",
                div(DT::dataTableOutput("PARVOsub_table"),
                    style = "font-size:60%;white-space: nowrap")),
            )
           ),
   tabPanel(
     title="Analysis Export",
     h4("File Download"),
     # textInput("csvName","Download Name (add trial info)", value = ""),
     
     shinyjs::useShinyjs(),
     shinyjs::disabled(downloadButton("downloadCSV","Download .xlsx")),
   )
  )
  )

server <- function(input, output) {
  df_FITmax <- reactive({
    inFile <- input$FITmax
    if (is.null(inFile))
      return(NULL)
    
    # error handling
    tryCatch({
      FITfile <- readFitFile(inFile$datapath)
      df <- records(FITfile) %>% 
        bind_rows() %>% 
        arrange(timestamp) 
      df$timestamp<-as.character(as.POSIXct(df$timestamp, tz = ""))
      return(df)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  df_FITsub <- reactive({
    inFile <- input$FITsub
    if (is.null(inFile))
      return(NULL)
    
    # error handling
    tryCatch({
      FITfile <- readFitFile(inFile$datapath)
      df <- records(FITfile) %>% 
        bind_rows() %>% 
        arrange(timestamp) 
      df$timestamp<-as.character(as.POSIXct(df$timestamp, tz = ""))
      return(df)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  range_submax <- reactive({
   range_submax<-eval(c(input$sub1_avg,input$sub2_avg,input$sub3_avg))
   return(range_submax)
  })
  
  sample_rate_sub <- reactive({
    subPARVO <- df_PARVOsub()
    if (is.null(subPARVO)) {return(NULL)}
    duration <- subPARVO$TIME
    samples <- vector(mode = "numeric", length = length(duration))
    samples
    for (i in 2:length(duration)){
      sample_duration <- duration[i]-duration[i-1]
      samples[i] <- sample_duration
    }
    sample_rate <- round((60/mean(samples)),0)
    return(sample_rate)
  })
  
  sample_rate_max <- reactive({
    maxPARVO <- df_PARVOmax()
    if (is.null(maxPARVO)) {return(NULL)}
    duration <- maxPARVO$TIME
    samples <- vector(mode = "numeric", length = length(duration))
    samples
    for (i in 2:length(duration)){
      sample_duration <- duration[i]-duration[i-1]
      samples[i] <- sample_duration
    }
    sample_rate <- round((60/mean(samples)),0)
    return(sample_rate)
  })
  
  export_max <- reactive({
    # req(input$FITmax,input$FITsub, input$PARVOmax,input$PARVOsubmax)
    range_submax <- range_submax()
    sample_rate <- sample_rate_sub()

    characteristics <- csv_maxdata()
    
    rFIT<- range_submax*60
    rParvo<- range_submax*sample_rate
    subFit <-df_FITsub()
    subPARVO <- df_PARVOsub()
    maxFit <- df_FITmax()
    maxPARVO <- df_PARVOmax_30s()
    PARVOname <- characteristics[6,2]
    name<-read.csv(text=PARVOname, header=FALSE, stringsAsFactors=FALSE,col.names = c("Last_Name", "First_Name"))
    firstname<-name[2]
    lastname<-name[1]
    ergo <- characteristics[13,2]
    height <- (as.numeric(characteristics[8,4]))/100
    weight <- as.numeric(characteristics[8,9])
    age <- as.numeric(characteristics[7,2])
    sex <- if (characteristics[7,5] == " F") {sex<-"Female"}  else {sex<-"Male"}
    abs_max <- max(maxPARVO$`VO2`)
    rel_max <- max(maxPARVO$`VO2/kg`)
    hr_max <- max(c(max(maxPARVO$HR),max(maxFit$heart_rate)))
    rer_max <- max(maxPARVO$RER)
    rpe_max <- input$RPEmax
    supra_abs <- ""
    supra_rel <- ""
    supra_dur <- ""
    power_1 <- mean(subFit$power[(rFIT[1]+1):rFIT[2]])
    power_2 <- mean(subFit$power[(rFIT[3]+1):rFIT[4]])
    power_3 <- mean(subFit$power[(rFIT[5]+1):rFIT[6]])

    abs_1 <- mean(subPARVO$`VO2`[(rParvo[1]+1):rParvo[2]])
    abs_2 <- mean(subPARVO$`VO2`[(rParvo[3]+1):rParvo[4]])
    abs_3 <- mean(subPARVO$`VO2`[(rParvo[5]+1):rParvo[6]])
    rel_1 <- mean(subPARVO$`VO2/kg`[(rParvo[1]+1):rParvo[2]])
    rel_2 <- mean(subPARVO$`VO2/kg`[(rParvo[3]+1):rParvo[4]])
    rel_3 <- mean(subPARVO$`VO2/kg`[(rParvo[5]+1):rParvo[6]])
    MET_1 <- mean(subPARVO$METS[(rParvo[1]+1):rParvo[2]])
    MET_2 <- mean(subPARVO$METS[(rParvo[3]+1):rParvo[4]])
    MET_3 <- mean(subPARVO$METS[(rParvo[5]+1):rParvo[6]])
    VE_1 <- mean(subPARVO$VE[(rParvo[1]+1):rParvo[2]])
    VE_2 <- mean(subPARVO$VE[(rParvo[3]+1):rParvo[4]])
    VE_3 <- mean(subPARVO$VE[(rParvo[5]+1):rParvo[6]])
    RER_1 <- mean(subPARVO$RER[(rParvo[1]+1):rParvo[2]])
    RER_2 <- mean(subPARVO$RER[(rParvo[3]+1):rParvo[4]])
    RER_3 <- mean(subPARVO$RER[(rParvo[5]+1):rParvo[6]])

    hr_1 <- mean(subFit$heart_rate[(rFIT[1]+1):rFIT[2]])
    hr_2 <- mean(subFit$heart_rate[(rFIT[3]+1):rFIT[4]])
    hr_3 <- mean(subFit$heart_rate[(rFIT[5]+1):rFIT[6]])
    rpm_1 <- mean(subFit$cadence[(rFIT[1]+1):rFIT[2]])
    rpm_2 <- mean(subFit$cadence[rFIT[3]:rFIT[4]])
    rpm_3 <- mean(subFit$cadence[(rFIT[5]+1):rFIT[6]])
    
    export_max <- data.frame("First_Name" = firstname, "Last_Name" = lastname,"Bike_or_Treadmill" = ergo,"Height" = height,"Weight" = weight,"Age" = age,"Gender" = sex, "VO2max_Absolute" = abs_max, "VO2max_Relative" = rel_max, "HR_max" = hr_max, "RER_max" = rer_max, "RPE_max" = rpe_max, "Supramax_VO2_Absolute"=supra_abs, "Supramax_VO2_Relative"=supra_rel, "Supramax_Duration" = supra_dur,"Submax_Power_1"=power_1,"Submax_Power_2"=power_2,"Submax_Power_3"=power_3,"VO2_Abs_1" = abs_1,"VO2_Abs_2" = abs_2,"VO2_Abs_3" = abs_3,"VO2_Rel_1" = rel_1, "VO2_Rel_2" = rel_2, "VO2_Rel_3" = rel_3, "METS_1" = MET_1, "METS_2" = MET_2, "METS_3" = MET_3,"VE_1" = VE_1, "VE_2" = VE_2, "VE_3" = VE_3,"RER_1" = RER_1, "RER_2" = RER_2, "RER_3" = RER_3, "HR_1" = hr_1,"HR_2" = hr_2,"HR_3" = hr_3,"Cadence_1" =rpm_1, "Cadence_2"=rpm_2, "Cadence_3" =rpm_3)
    
    return(export_max)
  })
  
  extra_data <- reactive({
    
    extra_data <- data.frame("Shirt" = input$top, "Shorts" = input$pant, "Bra"= input$bra, "Seat_Height" = input$seatVert, "Seat_Horizontal" = input$seatHoriz, "Handlebars" = input$handlebars, "Comments" = input$comments)
    return(extra_data)
  })
  
  csv_maxdata <- reactive({
    inFile <- input$PARVOmax
    if (is.null(inFile))
      return(NULL)
    c<-1:(max(count.fields(inFile$datapath,sep = ",")))
    csv_maxdata <- read.csv(inFile$datapath, header = F,col.names = c,blank.lines.skip = F)
    return(csv_maxdata)
  })
  
  csv_subdata <- reactive({
    inFile <- input$PARVOsubmax
    if (is.null(inFile))
      return(NULL)
    c<-1:(max(count.fields(inFile$datapath,sep = ",")))
    csv_subdata <- read.csv(inFile$datapath, header = F,col.names = c,blank.lines.skip = F)
    return(csv_subdata)
  })
  
  tempxlsxMAX <- reactive({
    tempxlsxMAX<-tempfile("1csv",fileext = ".xlsx")    
    write.xlsx(csv_maxdata(),tempxlsxMAX)
    return(tempxlsxMAX)
  })
  
  tempxlsxSUB <- reactive({
    tempxlsxSUB <- tempfile("2csv",fileext = ".xlsx")
    write.xlsx(csv_subdata(),tempxlsxSUB)
    return(tempxlsxSUB)
  })
  
  
  df_PARVOmax <- reactive({
    inFile <- input$PARVOmax
    if (is.null(inFile))
      return(NULL)
    df <- read_data(tempxlsxMAX(), metabolic_cart = "parvo",time_column = "TIME")
    return(df)
  })
  
  df_PARVOmax_30s <- reactive({
    df_PARVOmax_30s <- df_PARVOmax() 
    #  %>% interpolate() %>% 
    #   perform_average(type = "bin", bins = 30)
  })
  
  thresholds2 <- reactive({
    df_PARVOmax <- df_PARVOmax()
    if (is.null(df_PARVOmax)) {return(NULL)}
    range <- input$exDuration * sample_rate_max()
    if (range[2] > nrow(df_PARVOmax)) {range[2] <-nrow(df_PARVOmax)}
      
      
    df_PARVOmax$exCO2 <- ((df_PARVOmax$VCO2 ^ 2)/df_PARVOmax$VO2) - df_PARVOmax$VCO2
    df_PARVOmax$exVE <- ((df_PARVOmax$VE ^ 2)/df_PARVOmax$VCO2) - df_PARVOmax$VE
    df_PARVOmax$`VE/VO2` <- df_PARVOmax$VE/df_PARVOmax$VO2
    df_PARVOmax$`VE/VCO2` <- df_PARVOmax$VE/df_PARVOmax$VCO2
     
    lm_vslope1 <- lm(VCO2 ~ VO2, data = df_PARVOmax)
    seg_vslope1 <- segmented(lm_vslope1)
    VT1 <- seg_vslope1$psi[,2]
    VT1i <- which(abs(df_PARVOmax$VO2 - VT1) == min(abs(df_PARVOmax$VO2 - VT1)))
    
    lm_exCO2 <- lm(exCO2 ~ TIME, data = df_PARVOmax)
    seg_exCO2 <- segmented(lm_exCO2)
    
    lm_VEVO2 <- lm(`VE/VO2` ~ TIME, data = df_PARVOmax)
    seg_VEVO2 <- segmented(lm_VEVO2)
    
    VT1a <- df_PARVOmax$TIME[VT1i]
    VT1b <- seg_exCO2$psi[,2]
    VT1c <- seg_VEVO2$psi[,2]
    
    VT1av <- mean(c(VT1a, VT1b, VT1c))
    
    lm_vslope2 <- lm(VE ~ VCO2, data = df_PARVOmax)
    seg_vslope2 <- segmented(lm_vslope2)
    VT2 <- seg_vslope2$psi[,2]
    VT2i <- which(abs(df_PARVOmax$VCO2 - VT2) == min(abs(df_PARVOmax$VCO2 - VT2)))
    
    lm_exVE <- lm(exVE ~ TIME, data = df_PARVOmax)
    seg_exVE <- segmented(lm_exVE)
    
    lm_VEVCO2 <- lm(`VE/VCO2` ~ TIME, data = df_PARVOmax)
    seg_VEVCO2 <- segmented(lm_VEVCO2)
    
    VT2a <- df_PARVOmax$TIME[VT2i]
    VT2b <- seg_exVE$psi[,2]
    VT2c <- seg_VEVCO2$psi[,2]
    
    VT2av <- mean(c(VT2a, VT2b, VT2c))
    
    VTtable <- rbind(c(VT1a, VT1b, VT1c, VT1av),c(VT2a, VT2b, VT2c, VT2av))
    
    return(VTtable)
     })
  
  
  thresholds <- reactive({
    df_PARVOmax <- df_PARVOmax()
     export_max <- export_max()
    
    logest <- function(y, x, ...){
      if(missing(x) || is.null(x)) x <- seq_along(y)
      result <- lm(log(y) ~ x, ...)
      exp(coef(result))
    }
  
    expVCO2<-logest(df_PARVOmax$VCO2,df_PARVOmax$VO2)
    T1VO2 <- log(1/(expVCO2[1]*log(expVCO2[2])))/log(expVCO2[2])
    
    VO2linear <- c(export_max$VO2_Abs_1, export_max$VO2_Abs_2, export_max$VO2_Abs_3)
    HRlinear <- c(export_max$HR_1, export_max$HR_2, export_max$HR_3)
    Wlinear <- c(export_max$Submax_Power_1, export_max$Submax_Power_2, export_max$Submax_Power_3)
    HRVO2line<- lm(HRlinear ~ VO2linear)
    WVO2line <- lm(Wlinear ~ VO2linear)

    vt1hr<- HRVO2line$coefficients[2]*T1VO2 + HRVO2line$coefficients[1]
    vt1W <- WVO2line$coefficients[2]*T1VO2 + WVO2line$coefficients[1]


     T1index <- max(which(df_PARVOmax$HR < vt1hr, arr.ind = T))
    
    VT1<-data.frame(row.names = "VT1","VO2" = T1VO2,"HR" = round(vt1hr), "Index"=T1index)
    
    zVO2 <- (df_PARVOmax$VO2-mean(df_PARVOmax$VO2))/sd(df_PARVOmax$VO2)
    zVE <-(df_PARVOmax$VE-mean(df_PARVOmax$VE))/sd(df_PARVOmax$VE)
    x <- as.numeric(row(df_PARVOmax[1]))
    
    # Fit the first 3rd order polynomial model
    model1 <- lm(zVO2 ~ poly(x, 3, raw = T))
    
    # Fit the second 3rd order polynomial model
    model2 <- lm(zVE ~ poly(x, 3, raw = T))
    
    # Find the intersecting points by solving for x
    poly1_coeff <- coefficients(model1)
    poly2_coeff <- coefficients(model2)
    
    # Solve for the intersection
    intersection_x <- polyroot(poly2_coeff - poly1_coeff)
    
   VT2i <- round(max(as.numeric(intersection_x)))
    VT2 <- data.frame(row.names = "VT2","VO2" = df_PARVOmax$VO2[VT2i], "HR" = df_PARVOmax$HR[VT2i], "Index"=VT2i)
    VT <- rbind(VT1,VT2)

    return(VT)
  })
  
  df_PARVOsub <- reactive({
    inFile <- input$PARVOsubmax
    if (is.null(inFile))
      return(NULL)
    df <- read_data(tempxlsxSUB(), metabolic_cart = "parvo",time_column = "TIME")
    return(df)
  })
  
  filename <- reactive({
    export_max <- export_max()
    filename <- paste(export_max[1],export_max[2], "Baseline", sep = "_")
    return(filename)
  })
  
  output$downloadCSV <- downloadHandler(
    filename = function() {paste0(filename(), ".xlsx")},
    content = function(file) {
      df1 <- df_FITmax()
      df2 <- df_FITsub()
      df3 <- csv_maxdata()
      df4 <- csv_subdata()
      df5 <- export_max()
      df6 <- extra_data()
      write_xlsx(list("Garmin Max" = df1, "Garmin Submax" = df2, "Parvo Max" = df3, "Parvo Submax" = df4, "Rio Summary" = df5, "Extra Data" = df6), file)
    }
  )
  
  observeEvent(req(input$FITsub,input$PARVOmax, input$PARVOsubmax), {
      shinyjs::enable("downloadCSV")
  })
  
  output$FITmax_table<- DT::renderDataTable({
    df <- df_FITmax()
    DT::datatable(df,options = list(dom = 't', paging=FALSE, scrollX = T, scrollY = "680px"),filter = "none")
  })
  output$FITsub_table<- DT::renderDataTable({
    df <- df_FITsub()
    DT::datatable(df,options = list(dom = 't', paging=FALSE, scrollX = T, scrollY = "680px"),filter = "none")
  })
  output$PARVOmax_table<- DT::renderDataTable({
    df <- df_PARVOmax()
    DT::datatable(df,options = list(dom = 't', paging=FALSE, scrollX = T, scrollY = "680px"),filter = "none")
  })
  output$PARVOsub_table<- DT::renderDataTable({
    df <- df_PARVOsub()
    DT::datatable(df,options = list(dom = 't', paging=FALSE, scrollX = T, scrollY = "680px"),filter = "none")
  })
  
  output$plotFITmax <- renderPlot({
    df <- df_FITmax()
    if (is.null(df)) {return(NULL)}
    
    x_col <- row(df[1])/60

    y1_max <- input$y1_max
    y2_max <- input$y2_max
    ycoef <- y2_max/y1_max
    
    p <- ggplot(df, aes(x = x_col)) +
      geom_line(aes(y = heart_rate, color = "HR")) +
      scale_y_continuous(
        name = "Heart Rate (bpm)",
        breaks = scales::breaks_pretty(11),
        limits = c(0, y1_max)
      )+scale_color_manual(name = "",values = c("HR" = "red", "Power" = "blue"))+
      theme(legend.position = c(0.5,0.925),legend.direction = "horizontal") +
      scale_x_continuous(~.,breaks = scales::breaks_pretty(max(x_col)),name = "Time (min)")
    
    
    # Check if the selected column (e.g., "Power") exists in the dataframe
    if ("power" %in% colnames(df)) {
      p <- p +
        geom_line(aes(y = power/ycoef, color = "Power")) +
        scale_y_continuous(
          name = "Heart Rate (bpm)",
          breaks = scales::breaks_pretty(11),
          sec.axis = sec_axis(~.*ycoef, name = "Power (W)", breaks = scales::breaks_pretty(11)),
          limits = c(0, y1_max)
          )+
        scale_x_continuous(~.,breaks = scales::breaks_pretty(max(x_col)),name = "Time (min)")
      
    }
    
    p + labs(title = "Garmin Max Data")
  })
  
  output$plotFITsub <- renderPlot({
    range_submax<-range_submax()
    df <- df_FITsub()
    if (is.null(df)) {return(NULL)}
    
    x_col <- row(df[1])/60
    
    y1_max <- input$y1_max
    y2_max <- input$y2_max
    ycoef <- y2_max/y1_max
    
    p <- ggplot(df, aes(x = x_col)) +
      geom_line(aes(y = heart_rate, color = "HR")) +
      scale_y_continuous(
        name = "Heart Rate (bpm)",
        breaks = scales::breaks_pretty(11),
        limits = c(0, y1_max))+
        geom_line(aes(y = power/ycoef, color = "Power")) +
        scale_y_continuous(
          name = "Heart Rate (bpm)",
          breaks = scales::breaks_pretty(11),
          sec.axis = sec_axis(~.*ycoef, name = "Power (W)", breaks = scales::breaks_pretty(11)),
          limits = c(0, y1_max)) +
      geom_rect(aes(xmin = range_submax[1], xmax = range_submax[2], 
                    ymin = -Inf, ymax = Inf), alpha = 0.005, fill = "green") + 
      geom_rect(aes(xmin = range_submax[3], xmax = range_submax[4], 
                    ymin = -Inf, ymax = Inf), alpha = 0.005, fill = "orange") +
      geom_rect(aes(xmin = range_submax[5], xmax = range_submax[6], 
                    ymin = -Inf, ymax = Inf), alpha = 0.005, fill = "red")+
      scale_color_manual(name = "",values = c("HR" = "red", "Power" = "blue"))+
      theme(legend.position = c(0.5,0.925),legend.direction = "horizontal") +  
      scale_x_continuous(~.,breaks = scales::breaks_pretty(max(x_col)),name = "Time (min)")
    
    p + labs(x = "Time (min)", title = "Garmin Submax Data")
  })
  
  output$plotPARVOmax <- renderPlot({
   sample_rate <- sample_rate_max()
   df <- df_PARVOmax()
    if (is.null(df)) {
      # Handle the case where df is NULL (error occurred)
      return(NULL)
    }
    
    x_col <- row(df[1])/sample_rate
    
    # Set the limits for the primary and secondary y-axes separately
    y3_max <- input$y3_max
    y4_max <- input$y4_max
    ycoef<- y4_max/y3_max
    
    
    p <- ggplot(df, aes(x = x_col)) +
      geom_line(aes(y = `VO2/kg`, color = "VO2/kg")) +
      scale_y_continuous(
        name = "VO2 Relative (ml/kg/min)",
        breaks = scales::breaks_pretty(11),
        limits = c(0, y3_max)
      ) +
      geom_line(aes(y = RER/ycoef, color = "RER")) +
      scale_y_continuous(
        name = "VO2 Relative (ml/kg/min)",
        breaks = scales::breaks_pretty(11),
        sec.axis = sec_axis(~.*ycoef, name = "RER", breaks = scales::breaks_pretty(11)),
        limits = c(0, y3_max)) + 
      scale_color_manual(name = "",values = c("VO2/kg" = "purple", "RER" = "black"))+
      theme(legend.position = c(0.5,0.925),legend.direction = "horizontal") +
      scale_x_continuous(~.,breaks = scales::breaks_pretty(max(x_col)),name = "Time (min)")
    
    p + labs(x = "Time (min)", title = "Parvo Max Data")
  })

  output$plotPARVOsub <- renderPlot({
    range_submax<- range_submax()
    sample_rate <- sample_rate_sub()
    df <- df_PARVOsub()
    if (is.null(df)) {
      # Handle the case where df is NULL (error occurred)
      return(NULL)
    }
    
    x_col <- row(df[1])/sample_rate
    
    # Set the limits for the primary and secondary y-axes separately
    y3_max <- input$y3_max
    y4_max <- input$y4_max
    ycoef<- y4_max/y3_max
    
    
    p <- ggplot(df, aes(x = x_col)) +
      geom_line(aes(y = `VO2/kg`, color = "VO2/kg")) +
      scale_y_continuous(
        name = "VO2 Relative (ml/kg/min)",
        breaks = scales::breaks_pretty(11),
        limits = c(0, y3_max)
      ) +
      geom_line(aes(y = RER/ycoef, color = "RER")) +
      scale_y_continuous(
        name = "VO2 Relative (ml/kg/min)",
        breaks = scales::breaks_pretty(11),
        sec.axis = sec_axis(~.*ycoef, name = "RER", breaks = scales::breaks_pretty(11)),
        limits = c(0, y3_max)) + 
      geom_rect(aes(xmin = range_submax[1], xmax = range_submax[2], 
                    ymin = -Inf, ymax = Inf), alpha = 0.015, fill = "green") + 
      geom_rect(aes(xmin = range_submax[3], xmax = range_submax[4], 
                    ymin = -Inf, ymax = Inf), alpha = 0.015, fill = "orange") +
      geom_rect(aes(xmin = range_submax[5], xmax = range_submax[6], 
                    ymin = -Inf, ymax = Inf), alpha = 0.015, fill = "red")+
      scale_color_manual(name = "",values = c("VO2/kg" = "purple", "RER" = "black"))+
      theme(legend.position = c(0.5,0.925),legend.direction = "horizontal") +
      scale_x_continuous(~.,breaks = scales::breaks_pretty(max(x_col)),name = "Time (min)")
    
    p + labs(x = "Time (min)", title = "Parvo Submax Data")
  })
  
  output$plotThresholds <- renderPlot({
    sample_rate <- sample_rate_max()
    df <- df_PARVOmax()
    thresholds <- thresholds2()
    if (is.null(df)) {
      # Handle the case where df is NULL (error occurred)
      return(NULL)
    }
    
    x_col <- row(df[1])/sample_rate
    
    # Set the limits for the primary and secondary y-axes separately
     # y3_max <- 45
     # y4_max <- 200
     # ycoef<- y4_max/y3_max
    
    p <- ggplot(df, aes(x = x_col)) + 
      geom_line(aes(y= VE/VO2),color="blue")+
      geom_line(aes(y= VE/VCO2),color="black")+
      scale_y_continuous(
        name = "Ventilatory Equivalents",
        breaks = scales::breaks_pretty(11) )+
      geom_vline(xintercept = thresholds[1,4]/60, linetype="dotted",
                 color = "orange", linewidth=1)+
      geom_vline(xintercept = thresholds[2,4]/60, linetype="dotted",
                 color = "red", linewidth=1)+
        scale_x_continuous(~.,breaks = scales::breaks_pretty(max(x_col)),name = "Time (min)")
      

  # geom_line(aes(y = HR/ycoef),color = "red") +
  #      scale_y_continuous(
  #        name = "Ventilatory Equivalents",
  #        breaks = scales::breaks_pretty(11),
  #        sec.axis = sec_axis(~.*ycoef, name = "Heart Rate", breaks = scales::breaks_pretty(11)),
         # limits = c(0, y3_max)
         # ) + 
      # scale_color_manual(name = "",values = c("VO2/kg" = "purple", "RER" = "black"))+
      # theme(legend.position = c(0.5,0.925),legend.direction = "horizontal") +
     #   scale_x_continuous(~.,breaks = scales::breaks_pretty(max(x_col)),name = "Time (min)")
     # 
     p + labs(x = "Time (min)", title = "Parvo Threshold Data")
  })
  
   output$VentThresh <- DT::renderDataTable({
     req(input$PARVOmax)
     thresholds2()/60
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
