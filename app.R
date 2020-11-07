---
output: html_document
runtime: shiny
---
library(shiny)
library(tidyverse)
library(shinydashboard)
library(dplyr)
library(rvest)
library(DT)
library(ggplot2)
library(DBI)
library(maptools)

library(rgdal)
library(sf)
# Define UI for application that draws a histogram

ui <- fluidPage(
    
    # Application title
    titlePanel("Exclusive rating of regions of Ukraine for the price of agricultural land lease"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("range", label = "Price range:", min = 0, max = 8.1, value = c(0, 8.1))
        ),
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("map")
        )
    ),
    DT::dataTableOutput("table")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    df_treat_csv <- read.csv("d:\\plants\\toUse\\all.csv", header = TRUE)
    df_treat_csv$square <- as.numeric(as.character(df_treat_csv$square))
    df_treat_csv=df_treat_csv %>% mutate_if(is.character, as.factor)
    df_treat_csv <- df_treat_csv %>% na.omit()
    #df_treat_csv[, "region"] <- df_treat_csv$region
    #пробуем рассчитать стоимость земель в границах построенной модели
    model.start <- readRDS("d:/plants/models/boost_start.rds")
    model.result <- readRDS("d:/plants/models/rtp_result.rds")
    predict_treat = predict(model.start, df_treat_csv)
    df_treat_csv$start <- predict_treat
    predict_treat_result <- predict(model.result, df_treat_csv)
    df_treat_csv[,"result"] <- predict_treat_result
    fertility_price <- read.csv("d:\\plants\\fertility.csv", header = TRUE)
    
    df_w_fert <- df_treat_csv
    temp_fert <- fertility_price %>% filter(lables=="азот")
    df_w_fert$square <- as.numeric(as.character(df_w_fert$square))
    df_w_fert[, "azot"] <- mean(c(temp_fert$minim,temp_fert$maxim))/100*df_w_fert$square
    temp_fert <- fertility_price %>% filter(lables=="фосфор")
    df_w_fert[, "fosf"] <- mean(c(temp_fert$minim,temp_fert$maxim))/100*df_w_fert$square
    temp_fert <- fertility_price %>% filter(lables=="калійні")
    df_w_fert[, "kal"] <- mean(c(temp_fert$minim,temp_fert$maxim))/100*df_w_fert$square
    
    period_new <- as.data.frame(c(200,2021))
    names(period_new) <- c("period")
    df_w_grow <- df_w_fert
    lm.grow <- readRDS("d:/plants/models/grow/zern.rds")
    pred_grow_temp <- predict(lm.grow, period_new)
    df_w_grow[, "zern"] <- (df_w_grow$square*pred_grow_temp[1])/10
    lm.grow <- readRDS("d:/plants/models/grow/pshen.rds")
    pred_grow_temp <- predict(lm.grow, period_new)
    df_w_grow[, "pshen"] <- (df_w_grow$square*pred_grow_temp[1])/10
    lm.grow <- readRDS("d:/plants/models/grow/jito.rds")
    pred_grow_temp <- predict(lm.grow, period_new)
    df_w_grow[, "jito"] <- (df_w_grow$square*pred_grow_temp[1])/10
    lm.grow <- readRDS("d:/plants/models/grow/yachmin.rds")
    pred_grow_temp <- predict(lm.grow, period_new)
    df_w_grow[, "yachmin"] <- (df_w_grow$square*pred_grow_temp[1])/10
    lm.grow <- readRDS("d:/plants/models/grow/oves.rds")
    pred_grow_temp <- predict(lm.grow, period_new)
    df_w_grow[, "oves"] <- (df_w_grow$square*pred_grow_temp[1])/10
    lm.grow <- readRDS("d:/plants/models/grow/kukurudza.rds")
    pred_grow_temp <- predict(lm.grow, period_new)
    df_w_grow[, "kukurudza"] <- (df_w_grow$square*pred_grow_temp[1])/10
    lm.grow <- readRDS("d:/plants/models/grow/proso.rds")
    pred_grow_temp <- predict(lm.grow, period_new)
    df_w_grow[, "proso"] <- (df_w_grow$square*pred_grow_temp[1])/10
    lm.grow <- readRDS("d:/plants/models/grow/grechka.rds")
    pred_grow_temp <- predict(lm.grow, period_new)
    df_w_grow[, "grechka"] <- (df_w_grow$square*pred_grow_temp[1])/10
    lm.grow <- readRDS("d:/plants/models/grow/ris.rds")
    pred_grow_temp <- predict(lm.grow, period_new)
    df_w_grow[, "ris"] <- (df_w_grow$square*pred_grow_temp[1])/10
    lm.grow <- readRDS("d:/plants/models/grow/zernobobovi.rds")
    pred_grow_temp <- predict(lm.grow, period_new)
    df_w_grow[, "zernobobovi"] <- (df_w_grow$square*pred_grow_temp[1])/10
    lm.grow <- readRDS("d:/plants/models/grow/buryak.rds")
    pred_grow_temp <- predict(lm.grow, period_new)
    df_w_grow[, "bur"] <- (df_w_grow$square*pred_grow_temp[1])/10
    lm.grow <- readRDS("d:/plants/models/grow/sonyashnik.rds")
    pred_grow_temp <- predict(lm.grow, period_new)
    df_w_grow[, "son"] <- (df_w_grow$square*pred_grow_temp[1])/10
    lm.grow <- readRDS("d:/plants/models/grow/kartoplya.rds")
    pred_grow_temp <- predict(lm.grow, period_new)
    df_w_grow[, "potato"] <- (df_w_grow$square*pred_grow_temp[1])/10
    lm.grow <- readRDS("d:/plants/models/grow/ovochevi.rds")
    pred_grow_temp <- predict(lm.grow, period_new)
    df_w_grow[, "ovoch"] <- (df_w_grow$square*pred_grow_temp[1])/10
    lm.grow <- readRDS("d:/plants/models/grow/yagidno_plodovi.rds")
    pred_grow_temp <- predict(lm.grow, period_new)
    df_w_grow[, "yagid_plod"] <- (df_w_grow$square*pred_grow_temp[1])/10
    lm.grow <- readRDS("d:/plants/models/grow/vinograd.rds")
    pred_grow_temp <- predict(lm.grow, period_new)
    df_w_grow[, "vinograd"] <- (df_w_grow$square*pred_grow_temp[1])/10
    
    df_w_finance <- df_w_grow
    df_w_finance[, "fet_cost"] <- mean(azot_price)*df_w_finance$azot+mean(az_fosf_price)*df_w_finance$fosf+
        mean(kal_price)*df_w_finance$kal
    df_w_finance[, "rent_cost"] <- df_w_finance$start*12
    
    df_w_finance[, "total_cost"] <- df_w_finance$fet_cost+df_w_finance$rent_cost
    
    costs <- read.csv("d:\\plants\\cost.csv", header = TRUE)
    df_w_income <- df_w_finance
    df_w_income[, "zern_inc"] <- costs$zern_cost*df_w_income$zern
    df_w_income[, "pshen_inc"] <- costs$pshen_cost*df_w_income$pshen
    df_w_income[, "jito_inc"] <- costs$jito_cost*df_w_income$jito
    df_w_income[, "kukurudza_inc"] <- costs$kukurudza_cost*df_w_income$kukurudza
    df_w_income[, "yachmin_inc"] <- costs$yachmin_cost*df_w_income$yachmin
    df_w_income[, "son_inc"] <- costs$son_cost*df_w_income$son
    df_w_income[, "grechka_inc"] <- costs$grechka_cost*df_w_income$grechka
    df_w_income[, "ris_inc"] <- costs$ris_cost*df_w_income$ris
    df_w_income[, "potato_inc"] <- costs$potato_cost*df_w_income$potato
    df_w_income[, "bur_inc"] <- costs$bur_cost*df_w_income$bur
    df_w_income[, "ovoch_inc"] <- costs$ovoch_cost*df_w_income$ovoch
    df_w_income[, "zern_inc"] <- costs$zern_cost*df_w_income$zern
    df_w_income[, "oves_inc"] <- costs$X5900*df_w_income$oves
    df_w_income[, "zernobobovi_inc"] <- costs$zernobobovi_cost*df_w_income$zernobobovi
    library(matrixStats)
    ndrops <- c("zern_inc","pshen_inc", "jito_inc","kukurudza_inc","yachmin_inc", "son_inc",
                "grechka_inc", "potato_inc","ovoch_inc", "zern_inc",
                "oves_inc","zernobobovi_inc")
    df_income_o <- df_w_income[ , (names(df_w_income) %in% ndrops)]
    max_names <- colnames(df_income_o)[apply(df_income_o,1,which.max)]
    max_values <- rowMaxs(data.matrix(df_income_o))-df_w_income$total_cost
    
    df_w_income[,"max_income"] <- max_values
    df_w_income[,"max_income_name"] <- max_names
    
    
    #get data to render map of Ukraine
    Regions <- readOGR("d:/plants/shiny/UkraineLandIncome/map", "gadm36_UKR_1")
    #average prices of renting land by region
    avg <- df_w_income %>%group_by(region) %>%  dplyr::summarize(Mean = mean(max_income, na.rm=TRUE)) 
    nullRow <- data.frame("???", 0)
    names(nullRow) <- c("region", "Mean")
    avg <- rbind(avg, nullRow)
    avg <- rbind(avg, nullRow)
    avg <- rbind(avg, nullRow)
    #write prices to Regions@data
    (Regions@data$price <- factor(avg$Mean))
    #add colors palette for map
    mypalette <- colorRampPalette(c("seagreen", "red"))
    #add region lables for map
    l1 = list("sp.text", coordinates(Regions), as.character(Regions@data$NAME_1),col="black", cex=0.7,font=2)
    #render map to UI
    output$map <- renderPlot({
        if(input$range[1] == 0 && input$range[2] == 8.1) { #if user select all diapason (default)
            spplot(Regions, 'price', sp.layout=list(l1),
                   col.regions = mypalette(27),
                   col = "transparent",
                   par.settings = list(axis.line = list(col = NA)))
        } else { #select custom diapason
            outputAvg <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
            for(i in avg) {
                index <-which(avg==i)
                if(i >= input$range[1] && i <= input$range[2]){
                    outputAvg[index] <- i
                }
            }
            (Regions@data$price <- factor(outputAvg))
            spplot(Regions, 'price', sp.layout=list(l1),
                   col.regions = mypalette(27),
                   col = "transparent",
                   par.settings = list(axis.line = list(col = NA)))
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

