library(shiny)
library(shinyjs)
library(readr)
library(dplyr)
library(ggplot2)
library(stats)
library(tidyverse)
library(rworldmap)
library(maps)
library(zip)
library(broom)
if(!require("shinythemes")) install.packages("shinythemes")
library(shinythemes)
options(timeout = 1000)

indices <- c("Total agriculture output per capita (Index/2014-2016)",
             "Crops output per capita (Index/2014-2016)",
             "Livestock",
             "Total food")


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("superhero"),
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
    .commentary {
      color: green; 
    }
    ")),
    
    tags$style(HTML("
    .explanatory {
      color: orange; 
    }
    ")),
  ),
  # Application title
  titlePanel("Drought influence on agricultural productivity"),
  
  navbarPage(
   tabsetPanel(
    tabPanel(
      title = "Maps", 
      
      #world map  
      sidebarLayout(
        sidebarPanel(radioButtons("reg0",
                                  h3("Display map region"),
                                  choices = c("Global map" = "world",
                                              "Middle East" = "meast",
                                              "South America" = "soam"
                                  ),
                                  selected = "world")
        ),
        mainPanel(
          br(),
          h3(id = "loading-message", "Loading data... Please wait.", style = "display:none;"),
          
          plotOutput("region_map"),
          helpText("*Orange areas represent countries which were used as a source of data.", class = "commentary")
        )
      ),
    ),
    tabPanel(
      title = "Climate Indices",
      
      titlePanel("SPEI data for selected state (here negative value represent drought and positive values represent abundance of rainwater)"),
      
      sidebarLayout(
        sidebarPanel(
          radioButtons("reg4", 
                       h3("Select country for comparison"),
                       choices = c("Syria", "Iran", "Bolivia", "Colombia"),
                       selected = NULL
          ),
          h4("Range: 1900-2021"),
        ),
        mainPanel(
          plotOutput("spei_pl"),
        ),
      ),
      br(),
      h5("SPEI (Standardized Precipitation Evapotranspiration Index) is a drought index that combines precipitation and potential evapotranspiration to assess meteorological conditions. Evapotranspiration represents the amount of water that could potentially be evaporated and transpired by vegetation under prevailing meteorological conditions. The SPEI standardizes the difference between observed precipitation and potential evapotranspiration, expressing it in terms of standard deviations from the long-term mean. This standardization allows for the comparison of drought severity across different regions and climates. Positive SPEI values demonstrate wetter conditions, while negative values show drier conditions. The SPEI has been widely used in hydrology, agriculture, and climate studies to monitor and assess drought severity, helping policymakers and resource managers make informed decisions related to water resource management and drought mitigation strategies.", class = "explanatory"),
      
      titlePanel("PM2.5 - mean annual exposure (mg/m^3)"),
      
      sidebarLayout(
        sidebarPanel(
          radioButtons("reg2", 
                       h3("Select country for comparison"),
                       choices = c("Syria", "Iran", "Bolivia", "Colombia"),
                       selected = NULL
          ),
          h4("Range: 1990-2021"),
        ),
        mainPanel(
          plotOutput("pm25_pl"),
        ),
      ),
      br(),
      h5("PM stands for particulate matter and is related to air pollution). Here, PM2.5 index represents fine inhalable particles, with diameters that are generally 2.5 micrometers and smaller.", class = "explanatory"),
      
      titlePanel("Average temperature change per countries (due to climate trends)"),
      
      sidebarLayout(
        sidebarPanel(
          radioButtons("reg3", 
                       h3("Select country for comparison"),
                       choices = c("Syria", "Iran", "Bolivia", "Colombia"),
                       selected = NULL
          ),
          h4("Range: 1961-2022"),
        ),
        mainPanel(
          plotOutput("mtc_pl"),
        ),
      ),
      br(),
      h5("This index represents mean change of temperature. Positive values represent heating and negative values cooling.", class = "explanatory")
    ),
    tabPanel(
      title = "Agriculture and correlations",
      
      selectInput("reg1", 
                   h4("Pick country for analysis:"),
                   choices = c("Syria", "Iran", "Bolivia", "Colombia"),
                   selected = NULL
      ),
      
      sidebarLayout(
        sidebarPanel(
          helpText("Choose a variable for further comparison:"),
          radioButtons("ind", 
                       h3("Choose a variable to display"),
                       choices = c("Total agriculture output per capita (Index/2014-2016)" = "ind1",
                                   "Crops output per capita (Index/2014-2016)" = "ind2",
                                   "Livestock" = "ind5",
                                   "Total food" = "ind4"),
                       selected = "ind1"),
          h4("Range: 1960-2021")
        ),
        mainPanel(
          br(),
          plotOutput("index_pl")
        )
      ),
      br(),
      
      titlePanel = "Comparison",
      sidebarLayout( 
        #Comparison
        
        sidebarPanel(
          
          helpText("Choose an environmental parameter to compare:"),
          radioButtons("endex", 
                       h3("Choose an index"),
                       choices = c("Standardized Precipitation Evapotranspiration Index (SPEI)" = "tbz1",
                                   "Pollution by particles larger than 2.5 mm (PM 2.5)" = "tbz2",
                                   "Mean temperature change (from 1951-1980 baseline)" = "tbz3"),
                       selected = "tbz1"),
          radioButtons("powpom", 
                       h3("Select degree of polynomial regression"),
                       choices = c("1 (linear)" = "op1",
                                   "2 (quadratic)" = "op2"),
                       selected = "op1"),
          tableOutput("tbz")
        ),
        mainPanel(
          br(),
          plotOutput("comparison")
        )
      ),
    ),
    tabPanel(
      title = "Correlation details",
      selectInput("reg1b", 
                   h3("Select country for comparison:"),
                   choices = c("Syria", "Iran", "Bolivia", "Colombia"),
                   selected = NULL
      ),
      tableOutput("stat_table"),
    ),
    tabPanel(
      title = "References",
      fluidRow(
        sidebarPanel("FAOSTAT. 2023. Retrieved December 18, 2023, from https://www.fao.org/faostat/en/#data"),
        sidebarPanel("LCSC: Climatology and Climate Services Laboratory. 2023. SPEI series for world countries. Retrieved November 2, 2024, from https://lcsc.csic.es/software-2/spei-countries-of-the-world/"),
        sidebarPanel("SPEI. (n.d.). Information SPEI, The Standardised Precipitation-Evapotranspiration Index. Retrieved January 8, 2024, from https://spei.csic.es/home.html"),
        sidebarPanel("US EPA. 2016. April 19 Particulate Matter (PM) Basics. [Overviews and Factsheets]. Retrieved January 11, 2024, from https://www.epa.gov/pm-pollution/particulate-matter-pm-basics"),
        sidebarPanel("World Statistics. 2023. Retrieved November 18, 2023, from http://www.world-statistics.org/"),
        sidebarPanel("Worldbank. (n.d.). PM2.5 air pollution, mean annual exposure (micrograms per cubic meter) | Data. Retrieved January 15, 2024, from https://data.worldbank.org/indicator/EN.ATM.PM25.MC.M3")
      ),
    ),
  ),
 ),
)

server <- function(input, output, session) {
  
  #database download
  {
  # SPEI
  url1 <- "https://lcsc.csic.es/ficheros/countries_spei.zip"
  
  if(!file.exists("data")){
    dir.create("data")
  }
  if(!file.exists("data/spei")){
    dir.create("data/spei")
  }
  dest_file1 <- "./data/drought.zip"
  dir1 <- "./data/spei/"
  if(!file.exists(dest_file1)){
    download.file(url1, dest_file1)
  }
  unzip(dest_file1, exdir = dir1)
  
  # Pollution
  url2 <- "https://api.worldbank.org/v2/en/indicator/EN.ATM.PM25.MC.M3?downloadformat=csv"
  
  if(!file.exists("data")){
    dir.create("data")
  }
  if(!file.exists("data/pollution")){
    dir.create("data/pollution")
  }
  dest_file <- "./data/pollution.zip"
  dir2 <- "./data/pollution/"
  
  if(!file.exists(dest_file)){  
    download.file(url2, dest_file)
  }
  
  unzip(dest_file, exdir = dir2)
  wb_pollution <- read_delim("./data/pollution/API_EN.ATM.PM25.MC.M3_DS2_en_csv_v2_6299930.csv", 
                             delim = ",", skip = 3, escape_double = FALSE, trim_ws = TRUE)
  
  # Temperature
  url3 <- "https://fenixservices.fao.org/faostat/static/bulkdownloads/Environment_Temperature_change_E_All_Data.zip"
  
  if(!file.exists("data")){
    dir.create("data")
  }
  if(!file.exists("data/temperature")){
    dir.create("data/temperature")
  }
  dest_file <- "./data/temperature.zip"
  dir3 <- "./data/temperature/"
  
  if(!file.exists(dest_file)){
    download.file(url3, dest_file)
  }
  
  unzip(dest_file, exdir = dir3)
  fao_dtemp <- read_delim("./data/temperature/Environment_Temperature_change_E_All_Data.csv", 
                          delim = ",", escape_double = FALSE, trim_ws = TRUE)
  
  #agricultural production
  
  url01 = "https://fenixservices.fao.org/faostat/static/bulkdownloads/Production_Indices_E_All_Data.zip"
  dest_file01 <- "./data/agro_prod.zip"
  dir01 <- "./data/fao/"
  
  if(!file.exists(dest_file01)){
    download.file(url01, dest_file01)
  }
  
  unzip(dest_file01, exdir = dir01)
  fao_agro <- read_delim("./data/fao/Production_Indices_E_All_Data.csv", 
                         delim = ",", escape_double = FALSE, trim_ws = TRUE)
  
  #arable land areas
  
  url02 = "https://fenixservices.fao.org/faostat/static/bulkdownloads/Environment_LandUse_E_All_Data.zip"
  dest_file02 <- "./data/land_use.zip"
  dir02 <- "./data/fao/"
  
  if(!file.exists(dest_file02)){
   download.file(url02, dest_file02)
  }
  
  unzip(dest_file02, exdir = dir02)
  fao_land <- read_delim("./data/fao/Environment_LandUse_E_All_Data.csv", 
                         delim = ",", escape_double = FALSE, trim_ws = TRUE)
  
  #food supply in population 
  
  url03 = "https://fenixservices.fao.org/faostat/static/bulkdownloads/Food_Security_Data_E_All_Data.zip"
  dest_file03 <- "./data/food_supply.zip"
  dir03 <- "./data/fao/"
  
  if(!file.exists(dest_file03)){
    download.file(url03, dest_file03)
  }
  
  unzip(dest_file03, exdir = dir03)
  fao_supply <- read_delim("./data/fao/Food_Security_Data_E_All_Data_NOFLAG.csv", 
                           delim = ",", escape_double = FALSE, trim_ws = TRUE)
  }
  
  # Maps
  
  {
  map_meast1 <- map_data('world')[map_data('world')$region == "Syria",]
  map_soam1 <- map_data('world')[map_data('world')$region == "Bolivia",]
  map_meast2 <- map_data('world')[map_data('world')$region == "Iran",]
  map_soam2 <- map_data('world')[map_data('world')$region == "Colombia",]

  
  globe_map <- ggplot(map_data('world'), aes(x = long, y = lat, group = group)) +
    geom_polygon(fill = "darkgray", color = "gray") + 
    geom_polygon(data = map_meast1,
                 aes(x=long, y=lat, group = group),
                 color = 'darkgreen', fill = 'orange') +
    geom_polygon(data = map_soam1,
                 aes(x=long, y=lat, group = group),
                 color = 'darkgreen', fill = 'orange') +
    geom_polygon(data = map_meast2,
                 aes(x=long, y=lat, group = group),
                 color = 'darkgreen', fill = 'orange') +
    geom_polygon(data = map_soam2,
                 aes(x=long, y=lat, group = group),
                 color = 'darkgreen', fill = 'orange') +
    coord_fixed(1.3) +
    ggtitle("Global map") +
    theme(panel.background =element_rect(fill = 'blue'))
  
  meast_map <- ggplot() +
      ## First layer: worldwide map
      geom_polygon(data = map_data("world"),
                   aes(x=long, y=lat, group = group),
                   color = 'darkgray', fill = 'lightgray') +
      ## Second layer: Countries map
      geom_polygon(data = map_meast1,
                   aes(x=long, y=lat, group = group),
                   color = 'red', fill = 'orange') +
      geom_polygon(data = map_meast2,
                   aes(x=long, y=lat, group = group),
                   color = 'red', fill = 'orange') +
      coord_map() +
      coord_fixed(1.3,
                  xlim = c(20, 65),
                  ylim = c(20, 50)) +
      ggtitle("A map of Middle East") +
      theme(panel.background =element_rect(fill = 'blue'))
  
  soam_map <- ggplot() +
      ## First layer: worldwide map
      geom_polygon(data = map_data("world"),
                   aes(x=long, y=lat, group = group),
                   color = 'darkgray', fill = 'lightgray') +
      ## Second layer: Countries map
      geom_polygon(data = map_soam1,
                   aes(x=long, y=lat, group = group),
                   color = 'yellow', fill = 'darkgreen') +
      geom_polygon(data = map_soam2,
                   aes(x=long, y=lat, group = group),
                   color = 'yellow', fill = 'darkgreen') +
      coord_map() +
      coord_fixed(1.3,
                  xlim = c(-100, -25),
                  ylim = c(-30, 15)) +
      ggtitle("A map of South America") +
      theme(panel.background =element_rect(fill = 'blue'))
  
  Sys.sleep(5)
  observe({
    shinyjs::enable("loading-message")
  })
  
  output$region_map <- renderPlot(if (input$reg0 == "meast") {
          shinyjs::disable("loading-message")
          meast_map
        } else if (input$reg0 == "soam") {
          shinyjs::disable("loading-message")
          soam_map
        } else if (input$reg0 == "world") {
          shinyjs::disable("loading-message")
          globe_map
        }
    )
  }
  
  # Meteorological charts
  
  {
  output$spei_pl <- renderPlot(if (input$reg4 == "Syria") {
      plot(SPEI_Syr$Year, SPEI_Syr$SPEI_12, xlab="Year", ylab="SPEI")
    } else if (input$reg4 == "Colombia") {
      plot(SPEI_Clmb$Year, SPEI_Clmb$SPEI_12, xlab="Year", ylab="SPEI")
    } else if (input$reg4 == "Iran") {
      plot(SPEI_Iran$Year, SPEI_Iran$SPEI_12, xlab="Year", ylab="SPEI")
    } else if (input$reg4 == "Bolivia") {
      plot(SPEI_Blv$Year, SPEI_Blv$SPEI_12, xlab="Year", ylab="SPEI")
    } 
  )
  
  output$pm25_pl <- renderPlot(if (input$reg2 == "Syria") {
      plot(wb_clim_Syr$Year, wb_clim_Syr$Value, xlab="Year", ylab="PM2.5 (mg/m^3)")
    } else if (input$reg2 == "Colombia") {
      plot(wb_clim_Col$Year, wb_clim_Col$Value, xlab="Year", ylab="PM2.5 (mg/m^3)")
    } else if (input$reg2 == "Iran") {
      plot(wb_clim_Iran$Year, wb_clim_Iran$Value, xlab="Year", ylab="PM2.5 (mg/m^3)")
    } else if (input$reg2 == "Bolivia") {
      plot(wb_clim_Bol$Year, wb_clim_Bol$Value, xlab="Year", ylab="PM2.5 (mg/m^3)")
    } 
  )
  
  output$mtc_pl <- renderPlot(if (input$reg3 == "Syria") {
      plot(fao_clim_Syr$Year, fao_clim_Syr$Value, xlab="Year", ylab="Mean temperature change")
    } else if (input$reg3 == "Colombia") {
      plot(fao_clim_Col$Year, fao_clim_Col$Value, xlab="Year", ylab="Mean temperature change")
    } else if (input$reg3 == "Iran") {
      plot(fao_clim_Iran$Year, fao_clim_Iran$Value, xlab="Year", ylab="Mean temperature change")
    } else if (input$reg3 == "Bolivia") {
      plot(fao_clim_Bol$Year, fao_clim_Bol$Value, xlab="Year", ylab="Mean temperature change")
    } 
  )
  }
  
  # SPEI
  {
    SPEI_Syr <- read_delim(unz(dest_file1,"Syria.csv"), 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
    SPEI_Syr <- data.frame(SPEI_Syr)
    SPEI_Syr <- data.frame(SPEI_Syr$X1, SPEI_Syr$X13)
    SPEI_Syr <- SPEI_Syr %>% filter(grepl('-12-01', SPEI_Syr.X1))
    names(SPEI_Syr)[1] <- "Year"
    names(SPEI_Syr)[2] <- "SPEI_12"
    SPEI_Syr$Year = gsub(pattern = "-12-01*", replacement = "", x = SPEI_Syr$Year)
    SPEI_Syr$Year <- as.integer(SPEI_Syr$Year)
    
    SPEI_Iran <- read_delim(unz(dest_file1,"Iran.csv"), 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
    SPEI_Iran <- data.frame(SPEI_Iran)
    SPEI_Iran <- data.frame(SPEI_Iran$X1, SPEI_Iran$X13)
    SPEI_Iran <- SPEI_Iran %>% filter(grepl('-12-01', SPEI_Iran.X1))
    names(SPEI_Iran)[1] <- "Year"
    names(SPEI_Iran)[2] <- "SPEI_12"
    SPEI_Iran$Year = gsub(pattern = "-12-01*", replacement = "", x = SPEI_Iran$Year)
    SPEI_Iran$Year <- as.integer(SPEI_Iran$Year)
  }
  
  {
    SPEI_Clmb <- read_delim(unz(dest_file1,"Colombia.csv"), 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
    SPEI_Clmb <- data.frame(SPEI_Clmb)
    SPEI_Clmb <- data.frame(SPEI_Clmb$X1, SPEI_Clmb$X13)
    SPEI_Clmb <- SPEI_Clmb %>% filter(grepl('-12-01', SPEI_Clmb.X1))
    names(SPEI_Clmb)[1] <- "Year"
    names(SPEI_Clmb)[2] <- "SPEI_12"
    SPEI_Clmb$Year = gsub(pattern = "-12-01*", replacement = "", x = SPEI_Clmb$Year)
    SPEI_Clmb$Year <- as.integer(SPEI_Clmb$Year)
    
    SPEI_Blv <- read_delim(unz(dest_file1,"Bolivia.csv"), 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
    SPEI_Blv <- data.frame(SPEI_Blv)
    SPEI_Blv <- data.frame(SPEI_Blv$X1, SPEI_Blv$X13)
    SPEI_Blv <- SPEI_Blv %>% filter(grepl('-12-01', SPEI_Blv.X1))
    names(SPEI_Blv)[1] <- "Year"
    names(SPEI_Blv)[2] <- "SPEI_12"
    SPEI_Blv$Year = gsub(pattern = "-12-01*", replacement = "", x = SPEI_Blv$Year)
    SPEI_Blv$Year <- as.integer(SPEI_Blv$Year)
  }
  
  # Select index
  
  {
    #Agriculture
    fao_agri_Syr <- fao_agro[fao_agro$Item == "Agriculture" & fao_agro$Area == "Syrian Arab Republic" & fao_agro$Element == "Gross per capita Production Index Number (2014-2016 = 100)",]
    time_list <- c("Y1961", "Y1962", "Y1963", "Y1964", "Y1965", "Y1966", "Y1967", "Y1968", "Y1969", "Y1970", "Y1971", "Y1972", "Y1973", "Y1974", "Y1975", "Y1976", "Y1977", "Y1978", "Y1979", "Y1980", "Y1981", "Y1982", "Y1983", "Y1984", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994", "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005", "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020")
    temp <- t(subset(fao_agri_Syr[,time_list]))
    rownames(temp) <- c(1:length(time_list))
    colnames(temp) <- "Value"
    tempc <- as.data.frame(c(1:length(time_list)))
    colnames(tempc) <- "Year"
    tempc$Year <- tempc$Year + 1960
    tempb <- subset(fao_agri_Syr[,c("Area", "Item", "Element", "Unit")])
    fao_agri_Syr <- cbind(tempc, temp)
    fao_agri_Syr <- merge(tempb, fao_agri_Syr)
    
    fao_agri_Col <- fao_agro[fao_agro$Item == "Agriculture" & fao_agro$Area == "Colombia" & fao_agro$Element == "Gross per capita Production Index Number (2014-2016 = 100)",]
    time_list <- c("Y1961", "Y1962", "Y1963", "Y1964", "Y1965", "Y1966", "Y1967", "Y1968", "Y1969", "Y1970", "Y1971", "Y1972", "Y1973", "Y1974", "Y1975", "Y1976", "Y1977", "Y1978", "Y1979", "Y1980", "Y1981", "Y1982", "Y1983", "Y1984", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994", "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005", "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020")
    temp <- t(subset(fao_agri_Col[,time_list]))
    rownames(temp) <- c(1:length(time_list))
    colnames(temp) <- "Value"
    tempc <- as.data.frame(c(1:length(time_list)))
    colnames(tempc) <- "Year"
    tempc$Year <- tempc$Year + 1960
    tempb <- subset(fao_agri_Col[,c("Area", "Item", "Element", "Unit")])
    fao_agri_Col <- cbind(tempc, temp)
    fao_agri_Col <- merge(tempb, fao_agri_Col)
    
    fao_agri_Iran <- fao_agro[fao_agro$Item == "Agriculture" & fao_agro$Area == "Iran (Islamic Republic of)" & fao_agro$Element == "Gross per capita Production Index Number (2014-2016 = 100)",]
    time_list <- c("Y1961", "Y1962", "Y1963", "Y1964", "Y1965", "Y1966", "Y1967", "Y1968", "Y1969", "Y1970", "Y1971", "Y1972", "Y1973", "Y1974", "Y1975", "Y1976", "Y1977", "Y1978", "Y1979", "Y1980", "Y1981", "Y1982", "Y1983", "Y1984", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994", "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005", "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020")
    temp <- t(subset(fao_agri_Iran[,time_list]))
    rownames(temp) <- c(1:length(time_list))
    colnames(temp) <- "Value"
    tempc <- as.data.frame(c(1:length(time_list)))
    colnames(tempc) <- "Year"
    tempc$Year <- tempc$Year + 1960
    tempb <- subset(fao_agri_Iran[,c("Area", "Item", "Element", "Unit")])
    fao_agri_Iran <- cbind(tempc, temp)
    fao_agri_Iran <- merge(tempb, fao_agri_Iran)
    
    fao_agri_Bol <- fao_agro[fao_agro$Item == "Agriculture" & fao_agro$Area == "Bolivia (Plurinational State of)" & fao_agro$Element == "Gross per capita Production Index Number (2014-2016 = 100)",]
    time_list <- c("Y1961", "Y1962", "Y1963", "Y1964", "Y1965", "Y1966", "Y1967", "Y1968", "Y1969", "Y1970", "Y1971", "Y1972", "Y1973", "Y1974", "Y1975", "Y1976", "Y1977", "Y1978", "Y1979", "Y1980", "Y1981", "Y1982", "Y1983", "Y1984", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994", "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005", "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020")
    temp <- t(subset(fao_agri_Bol[,time_list]))
    rownames(temp) <- c(1:length(time_list))
    colnames(temp) <- "Value"
    tempc <- as.data.frame(c(1:length(time_list)))
    colnames(tempc) <- "Year"
    tempc$Year <- tempc$Year + 1960
    tempb <- subset(fao_agri_Bol[,c("Area", "Item", "Element", "Unit")])
    fao_agri_Bol <- cbind(tempc, temp)
    fao_agri_Bol <- merge(tempb, fao_agri_Bol)
    
    #Crops
    fao_crops_Syr <- fao_agro[fao_agro$Item == "Crops" & fao_agro$Area == "Syrian Arab Republic" & fao_agro$Element == "Gross per capita Production Index Number (2014-2016 = 100)",]
    time_list <- c("Y1961", "Y1962", "Y1963", "Y1964", "Y1965", "Y1966", "Y1967", "Y1968", "Y1969", "Y1970", "Y1971", "Y1972", "Y1973", "Y1974", "Y1975", "Y1976", "Y1977", "Y1978", "Y1979", "Y1980", "Y1981", "Y1982", "Y1983", "Y1984", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994", "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005", "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020")
    temp <- t(subset(fao_crops_Syr[,time_list]))
    rownames(temp) <- c(1:length(time_list))
    colnames(temp) <- "Value"
    tempc <- as.data.frame(c(1:length(time_list)))
    colnames(tempc) <- "Year"
    tempc$Year <- tempc$Year + 1960
    tempb <- subset(fao_crops_Syr[,c("Area", "Item", "Element", "Unit")])
    fao_crops_Syr <- cbind(tempc, temp)
    fao_crops_Syr <- merge(tempb, fao_crops_Syr)
    
    fao_crops_Col <- fao_agro[fao_agro$Item == "Crops" & fao_agro$Area == "Colombia" & fao_agro$Element == "Gross per capita Production Index Number (2014-2016 = 100)",]
    time_list <- c("Y1961", "Y1962", "Y1963", "Y1964", "Y1965", "Y1966", "Y1967", "Y1968", "Y1969", "Y1970", "Y1971", "Y1972", "Y1973", "Y1974", "Y1975", "Y1976", "Y1977", "Y1978", "Y1979", "Y1980", "Y1981", "Y1982", "Y1983", "Y1984", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994", "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005", "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020")
    temp <- t(subset(fao_crops_Col[,time_list]))
    rownames(temp) <- c(1:length(time_list))
    colnames(temp) <- "Value"
    tempc <- as.data.frame(c(1:length(time_list)))
    colnames(tempc) <- "Year"
    tempc$Year <- tempc$Year + 1960
    tempb <- subset(fao_crops_Col[,c("Area", "Item", "Element", "Unit")])
    fao_crops_Col <- cbind(tempc, temp)
    fao_crops_Col <- merge(tempb, fao_crops_Col)
    
    fao_crops_Iran <- fao_agro[fao_agro$Item == "Crops" & fao_agro$Area == "Iran (Islamic Republic of)" & fao_agro$Element == "Gross per capita Production Index Number (2014-2016 = 100)",]
    time_list <- c("Y1961", "Y1962", "Y1963", "Y1964", "Y1965", "Y1966", "Y1967", "Y1968", "Y1969", "Y1970", "Y1971", "Y1972", "Y1973", "Y1974", "Y1975", "Y1976", "Y1977", "Y1978", "Y1979", "Y1980", "Y1981", "Y1982", "Y1983", "Y1984", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994", "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005", "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020")
    temp <- t(subset(fao_crops_Iran[,time_list]))
    rownames(temp) <- c(1:length(time_list))
    colnames(temp) <- "Value"
    tempc <- as.data.frame(c(1:length(time_list)))
    colnames(tempc) <- "Year"
    tempc$Year <- tempc$Year + 1960
    tempb <- subset(fao_crops_Iran[,c("Area", "Item", "Element", "Unit")])
    fao_crops_Iran <- cbind(tempc, temp)
    fao_crops_Iran <- merge(tempb, fao_crops_Iran)
    
    fao_crops_Bol <- fao_agro[fao_agro$Item == "Crops" & fao_agro$Area == "Bolivia (Plurinational State of)" & fao_agro$Element == "Gross per capita Production Index Number (2014-2016 = 100)",]
    time_list <- c("Y1961", "Y1962", "Y1963", "Y1964", "Y1965", "Y1966", "Y1967", "Y1968", "Y1969", "Y1970", "Y1971", "Y1972", "Y1973", "Y1974", "Y1975", "Y1976", "Y1977", "Y1978", "Y1979", "Y1980", "Y1981", "Y1982", "Y1983", "Y1984", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994", "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005", "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020")
    temp <- t(subset(fao_crops_Bol[,time_list]))
    rownames(temp) <- c(1:length(time_list))
    colnames(temp) <- "Value"
    tempc <- as.data.frame(c(1:length(time_list)))
    colnames(tempc) <- "Year"
    tempc$Year <- tempc$Year + 1960
    tempb <- subset(fao_crops_Bol[,c("Area", "Item", "Element", "Unit")])
    fao_crops_Bol <- cbind(tempc, temp)
    fao_crops_Bol <- merge(tempb, fao_crops_Bol)
  }
  
  {
    #Food
    fao_food_Syr <- fao_agro[fao_agro$Item == "Food" & fao_agro$Area == "Syrian Arab Republic" & fao_agro$Element == "Gross per capita Production Index Number (2014-2016 = 100)",]
    time_list <- c("Y1961", "Y1962", "Y1963", "Y1964", "Y1965", "Y1966", "Y1967", "Y1968", "Y1969", "Y1970", "Y1971", "Y1972", "Y1973", "Y1974", "Y1975", "Y1976", "Y1977", "Y1978", "Y1979", "Y1980", "Y1981", "Y1982", "Y1983", "Y1984", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994", "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005", "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020")
    temp <- t(subset(fao_food_Syr[,time_list]))
    rownames(temp) <- c(1:length(time_list))
    colnames(temp) <- "Value"
    tempc <- as.data.frame(c(1:length(time_list)))
    colnames(tempc) <- "Year"
    tempc$Year <- tempc$Year + 1960
    tempb <- subset(fao_food_Syr[,c("Area", "Item", "Element", "Unit")])
    fao_food_Syr <- cbind(tempc, temp)
    fao_food_Syr <- merge(tempb, fao_food_Syr)
    
    fao_food_Col <- fao_agro[fao_agro$Item == "Food" & fao_agro$Area == "Colombia" & fao_agro$Element == "Gross per capita Production Index Number (2014-2016 = 100)",]
    time_list <- c("Y1961", "Y1962", "Y1963", "Y1964", "Y1965", "Y1966", "Y1967", "Y1968", "Y1969", "Y1970", "Y1971", "Y1972", "Y1973", "Y1974", "Y1975", "Y1976", "Y1977", "Y1978", "Y1979", "Y1980", "Y1981", "Y1982", "Y1983", "Y1984", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994", "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005", "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020")
    temp <- t(subset(fao_food_Col[,time_list]))
    rownames(temp) <- c(1:length(time_list))
    colnames(temp) <- "Value"
    tempc <- as.data.frame(c(1:length(time_list)))
    colnames(tempc) <- "Year"
    tempc$Year <- tempc$Year + 1960
    tempb <- subset(fao_food_Col[,c("Area", "Item", "Element", "Unit")])
    fao_food_Col <- cbind(tempc, temp)
    fao_food_Col <- merge(tempb, fao_food_Col)
    
    fao_food_Iran <- fao_agro[fao_agro$Item == "Food" & fao_agro$Area == "Iran (Islamic Republic of)" & fao_agro$Element == "Gross per capita Production Index Number (2014-2016 = 100)",]
    time_list <- c("Y1961", "Y1962", "Y1963", "Y1964", "Y1965", "Y1966", "Y1967", "Y1968", "Y1969", "Y1970", "Y1971", "Y1972", "Y1973", "Y1974", "Y1975", "Y1976", "Y1977", "Y1978", "Y1979", "Y1980", "Y1981", "Y1982", "Y1983", "Y1984", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994", "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005", "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020")
    temp <- t(subset(fao_food_Iran[,time_list]))
    rownames(temp) <- c(1:length(time_list))
    colnames(temp) <- "Value"
    tempc <- as.data.frame(c(1:length(time_list)))
    colnames(tempc) <- "Year"
    tempc$Year <- tempc$Year + 1960
    tempb <- subset(fao_food_Iran[,c("Area", "Item", "Element", "Unit")])
    fao_food_Iran <- cbind(tempc, temp)
    fao_food_Iran <- merge(tempb, fao_food_Iran)
    
    fao_food_Bol <- fao_agro[fao_agro$Item == "Food" & fao_agro$Area == "Bolivia (Plurinational State of)" & fao_agro$Element == "Gross per capita Production Index Number (2014-2016 = 100)",]
    time_list <- c("Y1961", "Y1962", "Y1963", "Y1964", "Y1965", "Y1966", "Y1967", "Y1968", "Y1969", "Y1970", "Y1971", "Y1972", "Y1973", "Y1974", "Y1975", "Y1976", "Y1977", "Y1978", "Y1979", "Y1980", "Y1981", "Y1982", "Y1983", "Y1984", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994", "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005", "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020")
    temp <- t(subset(fao_food_Bol[,time_list]))
    rownames(temp) <- c(1:length(time_list))
    colnames(temp) <- "Value"
    tempc <- as.data.frame(c(1:length(time_list)))
    colnames(tempc) <- "Year"
    tempc$Year <- tempc$Year + 1960
    tempb <- subset(fao_food_Bol[,c("Area", "Item", "Element", "Unit")])
    fao_food_Bol <- cbind(tempc, temp)
    fao_food_Bol <- merge(tempb, fao_food_Bol)
    
    #Livestock
    fao_live_Syr <- fao_agro[fao_agro$Item == "Livestock" & fao_agro$Area == "Syrian Arab Republic" & fao_agro$Element == "Gross per capita Production Index Number (2014-2016 = 100)",]
    time_list <- c("Y1961", "Y1962", "Y1963", "Y1964", "Y1965", "Y1966", "Y1967", "Y1968", "Y1969", "Y1970", "Y1971", "Y1972", "Y1973", "Y1974", "Y1975", "Y1976", "Y1977", "Y1978", "Y1979", "Y1980", "Y1981", "Y1982", "Y1983", "Y1984", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994", "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005", "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020")
    temp <- t(subset(fao_live_Syr[,time_list]))
    rownames(temp) <- c(1:length(time_list))
    colnames(temp) <- "Value"
    tempc <- as.data.frame(c(1:length(time_list)))
    colnames(tempc) <- "Year"
    tempc$Year <- tempc$Year + 1960
    tempb <- subset(fao_live_Syr[,c("Area", "Item", "Element", "Unit")])
    fao_live_Syr <- cbind(tempc, temp)
    fao_live_Syr <- merge(tempb, fao_live_Syr)
    
    fao_live_Col <- fao_agro[fao_agro$Item == "Livestock" & fao_agro$Area == "Colombia" & fao_agro$Element == "Gross per capita Production Index Number (2014-2016 = 100)",]
    time_list <- c("Y1961", "Y1962", "Y1963", "Y1964", "Y1965", "Y1966", "Y1967", "Y1968", "Y1969", "Y1970", "Y1971", "Y1972", "Y1973", "Y1974", "Y1975", "Y1976", "Y1977", "Y1978", "Y1979", "Y1980", "Y1981", "Y1982", "Y1983", "Y1984", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994", "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005", "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020")
    temp <- t(subset(fao_live_Col[,time_list]))
    rownames(temp) <- c(1:length(time_list))
    colnames(temp) <- "Value"
    tempc <- as.data.frame(c(1:length(time_list)))
    colnames(tempc) <- "Year"
    tempc$Year <- tempc$Year + 1960
    tempb <- subset(fao_live_Col[,c("Area", "Item", "Element", "Unit")])
    fao_live_Col <- cbind(tempc, temp)
    fao_live_Col <- merge(tempb, fao_live_Col)
    
    fao_live_Iran <- fao_agro[fao_agro$Item == "Livestock" & fao_agro$Area == "Iran (Islamic Republic of)" & fao_agro$Element == "Gross per capita Production Index Number (2014-2016 = 100)",]
    time_list <- c("Y1961", "Y1962", "Y1963", "Y1964", "Y1965", "Y1966", "Y1967", "Y1968", "Y1969", "Y1970", "Y1971", "Y1972", "Y1973", "Y1974", "Y1975", "Y1976", "Y1977", "Y1978", "Y1979", "Y1980", "Y1981", "Y1982", "Y1983", "Y1984", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994", "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005", "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020")
    temp <- t(subset(fao_live_Iran[,time_list]))
    rownames(temp) <- c(1:length(time_list))
    colnames(temp) <- "Value"
    tempc <- as.data.frame(c(1:length(time_list)))
    colnames(tempc) <- "Year"
    tempc$Year <- tempc$Year + 1960
    tempb <- subset(fao_live_Iran[,c("Area", "Item", "Element", "Unit")])
    fao_live_Iran <- cbind(tempc, temp)
    fao_live_Iran <- merge(tempb, fao_live_Iran)
    
    fao_live_Bol <- fao_agro[fao_agro$Item == "Livestock" & fao_agro$Area == "Bolivia (Plurinational State of)" & fao_agro$Element == "Gross per capita Production Index Number (2014-2016 = 100)",]
    time_list <- c("Y1961", "Y1962", "Y1963", "Y1964", "Y1965", "Y1966", "Y1967", "Y1968", "Y1969", "Y1970", "Y1971", "Y1972", "Y1973", "Y1974", "Y1975", "Y1976", "Y1977", "Y1978", "Y1979", "Y1980", "Y1981", "Y1982", "Y1983", "Y1984", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994", "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005", "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020")
    temp <- t(subset(fao_live_Bol[,time_list]))
    rownames(temp) <- c(1:length(time_list))
    colnames(temp) <- "Value"
    tempc <- as.data.frame(c(1:length(time_list)))
    colnames(tempc) <- "Year"
    tempc$Year <- tempc$Year + 1960
    tempb <- subset(fao_live_Bol[,c("Area", "Item", "Element", "Unit")])
    fao_live_Bol <- cbind(tempc, temp)
    fao_live_Bol <- merge(tempb, fao_live_Bol)
  }
  
  # additional climate indices
  # PM 2.5
{
  wb_clim_Syr <- wb_pollution[wb_pollution$`Country Name` == "Syrian Arab Republic",]
  time_list <- c("1961", "1962", "1963", "1964", "1965", "1966", "1967", "1968", "1969", "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
  temp <- t(subset(wb_clim_Syr[,time_list]))
  rownames(temp) <- c(1:length(time_list))
  colnames(temp) <- "Value"
  tempc <- as.data.frame(c(1:length(time_list)))
  colnames(tempc) <- "Year"
  tempc$Year <- tempc$Year + 1960
  tempb <- subset(wb_clim_Syr[,c("Country Name", "Indicator Name")])
  wb_clim_Syr <- cbind(tempc, temp)
  wb_clim_Syr <- merge(tempb, wb_clim_Syr)
  
  wb_clim_Saf <- wb_pollution[wb_pollution$`Country Name` == "South Africa",]
  time_list <- c("1961", "1962", "1963", "1964", "1965", "1966", "1967", "1968", "1969", "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
  temp <- t(subset(wb_clim_Saf[,time_list]))
  rownames(temp) <- c(1:length(time_list))
  colnames(temp) <- "Value"
  tempc <- as.data.frame(c(1:length(time_list)))
  colnames(tempc) <- "Year"
  tempc$Year <- tempc$Year + 1960
  tempb <- subset(wb_clim_Saf[,c("Country Name", "Indicator Name")])
  wb_clim_Saf <- cbind(tempc, temp)
  wb_clim_Saf <- merge(tempb, wb_clim_Saf)
  
  wb_clim_Peru <- wb_pollution[wb_pollution$`Country Name` == "Peru",]
  time_list <- c("1961", "1962", "1963", "1964", "1965", "1966", "1967", "1968", "1969", "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
  temp <- t(subset(wb_clim_Peru[,time_list]))
  rownames(temp) <- c(1:length(time_list))
  colnames(temp) <- "Value"
  tempc <- as.data.frame(c(1:length(time_list)))
  colnames(tempc) <- "Year"
  tempc$Year <- tempc$Year + 1960
  tempb <- subset(wb_clim_Peru[,c("Country Name", "Indicator Name")])
  wb_clim_Peru <- cbind(tempc, temp)
  wb_clim_Peru <- merge(tempb, wb_clim_Peru)
  
  wb_clim_Jor <- wb_pollution[wb_pollution$`Country Name` == "Jordan",]
  time_list <- c("1961", "1962", "1963", "1964", "1965", "1966", "1967", "1968", "1969", "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
  temp <- t(subset(wb_clim_Jor[,time_list]))
  rownames(temp) <- c(1:length(time_list))
  colnames(temp) <- "Value"
  tempc <- as.data.frame(c(1:length(time_list)))
  colnames(tempc) <- "Year"
  tempc$Year <- tempc$Year + 1960
  tempb <- subset(wb_clim_Jor[,c("Country Name", "Indicator Name")])
  wb_clim_Jor <- cbind(tempc, temp)
  wb_clim_Jor <- merge(tempb, wb_clim_Jor)
  
  wb_clim_Ang <- wb_pollution[wb_pollution$`Country Name` == "Angola",]
  time_list <- c("1961", "1962", "1963", "1964", "1965", "1966", "1967", "1968", "1969", "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
  temp <- t(subset(wb_clim_Ang[,time_list]))
  rownames(temp) <- c(1:length(time_list))
  colnames(temp) <- "Value"
  tempc <- as.data.frame(c(1:length(time_list)))
  colnames(tempc) <- "Year"
  tempc$Year <- tempc$Year + 1960
  tempb <- subset(wb_clim_Ang[,c("Country Name", "Indicator Name")])
  wb_clim_Ang <- cbind(tempc, temp)
  wb_clim_Ang <- merge(tempb, wb_clim_Ang)
  
  wb_clim_Col <- wb_pollution[wb_pollution$`Country Name` == "Colombia",]
  time_list <- c("1961", "1962", "1963", "1964", "1965", "1966", "1967", "1968", "1969", "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
  temp <- t(subset(wb_clim_Col[,time_list]))
  rownames(temp) <- c(1:length(time_list))
  colnames(temp) <- "Value"
  tempc <- as.data.frame(c(1:length(time_list)))
  colnames(tempc) <- "Year"
  tempc$Year <- tempc$Year + 1960
  tempb <- subset(wb_clim_Col[,c("Country Name", "Indicator Name")])
  wb_clim_Col <- cbind(tempc, temp)
  wb_clim_Col <- merge(tempb, wb_clim_Col)
  
  wb_clim_Iran <- wb_pollution[wb_pollution$`Country Name` == "Iran, Islamic Rep.",]
  time_list <- c("1961", "1962", "1963", "1964", "1965", "1966", "1967", "1968", "1969", "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
  temp <- t(subset(wb_clim_Iran[,time_list]))
  rownames(temp) <- c(1:length(time_list))
  colnames(temp) <- "Value"
  tempc <- as.data.frame(c(1:length(time_list)))
  colnames(tempc) <- "Year"
  tempc$Year <- tempc$Year + 1960
  tempb <- subset(wb_clim_Iran[,c("Country Name", "Indicator Name")])
  wb_clim_Iran <- cbind(tempc, temp)
  wb_clim_Iran <- merge(tempb, wb_clim_Iran)
  
  wb_clim_Nam <- wb_pollution[wb_pollution$`Country Name` == "Namibia",]
  time_list <- c("1961", "1962", "1963", "1964", "1965", "1966", "1967", "1968", "1969", "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
  temp <- t(subset(wb_clim_Nam[,time_list]))
  rownames(temp) <- c(1:length(time_list))
  colnames(temp) <- "Value"
  tempc <- as.data.frame(c(1:length(time_list)))
  colnames(tempc) <- "Year"
  tempc$Year <- tempc$Year + 1960
  tempb <- subset(wb_clim_Nam[,c("Country Name", "Indicator Name")])
  wb_clim_Nam <- cbind(tempc, temp)
  wb_clim_Nam <- merge(tempb, wb_clim_Nam)
  
  wb_clim_Bol <- wb_pollution[wb_pollution$`Country Name` == "Bolivia",]
  time_list <- c("1961", "1962", "1963", "1964", "1965", "1966", "1967", "1968", "1969", "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
  temp <- t(subset(wb_clim_Bol[,time_list]))
  rownames(temp) <- c(1:length(time_list))
  colnames(temp) <- "Value"
  tempc <- as.data.frame(c(1:length(time_list)))
  colnames(tempc) <- "Year"
  tempc$Year <- tempc$Year + 1960
  tempb <- subset(wb_clim_Bol[,c("Country Name", "Indicator Name")])
  wb_clim_Bol <- cbind(tempc, temp)
  wb_clim_Bol <- merge(tempb, wb_clim_Bol)
}  
  
  # Temperature change
{
  fao_clim_Syr <- fao_dtemp[fao_dtemp$Months == "Meteorological year" & fao_dtemp$Area == "Syrian Arab Republic" & fao_dtemp$Element == "Temperature change",]
  time_list <- c("Y1961", "Y1962", "Y1963", "Y1964", "Y1965", "Y1966", "Y1967", "Y1968", "Y1969", "Y1970", "Y1971", "Y1972", "Y1973", "Y1974", "Y1975", "Y1976", "Y1977", "Y1978", "Y1979", "Y1980", "Y1981", "Y1982", "Y1983", "Y1984", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994", "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005", "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020")
  temp <- t(subset(fao_clim_Syr[,time_list]))
  rownames(temp) <- c(1:length(time_list))
  colnames(temp) <- "Value"
  tempc <- as.data.frame(c(1:length(time_list)))
  colnames(tempc) <- "Year"
  tempc$Year <- tempc$Year + 1960
  tempb <- subset(fao_clim_Syr[,c("Area", "Months", "Element", "Unit")])
  fao_clim_Syr <- cbind(tempc, temp)
  fao_clim_Syr <- merge(tempb, fao_clim_Syr)
  
  fao_clim_Saf <- fao_dtemp[fao_dtemp$Months == "Meteorological year" & fao_dtemp$Area == "South Africa" & fao_dtemp$Element == "Temperature change",]
  time_list <- c("Y1961", "Y1962", "Y1963", "Y1964", "Y1965", "Y1966", "Y1967", "Y1968", "Y1969", "Y1970", "Y1971", "Y1972", "Y1973", "Y1974", "Y1975", "Y1976", "Y1977", "Y1978", "Y1979", "Y1980", "Y1981", "Y1982", "Y1983", "Y1984", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994", "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005", "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020")
  temp <- t(subset(fao_clim_Saf[,time_list]))
  rownames(temp) <- c(1:length(time_list))
  colnames(temp) <- "Value"
  tempc <- as.data.frame(c(1:length(time_list)))
  colnames(tempc) <- "Year"
  tempc$Year <- tempc$Year + 1960
  tempb <- subset(fao_clim_Saf[,c("Area", "Months", "Element", "Unit")])
  fao_clim_Saf <- cbind(tempc, temp)
  fao_clim_Saf <- merge(tempb, fao_clim_Saf)
  
  fao_clim_Peru <- fao_dtemp[fao_dtemp$Months == "Meteorological year" & fao_dtemp$Area == "Peru" & fao_dtemp$Element == "Temperature change",]
  time_list <- c("Y1961", "Y1962", "Y1963", "Y1964", "Y1965", "Y1966", "Y1967", "Y1968", "Y1969", "Y1970", "Y1971", "Y1972", "Y1973", "Y1974", "Y1975", "Y1976", "Y1977", "Y1978", "Y1979", "Y1980", "Y1981", "Y1982", "Y1983", "Y1984", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994", "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005", "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020")
  temp <- t(subset(fao_clim_Peru[,time_list]))
  rownames(temp) <- c(1:length(time_list))
  colnames(temp) <- "Value"
  tempc <- as.data.frame(c(1:length(time_list)))
  colnames(tempc) <- "Year"
  tempc$Year <- tempc$Year + 1960
  tempb <- subset(fao_clim_Peru[,c("Area", "Months", "Element", "Unit")])
  fao_clim_Peru <- cbind(tempc, temp)
  fao_clim_Peru <- merge(tempb, fao_clim_Peru)
  
  fao_clim_Jor <- fao_dtemp[fao_dtemp$Months == "Meteorological year" & fao_dtemp$Area == "Jordan" & fao_dtemp$Element == "Temperature change",]
  time_list <- c("Y1961", "Y1962", "Y1963", "Y1964", "Y1965", "Y1966", "Y1967", "Y1968", "Y1969", "Y1970", "Y1971", "Y1972", "Y1973", "Y1974", "Y1975", "Y1976", "Y1977", "Y1978", "Y1979", "Y1980", "Y1981", "Y1982", "Y1983", "Y1984", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994", "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005", "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020")
  temp <- t(subset(fao_clim_Jor[,time_list]))
  rownames(temp) <- c(1:length(time_list))
  colnames(temp) <- "Value"
  tempc <- as.data.frame(c(1:length(time_list)))
  colnames(tempc) <- "Year"
  tempc$Year <- tempc$Year + 1960
  tempb <- subset(fao_clim_Jor[,c("Area", "Months", "Element", "Unit")])
  fao_clim_Jor <- cbind(tempc, temp)
  fao_clim_Jor <- merge(tempb, fao_clim_Jor)
  
  fao_clim_Ang <- fao_dtemp[fao_dtemp$Months == "Meteorological year" & fao_dtemp$Area == "Angola" & fao_dtemp$Element == "Temperature change",]
  time_list <- c("Y1961", "Y1962", "Y1963", "Y1964", "Y1965", "Y1966", "Y1967", "Y1968", "Y1969", "Y1970", "Y1971", "Y1972", "Y1973", "Y1974", "Y1975", "Y1976", "Y1977", "Y1978", "Y1979", "Y1980", "Y1981", "Y1982", "Y1983", "Y1984", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994", "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005", "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020")
  temp <- t(subset(fao_clim_Ang[,time_list]))
  rownames(temp) <- c(1:length(time_list))
  colnames(temp) <- "Value"
  tempc <- as.data.frame(c(1:length(time_list)))
  colnames(tempc) <- "Year"
  tempc$Year <- tempc$Year + 1960
  tempb <- subset(fao_clim_Ang[,c("Area", "Months", "Element", "Unit")])
  fao_clim_Ang <- cbind(tempc, temp)
  fao_clim_Ang <- merge(tempb, fao_clim_Ang)
  
  fao_clim_Col <- fao_dtemp[fao_dtemp$Months == "Meteorological year" & fao_dtemp$Area == "Colombia" & fao_dtemp$Element == "Temperature change",]
  time_list <- c("Y1961", "Y1962", "Y1963", "Y1964", "Y1965", "Y1966", "Y1967", "Y1968", "Y1969", "Y1970", "Y1971", "Y1972", "Y1973", "Y1974", "Y1975", "Y1976", "Y1977", "Y1978", "Y1979", "Y1980", "Y1981", "Y1982", "Y1983", "Y1984", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994", "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005", "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020")
  temp <- t(subset(fao_clim_Col[,time_list]))
  rownames(temp) <- c(1:length(time_list))
  colnames(temp) <- "Value"
  tempc <- as.data.frame(c(1:length(time_list)))
  colnames(tempc) <- "Year"
  tempc$Year <- tempc$Year + 1960
  tempb <- subset(fao_clim_Col[,c("Area", "Months", "Element", "Unit")])
  fao_clim_Col <- cbind(tempc, temp)
  fao_clim_Col <- merge(tempb, fao_clim_Col)
  
  fao_clim_Iran <- fao_dtemp[fao_dtemp$Months == "Meteorological year" & fao_dtemp$Area == "Iran (Islamic Republic of)" & fao_dtemp$Element == "Temperature change",]
  time_list <- c("Y1961", "Y1962", "Y1963", "Y1964", "Y1965", "Y1966", "Y1967", "Y1968", "Y1969", "Y1970", "Y1971", "Y1972", "Y1973", "Y1974", "Y1975", "Y1976", "Y1977", "Y1978", "Y1979", "Y1980", "Y1981", "Y1982", "Y1983", "Y1984", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994", "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005", "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020")
  temp <- t(subset(fao_clim_Iran[,time_list]))
  rownames(temp) <- c(1:length(time_list))
  colnames(temp) <- "Value"
  tempc <- as.data.frame(c(1:length(time_list)))
  colnames(tempc) <- "Year"
  tempc$Year <- tempc$Year + 1960
  tempb <- subset(fao_clim_Iran[,c("Area", "Months", "Element", "Unit")])
  fao_clim_Iran <- cbind(tempc, temp)
  fao_clim_Iran <- merge(tempb, fao_clim_Iran)
  
  fao_clim_Nam <- fao_dtemp[fao_dtemp$Months == "Meteorological year" & fao_dtemp$Area == "Namibia" & fao_dtemp$Element == "Temperature change",]
  time_list <- c("Y1961", "Y1962", "Y1963", "Y1964", "Y1965", "Y1966", "Y1967", "Y1968", "Y1969", "Y1970", "Y1971", "Y1972", "Y1973", "Y1974", "Y1975", "Y1976", "Y1977", "Y1978", "Y1979", "Y1980", "Y1981", "Y1982", "Y1983", "Y1984", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994", "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005", "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020")
  temp <- t(subset(fao_clim_Nam[,time_list]))
  rownames(temp) <- c(1:length(time_list))
  colnames(temp) <- "Value"
  tempc <- as.data.frame(c(1:length(time_list)))
  colnames(tempc) <- "Year"
  tempc$Year <- tempc$Year + 1960
  tempb <- subset(fao_clim_Nam[,c("Area", "Months", "Element", "Unit")])
  fao_clim_Nam <- cbind(tempc, temp)
  fao_clim_Nam <- merge(tempb, fao_clim_Nam)
  
  fao_clim_Bol <- fao_dtemp[fao_dtemp$Months == "Meteorological year" & fao_dtemp$Area == "Bolivia (Plurinational State of)" & fao_dtemp$Element == "Temperature change",]
  time_list <- c("Y1961", "Y1962", "Y1963", "Y1964", "Y1965", "Y1966", "Y1967", "Y1968", "Y1969", "Y1970", "Y1971", "Y1972", "Y1973", "Y1974", "Y1975", "Y1976", "Y1977", "Y1978", "Y1979", "Y1980", "Y1981", "Y1982", "Y1983", "Y1984", "Y1985", "Y1986", "Y1987", "Y1988", "Y1989", "Y1990", "Y1991", "Y1992", "Y1993", "Y1994", "Y1995", "Y1996", "Y1997", "Y1998", "Y1999", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005", "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020")
  temp <- t(subset(fao_clim_Bol[,time_list]))
  rownames(temp) <- c(1:length(time_list))
  colnames(temp) <- "Value"
  tempc <- as.data.frame(c(1:length(time_list)))
  colnames(tempc) <- "Year"
  tempc$Year <- tempc$Year + 1960
  tempb <- subset(fao_clim_Bol[,c("Area", "Months", "Element", "Unit")])
  fao_clim_Bol <- cbind(tempc, temp)
  fao_clim_Bol <- merge(tempb, fao_clim_Bol)
  fao_clim_Bol <- merge(tempb, fao_clim_Bol)
}
  
  # Correlations 
  {
    c1d1 = merge(SPEI_Syr, fao_agri_Syr, by = "Year")
    c1d2 = merge(SPEI_Syr, fao_crops_Syr, by = "Year")
    c1d4 = merge(SPEI_Syr, fao_food_Syr, by = "Year")
    c1d5 = merge(SPEI_Syr, fao_live_Syr, by = "Year")
    
    c2d1 = merge(SPEI_Clmb, fao_agri_Col, by = "Year")
    c2d2 = merge(SPEI_Clmb, fao_crops_Col, by = "Year")
    c2d4 = merge(SPEI_Clmb, fao_food_Col, by = "Year")
    c2d5 = merge(SPEI_Clmb, fao_live_Col, by = "Year")
    
    c3d1 = merge(SPEI_Iran, fao_agri_Iran, by = "Year")
    c3d2 = merge(SPEI_Iran, fao_crops_Iran, by = "Year")
    c3d4 = merge(SPEI_Iran, fao_food_Iran, by = "Year")
    c3d5 = merge(SPEI_Iran, fao_live_Iran, by = "Year")
    
    c4d1 = merge(SPEI_Blv, fao_agri_Bol, by = "Year")
    c4d2 = merge(SPEI_Blv, fao_crops_Bol, by = "Year")
    c4d4 = merge(SPEI_Blv, fao_food_Bol, by = "Year")
    c4d5 = merge(SPEI_Blv, fao_live_Bol, by = "Year")
    
    c1p1 = merge(wb_clim_Syr, fao_agri_Syr, by = "Year")
    c1p1 <- na.omit(c1p1)
    c1p2 = merge(wb_clim_Syr, fao_crops_Syr, by = "Year")
    c1p2 <- na.omit(c1p2)
    c1p4 = merge(wb_clim_Syr, fao_food_Syr, by = "Year")
    c1p4 <- na.omit(c1p4)
    c1p5 = merge(wb_clim_Syr, fao_live_Syr, by = "Year")
    c1p5 <- na.omit(c1p5)
    
    c2p1 = merge(wb_clim_Col, fao_agri_Col, by = "Year")
    c2p1 <- na.omit(c2p1)
    c2p2 = merge(wb_clim_Col, fao_crops_Col, by = "Year")
    c2p2 <- na.omit(c2p2)
    c2p4 = merge(wb_clim_Col, fao_food_Col, by = "Year")
    c2p4 <- na.omit(c2p4)
    c2p5 = merge(wb_clim_Col, fao_live_Col, by = "Year")
    c2p5 <- na.omit(c2p5)
    
    c3p1 = merge(wb_clim_Iran, fao_agri_Iran, by = "Year")
    c3p1 <- na.omit(c3p1)
    c3p2 = merge(wb_clim_Iran, fao_crops_Iran, by = "Year")
    c3p2 <- na.omit(c3p2)
    c3p4 = merge(wb_clim_Iran, fao_food_Iran, by = "Year")
    c3p4 <- na.omit(c3p4)
    c3p5 = merge(wb_clim_Iran, fao_live_Iran, by = "Year")
    c3p5 <- na.omit(c3p5)
    
    c4p1 = merge(wb_clim_Bol, fao_agri_Bol, by = "Year")
    c4p1 <- na.omit(c4p1)
    c4p2 = merge(wb_clim_Bol, fao_crops_Bol, by = "Year")
    c4p2 <- na.omit(c4p2)
    c4p4 = merge(wb_clim_Bol, fao_food_Bol, by = "Year")
    c4p4 <- na.omit(c4p4)
    c4p5 = merge(wb_clim_Bol, fao_live_Bol, by = "Year")
    c4p5 <- na.omit(c4p5)
    
    c1t1 = merge(fao_clim_Syr, fao_agri_Syr, by = "Year")
    c1t2 = merge(fao_clim_Syr, fao_crops_Syr, by = "Year")
    c1t4 = merge(fao_clim_Syr, fao_food_Syr, by = "Year")
    c1t5 = merge(fao_clim_Syr, fao_live_Syr, by = "Year")
    
    c2t1 = merge(fao_clim_Col, fao_agri_Col, by = "Year")
    c2t2 = merge(fao_clim_Col, fao_crops_Col, by = "Year")
    c2t4 = merge(fao_clim_Col, fao_food_Col, by = "Year")
    c2t5 = merge(fao_clim_Col, fao_live_Col, by = "Year")
    
    c3t1 = merge(fao_clim_Iran, fao_agri_Iran, by = "Year")
    c3t2 = merge(fao_clim_Iran, fao_crops_Iran, by = "Year")
    c3t4 = merge(fao_clim_Iran, fao_food_Iran, by = "Year")
    c3t5 = merge(fao_clim_Iran, fao_live_Iran, by = "Year")
    
    c4t1 = merge(fao_clim_Bol, fao_agri_Bol, by = "Year")
    c4t2 = merge(fao_clim_Bol, fao_crops_Bol, by = "Year")
    c4t4 = merge(fao_clim_Bol, fao_food_Bol, by = "Year")
    c4t5 = merge(fao_clim_Bol, fao_live_Bol, by = "Year")
    
    # Linear SPEI
    
    lin111 <- lm(c1d1$Value ~ c1d1$SPEI_12)
    lin121 <- lm(c1d2$Value ~ c1d2$SPEI_12)
    lin141 <- lm(c1d4$Value ~ c1d4$SPEI_12)
    lin151 <- lm(c1d5$Value ~ c1d5$SPEI_12)
    
    lin211 <- lm(c2d1$Value ~ c2d1$SPEI_12)
    lin221 <- lm(c2d2$Value ~ c2d2$SPEI_12)
    lin241 <- lm(c2d4$Value ~ c2d4$SPEI_12)
    lin251 <- lm(c2d5$Value ~ c2d5$SPEI_12)
    
    lin311 <- lm(c3d1$Value ~ c3d1$SPEI_12)
    lin321 <- lm(c3d2$Value ~ c3d2$SPEI_12)
    lin341 <- lm(c3d4$Value ~ c3d4$SPEI_12)
    lin351 <- lm(c3d5$Value ~ c3d5$SPEI_12)
    
    lin411 <- lm(c4d1$Value ~ c4d1$SPEI_12)
    lin421 <- lm(c4d2$Value ~ c4d2$SPEI_12)
    lin441 <- lm(c4d4$Value ~ c4d4$SPEI_12)
    lin451 <- lm(c4d5$Value ~ c4d5$SPEI_12)
    
    # Quadratic SPEI
    
    qua111 <- lm(c1d1$Value ~ c1d1$SPEI_12 + I(c1d1$SPEI_12^2))
    qua121 <- lm(c1d2$Value ~ c1d2$SPEI_12 + I(c1d2$SPEI_12^2))
    qua141 <- lm(c1d4$Value ~ c1d4$SPEI_12 + I(c1d4$SPEI_12^2))
    qua151 <- lm(c1d5$Value ~ c1d5$SPEI_12 + I(c1d5$SPEI_12^2))
    
    qua211 <- lm(c2d1$Value ~ c2d1$SPEI_12 + I(c2d1$SPEI_12^2))
    qua221 <- lm(c2d2$Value ~ c2d2$SPEI_12 + I(c2d2$SPEI_12^2))
    qua241 <- lm(c2d4$Value ~ c2d4$SPEI_12 + I(c2d4$SPEI_12^2))
    qua251 <- lm(c2d5$Value ~ c2d5$SPEI_12 + I(c2d5$SPEI_12^2))
    
    qua311 <- lm(c3d1$Value ~ c3d1$SPEI_12 + I(c3d1$SPEI_12^2))
    qua321 <- lm(c3d2$Value ~ c3d2$SPEI_12 + I(c3d2$SPEI_12^2))
    qua341 <- lm(c3d4$Value ~ c3d4$SPEI_12 + I(c3d4$SPEI_12^2))
    qua351 <- lm(c3d5$Value ~ c3d5$SPEI_12 + I(c3d5$SPEI_12^2))
    
    qua411 <- lm(c4d1$Value ~ c4d1$SPEI_12 + I(c4d1$SPEI_12^2))
    qua421 <- lm(c4d2$Value ~ c4d2$SPEI_12 + I(c4d2$SPEI_12^2))
    qua441 <- lm(c4d4$Value ~ c4d4$SPEI_12 + I(c4d4$SPEI_12^2))
    qua451 <- lm(c4d5$Value ~ c4d5$SPEI_12 + I(c4d5$SPEI_12^2))
    
    # Linear PM 2.5
    
    lin112 <- lm(c1p1$Value.x ~ c1p1$Value.y)
    lin122 <- lm(c1p2$Value.x ~ c1p2$Value.y)
    lin142 <- lm(c1p4$Value.x ~ c1p4$Value.y)
    lin152 <- lm(c1p5$Value.x ~ c1p5$Value.y)
    
    lin212 <- lm(c2p1$Value.x ~ c2p1$Value.y)
    lin222 <- lm(c2p2$Value.x ~ c2p2$Value.y)
    lin242 <- lm(c2p4$Value.x ~ c2p4$Value.y)
    lin252 <- lm(c2p5$Value.x ~ c2p5$Value.y)
    
    lin312 <- lm(c3p1$Value.x ~ c3p1$Value.y)
    lin322 <- lm(c3p2$Value.x ~ c3p2$Value.y)
    lin342 <- lm(c3p4$Value.x ~ c3p4$Value.y)
    lin352 <- lm(c3p5$Value.x ~ c3p5$Value.y)
    
    lin412 <- lm(c4p1$Value.x ~ c4p1$Value.y)
    lin422 <- lm(c4p2$Value.x ~ c4p2$Value.y)
    lin442 <- lm(c4p4$Value.x ~ c4p4$Value.y)
    lin452 <- lm(c4p5$Value.x ~ c4p5$Value.y)
    
    # Quadratic PM 2.5
    
    qua112 <- lm(c1p1$Value.x ~ c1p1$Value.y + I(c1p1$Value.y^2))
    qua122 <- lm(c1p2$Value.x ~ c1p2$Value.y + I(c1p2$Value.y^2))
    qua142 <- lm(c1p4$Value.x ~ c1p4$Value.y + I(c1p4$Value.y^2))
    qua152 <- lm(c1p5$Value.x ~ c1p5$Value.y + I(c1p5$Value.y^2))
    
    qua212 <- lm(c2p1$Value.x ~ c2p1$Value.y + I(c2p1$Value.y^2))
    qua222 <- lm(c2p2$Value.x ~ c2p2$Value.y + I(c2p2$Value.y^2))
    qua242 <- lm(c2p4$Value.x ~ c2p4$Value.y + I(c2p4$Value.y^2))
    qua252 <- lm(c2p5$Value.x ~ c2p5$Value.y + I(c2p5$Value.y^2))
    
    qua312 <- lm(c3p1$Value.x ~ c3p1$Value.y + I(c3p1$Value.y^2))
    qua322 <- lm(c3p2$Value.x ~ c3p2$Value.y + I(c3p2$Value.y^2))
    qua342 <- lm(c3p4$Value.x ~ c3p4$Value.y + I(c3p4$Value.y^2))
    qua352 <- lm(c3p5$Value.x ~ c3p5$Value.y + I(c3p5$Value.y^2))
    
    qua412 <- lm(c4p1$Value.x ~ c4p1$Value.y + I(c4p1$Value.y^2))
    qua422 <- lm(c4p2$Value.x ~ c4p2$Value.y + I(c4p2$Value.y^2))
    qua442 <- lm(c4p4$Value.x ~ c4p4$Value.y + I(c4p4$Value.y^2))
    qua452 <- lm(c4p5$Value.x ~ c4p5$Value.y + I(c4p5$Value.y^2))
    
    # Linear dT
    
    lin113 <- lm(c1t1$Value.x ~ c1t1$Value.y)
    lin123 <- lm(c1t2$Value.x ~ c1t2$Value.y)
    lin143 <- lm(c1t4$Value.x ~ c1t4$Value.y)
    lin153 <- lm(c1t5$Value.x ~ c1t5$Value.y)
    
    lin213 <- lm(c2t1$Value.x ~ c2t1$Value.y)
    lin223 <- lm(c2t2$Value.x ~ c2t2$Value.y)
    lin243 <- lm(c2t4$Value.x ~ c2t4$Value.y)
    lin253 <- lm(c2t5$Value.x ~ c2t5$Value.y)
    
    lin313 <- lm(c3t1$Value.x ~ c3t1$Value.y)
    lin323 <- lm(c3t2$Value.x ~ c3t2$Value.y)
    lin343 <- lm(c3t4$Value.x ~ c3t4$Value.y)
    lin353 <- lm(c3t5$Value.x ~ c3t5$Value.y)
    
    lin413 <- lm(c4t1$Value.x ~ c4t1$Value.y)
    lin423 <- lm(c4t2$Value.x ~ c4t2$Value.y)
    lin443 <- lm(c4t4$Value.x ~ c4t4$Value.y)
    lin453 <- lm(c4t5$Value.x ~ c4t5$Value.y)
    
    # Quadratic dT
    
    qua113 <- lm(c1t1$Value.x ~ c1t1$Value.y + I(c1t1$Value.y^2))
    qua123 <- lm(c1t2$Value.x ~ c1t2$Value.y + I(c1t2$Value.y^2))
    qua143 <- lm(c1t4$Value.x ~ c1t4$Value.y + I(c1t4$Value.y^2))
    qua153 <- lm(c1t5$Value.x ~ c1t5$Value.y + I(c1t5$Value.y^2))
    
    qua213 <- lm(c2t1$Value.x ~ c2t1$Value.y + I(c2t1$Value.y^2))
    qua223 <- lm(c2t2$Value.x ~ c2t2$Value.y + I(c2t2$Value.y^2))
    qua243 <- lm(c2t4$Value.x ~ c2t4$Value.y + I(c2t4$Value.y^2))
    qua253 <- lm(c2t5$Value.x ~ c2t5$Value.y + I(c2t5$Value.y^2))
    
    qua313 <- lm(c3t1$Value.x ~ c3t1$Value.y + I(c3t1$Value.y^2))
    qua323 <- lm(c3t2$Value.x ~ c3t2$Value.y + I(c3t2$Value.y^2))
    qua343 <- lm(c3t4$Value.x ~ c3t4$Value.y + I(c3t4$Value.y^2))
    qua353 <- lm(c3t5$Value.x ~ c3t5$Value.y + I(c3t5$Value.y^2))
    
    qua413 <- lm(c4t1$Value.x ~ c4t1$Value.y + I(c4t1$Value.y^2))
    qua423 <- lm(c4t2$Value.x ~ c4t2$Value.y + I(c4t2$Value.y^2))
    qua443 <- lm(c4t4$Value.x ~ c4t4$Value.y + I(c4t4$Value.y^2))
    qua453 <- lm(c4t5$Value.x ~ c4t5$Value.y + I(c4t5$Value.y^2))
    
  }
  
  {
  output$index_pl <- renderPlot(if (input$reg1 == "Syria") {
    if (input$ind == "ind1") {
      plot(c1d1$Year, c1d1$Value, xlab="Year", ylab="Agriculture production relative to 2014-2016 mean (%)")
    } else if (input$ind == "ind2") {
      plot(c1d2$Year, c1d2$Value, xlab="Year", ylab="Crop production relative to 2014-2016 mean (%)")
    } else if (input$ind == "ind4") {
      plot(c1d4$Year, c1d4$Value, xlab="Year", ylab="Total food production relative to 2014-2016 mean (%)" )
    } else if (input$ind == "ind5") {
      plot(c1d5$Year, c1d5$Value, xlab="Year", ylab="Livestock relative to 2014-2016 mean (%)")
    } 
  } else if (input$reg1 == "Colombia") {
    if (input$ind == "ind1") {
      plot(c2d1$Year, c2d1$Value, xlab="Year", ylab="Agriculture production relative to 2014-2016 mean (%)")
    } else if (input$ind == "ind2") {
      plot(c2d2$Year, c2d2$Value, xlab="Year", ylab="Crop production relative to 2014-2016 mean (%)")
    } else if (input$ind == "ind4") {
      plot(c2d4$Year, c2d4$Value, xlab="Year", ylab="Total food production relative to 2014-2016 mean (%)")
    } else if (input$ind == "ind5") {
      plot(c2d5$Year, c2d5$Value, xlab="Year", ylab="Livestock relative to 2014-2016 mean (%)")
    } 
  } else if (input$reg1 == "Iran") {
    if (input$ind == "ind1") {
      plot(c3d1$Year, c3d1$Value, xlab="Year", ylab="Agriculture production relative to 2014-2016 mean (%)")
    } else if (input$ind == "ind2") {
      plot(c3d2$Year, c3d2$Value, xlab="Year", ylab="Crop production relative to 2014-2016 mean (%)")
    } else if (input$ind == "ind4") {
      plot(c3d4$Year, c3d4$Value, xlab="Year", ylab="Total food production relative to 2014-2016 mean (%)")
    } else if (input$ind == "ind5") {
      plot(c3d5$Year, c3d5$Value, xlab="Year", ylab="Livestock relative to 2014-2016 mean (%)")
    } 
  } else if (input$reg1 == "Bolivia") {
    if (input$ind == "ind1") {
      plot(c4d1$Year, c4d1$Value, xlab="Year", ylab="Agriculture production relative to 2014-2016 mean (%)")
    } else if (input$ind == "ind2") {
      plot(c4d2$Year, c4d2$Value, xlab="Year", ylab="Crop production relative to 2014-2016 mean (%)")
    } else if (input$ind == "ind4") {
      plot(c4d4$Year, c4d4$Value, xlab="Year", ylab="Total food production relative to 2014-2016 mean (%)")
    } else if (input$ind == "ind5") {
      plot(c4d5$Year, c4d5$Value, xlab="Year", ylab="Livestock relative to 2014-2016 mean (%)")
    }
  })
  }
  
  # Correlation outputs
  {
    output$tbz <- renderTable(if (input$endex == "tbz1") { if (input$reg1 == "Syria") {
      if (input$ind == "ind1") {
        if (input$powpom == "op1") {
          data.frame(
            Parameter = c("R-squared",
                     "P-value",
                     "AIC",
                     "BIC"),
            Value = as.character(c(glance(lin111)$r.squared,
                                   glance(lin111)$p.value,
                                   AIC(lin111),
                                   BIC(lin111))),
            stringsAsFactors = FALSE)
        } else if (input$powpom == "op2") {
          data.frame(
            Parameter = c("R-squared",
                     "P-value",
                     "AIC",
                     "BIC"),
            Value = as.character(c(glance(qua111)$r.squared,
                                   glance(qua111)$p.value,
                                   AIC(qua111),
                                   BIC(qua111))),
            stringsAsFactors = FALSE)
        }
      } else if (input$ind == "ind2") {
        if (input$powpom == "op1") {
          data.frame(
            Parameter = c("R-squared",
                     "P-value",
                     "AIC",
                     "BIC"),
            Value = as.character(c(glance(lin121)$r.squared,
                                   glance(lin121)$p.value,
                                   AIC(lin121),
                                   BIC(lin121))),
            stringsAsFactors = FALSE)
        } else if (input$powpom == "op2") {
          data.frame(
            Parameter = c("R-squared",
                     "P-value",
                     "AIC",
                     "BIC"),
            Value = as.character(c(glance(qua121)$r.squared,
                                   glance(qua121)$p.value,
                                   AIC(qua121),
                                   BIC(qua121))),
            stringsAsFactors = FALSE)
        } 
      } else if (input$ind == "ind4") {
        if (input$powpom == "op1") {
          data.frame(
            Parameter = c("R-squared",
                     "P-value",
                     "AIC",
                     "BIC"),
            Value = as.character(c(glance(lin141)$r.squared,
                                   glance(lin141)$p.value,
                                   AIC(lin141),
                                   BIC(lin141))),
            stringsAsFactors = FALSE)
        } else if (input$powpom == "op2") {
          data.frame(
            Parameter = c("R-squared",
                     "P-value",
                     "AIC",
                     "BIC"),
            Value = as.character(c(glance(qua141)$r.squared,
                                   glance(qua141)$p.value,
                                   AIC(qua141),
                                   BIC(qua141))),
            stringsAsFactors = FALSE)
        } 
      } else if (input$ind == "ind5") {
        if (input$powpom == "op1") {
          data.frame(
            Parameter = c("R-squared",
                     "P-value",
                     "AIC",
                     "BIC"),
            Value = as.character(c(glance(lin151)$r.squared,
                                   glance(lin151)$p.value,
                                   AIC(lin151),
                                   BIC(lin151))),
            stringsAsFactors = FALSE)
        } else if (input$powpom == "op2") {
          data.frame(
            Parameter = c("R-squared",
                     "P-value",
                     "AIC",
                     "BIC"),
            Value = as.character(c(glance(qua151)$r.squared,
                                   glance(qua151)$p.value,
                                   AIC(qua151),
                                   BIC(qua151))),
            stringsAsFactors = FALSE)
        }
      }
    } 
      else if (input$reg1 == "Colombia") {
        if (input$ind == "ind1") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin211)$r.squared,
                                     glance(lin211)$p.value,
                                     AIC(lin211),
                                     BIC(lin211))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua211)$r.squared,
                                     glance(qua211)$p.value,
                                     AIC(qua211),
                                     BIC(qua211))),
              stringsAsFactors = FALSE)
          }
        } else if (input$ind == "ind2") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin221)$r.squared,
                                     glance(lin221)$p.value,
                                     AIC(lin221),
                                     BIC(lin221))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua221)$r.squared,
                                     glance(qua221)$p.value,
                                     AIC(qua221),
                                     BIC(qua221))),
              stringsAsFactors = FALSE)
          } 
        } else if (input$ind == "ind4") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin241)$r.squared,
                                     glance(lin241)$p.value,
                                     AIC(lin241),
                                     BIC(lin241))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua241)$r.squared,
                                     glance(qua241)$p.value,
                                     AIC(qua241),
                                     BIC(qua241))),
              stringsAsFactors = FALSE)
          } 
        } else if (input$ind == "ind5") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin251)$r.squared,
                                     glance(lin251)$p.value,
                                     AIC(lin251),
                                     BIC(lin251))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua251)$r.squared,
                                     glance(qua251)$p.value,
                                     AIC(qua251),
                                     BIC(qua251))),
              stringsAsFactors = FALSE)
          }
        }
      } 
      else if (input$reg1 == "Iran") {
        if (input$ind == "ind1") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin311)$r.squared,
                                     glance(lin311)$p.value,
                                     AIC(lin311),
                                     BIC(lin311))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua311)$r.squared,
                                     glance(qua311)$p.value,
                                     AIC(qua311),
                                     BIC(qua311))),
              stringsAsFactors = FALSE)
          }
        } else if (input$ind == "ind2") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin321)$r.squared,
                                     glance(lin321)$p.value,
                                     AIC(lin321),
                                     BIC(lin321))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua321)$r.squared,
                                     glance(qua321)$p.value,
                                     AIC(qua321),
                                     BIC(qua321))),
              stringsAsFactors = FALSE)
          } 
        } else if (input$ind == "ind4") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin341)$r.squared,
                                     glance(lin341)$p.value,
                                     AIC(lin341),
                                     BIC(lin341))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua341)$r.squared,
                                     glance(qua341)$p.value,
                                     AIC(qua341),
                                     BIC(qua341))),
              stringsAsFactors = FALSE)
          } 
        } else if (input$ind == "ind5") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin351)$r.squared,
                                     glance(lin351)$p.value,
                                     AIC(lin351),
                                     BIC(lin351))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua351)$r.squared,
                                     glance(qua351)$p.value,
                                     AIC(qua351),
                                     BIC(qua351))),
              stringsAsFactors = FALSE)
          } 
        }
      } 
      else if (input$reg1 == "Bolivia") {
        if (input$ind == "ind1") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin411)$r.squared,
                                     glance(lin411)$p.value,
                                     AIC(lin411),
                                     BIC(lin411))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua411)$r.squared,
                                     glance(qua411)$p.value,
                                     AIC(qua411),
                                     BIC(qua411))),
              stringsAsFactors = FALSE)
          } 
        } else if (input$ind == "ind2") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin421)$r.squared,
                                     glance(lin421)$p.value,
                                     AIC(lin421),
                                     BIC(lin421))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua421)$r.squared,
                                     glance(qua421)$p.value,
                                     AIC(qua421),
                                     BIC(qua421))),
              stringsAsFactors = FALSE)
          }
        } else if (input$ind == "ind4") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin441)$r.squared,
                                     glance(lin441)$p.value,
                                     AIC(lin441),
                                     BIC(lin441))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua441)$r.squared,
                                     glance(qua441)$p.value,
                                     AIC(qua441),
                                     BIC(qua441))),
              stringsAsFactors = FALSE)
          } 
        } else if (input$ind == "ind5") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin451)$r.squared,
                                     glance(lin451)$p.value,
                                     AIC(lin451),
                                     BIC(lin451))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua451)$r.squared,
                                     glance(qua451)$p.value,
                                     AIC(qua451),
                                     BIC(qua451))),
              stringsAsFactors = FALSE)
          }
        }
      } 
    }
    
    else if (input$endex == "tbz2") { if (input$reg1 == "Syria") {
      if (input$ind == "ind1") {
        if (input$powpom == "op1") {
          data.frame(
            Parameter = c("R-squared",
                     "P-value",
                     "AIC",
                     "BIC"),
            Value = as.character(c(glance(lin112)$r.squared,
                                   glance(lin112)$p.value,
                                   AIC(lin112),
                                   BIC(lin112))),
            stringsAsFactors = FALSE)
        } else if (input$powpom == "op2") {
          data.frame(
            Parameter = c("R-squared",
                     "P-value",
                     "AIC",
                     "BIC"),
            Value = as.character(c(glance(qua112)$r.squared,
                                   glance(qua112)$p.value,
                                   AIC(qua112),
                                   BIC(qua112))),
            stringsAsFactors = FALSE)
        } 
      } else if (input$ind == "ind2") {
        if (input$powpom == "op1") {
          data.frame(
            Parameter = c("R-squared",
                     "P-value",
                     "AIC",
                     "BIC"),
            Value = as.character(c(glance(lin122)$r.squared,
                                   glance(lin122)$p.value,
                                   AIC(lin122),
                                   BIC(lin122))),
            stringsAsFactors = FALSE)
        } else if (input$powpom == "op2") {
          data.frame(
            Parameter = c("R-squared",
                     "P-value",
                     "AIC",
                     "BIC"),
            Value = as.character(c(glance(qua122)$r.squared,
                                   glance(qua122)$p.value,
                                   AIC(qua122),
                                   BIC(qua122))),
            stringsAsFactors = FALSE)
        }
      } else if (input$ind == "ind4") {
        if (input$powpom == "op1") {
          data.frame(
            Parameter = c("R-squared",
                     "P-value",
                     "AIC",
                     "BIC"),
            Value = as.character(c(glance(lin142)$r.squared,
                                   glance(lin142)$p.value,
                                   AIC(lin142),
                                   BIC(lin142))),
            stringsAsFactors = FALSE)
        } else if (input$powpom == "op2") {
          data.frame(
            Parameter = c("R-squared",
                     "P-value",
                     "AIC",
                     "BIC"),
            Value = as.character(c(glance(qua142)$r.squared,
                                   glance(qua142)$p.value,
                                   AIC(qua142),
                                   BIC(qua142))),
            stringsAsFactors = FALSE)
        } 
      } else if (input$ind == "ind5") {
        if (input$powpom == "op1") {
          data.frame(
            Parameter = c("R-squared",
                     "P-value",
                     "AIC",
                     "BIC"),
            Value = as.character(c(glance(lin152)$r.squared,
                                   glance(lin152)$p.value,
                                   AIC(lin152),
                                   BIC(lin152))),
            stringsAsFactors = FALSE)
        } else if (input$powpom == "op2") {
          data.frame(
            Parameter = c("R-squared",
                     "P-value",
                     "AIC",
                     "BIC"),
            Value = as.character(c(glance(qua152)$r.squared,
                                   glance(qua152)$p.value,
                                   AIC(qua152),
                                   BIC(qua152))),
            stringsAsFactors = FALSE)
        } 
      }
    } 
      else if (input$reg1 == "Colombia") {
        if (input$ind == "ind1") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin212)$r.squared,
                                     glance(lin212)$p.value,
                                     AIC(lin212),
                                     BIC(lin212))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua212)$r.squared,
                                     glance(qua212)$p.value,
                                     AIC(qua212),
                                     BIC(qua212))),
              stringsAsFactors = FALSE)
          } 
        } else if (input$ind == "ind2") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin222)$r.squared,
                                     glance(lin222)$p.value,
                                     AIC(lin222),
                                     BIC(lin222))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua222)$r.squared,
                                     glance(qua222)$p.value,
                                     AIC(qua222),
                                     BIC(qua222))),
              stringsAsFactors = FALSE)
          } 
        } else if (input$ind == "ind4") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin242)$r.squared,
                                     glance(lin242)$p.value,
                                     AIC(lin242),
                                     BIC(lin242))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua242)$r.squared,
                                     glance(qua242)$p.value,
                                     AIC(qua242),
                                     BIC(qua242))),
              stringsAsFactors = FALSE)
          } 
        } else if (input$ind == "ind5") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin252)$r.squared,
                                     glance(lin252)$p.value,
                                     AIC(lin252),
                                     BIC(lin252))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua252)$r.squared,
                                     glance(qua252)$p.value,
                                     AIC(qua252),
                                     BIC(qua252))),
              stringsAsFactors = FALSE)
          } 
        }
      } 
      else if (input$reg1 == "Iran") {
        if (input$ind == "ind1") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin312)$r.squared,
                                     glance(lin312)$p.value,
                                     AIC(lin312),
                                     BIC(lin312))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua312)$r.squared,
                                     glance(qua312)$p.value,
                                     AIC(qua312),
                                     BIC(qua312))),
              stringsAsFactors = FALSE)
          } 
        } else if (input$ind == "ind2") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin322)$r.squared,
                                     glance(lin322)$p.value,
                                     AIC(lin322),
                                     BIC(lin322))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua322)$r.squared,
                                     glance(qua322)$p.value,
                                     AIC(qua322),
                                     BIC(qua322))),
              stringsAsFactors = FALSE)
          }
        } else if (input$ind == "ind4") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin342)$r.squared,
                                     glance(lin342)$p.value,
                                     AIC(lin342),
                                     BIC(lin342))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua342)$r.squared,
                                     glance(qua342)$p.value,
                                     AIC(qua342),
                                     BIC(qua342))),
              stringsAsFactors = FALSE)
          } 
        } else if (input$ind == "ind5") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin352)$r.squared,
                                     glance(lin352)$p.value,
                                     AIC(lin352),
                                     BIC(lin352))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua352)$r.squared,
                                     glance(qua352)$p.value,
                                     AIC(qua352),
                                     BIC(qua352))),
              stringsAsFactors = FALSE)
          } 
        }
      } 
      else if (input$reg1 == "Bolivia") {
        if (input$ind == "ind1") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin412)$r.squared,
                                     glance(lin412)$p.value,
                                     AIC(lin412),
                                     BIC(lin412))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua412)$r.squared,
                                     glance(qua412)$p.value,
                                     AIC(qua412),
                                     BIC(qua412))),
              stringsAsFactors = FALSE)
          } 
        } else if (input$ind == "ind2") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin422)$r.squared,
                                     glance(lin422)$p.value,
                                     AIC(lin422),
                                     BIC(lin422))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua422)$r.squared,
                                     glance(qua422)$p.value,
                                     AIC(qua422),
                                     BIC(qua422))),
              stringsAsFactors = FALSE)
          }
        } else if (input$ind == "ind4") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin442)$r.squared,
                                     glance(lin442)$p.value,
                                     AIC(lin442),
                                     BIC(lin442))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua442)$r.squared,
                                     glance(qua442)$p.value,
                                     AIC(qua442),
                                     BIC(qua442))),
              stringsAsFactors = FALSE)
          }
        } else if (input$ind == "ind5") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin452)$r.squared,
                                     glance(lin452)$p.value,
                                     AIC(lin452),
                                     BIC(lin452))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua452)$r.squared,
                                     glance(qua452)$p.value,
                                     AIC(qua452),
                                     BIC(qua452))),
              stringsAsFactors = FALSE)
          } 
        }
      } 
    }
    
    else if (input$endex == "tbz3") { if (input$reg1 == "Syria") {
      if (input$ind == "ind1") {
        if (input$powpom == "op1") {
          data.frame(
            Parameter = c("R-squared",
                     "P-value",
                     "AIC",
                     "BIC"),
            Value = as.character(c(glance(lin113)$r.squared,
                                   glance(lin113)$p.value,
                                   AIC(lin113),
                                   BIC(lin113))),
            stringsAsFactors = FALSE)
        } else if (input$powpom == "op2") {
          data.frame(
            Parameter = c("R-squared",
                     "P-value",
                     "AIC",
                     "BIC"),
            Value = as.character(c(glance(qua113)$r.squared,
                                   glance(qua113)$p.value,
                                   AIC(qua113),
                                   BIC(qua113))),
            stringsAsFactors = FALSE)
        } 
      } else if (input$ind == "ind2") {
        if (input$powpom == "op1") {
          data.frame(
            Parameter = c("R-squared",
                     "P-value",
                     "AIC",
                     "BIC"),
            Value = as.character(c(glance(lin123)$r.squared,
                                   glance(lin123)$p.value,
                                   AIC(lin123),
                                   BIC(lin123))),
            stringsAsFactors = FALSE)
        } else if (input$powpom == "op2") {
          data.frame(
            Parameter = c("R-squared",
                     "P-value",
                     "AIC",
                     "BIC"),
            Value = as.character(c(glance(qua123)$r.squared,
                                   glance(qua123)$p.value,
                                   AIC(qua123),
                                   BIC(qua123))),
            stringsAsFactors = FALSE)
        }
      } else if (input$ind == "ind4") {
        if (input$powpom == "op1") {
          data.frame(
            Parameter = c("R-squared",
                     "P-value",
                     "AIC",
                     "BIC"),
            Value = as.character(c(glance(lin143)$r.squared,
                                   glance(lin143)$p.value,
                                   AIC(lin143),
                                   BIC(lin143))),
            stringsAsFactors = FALSE)
        } else if (input$powpom == "op2") {
          data.frame(
            Parameter = c("R-squared",
                     "P-value",
                     "AIC",
                     "BIC"),
            Value = as.character(c(glance(qua143)$r.squared,
                                   glance(qua143)$p.value,
                                   AIC(qua143),
                                   BIC(qua143))),
            stringsAsFactors = FALSE)
        } 
      } else if (input$ind == "ind5") {
        if (input$powpom == "op1") {
          data.frame(
            Parameter = c("R-squared",
                     "P-value",
                     "AIC",
                     "BIC"),
            Value = as.character(c(glance(lin153)$r.squared,
                                   glance(lin153)$p.value,
                                   AIC(lin153),
                                   BIC(lin153))),
            stringsAsFactors = FALSE)
        } else if (input$powpom == "op2") {
          data.frame(
            Parameter = c("R-squared",
                     "P-value",
                     "AIC",
                     "BIC"),
            Value = as.character(c(glance(qua153)$r.squared,
                                   glance(qua153)$p.value,
                                   AIC(qua153),
                                   BIC(qua153))),
            stringsAsFactors = FALSE)
        } 
      }
    } 
      else if (input$reg1 == "Colombia") {
        if (input$ind == "ind1") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin213)$r.squared,
                                     glance(lin213)$p.value,
                                     AIC(lin213),
                                     BIC(lin213))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua213)$r.squared,
                                     glance(qua213)$p.value,
                                     AIC(qua213),
                                     BIC(qua213))),
              stringsAsFactors = FALSE)
          } 
        } else if (input$ind == "ind2") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin223)$r.squared,
                                     glance(lin223)$p.value,
                                     AIC(lin223),
                                     BIC(lin223))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua223)$r.squared,
                                     glance(qua223)$p.value,
                                     AIC(qua223),
                                     BIC(qua223))),
              stringsAsFactors = FALSE)
          } 
        } else if (input$ind == "ind4") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin243)$r.squared,
                                     glance(lin243)$p.value,
                                     AIC(lin243),
                                     BIC(lin243))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua243)$r.squared,
                                     glance(qua243)$p.value,
                                     AIC(qua243),
                                     BIC(qua243))),
              stringsAsFactors = FALSE)
          } 
        } else if (input$ind == "ind5") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin253)$r.squared,
                                     glance(lin253)$p.value,
                                     AIC(lin253),
                                     BIC(lin253))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua253)$r.squared,
                                     glance(qua253)$p.value,
                                     AIC(qua253),
                                     BIC(qua253))),
              stringsAsFactors = FALSE)
          }
        }
      } 
      else if (input$reg1 == "Iran") {
        if (input$ind == "ind1") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin313)$r.squared,
                                     glance(lin313)$p.value,
                                     AIC(lin313),
                                     BIC(lin313))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua313)$r.squared,
                                     glance(qua313)$p.value,
                                     AIC(qua313),
                                     BIC(qua313))),
              stringsAsFactors = FALSE)
          } 
        } else if (input$ind == "ind2") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin323)$r.squared,
                                     glance(lin323)$p.value,
                                     AIC(lin323),
                                     BIC(lin323))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua323)$r.squared,
                                     glance(qua323)$p.value,
                                     AIC(qua323),
                                     BIC(qua323))),
              stringsAsFactors = FALSE)
          }
        } else if (input$ind == "ind4") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin343)$r.squared,
                                     glance(lin343)$p.value,
                                     AIC(lin343),
                                     BIC(lin343))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua343)$r.squared,
                                     glance(qua343)$p.value,
                                     AIC(qua343),
                                     BIC(qua343))),
              stringsAsFactors = FALSE)
          }
        } else if (input$ind == "ind5") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin353)$r.squared,
                                     glance(lin353)$p.value,
                                     AIC(lin353),
                                     BIC(lin353))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua353)$r.squared,
                                     glance(qua353)$p.value,
                                     AIC(qua353),
                                     BIC(qua353))),
              stringsAsFactors = FALSE)
          }
        }
      } 
      else if (input$reg1 == "Bolivia") {
        if (input$ind == "ind1") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin413)$r.squared,
                                     glance(lin413)$p.value,
                                     AIC(lin413),
                                     BIC(lin413))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua413)$r.squared,
                                     glance(qua413)$p.value,
                                     AIC(qua413),
                                     BIC(qua413))),
              stringsAsFactors = FALSE)
          }
        } else if (input$ind == "ind2") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin423)$r.squared,
                                     glance(lin423)$p.value,
                                     AIC(lin423),
                                     BIC(lin423))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua423)$r.squared,
                                     glance(qua423)$p.value,
                                     AIC(qua423),
                                     BIC(qua423))),
              stringsAsFactors = FALSE)
          }
        } else if (input$ind == "ind4") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin443)$r.squared,
                                     glance(lin443)$p.value,
                                     AIC(lin443),
                                     BIC(lin443))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua443)$r.squared,
                                     glance(qua443)$p.value,
                                     AIC(qua443),
                                     BIC(qua443))),
              stringsAsFactors = FALSE)
          }
        } else if (input$ind == "ind5") {
          if (input$powpom == "op1") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(lin453)$r.squared,
                                     glance(lin453)$p.value,
                                     AIC(lin453),
                                     BIC(lin453))),
              stringsAsFactors = FALSE)
          } else if (input$powpom == "op2") {
            data.frame(
              Parameter = c("R-squared",
                       "P-value",
                       "AIC",
                       "BIC"),
              Value = as.character(c(glance(qua453)$r.squared,
                                     glance(qua453)$p.value,
                                     AIC(qua453),
                                     BIC(qua453))),
              stringsAsFactors = FALSE)
          } 
        }
      } 
    }
    )
  } 

  # Comparison with indices
  { 
  output$comparison <- renderPlot(if (input$endex == "tbz1" ) {
    if (input$reg1 == "Syria") {
      if (input$ind == "ind1") {
        if (input$powpom == "op1") {
          plot(c1d1$SPEI_12, c1d1$Value, xlab="SPEI (year value)", ylab="Agriculture production relative to 2014-2016 mean (%)")
          abline(lin111, col = "red")
        } else if (input$powpom == "op2") {
          plot(c1d1$SPEI_12, c1d1$Value, xlab="SPEI (year value)", ylab="Agriculture production relative to 2014-2016 mean (%)")
          y <- predict(qua111, newdata = data.frame(x = c1d1$SPEI_12))
          lines(sort(c1d1$SPEI_12), y[order(c1d1$SPEI_12)], col = "green")
        }
      } else if (input$ind == "ind2") {
        if (input$powpom == "op1") {
          plot(c1d2$SPEI_12, c1d2$Value, xlab="SPEI (year value)", ylab="Crop production relative to 2014-2016 mean (%)")
          abline(lin121, col = "red")
        } else if (input$powpom == "op2") {
          plot(c1d2$SPEI_12, c1d2$Value, xlab="SPEI (year value)", ylab="Crop production relative to 2014-2016 mean (%)")
          y <- predict(qua121, newdata = data.frame(x = c1d2$SPEI_12))
          lines(sort(c1d2$SPEI_12), y[order(c1d2$SPEI_12)], col = "green")
        }
      } else if (input$ind == "ind4") {
        if (input$powpom == "op1") {
          plot(c1d4$SPEI_12, c1d4$Value, xlab="SPEI (year value)", ylab="Total food production relative to 2014-2016 mean (%)" )
          abline(lin141, col = "red")
        } else if (input$powpom == "op2") {
          plot(c1d4$SPEI_12, c1d4$Value, xlab="SPEI (year value)", ylab="Total food production relative to 2014-2016 mean (%)" )
          y <- predict(qua141, newdata = data.frame(x = c1d4$SPEI_12))
          lines(sort(c1d4$SPEI_12), y[order(c1d4$SPEI_12)], col = "green")
        }
      } else if (input$ind == "ind5") {
        if (input$powpom == "op1") {
          plot(c1d5$SPEI_12, c1d5$Value, xlab="SPEI (year value)", ylab="Livestock relative to 2014-2016 mean (%)")
          abline(lin151, col = "red")
        } else if (input$powpom == "op2") {
          plot(c1d5$SPEI_12, c1d5$Value, xlab="SPEI (year value)", ylab="Livestock relative to 2014-2016 mean (%)")
          y <- predict(qua151, newdata = data.frame(x = c1d5$SPEI_12))
          lines(sort(c1d5$SPEI_12), y[order(c1d5$SPEI_12)], col = "green")
        }
      }
    } else if (input$reg1 == "Colombia") {
      if (input$ind == "ind1") {
        if (input$powpom == "op1") {
          plot(c2d1$SPEI_12, c2d1$Value, xlab="SPEI (year value)", ylab="Agriculture production relative to 2014-2016 mean (%)")
          abline(lin211, col = "red")
        } else if (input$powpom == "op2") {
          plot(c2d1$SPEI_12, c2d1$Value, xlab="SPEI (year value)", ylab="Agriculture production relative to 2014-2016 mean (%)")
          y <- predict(qua211, newdata = data.frame(x = c2d1$SPEI_12))
          lines(sort(c2d1$SPEI_12), y[order(c2d1$SPEI_12)], col = "green")
        }
      } else if (input$ind == "ind2") {
        if (input$powpom == "op1") {
          plot(c2d2$SPEI_12, c2d2$Value, xlab="SPEI (year value)", ylab="Crop production relative to 2014-2016 mean (%)")
          abline(lin221, col = "red")
        } else if (input$powpom == "op2") {
          plot(c2d2$SPEI_12, c2d2$Value, xlab="SPEI (year value)", ylab="Crop production relative to 2014-2016 mean (%)")
          y <- predict(qua221, newdata = data.frame(x = c2d2$SPEI_12))
          lines(sort(c2d2$SPEI_12), y[order(c2d2$SPEI_12)], col = "green")
        }
      } else if (input$ind == "ind4") {
        if (input$powpom == "op1") {
          plot(c2d4$SPEI_12, c2d4$Value, xlab="SPEI (year value)", ylab="Total food production relative to 2014-2016 mean (%)" )
          abline(lin241, col = "red")
        } else if (input$powpom == "op2") {
          plot(c2d4$SPEI_12, c2d4$Value, xlab="SPEI (year value)", ylab="Total food production relative to 2014-2016 mean (%)" )
          y <- predict(qua241, newdata = data.frame(x = c2d4$SPEI_12))
          lines(sort(c2d4$SPEI_12), y[order(c2d4$SPEI_12)], col = "green")
        }
      } else if (input$ind == "ind5") {
        if (input$powpom == "op1") {
          plot(c2d5$SPEI_12, c2d5$Value, xlab="SPEI (year value)", ylab="Livestock relative to 2014-2016 mean (%)")
          abline(lin251, col = "red")
        } else if (input$powpom == "op2") {
          plot(c2d5$SPEI_12, c2d5$Value, xlab="SPEI (year value)", ylab="Livestock relative to 2014-2016 mean (%)")
          y <- predict(qua251, newdata = data.frame(x = c2d5$SPEI_12))
          lines(sort(c2d5$SPEI_12), y[order(c2d5$SPEI_12)], col = "green")
        }
      }
    } else if (input$reg1 == "Iran") {
      if (input$ind == "ind1") {
        if (input$powpom == "op1") {
          plot(c3d1$SPEI_12, c3d1$Value, xlab="SPEI (year value)", ylab="Agriculture production relative to 2014-2016 mean (%)")
          abline(lin311, col = "red")
        } else if (input$powpom == "op2") {
          plot(c3d1$SPEI_12, c3d1$Value, xlab="SPEI (year value)", ylab="Agriculture production relative to 2014-2016 mean (%)")
          y <- predict(qua311, newdata = data.frame(x = c3d1$SPEI_12))
          lines(sort(c3d1$SPEI_12), y[order(c3d1$SPEI_12)], col = "green")
        }
      } else if (input$ind == "ind2") {
        if (input$powpom == "op1") {
          plot(c3d2$SPEI_12, c3d2$Value, xlab="SPEI (year value)", ylab="Crop production relative to 2014-2016 mean (%)")
          abline(lin321, col = "red")
        } else if (input$powpom == "op2") {
          plot(c3d2$SPEI_12, c3d2$Value, xlab="SPEI (year value)", ylab="Crop production relative to 2014-2016 mean (%)")
          y <- predict(qua321, newdata = data.frame(x = c3d2$SPEI_12))
          lines(sort(c3d2$SPEI_12), y[order(c3d2$SPEI_12)], col = "green")
        }
      } else if (input$ind == "ind4") {
        if (input$powpom == "op1") {
          plot(c3d4$SPEI_12, c3d4$Value, xlab="SPEI (year value)", ylab="Total food production relative to 2014-2016 mean (%)" )
          abline(lin341, col = "red")
        } else if (input$powpom == "op2") {
          plot(c3d4$SPEI_12, c3d4$Value, xlab="SPEI (year value)", ylab="Total food production relative to 2014-2016 mean (%)" )
          y <- predict(qua341, newdata = data.frame(x = c3d4$SPEI_12))
          lines(sort(c3d4$SPEI_12), y[order(c3d4$SPEI_12)], col = "green")
        }
      } else if (input$ind == "ind5") {
        if (input$powpom == "op1") {
          plot(c3d5$SPEI_12, c3d5$Value, xlab="SPEI (year value)", ylab="Livestock relative to 2014-2016 mean (%)")
          abline(lin351, col = "red")
        } else if (input$powpom == "op2") {
          plot(c3d5$SPEI_12, c3d5$Value, xlab="SPEI (year value)", ylab="Livestock relative to 2014-2016 mean (%)")
          y <- predict(qua351, newdata = data.frame(x = c3d5$SPEI_12))
          lines(sort(c3d5$SPEI_12), y[order(c3d5$SPEI_12)], col = "green")
        }
      }
    } else if (input$reg1 == "Bolivia") {
      if (input$ind == "ind1") {
        if (input$powpom == "op1") {
          plot(c4d1$SPEI_12, c4d1$Value, xlab="SPEI (year value)", ylab="Agriculture production relative to 2014-2016 mean (%)")
          abline(lin411, col = "red")
        } else if (input$powpom == "op2") {
          plot(c4d1$SPEI_12, c4d1$Value, xlab="SPEI (year value)", ylab="Agriculture production relative to 2014-2016 mean (%)")
          y <- predict(qua411, newdata = data.frame(x = c4d1$SPEI_12))
          lines(sort(c4d1$SPEI_12), y[order(c4d1$SPEI_12)], col = "green")
        }
      } else if (input$ind == "ind2") {
        if (input$powpom == "op1") {
          plot(c4d2$SPEI_12, c4d2$Value, xlab="SPEI (year value)", ylab="Crop production relative to 2014-2016 mean (%)")
          abline(lin421, col = "red")
        } else if (input$powpom == "op2") {
          plot(c4d2$SPEI_12, c4d2$Value, xlab="SPEI (year value)", ylab="Crop production relative to 2014-2016 mean (%)")
          y <- predict(qua421, newdata = data.frame(x = c4d2$SPEI_12))
          lines(sort(c4d2$SPEI_12), y[order(c4d2$SPEI_12)], col = "green")
        }
      } else if (input$ind == "ind4") {
        if (input$powpom == "op1") {
          plot(c4d4$SPEI_12, c4d4$Value, xlab="SPEI (year value)", ylab="Total food production relative to 2014-2016 mean (%)" )
          abline(lin441, col = "red")
        } else if (input$powpom == "op2") {
          plot(c4d4$SPEI_12, c4d4$Value, xlab="SPEI (year value)", ylab="Total food production relative to 2014-2016 mean (%)" )
          y <- predict(qua441, newdata = data.frame(x = c4d4$SPEI_12))
          lines(sort(c4d4$SPEI_12), y[order(c4d4$SPEI_12)], col = "green")
        }
      } else if (input$ind == "ind5") {
        if (input$powpom == "op1") {
          plot(c4d5$SPEI_12, c4d5$Value, xlab="SPEI (year value)", ylab="Livestock relative to 2014-2016 mean (%)")
          abline(lin451, col = "red")
        } else if (input$powpom == "op2") {
          plot(c4d5$SPEI_12, c4d5$Value, xlab="SPEI (year value)", ylab="Livestock relative to 2014-2016 mean (%)")
          y <- predict(qua451, newdata = data.frame(x = c4d5$SPEI_12))
          lines(sort(c4d5$SPEI_12), y[order(c4d5$SPEI_12)], col = "green")
        }
      }
    }
  } else if (input$endex == "tbz2" ) {
    if (input$reg1 == "Syria") {
      if (input$ind == "ind1") {
        if (input$powpom == "op1") {
          plot(c1p1$Value.y, c1p1$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Agriculture production relative to 2014-2016 mean (%)")
          abline(lin112, col = "red")
        } else if (input$powpom == "op2") {
          plot(c1p1$Value.y, c1p1$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Crop production relative to 2014-2016 mean (%)")
          y <- predict(qua112, newdata = data.frame(x = c1p1$Value.y))
          lines(sort(c1p1$Value.y), y[order(c1p1$Value.y)], col = "green")
        }
      } else if (input$ind == "ind2") {
        if (input$powpom == "op1") {
          plot(c1p2$Value.y, c1p2$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Crop production relative to 2014-2016 mean (%)")
          abline(lin122, col = "red")
        } else if (input$powpom == "op2") {
          plot(c1p2$Value.y, c1p2$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Crop production relative to 2014-2016 mean (%)")
          y <- predict(qua122, newdata = data.frame(x = c1p2$Value.y))
          lines(sort(c1p2$Value.y), y[order(c1p2$Value.y)], col = "green")
        }
      } else if (input$ind == "ind4") {
        if (input$powpom == "op1") {
          plot(c1p4$Value.y, c1p4$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Total food production relative to 2014-2016 mean (%)" )
          abline(lin142, col = "red")
        } else if (input$powpom == "op2") {
          plot(c1p4$Value.y, c1p4$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Total food production relative to 2014-2016 mean (%)" )
          y <- predict(qua142, newdata = data.frame(x = c1p4$Value.y))
          lines(sort(c1p4$Value.y), y[order(c1p4$Value.y)], col = "green")
        }
      } else if (input$ind == "ind5") {
        if (input$powpom == "op1") {
          plot(c1p5$Value.y, c1p5$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Livestock relative to 2014-2016 mean (%)")
          abline(lin152, col = "red")
        } else if (input$powpom == "op2") {
          plot(c1p5$Value.y, c1p5$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Livestock relative to 2014-2016 mean (%)")
          y <- predict(qua152, newdata = data.frame(x = c1p5$Value.y))
          lines(sort(c1p5$Value.y), y[order(c1p5$Value.y)], col = "green")
        }
      }
    } else if (input$reg1 == "Colombia") {
      if (input$ind == "ind1") {
        if (input$powpom == "op1") {
          plot(c2p1$Value.y, c2p1$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Agriculture production relative to 2014-2016 mean (%)")
          abline(lin212, col = "red")
        } else if (input$powpom == "op2") {
          plot(c2p1$Value.y, c2p1$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Agriculture production relative to 2014-2016 mean (%)")
          y <- predict(qua212, newdata = data.frame(x = c2p1$Value.y))
          lines(sort(c2p1$Value.y), y[order(c2p1$Value.y)], col = "green")
        }
      } else if (input$ind == "ind2") {
        if (input$powpom == "op1") {
          plot(c2p2$Value.y, c2p2$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Crop production relative to 2014-2016 mean (%)")
          abline(lin222, col = "red")
        } else if (input$powpom == "op2") {
          plot(c2p2$Value.y, c2p2$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Crop production relative to 2014-2016 mean (%)")
          y <- predict(qua222, newdata = data.frame(x = c2p2$Value.y))
          lines(sort(c2p2$Value.y), y[order(c2p2$Value.y)], col = "green")
        }
      } else if (input$ind == "ind4") {
        if (input$powpom == "op1") {
          plot(c2p4$Value.y, c2p4$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Total food production relative to 2014-2016 mean (%)" )
          abline(lin242, col = "red")
        } else if (input$powpom == "op2") {
          plot(c2p4$Value.y, c2p4$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Total food production relative to 2014-2016 mean (%)" )
          y <- predict(qua242, newdata = data.frame(x = c2p4$Value.y))
          lines(sort(c2p4$Value.y), y[order(c2p4$Value.y)], col = "green")
        }
      } else if (input$ind == "ind5") {
        if (input$powpom == "op1") {
          plot(c2p5$Value.y, c2p5$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Livestock relative to 2014-2016 mean (%)")
          abline(lin252, col = "red")
        } else if (input$powpom == "op2") {
          plot(c2p5$Value.y, c2p5$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Livestock relative to 2014-2016 mean (%)")
          y <- predict(qua252, newdata = data.frame(x = c2p5$Value.y))
          lines(sort(c2p5$Value.y), y[order(c2p5$Value.y)], col = "green")
        }
      }
    } else if (input$reg1 == "Iran") {
      if (input$ind == "ind1") {
        if (input$powpom == "op1") {
          plot(c3p1$Value.y, c3p1$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Agriculture production relative to 2014-2016 mean (%)")
          abline(lin312, col = "red")
        } else if (input$powpom == "op2") {
          plot(c3p1$Value.y, c3p1$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Agriculture production relative to 2014-2016 mean (%)")
          y <- predict(qua312, newdata = data.frame(x = c3p1$Value.y))
          lines(sort(c3p1$Value.y), y[order(c3p1$Value.y)], col = "green")
        }
      } else if (input$ind == "ind2") {
        if (input$powpom == "op1") {
          plot(c3p2$Value.y, c3p2$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Crop production relative to 2014-2016 mean (%)")
          abline(lin322, col = "red")
        } else if (input$powpom == "op2") {
          plot(c3p2$Value.y, c3p2$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Crop production relative to 2014-2016 mean (%)")
          y <- predict(qua322, newdata = data.frame(x = c3p2$Value.y))
          lines(sort(c3p2$Value.y), y[order(c3p2$Value.y)], col = "green")
        }
      } else if (input$ind == "ind4") {
        if (input$powpom == "op1") {
          plot(c3p4$Value.y, c3p4$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Total food production relative to 2014-2016 mean (%)" )
          abline(lin342, col = "red")
        } else if (input$powpom == "op2") {
          plot(c3p4$Value.y, c3p4$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Total food production relative to 2014-2016 mean (%)" )
          y <- predict(qua342, newdata = data.frame(x = c3p4$Value.y))
          lines(sort(c3p4$Value.y), y[order(c3p4$Value.y)], col = "green")
        }
      } else if (input$ind == "ind5") {
        if (input$powpom == "op1") {
          plot(c3p5$Value.y, c3p5$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Livestock relative to 2014-2016 mean (%)")
          abline(lin352, col = "red")
        } else if (input$powpom == "op2") {
          plot(c3p5$Value.y, c3p5$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Livestock relative to 2014-2016 mean (%)")
          y <- predict(qua352, newdata = data.frame(x = c3p5$Value.y))
          lines(sort(c3p5$Value.y), y[order(c3p5$Value.y)], col = "green")
        }
      }
    } else if (input$reg1 == "Bolivia") {
      if (input$ind == "ind1") {
        if (input$powpom == "op1") {
          plot(c4p1$Value.y, c4p1$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Agriculture production relative to 2014-2016 mean (%)")
          abline(lin412, col = "red")
        } else if (input$powpom == "op2") {
          plot(c4p1$Value.y, c4p1$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Agriculture production relative to 2014-2016 mean (%)")
          y <- predict(qua412, newdata = data.frame(x = c4p1$Value.y))
          lines(sort(c4p1$Value.y), y[order(c4p1$Value.y)], col = "green")
        }
      } else if (input$ind == "ind2") {
        if (input$powpom == "op1") {
          plot(c4p2$Value.y, c4p2$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Crop production relative to 2014-2016 mean (%)")
          abline(lin422, col = "red")
        } else if (input$powpom == "op2") {
          plot(c4p2$Value.y, c4p2$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Crop production relative to 2014-2016 mean (%)")
          y <- predict(qua422, newdata = data.frame(x = c4p2$Value.y))
          lines(sort(c4p2$Value.y), y[order(c4p2$Value.y)], col = "green")
        }
      } else if (input$ind == "ind4") {
        if (input$powpom == "op1") {
          plot(c4p4$Value.y, c4p4$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Total food production relative to 2014-2016 mean (%)" )
          abline(lin442, col = "red")
        } else if (input$powpom == "op2") {
          plot(c4p4$Value.y, c4p4$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Total food production relative to 2014-2016 mean (%)" )
          y <- predict(qua442, newdata = data.frame(x = c4p4$Value.y))
          lines(sort(c4p4$Value.y), y[order(c4p4$Value.y)], col = "green")
        }
      } else if (input$ind == "ind5") {
        if (input$powpom == "op1") {
          plot(c4p5$Value.y, c4p5$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Livestock relative to 2014-2016 mean (%)")
          abline(lin452, col = "red")
        } else if (input$powpom == "op2") {
          plot(c4p5$Value.y, c4p5$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Livestock relative to 2014-2016 mean (%)")
          y <- predict(qua452, newdata = data.frame(x = c4p5$Value.y))
          lines(sort(c4p5$Value.y), y[order(c4p5$Value.y)], col = "green")
        }
      }
    }
  } else if (input$endex == "tbz3" ) {
    if (input$reg1 == "Syria") {
      if (input$ind == "ind1") {
        if (input$powpom == "op1") {
          plot(c1t1$Value.y, c1t1$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Agriculture production relative to 2014-2016 mean (%)")
          abline(lin113, col = "red")
        } else if (input$powpom == "op2") {
          plot(c1t1$Value.y, c1t1$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Agriculture production relative to 2014-2016 mean (%)")
          y <- predict(qua113, newdata = data.frame(x = c1t1$Value.y))
          lines(sort(c1t1$Value.y), y[order(c1t1$Value.y)], col = "green")
        }
      } else if (input$ind == "ind2") {
        if (input$powpom == "op1") {
          plot(c1t2$Value.y, c1t2$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Crop production relative to 2014-2016 mean (%)")
          abline(lin123, col = "red")
        } else if (input$powpom == "op2") {
          plot(c1t2$Value.y, c1t2$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Crop production relative to 2014-2016 mean (%)")
          y <- predict(qua123, newdata = data.frame(x = c1t2$Value.y))
          lines(sort(c1t2$Value.y), y[order(c1t2$Value.y)], col = "green")
        }
      } else if (input$ind == "ind4") {
        if (input$powpom == "op1") {
          plot(c1t4$Value.y, c1t4$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Total food production relative to 2014-2016 mean (%)" )
          abline(lin143, col = "red")
        } else if (input$powpom == "op2") {
          plot(c1t4$Value.y, c1t4$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Total food production relative to 2014-2016 mean (%)" )
          y <- predict(qua143, newdata = data.frame(x = c1t4$Value.y))
          lines(sort(c1t4$Value.y), y[order(c1t4$Value.y)], col = "green")
        }
      } else if (input$ind == "ind5") {
        if (input$powpom == "op1") {
          plot(c1t5$Value.y, c1t5$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Livestock relative to 2014-2016 mean (%)")
          abline(lin153, col = "red")
        } else if (input$powpom == "op2") {
          plot(c1t5$Value.y, c1t5$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Livestock relative to 2014-2016 mean (%)")
          y <- predict(qua153, newdata = data.frame(x = c1t5$Value.y))
          lines(sort(c1t5$Value.y), y[order(c1t5$Value.y)], col = "green")
        }
      }
    } else if (input$reg1 == "Colombia") {
      if (input$ind == "ind1") {
        if (input$powpom == "op1") {
          plot(c2t1$Value.y, c2t1$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Agriculture production relative to 2014-2016 mean (%)")
          abline(lin213, col = "red")
        } else if (input$powpom == "op2") {
          plot(c2t1$Value.y, c2t1$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Agriculture production relative to 2014-2016 mean (%)")
          y <- predict(qua213, newdata = data.frame(x = c2t1$Value.y))
          lines(sort(c2t1$Value.y), y[order(c2t1$Value.y)], col = "green")
        }
      } else if (input$ind == "ind2") {
        if (input$powpom == "op1") {
          plot(c2t2$Value.y, c2t2$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Crop production relative to 2014-2016 mean (%)")
          abline(lin223, col = "red")
        } else if (input$powpom == "op2") {
          plot(c2t2$Value.y, c2t2$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Crop production relative to 2014-2016 mean (%)")
          y <- predict(qua223, newdata = data.frame(x = c2t2$Value.y))
          lines(sort(c2t2$Value.y), y[order(c2t2$Value.y)], col = "green")
        }
      } else if (input$ind == "ind4") {
        if (input$powpom == "op1") {
          plot(c2t4$Value.y, c2t4$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Total food production relative to 2014-2016 mean (%)" )
          abline(lin243, col = "red")
        } else if (input$powpom == "op2") {
          plot(c2t4$Value.y, c2t4$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Total food production relative to 2014-2016 mean (%)" )
          y <- predict(qua243, newdata = data.frame(x = c2t4$Value.y))
          lines(sort(c2t4$Value.y), y[order(c2t4$Value.y)], col = "green")
        }
      } else if (input$ind == "ind5") {
        if (input$powpom == "op1") {
          plot(c2t5$Value.y, c2t5$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Livestock relative to 2014-2016 mean (%)")
          abline(lin253, col = "red")
        } else if (input$powpom == "op2") {
          plot(c2t5$Value.y, c2t5$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Livestock relative to 2014-2016 mean (%)")
          y <- predict(qua253, newdata = data.frame(x = c2t5$Value.y))
          lines(sort(c2t5$Value.y), y[order(c2t5$Value.y)], col = "green")
        }
      }
    } else if (input$reg1 == "Iran") {
      if (input$ind == "ind1") {
        if (input$powpom == "op1") {
          plot(c3t1$Value.y, c3t1$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Agriculture production relative to 2014-2016 mean (%)")
          abline(lin313, col = "red")
        } else if (input$powpom == "op2") {
          plot(c3t1$Value.y, c3t1$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Agriculture production relative to 2014-2016 mean (%)")
          y <- predict(qua313, newdata = data.frame(x = c3t1$Value.y))
          lines(sort(c3t1$Value.y), y[order(c3t1$Value.y)], col = "green")
        }
      } else if (input$ind == "ind2") {
        if (input$powpom == "op1") {
          plot(c3t2$Value.y, c3t2$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Crop production relative to 2014-2016 mean (%)")
          abline(lin323, col = "red")
        } else if (input$powpom == "op2") {
          plot(c3t2$Value.y, c3t2$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Crop production relative to 2014-2016 mean (%)")
          y <- predict(qua323, newdata = data.frame(x = c3t2$Value.y))
          lines(sort(c3t2$Value.y), y[order(c3t2$Value.y)], col = "green")
        }
      } else if (input$ind == "ind4") {
        if (input$powpom == "op1") {
          plot(c3t4$Value.y, c3t4$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Total food production relative to 2014-2016 mean (%)" )
          abline(lin343, col = "red")
        } else if (input$powpom == "op2") {
          plot(c3t4$Value.y, c3t4$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Total food production relative to 2014-2016 mean (%)" )
          y <- predict(qua343, newdata = data.frame(x = c3t4$Value.y))
          lines(sort(c3t4$Value.y), y[order(c3t4$Value.y)], col = "green")
        }
      } else if (input$ind == "ind5") {
        if (input$powpom == "op1") {
          plot(c3t5$Value.y, c3t5$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Livestock relative to 2014-2016 mean (%)")
          abline(lin353, col = "red")
        } else if (input$powpom == "op2") {
          plot(c3t5$Value.y, c3t5$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Livestock relative to 2014-2016 mean (%)")
          y <- predict(qua353, newdata = data.frame(x = c3t5$Value.y))
          lines(sort(c3t5$Value.y), y[order(c3t5$Value.y)], col = "green")
        }
      }
    } else if (input$reg1 == "Bolivia") {
      if (input$ind == "ind1") {
        if (input$powpom == "op1") {
          plot(c4t1$Value.y, c4t1$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Agriculture production relative to 2014-2016 mean (%)")
          abline(lin413, col = "red")
        } else if (input$powpom == "op2") {
          plot(c4t1$Value.y, c4t1$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Agriculture production relative to 2014-2016 mean (%)")
          y <- predict(qua413, newdata = data.frame(x = c4t1$Value.y))
          lines(sort(c4t1$Value.y), y[order(c4t1$Value.y)], col = "green")
        }
      } else if (input$ind == "ind2") {
        if (input$powpom == "op1") {
          plot(c4t2$Value.y, c4t2$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Crop production relative to 2014-2016 mean (%)")
          abline(lin423, col = "red")
        } else if (input$powpom == "op2") {
          plot(c4t2$Value.y, c4t2$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Crop production relative to 2014-2016 mean (%)")
          y <- predict(qua423, newdata = data.frame(x = c4t2$Value.y))
          lines(sort(c4t2$Value.y), y[order(c4t2$Value.y)], col = "green")
        }
      } else if (input$ind == "ind4") {
        if (input$powpom == "op1") {
          plot(c4t4$Value.y, c4t4$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Total food production relative to 2014-2016 mean (%)" )
          abline(lin443, col = "red")
        } else if (input$powpom == "op2") {
          plot(c4t4$Value.y, c4t4$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Total food production relative to 2014-2016 mean (%)" )
          y <- predict(qua443, newdata = data.frame(x = c4t4$Value.y))
          lines(sort(c4t4$Value.y), y[order(c4t4$Value.y)], col = "green")
        }
      } else if (input$ind == "ind5") {
        if (input$powpom == "op1") {
          plot(c4t5$Value.y, c4t5$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Livestock relative to 2014-2016 mean (%)")
          abline(lin453, col = "red")
        } else if (input$powpom == "op2") {
          plot(c4t5$Value.y, c4t5$Value.x, xlab="Mean temperature change (from 1951-1980 baseline)", ylab="Livestock relative to 2014-2016 mean (%)")
          y <- predict(qua453, newdata = data.frame(x = c4t5$Value.y))
          lines(sort(c4t5$Value.y), y[order(c4t5$Value.y)], col = "green")
        }
      }
    }
  } 
  )
}
  
  # Table
  {
    
    {
      t1 = data.frame(
        Relationship_between_indices = c("Total agricultural production vs SPEI (annual)",
                                         "Crop production vs SPEI (annual)",
                                         "Livestock production vs SPEI (annual)",
                                         "Total food production vs SPEI (annual)",
                                         "Total agricultural production vs PM2.5 (mean concentration)",
                                         "Crop production vs PM2.5 (mean concentration)",
                                         "Livestock production vs PM2.5 (mean concentration)",
                                         "Total food production vs PM2.5 (mean concentration)",
                                         "Total agricultural production vs average temperature change",
                                         "Crop production vs average temperature change",
                                         "Livestock production vs average temperature change",
                                         "Total food production vs average temperature change"),
        LM_R2_adjusted = c(
          glance(lin111)$adj.r.squared,
          glance(lin121)$adj.r.squared,
          glance(lin141)$adj.r.squared,
          glance(lin151)$adj.r.squared,
          glance(lin112)$adj.r.squared,
          glance(lin122)$adj.r.squared,
          glance(lin142)$adj.r.squared,
          glance(lin152)$adj.r.squared,
          glance(lin113)$adj.r.squared,
          glance(lin123)$adj.r.squared,
          glance(lin143)$adj.r.squared,
          glance(lin153)$adj.r.squared
        ),
        LM_P_value = c(
          glance(lin111)$p.value,
          glance(lin121)$p.value,
          glance(lin141)$p.value,
          glance(lin151)$p.value,
          glance(lin112)$p.value,
          glance(lin122)$p.value,
          glance(lin142)$p.value,
          glance(lin152)$p.value,
          glance(lin113)$p.value,
          glance(lin123)$p.value,
          glance(lin143)$p.value,
          glance(lin153)$p.value
        ),
        LM_AIC = c(
          glance(lin111)$AIC,
          glance(lin121)$AIC,
          glance(lin141)$AIC,
          glance(lin151)$AIC,
          glance(lin112)$AIC,
          glance(lin122)$AIC,
          glance(lin142)$AIC,
          glance(lin152)$AIC,
          glance(lin113)$AIC,
          glance(lin123)$AIC,
          glance(lin143)$AIC,
          glance(lin153)$AIC
        ),
        LM_BIC = c(
          glance(lin111)$BIC,
          glance(lin121)$BIC,
          glance(lin141)$BIC,
          glance(lin151)$BIC,
          glance(lin112)$BIC,
          glance(lin122)$BIC,
          glance(lin142)$BIC,
          glance(lin152)$BIC,
          glance(lin113)$BIC,
          glance(lin123)$BIC,
          glance(lin143)$BIC,
          glance(lin153)$BIC
        ),
        LM_standard_error = c(
          glance(lin111)$sigma,
          glance(lin121)$sigma,
          glance(lin141)$sigma,
          glance(lin151)$sigma,
          glance(lin112)$sigma,
          glance(lin122)$sigma,
          glance(lin142)$sigma,
          glance(lin152)$sigma,
          glance(lin113)$sigma,
          glance(lin123)$sigma,
          glance(lin143)$sigma,
          glance(lin153)$sigma
        ),
        LM_deviance = c(
          glance(lin111)$deviance,
          glance(lin121)$deviance,
          glance(lin141)$deviance,
          glance(lin151)$deviance,
          glance(lin112)$deviance,
          glance(lin122)$deviance,
          glance(lin142)$deviance,
          glance(lin152)$deviance,
          glance(lin113)$deviance,
          glance(lin123)$deviance,
          glance(lin143)$deviance,
          glance(lin153)$deviance
        ),
        QM_R2_adjusted = c(
          glance(qua111)$adj.r.squared,
          glance(qua121)$adj.r.squared,
          glance(qua141)$adj.r.squared,
          glance(qua151)$adj.r.squared,
          glance(qua112)$adj.r.squared,
          glance(qua122)$adj.r.squared,
          glance(qua142)$adj.r.squared,
          glance(qua152)$adj.r.squared,
          glance(qua113)$adj.r.squared,
          glance(qua123)$adj.r.squared,
          glance(qua143)$adj.r.squared,
          glance(qua153)$adj.r.squared
        ),
        QM_P_value = c(
          glance(qua111)$p.value,
          glance(qua121)$p.value,
          glance(qua141)$p.value,
          glance(qua151)$p.value,
          glance(qua112)$p.value,
          glance(qua122)$p.value,
          glance(qua142)$p.value,
          glance(qua152)$p.value,
          glance(qua113)$p.value,
          glance(qua123)$p.value,
          glance(qua143)$p.value,
          glance(qua153)$p.value
        ),
        QM_AIC = c(
          glance(qua111)$AIC,
          glance(qua121)$AIC,
          glance(qua141)$AIC,
          glance(qua151)$AIC,
          glance(qua112)$AIC,
          glance(qua122)$AIC,
          glance(qua142)$AIC,
          glance(qua152)$AIC,
          glance(qua113)$AIC,
          glance(qua123)$AIC,
          glance(qua143)$AIC,
          glance(qua153)$AIC
        ),
        QM_BIC = c(
          glance(qua111)$BIC,
          glance(qua121)$BIC,
          glance(qua141)$BIC,
          glance(qua151)$BIC,
          glance(qua112)$BIC,
          glance(qua122)$BIC,
          glance(qua142)$BIC,
          glance(qua152)$BIC,
          glance(qua113)$BIC,
          glance(qua123)$BIC,
          glance(qua143)$BIC,
          glance(qua153)$BIC
        ),
        QM_standard_error = c(
          glance(qua111)$sigma,
          glance(qua121)$sigma,
          glance(qua141)$sigma,
          glance(qua151)$sigma,
          glance(qua112)$sigma,
          glance(qua122)$sigma,
          glance(qua142)$sigma,
          glance(qua152)$sigma,
          glance(qua113)$sigma,
          glance(qua123)$sigma,
          glance(qua143)$sigma,
          glance(qua153)$sigma
        ),
        QM_deviance = c(
          glance(qua111)$deviance,
          glance(qua121)$deviance,
          glance(qua141)$deviance,
          glance(qua151)$deviance,
          glance(qua112)$deviance,
          glance(qua122)$deviance,
          glance(qua142)$deviance,
          glance(qua152)$deviance,
          glance(qua113)$deviance,
          glance(qua123)$deviance,
          glance(qua143)$deviance,
          glance(qua153)$deviance
        ),
        stringsAsFactors = FALSE)
      
      t2 = data.frame(
        Relationship_between_indices = c("Total agricultural production vs SPEI (annual)",
                                         "Crop production vs SPEI (annual)",
                                         "Livestock production vs SPEI (annual)",
                                         "Total food production vs SPEI (annual)",
                                         "Total agricultural production vs PM2.5 (mean concentration)",
                                         "Crop production vs PM2.5 (mean concentration)",
                                         "Livestock production vs PM2.5 (mean concentration)",
                                         "Total food production vs PM2.5 (mean concentration)",
                                         "Total agricultural production vs average temperature change",
                                         "Crop production vs average temperature change",
                                         "Livestock production vs average temperature change",
                                         "Total food production vs average temperature change"),
        LM_R2_adjusted = c(
          glance(lin211)$adj.r.squared,
          glance(lin221)$adj.r.squared,
          glance(lin241)$adj.r.squared,
          glance(lin251)$adj.r.squared,
          glance(lin212)$adj.r.squared,
          glance(lin222)$adj.r.squared,
          glance(lin242)$adj.r.squared,
          glance(lin252)$adj.r.squared,
          glance(lin213)$adj.r.squared,
          glance(lin223)$adj.r.squared,
          glance(lin243)$adj.r.squared,
          glance(lin253)$adj.r.squared
        ),
        LM_P_value = c(
          glance(lin211)$p.value,
          glance(lin221)$p.value,
          glance(lin241)$p.value,
          glance(lin251)$p.value,
          glance(lin212)$p.value,
          glance(lin222)$p.value,
          glance(lin242)$p.value,
          glance(lin252)$p.value,
          glance(lin213)$p.value,
          glance(lin223)$p.value,
          glance(lin243)$p.value,
          glance(lin253)$p.value
        ),
        LM_AIC = c(
          glance(lin211)$AIC,
          glance(lin221)$AIC,
          glance(lin241)$AIC,
          glance(lin251)$AIC,
          glance(lin212)$AIC,
          glance(lin222)$AIC,
          glance(lin242)$AIC,
          glance(lin252)$AIC,
          glance(lin213)$AIC,
          glance(lin223)$AIC,
          glance(lin243)$AIC,
          glance(lin253)$AIC
        ),
        LM_BIC = c(
          glance(lin211)$BIC,
          glance(lin221)$BIC,
          glance(lin241)$BIC,
          glance(lin251)$BIC,
          glance(lin212)$BIC,
          glance(lin222)$BIC,
          glance(lin242)$BIC,
          glance(lin252)$BIC,
          glance(lin213)$BIC,
          glance(lin223)$BIC,
          glance(lin243)$BIC,
          glance(lin253)$BIC
        ),
        LM_standard_error = c(
          glance(lin211)$sigma,
          glance(lin221)$sigma,
          glance(lin241)$sigma,
          glance(lin251)$sigma,
          glance(lin212)$sigma,
          glance(lin222)$sigma,
          glance(lin242)$sigma,
          glance(lin252)$sigma,
          glance(lin213)$sigma,
          glance(lin223)$sigma,
          glance(lin243)$sigma,
          glance(lin253)$sigma
        ),
        LM_deviance = c(
          glance(lin211)$deviance,
          glance(lin221)$deviance,
          glance(lin241)$deviance,
          glance(lin251)$deviance,
          glance(lin212)$deviance,
          glance(lin222)$deviance,
          glance(lin242)$deviance,
          glance(lin252)$deviance,
          glance(lin213)$deviance,
          glance(lin223)$deviance,
          glance(lin243)$deviance,
          glance(lin253)$deviance
        ),
        QM_R2_adjusted = c(
          glance(qua211)$adj.r.squared,
          glance(qua221)$adj.r.squared,
          glance(qua241)$adj.r.squared,
          glance(qua251)$adj.r.squared,
          glance(qua212)$adj.r.squared,
          glance(qua222)$adj.r.squared,
          glance(qua242)$adj.r.squared,
          glance(qua252)$adj.r.squared,
          glance(qua213)$adj.r.squared,
          glance(qua223)$adj.r.squared,
          glance(qua243)$adj.r.squared,
          glance(qua253)$adj.r.squared
        ),
        QM_P_value = c(
          glance(qua211)$p.value,
          glance(qua221)$p.value,
          glance(qua241)$p.value,
          glance(qua251)$p.value,
          glance(qua212)$p.value,
          glance(qua222)$p.value,
          glance(qua242)$p.value,
          glance(qua252)$p.value,
          glance(qua213)$p.value,
          glance(qua223)$p.value,
          glance(qua243)$p.value,
          glance(qua253)$p.value
        ),
        QM_AIC = c(
          glance(qua211)$AIC,
          glance(qua221)$AIC,
          glance(qua241)$AIC,
          glance(qua251)$AIC,
          glance(qua212)$AIC,
          glance(qua222)$AIC,
          glance(qua242)$AIC,
          glance(qua252)$AIC,
          glance(qua213)$AIC,
          glance(qua223)$AIC,
          glance(qua243)$AIC,
          glance(qua253)$AIC
        ),
        QM_BIC = c(
          glance(qua211)$BIC,
          glance(qua221)$BIC,
          glance(qua241)$BIC,
          glance(qua251)$BIC,
          glance(qua212)$BIC,
          glance(qua222)$BIC,
          glance(qua242)$BIC,
          glance(qua252)$BIC,
          glance(qua213)$BIC,
          glance(qua223)$BIC,
          glance(qua243)$BIC,
          glance(qua253)$BIC
        ),
        QM_standard_error = c(
          glance(qua211)$sigma,
          glance(qua221)$sigma,
          glance(qua241)$sigma,
          glance(qua251)$sigma,
          glance(qua212)$sigma,
          glance(qua222)$sigma,
          glance(qua242)$sigma,
          glance(qua252)$sigma,
          glance(qua213)$sigma,
          glance(qua223)$sigma,
          glance(qua243)$sigma,
          glance(qua253)$sigma
        ),
        QM_deviance = c(
          glance(qua211)$deviance,
          glance(qua221)$deviance,
          glance(qua241)$deviance,
          glance(qua251)$deviance,
          glance(qua212)$deviance,
          glance(qua222)$deviance,
          glance(qua242)$deviance,
          glance(qua252)$deviance,
          glance(qua213)$deviance,
          glance(qua223)$deviance,
          glance(qua243)$deviance,
          glance(qua253)$deviance
        ),
        stringsAsFactors = FALSE)
      
      t3 = data.frame(
        Relationship_between_indices = c("Total agricultural production vs SPEI (annual)",
                                         "Crop production vs SPEI (annual)",
                                         "Livestock production vs SPEI (annual)",
                                         "Total food production vs SPEI (annual)",
                                         "Total agricultural production vs PM2.5 (mean concentration)",
                                         "Crop production vs PM2.5 (mean concentration)",
                                         "Livestock production vs PM2.5 (mean concentration)",
                                         "Total food production vs PM2.5 (mean concentration)",
                                         "Total agricultural production vs average temperature change",
                                         "Crop production vs average temperature change",
                                         "Livestock production vs average temperature change",
                                         "Total food production vs average temperature change"),
        LM_R2_adjusted = c(
          glance(lin311)$adj.r.squared,
          glance(lin321)$adj.r.squared,
          glance(lin341)$adj.r.squared,
          glance(lin351)$adj.r.squared,
          glance(lin312)$adj.r.squared,
          glance(lin322)$adj.r.squared,
          glance(lin342)$adj.r.squared,
          glance(lin352)$adj.r.squared,
          glance(lin313)$adj.r.squared,
          glance(lin323)$adj.r.squared,
          glance(lin343)$adj.r.squared,
          glance(lin353)$adj.r.squared
        ),
        LM_P_value = c(
          glance(lin311)$p.value,
          glance(lin321)$p.value,
          glance(lin341)$p.value,
          glance(lin351)$p.value,
          glance(lin312)$p.value,
          glance(lin322)$p.value,
          glance(lin342)$p.value,
          glance(lin352)$p.value,
          glance(lin313)$p.value,
          glance(lin323)$p.value,
          glance(lin343)$p.value,
          glance(lin353)$p.value
        ),
        LM_AIC = c(
          glance(lin311)$AIC,
          glance(lin321)$AIC,
          glance(lin341)$AIC,
          glance(lin351)$AIC,
          glance(lin312)$AIC,
          glance(lin322)$AIC,
          glance(lin342)$AIC,
          glance(lin352)$AIC,
          glance(lin313)$AIC,
          glance(lin323)$AIC,
          glance(lin343)$AIC,
          glance(lin353)$AIC
        ),
        LM_BIC = c(
          glance(lin311)$BIC,
          glance(lin321)$BIC,
          glance(lin341)$BIC,
          glance(lin351)$BIC,
          glance(lin312)$BIC,
          glance(lin322)$BIC,
          glance(lin342)$BIC,
          glance(lin352)$BIC,
          glance(lin313)$BIC,
          glance(lin323)$BIC,
          glance(lin343)$BIC,
          glance(lin353)$BIC
        ),
        LM_standard_error = c(
          glance(lin311)$sigma,
          glance(lin321)$sigma,
          glance(lin341)$sigma,
          glance(lin351)$sigma,
          glance(lin312)$sigma,
          glance(lin322)$sigma,
          glance(lin342)$sigma,
          glance(lin352)$sigma,
          glance(lin313)$sigma,
          glance(lin323)$sigma,
          glance(lin343)$sigma,
          glance(lin353)$sigma
        ),
        LM_deviance = c(
          glance(lin311)$deviance,
          glance(lin321)$deviance,
          glance(lin341)$deviance,
          glance(lin351)$deviance,
          glance(lin312)$deviance,
          glance(lin322)$deviance,
          glance(lin342)$deviance,
          glance(lin352)$deviance,
          glance(lin313)$deviance,
          glance(lin323)$deviance,
          glance(lin343)$deviance,
          glance(lin353)$deviance
        ),
        QM_R2_adjusted = c(
          glance(qua311)$adj.r.squared,
          glance(qua321)$adj.r.squared,
          glance(qua341)$adj.r.squared,
          glance(qua351)$adj.r.squared,
          glance(qua312)$adj.r.squared,
          glance(qua322)$adj.r.squared,
          glance(qua342)$adj.r.squared,
          glance(qua352)$adj.r.squared,
          glance(qua313)$adj.r.squared,
          glance(qua323)$adj.r.squared,
          glance(qua343)$adj.r.squared,
          glance(qua353)$adj.r.squared
        ),
        QM_P_value = c(
          glance(qua311)$p.value,
          glance(qua321)$p.value,
          glance(qua341)$p.value,
          glance(qua351)$p.value,
          glance(qua312)$p.value,
          glance(qua322)$p.value,
          glance(qua342)$p.value,
          glance(qua352)$p.value,
          glance(qua313)$p.value,
          glance(qua323)$p.value,
          glance(qua343)$p.value,
          glance(qua353)$p.value
        ),
        QM_AIC = c(
          glance(qua311)$AIC,
          glance(qua321)$AIC,
          glance(qua341)$AIC,
          glance(qua351)$AIC,
          glance(qua312)$AIC,
          glance(qua322)$AIC,
          glance(qua342)$AIC,
          glance(qua352)$AIC,
          glance(qua313)$AIC,
          glance(qua323)$AIC,
          glance(qua343)$AIC,
          glance(qua353)$AIC
        ),
        QM_BIC = c(
          glance(qua311)$BIC,
          glance(qua321)$BIC,
          glance(qua341)$BIC,
          glance(qua351)$BIC,
          glance(qua312)$BIC,
          glance(qua322)$BIC,
          glance(qua342)$BIC,
          glance(qua352)$BIC,
          glance(qua313)$BIC,
          glance(qua323)$BIC,
          glance(qua343)$BIC,
          glance(qua353)$BIC
        ),
        QM_standard_error = c(
          glance(qua311)$sigma,
          glance(qua321)$sigma,
          glance(qua341)$sigma,
          glance(qua351)$sigma,
          glance(qua312)$sigma,
          glance(qua322)$sigma,
          glance(qua342)$sigma,
          glance(qua352)$sigma,
          glance(qua313)$sigma,
          glance(qua323)$sigma,
          glance(qua343)$sigma,
          glance(qua353)$sigma
        ),
        QM_deviance = c(
          glance(qua311)$deviance,
          glance(qua321)$deviance,
          glance(qua341)$deviance,
          glance(qua351)$deviance,
          glance(qua312)$deviance,
          glance(qua322)$deviance,
          glance(qua342)$deviance,
          glance(qua352)$deviance,
          glance(qua313)$deviance,
          glance(qua323)$deviance,
          glance(qua343)$deviance,
          glance(qua353)$deviance
        ),
        stringsAsFactors = FALSE)
      
      t4 = data.frame(
        Relationship_between_indices = c("Total agricultural production vs SPEI (annual)",
                                         "Crop production vs SPEI (annual)",
                                         "Livestock production vs SPEI (annual)",
                                         "Total food production vs SPEI (annual)",
                                         "Total agricultural production vs PM2.5 (mean concentration)",
                                         "Crop production vs PM2.5 (mean concentration)",
                                         "Livestock production vs PM2.5 (mean concentration)",
                                         "Total food production vs PM2.5 (mean concentration)",
                                         "Total agricultural production vs average temperature change",
                                         "Crop production vs average temperature change",
                                         "Livestock production vs average temperature change",
                                         "Total food production vs average temperature change"),
        LM_R2_adjusted = c(
          glance(lin411)$adj.r.squared,
          glance(lin421)$adj.r.squared,
          glance(lin441)$adj.r.squared,
          glance(lin451)$adj.r.squared,
          glance(lin412)$adj.r.squared,
          glance(lin422)$adj.r.squared,
          glance(lin442)$adj.r.squared,
          glance(lin452)$adj.r.squared,
          glance(lin413)$adj.r.squared,
          glance(lin423)$adj.r.squared,
          glance(lin443)$adj.r.squared,
          glance(lin453)$adj.r.squared
        ),
        LM_P_value = c(
          glance(lin411)$p.value,
          glance(lin421)$p.value,
          glance(lin441)$p.value,
          glance(lin451)$p.value,
          glance(lin412)$p.value,
          glance(lin422)$p.value,
          glance(lin442)$p.value,
          glance(lin452)$p.value,
          glance(lin413)$p.value,
          glance(lin423)$p.value,
          glance(lin443)$p.value,
          glance(lin453)$p.value
        ),
        LM_AIC = c(
          glance(lin411)$AIC,
          glance(lin421)$AIC,
          glance(lin441)$AIC,
          glance(lin451)$AIC,
          glance(lin412)$AIC,
          glance(lin422)$AIC,
          glance(lin442)$AIC,
          glance(lin452)$AIC,
          glance(lin413)$AIC,
          glance(lin423)$AIC,
          glance(lin443)$AIC,
          glance(lin453)$AIC
        ),
        LM_BIC = c(
          glance(lin411)$BIC,
          glance(lin421)$BIC,
          glance(lin441)$BIC,
          glance(lin451)$BIC,
          glance(lin412)$BIC,
          glance(lin422)$BIC,
          glance(lin442)$BIC,
          glance(lin452)$BIC,
          glance(lin413)$BIC,
          glance(lin423)$BIC,
          glance(lin443)$BIC,
          glance(lin453)$BIC
        ),
        LM_standard_error = c(
          glance(lin411)$sigma,
          glance(lin421)$sigma,
          glance(lin441)$sigma,
          glance(lin451)$sigma,
          glance(lin412)$sigma,
          glance(lin422)$sigma,
          glance(lin442)$sigma,
          glance(lin452)$sigma,
          glance(lin413)$sigma,
          glance(lin423)$sigma,
          glance(lin443)$sigma,
          glance(lin453)$sigma
        ),
        LM_deviance = c(
          glance(lin411)$deviance,
          glance(lin421)$deviance,
          glance(lin441)$deviance,
          glance(lin451)$deviance,
          glance(lin412)$deviance,
          glance(lin422)$deviance,
          glance(lin442)$deviance,
          glance(lin452)$deviance,
          glance(lin413)$deviance,
          glance(lin423)$deviance,
          glance(lin443)$deviance,
          glance(lin453)$deviance
        ),
        QM_R2_adjusted = c(
          glance(qua411)$adj.r.squared,
          glance(qua421)$adj.r.squared,
          glance(qua441)$adj.r.squared,
          glance(qua451)$adj.r.squared,
          glance(qua412)$adj.r.squared,
          glance(qua422)$adj.r.squared,
          glance(qua442)$adj.r.squared,
          glance(qua452)$adj.r.squared,
          glance(qua413)$adj.r.squared,
          glance(qua423)$adj.r.squared,
          glance(qua443)$adj.r.squared,
          glance(qua453)$adj.r.squared
        ),
        QM_P_value = c(
          glance(qua411)$p.value,
          glance(qua421)$p.value,
          glance(qua441)$p.value,
          glance(qua451)$p.value,
          glance(qua412)$p.value,
          glance(qua422)$p.value,
          glance(qua442)$p.value,
          glance(qua452)$p.value,
          glance(qua413)$p.value,
          glance(qua423)$p.value,
          glance(qua443)$p.value,
          glance(qua453)$p.value
        ),
        QM_AIC = c(
          glance(qua411)$AIC,
          glance(qua421)$AIC,
          glance(qua441)$AIC,
          glance(qua451)$AIC,
          glance(qua412)$AIC,
          glance(qua422)$AIC,
          glance(qua442)$AIC,
          glance(qua452)$AIC,
          glance(qua413)$AIC,
          glance(qua423)$AIC,
          glance(qua443)$AIC,
          glance(qua453)$AIC
        ),
        QM_BIC = c(
          glance(qua411)$BIC,
          glance(qua421)$BIC,
          glance(qua441)$BIC,
          glance(qua451)$BIC,
          glance(qua412)$BIC,
          glance(qua422)$BIC,
          glance(qua442)$BIC,
          glance(qua452)$BIC,
          glance(qua413)$BIC,
          glance(qua423)$BIC,
          glance(qua443)$BIC,
          glance(qua453)$BIC
        ),
        QM_standard_error = c(
          glance(qua411)$sigma,
          glance(qua421)$sigma,
          glance(qua441)$sigma,
          glance(qua451)$sigma,
          glance(qua412)$sigma,
          glance(qua422)$sigma,
          glance(qua442)$sigma,
          glance(qua452)$sigma,
          glance(qua413)$sigma,
          glance(qua423)$sigma,
          glance(qua443)$sigma,
          glance(qua453)$sigma
        ),
        QM_deviance = c(
          glance(qua411)$deviance,
          glance(qua421)$deviance,
          glance(qua441)$deviance,
          glance(qua451)$deviance,
          glance(qua412)$deviance,
          glance(qua422)$deviance,
          glance(qua442)$deviance,
          glance(qua452)$deviance,
          glance(qua413)$deviance,
          glance(qua423)$deviance,
          glance(qua443)$deviance,
          glance(qua453)$deviance
        ),
        stringsAsFactors = FALSE)
    }  
    
  output$stat_table <- renderTable(if (input$reg1b == "Syria") {
    colnames(t1) <- c("Index relationship","LM (R^2 - adjusted)", "LM (P-value)", "LM (AIC)", "LM (BIC)", "LM (standard error)", "LM (deviance)", "QM (R^2 - adj.)", "QM (P-value)", "QM (AIC)", "QM (BIC)", "QM (standard error)", "QM (deviance)")
    t1  
  } else if (input$reg1b == "Colombia") {
    colnames(t2) <- c("Index relationship","LM (R^2 - adjusted)", "LM (P-value)", "LM (AIC)", "LM (BIC)", "LM (standard error)", "LM (deviance)", "QM (R^2 - adj.)", "QM (P-value)", "QM (AIC)", "QM (BIC)", "QM (standard error)", "QM (deviance)")
    t2
  } else if (input$reg1b == "Iran") {
    colnames(t3) <- c("Index relationship","LM (R^2 - adjusted)", "LM (P-value)", "LM (AIC)", "LM (BIC)", "LM (standard error)", "LM (deviance)", "QM (R^2 - adj.)", "QM (P-value)", "QM (AIC)", "QM (BIC)", "QM (standard error)", "QM (deviance)")
    t3
  } else if (input$reg1b == "Bolivia") {
    colnames(t4) <- c("Index relationship","LM (R^2 - adjusted)", "LM (P-value)", "LM (AIC)", "LM (BIC)", "LM (standard error)", "LM (deviance)", "QM (R^2 - adj.)", "QM (P-value)", "QM (AIC)", "QM (BIC)", "QM (standard error)", "QM (deviance)")
    t4  
  })
  }
  
}

# Run the application 
shinyApp(ui = ui, server = server)
