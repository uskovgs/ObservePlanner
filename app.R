library(plotly, warn.conflicts = F, quietly = T)
library(lubridate, warn.conflicts = F, quietly = T)
library(dplyr, warn.conflicts = F, quietly = T)
library(tidyr, warn.conflicts = F, quietly = T)
library(stringr, warn.conflicts = F, quietly = T)
library(tools, warn.conflicts = F, quietly = T)
library(plotly, warn.conflicts = F, quietly = T)
library(astrolibR, warn.conflicts = F, quietly = T)
library(celestial, warn.conflicts = F, quietly = T)
library(suncalc, warn.conflicts = F, quietly = T)
library(shiny, warn.conflicts = F, quietly = T)
library(DT, warn.conflicts = F, quietly = T)
library(readr, warn.conflicts = F, quietly = T)


ui <- fluidPage(
  
  # Application title
  titlePanel("Plan of observations"),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      tags$div(htmlOutput("current_time"), style="font-family:'Courier',monospace;
                                                  font-size: 20px"),
      hr(),
      selectInput("observatory", 
                  label = "Choose observatory:", 
                  choices = c("Tubitak National Observatory" = "tub",
                              "Саянская обсерватория ИСЗФ СО РАН"="sso", "sso",
                              "SAO RAS" = "sao"),
                  selected = "tub"),
      fileInput("file_upload", "Choose CSV file", accept = c(".csv", "text/csv"), buttonLabel = "Import"),
      textInput("txt_radec", "obj hh:mm:ss dd:mm:ss"),
      column(12, fluidRow(actionButton("btn_add", "Add"),
                          actionButton("btn_del", "Remove"))),
      
      DT::dataTableOutput('table')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("plot_altitude"),
      hr(),
      plotlyOutput("plot_azimith")
    )
  )
)

get_lmst_ut <- function(ymd_hms_lubridate, longitude, timezone) {
  
  hour <- lubridate::hour(ymd_hms_lubridate) +
    lubridate::minute(ymd_hms_lubridate)/60 +
    lubridate::second(ymd_hms_lubridate)/3600
  
  day <- lubridate::day(ymd_hms_lubridate)
  month <- lubridate::month(ymd_hms_lubridate)
  year <- lubridate::year(ymd_hms_lubridate)
  
  jd <- astrolibR::jdcnv(yr = year, mn = month, day = day, hr = hour)
  
  astrolibR::ct2lst(lng = longitude, tz = timezone, tme = jd)
}
altaz_by_time <- function(ra, dec, ymd_hms_lubridate, longitude, latitude, timezone) {
  ra <- celestial::hms2deg(ra)
  dec <- celestial::dms2deg(dec)
  lmst_ut <- get_lmst_ut(ymd_hms_lubridate, longitude, timezone)
  
  H <- lmst_ut*15 - ra
  
  altaz <- astrolibR::hadec2altaz(ha = H, dec = dec, lat = latitude, ws = T)
  # alt <- asin(sin(latitude * pi/180) * sin(dec*pi/180) + cos(latitude*pi/180) * cos(dec*pi/180) * cos(H*pi/180)) * 180/pi
  return(list(alt = altaz$alt, azimuth = altaz$az))
}

server <- function(input, output) {
  
  values <- reactiveValues()
  values$df <- data.frame(obj = character(0), 
                          ra = character(0), 
                          dec = character(0))
  values$location <- list(observatory=character(0), 
                          latitude=double(0), 
                          longitude= double(0),
                          timezone=double(0))
  values$position <- data.frame(obj = character(0), 
                                ra = character(0), 
                                dec = character(0), 
                                UT=numeric(0), 
                                altitude = double(0), 
                                azimuth = double(0))
  
  observeEvent(input$btn_add, {
    input_txt <- stringr::str_squish(input$txt_radec)
    input_txt <- stringr::str_match_all(input_txt, "(.*)\\s(\\d+:\\d+:\\d.*)\\s(\\+?\\-?\\d+:\\d+:\\d.*)")[[1]]
    
    if(length(input_txt) == 4) values$df[nrow(values$df) + 1,] <- c(input_txt[2], input_txt[3], input_txt[4])
    
    
    
  })
  observeEvent(input$btn_del,{
    
    if (!is.null(input$table_rows_selected)) {
      
      values$df <- values$df[-as.numeric(input$table_rows_selected),]
      values$position <- values$position[-as.numeric(input$table_rows_selectedF),]
    }
  })
  observeEvent(input$observatory,{
    values$location <-
      switch(input$observatory,
             "tub" = list(observatory="tub", 
                          latitude=36 + 49/60 + 27/3600, 
                          longitude= 30 + 20/60 + 08/3600,
                          timezone=3),
             "sso" = list(observatory="sso",
                          latitude = 51 + 37/60 + 18/3600,
                          longitude = 100 + 55/60 + 07/3600,
                          timezone = 8),
             "sao" = list(observatory = "sao",
                          latitude = 43 + 38/60 + 48/3600,
                          longitude = 41 + 26/60 + 25/3600,
                          timezone = 3)
      )
  })
  observeEvent(input$file_upload, {
    file <- input$file_upload
    ext <- tools::file_ext(file$datapath)
    req(file)
    
    if(ext == 'csv'){
      a <- readr::read_csv(file$datapath, col_names = F, col_types = readr::cols(
        X1 = readr::col_character(),
        X2 = readr::col_character(),
        X3 = readr::col_character()
      ))
      colnames(a) <- colnames(values$df)
      values$df <- bind_rows(values$df, a)
    }
    else(showNotification("Error: upload csv file."))
    
    
    
  })
  output$table <- DT::renderDataTable(DT::datatable(values$df, 
                                                    rownames = F,
                                                    options = list(pageLength = 100, info = F, dom='t')))
  output$plot_altitude <- renderPlotly({
    if (nrow(values$df) > 0) {
      
      latitude <- values$location[["latitude"]]
      longitude <- values$location[["longitude"]]
      timezone <- values$location[["timezone"]]
      
      
      now <- now(tzone = "UTC")
      sunrise_time <- suncalc::getSunlightTimes(date = today("UTC"), latitude, longitude, tz = "UTC")$sunrise
      
      if(now < (sunrise_time + hours(1))){
        day_when_obs_starts <- today("UTC") - days(1)
        
      } 
      else {
        day_when_obs_starts <- today("UTC")
        
      }
      
      sunpos_today <- suncalc::getSunlightTimes(date = day_when_obs_starts, latitude, longitude, tz = "UTC") 
      sunpos_tomorrow <- suncalc::getSunlightTimes(date = day_when_obs_starts + lubridate::days(1), latitude, longitude, tz = "UTC") 
      
      twilight_today_start <- sunpos_today$sunset
      twilight_today_end <- sunpos_today$night
      twilight_tomorrow_start <- sunpos_tomorrow$nightEnd
      twilight_tomorrow_end <- sunpos_tomorrow$sunrise
      
      
      
      times <- tibble(UT = seq(twilight_today_start, twilight_tomorrow_end, by = seconds(60)))
      
      result_df <- crossing(values$df, times) %>%
        mutate(altitude = altaz_by_time(ra =  ra, dec = dec, UT, longitude = longitude, latitude = latitude, timezone = timezone)$alt,
               azimuth = altaz_by_time(ra =  ra, dec = dec, UT, longitude = longitude, latitude = latitude, timezone = timezone)$az)
      
      values$position <- result_df
      
      
      
      plot_ly(data = result_df, x = ~UT, y = ~altitude, color = ~obj, mode = "lines",
              type = 'scatter',
              text = ~paste("Object: ", obj,
                            "<br> alt: ", round(altitude,1),
                            "<br> zenith:", round(90-altitude,1),
                            "<br> UT: ", UT),
              hoverinfo = 'text') %>% 
        layout(shapes = list(
          list(type = "rect",
               x0=twilight_today_start, x1=twilight_today_end,
               y0=0, y1=1, yref='paper', fillcolor='gray', opacity=0.1, line = list(color='gray')), 
          list(type = "rect",
               x0=twilight_tomorrow_start, x1=twilight_tomorrow_end, 
               y0=0, y1=1, yref='paper', fillcolor='gray', opacity=0.1, line = list(color='gray'))),
          yaxis = list(range = c(-30,95), dtick = 20))
    }
    
  })
  output$plot_azimith <- renderPlotly({{
    if(nrow(values$position) > 0){
      df <- values$position %>% 
        filter(altitude > 0) %>% 
        # slice(seq(1,nrow(values$position), by=60)) %>% 
        mutate(r = cos(altitude*pi/180),
               azimuth = azimuth-90)
      
      plot_ly(data = df,type = 'scatterpolar',
              r = ~r,
              theta = ~azimuth,
              color = ~obj,
              mode = 'lines',
              hoverinfo = 'text',
              text = ~paste("Object: ", obj,
                            "<br> alt: ", round(altitude,1),
                            "<br> az:", round(90-altitude,1),
                            "<br> UT: ", UT)) %>% 
        layout(polar = list(radialaxis = list(showticklabels = FALSE),
                            angularaxis = list(ticktext = list("W", "NW", "N", "NE", "E", "SE", "S", "SW"), 
                                               tickvals = list(0, 45, 90, 135, 180, 225, 270, 315),
                                               tickmode = "array")))
    }
  }})
  output$current_time <- renderText({
    invalidateLater(1000)
    t_ut <- now(tzone="UTC")
    t_sideral <- get_lmst_ut(t_ut, values$location[["longitude"]], values$location[["timezone"]])
    t_sideral_human <- as.period(as.duration(days(x=1))*(t_sideral/24), ) %>% round(digits = 0)
    paste("<b>DATE</b>", as_date(t_ut), 
          "</br><b>LMST</b>", format(today() + t_sideral_human, "%H:%M:%S"),
          "</br>&nbsp;&nbsp;<b>UT</b>", format(t_ut, "%H:%M:%S")
    )
  })
  
}

shiny::shinyApp(ui = ui, server = server)



