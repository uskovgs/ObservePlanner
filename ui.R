library(plotly, warn.conflicts = F, quietly = T)

fluidPage(
  
  # Application title
  titlePanel("Plan of observations"),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      tags$div(htmlOutput("current_time"), style="font-family:'Courier',monospace"),
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
