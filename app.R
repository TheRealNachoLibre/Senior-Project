#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#Author: Grant Grisham
#    http://shiny.rstudio.com/
#

library(DT)
library(dygraphs)
library(shiny)
library(plotly)

ozone <- read.csv("www/antarctic-ozone-hole-area.csv") %>% 
  select(-Code)
ozone_2 <- read.csv("www/stratospheric-ozone-concentration-projections.csv") %>% 
  select(-Code)
temp <- read.csv("www/anomaly_data.csv")
seaice <- read.csv("www/seaice_average.csv")
co2_data <- read.csv("www/co2_data.csv")
global_data <- read.csv("www/global_data.csv")
global_pollution <- read.csv("www/global_data.csv")
comparison <- read.csv("www/comparison.csv")
global_names <- c("Entity", "Year", "coal", "oil", "gas", "cement", "flaring", "other")
seaice_names <- c("Year", "avg")
co2_names <- c("region", "Year", "emmision")
temp_names <- c("Entity", "Year", "Median_Temp")

global_data <- global_data %>% 
  filter("International transport" != Entity) %>% 
  filter("World" != Entity) %>% 
  filter("Asia" != Entity) %>% 
  filter("Europe" != Entity) %>% 
  filter("North America" != Entity)
colnames(global_data) <- c("Entity", "Year", "coal", "oil", "gas", "cement", "flaring", "other") 
entity_names <- global_data %>% 
  filter("International transport" != Entity) %>% 
  filter("World" != Entity) %>% 
  filter("Asia" != Entity) %>% 
  filter("Europe" != Entity) %>% 
  filter("North America" != Entity) %>% 
  select(Entity) 
# rownames(global_data) <- entity_names$Entity
global_data <- global_data %>% 
  filter(80 < coal)




# Define UI for application that draws a histogram
ui <- fluidPage(
  #tags$style(".container-fluid{background:red;}") , 
  includeCSS("www/BigData.css"),
  # Application title
    titlePanel("An Analysis of the Inverse Correlation between Carbon Emissions and Sea Ice Melting"),

   
    fluidRow(column(11, offset=0,
      plotlyOutput("plotly1"),
      plotlyOutput("plotly3"),
      selectInput("year", "Choose Year:", choices="", selected=""),
      plotlyOutput("plotly2")
                  )),
    
    fluidRow(column(6, offset=1,
                       selectInput("year_2", "Choose Year:", choices="", selected=""),
                       selectInput("country", "Choose Country:", choices="", selected=""))),
                       plotlyOutput("plotly4"),
                       plotlyOutput("plotly5")
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  updateSelectInput("year", session=session, choices=unique(global_data$Year), selected="2019")
  
  output$plotly1<-renderPlotly({
    #req(!input$yearchoice1=="")
    
    fig <- plot_ly(comparison, x = ~Year)
    fig <- fig %>% add_lines(y = ~emission, name = "1/1000 Billions\ntons CO2")
    fig <- fig %>% add_lines(y = ~avg, name = "Seaice sq. KM")
    fig <- fig %>% add_lines(y = ~Median_Temp, name = "Avg. Global Temp (C)")
    fig <- fig %>% layout(
      title = "Global Warming Data Comparison",
      xaxis = list(
        rangeslider = list(type = "date", format="yyyy")))

    fig
  
  })
  
  
  output$plotly2<-renderPlotly({
    #requiring an input before rendering the graph
    req(!input$year=="")
    
    #setting the year via dropdown input
    global_data<-global_data[global_data$Year==input$year,]
    data<-global_data
    #data <- as.matrix(global_data)
    #creating the 3d graph
    fig <- plot_ly(z = as.matrix(data[3:length(data)]), x=colnames(data)[3:length(data)], y=data$Entity) %>% add_surface(
      contours = list(
        z = list(
          show=TRUE,
          usecolormap=TRUE,
          highlightcolor="#ff0000",
          project=list(z=TRUE)
        )
      )
    )
    fig <- fig %>% layout(title = "Total Pollution per Entity", 
                          scene = list(
                            camera=list(
                              eye = list(x=1.25, y=1.25, z=1.25)
                            )
                          )
    )
    fig
  })
  
  output$plotly3<- renderPlotly({
    fig <- plot_ly(ozone, x = ~Year)
    fig <- fig %>% add_lines(y = ozone$Maximum.ozone.hole.area..NASA., name = "Maximum Ozone Hole Area")
    fig <- fig %>% add_lines(y = ozone$Mean.ozone.hole.area, name = "Mean Ozone Hole Area")
    fig <- fig %>% layout(
      title = "Antarctic Ozone Hole Area",
      xaxis = list(
        rangeslider = list(type = "date", format="yyyy")),
      yaxis = list(title = "Km"))
    
    fig
    
  })
  
  #needs work
  output$plotly5<- renderPlotly({
    ozone_2 <- as.data.frame(ozone_2)
    
    fig <- plot_ly(x = ozone_2$Year[ozone_2$Entity=="Global"])
    fig <- fig %>% add_lines(y = ozone_2$Ozone.concentration..1960...0.[ozone_2$Entity=="Global"], name = "Global")
    fig <- fig %>% add_lines(y = ozone_2$Ozone.concentration..1960...0.[ozone_2$Entity=="Arctic"], name = "Arctic")
    fig <- fig %>% add_lines(y = ozone_2$Ozone.concentration..1960...0.[ozone_2$Entity=="Antarctic"], name = "Antarctic")
    #fig <- fig %>% add_lines(y = ozone$Mean.ozone.hole.area, name = "Mean Ozone Hole Area")
    fig <- fig %>% layout(
      title = "Projected Ozone Recovery",
      xaxis = list(
        rangeslider = list(type = "date", format="yyyy")),
      yaxis = list(title = "Km"))
    
    fig
    
  })
  
  #needs work
  updateSelectInput("year_2", session=session, choices=unique(global_pollution$Year), selected="2019")
  updateSelectInput("country", session=session, choices=unique(global_pollution$Entity), selected="World")
  output$plotly4<- renderPlotly({
    glo <- global_pollution[global_pollution$Year==input$year_2,]
    glo <- glo[glo$Entity==input$country,]
    
    fig <- plot_ly() %>% 
      add_trace(x = glo$Year, type="bar", y = glo$coal, name="coal") %>% 
      add_trace(x = glo$Year, type="bar", y = glo$oil, name="oil") %>% 
      add_trace(x = glo$Year, type="bar", y = glo$gas, name="gas") %>% 
      add_trace(x = glo$Year, type="bar", y = glo$cement, name="cement") %>% 
      add_trace(x = glo$Year, type="bar", y = glo$flaring, name="flaring") %>% 
      add_trace(x = glo$Year, type="bar", y = glo$other, name="other")
    fig <- fig %>% layout(
      title = "Pollution by Country",
      yaxis = list(title = "Emissions")
    )
      
    fig
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)