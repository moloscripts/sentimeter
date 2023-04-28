library(shiny)
library(lubridate)
library(DT)

DF = data.frame(ID = c ("1","2","3","4","5"),
                product = c("A","B","C","A","C"),
                sentiment = c("good","medium","bad","medium","bad"),
                online = c(1,0,1,1,0),
                ooh = c(0,1,0,1,1),
                Date= c("2020-08-11", "2020-09-16","2021-10-20", "2020-11-25", "2022-11-27"), 
                event = c(1,1,0,0,0))


Year <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010)
Auckland <- c(1760, 1549, 1388, 1967, 1326, 1765, 1814, 1693, 1502, 1751)
Wellington <- c(2176, 3154, 1138, 1196, 2132, 3176, 4181, 5169, 3150, 4175)
Lyttelton <- c(2176, 3154, 1138, 1196, 2132, 3176, 4181, 5169, 3150, 4175)
my_data <- as.data.frame(cbind(Year,Auckland,Wellington, Lyttelton))



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Product Sentiment"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("sentiment", label = "Select sentiment", choices = unique(DF$sentiment)), 
      dateRangeInput("daterange", h5("Date range"), 
                     start = ymd("2020-08-11"), 
                     end = ymd("2022-09-06"), 
                     min = min(DF$Date), 
                     max = max(DF$Date),
                     format = "yyyy-mm-dd"
      ) 
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      dataTableOutput("DT_Table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  filtered_data <- reactive({
    DF %>%
      filter(sentiment == input$sentiment &
               (Date >= input$daterange[1] & Date <= input$daterange[2]))
  })
  
  output$DT_Table <- DT::renderDataTable(
    filtered_data(),
    options = list(pageLength = 5, autoWidth = TRUE, dom='t')
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)