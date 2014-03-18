library(shiny)


shinyUI(pageWithSidebar(
  
  headerPanel("Compare Airline Arrival Delays in 2003"),
  
  sidebarPanel( 
    selectInput("airline1", "Airline 1:", list("American Airlines (AA)" ,  
      "Delta Airlines (DL)", "Southwest Airlines (SW)", "United Airlines(UA)",
      "Northwest Airlines (NW)")),
    
    selectInput("airline2", "Airline 2:", list("American Airlines (AA)" ,  
      "Delta Airlines (DL)", "Southwest Airlines (WN)", "United Airlines(UA)",
      "Northwest Airlines (NW)"))
  
  ),
  
  
  mainPanel(
    h3(textOutput("caption")),
    plotOutput("plotgood"),
    plotOutput("whole"),
    
    plotOutput("month"),
    plotOutput("day")
    )
  
  ))
