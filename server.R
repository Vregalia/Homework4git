library(shiny)
library(datasets)

airData <- air


shinyServer(function(input, output){
  
  airline1Input <- reactive({
    switch(input$airline1,
      "American Airlines (AA)" = "AA", "Delta Airlines (DL)" = "DL", 
      "Southwest Airlines (SW)" = "SW", "United Airlines(UA)" = "UA",
      "Northwest Airlines (NW)" = "NW")
  })
  airline2Input <- reactive({
    switch(input$airline2,
      "American Airlines (AA)" = "AA", "Delta Airlines (DL)" = "DL", 
      "Southwest Airlines (WN)" = "WN", "United Airlines(UA)" = "UA",
      "Northwest Airlines (NW)" = "NW")
  })
  
 output$caption <- renderText({
   carrier1 <- airline1Input()
   carrier2 <- airline2Input()
   paste("Comparing Arrival Delays of", carrier1, "with", carrier2, sep = " ")
 })
  
 output$whole <- renderPlot({
   carrier1 <- airline1Input()
   carrier2 <- airline2Input()
   par(mfrow = c(1,2))
   boxplot(subset(airData$air.ArrDelay, airData$air.UniqueCarrier == carrier1), 
      main = paste("Arrival Delays for", carrier1, sep = " "), notch = TRUE, ylim =
      c(-200, 1500), col = "tomato")
    points(mean(subset(airData$air.ArrDelay, airData$air.UniqueCarrier == carrier1)))
   boxplot(subset(airData$air.ArrDelay, airData$air.UniqueCarrier == carrier2),
      main = paste("Arrival Delays for", carrier2, sep = " "), ylim = c(-200, 1500),
      col = "cadetblue", notch = TRUE)
 })
 
  output$month <- renderPlot({
    carrier1 <- airline1Input()
    carrier2 <- airline2Input()
    par(mfrow = c(1,2))
    
    boxplot(subset(airData$air.ArrDelay, airData$air.UniqueCarrier == carrier1) ~ 
      subset(airData$air.Month, airData$air.UniqueCarrier == carrier1), main = 
      paste("Arrival Delays by Month for ", carrier1), xlab = "Month", 
      ylab = "Arrival Delay", ylim = c(-200, 1500), notch = TRUE, col = 
      rep("tomato", times = 12))
    
    boxplot(subset(airData$air.ArrDelay, airData$air.UniqueCarrier == carrier2) ~ 
      subset(airData$air.Month, airData$air.UniqueCarrier == carrier2), main = 
      paste("Arrival Delays by Month for ", carrier2), xlab = "Month", 
      ylab = "Arrival Delay", ylim = c(-200, 1500), notch = TRUE, col = 
      rep("cadetblue", times = 12))
  })
 
 
 output$day <- renderPlot({
   carrier1 <- airline1Input()
   carrier2 <- airline2Input()
   par(mfrow = c(1,2))
   
   boxplot(subset(airData$air.ArrDelay, airData$air.UniqueCarrier == carrier1) ~
      subset(airData$air.DayOfWeek, airData$air.UniqueCarrier == carrier1), main = 
      paste("Arrival Delays by Day of Week for ", carrier1), xlab = "Day of Week", 
      ylab = "Arrival Delay", ylim = c(-200, 1500), notch = TRUE, col = 
      rep("tomato", times = 7))
   
   boxplot(subset(airData$air.ArrDelay, airData$air.UniqueCarrier == carrier2) ~
      subset(airData$air.DayOfWeek, airData$air.UniqueCarrier == carrier2), main = 
      paste("Arrival Delays by Day of Week for ", carrier2), xlab = "Day of Week", 
      ylab = "Arrival Delay", ylim = c(-200, 1500), notch = TRUE, col = 
      rep("cadetblue", times = 7))
 })
  
 output$plotgood <- renderPlot({
   carrier1 <- airline1Input()
   carrier2 <- airline2Input()
   a1 <- numeric(12)
   a2 <- numeric(12)
   
   for(i in 1:12){
     a1[i]= mean(subset(airData$air.ArrDelay, airData$air.Month == i 
        & airData$air.UniqueCarrier == carrier1), na.rm = TRUE)
        i = i+1
   }
   for(j in 1:12){
     a2[j] = mean(subset(airData$air.ArrDelay, airData$air.Month == j 
        & airData$air.UniqueCarrier == carrier2), na.rm = TRUE)
        j = j+1
   }
   months <- 1:12
   
   plot(a1 ~ months, type = "l", col = "tomato",
    main = "Average Arrival Delays per Month", xlab = "Month",
    ylab = "Average Arrival Delay", ylim = c(-10, 20))
   lines(a2 ~ months, type = "l", col = "cadetblue")
   legend("topleft", c(as.character(carrier1), as.character(carrier2)), fill =
      c("tomato", "cadetblue"), bty = "n", cex = 0.9)
 })
 
})

