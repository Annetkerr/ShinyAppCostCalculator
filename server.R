library(shiny)


# Define Server for Cost Estimator Shiny Application
# Author: Anne Kerr
# Class: Developing Data Products
# Assignment 1

# This application calculates the estimated costs two methods of implementing a specific software
# hosting solution. A company is willing to subsidize the cost to implement for each of its branch
# offices, but wants to look at the cost options for various combinations of standalone and shared 
# implementations. This application allows the user to input several assumtion factors and, based
# on those factors, evaluate the cost of the various options. The assumption factors are input as 
# integers and the number of each type of implementation to evaluate is presented on a slider. As 
# as the user manipulates the slider, the given assumptions are taken into account and the estimated
# costs for the specified level are displayed in an output table.

shinyServer(function(input, output) {
  
  # Reactive expression to calculate the cost estimates based on user input, and to compose a 
  # data frame for display
  sliderValues <- reactive({
     nSa <- input$nStandalone
     nSh <- input$nShared
     
     #percent main office pays for shared support
     sss <- input$sharedsupportsubsidy
     
     
     #overhead per site to support standardization in standalone sites
     saoh <- (input$sasupportfactor * 1000)
     
     #overhead per site to support standardization in shared sites
     aaoh <- (input$aasupportfactor * 1000)
     
     
     
     costsa <- format(((nSa * 5622) + (nSa * saoh)), big.mark=",", scientific=FALSE)
     if(nSh < 10) {
      costsh <- (2 * 93000)
     }  else {
      if (nSh > 25) {
        costsh <- (4 * 93000)
      } else {
         costsh <- (3 * 93000)
      }
     }
     

  
     #factor in annual software site cost $5562 and percent assumed by main office for shared systems admin support
     if (sss > 0) {
       costsh <- ((costsh + 5622)*sss/100)
     } else {
       costsh <- 5622
     }
     
     #add assumed overhead for standardization per office
     costsh <- costsh + (nSh*aaoh)
     costsh <- format((costsh), big.mark=",", scientific=FALSE)
     
 
    # Compose data frame
    df <- data.frame(
      Option = c("Standalone", 
               "Shared"),
      Count = as.character(c(input$nStandalone, 
                             input$nShared)), 
      cost = as.character(c(paste("$",costsa),paste("$",costsh))),
      stringsAsFactors=FALSE)
  }) 
  
  print(df)
  
  # Show the values using an HTML table
  output$values <- renderTable({
    sliderValues()
  })
})
