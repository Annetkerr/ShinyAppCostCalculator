library(shiny)

# Define UI for Cost Estimator Shiny Application
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

shinyUI(fluidPage(
  
  #  Application title
  titlePanel("Cost Estimator"),
  
  # Create a Sidebar layout with integer and slider to get the assumptions to 
  # use for estimating costs
  sidebarLayout(
    sidebarPanel(
      
        
      # Input Number of Branches Using Standalone site
      sliderInput("nStandalone", "Number of Standalone sites:", 
                  min=0, max=33, value=10),

      # Input per site annual cost assumption (in thousand of dollars) main office will incur 
      # per standardization ins standalone sites
      sliderInput("sasupportfactor", "Assumed annual cost of standardization across standalone sites: ($K)", 
                  min=0, max=60, value=30),
      
      
      # INput Number of Branches Using shared site
      sliderInput("nShared", "Number offices using shared site:", 
                  min=0, max=33, value=10),
      
      # Input per site annual cost assumption (in thousand of dollars) main office will incur 
      # supporting standardization in a shared site
      sliderInput("aasupportfactor", "Assumed annual cost of standarization in shared site: ($K)", 
                  min=0, max=50, value=25),
      
      # Input assumed percent of shared hosting paid by main office 
      sliderInput("sharedsupportsubsidy", "Assumed percent shared Systems Admin paid by main office: (%)", 
                  min=0, max=100, value=50)

    ),
    
    # Show a table summarizing the values entered
    mainPanel(
      h5("What is this all going to cost the main office annually?"),
      tableOutput("values")
#       h5("What is this going to cost the main office in one-time fees?")
    )
  )
))
