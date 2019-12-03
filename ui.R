#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Pokemon Data Challenge (2018) - STAT 240"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      #sliderInput("bins",
      #            "Number of bins:",
      #            min = 1,
      #            max = 50,
      #            value = 30)
      ((HTML('<h5>For "Appearances By Weather" Tab 
             and "Locations of Appearances" Tab:</h5>'))),
      selectInput("DateChoice", "Date (YYYY-MM-DD):", 
                  c("2018-02-19", "2018-02-18", "2018-02-17", "2018-02-16",
                    "2018-02-15", "2018-02-14","2018-02-13", "2018-02-12",
                    "2018-02-11", "2018-02-10","2018-02-09","2018-02-08")),
      
      checkboxInput("Gender", strong("Seperate Pokemon by Gender?"), FALSE),
      conditionalPanel(
        condition = "input.Gender == true",
        helpText((HTML("<h3>Select a Gender: </h3>"))),
        ((HTML("<h5>Please be patient while generating. </h5>"))),
        radioButtons("gendersource", "",
                     c(#"All" = "All",
                       "Female" = "Female",
                       "Male" = "Male"
                       #"No Gender" = "None"
                       ))
        ),
      ((HTML('<h5>For "Locations of Appearances" Tab: </h5>'))),
      checkboxInput("Weather", strong("Seperate Appearances by Weather?"), FALSE),
      conditionalPanel(
        condition = "input.Weather == true",
        helpText((HTML("<h3>Select Weather: </h3>"))),
        ((HTML("<h5>Please be patient while generating. </h5>"))),
        radioButtons("weathersource", "",
                     c("Windy" = "Windy","Snow" = "Snow","Rainy" = "Rainy",
                       "Partly Cloudy" = "Partly Cloudy", "None" = "None",
                       "Cloudy" = "Cloudy", "Clear" = "Clear"))
      ),
      
      ((HTML('<h5>For "Attacks VS Defense" and "Colors and Types" Tab: </h5>'))),
      selectInput("ColChoice", "Pokemon Color:", 
                  c("White", "Green", "Red", "Blue", "Brown", "Yellow",
                    "Purple", "Pink", "Grey", "Black")),
      
      ((HTML('<h5>For "Attacks VS Defense" Tab Only: </h5>'))),
      checkboxInput("LinReg",strong("Add Linear Regression Line?"), FALSE),
      checkboxInput("NewP", strong("Add yourself as a Pokemon?"), FALSE),
      
      conditionalPanel(
        condition = "input.NewP == true",   #Note th lowercase logical
        #helpText(HTML("<h5>For Attacks VS Defense Graph Only</h5>")),
        helpText(HTML("<h3>Choose your Attack and Defense levels.</h3>")),
        sliderInput("NewA", "Attack (will not effect regression):",
                    min = 0, max = 165, value = 75, step = 1),
        sliderInput("NewD", "Defense(will not effect regression):", 
                    min = 0, max = 230, value = 120, step = 1))
     ###checkboxInput("Word", strong("Create a Bar Plot for word use by
     ###                             bot account tweets?"),
     ###              FALSE),
     ###conditionalPanel(
     ###  condition = "input.Word == true",
     ###  helpText((HTML("<h3>Select the bot account: </h3>"))),
     ###  #((HTML("<h5>Please be patient while generating. </h5>"))),
     ###  radioButtons("datasource", "",
     ###               c("Vancouver (in BarPlots Tab)" = "Vancouver Pokemon Tweets",
     ###                 "Toronto (in BarPlots Tab)" = "Toronto Pokemon Tweets",
     ###                 "Chicago (in BarPlots Tab)" = "Chicago Pokemon Tweets"))
     ###  #helpText((HTML("<h5>Clouds with words used between 50 and 11000 times </h5>")))
     ###)
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("About", source("About.R")$value()),
                  tabPanel("Appearances By Weather", plotOutput("WeatherPlot")),
                  tabPanel("Locations of Appearances", plotOutput("MapPlot")),
                  tabPanel("Attacks VS Defense", plotOutput("ColorPlot")),
                  tabPanel("Colors and Types", plotOutput("HistPlot"))
                  #tabPanel("Bar Plots", plotOutput("WordPlot"))
                  
      )
    )
  )
))



