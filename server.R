#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
#install.packages("sp")
#library(sp)
#install.packages("rworldmap")
#library(rworldmap) 
#install.packages("rworldxtra")
#library(rworldxtra) 

source("checkpackages.R")
checkpackages("sp")
checkpackages("rworldmap")
checkpackages("rworldxtra")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$ColorPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    #x  <- faithful[, 2] 
    poke <- read.csv("pokemon_2018.csv", header = TRUE)
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    #draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    plot(poke$Attack, poke$Defense, xlab = "Attack", ylab = "Defense",
         xlim = c(min(poke$Attack),max(poke$Attack)), 
         ylim = c(min(poke$Defense),max(poke$Defense)),
         type = "n", main = "Pokemon Attacks VS Defense")
    if(input$NewP){
      points(y = input$NewD, x = input$NewA, pch = 8, cex = 2)
    }
    
    #if(input$DensityLogical){
    #  DENS = density(x,from=input$xmin)
    #  lines(DENS$x,DENS$y*2)
    #}
    
    if(input$ColChoice == "White"){
      pokecol = poke[poke[,"Color"] == "White",]
      pcA = pokecol[,"Attack"]
      pcD = pokecol[,"Defense"]
      points(pcA,pcD)
    }
    if(input$ColChoice == "Green"){
      pokecol = poke[poke[,"Color"] == "Green",]
      pcA = pokecol[,"Attack"]
      pcD = pokecol[,"Defense"]
      points(pcA,pcD, col = "green", pch = 16)
    }
    if(input$ColChoice == "Red"){
      pokecol = poke[poke[,"Color"] == "Red",]
      pcA = pokecol[,"Attack"]
      pcD = pokecol[,"Defense"]
      points(pcA,pcD, col = "red", pch = 16)
    }
    if(input$ColChoice == "Blue"){
      pokecol = poke[poke[,"Color"] == "Blue",]
      pcA = pokecol[,"Attack"]
      pcD = pokecol[,"Defense"]
      points(pcA,pcD, col = "blue", pch = 16)
    }
    if(input$ColChoice == "Brown"){
      pokecol = poke[poke[,"Color"] == "Brown",]
      pcA = pokecol[,"Attack"]
      pcD = pokecol[,"Defense"]
      points(pcA,pcD, col = "brown", pch = 16)
    }
    if(input$ColChoice == "Yellow"){
      pokecol = poke[poke[,"Color"] == "Yellow",]
      pcA = pokecol[,"Attack"]
      pcD = pokecol[,"Defense"]
      points(pcA,pcD, pch = 16, col = "yellow")
    }
    if(input$ColChoice == "Purple"){
      pokecol = poke[poke[,"Color"] == "Purple",]
      pcA = pokecol[,"Attack"]
      pcD = pokecol[,"Defense"]
      points(pcA,pcD, col = "purple", pch = 16)
    }
    if(input$ColChoice == "Pink"){
      pokecol = poke[poke[,"Color"] == "Pink",]
      pcA = pokecol[,"Attack"]
      pcD = pokecol[,"Defense"]
      points(pcA,pcD, col = "deeppink", pch = 16)
    }
    if(input$ColChoice == "Grey"){
      pokecol = poke[poke[,"Color"] == "Grey",]
      pcA = pokecol[,"Attack"]
      pcD = pokecol[,"Defense"]
      points(pcA,pcD, col = "grey", pch = 16)
    }
    if(input$ColChoice == "Black"){
      pokecol = poke[poke[,"Color"] == "Black",]
      pcA = pokecol[,"Attack"]
      pcD = pokecol[,"Defense"]
      points(pcA,pcD, pch = 16)
    }
    #poke[poke[,"Color"] == ColChoice,"Attack"]
    if(input$LinReg){
      abline(lm(poke[poke[,"Color"] == input$ColChoice,"Attack"]~
                  poke[poke[,"Color"] == input$ColChoice,"Defense"]), lwd = 2)
    }
    
  })
  
  output$HistPlot <- renderPlot({
    
    poke <- read.csv("pokemon_2018.csv", header = TRUE)
    #draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    plot(poke$Attack, poke$Defense, xlab = "Attack", ylab = "Defense",
         xlim = c(min(poke$Attack),max(poke$Attack)), 
         ylim = c(min(poke$Defense),max(poke$Defense)),
         type = "n", main = "Pokemone Attacks VS Defense")
    
    #if(input$DensityLogical){
    #  DENS = density(x,from=input$xmin)
    #  lines(DENS$x,DENS$y*2)
    #}
    
    if(input$ColChoice == "White"){
      plot(poke[poke[,"Color"] == "White","Type_1"], las = 2, col = "white",
           main = "Types In Color Chosen", ylab = "Count")
    }
    if(input$ColChoice == "Green"){
      plot(poke[poke[,"Color"] == "Green","Type_1"], las = 2, col = "green",
           main = "Types In Color Chosen", ylab = "Count")
    }
    if(input$ColChoice == "Red"){
      plot(poke[poke[,"Color"] == "Red","Type_1"], las = 2, col = "red",
           main = "Types In Color Chosen", ylab = "Count")
    }
    if(input$ColChoice == "Blue"){
      plot(poke[poke[,"Color"] == "Blue","Type_1"], las = 2, col = "blue",
           main = "Types In Color Chosen", ylab = "Count")
    }
    if(input$ColChoice == "Brown"){
      plot(poke[poke[,"Color"] == "Brown","Type_1"], las = 2, col = "brown",
           main = "Types In Color Chosen", ylab = "Count")
    }
    if(input$ColChoice == "Yellow"){
      plot(poke[poke[,"Color"] == "Yellow","Type_1"], las = 2, col = "yellow",
           main = "Types In Color Chosen", ylab = "Count")
    }
    if(input$ColChoice == "Purple"){
      plot(poke[poke[,"Color"] == "Purple","Type_1"], las = 2, col = "purple",
           main = "Types In Color Chosen", ylab = "Count")
    }
    if(input$ColChoice == "Pink"){
      plot(poke[poke[,"Color"] == "Pink","Type_1"], las = 2, col = "deeppink",
           main = "Types In Color Chosen", ylab = "Count")
    }
    if(input$ColChoice == "Grey"){
      plot(poke[poke[,"Color"] == "Grey","Type_1"], las = 2, col = "grey",
           main = "Types In Color Chosen", ylab = "Count")
    }
    if(input$ColChoice == "Black"){
      plot(poke[poke[,"Color"] == "Black","Type_1"], las = 2, col = "black",
           main = "Types In Color Chosen", ylab = "Count")
    }
  })
  
  output$WordPlot <- renderPlot({
    
    if(input$datasource == "Vancouver Pokemon Tweets"){
      #load("Vancouver Pokemon Tweets.Rdata")
      #x = UniqueAllUserTweets$text
      x = read.csv("VancouverWords.csv", header = T)
    }else{
      if(input$datasource == "Toronto Pokemon Tweets"){
        #load("Toronto Pokemon Tweets.Rdata")
        #x = UniqueAllUserTweets$text
        x = read.csv("TorontoWords.csv", header = T)
      }else{
        if(input$datasource == "Chicago Pokemon Tweets"){
          #load("Chicago Pokemon Tweets.Rdata")
          #x = UniqueAllUserTweets$text
          x = read.csv("ChicagoWords.csv", header = T)
        }else{
          x = NULL
        }
      }
    }
    barplot(x[,2], names.arg = x[,1], main = "Barplot of Word Counts in Tweets 
            for Words Used Between 50 and 11000 Times", las = 2, cex.names = 0.75,
            ylab = "Count")
    #sentence2 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", x)
    #sentence3 = gsub("@\\w+", " ", sentence2)
    #sentence4 = gsub("(?!')[[:punct:]]", "", sentence3, perl = T)
    #sentence5 = gsub("[[:cntrl:]]", "", sentence4)
    #sentence6 = gsub("[[:digit:]]", "", sentence5)
    #sentence7 = iconv(sentence6,"UTF-8", "ASCII",  sub = "")
    #sentence8 = tolower(sentence7)
    #sentence9 = gsub("http\\w+", "", sentence8)
    #sentence10 = gsub("[ \t]{2,}", " ", sentence9) 
    #sentence11 = gsub("^\\s+|\\s+$", "", sentence10)
    #word.list = strsplit(sentence11, " ")
    #words = unlist(word.list)
    #words = words[!words %in% tm::stopwords(kind = "english")]
    #words2 = table(words)
    #words2 = words2[words2 > 50]
    #words2 = words2[words2 < 11000]
    #library(wordcloud)
    #wordcloud(names(words2), words2, min.freq = 1, 
    #          colors = rainbow(32), random.order = FALSE)
    
  })
  
  output$MapPlot <- renderPlot({
    
    data = read.csv("Vancouver Discord Data.csv")
    data = data[data[,"date"] == input$DateChoice,]
    worldmap = getMap(resolution = "high") 
    NrthAm = worldmap[which (worldmap$REGION == "North America"), ]
    plot(NrthAm, col = "green", bg = "lightblue", xlim = c(-123, -122.95),
         ylim = c(49, 49.5), main = "Map of Vancouver BC and Area")
    points(y = data$latitude, x = data$longitude, col = "red",cex = 0.5,pch = 7)
    
    
    ##if(input$Gender == TRUE){
    ##  fem = data[data[,"gender"] == "Female",]
    ##  mal = data = data[data[,"gender"] == "Male",]
    ##  non = data[data[,"gender"] == "None",]
    ##  plot(NrthAm, col = "green", bg = "lightblue", xlim = c(-123, -122.75),
    ##       ylim = c(49, 49.5))
    ##  points(y = fem$latitude, x = fem$longitude, col = "deeppink",
    ##         cex = 0.5,pch = 7)
    ##  points(y = mal$latitude, x = mal$longitude, col = "blue",
    ##         cex = 0.5,pch = 7)
    ##  points(y = non$latitude, x = non$longitude, col = "grey",
    ##         cex = 0.5,pch = 7)
    ##}
    if(input$Gender == TRUE){
      #if(input$gendersource == "All"){
      #  plot(NrthAm, col = "green", bg = "lightblue", xlim = c(-123, -122.95),
      #       ylim = c(49, 49.5),main = "Map of Vancouver BC and Area")
      #  points(y = data$latitude, x = data$longitude, col = "red",cex = 0.5,pch = 7)
      #}
      if(input$gendersource == "Female"){
        data = data[data[,"gender"] == "Female",]
        plot(NrthAm, col = "green", bg = "lightblue", xlim = c(-123, -122.95),
             ylim = c(49, 49.5),main = "Map of Vancouver BC and Area")
        points(y = data$latitude, x = data$longitude, col = "deeppink",
               cex = 0.5,pch = 7)
      }else{
        if(input$gendersource == "Male"){
          data = data[data[,"gender"] == "Male",]
          plot(NrthAm, col = "green", bg = "lightblue", xlim = c(-123, -122.95),
               ylim = c(49, 49.5),main = "Map of Vancouver BC and Area")
          points(y = data$latitude, x = data$longitude, col = "blue",
                 cex = 0.5,pch = 7)
        }else{
          ##if(input$gendersource == "None"){
          ##  data = data[data[,"gender"] == "None",]
          ##  plot(NrthAm, col = "green", bg = "lightblue", xlim = c(-123, -122.95),
          ##       ylim = c(49, 49.5),main = "Map of Vancouver BC and Area")
          ##  points(y = data$latitude, x = data$longitude, col = "grey",
          ##         cex = 0.5,pch = 7)
          ##}else{
            data <- NULL
          }
        }
      }
    
    
    if(input$Weather == TRUE){
        data = data[data[,"weather"] == input$weathersource,]
        plot(NrthAm, col = "green", bg = "lightblue", xlim = c(-123, -122.95),
             ylim = c(49, 49.5),main = "Map of Vancouver BC and Area")
        points(y = data$latitude, x = data$longitude, col = "Purple",
               cex = 0.5,pch = 7)
    }
  })
  
  output$WeatherPlot <- renderPlot({
    data = read.csv("Vancouver Discord Data.csv")
    data = data[data[,"date"] == input$DateChoice,]
    barplot(table(data$weather), las = 2, cex.names = 0.75,
            main = "Number of Appearances in Each Weather Condition in Vancouver",
            ylab = "Number of Appearances")
    
    if(input$Gender == TRUE){
      data = data[data[,"gender"] == input$gendersource,]
      barplot(table(data$weather), las = 2, cex.names = 0.75,
              main = "Number of Appearances in Each Weather Condition in Vancouver",
              ylab = "Number of Appearances", col = "purple")
    }
  })
  
 #output$image1 <- renderImage({
 #  # Get width and height of image1
 #  width  <- session$clientData$output_image1_width
 #  height <- session$clientData$output_image1_height
 #  
 #  # A temp file to save the output.
 #  # This file will be automatically removed later by
 #  # renderImage, because of the deleteFile=TRUE argument.
 #  outfile <- tempfile(fileext = "20171111_145611.jpg")
 #})
  
})
