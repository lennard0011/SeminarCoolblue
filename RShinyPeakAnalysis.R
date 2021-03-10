# R shiny Peak Analysis -- Seminar Coolblue 2021
# @authors:

library(shiny)
library(lubridate)

# UI 
ui <- shinyUI(fluidPage(
  ## How it looks
  #header
  headerPanel(title = "Insights Dutch commercials Coolblue, first half 2019"),
  #panels
  sidebarLayout(
    #left panel
    sidebarPanel(
      #channel
      selectInput(inputId = "channel", label = "Choose your channel", 
                  choices = c("All", sort(unique(broadNet$channel)))),
      #month
      selectInput(inputId = "month", label = "Choose a month", 
                  choices = c("All", "January", "February", "March", "April", "May", "June") ),
      #hour
      sliderInput(inputId = "hour", label = "Choose range of time: ", 
                  min = 0, max = 24, value = c(0,24)),
      #length_of_spot
      selectInput(inputId = "length_of_spot", label = "Choose length of the spot", 
                  choices = c("All", sort(unique(broadNet$length_of_spot)))),
      #position_in_break_3option
      selectInput(inputId = "position_in_break_3option", label = "Choose position of spot", 
                  choices = c("All", unique(broadNet$position_in_break_3option))),
      #product_category
      selectInput(inputId = "product_category", label = "Choose product category", 
                  choices = c("All", sort(unique(broadNet$product_category)))),
      #Keuze op sorteren
      radioButtons(inputId = "choose_ordering", label = "Choose ordering of data",
                    choices = c("Date", "Gross Rating Point"), inline=T)
    ),
    #right panel
    mainPanel(
      tabsetPanel(type="tab", 
                  tabPanel("Data",tableOutput(outputId="Table")),
                  tabPanel("Summary",verbatimTextOutput(outputId="summ")),
                  tabPanel("Plot",uiOutput(outputId="plot"))
      )
    )
  )
)) 

# SERVER
server <- function(session, input, output) {
  # Create the reaction table
  mtreact <- reactive({
    reactTable = broadNet
    # subset on channel
    if (input$channel != "All") {
      reactTable = subset(reactTable, channel == input$channel)
    }
    # subset on length of spot
    if (input$length_of_spot != "All") {
      reactTable = subset(reactTable, length_of_spot == input$length_of_spot)
    }
    # subset on position in break
    if (input$position_in_break_3option != "All") {
      reactTable = subset(reactTable, position_in_break_3option == input$position_in_break_3option)
    }
    # subset on product category
    if (input$product_category != "All") {
      reactTable = subset(reactTable, product_category == input$product_category)
    }
    # subset on month
    if (input$month != "All") {
      nrMonth = 6
      if (input$month == "January") {
        nrMonth = 1
      } else if (input$month == "February") {
        nrMonth = 2
      } else if (input$month == "March") {
        nrMonth = 3
      } else if (input$month == "April") {
        nrMonth = 4
      } else if (input$month == "May") {
        nrMonth = 5
      }
      reactTable = subset(reactTable, month(date) == nrMonth)
    }
    # put time restriction
    reactTable = subset(reactTable, as.numeric(hours) >= input$hour[1])
    reactTable = subset(reactTable, as.numeric(hours) < input$hour[2])
    if (input$hour[1] == 0 || input$hour[1] == 1) {
      reactTable = subset(reactTable, as.numeric(hours) > input$hour[1])
    }
    # order on date or grp
    if (input$choose_ordering == "Date") {
      reactTable = reactTable[order(reactTable$date, reactTable$time),]
    } else {
      reactTable = reactTable[order(-reactTable$gross_rating_point), ]
    }
    # Output
    reactTable
  })
  
  ## Create the interactiveness
  #observe({
  #  # SUBSET the data
  #  broadNetSub = broadNet
  #  # subset on channel
  #  if (input$channel != "All") {
  #    broadNetSub = subset(broadNetSub, channel == input$channel)
  #  }
  #  # subset on length of spot
  #  if (input$length_of_spot != "All") {
  #    broadNetSub = subset(broadNetSub, length_of_spot == input$length_of_spot)
  #  }
  #  # subset on position in break
  #  if (input$position_in_break_3option != "All") {
  #    broadNetSub = subset(broadNetSub, position_in_break_3option == input$position_in_break_3option)
  #  }
  #  # subset on product category
  #  if (input$product_category != "All") {
  #    broadNetSub = subset(broadNetSub, product_category == input$product_category)
  #  }
  #  # subset on month
  #  if (input$month != "All") {
  #    nrMonth = 6
  #    if (input$month == "January") {
  #      nrMonth = 1
  #    } else if (input$month == "February") {
  #      nrMonth = 2
  #    } else if (input$month == "March") {
  #      nrMonth = 3
  #    } else if (input$month == "April") {
  #      nrMonth = 4
  #    } else if (input$month == "May") {
  #      nrMonth = 5
  #    }
  #    broadNetSub = subset(broadNetSub, month(date) == nrMonth)
  #  }
  #  
  #  # UPDATE the selectors (minute and hours)
  #  updateSelectInput(session, inputId = "length_of_spot", label = "Choose length of the spot",
  #                    choices = c("All", unique(broadNetSub$length_of_spot)))
  #})
  
  
  # Output table
  output$Table <- renderTable({
    outputTable = mtreact()[,c("channel", "date", "time", "gross_rating_point")]
    colnames(outputTable) = c("Channel", "Date", "Time", "Gross Rating Point")
    outputTable
  })
  
  output$summ <- renderText({
    dataT = mtreact()
    
    if (nrow(dataT) == 0) {
      print("No data to summarize!")
    } else {
      # names has to be outside sort!
      fullChannels = gsub(",","\n ", toString(rbind( names(sort(summary(as.factor(dataT$channel)),decreasing=T)),
                                                     sort(summary(as.factor(dataT$channel)),decreasing=T)))
      )
      maxLength = length(unique(dataT$program_before));
      if (maxLength > 6) {
        maxLength = 6
      }
      minLength = 1
      if ( names(sort(summary(as.factor(dataT$program_before)),decreasing=T)[1]) == "(Other)"  ) {
        minLength = 2
      }
      fullPrograms = gsub(",","\n ", toString(rbind( names(sort(summary(as.factor(dataT$program_before)),decreasing=T)[minLength:maxLength]),
                                                     sort(summary(as.factor(dataT$program_before)),decreasing=T)[minLength:maxLength]))
      )
      
            #fullLengthNames = names(sort(summary(as.factor(dataT$length_of_spot)),decreasing=T))
      #fullLengthNumbers = sort(summary(as.factor(dataT$length_of_spot)),decreasing=T)
      
      namesLength = names(sort(summary(as.factor(dataT$length_of_spot)),decreasing=T))
      for (i in 1:length(namesLength)) {
        if (namesLength[i] == "30") {
          namesLength[i] = "'Length 30'"
        } 
        if (namesLength[i] == "30 + 10") {
          namesLength[i] = "'Length 30+10'"
        } 
        if (namesLength[i] == "30 + 10 + 5") {
          namesLength[i] = "'Length 30+10+5'"
        } 
      }
      fullLength = gsub(",","  ", toString(rbind( namesLength,
                                                    sort(summary(as.factor(dataT$length_of_spot)),decreasing=T)))
      )
      
      namesPos = names(sort(summary(as.factor(dataT$position_in_break_3option)),decreasing=T))
      for (i in 1:length(namesPos)) {
        if (namesPos[i] == "begin") {
          namesPos[i] = "Begin"
        } 
        if (namesPos[i] == "middle") {
          namesPos[i] = "Middle"
        } 
        if (namesPos[i] == "end") {
          namesPos[i] = "End"
        } 
      }
      fullPosition = gsub(",","  ", toString(rbind( namesPos,
                                                     sort(summary(as.factor(dataT$position_in_break_3option)),decreasing=T)))
      )
      
      namesProdcat = names(sort(summary(as.factor(dataT$product_category)),decreasing=T))
      for (i in 1:length(namesProdcat)) {
        if (namesProdcat[i] == "televisies") {
          namesProdcat[i] = "TVs"
        } 
        if (namesProdcat[i] == "wasmachines") {
          namesProdcat[i] = "Washing machines"
        } 
        if (namesProdcat[i] == "laptops") {
          namesProdcat[i] = "Laptops"
        } 
      }
      
      fullProdcat = gsub(",","  ", toString(rbind( namesProdcat,
                                                    sort(summary(as.factor(dataT$product_category)),decreasing=T)))
      )
      
      fullGRPNames = c("Minimum:", "  Mean:", "  Maximum: ")
      fullGRPNumbers = summary(dataT$gross_rating_point)[c(1,4,6)]
        
        
      # Do the printing
      print(paste0("Number of commercials: ", nrow(dataT),
                 "\n\nChosen options: \n  Channel: ", input$channel,
                 "\n  Month: ", input$month, "\n  Time interval: between ",
                 input$hour[1], ":00 and ", input$hour[2],
                 ":00 \n  Length of spot: ", input$length_of_spot,
                 "\n  Position in break: ", input$position_in_break_3option,
                 "\n  Product category: ", input$product_category,
                 "\n\nChannels & frequency: \n  ", fullChannels,
                 "\n\nMost frequently broadcasted V programs: \n  ", fullPrograms,
                 "\n\nDistribution length of spot: \n  ", fullLength,
                 "\n\nDistribution position in break: \n  ", fullPosition,
                 "\n\nDistribution product category: \n  ", fullProdcat,
                 "\n\nDistribution of the Gross Rating Point: \n  ", 
                 fullGRPNames[1], " ", round(fullGRPNumbers[1],digits=3), fullGRPNames[2], 
                 " ", round(fullGRPNumbers[2],digits=3), fullGRPNames[3], 
                 round(fullGRPNumbers[3],digits=3)
                 ))
    }
  })
  
  output$plot <- renderUI({
    tableLength = nrow(mtreact())
    plot_output_list <- lapply(1:tableLength, function(i) {
      plotname <- paste("plot", i, sep="") 
      plotOutput(plotname, height = 350, width = 700)
    })
    
    do.call(tagList, plot_output_list)
    
  })
  
  
  for (k in 1:2) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    
    
    local({
      
      plotname <-  paste("plot", k, sep="") 
      
      output[[plotname]] <- renderPlot({
        
        tableInterest = mtreact()
        
        datecommercial <- tableInterest[k,"date"]
        timecommercial <- tableInterest[k,"time"]
        
        traffic_datesub <- subset(visitorsSum,grepl(datecommercial, visitorsSum$date) == TRUE)
        
        timecommercial <- str_split_fixed(timecommercial, ":", 3)
        colnames(timecommercial) <- c("hour", "minute", "seconds")
        timecommercial <- data.frame(timecommercial)
        timecommercial <- 60*as.numeric(timecommercial[1,"hour"]) + as.numeric(timecommercial[1,"minute"]) + 1
        
        interval <- 60
        timeStart <- timecommercial - interval
        timeEinde <- timecommercial + interval
        totalLength <- 2*interval + 1
        visitsVector <- as.matrix(rep(0,totalLength))
        row.names(visitsVector) <- c(seq(from = timeStart, to = timeEinde))
        
        for(j in 1:totalLength){
          visitsVector[j] <- traffic_datesub[(timeStart + j), "visitsWebNet"]
        }
        xlim = c(interval-10, interval + 10)
        plot(visitsVector, type = "l", xlim = xlim,  main = paste("Website visits (NL) commercial at",tableInterest[k,"date"], "with GDP", tableInterest[k,"gross_rating_point"]),  
             xlab = "Time (minutes)", ylab = "Visits Ratio")
        abline(v = interval + 1, col = "blue")
      })
    })
  }
}

shinyApp(ui=ui, server=server)

# TODO: dates in Dutch way 
# TODO: summary tab
# TODO: plot tab
