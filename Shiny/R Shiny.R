# R shiny + Tutorial -- Seminar Coolblue 2021

# TEMPLATE
# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("shinyWidgets")
# install.packages("stringr")
# install.packages("gridExtra")
library("shiny")
library("shinydashboard")
library("shinyWidgets")
library("stringr")
library("lubridate")
library("stringr")
library("gridExtra")

# Load trained model and data
load("fullModelSaved.rda")
load("testdf.rda")
load("broadNet.rda")
load("visitorsSum.rda")

# User Interface
ui = dashboardPage(
  dashboardHeader(title = "The effects of TV-commercials", titleWidth = 350),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dash", icon = icon("dashboard")),
      menuItem("About Us", tabName = "au"),
      menuItem("Data Exploration", tabName = "pa"),
      menuItem("Direct Effects", tabName = "de")
    )
  ),
  
  # Colours
  dashboardBody(
    tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #fe6c16;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #fe6c16;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #fe6c16;
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #fe6c16;
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #1daaef;
                                }

                                '))),
    
    # Different tabs
    tabItems(
      tabItem("dash",
              column(width = 12,
                     tags$img(height = 625,
                              width = 625, style = "display: block; margin-left: auto; margin-right: auto;",
                              src = "coollogo.png") 
              ) 
      ),
      tabItem("au",
              headerPanel(title = "About Us"),
              column(width = 12,
                     box(width = 12, align = "center",
                         "Welcome to our dashboard! We are four students of Business Analytics and Quantitative 
                      Marketing at the Erasmus University in Rotterdam. For the past two months, we researched the 
                      effects of TV commercials on website traffic for the e-commerce company Coolblue. We want to make 
                      these results more accessible using this application. In the tab 'Data Exploration', you can find 
                      information on all commercials that were broadcast in the first half of 2019, given certain criteria.
                      In the tab 'Direct Effects', you can again fill in certain criteria for a broadcast. The application
                      then tells you what the expected absolute increase in visitors would be. Thank you for expressing
                      your interest in our research!"
                     ),
              )
      ),
      tabItem("pa",
              headerPanel(title = "Insights into Dutch commercials of Coolblue (2019)"),
              
              fluidRow(
                box(width = 6,
                    title = "Commercial-specific effects", 
                    # Channel
                    selectInput(inputId = "channel", label = "Choose your channel", 
                                choices = c("All", as.character(sort(unique(broadNet$channel))))),
                    # Month
                    selectInput(inputId = "month", label = "Choose a month", 
                                choices = c("All", "January", "February", "March", "April", "May", "June")),
                    # Hour
                    sliderInput(inputId = "timer", label = "Choose time range: ", 
                                min = 0, max = 24, value = c(0, 24)),
                    # Length of spot
                    selectInput(inputId = "length_of_spot", label = "Choose the spot length", 
                                choices = c("All", as.character(sort(unique(broadNet$length_of_spot))))),
                    # Position in break
                    selectInput(inputId = "position_in_break_3option", label = "Choose the position in break", 
                                choices = c("All", "Begin", "Middle", "End")),
                    # Product category
                    selectInput(inputId = "product_category", label = "Choose product category", 
                                choices = c("All", "Washing machines", "Televisions", "Laptops")),
                    # Choose sort
                    radioButtons(inputId = "choose_ordering", label = "Choose ordering of data",
                                 choices = c("Date", "Gross Rating Point"), inline = T)
                ),
                box(width = 6,
                    tabsetPanel(type = "tab", 
                                tabPanel("Data", tableOutput(outputId = "Table")),
                                tabPanel("Summary", verbatimTextOutput(outputId = "summ")),
                                tabPanel("Plot", uiOutput(outputId = "plot"))
                    )
                ),
              )
      ),
      tabItem("de",
              headerPanel(title = "Insights into direct effects of Coolblue commercials"),
              fluidRow(
                column(width = 6,
                       box(width = 12,
                           title = "Commercial-specific effects", 
                           # Channels
                           selectInput(inputId = "channels", label = "Choose your channel", choices = c(as.character(sort(unique(broadNet$channel))), "Slam!TV")),
                           # GRP
                           sliderInput(inputId = "GRP", label = "Input Gross Rating Point", value = 0, min = 0, max = 7.5, step = 0.05),
                           textOutput(outputId = "warning"), tags$head(tags$style("#warning{color: red;
                                 }")),
                           # Day of the week
                           selectInput(inputId = "weekday", label = "Choose day of the week",
                                       choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
                           # Hour
                           sliderInput(inputId = "hour", label = "Choose broadcast time", value = 12, min = 0, max = 23)
                       ),
                       box(width = 12,
                           title = "Maximum amount of minutely visitors",
                           numericInput(inputId = "maxVD", label = "Maximum amount of minutely visitors", value = 1000, min = 0, max = 5000, step = 10)
                       )
                ),
                column(width = 6,
                       box(width = 12,
                           title = "Commercial-specific effects",
                           # Length of spot
                           radioButtons(inputId = "length_spot", label = "Choose the spot length", selected = "30",
                                        choices = c("30", "30 + 10", "30 + 10 + 5")),
                           # Position in break
                           radioButtons(inputId = "pos_break", label = "Choose the position in break", selected = "Begin",
                                        choices = c("Begin", "Middle", "End")),
                           # Product category
                           radioButtons(inputId = "prod_category", label = "Choose product category", selected = "Washing machines",
                                        choices = c("Washing machines", "Televisions", "Laptops"))
                       ),
                       box(width = 12,
                           title = "Extra amount of visitors",
                           textOutput(outputId = "text") 
                       )
                )
              )
      )
    )
  )
)

# Server
server = function(session, input, output){
  mtreact = reactive({
    reactTable = broadNet
    # Subset on channel
    if(input$channel != "All"){
      reactTable = subset(reactTable, channel == input$channel)
    }
    # Subset on length of spot
    if(input$length_of_spot != "All"){
      reactTable = subset(reactTable, length_of_spot == input$length_of_spot)
    }
    # Subset on position in break
    if(input$position_in_break_3option != "All"){
      if(input$position_in_break_3option == "Begin"){
        reactTable = subset(reactTable, position_in_break_3option == "begin") 
      }
      if(input$position_in_break_3option == "Middle"){
        reactTable = subset(reactTable, position_in_break_3option == "middle") 
      }
      if(input$position_in_break_3option == "End"){
        reactTable = subset(reactTable, position_in_break_3option == "end") 
      }
    }
    # Subset on product category
    if(input$product_category != "All"){
      if(input$product_category == "Washing machines"){
        reactTable = subset(reactTable, product_category == "wasmachines")
      }
      if(input$product_category == "Televisions"){
        reactTable = subset(reactTable, product_category == "televisies")
      }
      if(input$product_category == "Laptops"){
        reactTable = subset(reactTable, product_category == "laptops")
      }
    }
    # Subset on month
    if(input$month != "All"){
      nrMonth = 6
      if(input$month == "January"){
        nrMonth = 1
      } else if(input$month == "February"){
        nrMonth = 2
      } else if(input$month == "March"){
        nrMonth = 3
      } else if(input$month == "April"){
        nrMonth = 4
      } else if(input$month == "May"){
        nrMonth = 5
      }
      reactTable = subset(reactTable, month(date) == nrMonth)
    }
    # Put time restriction
    if(input$timer[1] == 0 || input$timer[1] == 1){
      reactTable = subset(reactTable, as.numeric(hours) > input$timer[1])
      reactTable = subset(reactTable, as.numeric(hours) <= input$timer[2])
    } else {
      reactTable = subset(reactTable, as.numeric(hours) >= input$timer[1])
      reactTable = subset(reactTable, as.numeric(hours) < input$timer[2])
    }
    # Order on date or GRP
    if(input$choose_ordering == "Date"){
      reactTable = reactTable[order(reactTable$date, reactTable$time), ]
    } else {
      reactTable = reactTable[order(- reactTable$gross_rating_point), ]
    }
    return(reactTable)
  })
  
  # Output table
  output$Table = renderTable({
    if(nrow(mtreact()) == 0){
      print("There is no data available!")
    } else {
      outputTable = mtreact()[, c("channel", "date", "time", "gross_rating_point")]
      colnames(outputTable) = c("Channel", "Date", "Time", "Gross Rating Point")
      outputTable
    }
  })
  
  # Output summary
  output$summ = renderText({
    dataT = mtreact()
    
    if(nrow(dataT) == 0){
      print("There is no data to summarize!")
    } else {
      fullChannels = gsub(",","\n ", toString(rbind(names(sort(summary(as.factor(dataT$channel)), decreasing = T)),
                                                    sort(summary(as.factor(dataT$channel)), decreasing = T)))
      )
      maxLength = length(unique(dataT$program_before))
      if(maxLength > 6){
        maxLength = 6
      }
      minLength = 1
      if(names(sort(summary(as.factor(dataT$program_before)), decreasing = T)[1]) == "(Other)"){
        minLength = 2
      }
      fullPrograms = gsub(",","\n ", toString(rbind(names(sort(summary(as.factor(dataT$program_before)), decreasing = T)[minLength:maxLength]),
                                                    sort(summary(as.factor(dataT$program_before)), decreasing = T)[minLength:maxLength]))
      )
      namesLength = names(sort(summary(as.factor(dataT$length_of_spot)), decreasing = T))
      for (i in 1:length(namesLength)){
        if(namesLength[i] == "30"){
          namesLength[i] = "'Length 30'"
        } 
        if(namesLength[i] == "30 + 10"){
          namesLength[i] = "'Length 30+10'"
        } 
        if(namesLength[i] == "30 + 10 + 5"){
          namesLength[i] = "'Length 30+10+5'"
        } 
      }
      fullLength = gsub(",","  ", toString(rbind(namesLength,
                                                 sort(summary(as.factor(dataT$length_of_spot)), decreasing = T)))
      )
      
      namesPos = names(sort(summary(as.factor(dataT$position_in_break_3option)), decreasing = T))
      for (i in 1:length(namesPos)){
        if(namesPos[i] == "begin"){
          namesPos[i] = "Begin"
        } 
        if(namesPos[i] == "middle"){
          namesPos[i] = "Middle"
        } 
        if(namesPos[i] == "end"){
          namesPos[i] = "End"
        } 
      }
      fullPosition = gsub(",","  ", toString(rbind(namesPos,
                                                   sort(summary(as.factor(dataT$position_in_break_3option)), decreasing = T)))
      )
      
      namesProdcat = names(sort(summary(as.factor(dataT$product_category)), decreasing = T))
      for (i in 1:length(namesProdcat)){
        if(namesProdcat[i] == "televisies"){
          namesProdcat[i] = "Televisions"
        } 
        if(namesProdcat[i] == "wasmachines"){
          namesProdcat[i] = "Washing machines"
        } 
        if(namesProdcat[i] == "laptops"){
          namesProdcat[i] = "Laptops"
        } 
      }
      
      fullProdcat = gsub(",","  ", toString(rbind( namesProdcat,
                                                   sort(summary(as.factor(dataT$product_category)), decreasing = T)))
      )
      
      fullGRPNames = c("Minimum:", "  Mean:", "  Maximum: ")
      fullGRPNumbers = summary(dataT$gross_rating_point)[c(1, 4, 6)]
      
      
      # Print
      print(paste0("Number of commercials: ", nrow(dataT),
                   "\n\nChosen options: \n  Channel: ", input$channel,
                   "\n  Month: ", input$month, "\n  Time interval: between ",
                   input$timer[1], ":00 and ", input$timer[2],
                   ":00 \n  Length of spot: ", input$length_of_spot,
                   "\n  Position in break: ", input$position_in_break_3option,
                   "\n  Product category: ", input$product_category,
                   "\n\nChannels & frequency: \n  ", fullChannels,
                   "\n\nMost frequently broadcasted TV programs: \n  ", fullPrograms,
                   "\n\nDistribution length of spot: \n  ", fullLength,
                   "\n\nDistribution position in break: \n  ", fullPosition,
                   "\n\nDistribution product category: \n  ", fullProdcat,
                   "\n\nDistribution of the Gross Rating Point: \n  ", 
                   
                   fullGRPNames[1], " ", round(fullGRPNumbers[1], digits = 3), fullGRPNames[2], 
                   " ", round(fullGRPNumbers[2], digits = 3), fullGRPNames[3], 
                   round(fullGRPNumbers[3], digits = 3)
      ))
    }
  })
  
  # Output plot
  output$plot = renderUI({
    tableLength = nrow(mtreact())
    if(tableLength > 0){
      if(tableLength > 10){
        tableLength = 10
      } 
      plot_output_list = lapply(1:tableLength, function(i){
        plotname = paste("plot", i, sep = "") 
        plotOutput(plotname, height = 50, width = 100)
        tableInterest = mtreact()
        output[[plotname]] = renderPlot({
          datecommercial = tableInterest[i, "date"]
          timecommercial = tableInterest[i, "time"]
          
          traffic_datesub = subset(visitorsSum,grepl(datecommercial, visitorsSum$date) == T)
          timecommercial = str_split_fixed(timecommercial, ":", 3)
          colnames(timecommercial) = c("hour", "minute", "seconds")
          timecommercial = data.frame(timecommercial)
          timecommercial = 60 * as.numeric(timecommercial[1, "hour"]) + as.numeric(timecommercial[1, "minute"]) + 1
          
          if(timecommercial > 9 & timecommercial < 1431){
            interval = 10
            timeStart = timecommercial - interval
            timeEinde = timecommercial + interval
            totalLength = 2 * interval + 1
            visitsVector = as.matrix(rep(0, totalLength))
            row.names(visitsVector) = c(seq(from = -10, to = 10))
            
            for(j in 1:totalLength){
              visitsVector[j] = traffic_datesub[(timeStart + j - 1), "visitsWebNet"]
            }
            
            plot(visitsVector, type = "l", xaxt = "n", main = c(paste("Commercial at", tableInterest[i, "channel"], "on date", tableInterest[i, "date"], tableInterest[i, "time"]),
                                                                paste("with GRP", tableInterest[i, "gross_rating_point"])),  
                 xlab = "Time (minutes)", ylab = "Visits Density")
            axis(side = 1, at = c(1, 11, 21), 
                 labels= c('-10', '0', '10'))
            abline(v = interval + 1, col = "blue")
            
            maxie = max(visitsVector[(11 + 1):(11 + 5)]) - visitsVector[11]
            maxie_per = 100 * (max(visitsVector[(11 + 1):(11 + 5)]) - visitsVector[11])/visitsVector[11]
            legend(x = "topleft", legend = c(paste0("VD increase in 5min: "), paste0(round(maxie, digits = 2), " (", round(maxie_per, digits = 2), "%)")), cex = 0.75)
          } else {
            if(timecommercial < 10){
              interval = 10
              timeStart = 1
              timeEinde = timecommercial + interval
              totalLength = timeEinde 
              visitsVector = as.matrix(rep(0, totalLength))
              row.names(visitsVector) = c(seq(from = (- timecommercial + 1), to = 10))
              
              for(j in 1:totalLength){
                visitsVector[j] = traffic_datesub[(timeStart + j - 1), "visitsWebNet"]
              }
              x = (- timecommercial + 1)
              plot(visitsVector, type = "l", xaxt = "n", main = c(paste("Commercial at", tableInterest[i, "channel"], "on date", tableInterest[i, "date"], tableInterest[i, "time"]),
                                                                  paste("with GRP", tableInterest[i, "gross_rating_point"])),  
                   xlab = "Time (minutes)", ylab = "Visit Density")
              axis(side = 1, at = c(1, timecommercial, timecommercial + 10), 
                   labels= c(x, '0', '10'))
              
              abline(v = timecommercial, col = "blue")
              
              maxie = max(visitsVector[(timecommercial + 1):(timecommercial + 5)]) - visitsVector[timecommercial]
              maxie_per = 100 * (max(visitsVector[(timecommercial + 1):(timecommercial + 5)]) - visitsVector[timecommercial])/visitsVector[timecommercial]
              legend(x = "topleft", legend = c(paste0("VD increase in 5min: "), paste0(round(maxie, digits = 2), " (", round(maxie_per,digits = 2), "%)")), cex = 0.75)
              
            }   else {
              interval = 10
              timeStart = timecommercial - interval
              timeEinde = 1439
              totalLength = 1439 - timecommercial + interval + 1
              visitsVector = as.matrix(rep(0, totalLength))
              row.names(visitsVector) = c(seq(from = -10, to = (1439 - timecommercial)))
              
              for(j in 1:totalLength){
                visitsVector[j] = traffic_datesub[(timeStart + j - 1), "visitsWebNet"]
              }
              x = 1439 - timecommercial
              plot(visitsVector, type = "l", xaxt = "n", main = c(paste("Commercial at", tableInterest[i, "channel"], "on date", tableInterest[i, "date"], tableInterest[i, "time"]),
                                                                  paste("with GRP", tableInterest[i, "gross_rating_point"])),  
                   xlab = "Time (minutes)", ylab = "Visit Density")
              axis(side = 1, at=c(1, 11, totalLength), 
                   labels= c('-10', '0', x))
              
              abline(v = interval + 1, col = "blue")
              
              maxie = max(visitsVector[(11 + 1):(11 + x)]) - visitsVector[11]
              maxie_per = 100 * (max(visitsVector[(11 + 1):(11 + x)]) - visitsVector[11])/visitsVector[11]
              legend(x = "topleft", legend = c(paste0("VD increase in 5min: "), paste0(round(maxie, digits = 2), " (", round(maxie_per, digits = 2), "%)")), cex = 0.75)
            }
          }
        })
      })
      
      do.call(tagList, plot_output_list)
    }
    else{
      print("There are no plots available!")
    }
    
  })
  
  
  newCoefficients = reactive({
    # Make copy of dataframe to fill in with data
    currentdf = testdf
    
    # Hours
    if(input$hour == 1 | input$hour >= 6){
      currentdf$hours = input$hour
    } else {
      currentdf$hours = 1
    }
    
    # GRP
    currentdf$gross_rating_point = input$GRP
    
    # Product Category
    if(input$prod_category == 'Laptops'){
      currentdf$product_category_laptops = 1
    }
    else if(input$prod_category == 'Washing machines'){
      currentdf$product_category_wasmachines = 1
    }
    
    # Channel
    for (i in 7:34){
      rowname = names(testdf)[i]
      if(substring(rowname, 1, 1) == "`"){
        n = nchar(rowname)
        chan = substring(rowname, 10, n - 1)
      } else {
        chan = substring(rowname, 9)
      }
      if(chan == input$channels){
        currentdf[i] = 1
        break
      }
    }
    
    # Length of spot
    if(input$length_spot == 30){
      currentdf$length_of_spot_30 = 1
    }
    else if(input$length_spot == '30 + 10'){
      currentdf$`length_of_spot_30 + 10` = 1
    }
    
    # Position in break
    if(input$pos_break == 'Begin'){
      currentdf$position_in_break_3option_begin = 1
    }
    else if(input$pos_break == 'End'){
      currentdf$position_in_break_3option_end = 1
    }
    
    # Weekday
    weekDay = matrix(0, 6)
    if(input$weekday == 'Tuesday'){
      currentdf$weekdays_dinsdag = 1
    }
    else if(input$weekday == 'Wednesday'){
      currentdf$weekdays_woensdag = 1
    }
    else if(input$weekday == 'Thursday'){
      currentdf$weekdays_donderdag = 1
    }
    else if(input$weekday == 'Friday'){
      currentdf$weekdays_vrijdag = 1
    }
    else if(input$weekday == 'Sunday'){
      currentdf$weekdays_zondag = 1
    }
    else if(input$weekday == 'Monday'){
      currentdf$weekdays_maandag = 1
    }
    
    currentdf["hours"] = as.factor(currentdf["hours"])
    return(predict(fullModelTest, currentdf, interval = "prediction", level = 0.95))
  })
  # Output expected increase
  output$text = renderText({
    if(input$channels == "Slam!TV"){
      paste0("We cannot give information for ", input$channels, ", as we do not have enough data for it.")
    }
    else if(input$hour >= 2 & input$hour <= 5){
      paste0("We cannot give information for this time of the day, as we have no data for it.")
    }
    else if(input$hour < 2 || input$hour > 5){
      if(round(input$maxVD * newCoefficients()[1]) > 0){
        if(input$GRP > max(broadNet$gross_rating_point[broadNet$channel == input$channels])){
          paste0("We expect the amount of visitors five minutes after the commercial to be ", round(input$maxVD * newCoefficients()[1]), " more than what would have been expected without a commercial. However, please take into account that usually the broadcasts on ", input$channels ," have a lower GRP.")  
        } else {
          paste0("We expect the amount of visitors five minutes after the commercial to be ", round(input$maxVD * newCoefficients()[1]), " more than what would have been expected without a commercial.")  
        }
      }
      else if(round(input$maxVD * newCoefficients()[1]) < 0){
        paste0("We could not find a significant effect for these settings.")
      } else {
        paste0("We expect the amount of visitors five minutes after the commercial to be equal to what would have been expected without a commercial.")
      }
    }
  })
  output$warning = renderText({
    if(input$channels != "Slam!TV" && input$GRP > max(broadNet$gross_rating_point[broadNet$channel == input$channels])){
      paste0("Warning: usually the broadcasts on ", input$channels ," have a lower GRP")
    }
    else if(input$channels == "Slam!TV"){
      paste0("Warning: not enough data")
    }
  })
}

# Final app
shinyApp(ui = ui, server = server)