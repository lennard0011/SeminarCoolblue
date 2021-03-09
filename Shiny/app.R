# R shiny + Tutorial -- Seminar Coolblue 2021
# @ Erik van der Heide

# TEMPLATE
# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("shinyWidgets")
library("shiny")
library("shinydashboard")
library("shinyWidgets")
library("stringr")

# Input: regression function


#fullModel = load(file = "C:/Users/Probook/my_fitted_model.rda",.GlobalEnv)
fullCoef = as.data.frame(fullModel$coefficients)
# fullCoef = as.data.frame(fullModel2$coefficients)
channels = append(unique(broadNet$channel), "Slam!TV")


ui = dashboardPage(
  dashboardHeader(title = "Effects of TV-commercials", titleWidth = 350),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dash", icon = icon("dashboard")),
      menuItem("Peak Analysis", tabName = "pa"),
      menuItem("Direct Effects", tabName = "de")
    )
  ),
  
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
    tabItems(
      tabItem("dash",
              fluidRow(width = 12,
                       tags$img(height = 1000,
                                width = 1000, style = "display: block; margin-left: auto; margin-right: auto;",
                                src = "coolblue2.png") 
              )
      ),
      tabItem("pa",
              headerPanel(title = "Insights into Dutch commercials of Coolblue (first half of 2019)"),
              
              fluidRow(
                box(width = 6,
                    title = "Commercial-specific effects", 
                    selectInput(inputId = "channel", label = "Choose your channel", 
                                choices = c("All", sort(unique(broadNet$channel)))),
                    #length_of_spot
                    selectInput(inputId = "length_of_spot", label = "Choose length of the spot", 
                                choices = c("All", "30", "30 + 10", "30 + 10 + 5")),
                    #position_in_break_3option
                    selectInput(inputId = "position_in_break_3option", label = "Choose the position in break", 
                                choices = c("All", "Begin", "Middle", "End")),
                    #product_category
                    selectInput(inputId = "product_category", label = "Choose product category", 
                                choices = c("All", "Washing machines", "Televisions", "Laptops")),
                    #month
                    selectInput(inputId = "month", label = "Choose month of the year", 
                                choices = c("All", "January", "February", "March", "April", "May", "June")),
                    radioButtons(inputId = "choose_ordering", label = "Choose ordering of data",
                                 choices = c("Date", "Gross Rating Point"), inline = T)
                ),
                box(width = 6,
                    tabsetPanel(type = "tab", 
                                tabPanel("Data", tableOutput(outputId="Table")),
                                tabPanel("Summary", verbatimTextOutput(outputId="summ")),
                                tabPanel("Plot", plotOutput(outputId="plot"))
                    )
                ),
                # Output text
                textOutput(outputId = "text1"),
                textOutput(outputId = "text2"),
                textOutput(outputId = "text3"),
                textOutput(outputId = "text4")
              )
      ),
      tabItem("de",
              headerPanel(title = "Insights into direct effects of Coolblue commercials"),
              fluidRow(
                column(width = 6,
                       box(width = 12,
                           title = "Commercial-specific effects", 
                           selectInput(inputId = "channels", label = "Choose your channel", choices = sort(unique(broadNet$channel))),
                           sliderInput(inputId = "GRP", label = "Input Gross Rating Point", value = 0.05, min = 0.05, max = 7.05),
                           textOutput(outputId = "warning"), tags$head(tags$style("#warning{color: red;
                                 }")),
                           selectInput(inputId = "weekday", label = "Choose day of the week",
                                       choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
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
                           radioButtons(inputId = "length_spot", label = "Choose the spot length", selected = "30",
                                        choices = c("30", "30 + 10", "30 + 10 + 5")),
                           radioButtons(inputId = "pos_break", label = "Choose the position in break", selected = "Begin",
                                        choices = c("Begin", "Middle", "End")),
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

server = function(session, input, output) {
  mtreact = reactive({
    reactTable = broadNet
    
    colnames(reactTable)[2] = "Channel"
    colnames(reactTable)[3] = "Date"
    colnames(reactTable)[4] = "Time"
    colnames(reactTable)[11] = "GRP"
    
    # subset on channel
    if (input$channel != "All") {
      reactTable = subset(reactTable, channel == input$channel)
      updateSelectInput(session, inputId = "length_of_spot", label = "Choose the spot length",
                        choices = c("All", unique(reactTable$length_of_spot)))
    }
    # subset on length of spot
    if (input$length_of_spot != "All") {
      reactTable = subset(reactTable, length_of_spot == input$length_of_spot)
    }
    # subset on position in break
    if (input$position_in_break_3option != "All") {
      if (input$position_in_break_3option == "Begin"){
        reactTable = subset(reactTable, position_in_break_3option == "begin") 
      }
      if (input$position_in_break_3option == "Middle"){
        reactTable = subset(reactTable, position_in_break_3option == "middle") 
      }
      if (input$position_in_break_3option == "End"){
        reactTable = subset(reactTable, position_in_break_3option == "end") 
      }
    }
    # subset on product category
    if (input$product_category != "All") {
      if (input$product_category == "Washing machines"){
        reactTable = subset(reactTable, product_category == "wasmachines")
      }
      if (input$product_category == "Televisions"){
        reactTable = subset(reactTable, product_category == "televisies")
      }
      if (input$product_category == "Laptops"){
        reactTable = subset(reactTable, product_category == "laptops")
      }
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
      reactTable = subset(reactTable, month(Date) == nrMonth)
    }
    # order on date or grp
    if (input$choose_ordering == "Date") {
      reactTable = reactTable[order(reactTable$Date, reactTable$Time),]
    } else {
      reactTable = reactTable[order(-reactTable$GRP), ]
    }
    reactTable = reactTable[, c("Channel", "Date", "Time", "GRP")]
    return(reactTable)
  })
  
  output$Table = renderTable({
    mtreact()
  })
  
  output$summ = renderPrint({
    summary(mtreact())
  })
  output$plot = renderPlot({
    with(broadNet, boxplot(gross_rating_point~hours)) # not the reactive one
    #with(mtreact(), boxplot(gross_rating_point~mtreact()[,2])) # will not yet work
  }
  )
  
  newCoefficients = reactive({
    # Hour
    hours = matrix(0, 19)
    if (input$hour == 1){
      hours[1] = 1
    }
    if (input$hour >= 6){
      hours[input$hour - 4] = 1
    }
    
    # GRP
    GRP = input$GRP
    
    # Product Category
    prod_cat = matrix(0, 2)
    if (input$prod_category == 'Laptops'){
      prod_cat[1] = 1
    }
    else if (input$prod_category == 'Washing machines'){
      prod_cat[2] = 1
    }
    
    # Channel
    channel = matrix(0, 28)
    for (i in 25:52){
      rowname = rownames(fullCoef)[i]
      if (substring(rowname, 1, 1) == "`"){
        n = nchar(rowname)
        chan = substring(rowname, 10, n - 1)
      } else {
        chan = substring(rowname, 9)
      }
      if (chan == input$channels){
        channel[i - 24] = 1
        break
      }
    }
    
    # Length of spot
    spotlength = matrix(0, 2)
    if (input$length_spot == 30){
      spotlength[1] = 1
    }
    else if (input$length_spot == '30 + 10'){
      spotlength[2] = 1
    }
    
    # Position in break
    breakPos = matrix(0, 2)
    if (input$pos_break == 'Begin'){
      breakPos[1] = 1
    }
    else if (input$pos_break == 'End'){
      breakPos[2] = 1
    }
    
    # Weekday
    weekDay = matrix(0, 6)
    if (input$weekday == 'Tuesday'){
      weekDay[1] = 1
    }
    else if (input$weekday == 'Wednesday'){
      weekDay[5] = 1
    }
    else if (input$weekday == 'Thursday'){
      weekDay[2] = 1
    }
    else if (input$weekday == 'Friday'){
      weekDay[4] = 1
    }
    else if (input$weekday == 'Sunday'){
      weekDay[6] = 1
    }
    else if (input$weekday == 'Monday'){
      weekDay[3] = 1
    }
    
    frame = as.data.frame(c(1, 0, hours, GRP, prod_cat, channel, spotlength, breakPos, weekDay, 0, 0))
    # frame = as.data.frame(c(1, 0, hours, GRP * prod_cat, GRP * channel, GRP * spotlength, GRP * breakPos, GRP * weekDay, 0, 0))
    row.names(frame) = rownames(fullCoef)
    return(frame)
  })
  output$text = renderText({
    if (input$channels == "Slam!TV"){
      paste0("We cannot give information for this channel, as we do not have enough data for it.")
    }
    else if (input$hour >= 2 & input$hour <= 5){
      paste0("We cannot give information for this time of the day, as we have no data for it.")
    }
    else if (input$hour < 2 || input$hour > 5){
      if (round(input$maxVD * sum(newCoefficients() * fullCoef)) > 0){
        paste0("We expect the amount of visitors five minutes after the commercial to be ", round(input$maxVD * sum(newCoefficients() * fullCoef)), " more than what would have been expected without a commercial")
      }
      else if (round(sum(newCoefficients() * fullCoef), 3) < 0){
        paste0("We could not find a significant effect for these settings.")
      } else {
        paste0("We expect the amount of visitors five minutes after the commercial to be equal to what would have been expected without a commercial.")
      }
    }
  })
  output$warning = renderText({
    if (input$GRP > max(broadNet$gross_rating_point[broadNet$channel == input$channels])){
      paste0("Warning: normally the broadcasts on ", input$channels ," have a lower GRP")
    }
  })
}
shinyApp(ui=ui, server=server)
