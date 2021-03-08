# R shiny + Tutorial -- Seminar Coolblue 2021
# @ Erik van der Heide

# TEMPLATE
#install.packages("shiny")
library("shiny")
library("stringr")

# Input: regression function


fullModel <- load(file = "C:/Users/Probook/my_fitted_model.rda",.GlobalEnv)
fullCoef = as.data.frame(fullModel$coefficients)
channels = unique(broadNet$channel)

ui = fluidPage(
  sliderInput(inputId = "GRP", label = "Input Gross Rating Point", value = 1, min = 0.1, max = 20.0),
  selectInput(inputId = "channels", label = "Choose your channel", choices = channels),
  sliderInput(inputId = "hour", label = "Choose broadcast time", value = 12, min = 0, max = 23),
  selectInput(inputId = "weekday", label = "Choose day of the week", selected = "Monday",
              choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
  radioButtons(inputId = "length_spot", label = "Choose the spot length", selected = "30",
               choices = c("30", "30 + 10", "30 + 10 + 5")),
  radioButtons(inputId = "pos_break", label = "Choose the position in break", selected = "Begin",
               choices = c("Begin", "Middle", "End")),
  radioButtons(inputId = "prod_category", label = "Choose product category", selected = "Washing machines",
               choices = c("Washing machines", "Televisions", "Laptops")),
  textOutput(outputId = "text")
)
server = function(input, output) {
  newCoefficients = reactive({
    # Hour
    hours = matrix(0, ncol = 19)
    if (input$hour == 1){
      hours[1] = 1
    }
    if (input$hour >= 6){
      hours[input$hour - 4] = 1
    }
    
    # GRP
    GRP = input$GRP
    
    # Product Category
    prod_cat = matrix(0, ncol = 2)
    if (input$prod_category == 'Laptops'){
      prod_cat[1] = 1
    }
    else if (input$prod_category == 'Washing machines'){
      prod_cat[2] = 1
    }
    
    # Channel
    channel = matrix(0, ncol = 28)
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
    spotlength = matrix(0, ncol = 2)
    if (input$length_spot == 30){
      spotlength[1] = 1
    } 
    else if (input$length_spot == '30 + 10'){
      spotlength[2] = 1
    }
    
    # Position in break
    breakPos = matrix(0, ncol = 2)
    if (input$pos_break == 'begin'){
      breakPos[1] = 1
    }
    else if (input$pos_break == 'end'){
      breakPos[2] = 1
    }
    
    # Weekday
    weekDay = matrix(0, ncol = 6)
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
    
    frame = as.data.frame(cbind(1, 0, hours, GRP, prod_cat, channel, spotlength, breakPos, weekDay, 0, 0))
    
    names(frame) = names(fullModel$coefficients)
    return(frame)
  })
  output$text = renderPrint({
    # print(paste0("GRP: ", input$GRP))
    # print(paste0("Channel: ", input$channels))
    # print(paste0("Hour: ", input$hour))
    # print(paste0("Weekday: ", input$weekday))
    # print(paste0("Length of spot: ", input$length_spot))
    # print(paste0("Position in break: ", input$pos_break))
    # print(paste0("Product category: ", input$prod_category))

    if (input$hour >= 2 & input$hour <= 5 & round(sum(newCoefficients() * fullCoef), 3) >= 0){
      paste0("We cannot give information for this time of the day, as we have no data for it.")
    } 
    else if (input$hour < 2 || input$hour > 5 & round(sum(newCoefficients() * fullCoef), 3) >= 0){
      paste0("We expect the visit density five minutes after the commercial to be ", round(sum(newCoefficients() * fullCoef), 3), " higher than would have been expected without a commercial.") 
    } else {
      paste0("We could not find a significant effect for these settings.")
    }
    # paste("The expected extra traffic is", predict(fullModel, newdata = newCoefficients()))

  })
}
shinyApp(ui=ui, server=server)
