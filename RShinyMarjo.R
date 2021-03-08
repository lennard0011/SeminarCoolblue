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


#fullModel <- load(file = "C:/Users/Probook/my_fitted_model.rda",.GlobalEnv)
fullCoef = as.data.frame(fullModel$coefficients)
# fullCoef = as.data.frame(fullModel2$coefficients)
channels = append(unique(broadNet$channel), "Slam!TV")

ui = dashboardPage(
  dashboardHeader(title = tags$strong("Direct effects of a TV-commercial"), titleWidth = 350),
  dashboardSidebar(disable = T),
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
    
    fluidRow(
      box(width = 4,
          title = "Commercial-specific effects", 
          sliderInput(inputId = "GRP", label = "Input Gross Rating Point", value = 1, min = 0.05, max = 2.75),
          selectInput(inputId = "channels", label = "Choose your channel", choices = channels),
          sliderInput(inputId = "hour", label = "Choose broadcast time", value = 12, min = 0, max = 23),
          selectInput(inputId = "weekday", label = "Choose day of the week",
                      choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
      ),
      box(width = 4,
          title = "Commercial-specific effects",
          radioButtons(inputId = "length_spot", label = "Choose the spot length", selected = "30",
                       choices = c("30", "30 + 10", "30 + 10 + 5")),
          radioButtons(inputId = "pos_break", label = "Choose the position in break", selected = "Begin",
                       choices = c("Begin", "Middle", "End")),
          radioButtons(inputId = "prod_category", label = "Choose product category", selected = "Washing machines",
                       choices = c("Washing machines", "Televisions", "Laptops"))
      ),
      column(width = 4,
             fluidRow(
               box(
                   title = "Maximum amount of minutely visitors",
                   numericInput(inputId = "maxVD", label = "Maximum amount of minutely visitors", value = 100, min = 0, max = 500, step = 10)
               ),
               box(
                   title = "Extra amount of visitors",
                   textOutput(outputId = "text") 
               ),
               box(
                 tags$img(height = 100,
                          width = 100,
                          src = "coolblue2.jpg")
               )
             )
      )
    )
  )
)

server = function(input, output) {
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
    # print(paste0("GRP: ", input$GRP))
    # print(paste0("Channel: ", input$channels))
    # print(paste0("Hour: ", input$hour))
    # print(paste0("Weekday: ", input$weekday))
    # print(paste0("Length of spot: ", input$length_spot))
    # print(paste0("Position in break: ", input$pos_break))
    # print(paste0("Product category: ", input$prod_category))
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
    # paste("The expected extra traffic is", predict(fullModel, newdata = newCoefficients()))
  })
}
shinyApp(ui=ui, server=server)
