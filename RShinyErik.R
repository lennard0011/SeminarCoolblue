# R shiny + Tutorial -- Seminar Coolblue 2021
# @ Erik van der Heide

# TEMPLATE
#install.packages("shiny")
library("shiny")

# Input: regression function
fullModel$coefficients

# dit in interval code voor file save(fit, file = "../my_shiny_app/my-fitted-boost.rda")


fit_boost <- load(file = "my_fitted_model.rda",.GlobalEnv)
predict(fit_boost, data)



ui <- fluidPage(
  
  titlePanel("Hello Shiny!"),
  
  
  sliderInput(inputId = "GRP", label = "Input Gross Rating Point", value=1, min=0.1, max =20.0),
  selectInput(inputId = "channels", label = "Choose your channel", choices = c("NPO1", "NPO2", "NPO3")),
  sliderInput(inputId = "hour", label = "Choose broadcast time", value = 20, min=0, max = 23),
  selectInput(inputId = "weekday", label = "Choose day of the week", selected = "Monday",
                     choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
  radioButtons(inputId = "length_spot", label = "Choose the spot length", selected = "30",
                     choices = c("30", "30+10", "30+10+5")),
  radioButtons(inputId = "pos_break", label = "Choose position in break", selected = "begin",
                     choices = c("begin", "middle", "end")),
  radioButtons(inputId = "prod_category", label = "Choose product category", selected = "Washing machines",
                     choices = c("Washing machines", "Televisions", "Laptops")),
  textOutput(outputId = "text")
)
server <- function(input, output) {
  output$text <- renderText({
    print(paste0("GRP: ", input$GRP))
    print(paste0("Channel: ", input$channels))
    print(paste0("Hour: ", input$hour))
    print(paste0("Weekday: ", input$weekday))
    print(paste0("Length of spot: ", input$length_spot))
    print(paste0("Position in break: ", input$pos_break))
    print(paste0("Product category: ", input$prod_category))
  })
}
shinyApp(ui=ui, server=server)
