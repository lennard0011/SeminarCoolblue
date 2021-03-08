# R shiny + Tutorial -- Seminar Coolblue 2021
# @ Erik van der Heide

# TEMPLATE
#install.packages("shiny")
library("shiny")

# Example
ui <- fluidPage(
  sliderInput(inputId = "num", label = "Choose a number", value=25, min=1, max =100),
  # inputId = specify input exactly, label = What to display, ?sliderInput gives extras  plotOutput("hist")
  plotOutput(outputId = "hist")
  # outputId = specify output exactly
)
server <- function(input, output) {
  # Rule 1: save objects to display to output$
  output$hist <- # code
  # Rule 2: built objects to display with render*()
  output$hist <- renderPlot({ 
    title <- "100 random normal values"
    hist(rnorm(input$num), main = "Title") # Rule 3: use input values with input$
    })  
}
shinyApp(ui=ui, server=server)

# *Input functions:
actionButton()        # Button
submitButton()        # Button
checkboxInput()       # Single checkbox
checkboxGroupInput()  # Checkbox group
dateInput()           # Date
dateRangeInput()      # Date range
fileInput()           # File input
numericInput()        # Numeric input
passwordInput()       # Password input
radioButtons()        # Round buttons
selectInput()         # Select Box (e.g. channels)
sliderInput()         # Slider (e.g. GRP)
textInput()           # Text input

# *Output functions:
dataTableOutput()     # Interactive table
htmlOutput()          # Raw HTML
imageOutput()         # Image
plotOutput()          # Plot
tableOutput()         # Table
textOutput()          # Text
uiOuput()             # Shiny UI element
verbatimTextOutput()  # Text

# render*() functions:
renderDataTable()     # Interactive table (from dataframe/matrix)
renderImage()         # Image (saved as link to source file)
renderPlot()          # Plot
renderPrint()         # Code block of printed output
renderTable()         # Table (from dataframe/matrix)
renderText()          # Character string
renderUI()            # Shiny UI element

# =======================================================
#                 Peak Analysis model
# =======================================================


# =======================================================
#               Start of regression model
# =======================================================

# Input: regression function
fullModel$coefficients
ui <- fluidPage(
  sliderInput(inputId = "GRP", label = "Input Gross Rating Point", value=1, min=0.1, max =20.0),
  selectInput(inputId = "channels", label = "Choose your channel", choices = c("NPO1", "NPO2", "NPO3")),
  sliderInput(inputId = "hour", label = "Choose broadcast time", value = 20, min=18, max = 23),
  checkboxGroupInput(inputId = "weekday", label = "Choose day of the week", selected = "Monday",
                     choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
  checkboxGroupInput(inputId = "length_spot", label = "Choose the spot length", selected = "30",
                     choices = c("30", "30+10", "30+10+5")),
  checkboxGroupInput(inputId = "pos_break", label = "Choose position in break", selected = "begin",
                     choices = c("begin", "middle", "end")),
  checkboxGroupInput(inputId = "prod_category", label = "Choose product category", selected = "washing machines",
                     choices = c("washing_machines", "televisions", "laptops")),
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
    print(paste0("Product category: ", input$washing_machines))
  })
}
shinyApp(ui=ui, server=server)

