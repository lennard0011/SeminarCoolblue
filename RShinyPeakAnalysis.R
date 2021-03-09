# R shiny Peak Analysis -- Seminar Coolblue 2021
# @authors:

library(shiny)

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
                  min = 6, max = 24, value = c(6,24)),
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
                  tabPanel("Plot",plotOutput(outputId="plot"))
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
    # order on date or grp
    if (input$choose_ordering == "Date") {
      reactTable = reactTable[order(reactTable$date, reactTable$time),]
    } else {
      reactTable = reactTable[order(-reactTable$gross_rating_point), ]
    }
    # Output
    reactTable = reactTable[,c("channel", "date", "time", "gross_rating_point")]
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
    mtreact()
  })
  
  output$summ <- renderPrint({
    summary(mtreact())
  })
  output$plot <- renderPlot({
    with(broadNet, boxplot(gross_rating_point~hours)) # not the reactive one
    #with(mtreact(), boxplot(gross_rating_point~mtreact()[,2])) # will not yet work
  })
}

shinyApp(ui=ui, server=server)

# TODO: dates in Dutch way 
# TODO: summary tab
# TODO: plot tab
