library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)


# Define UI
ui <- fluidPage(theme = shinytheme("darkly"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "My first app",
                  tabPanel("Navbar 1",
                           sidebarPanel(
                             tags$h3("Input:"),
                             textInput("txt1", "Given Name:", ""),
                             textInput("txt2", "Surname:", ""),
                             
                           ), # sidebarPanel
                           mainPanel(
                             h1("Header 1"),
                             
                             h4("Output 1"),
                             verbatimTextOutput("txtout"),
                             plotOutput("paymentPieChart")
                             
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Navbar 2", "This panel is intentionally left blank"),
                  tabPanel("Navbar 3", "This panel is intentionally left blank")
                  
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
  
  output$txtout <- renderText({
    paste( input$txt1, input$txt2, sep = " " )
  })
  
  payment_totals <- grc |>
    group_by(paymentType) |>
    summarize(TotalPrice = sum(total))
  
  payment_totals <- payment_totals |>
    mutate(Percentage = TotalPrice / sum(TotalPrice) * 100)
  
  output$paymentPieChart <- renderPlot({
    ggplot(payment_totals, aes(x = "", y = Percentage, fill = PaymentType)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      labs(title = "Payment Type Distribution",
           x = NULL, y = NULL,
           fill = "Payment Type",
           caption = "Total Price") +
      theme_void() +
      theme(legend.position = "right")
  })

} # server


# Create Shiny object
shinyApp(ui = ui, server = server)