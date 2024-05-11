library(shiny)
library(ggplot2)
library(dplyr)
library(ggdark)
library(viridis)
library(gridExtra)
library(shinythemes)
library(arules)



# Assume 'grc' is your dataset loaded earlier

# Define UI
ui <- fluidPage(theme = shinytheme("darkly"),
                tabsetPanel(
                  tabPanel("Pie Chart",
                           tabPanel("Charts",
                                    fluidRow(
                                      column(width = 12,
                                             plotOutput("paymentPieChart")
                                      ),
                                      column(width = 12,
                                             plotOutput("ageBarChart")
                                      ),
                                      column(width = 12,
                                             plotOutput("cityBarChart"))
                                    )
                           )
                  ),
                  tabPanel("Bar Chart",
                           titlePanel("Filter Rows by RND Value"),
                           fluidRow(
                             column(width = 12,
                                    sliderInput("rndSlider", "RND Value:",
                                                min = 1, max = max(grc$rnd), value = 1, step = 1)
                             )
                           ),
                           fluidRow(
                             column(width = 12,
                                    tableOutput("filteredRows")
                             )
                           )
                  ),
                  tabPanel("info",
                           titlePanel("K means"),
                           fluidRow(
                             column(width = 12,
                                    sliderInput("clusters", "Numbers of cluseters:",
                                                min = 2, max = 4, value = 3, step = 1)
                             )
                           ),
                           fluidRow(
                             column(width = 12,
                                    plotOutput("kmeans"),
                             )
                           ),
                           titlePanel("Association Rule Mining"),
                               sliderInput("minSupport", "Minimum Support:", min = 0.01, max = 0.5, value = 0.1, step = 0.01),
                               sliderInput("minConfidence", "Minimum Confidence:", min = 0.01, max = 1, value = 0.5, step = 0.01),
                               tableOutput("rulesOutput")
                    )
                  )
                  
)

# Define server function  
server <- function(input, output) {
  
  
  
  # Pie chart
  payment_totals <- grc %>%
    group_by(paymentType) %>%
    summarize(TotalPrice = sum(total)) %>%
    mutate(Percentage = TotalPrice / sum(TotalPrice) * 100)
  
  output$paymentPieChart <- renderPlot({
    ggplot(payment_totals, aes(x = "", y = Percentage, fill = paymentType)) +
      geom_bar(stat = "identity", width = 1) +
      geom_text(aes(label = paste0(round(Percentage), "%")), position = position_stack(vjust = 0.5)) +
      coord_polar("y") +
      labs(title = "Payment Type Distribution",
           x = NULL, y = NULL,
           fill = "Payment Type",
           caption = "Total Price") +
      theme_void() +
      theme(legend.position = "right") +
      theme_dark() +
      dark_theme_light()
  })
  
  # Bar chart
  age_totals <- grc %>%
    group_by(age) %>%
    summarize(TotalPrice = sum(total))
  
  output$ageBarChart <- renderPlot({
    ggplot(age_totals, aes(x = factor(age), y = TotalPrice, fill = TotalPrice)) +
      geom_col(color = "white", linewidth = 0.25) +
      scale_fill_viridis() +
      geom_bar(stat = "identity") +
      labs(title = "Total Price by Age Group",
           x = "Age Group", y = "Total Price",
           fill = "Age Group") +
      theme_dark() +
      dark_theme_light()
  })
  # city
  city_totals <- grc %>%
    group_by(city) %>%
    summarize(TotalSpending = sum(total)) %>%
    arrange(desc(TotalSpending))
  
  output$cityBarChart <- renderPlot({
    ggplot(city_totals, aes(x = reorder(city, TotalSpending), y = TotalSpending, fill = city)) +
      geom_bar(stat = "identity") +
      labs(title = "Total Spending by City",
           x = "City", y = "Total Spending",
           fill = "City") +
      theme_dark() +
      dark_theme_light() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
})
  
  # customers
  output$filteredRows <- renderTable({
    filtered_data <- grc %>%
      filter(rnd == input$rndSlider)
    filtered_data
  })
  
  # association
  output$rulesOutput <- renderTable({
    transactions <- strsplit(as.character(grc$items), ",")
    transactions <- as(transactions, "transactions")
    rules <- apriori(transactions, parameter = list(support = input$minSupport, confidence = input$minConfidence))
    rules_df <- as(rules, "data.frame")
    rules_df
  }
  )
  
  
  #K means
  output$kmeans <- renderPlot({
    data_for_clustering <- grc %>%
      group_by(rnd) %>%
      summarize(Age = mean(age), Total = sum(total))
    
    kmeans_model <- kmeans(data_for_clustering[, c("Age", "Total")], centers = input$clusters)
    
    data_for_clustering$Cluster <- factor(kmeans_model$cluster)
    
    ggplot(data_for_clustering, aes(x = Age, y = Total, color = Cluster)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "K-means Clustering of Age and Total",
           x = "Age", y = "Total",
           color = "Cluster") +
      theme_dark() +
      dark_theme_light()
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
