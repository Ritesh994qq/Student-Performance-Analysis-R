
library(shiny)
library(ggplot2)

# Generate sample data
set.seed(123)
n <- 100

students <- data.frame(
  Student_ID = 1:n,
  Study_Hours = round(runif(n, 1, 8), 1),
  Attendance = round(runif(n, 50, 100), 1),
  Previous_Marks = round(runif(n, 35, 90), 1)
)

students$Final_Marks <- round(
  0.35 * students$Previous_Marks +
    3 * students$Study_Hours +
    0.20 * students$Attendance +
    rnorm(n, 0, 7)
)

students$Final_Marks <- pmax(
  0, pmin(100, students$Final_Marks)
)

# User interface
ui <- fluidPage(
  titlePanel("Student Performance Analysis Using R"),

  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "attendance",
        "Minimum Attendance (%)",
        min = 50,
        max = 100,
        value = 50
      ),

      selectInput(
        "variable",
        "Select Factor",
        choices = c(
          "Study Hours" = "Study_Hours",
          "Attendance" = "Attendance",
          "Previous Marks" = "Previous_Marks"
        )
      )
    ),

    mainPanel(
      h3("Statistical Summary"),
      verbatimTextOutput("summary"),

      h3("Distribution of Final Marks"),
      plotOutput("marksPlot"),

      h3("Relationship with Final Marks"),
      plotOutput("relationshipPlot"),

      h3("Student Records"),
      tableOutput("studentTable")
    )
  )
)

# Server logic
server <- function(input, output, session) {

  filtered_data <- reactive({
    subset(
      students,
      Attendance >= input$attendance
    )
  })

  output$summary <- renderPrint({
    d <- filtered_data()

    cat("Number of students:", nrow(d), "\n")
    cat("Mean final marks:",
        round(mean(d$Final_Marks), 2), "\n")
    cat("Median final marks:",
        median(d$Final_Marks), "\n")
    cat("Standard deviation:",
        round(sd(d$Final_Marks), 2), "\n")

    if (nrow(d) >= 3) {
      cat(
        "Correlation with study hours:",
        round(cor(d$Study_Hours, d$Final_Marks), 3),
        "\n"
      )
    }
  })

  output$marksPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Final_Marks)) +
      geom_histogram(
        bins = 12,
        fill = "steelblue",
        color = "white"
      ) +
      labs(
        title = "Distribution of Final Marks",
        x = "Final Marks",
        y = "Number of Students"
      ) +
      theme_minimal()
  })

  output$relationshipPlot <- renderPlot({
    ggplot(
      filtered_data(),
      aes(
        x = .data[[input$variable]],
        y = Final_Marks
      )
    ) +
      geom_point() +
      geom_smooth(method = "lm", se = TRUE) +
      labs(
        x = input$variable,
        y = "Final Marks"
      ) +
      theme_minimal()
  })

  output$studentTable <- renderTable({
    head(filtered_data(), 10)
  })
}

# Required: create and return the Shiny application
shinyApp(ui = ui, server = server)
