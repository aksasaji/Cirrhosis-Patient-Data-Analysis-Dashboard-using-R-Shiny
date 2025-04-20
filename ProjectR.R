library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)

data <- read.csv("C:/Users/AKSA SAJI/Downloads/cirrhosis.csv", stringsAsFactors = FALSE)
data
str(data)
summary(data)

# Count total NA values per column
colSums(is.na(data))
# Find percentage of missing data per column
missing_percent <- sapply(data, function(x) sum(is.na(x)) / length(x)) * 100
missing_percent

# Median Imputation for Numeric Columns
numeric_cols <- sapply(data, is.numeric)

for (col in names(data)[numeric_cols]) {
  na_count <- sum(is.na(data[[col]]))
  if (na_count > 0) {
    # You can choose between median
    impute_value <- median(data[[col]], na.rm = TRUE)
    data[[col]][is.na(data[[col]])] <- impute_value
  }
}

# Mode Imputation for Categorical Columns
get_mode <- function(v) {
  uniqv <- na.omit(unique(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

categorical_cols <- sapply(data, is.character)

for (col in names(data)[categorical_cols]) {
  na_count <- sum(is.na(data[[col]]))
  if (na_count > 0) {
    mode_val <- get_mode(data[[col]])
    data[[col]][is.na(data[[col]])] <- mode_val
  }
}
data$Stage <- round(as.numeric(data$Stage))
data$Stage

# Final check for NA values
cat("After basic imputation:\n")
print(colSums(is.na(data)))

ui <- dashboardPage(
  dashboardHeader(title = "Cirrhosis Insights"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Visualizations", tabName = "visuals", icon = icon("chart-bar")),
      menuItem("Data Table", tabName = "data_table", icon = icon("table")),
      selectInput("Sex", "Select Gender", choices = c("All", unique(data$Sex))),
      selectInput("Stage", "Stage", choices = c("All", unique(data$Stage))),
      selectInput("Status", "Status", choices = c("All", unique(data$Status))),
      sliderInput("Age", "Age Range (Years)", min = 20, max = 100, value = c(40, 80))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBox(length(unique(data$ID)), "Total Patients", icon = icon("users"), color = "blue"),
                valueBox(round(mean(data$Age)/365, 1), "Average Age (Years)", icon = icon("calendar"), color = "green"),
                valueBox(round(mean(data$Prothrombin, na.rm = TRUE), 2), "Avg Prothrombin Time", icon = icon("clock"), color = "olive"),
                valueBox(
                  sum(data$Drug == "D-penicillamine", na.rm = TRUE),
                  "D-penicillamine Group", icon = icon("capsules"), color = "navy"
                ),
                valueBox(
                  round(mean(data$Sex == "M", na.rm = TRUE) * 100, 1),
                  "Male Patients (%)", icon = icon("male"), color = "aqua"
                ),
                valueBox(
                  round(mean(data$Sex == "F", na.rm = TRUE) * 100, 1),
                  "Female Patients (%)", icon = icon("female"), color = "fuchsia"
                ),
                valueBox(
                  sum(data$Albumin < 3.5, na.rm = TRUE),
                  "Low Albumin (<3.5)", icon = icon("thermometer-empty"), color = "yellow"
                ),
                valueBox(round(mean(data$Bilirubin, na.rm = TRUE), 2), "Avg Bilirubin", icon = icon("tint"), color = "purple")
              )
      ),
      tabItem(tabName = "visuals",
              fluidRow(
                box(title = "Survival Status Distribution", width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("plot1")),
                box(title = "Bilirubin vs Age", width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("plot2"))
              ),
              
              fluidRow(
                box(title = "Select Variable", width = 4,
                    selectInput("distVar", "Variable", choices = names(data)[sapply(data, is.numeric)])),
                box(title = "Histogram", width = 8,
                    plotOutput("histPlot"))
              ),
              fluidRow(
                box(title = "Select Numeric Variable", width = 4,
                    selectInput("boxVar", "Variable", choices = names(data)[sapply(data, is.numeric)])),
                box(title = "Boxplot by Status", width = 8,
                    plotOutput("boxPlot"))
              ),
              
      ),
      tabItem(tabName = "data_table",
              fluidRow(
                box(
                  width = 12,
                  title = "Cirrhosis Patient Data",
                  status = "primary",
                  solidHeader = TRUE,
                  DTOutput("full_table")
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  
  filteredData <- reactive({
    df <- data
    if (input$Sex != "All") df <- df[df$Sex == input$Sex, ]
    if (input$Stage != "All") df <- df[df$Stage == input$Stage, ]
    if (input$Status != "All") df <- df[df$Status == input$Status, ]
    df <- df[df$Age/365 >= input$Age[1] & df$Age/365 <= input$Age[2], ]
    return(df)
  })
  
  output$plot1 <- renderPlot({
    ggplot(filteredData(), aes(x = Status, fill = Status)) + 
      geom_bar() + theme_minimal() +
      labs(title = "Survival Status Distribution")
  })
  output$plot2 <- renderPlot({
    ggplot(filteredData(), aes(x = Age/365, y = Bilirubin, color = Status)) + 
      geom_point() + theme_light() +
      labs(title = "Bilirubin vs Age", x = "Age (Years)", y = "Bilirubin")
  })
  output$histPlot <- renderPlot({
    ggplot(data, aes_string(x = input$distVar)) +
      geom_histogram(bins = 30, fill = "skyblue", color = "black") +
      theme_minimal()
  })
  output$boxPlot <- renderPlot({
    ggplot(data, aes_string(x = "Status", y = input$boxVar, fill = "Status")) +
      geom_boxplot() +
      theme_minimal()
  })
  
  
  output$full_table <- renderDT({
    datatable(
      filteredData(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      filter = "top",
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)

