# Load required libraries
if (!require("shiny")) install.packages("shiny")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("plotly")) install.packages("plotly")
if (!require("DT")) install.packages("DT")
if (!require("leaflet")) install.packages("leaflet")

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(leaflet)

# Check if clean_survey.rds exists
if (!file.exists("clean_survey.rds")) {
  stop("clean_survey.rds not found. Please run clean_survey.R first.")
}

# Load the cleaned data
survey <- readRDS("clean_survey.rds")

# Verify data loaded correctly
print(paste("Loaded survey data with", nrow(survey), "rows"))

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "Mental Health in Tech Survey Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Treatment Analysis", tabName = "treatment", icon = icon("medkit")),
      menuItem("Work Environment", tabName = "work", icon = icon("building")),
      menuItem("Geographical Analysis", tabName = "geo", icon = icon("globe")),
      menuItem("Raw Data", tabName = "data", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
        fluidRow(
          valueBoxOutput("total_responses"),
          valueBoxOutput("treatment_rate"),
          valueBoxOutput("remote_work_rate")
        ),
        fluidRow(
          box(
            title = "Treatment by Company Size",
            status = "primary",
            plotlyOutput("treatment_by_size")
          ),
          box(
            title = "Work Interference Distribution",
            status = "primary",
            plotlyOutput("work_interference_dist")
          )
        )
      ),
      
      # Demographics Tab
      tabItem(tabName = "demographics",
        fluidRow(
          box(
            title = "Age Distribution",
            status = "primary",
            plotlyOutput("age_dist")
          ),
          box(
            title = "Gender Distribution",
            status = "primary",
            plotlyOutput("gender_dist")
          )
        ),
        fluidRow(
          box(
            title = "Country Distribution",
            status = "primary",
            plotlyOutput("country_dist")
          ),
          box(
            title = "Mental Health Consequences by Gender",
            status = "primary",
            plotlyOutput("mental_health_consequence_gender")
          )
        )
      ),
      
      # Treatment Analysis Tab
      tabItem(tabName = "treatment",
        fluidRow(
          box(
            title = "Treatment Seeking by Gender",
            status = "primary",
            plotlyOutput("treatment_by_gender")
          ),
          box(
            title = "Treatment vs Work Interference",
            status = "primary",
            plotlyOutput("treatment_by_interference")
          )
        ),
        fluidRow(
          box(
            title = "Treatment by Tech Company Status",
            status = "primary",
            plotlyOutput("treatment_by_tech")
          ),
          box(
            title = "Family History vs Treatment Seeking",
            status = "primary",
            plotlyOutput("family_history_treatment")
          )
        )
      ),
      
      # Work Environment Tab
      tabItem(tabName = "work",
        fluidRow(
          box(
            title = "Remote Work Distribution",
            status = "primary",
            plotlyOutput("remote_work_dist")
          ),
          box(
            title = "Benefits Distribution",
            status = "primary",
            plotlyOutput("benefits_dist")  # This box is for Benefits Distribution
          )
        ),
        fluidRow(
          box(
            title = "Work Interference by Company Size",
            status = "primary",
            plotlyOutput("work_interference_company_size")
          )
        )
      ),
      
      # Geographical Analysis Tab
      tabItem(tabName = "geo",
        fluidRow(
          box(
            title = "Treatment Seeking by Country",
            status = "primary",
            plotlyOutput("treatment_by_country")
          ),
          box(
            title = "Interactive Map of Responses",
            status = "primary",
            leafletOutput("geo_map")
          )
        )
      ),
      
      # Raw Data Tab
      tabItem(tabName = "data",
        fluidRow(
          box(
            title = "Raw Survey Data",
            status = "primary",
            width = 12,
            DTOutput("survey_table")
          )
        )
      )
    )
  )
)

# Server Definition
server <- function(input, output) {
  
  # Overview Tab Outputs
  output$total_responses <- renderValueBox({
    valueBox(
      nrow(survey),
      "Total Responses",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$treatment_rate <- renderValueBox({
    valueBox(
      paste0(round(mean(survey$treatment, na.rm = TRUE) * 100, 1), "%"),
      "Sought Treatment",
      icon = icon("medkit"),
      color = "green"
    )
  })
  
  output$remote_work_rate <- renderValueBox({
    valueBox(
      paste0(round(mean(survey$remote_work, na.rm = TRUE) * 100, 1), "%"),
      "Remote Workers",
      icon = icon("laptop-house"),
      color = "purple"
    )
  })
  
  # Overview Tab - Treatment by Company Size
  output$treatment_by_size <- renderPlotly({
    survey %>%
      count(no_employees, treatment) %>%
      group_by(no_employees) %>%
      mutate(prop = n/sum(n)) %>%
      plot_ly(x = ~no_employees, y = ~prop, color = ~treatment, type = "bar") %>%
      layout(title = "Treatment by Company Size",
             barmode = "stack",
             xaxis = list(title = "Company Size"),
             yaxis = list(title = "Proportion"))
  })
  
  # Overview Tab - Work Interference Distribution
  output$work_interference_dist <- renderPlotly({
    survey %>%
      count(work_interfere) %>%
      plot_ly(x = ~work_interfere, y = ~n, type = "bar",
              marker = list(color = "steelblue")) %>%
      layout(title = "Work Interference Distribution",
             xaxis = list(title = "Work Interference"),
             yaxis = list(title = "Count"))
  })
  
  # Work Environment - Benefits Distribution
  output$benefits_dist <- renderPlotly({
    survey %>%
      count(benefits) %>%
      plot_ly(x = ~benefits, y = ~n, type = "bar",
              marker = list(color = "steelblue")) %>%
      layout(title = "Benefits Distribution",
             xaxis = list(title = "Benefits"),
             yaxis = list(title = "Count"))
  })
  
  # Demographics Plots
  output$age_dist <- renderPlotly({
    plot_ly(survey, x = ~Age, type = "histogram", 
            marker = list(color = "steelblue")) %>% 
      layout(title = "Age Distribution",
             xaxis = list(title = "Age"),
             yaxis = list(title = "Count"))
  })
  
  output$gender_dist <- renderPlotly({
    survey %>%
      count(Gender) %>%
      plot_ly(x = ~Gender, y = ~n, type = "bar",
              marker = list(color = "steelblue")) %>%
      layout(title = "Gender Distribution",
             xaxis = list(title = "Gender"),
             yaxis = list(title = "Count"))
  })
  
  output$country_dist <- renderPlotly({
    survey %>%
      count(Country) %>%
      arrange(desc(n)) %>%
      head(10) %>%
      plot_ly(x = ~Country, y = ~n, type = "bar",
              marker = list(color = "steelblue")) %>%
      layout(title = "Top 10 Countries",
             xaxis = list(title = "Country"),
             yaxis = list(title = "Count"))
  })
  
  # Mental Health Consequences by Gender
  output$mental_health_consequence_gender <- renderPlotly({
    survey %>%
      count(Gender, mental_health_consequence) %>%
      plot_ly(x = ~Gender, y = ~n, color = ~mental_health_consequence, type = "bar") %>%
      layout(title = "Mental Health Consequences by Gender",
             xaxis = list(title = "Gender"),
             yaxis = list(title = "Count"))
  })
  
  # Treatment Analysis Plots
  output$treatment_by_gender <- renderPlotly({
    survey %>%
      count(Gender, treatment) %>%
      group_by(Gender) %>%
      mutate(prop = n/sum(n)) %>%
      plot_ly(x = ~Gender, y = ~prop, color = ~treatment, type = "bar") %>%
      layout(title = "Treatment Seeking by Gender",
             barmode = "stack",
             xaxis = list(title = "Gender"),
             yaxis = list(title = "Proportion"))
  })
  
  output$treatment_by_interference <- renderPlotly({
    survey %>%
      filter(!is.na(work_interfere)) %>%
      count(work_interfere, treatment) %>%
      group_by(work_interfere) %>%
      mutate(prop = n/sum(n)) %>%
      plot_ly(x = ~work_interfere, y = ~prop, color = ~treatment, type = "bar") %>%
      layout(title = "Treatment vs Work Interference",
             barmode = "stack",
             xaxis = list(title = "Work Interference"),
             yaxis = list(title = "Proportion"))
  })
  
  # Family History vs Treatment Seeking
  output$family_history_treatment <- renderPlotly({
    survey %>%
      count(family_history, treatment) %>%
      group_by(family_history) %>%
      mutate(prop = n/sum(n)) %>%
      plot_ly(x = ~family_history, y = ~prop, color = ~treatment, type = "bar") %>%
      layout(title = "Family History vs Treatment Seeking",
             barmode = "stack",
             xaxis = list(title = "Family History"),
             yaxis = list(title = "Proportion"))
  })
  
  # Work Environment Plots
  output$remote_work_dist <- renderPlotly({
    survey %>%
      count(remote_work) %>%
      plot_ly(x = ~remote_work, y = ~n, type = "bar",
              marker = list(color = "steelblue")) %>%
      layout(title = "Remote Work Distribution",
             xaxis = list(title = "Remote Work"),
             yaxis = list(title = "Count"))
  })
  
  # Work Interference by Company Size
  output$work_interference_company_size <- renderPlotly({
    survey %>%
      filter(!is.na(work_interfere)) %>%
      count(no_employees, work_interfere) %>%
      plot_ly(x = ~no_employees, y = ~n, color = ~work_interfere, type = "bar") %>%
      layout(title = "Work Interference by Company Size",
             xaxis = list(title = "Company Size"),
             yaxis = list(title = "Count"))
  })
  
  # Geographical Analysis
  output$treatment_by_country <- renderPlotly({
    survey %>%
      count(Country, treatment) %>%
      plot_ly(x = ~Country, y = ~n, color = ~treatment, type = "bar") %>%
      layout(title = "Treatment Seeking by Country",
             xaxis = list(title = "Country"),
             yaxis = list(title = "Count"))
  })
  
  # Raw Data Table
  output$survey_table <- renderDT({
    datatable(survey,
             options = list(pageLength = 10,
                            scrollX = TRUE,
                            scrollY = "500px"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
