#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

pacman::p_load(shiny,shinydashboard,shinythemes,
               scatterPlotMatrix, parallelPlot,
               cluster, factoextra, tidyverse)

whitewine <- read_csv("data/wine_quality.csv") %>%
  filter(type == "white") %>%
  select(c(1:11))

# Define UI for application that draws a histogram
ui <- navbarPage(
  title = "ShinyIDEA: Interactive Data Exploration and Analysis",
  fluid = TRUE,
  theme = shinytheme("superhero"),
  id = "navbarID",
  tabPanel("Introduction"),
  navbarMenu("Univariate"),
  navbarMenu("Bivariate"),
  navbarMenu("Multivariate",
             tabPanel("Scatter Plot Matrix",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     selectInput("corrPlotTypeSelect",
                                                 "Correlation Plot Type:",
                                                 choices = list(
                                                   "Empty" = "Empty",
                                                   "Circles" = "Circles",
                                                   "Text" = "Text",
                                                   "AbsText" = "AbsText"),
                                                 selected = "Text"),
                                     selectInput("distType",
                                                 "Distribution Representation:",
                                                 choices = list("Histogram" = 2,
                                                                "Density Plot" = 1),
                                                 selected = 1)),
                        mainPanel(width = 9,
                                  box(
                                    scatterPlotMatrixOutput("spMatrix",
                                                            height = "500px",
                                                            width = "640px"))
                        )
                      )),
             tabPanel("Optimal No. of Clusters"),
             tabPanel("kmeans Clustering")
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$spMatrix <- renderScatterPlotMatrix({
    scatterPlotMatrix(whitewine,
                      distribType = as.numeric(input$distType),
                      corrPlotType = input$corrPlotTypeSelect,
                      rotateTitle = TRUE)
  })
  
  output$optimalPlot <- renderPlot({
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)