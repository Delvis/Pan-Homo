library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(zoo)

# 1. Setup - Point to the correct path
source("R/prepare_data.R")

# 2. UI
ui <- fluidPage(
  titlePanel("PanHomopticon - Alpha version"),
  sidebarLayout(
    sidebarPanel(
      selectInput("source", "Evidence Source:",
                  choices = c("All", "Miscellaneous", "mtDNA", "Nuclear DNA", "Genomic")),

      sliderInput("threshold", "Fossil Threshold (Ma):",
                  min = 0, max = 8, value = 0, step = 0.1),

      hr(),
      radioButtons("fit_type", "Regression Model:",
                   choices = list("Linear (y ~ x)" = "linear",
                                  "Quadratic (y ~ x²)" = "poly"),
                   selected = "linear"),

      checkboxInput("show_ci", "Show Confidence Intervals", value = TRUE)
    ),
    mainPanel(
      plotlyOutput("regPlot", height = "600px")
    )
  )
)

# 3. Server
server <- function(input, output) {

  current_data <- reactive({
    df <- HomoPanDivergences %>%
      mutate(Author = paste(Reference, Year)) %>%
      mutate(plot_date = as.numeric(date)) %>%
      filter(ESTIMATION >= input$threshold)

    # Merge Genome and Integrative
    if (input$source == "Genomic") {
      df <- df %>% filter(Source %in% c("Genome", "Integrative"))
    } else if (input$source != "All") {
      df <- df %>% filter(as.character(Source) == input$source)
    }
    return(df)
  })

  output$regPlot <- renderPlotly({
    min_points <- if(input$fit_type == "poly") 4 else 2
    validate(
      need(nrow(current_data()) >= min_points,
           paste("Insufficient data points for a", input$fit_type, "regression."))
    )

    reg_formula <- if(input$fit_type == "poly") y ~ poly(x, 2) else y ~ x

    # We use suppressWarnings to hide the "unknown aesthetics" message from the console
    p <- suppressWarnings(
      ggplot(current_data(), aes(x = plot_date, y = ESTIMATION)) +
        # Map Author to 'label' for plotly tooltips
        geom_point(aes(label = Author), alpha = 0.5, color = "#2c3e50") +

        # Cleaned up geom_smooth (No border on CI)
        geom_smooth(aes(group = 1),
                    method = "lm",
                    formula = reg_formula,
                    fill = "grey80",
                    linetype = "solid",
                    se = input$show_ci) +

        scale_x_continuous(name = "Publication Year") +
        scale_y_continuous(name = "Divergence Estimate (Ma)", limits = c(0, 16)) +
        theme_minimal()
    )

    # Convert to Plotly and specify tooltip
    ggplotly(p, tooltip = "label") %>%
      layout(hovermode = "closest")
  })
}

shinyApp(ui, server)
