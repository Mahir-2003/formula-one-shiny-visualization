library(shiny)
library(tidyverse)

f1 = read_csv("https://raw.githubusercontent.com/Mahir-2003/formula-one-shiny-visualization/main/f1_results_2015_2025.csv")

constructors = pull(f1, constructor_name) %>%
  unique() %>%
  na.omit() %>%
  sort()

scatterplot = function(df) {
  # finished = true to filter out any DNFs/retirements
  selected_df = df %>% filter(selected == 1, finished == TRUE, grid_position > 0)
  unselected_df = df %>% filter(selected == 0, finished == TRUE, grid_position > 0)
  
  # explicit diagonal reference line since geom_abline breaks with reversed y-axis
  diagonal = data.frame(grid_position = 1:20, finish_position = 1:20)
  
  ggplot() +
    geom_jitter(
      data = unselected_df,
      aes(grid_position, finish_position),
      width = 0.3, height = 0.3,
      alpha = 0.08, size = 1.5, color = "gray") +
    geom_jitter(
      data = selected_df,
      aes(grid_position, finish_position),
      width = 0.3, height = 0.3,
      alpha = 0.4, size = 1.5, color = "#FF1801") +
    geom_line(
      data = diagonal,
      aes(grid_position, finish_position),
      linetype = "dashed", color = "black", alpha = 0.5, linewidth = 0.8) +
    geom_smooth(
      data = selected_df,
      aes(grid_position, finish_position),
      method = "lm", se = TRUE, color = "darkred", linewidth = 1.2) +
    scale_x_continuous("Start Position  (P1 = pole position / fastest qualifier)",
                       breaks = seq(1, 20, 2), limits = c(0.5, 20.5)) +
    scale_y_continuous("Finish Position  (P1 = race winner)",
                       breaks = seq(1, 20, 2), limits = c(20.5, 0.5),  # reversed manually
                       trans = "reverse") +
    labs(
      caption = "Each dot = one driver in one race. Red = selected seasons, gray = all other seasons.\nPoints above the dashed line gained places; below lost places. Pit lane starters & DNFs excluded.\nData: Jolpica F1 API."
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.caption = element_text(size = 15, color = "black", hjust = 0),
      axis.title = element_text(size = 15)
    )
}

format_brush_table = function(df) {
  df %>% 
    transmute(
      Season = season,
      Race = race_name,
      Driver = driver_name,
      Constructor = constructor_name,
      `Start (Grid)` = grid_position,
      `Finish` = finish_position,
      `Positions Gained` = positions_gained,
      Status = status
    ) %>% 
    arrange(desc(`Positions Gained`)) # arrange from most places gained
}

ui = fluidPage(
  
  titlePanel("Formula 1 - How Much Does Where You Start Determine Where You Finish in an F1 Race?"),
  p("In Formula 1, drivers compete in a qualifying session the day before the race to set their
    starting grid position. P1 (pole position) is the fastest qualifier and starts at the front.
    This app explores how strongly that starting position predicts the final race result,
    whether that relationship has changed across seasons, and how it differs between teams.",
    style = "max-width: 800px; margin-bottom: 15px;"),
  p("Use the checkbox to filter by constructor and the slider to filter by season.",
    style = "font-size: 15px"),
  # inline = TRUE puts them horizontally, saves vertical space
  
  checkboxGroupInput("constructors", "Filter by Constructor", choices = constructors, selected = constructors, inline = TRUE), 
  sliderInput("season", "Season Range", min = min(f1$season), max = max(f1$season), c(2015, 2025), sep = ""),
  
  p("Gray points show all other constructors/seasons for context. Drag a box over any region of the plot to see which drivers and races those points represent.",
    style = "font-size: 15px; margin-top: 4px;"),
  # brush interactivity
  plotOutput("driver_position_plot", brush = brushOpts(id = "plot_brush", resetOnNew = TRUE)),
  
  # conditional, only show table header + table when something is brushed
  conditionalPanel(
    condition = "output.brush_active",
    h4("Selected Races"),
    p("Showing driver results from selected region. Outliers far from the dashed line represent races where
       starting position was a poor predictor of finishing position, which can happen for a variety of reasons
       unique to each race, such as safety cars, crashes, or bad pit stop strategy", style = "font-size: 12px;"
    ),
    dataTableOutput("brush_table")
  )
)

server = function(input, output) {
  f1_subset = reactive({
    req(input$constructors) # ensures no crashing
    f1 %>% 
      mutate(selected = 1 * (
        (season >= input$season[1]) &
          (season <= input$season[2]) &
          (constructor_name %in% input$constructors)
      ))
  })
  
  output$driver_position_plot = renderPlot({
    scatterplot(f1_subset())
  })
  
  # only look at the valid rows that are actually plotted
  selected_plotted = reactive({
    f1_subset() %>%
      filter(selected == 1, finished == TRUE, grid_position > 0)
  })
  
  brushed = reactive({
    brushedPoints(selected_plotted(), input$plot_brush,
                  xvar = "grid_position", yvar = "finish_position")
  })
  
  # this output flag drives the conditional panel
  output$brush_active = reactive({ nrow(brushed()) > 0 })
  outputOptions(output, "brush_active", suspendWhenHidden = FALSE)
  
  output$brush_table = renderDataTable({
    format_brush_table(brushed())
  }, options = list(pageLength = 10, scrollX = TRUE))
}

shinyApp(ui, server)