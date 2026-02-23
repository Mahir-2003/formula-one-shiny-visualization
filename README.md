# Formula 1 — How Much Does Where You Start Determine Where You Finish?

An interactive Shiny app exploring the relationship between qualifying grid position and race finishing position across F1 seasons from 2015 to 2025.

## Running the App

The dataset is loaded directly from this repo, so no local setup is needed beyond having R installed.

```r
# Install required packages if needed
install.packages(c("shiny", "tidyverse", "plotly"))

# Then run
shiny::runApp("app.R")
```

## Data

Race results data sourced from the [Jolpica F1 API](https://jolpi.ca/), covering the 2015–2025 seasons.
